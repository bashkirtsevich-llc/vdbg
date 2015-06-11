unit floppy;
/////////////////////////////////////////////////////////////////////////
// $Id: floppy.cc,v 1.36 2002/03/17 20:55:27 vruppert Exp $
/////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 2002  MandrakeSoft S.A.
//
//    MandrakeSoft S.A.
//    43, rue d'Aboukir
//    75002 Paris - France
//    http://www.linux-mandrake.com/
//    http://www.mandrakesoft.com/
//
//  This library is free software; you can redistribute it and/or
//  mod_ify it under the terms of the GNU Lesser General Public
//  License as published by the Free Software Foundation; either
//  version 2 of the License, or (at your option) any later version.
//
//  This library is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  Lesser General Public License for more details.
//
//  You should have received a copy of the GNU Lesser General Public
//  License along with this library; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
//
//
// Floppy Disk Controller Docs:
// Intel 82077A Data sheet
//   ftp://procedure-core.2y.net/pub/docs/fdc/82077AA_FloppyControllerDatasheet.pdf
// Intel 82078 Data sheet
//   ftp://download.intel.com/design/periphrl/datashts/29047403.PDF
// Other FDC references
//   http://debs.future.easyspace.com/Programming/Hardware/FDC/floppy.html
// And a port list:
//   http://mudlist.eorbit.net/~adam/pickey/ports.html
//

//bx_floppy_ctrl_c bx_floppy;


(* for main status register *)

interface

uses Config, Iodev, CMos, math, WorkSpaceWizLogic_u;


const 
   FD_MS_MRQ  = $80;
   FD_MS_DIO  = $40;
   FD_MS_NDMA = $20;
   FD_MS_BUSY = $10;
   FD_MS_ACTD = $08;
   FD_MS_ACTC = $04;
   FD_MS_ACTB = $02;
   FD_MS_ACTA = $01;
   FLOPPY_DMA_CHAN = 2;
   FROM_FLOPPY = 10;
   TO_FLOPPY  = 11;

type
   PFloppy = ^TFloppy;
   TFloppy = record
      fd: integer;         // file descriptor of floppy image file
      sectors_per_track: unsigned;    // number of sectors/track */
      sectors: unsigned;    // number of formatted sectors on diskette */
      tracks: unsigned;      // number of tracks */
      heads: unsigned;      // number of heads */
      type_: unsigned;
      write_protected: unsigned;
   end;

   PFloppyDrive = ^TFloppyDrive;
   TFloppyDrive = class
   public
      devices: pbx_devices_c;
      fCfg: PConf;
      s: record
         data_rate: Bit8u;

         command: array[0..10] of Bit8u; (* largest command size ??? *)
         command_index: Bit8u;
         command_size: Bit8u;
         command_complete: Bool;
         pending_command: Bit8u;

         multi_track: Bool;
         pending_irq: Bool;
         reset_sensei: Bit8u;
         format_count: Bit8u;
         format_fillbyte: Bit8u;

         Result: array[0..10] of Bit8u;
         result_index: Bit8u;
         result_size: Bit8u;

         DOR: Bit8u; // Digital Ouput Register
         TDR: Bit8u; // Tape Drive Register
         cylinder: array[0..4] of Bit8u; // really only using 2 drives
         head: array[0..4] of Bit8u;     // really only using 2 drives
         sector: array[0..4] of Bit8u;   // really only using 2 drives

    (* MAIN STATUS REGISTER
     * b7: MRQ: main request 1=data register ready     0=data register not ready
     * b6: DIO: data input/output:
     *     1=controller->CPU (ready for data read)
     *     0=CPU->controller (ready for data write)
     * b5: NDMA: non-DMA mode: 1=controller not in DMA modes
     *                         0=controller in DMA mode
     * b4: BUSY: instruction(device busy) 1=active 0=not active
     * b3-0: ACTD, ACTC, ACTB, ACTA:
     *       drive D,C,B,A in positioning mode 1=active 0=not active
     *)
         main_status_reg: Bit8u;

         status_reg0: Bit8u;
         status_reg1: Bit8u;
         status_reg2: Bit8u;
         status_reg3: Bit8u;

         // drive field allows up to 4 drives, even though probably only 2 will
         // ever be used.
         media: array[0..4] of TFloppy;
         num_supported_floppies: unsigned;
         floppy_buffer: array_buffer_floppy; // 2 extra for good measure
         floppy_buffer_index: unsigned;
         floppy_timer_index: integer;
         media_present: array[0..2] of Bool;
         DIR: Bit8u; // Digital Input Register:
         // b7: 0=diskette is present and has not been changed
         //     1=diskette missing or changed
      end;  // state information

      constructor Create;
      destructor  Destroy; override;
      procedure init(d: pbx_devices_c; cmos: pbx_cmos_c; Cfg:PConf); overload;
      procedure init(d: pbx_devices_c; cmos: pbx_cmos_c; const aConf: TWorkSpaceConfig); overload;
      procedure reset(Source: unsigned);
      procedure dma_write(data_byte: pBit8u);
      procedure dma_read(data_byte: pBit8u);
      function  set_media_status(drive: unsigned; status: unsigned): unsigned;
      function  get_media_status(drive: unsigned): unsigned;

      function  read_handler(this_ptr: pointer; address: Bit32u;
         io_len: unsigned): Bit32u;
      procedure write_handler(this_ptr: pointer; address: Bit32u; Value: Bit32u;
         io_len: unsigned);
      procedure floppy_command;
      procedure floppy_xfer(drive: Bit8u; offset: Bit32u; var buffer: array_buffer_floppy;
         bytes: Bit32u; direction: Bit8u);
      procedure raise_interrupt;
      procedure timer_handler(this_ptr: pointer);

      procedure timer;
      procedure increment_sector;
      function  evaluate_media(type_: unsigned; path: PChar; media: PFloppy): word;
   end;

var
   bx_floppy: TFloppyDrive;

implementation

uses Service, cpu, SysUtils, Pic, HDD;

constructor TFloppyDrive.Create;
begin
end;

destructor TFloppyDrive.Destroy;
var
  i: integer;
begin
  for I := 0 to 3 do
    FileClose(self.s.media[i].fd);
end;

procedure TFloppyDrive.init(d: pbx_devices_c; cmos: pbx_cmos_c; Cfg:PConf);
var
   addr: unsigned;
begin
  fCfg := Cfg;
  //	 BX_DEBUG(('Init $Id: floppy.cc,v 1.36 2002/03/17 20:55:27 vruppert Exp $'));
  Self.devices := d;

  Self.devices^.register_irq(6, 'Floppy Drive');

  addr := $03F2;
  while addr <= $03F7 do
  begin
    Self.devices^.register_io_read_handler(Self, read_handler,
       addr, 'Floppy Drive');
    Self.devices^.register_io_write_handler(Self, write_handler,
       addr, 'Floppy Drive');
    Inc(Addr);
  end;


  cmos^.s.reg[$10] := $00; (* start out with: no drive 0, no drive 1 *)

  Self.s.num_supported_floppies := 0;

  //
  // Floppy A setup
  //
  Self.s.media[0].sectors_per_track := 0;
  Self.s.media[0].tracks            := 0;
  Self.s.media[0].heads             := 0;
  Self.s.media[0].sectors           := 0;
  Self.s.media[0].type_             := cfg.FloppyAType; //BX_FLOPPY_1_44;
  Self.s.media[0].fd                := -1;
  Self.s.media_present[0]           := 0;

  case (Self.s.media[0].type_) of
    BX_FLOPPY_NONE:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $0f) or $00;
    BX_FLOPPY_1_2:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $0f) or $20;
    BX_FLOPPY_720K:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $0f) or $30;
    BX_FLOPPY_1_44:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $0f) or $40;
    BX_FLOPPY_2_88:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $0f) or $50;
  else
     LogPanic(('unknown floppya type'));
  end;

  if boolean(Self.s.media[0].type_ <> BX_FLOPPY_NONE) then
  begin
    Inc(Self.s.num_supported_floppies);
//    if boolean(BX_FLOPPY_INSERTED = BX_INSERTED) then
//    begin
       if boolean(evaluate_media(Self.s.media[0].type_, PChar(cfg.FloppyAFile), @Self.s.media[0])) then
       begin
         Self.s.media_present[0] := 1;
         Self.s.media[0].write_protected := word(Cfg.FloppyAReadOnly);
       end;
        //BX_INFO(('fd0: '%s' ro:=%d, h:=%d,t:=%d,spt:=%d', bx_options.floppya.Opath^.getptr(),
          //Self.s.media[0].write_protected, Self.s.media[0].heads, Self.s.media[0].tracks, Self.s.media[0].sectors_per_track));
//    end;
  end;


  //
  // Floppy B setup
  //
  Self.s.media[1].sectors_per_track := 0;
  Self.s.media[1].tracks            := 0;
  Self.s.media[1].heads             := 0;
  Self.s.media[1].sectors           := 0;
  Self.s.media[1].type_             := cfg.FloppyBType; //BX_FLOPPY_NONE;
  Self.s.media[1].fd                := -1;
  Self.s.media_present[1]           := 0;


  case ({BX_GET_FLOPPYB_TYPE}Self.s.media[1].type_) of
    BX_FLOPPY_NONE:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $f0) or $00;
    BX_FLOPPY_1_2:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $f0) or $02;
    BX_FLOPPY_720K:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $f0) or $03;
    BX_FLOPPY_1_44:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $f0) or $04;
    BX_FLOPPY_2_88:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $f0) or $05;
  else
    LogPanic(('unknown floppyb type'));
  end;

  if boolean(Self.s.media[1].type_{BX_GET_FLOPPYB_TYPE}<> BX_FLOPPY_NONE) then
  begin
    Inc(Self.s.num_supported_floppies);
//    if boolean(BX_FLOPPY_INSERTED = BX_INSERTED) then
//    begin
       if boolean(evaluate_media(Self.s.media[1].type_, PChar(cfg.FloppyBFile), @Self.s.media[1])) then
       begin
         Self.s.media_present[1] := 1;
         Self.s.media[1].write_protected := word(Cfg.FloppyBReadOnly);
       end;
        //BX_INFO(('fd1: '%s' ro:=%d, h:=%d,t:=%d,spt:=%d', bx_options.floppyb.Opath^.getptr(),
          //Self.s.media[1].write_protected, Self.s.media[0].heads, Self.s.media[0].tracks, Self.s.media[0].sectors_per_track));
//    end;
  end;

  (* CMOS Equipment Byte register *)
  if boolean(Self.s.num_supported_floppies > 0) then
    cmos^.s.reg[$14] := (cmos^.s.reg[$14] and $3e) or
       ((Self.s.num_supported_floppies - 1) shl 6) or 1
  else
    cmos^.s.reg[$14] := (cmos^.s.reg[$14] and $3e);

  Self.s.floppy_timer_index :=
    bx_pc_system.register_timer(@bx_floppy, timer_handler, 50000, 0,0);

//   BX_DEBUG(Format('bx_options.Ofloppy_command_delay := %u', [50000]));
end;

procedure TFloppyDrive.reset(Source: unsigned);
var
   i: Bit32u;
begin
  Self.s.command_complete := 1; (* waiting for new command *)
  Self.s.command_index    := 0;
  Self.s.command_size     := 0;
  Self.s.pending_command  := 0;

  Self.s.pending_irq  := 0;
  Self.s.reset_sensei := 0; (* no reset result present *)

  Self.s.result_index := 0;
  Self.s.result_size  := 0;

  (* data register ready, not in DMA mod_e *)
  Self.s.main_status_reg := FD_MS_MRQ;
  Self.s.status_reg0 := 0;
  Self.s.status_reg1 := 0;
  Self.s.status_reg2 := 0;
  Self.s.status_reg3 := 0;

  // software reset (via DOR port $3f2 bit 2) does not change DOR
  if (Source = BX_RESET_HARDWARE) then
  begin
    Self.s.DOR := $0c;
    // motor off, drive 3..0
    // DMA/INT enabled
    // normal operation
    // drive select 0

    // DIR and CCR affected only by hard reset
    Self.s.DIR := Self.s.DIR or $80; // disk changed
    Self.s.data_rate := 0; (* 500 Kbps *)
  end;

  i := 0;
  while i < 4 do
  begin
    Self.s.cylinder[i] := 0;
    Self.s.head[i] := 0;
    Self.s.sector[i] := 0;
    Inc(i);
  end;

  Self.s.floppy_buffer_index := 0;

  bx_pic.lower_irq(6);
  bx_pc_system.set_DRQ(FLOPPY_DMA_CHAN, 0);
end;
  // static IO port read callback handler
  // redirects to non-static class handler to aprocedure virtual functions
function TFloppyDrive.read_handler(this_ptr: pointer; address: Bit32u;
   io_len: unsigned): Bit32u;
var
   status, Value: Bit8u;
begin

   if boolean(io_len > 1) then
      LogPanic(Format('io read from address %08x, len:=%u', [address, io_len]));

//   if boolean(BX_DEBUG_FLOPPY) then
      LogInfo(Format('read access to port %04x', [address]));

  case (address) of
    $3F2: // diskette controller digital output register
    begin
      Value := Self.s.DOR;
      Result := Value;
      Exit;
    end;

    $3F4: (* diskette controller main status register *)
    begin
      status := Self.s.main_status_reg;
      Result := status;
      Exit;
    end;

    $3F5: (* diskette controller data *)
    begin
      if boolean(Self.s.result_size = 0) then
      begin
        LogError(('port $3f5: no results to read'));
        Self.s.main_status_reg := 0;
        Result := Self.s.Result[0];
        Exit;
      end;

      Value := Self.s.Result[Self.s.result_index];
      Inc(Self.s.result_index);
      Self.s.main_status_reg := Self.s.main_status_reg and $F0;
      if boolean(Self.s.result_index >= Self.s.result_size) then
      begin
        Self.s.result_size := 0;
        Self.s.result_index := 0;
        Self.s.Result[0] := Value;
        Self.s.main_status_reg := FD_MS_MRQ;
        if boolean(Self.s.reset_sensei = 0) then
          Self.s.pending_irq := 0;
        bx_pic.lower_irq(6);
      end;
      Result := Value;
      Exit;
    end;

    $3F6: // Reserved for future floppy controllers
    // Self address shared with the hard drive controller
    begin
      Value := HardDrive.read_handler(HardDrive, address, io_len);
      Result := Value;
      Exit;
    end;

    $3F7: // diskette controller digital input register
    begin
      // Self address shared with the hard drive controller:
      //   Bit  7   : floppy
      //   Bits 6..0: hard drive
      Value := HardDrive.read_handler(HardDrive, address, io_len);
      Value := Value and $7f;
      // add in diskette change line
      Value := Value or (Self.s.DIR and $80);
      Result := Value;
      Exit;
    end;
  else
    begin
      LogError(Format('io_read: unsupported address $%04x', [address]));
      Result := 0;
    end;
  end;
end;

  // static IO port write callback handler
  // redirects to non-static class handler to aprocedure virtual functions

procedure TFloppyDrive.write_handler(this_ptr: pointer; address: Bit32u;
   Value: Bit32u; io_len: unsigned);
var
  dma_and_interrupt_enable: Bit8u;
  normal_operation, prev_normal_operation: Bit8u;
  drive_select: Bit8u;
  motor_on_drive0, motor_on_drive1: Bit8u;
begin
//   {$if BX_USE_FD_SMF=0}
//   bx_floppy_ctrl_c * class_ptr := (bx_floppy_ctrl_c *) Self_ptr;
//
//   class_ptr^.Write(address, Value, io_len);
//end;
//
//  (* writes to the floppy io ports *)
//procedure bx_floppy_ctrl_c.Write(Bit32u address, Bit32u Value, unsigned io_len)
//   begin
//   {$else}
//   //UNUSED(Self_ptr);
//   {$ifend}  // !BX_USE_FD_SMF

  if boolean(io_len > 1) then
    LogPanic(Format('io write to address %08x, len:=%u', [address, io_len]));

  {if boolean(BX_DEBUG_FLOPPY) then}    LogInfo(Format('write access to port %04x, value:=%02x',
    [address, Value]));

  case (address) of
    $3F2: (* diskette controller digital output register *)
    begin
      motor_on_drive1 := Value and $20;
      motor_on_drive0 := Value and $10;
      dma_and_interrupt_enable := Value and $08;
      //   if boolean(dma_and_interrupt_enable = 0) then
      //      BX_DEBUG(('DMA and interrupt capabilities disabled'));
      normal_operation := Value and $04;
      drive_select := Value and $03;

      prev_normal_operation := Self.s.DOR and $04;
      Self.s.DOR := Value;

      if boolean((prev_normal_operation = 0) and (normal_operation <> 0))
             // transition from RESET to NORMAL
        then bx_pc_system.activate_timer(Self.s.floppy_timer_index, 50000, 0)
        else

      if boolean((prev_normal_operation <> 0) and (normal_operation = 0)) then
      begin
        // transition from NORMAL to RESET
        Self.s.main_status_reg := FD_MS_BUSY;
        Self.s.pending_command := $fe; // RESET pending
      end;
//   BX_DEBUG(('io_write: digital output register'));
//   BX_DEBUG(Format('  motor on, drive1 := %d', [word(motor_on_drive1 > 0)]));
//   BX_DEBUG(Format('  motor on, drive0 := %d', [word(motor_on_drive0 > 0)]));
//   BX_DEBUG(Format('  dma_and_interrupt_enable:=%02x', [dma_and_interrupt_enable]));
//   BX_DEBUG(Format('  normal_operation:=%02x', [normal_operation]));
//   BX_DEBUG(Format('  drive_select:=%02x', [drive_select]));
      if boolean(drive_select > 1) then
        LogPanic(('io_write: drive_select>1'));
    end;

    $3f4: (* diskette controller data rate select register *)
      LogError(('io_write: data rate select register unsupported'));

    $3F5: (* diskette controller data *)
    begin
//      BX_DEBUG(Format('command := %02x', [Value]));
      if boolean(Self.s.command_complete) then
      begin
        if boolean(Self.s.pending_command <> 0) then
          LogPanic(Format('io: 3f5: receiving new comm, old one (%02x) pending',
            [Self.s.pending_command]));
        Self.s.command[0] := Value;
        Self.s.command_complete := 0;
        Self.s.command_index := 1;
        (* read/write command in progress *)
        Self.s.main_status_reg := FD_MS_MRQ or FD_MS_BUSY;
        case (Value) of
          $03: (* specify *)
            Self.s.command_size := 3;
          $04: // get status
            Self.s.command_size := 2;
          $07: (* recalibrate *)
            Self.s.command_size := 2;
          $08: (* sense interrupt status *)
            Self.s.command_size := 1;
          $0f: (* seek *)
            Self.s.command_size := 3;
          $4a: (* read ID *)
            Self.s.command_size := 2;
          $4d: (* format track *)
            Self.s.command_size := 6;
          $45,          $c5: (* write normal data *)
            Self.s.command_size := 9;
          $66,          $e6: (* read normal data *)
            Self.s.command_size := 9;

          $13: // Configure command (Enhanced)
            Self.s.command_size := 4;

          $0e, // dump registers (Enhanced drives)
          $10, // Version command, standard controller returns 80h
          $18: // National Semiconductor version command; return 80h
          // These commands are not implemented on the standard
          // controller and return an error.  They are available on
          // the enhanced controller.
          begin
            //      BX_DEBUG(Format('io_write: $3f5: unsupported floppy command $%02x', [Value]));
            Self.s.command_size := 1;
            Self.s.status_reg0 := $80; // status: invalid command
          end;
        else
          begin
            LogError(Format ('io_write: $3f5: invalid floppy command $%02x', [Value]));
            Self.s.command_size := 1;
            Self.s.status_reg0 := $80; // status: invalid command
          end;
        end;
      end else
      begin
        Self.s.command[Self.s.command_index] := Value;
        Inc(Self.s.command_index);
      end;
      if boolean(Self.s.command_index = Self.s.command_size) then
      begin
        (* read/write command not in progress any more *)
        floppy_command();
        Self.s.command_complete := 1;
      end;
      //   BX_DEBUG(('io_write: diskette controller data'));
      exit;
    end;

    $3F6: (* diskette controller (reserved) *)
      //   BX_DEBUG(('io_write: reserved register unsupported'));
      // Self address shared with the hard drive controller
      HardDrive.write_handler(HardDrive, address, Value, io_len);



    $3F7: (* diskette controller configuration control register *)
    begin
      //         BX_DEBUG(('io_write: config control register'));
      Self.s.data_rate := Value and $03;
      //         case (Self.s.data_rate) of
      //           0: BX_DEBUG(('  500 Kbps'));
      //           1: BX_DEBUG(('  300 Kbps'));
      //           2: BX_DEBUG(('  250 Kbps'));
      //           3: BX_DEBUG(('  1 Mbps'));
      //         end;

      exit;
    end;
  else
      LogError(Format('io_write ignored: $%04h := $%02h', [address, Value]));
  end;
end;

procedure TFloppyDrive.floppy_command;
var
   i: unsigned;
   step_rate_time: Bit8u;
   head_unload_time: Bit8u;
   head_load_time: Bit8u;
   motor_on: Bit8u;
   head, drive, cylinder, sector, eot: Bit8u;
   sector_size, data_length: Bit8u;
   logical_sector: Bit32u;
   bytesreaded: longword;
begin
//   {$if BX_PROVIDE_CPU_MEMORY=0}
//   LogPanic(('floppy_command(): uses DMA: not supported for'           ' external environment'));
//   {$else}

//   BX_DEBUG(('FLOPPY COMMAND: '));
//   I := 0;
//   while i < Self.s.command_size do
//      begin
//         BX_DEBUG(Format('[%02x] ', [Self.s.command[i]]));
//         Inc(i);
//      end;

//   {$if false}
//   (* execute phase of command is in progress (non DMA mod_e) *)
//   Self.s.main_status_reg |:= 20;
//   {$ifend}

  case (Self.s.command[0]) of
    $03: // specify
    begin
      // execution: specified parameters are loaded
      // result: no result bytes, no interrupt
      step_rate_time := Self.s.command[1] shr 4;
      head_unload_time := Self.s.command[1] and $0f;
      head_load_time := Self.s.command[2] shr 1;
      if boolean(Self.s.command[2] and $01) then
      LogError(('non DMA mod_e selected'));
      Self.s.main_status_reg := FD_MS_MRQ;
      exit;
    end;

    $04: // get status
    begin
      drive := (Self.s.command[1] and $03);
      s.result[0]:=$28 or (Self.s.head[drive] shl 2) or drive
        or ifthen(Self.s.media[drive].write_protected <> 0, $40, 0);
      if boolean(Self.s.cylinder[drive] = 0) then
          Self.s.Result[0] := Self.s.Result[0] or $10;
      Self.s.result_size := 1;
      Self.s.result_index := 0;
      Self.s.main_status_reg := FD_MS_MRQ or FD_MS_DIO or FD_MS_BUSY;
      exit;
    end;

    $07: // recalibrate
    begin
      drive := (Self.s.command[1] and $03);
      Self.s.DOR := Self.s.DOR and $fc;
      Self.s.DOR := Self.s.DOR or drive;
//        BX_DEBUG(Format('floppy_command(): recalibrate drive %u',
//          [drive]));
      if (drive > 1) then
        LogPanic(('floppy_command(): drive > 1'));
      //motor_on := Self.s.DOR  and $f0;
      motor_on := ((Self.s.DOR shr (drive + 4)) and $01);

      if (motor_on = 0) then
        LogInfo(('floppy_command(): recal drive with motor off'));

      if (drive = 0)
        then Self.s.DOR := Self.s.DOR or $10 // turn on MOTA
        else Self.s.DOR := Self.s.DOR or $20; // turn on MOTB

      Self.s.cylinder[drive] := 0;
      bx_pc_system.activate_timer(Self.s.floppy_timer_index,
        50000, 0);
      (* command head to track 0
       * controller set to non-busy
       * error condition noted in Status reg 0's equipment check bit
       * seek end bit set to 1 in Status reg 0 regardless of outcome
       *)
      (* data reg not ready, controller busy *)
      Self.s.main_status_reg := FD_MS_BUSY;
      Self.s.pending_command := $07; // recalibrate pending
      exit;
    end;

    $08: (* sense interrupt status *)
    begin
      (* execution:
       *   get status
       * result:
       *   no interupt
       *   byte0 := status reg0
       *   byte1 := current cylinder number (0 to 79)
       *)
      drive := Self.s.DOR and $03;
      if boolean(Self.s.pending_irq = 0) then
      begin
        Self.s.status_reg0 := $80;
        Self.s.result_size := 1;
      end else
      begin
        if boolean(Self.s.reset_sensei > 0) then
        begin
          drive := 4 - Self.s.reset_sensei;
          Self.s.status_reg0 := Self.s.status_reg0 and $fc;
          Self.s.status_reg0 := Self.s.status_reg0 or drive;
          dec(Self.s.reset_sensei);
        end;
        Self.s.Result[1] := Self.s.cylinder[drive];
        Self.s.result_size := 2;
      end;
      Self.s.Result[0] := Self.s.status_reg0;
      Self.s.result_index := 0;
      if boolean(Self.s.reset_sensei = 0) then
        Self.s.status_reg0 := 0;

      (* read ready *)
      Self.s.main_status_reg := Self.s.main_status_reg and $0f;
      Self.s.main_status_reg := Self.s.main_status_reg or
       (FD_MS_MRQ or FD_MS_DIO or FD_MS_BUSY);
//      BX_DEBUG(('sense interrupt status'));
      exit;
    end;

    $0f: (* seek *)
    begin
      (* command:
       *   byte0 := 0F
       *   byte1 := drive  and head select
       *   byte2 := cylinder number
       * execution:
       *   postion head over specified cylinder
       * result:
       *   no result bytes, issues an interrupt
       *)
      drive := Self.s.command[1] and $03;
      Self.s.DOR := Self.s.DOR and $fc;
      Self.s.DOR := Self.s.DOR or drive;

      Self.s.head[drive] := (Self.s.command[1] shr 2) and $01;
      Self.s.cylinder[drive] := Self.s.command[2];
      if boolean(drive > 1) then
        LogPanic(('floppy_command(): seek: drive>1'));
      (* ??? should also check cylinder validity *)
      bx_pc_system.activate_timer(Self.s.floppy_timer_index,
        50000, 0);
      (* data reg not ready, controller busy *)
      Self.s.main_status_reg := FD_MS_BUSY;
      Self.s.pending_command := $0f; (* seek pending *)
      exit;
    end;

    $13: // Configure
    begin
    //BX_DEBUG(('io: configure (mod_e:=%02xh, pretrack:=%02xh)',
      //(Self.s.command[2]), (unsigned)(Self.s.command[3]) ));
      Self.s.result_size := 0;
      Self.s.result_index := 0;
      Self.s.pending_command := 0;
      Self.s.main_status_reg := FD_MS_MRQ or FD_MS_BUSY;
      exit;
    end;

    $4a: // read ID
    begin
      drive := Self.s.command[1] and $03;
      Self.s.DOR := Self.s.DOR and $fc;
      Self.s.DOR := Self.s.DOR or drive;

      motor_on := (Self.s.DOR shr (drive + 4)) and $01;
      if boolean(motor_on = 0) then
      begin
        LogError(('floppy_command(): $4a: motor not on'));
        Self.s.main_status_reg := FD_MS_BUSY;
        exit;
      end;
      if boolean(drive > 1) then
        LogPanic(('io: 4a: bad drive #'));
      Self.s.result_size := 7;
      Self.s.result_index := 0;
      // setting result[0] in timer handler
      Self.s.Result[1] := Self.s.status_reg1;
      Self.s.Result[2] := Self.s.status_reg2;
      Self.s.Result[3] := Self.s.cylinder[drive];
      Self.s.Result[4] := Self.s.head[drive];
      Self.s.Result[5] := 1; (* sector at completion *)
      Self.s.Result[6] := 2; // sector size code
      bx_pc_system.activate_timer(Self.s.floppy_timer_index,
        50000, 0);
      (* data reg not ready, controller busy *)
      Self.s.main_status_reg := FD_MS_BUSY;
      Self.s.pending_command := $4a; (* read ID pending *)
      exit;
    end;

    $4d: // format track
    begin
      drive := Self.s.command[1] and $03;
      Self.s.DOR := Self.s.DOR and $fc;
      Self.s.DOR := Self.s.DOR or drive;

      motor_on := (Self.s.DOR shr (drive + 4)) and $01;

      if boolean(motor_on = 0) then
        LogPanic(('floppy_command(): format track: motor not on'));

      Self.s.head[drive] := (Self.s.command[1] shr 2) and $01;
      sector_size := Self.s.command[2];
      Self.s.format_count := Self.s.command[3];
      Self.s.format_fillbyte := Self.s.command[5];

      if boolean(drive > 1) then
        LogPanic(Format('format track: bad drive #%d', [drive]));

      if boolean(sector_size <> $02) then
      // 512 bytes
        LogPanic(('format track: sector_size not 512'));
      if boolean(Self.s.format_count <> Self.s.media[drive].sectors_per_track) then
          LogPanic(('format track: wrong number of sectors/track'));
      if boolean(Self.s.media_present[drive] = 0) then
      begin
        // media not in drive, return error
        LogInfo(('attempt to format track with media not present'));
        Self.s.result_size := 7;
        Self.s.result_index := 0;
        Self.s.status_reg0 := $40 or (Self.s.head[drive] shl 2) or drive;
        // abnormal termination
        Self.s.Result[0] := Self.s.status_reg0;
        Self.s.Result[1] := $25; // 0010 0101
        Self.s.Result[2] := $31; // 0011 0001
        // 4 result bytes are unused
        Self.s.pending_command := 0;
        Self.s.main_status_reg := FD_MS_MRQ or FD_MS_DIO or FD_MS_BUSY;
        raise_interrupt();
        exit;
      end;
      if boolean(Self.s.media[drive].write_protected) then
      begin
        // media write-protected, return error
        LogInfo(('attempt to format track with media write-protected'));
        Self.s.result_size := 7;
        Self.s.result_index := 0;
        Self.s.status_reg0 := $40 or (Self.s.head[drive] shl 2) or drive;
        // abnormal termination
        Self.s.Result[0] := Self.s.status_reg0;
        Self.s.Result[1] := $27; // 0010 0111
        Self.s.Result[2] := $31; // 0011 0001
        // 4 result bytes are unused
        Self.s.pending_command := 0;
        Self.s.main_status_reg := FD_MS_MRQ or FD_MS_DIO or FD_MS_BUSY;
        raise_interrupt();
        exit;
      end;

      (* 4 header bytes per sector are required *)
      Self.s.format_count := Self.s.format_count shl 2;

      bx_pc_system.set_DRQ(FLOPPY_DMA_CHAN, 1);

      (* data reg not ready, controller busy *)
      Self.s.main_status_reg := FD_MS_BUSY;
      Self.s.pending_command := $4d; (* format track pending *)
//      BX_DEBUG(('format track'));
      exit;
    end;

    $66, // read normal data, MT:=0
    $e6, // read normal data, MT:=1
    $45, // write normal data, MT:=0
    $c5: // write normal data, MT:=1
    begin
      Self.s.multi_track := (Self.s.command[0] shr 7);

      if boolean((Self.s.DOR and $08) = 0) then
        LogPanic(('read/write command with DMA and int disabled'));

      drive := Self.s.command[1] and $03;
      Self.s.DOR := Self.s.DOR and $fc;
      Self.s.DOR := Self.s.DOR or drive;

      motor_on := (Self.s.DOR shr (drive + 4)) and $01;

      if boolean(motor_on = 0) then
        LogPanic(('floppy_command(): read/write: motor not on'));

      head := Self.s.command[3] and $01;
      cylinder := Self.s.command[2]; (* 0..79 depending *)
      sector := Self.s.command[4];   (* 1..36 depending *)
      eot := Self.s.command[6];      (* 1..36 depending *)
      sector_size := Self.s.command[5];
      data_length := Self.s.command[8];
//      BX_DEBUG(('read/write normal data'));
//      BX_DEBUG(('BEFORE'));
//      BX_DEBUG(Format('  drive    := %u', [drive]));
//      BX_DEBUG(Format('  head     := %u', [head]));
//      BX_DEBUG(Format('  cylinder := %u', [cylinder]));
//      BX_DEBUG(Format('  sector   := %u', [sector]));
//      BX_DEBUG(Format('  eot      := %u', [eot]));
      if boolean(drive > 1) then
        LogPanic(('io: bad drive #'));

      if boolean(head > 1) then
        LogPanic(('io: bad head #'));

      // check that head number in command[1] bit two matches the head
      // reported in the head number field.  Real floppy drives are
      // picky about Self, as reported in SF bug #439945, (Floppy drive
      // read input error checking).
      if boolean(head <> (Self.s.command[1] shr 2) and 1) then
      begin
        LogError(('head number in command[1] doesn''t match head field'));
        Self.s.result_size := 7;
        Self.s.result_index := 0;
        Self.s.status_reg0 := $40 or (Self.s.head[drive] shl 2) or drive;
 // abnormal termination
        Self.s.Result[0] := Self.s.status_reg0;
        Self.s.Result[1] := $04; // 0000 0100
        Self.s.Result[2] := $00; // 0000 0000
        Self.s.Result[3] := Self.s.cylinder[drive];
        Self.s.Result[4] := Self.s.head[drive];
        Self.s.Result[5] := Self.s.sector[drive];
        Self.s.Result[6] := 2; // sector size := 512

        Self.s.pending_command := 0;
        Self.s.main_status_reg := FD_MS_MRQ or FD_MS_DIO or FD_MS_BUSY;
        raise_interrupt();
        exit;
      end;

      if boolean(Self.s.media_present[drive] = 0) then
      begin
        // media not in drive, return error

        LogInfo(Format('attempt to read/write sector %u, sectors/track:=%u', [sector,
                     Self.s.media[drive].sectors_per_track]));
        Self.s.result_size := 7;
        Self.s.result_index := 0;
        Self.s.status_reg0 := $40 or (Self.s.head[drive] shl 2) or drive;
        // abnormal termination
        Self.s.Result[0] := Self.s.status_reg0;
        Self.s.Result[1] := $25; // 0010 0101
        Self.s.Result[2] := $31; // 0011 0001
        Self.s.Result[3] := Self.s.cylinder[drive];
        Self.s.Result[4] := Self.s.head[drive];
        Self.s.Result[5] := Self.s.sector[drive];
        Self.s.Result[6] := 2; // sector size := 512

        Self.s.pending_command := 0;
        Self.s.main_status_reg := FD_MS_MRQ or FD_MS_DIO or FD_MS_BUSY;
        raise_interrupt();
        exit;
      end;

      if boolean(sector_size <> $02) then
        // 512 bytes
        LogPanic(('sector_size not 512'));

      if boolean(cylinder >= Self.s.media[drive].tracks) then
      begin
        LogPanic(Format('io: norm r/w parms out of range: sec#%02xh cyl#%02xh eot#%02h head#%02xh',
                 [sector, cylinder, eot, head]));
        exit;
      end;

      if boolean(sector > Self.s.media[drive].sectors_per_track) then
      begin
        // requested sector > last sector on track
        LogInfo(Format('attempt to read/write sector %u, sectors/track:=%u', [sector,
                     Self.s.media[drive].sectors_per_track]));
        // set controller to where drive would have left off
        // after it discovered the sector was past EOT
        Self.s.cylinder[drive] := cylinder;
        Self.s.head[drive] := head;
        Self.s.sector[drive] := Self.s.media[drive].sectors_per_track;

        Self.s.result_size := 7;

        Self.s.result_index := 0;
        // 0100 0HDD abnormal termination
        Self.s.status_reg0 := $40 or (Self.s.head[drive] shl 2) or drive;
        Self.s.Result[0] := Self.s.status_reg0;
        // 1000 0101 end of cyl/NDAT/NID
        Self.s.Result[1] := $86;
        // 0000 0000
        Self.s.Result[2] := $00;
        Self.s.Result[3] := Self.s.cylinder[drive];
        Self.s.Result[4] := Self.s.head[drive];
        Self.s.Result[5] := Self.s.sector[drive];
        Self.s.Result[6] := 2; // sector size := 512

        bx_pc_system.activate_timer(Self.s.floppy_timer_index,
          50000, 0);
        (* data reg not ready, controller busy *)
        Self.s.main_status_reg := FD_MS_BUSY;
        Self.s.pending_command := Self.s.command[0];
        exit;
      end;

//{$if false}
//      if boolean(eot <> Self.s.media[drive].sectors_per_track)
//        BX_DEBUG(('io: bad eot #%02xh', (unsigned) eot));
//{$ifend}

//      if boolean(cylinder <> Self.s.cylinder[drive]) then
//        BX_DEBUG(('io: cylinder request <> current cylinder'));

      logical_sector := (cylinder * 2 * Self.s.media[drive].sectors_per_track) +
                       (head * Self.s.media[drive].sectors_per_track) +
                       (sector - 1);

      if boolean(logical_sector >= Self.s.media[drive].sectors) then
        LogPanic(('io: logical sector out of bounds'));

      Self.s.cylinder[drive] := cylinder;
      Self.s.sector[drive] := sector;
      Self.s.head[drive] := head;

      if boolean((Self.s.command[0] and $7f) = $66) then
      begin // read
        floppy_xfer(drive, logical_sector * 512, Self.s.floppy_buffer,
                    512, FROM_FLOPPY);
        Self.s.floppy_buffer_index := 0;

        bx_pc_system.set_DRQ(FLOPPY_DMA_CHAN, 1);

        (* data reg not ready, controller busy *)
        Self.s.main_status_reg := FD_MS_BUSY;
        Self.s.pending_command := Self.s.command[0];
        exit;
      end else
      if boolean((Self.s.command[0] and $7f) = $45) then
      begin // write
        Self.s.floppy_buffer_index := 0;

        bx_pc_system.set_DRQ(FLOPPY_DMA_CHAN, 1);

        (* data reg not ready, controller busy *)
        Self.s.main_status_reg := FD_MS_BUSY;
        Self.s.pending_command := Self.s.command[0];
        exit;
      end else
        LogPanic(('floppy_command(): unknown read/write command'));

      exit;
    end;

  else // invalid or unsupported command
    begin
      Self.s.result_size := 1;
      Self.s.result_index := 0;
      Self.s.Result[0] := Self.s.status_reg0;
      Self.s.status_reg0 := 0;
      Self.s.main_status_reg := FD_MS_MRQ or FD_MS_DIO or FD_MS_BUSY;
    end;
  end;
//{$ifend}
end;

procedure TFloppyDrive.floppy_xfer(drive: Bit8u; offset: Bit32u;
   var buffer: array_buffer_floppy; bytes: Bit32u; direction: Bit8u);
var
  ret: integer;
  BytesReaded, ByteWrited: LongWord;
begin
  if (drive > 1) then
    LogPanic(('floppy_xfer: drive > 1'));

//  if boolean(BX_DEBUG_FLOPPY) then
//  begin
    LogInfo(Format('drive:=%u', [drive]));
    LogInfo(Format('offset:=%u', [offset]));
    LogInfo(Format('bytes:=%u', [bytes]));
    //BX_INFO(Format('direction:=%s'[(direction=FROM_FLOPPY)? 'from' : 'to'));
//  end;

//{$if BX_WITH_MACOS=1}
//  if boolean(strcmp(bx_options.floppya.Opath ^.getptr (), SuperDrive))
//{$ifend}
//    begin
  ret := fileseek(Self.s.media[drive].fd, offset, 0);
  if (ioresult < 0) then
    LogPanic(('could not perform lseek() on floppy image file'));
//    end;

  if (direction = FROM_FLOPPY) then
  begin
//{$if BX_WITH_MACOS=1}
//    if (!strcmp(bx_options.floppya.Opath ^.getptr (), SuperDrive))
//      ret := fd_read((char *) buffer, offset, bytes);
//    else
//{$ifend}
    bytesreaded := FileRead(Self.s.media[drive].fd, buffer, bytes);
    if (bytesreaded < int(bytes)) then
    begin
      (* ??? *)
      if (bytesreaded > 0)
        then LogInfo(Format('partial read() on floppy image returns %u/%u', [ret, bytes]))
        //FillChar(buffer + ret, bytes - ret,0);
        else
      begin
        LogInfo(('read() on floppy image returns 0'));
        //memset(buffer, 0, bytes);
      end;
    end;
  end else
  begin // TO_FLOPPY
//    assert (Self.s.media[drive].write_protected = 0);
    if Self.s.media[drive].write_protected = 1 then
    begin
      ByteWrited := bytes;
      exit;
    end;
//{$if BX_WITH_MACOS=1}
//    if boolean(!strcmp(bx_options.floppya.Opath ^.getptr (), SuperDrive))
//      ret := fd_write((char *) buffer, offset, bytes);
//    else
//{$ifend}
    ByteWrited := integer(FileWrite(Self.s.media[drive].fd, buffer, bytes));

    if boolean(ByteWrited < int(bytes)) then
      LogPanic(('could not perform write() on floppy image file'));
  end;
end;

procedure TFloppyDrive.timer_handler(this_ptr: pointer);
//var
//   class_ptr: PFloppyDrive;
begin
  PFloppyDrive(this_ptr)^.timer();
//   class_ptr := PFloppyDrive(this_ptr);
//   class_ptr ^.timer();
end;

procedure TFloppyDrive.timer;
var
  drive: Bit8u;
label
  reset_changeline;
begin
  drive := Self.s.DOR and $03;
  case (Self.s.pending_command) of
    $07: // recal
    begin
      Self.s.pending_command := 0;
      (* write ready, not busy *)
      Self.s.main_status_reg := FD_MS_MRQ or (1 shl drive);
      Self.s.status_reg0 := $20 or drive;
      raise_interrupt();
      goto reset_changeline;
    end;

    $0f: // seek
    begin
       Self.s.pending_command := 0;
       (* write ready, not busy *)
       Self.s.main_status_reg := FD_MS_MRQ or (1 shl drive);
       Self.s.status_reg0 := $20 or drive;
       raise_interrupt();
       goto reset_changeline;
    end;

    $4a: (* read ID *)
    begin
      Self.s.pending_command := 0;
      (* read ready, busy *)
      Self.s.main_status_reg := FD_MS_MRQ or FD_MS_DIO;
      Self.s.status_reg0 := $20 or drive;
      Self.s.Result[0] := Self.s.status_reg0;
      raise_interrupt();
    end;

    $66, // read normal data
    $e6,
    $45, // write normal data
    $c5:
    begin
      Self.s.pending_command := 0;
      (* read ready, busy *)
      Self.s.main_status_reg := FD_MS_MRQ or FD_MS_DIO or FD_MS_BUSY or (1 shl drive);
      Self.s.status_reg0 := $20 or drive;
      raise_interrupt();
    end;

    $fe: // (contrived) RESET
    begin
      reset(BX_RESET_SOFTWARE);
      Self.s.pending_command := 0;
      Self.s.status_reg0 := $c0;
      raise_interrupt();
      Self.s.reset_sensei := 4;
    end;

    $00: // nothing pending?
      exit; // !!!

  else
    LogPanic(Format('floppy:timer(): unknown case %02x', [Self.s.pending_command]));
  end;
  //exit;
reset_changeline:
  if (drive > 1) then
    exit;

  if boolean(Self.s.media_present[drive]) then
    Self.s.DIR := Self.s.DIR and not $80; // clear disk change line
end;

procedure TFloppyDrive.dma_write(data_byte: PBit8u);
var
   drive: Bit8u;
   logical_sector: Bit32u;
begin
  // A DMA write is from I/O to Memory
  // We need to return then next data byte from the floppy buffer
  // to be transfered via the DMA to memory. (read block from floppy)

  data_byte ^ := Self.s.floppy_buffer[Self.s.floppy_buffer_index];
  Inc(Self.s.floppy_buffer_index);

  if boolean(Self.s.floppy_buffer_index >= 512) then
  begin
    drive := Self.s.DOR and $03;
    increment_sector(); // increment to next sector before retrieving next one
    Self.s.floppy_buffer_index := 0;

    if boolean(bx_pc_system.TC) then
    begin // Terminal Count line, done
      Self.s.pending_command := 0;
      Self.s.main_status_reg := FD_MS_MRQ or FD_MS_DIO or FD_MS_BUSY or (1 shl drive);
      Self.s.result_size     := 7;
      Self.s.result_index    := 0;
      Self.s.status_reg0     := $20 or (Self.s.head[drive] shl 2) or drive;
      Self.s.Result[0]       := Self.s.status_reg0;
      Self.s.Result[1]       := 0;
      Self.s.Result[2]       := 0;
      Self.s.Result[3]       := Self.s.cylinder[drive];
      Self.s.Result[4]       := Self.s.head[drive];
      Self.s.Result[5]       := Self.s.sector[drive];
      Self.s.Result[6]       := 2;

//      if boolean(BX_DEBUG_FLOPPY) then
//      begin
      LogInfo(('shlREAD DONEshr'));
      LogInfo(('AFTER'));
      LogInfo(Format('  drive    := %u', [drive]));
      LogInfo(Format('  head     := %u', [Self.s.head[drive]]));
      LogInfo(Format('  cylinder := %u', [Self.s.cylinder[drive]]));
      LogInfo(Format('  sector   := %u', [Self.s.sector[drive]]));
//      end;

      raise_interrupt();
      bx_pc_system.set_DRQ(FLOPPY_DMA_CHAN, 0);
    end else
    begin // more data to transfer
      logical_sector := (Self.s.cylinder[drive] * 2 *
         Self.s.media[drive].sectors_per_track) +
         (Self.s.head[drive] *
         Self.s.media[drive].sectors_per_track) +
         (Self.s.sector[drive] - 1);
      floppy_xfer(drive, logical_sector * 512, Self.s.floppy_buffer, 512, FROM_FLOPPY);
    end;
  end;
end;

procedure TFloppyDrive.dma_read(data_byte: PBit8u);
var
  drive: Bit8u;
  logical_sector: Bit32u;
  i: unsigned;
begin
  // A DMA read is from Memory to I/O
  // We need to write the data_byte which was already transfered from memory
  // via DMA to I/O (write block to floppy)


  drive := Self.s.DOR and $03;
  if boolean(Self.s.pending_command = $4d) then
  begin // format track in progress
    dec(Self.s.format_count);
    case (3 - (Self.s.format_count and $03)) of
      0: Self.s.cylinder[drive] := data_byte^;
      1:
      begin
        if boolean(data_byte^ <> Self.s.head[drive]) then
          LogError(('head number does not match head field'));
      end;
      2: Self.s.sector[drive] := data_byte^;
      3:
      begin
        if boolean(data_byte^ <> 2) then
          LogError(('sector size code not 2'));
//            BX_DEBUG(Format('formatting cylinder %u head %u sector %u',
//               [Self.s.cylinder[drive], Self.s.head[drive],
//               Self.s.sector[drive]]));
        i := 0;
        while i < 512 do
        begin
          Self.s.floppy_buffer[i] := Self.s.format_fillbyte;
          Inc(i);
        end;
        logical_sector := (Self.s.cylinder[drive] * 2 *
           Self.s.media[drive].sectors_per_track) +
           (Self.s.head[drive] * Self.s.media[drive].sectors_per_track) +
           (Self.s.sector[drive] - 1);

        floppy_xfer(drive, logical_sector * 512, Self.s.floppy_buffer, 512, TO_FLOPPY);
      end;
    end;
    if boolean((Self.s.format_count = 0) or (bx_pc_system.TC <> 0)) then
    begin
      Self.s.format_count := 0;
      Self.s.pending_command := 0;
      Self.s.main_status_reg := FD_MS_MRQ or FD_MS_DIO or FD_MS_BUSY or (1 shl drive);
      Self.s.result_size := 7;
      Self.s.result_index := 0;
      Self.s.status_reg0 := (Self.s.head[drive] shl 2) or drive;
      Self.s.Result[0] := Self.s.status_reg0;
      Self.s.Result[1] := Self.s.status_reg1;
      Self.s.Result[2] := Self.s.status_reg2;
      // 4 result bytes are unused
      raise_interrupt();
      bx_pc_system.set_DRQ(FLOPPY_DMA_CHAN, 0);
    end;
    exit;
  end;

  Self.s.floppy_buffer[Self.s.floppy_buffer_index] := data_byte^;

  Inc(Self.s.floppy_buffer_index);

  if (Self.s.floppy_buffer_index >= 512) then
  begin
    logical_sector := (Self.s.cylinder[drive] * 2 * Self.s.media[drive].sectors_per_track) +
                      (Self.s.head[drive] * Self.s.media[drive].sectors_per_track) +
                      (Self.s.sector[drive] - 1);
    // по сути можно выкинуть, ибо на записи в файл проверяется флаг чтения.
    // но без этого не получится увидеть в ОС сообщение "диск защищен от записи".
    if boolean(Self.s.media[drive].write_protected) then
    begin
      // write protected error
      LogInfo(Format('tried to write disk %u, which is write-protected', [drive]));
      Self.s.result_size := 7;
      Self.s.result_index := 0;
      // ST0: IC1,0:=01  (abnormal termination: started execution but failed)
      Self.s.Result[0] := $40 or (Self.s.head[drive] shl 2) or drive;
      // ST1: DataError:=1, NDAT:=1, NotWritable:=1, NID:=1
      Self.s.Result[1] := $27; // 0010 0111
      // ST2: CRCE:=1, SERR:=1, BCYL:=1, NDAM:=1.
      Self.s.Result[2] := $31; // 0011 0001
      Self.s.Result[3] := Self.s.cylinder[drive];
      Self.s.Result[4] := Self.s.head[drive];
      Self.s.Result[5] := Self.s.sector[drive];
      Self.s.Result[6] := 2; // sector size := 512

      Self.s.pending_command := 0;
      Self.s.main_status_reg := FD_MS_MRQ or FD_MS_DIO or FD_MS_BUSY or (1 shl drive);
      raise_interrupt();
      exit;
    end;
    floppy_xfer(drive, logical_sector * 512, Self.s.floppy_buffer, 512, TO_FLOPPY);
    increment_sector(); // increment to next sector after writing current one
    Self.s.floppy_buffer_index := 0;
    if boolean(bx_pc_system.TC) then
    begin // Terminal Count line, done
      Self.s.pending_command := 0;
      Self.s.main_status_reg := FD_MS_MRQ or FD_MS_DIO or FD_MS_BUSY;
      Self.s.result_size := 7;
      Self.s.result_index := 0;
      Self.s.status_reg0 := $20 or (Self.s.head[drive] shl 2) or drive;
      Self.s.Result[0] := Self.s.status_reg0;
      Self.s.Result[1] := 0;
      Self.s.Result[2] := 0;
      Self.s.Result[3] := Self.s.cylinder[drive];
      Self.s.Result[4] := Self.s.head[drive];
      Self.s.Result[5] := Self.s.sector[drive];
      Self.s.Result[6] := 2;
//      if boolean(BX_DEBUG_FLOPPY) then
//      begin
      LogInfo(('>>WRITE DONE<<'));
      LogInfo(('AFTER'));
      LogInfo(Format('  drive    := %u', [drive]));
      LogInfo(Format('  head     := %u', [Self.s.head[drive]]));
      LogInfo(Format('  cylinder := %u', [Self.s.cylinder[drive]]));
      LogInfo(Format('  sector   := %u', [Self.s.sector[drive]]));
//      end;

      raise_interrupt();
      bx_pc_system.set_DRQ(FLOPPY_DMA_CHAN, 0);
    end;
  end; // if Self.s.floppy_buffer_index >= 512
end;

procedure TFloppyDrive.raise_interrupt;
begin
  bx_pic.raise_irq(6);
  Self.s.pending_irq := 1;
  Self.s.reset_sensei := 0;
end;

procedure TFloppyDrive.increment_sector;
var
  drive: Bit8u;
begin
  drive := Self.s.DOR and $03;
  // values after completion of data xfer
  // ??? calculation depends on base_count being multiple of 512
  Inc(Self.s.sector[drive]);
  if (Self.s.sector[drive] > Self.s.media[drive].sectors_per_track) then
  begin
    Self.s.sector[drive] := 1;
    if boolean(Self.s.multi_track) then
    begin
      Inc(Self.s.head[drive]);
      if (Self.s.head[drive] > 1) then
      begin
        Self.s.head[drive] := 0;
        Inc(Self.s.cylinder[drive]);
      end;
    end else
      Inc(Self.s.cylinder[drive]);

    if (Self.s.cylinder[drive] >= Self.s.media[drive].tracks) then
    begin
      // Set to 1 past last possible cylinder value.
      // I notice if I set it to tracks-1, prama linux won't boot.
      Self.s.cylinder[drive] := Self.s.media[drive].tracks;
      LogInfo(('increment_sector: clamping cylinder to max'));
    end;
  end;
end;

procedure TFloppyDrive.init(d: pbx_devices_c; cmos: pbx_cmos_c;
  const aConf: TWorkSpaceConfig);
var
   addr: unsigned;
   i, a, b: Integer;
begin
  a := -1;
  b := -1;
  for i := 0 to Pred(length(aConf.Devices)) do
  begin
    if (a = -1) and (aConf.Devices[i].DeviceType = dFDD) then
    begin
      Inc(a);
      Continue;
    end;
    if (a <> -1) and (aConf.Devices[i].DeviceType = dFDD) then
    begin
      Inc(b);
      Break;
    end;
  end;

  Self.devices := d;
  Self.devices^.register_irq(6, 'Floppy Drive');
  addr := $03F2;
  while addr <= $03F7 do
  begin
    Self.devices^.register_io_read_handler(Self, read_handler,
       addr, 'Floppy Drive');
    Self.devices^.register_io_write_handler(Self, write_handler,
       addr, 'Floppy Drive');
    Inc(Addr);
  end;

  cmos^.s.reg[$10] := $00; (* start out with: no drive 0, no drive 1 *)
  Self.s.num_supported_floppies := 0;
  //
  // Floppy A setup
  //
  Self.s.media[0].sectors_per_track := 0;
  Self.s.media[0].tracks            := 0;
  Self.s.media[0].heads             := 0;
  Self.s.media[0].sectors           := 0;
  Self.s.media[0].type_             := IfThen(a <> -1, BX_FLOPPY_1_44, BX_FLOPPY_NONE); //cfg.FloppyAType;
  Self.s.media[0].fd                := -1;
  Self.s.media_present[0]           := 0;

  case (Self.s.media[0].type_) of
    BX_FLOPPY_NONE:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $0f) or $00;
    BX_FLOPPY_1_2:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $0f) or $20;
    BX_FLOPPY_720K:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $0f) or $30;
    BX_FLOPPY_1_44:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $0f) or $40;
    BX_FLOPPY_2_88:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $0f) or $50;
  else
     LogPanic(('unknown floppya type'));
  end;

  if boolean(Self.s.media[0].type_ <> BX_FLOPPY_NONE) then
  begin
    Inc(Self.s.num_supported_floppies);
    if boolean(evaluate_media(Self.s.media[0].type_, PChar(aConf.Devices[a].DeviceFileName), @Self.s.media[0])) then
    begin
      Self.s.media_present[0] := 1;
      Self.s.media[0].write_protected := word(aConf.Devices[a].DeviceReadOnly);
    end;
  end;
  //
  // Floppy B setup
  //
  Self.s.media[1].sectors_per_track := 0;
  Self.s.media[1].tracks            := 0;
  Self.s.media[1].heads             := 0;
  Self.s.media[1].sectors           := 0;
  Self.s.media[1].type_             := IfThen(b <> -1, BX_FLOPPY_1_44, BX_FLOPPY_NONE); //cfg.FloppyBType;
  Self.s.media[1].fd                := -1;
  Self.s.media_present[1]           := 0;


  case ({BX_GET_FLOPPYB_TYPE}Self.s.media[1].type_) of
    BX_FLOPPY_NONE:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $f0) or $00;
    BX_FLOPPY_1_2:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $f0) or $02;
    BX_FLOPPY_720K:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $f0) or $03;
    BX_FLOPPY_1_44:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $f0) or $04;
    BX_FLOPPY_2_88:
       cmos^.s.reg[$10] := (cmos^.s.reg[$10] and $f0) or $05;
  else
    LogPanic(('unknown floppyb type'));
  end;

  if boolean(Self.s.media[1].type_{BX_GET_FLOPPYB_TYPE} <> BX_FLOPPY_NONE) then
  begin
    Inc(Self.s.num_supported_floppies);
    if boolean(evaluate_media(Self.s.media[1].type_, PChar(aConf.Devices[b].DeviceFileName), @Self.s.media[1])) then
    begin
      Self.s.media_present[1] := 1;
      Self.s.media[1].write_protected := word(aConf.Devices[b].DeviceReadOnly);
    end;
  end;

  (* CMOS Equipment Byte register *)
  if boolean(Self.s.num_supported_floppies > 0) then
    cmos^.s.reg[$14] := (cmos^.s.reg[$14] and $3e) or
       ((Self.s.num_supported_floppies - 1) shl 6) or 1
  else
    cmos^.s.reg[$14] := (cmos^.s.reg[$14] and $3e);

  Self.s.floppy_timer_index :=
    bx_pc_system.register_timer(@bx_floppy, timer_handler, 50000, 0,0);
end;

function TFloppyDrive.set_media_status(drive: unsigned;
  status: unsigned): unsigned;
var
  path: String;
  type_: unsigned;
begin

  // if setting to the current value, nothing to do
  if (status = Self.s.media_present[drive]) then
    Exit(status);

  if (status = 0) then
  begin
    // eject floppy
    if (Self.s.media[drive].fd >= 0) then
    begin
      FileClose(Self.s.media[drive].fd);
      Self.s.media[drive].fd := -1;
    end;
    Self.s.media_present[drive] := 0;
    {    if Boolean(drive = 0) then begin
    bx_options.floppya.Oinitial_status^.set(BX_EJECTED);
    end; else begin
    bx_options.floppyb.Oinitial_status^.set(BX_EJECTED);
    end;}
    Self.s.DIR := Self.s.DIR or $80; // disk changed line
    Exit(0);
  end else
  begin
    // insert floppy
    // странный код. разобраться!!!
    if (drive = 0) then   // фигня, т.к. работать будет только с флопешником "А"
    begin
      path := BX_FLOPPY_PATH;
      type_ := BX_FLOPPY_1_44;
    end else
    begin
      path := BX_FLOPPY_PATH;
      type_ := BX_FLOPPY_1_44;
    end;
    Self.s.media_present[drive] := 0;
    if boolean(evaluate_media(type_, PChar(path), @Self.s.media[drive])) then
    begin
//      Self.s.media_present[drive] := 0;
{      if Boolean(drive = 0) then begin
        bx_options.floppya.Oinitial_status^.set(BX_INSERTED);
      end; else begin
        bx_options.floppyb.Oinitial_status^.set(BX_INSERTED);
      end;}
      Self.s.DIR := Self.s.DIR or $80; // disk changed line
      Exit(1);
    end else
    begin
//      Self.s.media_present[drive] := 0;
{      if Boolean(drive = 0) then begin
        bx_options.floppya.Oinitial_status^.set(BX_EJECTED);
      end; else begin
        bx_options.floppyb.Oinitial_status^.set(BX_EJECTED);
      end;}
      Exit(0);
    end;
  end;
end;

function TFloppyDrive.get_media_status(drive: unsigned): unsigned;
begin
  Result := (Self.s.media_present[drive]);
end;

function TFloppyDrive.evaluate_media(type_: unsigned; path: PChar;
  media: PFloppy): word;
begin
  if boolean(type_ = BX_FLOPPY_NONE) then
    Exit(0);

   //If media file is already open, close it before reopening.
  if (media ^.fd >= 0) then
  begin
    FileClose(media^.fd);
    media^.fd := -1;
  end;

  // open media file (image file or device)
  media ^.write_protected := 0;

  media^.fd := FileOpen(Path,fmOpenReadWrite  or fmShareDenyWrite);

  if media^.fd = -1 then
    Exit(0);

  {if boolean((path[1] = ':') and (strlen(path) = 2)) then
  begin
  StrPCopy(sTemp, Format('\\.\%s', [path]));
  media^.fd := FileCreate(sTemp);
  end
  else
  begin
  media ^.fd := FileCreate(Path);
  end;

  if boolean(media ^.fd < 0) then
  begin
  BX_INFO('tried to open %s');
  // try opening the file read-only
  media ^.write_protected := 1;
  if boolean((path[1] = ':') and (strlen(path) = 2)) then
  begin
  StrPCopy(sTemp, Format('\\.\%s', [path]));
  media ^.fd := FileCreate(sTemp);
  end
  else
  begin
  media ^.fd := FileCreate(path);
  end;
  if boolean(media ^.fd < 0) then
  begin
  // failed to open read-only too
  BX_INFO('tried to open ');
  Result := 0;
  Exit;
  end;
  end;}

  media ^.sectors_per_track := 18;

  media ^.tracks := 80;

  media ^.heads := 2;

  media ^.sectors := media ^.heads * media ^.tracks * media ^.sectors_per_track;

  Result := 1; // success
end;

end.

