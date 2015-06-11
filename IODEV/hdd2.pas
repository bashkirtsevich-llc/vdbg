/////////////////////////////////////////////////////////////////////////
// $Id: harddrv.h,v 1.9 2002/02/03 20:49:44 vruppert Exp $
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
//  modify it under the terms of the GNU Lesser General Public
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

unit HDD;

interface

uses Config, iodev, cmos, cdrom;

const
   BX_CONCAT_MAX_IMAGES = 8;
   model_no :array[0..39] of char= 'Generic 1234                            ';
type
   sense_t = (
         SENSE_NONE = 0, SENSE_NOT_READY = 2, SENSE_ILLEGAL_REQUEST = 5,
         SENSE_UNIT_ATTENTION = 6);

   asc_t = (
      ASC_INV_FIELD_IN_CMD_PACKET = $24,
      ASC_MEDIUM_NOT_PRESENT = $3a,
      ASC_SAVING_PARAMETERS_NOT_SUPPORTED = $39,
      ASC_LOGICAL_BLOCK_OOR = $21);

//LOWLEVEL_CDROM = class;

pdevice_image_t=^device_image_t;
device_image_t=class
  public
      cylinders:unsigned;
      heads:unsigned;
      sectors:unsigned;
      // Open a image. Returns non-negative if successful.
      function open(const pathname:String):integer; virtual; abstract;

      // Close the image.
      procedure close;virtual; abstract;

      // Position ourselves. Return the resulting offset from the
      // beginning of the file.
      function lseek (offset:off_t;whence:Integer):Bit32u; virtual;abstract;

      // Read count bytes to the buffer buf. Return the number of
      // bytes read (count).
      function read (var buf:array_buffer_disk; count:size_t):ssize_t;virtual;abstract;

      // Write count bytes from buf. Return the number of bytes
      // written (count).
      function write (buf:array_buffer_disk; count:size_t):Bit32u;virtual;abstract;

   end;

default_image_t = class(device_image_t)
  public
      // Open a image. Returns non-negative if successful.
      function open (const pathname:String):integer;

      // Close the image.
      procedure close;

      // Position ourselves. Return the resulting offset from the
      // beginning of the file.
      function lseek (offset:off_t; whence:Integer):off_t; override;

      // Read count bytes to the buffer buf. Return the number of
      // bytes read (count).
      function read (var buf:array_buffer_disk; count:size_t):ssize_t; override;

      // Write count bytes from buf. Return the number of bytes
      // written (count).
      function write (buf:array_buffer_disk; count:size_t):ssize_t; override;

  private
      fd:file;

end;

{$if BX_SPLIT_HD_SUPPORT=1}
concat_image_t = class(device_image_t)
  public
      // Open a image. Returns non-negative if successful.
      function open (pathname:String):Integer;

      // Close the image.
      procedure close;

      // Position ourselves. Return the resulting offset from the
      // beginning of the file.
      function lseek (offset:off_t; whence:Integer):off_t;

      // Read count bytes to the buffer buf. Return the number of
      // bytes read (count).
      function read (buf:pointer;count:size_t):ssize_t;

      // Write count bytes from buf. Return the number of bytes
      // written (count).
      function write (buf:array_buffer_disk; count:size_t):ssize_t;

  private
      fd_table:array[0..BX_CONCAT_MAX_IMAGES] of integer;
      start_offset_table:array[0..BX_CONCAT_MAX_IMAGES] of ssize_t;
      length_table:array[0..BX_CONCAT_MAX_IMAGES] of ssize_t;
      maxfd:Integer;  // number of entries in tables that are valid
      seek_was_last_op:Integer;
      index:Integer;  // index into table
      fd:Integer;     // fd to use for reads and writes
      thismin, thismax:Integer; // byte offset boundary of this image
      procedure increment_string (str:String);

      // notice if anyone does sequential read or write without seek in between.
      // This can be supported pretty easily, but needs additional checks.
      // 0=something other than seek was last operation
      // 1=seek was last operation

      // the following variables tell which partial image file to use for
      // the next read and write.
end;
{$ifend} // BX_SPLIT_HD_SUPPORT 

t_beta = record
case integer of
   0:(
    sector_count:Bit8u;
    interrupt_reason:record
{$if BX_LITTLE_ENDIAN=1}
      c_d:unsigned{ : 1};
      i_o:unsigned{ : 1};
      rel:unsigned{ : 1};
      tag:unsigned{ : 5};
{$else}  // BX_BIG_ENDIAN
      tag:unsigned{ : 5};
      rel:unsigned{ : 1};
      i_o:unsigned{ : 1};
      c_d:unsigned{ : 1};
{$ifend}
    end;
    );
    end;

  t_alfa=record
  case integer of
  0:(
    cylinder_no:Bit16u;
    byte_count:Bit16u;
    );
  end;
controller_t = record
  status : record
    busy:bool;
    drive_ready:bool;
    write_fault:bool;
    seek_complete:bool;
    drq:bool;
    corrected_data:bool;
    index_pulse:bool;
    index_pulse_count:unsigned;
    err:Bool;
  end;
  error_register:Bit8u;
  // Bit8u    drive_select; this field was moved :^(
  head_no:Bit8u;
  beta:t_beta;
  sector_no:Bit8u;
  alfa:t_alfa;
  buffer:array_buffer_disk;
  buffer_index:unsigned;
  current_command:Bit16u;
  sectors_per_block:Bit8u;
  lba_mode:Bit8u;
  control : record
    reset:Bool;       // 0=normal, 1=reset controller
    disable_irq:Bool; // 0=allow irq, 1=disable irq
  end;
  reset_in_progress:Bit8u;
  features:Bit8u;
end;

sense_info_t = record
  sense_key:sense_t;
  information:record
    arr:array[0..4] of Bit8u;
  end;
  specific_inf : record
    arr:array[0..4] of Bit8u;
  end;
  key_spec : record
    arr:array[0..3] of Bit8u;
  end;
  fruc:Bit8u;
  asc:Bit8u;
  ascq:Bit8u;
end;

error_recovery_t = record
  data:array[0..8] of char;

  //error_recovery_t ();
  end;

function read_16bit(buf:puint8):uint16;
function read_32bit(buf:puint8):uint32;

type
cdrom_t = record
  ready:Bool;
  locked:Bool;
{$if LOWLEVEL_CDROM=1}
  cd:bx_cdrom;
{$ifend}
  capacity:uint32;
  next_lba:integer;
  remaining_blocks:integer;
  current : record
    error_recovery:error_recovery_t;
  end;
end;

atapi_t = record
  command:uint8;
  drq_bytes:integer;
  total_bytes_remaining:integer;
end;

//#define BX_SELECTED_HD BX_HD_THIS s[BX_HD_THIS drive_select]
//#define CDROM_SELECTED (BX_HD_THIS s[BX_HD_THIS drive_select].device_type == IDE_CDROM)
//#define DEVICE_TYPE_STRING ((CDROM_SELECTED) ? "CD-ROM" : "DISK")

device_type_t = (IDE_DISK, IDE_CDROM);

bx_hard_drive_c = class
public

  constructor Create;
  destructor Destroy;
  procedure   close_harddrive;
  procedure   init(d:pbx_devices_c; cmos:pbx_cmos_c);
  function get_cd_media_status:unsigned;
  function set_cd_media_status(status:unsigned):unsigned;

{$if BX_USE_HD_SMF=0}
  function read(address:Bit32u; io_len:unsigned):Bit32u;
  void   write(address:Bit32u; value:Bit32u; io_len:unsigned);
{$ifend}

  function read_handler(this_ptr:pointer; address:Bit32u; io_len:unsigned):Bit32u;
  procedure write_handler(this_ptr:pointer; address:Bit32u; value:Bit32u; io_len:unsigned);

public

  s : array[0..2] of record
    hard_drive:default_image_t;
    device_type:device_type_t;
    // 512 byte buffer for ID drive command
    // These words are stored in native word endian format, as
    // they are fetched and returned via a return(), so
    // there's no need to keep them in x86 endian format.
    id_drive:array[0..256] of Bit16u;

    controller:controller_t;
    cdrom:cdrom_t;
    sense:sense_info_t;
    atapi:atapi_t;

  end;

  drive_select:unsigned;

  devices:pbx_devices_c;
  function calculate_logical_address(var sector:bit32u):Bool;
  procedure increment_address();
  procedure identify_drive(drive:unsigned);
  procedure identify_ATAPI_drive(drive:unsigned);
  procedure command_aborted(command:unsigned);

  procedure init_send_atapi_command(command:Bit8u; req_length:integer; alloc_length:integer; lazy:bool = 0);
  procedure ready_to_send_atapi();
  procedure raise_interrupt();
  procedure atapi_cmd_error(sense_key:sense_t; asc:asc_t);
  procedure init_mode_sense_single(src:pointer; size:Integer);
  procedure atapi_cmd_nop();
end;

var
  bx_hard_drive:bx_hard_drive_c;

implementation

uses Service,SysUtils,pic;


const
TEST_READ_BEYOND_END = 0;
TEST_WRITE_BEYOND_END = 0;
{$if (TEST_READ_BEYOND_END<>0) or (TEST_WRITE_BEYOND_END<>0)}
{$message warn 'BEWARE: Dangerous options are enabled in harddrv.cc'}
{$message warn 'If you are not trying to provoke hard drive errors you should disable them right now'}
{$ifend}
// end of dangerous options.


INDEX_PULSE_CYCLE = 10;

PACKET_SIZE = 12;

mod_el_no:array[0..41] of char =
  'Generic 1234                            ';

max_multiple_sectors:unsigned  = 0; // was $3f
curr_multiple_sectors:unsigned = 0; // was $3f

// some packet handling macros

function Extract_field(arr:array of byte;byte_:byte;start:Integer;num_bits:byte):bit32u;
begin
  Result:=((arr[byte_] shr (start))  and ((1 shl (num_bits)) - 1));
end;

function get_packet_field(b:byte;s:Integer;n:byte):Bit32u;
begin
  Result:=EXTRACT_FIELD(bx_hard_drive.s[bx_hard_drive.drive_select].controller.buffer,b,s,n);
end;

function get_packet_byte(b:byte):uint8;
begin
  Result:=bx_hard_drive.s[bx_hard_drive.drive_select].controller.buffer[b];
end;

function get_packet_word(b:byte):uint16;
begin
  Result:=(bx_hard_drive.s[bx_hard_drive.drive_select].controller.buffer[b] shl 8) or (bx_hard_drive.s[0].controller.buffer[b+1]);
end;

//#define EXTRACT_FIELD(arr,byte,start,num_bits) (((arr)[(byte)] shr (start))  and ((1 shl (num_bits)) - 1))
//#define get_packet_field(b,s,n) (EXTRACT_FIELD((BX_SELECTED_CONTROLLER.buffer),(b),(s),(n)))
//#define get_packet_byte(b) (BX_SELECTED_CONTROLLER.buffer[(b)])
//#define get_packet_word(b) (((uint16)BX_SELECTED_CONTROLLER.buffer[(b)] shl 8)orBX_SELECTED_CONTROLLER.buffer[(b)+1])


//#define BX_CONTROLLER(a) (BX_HD_THIS s[(a)]).controller s[0]).controller
//#define BX_SELECTED_CONTROLLER (BX_CONTROLLER(BX_HD_THIS drive_select))

procedure WRITE_SECTOR_NUMBER(a:uint8);
var
 _a:uint8;
begin
  _a:=a;
  bx_hard_drive.s[0].controller.sector_no:=_a;
  bx_hard_drive.s[1].controller.sector_no:=_a;
end;

procedure WRITE_FEATURES(a:uint8);
var
  _a:uint8;
begin
  _a:=a;
  bx_hard_drive.s[0].controller.features:=_a;
  bx_hard_drive.s[1].controller.features:=_a;
end;

procedure WRITE_SECTOR_COUNT(a:uint8);
var
  _a:uint8;
begin
  _a:=a;
  bx_hard_drive.s[0].controller.beta.sector_count:=_a;
  bx_hard_drive.s[1].controller.beta.sector_count:=_a;
end;

procedure WRITE_CYLINDER_LOW(a:uint8);
var
  _a:uint8;
begin
  _a:=a;
  bx_hard_drive.s[0].controller.alfa.cylinder_no:=(bx_hard_drive.s[0].controller.alfa.cylinder_no and $ff00) or _a;
  bx_hard_drive.s[1].controller.alfa.cylinder_no:=(bx_hard_drive.s[1].controller.alfa.cylinder_no and $ff00) or _a;
end;

procedure WRITE_CYLINDER_HIGH(a:uint8);
var
  _a:uint8;
begin
  _a:=a;
  bx_hard_drive.s[0].controller.alfa.cylinder_no:=(_a shl 8) or (bx_hard_drive.s[0].controller.alfa.cylinder_no and $ff);
  bx_hard_drive.s[1].controller.alfa.cylinder_no:=(_a shl 8) or (bx_hard_drive.s[1].controller.alfa.cylinder_no and $ff);
end;

procedure WRITE_HEAD_NO(a:uint8);
var
  _a:uint8;
begin
  _a:=a;
  bx_hard_drive.s[0].controller.head_no:=_a;
  bx_hard_drive.s[1].controller.head_no:=_a;
end;

procedure WRITE_LBA_MODE(a:uint8);
var
  _a:uint8;
begin
  _a:=a;
  bx_hard_drive.s[0].controller.lba_mode:=_a;
  bx_hard_drive.s[1].controller.lba_mode:=_a;
end;

//static unsigned im_here := 0;

constructor bx_hard_drive_c.Create;
begin
      s[0].hard_drive :=  NULL;
      s[1].hard_drive :=  NULL;

{$if BX_SPLIT_HD_SUPPORT=1}
      // use new concatenated image object
      s[0].hard_drive := concat_image_t.Create;
      s[1].hard_drive := concat_image_t.Create;
{$else}
      s[0].hard_drive := default_image_t.Create;
      s[1].hard_drive := default_image_t.Create;
{$ifend}

end;

destructor bx_hard_drive_c.Destroy;
begin
  if Boolean( s[0].hard_drive <> NULL ) then      (* DT 17.12.2001 21:55 *)
  begin
    s[0].hard_drive.Free;
    s[0].hard_drive :=  NULL;
  end;
  if Boolean( s[1].hard_drive <> NULL ) then
  begin
    s[1].hard_drive.Free;
    s[1].hard_drive :=  NULL;        (* DT 17.12.2001 21:56 *)
  end;
end;

procedure bx_hard_drive_c.init(d:pbx_devices_c;cmos:pbx_cmos_c);
var
  addr,id:unsigned;
begin
  self.devices := d;
	BX_DEBUG(('Init $Id: harddrv.cc,v 1.52 2002/03/25 01:47:13 cbothamy Exp $'));

  (* HARD DRIVE 0 *)

  self.devices^.register_irq(14, 'Hard Drive 0');
  addr:=$01F0;
  while addr<=$01F7 do begin
    self.devices^.register_io_read_handler(self, read_handler,
                                        addr, 'Hard Drive 0');
    self.devices^.register_io_write_handler(self, write_handler,
                                        addr, 'Hard Drive 0');
    inc(addr);                                    
    end;
{$if false} //
  // self would be necessary to make the second HD master on the
  // second controller, using $170-$177 and irq15.  But it currently
  // works as second disk on the first IDE controller, so self code
  // is not needed.
  self.devices^.register_irq(15, 'Hard Drive 1');
  for (unsigned addr:=$0170; addr<=$0177; addr++) then begin
    self.devices^.register_io_read_handler(self, read_handler,
                                        addr, 'Hard Drive 1');
    self.devices^.register_io_write_handler(self, write_handler,
                                        addr, 'Hard Drive 1');
    end;
{$ifend}

  self.drive_select := 0;

  self.s[0].hard_drive.cylinders := HDD_NUM_CYL;
  self.s[0].hard_drive.heads     := HDD_NUM_HEADS;
  self.s[0].hard_drive.sectors   := HDD_SECTOR_PER_TRACK;
  self.s[0].device_type           := IDE_DISK;
  self.s[1].hard_drive.cylinders := HDD_NUM_CYL;
  self.s[1].hard_drive.heads     := HDD_NUM_HEADS;
  self.s[1].hard_drive.sectors   := HDD_SECTOR_PER_TRACK;
  self.s[1].device_type           := IDE_DISK;

  if Boolean(BX_CDROM_PRESENT) then
    begin
      BX_DEBUG(( 'Experimental CDROM on target 1' ));
      self.s[1].device_type := IDE_CDROM;
      self.s[1].cdrom.locked := 0;
      self.s[1].sense.sense_key := SENSE_NONE;
      self.s[1].sense.asc := 0;
      self.s[1].sense.ascq := 0;
      // Check bit fields
      bx_hard_drive.s[1].controller.beta.sector_count := 1;
      bx_hard_drive.s[1].controller.beta.interrupt_reason.c_d := 1;
      if Boolean(bx_hard_drive.s[1].controller.beta.sector_count <> $01) then
            BX_PANIC(('interrupt reason bit field error'));

      bx_hard_drive.s[1].controller.beta.sector_count := 2;
      bx_hard_drive.s[1].controller.beta.interrupt_reason.i_o := 1;
      if Boolean(bx_hard_drive.s[1].controller.beta.sector_count <> $02) then
            BX_PANIC(('interrupt reason bit field error'));

      bx_hard_drive.s[1].controller.beta.sector_count := 4;
      bx_hard_drive.s[1].controller.beta.interrupt_reason.rel := 1;
      if Boolean(bx_hard_drive.s[1].controller.beta.sector_count <> $04) then
            BX_PANIC(('interrupt reason bit field error'));

      bx_hard_drive.s[1].controller.beta.sector_count := $18;
      bx_hard_drive.s[1].controller.beta.interrupt_reason.tag := 3;
      if Boolean(bx_hard_drive.s[1].controller.beta.sector_count <> $18) then
            BX_PANIC(('interrupt reason bit field error'));
      bx_hard_drive.s[1].controller.beta.sector_count := 0;

      // allocate low level driver

    {$if LOWLEVEL_CDROM=1}
      self.s[1].cdrom.cd := bx_cdrom.Init(BX_CD_FILEPATH);
    {$ifend}

    {$if LOWLEVEL_CDROM=1}
      if Boolean(BX_CD_STATE = true) then begin
            if Boolean(self.s[1].cdrom.cd.insert_cdrom('c:\w95.iso')) then begin
            BX_INFO(( 'Media present in CD-ROM drive'));
            self.s[1].cdrom.ready := 1;
            self.s[1].cdrom.capacity := self.s[1].cdrom.cd.capacity();
            end else begin
            BX_INFO(( 'Could not locate CD-ROM, continuing with media not present'));
            self.s[1].cdrom.ready := 0;
            //bx_options.cdromd.Oinserted^.set(BX_EJECTED);
            end;
      end else begin
    {$ifend}
            BX_INFO(( 'Media not present in CD-ROM drive' ));
            self.s[1].cdrom.ready := 0;
    {$if LOWLEVEL_CDROM=1}
      end;
    {$ifend}
  end;
  (* open hard drive image file *)
  if Boolean(BX_C_PRESENT) then begin
	if Boolean(self.s[0].hard_drive.open(HDD_FILE_DISK) < 0) then begin
	      BX_PANIC('could not open hard drive image file');
	end;
	//BX_INFO(('hd0: '%s'',bx_options.diskc.Opath^.getptr ()));
  end;
{  if Boolean(bx_options.diskd.Opresent^.get () @ and !bx_options.cdromd.Opresent^.get ()) then begin
	if Boolean((self.s[1].hard_drive^.open(bx_options.diskd.Opath^.getptr ())) < 0) then begin
	      BX_PANIC(('could not open hard drive image file '%s'',
		       bx_options.diskd.Opath^.getptr ()));
	end;
	BX_INFO(('hd1: '%s'',bx_options.diskd.Opath^.getptr()));
  end;}

  // generate CMOS values for hard drive if not using a CMOS image

    if Boolean(BX_C_PRESENT) then begin
      // Flag drive type as Fh, use extended CMOS location as real type
      cmos^.s.reg[$12] := (cmos^.s.reg[$12]  and $0f) or $f0;
      cmos^.s.reg[$19] := 47; // user definable type
      // AMI BIOS: 1st hard disk #cyl low byte
      cmos^.s.reg[$1b] := (HDD_NUM_CYL{981} and $00ff);
      // AMI BIOS: 1st hard disk #cyl high byte
      cmos^.s.reg[$1c] := (HDD_NUM_CYL {981} and $ff00) shr 8;
      // AMI BIOS: 1st hard disk #heads
      cmos^.s.reg[$1d] := HDD_NUM_HEADS{16};
      // AMI BIOS: 1st hard disk write precompensation cylinder, low byte
      cmos^.s.reg[$1e] := $ff; // -1
      // AMI BIOS: 1st hard disk write precompensation cylinder, high byte
      cmos^.s.reg[$1f] := $ff; // -1
      // AMI BIOS: 1st hard disk control byte
      cmos^.s.reg[$20] := $c0 or (Word(HDD_NUM_HEADS > 8) shl 3);
      // AMI BIOS: 1st hard disk landing zone, low byte
      cmos^.s.reg[$21] := cmos^.s.reg[$1b];
      // AMI BIOS: 1st hard disk landing zone, high byte
      cmos^.s.reg[$22] := cmos^.s.reg[$1c];
      // AMI BIOS: 1st hard disk sectors/track
      cmos^.s.reg[$23] := HDD_SECTOR_PER_TRACK{63};
    end;

    //set up cmos for second hard drive
{    if Boolean(bx_options.diskd.Opresent^.get () @ and !bx_options.cdromd.Opresent^.get ()) then begin
      BX_DEBUG(('1: I will put $f into the second hard disk field'));
      // fill in lower 4 bits of $12 for second HD
      cmos^.s.reg[$12] := (cmos^.s.reg[$12]  and $f0)or$0f;
      cmos^.s.reg[$1a] := 47; // user definable type
      // AMI BIOS: 2nd hard disk #cyl low byte
      cmos^.s.reg[$24] := (bx_options.diskd.Ocylinders^.get ()  and $00ff);
      // AMI BIOS: 2nd hard disk #cyl high byte
      cmos^.s.reg[$25] := (bx_options.diskd.Ocylinders^.get ()  and $ff00) shr 8;
      // AMI BIOS: 2nd hard disk #heads
      cmos^.s.reg[$26] := (bx_options.diskd.Oheads^.get ());
      // AMI BIOS: 2nd hard disk write precompensation cylinder, low byte
      cmos^.s.reg[$27] := $ff; // -1
      // AMI BIOS: 2nd hard disk write precompensation cylinder, high byte
      cmos^.s.reg[$28] := $ff; // -1
      // AMI BIOS: 2nd hard disk, $80 if heads>8
      cmos^.s.reg[$29] := (bx_options.diskd.Oheads^.get () > 8) ? $80 : $00;
      // AMI BIOS: 2nd hard disk landing zone, low byte
      cmos^.s.reg[$2a] := cmos^.s.reg[$24];
      // AMI BIOS: 2nd hard disk landing zone, high byte
      cmos^.s.reg[$2b] := cmos^.s.reg[$25];
      // AMI BIOS: 2nd hard disk sectors/track
      cmos^.s.reg[$2c] := bx_options.diskd.Ospt^.get ();
    end;}


    // Set the 'non-extended' boot device. self will default to DISKC if cdrom
    if Boolean( BX_BOOT_DRIVE <> BX_BOOT_FLOPPYA) then begin
      // system boot sequence C:, A:
      cmos^.s.reg[$2d] := cmos^.s.reg[$2d] and $df;
      end
  else begin // 'a'
      // system boot sequence A:, C:
      cmos^.s.reg[$2d]:=cmos^.s.reg[$2d] or $20;
      end;

    // Set the 'extended' boot device, byte $3D (needed for cdrom booting)
    if Boolean( BX_BOOT_DRIVE = BX_BOOT_FLOPPYA) then begin
      // system boot sequence A:
      cmos^.s.reg[$3d] := $01;
      end
  else if Boolean( BX_BOOT_DRIVE = BX_BOOT_DISKC) then begin
      // system boot sequence C:
      cmos^.s.reg[$3d] := $02;
      end
  else if Boolean( BX_BOOT_DRIVE = BX_BOOT_CDROM) then begin 
      // system boot sequence cdrom
      cmos^.s.reg[$3d] := $03;
      end;

  //switch (stat_buf.st_size) then begin
  //  end;

  id:=0;
  while id < 2 do
  begin
    bx_hard_drive.s[id].controller.status.busy           := 0;
    bx_hard_drive.s[id].controller.status.drive_ready    := 1;
    bx_hard_drive.s[id].controller.status.write_fault    := 0;
    bx_hard_drive.s[id].controller.status.seek_complete  := 1;
    bx_hard_drive.s[id].controller.status.drq            := 0;
    bx_hard_drive.s[id].controller.status.corrected_data := 0;
    bx_hard_drive.s[id].controller.status.index_pulse    := 0;
    bx_hard_drive.s[id].controller.status.index_pulse_count := 0;
    bx_hard_drive.s[id].controller.status.err            := 0;

    bx_hard_drive.s[id].controller.error_register := $01; // diagnostic code: no error
    bx_hard_drive.s[id].controller.head_no        := 0;
    bx_hard_drive.s[id].controller.beta.sector_count   := 1;
    bx_hard_drive.s[id].controller.sector_no      := 1;
    bx_hard_drive.s[id].controller.alfa.cylinder_no    := 0;
    bx_hard_drive.s[id].controller.current_command := $00;
    bx_hard_drive.s[id].controller.buffer_index := 0;

    bx_hard_drive.s[id].controller.control.reset       := 0;
    bx_hard_drive.s[id].controller.control.disable_irq := 0;

    bx_hard_drive.s[id].controller.reset_in_progress   := 0;

    bx_hard_drive.s[id].controller.sectors_per_block   := $80;
    bx_hard_drive.s[id].controller.lba_mode            := 0;

    bx_hard_drive.s[id].controller.features            := 0;
    Inc(id);
  end;
end;


{#define GOTO_RETURN_VALUE  if(io_len=4)begin\
                             goto return_value32;\
                             end;\
                           else if(io_len=2)begin\
                             value16:=(Bit16u)value32;\
                             goto return_value16;\
                             end;\
                           elsebegin\
                             value8:=(Bit8u)value32;\
                             goto return_value8;\
                             end;}
                           

  // static IO port read callback handler
  // redirects to non-static class handler to aprocedure virtual functions

function bx_hard_drive_c.read_handler(this_ptr:pointer; address:Bit32u; io_len:unsigned):Bit32u;
var
  value8:Bit8u;
  value16:Bit16u;
  value32:Bit32u;
  logical_sector:Bit32u;
  ret:integer;
  index:unsigned;
  label return_value32,return_value16,return_value8;
begin

  if Boolean((io_len=2) and (address<>$1f0)) then begin
    BX_PANIC(Format('non-byte IO read to %04x',[address]));
    end;

  case (address) of
    $1f0: // hard disk data (16bit)
      begin
      if Boolean(bx_hard_drive.s[0].controller.status.drq = 0) then begin
	    BX_PANIC(Format('IO read(1f0h) with drq = 0: last command was %02xh',[bx_hard_drive.s[0].controller.current_command]));
      end;
      BX_DEBUG(Format('IO read(1f0h): current command is %02xh',[bx_hard_drive.s[0].controller.current_command]));
      case (bx_hard_drive.s[0].controller.current_command) of
        $20, // READ SECTORS, with retries
        $21: // READ SECTORS, without retries
          begin
            if Boolean(io_len = 1) then begin
              BX_PANIC(Format('byte IO read from %04x',[address]));
              end;
          if Boolean(bx_hard_drive.s[0].controller.buffer_index >= 512) then
            BX_PANIC(('IO read(1f0): buffer_index >= 512'));

          value32 := 0;
          case(io_len) of
            4:
              begin
                value32 := value32 or (bx_hard_drive.s[0].controller.buffer[bx_hard_drive.s[0].controller.buffer_index+3] shl 24);
                value32 := value32 or (bx_hard_drive.s[0].controller.buffer[bx_hard_drive.s[0].controller.buffer_index+2] shl 16);
              end;
            2:
              begin
                value32 := value32 or (bx_hard_drive.s[0].controller.buffer[bx_hard_drive.s[0].controller.buffer_index+1] shl 8);
                value32 := value32 or bx_hard_drive.s[0].controller.buffer[bx_hard_drive.s[0].controller.buffer_index];
              end;
            end;

          bx_hard_drive.s[0].controller.buffer_index:=bx_hard_drive.s[0].controller.buffer_index + io_len;

          // if buffer completely read
          if Boolean(bx_hard_drive.s[0].controller.buffer_index >= 512) then begin
            // update sector count, sector number, cylinder,
            // drive, head, status
            // if there are more sectors, read next one in...
            //
            bx_hard_drive.s[0].controller.buffer_index := 0;

	    increment_address();

            bx_hard_drive.s[0].controller.status.busy := 0;
            bx_hard_drive.s[0].controller.status.drive_ready := 1;
            bx_hard_drive.s[0].controller.status.write_fault := 0;
            if Boolean(BX_NEW_HDD_SUPPORT) then
              bx_hard_drive.s[0].controller.status.seek_complete := 1
            else
              bx_hard_drive.s[0].controller.status.seek_complete := 0;
            bx_hard_drive.s[0].controller.status.corrected_data := 0;
            bx_hard_drive.s[0].controller.status.err := 0;

            if Boolean(bx_hard_drive.s[0].controller.beta.sector_count=0) then begin
              bx_hard_drive.s[0].controller.status.drq := 0;
              end
            else begin (* read next one into controller buffer *)

              bx_hard_drive.s[0].controller.status.drq := 1;
              bx_hard_drive.s[0].controller.status.seek_complete := 1;

{$if TEST_READ_BEYOND_END=1}
	      bx_hard_drive.s[0].controller.alfa.cylinder_no +:= 100000;
{$ifend}
	      if Boolean(calculate_logical_address(logical_sector)=0) then begin
	        BX_ERROR(Format('multi-sector read reached invalid sector %u, aborting',[logical_sector]));
      		command_aborted (bx_hard_drive.s[0].controller.current_command);
          if(io_len=4) then
            begin
               goto return_value32;
            end
          else if(io_len=2) then begin
            value16:=Bit16u(value32);
            goto return_value16;
          end
          else begin
            value8:=Bit8u(value32);
            goto return_value8;
          end;
 	      end;
	      ret := s[self.drive_select].hard_drive.lseek(logical_sector * 512, 0);
              if Boolean(ret < 0) then begin
                BX_ERROR(('could not lseek() hard drive image file'));
		command_aborted (bx_hard_drive.s[0].controller.current_command);
          if(io_len=4) then
            begin
               goto return_value32;
            end
          else if(io_len=2) then begin
            value16:=Bit16u(value32);
            goto return_value16;
          end
          else begin
            value8:=Bit8u(value32);
            goto return_value8;
          end;
	      end;
	      ret := s[self.drive_select].hard_drive.read(bx_hard_drive.s[0].controller.buffer, 512);
              if Boolean(ret < 512) then begin
                BX_ERROR(Format('logical sector was %u',[logical_sector]));
                BX_ERROR(Format('could not read() hard drive image file at byte %d',[logical_sector*512]));
		command_aborted (bx_hard_drive.s[0].controller.current_command);
          if(io_len=4) then
            begin
               goto return_value32;
            end
          else if(io_len=2) then begin
            value16:=Bit16u(value32);
            goto return_value16;
          end
          else begin
            value8:=Bit8u(value32);
            goto return_value8;
          end;
	      end;

              bx_hard_drive.s[0].controller.buffer_index := 0;
	      raise_interrupt();
	    end;
	  end;
          if(io_len=4) then
            begin
               goto return_value32;
            end
          else if(io_len=2) then begin
            value16:=Bit16u(value32);
            goto return_value16;
          end
          else begin
            value8:=Bit8u(value32);
            goto return_value8;
          end;
     end;

  $ec,    // IDENTIFY DEVICE
	$a1:
    begin
          if Boolean(BX_NEW_HDD_SUPPORT) then begin

            bx_hard_drive.s[0].controller.status.busy := 0;
            bx_hard_drive.s[0].controller.status.drive_ready := 1;
            bx_hard_drive.s[0].controller.status.write_fault := 0;
            bx_hard_drive.s[0].controller.status.seek_complete := 1;
            bx_hard_drive.s[0].controller.status.corrected_data := 0;
            bx_hard_drive.s[0].controller.status.err := 0;

            index := bx_hard_drive.s[0].controller.buffer_index;
            value32 := bx_hard_drive.s[0].controller.buffer[index];
            inc(index);
            if Boolean(io_len >= 2) then begin
              value32 := value32 or (bx_hard_drive.s[0].controller.buffer[index] shl 8);
              Inc(index);
              end;
            if Boolean(io_len = 4) then begin
              value32 := value32 or (bx_hard_drive.s[0].controller.buffer[index] shl 16);
              value32 := value32 or (bx_hard_drive.s[0].controller.buffer[index+1] shl 24);
              index := index + 2;
              end;
            bx_hard_drive.s[0].controller.buffer_index := index;

            if Boolean(bx_hard_drive.s[0].controller.buffer_index >= 512) then begin
              bx_hard_drive.s[0].controller.status.drq := 0;
            end;
          if(io_len=4) then
            begin
               goto return_value32;
            end
          else if(io_len=2) then begin
            value16:=Bit16u(value32);
            goto return_value16;
          end
          else begin
            value8:=Bit8u(value32);
            goto return_value8;
          end;
        end
          else
            BX_PANIC(Format('IO read(1f0h): current command is %02xh',[ bx_hard_drive.s[0].controller.current_command]));
         end;
	    $a0: begin
		  index := bx_hard_drive.s[0].controller.buffer_index;

		  // Load block if necessary
		  if Boolean(index >= 2048) then begin
			if Boolean(index > 2048) then
			      BX_PANIC(('index > 2048'));
			case (s[self.drive_select].atapi.command) of
			      $28, // read (10)
			      $a8: // read (12)
			      begin
      {$if LOWLEVEL_CDROM=1}
            s[self.drive_select].cdrom.cd.read_block(@s[0].controller.buffer,s[self.drive_select].cdrom.next_lba);
            s[self.drive_select].cdrom.next_lba:=s[self.drive_select].cdrom.next_lba + 1;
				    s[self.drive_select].cdrom.remaining_blocks:=s[self.drive_select].cdrom.remaining_blocks-1;

				    {if Boolean(bx_dbg.disk or (CDROM_SELECTED @ and bx_dbg.cdrom))
  					  if Boolean(!s[self.drive_select].cdrom.remaining_blocks)
	    					BX_INFO(('Last READ block loaded beginCDROMend;'));
		  			  else
			     			BX_INFO(('READ block loaded (%d remaining) then beginCDROMend;',
					  s[self.drive_select].cdrom.remaining_blocks));}

				    // one block transfered
				    s[self.drive_select].atapi.drq_bytes := s[self.drive_select].atapi.drq_bytes - 2048;
				    s[self.drive_select].atapi.total_bytes_remaining := s[self.drive_select].atapi.total_bytes_remaining -
             2048;
				    index := 0;
{$else}
				    BX_PANIC(('Read with no LOWLEVEL_CDROM'));
{$ifend}
				    end;

			      //default: // no need to load a new block
				    //break;
			end;

		  value32 := bx_hard_drive.s[0].controller.buffer[index];
		  inc(index);
		  if Boolean(io_len >= 2) then begin
			value32 := value32 or (bx_hard_drive.s[0].controller.buffer[index] shl 8);
			inc(index);
		  end;
		  if Boolean(io_len = 4) then begin
			value32 := value32 or (bx_hard_drive.s[0].controller.buffer[index] shl 16);
			value32 := value32 or  (bx_hard_drive.s[0].controller.buffer[index+1] shl 24);
			Inc(index,2);
		  end;
		  bx_hard_drive.s[0].controller.buffer_index := index;

		  if Boolean(bx_hard_drive.s[0].controller.buffer_index >= s[self.drive_select].atapi.drq_bytes) then begin
			bx_hard_drive.s[0].controller.status.drq := 0;

			s[self.drive_select].atapi.total_bytes_remaining :=
        s[self.drive_select].atapi.total_bytes_remaining - s[self.drive_select].atapi.drq_bytes;

			if Boolean(s[self.drive_select].atapi.total_bytes_remaining > 0) then begin
			      // one or more blocks remaining (works only for single block commands)
			      bx_hard_drive.s[0].controller.beta.interrupt_reason.i_o := 1;
			      bx_hard_drive.s[0].controller.status.busy := 0;
			      bx_hard_drive.s[0].controller.status.drq := 1;
			      bx_hard_drive.s[0].controller.beta.interrupt_reason.c_d := 0;

			      // set new byte count if last block
			      if Boolean(s[self.drive_select].atapi.total_bytes_remaining < bx_hard_drive.s[0].controller.alfa.byte_count) then begin
				    bx_hard_drive.s[0].controller.alfa.byte_count := s[self.drive_select].atapi.total_bytes_remaining;
			      end;
			      s[self.drive_select].atapi.drq_bytes :=
              s[self.drive_select].atapi.drq_bytes + bx_hard_drive.s[0].controller.alfa.byte_count;

			      raise_interrupt();
			end else begin
			      // all bytes read
			      bx_hard_drive.s[0].controller.beta.interrupt_reason.i_o := 1;
			      bx_hard_drive.s[0].controller.beta.interrupt_reason.c_d := 1;
			      bx_hard_drive.s[0].controller.status.drive_ready := 1;
			      bx_hard_drive.s[0].controller.beta.interrupt_reason.rel := 0;
			      bx_hard_drive.s[0].controller.status.busy := 0;
			      bx_hard_drive.s[0].controller.status.drq := 0;
			      bx_hard_drive.s[0].controller.status.err := 0;
			      
			      raise_interrupt();
			end;
		  end;
          if(io_len=4) then
            begin
               goto return_value32;
            end
          else if(io_len=2) then begin
            value16:=Bit16u(value32);
            goto return_value16;
          end
          else begin
            value8:=Bit8u(value32);
            goto return_value8;
          end;
		  end;
end;
	// List all the read operations that are defined in the ATA/ATAPI spec
	// that we don't support.  Commands that are listed here will cause a
	// BX_ERROR, which is non-fatal, and the command will be aborted.

	$08:begin BX_ERROR(('read cmd $08 (DEVICE RESET) not supported')); command_aborted($08); end;
	$10:begin BX_ERROR(('read cmd $10 (RECALIBRATE) not supported')); command_aborted($10); end;
	$22:begin BX_ERROR(('read cmd $22 (READ LONG) not supported')); command_aborted($22); end;
	$23:begin BX_ERROR(('read cmd $23 (READ LONG NO RETRY) not supported')); command_aborted($23); end;
	$24:begin BX_ERROR(('read cmd $24 (READ SECTORS EXT) not supported')); command_aborted($24); end;
	$25:begin BX_ERROR(('read cmd $25 (READ DMA EXT) not supported')); command_aborted($25); end;
	$26:begin BX_ERROR(('read cmd $26 (READ DMA QUEUED EXT) not supported')); command_aborted($26); end;
	$27:begin BX_ERROR(('read cmd $27 (READ NATIVE MAX ADDRESS EXT) not supported')); command_aborted($27); end;
	$29:begin BX_ERROR(('read cmd $29 (READ MULTIPLE EXT) not supported')); command_aborted($29); end;
	$2A:begin BX_ERROR(('read cmd $2A (READ STREAM DMA) not supported')); command_aborted($2A); end;
	$2B:begin BX_ERROR(('read cmd $2B (READ STREAM PIO) not supported')); command_aborted($2B); end;
	$2F:begin BX_ERROR(('read cmd $2F (READ LOG EXT) not supported')); command_aborted($2F); end;
	$30:begin BX_ERROR(('read cmd $30 (WRITE SECTORS) not supported')); command_aborted($30); end;
	$31:begin BX_ERROR(('read cmd $31 (WRITE SECTORS NO RETRY) not supported')); command_aborted($31); end;
	$32:begin BX_ERROR(('read cmd $32 (WRITE LONG) not supported')); command_aborted($32); end;
	$33:begin BX_ERROR(('read cmd $33 (WRITE LONG NO RETRY) not supported')); command_aborted($33); end;
	$34:begin BX_ERROR(('read cmd $34 (WRITE SECTORS EXT) not supported')); command_aborted($34); end;
	$35:begin BX_ERROR(('read cmd $35 (WRITE DMA EXT) not supported')); command_aborted($35); end;
	$36:begin BX_ERROR(('read cmd $36 (WRITE DMA QUEUED EXT) not supported')); command_aborted($36); end;
	$37:begin BX_ERROR(('read cmd $37 (SET MAX ADDRESS EXT) not supported')); command_aborted($37); end;
	$38:begin BX_ERROR(('read cmd $38 (CFA WRITE SECTORS W/OUT ERASE) not supported')); command_aborted($38); end;
	$39:begin BX_ERROR(('read cmd $39 (WRITE MULTIPLE EXT) not supported')); command_aborted($39); end;
	$3A:begin BX_ERROR(('read cmd $3A (WRITE STREAM DMA) not supported')); command_aborted($3A); end;
	$3B:begin BX_ERROR(('read cmd $3B (WRITE STREAM PIO) not supported')); command_aborted($3B); end;
	$3F:begin BX_ERROR(('read cmd $3F (WRITE LOG EXT) not supported')); command_aborted($3F); end;
	$40:begin BX_ERROR(('read cmd $40 (READ VERIFY SECTORS) not supported')); command_aborted($40); end;
	$41:begin BX_ERROR(('read cmd $41 (READ VERIFY SECTORS NO RETRY) not supported')); command_aborted($41); end;
	$42:begin BX_ERROR(('read cmd $42 (READ VERIFY SECTORS EXT) not supported')); command_aborted($42); end;
	$50:begin BX_ERROR(('read cmd $50 (FORMAT TRACK) not supported')); command_aborted($50); end;
	$51:begin BX_ERROR(('read cmd $51 (CONFIGURE STREAM) not supported')); command_aborted($51); end;
	$70:begin BX_ERROR(('read cmd $70 (SEEK) not supported')); command_aborted($70); end;
	$87:begin BX_ERROR(('read cmd $87 (CFA TRANSLATE SECTOR) not supported')); command_aborted($87); end;
	$90:begin BX_ERROR(('read cmd $90 (EXECUTE DEVICE DIAGNOSTIC) not supported')); command_aborted($90); end;
	$91:begin BX_ERROR(('read cmd $91 (INITIALIZE DEVICE PARAMETERS) not supported')); command_aborted($91); end;
	$92:begin BX_ERROR(('read cmd $92 (DOWNLOAD MICROCODE) not supported')); command_aborted($92); end;
	$94:begin BX_ERROR(('read cmd $94 (STANDBY IMMEDIATE) not supported')); command_aborted($94); end;
	$95:begin BX_ERROR(('read cmd $95 (IDLE IMMEDIATE) not supported')); command_aborted($95); end;
	$96:begin BX_ERROR(('read cmd $96 (STANDBY) not supported')); command_aborted($96); end;
	$97:begin BX_ERROR(('read cmd $97 (IDLE) not supported')); command_aborted($97); end;
	$98:begin BX_ERROR(('read cmd $98 (CHECK POWER mod_E) not supported')); command_aborted($98); end;
	$99:begin BX_ERROR(('read cmd $99 (SLEEP) not supported')); command_aborted($99); end;
	$A2:begin BX_ERROR(('read cmd $A2 (SERVICE) not supported')); command_aborted($A2); end;
	$B0:begin BX_ERROR(('read cmd $B0 (SMART DISABLE OPERATIONS) not supported')); command_aborted($B0); end;
	$B1:begin BX_ERROR(('read cmd $B1 (DEVICE CONFIGURATION FREEZE LOCK) not supported')); command_aborted($B1); end;
	$C0:begin BX_ERROR(('read cmd $C0 (CFA ERASE SECTORS) not supported')); command_aborted($C0); end;
	$C4:begin BX_ERROR(('read cmd $C4 (READ MULTIPLE) not supported')); command_aborted($C4); end;
	$C5:begin BX_ERROR(('read cmd $C5 (WRITE MULTIPLE) not supported')); command_aborted($C5); end;
	$C6:begin BX_ERROR(('read cmd $C6 (SET MULTIPLE mod_E) not supported')); command_aborted($C6); end;
	$C7:begin BX_ERROR(('read cmd $C7 (READ DMA QUEUED) not supported')); command_aborted($C7); end;
	$C8:begin BX_ERROR(('read cmd $C8 (READ DMA) not supported')); command_aborted($C8); end;
	$C9:begin BX_ERROR(('read cmd $C9 (READ DMA NO RETRY) not supported')); command_aborted($C9); end;
	$CA:begin BX_ERROR(('read cmd $CA (WRITE DMA) not supported')); command_aborted($CA); end;
	$CC:begin BX_ERROR(('read cmd $CC (WRITE DMA QUEUED) not supported')); command_aborted($CC); end;
	$CD:begin BX_ERROR(('read cmd $CD (CFA WRITE MULTIPLE W/OUT ERASE) not supported')); command_aborted($CD); end;
	$D1:begin BX_ERROR(('read cmd $D1 (CHECK MEDIA CARD TYPE) not supported')); command_aborted($D1); end;
	$DA:begin BX_ERROR(('read cmd $DA (GET MEDIA STATUS) not supported')); command_aborted($DA); end;
	$DE:begin BX_ERROR(('read cmd $DE (MEDIA LOCK) not supported')); command_aborted($DE); end;
	$DF:begin BX_ERROR(('read cmd $DF (MEDIA UNLOCK) not supported')); command_aborted($DF); end;
	$E0:begin BX_ERROR(('read cmd $E0 (STANDBY IMMEDIATE) not supported')); command_aborted($E0); end;
	$E1:begin BX_ERROR(('read cmd $E1 (IDLE IMMEDIATE) not supported')); command_aborted($E1); end;
	$E2:begin BX_ERROR(('read cmd $E2 (STANDBY) not supported')); command_aborted($E2); end;
	$E3:begin BX_ERROR(('read cmd $E3 (IDLE) not supported')); command_aborted($E3); end;
	$E4:begin BX_ERROR(('read cmd $E4 (READ BUFFER) not supported')); command_aborted($E4); end;
	$E5:begin BX_ERROR(('read cmd $E5 (CHECK POWER mod_E) not supported')); command_aborted($E5); end;
	$E6:begin BX_ERROR(('read cmd $E6 (SLEEP) not supported')); command_aborted($E6); end;
	$E7:begin BX_ERROR(('read cmd $E7 (FLUSH CACHE) not supported')); command_aborted($E7); end;
	$E8:begin BX_ERROR(('read cmd $E8 (WRITE BUFFER) not supported')); command_aborted($E8); end;
	$EA:begin BX_ERROR(('read cmd $EA (FLUSH CACHE EXT) not supported')); command_aborted($EA); end;
	$ED:begin BX_ERROR(('read cmd $ED (MEDIA EJECT) not supported')); command_aborted($ED); end;
	$EF:begin BX_ERROR(('read cmd $EF (SET FEATURES) not supported')); command_aborted($EF); end;
	$F1:begin BX_ERROR(('read cmd $F1 (SECURITY SET PASSWORD) not supported')); command_aborted($F1); end;
	$F2:begin BX_ERROR(('read cmd $F2 (SECURITY UNLOCK) not supported')); command_aborted($F2); end;
	$F3:begin BX_ERROR(('read cmd $F3 (SECURITY ERASE PREPARE) not supported')); command_aborted($F3); end;
	$F4:begin BX_ERROR(('read cmd $F4 (SECURITY ERASE UNIT) not supported')); command_aborted($F4); end;
	$F5:begin BX_ERROR(('read cmd $F5 (SECURITY FREEZE LOCK) not supported')); command_aborted($F5); end;
	$F6:begin BX_ERROR(('read cmd $F6 (SECURITY DISABLE PASSWORD) not supported')); command_aborted($F6); end;
	$F8:begin BX_ERROR(('read cmd $F8 (READ NATIVE MAX ADDRESS) not supported')); command_aborted($F8); end;
	$F9:begin BX_ERROR(('read cmd $F9 (SET MAX ADDRESS) not supported')); command_aborted($F9); end;

        else
          BX_PANIC(Format('IO read(1f0h): current command is %02xh',[bx_hard_drive.s[0].controller.current_command]));
        end;
  end;

  $1f1: // hard disk error register
    begin
      bx_hard_drive.s[0].controller.status.err := 0;
      value8 := bx_hard_drive.s[0].controller.error_register;
      goto return_value8;
    end;
  $1f2: // hard disk sector count / interrupt reason
    begin
      value8 := bx_hard_drive.s[0].controller.beta.sector_count;
      goto return_value8;
    end;
    $1f3: // sector number
      begin
      value8 := bx_hard_drive.s[0].controller.sector_no;
      goto return_value8;
    end;  
    $1f4: // cylinder low
      begin
        value8 := (bx_hard_drive.s[0].controller.alfa.cylinder_no  and $00ff);
        goto return_value8;
      end;
    $1f5: // cylinder high
      begin
        value8 := bx_hard_drive.s[0].controller.alfa.cylinder_no shr 8;
        goto return_value8;
      end;

    $1f6: // hard disk drive and head register
      begin
      // b7 Extended data field for ECC
      // b6/b5: Used to be sector size.  00:=256,01:=512,10:=1024,11:=128
      //   Since 512 was always used, bit 6 was taken to mean LBA mod_e:
      //     b6 1:=LBA mod_e, 0:=CHS mod_e
      //     b5 1
      // b4: DRV
      // b3..0 HD3..HD0
      value8 := (1 shl 7) or
               (Word(bx_hard_drive.s[0].controller.lba_mode > 0) shl 6) or
               (1 shl 5) or// 01b := 512 sector size
               (self.drive_select shl 4) or
               (bx_hard_drive.s[0].controller.head_no shl 0);
      goto return_value8;
      end;
//BX_CONTROLLER(0).lba_mod_e

    $1f7, // Hard Disk Status
    $3f6: // Hard Disk Alternate Status
      begin
      if Boolean(((self.drive_select=0) and (BX_C_PRESENT=0)) or
          ((self.drive_select<>0) and (BX_D_PRESENT=0))) then begin
	    // (mch) Just return zero for these registers
	    value8 := 0;
      end else begin
      value8 := (
        (bx_hard_drive.s[0].controller.status.busy shl 7) or
        (bx_hard_drive.s[0].controller.status.drive_ready shl 6) or
        (bx_hard_drive.s[0].controller.status.write_fault shl 5) or
        (bx_hard_drive.s[0].controller.status.seek_complete shl 4) or
        (bx_hard_drive.s[0].controller.status.drq shl 3) or
        (bx_hard_drive.s[0].controller.status.corrected_data shl 2) or
        (bx_hard_drive.s[0].controller.status.index_pulse shl 1) or
        (bx_hard_drive.s[0].controller.status.err) );
      inc(bx_hard_drive.s[0].controller.status.index_pulse_count);
      bx_hard_drive.s[0].controller.status.index_pulse := 0;
        if Boolean(bx_hard_drive.s[0].controller.status.index_pulse_count >= INDEX_PULSE_CYCLE) then begin
        bx_hard_drive.s[0].controller.status.index_pulse := 1;
        bx_hard_drive.s[0].controller.status.index_pulse_count := 0;
        end;
      end;
      if Boolean(address = $1f7) then bx_pic.lower_irq(14);
      goto return_value8;
      end;

    $3f7: // Hard Disk Address Register
      begin
      // Obsolete and unsupported register.  Not driven by hard
      // disk controller.  Report all 1's.  If floppy controller
      // is handling self address, it will call self function
      // set/clear D7 (the only bit it handles), then return
      // the combined value
        value8 := $ff;
      goto return_value8;
     end;

    else
      BX_PANIC(Format('hard drive: io read to address %x unsupported',[address]));
    end;

  BX_PANIC(('hard drive: shouldnt get here!'));
  result:=(0);
  exit;

  return_value32:
  BX_DEBUG(Format('32-bit read from %04x := %08x;',[address, value32]));
  Result:= value32;
  Exit;

  return_value16:
  BX_DEBUG(Format('16-bit read from %04x := %04x',[address, value16]));
  Result:= value16;
  Exit;

  return_value8:
  BX_DEBUG(Format('8-bit read from %04x := %02x',[address, value8]));
  Result:= value8;
  Exit;
end;


  // static IO port write callback handler
  // redirects to non-static class handler to aprocedure virtual functions

procedure bx_hard_drive_c.write_handler(this_ptr:pointer; address:Bit32u; value:Bit32u; io_len:unsigned);
  const vendor_id = 'VTAB\0\0\0\0';
  const product_id = 'Turbo CD-ROM\0\0\0\0';
  const rev_level = 'R0\0\0';
var
  format_:bit8u;
  logical_sector:Bit32u;
  ret:integer;
  prev_control_reset:Bool;
  atapi_command:Bit8u;
  alloc_length:uint16;
  LoEj:Bool;
	Start:Bool;
  PC:Bit8u;
  PageCode:Bit8u;
  i:Integer;
  capacity:uint32;
  transfer_length:uint32;
  lba:uint32;
  msf:bool;
  sub_q:bool;
  data_format:uint8;
  track_number:uint8;
  ret_len:integer;
  drvsel:Bit32u;
  id:unsigned;
  starting_track:uint8;
  toc_length:uint8;
begin
{$if BX_USE_HD_SMF=0}
  bx_hard_drive_c *class_ptr := (bx_hard_drive_c *) self_ptr;

  class_ptr^.write(address, value, io_len);
end;

  procedure
bx_hard_drive_c.write(Bit32u address, Bit32u value, unsigned io_len)
begin
{$else}
  //UNUSED(self_ptr);
{$ifend}  // !BX_USE_HD_SMF

  if Boolean((io_len=2) and (address<>$1f0)) then begin
    BX_PANIC(Format('non-byte IO write to %04x',[address]));
    end;

(*  if Boolean(bx_dbg.disk or (CDROM_SELECTED @ and bx_dbg.cdrom)) then begin
	switch (io_len) then begin
	      case 1:
		    BX_INFO(('8-bit write to %04x := %02x begin%send;',
			      (unsigned) address, (unsigned) value, DEVICE_TYPE_STRING));
		    break;

	      case 2:
		    BX_INFO(('16-bit write to %04x := %04x begin%send;',
			      (unsigned) address, (unsigned) value, DEVICE_TYPE_STRING));
		    break;

	      case 4:
		    BX_INFO(('32-bit write to %04x := %08x begin%send;',
			      (unsigned) address, (unsigned) value, DEVICE_TYPE_STRING));
		    break;

	      default:
		    BX_INFO(('unknown-size write to %04x := %08x begin%send;',
			      (unsigned) address, (unsigned) value, DEVICE_TYPE_STRING));
		    break;
	end;
  end;*)

BX_DEBUG(Format('IO write to %04x := %02x',[address, value]));

  case (address) of
    $1f0:
      begin
        if Boolean(io_len = 1) then begin
          BX_PANIC(Format('byte IO read from %04x',[address]));
        end;
        case (bx_hard_drive.s[0].controller.current_command) of
          $30: // WRITE SECTORS
            begin
              if Boolean(bx_hard_drive.s[0].controller.buffer_index >= 512) then
                BX_PANIC(('IO write(1f0): buffer_index >= 512'));
              case (io_len ) of
                4:
                  begin
                    bx_hard_drive.s[0].controller.buffer[bx_hard_drive.s[0].controller.buffer_index+3] := Bit8u(value shr 24);
                    bx_hard_drive.s[0].controller.buffer[bx_hard_drive.s[0].controller.buffer_index+2] := Bit8u(value shr 16);
                  end;
                2:
                  begin
                    bx_hard_drive.s[0].controller.buffer[bx_hard_drive.s[0].controller.buffer_index+1] := Bit8u(value shr 8);
                    bx_hard_drive.s[0].controller.buffer[bx_hard_drive.s[0].controller.buffer_index]   := Bit8u(value);
                  end;
              end;

            bx_hard_drive.s[0].controller.buffer_index:=
            bx_hard_drive.s[0].controller.buffer_index + io_len;

            (* if buffer completely writtten *)
            if Boolean(bx_hard_drive.s[0].controller.buffer_index >= 512) then begin

              {$if TEST_WRITE_BEYOND_END=1}
        	    bx_hard_drive.s[0].controller.alfa.cylinder_no :=
              bx_hard_drive.s[0].controller.alfa.cylinder_no + 100000;
              {$ifend}
      	    if Boolean(calculate_logical_address(logical_sector)=0) then begin
      	      BX_ERROR(Format('write reached invalid sector %u, aborting',[logical_sector]));
    	      command_aborted (bx_hard_drive.s[0].controller.current_command);
	          exit;
            end;
{$if TEST_WRITE_BEYOND_END=2}
           logical_sector +:= 100000;
{$ifend}
	         ret := s[self.drive_select].hard_drive.lseek(logical_sector * 512, 0);
           if Boolean(ret < 0) then begin
              BX_ERROR(Format('could not lseek() hard drive image file at byte %u',[logical_sector * 512]));
	            command_aborted (bx_hard_drive.s[0].controller.current_command);
	            exit;
	         end;
      	   ret := s[self.drive_select].hard_drive.write(bx_hard_drive.s[0].controller.buffer, 512);
           if Boolean(ret < 512) then begin
              BX_ERROR(Format('could not write() hard drive image file at byte %d',[logical_sector*512]));
      	      command_aborted (bx_hard_drive.s[0].controller.current_command);
	            exit;
      	   end;

           bx_hard_drive.s[0].controller.buffer_index := 0;

            (* update sector count, sector number, cylinder,
             * drive, head, status
             * if there are more sectors, read next one in...
             *)

     	     increment_address();

            (* When the write is complete, controller clears the DRQ bit and
             * sets the BSY bit.
             * If at least one more sector is to be written, controller sets DRQ bit,
             * clears BSY bit, and issues IRQ 14
             *)

            if Boolean(bx_hard_drive.s[0].controller.beta.sector_count<>0) then begin
              bx_hard_drive.s[0].controller.status.busy := 0;
              bx_hard_drive.s[0].controller.status.drive_ready := 1;
              bx_hard_drive.s[0].controller.status.drq := 1;
              bx_hard_drive.s[0].controller.status.corrected_data := 0;
              bx_hard_drive.s[0].controller.status.err := 0;
              end
            else begin (* no more sectors to write *)
              bx_hard_drive.s[0].controller.status.busy := 0;
              bx_hard_drive.s[0].controller.status.drive_ready := 1;
              bx_hard_drive.s[0].controller.status.drq := 0;
              bx_hard_drive.s[0].controller.status.err := 0;
              bx_hard_drive.s[0].controller.status.corrected_data := 0;
              end;
	    raise_interrupt();
            end;
          end;

	    $a0: // PACKET
        begin
    		  if Boolean(bx_hard_drive.s[0].controller.buffer_index >= PACKET_SIZE) then
    			BX_PANIC(('IO write(1f0): buffer_index >= PACKET_SIZE'));
    		  bx_hard_drive.s[0].controller.buffer[bx_hard_drive.s[0].controller.buffer_index] := value;
		      bx_hard_drive.s[0].controller.buffer[bx_hard_drive.s[0].controller.buffer_index+1] := (value shr 8);
    		  bx_hard_drive.s[0].controller.buffer_index := bx_hard_drive.s[0].controller.buffer_index + 2;

		  (* if packet completely writtten *)
		  if Boolean(bx_hard_drive.s[0].controller.buffer_index >= PACKET_SIZE) then begin
			// complete command received
			atapi_command := bx_hard_drive.s[0].controller.buffer[0];

			if Boolean(BX_CDROM_PRESENT) then
				BX_INFO(Format('cdrom: ATAPI command $%x started',[atapi_command]));

			case (atapi_command) of
			      $00: // test unit ready
              begin
    				    if Boolean(s[self.drive_select].cdrom.ready) then begin
		    			  atapi_cmd_nop();
				      end else begin
    					  atapi_cmd_error(SENSE_NOT_READY, ASC_MEDIUM_NOT_PRESENT);
		  		    end;
				    raise_interrupt();
				    end;

			      $03: begin // request sense
  				    alloc_length := bx_hard_drive.s[0].controller.buffer[4];
	  			    init_send_atapi_command(atapi_command, 18, alloc_length);

				    // sense data
				    bx_hard_drive.s[0].controller.buffer[0] := $70 or (1 shl 7);
				    bx_hard_drive.s[0].controller.buffer[1] := 0;
				    bx_hard_drive.s[0].controller.buffer[2] := Ord(s[self.drive_select].sense.sense_key);
				    bx_hard_drive.s[0].controller.buffer[3] := s[self.drive_select].sense.information.arr[0];
				    bx_hard_drive.s[0].controller.buffer[4] := s[self.drive_select].sense.information.arr[1];
				    bx_hard_drive.s[0].controller.buffer[5] := s[self.drive_select].sense.information.arr[2];
				    bx_hard_drive.s[0].controller.buffer[6] := s[self.drive_select].sense.information.arr[3];
				    bx_hard_drive.s[0].controller.buffer[7] := 17-7;
				    bx_hard_drive.s[0].controller.buffer[8] := s[self.drive_select].sense.specific_inf.arr[0];
				    bx_hard_drive.s[0].controller.buffer[9] := s[self.drive_select].sense.specific_inf.arr[1];
				    bx_hard_drive.s[0].controller.buffer[10] := s[self.drive_select].sense.specific_inf.arr[2];
				    bx_hard_drive.s[0].controller.buffer[11] := s[self.drive_select].sense.specific_inf.arr[3];
				    bx_hard_drive.s[0].controller.buffer[12] := s[self.drive_select].sense.asc;
				    bx_hard_drive.s[0].controller.buffer[13] := s[self.drive_select].sense.ascq;
				    bx_hard_drive.s[0].controller.buffer[14] := s[self.drive_select].sense.fruc;
				    bx_hard_drive.s[0].controller.buffer[15] := s[self.drive_select].sense.key_spec.arr[0];
				    bx_hard_drive.s[0].controller.buffer[16] := s[self.drive_select].sense.key_spec.arr[1];
				    bx_hard_drive.s[0].controller.buffer[17] := s[self.drive_select].sense.key_spec.arr[2];

				    ready_to_send_atapi();
			      end;

			      $1b: begin // start stop unit
				    //Boolean Immed := (bx_hard_drive.s[0].controller.buffer[1] shr 0)  and 1;
  				    LoEj := (bx_hard_drive.s[0].controller.buffer[4] shr 1)  and 1;
	  			    Start := (bx_hard_drive.s[0].controller.buffer[4] shr 0)  and 1;

				    if Boolean((LoEj=0) and (Start=0)) then begin // stop the disc
					  BX_PANIC(('Stop disc not implemented'));
				    end else if Boolean((LoEj=0) and (Start<>0)) then begin // start the disc and read the TOC
					  BX_PANIC(('Start disc not implemented'));
				    end else if Boolean((LoEj=0) and (Start=0)) then begin // Eject the disc
                                          atapi_cmd_nop();
                                          if Boolean(self.s[1].cdrom.ready) then begin
{$if LOWLEVEL_CDROM=1}
                                            self.s[1].cdrom.cd.eject_cdrom();
{$ifend}
                                            self.s[1].cdrom.ready := 0;
                                            //bx_options.cdromd.Oinserted^.set(BX_EJECTED);
                                            //bx_gui.update_drive_status_buttons();
                                          end;
                                          raise_interrupt();
				    end else begin // Load the disc
					  // My guess is that self command only closes the tray, that's a no-op for us
					  //atapi_cmd_nop();
					  //raise_interrupt();
				    end;
			      end;

			      $bd: begin // mechanism status
				    alloc_length := read_16bit(@bx_hard_drive.s[0].controller.buffer[8]);

				    if Boolean(alloc_length = 0) then
					  BX_PANIC(('Zero allocation length to MECHANISM STATUS not impl.'));

				    init_send_atapi_command(atapi_command, 8, alloc_length);

				    bx_hard_drive.s[0].controller.buffer[0] := 0; // reserved for non changers
				    bx_hard_drive.s[0].controller.buffer[1] := 0; // reserved for non changers

				    bx_hard_drive.s[0].controller.buffer[2] := 0; // Current LBA (TODO!)
				    bx_hard_drive.s[0].controller.buffer[3] := 0; // Current LBA (TODO!)
				    bx_hard_drive.s[0].controller.buffer[4] := 0; // Current LBA (TODO!)

				    bx_hard_drive.s[0].controller.buffer[5] := 1; // one slot

				    bx_hard_drive.s[0].controller.buffer[6] := 0; // slot table length
				    bx_hard_drive.s[0].controller.buffer[7] := 0; // slot table length

				    ready_to_send_atapi();
			      end;


			      $5a: begin // mod_e sense
				    alloc_length := read_16bit(@bx_hard_drive.s[0].controller.buffer[7]);

				    PC := bx_hard_drive.s[0].controller.buffer[2] shr 6;
				    PageCode := bx_hard_drive.s[0].controller.buffer[2]  and $3f;

				    case (PC) of
					  $0: // current values
              begin
    						case (PageCode) of
						      $01: // error recovery
                    begin
    							    init_send_atapi_command(atapi_command, sizeof(error_recovery_t) + 8, alloc_length);

		    					    init_mode_sense_single(@s[self.drive_select].cdrom.current.error_recovery,
				  						   sizeof(error_recovery_t));
  					  		    ready_to_send_atapi();
						  	    end;

						      $2a: // CD-ROM capabilities  and mech. status
                    begin
                      init_send_atapi_command(atapi_command, 28, alloc_length);
                      init_mode_sense_single(@bx_hard_drive.s[0].controller.buffer[8], 28);
                      bx_hard_drive.s[0].controller.buffer[8] := $2a;
                      bx_hard_drive.s[0].controller.buffer[9] := $12;
                      bx_hard_drive.s[0].controller.buffer[10] := $00;
                      bx_hard_drive.s[0].controller.buffer[11] := $00;
                      bx_hard_drive.s[0].controller.buffer[12] := $00;
                      bx_hard_drive.s[0].controller.buffer[13] := (3 shl 5);
                      if Boolean(s[self.drive_select].cdrom.locked) then
                      bx_hard_drive.s[0].controller.buffer[14] := (1 or ((1 shl 1)) or
                        (1 shl 3) or
                        (1 shl 5)) else
                      bx_hard_drive.s[0].controller.buffer[14] := (1 or (0) or
                        (1 shl 3) or
                        (1 shl 5));
                      bx_hard_drive.s[0].controller.buffer[15] := $00;
                      bx_hard_drive.s[0].controller.buffer[16] := (706 shr 8)  and $ff;
                      bx_hard_drive.s[0].controller.buffer[17] := 706  and $ff;
                      bx_hard_drive.s[0].controller.buffer[18] := 0;
                      bx_hard_drive.s[0].controller.buffer[19] := 2;
                      bx_hard_drive.s[0].controller.buffer[20] := (512 shr 8)  and $ff;
                      bx_hard_drive.s[0].controller.buffer[21] := 512  and $ff;
                      bx_hard_drive.s[0].controller.buffer[22] := (706 shr 8)  and $ff;
                      bx_hard_drive.s[0].controller.buffer[23] := 706  and $ff;
                      bx_hard_drive.s[0].controller.buffer[24] := 0;
                      bx_hard_drive.s[0].controller.buffer[25] := 0;
                      bx_hard_drive.s[0].controller.buffer[26] := 0;
                      bx_hard_drive.s[0].controller.buffer[27] := 0;
                      ready_to_send_atapi();
							      end;

						      $0d, // CD-ROM
						      $0e, // CD-ROM audio control
						      $3f: // all
  							    BX_PANIC(Format('cdrom: mod_E SENSE (curr), code:=%x',[PageCode]));

						      else
                    begin
    							    // not implemeted by self device
		    					    BX_INFO(Format('cdrom: mod_E SENSE PC:=%x, PageCode:=%x, not implemented by device', [PC, PageCode]));
    							    atapi_cmd_error(SENSE_ILLEGAL_REQUEST,
									    ASC_INV_FIELD_IN_CMD_PACKET);
		    					    raise_interrupt();
							      end;
               end; end;
					  $1: // changeable values
              begin
    						case (PageCode) of
						      $01, // error recovery
						      $0d, // CD-ROM
						      $0e, // CD-ROM audio control
						      $2a, // CD-ROM capabilities  and mech. status
						      $3f: // all
  							    BX_PANIC(Format('cdrom: mod_E SENSE (chg), code:=%x',[PageCode]));

						      else
							    // not implemeted by self device
                    begin
    							    BX_INFO(Format('cdrom: mod_E SENSE PC:=%x, PageCode:=%x, not implemented by device',[PC, PageCode]));
  	  						    atapi_cmd_error(SENSE_ILLEGAL_REQUEST, ASC_INV_FIELD_IN_CMD_PACKET);
  		  					    raise_interrupt();
							      end;
						end;
            end;

					  $2: // default values
              begin
    						case (PageCode) of
						      $01, // error recovery
						      $0d, // CD-ROM
						      $0e, // CD-ROM audio control
						      $2a, // CD-ROM capabilities  and mech. status
						      $3f: // all
                    begin
    							    BX_PANIC(Format('cdrom: mod_E SENSE (dflt), code:=%x',[PageCode]));
							      end;

						      else
                    begin
    							    // not implemeted by self device
		    					    BX_INFO(Format('cdrom: mod_E SENSE PC:=%x, PageCode:=%x, not implemented by device',[PC, PageCode]));
    							    atapi_cmd_error(SENSE_ILLEGAL_REQUEST,ASC_INV_FIELD_IN_CMD_PACKET);
    							    raise_interrupt();
							      end;
						end;
						end;

					  $3: // saved values not implemented
              begin
  	  					atapi_cmd_error(SENSE_ILLEGAL_REQUEST, ASC_SAVING_PARAMETERS_NOT_SUPPORTED);
	  	  				raise_interrupt();
						  end;

					  else
  						BX_PANIC(('Should not get here!'));
				    end;
			      end;

			      $12: begin // inquiry
				    alloc_length := bx_hard_drive.s[0].controller.buffer[4];

				    init_send_atapi_command(atapi_command, 36, alloc_length);

				    bx_hard_drive.s[0].controller.buffer[0] := $05; // CD-ROM
				    bx_hard_drive.s[0].controller.buffer[1] := $80; // Removable
				    bx_hard_drive.s[0].controller.buffer[2] := $00; // ISO, ECMA, ANSI version
				    bx_hard_drive.s[0].controller.buffer[3] := $21; // ATAPI-2, as specified
				    bx_hard_drive.s[0].controller.buffer[4] := 31; // additional length (total 36)
				    bx_hard_drive.s[0].controller.buffer[5] := $00; // reserved
				    bx_hard_drive.s[0].controller.buffer[6] := $00; // reserved
				    bx_hard_drive.s[0].controller.buffer[7] := $00; // reserved

				    // Vendor ID
				    for i := 0 to 8 do
  					  bx_hard_drive.s[0].controller.buffer[8+i] := Bit8u(vendor_id[i]);

				    // Product ID
				    for i := 0 to 16 do
  					  bx_hard_drive.s[0].controller.buffer[16+i] := Bit8u(product_id[i]);

				    // Product Revision level
				    for i := 0 to 4 do
  					  bx_hard_drive.s[0].controller.buffer[32+i] := Bit8u(rev_level[i]);

				    ready_to_send_atapi();
			      end;

			      $25: begin // read cd-rom capacity
				    // no allocation length???
				    init_send_atapi_command(atapi_command, 8, 8);

				    if Boolean(s[self.drive_select].cdrom.ready) then begin
					  capacity := s[self.drive_select].cdrom.capacity;
					  BX_INFO(Format('Capacity is %d sectors (%d bytes)',[capacity, capacity * 2048]));
					  bx_hard_drive.s[0].controller.buffer[0] := (capacity shr 24)  and $ff;
					  bx_hard_drive.s[0].controller.buffer[1] := (capacity shr 16)  and $ff;
					  bx_hard_drive.s[0].controller.buffer[2] := (capacity shr 8)  and $ff;
					  bx_hard_drive.s[0].controller.buffer[3] := (capacity shr 0)  and $ff;
					  bx_hard_drive.s[0].controller.buffer[4] := (2048 shr 24)  and $ff;
					  bx_hard_drive.s[0].controller.buffer[5] := (2048 shr 16)  and $ff;
					  bx_hard_drive.s[0].controller.buffer[6] := (2048 shr 8)  and $ff;
					  bx_hard_drive.s[0].controller.buffer[7] := (2048 shr 0)  and $ff;
					  ready_to_send_atapi();
				    end else begin
  					  atapi_cmd_error(SENSE_NOT_READY, ASC_MEDIUM_NOT_PRESENT);
	  				  raise_interrupt();
				    end;
			      end;

			      $be: begin // read cd
  				    if Boolean(s[self.drive_select].cdrom.ready) then begin
	  				  BX_PANIC(('Read CD with CD present not implemented'));
		  		    end else begin
					  atapi_cmd_error(SENSE_NOT_READY, ASC_MEDIUM_NOT_PRESENT);
					  raise_interrupt();
				    end;
			      end;

			      $43: begin // read toc
				    if Boolean(s[self.drive_select].cdrom.ready) then begin
{$if LOWLEVEL_CDROM=1}
					  msf := (bx_hard_drive.s[0].controller.buffer[1] shr 1)  and 1;
					  starting_track := bx_hard_drive.s[0].controller.buffer[6];
{$ifend}
					  alloc_length := read_16bit(@bx_hard_drive.s[0].controller.buffer[7]);

					  format_ := (bx_hard_drive.s[0].controller.buffer[9] shr 6);
					  case (format_) of
						0:
              begin
{$if LOWLEVEL_CDROM=1}
						      if Boolean(s[self.drive_select].cdrom.cd.read_toc(@bx_hard_drive.s[0].controller.buffer,
										       toc_length, msf, starting_track)=false) then begin
							    atapi_cmd_error(SENSE_ILLEGAL_REQUEST,
									    ASC_INV_FIELD_IN_CMD_PACKET);
							    raise_interrupt();
						      end else begin
							    init_send_atapi_command(atapi_command, toc_length, alloc_length);
							    ready_to_send_atapi();
						      end;
{$else}
						      BX_PANIC(('LOWLEVEL_CDROM not defined'));
{$ifend}
						      end;

						1:
              begin
						      // multi session stuff. we ignore self and emulate a single session only
						      init_send_atapi_command(atapi_command, 12, alloc_length);

						      bx_hard_drive.s[0].controller.buffer[0] := 0;
						      bx_hard_drive.s[0].controller.buffer[1] := $0a;
						      bx_hard_drive.s[0].controller.buffer[2] := 1;
						      bx_hard_drive.s[0].controller.buffer[3] := 1;
						      for i := 0 to 8 do
  							    bx_hard_drive.s[0].controller.buffer[4+i] := 0;

						      ready_to_send_atapi();
						  end;

						2:
						      BX_PANIC(Format('(READ TOC) Format %d not supported',[format_]));
						else
						      BX_PANIC(Format('(READ TOC) Format %d not supported',[format_]));
					  end;
				    end else begin
					  atapi_cmd_error(SENSE_NOT_READY, ASC_MEDIUM_NOT_PRESENT);
					  raise_interrupt();
				    end;
			      end;

			      $28: begin // read (10)
				    transfer_length := read_16bit(@bx_hard_drive.s[0].controller.buffer[7]);
				    lba := read_32bit(@bx_hard_drive.s[0].controller.buffer[2]);

				    if Boolean(s[self.drive_select].cdrom.ready=0) then begin
  					  atapi_cmd_error(SENSE_NOT_READY, ASC_MEDIUM_NOT_PRESENT);
	  				  raise_interrupt();
              exit; //!!!
					  end;

				    if Boolean(transfer_length = 0) then begin
					  atapi_cmd_nop();
					  raise_interrupt();
					  BX_INFO(('READ(10) with transfer length 0, ok'));
              exit; //!!!
					  end;

				    if Boolean(lba + transfer_length > s[self.drive_select].cdrom.capacity) then begin
					  atapi_cmd_error(SENSE_ILLEGAL_REQUEST, ASC_LOGICAL_BLOCK_OOR);
					  raise_interrupt();
              exit; //!!!
				    end;

				    //BX_INFO(('cdrom: READ LBA:=%d LEN:=%d', lba, transfer_length));

				    // handle command
				    init_send_atapi_command(atapi_command, transfer_length * 2048, transfer_length * 2048, 1);
				    s[self.drive_select].cdrom.remaining_blocks := transfer_length;
				    s[self.drive_select].cdrom.next_lba := lba;
				    ready_to_send_atapi();
			      end;


				$2b: begin // seek
					lba := read_32bit(@bx_hard_drive.s[0].controller.buffer[2]);
					if Boolean(s[self.drive_select].cdrom.ready=0) then begin
						atapi_cmd_error(SENSE_NOT_READY, ASC_MEDIUM_NOT_PRESENT);
						raise_interrupt();
					end;

					if Boolean(lba > s[self.drive_select].cdrom.capacity) then begin
						atapi_cmd_error(SENSE_ILLEGAL_REQUEST, ASC_LOGICAL_BLOCK_OOR);
						raise_interrupt();
					end;
					BX_INFO(('cdrom: SEEK (ignored)'));
					atapi_cmd_nop();
					raise_interrupt();
				end;

			      $1e: begin // prevent/allow medium removal
				    if Boolean(s[self.drive_select].cdrom.ready) then begin
					  s[self.drive_select].cdrom.locked := bx_hard_drive.s[0].controller.buffer[4]  and 1;
					  atapi_cmd_nop();
				    end else begin
					  atapi_cmd_error(SENSE_NOT_READY, ASC_MEDIUM_NOT_PRESENT);
				    end;
				    raise_interrupt();
			      end;

			      $42: begin // read sub-channel
				    msf := get_packet_field(1, 1, 1);
				    sub_q := get_packet_field(2, 6, 1);
				    data_format := get_packet_byte(3);
				    track_number := get_packet_byte(6);
				    alloc_length := get_packet_word(7);
                                    //UNUSED(msf);
                                    //UNUSED(data_format);
                                    //UNUSED(track_number);

				    if Boolean(s[self.drive_select].cdrom.ready=0) then begin
					  atapi_cmd_error(SENSE_NOT_READY, ASC_MEDIUM_NOT_PRESENT);
					  raise_interrupt();
				    end else begin
					  bx_hard_drive.s[0].controller.buffer[0] := 0;
					  bx_hard_drive.s[0].controller.buffer[1] := 0; // audio not supported
					  bx_hard_drive.s[0].controller.buffer[2] := 0;
					  bx_hard_drive.s[0].controller.buffer[3] := 0;

					  ret_len := 4; // header size

					  if Boolean(sub_q) then begin // !sub_q = header only
						BX_PANIC(('Read sub-channel with SubQ not implemented'));
					  end;

					  init_send_atapi_command(atapi_command, ret_len, alloc_length);
					  ready_to_send_atapi();
				    end;
			      end;


			      $51: begin // read disc info
                                // no-op to keep the Linux CD-ROM driver happy
			        atapi_cmd_error(SENSE_ILLEGAL_REQUEST, ASC_INV_FIELD_IN_CMD_PACKET);
			        raise_interrupt();
			      end;


			      $a8, // read (12)
			      $55, // mod_e select
			      $a6, // load/unload cd
			      $4b, // pause/resume
			      $45, // play audio
			      $47, // play audio msf
			      $bc, // play cd
			      $b9, // read cd msf
			      $44, // read header
			      $ba, // scan
			      $bb, // set cd speed
			      $4e: // stop play/scan
  				    BX_PANIC(Format('Unknown ATAPI command $%x (%d)',[atapi_command, atapi_command]));
			      else
  				    BX_PANIC(Format('Unknown ATAPI command $%x (%d)',[atapi_command, atapi_command]));
				   end;
		  end

        else
          BX_PANIC(Format('IO write(1f0h): current command is %02xh',[bx_hard_drive.s[0].controller.current_command]));
        end;
      end;
      end;
    $1f1: (* hard disk write precompensation *)
      begin
    	  WRITE_FEATURES(value);
      end;

    $1f2: (* hard disk sector count *)
      begin
    	  WRITE_SECTOR_COUNT(value);
    	  if Boolean(BX_DEBUG_HDD) then
      		BX_INFO(Format('sector count := %u',[value]));
	    end;

    $1f3: (* hard disk sector number *)
      begin
    	  WRITE_SECTOR_NUMBER(value);
    	  if Boolean(BX_DEBUG_HDD) then
      		BX_INFO(Format('sector number := %u',[value]));
        end;

    $1f4: (* hard disk cylinder low *)
      begin
    	  WRITE_CYLINDER_LOW(value);
    	  if Boolean(BX_DEBUG_HDD) then
      		BX_INFO(Format('cylinder low := %02xh',[value]));
	    end;

    $1f5: (* hard disk cylinder high *)
      begin
    	  WRITE_CYLINDER_HIGH(value);
    	  if Boolean(BX_DEBUG_HDD) then
      		BX_INFO(Format('cylinder high := %02xh ',[value]));
	    end;

    $1f6: // hard disk drive and head register
      // b7 Extended data field for ECC
      // b6/b5: Used to be sector size.  00:=256,01:=512,10:=1024,11:=128
      //   Since 512 was always used, bit 6 was taken to mean LBA mod_e:
      //     b6 1:=LBA mod_e, 0:=CHS mod_e
      //     b5 1
      // b4: DRV
      // b3..0 HD3..HD0
      begin
        if Boolean( (value  and $a0) <> $a0 ) then // 1x1xxxxx
          BX_INFO(Format('IO write 1f6 (%02x): not 1x1xxxxxb',[value]));
          self.drive_select := (value shr 4)  and $01;
        drvsel := self.drive_select;
        WRITE_HEAD_NO(value  and $f);
        if Boolean((bx_hard_drive.s[0].controller.lba_mode = 0) and (((value shr 6) and 1) = 1)) then
          BX_INFO(('enabling LBA mod_e'));
        WRITE_LBA_MODE((value shr 6)  and 1);
        if Boolean((drvsel=0) and (BX_C_PRESENT=0)) then begin
          BX_ERROR (('device set to 0 which does not exist'));
          bx_hard_drive.s[0].controller.error_register := $04; // aborted
          bx_hard_drive.s[0].controller.status.err := 1;
        end
       else if ((drvsel<>0) and (BX_D_PRESENT=0)) then
        begin
          BX_ERROR (('device set to 1 which does not exist'));
          bx_hard_drive.s[0].controller.error_register := $04; // aborted
          bx_hard_drive.s[0].controller.status.err := 1;
        end;
     end;   

    $1f7: // hard disk command
      begin
	  // (mch) Writes to the command register with drive_select <> 0
	  // are ignored if no secondary device is present
      if Boolean((self.drive_select <> 0) and (value <> $90) and (BX_D_PRESENT=1)) then
	      exit;

      if Boolean(bx_hard_drive.s[0].controller.status.busy) then
        BX_PANIC(('hard disk: command sent, controller BUSY'));
      if Boolean( (value  and $f0) = $10 ) then
        value := $10;
      case (value) of

        $10: // CALIBRATE DRIVE
          begin
       	  if Boolean(s[self.drive_select].device_type <> IDE_DISK) then
           BX_PANIC(('calibrate drive issued to non-disk'));
           if Boolean((self.drive_select = 0) and (BX_C_PRESENT=0) or
              (self.drive_select <> 0) and (BX_D_PRESENT=0)) then
              begin
                bx_hard_drive.s[0].controller.error_register := $02; // Track 0 not found
                bx_hard_drive.s[0].controller.status.busy := 0;
                bx_hard_drive.s[0].controller.status.drive_ready := 1;
                bx_hard_drive.s[0].controller.status.seek_complete := 0;
                bx_hard_drive.s[0].controller.status.drq := 0;
                bx_hard_drive.s[0].controller.status.err := 1;
      	        raise_interrupt();
                BX_INFO(('calibrate drive: disk not present'));
                exit;
              end;

          (* move head to cylinder 0, issue IRQ 14 *)
          bx_hard_drive.s[0].controller.error_register := 0;
          bx_hard_drive.s[0].controller.alfa.cylinder_no := 0;
          bx_hard_drive.s[0].controller.status.busy := 0;
          bx_hard_drive.s[0].controller.status.drive_ready := 1;
          bx_hard_drive.s[0].controller.status.seek_complete := 1;
          bx_hard_drive.s[0].controller.status.drq := 0;
          bx_hard_drive.s[0].controller.status.err := 0;
      	  raise_interrupt();
         end;

        $20, // READ MULTIPLE SECTORS, with retries
        $21: // READ MULTIPLE SECTORS, without retries
          (* update sector_no, always points to current sector
           * after each sector is read to buffer, DRQ bit set and issue IRQ 14
           * if interrupt handler transfers all data words into main memory,
           * and more sectors to read, then set BSY bit again, clear DRQ and
           * read next sector into buffer
           * sector count of 0 means 256 sectors
           *)
    begin
	  if Boolean(s[self.drive_select].device_type <> IDE_DISK) then
		BX_PANIC(('read multiple issued to non-disk'));

          bx_hard_drive.s[0].controller.current_command := value;

	  // Lose98 accesses 0/0/0 in CHS mod_e
	  if Boolean((bx_hard_drive.s[0].controller.lba_mode=0) and
               (bx_hard_drive.s[0].controller.head_no=0) and
               (bx_hard_drive.s[0].controller.alfa.cylinder_no=0) and
	             (bx_hard_drive.s[0].controller.sector_no=0)) then begin
          		BX_INFO(('Read from 0/0/0, aborting command'));
          		command_aborted(value);
		end;

{$if TEST_READ_BEYOND_END=2}
	  bx_hard_drive.s[0].controller.alfa.cylinder_no +:= 100000;
{$ifend}
	  if Boolean(calculate_logical_address(logical_sector)=0) then begin
	    BX_ERROR(Format('initial read from sector %u out of bounds, aborting',[logical_sector]));
	    command_aborted(value);
	  end;
{$if TEST_READ_BEYOND_END=3}
	  logical_sector +:= 100000;
{$ifend}
	  ret:=s[self.drive_select].hard_drive.lseek(logical_sector * 512, SEEK_SET);
          if Boolean(ret < 0) then begin
            BX_ERROR (('could not lseek() hard drive image file, aborting'));
	    command_aborted(value);
	  end;
	  ret := s[self.drive_select].hard_drive.read(bx_hard_drive.s[0].controller.buffer, 512);
          if Boolean(ret < 512) then begin
            BX_ERROR(Format('logical sector was %u',[logical_sector]));
            BX_ERROR(Format('could not read() hard drive image file at byte %d',[logical_sector*512]));
	    command_aborted(value);
	  end;

          bx_hard_drive.s[0].controller.error_register := 0;
          bx_hard_drive.s[0].controller.status.busy  := 0;
          bx_hard_drive.s[0].controller.status.drive_ready := 1;
          bx_hard_drive.s[0].controller.status.seek_complete := 1;
          bx_hard_drive.s[0].controller.status.drq   := 1;
          bx_hard_drive.s[0].controller.status.corrected_data := 0;
          bx_hard_drive.s[0].controller.status.err   := 0;
          bx_hard_drive.s[0].controller.buffer_index := 0;
      	  raise_interrupt();
    end;
        $30: (* WRITE SECTORS, with retries *)
          begin
          (* update sector_no, always points to current sector
           * after each sector is read to buffer, DRQ bit set and issue IRQ 14
           * if interrupt handler transfers all data words into main memory,
           * and more sectors to read, then set BSY bit again, clear DRQ and
           * read next sector into buffer
           * sector count of 0 means 256 sectors
           *)

       if Boolean(s[self.drive_select].device_type <> IDE_DISK) then
      		BX_PANIC(('write multiple issued to non-disk'));

        if Boolean(bx_hard_drive.s[0].controller.status.busy) then begin
          BX_PANIC(('write command: BSY bit set'));
        end;
        bx_hard_drive.s[0].controller.current_command := value;

        // implicit seek done :^)
        bx_hard_drive.s[0].controller.error_register := 0;
        bx_hard_drive.s[0].controller.status.busy := 0;
        // bx_hard_drive.s[0].controller.status.drive_ready := 1;
        bx_hard_drive.s[0].controller.status.seek_complete := 1;
        bx_hard_drive.s[0].controller.status.drq := 1;
        bx_hard_drive.s[0].controller.status.err   := 0;
        bx_hard_drive.s[0].controller.buffer_index := 0;
        end;
        $90: // EXECUTE DEVICE DIAGNOSTIC
          begin
            if Boolean(bx_hard_drive.s[0].controller.status.busy) then begin
              BX_PANIC(('diagnostic command: BSY bit set'));
            end;
	  if Boolean(s[self.drive_select].device_type <> IDE_DISK) then
		BX_PANIC(('drive diagnostics issued to non-disk'));
          bx_hard_drive.s[0].controller.error_register := $81; // Drive 1 failed, no error on drive 0
          // bx_hard_drive.s[0].controller.status.busy := 0; // not needed
          bx_hard_drive.s[0].controller.status.drq := 0;
          bx_hard_drive.s[0].controller.status.err := 0;
         end;

        $91: // INITIALIZE DRIVE PARAMETERS
          begin
          if Boolean(bx_hard_drive.s[0].controller.status.busy) then begin
            BX_PANIC(('init drive parameters command: BSY bit set'));
            end;
	  if Boolean(s[self.drive_select].device_type <> IDE_DISK) then
		BX_PANIC(('initialize drive parameters issued to non-disk'));
          // sets logical geometry of specified drive
          BX_DEBUG(Format('init drive params: sec:=%u, drive sel:=%u, head:=%u',[
            bx_hard_drive.s[0].controller.beta.sector_count,
            self.drive_select,
            bx_hard_drive.s[0].controller.head_no]));
          if Boolean((self.drive_select = 0) and (BX_C_PRESENT=1)) or
              (self.drive_select <> 0) and (BX_D_PRESENT<>0) then begin
            BX_DEBUG(('init drive params: disk%c not present'));
            //bx_hard_drive.s[0].controller.error_register := $12;
            bx_hard_drive.s[0].controller.status.busy := 0;
            bx_hard_drive.s[0].controller.status.drive_ready := 1;
            bx_hard_drive.s[0].controller.status.drq := 0;
            bx_hard_drive.s[0].controller.status.err := 0;
	    raise_interrupt();
	  end;
          if Boolean(bx_hard_drive.s[0].controller.beta.sector_count <> s[self.drive_select].hard_drive.sectors) then
            BX_PANIC(('init drive params: sector count doesnt match'));
          if Boolean( bx_hard_drive.s[0].controller.head_no <> (s[self.drive_select].hard_drive.heads-1) ) then
            BX_PANIC(('init drive params: head number doesn''t match'));
          bx_hard_drive.s[0].controller.status.busy := 0;
          bx_hard_drive.s[0].controller.status.drive_ready := 1;
          bx_hard_drive.s[0].controller.status.drq := 0;
          bx_hard_drive.s[0].controller.status.err := 0;
	  raise_interrupt();
          end;

        $ec: // IDENTIFY DEVICE
          begin
          if Boolean(BX_NEW_HDD_SUPPORT=1) then begin
    		  BX_INFO(('Drive ID Command issued : $ec '));

            if Boolean((self.drive_select<>0) and (BX_C_PRESENT=0)) then begin
              BX_INFO(('1st drive not present, aborting'));
              command_aborted(value);
              end;
            if Boolean((self.drive_select<>0) and (BX_D_PRESENT=0)) then begin
              BX_INFO(('2nd drive not present, aborting'));
              command_aborted(value);
              end;
	    if Boolean(s[self.drive_select].device_type = IDE_CDROM) then begin
		  bx_hard_drive.s[0].controller.head_no        := 0;
		  bx_hard_drive.s[0].controller.beta.sector_count   := 1;
		  bx_hard_drive.s[0].controller.sector_no      := 1;
		  bx_hard_drive.s[0].controller.alfa.cylinder_no    := $eb14;
		  command_aborted($ec);
	    end else begin
		  bx_hard_drive.s[0].controller.current_command := value;
		  bx_hard_drive.s[0].controller.error_register := 0;

		  // See ATA/ATAPI-4, 8.12
		  bx_hard_drive.s[0].controller.status.busy  := 0;
		  bx_hard_drive.s[0].controller.status.drive_ready := 1;
		  bx_hard_drive.s[0].controller.status.write_fault := 0;
		  bx_hard_drive.s[0].controller.status.drq   := 1;
		  bx_hard_drive.s[0].controller.status.err   := 0;

		  bx_hard_drive.s[0].controller.status.seek_complete := 1;
		  bx_hard_drive.s[0].controller.status.corrected_data := 0;

		  bx_hard_drive.s[0].controller.buffer_index := 0;
		  raise_interrupt();
		  identify_drive(self.drive_select);
	    end;
	  end
          else begin
	    BX_INFO(('sent IDENTIFY DEVICE ($ec) to old hard drive'));
            command_aborted(value);
	  end;
          end;

    $ef: // SET FEATURES
      begin
    	  case (bx_hard_drive.s[0].controller.features) of
	    $02, // Enable and
	    $82, //  Disable write cache.
	    $AA, // Enable and
	    $55: //  Disable look-ahead cache.
        begin
  	      BX_INFO(('SET FEATURES subcommand not supported by disk.'));
	        command_aborted(value);
	      end;

	    else
	      BX_PANIC(Format('SET FEATURES with unknown subcommand: $%02x',[bx_hard_drive.s[0].controller.features]));
	  end;
    end;

    $40: // READ VERIFY SECTORS
      begin
          if BX_NEW_HDD_SUPPORT = 1 then begin
	    if Boolean(s[self.drive_select].device_type <> IDE_DISK) then
  		BX_PANIC(('read verify issued to non-disk'));
            BX_INFO(('Verify Command : $40 ! '));
            bx_hard_drive.s[0].controller.status.busy := 0;
            bx_hard_drive.s[0].controller.status.drive_ready := 1;
            bx_hard_drive.s[0].controller.status.drq := 0;
            bx_hard_drive.s[0].controller.status.err := 0;
	    raise_interrupt();
            end
          else begin
	    BX_INFO(('sent READ VERIFY SECTORS ($40) to old hard drive'));
            command_aborted(value);
	  end;
    end;

	$c6: // SET MULTIPLE mod_E (mch)
    begin
	    if Boolean((bx_hard_drive.s[0].controller.beta.sector_count <> 128) and
		  (bx_hard_drive.s[0].controller.beta.sector_count <> 64) and
		  (bx_hard_drive.s[0].controller.beta.sector_count <> 32) and
		  (bx_hard_drive.s[0].controller.beta.sector_count <> 16) and
		  (bx_hard_drive.s[0].controller.beta.sector_count <> 8) and
		  (bx_hard_drive.s[0].controller.beta.sector_count <> 4) and
		  (bx_hard_drive.s[0].controller.beta.sector_count <> 2)) then
		    command_aborted(value);

	      if Boolean(s[self.drive_select].device_type <> IDE_DISK) then
    		BX_PANIC(('set multiple mod_e issued to non-disk'));

	      bx_hard_drive.s[0].controller.sectors_per_block := bx_hard_drive.s[0].controller.beta.sector_count;
	      bx_hard_drive.s[0].controller.status.busy := 0;
	      bx_hard_drive.s[0].controller.status.drive_ready := 1;
	      bx_hard_drive.s[0].controller.status.write_fault := 0;
	      bx_hard_drive.s[0].controller.status.drq := 0;
	      bx_hard_drive.s[0].controller.status.err := 0;
    end;

        // ATAPI commands
        $a1: // IDENTIFY PACKET DEVICE
          begin
            if Boolean(s[self.drive_select].device_type = IDE_CDROM) then begin
            bx_hard_drive.s[0].controller.current_command := value;
            bx_hard_drive.s[0].controller.error_register := 0;

            bx_hard_drive.s[0].controller.status.busy := 0;
            bx_hard_drive.s[0].controller.status.drive_ready := 1;
            bx_hard_drive.s[0].controller.status.write_fault := 0;
            bx_hard_drive.s[0].controller.status.drq   := 1;
            bx_hard_drive.s[0].controller.status.err   := 0;

            bx_hard_drive.s[0].controller.status.seek_complete := 1;
            bx_hard_drive.s[0].controller.status.corrected_data := 0;

            bx_hard_drive.s[0].controller.buffer_index := 0;
            raise_interrupt();
            identify_ATAPI_drive(self.drive_select);
            end else begin
            command_aborted($a1);
            end;
	      end;

        $08: // DEVICE RESET (atapi)
          begin
            if Boolean(s[self.drive_select].device_type = IDE_CDROM) then begin
            bx_hard_drive.s[0].controller.status.busy := 1;
            //bx_hard_drive.s[0].controller.error_register @:= ~(1 shl 7); !!!
            bx_hard_drive.s[0].controller.error_register :=
              bx_hard_drive.s[0].controller.error_register and not (1 shl 7);

            // device signature
            bx_hard_drive.s[0].controller.head_no        := 0;
            bx_hard_drive.s[0].controller.beta.sector_count   := 1;
            bx_hard_drive.s[0].controller.sector_no      := 1;
            bx_hard_drive.s[0].controller.alfa.cylinder_no    := $eb14;

            bx_hard_drive.s[0].controller.status.write_fault := 0;
            bx_hard_drive.s[0].controller.status.drq := 0;
            bx_hard_drive.s[0].controller.status.corrected_data := 0;
            bx_hard_drive.s[0].controller.status.err := 0;

            bx_hard_drive.s[0].controller.status.busy := 0;

            end else begin
        command_aborted($08);
            end;
         end;
        $a0: // SEND PACKET (atapi)
          begin
            if Boolean(s[self.drive_select].device_type = IDE_CDROM) then begin
            // PACKET
            if Boolean(bx_hard_drive.s[0].controller.features  and (1 shl 0)) then
              BX_PANIC(('PACKET-DMA not supported'));
            if Boolean(bx_hard_drive.s[0].controller.features  and (1 shl 1)) then
              BX_PANIC(('PACKET-overlapped not supported'));

            // We're already ready!
            bx_hard_drive.s[0].controller.beta.sector_count := 1;
            bx_hard_drive.s[0].controller.status.busy := 0;
            bx_hard_drive.s[0].controller.status.write_fault := 0;
            // serv bit??
            bx_hard_drive.s[0].controller.status.drq := 1;
            bx_hard_drive.s[0].controller.status.err := 0;

            // NOTE: no interrupt here
            bx_hard_drive.s[0].controller.current_command := value;
            bx_hard_drive.s[0].controller.buffer_index := 0;
            end else begin
        command_aborted ($a0);
            end;
          end;

        $a2: // SERVICE (atapi), optional
          begin
    	      if Boolean(s[self.drive_select].device_type = IDE_CDROM) then begin
		          BX_PANIC(('ATAPI SERVICE not implemented'));
	          end else begin
          		command_aborted ($a2);
    	      end;
	        end;

        // power management
	$e5: // CHECK POWER mod_E
    begin
	  bx_hard_drive.s[0].controller.status.busy := 0;
	  bx_hard_drive.s[0].controller.status.drive_ready := 1;
	  bx_hard_drive.s[0].controller.status.write_fault := 0;
	  bx_hard_drive.s[0].controller.status.drq := 0;
	  bx_hard_drive.s[0].controller.status.err := 0;
	  bx_hard_drive.s[0].controller.beta.sector_count := $ff; // Active or Idle mod_e
	  raise_interrupt();
	  end;

	// List all the write operations that are defined in the ATA/ATAPI spec
	// that we don't support.  Commands that are listed here will cause a
	// BX_ERROR, which is non-fatal, and the command will be aborted.
	$22: begin BX_ERROR(('write cmd $22 (READ LONG) not supported')); command_aborted($22); end;
	$23: begin BX_ERROR(('write cmd $23 (READ LONG NO RETRY) not supported')); command_aborted($23); end;
	$24: begin BX_ERROR(('write cmd $24 (READ SECTORS EXT) not supported'));command_aborted($24); end;
	$25: begin BX_ERROR(('write cmd $25 (READ DMA EXT) not supported'));command_aborted($25); end;
	$26: begin BX_ERROR(('write cmd $26 (READ DMA QUEUED EXT) not supported'));command_aborted($26); end;
	$27: begin BX_ERROR(('write cmd $27 (READ NATIVE MAX ADDRESS EXT) not supported'));command_aborted($27); end;
	$29: begin BX_ERROR(('write cmd $29 (READ MULTIPLE EXT) not supported'));command_aborted($29); end;
	$2A: begin BX_ERROR(('write cmd $2A (READ STREAM DMA) not supported'));command_aborted($2A); end;
	$2B: begin BX_ERROR(('write cmd $2B (READ STREAM PIO) not supported'));command_aborted($2B); end;
	$2F: begin BX_ERROR(('write cmd $2F (READ LOG EXT) not supported'));command_aborted($2F); end;
	$31: begin BX_ERROR(('write cmd $31 (WRITE SECTORS NO RETRY) not supported')); command_aborted($31); end;
	$32: begin BX_ERROR(('write cmd $32 (WRITE LONG) not supported')); command_aborted($32); end;
	$33: begin BX_ERROR(('write cmd $33 (WRITE LONG NO RETRY) not supported')); command_aborted($33); end;
	$34: begin BX_ERROR(('write cmd $34 (WRITE SECTORS EXT) not supported'));command_aborted($34); end;
	$35: begin BX_ERROR(('write cmd $35 (WRITE DMA EXT) not supported'));command_aborted($35); end;
	$36: begin BX_ERROR(('write cmd $36 (WRITE DMA QUEUED EXT) not supported'));command_aborted($36); end;
	$37: begin BX_ERROR(('write cmd $37 (SET MAX ADDRESS EXT) not supported'));command_aborted($37); end;
	$38: begin BX_ERROR(('write cmd $38 (CFA WRITE SECTORS W/OUT ERASE) not supported'));command_aborted($38); end;
	$39: begin BX_ERROR(('write cmd $39 (WRITE MULTIPLE EXT) not supported'));command_aborted($39); end;
	$3A: begin BX_ERROR(('write cmd $3A (WRITE STREAM DMA) not supported'));command_aborted($3A); end;
	$3B: begin BX_ERROR(('write cmd $3B (WRITE STREAM PIO) not supported'));command_aborted($3B); end;
	$3F: begin BX_ERROR(('write cmd $3F (WRITE LOG EXT) not supported'));command_aborted($3F); end;
	$41: begin BX_ERROR(('write cmd $41 (READ VERIFY SECTORS NO RETRY) not supported')); command_aborted($41); end;
	$42: begin BX_ERROR(('write cmd $42 (READ VERIFY SECTORS EXT) not supported'));command_aborted($42); end;
	$50: begin BX_ERROR(('write cmd $50 (FORMAT TRACK) not supported')); command_aborted($50); end;
	$51: begin BX_ERROR(('write cmd $51 (CONFIGURE STREAM) not supported'));command_aborted($51); end;
	$70: begin BX_ERROR(('write cmd $70 (SEEK) not supported'));command_aborted($70); end;
	$87: begin BX_ERROR(('write cmd $87 (CFA TRANSLATE SECTOR) not supported'));command_aborted($87); end;
	$92: begin BX_ERROR(('write cmd $92 (DOWNLOAD MICROCODE) not supported'));command_aborted($92); end;
	$94: begin BX_ERROR(('write cmd $94 (STANDBY IMMEDIATE) not supported')); command_aborted($94); end;
	$95: begin BX_ERROR(('write cmd $95 (IDLE IMMEDIATE) not supported')); command_aborted($95); end;
	$96: begin BX_ERROR(('write cmd $96 (STANDBY) not supported')); command_aborted($96); end;
	$97: begin BX_ERROR(('write cmd $97 (IDLE) not supported')); command_aborted($97); end;
	$98: begin BX_ERROR(('write cmd $98 (CHECK POWER mod_E) not supported')); command_aborted($98); end;
	$99: begin BX_ERROR(('write cmd $99 (SLEEP) not supported')); command_aborted($99); end;
	$B0: begin BX_ERROR(('write cmd $B0 (SMART commands) not supported'));command_aborted($B0); end;
	$B1: begin BX_ERROR(('write cmd $B1 (DEVICE CONFIGURATION commands) not supported'));command_aborted($B1); end;
	$C0: begin BX_ERROR(('write cmd $C0 (CFA ERASE SECTORS) not supported'));command_aborted($C0); end;
	$C4: begin BX_ERROR(('write cmd $C4 (READ MULTIPLE) not supported'));command_aborted($C4); end;
	$C5: begin BX_ERROR(('write cmd $C5 (WRITE MULTIPLE) not supported'));command_aborted($C5); end;
	$C7: begin BX_ERROR(('write cmd $C7 (READ DMA QUEUED) not supported'));command_aborted($C7); end;
	$C8: begin BX_ERROR(('write cmd $C8 (READ DMA) not supported'));command_aborted($C8); end;
	$C9: begin BX_ERROR(('write cmd $C9 (READ DMA NO RETRY) not supported')); command_aborted($C9); end;
	$CA: begin BX_ERROR(('write cmd $CA (WRITE DMA) not supported'));command_aborted($CA); end;
	$CC: begin BX_ERROR(('write cmd $CC (WRITE DMA QUEUED) not supported'));command_aborted($CC); end;
	$CD: begin BX_ERROR(('write cmd $CD (CFA WRITE MULTIPLE W/OUT ERASE) not supported'));command_aborted($CD); end;
	$D1: begin BX_ERROR(('write cmd $D1 (CHECK MEDIA CARD TYPE) not supported'));command_aborted($D1); end;
	$DA: begin BX_ERROR(('write cmd $DA (GET MEDIA STATUS) not supported'));command_aborted($DA); end;
	$DE: begin BX_ERROR(('write cmd $DE (MEDIA LOCK) not supported'));command_aborted($DE); end;
	$DF: begin BX_ERROR(('write cmd $DF (MEDIA UNLOCK) not supported'));command_aborted($DF); end;
	$E0: begin BX_ERROR(('write cmd $E0 (STANDBY IMMEDIATE) not supported'));command_aborted($E0); end;
	$E1: begin BX_ERROR(('write cmd $E1 (IDLE IMMEDIATE) not supported'));command_aborted($E1); end;
	$E2: begin BX_ERROR(('write cmd $E2 (STANDBY) not supported'));command_aborted($E2); end;
	$E3: begin BX_ERROR(('write cmd $E3 (IDLE) not supported'));command_aborted($E3); end;
	$E4: begin BX_ERROR(('write cmd $E4 (READ BUFFER) not supported'));command_aborted($E4); end;
	$E6: begin BX_ERROR(('write cmd $E6 (SLEEP) not supported'));command_aborted($E6); end;
        $E7: begin BX_ERROR(('write cmd $E7 (FLUSH CACHE) not supported'));command_aborted($E7); end;
	$E8: begin BX_ERROR(('write cmd $E8 (WRITE BUFFER) not supported'));command_aborted($E8); end;
	$EA: begin BX_ERROR(('write cmd $EA (FLUSH CACHE EXT) not supported'));command_aborted($EA); end;
	$ED: begin BX_ERROR(('write cmd $ED (MEDIA EJECT) not supported'));command_aborted($ED); end;
	$F1: begin BX_ERROR(('write cmd $F1 (SECURITY SET PASSWORD) not supported'));command_aborted($F1); end;
	$F2: begin BX_ERROR(('write cmd $F2 (SECURITY UNLOCK) not supported'));command_aborted($F2); end;
	$F3: begin BX_ERROR(('write cmd $F3 (SECURITY ERASE PREPARE) not supported'));command_aborted($F3); end;
	$F4: begin BX_ERROR(('write cmd $F4 (SECURITY ERASE UNIT) not supported'));command_aborted($F4); end;
	$F5: begin BX_ERROR(('write cmd $F5 (SECURITY FREEZE LOCK) not supported'));command_aborted($F5); end;
	$F6: begin BX_ERROR(('write cmd $F6 (SECURITY DISABLE PASSWORD) not supported'));command_aborted($F6); end;
	$F8: begin BX_ERROR(('write cmd $F8 (READ NATIVE MAX ADDRESS) not supported'));command_aborted($F8); end;
	$F9: begin BX_ERROR(('write cmd $F9 (SET MAX ADDRESS) not supported'));command_aborted($F9); end;

	else
    begin
          BX_PANIC(Format('IO write(1f7h): command $%02x',[value]));
	  // if user foolishly decides to continue, abort the command
	  // so that the software knows the drive didn't understand it.
          command_aborted(value);
        end;
        end;end;

    $3f6: // hard disk adapter control
	  // (mch) Even if device 1 was selected, a write to self register
	  // goes to device 0 (if device 1 is absent)
      begin
    	  prev_control_reset := bx_hard_drive.s[0].controller.control.reset;
    	  self.s[0].controller.control.reset         := value  and $04;
    	  self.s[1].controller.control.reset         := value  and $04;
	      bx_hard_drive.s[0].controller.control.disable_irq    := value  and $02;
      //BX_DEBUG(( 'adpater control reg: reset controller := %d',
      //  (unsigned) (bx_hard_drive.s[0].controller.control.reset) ? 1 : 0 ));
      //BX_DEBUG(( 'adpater control reg: disable_irq(14) := %d',
      //  (unsigned) (bx_hard_drive.s[0].controller.control.disable_irq) ? 1 : 0 ));
    	  if Boolean((prev_control_reset=0) and (bx_hard_drive.s[0].controller.control.reset<>0)) then begin
		// transition from 0 to 1 causes all drives to reset
		    BX_DEBUG(('hard drive: RESET'));

		// (mch) Set BSY, drive not ready
      id:=0;
  		while id < 2 do
        begin
		      s[0].controller.status.busy           := 1;
		      s[0].controller.status.drive_ready    := 0;
		      s[0].controller.reset_in_progress     := 1;

		      s[0].controller.status.write_fault    := 0;
		      s[0].controller.status.seek_complete  := 1;
		      s[0].controller.status.drq            := 0;
		      s[0].controller.status.corrected_data := 0;
		      s[0].controller.status.err            := 0;

		      s[0].controller.error_register := $01; // diagnostic code: no error

		      s[0].controller.current_command := $00;
		      s[0].controller.buffer_index := 0;

		      s[0].controller.sectors_per_block := $80;
		      s[0].controller.lba_mode          := 0;

		      s[0].controller.control.disable_irq := 0;
          inc(id);
		end;
	  end else if Boolean((bx_hard_drive.s[0].controller.reset_in_progress<>0) and
		     (bx_hard_drive.s[0].controller.control.reset=0)) then begin
		// Clear BSY and DRDY
      id:=0;
      while id < 2 do
  		  begin
		      s[0].controller.status.busy           := 0;
		      s[0].controller.status.drive_ready    := 1;
		      s[0].controller.reset_in_progress     := 0;

		      // Device signature
		      if Boolean(self.s[id].device_type = IDE_DISK) then begin
			    s[0].controller.head_no        := 0;
			    s[0].controller.beta.sector_count   := 1;
			    s[0].controller.sector_no      := 1;
			    s[0].controller.alfa.cylinder_no    := 0;
		      end else begin
			    s[0].controller.head_no        := 0;
			    s[0].controller.beta.sector_count   := 1;
			    s[0].controller.sector_no      := 1;
			    s[0].controller.alfa.cylinder_no    := $eb14;
		      end;
          inc(id);
		end;
    end;
end;
end;
end;

procedure bx_hard_drive_c.close_harddrive;
begin
  self.s[0].hard_drive.close;
  self.s[1].hard_drive.close;
end;


function bx_hard_drive_c.calculate_logical_address(var sector:bit32u):Bool;
var
  logical_sector:Bit32u;
begin

      if Boolean(bx_hard_drive.s[0].controller.lba_mode) then begin
            //bx_printf ('disk: calculate: %d %d %d\n', ((Bit32u)bx_hard_drive.s[0].controller.head_no), ((Bit32u)bx_hard_drive.s[0].controller.alfa.cylinder_no), (Bit32u)bx_hard_drive.s[0].controller.sector_no);
	    logical_sector := (Bit32u(bx_hard_drive.s[0].controller.head_no)) shl 24 or
		  (Bit32u(bx_hard_drive.s[0].controller.alfa.cylinder_no)) shl 8 or
		  Bit32u(bx_hard_drive.s[0].controller.sector_no);
            //bx_printf ('disk: result: %u\n', logical_sector);
      end else
	    logical_sector := (bx_hard_drive.s[0].controller.alfa.cylinder_no * s[self.drive_select].hard_drive.heads *
			      s[self.drive_select].hard_drive.sectors) +
		  (bx_hard_drive.s[0].controller.head_no * s[self.drive_select].hard_drive.sectors) +
		  (bx_hard_drive.s[0].controller.sector_no - 1);

      if Boolean(logical_sector >= (s[self.drive_select].hard_drive.cylinders * s[self.drive_select].hard_drive.heads * s[self.drive_select].hard_drive.sectors)) then begin
            BX_ERROR (('calc_log_addr: out of bounds'));
	    Result:=0;
      exit;
      end;
      sector := logical_sector;
	    Result:=1;
      exit;
end;

procedure bx_hard_drive_c.increment_address;
var
  current_address:Bit32u;
begin
      bx_hard_drive.s[0].controller.beta.sector_count:=bx_hard_drive.s[0].controller.beta.sector_count-1;

      if Boolean(bx_hard_drive.s[0].controller.lba_mode) then begin
	    calculate_logical_address(current_address);
	    inc(current_address);
	    bx_hard_drive.s[0].controller.head_no := (current_address shr 24)  and $f;
	    bx_hard_drive.s[0].controller.alfa.cylinder_no := (current_address shr 8)  and $ffff;
	    bx_hard_drive.s[0].controller.sector_no := (current_address)  and $ff;
      end else begin
            inc(bx_hard_drive.s[0].controller.sector_no);
            if Boolean(bx_hard_drive.s[0].controller.sector_no > s[self.drive_select].hard_drive.sectors) then begin
		  bx_hard_drive.s[0].controller.sector_no := 1;
		  inc(bx_hard_drive.s[0].controller.head_no);
		  if Boolean(bx_hard_drive.s[0].controller.head_no >= s[self.drive_select].hard_drive.heads) then begin
			bx_hard_drive.s[0].controller.head_no := 0;
			inc(bx_hard_drive.s[0].controller.alfa.cylinder_no);
			if Boolean(bx_hard_drive.s[0].controller.alfa.cylinder_no >= s[self.drive_select].hard_drive.cylinders) then
			      bx_hard_drive.s[0].controller.alfa.cylinder_no := s[self.drive_select].hard_drive.cylinders - 1;
		  end;
	    end;
      end;
end;

procedure bx_hard_drive_c.identify_ATAPI_drive(drive:unsigned);
const serial_number = ' VT00001\0\0\0\0\0\0\0\0\0\0\0\0';
const firmware = 'ALPHA1  ';
var
  i:unsigned;
  temp16:Bit16u ;
  _lenModelNo:Integer;
begin

  if Boolean(drive <> unsigned(self.drive_select)) then begin
	BX_PANIC(('identify_drive panic (drive <> drive_select)'));
  end;

  s[self.drive_select].id_drive[0] := (2 shl 14)or(5 shl 8)or(1 shl 7)or(2 shl 5)or(0 shl 0); // Removable CDROM, 50us response, 12 byte packets

  FillChar(s[self.drive_select].id_drive[i],256,0);

  i:=1;
  while i <= 9 do
    begin
    	s[self.drive_select].id_drive[i] := 0;
      inc(i);
    end;

  i:=0;
  while i < 10 do
    begin
    	s[self.drive_select].id_drive[10+i] := (byte(serial_number[i*2]) shl 8) or byte(serial_number[i*2 + 1]);
      inc(i);
    end;

  i:=20;
  while i<=22 do
    begin
    	s[self.drive_select].id_drive[i] := 0;
      inc(i);
    end;

  i:=0;
  while i < Length(firmware) div 2 do
    begin
    	s[self.drive_select].id_drive[23+i] := (byte(firmware[i*2]) shl 8) or byte(firmware[i*2 + 1]);
      inc(i);
    end;
  assert((23+i) = 27);
  i:=0;
  _lenModelNo:=strlen(model_no);
  while i < (_lenModelNo div 2) do
    begin
    	s[self.drive_select].id_drive[27+i] := (byte(model_no[i*2]) shl 8) or
	      (byte(model_no[i*2 + 1]));
    inc(i);
  end;
  assert((27+i) = 47);

  s[self.drive_select].id_drive[47] := 0;
  s[self.drive_select].id_drive[48] := 0;

  s[self.drive_select].id_drive[49] := (1 shl 9); // LBA supported

  s[self.drive_select].id_drive[50] := 0;
  s[self.drive_select].id_drive[51] := 0;
  s[self.drive_select].id_drive[52] := 0;

  s[self.drive_select].id_drive[53] := 3; // words 64-70, 54-58 valid
  i:=54;
  while i <= 62 do
    begin
    	s[self.drive_select].id_drive[i] := 0;
      inc(i);
    end;

  // copied from CFA540A
  s[self.drive_select].id_drive[63] := $0103; // variable (DMA stuff)
  s[self.drive_select].id_drive[64] := $0001; // PIO
  s[self.drive_select].id_drive[65] := $00b4;
  s[self.drive_select].id_drive[66] := $00b4;
  s[self.drive_select].id_drive[67] := $012c;
  s[self.drive_select].id_drive[68] := $00b4;

  s[self.drive_select].id_drive[69] := 0;
  s[self.drive_select].id_drive[70] := 0;
  s[self.drive_select].id_drive[71] := 30; // faked
  s[self.drive_select].id_drive[72] := 30; // faked
  s[self.drive_select].id_drive[73] := 0;
  s[self.drive_select].id_drive[74] := 0;

  s[self.drive_select].id_drive[75] := 0;

  i:=76;
  while i <= 79 do
    begin
    	s[self.drive_select].id_drive[i] := 0;
      inc(i);
    end;

  s[self.drive_select].id_drive[80] := $1e; // supports up to ATA/ATAPI-4
  s[self.drive_select].id_drive[81] := 0;
  s[self.drive_select].id_drive[82] := 0;
  s[self.drive_select].id_drive[83] := 0;
  s[self.drive_select].id_drive[84] := 0;
  s[self.drive_select].id_drive[85] := 0;
  s[self.drive_select].id_drive[86] := 0;
  s[self.drive_select].id_drive[87] := 0;
  s[self.drive_select].id_drive[88] := 0;

  i:=89;
  while i <= 126 do
    begin
    	s[self.drive_select].id_drive[i] := 0;
      inc(i);
    end;

  s[self.drive_select].id_drive[127] := 0;
  s[self.drive_select].id_drive[128] := 0;

  i:=129;
  while i <= 159 do
    begin
    	s[self.drive_select].id_drive[i] := 0;
      inc(i);
    end;

  i:=160;
  while i <= 255 do
    begin
    	s[self.drive_select].id_drive[i] := 0;
      inc(i);
    end;

  // now convert the id_drive array (native 256 word format) to
  // the controller buffer (512 bytes)
  i:=0;
  while i <= 255 do begin
  	temp16 := s[self.drive_select].id_drive[i];
	  bx_hard_drive.s[0].controller.buffer[i*2] := temp16  and $00ff;
  	bx_hard_drive.s[0].controller.buffer[i*2+1] := temp16 shr 8;
    Inc(i);
  end;
end;

procedure bx_hard_drive_c.identify_drive(drive:unsigned);
var
  i:unsigned;
  temp32:Bit32u;
  temp16:Bit16u;
  num_sects:Bit32u;
begin

  if Boolean(drive <> self.drive_select) then begin
	BX_PANIC(('identify_drive panic (drive <> drive_select)'));
  end;

{$if CONNER_CFA540A = 1}
  s[self.drive_select].id_drive[0] := $0c5a;
  s[self.drive_select].id_drive[1] := $0418;
  s[self.drive_select].id_drive[2] := 0;
  s[self.drive_select].id_drive[3] := s[self.drive_select].hard_drive^.heads;
  s[self.drive_select].id_drive[4] := $9fb7;
  s[self.drive_select].id_drive[5] := $0289;
  s[self.drive_select].id_drive[6] := s[self.drive_select].hard_drive^.sectors;
  s[self.drive_select].id_drive[7] := $0030;
  s[self.drive_select].id_drive[8] := $000a;
  s[self.drive_select].id_drive[9] := $0000;

  char* serial_number := ' CA00GSQ\0\0\0\0\0\0\0\0\0\0\0\0';
  for (i := 0; i < 10; i++) then begin
	s[self.drive_select].id_drive[10+i] := (serial_number[i*2] shl 8) |
	      serial_number[i*2 + 1];
  end;

  s[self.drive_select].id_drive[20] := 3;
  s[self.drive_select].id_drive[21] := 512; // 512 Sectors := 256kB cache
  s[self.drive_select].id_drive[22] := 4;

  char* firmware := '8FT054  ';
  for (i := 0; i < strlen(firmware)/2; i++) then begin
	s[self.drive_select].id_drive[23+i] := (firmware[i*2] shl 8) |
	      firmware[i*2 + 1];
  end;
  BX_ASSERT((23+i) = 27);

  char* mod_el := 'Conner Peripherals 540MB - CFA540A      ';
  for (i := 0; i < strlen(mod_el)/2; i++) then begin
	s[self.drive_select].id_drive[27+i] := (mod_el[i*2] shl 8) |
	      mod_el[i*2 + 1];
  end;
  BX_ASSERT((27+i) = 47);

  s[self.drive_select].id_drive[47] := $8080; // multiple mod_e identification
  s[self.drive_select].id_drive[48] := 0;
  s[self.drive_select].id_drive[49] := $0f01;

  s[self.drive_select].id_drive[50] := 0;

  s[self.drive_select].id_drive[51] := 0;
  s[self.drive_select].id_drive[52] := $0002;
  s[self.drive_select].id_drive[53] := $0003;
  s[self.drive_select].id_drive[54] := $0418;

  s[self.drive_select].id_drive[55] := s[self.drive_select].hard_drive^.heads;
  s[self.drive_select].id_drive[56] := s[self.drive_select].hard_drive^.sectors;

  s[self.drive_select].id_drive[57] := $1e80;
  s[self.drive_select].id_drive[58] := $0010;
  s[self.drive_select].id_drive[59] := $0100orbx_hard_drive.s[0].controller.sectors_per_block;
  s[self.drive_select].id_drive[60] := $20e0;
  s[self.drive_select].id_drive[61] := $0010;

  s[self.drive_select].id_drive[62] := 0;

  s[self.drive_select].id_drive[63] := $0103; // variable (DMA stuff)
  s[self.drive_select].id_drive[64] := $0001; // PIO
  s[self.drive_select].id_drive[65] := $00b4;
  s[self.drive_select].id_drive[66] := $00b4;
  s[self.drive_select].id_drive[67] := $012c;
  s[self.drive_select].id_drive[68] := $00b4;

  for (i := 69; i <= 79; i++)
	s[self.drive_select].id_drive[i] := 0;

  s[self.drive_select].id_drive[80] := 0;

  s[self.drive_select].id_drive[81] := 0;

  s[self.drive_select].id_drive[82] := 0;
  s[self.drive_select].id_drive[83] := 0;
  s[self.drive_select].id_drive[84] := 0;
  s[self.drive_select].id_drive[85] := 0;
  s[self.drive_select].id_drive[86] := 0;
  s[self.drive_select].id_drive[87] := 0;

  for (i := 88; i <= 127; i++)
	s[self.drive_select].id_drive[i] := 0;

  s[self.drive_select].id_drive[128] := $0418;
  s[self.drive_select].id_drive[129] := $103f;
  s[self.drive_select].id_drive[130] := $0418;
  s[self.drive_select].id_drive[131] := $103f;
  s[self.drive_select].id_drive[132] := $0004;
  s[self.drive_select].id_drive[133] := $ffff;
  s[self.drive_select].id_drive[134] := 0;
  s[self.drive_select].id_drive[135] := $5050;

  for (i := 136; i <= 144; i++)
	s[self.drive_select].id_drive[i] := 0;

  s[self.drive_select].id_drive[145] := $302e;
  s[self.drive_select].id_drive[146] := $3245;
  s[self.drive_select].id_drive[147] := $2020;
  s[self.drive_select].id_drive[148] := $2020;

  for (i := 149; i <= 255; i++)
	s[self.drive_select].id_drive[i] := 0;

{$else}

  // Identify Drive command return values definition
  //
  // self code is rehashed from some that was donated.
  // I'm using ANSI X3.221-1994, AT Attachment Interface for Disk Drives
  // and X3T10 2008D Working Draft for ATA-3


  // Word 0: general config bit-significant info
  //   Note: bits 1-5 and 8-14 are now 'Vendor specific (obsolete)'
  //   bit 15: 0:=ATA device
  //           1:=ATAPI device
  //   bit 14: 1:=format speed tolerance gap required
  //   bit 13: 1:=track offset option available
  //   bit 12: 1:=data strobe offset option available
  //   bit 11: 1:=rotational speed tolerance is > 0,5% (typo?)
  //   bit 10: 1:=disk transfer rate > 10Mbs
  //   bit  9: 1:=disk transfer rate > 5Mbs but <= 10Mbs
  //   bit  8: 1:=disk transfer rate <= 5Mbs
  //   bit  7: 1:=removable cartridge drive
  //   bit  6: 1:=fixed drive
  //   bit  5: 1:=spindle motor control option implemented
  //   bit  4: 1:=head switch time > 15 usec
  //   bit  3: 1:=not MFM encoded
  //   bit  2: 1:=soft sectored
  //   bit  1: 1:=hard sectored
  //   bit  0: 0:=reserved
  s[self.drive_select].id_drive[0] := $0040;

  // Word 1: number of user-addressable cylinders in
  //   default translation mod_e.  If the value in words 60-61
  //   exceed 16,515,072, self word shall contain 16,383.
  s[self.drive_select].id_drive[1] := s[self.drive_select].hard_drive.cylinders;

  // Word 2: reserved
  s[self.drive_select].id_drive[2] := 0;

  // Word 3: number of user-addressable heads in default
  //   translation mod_e
  s[self.drive_select].id_drive[3] := s[self.drive_select].hard_drive.heads;

  // Word 4: # unformatted bytes per translated track in default xlate mod_e
  // Word 5: # unformatted bytes per sector in default xlated mod_e
  // Word 6: # user-addressable sectors per track in default xlate mod_e
  // Note: words 4,5 are now 'Vendor specific (obsolete)'
  s[self.drive_select].id_drive[4] := (512 * s[self.drive_select].hard_drive.sectors);
  s[self.drive_select].id_drive[5] := 512;
  s[self.drive_select].id_drive[6] := s[self.drive_select].hard_drive.sectors;

  // Word 7-9: Vendor specific
  i:=7;
  while i<=9 do
    begin
      s[self.drive_select].id_drive[i] := 0;
      inc(i);
    end;

  // Word 10-19: Serial number (20 ASCII characters, 0000h:=not specified)
  // self field is right justified and padded with spaces (20h).
  i:=10;

  while i<=19 do
    begin
      s[self.drive_select].id_drive[i] := 0;
      Inc(I);
    end;

  // Word 20: buffer type
  //          0000h := not specified
  //          0001h := single ported single sector buffer which is
  //                  not capable of simulataneous data xfers to/from
  //                  the host and the disk.
  //          0002h := dual ported multi-sector buffer capable of
  //                  simulatenous data xfers to/from the host and disk.
  //          0003h := dual ported mutli-sector buffer capable of
  //                  simulatenous data xfers with a read caching
  //                  capability.
  //          0004h-ffffh := reserved
  s[self.drive_select].id_drive[20] := 3;

  // Word 21: buffer size in 512 byte increments, 0000h := not specified
  s[self.drive_select].id_drive[21] := 512; // 512 Sectors := 256kB cache

  // Word 22: # of ECC bytes available on read/write long cmds
  //          0000h := not specified
  s[self.drive_select].id_drive[22] := 4;

  // Word 23..26: Firmware revision (8 ascii chars, 0000h:=not specified)
  // self field is left justified and padded with spaces (20h)
  i:=23;
  while i<=26 do
    begin
      s[self.drive_select].id_drive[i] := 0;
      inc(i);
    end;

  // Word 27..46: mod_el number (40 ascii chars, 0000h:=not specified)
  // self field is left justified and padded with spaces (20h)
//  for (i:=27; i<=46; i++)
//    s[self.drive_select].id_drive[i] := 0;
  i:=0;
  while i<20 do begin
    s[self.drive_select].id_drive[27+i] := (Bit8u(model_no[i*2]) shl 8) or
                                  Bit8u(model_no[i*2 + 1]);
    inc(i);
    end;

  // Word 47: 15-8 Vendor unique
  //           7-0 00h:= read/write multiple commands not implemented
  //               xxh:= maximum # of sectors that can be transferred
  //                    per interrupt on read and write multiple commands
  s[self.drive_select].id_drive[47] := max_multiple_sectors;

  // Word 48: 0000h := cannot perform dword IO
  //          0001h := can    perform dword IO
  s[self.drive_select].id_drive[48] := 1;

  // Word 49: Capabilities
  //   15-10: 0 := reserved
  //       9: 1 := LBA supported
  //       8: 1 := DMA supported
  //     7-0: Vendor unique
  s[self.drive_select].id_drive[49] := 0;

  // Word 50: Reserved
  s[self.drive_select].id_drive[50] := 0;

  // Word 51: 15-8 PIO data transfer cycle timing mod_e
  //           7-0 Vendor unique
  s[self.drive_select].id_drive[51] := $200;

  // Word 52: 15-8 DMA data transfer cycle timing mod_e
  //           7-0 Vendor unique
  s[self.drive_select].id_drive[52] := $200;

  // Word 53: 15-1 Reserved
  //             0 1:=the fields reported in words 54-58 are valid
  //               0:=the fields reported in words 54-58 may be valid
  s[self.drive_select].id_drive[53] := 0;

  // Word 54: # of user-addressable cylinders in curr xlate mod_e
  // Word 55: # of user-addressable heads in curr xlate mod_e
  // Word 56: # of user-addressable sectors/track in curr xlate mod_e
  s[self.drive_select].id_drive[54] := s[self.drive_select].hard_drive.cylinders;
  s[self.drive_select].id_drive[55] := s[self.drive_select].hard_drive.heads;
  s[self.drive_select].id_drive[56] := s[self.drive_select].hard_drive.sectors;

  // Word 57-58: Current capacity in sectors
  // Excludes all sectors used for device specific purposes.
  temp32 := 
    s[self.drive_select].hard_drive.cylinders *
    s[self.drive_select].hard_drive.heads *
    s[self.drive_select].hard_drive.sectors;
  s[self.drive_select].id_drive[57] := (temp32  and $ffff); // LSW
  s[self.drive_select].id_drive[58] := (temp32 shr 16);    // MSW

  // Word 59: 15-9 Reserved
  //             8 1:=multiple sector setting is valid
  //           7-0 current setting for number of sectors that can be
  //               transferred per interrupt on R/W multiple commands
  s[self.drive_select].id_drive[59] := $0000 or curr_multiple_sectors;

  // Word 60-61:
  // If drive supports LBA mod_e, these words reflect total # of user
  // addressable sectors.  self value does not depend on the current
  // drive geometry.  If the drive does not support LBA mod_e, these
  // words shall be set to 0.
  num_sects := s[self.drive_select].hard_drive.cylinders * s[self.drive_select].hard_drive.heads * s[self.drive_select].hard_drive.sectors;
  s[self.drive_select].id_drive[60] := num_sects  and $ffff; // LSW
  s[self.drive_select].id_drive[61] := num_sects shr 16; // MSW

  // Word 62: 15-8 single word DMA transfer mod_e active
  //           7-0 single word DMA transfer mod_es supported
  // The low order byte identifies by bit, all the mod_es which are
  // supported e.g., if mod_e 0 is supported bit 0 is set.
  // The high order byte contains a single bit set to indiciate
  // which mod_e is active.
  s[self.drive_select].id_drive[62] := $0;

  // Word 63: 15-8 multiword DMA transfer mod_e active
  //           7-0 multiword DMA transfer mod_es supported
  // The low order byte identifies by bit, all the mod_es which are
  // supported e.g., if mod_e 0 is supported bit 0 is set.
  // The high order byte contains a single bit set to indiciate
  // which mod_e is active.
  s[self.drive_select].id_drive[63] := $0;

  // Word 64-79 Reserved
  i:=64;
  while i<=79 do
    begin
      s[self.drive_select].id_drive[i] := 0;
      inc(i);
    end;

  // Word 80: 15-5 reserved
  //             4 supports ATA/ATAPI-4
  //             3 supports ATA-3
  //             2 supports ATA-2
  //             1 supports ATA-1
  //             0 reserved
  s[self.drive_select].id_drive[80] := (1 shl 2)or(1 shl 1);

  // Word 81: Minor version number
  s[self.drive_select].id_drive[81] := 0;

  // Word 82: 15 obsolete
  //          14 NOP command supported
  //          13 READ BUFFER command supported
  //          12 WRITE BUFFER command supported
  //          11 obsolete
  //          10 Host protected area feature set supported
  //           9 DEVICE RESET command supported
  //           8 SERVICE interrupt supported
  //           7 release interrupt supported
  //           6 look-ahead supported
  //           5 write cache supported
  //           4 supports PACKET command feature set
  //           3 supports power management feature set
  //           2 supports removable media feature set
  //           1 supports securite mod_e feature set
  //           0 support SMART feature set
  s[self.drive_select].id_drive[82] := 1 shl 14;
  s[self.drive_select].id_drive[83] := 1 shl 14;
  s[self.drive_select].id_drive[84] := 1 shl 14;
  s[self.drive_select].id_drive[85] := 1 shl 14;
  s[self.drive_select].id_drive[86] := 0;
  s[self.drive_select].id_drive[87] := 1 shl 14;

  i:=88;
  while i<=127 do
    begin
      s[self.drive_select].id_drive[i] := 0;
      inc(i);
    end;

  // Word 128-159 Vendor unique
  i:=128;
  while i<=159 do
    begin
      s[self.drive_select].id_drive[i] := 0;
      inc(i);
    end;

  // Word 160-255 Reserved
  i:=160;
  while i<=255 do
    begin
      s[self.drive_select].id_drive[i] := 0;
      inc(i);
    end;

{$ifend}


  // now convert the id_drive array (native 256 word format) to
  // the controller buffer (512 bytes)
  i:=0;
  while i<=255 do begin
    temp16 := s[self.drive_select].id_drive[i];
    bx_hard_drive.s[0].controller.buffer[i*2] := temp16  and $00ff;
    bx_hard_drive.s[0].controller.buffer[i*2+1] := temp16 shr 8;
    inc(i);
    end;
end;

  procedure
bx_hard_drive_c.init_send_atapi_command(command:Bit8u; req_length:integer; alloc_length:integer; lazy:bool = 0);
begin
      if Boolean(bx_hard_drive.s[0].controller.alfa.byte_count = 0) then
	    BX_PANIC(('ATAPI command with zero byte count'));

      if Boolean(alloc_length <= 0) then
	    BX_PANIC(('Allocation length <= 0'));

      bx_hard_drive.s[0].controller.beta.interrupt_reason.i_o := 1;
      bx_hard_drive.s[0].controller.beta.interrupt_reason.c_d := 0;
      bx_hard_drive.s[0].controller.status.busy := 0;
      bx_hard_drive.s[0].controller.status.drq := 1;
      bx_hard_drive.s[0].controller.status.err := 0;

      // no bytes transfered yet
      if Boolean(lazy) then
	    bx_hard_drive.s[0].controller.buffer_index := 2048
      else
	    bx_hard_drive.s[0].controller.buffer_index := 0;

      if Boolean(bx_hard_drive.s[0].controller.alfa.byte_count > req_length) then
	    bx_hard_drive.s[0].controller.alfa.byte_count := req_length;

      if Boolean(bx_hard_drive.s[0].controller.alfa.byte_count > alloc_length) then
	    bx_hard_drive.s[0].controller.alfa.byte_count := alloc_length;

      s[self.drive_select].atapi.command := command;
      s[self.drive_select].atapi.drq_bytes := bx_hard_drive.s[0].controller.alfa.byte_count;
      if req_length < alloc_length then
        s[self.drive_select].atapi.total_bytes_remaining := req_length
      else
        s[self.drive_select].atapi.total_bytes_remaining:=alloc_length;

      if Boolean(lazy) then begin
	    // bias drq_bytes and total_bytes_remaining
	    s[self.drive_select].atapi.drq_bytes := s[self.drive_select].atapi.drq_bytes + 2048;
	    s[self.drive_select].atapi.total_bytes_remaining :=
        s[self.drive_select].atapi.total_bytes_remaining + 2048;
      end;
end;

procedure
bx_hard_drive_c.atapi_cmd_error(sense_key:sense_t; asc:asc_t);
begin
      bx_hard_drive.s[0].controller.error_register := Ord(sense_key) shl 4;
      bx_hard_drive.s[0].controller.beta.interrupt_reason.i_o := 1;
      bx_hard_drive.s[0].controller.beta.interrupt_reason.c_d := 1;
      bx_hard_drive.s[0].controller.beta.interrupt_reason.rel := 0;
      bx_hard_drive.s[0].controller.status.busy := 0;
      bx_hard_drive.s[0].controller.status.drive_ready := 1;
      bx_hard_drive.s[0].controller.status.write_fault := 0;
      bx_hard_drive.s[0].controller.status.drq := 0;
      bx_hard_drive.s[0].controller.status.err := 1;

      s[self.drive_select].sense.sense_key := sense_key;
      s[self.drive_select].sense.asc := ord(asc);
      s[self.drive_select].sense.ascq := 0;
end;

procedure bx_hard_drive_c.atapi_cmd_nop;
begin
      bx_hard_drive.s[0].controller.beta.interrupt_reason.i_o := 1;
      bx_hard_drive.s[0].controller.beta.interrupt_reason.c_d := 1;
      bx_hard_drive.s[0].controller.beta.interrupt_reason.rel := 0;
      bx_hard_drive.s[0].controller.status.busy := 0;
      bx_hard_drive.s[0].controller.status.drive_ready := 1;
      bx_hard_drive.s[0].controller.status.drq := 0;
      bx_hard_drive.s[0].controller.status.err := 0;
end;

procedure
bx_hard_drive_c.init_mode_sense_single(src:pointer; size:Integer);
begin
      // Header
      bx_hard_drive.s[0].controller.buffer[0] := (size+6) shr 8;
      bx_hard_drive.s[0].controller.buffer[1] := (size+6)  and $ff;
      bx_hard_drive.s[0].controller.buffer[2] := $70; // no media present
      bx_hard_drive.s[0].controller.buffer[3] := 0; // reserved
      bx_hard_drive.s[0].controller.buffer[4] := 0; // reserved
      bx_hard_drive.s[0].controller.buffer[5] := 0; // reserved
      bx_hard_drive.s[0].controller.buffer[6] := 0; // reserved
      bx_hard_drive.s[0].controller.buffer[7] := 0; // reserved

      // Data
      move(src, bx_hard_drive.s[0].controller.buffer[8], size);
end;

procedure bx_hard_drive_c.ready_to_send_atapi;
begin
      raise_interrupt();
end;

procedure bx_hard_drive_c.raise_interrupt;
var
  irq:Bit32u;
begin
      if Boolean(bx_hard_drive.s[0].controller.control.disable_irq=0) then begin
	     irq:= 14;  // always 1st IDE controller
	    // for second controller, you would want irq 15
	    bx_pic.raise_irq(irq);
      end else begin
	    if Boolean(BX_DEBUG_HDD) then
  		  BX_INFO('Interrupt masked');
      end;
end;

procedure bx_hard_drive_c.command_aborted(command:unsigned);
begin
  bx_hard_drive.s[0].controller.current_command := 0;
  bx_hard_drive.s[0].controller.status.busy := 0;
  bx_hard_drive.s[0].controller.status.drive_ready := 1;
  bx_hard_drive.s[0].controller.status.err := 1;
  bx_hard_drive.s[0].controller.error_register := $04; // command ABORTED
  bx_hard_drive.s[0].controller.status.drq := 0;
  bx_hard_drive.s[0].controller.status.seek_complete := 0;
  bx_hard_drive.s[0].controller.status.corrected_data := 0;
  bx_hard_drive.s[0].controller.buffer_index := 0;
  raise_interrupt();
end;

function bx_hard_drive_c.get_cd_media_status:unsigned;
begin
  Result:= self.s[1].cdrom.ready ;
end;

function bx_hard_drive_c.set_cd_media_status(status:unsigned):unsigned;
begin
  // if setting to the current value, nothing to do
  if Boolean(status = self.s[1].cdrom.ready) then
    begin
      Result:=(status);
      exit;
    end;
  // return 0 if no cdromd is present
  if Boolean(BX_CDROM_PRESENT) then
    begin
      Result:=0;
      exit;
    end;

  if Boolean(status = 0) then begin
    // eject cdrom if not locked by guest OS
    if Boolean(self.s[1].cdrom.locked) then
    begin
      Result:=1;
      exit;
    end
    else begin
{$if LOWLEVEL_CDROM=1}
      self.s[1].cdrom.cd.eject_cdrom();
{$ifend}
      self.s[1].cdrom.ready := 0;
      end;
    end
  else begin
    // insert cdrom
{$if LOWLEVEL_CDROM=1}
    if Boolean(self.s[1].cdrom.cd.insert_cdrom(BX_CD_FILEPATH)) then begin
      BX_INFO(( 'Media present in CD-ROM drive'));
      self.s[1].cdrom.ready := 1;
      self.s[1].cdrom.capacity := self.s[1].cdrom.cd.capacity();
      //!*!bx_options.cdromd.Oinserted^.set(BX_INSERTED);
      s[self.drive_select].sense.sense_key := SENSE_UNIT_ATTENTION;
      s[self.drive_select].sense.asc := 0;
      s[self.drive_select].sense.ascq := 0;
      raise_interrupt();
      end
  else begin
{$ifend}
      BX_INFO(( 'Could not locate CD-ROM, continuing with media not present'));
      self.s[1].cdrom.ready := 0;
{$if LOWLEVEL_CDROM=1}
      end;
{$ifend}
    end;
  Result:=( self.s[1].cdrom.ready );
end;


(*** default_image_t function definitions ***)

function default_image_t.open(const pathname:String):integer;
begin
  assignfile(fd,pathname);
  reset(fd,1);

      (* look at size of image file to calculate disk geometry *)
   Result:=1;
end;

procedure default_image_t.close;
begin
  CloseFile(fd);
end;

function default_image_t.lseek(offset:off_t;whence:Integer):Bit32u;
begin
  seek(fd, offset);
  Result:=FilePos(fd);
end;

function default_image_t.read(var buf:array_buffer_disk; count:size_t):ssize_t;
var
   Readed:Integer;
begin
  BlockRead(fd, buf, count, Readed);
  Result:=Readed;
end;

function default_image_t.write (buf:array_buffer_disk; count:size_t):Bit32u;
var
  Written:Integer;
begin
{$if HDD_READ_ONLY = 0}
  blockwrite(fd, buf, count, Written);
  Result:=Written;
{$else}
  Result:=count;
{$ifend}
end;

{$if BX_SPLIT_HD_SUPPORT=1}
(*** concat_image_t function definitions ***)

procedure concat_image_t.increment_string (str:String);
var
  p:pchar;
begin
  // find the last character of the string, and increment it.
  p := pchar(str);
  while (p^ <> #0) do inc(p);
  dec(p);  // point to last character of the string
  byte(p^):=byte(p^)+1;  // increment to next ascii code.
  BX_DEBUG(Format('concat_image.increment string returning %s',[str]));
end;

{$ifend}

function read_16bit(buf:puint8):uint16;
var
  p1,p2:puint8;
begin
  p1:=buf;
  p2:=puint8(integer(p1)+1);
  Result:= (p1^ shl 8) or p2^;
end;

function read_32bit(buf:puint8):uint32;
begin
  Result:=(puint8(integer(buf))^ shl 24) or
     (puint8(integer(buf)+1)^ shl 16)
     or (puint8(integer(buf)+2)^ shl 8) or puint8(integer(buf)+3)^;
end;
end.
