/////////////////////////////////////////////////////////////////////////
// $Id: vga.cc,v 1.29 2002/03/16 11:37:43 japj Exp $
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
//  self library is free software; you can redistribute it and/or
//  mod_ify it under the terms of the GNU Lesser General Public
//  License as published by the Free Software Foundation; either
//  version 2 of the License, or (at your option) any later version.
//
//  self library is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  Lesser General Public License for more details.
//
//  You should have received a copy of the GNU Lesser General Public
//  License along with self library; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

(* ***** *)
unit vga;

interface

uses Config, iodev, cmos;

const
  VGA_MEMORY_SIZE                 = 256 * 1024;
  VBE_DISPI_TOTAL_VIDEO_MEMORY_MB = 4;

  VBE_DISPI_BANK_ADDRESS          = $A0000;
  VBE_DISPI_BANK_SIZE_KB          = 64;

  VBE_DISPI_MAX_XRES              = 1024;
  VBE_DISPI_MAX_YRES              = 768;

  VBE_DISPI_IOPORT_INDEX          = $FF80;
  VBE_DISPI_IOPORT_DATA           = $FF81;

  VBE_DISPI_INDEX_ID              = $0;
  VBE_DISPI_INDEX_XRES            = $1;
  VBE_DISPI_INDEX_YRES            = $2;
  VBE_DISPI_INDEX_BPP             = $3;
  VBE_DISPI_INDEX_ENABLE          = $4;
  VBE_DISPI_INDEX_BANK            = $5;

  VBE_DISPI_ID0                   = $B0C0;

  VBE_DISPI_BPP_8                 = $0;
// The following is not support yet, but just for reference available.
//  VBE_DISPI_BPP_RGB565            $1
//  VBE_DISPI_BPP_RGB555            $2

  VBE_DISPI_DISABLED              = $00;
  VBE_DISPI_ENABLED               = $01;

  BX_MAX_XRES = VBE_DISPI_MAX_XRES;
  BX_MAX_YRES = VBE_DISPI_MAX_YRES;


//CGA_TEXT_ADDR(row, column) ($18000 + ((row)*80 + (column))*2)

  X_TILESIZE = 16;
  Y_TILESIZE = 16;
  BX_NUM_X_TILES = (BX_MAX_XRES div X_TILESIZE);
  BX_NUM_Y_TILES = (BX_MAX_YRES div Y_TILESIZE);

// Support varying number of rows of text.  self used to
// be limited to only 25 lines.
  BX_MAX_TEXT_LINES = 260;


type
  PVga_c1=^TVga_c1;
  TVga_c1 = class
  public
    s :  record
      misc_output :  record
        color_emulation: Bool;  // 1=color emulation, base address = 3Dx
                                  // 0=mono emulation,  base address = 3Bx
        enable_ram: Bool;       // enable CPU access to video memory if set
        clock_select: Bit8u;     // 0=25Mhz 1=28Mhz
        select_high_bank: Bool; // when in odd/even modes, select
                                  // high 64k bank if set
        horiz_sync_pol: Bool;   // bit6:  negative if set
        vert_sync_pol: Bool;    // bit7:  negative if set
                                  //   bit7,bit6 represent number of lines on display:
                                  //   0 = reserved
                                  //   1 = 400 lines
                                  //   2 = 350 lines
                                  //   3 - 480 lines
      end;

      CRTC :  record
        address: Bit8u;
        reg: array[0..$19] of Bit8u;
      end;

      attribute_ctrl: record
        flip_flop: Bool; (* 0 = address, 1 = data-write *)
        address: unsigned;  (* register number *)
        video_enabled: Bool;
        palette_reg: array[0..16] of Bit8u;
        overscan_color: Bit8u;
        color_plane_enable: Bit8u;
        horiz_pel_panning: Bit8u;
        color_select: Bit8u;
        mode_ctrl :  record
          graphics_alpha: Bool;
          display_type: Bool;
          enable_line_graphics: Bool;
          blink_intensity: Bool;
          pixel_panning_compat: Bool;
          pixel_clock_select: Bool;
          internal_palette_size: Bool;
        end;
      end;

      pel: record
        write_data_register: Bit8u;
        write_data_cycle: Bit8u; (* 0, 1, 2 *)
        read_data_register: Bit8u;
        read_data_cycle: Bit8u; (* 0, 1, 2 *)
        data: array[0..256] of record
          red: Bit8u;
          green: Bit8u;
          blue: Bit8u;
        end;
        mask: Bit8u;
      end;


      graphics_ctrl :  record
        index: Bit8u;
        set_reset: Bit8u;
        enable_set_reset: Bit8u;
        color_compare: Bit8u;
        data_rotate: Bit8u;
        raster_op: Bit8u;
        read_map_select: Bit8u;
        write_mode: Bit8u;
        read_mode: Bool;
        odd_even: Bool;
        chain_odd_even: Bool;
        shift_reg: Bit8u;
        graphics_alpha: Bool;
        memory_mapping: Bit8u;  (* 0 = use A0000-BFFFF
                                 * 1 = use A0000-AFFFF EGA/VGA graphics modes
                                 * 2 = use B0000-B7FFF Monochrome modes
                                 * 3 = use B8000-BFFFF CGA modes
                                 *)
        color_dont_care: Bit8u;
        bitmask: Bit8u;
        latch: array[0..4] of Bit8u;
      end;

      sequencer :  record
        index: Bit8u;
        map_mask: Bit8u;
        map_mask_bit: array[0..4] of bit8u;
        bit0: Bool;
        bit1: Bool;
        reg1: Bit8u;
        char_map_select: Bit8u;
        extended_mem: Bool;
        odd_even: Bool;
        chain_four: Bool;
      end;

      vga_mem_updated: Bool;
      x_tilesize: unsigned;
      y_tilesize: unsigned;
      scan_bits: unsigned;
      vga_tile_updated: array[0..BX_NUM_X_TILES, 0..BX_NUM_Y_TILES] of Bool;
      vga_memory: array[0..VGA_MEMORY_SIZE] of Bit8u;
      text_snapshot: array[0..2 * 80 * BX_MAX_TEXT_LINES] of bit8u; // current text snapshot
      horiz_tick: unsigned;
      vert_tick: unsigned;
      rgb: array[0..3 * 256] of Bit8u;
      tile: array[0..X_TILESIZE * Y_TILESIZE] of Bit8u;

      vbe_memory: array[0..VBE_DISPI_TOTAL_VIDEO_MEMORY_MB * 1024 * 1024] of Bit8u;
      vbe_xres: Bit16u;
      vbe_yres: Bit16u;
      vbe_bpp: Bit16u;
      vbe_bank: Bit16u;
      vbe_enabled: Bool;
      vbe_curindex: Bit16u;
      vbe_visable_screen_size: Bit32u; // in bytes
    end;
    devices: pbx_devices_c;
    constructor Create;
    //destructor Destroy; override;
    procedure init(d:pbx_devices_c ; cmos:pbx_cmos_c);
    function  mem_read(addr:Bit32u):Bit8u;
    // Note: either leave value of type Bit8u, or mask it when
    //       used to 8 bits, in memory.cc
    procedure mem_write(addr:Bit32u; value:Bit8u);
    function  vbe_mem_read(addr:Bit32u):bit8u;
    procedure vbe_mem_write(addr:Bit32u; value:Bit8u);
    //procedure redraw_area(x0:unsigned; y0:unsigned; width:unsigned; height:unsigned);
  private
    function  read_handler(self_ptr:pointer; address:Bit32u; io_len:unsigned):Bit32u;
    procedure write_handler(self_ptr:Pointer; address:Bit32u; dvalue:Bit32u; io_len:unsigned);
    class procedure write_handler_no_log(self_ptr:Pointer; address:Bit32u; dvalue:Bit32u; io_len:unsigned);
    function  vbe_read_handler(self_ptr:Pointer; address: Bit32u; io_len:unsigned):Bit32u;
    procedure vbe_write_handler(self_ptr:pointer; address:Bit32u; value:Bit32u; io_len:unsigned);
    procedure write(address:Bit32u; value:Bit32u; io_len:unsigned; no_log:Bool);
  public
    timer_id: Integer;
    procedure timer_handler(self_ptr:pointer);
    procedure timer;
    procedure set_update_interval (interval:unsigned);
    //procedure  get_text_snapshot(Bit8u **text_snapshot, unsigned *txHeight,
    //                                                          unsigned *txWidth);
    procedure update;
    //procedure   dump_status;
    procedure determine_screen_dimensions(piHeight:punsigned;piWidth:punsigned);
  end;

var
   bx_vga: TVga_c1;
   old_iHeight, old_iWidth: unsigned;

function SetEvent(hEvent: THandle): BOOL; stdcall;
function SetEvent; external 'kernel32.dll' name 'SetEvent';

implementation

uses Service, SysUtils, cpu, Gui32;

function CGA_TEXT_ADDR(row:Integer; column:Integer):Integer;
begin
  Result:=($18000 + ((row) * 80 + (column)) * 2);
end;

constructor TVga_c1.Create;
begin
  old_iHeight := 0;
  old_iWidth := 0;
  self.s.vga_mem_updated := 0;
  self.s.x_tilesize := X_TILESIZE;
  self.s.y_tilesize := Y_TILESIZE;
  //self.put('VGA');
end;

procedure TVga_c1.init(d:pbx_devices_c ; cmos:pbx_cmos_c);
var
  i: unsigned;
  x,y: unsigned;
  addr: unsigned;
begin
  self.devices := d;
  addr := $03b4;
  while addr <= $03B5 do
  begin
    self.devices^.register_io_read_handler(self, read_handler, addr, 'vga video');
    self.devices^.register_io_write_handler(self, write_handler, addr, 'vga video');
    Inc(Addr);
  end;

  addr := $03BA;
  while addr <= $03BA do
  begin
    self.devices^.register_io_read_handler(self, read_handler, addr, 'vga video');
    self.devices^.register_io_write_handler(self, write_handler, addr, 'vga video');
    Inc(Addr);
  end;

  addr := $03C0;
  while addr <= $03CF do
  begin
    self.devices^.register_io_read_handler(self, read_handler, addr, 'vga video');
    self.devices^.register_io_write_handler(self, write_handler, addr, 'vga video');
    Inc(Addr);
  end;

  addr := $03D4;
  while addr <= $03D5 do
  begin
    self.devices^.register_io_read_handler(self, read_handler, addr, 'vga video');
    self.devices^.register_io_write_handler(self, write_handler, addr, 'vga video');
    Inc(Addr);
  end;

  addr := $03DA;
  while addr <= $03DA do
  begin
    self.devices^.register_io_read_handler(self, read_handler, addr, 'vga video');
    self.devices^.register_io_write_handler(self, write_handler, addr, 'vga video');
    Inc(Addr);
  end;


  self.s.misc_output.color_emulation  := 1;
  self.s.misc_output.enable_ram       := 1;
  self.s.misc_output.clock_select     := 0;
  self.s.misc_output.select_high_bank := 0;
  self.s.misc_output.horiz_sync_pol   := 1;
  self.s.misc_output.vert_sync_pol    := 1;

  self.s.CRTC.address := 0;

  self.s.attribute_ctrl.mode_ctrl.graphics_alpha        := 0;
  self.s.attribute_ctrl.mode_ctrl.display_type          := 0;
  self.s.attribute_ctrl.mode_ctrl.enable_line_graphics  := 1;
  self.s.attribute_ctrl.mode_ctrl.blink_intensity       := 0;
  self.s.attribute_ctrl.mode_ctrl.pixel_panning_compat  := 0;
  self.s.attribute_ctrl.mode_ctrl.pixel_clock_select    := 0;
  self.s.attribute_ctrl.mode_ctrl.internal_palette_size := 0;

  self.s.scan_bits := 640;
  i := 0;

  while i < $18 do
  begin
    self.s.CRTC.reg[i] := 0;
    Inc(I);
  end;
  self.s.CRTC.address := 0;

  self.s.attribute_ctrl.flip_flop     := 0;
  self.s.attribute_ctrl.address       := 0;
  self.s.attribute_ctrl.video_enabled := 1;
  i := 0;
  while i < 16 do
  begin
    self.s.attribute_ctrl.palette_reg[i] := 0;
    Inc(I);
  end;
  self.s.attribute_ctrl.overscan_color      := 0;
  self.s.attribute_ctrl.color_plane_enable  := $0f;
  self.s.attribute_ctrl.horiz_pel_panning   := 0;
  self.s.attribute_ctrl.color_select        := 0;
  I := 0;
  while i < 256 do
  begin
    self.s.pel.data[i].red := 0;
    self.s.pel.data[i].green := 0;
    self.s.pel.data[i].blue := 0;
    Inc(i);
  end;
  self.s.pel.write_data_register := 0;
  self.s.pel.write_data_cycle    := 0;
  self.s.pel.read_data_register  := 0;
  self.s.pel.read_data_cycle     := 0;
  self.s.pel.mask                := $ff;

  self.s.graphics_ctrl.index            := 0;
  self.s.graphics_ctrl.set_reset        := 0;
  self.s.graphics_ctrl.enable_set_reset := 0;
  self.s.graphics_ctrl.color_compare    := 0;
  self.s.graphics_ctrl.data_rotate      := 0;
  self.s.graphics_ctrl.raster_op        := 0;
  self.s.graphics_ctrl.read_map_select  := 0;
  self.s.graphics_ctrl.write_mode       := 0;
  self.s.graphics_ctrl.read_mode        := 0;
  self.s.graphics_ctrl.odd_even         := 0;
  self.s.graphics_ctrl.chain_odd_even   := 0;
  self.s.graphics_ctrl.shift_reg        := 0;
  self.s.graphics_ctrl.graphics_alpha   := 0;
  self.s.graphics_ctrl.memory_mapping   := 3;//2; // monochrome text mode
  self.s.graphics_ctrl.color_dont_care  := 0;
  self.s.graphics_ctrl.bitmask          := 0;
  i := 0;
  while i < 4 do
  begin
    self.s.graphics_ctrl.latch[i] := 0;
    Inc(i);
  end;

  self.s.sequencer.index    := 0;
  self.s.sequencer.map_mask := 0;
  i := 0;
  while i < 4 do
  begin
    self.s.sequencer.map_mask_bit[i] := 0;
    Inc(i);
  end;
  self.s.sequencer.bit0 := 0;
  self.s.sequencer.bit1 := 0;
  self.s.sequencer.reg1 := 0;
  self.s.sequencer.char_map_select := 0;
  self.s.sequencer.extended_mem    := 1; // display mem greater than 64K
  self.s.sequencer.odd_even        := 1; // use sequential addressing mode
  self.s.sequencer.chain_four      := 0; // use map mask  and read map select

  FillChar(self.s.vga_memory, sizeof(self.s.vga_memory), 0);

  self.s.vga_mem_updated := 0;
  for y := 0 to 480 div Y_TILESIZE do
    for x := 0 to 640 div X_TILESIZE do
      self.s.vga_tile_updated[x,y] := 0;

  //begin
  (* ??? should redo self to pass X args *)
  //bx_gui.init(1, @argv[0], self.s.x_tilesize, self.s.y_tilesize); !!!
  //end;

  LogInfo(Format('interval:=%u', [2000]));
  self.timer_id := bx_pc_system.register_timer(@bx_vga, timer_handler, 30000, 1, 1);

  cmos^.s.reg[$14] := (cmos^.s.reg[$14] and $cf) or $00; (* video card with BIOS ROM *)

  self.s.horiz_tick := 0;
  self.s.vert_tick  := 0;

  addr := $ff80;
  while addr <= $ff81 do
  begin
    self.devices^.register_io_read_handler(self, vbe_read_handler, addr, 'vga video');
    self.devices^.register_io_write_handler(self, vbe_write_handler, addr, 'vga video');
    Inc(addr);
  end;
  self.s.vbe_xres     := 640;
  self.s.vbe_yres     := 480;
  self.s.vbe_bpp      := 8;
  self.s.vbe_bank     := 0;
  self.s.vbe_enabled  := 0;
  self.s.vbe_curindex := 0;
end;

procedure TVga_c1.determine_screen_dimensions(piHeight: punsigned; piWidth: punsigned);
var
  ai: array[0..$20] of integer;
  i: integer;
begin
  self.s.scan_bits := 640;
  i := 0;
  while i < $20 do
  begin
     ai[i] := self.s.CRTC.reg[i];
     Inc(i);
  end;

  (*
  switch ( ( self.s.misc_output.vert_sync_pol shl 1)orself.s.misc_output.horiz_sync_pol )
   begin
   case 0: *piHeight := 200; break;
   case 1: *piHeight := 400; break;
   case 2: *piHeight := 350; break;
   case 3: *piHeight := 480; break;
   end;
  *)

  if ( self.s.graphics_ctrl.shift_reg = 0 ) then
  begin
    piWidth^ := 640;
    piHeight^ := 480;

    if ( self.s.CRTC.reg[6] = $BF ) then
    begin
      if ((self.s.CRTC.reg[23] = $A3) and (self.s.CRTC.reg[20] = $40) and
         (self.s.CRTC.reg[9] = $41)) then
      begin
         piWidth^ := 320;
         piHeight^ := 240;
      end;
      if ((self.s.CRTC.reg[23] = $E3) and (self.s.CRTC.reg[20] = $F) and
         (self.s.CRTC.reg[9] = $40)) then
      begin
         piWidth^ := 640;
         piHeight^ := 352;
      end;
      if ((self.s.CRTC.reg[23] = $E3) and (self.s.CRTC.reg[20] = 0) and
         (self.s.CRTC.reg[9] = $C0)) then
      begin
        if (self.s.CRTC.reg[19] = $14) then
        begin
           self.s.scan_bits :=320;
           piWidth^ := 320;
           piHeight^ := 192;
        end else
        begin
           piWidth^ := 640;
           piHeight^ := 208;
        end;
      end;
    end;
  end else
  if ( self.s.graphics_ctrl.shift_reg = 2 ) then
  begin
    piWidth^ := 320;
    if ( self.s.sequencer.chain_four ) <> 0
      then piHeight^ := 208
      else
      begin
        piHeight^ := 240;

        if ((self.s.CRTC.reg[23] = $E3) and (self.s.CRTC.reg[20] = 0) and
           (self.s.CRTC.reg[9] = $40)) then
          piHeight^ := 480;
      end;
  end else
  begin
    piWidth^ := 640;
    piHeight^ := 480;
  end;
end;
  // static IO port read callback handler
  // redirects to non-static class handler to aprocedure virtual functions
function TVga_c1.read_handler(self_ptr:pointer; address:Bit32u; io_len:unsigned):Bit32u;
var
  horiz_retrace, vert_retrace: Bool;
  retval: Bit8u;
  ret: Bit32u;
  label read_exit;
begin
  ret := 0;
  if ( (address >= $03b0) and (address <= $03bf) and
       (self.s.misc_output.color_emulation <> 0) ) then
  begin
    ret := $ff;
    goto read_exit;
  end;

  if ( (address >= $03d0) and (address <= $03df) and
       (self.s.misc_output.color_emulation = 0) ) then
  begin
    ret := $ff;
    goto read_exit;
  end;

  case (address) of
    $03ba, (* Input Status 1 (monochrome emulation modes) *)
    $03ca, (* Feature Control ??? *)
    $03da: (* Input Status 1 (color emulation modes) *)
      // bit3: Vertical Retrace
      //       0 := display is in the display mode
      //       1 := display is in the vertical retrace mode
      // bit0: Display Enable
      //       0 := display is in the display mode
      //       1 := display is not in the display mode; either the
      //           horizontal or vertical retrace period is active

      //      printf('horiz := %d, vert := %d', self.s.horiz_tick, self.s.vert_tick);
    begin
      if(self.s.misc_output.clock_select = 0) then // 25.175 clock 112.5% the length of 28.32
      begin
        if (self.s.horiz_tick >= 112) then
        begin
      	  self.s.horiz_tick := 0;
      	  horiz_retrace := 1;
        end else
        begin
          inc(self.s.horiz_tick);
	        horiz_retrace := 0;
        end;
   	    if (self.s.vert_tick >= 112) then
        begin
          self.s.vert_tick := 0;
          vert_retrace := 1;
       	end else
        begin
          Inc(self.s.vert_tick);
          vert_retrace := 0;
        end;
      end else // clock_select 1 is assumed to be the 28.32 clock in XF86_VGA16
      begin
        if (self.s.horiz_tick >= 100) then
        begin // ??? bogus # 100
      	  self.s.horiz_tick := 0;
      	  horiz_retrace := 1;
        end else
        begin
      	  Inc(self.s.horiz_tick);
      	  horiz_retrace := 0;
        end;
       	if (self.s.vert_tick >= 100) then
        begin // ??? bogus # 100
       	  self.s.vert_tick := 0;
	        vert_retrace := 1;
       	end else
        begin
	        Inc(self.s.vert_tick);
   	      vert_retrace := 0;
        end;
      end; // probably add more clock modes here for diffrent resolutions

      retval := 0;
      if (horiz_retrace <> 0) or (vert_retrace <> 0) then
        retval := $01;
      if (vert_retrace) <> 0 then
        retval := retval or $08;

      (* reading self port resets the flip-flop to address mode *)
      self.s.attribute_ctrl.flip_flop := 0;

      ret := retval;
      goto read_exit;
    end;

    $03c0: (* *)
    begin
       if (self.s.attribute_ctrl.flip_flop = 0) then
       begin
         retval := (self.s.attribute_ctrl.video_enabled shl 5) or
                    self.s.attribute_ctrl.address;
         ret := retval;
         goto read_exit;
       end else
         Exit(0);
    end;

    $03c1: (* *)
    begin
      case (self.s.attribute_ctrl.address) of
        $00,  $01,  $02,  $03,
        $04,  $05,  $06,  $07,
        $08,  $09,  $0a,  $0b,
        $0c,  $0d,  $0e,  $0f:
        begin
          retval := self.s.attribute_ctrl.palette_reg[self.s.attribute_ctrl.address];
          ret := retval;
          goto read_exit;
        end;
        $10: (* mode control register *)
        begin
           retval :=
          (self.s.attribute_ctrl.mode_ctrl.graphics_alpha shl 0) or
          (self.s.attribute_ctrl.mode_ctrl.display_type shl 1) or
          (self.s.attribute_ctrl.mode_ctrl.enable_line_graphics shl 2) or
          (self.s.attribute_ctrl.mode_ctrl.blink_intensity shl 3) or
          (self.s.attribute_ctrl.mode_ctrl.pixel_panning_compat shl 5) or
          (self.s.attribute_ctrl.mode_ctrl.pixel_clock_select shl 6) or
          (self.s.attribute_ctrl.mode_ctrl.internal_palette_size shl 7);
          ret := retval;
          goto read_exit;
        end;
        $11: (* overscan color register *)
        begin
          ret := self.s.attribute_ctrl.overscan_color;
          goto read_exit;
        end;
        $12: (* color plane enable *)
        begin
          ret := self.s.attribute_ctrl.color_plane_enable;
          goto read_exit;
        end;
        $13: (* horizontal PEL panning register *)
        begin
          ret := self.s.attribute_ctrl.horiz_pel_panning;
          goto read_exit;
        end;
        $14: (* color select register *)
        begin
          ret := self.s.attribute_ctrl.color_select;
          goto read_exit;
        end;
      else
        begin
          ret := 0;
          goto read_exit;
        end;
      end;
    end;

    $03c2: (* Input Status 0 *)
    begin
      ret := 0;
      goto read_exit;
    end;

    $03c3: (* VGA Enable Register *)
    begin
      ret := 1;
      goto read_exit;
    end;

    $03c4: (* Sequencer Index Register *)
    begin
      ret := self.s.sequencer.index;
      goto read_exit;
    end;

    $03c5: (* Sequencer Registers 00..04 *)
    begin
      case (self.s.sequencer.index) of
        0: (* sequencer: reset *)
        begin
          ret := self.s.sequencer.bit0 or (self.s.sequencer.bit1 shl 1);
          goto read_exit;
        end;
        1: (* sequencer: clocking mode *)
        begin
          ret := self.s.sequencer.reg1;
          goto read_exit;
        end;
        2: (* sequencer: map mask register *)
        begin
          ret := self.s.sequencer.map_mask;
          goto read_exit;
        end;
        3: (* sequencer: character map select register *)
        begin
          ret := self.s.sequencer.char_map_select;
          goto read_exit;
        end;
        4: (* sequencer: memory mode register *)
        begin
          retval := (self.s.sequencer.extended_mem   shl 1) or
                    (self.s.sequencer.odd_even       shl 2) or
                    (self.s.sequencer.chain_four     shl 3);
          ret := retval;
          goto read_exit;
        end;
      else
        begin
          ret := 0;
          goto read_exit;
        end;
      end;
    end;   //Case
    $03c6: (* PEL mask ??? *)
    begin
      ret := self.s.pel.mask;
      goto read_exit;
    end;

    $03c9: (* PEL Data Register, colors 00..FF *)
    begin
      case (self.s.pel.read_data_cycle) of
        0: retval := self.s.pel.data[self.s.pel.read_data_register].red;
        1: retval := self.s.pel.data[self.s.pel.read_data_register].green;
        2: retval := self.s.pel.data[self.s.pel.read_data_register].blue;
        else
          retval := 0; // keep compiler happy
      end;
      inc(self.s.pel.read_data_cycle);
      if (self.s.pel.read_data_cycle >= 3) then
      begin
        self.s.pel.read_data_cycle := 0;
        inc(self.s.pel.read_data_register);
      end;
      ret := retval;
      goto read_exit;
    end;

    $03cc: (* Miscellaneous Output / Graphics 1 Position ??? *)
    begin
    retval := ((self.s.misc_output.color_emulation   and $01) shl 0) or
              ((self.s.misc_output.enable_ram        and $01) shl 1) or
              ((self.s.misc_output.clock_select      and $03) shl 2) or
              ((self.s.misc_output.select_high_bank  and $01) shl 5) or
              ((self.s.misc_output.horiz_sync_pol    and $01) shl 6) or
              ((self.s.misc_output.vert_sync_pol     and $01) shl 7);
      ret := retval;
      goto read_exit;
    end;

    $03ce: (* Graphics Controller Index Register *)
    begin
      ret := self.s.graphics_ctrl.index;
      goto read_exit;
    end;

    $03cd: (* ??? *)
    begin
      ret := $00;
      goto read_exit;
    end;

    $03cf: (* Graphics Controller Registers 00..08 *)
    begin
      case (self.s.graphics_ctrl.index) of
        0: (* Set/Reset *)
        begin
          ret := self.s.graphics_ctrl.set_reset;
          goto read_exit;
        end;
        1: (* Enable Set/Reset *)
        begin
          ret := self.s.graphics_ctrl.enable_set_reset;
          goto read_exit;
        end;
        2: (* Color Compare *)
        begin
          ret := self.s.graphics_ctrl.color_compare;
          goto read_exit;
        end;
        3: (* Data Rotate *)
        begin
          retval := ((self.s.graphics_ctrl.raster_op  and $03) shl 3) or
                    ((self.s.graphics_ctrl.data_rotate  and $07) shl 0);
          ret := retval;
          goto read_exit;
        end;
        4: (* Read Map Select *)
        begin
          ret := self.s.graphics_ctrl.read_map_select;
          goto read_exit;
        end;
        5: (* mode *)
        begin
          retval := ((self.s.graphics_ctrl.shift_reg  and $03) shl 5) or
                    ((self.s.graphics_ctrl.odd_even  and $01 ) shl 4) or
                    ((self.s.graphics_ctrl.read_mode  and $01) shl 3) or
                    ((self.s.graphics_ctrl.write_mode  and $03) shl 0);

          ret := RetVal;
          goto read_exit;
        end;
        6: (* Miscellaneous *)
        begin
          retval := ((self.s.graphics_ctrl.memory_mapping  and $03 ) shl 2) or
                    ((self.s.graphics_ctrl.odd_even  and $01) shl 1) or
                    ((self.s.graphics_ctrl.graphics_alpha  and $01) shl 0);
          ret := RetVal;
          goto read_exit;
        end;
        7: (* Color Don't Care *)
        begin
          ret := self.s.graphics_ctrl.color_dont_care;
          goto read_exit;
        end;
        8: (* Bit Mask *)
        begin
          ret := self.s.graphics_ctrl.bitmask;
          goto read_exit;
        end;
      else
        begin
          (* ??? *)
          ret := 0;
          goto read_exit;
        end;
      end;
    end;
    $03d4: (* CRTC Index Register (color emulation modes) *)
    begin
      ret := self.s.CRTC.address;
      goto read_exit;
    end;

    $03b5, (* CRTC Registers (monochrome emulation modes) *)
    $03d5: (* CRTC Registers (color emulation modes) *)
    begin
      ret := self.s.CRTC.reg[self.s.CRTC.address];
      goto read_exit;
    end;

    $03b4, (* CRTC Index Register (monochrome emulation modes) *)
    $03c7, (* not sure but OpenBSD reads it a lot *)
    $03cb, (* not sure but OpenBSD reads it a lot *)
    $03c8: (* *)
    begin
      ret := 0;
      goto read_exit;
    end;
  else
    begin
       ret := 0;
       goto read_exit;
    end;
  end;

  read_exit:
  Result := ret;
end;
  // static IO port write callback handler
  // redirects to non-static class handler to aprocedure virtual functions
procedure TVga_c1.write_handler(self_ptr:Pointer; address:Bit32u; dvalue:Bit32u; io_len:unsigned);
begin
  bx_vga.write(address, dvalue, io_len, 0);
end;

class procedure TVga_c1.write_handler_no_log(self_ptr:Pointer; address:Bit32u; dvalue:Bit32u; io_len:unsigned);
begin
  bx_vga.write(address, dvalue, io_len, 1);
end;

procedure TVga_c1.write(address:Bit32u; value:Bit32u; io_len:unsigned; no_log:Bool);
var
  i: unsigned;
  prev_video_enabled: Bool;
  charmap1, charmap2, prev_memory_mapping: Bit8u;
  prev_graphics_alpha, prev_chain_odd_even: Bool;
  needs_update: Bool;
  xti,yti: Integer;
  iHeight, iWidth: unsigned;
  label flipflop;
begin
  if (io_len = 2) then
  begin
    TVga_c1.write_handler_no_log(0, address, value  and $ff, 1);
    TVga_c1.write_handler_no_log(0, address+1, (value shr 8)  and $ff, 1);
    exit;
  end;

  if ( (address >= $03b0) and (address <= $03bf) and
       (self.s.misc_output.color_emulation<>0) ) then
    exit;
  if ( (address >= $03d0) and (address <= $03df) and
       (self.s.misc_output.color_emulation=0) ) then
    exit;

  case (address) of
    $03b4: (* CRTC Index Register (monochrome emulation modes) *)
    begin
      self.s.CRTC.address := value;
    end;

    $03b5: (* CRTC Registers (monochrome emulation modes) *)
    begin
      self.s.CRTC.reg[self.s.CRTC.address] := value;
    end;

    $03ba: (* Feature Control (monochrome emulation modes) *)
    begin
    end;

    $03c0: (* Attribute Controller *)
    begin
      if (self.s.attribute_ctrl.flip_flop = 0) then
      begin (* address mode *)
        prev_video_enabled := self.s.attribute_ctrl.video_enabled;
        self.s.attribute_ctrl.video_enabled := (value shr 5)  and $01;
        if (self.s.attribute_ctrl.video_enabled = 0)
          then clear_screen
          else
        if (prev_video_enabled = 0) then
        begin
           // Mark all video as updated so the color changes will go through
           FillChar(self.s.text_snapshot, sizeof(self.s.text_snapshot),0 );
           self.s.vga_mem_updated := 1;
           for xti := 0 to BX_NUM_X_TILES do
             for yti := 0 to BX_NUM_Y_TILES do
               self.s.vga_tile_updated[xti][yti] := 1;
        end;
        value := value and $1f; (* address := bits 0..4 *)
        self.s.attribute_ctrl.address := value;
        case (value) of
           $00,  $01,  $02,  $03,
           $04,  $05,  $06,  $07,
           $08,  $09,  $0a,  $0b,
           $0c,  $0d,  $0e,  $0f:
            goto flipflop;
        end;
      end else
      begin (* data-write mode *)
        case (self.s.attribute_ctrl.address) of
          $00,  $01,  $02,  $03,
          $04,  $05,  $06,  $07,
          $08,  $09,  $0a,  $0b,
          $0c,  $0d,  $0e,  $0f:
          begin
             self.s.attribute_ctrl.palette_reg[self.s.attribute_ctrl.address] := value;
             //BX_DEBUG(('io write: 3c0 palette reg[%u] := %02x',
             //  (unsigned) self.s.attribute_ctrl.address,
             //  (unsigned) value);
          end;
           $10: // mode control register
          begin
             self.s.attribute_ctrl.mode_ctrl.graphics_alpha := (value shr 0)  and $01;
             self.s.attribute_ctrl.mode_ctrl.display_type := (value shr 1)  and $01;
             self.s.attribute_ctrl.mode_ctrl.enable_line_graphics := (value shr 2)  and $01;
             self.s.attribute_ctrl.mode_ctrl.blink_intensity := (value shr 3)  and $01;
             self.s.attribute_ctrl.mode_ctrl.pixel_panning_compat := (value shr 5)  and $01;
             self.s.attribute_ctrl.mode_ctrl.pixel_clock_select := (value shr 6)  and $01;
             self.s.attribute_ctrl.mode_ctrl.internal_palette_size := (value shr 7)  and $01;
          end;
          $11: // Overscan Color Register
          begin
             self.s.attribute_ctrl.overscan_color := (value and $3f);
          end;
          $12: // Color Plane Enable Register
          begin
             self.s.attribute_ctrl.color_plane_enable := (value  and $0f);
          end;
          $13: // Horizontal Pixel Panning Register
          begin
             self.s.attribute_ctrl.horiz_pel_panning := (value  and $0f);
          end;
          $14: // Color Select Register
          begin
             self.s.attribute_ctrl.color_select := (value  and $0f);
          end;
//          else
//            BX_DEBUG(Format('io write 3c0: data-write mode %02x h',[self.s.attribute_ctrl.address]));
        end;
      end;
      flipflop:
      self.s.attribute_ctrl.flip_flop := word(self.s.attribute_ctrl.flip_flop=0);
    end;

    $03c2: // Miscellaneous Output Register
    begin
      self.s.misc_output.color_emulation  := (value shr 0)  and $01;
      self.s.misc_output.enable_ram       := (value shr 1)  and $01;
      self.s.misc_output.clock_select     := (value shr 2)  and $03;
      self.s.misc_output.select_high_bank := (value shr 5)  and $01;
      self.s.misc_output.horiz_sync_pol   := (value shr 6)  and $01;
      self.s.misc_output.vert_sync_pol    := (value shr 7)  and $01;
(*#if !defined(VGA_TRACE_FEATURE)
        BX_DEBUG(('io write 3c2:'));
        BX_DEBUG(('  color_emulation (attempted) := %u',
                  (value shr 0)  and $01 );
        BX_DEBUG(('  enable_ram := %u',
                  (unsigned) self.s.misc_output.enable_ram);
        BX_DEBUG(('  clock_select := %u',
                  (unsigned) self.s.misc_output.clock_select);
        BX_DEBUG(('  select_high_bank := %u',
                  (unsigned) self.s.misc_output.select_high_bank);
        BX_DEBUG(('  horiz_sync_pol := %u',
                  (unsigned) self.s.misc_output.horiz_sync_pol);
        BX_DEBUG(('  vert_sync_pol := %u',
                  (unsigned) self.s.misc_output.vert_sync_pol);
{$ifend}*)
    end;

    $03c3: // VGA enable
    begin
    // bit0: enables VGA display if set
    end;

    $03c4: (* Sequencer Index Register *)
    begin
//        if (value > 4) then begin
//          BX_DEBUG(('io write 3c4: value > 4'));
//          end;
      self.s.sequencer.index := value;
    end;

    $03c5: (* Sequencer Registers 00..04 *)
    begin
      case (self.s.sequencer.index) of
        0: (* sequencer: reset *)
        begin
          self.s.sequencer.bit0 := (value shr 0)  and $01;
          self.s.sequencer.bit1 := (value shr 1)  and $01;
        end;
        1: (* sequencer: clocking mode *)
        begin
          self.s.sequencer.reg1 := value  and $3f;
        end;
        2: (* sequencer: map mask register *)
        begin
          self.s.sequencer.map_mask := (value  and $0f);
          i:=0;
          while (i < 4) do
            begin
              self.s.sequencer.map_mask_bit[i] := (value shr i)  and $01;
              Inc(I);
            end;
        end;
        3: (* sequencer: character map select register *)
        begin
          self.s.sequencer.char_map_select := value;
          charmap1 := value  and $13;
          if (charmap1 > 3) then charmap1 := (charmap1  and 3) + 4;
          charmap2 := (value  and $2C) shr 2;
          if (charmap2 > 3) then charmap2 := (charmap2  and 3) + 4;
          LogInfo(Format('char map select: #1:=%d / #2:=%d (unused)',[charmap1, charmap2]));
        end;
        4: (* sequencer: memory mode register *)
        begin
          self.s.sequencer.extended_mem   := (value shr 1)  and $01;
          self.s.sequencer.odd_even       := (value shr 2)  and $01;
          self.s.sequencer.chain_four     := (value shr 3)  and $01;
        end;
//        else
//          BX_DEBUG(Format('io write 3c5: index %u unhandled',[self.s.sequencer.index]));
      end;
    end;

    $03c6: (* PEL mask *)
    begin
      self.s.pel.mask := value;
//        if (self.s.pel.mask <> $ff) then
//          BX_DEBUG(('io write 3c6: PEL mask:=$%02x !:= $FF'));
      // self.s.pel.mask should be and'd with final value before
      // indexing into color registerself.s.
    end;

    $03c7: // PEL address, read mode
    begin
      self.s.pel.read_data_register := value;
      self.s.pel.read_data_cycle    := 0;
    end;

    $03c8: (* PEL address write mode *)
    begin
      self.s.pel.write_data_register := value;
      self.s.pel.write_data_cycle    := 0;
    end;

    $03c9: (* PEL Data Register, colors 00..FF *)
    begin
      case (self.s.pel.write_data_cycle) of
        0:
          self.s.pel.data[self.s.pel.write_data_register].red := value;
        1:
          self.s.pel.data[self.s.pel.write_data_register].green := value;
        2:
        begin
          self.s.pel.data[self.s.pel.write_data_register].blue := value;
          if (self.s.vbe_enabled)<>0 then
          begin
            iWidth := self.s.vbe_xres;
            old_iWidth := iWidth;
            iHeight := self.s.vbe_yres;
            old_iHeight := iHeight;
          end else
          begin
            // 'normal vga' operation
            determine_screen_dimensions(@iHeight, @iWidth);
            if( (iWidth <> old_iWidth) or (iHeight <> old_iHeight) ) then
            begin
              dimension_update(iWidth, iHeight);
              old_iWidth := iWidth;
              old_iHeight := iHeight;
            end;
          end;

          needs_update := palette_change(self.s.pel.write_data_register,
          self.s.pel.data[self.s.pel.write_data_register].red shl 2,
          self.s.pel.data[self.s.pel.write_data_register].green shl 2,
          self.s.pel.data[self.s.pel.write_data_register].blue shl 2);

          if (needs_update)<>0 then
          begin
            // Mark all video as updated so the color changes will go through
            self.s.vga_mem_updated := 1;
            for xti := 0 to BX_NUM_X_TILES do
              for yti := 0 to BX_NUM_Y_TILES do
                self.s.vga_tile_updated[xti][yti] := 1;
          end;
        end;
      end;

      inc(self.s.pel.write_data_cycle);
      if (self.s.pel.write_data_cycle >= 3) then
      begin
        //BX_INFO(('self.s.pel.data[%u] beginr:=%u, g:=%u, b:=%uend;',
        //  (unsigned) self.s.pel.write_data_register,
        //  (unsigned) self.s.pel.data[self.s.pel.write_data_register].red,
        //  (unsigned) self.s.pel.data[self.s.pel.write_data_register].green,
        //  (unsigned) self.s.pel.data[self.s.pel.write_data_register].blue);
        self.s.pel.write_data_cycle := 0;
        inc(self.s.pel.write_data_register);
      end;
    end;

    $03ca: (* Graphics 2 Position (EGA) *)
    begin
    // ignore, EGA only???
    end;

    $03cc: (* Graphics 1 Position (EGA) *)
    begin
    // ignore, EGA only???
    end;

    $03ce: (* Graphics Controller Index Register *)
    begin
//        if (value > $08) then
//          BX_DEBUG(('io write: 3ce: value > 8'));
      self.s.graphics_ctrl.index := value;
    end;

//    $03cd: (* ??? *)
//      begin
//        BX_DEBUG(Format('io write to 03cd := %02x',[value]));
//      end;

    $03cf: (* Graphics Controller Registers 00..08 *)
    begin
      case (self.s.graphics_ctrl.index) of
        0: (* Set/Reset *)
        begin
          self.s.graphics_ctrl.set_reset := value  and $0f;
        end;
        1: (* Enable Set/Reset *)
        begin
          self.s.graphics_ctrl.enable_set_reset := value  and $0f;
        end;
        2: (* Color Compare *)
        begin
          self.s.graphics_ctrl.color_compare := value  and $0f;
        end;
        3: (* Data Rotate *)
        begin
          self.s.graphics_ctrl.data_rotate := value  and $07;
          (* ??? is self bits 3..4 or 4..5 *)
          self.s.graphics_ctrl.raster_op    := (value shr 3)  and $03; (* ??? *)
        end;
        4: (* Read Map Select *)
        begin
          self.s.graphics_ctrl.read_map_select := value  and $03;
        end;
        5:  (* mode *)
        begin
          self.s.graphics_ctrl.write_mode        := value  and $03;
          self.s.graphics_ctrl.read_mode         := (value shr 3)  and $01;
          self.s.graphics_ctrl.odd_even   := (value shr 4)  and $01;
          self.s.graphics_ctrl.shift_reg         := (value shr 5)  and $03;

//              if (self.s.graphics_ctrl.odd_even)<>0 then
//                BX_DEBUG(Format('io write: 3cf: reg 05: value := %02xh',[value]));
//              if (self.s.graphics_ctrl.shift_reg)<>0 then
//                BX_DEBUG(Format('io write: 3cf: reg 05: value := %02xh',[value]));
        end;
        6: (* Miscellaneous *)
        begin
          prev_graphics_alpha := self.s.graphics_ctrl.graphics_alpha;
          prev_chain_odd_even := self.s.graphics_ctrl.chain_odd_even;
          prev_memory_mapping := self.s.graphics_ctrl.memory_mapping;

          self.s.graphics_ctrl.graphics_alpha := value  and $01;
          self.s.graphics_ctrl.chain_odd_even := (value shr 1)  and $01;
          self.s.graphics_ctrl.memory_mapping := (value shr 2)  and $03;
          if (prev_memory_mapping <> self.s.graphics_ctrl.memory_mapping) then
            self.s.vga_mem_updated := 1;
          // other checks here ???
          // check for transition from graphics to alpha mode and clear
          // old snapshot memory
        end;
      7: (* Color Don't Care *)
        begin
          self.s.graphics_ctrl.color_dont_care := value  and $0f;
        end;
      8: (* Bit Mask *)
        begin
          self.s.graphics_ctrl.bitmask := value;
        end;
//        else
//          (* ??? *)
//          BX_DEBUG(Format('io write: 3cf: index %u unhandled',[self.s.graphics_ctrl.index]));
      end;
    end;

    $03d4: (* CRTC Index Register (color emulation modes) *)
    begin
      self.s.CRTC.address := value;
//        if (self.s.CRTC.address > $18) then
//          BX_DEBUG(Format('vga_io_write: 3d4: address := %02xh',[self.s.CRTC.address]));
    end;

    $03d5: (* CRTC Registers (color emulation modes) *)
    begin
//        if (self.s.CRTC.address > $18) then
//          BX_DEBUG(Format('vga_io_write: 3d5: address := %02xh',[self.s.CRTC.address]));
      self.s.CRTC.reg[self.s.CRTC.address] := value;
//        BX_DEBUG(('color CRTC Reg[%u] := %02x',
//          (unsigned) self.s.CRTC.address, (unsigned) value);
      if ((self.s.CRTC.address>=$0C) or (self.s.CRTC.address<=$0F)) then begin
        // Start Address or Cursor Location change
        self.s.vga_mem_updated := 1;
      end;
    end;

//    $03da: (* Feature Control (color emulation modes) *)
//      begin
//        BX_DEBUG(('io write: 3da: ignoring: feature ctrl  and vert sync'));
//      end;

    $03c1: (* *)
    begin
      LogError(Format('unsupported io write to port $%04x, val:=$%02x',[address, value]));
    end;
  else
    LogError(Format('unsupported io write to port $%04x, val:=$%02x',[address,value]));
  end;
end;

procedure TVga_c1.set_update_interval(interval:unsigned);
begin
  LogInfo (Format('Changing timer interval to %d\n', [interval]));
  self.timer ();
  bx_pc_system.activate_timer (self.timer_id, interval, 1);
end;

procedure TVga_c1.timer_handler(self_ptr:pointer);
var
  class_ptr:PVga_c1;
begin
  class_ptr := PVga_c1(self_ptr);
  class_ptr^.timer();
end;

procedure TVga_c1.timer;
begin
  update();
  FlushGui();
end;

procedure TVga_c1.update;
var
  iHeight, iWidth:unsigned;
  xti, yti:unsigned;
  color:Bit8u;
  r, c:unsigned;
  byte_offset:LongWord;
  fbyte_offset:real;
  pixely, pixelx:LongWord;
  bit_no:unsigned ;
  attribute, palette_reg_val, DAC_regno:Bit8u;
  plane:LongWord;
  start_addr:LongWord;
  start_address:LongWord;
  cursor_address, cursor_x, cursor_y:LongWord;
  cursor_state:Bit16u;
  VDE, MSL, rows:unsigned;
begin
  self.s.vga_mem_updated := 0;

  if (self.s.vbe_enabled)<>0 then
  begin
    // specific VBE code display update code
    // self is partly copied/mod_ified from the 32$20$8 update more below

    iWidth:=self.s.vbe_xres;
    iHeight:=self.s.vbe_yres;

    for yti:=0 to iHeight div Y_TILESIZE do
      for xti:=0 to iWidth div X_TILESIZE do
      begin
        if (self.s.vga_tile_updated[xti][yti])<>0 then
        begin
          for r:=0 to Y_TILESIZE do
          begin
            for c:=0 to X_TILESIZE do
            begin
              pixely := ((yti*Y_TILESIZE) + r);
              pixelx := ((xti*X_TILESIZE) + c);

              byte_offset := (pixely*iHeight) + (pixelx);
              color := self.s.vbe_memory[byte_offset];
              self.s.tile[r*X_TILESIZE + c] := color;
            end;
          end;
          graphics_tile_update(@self.s.tile, xti*X_TILESIZE, yti*Y_TILESIZE);
          self.s.vga_tile_updated[xti][yti] := 0;
        end;
      end;
    // after a vbe display update, don't try to do any 'normal vga' updates anymore
    exit;
  end;
  // fields that effect the way video memory is serialized into screen output:
  // GRAPHICS CONTROLLER:
  //   self.s.graphics_ctrl.shift_reg:
  //     0: output data in standard VGA format
  //     1: output data in CGA-compatible 32$200 4 color graphics mode
  //        (modes 4  and 5)
  //     2: output data 8 bits at a time from the 4 bit planes
  //        (mode 13 and variants like modeX)
//fprintf(stderr, '# update()');

  // if (self.s.vga_mem_updated=0 or self.s.attribute_ctrl.video_enabled = 0)

  if (self.s.graphics_ctrl.graphics_alpha)<>0 then
  begin


//BX_DEBUG(('update: shiftreg:=%u, chain4:=%u, mapping:=%u',
//  (unsigned) self.s.graphics_ctrl.shift_reg,
//  (unsigned) self.s.sequencer.chain_four,
//  (unsigned) self.s.graphics_ctrl.memory_mapping);

    case ( self.s.graphics_ctrl.shift_reg ) of
      0: // output data in serial fashion with each display plane
      begin
              // output on its associated serial output.  Standard EGA/VGA format

        determine_screen_dimensions(@iHeight, @iWidth);

        //BX_DEBUG(('update(): mode 12h: 64$48$16colors'));
        if( (iWidth <> old_iWidth) or (iHeight <> old_iHeight) ) then
        begin
          dimension_update(iWidth, iHeight);
          old_iWidth := iWidth;
          old_iHeight := iHeight;
        end;

        for yti:=0 to iHeight div Y_TILESIZE do
        begin
          for xti:=0 to iWidth div X_TILESIZE do
          begin
            if (self.s.vga_tile_updated[xti][yti])<>0 then
            begin
              for r:=0 to Y_TILESIZE do
              begin
                for c:=0 to X_TILESIZE do
                begin
                  bit_no := 7 - (c mod 8); /// !!! %
                  byte_offset := Trunc((yti*Y_TILESIZE+r) * (self.s.scan_bits/8) + (xti*X_TILESIZE+c)/8);
                  attribute :=
                    (((self.s.vga_memory[0*65536 + byte_offset] shr bit_no)  and $01) shl 0) or
                    (((self.s.vga_memory[1*65536 + byte_offset] shr bit_no)  and $01) shl 1) or
                    (((self.s.vga_memory[2*65536 + byte_offset] shr bit_no)  and $01) shl 2) or
                    (((self.s.vga_memory[3*65536 + byte_offset] shr bit_no)  and $01) shl 3);

                  attribute := attribute and self.s.attribute_ctrl.color_plane_enable;
                  palette_reg_val := self.s.attribute_ctrl.palette_reg[attribute];
                  if (self.s.attribute_ctrl.mode_ctrl.internal_palette_size)<>0 then
                  begin
                    // use 4 lower bits from palette register
                    // use 4 higher bits from color select register
                    // 16 banks of 16-color registers
                    DAC_regno := (palette_reg_val  and $0f) or (self.s.attribute_ctrl.color_select shl 4);
                  end else
                  begin
                    // use 6 lower bits from palette register
                    // use 2 higher bits from color select register
                    // 4 banks of 64-color registers
                      DAC_regno := (palette_reg_val  and $3f) or((self.s.attribute_ctrl.color_select  and $0c) shl 4);
                  end;
                  // DAC_regno @:= video DAC mask register ???

                  self.s.tile[r*X_TILESIZE + c] := DAC_regno;
                end;
              end;
              graphics_tile_update(@self.s.tile, xti*X_TILESIZE, yti*Y_TILESIZE);
              self.s.vga_tile_updated[xti][yti] := 0;
            end;
          end;
        end; // case 0
      end;
      1: // output the data in a CGA-compatible 32$200 4 color graphics
              // mode.  (modes 4  and 5)
        (* CGA 32$20$4 start *)
      begin
        iHeight:=200; iWidth:=320;
        if( (iWidth <> old_iWidth) or (iHeight <> old_iHeight) ) then
        begin
          dimension_update(iWidth, iHeight);
          old_iWidth := iWidth;
          old_iHeight := iHeight;
        end;

        for yti:=0 to iHeight div Y_TILESIZE do
          for xti:=0 to iWidth div X_TILESIZE do
          begin
            if (self.s.vga_tile_updated[xti][yti])<>0 then
            begin
              for r:=0 to Y_TILESIZE do
              begin
                for c:=0 to X_TILESIZE do
                begin

                  (* 0 or $2000 *)
                  byte_offset := ((yti*Y_TILESIZE + r)  and 1) shl 13;
                  (* to the start of the line *)
                  byte_offset := byte_offset + (320 div 4) * ((yti*Y_TILESIZE + r) div 2);
                  (* to the byte start *)
                  byte_offset := byte_offset + ((xti*X_TILESIZE + c) div 4);

                  attribute := 6 - 2*((xti*X_TILESIZE + c) mod 4);
                  palette_reg_val := (self.s.vga_memory[byte_offset]) shr attribute;
                  palette_reg_val := palette_reg_val and 3;
                  palette_reg_val := palette_reg_val or self.s.attribute_ctrl.mode_ctrl.enable_line_graphics shl 2;
                  // palette_reg_val |:= self.s.attribute_ctrl.mode_ctrl.blink_intensity shl 3;
                  DAC_regno := self.s.attribute_ctrl.palette_reg[palette_reg_val];
                  self.s.tile[r*X_TILESIZE + c] := DAC_regno;
                end;
              end;
              graphics_tile_update(@self.s.tile, xti*X_TILESIZE, yti*Y_TILESIZE);
              self.s.vga_tile_updated[xti][yti] := 0;
            end;
          end;
            (* CGA 32$20$4 end *)

      end; // case 1

      2: // output the data eight bits at a time from the 4 bit planeself.s.
              // (format for VGA mode 13 hex)
      begin
          determine_screen_dimensions(@iHeight, @iWidth);

        if ( self.s.sequencer.chain_four )<>0 then
        begin
          // bx_vga_dump_status();

          if (self.s.misc_output.select_high_bank <> 1) then
            LogPanic(('update: select_high_bank !:= 1'));

          if( (iHeight <> old_iHeight) or (iWidth <> old_iWidth) ) then
          begin
            dimension_update(iWidth, iHeight);
            old_iHeight := iHeight;
            old_iWidth := iWidth;
          end;

          for yti:=0 to iHeight div Y_TILESIZE do
            for xti:=0 to iWidth div X_TILESIZE do
            begin
              if (self.s.vga_tile_updated[xti][yti])<>0 then
              begin // end;
              // if (1) then begin
                for r:=0 to Y_TILESIZE do
                begin
                  for c:=0 to X_TILESIZE do
                  begin
                    pixely := ((yti*Y_TILESIZE) + r);
                    pixelx := ((xti*X_TILESIZE) + c);
                    plane  := (pixelx mod 4);
                    //byte_offset := (plane * 65536) + (pixely * 320) + (pixelx  and ~$03);
                    byte_offset := (plane * 65536) + (pixely * 320) + (pixelx  and not $03);
// simulate wrap of upper two address bits into low order bits
//byte_offset |:= ((byte_offset  and $c000) shr 14);
                    color := self.s.vga_memory[byte_offset];
                    self.s.tile[r*X_TILESIZE + c] := color;
                  end;
                end;
                graphics_tile_update(@self.s.tile, xti*X_TILESIZE, yti*Y_TILESIZE);
                self.s.vga_tile_updated[xti][yti] := 0;
              end;
            end;
        end else
        begin // chain_four = 0, modeX

          if( (iWidth <> old_iWidth) or (iHeight <> old_iHeight) ) then
          begin
            dimension_update(iWidth, iHeight);
            old_iWidth := iWidth;
            old_iHeight := iHeight;
          end;

          start_addr := (self.s.CRTC.reg[$0c] shl 8) or self.s.CRTC.reg[$0d];
          for yti:=0 to iHeight div Y_TILESIZE do
            for xti:=0 to iWidth div X_TILESIZE do
            begin
              // if (self.s.vga_tile_updated[xti][yti]) then begin // end;
              if (self.s.vga_tile_updated[xti][yti])<>0 then
              begin
                for r:=0 to Y_TILESIZE do
                begin
                  for c:=0 to X_TILESIZE do
                  begin
                    pixely := ((yti*Y_TILESIZE) + r);
                    pixelx := ((xti*X_TILESIZE) + c);
                    plane  := (pixelx mod 4);
                    byte_offset := (plane * 65536) +
                                  (pixely * 80) + (pixelx shr 2);
                    color := self.s.vga_memory[start_addr + byte_offset];
                    self.s.tile[r*X_TILESIZE + c] := color;
                  end;
                end;
                graphics_tile_update(@self.s.tile, xti*X_TILESIZE, yti*Y_TILESIZE);
                self.s.vga_tile_updated[xti][yti] := 0;
              end;
            end;
        end;
      end; // case 2

    else
      LogPanic(Format('update: shift_reg = %u', [self.s.graphics_ctrl.shift_reg]));
    end;

    self.s.vga_mem_updated := 0;
    exit;
  end else
  begin // text mode
    case (self.s.graphics_ctrl.memory_mapping) of
      2: // B0000 .. B7FFF
      begin
        iWidth := 8*80;	// TODO: should use font size
        iHeight := 16*25;
        if( (iWidth <> old_iWidth) or (iHeight <> old_iHeight) ) then
        begin
          dimension_update(iWidth, iHeight);
          old_iWidth := iWidth;
          old_iHeight := iHeight;
        end;
//BX_DEBUG(('update(): case 2'));
        (* pass old text snapshot  and new VGA memory contents *)
        start_address := 2*((self.s.CRTC.reg[12] shl 8) + self.s.CRTC.reg[13]);
        cursor_address := 2*((self.s.CRTC.reg[$0e] shl 8) or self.s.CRTC.reg[$0f]);
        if (cursor_address < start_address) then
        begin
          cursor_x := $ffff;
          cursor_y := $ffff;
        end else
        begin
          cursor_x := Trunc(((cursor_address - start_address)/2) / 80);
          cursor_y := Trunc(((cursor_address - start_address)/2)) div 80;
        end;
        cursor_state := (bx_vga.s.CRTC.reg[$0a] shl 8) or bx_vga.s.CRTC.reg[$0b];
        text_update(PAnsiChar(@self.s.text_snapshot),
         PAnsiChar(@self.s.vga_memory[start_address]),cursor_x, cursor_y, cursor_state, 25);
        // screen updated, copy new VGA memory contents into text snapshot
        Move(self.s.vga_memory[start_address], self.s.text_snapshot, 2*80*25); //!!!
        self.s.vga_mem_updated := 0;
      end;

      3: // B8000 .. BFFFF
      begin
        // Verticle Display End: find out how many lines are displayed
        VDE := bx_vga.s.CRTC.reg[$12] or ((bx_vga.s.CRTC.reg[$07] shl 7) and $100) or
            ((bx_vga.s.CRTC.reg[$07] shl 3) and $200);
        // Maximum Scan Line: height of character cell
        MSL := bx_vga.s.CRTC.reg[$09] and $1f;
        rows := Trunc((VDE + 1)/(MSL + 1));
        if (rows > BX_MAX_TEXT_LINES) then
          LogPanic(Format('text rows>%d: %d',[BX_MAX_TEXT_LINES,rows]));

        iWidth := 8 * 80;	// TODO: should use font size
        iHeight := 16 * rows;
        if( (iWidth <> old_iWidth) or (iHeight <> old_iHeight) ) then
        begin
          dimension_update(iWidth, iHeight);
          old_iWidth := iWidth;
          old_iHeight := iHeight;
        end;
        // pass old text snapshot  and new VGA memory contents
        start_address := 2*((self.s.CRTC.reg[12] shl 8) + self.s.CRTC.reg[13]);
        cursor_address := 2*((self.s.CRTC.reg[$0e] shl 8) or self.s.CRTC.reg[$0f]);
        if (cursor_address < start_address) then
        begin
          cursor_x := $ffff;
          cursor_y := $ffff;
        end else
        begin
          cursor_x := Trunc(((cursor_address - start_address)/2)) mod 80;
          cursor_y := trunc(((cursor_address - start_address)/2)) div 80;
        end;
        cursor_state := (bx_vga.s.CRTC.reg[$0a] shl 8) or bx_vga.s.CRTC.reg[$0b];
        text_update(PAnsiChar(@self.s.text_snapshot),
         PAnsiChar(@self.s.vga_memory[start_address]), cursor_x, cursor_y, cursor_state, rows);
        // screen updated, copy new VGA memory contents into text snapshot
        move(self.s.vga_memory[start_address], self.s.text_snapshot, 2*80*rows);
        self.s.vga_mem_updated := 0;
      end;
//      else
//        BX_DEBUG(Format('update(): color text mode: mem map is %u',[self.s.graphics_ctrl.memory_mapping]));
    end;
  end;
end;

function  TVga_c1.mem_read(addr: Bit32u): Bit8u;
var
  offset: Bit32u;
  color_compare, color_dont_care: Bit8u;
  latch0, latch1, latch2, latch3, retval, pixel_val: Bit8u;
  b: unsigned;
begin
  // if in a vbe enabled mode, read from the vbe_memory
  if (self.s.vbe_enabled)<>0 then
  begin
    Result:=vbe_mem_read(addr);
    exit;
  end;

// ??? should get rid of references to shift_reg in self function


  if (self.s.graphics_ctrl.graphics_alpha)<>0 then
  begin
    if (self.s.graphics_ctrl.memory_mapping = 3) then
    begin // $B8000 .. $BFFFF
      if (addr < $B8000) then
      begin
        result:=($ff);
        Exit;
      end;
      offset := addr - $B8000;

      if ( (self.s.graphics_ctrl.shift_reg <> 1) and (self.s.graphics_ctrl.shift_reg <> 2) ) then
        LogPanic(Format('vga_mem_read: shift_reg := %u',[self.s.graphics_ctrl.shift_reg]));

      Result:=(self.s.vga_memory[offset]);
      Exit;
    end;

    if (self.s.graphics_ctrl.memory_mapping <> 1) then
    begin
//      BX_DEBUG(Format('  location %08x',[addr]));
      LogPanic(Format('vga_mem_read: graphics: mapping := %u?',[self.s.graphics_ctrl.memory_mapping]));
      Exit(0);
    end;

    if (addr > $AFFFF) then
      Exit($ff);

    // addr between $A0000 and $AFFFF
    offset := addr - $A0000;
    if ( self.s.sequencer.chain_four ) <> 0 then
    begin
      // mode 13h: 320 x 200 256 color mode: chained pixel representation
      Result := ( self.s.vga_memory[(offset  and not $03) + (offset mod 4)*65536] );
      //Result:=( self.s.vga_memory[(offset  and ~$03) + (offset % 4)*65536] ); !!!
      Exit;
    end;

  end else
  begin
    case (self.s.graphics_ctrl.memory_mapping) of
      1: // $A0000 .. $AFFFF
      begin
        if (addr > $AFFFF) then
        begin
          Result:=($ff);
          Exit;
        end;
        offset := addr - $A0000;
      end;
      2: // $B0000 .. $B7FFF
      begin
        if ((addr < $B0000) or (addr > $B7FFF)) then
        begin
           Result:=($ff);
           Exit;
        end;
        offset := addr - $B0000;
      end;
      3: // $B8000 .. $BFFFF
      begin
        if (addr < $B8000) then
        begin
          Result:=($ff);
          Exit;
        end;
        offset := addr - $B8000;
      end;
    else // $A0000 .. $BFFFF
      offset := addr - $A0000;
    end;
    if (self.s.graphics_ctrl.memory_mapping <> 1) then
      Result:=(self.s.vga_memory[offset]);
  end;

  (* addr between $A0000 and $AFFFF *)
  case (self.s.graphics_ctrl.read_mode) of
    0: (* read mode 0 *)
    begin
      self.s.graphics_ctrl.latch[0] := self.s.vga_memory[          offset];
      self.s.graphics_ctrl.latch[1] := self.s.vga_memory[1*65536 + offset];
      self.s.graphics_ctrl.latch[2] := self.s.vga_memory[2*65536 + offset];
      self.s.graphics_ctrl.latch[3] := self.s.vga_memory[3*65536 + offset];
      Result:=(self.s.graphics_ctrl.latch[self.s.graphics_ctrl.read_map_select]);
      Exit;
    end;

    1: (* read mode 1 *)
    begin

      color_compare   := self.s.graphics_ctrl.color_compare  and $0f;
      color_dont_care := self.s.graphics_ctrl.color_dont_care  and $0f;
      self.s.graphics_ctrl.latch[0] := self.s.vga_memory[          offset];
      latch0 := self.s.graphics_ctrl.latch[0];
      self.s.graphics_ctrl.latch[1] := self.s.vga_memory[1*65536 + offset];
      latch1 :=self.s.graphics_ctrl.latch[1];
      self.s.graphics_ctrl.latch[2] := self.s.vga_memory[2*65536 + offset];
      latch2 := self.s.graphics_ctrl.latch[2];
      self.s.graphics_ctrl.latch[3] := self.s.vga_memory[3*65536 + offset];
      latch3 := self.s.graphics_ctrl.latch[3];
      retval := 0;
      for b:=0 to 8 do
      begin
        pixel_val :=
          ((latch0 shl 0)  and $01) or
          ((latch1 shl 1)  and $02) or
          ((latch2 shl 2)  and $04) or
          ((latch3 shl 3)  and $08);
        latch0 := latch0 shr 1;
        latch1 := latch1 shr 1;
        latch2 := latch2 shr 1;
        latch3 := latch3 shr 1;
        if ( (pixel_val  and color_dont_care) = (color_compare  and color_dont_care) ) then
          retval := retval or (1 shl b);
      end;
      Exit(retval);
    end;
  else
    Exit(0);
  end;
end;

procedure TVga_c1.mem_write(addr:Bit32u; value:Bit8u);
var
  offset: Bit32u;
  new_val: array[0..4] of Bit8u;
  cpu_data_b: array[0..4] of Bit8u;
  new_bit: Bit8u;
  x_tileno, y_tileno, isEven: unsigned ;
  and_mask, bitmask: Bit8u;
  set_reset_b: array[0..4] of Bit8u;
  i, b: unsigned;
begin

{$if BX_SUPPORT_VBE=1}
  // if in a vbe enabled mode, write to the vbe_memory
  if (self.s.vbe_enabled)<>0 then
  begin
        vbe_mem_write(addr,value);
        exit;
  end;
{$ifend}


  if (self.s.graphics_ctrl.graphics_alpha) <> 0 then
  begin
    if (self.s.graphics_ctrl.memory_mapping = 1) then
    begin // $A0000 .. $AFFFF
      // unsigned x_tileno, y_tileno;
      if ( (addr < $A0000) or (addr > $AFFFF) )then
        exit;

      offset := addr - $A0000;
    end else
    if (self.s.graphics_ctrl.memory_mapping = 3) then
    begin // $B8000 .. $BFFFF
      if ( (addr < $B8000) or (addr > $BFFFF) ) then
        exit;

      offset := addr - $B8000;

      (* CGA 32$20$4 start *)
//      isEven := Word(offset >= $2000);
//      if (offset>=$2000)
//        then isEven:=1
//        else isEven:=0;

      if offset >= $2000 {(isEven) <> 0} then
      begin
        y_tileno := offset - $2000;
        y_tileno := y_tileno div (320 div 4);
        y_tileno := y_tileno shl 1; //2 * y_tileno;
        Inc(y_tileno);
        x_tileno := (offset - $2000) mod (320 div 4);
        x_tileno := x_tileno shl 2; //:= 4;
      end else
      begin
        y_tileno := offset div (320 div 4);
        y_tileno := offset shl 1; //2 * offset;
        x_tileno := offset mod (320 div 4);
        x_tileno := x_tileno shl 2; ///:=4;
      end;
      x_tileno:=x_tileno div X_TILESIZE;
      y_tileno:=y_tileno div Y_TILESIZE;

      self.s.vga_mem_updated := 1;
      self.s.vga_tile_updated[x_tileno][y_tileno] := 1;
      (* CGA 32$20$4 end *)
    end else
    begin
      LogPanic(Format('vga_mem_write: graphics: mapping := %u',[self.s.graphics_ctrl.memory_mapping]));
      exit;
    end;

    if ( self.s.sequencer.chain_four ) <> 0 then
    begin
      offset := addr - $A0000;

      //self.s.vga_memory[(offset  and ~$03) + (offset % 4)*65536] := value; !!!!
      self.s.vga_memory[(offset and not $03) + (offset mod 4)*65536] := value;
      // 320 x 200 256 color mode: chained pixel representation
      y_tileno := Trunc((offset / 320) / Y_TILESIZE);
      x_tileno := Trunc((offset mod 320) / X_TILESIZE);
      self.s.vga_mem_updated := 1;
      self.s.vga_tile_updated[x_tileno][y_tileno] := 1;
      exit;
    end;
  end else
  begin
    case (self.s.graphics_ctrl.memory_mapping) of
      1: // $A0000 .. $AFFFF
      begin
        if (addr > $AFFFF) then
          exit;
        offset := addr - $A0000;
      end;
      2: // $B0000 .. $B7FFF
      begin
        if ((addr < $B0000) or (addr > $B7FFF)) then
          exit;
        offset := addr - $B0000;
      end;
      3: // $B8000 .. $BFFFF
      begin
        if (addr < $B8000) then
          exit;
        offset := addr - $B8000;
      end;
    else // $A0000 .. $BFFFF
      offset := addr - $A0000;
    end;
    if (self.s.graphics_ctrl.memory_mapping <> 1) then
    begin
      self.s.vga_memory[offset] := value;
      self.s.vga_mem_updated := 1;
      exit;
    end;
  end;

  (* addr between $A0000 and $AFFFF *)
  case (self.s.graphics_ctrl.write_mode) of

    0: (* write mode 0 *)
    begin
        (* perform rotate on CPU data in case its needed *)
        value := (value shr self.s.graphics_ctrl.data_rotate) or (value shl (8 - self.s.graphics_ctrl.data_rotate));
        bitmask := self.s.graphics_ctrl.bitmask;
        for i:=0 to 4 do begin
          new_val[i] := 0;
        end;
      set_reset_b[0] := (self.s.graphics_ctrl.set_reset shr 0)  and $01;
      set_reset_b[1] := (self.s.graphics_ctrl.set_reset shr 1)  and $01;
      set_reset_b[2] := (self.s.graphics_ctrl.set_reset shr 2)  and $01;
      set_reset_b[3] := (self.s.graphics_ctrl.set_reset shr 3)  and $01;
      and_mask := 1;
      for b := 0 to 7 do
      begin
        if (bitmask  and $01) <> 0 then
        begin (* bit-mask bit set, perform op *)
          for i:=0 to 4 do
          begin
            (* derive bit from set/reset register *)
            if ( (self.s.graphics_ctrl.enable_set_reset shr i)  and $01 )<>0
              then new_bit := (set_reset_b[i] shl b)
              (* derive bit from rotated CPU data *)
              else new_bit := (value  and and_mask);

            case (self.s.graphics_ctrl.raster_op) of
              0: (* replace *)
                new_val[i] := new_val[i] or new_bit;
              1: (* AND with latch data *)
                new_val[i] := new_val[i] or (new_bit  and (self.s.graphics_ctrl.latch[i]  and and_mask));
              2: (* OR with latch data *)
                new_val[i] := new_val[i] or (new_bit or (self.s.graphics_ctrl.latch[i]  and and_mask));
              3: (* XOR with latch data *)
                new_val[i] := new_val[i] or (new_bit xor (self.s.graphics_ctrl.latch[i]  and and_mask));
            else
                LogPanic(Format('vga_mem_write: write mode 0: op := %u',[self.s.graphics_ctrl.raster_op]));
            end;
          end;
        end else
        begin (* bit-mask bit clear, pass data thru from latch *)
          new_val[0] := new_val[0] or (self.s.graphics_ctrl.latch[0]  and and_mask);
          new_val[1] := new_val[1] or (self.s.graphics_ctrl.latch[1]  and and_mask);
          new_val[2] := new_val[2] or (self.s.graphics_ctrl.latch[2]  and and_mask);
          new_val[3] := new_val[3] or (self.s.graphics_ctrl.latch[3]  and and_mask);
        end;
        bitmask := bitmask shr 1;
        and_mask := and_mask shl 1;
      end;
    end;

    1: (* write mode 1 *)
    begin
      for i:=0 to 3 do
        new_val[i] := self.s.graphics_ctrl.latch[i];
    end;

    2: (* write mode 2 *)
    begin
        if (self.s.graphics_ctrl.raster_op)<>0 then
          LogPanic(Format('vga_mem_write: write mode 2: op := %u',[self.s.graphics_ctrl.raster_op]));
      bitmask := self.s.graphics_ctrl.bitmask;
      for i:=0 to 4 do begin
        new_val[i] := 0;
        end;
      cpu_data_b[0] := (value shr 0)  and $01;
      cpu_data_b[1] := (value shr 1)  and $01;
      cpu_data_b[2] := (value shr 2)  and $01;
      cpu_data_b[3] := (value shr 3)  and $01;
      and_mask := 1;
      for b := 0 to 7 do
      begin
        if (bitmask  and $01) <> 0 then
        begin (* bit-mask bit set, perform op *)
          case (self.s.graphics_ctrl.raster_op) of
            0: (* replace: write cpu data unmod_ified *)
            begin
              new_val[0] := new_val[0] or new_val[0] or cpu_data_b[0] shl b;
              new_val[1] := new_val[1] or cpu_data_b[1] shl b;
              new_val[2] := new_val[2] or cpu_data_b[2] shl b;
              new_val[3] := new_val[3] or cpu_data_b[3] shl b;
            end;
            1, (* AND *)
            2, (* OR *)
            3: (* XOR *)
            begin
              LogPanic(Format('vga_mem_write: raster_op := %u?',[self.s.graphics_ctrl.raster_op]));
            end;
          else
            LogPanic(Format('vga_mem_write: raster_op := %u?',[self.s.graphics_ctrl.raster_op]));
          end;
        end else
        begin (* bit-mask bit clear, pass data thru from latch *)
          new_val[0] := new_val[0] or (self.s.graphics_ctrl.latch[0]  and and_mask);
          new_val[1] := new_val[1] or  (self.s.graphics_ctrl.latch[1]  and and_mask);
          new_val[2] := new_val[2] or  (self.s.graphics_ctrl.latch[2]  and and_mask);
          new_val[3] := new_val[3] or  (self.s.graphics_ctrl.latch[3]  and and_mask);
        end;
        bitmask := bitmask shr 1;
        and_mask := and_mask shl 1;
      end;
    end;

    3: (* write mode 3 *)
    begin
      (* perform rotate on CPU data *)
      value := (value shr self.s.graphics_ctrl.data_rotate) or
               (value shl (8 - self.s.graphics_ctrl.data_rotate));
      bitmask := (value  and self.s.graphics_ctrl.bitmask);

      for i:=0 to 4 do
          new_val[i] := 0;

      set_reset_b[0] := (self.s.graphics_ctrl.set_reset shr 0)  and $01;
      set_reset_b[1] := (self.s.graphics_ctrl.set_reset shr 1)  and $01;
      set_reset_b[2] := (self.s.graphics_ctrl.set_reset shr 2)  and $01;
      set_reset_b[3] := (self.s.graphics_ctrl.set_reset shr 3)  and $01;
      and_mask := 1;
      for b:=0 to 7 do
      begin
        if (bitmask  and $01)<>0 then
        begin (* bit-mask bit set, perform op *)
          for i:=0 to 3 do
          begin
            (* derive bit from set/reset register *)
            (* (mch) I can't find any justification for self... *)
            if ( (* (mch) *) 1 or ((self.s.graphics_ctrl.enable_set_reset shr i)  and $01 ))<>0 then
            begin
              // (mch) My guess is that the function select logic should go here
              case (self.s.graphics_ctrl.raster_op) of
                0: // write
                begin
                  new_val[i] := new_val[i] or (set_reset_b[i] shl b);
                end;
                1: // AND
                begin
                  new_val[i] := new_val[i] or ((set_reset_b[i] shl b) and
                  self.s.graphics_ctrl.latch[i]  and (1 shl b));
                end;
                2: // OR
                begin
                  new_val[i] := new_val[i] or (set_reset_b[i] shl b) or
                  (self.s.graphics_ctrl.latch[i]  and (1 shl b));
                end;
                3: // XOR
                begin
                  new_val[i] := new_val[i] or ((set_reset_b[i] shl b) xor
                  self.s.graphics_ctrl.latch[i]  and (1 shl b));
                end;
              end;
            end
            (* derive bit from rotated CPU data *)
            else
              new_val[i] := new_val[i] or (value  and and_mask);
          end;
        end else
        begin (* bit-mask bit clear, pass data thru from latch *)
          new_val[0] := new_val[0] or (self.s.graphics_ctrl.latch[0]  and and_mask);
          new_val[1] := new_val[1] or (self.s.graphics_ctrl.latch[1]  and and_mask);
          new_val[2] := new_val[2] or (self.s.graphics_ctrl.latch[2]  and and_mask);
          new_val[3] := new_val[3] or (self.s.graphics_ctrl.latch[3]  and and_mask);
        end;
        bitmask := bitmask shr 1;
        and_mask := and_mask shl 1;
      end;
    end;

  else
    LogPanic(Format('vga_mem_write: write mode %u ?',[self.s.graphics_ctrl.write_mode]));
  end;

  if (self.s.sequencer.map_mask  and $0f)<>0 then
  begin
    self.s.vga_mem_updated := 1;
    if (self.s.sequencer.map_mask_bit[0])<>0 then
      self.s.vga_memory[0*65536 + offset] := new_val[0];

    if (self.s.sequencer.map_mask_bit[1])<>0 then
      self.s.vga_memory[1*65536 + offset] := new_val[1];

    if (self.s.sequencer.map_mask_bit[2])<>0 then
      self.s.vga_memory[2*65536 + offset] := new_val[2];

    if (self.s.sequencer.map_mask_bit[3])<>0 then
      self.s.vga_memory[3*65536 + offset] := new_val[3];

    //x_tileno := (offset % (self.s.scan_bits/8)) / (X_TILESIZE / 8);!!!
    x_tileno := (offset mod (self.s.scan_bits div 8)) div (X_TILESIZE div 8);
    y_tileno := (offset div (self.s.scan_bits div 8)) div Y_TILESIZE;
    self.s.vga_tile_updated[x_tileno][y_tileno] := 1;
  end;
end;
(*procedure bx_vga_c.get_text_snapshot(Bit8u **text_snapshot, unsigned *txHeight,
                                                   unsigned *txWidth)
begin
  unsigned VDE, MSL;

  if (!self.s.graphics_ctrl.graphics_alpha) then begin
    *text_snapshot := @self.s.text_snapshot[0];
    VDE := bx_vga.s.CRTC.reg[$12] |
          ((bx_vga.s.CRTC.reg[$07]shl7)@$100) |
          ((bx_vga.s.CRTC.reg[$07]shl3)@$200);
    MSL := bx_vga.s.CRTC.reg[$09]  and $1f;
    *txHeight := (VDE+1)/(MSL+1);
    *txWidth := self.s.CRTC.reg[1] + 1;
  end; else begin
    *txHeight := 0;
    *txWidth := 0;
  end;
end;  *)

{  procedure
bx_vga_c.dump_status(procedure)
begin
  BX_INFO(('s.misc_output.color_emulation := %u',
            (unsigned) self.s.misc_output.color_emulation));
  BX_INFO(('s.misc_output.enable_ram := %u',
            (unsigned) self.s.misc_output.enable_ram));
  BX_INFO(('s.misc_output.clock_select := %u',
            (unsigned) self.s.misc_output.clock_select));
  if (self.s.misc_output.clock_select = 0)
    BX_INFO(('  25Mhz 640 horiz pixel clock'));
  else
    BX_INFO(('  28Mhz 720 horiz pixel clock'));
  BX_INFO(('s.misc_output.select_high_bank := %u',
            (unsigned) self.s.misc_output.select_high_bank));
  BX_INFO(('s.misc_output.horiz_sync_pol := %u',
            (unsigned) self.s.misc_output.horiz_sync_pol));
  BX_INFO(('s.misc_output.vert_sync_pol := %u',
            (unsigned) self.s.misc_output.vert_sync_pol));
  switch ( (self.s.misc_output.vert_sync_pol shl 1) |
           self.s.misc_output.horiz_sync_pol ) then begin
    case 0: BX_INFO(('  (reserved')); break;
    case 1: BX_INFO(('  400 lines')); break;
    case 2: BX_INFO(('  350 lines')); break;
    case 3: BX_INFO(('  480 lines')); break;
    default: BX_INFO(('  ???'));
    end;

  BX_INFO(('s.graphics_ctrl.odd_even := %u',
            (unsigned) self.s.graphics_ctrl.odd_even));
  BX_INFO(('s.graphics_ctrl.chain_odd_even := %u',
            (unsigned) self.s.graphics_ctrl.chain_odd_even));
  BX_INFO(('s.graphics_ctrl.shift_reg := %u',
            (unsigned) self.s.graphics_ctrl.shift_reg));
  BX_INFO(('s.graphics_ctrl.graphics_alpha := %u',
            (unsigned) self.s.graphics_ctrl.graphics_alpha));
  BX_INFO(('s.graphics_ctrl.memory_mapping := %u',
            (unsigned) self.s.graphics_ctrl.memory_mapping));
  switch (self.s.graphics_ctrl.memory_mapping) then begin
    case 0: BX_INFO(('  A0000-BFFFF')); break;
    case 1: BX_INFO(('  A0000-AFFFF')); break;
    case 2: BX_INFO(('  B0000-B7FFF')); break;
    case 3: BX_INFO(('  B8000-BFFFF')); break;
    default: BX_INFO(('  ???'));
    end;

  BX_INFO(('s.sequencer.extended_mem := %u',
            (unsigned) self.s.sequencer.extended_mem));
  BX_INFO(('s.sequencer.odd_even := %u (inverted)',
            (unsigned) self.s.sequencer.odd_even));
  BX_INFO(('s.sequencer.chain_four := %u',
            (unsigned) self.s.sequencer.chain_four));

  BX_INFO(('s.attribute_ctrl.video_enabled := %u',
            (unsigned) self.s.attribute_ctrl.video_enabled));
  BX_INFO(('s.attribute_ctrl.mode_ctrl.graphics_alpha := %u',
            (unsigned) self.s.attribute_ctrl.mode_ctrl.graphics_alpha));
  BX_INFO(('s.attribute_ctrl.mode_ctrl.display_type := %u',
            (unsigned) self.s.attribute_ctrl.mode_ctrl.display_type));
  BX_INFO(('s.attribute_ctrl.mode_ctrl.internal_palette_size := %u',
            (unsigned) self.s.attribute_ctrl.mode_ctrl.internal_palette_size));
  BX_INFO(('s.attribute_ctrl.mode_ctrl.pixel_clock_select := %u',
            (unsigned) self.s.attribute_ctrl.mode_ctrl.pixel_clock_select));
end;

}
(*  procedure
bx_vga_c.redraw_area(unsigned x0, unsigned y0, unsigned width,
                      unsigned height)
begin
  unsigned xi, yi, x1, y1;

  self.s.vga_mem_updated := 1;

  if (self.s.graphics_ctrl.graphics_alpha) then begin
    // graphics mode
    self.s.vga_mem_updated := 1;
    x1 := x0 + width  - 1;
    y1 := y0 + height - 1;

    for (yi:=0; yi<480; yi+:=Y_TILESIZE) then begin
      for (xi:=0; xi<640; xi+:=X_TILESIZE) then begin
        // is redraw rectangle outside x boundaries of self tile?
        if (x1 < xi) continue;
        if (x0 > (xi+X_TILESIZE-1)) continue;

        // is redraw rectangle outside y boundaries of self tile?
        if (y1 < yi) continue;
        if (y0 > (yi+X_TILESIZE-1)) continue;
        self.s.vga_tile_updated[xi/X_TILESIZE][yi/Y_TILESIZE] := 1;
        end;
      end;
    end;
  else begin
    // text mode
    memset(self.s.text_snapshot, 0,
           sizeof(self.s.text_snapshot));
    self.s.vga_mem_updated := 1;
    end;
end;*)


function TVga_c1.vbe_mem_read(addr:Bit32u):Bit8u;
var
  offset: Bit32u;
begin
  offset := addr - $A0000;
  Result := (self.s.vbe_memory[self.s.vbe_bank * 65536 + offset]);
end;

procedure TVga_c1.vbe_mem_write(addr:Bit32u;value:Bit8u);
var
  offset: Bit32u;
  x_tileno, y_tileno: unsigned;
  count: Integer;
begin
  offset := self.s.vbe_bank * 65536 + (addr - $A0000);

  // check for out of memory write
  if (offset < sizeof(self.s.vbe_memory))
    then self.s.vbe_memory[offset]:=value
    else

  if (count < 100) then  // make sure we don't flood the logfile
    Inc(count);

  // only update the UI when writing 'onscreen'
  // FIXME: deal with logical scan lengths  and display start sometime (when new DISPI)
  if (offset < self.s.vbe_visable_screen_size) then
  begin
    y_tileno := (offset div self.s.vbe_xres) div Y_TILESIZE;
    x_tileno := (offset mod self.s.vbe_xres) div X_TILESIZE;
    self.s.vga_mem_updated := 1;
    self.s.vga_tile_updated[x_tileno][y_tileno] := 1;
  end;
end;

function TVga_c1.vbe_read_handler(self_ptr:Pointer; address: Bit32u; io_len:unsigned):Bit32u;
begin
  if (address=VBE_DISPI_IOPORT_INDEX)
    then Exit(self.s.vbe_curindex)
    else
    case (self.s.vbe_curindex) of
      VBE_DISPI_INDEX_ID: // Display Interface ID check
        Exit(VBE_DISPI_ID0);

      VBE_DISPI_INDEX_XRES: // x resolution
        Exit(self.s.vbe_xres);

      VBE_DISPI_INDEX_YRES: // y resolution
        Exit(self.s.vbe_yres);

      VBE_DISPI_INDEX_BPP: // bpp
        Exit(self.s.vbe_bpp);

      VBE_DISPI_INDEX_ENABLE: // vbe enabled
        Exit(self.s.vbe_enabled);

      VBE_DISPI_INDEX_BANK: // current bank
        Exit(self.s.vbe_bank);
    end;
end;

procedure TVga_c1.vbe_write_handler(self_ptr:pointer; address:Bit32u; value:Bit32u; io_len:unsigned);
var
  count: integer;
begin
  if (address=VBE_DISPI_IOPORT_INDEX)
    then self.s.vbe_curindex := Bit16u(value)
    else
  begin
    // data register
    // FIXME: maybe do some 'sanity' checks on received data?
    
    case (self.s.vbe_curindex) of
      VBE_DISPI_INDEX_ID: // Display Interface ID check
        if (value = VBE_DISPI_ID0) then
          // make sure we don't flood the logfile
          if (count < 100) then
            inc(count);

      VBE_DISPI_INDEX_XRES: // set xres
        // check that we don't set xres during vbe enabled
        if (self.s.vbe_enabled = 0) then
        begin
          // check for within max xres range
          if (value <= VBE_DISPI_MAX_XRES) then
            self.s.vbe_xres := Bit16u(value);
        end;

      VBE_DISPI_INDEX_YRES: // set yres
        // check that we don't set yres during vbe enabled
        if (self.s.vbe_enabled = 0) then
        begin
          // check for within max yres range
          if (value <= VBE_DISPI_MAX_YRES) then
            self.s.vbe_yres := Bit16u(value);
        end;

      VBE_DISPI_INDEX_BPP: // set bpp
        // check that we don't set bpp during vbe enabled
        if (self.s.vbe_enabled = 0) then
          // check for correct bpp range
          if (value = VBE_DISPI_BPP_8) then
            self.s.vbe_bpp := Bit16u(value);

      VBE_DISPI_INDEX_BANK: // set bank
      begin
        value := value  and $ff ; // FIXME lobyte := vbe bank A?
        // check for max bank nr
        if (value < (VBE_DISPI_TOTAL_VIDEO_MEMORY_MB * 1024 div 64)) then
          self.s.vbe_bank:=value;
      end;

      VBE_DISPI_INDEX_ENABLE: // enable video
      begin
        if value <> 0 then
        begin
          // FIXME: VBE allows for *not* clearing the screen when setting a mode
          // FIXME: make dependant on bpp (currently only 8bpp := 1byte)
          self.s.vbe_visable_screen_size := (self.s.vbe_xres) * (self.s.vbe_yres) * 1;
          FillChar(self.s.vbe_memory, self.s.vbe_visable_screen_size, 0);

          dimension_update(self.s.vbe_xres, self.s.vbe_yres);
        end;
        self.s.vbe_enabled:=Bool(value);
      end;
    end;
  end;
end;

end.

