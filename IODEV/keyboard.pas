/////////////////////////////////////////////////////////////////////////
// $Id: keyboard.cc,v 1.53 2002/03/27 16:42:54 bdenney Exp $
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


// Now features proper implementation of keyboard opcodes $F4 to $F6
// Silently ignores PS/2 keyboard extensions ($F7 to $FD)
// Explicit panic on resend ($FE)
//
// Emmanuel Marty <core@ggi-project.org>

// NB: now the PS/2 mouse support is in, outb changes meaning
// in conjunction with auxb
// auxb = 0 @ and outb = 0  :=> both buffers empty (nothing to read)
// auxb = 0 @ and outb = 1  :=> keyboard controller output buffer full
// auxb = 1 @ and outb = 0  :=> not used
// auxb = 1 @ and outb = 1  :=> mouse output buffer full.
// (das)

// Notes from Christophe Bothamy <cbbochs@free.fr>
//
// This file includes code from Ludovic Lange (http://ludovic.lange.free.fr)
// Implementation of 3 scancodes sets mf1,mf2,mf3 with or without translation.
// Default is mf2 with translation
// Ability to switch between scancodes sets
// Ability to turn translation on or off
unit keyboard;

interface

uses Config,iodev,cmos;

const
  BX_KBD_ELEMENTS     = 16;
  BX_MOUSE_BUFF_SIZE  = 48;
  MOUSE_MODE_RESET    = 10;
  MOUSE_MODE_STREAM   = 11;
  MOUSE_MODE_REMOTE   = 12;
  MOUSE_MODE_WRAP     = 13;
  BX_KBD_CONTROLLER_QSIZE = 5;
  VERBOSE_KBD_DEBUG   = 0;
  BX_KEY_PRESSED      = $00000000;
  BX_KEY_RELEASED     = $80000000;

type

  TKeyBoard = class
  public
    s: packed record
      kbd_controller:packed record
        // status bits matching the status port
        pare: bool; // Bit7, 1= parity error from keyboard/mouse - ignored.
        tim : bool;  // Bit6, 1= timeout from keyboard - ignored.
        auxb: bool; // Bit5, 1= mouse data waiting for CPU to read.
        keyl: bool; // Bit4, 1= keyswitch in lock position - ignored.
        c_d : bool;  //  Bit3, 1=command to port 64h, 0=data to port 60h */
        sysf: bool; // Bit2,
        inpb: bool; // Bit1,
        outb: bool; // Bit0, 1= keyboard data or mouse data ready for CPU
                      //       check aux to see which. Or just keyboard
                      //       data before AT style machines

        // internal to our version of the keyboard controller
        kbd_clock_enabled: bool;
        aux_clock_enabled: bool;
        allow_irq1: bool;
        allow_irq12: bool;
        kbd_output_buffer: Bit8u;
        aux_output_buffer: Bit8u;
        last_comm: Bit8u;
        expecting_port60h: Bit8u;
        expecting_mouse_parameter: Bit8u;
        last_mouse_command: Bit8u;
        timer_pending: Bit32u;
        irq1_requested: bool;
        irq12_requested: bool;
        scancodes_translate: bool;
        expecting_scancodes_set: bool;
        current_scancodes_set: Bit8u;
      end;
      mouse : packed record
        sample_rate:Bit8u;
        resolution_cpmm:Bit8u; // resolution in counts per mm
        scaling:Bit8u;
        mode:Bit8u;
        saved_mode:Bit8u;  // the mode prior to entering wrap mode
        enable:bool;
        button_status:Bit8u;
        delayed_dx:Bit16s;
        delayed_dy:Bit16s;
      end;
       kbd_internal_buffer: packed record
        num_elements:Integer;
        buffer:array[0..BX_KBD_ELEMENTS] of Bit8u;
        head:Integer;
        expecting_typematic:Bool;
        expecting_led_write:bool;
        delay:Bit8u;
        repeat_rate:Bit8u;
        led_status:Bit8u;
        scanning_enabled:bool;
       end;
        mouse_internal_buffer: packed record
          num_elements:Integer;
          buffer:array[0..BX_MOUSE_BUFF_SIZE] of Bit8u;
          head:Integer;
        end;
       controller_Q: array[0..BX_KBD_CONTROLLER_QSIZE] of Bit8u;
       controller_Qsize:unsigned;
       controller_Qsource:unsigned; // 0=keyboard, 1=mouse
     end;
      pastebuf: array of Bit8u;   // ptr to bytes to be pasted, or NULL if none in progress
      pastebuf_len: Bit32u; // length of pastebuf
      pastebuf_ptr: Bit32u; // ptr to next byte to be added to hw buffer
      pastedelay: Bit32u;   // count before paste
      devices: Pbx_devices_c;

      constructor Create;
      destructor  Done;
      procedure   init(d:Pbx_devices_c; cmos:Pbx_cmos_c);
      procedure   gen_scancode(Key:Bit32u);
      procedure   paste_bytes(bytes:PBit8u; length:Bit32s);
      procedure   service_paste_buf;
      function    get_kbd_enable:Bit8u;
      procedure   mouse_motion(delta_x:Integer; delta_y:Integer;button_state:unsigned);
      procedure   mouse_enabled_changed(enabled:bool);
      procedure   create_mouse_packet(force_enq:bool);
      //procedure   mouse_button(mouse_state:unsigned);
      function    periodic( usec_delta:Bit32u ):unsigned;
      procedure   put_scancode( code:PChar; count:Integer );

      // update the paste delay based on bx_options.Okeyboard_paste_delay
      procedure   paste_delay_changed ();

  private

    function  get_status_byte:Bit8u;
    function  get_resolution_byte:Bit8u;
    function  read_handler(this_ptr:pointer; address:Bit32u; io_len:unsigned):Bit32u;
    procedure write_handler(this_ptr:Pointer; address:Bit32u; value:Bit32u; io_len:unsigned);
    procedure resetinternals(powerup:Bool);
    procedure set_kbd_clock_enable(value:Bit8u);
    procedure set_aux_clock_enable(value:Bit8u);
    procedure kbd_ctrl_to_kbd(value:Bit8u);
    procedure kbd_ctrl_to_mouse(value:Bit8u);
    procedure kbd_enQ(scancode:Bit8u);
    procedure kbd_enQ_imm(val:Bit8u);
    procedure activate_timer;
    procedure controller_enQ(data:Bit8u; source:unsigned);
    function  mouse_enQ_packet(b1:Bit8u;b2:Bit8u;b3:Bit8u):Bool;
    procedure mouse_enQ(mouse_data:Bit8u);
  end;

var
  bx_keyboard: TKeyBoard;
  multiple: Integer = 0;
  count_before_paste: integer=0;
  pooltimer: Word = 0;

implementation

uses Service, SysUtils, Pic, Cpu, KeyMap, Scd, Math, Gui32;

constructor TKeyBoard.Create;
begin
  // constructor
  // should zero out state info here???
  FillChar( s, sizeof(s), 0 );
//  BX_DEBUG(('Init $Id: keyboard.cc,v 1.53 2002/03/27 16:42:54 bdenney Exp $'));
end;

destructor TKeyBoard.Done;
begin
  // destructor
//  BX_DEBUG(('Exit.'));
end;


// flush internal buffer and reset keyboard settings to power-up condition
procedure TKeyBoard.resetinternals(powerup: Bool);
var
  i:Bit32u;
begin
  self.s.kbd_internal_buffer.num_elements := 0;
  I := 0;
  while i < BX_KBD_ELEMENTS do
  begin
    self.s.kbd_internal_buffer.buffer[i] := 0;
    Inc(i);
  end;
  self.s.kbd_internal_buffer.head := 0;

  self.s.kbd_internal_buffer.expecting_typematic := 0;

  // Default scancode set is mf2 with translation
  self.s.kbd_controller.expecting_scancodes_set := 0;
  self.s.kbd_controller.current_scancodes_set := 1;
  self.s.kbd_controller.scancodes_translate := 1;

  if (powerup)<>0 then
  begin
    self.s.kbd_internal_buffer.expecting_led_write := 0;
    self.s.kbd_internal_buffer.delay := 1; // 500 mS
    self.s.kbd_internal_buffer.repeat_rate := $0b; // 10.9 chars/sec
  end;
end;

procedure TKeyBoard.init(d:Pbx_devices_c; cmos:Pbx_cmos_c);
var
  i:Bit32u;
begin
//  BX_DEBUG(('Init $Id: keyboard.cc,v 1.53 2002/03/27 16:42:54 bdenney Exp $'));

  self.devices := d;

  self.devices^.register_irq(1, '8042 Keyboard controller');

  self.devices^.register_io_read_handler(self, read_handler,
                                      $0060, '8042 Keyboard controller');
  self.devices^.register_io_read_handler(self, read_handler,
                                      $0064, '8042 Keyboard controller');
  self.devices^.register_io_write_handler(self, write_handler,
                                      $0060, '8042 Keyboard controller');
  self.devices^.register_io_write_handler(self, write_handler,
                                      $0064, '8042 Keyboard controller');

  resetinternals(1);

  self.s.kbd_internal_buffer.led_status := 0;
  self.s.kbd_internal_buffer.scanning_enabled := 1;

  self.s.mouse_internal_buffer.num_elements := 0;
  i := 0;
  while i < BX_MOUSE_BUFF_SIZE do
  begin
    self.s.mouse_internal_buffer.buffer[i] := 0;
    Inc(I);
  end;
  self.s.mouse_internal_buffer.head := 0;

  //BX_INFO(Format('kbd: %04d outb 0 auxb 0',[__LINE__])); // das
  self.s.kbd_controller.pare := 0;
  self.s.kbd_controller.tim  := 0;
  self.s.kbd_controller.auxb := 0;
  self.s.kbd_controller.keyl := 1;
  self.s.kbd_controller.c_d  := 1;
  self.s.kbd_controller.sysf := 0;
  self.s.kbd_controller.inpb := 0;
  self.s.kbd_controller.outb := 0;

  self.s.kbd_controller.kbd_clock_enabled := 1;
  self.s.kbd_controller.aux_clock_enabled := 0;
  self.s.kbd_controller.allow_irq1        := 1;
  self.s.kbd_controller.allow_irq12       := 1;
  self.s.kbd_controller.kbd_output_buffer := 0;
  self.s.kbd_controller.aux_output_buffer := 0;
  self.s.kbd_controller.last_comm         := 0;
  self.s.kbd_controller.expecting_port60h := 0;
  self.s.kbd_controller.irq1_requested    := 0;
  self.s.kbd_controller.irq12_requested   := 0;

//bx_debug(( '# Okeyboard_serial_delay is %u usec',
//        (unsigned) bx_options.Okeyboard_serial_delay^.get ()));
  self.s.kbd_controller.timer_pending := 0;

  // Mouse initialization stuff
  self.s.mouse.sample_rate     := 100; // reports per second
  self.s.mouse.resolution_cpmm := 4;   // 4 counts per millimeter
  self.s.mouse.scaling         := 1;   (* 1:1 (default) *)
  self.s.mouse.mode            := MOUSE_modE_RESET;
  self.s.mouse.enable          := 0;
  self.s.mouse.delayed_dx      := 0;
  self.s.mouse.delayed_dy      := 0;
  I:=0;
  while (i<BX_KBD_CONTROLLER_QSIZE) do
  begin
    self.s.controller_Q[i] := 0;
    Inc(I);
  end;
  self.s.controller_Qsize   := 0;
  self.s.controller_Qsource := 0;

  // clear paste buffer
  self.pastebuf     := NULL;
  self.pastebuf_len := 0;
  self.pastebuf_ptr := 0;
  self.paste_delay_changed ();

  // mouse port installed on system board
  cmos^.s.reg[$14] := cmos^.s.reg[$14] or $04;
end;

procedure TKeyBoard.paste_delay_changed;
begin
  self.pastedelay := KEYBOARD_PASTE_DELAY div BX_IODEV_HANDLER_PERIOD;
  LogInfo(Format('will paste characters every %d keyboard ticks',[self.pastedelay]));
end;

  // static IO port read callback handler
  // redirects to non-static class handler to aprocedure virtual functions

// read function - the big picture:
// if address = data port then
//    if byte for mouse then return it
//    else if byte for keyboard then return it
// else address= status port
//    assemble the status bits and return them.
//
function TKeyBoard.read_handler(this_ptr: pointer; address: Bit32u; io_len: unsigned): Bit32u;
var
  val_: Bit8u;
  i: unsigned;
begin
  if (io_len > 1) then
    LogPanic(Format('kbd: io read to address %08x, len:=%u',[unsigned(address),
      unsigned(io_len)]));


//bx_debug(Format( 'read from port $%04x', [address]));

  if (address = $60) then
  begin (* output buffer *)
    if (self.s.kbd_controller.auxb) <> 0 then
    begin (* mouse byte available *)
      val_ := self.s.kbd_controller.aux_output_buffer;
      self.s.kbd_controller.aux_output_buffer := 0;
      //BX_INFO(('kbd: %04d outb 0 auxb 0',__LINE__)); // das
      self.s.kbd_controller.outb := 0;
      self.s.kbd_controller.auxb := 0;
      self.s.kbd_controller.irq12_requested := 0;

      if (self.s.controller_Qsize) <> 0 then
      begin
        self.s.kbd_controller.aux_output_buffer := self.s.controller_Q[0];
        //BX_INFO(('kbd: %04d outb 1 auxb 1',__LINE__)); // das
        self.s.kbd_controller.outb := 1;
        self.s.kbd_controller.auxb := 1;

        if (self.s.kbd_controller.allow_irq12) <> 0 then
          self.s.kbd_controller.irq12_requested := 1;

        i := 0;
        while i < self.s.controller_Qsize - 1 do
        begin
          // move Q elements towards head of queue by one
          self.s.controller_Q[i] := self.s.controller_Q[i+1];
          inc(I);
        end;
        dec(self.s.controller_Qsize);
      end;

//bx_debug(('mouse: ___io_read aux := $%02x', (unsigned) val_));

      //self.devices^.pic^.lower_irq(12);
      bx_pic.lower_irq(12);
      activate_timer;
//      BX_DEBUG(Format('[KBD]READ(%02x) (from mouse) := %02x',[unsigned(address),
//          unsigned(val_)]));
      Result:=val_;
      exit;
    end else
    if (self.s.kbd_controller.outb) <> 0 then
    begin (* kbd byte available *)
      val_ := self.s.kbd_controller.kbd_output_buffer;
      //BX_INFO(('kbd: %04d outb 0 auxb 0',__LINE__)); // das
      self.s.kbd_controller.outb := 0;
      self.s.kbd_controller.auxb := 0;
      self.s.kbd_controller.irq1_requested := 0;
//bx_debug(( '___io_read kbd'));

      if (self.s.controller_Qsize) <> 0 then
      begin
        self.s.kbd_controller.aux_output_buffer := self.s.controller_Q[0];
		//BX_INFO(('kbd: %04d outb 1 auxb 1',__LINE__)); // das
        self.s.kbd_controller.outb := 1;
        self.s.kbd_controller.auxb := 1;

        if (self.s.kbd_controller.allow_irq1)<>0 then
          self.s.kbd_controller.irq1_requested := 1;

        i:=0;
        while i<self.s.controller_Qsize-1 do
        begin
          // move Q elements towards head of queue by one
          self.s.controller_Q[i] := self.s.controller_Q[i+1];
          Inc(i);
        end;
//    	BX_DEBUG(Format('[KBD]s.controller_Qsize: %02X',[self.s.controller_Qsize]));
        dec(self.s.controller_Qsize);
      end;

      //self.devices^.pic^.lower_irq(1);
      bx_pic.lower_irq(1);
      activate_timer;
//      BX_DEBUG(Format('[KBD]READ(%02x) := %02x',[unsigned(address),unsigned(val_)]));
      result:=val_;
      Exit;
    end else
    begin
//        BX_DEBUG(Format('[KBD]num_elements := %d',[self.s.kbd_internal_buffer.num_elements]));
//        BX_DEBUG(('read from port 60h with outb empty'));
      val_   := self.s.kbd_controller.kbd_output_buffer;
      Result := self.s.kbd_controller.kbd_output_buffer;
      Exit;
    end;
  end else
  if (address = $64) then
  begin (* status register *)
    Result := (self.s.kbd_controller.pare shl 7)  or
              (self.s.kbd_controller.tim  shl 6)  or
              (self.s.kbd_controller.auxb shl 5)  or
              (self.s.kbd_controller.keyl shl 4)  or
              (self.s.kbd_controller.c_d  shl 3)  or
              (self.s.kbd_controller.sysf shl 2)  or
              (self.s.kbd_controller.inpb shl 1)  or
              self.s.kbd_controller.outb;
    exit;
  end;

  LogPanic(Format('unknown address in io read to keyboard port %x',[unsigned(address)]));
  Result := 0; (* keep compiler happy *)
end;
  // static IO port write callback handler
  // redirects to non-static class handler to aprocedure virtual functions

procedure TKeyBoard.write_handler(this_ptr:Pointer; address:Bit32u; value:Bit32u; io_len:unsigned);
var
  command_byte: Bit8u;
  kbd_initialized: integer;
  scan_convert, disable_keyboard, disable_aux: Bool;
begin
//{$if BX_USE_KEY_SMF=0}
//  bx_keyb_c *class_ptr := (bx_keyb_c *) self_ptr;
//
//  class_ptr^.write(address, value, io_len);
//end;
//
//procedure bx_keyb_c.write( Bit32u   address, Bit32u   value, unsigned io_len)
//begin
//{$else}
//  //UNUSED(self_ptr);
//{$ifend}  // !BX_USE_KEY_SMF
  kbd_initialized := 0;

  if (io_len > 1) then
    LogPanic(Format('kbd: io write to address %08x, len:=%u',[
             unsigned(address), unsigned(io_len)]));

//  BX_DEBUG(Format('[KBD]keyboard: 8-bit write to %04x := %02x',[unsigned(address),
//    unsigned(value)]));


//    BX_DEBUG(Format('[KBD]WRITE(%02x) := %02x',[address,value]));

  case address of
    $60:
    begin // input buffer
    // if expecting data byte from command last sent to port 64h
      if (self.s.kbd_controller.expecting_port60h) <> 0 then
      begin
        self.s.kbd_controller.expecting_port60h := 0;
        // data byte written last to $60
        self.s.kbd_controller.c_d := 0;
        if (self.s.kbd_controller.inpb)<>0 then
          LogPanic(('write to port 60h, not ready for write'));
        case (self.s.kbd_controller.last_comm) of
          $60: // write command byte
          begin
            scan_convert := (value shr 6)  and $01;
            disable_aux      := (value shr 5)  and $01;
            disable_keyboard := (value shr 4)  and $01;
            self.s.kbd_controller.sysf := (value shr 2)  and $01;
            self.s.kbd_controller.allow_irq1  := (value shr 0)  and $01;
            self.s.kbd_controller.allow_irq12 := (value shr 1)  and $01;
            set_kbd_clock_enable(Word(disable_keyboard=0));
            set_aux_clock_enable(Word(disable_aux=0));
            if ((self.s.kbd_controller.allow_irq12<>0) and (self.s.kbd_controller.auxb<>0))
              then self.s.kbd_controller.irq12_requested := 1
              else
            if ((self.s.kbd_controller.allow_irq1 <> 0) and (self.s.kbd_controller.outb <> 0))
              then self.s.kbd_controller.irq1_requested := 1;

//        			BX_DEBUG(Format( ' allow_irq12 set to %u',[unsigned(self.s.kbd_controller.allow_irq12)]));
            if ( scan_convert = 0 ) then
              LogError(('keyboard: (mch) scan convert turned off'));

            // (mch) NT needs self
            self.s.kbd_controller.scancodes_translate := scan_convert;
          end;
          $d1: // write output port
          begin
//              BX_DEBUG(Format('[KBD]write output port with value %02xh',[unsigned(value)]));
            bx_pc_system.set_enable_a20( Bool((value  and $02) <> 0 ));
            if ((value  and $01) = 0) then
              LogPanic(('IO write: processor reset requested!'));
          end;
          $d4: // Write to mouse
          begin
            // I don't think self enables the AUX clock
            //set_aux_clock_enable(1); // enable aux clock line
              kbd_ctrl_to_mouse(value);
            // ??? should I reset to previous value of aux enable?
          end;

          $d3: // write mouse output buffer
          begin
          // Queue in mouse output buffer
            controller_enQ(value, 1);
          end;

        else
          LogPanic(Format('=:= unsupported write to port 60h(lastcomm:=%02x): %02x',[
            unsigned(self.s.kbd_controller.last_comm), unsigned(value)]));
        end;
      end else
      begin
        // data byte written last to $60
        self.s.kbd_controller.c_d := 0;
        self.s.kbd_controller.expecting_port60h := 0;
        (* pass byte to keyboard *)
        (* ??? should conditionally pass to mouse device here ??? *)
        if (self.s.kbd_controller.kbd_clock_enabled=0) then
          LogError(Format('keyboard disabled  and send of byte %02x to kbd',[unsigned(value)]));

        kbd_ctrl_to_kbd(value);
      end;
    end;

    $64: // control register
    begin
      // command byte written last to $64
      self.s.kbd_controller.c_d := 1;
      self.s.kbd_controller.last_comm := value;
      // most commands NOT expecting port60 write next
      self.s.kbd_controller.expecting_port60h := 0;

      case value of
        $20: // get keyboard command byte
        begin
//              BX_DEBUG(('get keyboard command byte'));
          // controller output buffer must be empty
          if (self.s.kbd_controller.outb)<>0 then
          begin
            LogError(Format('kbd: OUTB set and command $%02x encountered',[value]));
            Halt; // vai alla fine del case
          end;

          command_byte :=
              (self.s.kbd_controller.scancodes_translate shl 6) or
              (Word(self.s.kbd_controller.aux_clock_enabled = 0) shl 5) or
              (Word(self.s.kbd_controller.kbd_clock_enabled = 0) shl 4) or
              (0 shl 3) or
              (self.s.kbd_controller.sysf shl 2) or
              (self.s.kbd_controller.allow_irq12 shl 1) or
                (self.s.kbd_controller.allow_irq1  shl 0);
          controller_enQ(command_byte, 0);
        end; // following byte written to port 60h is command byte
        $60: self.s.kbd_controller.expecting_port60h := 1;// write command byte

        $a1: LogError(('Dummy out Green PC for now : $a1'));

        $a7: set_aux_clock_enable(0);// disable the aux device

      $a8: set_aux_clock_enable(1);// enable the aux device

        $a9: // Test Mouse Port
        begin
          // controller output buffer must be empty
          if (self.s.kbd_controller.outb)<>0 then
          begin
            LogPanic(Format('kbd: OUTB set and command $%02x encountered',[value]));
            Halt; // Vai alla fine del case
          end;
          controller_enQ($00, 0); // no errors detected
        end;
        $aa: // motherboard controller self test
        begin
//            BX_DEBUG(('Self Test'));
          if( kbd_initialized = 0 ) then
          begin
            self.s.controller_Qsize := 0;
            self.s.kbd_controller.outb := 0;
            Inc(kbd_initialized);
          end;
          // controller output buffer must be empty
          if (self.s.kbd_controller.outb) <> 0 then
          begin
            LogError(Format('kbd: OUTB set and command $%02x encountered',[value]));
            Halt; // Vai alla fine del case
          end;
  // (mch) Why is self commented out??? Enabling
          self.s.kbd_controller.sysf := 1; // self test complete
          controller_enQ($55, 0); // controller OK
        end;
      $ab: // Interface Test
        begin
          // controller output buffer must be empty
          if (self.s.kbd_controller.outb)<>0 then
          begin
            LogPanic(Format('kbd: OUTB set and command $%02x encountered',[value]));
            Halt; // Vai alla fine del case
          end;
          controller_enQ($00, 0);
        end;
        $ad: set_kbd_clock_enable(0);// disable keyboard

        $ae: set_kbd_clock_enable(1);// enable keyboard

        $c0: // read input port
        begin
          // controller output buffer must be empty
          if (self.s.kbd_controller.outb) <> 0 then
          begin
            LogPanic(Format('kbd: OUTB set and command $%02x encountered',[value]));
            Halt; // vai alla fine del case
          end;
        // keyboard power normal
          controller_enQ($00, 0);
        end;
        $d0: // read output port: next byte read from port 60h
        begin
//            BX_DEBUG(('io write to port 64h, command d0h (partial)'));
          // controller output buffer must be empty
          if (self.s.kbd_controller.outb) <> 0 then
          begin
            LogPanic(Format('kbd: OUTB set and command $%02x encountered',[value]));
            Halt;
          end;
          controller_enQ(
            (self.s.kbd_controller.auxb shl 5) or
            (self.s.kbd_controller.outb shl 4) or
            (bx_pc_system.get_enable_a20 shl 1) or
            $01, 0);
        end;
             // following byte to port 60h written to output port
        $d1: self.s.kbd_controller.expecting_port60h := 1;// write output port: next byte written to port 60h
              // following byte to port 60h written to output port as mouse write.
        $d3: self.s.kbd_controller.expecting_port60h := 1;// write mouse output buffer
            // following byte written to port 60h
        $d4:  self.s.kbd_controller.expecting_port60h := 1;// write to mouse


        $d2, // write keyboard output buffer
        $dd, // Disable A20 Address Line
        $df, // Enable A20 Address Line
        $c1, // Continuous Input Port Poll, Low
        $c2, // Continuous Input Port Poll, High
        $e0: // Read Test Inputs
            LogPanic(Format('io write $64: command := %02xh', [unsigned(value)]));

        $fe: // System Reset, transition to real mode
        begin
          LogInfo(('system reset'));
          bx_pc_system.ResetSignal( PCS_SET ); (* XXX is self right? *)
          bx_cpu.reset(0);
          // Use bx_pc_system if necessary bx_cpu.reset_cpu();
          // bx_pc_system.ResetSignal( PCS_SET );
        end;

      else
        begin
          if ((value = $ff) or ((value >= $f0) and (value <= $fd))) then
            exit;
          LogPanic(Format('unsupported io write to keyboard port %x, value := %x',[unsigned(address),
            unsigned(value)]));
        end;
      end;
    end;
  end;
end;

// service_paste_buf() transfers data from the paste buffer to the hardware
// keyboard buffer.  It tries to transfer as many chars as possible at a
// time, but because different chars require different numbers of scancodes
// we have to be conservative.  Note that self process depends on the
// keymap tables to know what chars correspond to what keys, and which
// chars require a shift or other modifier.
procedure TKeyBoard.service_paste_buf;
var
  fill_threshold: Integer;
  byte: Bit8u;
  entry: PBXKeyEntry;
begin
  if (self.pastebuf = nil) then
    exit;
//  BX_DEBUG (Format('service_paste_buf: ptr at %d out of %d',[self.pastebuf_ptr, self.pastebuf_len]));
  fill_threshold := BX_KBD_ELEMENTS - 8;
  while (self.pastebuf_ptr < self.pastebuf_len) do
  begin
    if (self.s.kbd_internal_buffer.num_elements >= fill_threshold) then
      exit;
    // there room in the buffer for a keypress and a key release.
    // send one keypress and a key release.
    byte := self.pastebuf[self.pastebuf_ptr];
    entry := bx_keymap.getKeyASCII (byte);
    if (entry = nil)
      then LogError (Format('paste character $%02x ignored',[byte]))
      else
      begin
  //      BX_DEBUG (Format('pasting character $%02x. baseKey is %04x',[byte, entry^.baseKey]));
        if (entry^.modKey <> BX_KEYMAP_UNKNOWN) then
          bx_keyboard.gen_scancode(entry^.modKey);
        bx_keyboard.gen_scancode(entry^.baseKey);
        bx_keyboard.gen_scancode(entry^.baseKey or BX_KEY_RELEASED);
        if (entry^.modKey <> BX_KEYMAP_UNKNOWN) then
          bx_keyboard.gen_scancode(entry^.modKey or BX_KEY_RELEASED);
      end;
    Inc(self.pastebuf_ptr);
  end;
  SetLength(self.pastebuf,0);
  // reached end of pastebuf.  free the memory it was using.
end;

// paste_bytes schedules an arbitrary number of ASCII characters to be
// inserted into the hardware queue as it become available.  Any previous
// paste which is still in progress will be thrown out.  BYTES is a pointer
// to a region of memory containing the chars to be pasted. When the paste
// is complete, the keyboard code will call free(BYTES).
procedure TKeyBoard.paste_bytes (bytes:PBit8u; length:Bit32s);
begin
//  BX_DEBUG (Format('paste_bytes: %d bytes',[length]));
  if (self.pastebuf)<>nil then
    LogError (Format('previous paste was not completed!  %d chars lost',[self.pastebuf_len - self.pastebuf_ptr]));

  SetLength(self.pastebuf,length);
  Move(self.pastebuf, bytes, length);

  self.pastebuf_ptr := 0;
  self.pastebuf_len := length;
  self.service_paste_buf ();
end;

procedure TKeyBoard.gen_scancode(Key: Bit32u);
//var
//  escaped: Bit8u;
//  scancode: PChar;
//  i: Bit8u;
begin
{  BX_DEBUG(Format( 'gen_scancode %lld %x',[bx_pc_system.time_ticks(), key]));

  if (self.s.kbd_controller.scancodes_translate=0) then
	BX_DEBUG(('keyboard: gen_scancode with scancode_translate cleared'));

  BX_DEBUG(Format('[KBD]gen_scancode(): scancode: %08x',[key]));

  // Ignore scancode if keyboard clock is driven low
  if (self.s.kbd_controller.kbd_clock_enabled=0) then
    exit;

  // Ignore scancode if scanning is disabled
  if (self.s.kbd_internal_buffer.scanning_enabled=0) then
    exit;

  // Switch between make and break code
  if (key  and BX_KEY_RELEASED) then
    scancode:=scancodes[(key and $FF),self.s.kbd_controller.current_scancodes_set].brek
  else
    scancode:=scancodes[(key and $FF),self.s.kbd_controller.current_scancodes_set].make;

  if (self.s.kbd_controller.scancodes_translate) then begin
    // Translate before send
    escaped:=$00;
    i:=0;

    while i<strlen(scancode ) do begin
      if (scancode[i] = #$F0) then
        escaped:=$80
      else begin
        kbd_enQ(translation8042[scancode[i]] or escaped );
      	BX_DEBUG(('keyboard: writing translated %02x',translation8042[scancode[i] ]orescaped));
        escaped:=$00;
      end;
      Inc(i);
    end;
  end;
  else begin
    // Send raw data
    for (i:=0; i<strlen( (const char *)scancode ); i++) then begin
      kbd_enQ( scancode[i] );
      BX_DEBUG(('keyboard: writing raw %02x',scancode[i]));
    end;
  end;}
end;

procedure TKeyBoard.set_kbd_clock_enable(value:Bit8u);
var
  prev_kbd_clock_enabled: Bool;
begin
  if (value = 0)
    then self.s.kbd_controller.kbd_clock_enabled := 0
    else
    begin
      (* is another byte waiting to be sent from the keyboard ? *)
      prev_kbd_clock_enabled := self.s.kbd_controller.kbd_clock_enabled;
      self.s.kbd_controller.kbd_clock_enabled := 1;

      if ((prev_kbd_clock_enabled = 0) and (self.s.kbd_controller.outb = 0)) then
        activate_timer;
    end;
end;

procedure TKeyBoard.set_aux_clock_enable(value: Bit8u);
var
  prev_aux_clock_enabled: Bool;
begin

//  BX_DEBUG(Format('[KBD]set_aux_clock_enable(%u)',[value]));
  if (value = 0)
    then self.s.kbd_controller.aux_clock_enabled := 0
    else
    begin
      (* is another byte waiting to be sent from the keyboard ? *)
      prev_aux_clock_enabled := self.s.kbd_controller.aux_clock_enabled;
      self.s.kbd_controller.aux_clock_enabled := 1;
      if ((prev_aux_clock_enabled = 0) and (self.s.kbd_controller.outb = 0)) then
        activate_timer;
    end;
end;

function TKeyBoard.get_kbd_enable:Bit8u;
begin
//  BX_DEBUG(Format('[KBD]get_kbd_enable(): getting kbd_clock_enabled of: %02x',[self.s.kbd_controller.kbd_clock_enabled]));

  Result := self.s.kbd_controller.kbd_clock_enabled;
end;

procedure TKeyBoard.controller_enQ(data:Bit8u; source:unsigned);
begin
  // source is 0 for keyboard, 1 for mouse

//  BX_DEBUG(Format('[KBD]controller_enQ(%02x) source:=%02x',[data,source]));

  if (self.s.kbd_controller.outb)<>0 then
    LogError(('controller_enQ(): OUTB set!'));

  // see if we need to Q self byte from the controller
  // remember self includes mouse bytes.
  if (self.s.kbd_controller.outb) <> 0 then
  begin
    if (self.s.controller_Qsize >= BX_KBD_CONTROLLER_QSIZE) then
      LogPanic(('controller_enq(): controller_Q full!'));

    self.s.controller_Q[self.s.controller_Qsize] := data;
    Inc(self.s.controller_Qsize);
    self.s.controller_Qsource := source;
    exit;
  end;

  // the Q is empty
  if (source = 0) then
  begin // keyboard
    self.s.kbd_controller.kbd_output_buffer := data;
    //BX_INFO(('kbd: %04d outb 1 auxb 0',__LINE__)); // das
    self.s.kbd_controller.outb := 1;
    self.s.kbd_controller.auxb := 0;
    self.s.kbd_controller.inpb := 0;

    if (self.s.kbd_controller.allow_irq1) <> 0 then
      self.s.kbd_controller.irq1_requested := 1;
  end else
  begin // mouse
    self.s.kbd_controller.aux_output_buffer := data;
    //BX_INFO(('kbd: %04d outb 1 auxb 1',__LINE__)); // das
    self.s.kbd_controller.outb := 1;
    self.s.kbd_controller.auxb := 1;
    self.s.kbd_controller.inpb := 0;
    if (self.s.kbd_controller.allow_irq12)<>0 then
      self.s.kbd_controller.irq12_requested := 1;
  end;
end;

procedure TKeyBoard.kbd_enQ_imm(val:Bit8u);
var
  tail: Integer;
begin
  if (self.s.kbd_internal_buffer.num_elements >= BX_KBD_ELEMENTS) then
  begin
    LogPanic(('internal keyboard buffer full (imm)'));
    exit;
  end;

  (* enqueue scancode in multibyte internal keyboard buffer *)
  tail := (self.s.kbd_internal_buffer.head + self.s.kbd_internal_buffer.num_elements) mod BX_KBD_ELEMENTS;

  //BX_INFO(('kbd: %04d outb 1',__LINE__)); // das
  self.s.kbd_controller.kbd_output_buffer := val;
  self.s.kbd_controller.outb := 1;

  if (self.s.kbd_controller.allow_irq1) <> 0 then
    self.s.kbd_controller.irq1_requested := 1;
end;

procedure TKeyBoard.kbd_enQ(scancode:Bit8u);
var
  tail: Integer;
begin
//  BX_DEBUG(Format('[KBD]enQ(%02x)',[scancode]));
  if (self.s.kbd_internal_buffer.num_elements >= BX_KBD_ELEMENTS) then
  begin
    LogInfo(Format('internal keyboard buffer full, ignoring scancode.(%02x)',[scancode]));
    exit;
  end;

  (* enqueue scancode in multibyte internal keyboard buffer *)
//  BX_DEBUG(Format('[KBD]enQ: putting scancode %02x in internal buffer',[scancode]));
  tail := (self.s.kbd_internal_buffer.head + self.s.kbd_internal_buffer.num_elements) mod
    BX_KBD_ELEMENTS;
  self.s.kbd_internal_buffer.buffer[tail] := scancode;
  Inc(Self.s.kbd_internal_buffer.num_elements);

  if ((self.s.kbd_controller.outb = 0) and (self.s.kbd_controller.kbd_clock_enabled <> 0)) then
  begin
    activate_timer;
//	BX_DEBUG(('activating timer...'));
    exit;
  end;
//bx_debug( '# not activating timer...');
//bx_debug(Format( '#   allow_irq1 := %u',[self.s.kbd_controller.allow_irq1]));
//bx_debug(Format( '#   outb       := %u',[self.s.kbd_controller.outb]));
//bx_debug(Format( '#   clock_enab := %u',[self.s.kbd_controller.kbd_clock_enabled]));
//bx_debug(Format( '#   out_buffer := %u',[self.s.kbd_controller.kbd_output_buffer]));
end;

function TKeyBoard.mouse_enQ_packet(b1:Bit8u;b2:Bit8u;b3:Bit8u):Bool;
begin
  if ((self.s.mouse_internal_buffer.num_elements + 3) >= BX_MOUSE_BUFF_SIZE) then
  begin
    Result := 0; (* buffer doesn't have the space *)
    Exit;
  end;

  mouse_enQ(b1);
  mouse_enQ(b2);
  mouse_enQ(b3);
  Result := 1;
end;

procedure TKeyBoard.mouse_enQ(mouse_data:Bit8u);
var
  tail: Integer;
begin
//  BX_DEBUG(Format('[KBD]mouse_enQ(%02x)', [mouse_data]));
  if (self.s.mouse_internal_buffer.num_elements >= BX_MOUSE_BUFF_SIZE) then
  begin
    LogError(Format('mouse: internal mouse buffer full, ignoring mouse data.(%02x)',[mouse_data]));
    exit;
  end;
//bx_debug(Format( '# mouse_enq() aux_clock_enabled := %u',[self.s.kbd_controller.aux_clock_enabled]));

  (* enqueue mouse data in multibyte internal mouse buffer *)
  tail := (self.s.mouse_internal_buffer.head + self.s.mouse_internal_buffer.num_elements) mod
    BX_MOUSE_BUFF_SIZE;
  self.s.mouse_internal_buffer.buffer[tail] := mouse_data;
  Inc(self.s.mouse_internal_buffer.num_elements);

  if ((self.s.kbd_controller.outb = 0) and (self.s.kbd_controller.aux_clock_enabled <> 0)) then
  begin
    activate_timer;
//bx_debug(( '# activating timer...'));
    exit;
  end;
//bx_debug( '# not activating timer...');
//bx_debug(Format( '#   allow_irq12:= %u',[self.s.kbd_controller.allow_irq12]));
//bx_debug(Format( '#   outb       := %u',[self.s.kbd_controller.outb]));
//bx_debug(Format( '#   clock_enab := %u',[self.s.kbd_controller.aux_clock_enabled]));
//bx_debug(Format( '#   out_buffer := %u',[self.s.kbd_controller.aux_output_buffer]));
end;

procedure TKeyBoard.kbd_ctrl_to_kbd(value:Bit8u);
var
  cps: double;
begin
//  BX_DEBUG(Format('[KBD]controller passed byte %02xh to keyboard',[value]));
  if (self.s.kbd_internal_buffer.expecting_typematic) <> 0 then
  begin
    self.s.kbd_internal_buffer.expecting_typematic := 0;
    self.s.kbd_internal_buffer.delay := (value shr 5)  and $03;
    case (self.s.kbd_internal_buffer.delay) of
      0: LogInfo(('setting delay to 250 mS (unused)'));
      1: LogInfo(('setting delay to 500 mS (unused)'));
      2: LogInfo(('setting delay to 750 mS (unused)'));
      3: LogInfo(('setting delay to 1000 mS (unused)'));
    end;
    self.s.kbd_internal_buffer.repeat_rate := value  and $1f;
    cps := 1 /((8 + (value  and $07)) * exp(log10(2) * ((value shr 3)  and $03)) * 0.00417);
    LogInfo(Format('setting repeat rate to %.1f cps (unused)',[cps]));
    kbd_enQ($FA); // send ACK
    exit;
  end;

  if (self.s.kbd_internal_buffer.expecting_led_write) <> 0 then
  begin
    self.s.kbd_internal_buffer.expecting_led_write := 0;
    self.s.kbd_internal_buffer.led_status := value;
//    BX_DEBUG(Format('[KBD]LED status set to %02x',[self.s.kbd_internal_buffer.led_status]));
    kbd_enQ($FA); // send ACK %%%
    exit;
  end;

  if (self.s.kbd_controller.expecting_scancodes_set) <> 0 then
  begin
    self.s.kbd_controller.expecting_scancodes_set := 0;
    if( value <> 0 ) then
    begin
      if( value < 4 ) then
      begin
        self.s.kbd_controller.current_scancodes_set := (value - 1);
        LogInfo(Format('Switched to scancode set %d\n',[self.s.kbd_controller.current_scancodes_set]));
        kbd_enQ($FA);
      end else
      begin
        LogError(Format('Received scancodes set out of range: %d\n',[value]));
        kbd_enQ($FF); // send ERROR
      end;
    end else
      // Send current scancodes set to port $60
      kbd_enQ( 1 + (self.s.kbd_controller.current_scancodes_set) );

    exit;
  end;

  case (value) of
    $00: // ??? ignore and let OS timeout with no response
    begin
      kbd_enQ($FA); // send ACK %%%
      exit;
    end;

    $05: // ???
	  // (mch) trying to get self to work...
    begin
      self.s.kbd_controller.sysf := 1;
      kbd_enQ_imm($fe);
      exit;
    end;

    $ed: // LED Write
    begin
      self.s.kbd_internal_buffer.expecting_led_write := 1;
      kbd_enQ_imm($FA); // send ACK %%%
      exit;
    end;

    $ee: // echo
    begin
      kbd_enQ($EE); // return same byte (EEh) as echo diagnostic
      exit;
    end;

    $f0: // Select alternate scan code set
    begin
      self.s.kbd_controller.expecting_scancodes_set := 1;
//      BX_DEBUG(('Expecting scancode set info...\n'));
      kbd_enQ($FA); // send ACK
      exit;
    end;

    $f2:  // identify keyboard
    begin
      LogInfo(('identify keyboard command received'));

        {kbd_enQ($FA);
          kbd_enQ($AB);
            kbd_enQ($41);}
      // XT sends nothing, AT sends ACK
      // MFII with translation sends ACK+ABh+41h
      // MFII without translation sends ACK+ABh+83h
(*      if (bx_options.Okeyboard_type^.get() <> BX_KBD_XT_TYPE) then begin
        kbd_enQ($FA);
        if (bx_options.Okeyboard_type^.get() = BX_KBD_MF_TYPE) then begin
          kbd_enQ($AB);

          if(self.s.kbd_controller.scancodes_translate)
            kbd_enQ($41);
          else
            kbd_enQ($83);
          end;
        end;
      exit; !!!*)
    end;

    $f3:  // typematic info
    begin
      self.s.kbd_internal_buffer.expecting_typematic := 1;
      LogInfo(('setting typematic info'));
      kbd_enQ($FA); // send ACK
      exit;
    end;

    $f4:  // flush scancodes buffer and modes, then enable keyboard
    begin
      resetinternals(0);
      kbd_enQ($FA); // send ACK
      self.s.kbd_internal_buffer.scanning_enabled := 1;
      exit;
    end;

    $f5:  // reset keyboard to power-up settings and disable scanning
    begin
      resetinternals(1);
      kbd_enQ($FA); // send ACK
      self.s.kbd_internal_buffer.scanning_enabled := 0;
      LogInfo(('reset-disable command received'));
      exit;
    end;

    $f6:  // reset keyboard to power-up settings and enable scanning
    begin
      resetinternals(1);
      kbd_enQ($FA); // send ACK
      self.s.kbd_internal_buffer.scanning_enabled := 1;
      LogInfo(('reset-enable command received'));
      exit;
    end;

    $f7,  // PS/2 Set All Keys To Typematic
    $f8,  // PS/2 Set All Keys to Make/Break
    $f9,  // PS/2 PS/2 Set All Keys to Make
    $fa,  // PS/2 Set All Keys to Typematic Make/Break
    $fb,  // PS/2 Set Key Type to Typematic
    $fc,  // PS/2 Set Key Type to Make/Break
    $fd:  // PS/2 Set Key Type to Make
    begin
      // Silently ignore and let the OS timeout, for now.
      // If anyone has code around that makes use of that, I can
      // provide documentation on their behavior (ask core@ggi-project.org)
      exit;
    end;

    $fe:  // resend. aiiee.
    begin
      LogPanic( ('got $FE (resend)'));
      exit;
    end;

    $ff:  // reset: internal keyboard reset and afterwards the BAT
    begin
//      BX_DEBUG(('reset command received'));
      kbd_enQ($FA); // send ACK
      kbd_enQ($AA); // BAT test passed
      exit;
    end;

    $d3: kbd_enQ($fa);

  else
    (* XXX fix self properly:
    http://panda.cs.ndsu.nodak.edu/~achapwes/PICmicro/mouse/mouse.html
    http://sourceforge.net/tracker/index.php?func:=detail@aid:=422457@group_id:=12580@atid:=112580
     *)
    begin
      LogError(Format('kbd_ctrl_to_kbd(): got value of %02x',[value]));
      kbd_enQ($FA); (* send ACK ??? *)
      exit;
    end;
  end;
end;

function  TKeyBoard.periodic( usec_delta:Bit32u ):unsigned;
var
  retval: Bit8u;
begin
  //UNUSED( usec_delta );
  inc(multiple);
  if (multiple = 10) then
  begin
    multiple := 0;
    handle_events;
  end;

  if (self.s.kbd_controller.kbd_clock_enabled ) <> 0 then
  begin
    Inc(count_before_paste);
    if(count_before_paste >= self.pastedelay) then
    begin //!!!!!!!!!!!!!!
      // after the paste delay, consider adding moving more chars
      // from the paste buffer to the keyboard buffer.
      self.service_paste_buf ();
      count_before_paste := 0;
    end;
  end;

  retval := self.s.kbd_controller.irq1_requested or (self.s.kbd_controller.irq12_requested shl 1);
  self.s.kbd_controller.irq1_requested := 0;
  self.s.kbd_controller.irq12_requested := 0;

  if ( self.s.kbd_controller.timer_pending = 0 ) then
  begin
    Result := retval;
    Exit;
  end;

  if ( usec_delta >= self.s.kbd_controller.timer_pending )
    then self.s.kbd_controller.timer_pending := 0
    else
    begin
      Dec(self.s.kbd_controller.timer_pending,usec_delta);
      Result := retval;
      exit;
    end;

  if (self.s.kbd_controller.outb) <> 0 then
  begin
    Result := retval;
    exit;
  end;

  (* nothing in outb, look for possible data xfer from keyboard or mouse *)
  if ((self.s.kbd_controller.kbd_clock_enabled <> 0) and (self.s.kbd_internal_buffer.num_elements <> 0)) then
  begin
//  bx_debug( '#   servicing keyboard code');
//    BX_DEBUG(('service_keyboard: key in internal buffer waiting'));
    self.s.kbd_controller.kbd_output_buffer :=
      self.s.kbd_internal_buffer.buffer[self.s.kbd_internal_buffer.head];
        //BX_INFO(('kbd: %04d outb 1',__LINE__)); // das
    self.s.kbd_controller.outb := 1;
    // commented out since self would override the current state of the
    // mouse buffer flag - no bug seen - just seems wrong (das)
    //    self.s.kbd_controller.auxb := 0;
//    bx_debug( '# ___kbd.periodic kbd');
    self.s.kbd_internal_buffer.head := (self.s.kbd_internal_buffer.head + 1) mod
      BX_KBD_ELEMENTS;
    dec(self.s.kbd_internal_buffer.num_elements);
    if (self.s.kbd_controller.allow_irq1)<>0 then
      self.s.kbd_controller.irq1_requested := 1;
  end else
  begin
    //create_mouse_packet(0);
    if ((self.s.kbd_controller.aux_clock_enabled <> 0) and
       (self.s.mouse_internal_buffer.num_elements <> 0)) then
    begin
    //bx_debug( '#   servicing mouse code');
    //      BX_DEBUG(('service_keyboard: key(from mouse) in internal buffer waiting'));
          self.s.kbd_controller.aux_output_buffer :=
      self.s.mouse_internal_buffer.buffer[self.s.mouse_internal_buffer.head];

            //BX_INFO(('kbd: %04d outb 1 auxb 1',__LINE__)); //das
      self.s.kbd_controller.outb := 1;
      self.s.kbd_controller.auxb := 1;
    //bx_debug( '# ___kbd:periodic aux');
      self.s.mouse_internal_buffer.head := (self.s.mouse_internal_buffer.head + 1) mod
      BX_MOUSE_BUFF_SIZE;
      dec(self.s.mouse_internal_buffer.num_elements);
    //BX_DEBUG(Format('[KBD]#   allow12 := %u',[self.s.kbd_controller.allow_irq12]));
      if (self.s.kbd_controller.allow_irq12) <> 0 then
        self.s.kbd_controller.irq12_requested := 1;
    end;
  end;
  Result := retval;
end;

procedure TKeyBoard.activate_timer;
begin
  if (self.s.kbd_controller.timer_pending = 0)
    then self.s.kbd_controller.timer_pending := KEYBOARD_DELAY;
end;

function TKeyBoard.get_resolution_byte: Bit8u;
begin
  case s.mouse.resolution_cpmm of
    1: Result := 0;
    2: Result := 1;
    4: Result := 2;
    8: Result := 3;
  else
      LogPanic('mouse: invalid resolution_cpmm');
  end;
end;

function TKeyBoard.get_status_byte:Bit8u;
var
  ret: Bit8u;
begin
  ret := $40 * WORD(s.mouse.mode = MOUSE_MODE_REMOTE);
//  if (s.mouse.mode = MOUSE_MODE_REMOTE)
//    then ret := $40
//    else ret := 0;

  ret := ret or  (s.mouse.enable shl 5);
  if s.mouse.scaling = 1
    then ret := ret
    else ret := ret or 1 shl 4;

  ret := ret or ((s.mouse.button_status and $1) shl 2);
  ret := ret or ((s.mouse.button_status and $2) shl 0);
  Result := ret;
end;

procedure TKeyBoard.kbd_ctrl_to_mouse(value:Bit8u);
begin
//  BX_DEBUG(Format('[KBD]MOUSE: kbd_ctrl_to_mouse(%02xh)', [value]));
//  BX_DEBUG(Format('[KBD]  enable := %u',[self.s.mouse.enable]));
//  BX_DEBUG(Format('[KBD]  allow_irq12 := %u',[self.s.kbd_controller.allow_irq12]));
//  BX_DEBUG(Format('[KBD]  aux_clock_enabled := %u',[self.s.kbd_controller.aux_clock_enabled]));
//  bx_debug(Format( 'MOUSE: kbd_ctrl_to_mouse(%02xh)',[ value]));

  // an ACK ($FA) is always the first response to any valid input
  // received from the system other than Set-Wrap-mode  and Resend-Command


  if (self.s.kbd_controller.expecting_mouse_parameter) <> 0 then
  begin
    self.s.kbd_controller.expecting_mouse_parameter := 0;
    case (self.s.kbd_controller.last_mouse_command) of
      $f3: // Set Mouse Sample Rate
      begin
        self.s.mouse.sample_rate := value;
//           BX_DEBUG(Format('[KBD][mouse] Sampling rate set: %d Hz',[value]));
        controller_enQ($FA, 1); // ack
      end;

      $e8: // Set Mouse Resolution
      begin
        case (value) of
          0: self.s.mouse.resolution_cpmm := 1;
          1: self.s.mouse.resolution_cpmm := 2;
          2: self.s.mouse.resolution_cpmm := 4;
          3: self.s.mouse.resolution_cpmm := 8;
        else LogPanic(Format('[mouse] Unknown resolution %d', [value]));
        end;
//		   BX_DEBUG(Format('[KBD][mouse] Resolution set to %d counts per mm',[self.s.mouse.resolution_cpmm]));
        controller_enQ($FA, 1); // ack
      end;
     else
       LogPanic(Format('MOUSE: unknown last command (%02xh)',[self.s.kbd_controller.last_mouse_command]));
     end;
  end else
  begin
    self.s.kbd_controller.expecting_mouse_parameter := 0;
    self.s.kbd_controller.last_mouse_command := value;

    // test for wrap mode first
    if (self.s.mouse.mode = MOUSE_modE_WRAP) then
    begin
      // if not a reset command or reset wrap mode
      // then just echo the byte.
      if ((value <> $ff) and (value <> $ec)) then
      begin
        if (True) then
          LogInfo(Format('[mouse] wrap mode: Ignoring command %$02.',[value]));
        controller_enQ(value, 1);
        // bail out
        exit;
      end;
    end;

    case ( value ) of
      $e6: // Set Mouse Scaling to 1:1
      begin
        controller_enQ($FA, 1); // ACK
        self.s.mouse.scaling := 2;
//	      BX_DEBUG(('[mouse] Scaling set to 1:1'));
      end;

      $e7: // Set Mouse Scaling to 2:1
      begin
        controller_enQ($FA, 1); // ACK
        self.s.mouse.scaling         := 2;
//	  BX_DEBUG(('[mouse] Scaling set to 2:1'));
      end;

      $e8: // Set Mouse Resolution
      begin
        controller_enQ($FA, 1); // ACK
        self.s.kbd_controller.expecting_mouse_parameter := 1;
      end;

      $ea: // Set Stream mode
      begin
        if (True) then
          LogInfo(('[mouse] Mouse stream mode on.'));
        self.s.mouse.mode := MOUSE_modE_STREAM;
        controller_enQ($FA, 1); // ACK
      end;

      $ec: // Reset Wrap mode
      begin
        // unless we are in wrap mode ignore the command
        if ( self.s.mouse.mode = MOUSE_modE_WRAP) then
        begin
          LogInfo(('[mouse] Mouse wrap mode off.'));
          // restore previous mode except disable stream mode reporting.
          // ### TODO disabling reporting in stream mode
          self.s.mouse.mode := self.s.mouse.saved_mode;
          controller_enQ($FA, 1); // ACK
        end;
      end;
      $ee: // Set Wrap mode
      begin
        // ### TODO flush output queue.
        // ### TODO disable interrupts if in stream mode.
        LogInfo(('[mouse] Mouse wrap mode on.'));
        self.s.mouse.saved_mode := self.s.mouse.mode;
        self.s.mouse.mode := MOUSE_modE_WRAP;
        controller_enQ($FA, 1); // ACK
      end;

      $f0: // Set Remote mode (polling mode, i.e. not stream mode.)
      begin
        LogInfo(('[mouse] Mouse remote mode on.'));
        // ### TODO should we flush/discard/ignore any already queued packets?
        self.s.mouse.mode := MOUSE_modE_REMOTE;
        controller_enQ($FA, 1); // ACK
      end;


      $f2: // Read Device Type
      begin
        controller_enQ($FA, 1); // ACK
        controller_enQ($00, 1); // Device ID
//	  BX_DEBUG(('[mouse] Read mouse ID'));
      end;

      $f3: // Set Mouse Sample Rate (sample rate written to port 60h)
      begin
        controller_enQ($FA, 1); // ACK
        self.s.kbd_controller.expecting_mouse_parameter := 1;
      end;

      $f4: // Enable (in stream mode)
      begin
        self.s.mouse.enable := 1;
        controller_enQ($FA, 1); // ACK
//	  BX_DEBUG(('[mouse] Mouse enabled (stream mode)'));
      end;

      $f5: // Disable (in stream mode)
      begin
        self.s.mouse.enable := 0;
        controller_enQ($FA, 1); // ACK
//	  BX_DEBUG(('[mouse] Mouse disabled (stream mode)'));
      end;

      $f6: // Set Defaults
      begin
        self.s.mouse.sample_rate     := 100; (* reports per second (default) *)
        self.s.mouse.resolution_cpmm := 4; (* 4 counts per millimeter (default) *)
        self.s.mouse.scaling         := 1;   (* 1:1 (default) *)
        self.s.mouse.enable          := 0;
        self.s.mouse.mode            := MOUSE_modE_STREAM;
        controller_enQ($FA, 1); // ACK
//	  BX_DEBUG(('[mouse] Set Defaults'));
      end;

      $ff: // Reset
      begin
        self.s.mouse.sample_rate     := 100; (* reports per second (default) *)
        self.s.mouse.resolution_cpmm := 4; (* 4 counts per millimeter (default) *)
        self.s.mouse.scaling         := 1;   (* 1:1 (default) *)
        self.s.mouse.mode            := MOUSE_modE_RESET;
        self.s.mouse.enable          := 0;
        (* (mch) NT expects an ack here *)
        controller_enQ($FA, 1); // ACK
        controller_enQ($AA, 1); // completion code
        controller_enQ($00, 1); // ID code (normal mouse, wheelmouse has id $3)
//	  BX_DEBUG(('[mouse] Mouse reset'));
      end;

      $e9: // Get mouse information
      begin
        // should we ack here? (mch): Yes
        controller_enQ($FA, 1); // ACK
        controller_enQ(get_status_byte, 1); // status
        controller_enQ(get_resolution_byte, 1); // resolution
        controller_enQ(self.s.mouse.sample_rate, 1); // sample rate
//	  BX_DEBUG(('[mouse] Get mouse information'));
      end;

      $eb: // Read Data (send a packet when in Remote mode)
      begin
        controller_enQ($FA, 1); // ACK
        // perhaps we should be adding some movement here.
        mouse_enQ_packet( ((self.s.mouse.button_status  and $0f)or$08),
        $00, $00 ); // bit3 of first byte always set
        //assumed we really aren't in polling mode, a rather odd assumption.
//      BX_ERROR(('[mouse] Warning: Read Data command partially supported.'));
      end;
    else
      //FEh Resend
      LogPanic(Format('MOUSE: kbd_ctrl_to_mouse(%02xh)',[value]));
    end;
  end;
end;

procedure TKeyBoard.create_mouse_packet(force_enq: bool);
var
  b1, b2, b3: Bit8u;
  delta_x: Bit16s;
  delta_y: Bit16s;
  button_state: Bit8u;
begin
  //  BX_DEBUG('Calling create_mouse_packet: force_enq:=%d\n',force_enq);
  if((self.s.mouse_internal_buffer.num_elements <> 0) and (force_enq = 0)) then
    exit;

  //  BX_DEBUG('Got to first milestone: force_enq:=%d\n',force_enq);

  delta_x := self.s.mouse.delayed_dx;
  delta_y := self.s.mouse.delayed_dy;
  button_state := self.s.mouse.button_status or $08;

  if((force_enq = 0) and (delta_x = 0) and (delta_y = 0)) then
    exit;

  //  BX_DEBUG('Got to second milestone: delta_x:=%d, delta_y:=%d\n',delta_x,delta_y);

  if(delta_x > 254 ) then delta_x := 254;
  if(delta_x < -254) then delta_x := -254;
  if(delta_y > 254 ) then delta_y := 254;
  if(delta_y < -254) then delta_y := -254;

  b1 := (button_state  and $0f) or $08; // bit3 always set

  if ( (delta_x>=0) and (delta_x<=255) ) then
  begin
    b2 := delta_x;
    dec(self.s.mouse.delayed_dx, Delta_x);
  end else
  if ( delta_x > 255 ) then
  begin
    b2 := $ff;
    dec(self.s.mouse.delayed_dx, 255);
  end else
  if ( delta_x >= -256 ) then
  begin
    b2 := delta_x;
    b1 := b1 or $10;
    dec(self.s.mouse.delayed_dx, delta_x);
  end else
  begin
    b2 := $00;
    b1 := b1 or $10;
    inc(self.s.mouse.delayed_dx, 256);
  end;

  if ( (delta_y >= 0) and (delta_y <= 255) ) then
  begin
    b3 := delta_y;
    dec(self.s.mouse.delayed_dy, delta_y);
  end else
  if ( delta_y > 255 ) then
  begin
    b3 := $ff;
    dec(self.s.mouse.delayed_dy, 255);
  end else
  if ( delta_y >= -256 ) then
  begin
    b3 := delta_y;
    b1 := b1 or $20;
    dec(self.s.mouse.delayed_dy, delta_y);
  end else
  begin
    b3 := $00;
    b1 := b1 or $20;
    inc(self.s.mouse.delayed_dy, 256);
  end;
  mouse_enQ_packet(b1, b2, b3);
end;

procedure TKeyBoard.mouse_enabled_changed(enabled:bool);
begin
  if((self.s.mouse.delayed_dx <> 0) or (self.s.mouse.delayed_dy <> 0)) then
    create_mouse_packet(1);

  self.s.mouse.delayed_dx := 0;
  self.s.mouse.delayed_dy := 0;
end;

procedure TKeyBoard.mouse_motion(delta_x:Integer; delta_y:Integer;button_state:unsigned);
var
  force_enq: bool;
begin
  force_enq := 0;
  // If mouse events are disabled on the GUI headerbar, don't
  // generate any mouse data

  // don't generate interrupts if we are in remote mode.

  // Note: enable only applies in STREAM modE.
  if ( self.s.mouse.enable = 0 ) then
    exit;

  // scale down the motion
  if ( (delta_x < -1) or (delta_x > 1) ) then
    delta_x := delta_x div 2;
  if ( (delta_y < -1) or (delta_y > 1) ) then
    delta_y := delta_y div 2;

  if( (delta_x = 0) and (delta_y = 0) and
      (self.s.mouse.button_status = button_state  and $3) ) then
    exit;

  if(self.s.mouse.button_status <> (button_state  and $3)) then
    force_enq := 1;

  self.s.mouse.button_status := button_state  and $3;

  if(delta_x > 255 ) then delta_x := 255;
  if(delta_y > 255 ) then delta_y := 255;
  if(delta_x < -256) then delta_x := -256;
  if(delta_y < -256) then delta_y := -256;

  inc(self.s.mouse.delayed_dx,delta_x);
  inc(self.s.mouse.delayed_dy,delta_y);

  if((self.s.mouse.delayed_dx > 255 )or
     (self.s.mouse.delayed_dx < -256)or
     (self.s.mouse.delayed_dy > 255 )or
     (self.s.mouse.delayed_dy < -256)) then
    force_enq := 1;

  create_mouse_packet(force_enq);
end;

procedure TKeyBoard.put_scancode( code: PChar; count: Integer  );
var
  I: Integer;
begin
  I := 0;
  while i < count do
  begin
      kbd_enQ(Bit8u(code[i]));
      inc(I);
  end;
end;

end.
