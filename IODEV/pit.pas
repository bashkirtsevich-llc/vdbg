unit pit;

interface

uses Config, iodev;

const
   BX_PIT_LATCH_MODE_LSB   = 10;
   BX_PIT_LATCH_MODE_MSB   = 11;
   BX_PIT_LATCH_MODE_16BIT = 12;

type

bx_pit_t=record
  mode:Bit8u;
  latch_mode:Bit8u;
  input_latch_value:Bit16u;
  input_latch_toggle:Bool;
  output_latch_value:Bit16u;
  output_latch_toggle:Bool;
  output_latch_full:Bool;
  counter_max:Bit16u;
  counter:Bit16u;
  bcd_mode:Bool;
  active:Bool;
  GATE:Bool;     // GATE input  pin
  OUT_:Bool;      // OUT  output pin
end;

bx_pit_c = class
public
  s:record
    timer:array[0..3] of bx_pit_t;
    speaker_data_on:Bit8u;
    refresh_clock_div2:Bool;
    timer_handle:array[0..3] of integer;
  end;

  devices:pbx_devices_c;
  Constructor Create;
  destructor Destroy; overload;
  function init( d:pbx_devices_c):Integer;
  function periodic( usec_delta:Bit32u ):Bool;

  //function SaveState( class state_file *fd ):Integer;
  //function LoadState( class state_file *fd ):Integer;


  function read_handler(this_ptr:Pointer; address:Bit32u; io_len:unsigned):Bit32u;
  procedure write_handler(this_ptr:Pointer; address:Bit32u; dvalue:Bit32u; io_len:unsigned);
{$if BX_USE_PIT_SMF = 0}
  Bit32u   read( Bit32u   addr, unsigned int len );
  void write( Bit32u   addr, Bit32u   Value, unsigned int len );
{$ifend}



  procedure write_count_reg( value:Bit8u; timerid:unsigned );
  function read_counter( timerid:unsigned ):Bit8u;
  procedure latch( timerid:unsigned );
  procedure set_GATE(pit_id:unsigned; value:unsigned);
  procedure start(timerid:unsigned);
end;

var
  bx_pit:bx_pit_c;

implementation

uses Service, SysUtils;

constructor bx_pit_c.Create;
begin
end;

destructor bx_pit_c.Destroy;
begin
   inherited;
end;

function bx_pit_c.init( d:pbx_devices_c):Integer;
begin
  Self.devices := d;

  Self.devices^.register_irq(0, '8254 PIT');
  Self.devices^.register_io_read_handler(Self, read_handler, $0040, '8254 PIT');
  Self.devices^.register_io_read_handler(Self, read_handler, $0041, '8254 PIT');
  Self.devices^.register_io_read_handler(Self, read_handler, $0042, '8254 PIT');
  Self.devices^.register_io_read_handler(Self, read_handler, $0043, '8254 PIT');
  Self.devices^.register_io_read_handler(Self, read_handler, $0061, '8254 PIT');

  Self.devices^.register_io_write_handler(Self, write_handler, $0040, '8254 PIT');
  Self.devices^.register_io_write_handler(Self, write_handler, $0041, '8254 PIT');
  Self.devices^.register_io_write_handler(Self, write_handler, $0042, '8254 PIT');
  Self.devices^.register_io_write_handler(Self, write_handler, $0043, '8254 PIT');
  Self.devices^.register_io_write_handler(Self, write_handler, $0061, '8254 PIT');

  Self.s.speaker_data_on := 0;
  Self.s.refresh_clock_div2 := 0;

  Self.s.timer[0].mode        := 3;  (* periodic rate generator *)
  Self.s.timer[0].latch_mode  := BX_PIT_LATCH_MODE_16BIT;
  Self.s.timer[0].input_latch_value := 0;
  Self.s.timer[0].input_latch_toggle := 0;
  Self.s.timer[0].output_latch_value := 0;
  Self.s.timer[0].output_latch_toggle := 0;
  Self.s.timer[0].output_latch_full := 0;
  Self.s.timer[0].counter_max := 0;  (* $FFFF + 1 : (1193182 / 65535 := 18.2Hz) *)
  Self.s.timer[0].counter     := 0;  (* $FFFF + 1 : (1193182 / 65535 := 18.2Hz) *)
  Self.s.timer[0].bcd_mode    := 0;  (* binary counting mod_e *)
  Self.s.timer[0].GATE        := 1;  (* GATE tied to + logic *)
  Self.s.timer[0].OUT_         := 1;
  Self.s.timer[0].active      := 0;

  Self.s.timer[1].mode        := 3;  (* periodic rate generator *)
  Self.s.timer[1].latch_mode  := BX_PIT_LATCH_MODE_16BIT;
  Self.s.timer[1].input_latch_value := 0;
  Self.s.timer[1].input_latch_toggle := 0;
  Self.s.timer[1].output_latch_value := 0;
  Self.s.timer[1].output_latch_toggle := 0;
  Self.s.timer[1].output_latch_full := 0;
  Self.s.timer[1].counter_max := 0;  (* $FFFF + 1 : (1193182 / 65535 := 18.2Hz) *)
  Self.s.timer[1].counter     := 0;  (* $FFFF + 1 : (1193182 / 65535 := 18.2Hz) *)
  Self.s.timer[1].bcd_mode    := 0;  (* binary counting mod_e *)
  Self.s.timer[1].GATE        := 1;  (* GATE tied to + logic *)
  Self.s.timer[1].OUT_         := 1;
  Self.s.timer[1].active      := 0;

  Self.s.timer[2].mode        := 3;  (* periodic rate generator *)
  Self.s.timer[2].latch_mode  := BX_PIT_LATCH_MODE_16BIT;
  Self.s.timer[2].input_latch_value := 0;
  Self.s.timer[2].input_latch_toggle := 0;
  Self.s.timer[2].output_latch_value := 0;
  Self.s.timer[2].output_latch_toggle := 0;
  Self.s.timer[2].output_latch_full := 0;
  Self.s.timer[2].counter_max := 0;  (* $FFFF + 1 : (1193182 / 65535 := 18.2Hz) *)
  Self.s.timer[2].counter     := 0;  (* $FFFF + 1 : (1193182 / 65535 := 18.2Hz) *)
  Self.s.timer[2].bcd_mode    := 0;  (* binary counting mod_e *)
  Self.s.timer[2].GATE        := 0;  (* timer2 gate controlled by port 61h bit 0 *)
  Self.s.timer[2].OUT_         := 1;
  Self.s.timer[2].active      := 0;

  Result:=1;
end;

  // static IO port read callback handler
  // redirects to non-static class handler to aprocedure virtual functions

function bx_pit_c.read_handler(this_ptr:Pointer; address:Bit32u; io_len:unsigned):Bit32u;
begin
  if Boolean(io_len > 1) then
    LogPanic(Format('pit: io read from port %04x, len:=%u',[unsigned(address),
             unsigned(io_len)]));

  //if Boolean(bx_dbg.pit) then
    //BX_INFO(('pit: io read from port %04x', (unsigned) address));

  case (address) of
    $40: (* timer 0 - system ticks *)
      begin
         Result:=read_counter(0);
         Exit;
      end;

    $42: (* timer 2 read *)
      begin
         Result:=read_counter(2);
         Exit;
      end;

    $61:
      begin
      (* AT, port 61h *)
      Self.s.refresh_clock_div2 := Self.s.refresh_clock_div2;
      //Self.s.refresh_clock_div2 := not Self.s.refresh_clock_div2;
      Result:=( (Self.s.timer[2].OUT_ shl 5) or
              (Self.s.refresh_clock_div2 shl 4) or
              (Self.s.speaker_data_on shl 1) or
              (Self.s.timer[2].GATE) );
         Exit;
      end;

    else
      LogPanic(Format('pit: unsupported io read from port %04x',[address]));
    end;
  Result:=(0); (* keep compiler happy *)
end;


  // static IO port write callback handler
  // redirects to non-static class handler to aprocedure virtual functions

procedure bx_pit_c.write_handler(this_ptr:Pointer; address:Bit32u; dvalue:Bit32u; io_len:unsigned);
var
  command, mode, bcd_mode:Bit8u;
  value:Bit8u;
begin
{$if BX_USE_PIT_SMF=0}
  bx_pit_c *class_ptr := (bx_pit_c *) this_ptr;

  class_ptr^.write(address, dvalue, io_len);
end;

  procedure
bx_pit_c.write( Bit32u   address, Bit32u   dvalue,
                unsigned int io_len )
begin
{$else}
  //UNUSED(this_ptr);
{$ifend}  // !BX_USE_PIT_SMF

  value := Bit8u(dvalue);

  if Boolean(io_len > 1) then
    LogPanic(Format('pit: io write to port %04x, len:=%u',[unsigned(address),
             unsigned(io_len)]));

  (*if Boolean(bx_dbg.pit)
    BX_INFO(('pit: write to port %04x := %02x',
      (unsigned) address, (unsigned) value));*)

  case (address) of
    $40: (* timer 0: write count register *)
      begin
         write_count_reg( value, 0 );
      end;

    $41: (* timer 1: write count register *)
      begin
         write_count_reg( value, 1 );
      end;

    $42: (* timer 2: write count register *)
      begin
         write_count_reg( value, 2 );
      end;
    $43: (* timer 0-2 mod_e control *)
      (* |7 6 5 4|3 2 1|0|
       * |-------|-----|-|
       * |command|mod_e |bcd/binary|
       *)
       begin
         command  := value shr 4;
         mode     := (value shr 1)  and $07;
         bcd_mode := value  and $01;
{$if 0=1}
LogInfo(('timer 0-2 mod_e control: comm:%02x mod_e:%02x bcd_mod_e:%u',
  (unsigned) command, (unsigned) mod_e, (unsigned) bcd_mod_e));
{$ifend}

      if Boolean( (mode > 5) or (command > $0e) ) then
        LogPanic(Format('pit: outp(43h):=%02xh out of range',[unsigned(value)]));
      if Boolean(bcd_mode) then
        LogPanic(Format('pit: outp(43h):=%02xh: bcd mod_e unhandled',[
          unsigned(bcd_mode)]));

      if Command = 0 then
        (* timer 0: counter latch *)
         begin
          latch( 0 );
         end;
      if (Command=$1) or (Command=$2) then
        (* timer 0: LSB mod_e *)
        (* timer 0: MSB mod_e *)
         begin
          LogPanic(Format('pit: outp(43h): command %02xh unhandled',[
            unsigned(command)]));
         end;
      if Command = $3 then  (* timer 0: 16-bit mod_e *)
         begin
          Self.s.timer[0].mode := mode;
          Self.s.timer[0].latch_mode   := BX_PIT_LATCH_MODE_16BIT;
          Self.s.timer[0].input_latch_value := 0;
          Self.s.timer[0].input_latch_toggle := 0;
          Self.s.timer[0].bcd_mode    := bcd_mode;
          if Boolean( ((mode<>3) and (mode<>2) and (mode<>0)) or (bcd_mode<>0) ) then
            LogPanic(Format('pit: outp(43h): comm 3, mod_e %02x, bcd %02x unhandled',
              [unsigned(mode), bcd_mode]));
          end;
       if Command = $4 then (* timer 1: counter latch *)
         begin
          latch( 1 );
         end;

       if (Command = $5) or (Command=$6) then (* timer 1: LSB mod_e *)(* timer 1: MSB mod_e *)
          begin
            LogInfo(Format('pit: outp(43h): command %02xh unhandled (ignored)',
              [unsigned(command)]));
          end;
       if Command = $7 then (* timer 1: 16-bit mod_e *)
          begin
            Self.s.timer[1].mode := mode;
            Self.s.timer[1].latch_mode   := BX_PIT_LATCH_modE_16BIT;
            Self.s.timer[1].input_latch_value := 0;
            Self.s.timer[1].input_latch_toggle := 0;
            Self.s.timer[1].bcd_mode    := bcd_mode;
            if Boolean( ((mode<>3) and (mode<>2) and (mode<>0)) or (bcd_mode<>0)) then
              LogPanic(Format('pit: outp(43h): comm 7, mode %02x, bcd %02x unhandled',
                [unsigned(mode), bcd_mode]));
          end;
       if Command = $8 then (* timer 2: counter latch *)
          begin
            latch( 2 );
          end;

       if (Command = $9) or (Command = $a) then  (* timer 2: LSB mode *)(* timer 2: MSB mode *)
          begin
            LogPanic(Format('pit: outp(43h): command %02xh unhandled',[unsigned(command)]));
          end;
       if (Command = $b) then (* timer 2: 16-bit mode *)
          begin
            Self.s.timer[2].mode := mode;
            Self.s.timer[2].latch_mode   := BX_PIT_LATCH_modE_16BIT;
            Self.s.timer[2].input_latch_value := 0;
            Self.s.timer[2].input_latch_toggle := 0;
            Self.s.timer[2].bcd_mode    := bcd_mode;
            if Boolean( ((mode<>3) and (mode<>2) and (mode<>0)) or (bcd_mode<>0)) then
              LogPanic(Format('pit: outp(43h): comm Bh, mode %02x, bcd %02x unhandled',
                [unsigned(mode), bcd_mode]));
          end;
{$if 0=1}
        case $d: (* general counter latch *)
          if Boolean(value  and $08) (* select counter 2 *)
            latch( 2 );
          if Boolean(value  and $04) (* select counter 1 *)
            latch( 1 );
          if Boolean(value  and $02) (* select counter 0 *)
            latch( 0 );
          break;

        case $e: (* latch status of timers *)
          LogPanic(('pit: outp(43h): command %02xh unhandled',
            (unsigned) command);
          break;
{$ifend}
        if (Command = $c) or (Command = $d) or (Command = $e) or (Command = $f) then
          begin
            LogInfo(Format('pit: ignoring 8254 command %u',[unsigned(command)]));
          end;

        if (Command > $f) then (* $c  and $f *)
          begin
            LogPanic(Format('pit: outp(43h) command %1xh unhandled',
             [unsigned( command)]));
          end;
        end;

    $61:
      begin
        Self.s.speaker_data_on := (value shr 1)  and $01;
  (*??? only on AT+ *)
        set_GATE(2, value  and $01);
  {$if BX_CPU_LEVEL < 2}
        (* ??? XT: *)
        bx_kbd_port61h_write(value);
  {$ifend}
      end;

    else
      LogPanic(Format('pit: unsupported io write to port %04x := %02x',
       [unsigned( address),unsigned( value)]));
    end;
end;

procedure bx_pit_c.write_count_reg( value:Bit8u; timerid:unsigned );
var
  xfer_complete:Bool;
begin

  case ( Self.s.timer[timerid].latch_mode ) of
    BX_PIT_LATCH_modE_16BIT: (* write1:=LSB, write2:=MSB *)
      begin
        if Boolean(Self.s.timer[timerid].input_latch_toggle=0) then begin
          Self.s.timer[timerid].input_latch_value := value;
          Self.s.timer[timerid].input_latch_toggle := 1;
          xfer_complete := 0;
          if Boolean(BX_PIT_DEBUG) then
            LogInfo(Format('pit: Self.s.timer[timerid] write L := %02x',[unsigned( value)]));
          end
        else begin
          Self.s.timer[timerid].input_latch_value :=
            Self.s.timer[timerid].input_latch_value or (value shl 8);
          Self.s.timer[timerid].input_latch_toggle := 0;
          xfer_complete := 1;
          if Boolean(BX_PIT_DEBUG) then
            LogInfo(Format('pit: Self.s.timer[timerid] write H := %02x',[unsigned( value)]));
          end;
      end;

    BX_PIT_LATCH_modE_MSB: (* write1:=MSB, LSB:=0 *)
      begin
        Self.s.timer[timerid].input_latch_value := (value shl 8);
        xfer_complete := 1;
        if Boolean(BX_PIT_DEBUG) then
          LogInfo(Format('pit: Self.s.timer[timerid] write H := %02x',[unsigned( value)]));
      end;

    BX_PIT_LATCH_modE_LSB: (* write1:=LSB, MSB:=0 *)
      begin
        Self.s.timer[timerid].input_latch_value := value;
        xfer_complete := 1;
        if Boolean(BX_PIT_DEBUG) then
          LogInfo(Format('pit: Self.s.timer[timerid] write L := %02x',[unsigned( value)]));
      end;

    else
      begin
        LogPanic(('write_count_reg: latch_mode unknown'));
        xfer_complete := 0;
      end;
    end;

  if Boolean(xfer_complete) then begin
    Self.s.timer[timerid].counter_max := Self.s.timer[timerid].input_latch_value;

    // reprogramming counter clears latch
    Self.s.timer[timerid].output_latch_full := 0;

    // counter bounds
    // mode      minimum    maximum
    //  0           1          0
    //  1           1          0
    //  2           2          0
    //  3           2          0
    //  4           1          0
    //  5           1          0
    case (Self.s.timer[timerid].mode) of
      0:
        begin
          Self.s.timer[timerid].counter := Self.s.timer[timerid].counter_max;
          Self.s.timer[timerid].active := 1;
          if Boolean(Self.s.timer[timerid].GATE) then begin
            Self.s.timer[timerid].OUT_ := 0; // OUT pin starts low
            start( timerid );
            end;
        end;
      1:
        begin
          LogPanic(Format('pit:write_count_reg(%u): mode1 unsupported',[timerid]));
        end;
      2:
        begin
          if Boolean( Self.s.timer[timerid].counter_max = 1 ) then
            LogPanic(Format('pit:write_count_reg(%u): mode %u counter_max:=1',
                     [timerid,unsigned( Self.s.timer[timerid].mode)]));
          if Boolean( Bool(Self.s.timer[timerid].GATE) and (not Self.s.timer[timerid].active) ) then begin
            // software triggered
            Self.s.timer[timerid].counter := Self.s.timer[timerid].counter_max;
            Self.s.timer[timerid].active  := 1;
            Self.s.timer[timerid].OUT_     := 1; // initially set high
            start( timerid );
            end;
        end;
      3:
        begin
          if Boolean( Self.s.timer[timerid].counter_max = 1 ) then
            LogPanic(Format('pit:write_count_reg(%u): mode %u counter_max:=1',
                     [timerid,unsigned( Self.s.timer[timerid].mode)]));
          Self.s.timer[timerid].counter_max := Self.s.timer[timerid].counter_max  and $fffe;
          if Boolean( Bool(Self.s.timer[timerid].GATE) and (not Self.s.timer[timerid].active) ) then begin
            // software triggered
            Self.s.timer[timerid].counter := Self.s.timer[timerid].counter_max;
            Self.s.timer[timerid].active  := 1;
            Self.s.timer[timerid].OUT_     := 1; // initially set high
            start( timerid );
            end;
        end;
      4:
        begin
          LogPanic(Format('pit:write_count_reg(%u): mode4 unsupported',[timerid]));
        end;
      5:
        begin
          LogPanic(Format('pit:write_count_reg(%u): mode5 unsupported',[timerid]));
        end;
      end;
    end;
end;

function bx_pit_c.read_counter( timerid:unsigned ):Bit8u;
var
  counter_value:Bit16u;
  retval:Bit8u;
begin

  if Boolean(Self.s.timer[timerid].output_latch_full) then begin (* latched read *)
    counter_value := Self.s.timer[timerid].output_latch_value;
    end
  else begin (* direct unlatched read *)
    counter_value := Self.s.timer[timerid].counter;
  LogInfo(Format('CV:=%04x',[unsigned( Self.s.timer[timerid].counter)]));
    end;

  case (Self.s.timer[timerid].latch_mode) of
    BX_PIT_LATCH_modE_LSB:
      begin
        retval := Bit8u(counter_value);
        Self.s.timer[timerid].output_latch_full := 0;
      end;
    BX_PIT_LATCH_modE_MSB:
      begin
        retval := Bit8u ( counter_value shr 8 );
        Self.s.timer[timerid].output_latch_full := 0;
      end;
    BX_PIT_LATCH_modE_16BIT:
      begin
        if Boolean(Self.s.timer[timerid].output_latch_toggle=0) then begin (* LSB 1st *)
          retval := Bit8u(counter_value);
          end
        else begin (* MSB 2nd *)
          retval := Bit8u(counter_value shr 8 );
          end;
        Self.s.timer[timerid].output_latch_toggle := Word(not Boolean(Self.s.timer[timerid].output_latch_toggle));
        //Self.s.timer[timerid].output_latch_toggle:= not Self.s.timer[timerid].output_latch_toggle;
        if Boolean(Self.s.timer[timerid].output_latch_toggle = 0) then
          Self.s.timer[timerid].output_latch_full := 0;
      end;
    else
      begin
        LogPanic(('pit: io read from port 40h: unknown latch mode'));
        retval := 0; (* keep compiler happy *)
      end;
    end;
  Result:=retval;
end;

procedure bx_pit_c.latch( timerid:unsigned );
begin
  (* subsequent counter latch commands are ignored until value read out *)
  if Boolean(Self.s.timer[timerid].output_latch_full) then begin
    LogInfo(Format('pit: pit(%u) latch: output latch full, ignoring',[timerid]));
    exit;
    end;

  Self.s.timer[timerid].output_latch_value := Self.s.timer[timerid].counter;

  if Boolean(BX_PIT_DEBUG) then
    begin
      LogInfo(Format('pit: latch_value := %d',[Self.s.timer[timerid].output_latch_value]));
      Self.s.timer[timerid].output_latch_toggle := 0;
      Self.s.timer[timerid].output_latch_full   := 1;
    end;
end;

procedure bx_pit_c.set_GATE(pit_id:unsigned; value:unsigned);
begin
  // GATE's for Timer 0  and Timer 1 are tied high.
  if Boolean(pit_id <> 2) then
    LogPanic(('pit:set_GATE: pit_id <> 2'));

  value := Bool(value > 0);

  (* if no transition of GATE input line, then nothing to do *)
  if Boolean(value = Self.s.timer[2].GATE) then
    exit;

  if Boolean(value) then begin (* PIT2: GATE transition from 0 to 1 *)
    Self.s.timer[2].GATE  := 1;
    case( Self.s.timer[2].mode ) of
      0:
        begin
          Self.s.timer[2].counter := Self.s.timer[2].counter_max;
          if Boolean(Self.s.timer[2].active) then begin
            Self.s.timer[2].OUT_ := 0;
            end;
          start( 2 );
        end;
      2:
        begin
          // begin counting, reload counter
          Self.s.timer[2].active := 1;
          Self.s.timer[2].OUT_ := 1;
          Self.s.timer[2].counter := Self.s.timer[2].counter_max;
          start( 2 );
        end;
      3:
        begin
          // begin counting, reload counter
          Self.s.timer[2].active := 1;
          Self.s.timer[2].OUT_ := 1;
          Self.s.timer[2].counter := Self.s.timer[2].counter_max;
          start( 2 );
        end;
      1,
      4,
      5:    // default:
        begin
          LogPanic(Format('bx_pit_c.set_GATE: unhandled timer2 mode %u', [unsigned( Self.s.timer[2].mode)]));
        end;
      end;
    end
  else begin       // PIT2: GATE transition from 1 to 0, deactivate
    Self.s.timer[2].GATE  := 0;
    case ( Self.s.timer[2].mode ) of
      0:
        exit;
      2:
        begin
          // 1) stops count, 2) OUT goes immediately high
          Self.s.timer[2].active := 0;
          Self.s.timer[2].OUT_ := 1;
        end;
      3:
        begin
          // 1) stops count, 2) OUT goes immediately high
          Self.s.timer[2].active := 0;
          Self.s.timer[2].OUT_ := 1;
        end;
      1,
      4,
      5: //default
        LogPanic(Format('bx_pit_c.set_GATE: unhandled timer2 mode %u',[unsigned( Self.s.timer[2].mode)]));
      end;
    end;
end;

procedure bx_pit_c.start(timerid:unsigned);
var
  period_hz:LongWord;
begin

  if Boolean(Self.s.timer[timerid].counter_max = $0000) then begin
    period_hz   := 1193182 div 65536;
    end
  else begin
    period_hz := 1193182 div Self.s.timer[timerid].counter_max;
    end;
  LogInfo(Format('timer %d period set to %d hz',[timerid, period_hz]));

  if (Self.s.timer[timerid].mode<>0) and (Self.s.timer[timerid].mode<>2)
    and (Self.s.timer[timerid].mode<>3) then LogPanic('PANIC IN pit.start');
end;

{$if 0=1}
  procedure
bx_kbd_port61h_write(Bit8u   value)
begin
//  PcError('KBD_PORT61H_WRITE(): not implemented yet');
  UNUSED( value );
end;
{$ifend}


function bx_pit_c.periodic( usec_delta:Bit32u ):Bool;
var
  prev_timer0_out:Bool;
  I:unsigned;
begin

  prev_timer0_out := Self.s.timer[0].OUT_;

  for i := 0 to 3 do
    begin
      // is timer enabled and active?
      if Boolean( Bool(Self.s.timer[i].GATE) and Bool(Self.s.timer[i].active) ) then begin
        case ( Self.s.timer[i].mode ) of
          0: // mode 0: Single Timeout
            begin
              // wraps after count expires
              if Boolean( Self.s.timer[i].counter = 0 ) then begin
                // counter previously expired, wrap counter
                Self.s.timer[i].counter := $ffff;
                end
              else if Boolean( usec_delta >= Self.s.timer[i].counter ) then begin
                // counter expired
                Self.s.timer[i].counter := 0;
                Self.s.timer[i].OUT_     := 1;
                end
              else begin
                // decrement counter by elapsed useconds
                Self.s.timer[i].counter :=
                  Self.s.timer[i].counter - Bit16u(usec_delta);
                end;
            end;

          1: // mode 1: Retriggerable One-Shot
            begin
              // wraps after count expires
              LogPanic(Format('bx_pit_c.periodic: bad mode: timer[%u], mode %u',
                            [i,unsigned( Self.s.timer[i].mode)]));
            end;

          2: // mode 2: Rate Generator
            begin
              // reloads after count expires
              // OUT is low when counter:=1, high otherwise
              // min count:=2, max count:=0
              if Boolean( Self.s.timer[i].counter = 0 ) then begin
                // max counter val, just wrap
                Self.s.timer[i].counter := $ffff;
                Self.s.timer[i].OUT_     := 1;
                end
              else if Boolean( Self.s.timer[i].counter = 1 ) then begin
                // counter previously expired, reload
                Self.s.timer[i].counter := Self.s.timer[i].counter_max;
                Self.s.timer[i].OUT_     := 1;
                end
              else if Boolean( (Self.s.timer[i].counter = 2) or
                        (usec_delta >= (Bit32u(Self.s.timer[i].counter) - 1)) ) then begin
                // in either case, counter will reach 1
                Self.s.timer[i].counter := 1;
                Self.s.timer[i].OUT_ := 0;
                end
              else begin
                // decrement counter by elapsed useconds
                Self.s.timer[i].counter :=
                  Self.s.timer[i].counter - Bit16u(usec_delta);
                end;
            end;

          3: // mode 3: Square Wave mode
            begin
              // reloads after count expires
              // min count:=2, max count:=0
              if Boolean( Self.s.timer[i].counter = 0 ) then begin
                // max count, dec by 2
                Self.s.timer[i].counter := $fffe;
                end
              else if Boolean( (Self.s.timer[i].counter <= 2) or
                        ( (usec_delta*2) >= Self.s.timer[i].counter ) ) then begin
                // counter expired, reload
                Self.s.timer[i].counter := Self.s.timer[i].counter_max;
                Self.s.timer[i].OUT_     := Word(not Boolean(Self.s.timer[i].OUT_));
                //Self.s.timer[i].OUT_     := not Self.s.timer[i].OUT_;
                //BX_INFO(('CV: reload t%u to %04x',[unsigned( i,[unsigned(
                //  Self.s.timer[i].counter));
                end
              else begin
                // decrement counter by elapsed useconds
                Self.s.timer[i].counter := Self.s.timer[i].counter - Bit16u( 2*usec_delta );
                //BX_INFO(('CV: dec count to %04x',
                //         [unsigned( Self.s.timer[i].counter));
                end;
            end;

          4: // mode 4: Software Triggered Strobe
            begin
            // wraps after count expires
            LogPanic(Format('bx_pit_c.periodic: bad mode: timer[%u], mode %u',
                          [i,unsigned( Self.s.timer[i].mode)]));
            end;

          5:
            begin// mode 5: Hardware Retriggerable Strobe
              // wraps after count expires
              LogPanic(Format('bx_pit_c.periodic: bad mode: timer[%u], mode %u',
                          [i,unsigned( Self.s.timer[i].mode)]));
            end;
          else
            begin
              LogPanic(Format('bx_pit_c.periodic: bad mode: timer[%u], mode %u',
                            [i,unsigned( Self.s.timer[i].mode)]));
            end;
          end; // switch ( Self.s.tim...
        end; // if Boolean( Self.s.timer[i]...
      end; // for (unsigned i...

  // see if there's a rising edge on timer0's output to trigger an IRQ0.
  if Boolean( (prev_timer0_out=0) and (Self.s.timer[0].OUT_=1) ) then
    Result:=1 // request IRQ 0
  else
    Result:=0;
end;
end.

