/////////////////////////////////////////////////////////////////////////
// $Id: pit_wrap.cc,v 1.17 2002/02/08 22:27:51 yakovlev Exp $
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
//  Self. library is free software; you can redistribute it and/or
//  mod_ify it under the terms of the GNU Lesser General Public
//  License as published by the Free Software Foundation; either
//  version 2 of the License, or (at your option) any later version.
//
//  Self. library is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  Lesser General Public License for more details.
//
//  You should have received a copy of the GNU Lesser General Public
//  License along with Self. library; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

unit pit_wrap;

interface

uses Config, iodev, pit82c54;

const DEBUG_REALTIME_WITH_PRINTF = 0;

const  TIME_DIVIDER = 1;
const  TIME_MULTIPLIER = 1;
const  TIME_HEADSTART = 1;
//USEC_ALPHA is multiplier for the past.
//USEC_ALPHA_B is 1-USEC_ALPHA, or multiplier for the present.
const  USEC_ALPHA = 0.8;
const  USEC_ALPHA_B = 1-USEC_ALPHA;
const  MIN_MULT = 0.9;
const  MIN_MULT_FLOOR = 0.75;
const  MAX_MULT = 1.25;
const  MAX_MULT_CEILING = 1.5;

//How many timer ticks per usecond.
//1.193181MHz Clock
//1193/1000 Ticks Per usecond.
const  TICKS_PER_SECOND = 1193181;
const  USEC_PER_SECOND  = 1000000;
const  TIME_MULT        = 1.193;

const
   BX_PIT_LATCH_MODE_LSB   = 10;
   BX_PIT_LATCH_MODE_MSB   = 11;
   BX_PIT_LATCH_MODE_16BIT = 12;

type
  bx_pit_t = packed record
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

  pbx_pit_c=^bx_pit_c;
  bx_pit_c = class
  public
    s: packed record
      timer:pit_82C54;
      speaker_data_on:Bit8u;
      refresh_clock_div2:Bool;
      timer_handle:array[0..3] of integer;
      last_usec:Bit64u;
      last_next_event_time:Bit32u;
      total_ticks:Bit64u;
      total_usec:Bit64u;
    end;

  devices:pbx_devices_c;
  Constructor Create;
  destructor Destroy; overload;
  function init( d:pbx_devices_c):Integer;
  function periodic( usec_delta:Bit32u ):Bool;

  //function SaveState( class state_file *fd ):Integer;
  //function LoadState( class state_file *fd ):Integer;


  function read_handler(_ptr:Pointer; address:Bit32u; io_len:unsigned):Bit32u;
  procedure write_handler(_ptr:Pointer; address:Bit32u; dvalue:Bit32u; io_len:unsigned);
{$if BX_USE_PIT_SMF = 0}
  Bit32u   read( Bit32u   addr, unsigned int len );
  void write( Bit32u   addr, Bit32u   Value, unsigned int len );
{$ifend}



//  procedure write_count_reg( value:Bit8u; timerid:unsigned );
//  function read_counter( timerid:unsigned ):Bit8u;
//  procedure latch( timerid:unsigned );
//  procedure set_GATE(pit_id:unsigned; value:unsigned);
//  procedure start(timerid:unsigned);

  procedure timer_handler(this_ptr:Pointer);
  procedure handle_timer;
end;

//function TICKS_TO_USEC(a:Bit64u):Bit64u;
//function USEC_TO_TICKS(a:Bit64u):Bit64u;

var
  bx_pit:bx_pit_c;
implementation

uses Service, cpu, Math, SysUtils, Pic;

//function TICKS_TO_USEC(a:Bit64u):Bit64u;
//begin
//  Result := Trunc((a*USEC_PER_SECOND)/TICKS_PER_SECOND);
//end;
//
//function USEC_TO_TICKS(a:Bit64u):Bit64u;
//begin
//  Result := Trunc((a*TICKS_PER_SECOND)/USEC_PER_SECOND);
//end;
(*
#if BX_USE_REALTIME_PIT
#  define TICKS_TO_USEC(a) ( ((a)*Self.s.usec_per_second)/Self.s.ticks_per_second )
#  define USEC_TO_TICKS(a) ( ((a)*Self.s.ticks_per_second)/Self.s.usec_per_second )
{$else}*)
//#  define TICKS_TO_USEC(a) ( ((a)*USEC_PER_SECOND)/TICKS_PER_SECOND )
//#  define USEC_TO_TICKS(a) ( ((a)*TICKS_PER_SECOND)/USEC_PER_SECOND )
(*{$ifend}*)

constructor bx_pit_c.Create;
begin
  s.speaker_data_on:=0;

  (* 8254 PIT (Programmable Interval Timer) *)

  Self.s.timer_handle[1] := BX_NULL_TIMER_HANDLE;
  Self.s.timer_handle[2] := BX_NULL_TIMER_HANDLE;
  Self.s.timer_handle[0] := BX_NULL_TIMER_HANDLE;
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

//  BX_DEBUG(('pit: starting init'));

  Self.s.speaker_data_on := 0;
  Self.s.refresh_clock_div2 := 0;

  Self.s.timer:=bx_pit_82C54;
  //Self.s.timer.init;

  Self.s.timer_handle[0] := bx_pc_system.register_timer(@bx_pit, timer_handler, unsigned(100) , 1, 1);
//  BX_DEBUG(('pit: RESETting timer.'));
  bx_pc_system.deactivate_timer(Self.s.timer_handle[0]);
//  BX_DEBUG(('deactivated timer.'));
  if Boolean(Self.s.timer.get_next_event_time()) then begin
    bx_pc_system.activate_timer(Self.s.timer_handle[0],
				MAX(1,(Self.s.timer.get_next_event_time()*USEC_PER_SECOND) div TICKS_PER_SECOND),
				0);
//    BX_DEBUG(('activated timer.'));
  end;
  Self.s.last_next_event_time := Self.s.timer.get_next_event_time();
  Self.s.last_usec:=bx_pc_system.time_usec();

  Self.s.total_ticks:=0;

{$if BX_USE_REALTIME_PIT=1}
  Self.s.usec_per_second:=USEC_PER_SECOND;
  Self.s.ticks_per_second:=TICKS_PER_SECOND;
  Self.s.total_sec:=0;
  Self.s.last_time:=(time(NULL)*TIME_MULTIPLIER/TIME_DIVIDER)+TIME_HEADSTART;
{$else}
  Self.s.total_usec:=0;
{$ifend}

//  BX_DEBUG(('pit: finished init'));
//
//  BX_DEBUG(Format('s.last_usec:=%d',[Self.s.last_usec]));
//  BX_DEBUG(Format('s.timer_id:=%d',[Self.s.timer_handle[0]]));
//  BX_DEBUG(Format('s.timer.get_next_event_time:=%d',[Self.s.timer.get_next_event_time()]));
//  BX_DEBUG(Format('s.last_next_event_time:=%d',[Self.s.last_next_event_time]));

  Result:=1;
end;


procedure bx_pit_c.timer_handler(this_ptr:Pointer);
var
   class_ptr: pbx_pit_c;
begin
  class_ptr:= pbx_pit_c(this_ptr);

  class_ptr^.handle_timer();
end;

procedure bx_pit_c.handle_timer;
var
  time_passed :Bit64u;
  time_passed32:Bit32u;
begin
  time_passed   := bx_pc_system.time_usec - Self.s.last_usec;
  time_passed32 := time_passed;

//  BX_DEBUG(('pit: entering timer handler'));

  if time_passed32 <> 0 then
    periodic(time_passed32);

  Self.s.last_usec:=Self.s.last_usec + time_passed;
  if Boolean((time_passed <> 0) or (Self.s.last_next_event_time <>
              Self.s.timer.get_next_event_time())) then
  begin
//    BX_DEBUG(('pit: RESETting timer.'));
    bx_pc_system.deactivate_timer(Self.s.timer_handle[0]);
//    BX_DEBUG(('deactivated timer.'));
    if Self.s.timer.get_next_event_time() <> 0 then
    begin
      bx_pc_system.activate_timer(Self.s.timer_handle[0], //!!!
				  MAX(1,(Self.s.timer.get_next_event_time()*USEC_PER_SECOND) div TICKS_PER_SECOND),
				  0);
//      BX_DEBUG(('activated timer.'));
    end;
    Self.s.last_next_event_time := Self.s.timer.get_next_event_time();
  end;
//  BX_DEBUG(Format('s.last_usec:=%d',[Self.s.last_usec]));
//  BX_DEBUG(Format('s.timer_id:=%d',[Self.s.timer_handle[0]]));
//  BX_DEBUG(Format('s.timer.get_next_event_time:=%x',[Self.s.timer.get_next_event_time()]));
//  BX_DEBUG(Format('s.last_next_event_time:=%d',[Self.s.last_next_event_time]));
end;


  // static IO port read callback handler
  // redirects to non-static class handler to aprocedure virtual functions

function bx_pit_c.read_handler(_ptr:Pointer; address:Bit32u; io_len:unsigned):Bit32u;
begin
//  BX_DEBUG(('pit: entering read handler'));

  handle_timer();

  if Boolean(io_len > 1) then
    LogPanic(Format('pit: io read from port %04x, len:=%u',[address,
             io_len]));

  if Boolean(BX_PIT_DEBUG) then
    LogInfo(Format('pit: io read from port %04x',[address]));

  case(address) of

    $40: (* timer 0 - system ticks *)
      begin
      Result:=(Self.s.timer.read(0));
      Exit;
      end;
    $41: (* timer 1 read *)
    begin
      Result:=(Self.s.timer.read(1));
      Exit;
      end;
    $42: (* timer 2 read *)
    begin
      Result:=(Self.s.timer.read(2));
      Exit;
      end;
    $43: (* timer 1 read *)
    begin
      Result:=(Self.s.timer.read(3));
      Exit;
      end;

    $61:
    begin
      (* AT, port 61h *)
      //Self.s.refresh_clock_div2 := not Self.s.refresh_clock_div2;
      Self.s.refresh_clock_div2     := Word(Self.s.refresh_clock_div2=0);
      Result:=( (Self.s.timer.read_OUT(2) shl 5) or
              (Self.s.refresh_clock_div2 shl 4) or
              (Self.s.speaker_data_on shl 1) or
              (IfThen(Self.s.timer.read_GATE(2)<>0,1,0)) );
      Exit;
      end;

    else
      LogPanic(Format('pit: unsupported io read from port %04x',[address]));
  end;
  Result:=(0); (* keep compiler happy *)
end;


  // static IO port write callback handler
  // redirects to non-static class handler to aprocedure virtual functions

procedure bx_pit_c.write_handler(_ptr:Pointer; address:Bit32u; dvalue:Bit32u; io_len:unsigned);
var
  value:Bit8u;
  time_passed:Bit64u;
  time_passed32:Bit32u;
begin
{$if BX_USE_PIT_SMF=0}
  bx_pit_c *class_ptr := (bx_pit_c *) Self._ptr;

  class_ptr^.write(address, dvalue, io_len);
end;

  procedure
bx_pit_c.write( Bit32u   address, Bit32u   dvalue,
                unsigned int io_len )
begin
{$else}
  //UNUSED(Self._ptr);
{$ifend}  // !BX_USE_PIT_SMF

  time_passed:=bx_pc_system.time_usec()-Self.s.last_usec;
  time_passed32 := time_passed;
//  BX_DEBUG(('pit: entering write handler'));

  if time_passed32<>0 then begin
    periodic(time_passed32);
  end;
  Self.s.last_usec:=Self.s.last_usec + time_passed;

  value := Bit8u(dvalue);

  if Boolean(io_len > 1) then
    LogPanic(Format('pit: io write to port %04x, len:=%u',[address,io_len]));

  if Boolean(BX_PIT_DEBUG) then
    LogInfo(Format('pit: write to port %04x := %02x',[address,value]));

  case (address) of
    $40: (* timer 0: write count register *)
      begin
        Self.s.timer.write(0,value);
      end;

    $41: (* timer 1: write count register *)
      begin
      Self.s.timer.write( 1,value );
      end;

    $42: (* timer 2: write count register *)
      begin
      Self.s.timer.write( 2,value );
      end;

    $43: (* timer 0-2 mod_e control *)
      begin
      Self.s.timer.write( 3,value );
      end;

    $61:
      begin
      Self.s.speaker_data_on := (value shr 1)  and $01;
(*??? only on AT+ *)
      Self.s.timer.set_GATE(2, value  and $01);
{$if BX_CPU_LEVEL < 2}
      (* ??? XT: *)
      bx_kbd_port61h_write(value);
{$ifend}
      end;

    else
      LogPanic(Format('pit: unsupported io write to port %04x := %02x',[address,value]));
  end;

  if Boolean((Self.s.timer.read_OUT(0))=1) then begin
    bx_pic.raise_irq(0);
  end else begin
    bx_pic.lower_irq(0);
  end;

  if Boolean((time_passed<>0) or
     (Self.s.last_next_event_time <> Self.s.timer.get_next_event_time())) then begin
//    BX_DEBUG(('pit: RESETting timer.'));
    bx_pc_system.deactivate_timer(Self.s.timer_handle[0]);
//    BX_DEBUG(('deactivated timer.'));
    if Boolean(Self.s.timer.get_next_event_time()) then begin
      bx_pc_system.activate_timer(Self.s.timer_handle[0],
				  MAX(1,(Self.s.timer.get_next_event_time()*USEC_PER_SECOND) div TICKS_PER_SECOND),
				  0);
//      BX_DEBUG(('activated timer.'));
    end;
    Self.s.last_next_event_time := Self.s.timer.get_next_event_time();
  end;
//  BX_DEBUG(Format('s.last_usec:=%d',[Self.s.last_usec]));
//  BX_DEBUG(Format('s.timer_id:=%d',[Self.s.timer_handle[0]]));
Self.s.timer.get_next_event_time();
//  BX_DEBUG(Format('s.timer.get_next_event_time:=%x',[Self.s.timer.get_next_event_time()]));
//  BX_DEBUG(Format('s.last_next_event_time:=%d',[Self.s.last_next_event_time]));

end;

function bx_pit_c.periodic( usec_delta:Bit32u ):Bool;
var
  prev_timer0_out:Bool;
  want_interrupt:Bool;
  ticks_delta:Bit32u;
  maxchange:Bit32u;
  timedelta:Bit32u;
begin
  prev_timer0_out := Self.s.timer.read_OUT(0);
  want_interrupt := 0;
  ticks_delta := 0;
{$if BX_USE_REALTIME_PIT=1}
  if Boolean(Self.s.total_ticks < (MAX_MULT_CEILING*TICKS_PER_SECOND + (TICKS_PER_SECOND * Self.s.total_sec))) then begin
    ticks_delta:=(Bit32u)(USEC_TO_TICKS(usec_delta));
    Self.s.total_ticks +:= ticks_delta;
  end;
  second_update_data();
{$else}
  Self.s.total_usec := Self.s.total_usec + usec_delta;
  //  ticks_delta=(Bit32u)((USEC_TO_TICKS((Bit64u)(BX_PIT_THIS s.total_usec)))-BX_PIT_THIS s.total_ticks);

  ticks_delta:=((Self.s.total_usec*TICKS_PER_SECOND) div USEC_PER_SECOND)-Self.s.total_ticks;
  Self.s.total_ticks := Self.s.total_ticks + ticks_delta;

  while ((Self.s.total_ticks >= 1193181) and (Self.s.total_usec >= 1000000)) do
  begin
    Self.s.total_ticks := Self.s.total_ticks - 1193181;
    Self.s.total_usec  := Self.s.total_usec - 1000000;
  end;
{$ifend}

  while(ticks_delta>0) do
  begin
    maxchange:=Self.s.timer.get_next_event_time();
    timedelta:=maxchange;
    if Boolean((maxchange=0) or (maxchange>ticks_delta)) then begin
      timedelta:=ticks_delta;
    end;
    Self.s.timer.clock_all(timedelta);
    if Boolean( (prev_timer0_out=0) ) then begin
      if Boolean((Self.s.timer.read_OUT(0))=1) then begin
        bx_pic.raise_irq(0);
        prev_timer0_out:=1;
      end;
    end else begin
      if Boolean((Self.s.timer.read_OUT(0))=0) then begin
        bx_pic.lower_irq(0);
        prev_timer0_out:=0;
      end;
    end;
    prev_timer0_out:=Self.s.timer.read_OUT(0);
    ticks_delta:=ticks_delta-timedelta;
  end;

  Result:=(want_interrupt);
end;

end.

