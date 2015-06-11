{ ****************************************************************************** }
{ Mozaa 0.95 - Virtual PC emulator - developed by Massimiliano Boidi 2003 - 2004 }
{ ****************************************************************************** }

{ For any question write to maxboidi@libero.it }

(* **** *)

unit pc_system;
interface

const
 BX_MAX_TIMERS=16;
 BX_NULL_TIMER_HANDLE = 10000; // set uninitialized timer handles to this

type
  PCS_OP = (PCS_CLEAR, PCS_SET, PCS_TOGGLE );
  bx_timer_handler_t = procedure(this_ptr:Pointer) of object;
  //typedef void (*bx_timer_handler_t)(void *);

bx_pc_system_c = class
public

  timer:array [0..BX_MAX_TIMERS] of record
    period:Bit64u;
    remaining:Bit64u;
    active:Bool;
    continuous:Bool;
    triggered:Bool;
    funct:bx_timer_handler_t;
    this_ptr:Pointer;
    end;
  num_timers:unsigned;
  num_cpu_ticks_in_period:Bit64u;
  num_cpu_ticks_left:Bit64u;

  DRQ:array[0..8] of Bool;  // DMA Request
  DACK:array[0..8] of Bool; // DMA Acknowlege
  TC:bool;      // Terminal Count
  HRQ:bool;     // Hold Request
  HLDA:bool;    // Hold Acknowlege
  //Boolean INTR;    // Interrupt


    // Address line 20 control:
    //   1 = enabled: extended memory is accessible
    //   0 = disabled: A20 address line is forced low to simulate
    //       an 8088 address map
  enable_a20:Bool;

    // start out masking physical memory addresses to:
    //   8086:      20 bits
    //    286:      24 bits
    //    386:      32 bits
    // when A20 line is disabled, mask physical memory addresses to:
    //    286:      20 bits
    //    386:      20 bits
    //
  a20_mask:Bit32u;
  COUNTER_INTERVAL:Bit64u;
  counter:Bit64u;
  counter_timer_index:integer;

  constructor Create;
  procedure set_DRQ(channel:unsigned; val:bool);
  procedure set_DACK(channel:unsigned; val:bool);
  procedure set_TC(val:bool);   // set the Terminal Count line
  procedure set_HRQ(val:bool);  // set the Hold ReQuest line
  procedure raise_HLDA; // raise the HoLD Acknowlege line
  procedure set_INTR(value:bool); // set the INTR line to value

  function IntEnabled:Integer;
  function InterruptSignal( operation:PCS_OP ):Integer;
  function ResetSignal( operation:PCS_OP ):Integer;
  function IAC:Bit8u;

  procedure bx_pc_system_c;
  procedure   init_ips(ips:Bit32u);
  procedure   timer_handler;
  function ticks_remaining(index:Integer):Int64;
  function    register_timer( this_ptr:Pointer; funct:bx_timer_handler_t; useconds:Bit32u; continuous:Bool;active:Bool):Integer;
  procedure start_timers;
  procedure activate_timer( timer_index:unsigned; useconds:Bit32u; continuous:Bool );
  procedure deactivate_timer( timer_index:unsigned );
  procedure tick1;
  procedure tickn(n:Bit64u);

  function register_timer_ticks(this_ptr:Pointer; funct:bx_timer_handler_t; Instructions:Bit64u; continuous:Bool; active:Bool):Integer;
  procedure activate_timer_ticks(timer_index:unsigned; instructions:Bit64u; continuous:Bool);
  procedure counter_timer_handler(this_ptr:Pointer);
  procedure wait_for_event();

  function time_usec:Bit64u;
  function time_ticks:Bit64u;

  procedure dma_write8(phy_addr:Bit32u; channel:unsigned; verify:Bool);
  procedure dma_read8(phy_addr:Bit32u; channel:unsigned);
  procedure dma_write16(phy_addr:Bit32u; channel:unsigned; verify:Bool);
  procedure dma_read16(phy_addr:Bit32u; channel:unsigned);

  function inp(addr:Bit16u; io_len:unsigned):Bit32u;
  procedure outp(addr:Bit16u; value:Bit32u; io_len:unsigned);
  procedure set_enable_a20(value:Bit8u);
  function get_enable_a20:Bool;
  procedure exit;

end;

const COUNTER_INTERVAL:Bit64u = 100000;

implementation

function bx_pc_system_c.ticks_remaining(index:Integer):Int64;
begin
	Result:=timer[index].remaining;
end;

procedure bx_pc_system_c.tick1;
begin
  inc(ips_count);
  dec(num_cpu_ticks_left);
    if (num_cpu_ticks_left = 0) then
      begin
        timer_handler();
      end;
end;

procedure bx_pc_system_c.tickn(n:Bit64u);
begin
  ips_count := ips_count + n;
    if (num_cpu_ticks_left > n) then
      begin
        dec(num_cpu_ticks_left, n);
        exit;
      end;
    while (n >= num_cpu_ticks_left) do
      begin
        n := n - num_cpu_ticks_left;
        num_cpu_ticks_left := 0;
        timer_handler();
      end;
end;

  // constructor
constructor bx_pc_system_c.Create;
var
  I:Word;
begin
  //Self^.put('SYS');
  COUNTER_INTERVAL:=100000;

  num_timers := 0;
  // set ticks period and remaining to max Bit32u value
  num_cpu_ticks_left := Bit32u(-1);
  num_cpu_ticks_in_period := num_cpu_ticks_left;
  m_ips := 0.0;

  for I:=0 to 8 do //for (unsigned int i:=0; i < 8; i++)
    begin
      DRQ[i] := 0;
      DACK[i] := 0;
    end;
  TC := 0;
  HRQ := 0;
  HLDA := 0;

  enable_a20 := 1;
  //set_INTR (0);

{$if BX_CPU_LEVEL < 2}
  a20_mask   :=    $fffff;
{$elseif BX_CPU_LEVEL = 2}
  a20_mask   :=   $ffffff;
{$else} (* 386+ *)
  a20_mask   := $ffffffff;
{$ifend}

  counter := 0;
  counter_timer_index := register_timer_ticks(Self, counter_timer_handler, COUNTER_INTERVAL, 1, 1);
end;

procedure bx_pc_system_c.init_ips(ips:Bit32u);
begin
  // parameter 'ips' is the processor speed in Instructions-Per-Second
  m_ips := ips / 1000000.0;
  BX_DEBUG(Format('ips := %u', [ips]));
end;

procedure bx_pc_system_c.raise_HLDA;
begin
  HLDA := 1;
  bx_devices.raise_hlda();
  HLDA := 0;
end;

  procedure
bx_pc_system_c.set_DRQ(channel:unsigned; val:bool);
begin
  if Boolean(channel > 7) then
    BX_PANIC(('set_DRQ() channel > 7'));
  DRQ[channel] := val;
  bx_devices.drq(channel, val);
end;

procedure bx_pc_system_c.set_HRQ(val:bool);
begin
  HRQ := val;
  if Boolean(val) then
    bx_cpu.async_event := 1
  else
    HLDA := 0; // ??? needed?
end;

  procedure
bx_pc_system_c.set_TC(val:bool);
begin
  TC := val;
end;

  procedure
bx_pc_system_c.set_DACK(channel:unsigned; val:Bool);
begin
  DACK[channel] := val;
end;

procedure bx_pc_system_c.dma_write8(phy_addr:Bit32u; channel:unsigned; verify:Bool);
var
  data_byte:Bit8u;
begin
  // DMA controlled xfer of byte from I/O to Memory

  bx_devices.dma_write8(channel, @data_byte); 
  if Boolean(verify=0) then begin
    sysmemory.write_physical(bx_cpu, phy_addr, 1, @data_byte);

    //BX_DBG_DMA_REPORT(phy_addr, 1, BX_WRITE, data_byte);
    end;
end;

procedure bx_pc_system_c.dma_read8(phy_addr:Bit32u; channel:unsigned);
var
  data_byte:Bit8u;
begin
  // DMA controlled xfer of byte from Memory to I/O

  sysmemory.read_physical(bx_cpu, phy_addr, 1, @data_byte);
  bx_devices.dma_read8(channel, @data_byte);

  //BX_DBG_DMA_REPORT(phy_addr, 1, BX_READ, data_byte);
end;

procedure bx_pc_system_c.dma_write16(phy_addr:Bit32u; channel:unsigned; verify:Bool);
var
  data_word:Bit16u;
begin
  // DMA controlled xfer of word from I/O to Memory

  bx_devices.dma_write16(channel, @data_word); 
  if Boolean(verify=0) then begin
    sysmemory.write_physical(bx_cpu, phy_addr, 2, @data_word);

    //BX_DBG_DMA_REPORT(phy_addr, 2, BX_WRITE, data_word);
    end;
end;

procedure bx_pc_system_c.dma_read16(phy_addr:Bit32u; channel:unsigned);
var
  data_word:Bit16u;
begin
  // DMA controlled xfer of word from Memory to I/O

  sysmemory.read_physical(bx_cpu, phy_addr, 2, @data_word);
  bx_devices.dma_read16(channel, @data_word); 

  //BX_DBG_DMA_REPORT(phy_addr, 2, BX_READ, data_word);
end;

procedure bx_pc_system_c.set_INTR(value:bool);
begin
  BX_INFO(Format('pc_system: Setting INTR:=%d on bootstrap processor %d', [value, BX_BOOTSTRAP_PROCESSOR]));
  //INTR := value;
  bx_cpu.set_INTR(value);
end;

//
// Read from the IO memory address space
//

function bx_pc_system_c.inp(addr:Bit16u; io_len:unsigned):Bit32u;
var
  ret:Bit32u;
begin

  ret := bx_devices.inp(addr, io_len); 

  Result:= ret ;
end;

//
// Write to the IO memory address space.
//

procedure bx_pc_system_c.outp(addr:Bit16u; value:Bit32u; io_len:unsigned);
begin
  bx_devices.outp(addr, value, io_len); 
end;

procedure bx_pc_system_c.set_enable_a20(value:Bit8u);
begin
{$if BX_CPU_LEVEL < 2}
    BX_PANIC(('set_enable_a20() called: 8086 emulation'));
{$else}

{$if BX_SUPPORT_A20=1}
  if Boolean(value) then begin
    enable_a20 := 1;
{$if BX_CPU_LEVEL = 2}
    a20_mask   := $ffffff;   (* 286: enable all 24 address lines *)
{$else} (* 386+ *)
    a20_mask   := $ffffffff; (* 386: enable all 32 address lines *)
{$ifend}
    end
  else begin
    enable_a20 := 0;
    a20_mask   := $ffefffff;   (* mask off A20 address line *)
    end;

  //BX_DBG_A20_REPORT(value);

  BX_DEBUG(Format('A20: set() := %u', [enable_a20]));
{$else}
  BX_DEBUG(('set_enable_a20: ignoring: SUPPORT_A20 := 0'));
{$ifend}  // #if BX_SUPPORT_A20

{$ifend}
end;

function bx_pc_system_c.get_enable_a20:Bool;
begin
{$if BX_SUPPORT_A20=1}
  BX_INFO(Format('A20: get() := %u',[enable_a20]));

  if Boolean(enable_a20) then begin Result:=1; exit; end
  else begin Result:=0; exit; end;
{$else}
  BX_INFO(('get_enable_a20: ignoring: SUPPORT_A20 := 0'));
  return(1);
{$ifend}  // #if BX_SUPPORT_A20
end;

function bx_pc_system_c.ResetSignal( operation:PCS_OP ):Integer;
begin
  //UNUSED( operation );
  // Reset the processor.

  BX_ERROR(( '# bx_pc_system_c.ResetSignal() called' ));
  {for (int i:=0; i<BX_SMP_PROCESSORS; i++)
    BX_CPU(i)^.reset(BX_RESET_SOFTWARE);}
  Result:=0;
end;

function IAC:Bit8u;
begin
  Result:=bx_devices.pic^.IAC();
end;

procedure bx_pc_system_c.exit;
begin
{  if Boolean(bx_devices.hard_drive) then
    bx_devices.hard_drive^.close_harddrive();}
  //BX_INFO(Format('Last time is %d',[bx_cmos.s.timeval])); 
  //bx_gui.exit();
end;


//
// bochs timer support
//

procedure bx_pc_system_c.timer_handler;
var
  min:Bit64u;
  i:unsigned;
  delta:Bit64u;
begin

  //  BX_ERROR(( 'Time handler ptime := %d', bx_pc_system.time_ticks() ));

  delta := num_cpu_ticks_in_period - num_cpu_ticks_left;
{$if BX_TIMER_DEBUG}
  if Boolean(num_cpu_ticks_left <> 0)
    BX_PANIC(('timer_handler: ticks_left!:=0'));
{$ifend}

  for i:=0 to num_timers do
    begin
      timer[i].triggered := 0;
      if Boolean(timer[i].active) then begin
      timer[i].remaining := timer[i].remaining - delta;
      if Boolean(timer[i].remaining = 0) then begin
        timer[i].triggered := 1;
        // reset remaining period for triggered timer
        timer[i].remaining := timer[i].period;

        // if triggered timer is one-shot, deactive
        if Boolean(timer[i].continuous=0) then
          timer[i].active := 0;
        end;
      end;
    end;

  min := Bit64u(-1); // max number in Bit64u range
  for i:=0 to num_timers do
    begin
    if Boolean((timer[i].active<>0) and (timer[i].remaining < min)) then
      min := timer[i].remaining;
    end;
  num_cpu_ticks_left := min;
  num_cpu_ticks_in_period := num_cpu_ticks_left;

  for  i:=0 to num_timers do begin
    // call requested timer function.  It may request a different
    // timer period or deactivate, all cases handled below
    if Boolean(timer[i].triggered) then begin
      timer[i].funct(timer[i].this_ptr);
      end;
    end;
end;

procedure bx_pc_system_c.expire_ticks;
var
  i:unsigned;
  ticks_delta:Bit64u;
begin

  ticks_delta := num_cpu_ticks_in_period - num_cpu_ticks_left;
  if Boolean(ticks_delta = 0) then exit; // no ticks occurred since
  for i:=0 to num_timers do begin
    if Boolean(timer[i].active) then begin
      timer[i].remaining := timer[i].remaining - ticks_delta; // must be >= 1 here
      end;
    end;

  // set new period to number of ticks left
  num_cpu_ticks_in_period := num_cpu_ticks_left;
end;

function bx_pc_system_c.register_timer( this_ptr:Pointer; funct:bx_timer_handler_t; useconds:Bit32u; continuous:Bool;active:Bool):Integer;
var
  instructions:Bit64u;
begin

  if Boolean(num_timers >= BX_MAX_TIMERS) then begin
    BX_PANIC(('register_timer: too many registered timers.'));
    end;

  if Boolean(this_ptr = NULL) then
    BX_PANIC(('register_timer: this_ptr is NULL'));
  if Boolean(@funct = nil) then
    BX_PANIC(('register_timer: funct is NULL'));

  // account for ticks up to now
  expire_ticks();

  // convert useconds to number of instructions
  instructions := Trunc(useconds * m_ips);
  if((useconds<>0) and (instructions=0)) then instructions := 1;

  Result:= register_timer_ticks(this_ptr, funct, instructions, continuous, active);
end;

function bx_pc_system_c.register_timer_ticks(this_ptr:Pointer; funct:bx_timer_handler_t; Instructions:Bit64u; continuous:Bool; active:Bool):Integer;
var
  i:Word;
begin

  if Boolean(num_timers >= BX_MAX_TIMERS) then begin
    BX_PANIC(('register_timer: too many registered timers.'));
    end;

  if Boolean(this_ptr = NULL) then
    BX_PANIC(('register_timer: this_ptr is NULL'));
  if Boolean(@funct = NULL) then
    BX_PANIC(('register_timer: funct is NULL'));

  i := num_timers;
  inc(num_timers);
  timer[i].period    := instructions;
  timer[i].remaining := instructions;
  timer[i].active    := active;
  timer[i].funct     := funct;
  timer[i].continuous := continuous;
  timer[i].this_ptr   := this_ptr;

  if Boolean(active) then begin
    if Boolean(num_cpu_ticks_in_period = 0) then begin
      // no active timers
      num_cpu_ticks_in_period := instructions;
      num_cpu_ticks_left      := instructions;
      end
  else begin
      if Boolean(instructions < num_cpu_ticks_left) then begin
        num_cpu_ticks_in_period := instructions;
        num_cpu_ticks_left      := instructions;
        end;
      end;
    end;

  // return timer id
  Result:=i;
end;

procedure bx_pc_system_c.counter_timer_handler(this_ptr:Pointer);
begin
      //UNUSED(this_ptr);

   Inc(counter);
end;

function bx_pc_system_c.time_usec:Bit64u;
begin
  Result:= Trunc(time_ticks() / m_ips );
end;

function bx_pc_system_c.time_ticks:Bit64u;
begin
      Result:= (counter + 1) * COUNTER_INTERVAL
	    - ticks_remaining(counter_timer_index)
	    + (Bit64u(num_cpu_ticks_in_period) - Bit64u(num_cpu_ticks_left));
end;

procedure bx_pc_system_c.start_timers;
begin
end;

procedure bx_pc_system_c.activate_timer_ticks (timer_index:unsigned; instructions:Bit64u; continuous:Bool);
begin
  if Boolean(timer_index >= num_timers) then
    BX_PANIC(('activate_timer(): bad timer index given'));

  // set timer continuity to new value (1:=continuous, 0:=one-shot)
  timer[timer_index].continuous := continuous;

  timer[timer_index].active := 1;
  timer[timer_index].remaining := instructions;

  if Boolean(num_cpu_ticks_in_period = 0) then begin
    // no active timers
    num_cpu_ticks_in_period := instructions;
    num_cpu_ticks_left      := instructions;
    end
  else begin
    if Boolean(instructions < num_cpu_ticks_left) then begin
      num_cpu_ticks_in_period := instructions;
      num_cpu_ticks_left      := instructions;
      end;
    end;
end;

procedure bx_pc_system_c.activate_timer( timer_index:unsigned; useconds:Bit32u; continuous:Bool);
var
  instructions:Bit64u;
begin

  if Boolean(timer_index >= num_timers) then
    BX_PANIC(('activate_timer(): bad timer index given'));

  // account for ticks up to now
  expire_ticks();

  // set timer continuity to new value (1:=continuous, 0:=one-shot)
  timer[timer_index].continuous := continuous;

  // if useconds := 0, use default stored in period field
  // else set new period from useconds
  if Boolean(useconds=0) then
    instructions := timer[timer_index].period
  else begin
    // convert useconds to number of instructions
    instructions := Trunc(useconds * m_ips);
    if(instructions=0) then instructions := 1;
    timer[timer_index].period := instructions;
    end;

  timer[timer_index].active := 1;
  timer[timer_index].remaining := instructions;

  if Boolean(num_cpu_ticks_in_period = 0) then begin
    // no active timers
    num_cpu_ticks_in_period := instructions;
    num_cpu_ticks_left      := instructions;
    end
  else begin
    if Boolean(instructions < num_cpu_ticks_left) then begin
      num_cpu_ticks_in_period := instructions;
      num_cpu_ticks_left      := instructions;
      end;
    end;
end;

procedure bx_pc_system_c.deactivate_timer( timer_index:unsigned );
begin
  if Boolean(timer_index >= num_timers) then
    BX_PANIC(('deactivate_timer(): bad timer index given'));

  timer[timer_index].active := 0;
end;
end.
