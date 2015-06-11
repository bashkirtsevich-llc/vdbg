//        MOZAA - Virtual PC - Pascal Conversion by Max Boidi - www.mozaa.org

//  Copyright (C) 2001  MandrakeSoft S.A.
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

unit cmos;

interface

uses iodev,service,config, SysUtils, PIC,DateUtils;

type

  tm = record
   tm_hour : Byte;
   tm_min  : Byte;
   tm_mon  : Byte;
   tm_sec  : Byte;
   tm_wday : Byte; //Day of week (0-6; Sunday = 0)
   tm_yday : Integer;
   tm_year : integer;
   tm_mday : Byte;
   tm_isdst: Bool;
  end;

  pbx_cmos_c = ^bx_cmos_c;
  bx_cmos_c = class
  public
    s: record
      periodic_timer_index:integer;
      periodic_interval_usec:Bit32u;
      one_second_timer_index:integer;
      timeval:TDateTime;
      cmos_mem_address:Bit8u;

      reg: array[0..BX_NUM_CMOS_REGS] of Bit8u;
    end;  // state information

    procedure init(d:pbx_devices_c);
    procedure checksum_cmos;
    procedure reset;


  private
    devices:pbx_devices_c;

    function  read_handler(self_ptr:Pointer; address:Bit32u; io_len:unsigned):Bit32u;
    procedure write_handler(self_ptr:Pointer; address:Bit32u; value:Bit32u; io_len:unsigned);
  {$if BX_USE_PIC_SMF=0}
    function  read(address:Bit32u; io_len:unsigned):Bit32u;
    procedure write(address:Bit32u; value:Bit32u; io_len:unsigned);
  {$ifend}

  public
    procedure periodic_timer_handler(self_ptr:pointer);
    procedure one_second_timer_handler(self_ptr:pointer);
    procedure periodic_timer;
    procedure one_second_timer;
  private
    procedure update_clock;
    procedure CRA_change;
  end;

var
  bx_cmos:bx_cmos_c;

implementation

uses cpu;

procedure bx_cmos_c.init(d:pbx_devices_c);
var
	i:unsigned;
begin
//	BX_DEBUG(('Init $Id: cmos.cc,v 1.16 2002/01/29 17:20:11 vruppert Exp $'));

	// CMOS RAM  and RTC

	self.devices := d;

	self.devices^.register_io_read_handler(@bx_cmos,read_handler, $0070,'CMOS RAM');
	self.devices^.register_io_read_handler(@bx_cmos,read_handler,	$0071,'CMOS RAM');
	self.devices^.register_io_write_handler(@bx_cmos,write_handler,$0070, 'CMOS RAM');
	self.devices^.register_io_write_handler(@bx_cmos,	write_handler,$0071, 'CMOS RAM');
	self.devices^.register_irq(8, 'CMOS RTC');

	self.s.periodic_timer_index := bx_pc_system.register_timer(@bx_cmos, periodic_timer_handler,
      1000000, 1,0); // continuous, not-active
	self.s.one_second_timer_index :=  bx_pc_system.register_timer(@bx_cmos, one_second_timer_handler,
      1000000, 1,0); // continuous, not-active

  for i := 0 to BX_NUM_CMOS_REGS do
  begin
    self.s.reg[i] := 0;
  end;

{$if BX_USE_SPECIFIED_TIME0 = 0}
	// ??? self will not be correct for using an image file.
	// perhaps take values in CMOS and work backwards to find
	// s.timeval from values read in.
	self.s.timeval := time;
{$else}
	self.s.timeval := BX_USE_SPECIFIED_TIME0;
{$ifend}

  if (Options_cmos_time = 1)
    then self.s.timeval := time
    else

  if (Options_cmos_time <> 0)
    then self.s.timeval := 0;

	{while( (tmptime <>  strdup(ctime(@(self.s.timeval)))) = NULL) then begin
		BX_PANIC(('Out of memory.'));
	end;
	tmptime[strlen(tmptime)-1]:='\0';}

	LogInfo(Format('Setting initial clock to: %d  ---  occhio!!!',[0]));

	update_clock();

	// load CMOS from image file if requested.
  if Boolean(Options_cmos_use_image) then
  begin(*
    // CMOS image file requested
    int fd, ret;
    struct stat stat_buf;

    fd := open(bx_options.cmos.Opath^.getptr (), O_RDONLY
#ifdef O_BINARY
			orO_BINARY
{$ifend}
			  );
    if Boolean(fd < 0) then begin
      BX_PANIC(('trying to open cmos image file '%s'',
		 bx_options.cmos.Opath^.getptr ()));
      end;
    ret := fstat(fd, @stat_buf);
    if Boolean(ret) then begin
      BX_PANIC(('CMOS: could not fstat() image file.'));
      end;
    if Boolean(stat_buf.st_size !:= BX_NUM_CMOS_REGS) then begin
      BX_PANIC(('CMOS: image file not same size as BX_NUM_CMOS_REGS.'));
      end;

    ret := .read(fd, (bx_ptr_t) self.s.reg, BX_NUM_CMOS_REGS);
    if Boolean(ret !:= BX_NUM_CMOS_REGS) then begin
      BX_PANIC(('CMOS: error reading cmos file.'));
      end;
    close(fd);
    BX_INFO(('successfuly read from image file '%s'.',
      bx_options.cmos.Opath^.getptr ()));
    end;
	else begin*)
    // CMOS values generated
  end else
  begin
    self.s.reg[$0a] := $26;
    self.s.reg[$0b] := $02;
    self.s.reg[$0c] := $00;
    self.s.reg[$0d] := $80;
    if BX_SUPPORT_FPU
      then
      self.s.reg[$14] := $02;
  end;
end;

procedure bx_cmos_c.reset;
begin
	self.s.cmos_mem_address := 0;

	// RESET affects the following registers:
	//  CRA: no effects
	//  CRB: bits 4,5,6 forced to 0
	//  CRC: bits 4,5,6,7 forced to 0
	//  CRD: no effects
	self.s.reg[$0b] := self.s.reg[$0b] and $8f;
	self.s.reg[$0c] := 0;

	// One second timer for updating clock  and alarm functions
	bx_pc_system.activate_timer(self.s.one_second_timer_index, 1000000, 1);

	// handle periodic interrupt rate select
	self.CRA_change();
end;

procedure bx_cmos_c.CRA_change;
var
	nibble: unsigned;
begin
  // Periodic Interrupt timer
  nibble := self.s.reg[$0a] and $0f;
  if (nibble = 0) then
  begin
    // No Periodic Interrupt Rate when 0, deactivate timer
    bx_pc_system.deactivate_timer(self.s.periodic_timer_index);
    self.s.periodic_interval_usec := Bit32u(-1); // max value
  end else
  begin
    // values 0001b and 0010b are the same as 1000b and 1001b
    if (nibble <= 2) then
      inc(nibble, 7);

    self.s.periodic_interval_usec := unsigned(1000000 div
                                              (32768 div (1 shl (nibble - 1))));

    // if Periodic Interrupt Enable bit set, activate timer
    if ( self.s.reg[$0b] and $40 ) <> 0
      then bx_pc_system.activate_timer(self.s.periodic_timer_index,
		                    self.s.periodic_interval_usec, 1)
      else bx_pc_system.deactivate_timer(self.s.periodic_timer_index);
  end;
end;
	// static IO port read callback handler
	// redirects to non-static class handler to aprocedure virtual functions
function bx_cmos_c.read_handler(self_ptr: Pointer; address: Bit32u;
  io_len: unsigned): Bit32u;
var
	ret8: Bit8u;
begin
  if (io_len > 1) then
    LogPanic(Format('io read from address %08x len:=%u', [unsigned(address), unsigned(io_len)]));

  if (BX_CMOS_DEBUG) <> 0 then
    LogInfo(Format('CMOS read of CMOS register $%x', [unsigned(self.s.cmos_mem_address)]));



  if address = $0071 then
  begin
    if (self.s.cmos_mem_address >= BX_NUM_CMOS_REGS) then
       LogPanic(Format('unsupported cmos io read, register($%02x)!',[unsigned(self.s.cmos_mem_address)]));

    ret8 := self.s.reg[self.s.cmos_mem_address];
    // all bits of Register C are cleared after a read occurs.
    if (self.s.cmos_mem_address = $0c) then
    begin
      self.s.reg[$0c] := $00;
      //self.devices^.pic^.lower_irq(8); !!!
      bx_pic.lower_irq(8);
    end;

    exit(ret8);
  end else
    LogPanic(Format('unsupported cmos read, address:=%x!',[unsigned(address)]));

  Result := 0;
end;
	// static IO port write callback handler
	// redirects to non-static class handler to aprocedure virtual functions
procedure bx_cmos_c.write_handler(self_ptr:Pointer; address:Bit32u; value:Bit32u; io_len:unsigned);
var
  dcc: unsigned;
  prev_CRB: unsigned;
begin
{$if BX_USE_CMOS_SMF = 0}
	bx_cmos_c *class_ptr := (bx_cmos_c *) self_ptr;

	class_ptr^.write(address, value, io_len);
end;

procedure bx_cmos_c.write(Bit32u address, Bit32u value, unsigned io_len)
begin
{$else}
	//UNUSED(self_ptr);
{$ifend}  // !BX_USE_CMOS_SMF

  if (io_len > 1) then
    LogPanic(Format('io write to address %08x len:=%u', [unsigned(address), unsigned(io_len)]));

  if (BX_CMOS_DEBUG)<>0 then
    LogInfo(Format('CMOS write to address: $%x := $%x', [unsigned(address), unsigned(value)]));


  case (address) of
    $0070:
    begin
      {$if BX_NUM_CMOS_REGS = 64}
      self.s.cmos_mem_address := value  and $3F;
      {$else}
      self.s.cmos_mem_address := value  and $7F;
      {$ifend}
    end;

    $0071:
    begin
      if (self.s.cmos_mem_address >= BX_NUM_CMOS_REGS) then
      begin
        LogPanic(Format('unsupported cmos io write, register($%02x):=%02x!',[unsigned(self.s.cmos_mem_address), unsigned(value)]));
        exit;
      end;

      if self.s.cmos_mem_address in [$00..$09] then
      begin
        (*case $00: // seconds
        case $01: // seconds alarm
        case $02: // minutes
        case $03: // minutes alarm
        case $04: // hours
        case $05: // hours alarm
        case $06: // day of the week
        case $07: // day of the month
        case $08: // month
        case $09: *)// year
         //BX_INFO(('write reg %02xh: value := %02xh',
         //    (unsigned) self.s.cmos_mem_address, (unsigned) value);
        self.s.reg[self.s.cmos_mem_address] := 0;
        exit;
      end;

      if self.s.cmos_mem_address = $0a then
      begin  // Control Register A
        // bit 7: Update in Progress (read-only)
        //   1 := signifies time registers will be updated within 244us
        //   0 := time registers will not occur before 244us
        //   note: self bit reads 0 when CRB bit 7 is 1
        // bit 6..4: Divider Chain Control
        //   000 oscillator disabled
        //   001 oscillator disabled
        //   010 Normal operation
        //   011 TEST
        //   100 TEST
        //   101 TEST
        //   110 Divider Chain RESET
        //   111 Divider Chain RESET
        // bit 3..0: Periodic Interrupt Rate Select
        //   0000 None
        //   0001 3.90625  ms
        //   0010 7.8125   ms
        //   0011 122.070  us
        //   0100 244.141  us
        //   0101 488.281  us
        //   0110 976.562  us
        //   0111 1.953125 ms
        //   1000 3.90625  ms
        //   1001 7.8125   ms
        //   1010 15.625   ms
        //   1011 31.25    ms
        //   1100 62.5     ms
        //   1101 125      ms
        //   1110 250      ms
        //   1111 500      ms

        dcc := (value shr 4) and $07;
        if (dcc <> $02) then
         LogPanic(Format('CRA: divider chain control $%x', [dcc]));

        self.s.reg[$0a] := value and $7f;
        self.CRA_change();
        exit;
      end;

      if self.s.cmos_mem_address = $0b then
      begin// Control Register B
        // bit 0: Daylight Savings Enable
        //   1 := enable daylight savings
        //   0 := disable daylight savings
        // bit 1: 24/12 houre mod_e
        //   1 := 24 hour format
        //   0 := 12 hour format
        // bit 2: Data mod_e
        //   1 := binary format
        //   0 := BCD format
        // bit 3: 'square wave enable'
        //   Not supported and always read as 0
        // bit 4: Update Ended Interrupt Enable
        //   1 := enable generation of update ended interrupt
        //   0 := disable
        // bit 5: Alarm Interrupt Enable
        //   1 := enable generation of alarm interrupt
        //   0 := disable
        // bit 6: Periodic Interrupt Enable
        //   1 := enable generation of periodic interrupt
        //   0 := disable
        // bit 7: Set mod_e
        //   1 := user copy of time is 'frozen' allowing time registers
        //       to be accessed without regard for an occurance of an update
        //   0 := time updates occur normally

        // can not handle binary or 12-hour mod_e yet.
        if (value  and $04) <> 0 then
          LogPanic(('write status reg B, binary format enabled.'));

        if (value  and $02) = 0 then
          LogPanic(('write status reg B, 12 hour mod_e enabled.'));

         value := value and $f7; // bit3 always 0
        // Note: setting bit 7 clears bit 4
        if (value  and $80)<>0 then
         value := value and $ef;

        prev_CRB := self.s.reg[$0b];
        self.s.reg[$0b] := value;

        if ( (prev_CRB and $40) <> (value and $40) ) then
        begin
          // Periodic Interrupt Enabled changed
          if (prev_CRB and $40) <> 0
                 // transition from 1 to 0, deactivate timer
            then bx_pc_system.deactivate_timer(self.s.periodic_timer_index)
            else
            begin
              // transition from 0 to 1
              // if rate select is not 0, activate timer
              if ( (self.s.reg[$0a] and $0f) <> 0 ) then
              begin
                bx_pc_system.activate_timer(
                self.s.periodic_timer_index,
                self.s.periodic_interval_usec, 1);
              end;
            end;
        end;
        exit;
      end;

      if (self.s.cmos_mem_address = $0c) or (self.s.cmos_mem_address = $0d) then // Control Register C
      begin
        LogError(Format('write to control register $%x (read-only)',[self.s.cmos_mem_address]));
        exit;
      end;

      if (self.s.cmos_mem_address = $0e) then // diagnostic status
      begin
        //BX_DEBUG(Format('write register 0Eh: %02x',[unsigned(value)]));
        Exit;
      end;

      if (self.s.cmos_mem_address = $0f) then
      begin
        if value = $00 then
        begin
          //BX_DEBUG(('Reg 0F set to 0: shutdown action := normal POST'));
          exit;
        end;
        if value = $02 then
        begin
          //BX_DEBUG(('Reg 0Fh: request to change shutdown action to shutdown after memory test'));
          exit;
        end;
        if value = $03 then
        begin
          //BX_DEBUG(('Reg 0Fh(03) : Shutdown after memory test !'));
          exit;
        end;
        if value = $04 then
        begin
          //BX_DEBUG(('Reg 0Fh: request to change shutdown action to jump to disk bootstrap routine.'));
          exit;
        end;
        if value = $06 then
        begin
          //BX_DEBUG(('Reg 0Fh(06) : Shutdown after memory test !'));
          exit;
        end;
        if value = $09 then
        begin
          //BX_DEBUG(('Reg 0Fh: request to change shutdown action to return to BIOS extended memory block move.'));
          exit;
        end;
        if value = $0a then
        begin
          //BX_DEBUG(('Reg 0Fh: request to change shutdown action to jump to DWORD at 40:67'));
          exit;
        end;
        self.s.reg[self.s.cmos_mem_address] := value;
      end;
    end;
  end;
end;


procedure bx_cmos_c.checksum_cmos;
var
	i: unsigned;
	sum: Bit16u;
begin
	sum := 0;

  for i := $10 to $2d do
    sum := sum + self.s.reg[i];

	self.s.reg[$2e] := (sum shr 8) and $ff; (* checksum high *)
	self.s.reg[$2f] := (sum  and $ff);      (* checksum low *)
end;

procedure bx_cmos_c.periodic_timer_handler(self_ptr: pointer);
var
  class_ptr: pbx_cmos_c;
begin
  class_ptr := pbx_cmos_c(self_ptr);
  class_ptr^.periodic_timer();
end;

procedure bx_cmos_c.periodic_timer;
begin
  // if periodic interrupts are enabled, trip IRQ 8, and
  // update status register C
  if (self.s.reg[$0b]  and $40) <> 0 then
  begin
    self.s.reg[$0c] := self.s.reg[$0c] or $c0; // Interrupt Request, Periodic Int
    //self.devices^.pic^.raise_irq(8); !!!
    bx_pic.raise_irq(8);
  end;
end;

procedure bx_cmos_c.one_second_timer_handler(self_ptr:pointer);
var
  class_ptr: pbx_cmos_c;
begin
  class_ptr := pbx_cmos_c(self_ptr);
  class_ptr^.one_second_timer();
end;

procedure bx_cmos_c.one_second_timer();
var
  alarm_match: Bool;
begin
  // update internal time/date buffer
  self.s.timeval := self.s.timeval + 1;

  // Dont update CMOS user copy of time/date if CRB bit7 is 1
  // Nothing else do to
  if (self.s.reg[$0b] and $80) <> 0 then
    exit;

  update_clock();

  // if update interrupts are enabled, trip IRQ 8, and
  // update status register C
  if (self.s.reg[$0b] and $10)<>0 then
  begin
    self.s.reg[$0c] := self.s.reg[$0c] or $90; // Interrupt Request, Update Ended
    //self.devices^.pic^.raise_irq(8); !!!!
    bx_pic.raise_irq(8);
  end;

  alarm_match := 1;
  // compare CMOS user copy of time/date to alarm time/date here
  if (self.s.reg[$0b]  and $20) <> 0 then
  begin
    // Alarm interrupts enabled
    if ( (self.s.reg[$01]  and $c0) <> $c0 ) then
    begin
      // seconds alarm not in dont care mod_e
      if (self.s.reg[$00] <> self.s.reg[$01]) then
		    alarm_match := 0;
    end;
    if ( (self.s.reg[$03]  and $c0) <> $c0 ) then
    begin
      // minutes alarm not in dont care mod_e
      if (self.s.reg[$02] <> self.s.reg[$03]) then
		    alarm_match := 0;
    end;
    if ( (self.s.reg[$05]  and $c0) <> $c0 ) then
    begin
      // hours alarm not in dont care mod_e
      if (self.s.reg[$04] <> self.s.reg[$05]) then
		    alarm_match := 0;
    end;
    if (alarm_match) <> 0 then
    begin
      self.s.reg[$0c] := self.s.reg[$0c] or $a0; // Interrupt Request, Alarm Int
      //self.devices^.pic^.raise_irq(8); !!!
      bx_pic.raise_irq(8);
    end;
  end;
end;


procedure bx_cmos_c.update_clock;
var
	time_calendar: tm;
	year, month, day, century: unsigned;
	val_bcd: Bit8u;
  Hour,Minutes,Seconds,MSec: Word;
  NowDay,NowMonth,NowYear: Word;
begin
  DecodeTime(Now,Hour, Minutes, Seconds, MSec);
  DecodeDate(Now, NowYear, NowMonth, NowDay);
  time_calendar.tm_hour  := HourOf(Now);
  time_calendar.tm_min   := MinuteOf(Now);
  time_calendar.tm_sec   := SecondOf(Now);
  time_calendar.tm_year  := YearOf(Now) - 1900;
  time_calendar.tm_mon   := MonthOf(Now) - 1;
  time_calendar.tm_wday  := DayOfWeek(Now) - 1;
  time_calendar.tm_mday  := DayOf(Now);
  time_calendar.tm_isdst := 0;

	// update seconds
	val_bcd := (((time_calendar.tm_sec  div 10)) shl 4) or (time_calendar.tm_sec mod 10);
	self.s.reg[$00] := val_bcd;

	// update minutes
	val_bcd := ((time_calendar.tm_min  div 10) shl 4) or (time_calendar.tm_min mod 10);
	self.s.reg[$02] := val_bcd;

	// update hours
	val_bcd := ((time_calendar.tm_hour  div 10) shl 4) or (time_calendar.tm_hour mod 10);
	self.s.reg[$04] := val_bcd;

	// update day of the week
	day := time_calendar.tm_wday; // 0..6 to 1..7
	self.s.reg[$06] := ((day div 10) shl 4) or (day mod 10);

	// update day of the month
	day := time_calendar.tm_mday;
	self.s.reg[$07] := ((day div 10) shl 4) or (day mod 10);

	// update month
	month := time_calendar.tm_mon;
	self.s.reg[$08] := ((month div 10) shl 4) or (month mod 10);

	// update year
	year := time_calendar.tm_year mod 100;
	self.s.reg[$09] := ((year  div 10) shl 4) or (year mod 10);

	// update century
	century := Trunc(time_calendar.tm_year div 100) + 19;
	self.s.reg[$32] := ((century  div 10) shl 4) or (century mod 10);
end;

end.
