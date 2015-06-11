unit pit82c54;

interface

uses Config, Service, SysUtils;

const
  UNL_2P_READ     = 1;
  MAX_COUNTER     = 2;
  MAX_ADDRESS     = 3;
  CONTROL_ADDRESS = 3;

const
  LSByte = 0;
  MSByte = 1;
  LSByte_multiple = 2;
  MSByte_multiple = 3;

type
  real_RW_status = (
    LSB_real  = 1,
    MSB_real  = 2,
    BOTH_real = 3
    );

  pcounter_type = ^counter_type;

  counter_type = record
    //Chip IOs;
    GATE:   Bool; //GATE Input value at end of cycle
    OUTpin: Bool; //OUT output this cycle

    //Architected state;
    Count:        Bit32u; //Counter value this cycle
    outlatch:     Bit16u; //Output latch this cycle
    inlatch:      Bit16u; //Input latch this cycle
    status_latch: Bit8u;

    //Status Register data;
    rw_mode:    Bit8u; //2-bit R/W mode from command word register.
    mode:       Bit8u; //3-bit mode from command word register.
    bcd_mode:   bool; //1-bit BCD vs. Binary setting.
    null_count: bool; //Null count bit of status register.

    //Latch status data;
    count_LSB_latched: bool;
    count_MSB_latched: bool;
    status_latched:    bool;

    //Miscelaneous State;
    count_binary:     Bit32u; //Value of the count in binary.
    triggerGATE:      bool; //Whether we saw GATE rise this cycle.
    write_state:      Bit8u; //Read state this cycle
    read_state:       Bit8u;//rw_status; //Read state this cycle
    count_written:    bool; //Whether a count written since programmed
    first_pass:       bool; //Whether or not this is the first loaded count.
    state_bit_1:      bool; //Miscelaneous state bits.
    state_bit_2:      bool;
    next_change_time: Bit32u; //Next time something besides count changes.
    //0 means never.
  end;

  pit_82C54 = class
  private
    counter:       packed array[0..3] of counter_type;
    controlword:   Bit8u;
    seen_problems: integer;

    procedure latch_counter (thisctr: pcounter_type);
    procedure set_OUT (thisctr: pcounter_type; Data: bool);
    procedure set_count (thisctr: pcounter_type; Data: Bit32u);
    procedure set_count_to_binary (thisctr: pcounter_type);
    procedure set_binary_to_count (thisctr: pcounter_type);
    procedure decrement (thisctr: pcounter_type);
    procedure decrement_multiple (thisctr: pcounter_type; cycles: Bit32u);
    procedure clock (cnum: Bit8u);
//    procedure print_counter (thisctr: pcounter_type);
  public

    procedure   init;
    constructor Create;

    procedure clock_all (cycles: Bit32u);
    procedure clock_multiple (cnum: Bit8u; cycles: Bit32u);

    function  Read (address: Bit8u): Bit8u;
    procedure Write (address: Bit8u; Data: Bit8u);

    procedure set_GATE (cnum: Bit8u; Data: bool);
    function  read_GATE (cnum: Bit8u): bool;

    function  read_OUT (cnum: Bit8u): bool;

    function  get_clock_event_time (cnum: Bit8u): Bit32u;
    function  get_next_event_time: Bit32u;

    procedure print_cnum (cnum: Bit8u);
  end;

var
  bx_pit_82C54: pit_82C54;

implementation

//procedure pit_82C54.print_counter (thisctr: pcounter_type);
//  begin
//{$if 1=2}
//    BX_INFO(("Printing Counter"));
//    BX_INFO(("count: %d", thisctr.Count));
//    BX_INFO(("count_binary: %x", thisctr.count_binary));
//    BX_INFO(("counter gate: %x", thisctr.GATE));
//    BX_INFO(("counter OUT: %x", thisctr.OUTpin));
//    BX_INFO(("next_change_time: %d", thisctr.next_change_time));
//    BX_INFO(("End Counter Printout"));
//{$ifend}
//  end;

procedure pit_82C54.print_cnum (cnum: Bit8u);
  begin
  end;

procedure pit_82C54.latch_counter (thisctr: pcounter_type);
  begin
    if ((thisctr.count_LSB_latched <> 0) or (thisctr.count_MSB_latched <> 0)) then
      begin
      //Do nothing because previous latch has not been read.;
      end
    else
      begin
      case thisctr.read_state of
        MSByte:
          begin
          thisctr.outlatch := thisctr.Count and $FFFF;
          thisctr.count_MSB_latched := 1;
          end;
        LSByte:
          begin
          thisctr.outlatch := thisctr.Count and $FFFF;
          thisctr.count_LSB_latched := 1;
          end;
        LSByte_multiple:
          begin
          thisctr.outlatch := thisctr.Count and $FFFF;
          thisctr.count_LSB_latched := 1;
          thisctr.count_MSB_latched := 1;
          end;
        MSByte_multiple:
          begin
          if ((seen_problems and UNL_2P_READ) = 0) then
            begin
            //    seen_problems|:=UNL_2P_READ;
            LogError(('Unknown behavior when latching during 2-part read.'));
            LogError(('  This message will not be repeated.'));
            end;
          //I guess latching and resetting to LSB first makes sense;
//          BX_DEBUG(('Setting read_state to LSB_mult'));
          thisctr.read_state := LSByte_multiple;
          thisctr.outlatch := thisctr.Count and $FFFF;
          thisctr.count_LSB_latched := 1;
          thisctr.count_MSB_latched := 1;
          end;
        else
          LogError(('Unknown read mod_e found during latch command.'));
        end;
      end;
  end;

procedure pit_82C54.set_OUT (thisctr: pcounter_type; Data: bool);
  begin
    //This will probably have a callback, so I put it here.
    thisctr.OUTpin := Data;
  end;

procedure pit_82C54.set_count (thisctr: pcounter_type; Data: Bit32u);
  begin
    thisctr.Count := Data and $FFFF;
    set_binary_to_count(thisctr);
  end;

procedure pit_82C54.set_count_to_binary (thisctr: pcounter_type);
  begin
    if boolean(thisctr.bcd_mode) then
      begin
      thisctr.Count :=
        (((thisctr.count_binary div 1) mod 10) shl 0) or
        (((thisctr.count_binary div 10) mod 10) shl 4) or
        (((thisctr.count_binary div 100) mod 10) shl 8) or
        (((thisctr.count_binary div 1000) mod 10) shl 12);
      end
    else
      begin
      thisctr.Count := thisctr.count_binary;
      end;
  end;

procedure pit_82C54.set_binary_to_count (thisctr: pcounter_type);
  begin
    if boolean(thisctr.bcd_mode) then
      begin
      thisctr.count_binary :=
        (1 * ((thisctr.Count shr 0) and $F)) + (10 * ((thisctr.Count shr 4) and $F)) +
        (100 * ((thisctr.Count shr 8) and $F)) +
        (1000 * ((thisctr.Count shr 12) and $F));
      end
    else
      begin
      thisctr.count_binary := thisctr.Count;
      end;
  end;

procedure pit_82C54.decrement (thisctr: pcounter_type);
  begin
    if (thisctr.Count = 0) then
      begin
      if boolean(thisctr.bcd_mode) then
        begin
        thisctr.Count := $9999;
        thisctr.count_binary := 9999;
        end
      else
        begin
        thisctr.Count := $FFFF;
        thisctr.count_binary := $FFFF;
        end;
      end
    else
      begin
      Dec(thisctr.count_binary);
      set_count_to_binary(thisctr);
      end;
  end;

procedure pit_82C54.init;
  var
    i: Bit8u;
  begin
    i := 0;
    while i < 3 do
      begin
//      BX_DEBUG(('Setting read_state to LSB'));
      counter[i].read_state := LSByte;
      counter[i].write_state := LSByte;
      counter[i].GATE  := 1;
      counter[i].OUTpin := 1;
      counter[i].triggerGATE := 0;
      counter[i].mode  := 4;
      counter[i].first_pass := 0;
      counter[i].bcd_mode := 0;
      counter[i].Count := 0;
      counter[i].count_binary := 0;
      counter[i].state_bit_1 := 0;
      counter[i].state_bit_2 := 0;
      counter[i].null_count := 0;
      counter[i].rw_mode := 1;
      counter[i].count_written := 1;
      counter[i].count_LSB_latched := 0;
      counter[i].count_MSB_latched := 0;
      counter[i].status_latched := 0;
      counter[i].next_change_time := 0;
      Inc(i);
      end;
    seen_problems := 0;
  end;

constructor pit_82C54.Create;
  begin
    inherited;
    init();
  end;

procedure pit_82C54.decrement_multiple (thisctr: pcounter_type; cycles: Bit32u);
  begin
    while (cycles > 0) do
      begin
      if (cycles <= thisctr.count_binary) then
        begin
        thisctr.count_binary := thisctr.count_binary - cycles;
        cycles := cycles - cycles;
        set_count_to_binary(thisctr);
        end
      else
        begin
        cycles := cycles - (thisctr.count_binary + 1);
        thisctr.count_binary := thisctr.count_binary - thisctr.count_binary;
        set_count_to_binary(thisctr);
        decrement(thisctr);
        end;
      end;
  end;

procedure pit_82C54.clock_multiple (cnum: Bit8u; cycles: Bit32u);
  var
    thisctr: pcounter_type;
  begin
    if (cnum > MAX_COUNTER) then
      begin
      LogError(('Counter number too high in clock'));
      end
    else
      begin
      thisctr := @counter[cnum];
      while (cycles > 0) do
        begin
        if (thisctr.next_change_time = 0) then
          begin
          if boolean(thisctr.count_written) then
            begin
            case (thisctr.mode) of
              0:
                begin
                if ((thisctr.GATE <> 0) and (thisctr.write_state <>
                  MSByte_multiple)) then
                  begin
                  decrement_multiple(thisctr, cycles);
                  end;
                end;
              1:
                begin
                decrement_multiple(thisctr, cycles);
                end;
              2:
                begin
                if ((thisctr.first_pass = 0) and (thisctr.GATE <> 0)) then
                  begin
                  decrement_multiple(thisctr, cycles);
                  end;
                end;
              3:
                begin
                if ((thisctr.first_pass = 0) and (thisctr.GATE <> 0)) then
                  begin
                  decrement_multiple(thisctr, 2 * cycles);
                  end;
                end;
              4:
                begin
                if boolean(thisctr.GATE) then
                  begin
                  decrement_multiple(thisctr, cycles);
                  end;
                end;
              5:
                begin
                decrement_multiple(thisctr, cycles);
                end;
              else
                break;
              end;
            end;
          cycles := cycles - cycles;
          end
        else
          begin
          case (thisctr.mode) of
            0,
            1,
            2,
            4,
            5:
              begin
              if (thisctr.next_change_time > cycles) then
                begin
                decrement_multiple(thisctr, cycles);
                thisctr.next_change_time := thisctr.next_change_time - cycles;
                cycles := cycles - cycles;
                end
              else
                begin
                decrement_multiple(thisctr, (thisctr.next_change_time - 1));
                cycles := cycles - thisctr.next_change_time;
                clock(cnum);
                end;
              end;
            3:
              begin
              if (thisctr.next_change_time > cycles) then
                begin
                decrement_multiple(thisctr, cycles * 2);
                thisctr.next_change_time := thisctr.next_change_time - cycles;
                cycles := cycles - cycles;
                end
              else
                begin
                decrement_multiple(thisctr, (thisctr.next_change_time - 1) * 2);
                cycles := cycles - thisctr.next_change_time;
                clock(cnum);
                end;
              end;
            else
              cycles := cycles - cycles;
            end;
          end;
        end;
{$if 0=1}
      print_counter(thisctr);
{$ifend}
      end;
  end;

procedure pit_82C54.clock (cnum: Bit8u);
  var
    thisctr: pcounter_type;
  begin
    if (cnum > MAX_COUNTER) then
      begin
      LogError(('Counter number too high in clock'));
      end
    else
      begin
      thisctr := @counter[cnum];
      case (thisctr.mode) of
        0:
          begin
          if boolean(thisctr.count_written) then
            begin
            if boolean(thisctr.null_count) then
              begin
              set_count(thisctr, thisctr.inlatch);
              if boolean(thisctr.GATE) then
                begin
                if boolean(thisctr.count_binary = 0) then
                  begin
                  thisctr.next_change_time := 1;
                  end
                else
                  begin
                  thisctr.next_change_time := thisctr.count_binary and $FFFF;
                  end;
                end
              else
                begin
                thisctr.next_change_time := 0;
                end;
              thisctr.null_count := 0;
              end
            else
              begin
              if ((thisctr.GATE <> 0) and (thisctr.write_state <> MSByte_multiple)) then
                begin
                decrement(thisctr);
                if (thisctr.OUTpin = 0) then
                  begin
                  thisctr.next_change_time := thisctr.count_binary and $FFFF;
                  if (thisctr.Count = 0) then
                    begin
                    set_OUT(thisctr, 1);
                    end;
                  end
                else
                  begin
                  thisctr.next_change_time := 0;
                  end;
                end
              else
                begin
                thisctr.next_change_time := 0; //if the clock isn't moving.
                end;
              end;
            end
          else
            begin
            thisctr.next_change_time := 0; //default to 0.
            end;
          thisctr.triggerGATE := 0;
          end;
        1:
          begin
          if boolean(thisctr.count_written) then
            begin
            if boolean(thisctr.triggerGATE) then
              begin
              set_count(thisctr, thisctr.inlatch);
              if (thisctr.count_binary = 0) then
                begin
                thisctr.next_change_time := 1;
                end
              else
                begin
                thisctr.next_change_time := thisctr.count_binary and $FFFF;
                end;
              thisctr.null_count := 0;
              set_OUT(thisctr, 0);
              if (thisctr.write_state = MSByte_multiple) then
                begin
                LogError(('Undefined behavior when loading a half loaded count.'));
                end;
              end
            else
              begin
              decrement(thisctr);
              if (thisctr.OUTpin = 0) then
                begin
                if (thisctr.count_binary = 0) then
                  begin
                  thisctr.next_change_time := 1;
                  end
                else
                  begin
                  thisctr.next_change_time := thisctr.count_binary and $FFFF;
                  end;
                if (thisctr.Count = 0) then
                  begin
                  set_OUT(thisctr, 1);
                  end;
                end
              else
                begin
                thisctr.next_change_time := 0;
                end;
              end;
            end
          else
            begin
            thisctr.next_change_time := 0; //default to 0.
            end;
          thisctr.triggerGATE := 0;
          end;
        2:
          begin
          if boolean(thisctr.count_written) then
            begin
            if boolean((thisctr.triggerGATE <> 0) or (thisctr.first_pass <> 0)) then
              begin
              set_count(thisctr, thisctr.inlatch);
              thisctr.next_change_time := (thisctr.count_binary - 1) and $FFFF;
              thisctr.null_count := 0;
              if (thisctr.inlatch = 1) then
                begin
                LogError(('ERROR: count of 1 is invalid in pit mod_e 2.'));
                end;
              if (thisctr.OUTpin = 0) then
                begin
                set_OUT(thisctr, 1);
                end;
              if (thisctr.write_state = MSByte_multiple) then
                begin
                LogError(('Undefined behavior when loading a half loaded count.'));
                end;
              thisctr.first_pass := 0;
              end
            else
              begin
              if boolean(thisctr.GATE) then
                begin
                decrement(thisctr);
                thisctr.next_change_time := (thisctr.count_binary - 1) and $FFFF;
                if (thisctr.Count = 1) then
                  begin
                  thisctr.next_change_time := 1;
                  set_OUT(thisctr, 0);
                  thisctr.first_pass := 1;
                  end;
                end
              else
                begin
                thisctr.next_change_time := 0;
                end;
              end;
            end
          else
            begin
            thisctr.next_change_time := 0;
            end;
          thisctr.triggerGATE := 0;
          end;
        3:
          begin
          if boolean(thisctr.count_written) then
            begin
            if (((thisctr.triggerGATE <> 0) or (thisctr.first_pass <> 0) or
              (thisctr.state_bit_2 <> 0)) and (thisctr.GATE <> 0)) then
              begin
              set_count(thisctr, thisctr.inlatch and $FFFE);
              thisctr.state_bit_1 := thisctr.inlatch and $1;
              if ((thisctr.OUTpin = 0) or ((thisctr.state_bit_1 = 0))) then
                begin
                if ((Trunc(thisctr.count_binary / 2) - 1) = 0) then
                  begin
                  thisctr.next_change_time := 1;
                  end
                else
                  begin
                  thisctr.next_change_time :=
                    Trunc(((thisctr.count_binary / 2) - 1)) and $FFFF;
                  end;
                end
              else
                begin
                if (Trunc(thisctr.count_binary / 2) = 0) then
                  begin
                  thisctr.next_change_time := 1;
                  end
                else
                  begin
                  thisctr.next_change_time :=
                    Trunc((thisctr.count_binary / 2)) and $FFFF;
                  end;
                end;
              thisctr.null_count := 0;
              if (thisctr.inlatch = 1) then
                begin
                LogError(('Count of 1 is invalid in pit mod_e 3.'));
                end;
              if (thisctr.OUTpin = 0) then
                begin
                set_OUT(thisctr, 1);
                end
              else
                if ((thisctr.OUTpin <> 0) and (thisctr.first_pass = 0)) then
                  begin
                  set_OUT(thisctr, 0);
                  end;
              if (thisctr.write_state = MSByte_multiple) then
                begin
                LogError(('Undefined behavior when loading a half loaded count.'));
                end;
              thisctr.state_bit_2 := 0;
              thisctr.first_pass  := 0;
              end
            else
              begin
              if (thisctr.GATE <> 0) then
                begin
                decrement(thisctr);
                decrement(thisctr);
                if ((thisctr.OUTpin = 0) or (thisctr.state_bit_1 = 0)) then
                  begin
                  thisctr.next_change_time :=
                    Trunc((thisctr.count_binary / 2) - 1) and $FFFF;
                  end
                else
                  begin
                  thisctr.next_change_time :=
                    Trunc((thisctr.count_binary / 2)) and $FFFF;
                  end;
                if (thisctr.Count = 0) then
                  begin
                  thisctr.state_bit_2 := 1;
                  thisctr.next_change_time := 1;
                  end;
                if ((thisctr.Count = 2) and ((thisctr.OUTpin = 0) or
                  ((thisctr.state_bit_1) = 0))) then
                  begin
                  thisctr.state_bit_2 := 1;
                  thisctr.next_change_time := 1;
                  end;
                end
              else
                begin
                thisctr.next_change_time := 0;
                end;
              end;
            end
          else
            begin
            thisctr.next_change_time := 0;
            end;
          thisctr.triggerGATE := 0;
          end;
        4:
          begin
          if (thisctr.count_written <> 0) then
            begin
            if (thisctr.OUTpin = 0) then
              begin
              set_OUT(thisctr, 1);
              end;
            if (thisctr.null_count <> 0) then
              begin
              set_count(thisctr, thisctr.inlatch);
              if (thisctr.GATE <> 0) then
                begin
                if (thisctr.count_binary = 0) then
                  begin
                  thisctr.next_change_time := 1;
                  end
                else
                  begin
                  thisctr.next_change_time := thisctr.count_binary and $FFFF;
                  end;
                end
              else
                begin
                thisctr.next_change_time := 0;
                end;
              thisctr.null_count := 0;
              if (thisctr.write_state = MSByte_multiple) then
                begin
                LogError(('Undefined behavior when loading a half loaded count.'));
                end;
              thisctr.first_pass := 1;
              end
            else
              begin
              if boolean(thisctr.GATE) then
                begin
                decrement(thisctr);
                if boolean(thisctr.first_pass) then
                  begin
                  thisctr.next_change_time := thisctr.count_binary and $FFFF;
                  if boolean(thisctr.Count = 0) then
                    begin
                    set_OUT(thisctr, 0);
                    thisctr.next_change_time := 1;
                    thisctr.first_pass := 0;
                    end;
                  end
                else
                  begin
                  thisctr.next_change_time := 0;
                  end;
                end
              else
                begin
                thisctr.next_change_time := 0;
                end;
              end;
            end
          else
            begin
            thisctr.next_change_time := 0;
            end;
          thisctr.triggerGATE := 0;
          end;
        5:
          begin
          if boolean(thisctr.count_written) then
            begin
            if boolean(thisctr.OUTpin = 0) then
              begin
              set_OUT(thisctr, 1);
              end;
            if boolean(thisctr.triggerGATE) then
              begin
              set_count(thisctr, thisctr.inlatch);
              if boolean(thisctr.count_binary = 0) then
                begin
                thisctr.next_change_time := 1;
                end
              else
                begin
                thisctr.next_change_time := thisctr.count_binary and $FFFF;
                end;
              thisctr.null_count := 0;
              if boolean(thisctr.write_state = MSByte_multiple) then
                begin
                LogError(('Undefined behavior when loading a half loaded count.'));
                end;
              thisctr.first_pass := 1;
              end
            else
              begin
              decrement(thisctr);
              if boolean(thisctr.first_pass) then
                begin
                thisctr.next_change_time := thisctr.count_binary and $FFFF;
                if boolean(thisctr.Count = 0) then
                  begin
                  set_OUT(thisctr, 0);
                  thisctr.next_change_time := 1;
                  thisctr.first_pass := 0;
                  end;
                end
              else
                begin
                thisctr.next_change_time := 0;
                end;
              end;
            end
          else
            begin
            thisctr.next_change_time := 0;
            end;
          thisctr.triggerGATE := 0;
          end;
        else
          begin
          LogError(('mod_e not implemented.'));
          thisctr.next_change_time := 0;
          thisctr.triggerGATE := 0;
          end;
        end;
      end;
  end;

procedure pit_82C54.clock_all (cycles: Bit32u);
  begin
//    BX_DEBUG(Format('clock_all:  cycles:=%d', [cycles]));
    clock_multiple(0, cycles);
    clock_multiple(1, cycles);
    clock_multiple(2, cycles);
  end;

function pit_82C54.Read (address: Bit8u): Bit8u;
  var
    thisctr: pcounter_type;
  begin
    if boolean(address > MAX_ADDRESS) then
      begin
      LogError(('Counter address incorrect in data read.'));
      end
    else
      if boolean(address = CONTROL_ADDRESS) then
        begin
//        BX_DEBUG(('PIT Read: Control Word Register.'));
        //Read from control word register;
      (* This might be okay.  If so, 0 seems the most logical
       *  return value from looking at the docs.
       *)
        LogError(('Read from control word register not defined.'));
        Result := 0;
        exit;
        end
      else
        begin
        //Read from a counter;
//        BX_DEBUG(Format('PIT Read: Counter %d.', [address]));
        thisctr := @counter[address];
        if boolean(thisctr.status_latched) then
          begin
          //Latched Status Read;
          if boolean((thisctr.count_MSB_latched <> 0) and
            (thisctr.read_state = MSByte_multiple)) then
            begin
            LogError(('Undefined output when status latched and count half read.'));
            end
          else
            begin
            thisctr.status_latched := 0;
            Result := thisctr.status_latch;
            Exit;
            end;
          end
        else
          begin
          //Latched Count Read;
          if boolean(thisctr.count_LSB_latched) then
            begin
            //Read Least Significant Byte;
            if boolean(thisctr.read_state = LSByte_multiple) then
              begin
//              BX_DEBUG(('Setting read_state to MSB_mult'));
              thisctr.read_state := MSByte_multiple;
              end;
            thisctr.count_LSB_latched := 0;
            Result := (thisctr.outlatch and $FF);
            Exit;
            end
          else
            if boolean(thisctr.count_MSB_latched) then
              begin
              //Read Most Significant Byte;
              if boolean(thisctr.read_state = MSByte_multiple) then
                begin
//                BX_DEBUG(('Setting read_state to LSB_mult'));
                thisctr.read_state := LSByte_multiple;
                end;
              thisctr.count_MSB_latched := 0;
              Result := ((thisctr.outlatch shr 8) and $FF);
              Exit;
              end
            else
              begin
              //Unlatched Count Read;
              if (thisctr.read_state and $1) = 0 then
                begin
                //Read Least Significant Byte;
                if boolean(thisctr.read_state = LSByte_multiple) then
                  begin
                  thisctr.read_state := MSByte_multiple;
//                  BX_DEBUG(('Setting read_state to MSB_mult'));
                  end;
                Result := (thisctr.Count and $FF);
                Exit;
                end
              else
                begin
                //Read Most Significant Byte;
                if boolean(thisctr.read_state = MSByte_multiple) then
                  begin
//                  BX_DEBUG(('Setting read_state to LSB_mult'));
                  thisctr.read_state := LSByte_multiple;
                  end;
                Result := ((thisctr.Count shr 8) and $FF);
                Exit;
                end;
              end;
          end;
        end;
    //Should only get here on errors;
    Result := 0;
    Exit;
  end;

procedure pit_82C54.Write (address: Bit8u; Data: Bit8u);
  var
    SC, RW, M, BCD: Bit8u;
    I: integer;
    thisctr: pcounter_type;
  begin
    if boolean(address > MAX_ADDRESS) then
      begin
      LogError(('Counter address incorrect in data write.'));
      end
    else
      if boolean(address = CONTROL_ADDRESS) then
        begin
        controlword := Data;
//        BX_DEBUG(('Control Word Write.'));
        SC := (controlword shr 6) and $3;
        RW := (controlword shr 4) and $3;
        M  := (controlword shr 1) and $7;
        BCD := controlword and $1;
        if boolean(SC = 3) then
          begin
          //READ_BACK command;
//          BX_DEBUG(('READ_BACK command.'));
          I := 0;
          while i < MAX_COUNTER do
            begin
            if boolean((M shr i) and $1) then
              begin
              //If we are using this counter;
              thisctr := @counter[i];
              if boolean(((controlword shr 5) and 1) = 0) then
                begin
                //Latch Count;
                latch_counter(thisctr);
                end;
              if boolean(((controlword shr 4) and 1) = 0) then
                begin
                //Latch Status;
                if boolean(thisctr.status_latched) then
                  begin
                  //Do nothing because latched status has not been read.;
                  end
                else
                  begin
                  thisctr.status_latch :=
                    ((thisctr.OUTpin and $1) shl 7) or
                    ((thisctr.null_count and $1) shl 6) or
                    ((thisctr.rw_mode and $3) shl 4) or
                    ((thisctr.mode and $7) shl 1) or (thisctr.bcd_mode and $1);
                  thisctr.status_latched := 1;
                  end;
                end;
              end;
            Inc(i);
            end;
          end
        else
          begin
          thisctr := @counter[SC];
          if boolean(RW = 0) then
            begin
            //Counter Latch command;
//            BX_DEBUG(Format('Counter Latch command.  SC:=%d', [SC]));
            latch_counter(thisctr);
            end
          else
            begin
            //Counter Program Command;
//            BX_DEBUG(Format('Counter Program command.  SC:=%d, RW:=%d, M:=%d, BCD:=%d',
//              [SC, RW, M, BCD]));
            thisctr.null_count := 1;
            thisctr.count_LSB_latched := 0;
            thisctr.count_MSB_latched := 0;
            thisctr.status_latched := 0;
            thisctr.inlatch := 0;
            thisctr.count_written := 0;
            thisctr.first_pass := 1;
            thisctr.rw_mode := RW;
            thisctr.bcd_mode := BCD;
            thisctr.mode := M;
            case (RW) of
              $1:
                begin
//                BX_DEBUG(('Setting read_state to LSB'));
                thisctr.read_state  := LSByte;
                thisctr.write_state := LSByte;
                end;
              $2:
                begin
//                BX_DEBUG(('Setting read_state to MSB'));
                thisctr.read_state  := MSByte;
                thisctr.write_state := MSByte;
                end;
              $3:
                begin
//                BX_DEBUG(('Setting read_state to LSB_mult'));
                thisctr.read_state  := LSByte_multiple;
                thisctr.write_state := LSByte_multiple;
                end;
              else
                LogError(('RW field invalid in control word write.'));
              end;
            //All mod_es except mod_e 0 have initial output of 1.;
            if boolean(M) then
              begin
              set_OUT(thisctr, 1);
              end
            else
              begin
              set_OUT(thisctr, 0);
              end;
            thisctr.next_change_time := 0;
            end;
          end;
        end
      else
        begin
        //Write to counter initial value.
        thisctr := @counter[address];
//        BX_DEBUG(Format('Write Initial Count: counter:=%d, count:=%d',
//          [address, Data]));
        case (thisctr.write_state) of
          LSByte_multiple:
            begin
            thisctr.inlatch := (thisctr.inlatch and ($FF shl 8)) or Data;
            thisctr.write_state := MSByte_multiple;
            end;
          LSByte:
            begin
            thisctr.inlatch := (thisctr.inlatch and ($FF shl 8)) or Data;
            thisctr.null_count := 1;
            thisctr.count_written := 1;
            end;
          MSByte_multiple:
            begin
            thisctr.write_state := LSByte_multiple;
            thisctr.inlatch := (thisctr.inlatch and $FF) or (Data shl 8);
            thisctr.null_count := 1;
            thisctr.count_written := 1;
            end;
          MSByte: //shared between MSB_multiple and MSByte
            begin
            thisctr.inlatch := (thisctr.inlatch and $FF) or (Data shl 8);
            thisctr.null_count := 1;
            thisctr.count_written := 1;
            end;
          else
            LogError(('write counter in invalid write state.'));
          end;
        case (thisctr.mode) of
          0:
            begin
            if boolean(thisctr.write_state = MSByte_multiple) then
              begin
              set_OUT(thisctr, 0);
              end;
            thisctr.next_change_time := 1;
            end;
          1:
            begin
            if boolean(thisctr.triggerGATE) then
              begin //for initial writes, if already saw trigger.
              thisctr.next_change_time := 1;
              end; //Otherwise, no change.
            end;
          6,
          2:
            begin
            thisctr.next_change_time := 1; //FIXME: this could be loosened.
            end;
          7,
          3:
            begin
            thisctr.next_change_time := 1; //FIXME: this could be loosened.
            end;
          4:
            begin
            thisctr.next_change_time := 1;
            end;
          5:
            begin
            if boolean(thisctr.triggerGATE) then
              begin //for initial writes, if already saw trigger.
              thisctr.next_change_time := 1;
              end; //Otherwise, no change.
            end;
          end;
        end;
  end;

procedure pit_82C54.set_GATE (cnum: Bit8u; Data: bool);
  var
    thisctr:  pcounter_type;
    uno, due: boolean;
  begin
    if boolean(cnum > MAX_COUNTER) then
      begin
      LogError(('Counter number incorrect in 82C54 set_GATE'));
      end
    else
      begin
      thisctr := @counter[cnum];
      uno := ((thisctr.GATE <> 0) and (Data <> 0));
      due := ((thisctr.GATE <> 0) or (Data <> 0));
      if ((uno = True) or (due = False)) then
        begin
        LogInfo(Format('Changing GATE %d to: %d', [cnum, Data]));
        thisctr.GATE := Data;
        if boolean(thisctr.GATE) then
          begin
          thisctr.triggerGATE := 1;
          end;
        case (thisctr.mode) of
          0:
            begin
            if boolean((Data <> 0) and (thisctr.count_written <> 0)) then
              begin
              if boolean(thisctr.null_count) then
                begin
                thisctr.next_change_time := 1;
                end
              else
                begin
                if boolean((thisctr.OUTpin = 0) and
                  (thisctr.write_state <> MSByte_multiple)) then
                  begin
                  if boolean(thisctr.count_binary = 0) then
                    begin
                    thisctr.next_change_time := 1;
                    end
                  else
                    begin
                    thisctr.next_change_time := thisctr.count_binary and $FFFF;
                    end;
                  end
                else
                  begin
                  thisctr.next_change_time := 0;
                  end;
                end;
              end
            else
              begin
              if boolean(thisctr.null_count) then
                begin
                thisctr.next_change_time := 1;
                end
              else
                begin
                thisctr.next_change_time := 0;
                end;
              end;
            end;
          1:
            begin
            if boolean((Data <> 0) and (thisctr.count_written <> 0)) then
              begin //only triggers cause a change.
              thisctr.next_change_time := 1;
              end;
            end;
          2:
            begin
            if boolean(Data = 0) then
              begin
              set_OUT(thisctr, 1);
              thisctr.next_change_time := 0;
              end
            else
              begin
              if boolean(thisctr.count_written) then
                begin
                thisctr.next_change_time := 1;
                end
              else
                begin
                thisctr.next_change_time := 0;
                end;
              end;
            end;
          3:
            begin
            if boolean(Data = 0) then
              begin
              set_OUT(thisctr, 1);
              thisctr.first_pass := 1;
              thisctr.next_change_time := 0;
              end
            else
              begin
              if boolean(thisctr.count_written) then
                begin
                thisctr.next_change_time := 1;
                end
              else
                begin
                thisctr.next_change_time := 0;
                end;
              end;
            end;
          4:
            begin
            if boolean((thisctr.OUTpin = 0) or (thisctr.null_count <> 0)) then
              begin
              thisctr.next_change_time := 1;
              end
            else
              begin
              if boolean((Data <> 0) and (thisctr.count_written <> 0)) then
                begin
                if boolean(thisctr.first_pass) then
                  begin
                  if boolean(thisctr.count_binary = 0) then
                    begin
                    thisctr.next_change_time := 1;
                    end
                  else
                    begin
                    thisctr.next_change_time := thisctr.count_binary and $FFFF;
                    end;
                  end
                else
                  begin
                  thisctr.next_change_time := 0;
                  end;
                end
              else
                begin
                thisctr.next_change_time := 0;
                end;
              end;
            end;
          5:
            begin
            if boolean((Data <> 0) and (thisctr.count_written <> 0)) then
              begin //only triggers cause a change.
              thisctr.next_change_time := 1;
              end;
            end;
          else
            exit;
          end;
        end;
      end;
  end;

function pit_82C54.read_OUT (cnum: Bit8u): bool;
  begin
    if boolean(cnum > MAX_COUNTER) then
      begin
      LogError(('Counter number incorrect in 82C54 read_OUT'));
      Result := 0;
      end
    else
      begin
      Result := counter[cnum].OUTpin;
      end;
  end;

function pit_82C54.read_GATE (cnum: Bit8u): bool;
  begin
    if boolean(cnum > MAX_COUNTER) then
      begin
      LogError(('Counter number incorrect in 82C54 read_GATE'));
      Result := 0;
      Exit;
      end
    else
      begin
      Result := counter[cnum].GATE;
      Exit;
      end;
  end;

function pit_82C54.get_clock_event_time (cnum: Bit8u): Bit32u;
  begin
    if boolean(cnum > MAX_COUNTER) then
      begin
      LogError(('Counter number incorrect in 82C54 read_GATE'));
      Result := 0;
      Exit;
      end
    else
      begin
      Result := counter[cnum].next_change_time;
      Exit;
      end;
  end;

function pit_82C54.get_next_event_time: Bit32u;
  var
    out_:  Bit32u;
    time0: Bit32u;
    time1: Bit32u;
    time2: Bit32u;
  begin
    time0 := get_clock_event_time(0);
    time1 := get_clock_event_time(1);
    time2 := get_clock_event_time(2);

    out_ := time0;
    if boolean((time1 <> 0) and (time1 < out_)) then
      out_ := time1;
    if boolean((time2 <> 0) and (time2 < out_)) then
      out_ := time2;
    Result := out_;
  end;

end.
