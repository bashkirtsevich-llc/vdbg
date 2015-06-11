unit PIC;

interface

uses iodev, Config, Service, SysUtils;

type
  bx_pic_t1 = record
    single_PIC:Bit8u;        (* 0=cascaded PIC, 1=master only *)
    Interrupt_offset:Bit8u;  (* programmable interrupt vector offset *)
    case u:integer of
      0:
        (
          slave_connect_mask:Bit8u; (* for master, a bit for each interrupt line
                                     0=not connect to a slave, 1=connected *)
          slave_id:Bit8u;           (* for slave, id number of slave PIC *)
        );
  end;
  bx_pic_t2 = record
    Part1:bx_pic_t1;
    sfnm:Bit8u;              (* specially fully nested mode: 0=no, 1=yes*)
    buffered_mode:Bit8u;     (* 0=no buffered mode, 1=buffered mode *)
    master_slave:Bit8u ;      (* master/slave: 0=slave PIC, 1=master PIC *)
    auto_eoi:Bit8u;          (* 0=manual EOI, 1=automatic EOI *)
    imr:Bit8u;               (* interrupt mask register, 1=masked *)
    isr:Bit8u;               (* in service register *)
    irr:Bit8u;               (* interrupt request register *)
    read_reg_select:Bit8u;   (* 0=IRR, 1=ISR *)
    irq:Bit8u;               (* current IRQ number *)
    lowest_priority:Bit8u;   (* current lowest priority irq *)
    INT:Bool;             (* INT request pin of PIC *)
    IRQ_line:array[0..8] of Bool;     (* IRQ pins of PIC *)
    init:record
      in_init:Bool;
      requires_4:Bool;
      byte_expected:Integer;
    end;
    special_mask:Bool;
    polled:Bool;            (* Set when poll command is issued. *)
    rotate_on_autoeoi:Bool; (* Set when should rotate in auto-eoi mode. *)
  end;

  pbx_pic_t = ^bx_pic_t;
  bx_pic_t = record
    a:bx_pic_t1;
    b:bx_pic_t2;
  end;
  bx_pic_c = class

  public
    s:record
      master_pic:bx_pic_t;
      slave_pic:bx_pic_t;
    end;

    devices:pbx_devices_c;
    constructor Create;
    Destructor Destroy; override;
    procedure init(d:Pbx_devices_c);
    procedure lower_irq(irq_no:unsigned);
    procedure raise_irq(irq_no:unsigned);
    function IAC:Bit8u;


    function read_handler(this_ptr:Pointer; address:Bit32u; io_len:unsigned):Bit32u;
    procedure write_handler(this_ptr:Pointer; address:Bit32u; value:Bit32u; io_len:unsigned);
  {$if BX_USE_PIC_SMF=0}
    function read(address:Bit32u; io_len:unsigned):Bit32u;
    procedure write(address:Bit32u; value:Bit32u; io_len:unsigned);
  {$ifend}

    procedure service_master_pic;
    procedure service_slave_pic;
    procedure show_pic_state;
    procedure clear_highest_interrupt(pic:pbx_pic_t);
  end;

var
  bx_pic:bx_pic_c;
implementation

uses cpu;

constructor bx_pic_c.Create;
begin
  inherited;
end;

destructor bx_pic_c.Destroy;
begin
  // nothing for now
end;

procedure bx_pic_c.init(d:pbx_devices_c);
var
  I:Integer;
begin
  Self.devices := d;

  (* 8259 PIC (Programmable Interrupt Controller) *)
  Self.devices^.register_io_read_handler(Self, read_handler, $0020, '8259 PIC');
  Self.devices^.register_io_read_handler(Self, read_handler, $0021, '8259 PIC');
  Self.devices^.register_io_read_handler(Self, read_handler, $00A0, '8259 PIC');
  Self.devices^.register_io_read_handler(Self, read_handler, $00A1, '8259 PIC');

  Self.devices^.register_io_write_handler(Self, write_handler, $0020, '8259 PIC');
  Self.devices^.register_io_write_handler(Self, write_handler, $0021, '8259 PIC');
  Self.devices^.register_io_write_handler(Self, write_handler, $00A0, '8259 PIC');
  Self.devices^.register_io_write_handler(Self, write_handler, $00A1, '8259 PIC');


  Self.s.master_pic.a.single_PIC := 0;
  Self.s.master_pic.a.interrupt_offset := $08; (* IRQ0 := INT $08 *)
  (* slave PIC connected to IRQ2 of master *)
  Self.s.master_pic.a.slave_connect_mask := $04;
  Self.s.master_pic.b.sfnm := 0; (* normal nested mod_e *)
  Self.s.master_pic.b.buffered_mode := 0; (* unbuffered mod_e *)
  Self.s.master_pic.b.master_slave  := 0; (* no meaning, buffered_mod_e:=0 *)
  Self.s.master_pic.b.auto_eoi      := 0; (* manual EOI from CPU *)
  Self.s.master_pic.b.imr           := $FF; (* all IRQ's initially masked *)
  Self.s.master_pic.b.isr           := $00; (* no IRQ's in service *)
  Self.s.master_pic.b.irr           := $00; (* no IRQ's requested *)
  Self.s.master_pic.b.read_reg_select := 0; (* IRR *)
  Self.s.master_pic.b.irq := 0;
  Self.s.master_pic.b.INT := 0;
  Self.s.master_pic.b.init.in_init := 0;
  Self.s.master_pic.b.init.requires_4 := 0;
  Self.s.master_pic.b.init.byte_expected := 0;
  Self.s.master_pic.b.special_mask := 0;
  Self.s.master_pic.b.lowest_priority := 7;
  Self.s.master_pic.b.polled := 0;
  Self.s.master_pic.b.rotate_on_autoeoi := 0;

  Self.s.slave_pic.a.single_PIC := 0;
  Self.s.slave_pic.a.interrupt_offset := $70; (* IRQ8 := INT $70 *)
  Self.s.slave_pic.a.slave_id := $02; (* slave PIC connected to IRQ2 of master *)
  Self.s.slave_pic.b.sfnm       := 0; (* normal nested mod_e *)
  Self.s.slave_pic.b.buffered_mode := 0; (* unbuffered mod_e *)
  Self.s.slave_pic.b.master_slave  := 0; (* no meaning, buffered_mod_e:=0 *)
  Self.s.slave_pic.b.auto_eoi      := 0; (* manual EOI from CPU *)
  Self.s.slave_pic.b.imr           := $FF; (* all IRQ's initially masked *)
  Self.s.slave_pic.b.isr           := $00; (* no IRQ's in service *)
  Self.s.slave_pic.b.irr           := $00; (* no IRQ's requested *)
  Self.s.slave_pic.b.read_reg_select := 0; (* IRR *)
  Self.s.slave_pic.b.irq := 0;
  Self.s.slave_pic.b.INT := 0;
  Self.s.slave_pic.b.init.in_init := 0;
  Self.s.slave_pic.b.init.requires_4 := 0;
  Self.s.slave_pic.b.init.byte_expected := 0;
  Self.s.slave_pic.b.special_mask := 0;
  Self.s.slave_pic.b.lowest_priority := 7;
  Self.s.slave_pic.b.polled := 0;
  Self.s.slave_pic.b.rotate_on_autoeoi := 0;

  for i:=0 to 8 do begin (* all IRQ lines low *)
    Self.s.master_pic.b.IRQ_line[i] := 0;
    Self.s.slave_pic.b.IRQ_line[i] := 0;
  end;
end;



  // static IO port read callback handler
  // redirects to non-static class handler to aprocedure virtual functions

function bx_pic_c.read_handler(this_ptr:Pointer; address:Bit32u; io_len:unsigned):Bit32u;
begin
  if (io_len > 1) then
    LogPanic(Format('io read from port %04x, len:=%u',[address, io_len]));

//  BX_DEBUG(Format('PIC MODULE : IO read from %04x',[address]));

  (*
   8259A PIC
   *)

  if(((address = $20) or (address = $21)) and (Self.s.master_pic.b.polled<>0)) then begin
    // In polled mod_e. Treat Self as an interrupt acknowledge
    clear_highest_interrupt(@Self.s.master_pic);
    Self.s.master_pic.b.polled := 0;
    service_master_pic();
    result:= Self.s.master_pic.b.irq;  // Return the current irq requested
    exit;
  end;

  if(((address = $a0) or (address = $a1)) and (Self.s.slave_pic.b.polled<>0)) then begin
    // In polled mod_e. Treat Self as an interrupt acknowledge
    clear_highest_interrupt(@Self.s.slave_pic);
    Self.s.slave_pic.b.polled := 0;
    service_slave_pic();
    Result:= Self.s.slave_pic.b.irq;  // Return the current irq requested
    exit;
  end;


  case address of
    $20:
      begin
        if (Self.s.master_pic.b.read_reg_select)<>0 then begin (* ISR *)
          LogInfo(Format('read master ISR := %02x',[Self.s.master_pic.b.isr]));
      	Result:=Self.s.master_pic.b.isr;
        exit;
	    end
      else begin (* IRR *)
        LogInfo(Format('read master IRR := %02x',[Self.s.master_pic.b.irr]));
      	Result:=(Self.s.master_pic.b.irr);
        exit;
    	end;
    end;
    $21:
      begin
        LogInfo(Format('read master IMR := %02x',[Self.s.master_pic.b.imr]));
        Result:=(Self.s.master_pic.b.imr);
        exit;
      end;
    $A0:
      begin
      if (Self.s.slave_pic.b.read_reg_select)<>0 then begin (* ISR *)
        LogInfo(Format('read slave ISR := %02x', [Self.s.slave_pic.b.isr]));
    	Result:=(Self.s.slave_pic.b.isr);
      exit;
  	end
      else begin (* IRR *)
        LogInfo(Format('read slave IRR := %02x', [Self.s.slave_pic.b.irr]));
      	Result:=(Self.s.slave_pic.b.irr);
        exit;
    	end;
      end;
    $A1:
      begin
        LogInfo(format('read slave IMR := %02x',[Self.s.slave_pic.b.imr]));
        Result:=(Self.s.slave_pic.b.imr);
        exit;
      end;
    end;

  LogPanic(Format('io read to address %04x',[address]));
  Result:=(0); (* default if not found above *)
end;


  // static IO port write callback handler
  // redirects to non-static class handler to aprocedure virtual functions

procedure bx_pic_c.write_handler(this_ptr:Pointer; address:Bit32u; value:Bit32u; io_len:unsigned);
var
  special_mask, poll, read_op:Bit8u;
begin
{$if BX_USE_PIC_SMF=0}
  bx_pic_c *class_ptr := (bx_pic_c *) this_ptr;

  class_ptr^.write(address, value, io_len);
end;

procedure bx_pic_c.write(Bit32u address, Bit32u value, unsigned io_len)
var
  special_mask, poll, read_op:Bit8u;
begin
{$else}
  //UNUSED(this_ptr);
{$ifend} // !BX_USE_PIC_SMF

  if (io_len > 1) then
    LogPanic(format('io write to port %04x, len:=%u',[address, io_len]));

    if (1=2) then
      LogInfo(format('IO write to %04x := %02x',[address, value]));

  (*
   8259A PIC
   *)

  case address of
    $20:
      begin
        if (value and $10)<>0 then
          begin (* initialization command 1 *)
        // (mch) Ignore...
        // BX_INFO(('pic:master: init command 1 found %02x', (unsigned) value));
            if BX_PIC_DEBUG = 1 then begin
              LogInfo(('master: init command 1 found'));
              LogInfo(Format('        requires 4 := %u',[(value and $01)]));
              LogInfo(Format('        cascade mod_e: [0:=cascade,1:=single] %u',[((value  and $02) shr 1)]));
            end;
            Self.s.master_pic.b.init.in_init := 1;
            Self.s.master_pic.b.init.requires_4 := (value  and $01);
            Self.s.master_pic.b.init.byte_expected := 2; (* operation command 2 *)
            Self.s.master_pic.b.imr           := $FF; (* all IRQ's initially masked *)
            Self.s.master_pic.b.isr           := $00; (* no IRQ's in service *)
            Self.s.master_pic.b.irr           := $00; (* no IRQ's requested *)
            Self.s.master_pic.b.lowest_priority := 7;
            Self.s.master_pic.b.INT := 0; (* reprogramming clears previous INTR request *)
            Self.s.master_pic.b.auto_eoi := 0;
            Self.s.master_pic.b.rotate_on_autoeoi := 0;
            if ( (value  and $02) = 1 ) then
              LogPanic(('master: ICW1: single mod_e not supported'));
            if ( (value  and $08) = 1 ) then begin
              LogPanic(('master: ICW1: level sensitive mod_e not supported'));
            end               {

                             else begin
                               BX_DEBUG(('master: ICW1: edge triggered mod_e selected'));
                             end
               };
          bx_pc_system.set_intr(0);
          exit;
        end;

      if ( (value  and $18) = $08 ) then begin (* OCW3 *)

        special_mask := (value  and $60) shr 5;
        poll         := (value  and $04) shr 2;
        read_op      := (value  and $03);
        if (poll)<>0 then begin
          Self.s.master_pic.b.polled := 1;
          exit;
        end;
        if (read_op = $02) then (* read IRR *)
      	  Self.s.master_pic.b.read_reg_select := 0
        else if (read_op = $03) then (* read ISR *)
            	  Self.s.master_pic.b.read_reg_select := 1;
        if (special_mask = $02) then begin (* cancel special mask *)
          Self.s.master_pic.b.special_mask := 0;
          end
        else if (special_mask = $03) then begin (* set specific mask *)
          Self.s.master_pic.b.special_mask := 1;
          service_master_pic();
          end;
        exit;
      end;

      (* OCW2 *)
      if (Value =$00) or (Value =$80) then
          begin
            Self.s.master_pic.b.rotate_on_autoeoi := Bool(value <> 0);
            exit;
          end;
    	if (Value = $0A) then  (* select read interrupt request register *)
        begin
      	  Self.s.master_pic.b.read_reg_select := 0;
            exit;
        end;
    	if (Value = $0B) then (* select read interrupt in-service register *)
        begin
      	  Self.s.master_pic.b.read_reg_select := 1;
            exit;
      	end;
      if (Value = $A0) or (Value = $20) then
        begin
          clear_highest_interrupt(@Self.s.master_pic);

          if(value = $A0) then begin// Rotate in Auto-EOI mod_e
            Self.s.master_pic.b.lowest_priority:=Self.s.master_pic.b.lowest_priority+1;
            if(Self.s.master_pic.b.lowest_priority > 7) then
              Self.s.master_pic.b.lowest_priority := 0;
          end;

          service_master_pic();
            exit;
        end;

      if (Value = $40) then // Intel PIC spec-sheet seems to indicate Self should be ignored
          begin
            LogInfo(('IRQ no-op'));
            exit;
          end;
      if (Value in [$60..$67]) then
          begin
            //Self.s.master_pic.isr @:= ~(1 shl (value-$60));
            Self.s.master_pic.b.isr := Self.s.master_pic.b.isr and not (1 shl (value-$60));
            service_master_pic();
            exit;
          end;
      if (Value in [$C0..$C7]) then
          begin
            LogInfo(Format('IRQ lowest command $%x',[value]));
            Self.s.master_pic.b.lowest_priority := value - $C0;
            exit;
          end;
      if (Value in [$E0..$E7]) then
          begin
            //Self.s.master_pic.isr @:= ~(1 shl (value-$E0));
            Self.s.master_pic.b.isr :=Self.s.master_pic.b.isr and not (1 shl (value-$E0));
            Self.s.master_pic.b.lowest_priority := (value - $E0);
            service_master_pic();
            exit;
          end
        else
          begin
            LogPanic(Format('write to port 20h := %02x',[value]));
          end;
    	end; (* switch (value) *)

    $21:
      begin
      (* initialization mod_e operation *)
      if (Self.s.master_pic.b.init.in_init)<>0 then begin
        if Self.s.master_pic.b.init.byte_expected = 2 then
            begin
              Self.s.master_pic.a.interrupt_offset := value and $f8;
              Self.s.master_pic.b.init.byte_expected := 3;
        	    if (BX_PIC_DEBUG)<>0 then begin
          		  LogInfo(Format('master: init command 2 := %02x',[value]));
          		  LogInfo(Format('        offset := INT %02x',[Self.s.master_pic.a.interrupt_offset]));
        	    end;
              exit;
            end;
      if Self.s.master_pic.b.init.byte_expected = 3 then
        begin
    	    if (BX_PIC_DEBUG)<>0 then
  		      LogInfo(Format('master: init command 3 := %02x',[value]));
              if (Self.s.master_pic.b.init.requires_4)<>0 then begin
                Self.s.master_pic.b.init.byte_expected := 4;
    	        end
              else begin
              Self.s.master_pic.b.init.in_init := 0;
    	        end;
          exit;
       end;
      if Self.s.master_pic.b.init.byte_expected = 4 then
          begin
//            BX_DEBUG(Format('master: init command 4 := %02x',[value]));
            if (value and $02)<>0 then begin
//              BX_DEBUG(('       auto EOI'));
              Self.s.master_pic.b.auto_eoi := 1;
            end
            else begin
//              BX_DEBUG(('normal EOI interrupt'));
              Self.s.master_pic.b.auto_eoi := 0;
            end;
    	      if (value and $01)=0 then
          		  LogPanic(('       not 8$86 mod_e'));
            Self.s.master_pic.b.init.in_init := 0;
            exit;
            end;
       if (Self.s.master_pic.b.init.byte_expected <> 2) and
           (Self.s.master_pic.b.init.byte_expected <> 3) and
           (Self.s.master_pic.b.init.byte_expected <> 4) then
            begin
            LogPanic(('master expecting bad init command'));
          end;
      end;
      (* normal operation *)
      if (BX_PIC_DEBUG)<>0 then
        LogInfo(Format('setting master pic IMR to %02x',[value]));
      Self.s.master_pic.b.imr := value;
      service_master_pic();
      exit;
      end;

    $A0:
      begin
        if (value  and $10)<>0 then begin (* initialization command 1 *)
//        BX_DEBUG(('slave: init command 1 found'));
//        BX_DEBUG(Format('       requires 4 := %u',[(value and $01)]));
//        BX_DEBUG(Format('       cascade mod_e: [0:=cascade,1:=single] %u',
//            [((value  and $02) shr 1)]));
        Self.s.slave_pic.b.init.in_init := 1;
        Self.s.slave_pic.b.init.requires_4 := (value  and $01);
        Self.s.slave_pic.b.init.byte_expected := 2; (* operation command 2 *)
        Self.s.slave_pic.b.imr           := $FF; (* all IRQ's initially masked *)
        Self.s.slave_pic.b.isr           := $00; (* no IRQ's in service *)
        Self.s.slave_pic.b.irr           := $00; (* no IRQ's requested *)
        Self.s.slave_pic.b.lowest_priority := 7;
        Self.s.slave_pic.b.INT := 0; (* reprogramming clears previous INTR request *)
        Self.s.slave_pic.b.auto_eoi := 0;
        Self.s.slave_pic.b.rotate_on_autoeoi := 0;
        if ( (value  and $02) = 1 ) then
          LogPanic(('slave: ICW1: single mod_e not supported'));
        if ( (value  and $08) = 1 ) then begin
          LogPanic(('slave: ICW1: level sensitive mod_e not supported'));
	      end{
      	else
        begin
          BX_DEBUG(('slave: ICW1: edge triggered mod_e selected'));
  	    end};
      exit;
      end;

      if ( (value  and $18) = $08 ) then begin (* OCW3 *)

        special_mask := (value  and $60) shr 5;
        poll         := (value  and $04) shr 2;
        read_op      := (value  and $03);
        if (poll)<>0 then begin
          Self.s.slave_pic.b.polled := 1;
          exit;
        end;
        if (read_op = $02) then (* read IRR *)
      	  Self.s.slave_pic.b.read_reg_select := 0
        else if (read_op = $03) then (* read ISR *)
    	  Self.s.slave_pic.b.read_reg_select := 1;
        if (special_mask = $02) then begin (* cancel special mask *)
          Self.s.slave_pic.b.special_mask := 0;
          end
        else if (special_mask = $03) then begin (* set specific mask *)
          Self.s.slave_pic.b.special_mask := 1;
          service_slave_pic();
          end;
        exit;
        end;

      if (value=$00) or (value=$80) then
        begin
          Self.s.slave_pic.b.rotate_on_autoeoi := Bool(value <> 0);
          exit;
        end;

    	if (value=$0A) then (* select read interrupt request register *)
        begin
      	  Self.s.slave_pic.b.read_reg_select := 0;
          exit;
	      end;
    	if (value=$0B) then (* select read interrupt in-service register *)
        begin
      	  Self.s.slave_pic.b.read_reg_select := 1;
          exit;
    	  end;
      if (value=$a0) or (value=$20) then
        begin
          clear_highest_interrupt(@Self.s.slave_pic);

          if(value = $A0) then begin// Rotate in Auto-EOI mod_e
            Self.s.slave_pic.b.lowest_priority:=Self.s.slave_pic.b.lowest_priority+1;
            if(Self.s.slave_pic.b.lowest_priority > 7) then
              Self.s.slave_pic.b.lowest_priority := 0;
          end;

          service_slave_pic();
        end;

        if (Value=$40) then // Intel PIC spec-sheet seems to indicate Self should be ignored
          begin
            LogInfo(('IRQ no-op'));
            exit;
          end;

        if (value in [$60..$67]) then
          begin
            //Self.s.slave_pic.isr @:= ~(1 shl (value-$60));
            Self.s.slave_pic.b.isr := Self.s.slave_pic.b.isr and not (1 shl (value-$60));
            service_slave_pic();
            exit;
          end;

        // IRQ lowest priority commands
        if (value in [$C0..$C7]) then
          begin
            LogInfo(Format('IRQ lowest command $%x',[value]));
            Self.s.slave_pic.b.lowest_priority := value - $C0;
            exit;
          end;

        if (value in [$E0..$E7]) then
          begin
            //Self.s.slave_pic.isr @:= ~(1 shl (value-$E0));
            Self.s.slave_pic.b.isr := Self.s.slave_pic.b.isr and not (1 shl (value-$E0));
            Self.s.slave_pic.b.lowest_priority := (value - $E0);
            service_slave_pic();
            exit;
          end;

        {else
          BX_PANIC(('write to port A0h := %02x', value)); !!!}
  	end; (* switch (value) *)

    $A1:
      begin
        (* initialization mod_e operation *)
        if (Self.s.slave_pic.b.init.in_init)<>0 then begin
          if Self.s.slave_pic.b.init.byte_expected = 2 then
            begin
              Self.s.slave_pic.a.interrupt_offset := value  and $f8;
              Self.s.slave_pic.b.init.byte_expected := 3;
//	            if (BX_PIC_DEBUG)<>0 then begin
//          		  BX_DEBUG(Format('slave: init command 2 := %02x',[value]));
//          		  BX_DEBUG(Format('       offset := INT %02x',[Self.s.slave_pic.a.interrupt_offset]));
//        	    end;
            exit;
            end;
          if Self.s.slave_pic.b.init.byte_expected = 3 then
            begin
//              BX_DEBUG(Format('slave: init command 3 := %02x',[value]));
              if (Self.s.slave_pic.b.init.requires_4)<>0 then begin
                Self.s.slave_pic.b.init.byte_expected := 4;
	          	end else
                begin
                  Self.s.slave_pic.b.init.in_init := 0;
        	      end;
            exit;
            end;
          if Self.s.slave_pic.b.init.byte_expected = 4 then
            begin
//              BX_DEBUG(Format('slave: init command 4 := %02x',[value]));
              if (value  and $02)<>0 then begin
//                BX_DEBUG(('       auto EOI'));
                Self.s.slave_pic.b.auto_eoi := 1;
              end
            else begin
//              BX_DEBUG(('normal EOI interrupt'));
              Self.s.slave_pic.b.auto_eoi := 0;
              end;
      	    if (value  and $01)<>0 then begin
        		  if (BX_PIC_DEBUG)<>0 then
          			LogInfo(('       8$86 mod_e'));
        	    end else
          		  LogPanic(('       not 8$86 mod_e'));
            Self.s.slave_pic.b.init.in_init := 0;
            exit;
            end
          else
            LogPanic(('slave: expecting bad init command'));
          end;

      (* normal operation *)
      if (BX_PIC_DEBUG=1) then
        LogInfo(Format('setting slave pic IMR to %02x',[value]));
      Self.s.slave_pic.b.imr := value;
      service_slave_pic();
      exit;
      end;
    end; (* switch (address) *)

end;

// new IRQ signal handling routines

procedure bx_pic_c.lower_irq(irq_no:unsigned);
begin

  if ((irq_no <= 7) and (Self.s.master_pic.b.IRQ_line[irq_no]<>0)) then begin
//    BX_DEBUG(Format('IRQ line %d now low',[irq_no]));
    Self.s.master_pic.b.IRQ_line[irq_no] := 0;
    //Self.s.master_pic.b.irr @:= ~(1 shl irq_no);
    Self.s.master_pic.b.irr := Self.s.master_pic.b.irr and not (1 shl irq_no);
    if (Self.s.master_pic.b.irr and not Self.s.master_pic.b.imr)=0 then begin
      bx_pc_system.set_intr(0);
      Self.s.master_pic.b.INT := 0;
    end;
  end else if ((irq_no > 7) and (irq_no <= 15) and
             (Self.s.slave_pic.b.IRQ_line[irq_no-8]<>0)) then begin
//    BX_DEBUG(Format('IRQ line %d now low',[irq_no]));
    Self.s.slave_pic.b.IRQ_line[irq_no - 8] := 0;
    //Self.s.slave_pic.irr @:= ~(1 shl (irq_no - 8));
    Self.s.slave_pic.b.irr := Self.s.slave_pic.b.irr and not (1 shl (irq_no - 8));
    if (Self.s.slave_pic.b.irr and not Self.s.slave_pic.b.imr )= 0 then begin
      Self.s.slave_pic.b.INT := 0;
      lower_irq(2);
    end;
  end;
end;

procedure bx_pic_c.raise_irq(irq_no:unsigned);
begin

  if ((irq_no <= 7) and (Self.s.master_pic.b.IRQ_line[irq_no]=0)) then begin
//    BX_DEBUG(Format('IRQ line %d now high',[irq_no]));
    Self.s.master_pic.b.IRQ_line[irq_no] := 1;
    Self.s.master_pic.b.irr := Self.s.master_pic.b.irr or (1 shl irq_no);
    service_master_pic();
  end else if ((irq_no > 7) and (irq_no <= 15) and
             (Self.s.slave_pic.b.IRQ_line[irq_no-8]=0)) then begin
//    BX_DEBUG(Format('IRQ line %d now high',[irq_no]));
    Self.s.slave_pic.b.IRQ_line[irq_no - 8] := 1;
    Self.s.slave_pic.b.irr := Self.s.slave_pic.b.irr or (1 shl (irq_no - 8));
    service_slave_pic();
  end;
end;

procedure  bx_pic_c.clear_highest_interrupt(pic:pbx_pic_t);
var
  irq:Integer;
  lowest_priority:Integer;
  highest_priority:Integer;
begin

  (* clear highest current in service bit *)
  lowest_priority := pic^.b.lowest_priority;
  highest_priority := lowest_priority + 1;
  if(highest_priority > 7) then
    highest_priority := 0;

  irq := highest_priority;
  repeat
    if (pic^.b.isr  and (1 shl irq))<>0 then begin
      //pic^.b.isr @:= ~(1 shl irq);
      pic^.b.isr := pic^.b.isr and not (1 shl irq);
      break; (* Return mask of bit cleared. *)
    end;

    inc(irq);
    if(irq > 7) then
      irq := 0;
  until(irq = highest_priority);

end;

  (* *)
procedure bx_pic_c.service_master_pic;
var
  unmasked_requests:Bit8u;
  irq:Integer;
  isr, max_irq:Bit8u;
  highest_priority:Bit8u;
begin
  highest_priority:=Self.s.master_pic.b.lowest_priority + 1;
  if(highest_priority > 7) then
    highest_priority := 0;

  if (Self.s.master_pic.b.INT)<>0 then begin (* last interrupt still not acknowleged *)
    exit;
    end;

  if (Self.s.master_pic.b.special_mask)<>0 then begin
    (* all priorities may be enabled.  check all IRR bits except ones
     * which have corresponding ISR bits set
     *)
    max_irq := highest_priority;
    end
  else begin (* normal mod_e *)
    (* Find the highest priority IRQ that is enabled due to current ISR *)
    isr := Self.s.master_pic.b.isr;
    if (isr)<>0 then begin // !!!!!!!
      max_irq := highest_priority;
      while ( (isr and (1 shl max_irq)) = 0) do
        begin
        inc(max_irq);
        if(max_irq > 7) then
          max_irq := 0;
        end;
      if (max_irq = highest_priority ) then exit; (* Highest priority interrupt in-service,
                                                 * no other priorities allowed *)
      if (max_irq > 7) then LogPanic(('error in service_master_pic()'));
      end
  else
      max_irq := highest_priority; (* 0..7 bits in ISR are cleared *)
    end;


  (* now, see if there are any higher priority requests *)
  unmasked_requests := (Self.s.master_pic.b.irr and not Self.s.master_pic.b.imr);
  if (unmasked_requests)<>0 then begin
    irq := highest_priority;
    repeat
      (* for special mod_e, since we're looking at all IRQ's, skip if
       * current IRQ is already in-service
       *)
      if ( Self.s.master_pic.b.special_mask and ((Self.s.master_pic.b.isr shr irq)  and $01) )<>0 then
        continue;
      if (unmasked_requests  and (1 shl irq))<>0 then begin
//        BX_DEBUG(Format('signalling IRQ(%u)',[irq]));
        Self.s.master_pic.b.INT := 1;
        bx_pc_system.set_intr(1);
        Self.s.master_pic.b.irq := irq;
        exit;
        end; (* if Boolean(unmasked_requests  and ... *)

      inc(irq);
      if(irq > 7) then
        irq := 0;
      until(irq = max_irq); (* do ... *)
    end; (* if Boolean(unmasked_requests := ... *)
end;

procedure bx_pic_c.service_slave_pic;
var
  unmasked_requests:Bit8u;
  irq:integer;
  isr, max_irq:Bit8u;
  highest_priority:Bit8u;
begin
  highest_priority:=Self.s.slave_pic.b.lowest_priority + 1;
  if(highest_priority > 7) then
    highest_priority := 0;

  if (Self.s.slave_pic.b.INT)<>0 then begin (* last interrupt still not acknowleged *)
    exit;
    end;

  if (Self.s.slave_pic.b.special_mask)<>0 then begin
    (* all priorities may be enabled.  check all IRR bits except ones
     * which have corresponding ISR bits set
     *)
    max_irq := highest_priority;
  end
  else begin (* normal mod_e *)
    (* Find the highest priority IRQ that is enabled due to current ISR *)
    isr := Self.s.slave_pic.b.isr;
    if (isr)<>0 then begin
      max_irq := highest_priority;
      while ( (isr  and (1 shl max_irq)) = 0) do begin
        inc(max_irq);
        if(max_irq > 7) then
          max_irq := 0;
        end;
      if (max_irq = highest_priority ) then exit; (* Highest priority interrupt in-service,
                                                 * no other priorities allowed *)
      if (max_irq > 7) then LogPanic(('error in service_master_pic()'));
      end
  else
      max_irq := highest_priority; (* 0..7 bits in ISR are cleared *)
  end;


  (* now, see if there are any higher priority requests *)
  unmasked_requests := (Self.s.slave_pic.b.irr  and not Self.s.slave_pic.b.imr);
  if (unmasked_requests)<>0 then begin
    irq := highest_priority;
    repeat
      (* for special mod_e, since we're looking at all IRQ's, skip if
       * current IRQ is already in-service
       *)
      if ( Self.s.slave_pic.b.special_mask and ((Self.s.slave_pic.b.isr shr irq)  and $01) )<>0 then
        continue;
      if (unmasked_requests  and (1 shl irq))<>0 then begin
//        BX_DEBUG(Format('slave: signalling IRQ(%u)',[8 + irq]));

        Self.s.slave_pic.b.INT := 1;
        Self.s.slave_pic.b.irq := irq;
        raise_irq(2); (* request IRQ 2 on master pic *)
        exit;
        end; (* if Boolean(unmasked_requests  and ... *)

        inc(irq);
        if(irq > 7) then
          irq := 0;
    until (irq = max_irq); (* do ... *)
    end; (* if Boolean(unmasked_requests := ... *)
end;

  (* CPU handshakes with PIC after acknowledging interrupt *)
function bx_pic_c.IAC:Bit8u;
var
  vector:Bit8u;
  irq:Bit8u;
begin

  bx_pc_system.set_intr(0);
  Self.s.master_pic.b.INT := 0;
  Self.s.master_pic.b.irr := Self.s.master_pic.b.irr and not (1 shl Self.s.master_pic.b.irq);
  // In autoeoi mod_e don't set the isr bit.
  if (Self.s.master_pic.b.auto_eoi=0) then
    Self.s.master_pic.b.isr := Self.s.master_pic.b.isr or (1 shl Self.s.master_pic.b.irq)
  else if (Self.s.slave_pic.b.rotate_on_autoeoi)<>0 then
    Self.s.slave_pic.b.lowest_priority := Self.s.master_pic.b.irq;

  if (Self.s.master_pic.b.irq <> 2) then begin
    irq    := Self.s.master_pic.b.irq;
    vector := irq + Self.s.master_pic.a.interrupt_offset;
    end
  else begin (* IRQ2 := slave pic IRQ8..15 *)
    Self.s.slave_pic.b.INT := 0;
    Self.s.master_pic.b.IRQ_line[2] := 0;
    irq    := Self.s.slave_pic.b.irq;
    vector := irq + Self.s.slave_pic.a.interrupt_offset;
    Self.s.slave_pic.b.irr := Self.s.slave_pic.b.irr and not (1 shl Self.s.slave_pic.b.irq);
    // In autoeoi mod_e don't set the isr bit.
    if (Self.s.slave_pic.b.auto_eoi=0) then
      Self.s.slave_pic.b.isr := Self.s.slave_pic.b.isr or (1 shl Self.s.slave_pic.b.irq)
    else if (Self.s.slave_pic.b.rotate_on_autoeoi)<>0 then
      Self.s.slave_pic.b.lowest_priority := Self.s.slave_pic.b.irq;
    service_slave_pic();
    inc(irq,8);
    end;

  service_master_pic();

  //BX_DBG_IAC_REPORT(vector, irq);
  Result:=vector;
end;

procedure bx_pic_c.show_pic_state;
begin
  LogInfo(Format('s.master_pic.imr := %02x',[Self.s.master_pic.b.imr]));
  LogInfo(Format('s.master_pic.isr := %02x',[Self.s.master_pic.b.isr]));
  LogInfo(Format('s.master_pic.irr := %02x',[Self.s.master_pic.b.irr]));
  LogInfo(Format('s.master_pic.irq := %02x',[Self.s.master_pic.b.irq]));
end;
end.
