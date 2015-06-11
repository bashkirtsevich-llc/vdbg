unit Unmapped;

interface

uses
  iodev,config;
const
  BX_BIOS_MESSAGE_SIZE = 80;

type
  bx_unmapped_c = class
  public
    constructor Create;
    destructor Destroy; override;
    procedure init(d:pbx_devices_c);
  private
    s:record
      port80:Bit8u;
      port8e:Bit8u;

      bios_message:array[0..BX_BIOS_MESSAGE_SIZE] of char;
      bios_message_i:unsigned;
    end;
    devices:pbx_devices_c;
    function read_handler(this_ptr:pointer; address:Bit32u; io_len:unsigned):Bit32u;
    procedure write_handler(this_ptr:Pointer; address:Bit32u; value:Bit32u; io_len:unsigned);
  end;

var
  bx_unmapped:bx_unmapped_c;
implementation

uses Service, SysUtils;

constructor bx_unmapped_c.Create;
begin
  s.port80 := $00;
  s.port8e := $00;

  s.bios_message_i := 0;
end;

destructor bx_unmapped_c.Destroy;
begin
end;

procedure bx_unmapped_c.init(d:pbx_devices_c);
var
  addr:Bit32u;
begin
  Self.devices := d;

  for addr:=0 to $10000 do
    begin
      Self.devices^.register_io_read_handler(Self, read_handler, addr, 'Unmapped');
      Self.devices^.register_io_write_handler(Self, write_handler, addr, 'Unmapped');
      end;
end;

  // static IO port read callback handler
  // redirects to non-static class handler to aprocedure virtual functions

function bx_unmapped_c.read_handler(this_ptr:pointer; address:Bit32u; io_len:unsigned):Bit32u;
var
  retval:Bit32u;
  label return_from_read;
begin
  //UNUSED(io_len);

  // This function gets called for access to any IO ports which
  // are not mapped to any device handler.  Reads return 0

  if Boolean((address >= $02e0) and (address <= $02ef)) then begin
	retval := 0;
	goto return_from_read;
  end;

  case (address) of
    $80:
      begin
    	  retval := Self.s.port80;
	    end;
    $8e:
      begin
    	  retval := Self.s.port8e;
	    end;
{$if BX_PORT_E9_HACK=1}
    // Unused port on ISA - Self can be used by the emulated code
    // to detect it is running inside Bochs and that the debugging
    // features are available (write $FF or something on unused
    // port $80, then read from $e9, if value is $e9, debug
    // output is available) (see write() for that) -- Andreas and Emmanuel
    $e9:
      begin
    	  retval := $e9;
      end;
{$ifend}
    $03df:
      begin
    	  retval := $ffffffff;
//        BX_DEBUG(Format('unsupported IO read from port %04x (CGA)', [address]));
      end;
    $023a,
    $02f8, (* UART *)
    $02f9, (* UART *)
    $02fb, (* UART *)
    $02fc, (* UART *)
    $02fd, (* UART *)
    $02ea,
    $02eb,
    $03e8,
    $03e9,
    $03ea,
    $03eb,
    $03ec,
    $03ed,
    $03f8, (* UART *)
    $03f9, (* UART *)
    $03fb, (* UART *)
    $03fc, (* UART *)
    $03fd, (* UART *)
    $17c6:
      begin
    	  retval := $ffffffff;
//        BX_DEBUG(Format('unsupported IO read from port %04x',[address]));
      end;
    else
      begin
    	  retval := $ffffffff;
      end;
    end;

  return_from_read:
  if Boolean(dbgunsupported_io) then
    begin
      case (io_len) of
      1:
        begin
          retval := Bit8u(retval);
//          BX_DEBUG(Format('unmapped: 8-bit read from %04x := %02x',[address, retval]));
        end;
      2:
        begin
          retval := Bit16u(retval);
//          BX_DEBUG(Format('unmapped: 16-bit read from %04x := %04x',[address, retval]));
        end;
//      4:
//        begin
//          BX_DEBUG(Format('unmapped: 32-bit read from %04x := %08x',[address, retval]));
//        end;
//      else
//        BX_DEBUG(Format('unmapped: ??-bit read from %04x := %x',[address, retval]));
      end;
    end;
  Result:=retval;
end;


  // static IO port write callback handler
  // redirects to non-static class handler to aprocedure virtual functions

procedure bx_unmapped_c.write_handler(this_ptr:Pointer; address:Bit32u; value:Bit32u; io_len:unsigned);
  label return_from_write,EndCase;
begin
{$if BX_USE_UM_SMF=0}
  bx_unmapped_c *class_ptr := (bx_unmapped_c *) this_ptr;

  class_ptr^.write(address, value, io_len);
end;

  procedure
bx_unmapped_c.write(Bit32u address, Bit32u value, unsigned io_len)
begin
{$else}
  //UNUSED(this_ptr);
{$ifend}  // !BX_USE_UM_SMF
  //UNUSED(io_len);


  // This function gets called for access to any IO ports which
  // are not mapped to any device handler.  Writes to an unmapped
  // IO port are ignored.

// ???


  if Boolean((address >= $02e0) and (address <= $02ef)) then
	goto return_from_write;

  case (address) of
    $80: // diagnostic test port to display progress of POST
      //BX_DEBUG(('Diagnostic port 80h: write := %02xh', (unsigned) value));
        begin
          Self.s.port80 := value;
        end;

    $8e: // ???
      begin
        Self.s.port8e := value;
      end;

{$if BX_PORT_E9_HACK = 1}
    // This port doesn't exist on normal ISA architecture. However,
    // we define a convention here, to display on the console of the
    // system running Bochs, anything that is written to it. The
    // idea is to provide debug output very early when writing
    // BIOS or OS code for example, without having to bother with
    // properly setting up a serial port or anything.
    //
    // Idea by Andreas Beck (andreas.beck@ggi-project.org)

    $e9:
      begin
        putchar(value);
        //fflush(stdout);
      end;
{$ifend}
    $ed: // Dummy port used as I/O delay
	    goto EndCase;
    $ee: // ???
	    goto EndCase;

    $2f2,
    $2f3,
    $2f4,
    $2f5,
    $2f6,
    $2f7,
    $3e8,
    $3e9,
    $3eb,
    $3ec,
    $3ed:
	    // BX_DEBUG(('unsupported IO write to port %04x of %02x',
	    // address, value));
      goto EndCase;
    $0401:
      begin
        if Boolean(Self.s.bios_message_i > 0) then begin
        // if there are bits of message in the buffer, print them as the
        // panic message.  Otherwise fall into the next case.
        if Boolean(Self.s.bios_message_i >= BX_BIOS_MESSAGE_SIZE) then
          Self.s.bios_message_i := BX_BIOS_MESSAGE_SIZE-1;
          Self.s.bios_message[ Self.s.bios_message_i] := #0;
        Self.s.bios_message_i := 0;
        LogPanic((Self.s.bios_message));
        end;
      end;
    $0400:
      begin
        LogPanic(Format('BIOS panic at rombios.c, line %d', [value]));
      end;

    $fff0:
      begin
        Self.s.bios_message[Self.s.bios_message_i] := Char(Bit8u(value));
        Self.s.bios_message_i := Self.s.bios_message_i + 1;
        if Boolean( Self.s.bios_message_i >= BX_BIOS_MESSAGE_SIZE ) then begin
          Self.s.bios_message[ BX_BIOS_MESSAGE_SIZE - 1] := #0;
          Self.s.bios_message_i := 0;
          LogInfo(Format('%s', [Self.s.bios_message]));
          end
        else if Boolean(Char((value and $ff)) = chr(13)) then begin
          Self.s.bios_message[ Self.s.bios_message_i - 1 ] := #0;
          Self.s.bios_message_i := 0;
          LogInfo(Format('%s',[Self.s.bios_message]));
          end;
        end;

    else
	    goto EndCase;
    end;
  EndCase:
  return_from_write:
  if Boolean(dbgunsupported_io) then
    begin
      case (io_len) of
      1:
        LogInfo(Format('unmapped: 8-bit write to %04x := %02x', [address, value]));
      2:
        LogInfo(Format('unmapped: 16-bit write to %04x := %04x',[address, value]));
      4:
        LogInfo(Format('unmapped: 32-bit write to %04x := %08x',[address, value]));
      else
        LogInfo(Format('unmapped: ??-bit write to %04x := %x',[address, value]));
      end;
    end;  
end;
end.
