unit PCI;

interface

uses SysUtils,Iodev, Config, Service;

type
  bx_def440fx_t = record
    confAddr: Bit32u;
    confData: Bit32u;
    array_: array[0..256] of Bit8u;
    shadow: array[0..4 * 16 * 4096] of Bit8u;     // 256k of memory
  end;

  TPCI_bx = class
  public
    s: record
      i440fx: bx_def440fx_t;
    end;

    constructor Create;
    destructor Destroy;
    procedure init(d:Pbx_devices_c);
    procedure reset;
    procedure print_i440fx_state;
    function  rd_memType(addr:Bit32u):Bit32u;
    function  wr_memType(addr:Bit32u):Bit32u;
    function  i440fx_fetch_ptr(addr:Bit32u):PBit8u;
  private
    devices: pbx_devices_c;
    function read_handler(this_ptr:pointer; address:Bit32u; io_len:unsigned):Bit32u;
    procedure write_handler(this_ptr:pointer; address:Bit32u; value:Bit32u; io_len:unsigned);
  {$if BX_USE_PCI_SMF=0}
    Bit32u read(Bit32u address, unsigned io_len);
    void   write(Bit32u address, Bit32u value, unsigned io_len);
  {$ifend}
    function mapRead(val:Bit32u):Bit32u;
    function mapWrite(val:Bit32u):Bit32u;
  end;

var
  bx_pci: TPCI_bx;

implementation

constructor TPCI_bx.Create;
begin
  inherited;
end;

destructor TPCI_bx.Destroy;
begin
  // nothing for now
  inherited;
end;

procedure TPCI_bx.init(d: pbx_devices_c );
var
  I: unsigned;
begin
  // called once when bochs initializes

  Self.devices := d;

  if SUPPORT440FX <> 0 then
  begin
    for i := $0CFC to $0CFF do
      d^.register_io_read_handler(Self, read_handler, i, 'i440FX');

    d^.register_io_write_handler(Self, write_handler, $0CF8, 'i440FX');
    for i := $0CFC to $0CFF do
      d^.register_io_write_handler(Self, write_handler, i, 'i440FX');

    for i := 0 to 256 do
      Self.s.i440fx.array_[i] := $0;
  end;
  // readonly registers
  Self.s.i440fx.array_[$00] := $86;
  Self.s.i440fx.array_[$01] := $80;
  Self.s.i440fx.array_[$02] := $37;
  Self.s.i440fx.array_[$03] := $12;
  Self.s.i440fx.array_[$0b] := $06;
end;

procedure TPCI_bx.reset;
var
  I:unsigned;
begin
  Self.s.i440fx.array_[$04] := $06;
  Self.s.i440fx.array_[$05] := $00;
  Self.s.i440fx.array_[$06] := $80;
  Self.s.i440fx.array_[$07] := $02;
  Self.s.i440fx.array_[$0d] := $00;
  Self.s.i440fx.array_[$0f] := $00;
  Self.s.i440fx.array_[$50] := $00;
  Self.s.i440fx.array_[$51] := $01;
  Self.s.i440fx.array_[$52] := $00;
  Self.s.i440fx.array_[$53] := $80;
  Self.s.i440fx.array_[$54] := $00;
  Self.s.i440fx.array_[$55] := $00;
  Self.s.i440fx.array_[$56] := $00;
  Self.s.i440fx.array_[$57] := $01;
  Self.s.i440fx.array_[$58] := $10;
  for i:= $59 to $60 do
    Self.s.i440fx.array_[i] := $00;
end;

  // static IO port read callback handler
  // redirects to non-static class handler to aprocedure virtual functions

function TPCI_bx.read_handler(this_ptr: pointer; address: Bit32u; io_len: unsigned):Bit32u;
var
  val440fx, retMask: Bit32u;
begin
  case (address) of
    $0CFC,
    $0CFD,
    $0CFE,
    $0CFF:
    begin
      // PMC is bus 0 / device 0 / function 0
      if (Self.s.i440fx.confAddr and $80FFFF00) = $80000000 then
      begin
        val440fx := Self.s.i440fx.confData shr ((address  and $3)*8);

        if io_len = 1 then
          retMask:=$FF;
        if io_len = 2 then
          retMask:=$FFFF;
        if io_len = 4 then
          retMask:=$FFFFFFFF;
        if (io_len <> 1) and (io_len <> 2) and (io_len <> 4) then
          retMask:=$FFFFFFFF;

        val440fx := (val440fx  and retMask);
//      	BX_DEBUG(Format('440FX PMC read register $%02x value $%08x',[
//  		  Self.s.i440fx.confAddr + (address and $3), val440fx]));
        exit(val440fx);
      end else
        exit($FFFFFFFF);
    end;
  end;

  LogPanic(Format('unsupported IO read to port $%x', [unsigned(address)]));
  Result := $ffffffff;
end;

  // static IO port write callback handler
  // redirects to non-static class handler to aprocedure virtual functions

procedure TPCI_bx.write_handler(this_ptr: pointer; address: Bit32u; value: Bit32u;
  io_len: unsigned);
var
  idx440fx:Bit8u;
  max_len:Bit8u;
  i:unsigned;
  label end_case;
begin
{$if BX_USE_PCI_SMF=0}
  TPCI_bx *class_ptr := (TPCI_bx *) Self_ptr;

  class_ptr^.write(address, value, io_len);
end;

procedure TPCI_bx.write(Bit32u address, Bit32u value, unsigned io_len)
begin
{$else}
  //UNUSED(Self_ptr);
{$ifend} // !BX_USE_PCI_SMF

  case address of
    $CF8:
      begin
        // confAddr accepts a dword value only
        if io_len = 4 then
        begin
          Self.s.i440fx.confAddr := value;
          if Boolean((value  and $80FFFF00) = $80000000) then
          begin
            idx440fx := Bit8u(value  and $FC);
            Move(Self.s.i440fx.confData, Self.s.i440fx.array_[idx440fx], 4);
//            BX_DEBUG(Format('440FX PMC register $%02x selected',[idx440fx]));
          end else
            Self.s.i440fx.confData := 0;
//            BX_DEBUG(Format('440FX request for bus $%02x device $%02x function $%02x',
//                [(value shr 16)  and $FF, (value shr 11)  and $1F, (value shr 8)  and $07]));
        end;
      end;

    $CFC,
    $CFD,
    $CFE,
    $CFF:
    begin

      idx440fx := Bit8u((Self.s.i440fx.confAddr  and $FC) + (address  and $3));
      max_len := 4 - (address  and $3);
      if Boolean(io_len < max_len) then max_len := io_len;
      if Boolean((Self.s.i440fx.confAddr  and $80FFFF00) = $80000000) then
      begin
        for i:=0 to max_len do
        begin
          case idx440fx+i of
            $00,
            $01,
            $02,
            $03,
            $06,
            $08,
            $09,
            $0a,
            $0b:
              goto end_case;
          else
            begin
              Self.s.i440fx.array_[idx440fx+i] := (value shr (i*8))  and $FF;
  //                  BX_DEBUG(Format('440FX PMC write register $%02x value $%02x',
  //                  [idx440fx, (value shr (i*8))  and $FF]));
            end;
          end;
        end;
        end_case:
        Move(Self.s.i440fx.confData, Self.s.i440fx.array_[idx440fx], 4);
      end;
    end else
      LogPanic(Format('IO write to port $%x',[unsigned(address)]));
  end;
end;

function TPCI_bx.mapRead(val: Bit32u): Bit32u;
begin
  case val of
    $0,
    $2: Exit(1); // (0) Goto ROM
    $1,
    $3: Exit(0); // (1) Goto Shadow
  end;
  Result := 2;
end;

function TPCI_bx.mapWrite(val: Bit32u): Bit32u;
begin
  case val of
    $0,
    $1: Exit(1); // (0) Goto ROM
    $2,
    $3: Exit(0); // (1) Goto Shadow
  end;
  Result := 2;
end;

function TPCI_bx.rd_memType(addr: Bit32u): Bit32u;
begin
  case ((addr and $FC000) shr 12) of
    $C0: Result:=(mapRead (  Self.s.i440fx.array_[$5A] and $3));
    $C4: Result:=(mapRead ( (Self.s.i440fx.array_[$5A] shr 4)  and $3));
    $C8: Result:=(mapRead (  Self.s.i440fx.array_[$5B] and $3));
    $CC: Result:=(mapRead ( (Self.s.i440fx.array_[$5B] shr 4)  and $3));
    $D0: Result:=(mapRead (  Self.s.i440fx.array_[$5C] and $3));
    $D4: Result:=(mapRead ( (Self.s.i440fx.array_[$5C] shr 4)  and $3));
    $D8: Result:=(mapRead (  Self.s.i440fx.array_[$5D] and $3));
    $DC: Result:=(mapRead ( (Self.s.i440fx.array_[$5D] shr 4)  and $3));
    $E0: Result:=(mapRead (  Self.s.i440fx.array_[$5E] and $3));
    $E4: Result:=(mapRead ( (Self.s.i440fx.array_[$5E] shr 4)  and $3));
    $E8: Result:=(mapRead (  Self.s.i440fx.array_[$5F] and $3));
    $EC: Result:=(mapRead ( (Self.s.i440fx.array_[$5F] shr 4)  and $3));
    $F0,
    $F4,
    $F8,
    $FC: Result:=(mapRead ( (Self.s.i440fx.array_[$59] shr 4)  and $3));
  else
    begin
      LogPanic(('rd_memType () Error: Memory Type not known !'));
      exit(0);
    end;
  end;
end;

function TPCI_bx.wr_memType(addr: Bit32u): Bit32u;
begin
  case ((addr and $FC000) shr 12) of
    $C0: Result:=(mapwrite (  Self.s.i440fx.array_[$5A] and $3));
    $C4: Result:=(mapwrite ( (Self.s.i440fx.array_[$5A] shr 4)  and $3));
    $C8: Result:=(mapwrite (  Self.s.i440fx.array_[$5B] and $3));
    $CC: Result:=(mapwrite ( (Self.s.i440fx.array_[$5B] shr 4)  and $3));
    $D0: Result:=(mapwrite (  Self.s.i440fx.array_[$5C] and $3));
    $D4: Result:=(mapwrite ( (Self.s.i440fx.array_[$5C] shr 4)  and $3));
    $D8: Result:=(mapwrite (  Self.s.i440fx.array_[$5D] and $3));
    $DC: Result:=(mapwrite ( (Self.s.i440fx.array_[$5D] shr 4)  and $3));
    $E0: Result:=(mapwrite (  Self.s.i440fx.array_[$5E] and $3));
    $E4: Result:=(mapwrite ( (Self.s.i440fx.array_[$5E] shr 4)  and $3));
    $E8: Result:=(mapwrite (  Self.s.i440fx.array_[$5F] and $3));
    $EC: Result:=(mapwrite ( (Self.s.i440fx.array_[$5F] shr 4)  and $3));
    $F0,
    $F4,
    $F8,
    $FC: Result:=(mapwrite ( (Self.s.i440fx.array_[$59] shr 4)  and $3));
  else
    begin
      LogPanic(('rd_memType () Error: Memory Type not known !'));
      exit(0);
    end;
  end;
end;

procedure TPCI_bx.print_i440fx_state;
var
  i: integer;
begin
  LogInfo(Format( 'i440fxConfAddr:$%08x', [Self.s.i440fx.confAddr]));
  LogInfo(Format( 'i440fxConfData:$%08x', [Self.s.i440fx.confData]));
{$if DUMP_FULL_I440FX=1}
//  for (i:=0; i<256; i++) then begin
//    LogInfo(( 'i440fxArray%02x:$%02x', i, Self.s.i440fx.array[i] ));
//    end;
{$else} (* DUMP_FULL_I440FX *)
  for i:=$59 to $60 do
    LogInfo(Format( 'i440fxArray%02x:$%02x',[ i, Self.s.i440fx.array_[i]]));
{$ifend} (* DUMP_FULL_I440FX *)
end;

function TPCI_bx.i440fx_fetch_ptr(addr: Bit32u): PBit8u;
begin
  if SUPPORT440FX <> 0 then
  begin
    case (rd_memType (addr)) of
      $0: exit(@Self.devices^.mem^.vector[addr]); // Read from ShadowRAM
      $1: exit(@Self.s.i440fx.shadow[(addr - $c0000)]); // Read from ROM
    else
      begin
        LogPanic(('i440fx_fetch_ptr(): default case'));
        exit(0);
      end;
    end;
  end else
    Result:=(@Self.devices^.mem^.vector[addr]);
end;

end.
