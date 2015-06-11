{ ****************************************************************************** }
{ Mozaa 0.95 - Virtual PC emulator - developed by Massimiliano Boidi 2003 - 2004 }
{ ****************************************************************************** }

{ For any question write to maxboidi@libero.it }

(* **** *)
unit Memory;

interface

uses config, Classes, m2fMain;

type
  TRomType = (romtype_pcbios, romtype_vgabios);

type
  PMEM_C = ^TMEM_C;

  TMEM_C = class
  public
    vector: array of bit8u;
    len: size_t;
    megabytes: size_t; // (len in Megabytes)

    constructor Create(memsize: size_t);
    destructor  Destroy;
    // procedure  init_memory(memsize:integer);
    procedure read_physical(addr: Bit32u; len: word; const data_2: Pointer);
    procedure write_physical(addr: Bit32u; len: word; Data: Pointer);
    procedure load_ROM(romtype: TRomType; romaddress: Bit32u);
    procedure LoadInROM(const aFileName: string; const aRomAddress: Bit32u);
    function  get_memory_in_k: Bit32u;
  end;

var
  sysmemory: TMEM_C;

implementation

uses cpu, vga;
{.$I ..\patches\pcbios.inc}
{.$I ..\patches\vgabios.inc}

constructor TMEM_C.Create(memsize: size_t);
var
  I: longword;
begin
  SetLength(vector, memsize);
  len := memsize;
  megabytes := Trunc(len / (1024 * 1024));
  I := $C0000;
  while I <> $C0000 + $40000 do
  begin
    Self.vector[I] := $FF;
    Inc(I);
  end;
end;

destructor TMEM_C.Destroy;
begin
  SetLength(vector, 0);
  inherited;
end;

procedure TMEM_C.load_ROM(romtype: TRomType; romaddress: Bit32u);
var
  s_bios: TMemoryStream;
begin
//  case romtype of
//    romtype_pcbios:
//      Move(PCBIOS, vector[romaddress], SizeOf(PCBIOS));
//    romtype_vgabios:
//      Move(VGABIOS, vector[romaddress], SizeOf(VGABIOS));
//  end;
  case romtype of
    romtype_pcbios:
    begin
      s_bios := TMemoryStream.Create;
      s_bios.LoadFromFile(VMConfig.BIOS);
      Move(s_bios.Memory^, vector[romaddress], s_bios.Size);
      s_bios.Free;
    end;
    romtype_vgabios:
    begin
      s_bios := TMemoryStream.Create;
      s_bios.LoadFromFile(VMConfig.VGA_BIOS);
      Move(s_bios.Memory^, vector[romaddress], s_bios.Size);
      s_bios.Free;
    end;
  end;
end;

function TMEM_C.get_memory_in_k: Bit32u;
begin
  Result := megabytes * 1024;
end;

procedure TMEM_C.LoadInROM(const aFileName: string; const aRomAddress: Bit32u);
var
  _DataFile: TMemoryStream;
begin
  try
    _DataFile := TMemoryStream.Create;
    _DataFile.LoadFromFile(aFileName);
    Move(_DataFile.Memory^, vector[aRomAddress], _DataFile.Size);
  finally
    _DataFile.Free;
  end;
end;

procedure TMEM_C.read_physical(addr: Bit32u; len: word;
  const data_2: Pointer);
var
  data_2_ptr: PBit8u;
  a20addr: Bit32u;
  data32: Bit32u;
  data16: Bit16u;
  data8: bit8u;
  I: unsigned;
label read_one, inc_one;
begin
  a20addr := addr and bx_pc_system.a20_mask;

  if ((a20addr + len) <= Self.len) then
  begin
    // all of data_2 is within limits of physical memory
    if ((a20addr and $FFF80000) <> $00080000) then
    begin
      if (len = 4) then
      begin
        if ((a20addr and $00000003) = 0) then
        begin
          // read 4-byte data_2 from aligned memory location
          data32 := PBit32u(@vector[a20addr])^;
          PBit32u(data_2)^ := data32;
          exit;
        end else
        begin

          data32 := PBit8u(@vector[(addr + 3) and bx_pc_system.a20_mask])^;
          data32 := data32 shl 8;
          data32 := data32 or PBit8u
            (@vector[(addr + 2) and bx_pc_system.a20_mask])^;
          data32 := data32 shl 8;
          data32 := data32 or PBit8u
            (@vector[(addr + 1) and bx_pc_system.a20_mask])^;
          data32 := data32 shl 8;
          data32 := data32 or PBit8u(@vector[addr and bx_pc_system.a20_mask])^;

          PBit32u(data_2)^ := data32;
          exit;
        end;
      end;

      if (len = 2) then
      begin
        if ((a20addr and $00000001) = 0) then
        begin
          // read 2-byte data_2 from aligned memory location
          data16 := PBit16u(@vector[a20addr])^;
          PBit16u(data_2)^ := data16;
          exit;
        end else
        begin
          data16 := PBit8u(@vector[(addr + 1) and bx_pc_system.a20_mask])^;
          data16 := data16 shl 8;
          data16 := data16 or PBit8u(@vector[addr and bx_pc_system.a20_mask])^;

          PBit16u(data_2)^ := data16;
          exit;
        end;
      end;

      if (len = 1) then
      begin
        data8 := PBit8u(@vector[a20addr])^;
        PBit8u(data_2)^ := data8;
        exit;
      end;
      // len = 3 case can just fall thru to special cases handling
    end;
    data_2_ptr := PBit8u(data_2);
read_one :
    if ((a20addr and $FFF80000) <> $00080000) then
    begin
      // addr *not* in range 00080000 .. 000FFFFF
      data_2_ptr^ := vector[a20addr];
inc_one :
      if (len = 1) then
        exit;
      Dec(len);
      Inc(addr);
      a20addr := addr and bx_pc_system.a20_mask;
      Inc(data_2_ptr);
      goto read_one;
    end;

    // addr in range 00080000 .. 000FFFFF
//{$IF BX_PCI_SUPPORT = 0}
    if ((a20addr <= $0009FFFF) or (a20addr >= $000C0000)) then
    begin
      // regular memory 80000 .. 9FFFF, C0000 .. F0000
      data_2_ptr^ := vector[a20addr];
      goto inc_one;
    end;
    // VGA memory A0000 .. BFFFF
    data_2_ptr^ := bx_vga.mem_read(a20addr);
    // BX_DBG_UCMEM_REPORT(a20addr, 1, BX_READ, *data_2_ptr); // obsolete
    goto inc_one;
//{$ELSE}   // #if BX_PCI_SUPPORT = 0
//    if (a20addr <= $0009FFFF) then
//    begin
//      * data_2_ptr := vector[a20addr];
//      goto inc_one;
//    end;
//    if (a20addr <= $000BFFFF) then
//    begin
//      // VGA memory A0000 .. BFFFF
//      * data_2_ptr := BX_VGA_MEM_READ(a20addr);
//      BX_DBG_UCMEM_REPORT(a20addr, 1, BX_READ, * data_2_ptr);
//      goto inc_one;
//    end;
//
//    // a20addr in C0000 .. FFFFF
//    if (! bx_options.Oi440FXSupport^.get()) then
//    begin
//      * data_2_ptr := vector[a20addr];
//      goto inc_one;
//    end
//    else
//    begin
//      switch(bx_devices.pci^.rd_memType(a20addr and $FC000)) then
//      begin
//        case $0: // Read from ShadowRAM
//          * data_2_ptr := vector[a20addr];
//          BX_INFO(('Reading from ShadowRAM %08x, data_2 %02x ', (unsigned)
//                a20addr, * data_2_ptr)); goto inc_one;
//
//          case $1: // Read from ROM
//            * data_2_ptr := bx_pci.s.i440fx.shadow[(a20addr - $C0000)];
//          // BX_INFO(('Reading from ROM %08x, data_2 %02x  ', (unsigned) a20addr, *data_2_ptr));
//          goto inc_one; default:
//            BX_PANIC(('.read_physical: default case'));
//          end;
//        end;
//        goto inc_one;
//{$IFEND}// #if BX_PCI_SUPPORT = 0
  end else
  begin
      // some or all of data_2 is outside limits of physical memory
      data_2_ptr := PBit8u(data_2);
      for I := 0 to len do
      begin
//{$IF BX_PCI_SUPPORT = 0}
        if (a20addr < Self.len) then
          data_2_ptr^ := vector[a20addr]
        else
          data_2_ptr^ := $FF;
//{$ELSE}   // BX_PCI_SUPPORT = 0
//        if (a20addr < Self.len) then
//        begin
//          if ((a20addr >= $000C0000) and (a20addr <= $000FFFFF)) then
//          begin
//            if (not bx_options.Oi440FXSupport^.get()) * data_2_ptr := vector
//              [a20addr];
//            else
//            begin
//              switch(bx_devices.pci^.rd_memType(a20addr and $FC000)) then
//              begin
//                case $0: // Read from ROM
//                  * data_2_ptr := vector[a20addr];
//                  // BX_INFO(('Reading from ROM %08x, data_2 %02x ', (unsigned) a20addr, *data_2_ptr));
//                  break;
//
//                  case $1: // Read from Shadow RAM
//                    * data_2_ptr := bx_pci.s.i440fx.shadow[(a20addr - $C0000)];
//                  BX_INFO(('Reading from ShadowRAM %08x, data_2 %02x  ',
//                      (unsigned)a20addr, * data_2_ptr)); break; default:
//                    BX_PANIC(('read_physical: default case'));
//                end; // Switch
//              end;
//            end;
//          else
//          begin
//            * data_2_ptr := vector[a20addr];
//            BX_INFO(('Reading from Norm %08x, data_2 %02x  ', (unsigned)
//                  a20addr, * data_2_ptr));
//          end;
//        end;
//      else
//        * data_2_ptr := $FF;
//{$IFEND}// BX_PCI_SUPPORT = 0
      Inc(addr);
      a20addr := addr and bx_pc_system.a20_mask;
      Inc(data_2_ptr);
    end;
    exit;
  end;
end;

procedure TMEM_C.write_physical(addr: Bit32u; len: word; Data: Pointer);
var
  data_ptr: PBit8u;
  a20addr: Bit32u;
  data32: Bit32u;
  data16: Bit16u;
  data8: bit8u;
  I: unsigned;
label write_one, inc_one;
begin
  a20addr := addr and bx_pc_system.a20_mask;

  if ((a20addr + len) <= Self.len) then
  begin
    // all of data is within limits of physical memory
    if ((a20addr and $FFF80000) <> $00080000) then
    begin
      if (len = 4) then
      begin
        if ((a20addr and $00000003) = 0) then
        begin
          // write 4byte data to aligned memory location
          data32 := PBit32u(Data)^;
          PBit32u(@vector[a20addr])^ := data32;
          exit;
        end
        else
        begin
          data32 := PBit32u(Data)^;
          PBit8u(@vector[a20addr])^ := data32;
          data32 := data32 shr 8;
          PBit8u(@vector[(addr + 1) and bx_pc_system.a20_mask])^ := data32;
          data32 := data32 shr 8;
          PBit8u(@vector[(addr + 2) and bx_pc_system.a20_mask])^ := data32;
          data32 := data32 shr 8;
          PBit8u(@vector[(addr + 3) and bx_pc_system.a20_mask])^ := data32;
          // worst case, last byte is in different page; possible extra dirty page
          exit;
        end;
      end;
      if (len = 2) then
      begin
        if ((a20addr and $00000001) = 0) then
        begin
          // write 2-byte data to aligned memory location
          data16 := PBit16u(Data)^;
          PBit16u(@vector[a20addr])^ := data16;
          exit;
        end else
        begin
          data16 := PBit16u(Data)^;
          PBit8u(@vector[a20addr])^ := bit8u(data16);
          PBit8u(@vector[(a20addr + 1) and bx_pc_system.a20_mask])^ := (data16 shr 8);
          exit;
        end;
      end;
      if (len = 1) then
      begin

        data8 := PBit8u(Data)^;
        PBit8u(@vector[a20addr])^ := data8;
        exit;
      end;
      // len = 3 case can just fall thru to special cases handling
    end;
    data_ptr := PBit8u(Data);
write_one :
    if ((a20addr and $FFF80000) <> $00080000) then
    begin
      // addr *not* in range 00080000 .. 000FFFFF
      vector[a20addr] := data_ptr^;
inc_one :
      if (len = 1) then
        exit;
      Dec(len);
      Inc(addr);
      a20addr := addr and bx_pc_system.a20_mask;
      Inc(data_ptr);
      goto write_one;
    end;
    // addr in range 00080000 .. 000FFFFF
    if (a20addr <= $0009FFFF) then
    begin
      // regular memory 80000 .. 9FFFF
      vector[a20addr] := data_ptr^;
      goto inc_one;
    end;
    if (a20addr <= $000BFFFF) then
    begin
      // VGA memory A0000 .. BFFFF
      bx_vga.mem_write(a20addr, data_ptr^);
      goto inc_one;
    end;
    // adapter ROM     C0000 .. DFFFF
    // ROM BIOS memory E0000 .. FFFFF
    // (ignore write)
    // BX_INFO(('ROM lock %08x: len:=%u',
    // (unsigned) _A20ADDR_, (unsigned) len));
{$IF BX_PCI_SUPPORT = 0}
  {$IF BX_SHADOW_RAM=1}
    // Write it since its in shadow RAM
    vector[a20addr] := data_ptr^;
  {$ELSE}
      // ignore write to ROM
  {$IFEND}
{$ELSE}
// довести до ума
    // Write Based on 440fx Programming
    if (bx_options.Oi440FXSupport^.get()@@((_A20ADDR_ >= $C0000)@ and
          (_A20ADDR_ <= $FFFFF))) then
    begin
//      switch(bx_devices.pci^.wr_memType(_A20ADDR_ and $FC000)) then
      case bx_devices.pci^.wr_memType(_A20ADDR_ and $FC000) of
        $0: begin
          // Writes to ShadowRAM
          // BX_INFO(('Writing to ShadowRAM %08x, len %u ! ', (unsigned) _A20ADDR_, (unsigned) len));
          vector[_A20ADDR_] := * data_ptr;
          BX_DBG_DIRTY_PAGE(_A20ADDR_ shr 12);
          BX_DYN_DIRTY_PAGE(_A20ADDR_ shr 12); goto inc_one;
        end;
        $1: begin
            //  Writes to ROM, Inhibit
            // bx_pci.s.i440fx.shadow[(_A20ADDR_ - $c0000)] := *data_ptr;
            // BX_INFO(('Writing to ROM %08x, Data %02x ! ', (unsigned) _A20ADDR_, *data_ptr));
            goto inc_one;
        end;
      else
        begin
//          BX_PANIC(('write_physical: default case'));
          goto inc_one;
        end;
      end;
//      begin
//        case $0: // Writes to ShadowRAM
//        // BX_INFO(('Writing to ShadowRAM %08x, len %u ! ', (unsigned) _A20ADDR_, (unsigned) len));
//          vector[_A20ADDR_] := * data_ptr;
//          BX_DBG_DIRTY_PAGE(_A20ADDR_ shr 12);
//          BX_DYN_DIRTY_PAGE(_A20ADDR_ shr 12); goto inc_one;
//
//          case $1: // Writes to ROM, Inhibit
//            // bx_pci.s.i440fx.shadow[(_A20ADDR_ - $c0000)] := *data_ptr;
//            // BX_INFO(('Writing to ROM %08x, Data %02x ! ', (unsigned) _A20ADDR_, *data_ptr));
//            goto inc_one;
//          default:
//            BX_PANIC(('write_physical: default case'));
//          goto inc_one; end;
    end;
{$IFEND}
    goto inc_one;
  end else
  begin
          // some or all of data is outside limits of physical memory
{$IF BX_LITTLE_ENDIAN=1}
          data_ptr := PBit8u(Data);
{$ELSE} // BX_BIG_ENDIAN
          data_ptr := (bit8u * )Data + (len - 1);
{$IFEND}
    for I := 0 to len do
    begin
      if (a20addr < Self.len)then
        vector[a20addr] := data_ptr^;
      // otherwise ignore byte, since it overruns memory
      Inc(addr);
      a20addr := addr and bx_pc_system.a20_mask;
{$IF BX_LITTLE_ENDIAN=1}
      Inc(data_ptr);
{$ELSE} // BX_BIG_ENDIAN
      Dec(data_ptr);
{$IFEND}
    end;
  end;
end;

end.
