unit Iodev;

interface

uses cpu, Config, Service, SysUtils, Memory, ExtCtrls, WorkSpaceWizLogic_u;

const
  BX_IODEV_HANDLER_PERIOD = 100;   // microseconds (old 100)
  (* maximum number of emulated devices allowed.  floppy, mda, etc...
     you can increase this to anything below 256 since an 8-bit handle
     is used for each device *)
  BX_MAX_IO_DEVICES = 20;

  (* number of IRQ lines supported.  In an ISA PC there are two
     PIC chips cascaded together.  each has 8 IRQ lines, so there
     should be 16 IRQ's total *)
  BX_MAX_IRQS = 16;
  BX_NO_IRQ = -1;
type

  // Bit32u (*bx_read_handler_t)(void *, Bit32u, unsigned);
  pbx_read_handler_t = ^ bx_read_handler_t;
  bx_read_handler_t = function(P:Pointer; Addr:Bit32u; c1:Word):Bit32u of object;
  // typedef void   (*bx_write_handler_t)(void *, Bit32u, Bit32u, unsigned);
  pbx_write_handler_t = ^bx_write_handler_t;
  bx_write_handler_t = procedure(P:Pointer; Addr:Bit32u; Cx:Bit32u; C1:Word) of object;

  pbx_devices_c = ^bx_devices_c;
  bx_devices_c = class
  public
    mem:PMEM_C;  // address space associated with these devices

    timer_handle:Integer;
    constructor Create;
    Destructor Destroy; override;
    procedure init(memory:PMEM_C;Cfg:PConf); overload;
    procedure Init(const aConf: TWorkSpaceConfig); overload;
    procedure register_io_read_handler(this_ptr:Pointer; f:bx_read_handler_t; addr:Bit32u; name:PChar );
    procedure register_io_write_handler(this_ptr: Pointer; f: bx_write_handler_t; addr: Bit32u; name: PChar);
    procedure register_irq(irq:unsigned; name:PChar);
    procedure unregister_irq(irq:unsigned; name:PChar);
    //procedure iodev_init;
    function inp(addr:Bit16u; io_len:unsigned):Bit32u;
    procedure outp(addr:Bit16u; value:Bit32u; io_len:unsigned);

    procedure dma_write8(channel:unsigned; data:PBit8u);
    procedure dma_read8(channel:unsigned; data:PBit8u);
    procedure dma_write16(channel:unsigned; data:PBit16u);
    procedure dma_read16(channel:unsigned; data:PBit16u);
    procedure drq(channel:unsigned; val:Bool);
    procedure raise_hlda;
    procedure timer_handler(this_ptr:Pointer);
    procedure timer;

    (*bx_ioapic_c      *ioapic;
    bx_pci_c         *pci;
    bx_pit_c         *pit;
    bx_keyb_c        *keyboard;
    bx_dma_c         *dma;
    bx_floppy_ctrl_c *floppy;
    bx_cmos_c        *cmos;
    bx_serial_c      *serial;
    bx_parallel_c    *parallel;
    bx_unmapped_c    *unmapped;
    bx_vga_c         *vga;
    bx_hard_drive_c  *hard_drive;
    bx_sb16_c        *sb16;
    bx_ne2k_c        *ne2k;
    bx_g2h_c         *g2h;
  #if BX_IODEBUG_SUPPORT
    bx_iodebug_c	   *iodebug;
  #endif*)

  private
    read_handler_id:array [0..$10000] of Bit8u;  // 64K
    io_read_handler:array[0..BX_MAX_IO_DEVICES] of record
      funct:bx_read_handler_t;
      this_ptr:Pointer;
      handler_name:PChar;  // name of device
    end;
    num_read_handles:Unsigned;

    write_handler_id:array[0..$10000] of bit8u; // 64K
    io_write_handler:array[0..BX_MAX_IO_DEVICES] of record
      funct:bx_write_handler_t;
      this_ptr:Pointer;
      handler_name:PChar;  // name of device
    end;
    num_write_handles:Unsigned;

    // more for informative purposes, the names of the devices which
    // are use each of the IRQ 0..15 lines are stored here
    irq_handler_name:array[0..BX_MAX_IRQS] of pchar;

    function read_handler(this_ptr:Pointer; address:Bit32u; io_len:unsigned):Bit32u;
    procedure write_handler(this_ptr:Pointer; address:Bit32u; value:Bit32u; io_len:unsigned);
    function port92_read(address:Bit32u; io_len:unsigned):Bit32u;
    procedure port92_write(address:Bit32u; value:Bit32u; io_len:unsigned);
  end;

var
  bx_devices:bx_devices_c;

implementation

uses
  PIC, Unmapped, cmos, dma, PCI,{$if BX_USE_NEW_PIT=1} pit_wrap, pit82c54 {$else} pit {$ifend}
  , keyboard, HDD, vga, floppy;

const
  BASE_MEMORY_IN_K = 640;

//bx_devices_c bx_devices;

constructor bx_devices_c.Create;
var
  I:Word;
begin

{$if BX_PCI_SUPPORT = 1}
//  pci = NULL;
{$ifend}
//  pit = NULL;
//  keyboard = NULL;
//  dma = NULL;
//  floppy = NULL;
//  cmos = NULL;
//  serial = NULL;
//  parallel = NULL;
//  unmapped = NULL;
//  vga = NULL;
//  pic = NULL;
//  hard_drive = NULL;
//  sb16 = NULL;
//  ne2k = NULL;
//  g2h = NULL;
{$if BX_IODEBUG_SUPPORT = 1}
  //iodebug = NULL;
{$ifend}

  num_read_handles := 0;
  num_write_handles := 0;

  //set unused elements to appropriate values
  for i:=0 to BX_MAX_IO_DEVICES - 1 do
    begin
      io_read_handler[i].funct  := nil;
      io_write_handler[i].funct := nil;
    end;

  i:=0;
  for i:=0 to $10000-1 do
    begin
      read_handler_id[i] := BX_MAX_IO_DEVICES;  // not assigned
      write_handler_id[i] := BX_MAX_IO_DEVICES;  // not assigned
    end;

  for i:=0 to BX_MAX_IRQS do
    irq_handler_name[i] := nil;

  timer_handle := BX_NULL_TIMER_HANDLE;
end;

destructor bx_devices_c.Destroy;
begin
   bx_pic.Free;
   bx_cmos.Free;
   bx_unmapped.Free;
   bx_floppy.Free;
   bx_dma.Free;
   bx_vga.Free;
  {$if BX_USE_NEW_PIT=1}
   bx_pit_82C54.Free;
  {$ifend}
  bx_pit.Free;
  bx_keyboard.Free;
  HardDrive.close_harddrive;
  HardDrive.Free;
end;

procedure bx_devices_c.timer;
var
  retval: unsigned;
begin
  retval := bx_keyboard.periodic( BX_IODEV_HANDLER_PERIOD );
  if Boolean(retval and $01) then
    bx_pic.raise_irq(1);

  if Boolean(retval and $02) then
    bx_pic.raise_irq(12);

//{$if BX_SUPPORT_APIC=1}
//  // update local APIC timers
//  for (int i=0; i<BX_SMP_PROCESSORS; i++) {
//    BX_CPU(i)->local_apic.periodic (BX_IODEV_HANDLER_PERIOD);
//  }
//{$ifend}
end;

procedure bx_devices_c.init(memory:PMEM_C;Cfg:PConf);
var
  extended_memory_in_k:Bit16u;
begin
  bx_unmapped:=bx_unmapped_c.Create;
  bx_unmapped.init(@Self);

{$if BX_PCI_SUPPORT = 1}
  bx_pci := TPCI_bx.Create;
  bx_pci.Init(@Self);
  bx_pci.Reset;
{$ifend}
  bx_cmos:=bx_cmos_c.Create;
  bx_cmos.init(@Self);
  bx_cmos.Reset;

  bx_dma:=bx_dma_c.Create;
  bx_dma.init(@Self);

  bx_pic:=bx_pic_c.Create;
  bx_pic.init(@Self);

  HardDrive:=THardDrive.Create;
  HardDrive.init(@Self,@bx_cmos);
  HardDrive.set_cd_media_status(1);
  bx_vga := TVga_c1.Create;
  bx_vga.init(@Self, @bx_cmos);

  bx_pit_82C54:=pit_82C54.Create;

  bx_pit:=bx_pit_c.Create;
  bx_pit.Init(@Self);

  bx_floppy := TFloppyDrive.Create;
  bx_floppy.init(@Self, @bx_cmos,Cfg);
  bx_floppy.reset(BX_RESET_HARDWARE);

  bx_keyboard:=TKeyBoard.Create;
  bx_keyboard.init(@Self,@bx_cmos);
  mem := memory;

  register_io_read_handler(self, read_handler, $0092, 'Port 92h System Control' );
  register_io_write_handler(self, write_handler, $0092, 'Port 92h System Control' );

  // misc. CMOS
  //extended_memory_in_k := mem^.get_memory_in_k() - 1024;
  extended_memory_in_k := (MemoryMB*1024)-1024;
  bx_cmos.s.reg[$15] := Bit8u(BASE_MEMORY_IN_K);
  bx_cmos.s.reg[$16] := Bit8u(BASE_MEMORY_IN_K shr 8);
  bx_cmos.s.reg[$17] := Bit8u(extended_memory_in_k);
  bx_cmos.s.reg[$18] := Bit8u(extended_memory_in_k shr 8);
  bx_cmos.s.reg[$30] := Bit8u(extended_memory_in_k);
  bx_cmos.s.reg[$31] := Bit8u(extended_memory_in_k shr 8);

  (* now perform checksum of CMOS memory *)
  bx_cmos.checksum_cmos();

  timer_handle := bx_pc_system.register_timer( self, timer_handler,
    unsigned(BX_IODEV_HANDLER_PERIOD), 1, 1);
end;

procedure bx_devices_c.register_io_read_handler( this_ptr:Pointer; f:bx_read_handler_t; addr:Bit32u; name:PChar );
var
  Handle:Word;
begin

  addr := addr and $0000ffff;

  // first find existing handle for function or create new one
  handle:=0;
  while handle < num_read_handles do
  begin
    if @io_read_handler[handle].funct = @f then
      break;
    Inc(handle);
  end;

  if (handle >= num_read_handles) then
  begin
    // no existing handle found, create new one

    Inc(num_read_handles);
    io_read_handler[handle].funct          := f;
    io_read_handler[handle].this_ptr       := this_ptr;
    io_read_handler[handle].handler_name   := name;
  end;

  // change table to reflect new handler id for that address 
  if (read_handler_id[addr] < BX_MAX_IO_DEVICES) then
  begin
    // another handler is already registered for that address
    // if it is not the Unmapped port handler, bail
    if ( io_read_handler[read_handler_id[addr]].handler_name <> 'Unmapped' )  then
    begin
      LogInfo(Format('IO device address conflict(read) at IO address %xh', [addr]));
      LogPanic(Format(' conflicting devices: %s & %s',
        [io_read_handler[handle].handler_name, io_read_handler[read_handler_id[addr]].handler_name]));
    end;
  end;
  read_handler_id[addr] := handle;
end;

procedure bx_devices_c.register_io_write_handler(this_ptr: Pointer;
  f: bx_write_handler_t; addr: Bit32u; name: PChar);
var
  Handle: Word;
begin
  addr := addr and $0000ffff;
  handle:=0;
  while handle < num_write_handles do
  begin
    if @io_write_handler[handle].funct = @f then break;
    Inc(Handle);
  end;

  if (handle >= num_write_handles) then
  begin
    //no existing handle found, create new one
    if num_write_handles >= BX_MAX_IO_DEVICES then
    begin
      LogInfo('too many IO devices installed.');
      LogPanic('  try increasing BX_MAX_IO_DEVICES');
    end;
    Inc(num_write_handles);
    io_write_handler[handle].funct          := f;
    io_write_handler[handle].this_ptr       := this_ptr;
    io_write_handler[handle].handler_name   := name;
  end;
  // change table to reflect new handler id for that address
  if write_handler_id[addr] < BX_MAX_IO_DEVICES then
  begin
    // another handler is already registered for that address
    // if it is not the Unmapped port handler, bail
    if io_write_handler[write_handler_id[addr]].handler_name <> 'Unmapped' then
    begin
      LogInfo(Format('IO device address conflict(read) at IO address %xh', [addr]));
      LogPanic(Format(' conflicting devices: %s & %s',
        [io_write_handler[handle].handler_name, io_write_handler[read_handler_id[addr]].handler_name]));
    end;
  end;
  write_handler_id[addr] := handle;
end;

  procedure
bx_devices_c.register_irq(irq:unsigned; name:PChar);
begin
  if Boolean(irq >= BX_MAX_IRQS) then
    LogPanic(Format('IO device %s registered with IRQ:=%d above %u',[name, irq, BX_MAX_IRQS-1]));

  if Boolean(irq_handler_name[irq]) then
    LogPanic(Format('IRQ %u conflict, %s with %s',[ irq, irq_handler_name[irq], name]));

  irq_handler_name[irq] := name;
end;

procedure bx_devices_c.unregister_irq(irq:unsigned; name:PChar);
begin
  if Boolean(irq >= BX_MAX_IRQS)
    then LogPanic(Format('IO device %s tried to unregister IRQ %d above %u',[name, irq, BX_MAX_IRQS-1]));

  if Boolean(irq_handler_name[irq]=nil) then
  begin
    LogInfo(Format('IO device %s tried to unregister IRQ %d, not registered',[name, irq]));
    exit;
  end;

  if Boolean(StrComp(irq_handler_name[irq], name)) then
  begin
    LogInfo(Format('IRQ %u not registered to %s but to %s',[irq, name, irq_handler_name[irq]]));
    exit;
  end;
  irq_handler_name[irq] := NULL;
end;

procedure bx_devices_c.init(const aConf: TWorkSpaceConfig);
var
  extended_memory_in_k:Bit16u;
begin
  bx_unmapped:=bx_unmapped_c.Create;
  bx_unmapped.init(@Self);

{$if BX_PCI_SUPPORT = 1}
  bx_pci := TPCI_bx.Create;
  bx_pci.Init(@Self);
  bx_pci.Reset;
{$ifend}
  bx_cmos:=bx_cmos_c.Create;
  bx_cmos.init(@Self);
  bx_cmos.Reset;

  bx_dma:=bx_dma_c.Create;
  bx_dma.init(@Self);

  bx_pic:=bx_pic_c.Create;
  bx_pic.init(@Self);

  HardDrive := THardDrive.Create;
  HardDrive.init(@Self, @bx_cmos);
  HardDrive.set_cd_media_status(1);
  bx_vga := TVga_c1.Create;
  bx_vga.init(@Self, @bx_cmos);

  bx_pit_82C54:=pit_82C54.Create;

  bx_pit:=bx_pit_c.Create;
  bx_pit.Init(@Self);

  bx_floppy := TFloppyDrive.Create;
  bx_floppy.init(@Self, @bx_cmos, aConf);
  bx_floppy.reset(BX_RESET_HARDWARE);

  bx_keyboard:=TKeyBoard.Create;
  bx_keyboard.init(@Self,@bx_cmos);
  mem := nil;

  register_io_read_handler(self, read_handler, $0092, 'Port 92h System Control' );
  register_io_write_handler(self, write_handler, $0092, 'Port 92h System Control' );

  // misc. CMOS
  //extended_memory_in_k := mem^.get_memory_in_k() - 1024;
  extended_memory_in_k := (MemoryMB*1024)-1024;
  bx_cmos.s.reg[$15] := Bit8u(BASE_MEMORY_IN_K);
  bx_cmos.s.reg[$16] := Bit8u(BASE_MEMORY_IN_K shr 8);
  bx_cmos.s.reg[$17] := Bit8u(extended_memory_in_k);
  bx_cmos.s.reg[$18] := Bit8u(extended_memory_in_k shr 8);
  bx_cmos.s.reg[$30] := Bit8u(extended_memory_in_k);
  bx_cmos.s.reg[$31] := Bit8u(extended_memory_in_k shr 8);

  (* now perform checksum of CMOS memory *)
  bx_cmos.checksum_cmos();

  timer_handle := bx_pc_system.register_timer( self, timer_handler,
    unsigned(BX_IODEV_HANDLER_PERIOD), 1, 1);
end;

function bx_devices_c.inp(addr:Bit16u; io_len:unsigned):Bit32u;
var
  handle: Bit8u;
begin
  //BX_INSTR_INP(addr, io_len);
  handle := read_handler_id[addr];
  result := bx_read_handler_t(io_read_handler[handle].funct)(io_read_handler[handle].this_ptr, Bit32u(addr), io_len);
  //BX_INSTR_INP2(addr, io_len, ret);
  //BX_DBG_IO_REPORT(addr, io_len, BX_READ, ret);
end;

(*
 * Write a byte of data to the IO memory address space.
 *)

procedure bx_devices_c.outp(addr:Bit16u; value:Bit32u; io_len:unsigned);
var
  handle: Bit8u;
begin
  //BX_INSTR_OUTP(addr, io_len);
  //BX_INSTR_OUTP2(addr, io_len, value);
  //BX_DBG_IO_REPORT(addr, io_len, BX_WRITE, value);
  handle := write_handler_id[addr];
  bx_write_handler_t(io_write_handler[handle].funct)(io_write_handler[handle].this_ptr, Bit32u(addr), value,io_len);
end;

procedure bx_devices_c.dma_read8(channel:unsigned; data:PBit8u);
begin
  if Boolean(channel = 2) then
    bx_floppy.dma_read(data);
end;

procedure bx_devices_c.dma_write8(channel:unsigned; data:PBit8u);
begin
  if Boolean(channel = 2) then
    bx_floppy.dma_write(data);
end;

procedure bx_devices_c.dma_read16(channel:unsigned; data:PBit16u);
begin
  //UNUSED(channel);
  //UNUSED(data_word);
end;

procedure bx_devices_c.dma_write16(channel:unsigned; data:PBit16u);
begin
  //UNUSED(channel);
  //UNUSED(data_word);
end;

procedure bx_devices_c.drq(channel:unsigned; val:Bool);
begin
  bx_dma.DRQ(channel, val);
end;

procedure bx_devices_c.raise_hlda;
begin
  bx_dma.raise_HLDA( @bx_pc_system );
end;

function bx_devices_c.read_handler(this_ptr:Pointer; address:Bit32u; io_len:unsigned):Bit32u;
var
  class_ptr:pbx_devices_c;
begin
  class_ptr:=pbx_devices_c(this_ptr);
  Result:=class_ptr.port92_read(address,io_len);
end;

procedure bx_devices_c.write_handler(this_ptr:Pointer; address:Bit32u; value:Bit32u; io_len:unsigned);
var
  class_ptr:pbx_devices_c;
begin
  class_ptr:=pbx_devices_c(this_ptr);
  class_ptr.port92_write(address,value,io_len);
end;

function bx_devices_c.port92_read(address:Bit32u; io_len:unsigned):Bit32u;
begin
  if (io_len > 1) then
    LogPanic(Format('port 92h: io read from address %08x, len=%u',[address, io_len]));

//  BX_DEBUG(('port92h read partially supported!!!'));
//  BX_DEBUG(Format('  returning %02x', [bx_pc_system.Get_enable_a20 shl 1]));
  Result:=bx_pc_system.Get_enable_a20 shl 1;
end;

procedure bx_devices_c.port92_write(address:Bit32u; value:Bit32u; io_len:unsigned);
begin
//  BX_DEBUG(Format('port92h write of %02x partially supported!!!',[unsigned(value)]));
//  BX_DEBUG(('A20: set_enable_a20() called'));
  bx_pc_system.set_enable_a20((value and $02) shr 1);
end;

procedure bx_devices_c.timer_handler(this_ptr:Pointer);
var
  class_ptr:pbx_devices_c;
begin
  class_ptr:=pbx_devices_c(this_ptr);
  class_ptr^.timer;
end;

end.



