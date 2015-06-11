unit Iodev;

interface

uses cpu, Config, Service, SysUtils;

const
  (* maximum number of emulated devices allowed.  floppy, mda, etc...
     you can increase this to anything below 256 since an 8-bit handle
     is used for each device *)
  BX_MAX_IO_DEVICES = 20;

  (* number of IRQ lines supported.  In an ISA PC there are two
     PIC chips cascaded together.  each has 8 IRQ lines, so there
     should be 16 IRQ's total *)
  BX_MAX_IRQS = 16;
  BX_NO_IRQ   = -1;

type

  // Bit32u (*bx_read_handler_t)(void *, Bit32u, unsigned);
  pbx_read_handler_t = ^ bx_read_handler_t;
  bx_read_handler_t  = function (P: Pointer; Addr: Bit32u; c1: word): Bit32u of object;
  // typedef void   (*bx_write_handler_t)(void *, Bit32u, Bit32u, unsigned);
  pbx_write_handler_t = ^bx_write_handler_t;
  bx_write_handler_t  = procedure (P: Pointer; Addr: Bit32u; Cx: Bit32u;
    C1: word) of object;

  pbx_devices_c = ^bx_devices_c;

  bx_devices_c = class
  public
    mem: PBX_MEM_C;  // address space associated with these devices
    constructor Create;
    destructor Destroy;
    procedure init (memory: PBX_MEM_C);
    procedure register_io_read_handler (this_ptr: Pointer; f: bx_read_handler_t;
      addr: Bit32u; Name: PChar);
    procedure register_io_write_handler (this_ptr: Pointer;
      f: bx_write_handler_t; addr: Bit32u; Name: PChar);
    procedure register_irq (irq: unsigned; Name: PChar);
    procedure unregister_irq (irq: unsigned; Name: PChar);
    //procedure iodev_init;
    function inp (addr: Bit16u; io_len: unsigned): Bit32u;
    procedure outp (addr: Bit16u; Value: Bit32u; io_len: unsigned);

    procedure dma_write8 (channel: unsigned; Data: PBit8u);
    procedure dma_read8 (channel: unsigned; Data: Bit8u);
    procedure dma_write16 (channel: unsigned; Data: PBit16u);
    procedure dma_read16 (channel: unsigned; Data: PBit16u);
    procedure drq (channel: unsigned; val: Bool);
    procedure raise_hlda;
    procedure timer_handler (this_ptr: Pointer);
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
    bx_pic_c         *pic;
    bx_hard_drive_c  *hard_drive;
    bx_sb16_c        *sb16;
    bx_ne2k_c        *ne2k;
    bx_g2h_c         *g2h;
  #if BX_IODEBUG_SUPPORT
    bx_iodebug_c     *iodebug;
  #endif*)

  private
    read_handler_id: array [0..$10000] of Bit8u;  // 64K
    io_read_handler: array[0..BX_MAX_IO_DEVICES] of record
      funct:        pbx_read_handler_t;
      this_ptr:     Pointer;
      handler_name: PChar;  // name of device
    end;
    num_read_handles: Unsigned;

    write_handler_id: array[0..$10000] of bit8u; // 64K
    io_write_handler: array[0..BX_MAX_IO_DEVICES] of record
      funct:        pbx_write_handler_t;
      this_ptr:     Pointer;
      handler_name: PChar;  // name of device
    end;
    num_write_handles: Unsigned;

    // more for informative purposes, the names of the devices which
    // are use each of the IRQ 0..15 lines are stored here
    irq_handler_name: array[0..BX_MAX_IRQS] of PChar;

    function read_handler (this_ptr: Pointer; address: Bit32u; io_len: unsigned): Bit32u;
    procedure write_handler (this_ptr: Pointer; address: Bit32u;
      Value: Bit32u; io_len: unsigned);
    function port92_read (address: Bit32u; io_len: unsigned): Bit32u;
    procedure port92_write (address: Bit32u; Value: Bit32u; io_len: unsigned);

  end;

implementation

const
  BASE_MEMORY_IN_K = 640;


//bx_devices_c bx_devices;


constructor bx_devices_c.Create;
  var
    I: word;
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

    num_read_handles  := 0;
    num_write_handles := 0;

    //set unused elements to appropriate values
    for i := 0 to BX_MAX_IO_DEVICES - 1 do
      begin
      io_read_handler[i].funct  := nil;
      io_write_handler[i].funct := nil;
      end;

    for i := 0 to $10000 - 1 do
      begin
      read_handler_id[i]  := BX_MAX_IO_DEVICES;  // not assigned
      write_handler_id[i] := BX_MAX_IO_DEVICES;  // not assigned
      end;

    for i := 0 to BX_MAX_IRQS do
      irq_handler_name[i] := nil;

    //timer_handle := BX_NULL_TIMER_HANDLE;
  end;

destructor bx_devices_c.Destroy;
  begin
  end;

procedure bx_devices_c.timer;
  begin

  end;

procedure bx_devices_c.init (memory: PBX_MEM_C);
  begin
    mem := memory;
    //unmapped := bx_unmapped_c.Create; !!! MANCA
    //unmapped.init(Self);

  end;

procedure bx_devices_c.register_io_read_handler (this_ptr: Pointer;
  f: bx_read_handler_t; addr: Bit32u; Name: PChar);
  var
    Handle: word;
  begin

    addr := addr and $0000ffff;

    // first find existing handle for function or create new one
    for handle := 0 to num_read_handles do
      begin
      if @io_read_handler[handle].funct = @f then
        break;
      end;

    if (handle >= num_read_handles) then
      begin
      // no existing handle found, create new one
      if num_read_handles >= BX_MAX_IO_DEVICES then
        begin
        BX_INFO('too many IO devices installed.');
        BX_PANIC(' try increasing BX_MAX_IO_DEVICES');
        end;
      Inc(num_read_handles);
      io_read_handler[handle].funct := @f;
      io_read_handler[handle].this_ptr := this_ptr;
      io_read_handler[handle].handler_name := Name;
      end;

    // change table to reflect new handler id for that address 
    if (read_handler_id[addr] < BX_MAX_IO_DEVICES) then
      begin
      // another handler is already registered for that address
      // if it is not the Unmapped port handler, bail
      if (io_read_handler[read_handler_id[addr]].handler_name <> 'Unmapped') then
        begin
        BX_INFO(Format('IO device address conflict(read) at IO address %xh', [addr]));
        BX_PANIC(Format(' conflicting devices: %s & %s',
          [io_read_handler[handle].handler_name,
          io_read_handler[read_handler_id[addr]].handler_name]));
        end;
      end;
    read_handler_id[addr] := handle;
  end;

procedure bx_devices_c.register_io_write_handler (this_ptr: Pointer;
  f: bx_write_handler_t; addr: Bit32u; Name: PChar);
  var
    Handle: word;
  begin
    addr := addr and $0000ffff;
    for handle := 0 to num_write_handles do
      if @io_write_handler[handle].funct = @f then
        break;

    if (handle >= num_write_handles) then
      begin
      //no existing handle found, create new one
      if num_write_handles >= BX_MAX_IO_DEVICES then
        begin
        BX_INFO('too many IO devices installed.');
        BX_PANIC('  try increasing BX_MAX_IO_DEVICES');
        end;
      Inc(num_write_handles);
      io_write_handler[handle].funct := @f;
      io_write_handler[handle].this_ptr := this_ptr;
      io_write_handler[handle].handler_name := Name;
      end;
    // change table to reflect new handler id for that address
    if write_handler_id[addr] < BX_MAX_IO_DEVICES then
      begin
      // another handler is already registered for that address
      // if it is not the Unmapped port handler, bail
      if io_write_handler[write_handler_id[addr]].handler_name <> 'Unmapped' then
        begin
        BX_INFO(Format('IO device address conflict(read) at IO address %xh', [addr]));
        BX_PANIC(Format(' conflicting devices: %s & %s',
          [io_write_handler[handle].handler_name,
          io_write_handler[read_handler_id[addr]].handler_name]));
        end;
      end;
    write_handler_id[addr] := handle;
  end;

procedure bx_devices_c.register_irq (irq: unsigned; Name: PChar);
  begin
    if boolean(irq >= BX_MAX_IRQS) then
      begin
      BX_PANIC(Format('IO device %s registered with IRQ:=%d above %u',
        [Name, irq, BX_MAX_IRQS - 1]));
      end;
    if boolean(irq_handler_name[irq]) then
      begin
      BX_PANIC(Format('IRQ %u conflict, %s with %s',
        [irq, irq_handler_name[irq], Name]));
      end;
    irq_handler_name[irq] := Name;
  end;

procedure bx_devices_c.unregister_irq (irq: unsigned; Name: PChar);
  begin
    if boolean(irq >= BX_MAX_IRQS) then
      begin
      BX_PANIC(Format('IO device %s tried to unregister IRQ %d above %u',
        [Name, irq, BX_MAX_IRQS - 1]));
      end;

    if boolean(irq_handler_name[irq] = nil) then
      begin
      BX_INFO(Format('IO device %s tried to unregister IRQ %d, not registered',
        [Name, irq]));
      exit;
      end;

    if boolean(StrComp(irq_handler_name[irq], Name)) then
      begin
      BX_INFO(Format('IRQ %u not registered to %s but to %s',
        [irq, Name, irq_handler_name[irq]]));
      exit;
      end;
    irq_handler_name[irq] := NULL;
  end;

function bx_devices_c.inp (addr: Bit16u; io_len: unsigned): Bit32u;
  var
    handle: Bit8u;
    ret: Bit32u;
  begin

    //BX_INSTR_INP(addr, io_len);

    handle := read_handler_id[addr];
    ret := pbx_read_handler_t(io_read_handler[handle].funct)^(
      io_read_handler[handle].this_ptr, Bit32u(addr), io_len);
    //BX_INSTR_INP2(addr, io_len, ret);
    //BX_DBG_IO_REPORT(addr, io_len, BX_READ, ret);
    Result := ret;
  end;

(*
 * Write a byte of data to the IO memory address space.
 *)

procedure bx_devices_c.outp (addr: Bit16u; Value: Bit32u; io_len: unsigned);
  var
    handle: Bit8u;
  begin

    //BX_INSTR_OUTP(addr, io_len);
    //BX_INSTR_OUTP2(addr, io_len, value);

    //BX_DBG_IO_REPORT(addr, io_len, BX_WRITE, value);
    handle := write_handler_id[addr];
    pbx_write_handler_t(io_read_handler[handle].funct)^(
      io_write_handler[handle].this_ptr,
      Bit32u(addr), Value, io_len);
  end;

procedure bx_devices_c.dma_read8 (channel: unsigned; Data: Bit8u);
  begin
  {if Boolean(channel = 2) then
    floppy^.dma_read(data_byte); !!! MANCA}
  end;

procedure bx_devices_c.dma_write8 (channel: unsigned; Data: PBit8u);
  begin
{  if Boolean(channel = 2)
    floppy^.dma_write(data_byte); !!! MANCA}
  end;

procedure bx_devices_c.dma_read16 (channel: unsigned; Data: PBit16u);
  begin
    //UNUSED(channel);
    //UNUSED(data_word);
  end;

procedure bx_devices_c.dma_write16 (channel: unsigned; Data: PBit16u);
  begin
    //UNUSED(channel);
    //UNUSED(data_word);
  end;

procedure bx_devices_c.drq (channel: unsigned; val: Bool);
  begin
    //dma->DRQ(channel, val); !!! MANCA
  end;

procedure bx_devices_c.raise_hlda;
  begin
    //dma->raise_HLDA( &bx_pc_system ); !!! MANCA
  end;

function bx_devices_c.read_handler (this_ptr: Pointer; address: Bit32u;
  io_len: unsigned): Bit32u;
  var
    class_ptr: pbx_devices_c;
  begin
    class_ptr := pbx_devices_c(this_ptr);
    Result := class_ptr.port92_read(address, io_len);
  end;

procedure bx_devices_c.write_handler (this_ptr: Pointer; address: Bit32u;
  Value: Bit32u; io_len: unsigned);
  var
    class_ptr: pbx_devices_c;
  begin
    class_ptr := pbx_devices_c(this_ptr);
    class_ptr.port92_write(address, Value, io_len);
  end;

function bx_devices_c.port92_read (address: Bit32u; io_len: unsigned): Bit32u;
  begin
    if (io_len > 1) then
      BX_PANIC(Format('port 92h: io read from address %08x, len=%u', [address, io_len]));

    BX_DEBUG(('port92h read partially supported!!!'));
    BX_DEBUG(Format('  returning %02x', [bx_cpu.pcsystem.Get_enable_a20 shl 1]));
    Result := bx_cpu.pcsystem.Get_enable_a20 shl 1;
  end;

procedure bx_devices_c.port92_write (address: Bit32u; Value: Bit32u; io_len: unsigned);
  begin
    bx_cpu.pcsystem.set_enable_a20((Value and $02) shr 1);
  end;

procedure bx_devices_c.timer_handler (this_ptr: Pointer);
  var
    class_ptr: pbx_devices_c;
  begin
    class_ptr := pbx_devices_c(this_ptr);
    class_ptr^.timer;
  end;

end.
