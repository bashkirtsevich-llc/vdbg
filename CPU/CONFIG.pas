 { ****************************************************************************** }
 { Mozaa 0.95 - Virtual PC emulator - developed by Massimiliano Boidi 2003 - 2004 }
 { ****************************************************************************** }

{ For any question write to maxboidi@libero.it }

(* **** *)
{$include defines.pas}
unit CONFIG;

interface

uses Windows, WorkSpaceWizLogic_u;

const

  VK_RETURN = 13;
  KEYEVENTF_KEYUP = 2;
  CR   = #13;
  NULL = nil;
  BX_BOOT_FLOPPYA = 0;
  BX_BOOT_DISKC = 1;
  BX_BOOT_CDROM = 2;

  BX_INSTRUMENTATION = $00;
  BX_SIM_ID          = $00;
  BX_X86_DEBUGGER    = $00;
  BX_SUPPORT_TASKING = $01;
  BX_SUPPORT_A20     = $01;

  BX_CPU_LEVEL        = $06; // Set to maximum cpu option
  BX_CPU_LEVEL_HACKED = $06;

var
  BX_SUPPORT_FPU: boolean = False;

const
  BX_USE_CPU_SMF    = $01;
  BX_USE_HD_SMF     = $01;
  BX_USE_FD_SMF     = $01;
  BX_DYNAMIC_TRANSLATION = $00;
  BX_USE_TLB        = $01;
  BX_BIG_ENDIAN     = $00;
  BX_LITTLE_ENDIAN  = $01;
  BX_SUPPORT_APIC   = $00;
  REGISTER_IADDR    = $00;
  BX_BOOTSTRAP_PROCESSOR = $00;
  BX_DBG_ASYNC_INTR = $1;

  BX_USE_DMA_SMF   = 1;  // DMA
  BX_USE_PIC_SMF   = 1;  // PIC
  BX_USE_KEY_SMF   = 1;
  BX_DMA_FLOPPY_IO = 1;

  BX_PIC_DEBUG     = 0;
  BX_PIT_DEBUG     = 0;
  BX_CMOS_DEBUG    = 0;
//  BX_TIMER_DEBUG   = 0;
  BX_USE_IDLE_HACK = 0;
  BX_PORT_E9_HACK  = 1;

  BX_USE_SPECIFIED_TIME0 = 0;

  dbgunsupported_io = 0;

  BYTES_PER_SECTOR = 512;


  // APIC -  Da implementare
  APIC_BASE_ADDR = $fee00000;  // default APIC address
  // FINE - APIC -  Da implementare

  BX_INHIBIT_INTERRUPTS = $01;
  BX_INHIBIT_DEBUG      = $02;
  BX_PROVIDE_CPU_MEMORY = $01;
  BX_DEBUGGER           = $00;
  MAGIC_BREAKPOINT      = $01;
  BX_SHADOW_RAM         = $00;

  BX_TLB_SIZE = 1024;

  BX_USE_MEM_SMF     = $00;
  BX_USE_UM_SMF      = $01;
  BX_USE_CMOS_SMF    = $01;
  BX_USE_VGA_SMF     = $01;
  BX_SMP_PROCESSORS  = $01;
  BX_PCI_SUPPORT     = $00;
  BX_IODEBUG_SUPPORT = $00;
  BX_USE_PCI_SMF     = $01;
  BX_USE_PIT_SMF     = $01;

  BX_SUPPORT_PAGING = $01;
  BX_SUPPORT_VBE    = $01;

  BX_PF_EXCEPTION    = $100;
  INTEL_DIV_FLAG_BUG = $00;
  VGA_TRACE_FEATURE  = $01;

  SHOW_EXIT_STATUS      = $01;
  BX_SUPPORT_V8086_MODE = $01;

  SUPPORT440FX     = 0;
  DUMP_FULL_I440FX = 0;

  BX_NUM_CMOS_REGS    = 64;
  BX_SPLIT_HD_SUPPORT = 00;

  //Options

  Options_cmos_time      = 1;
  Options_cmos_use_image = 0;

var
  BX_CD_STATE: boolean = True;

//const
//  BX_MOUSE_ENABLED = 1;
//  LOWLEVEL_CDROM   = 1;

var
  BX_C_PRESENT: integer = 1;

const
  BX_D_PRESENT     = 1;
  BX_CDROM_PRESENT = 1;
  BX_DEBUG_HDD     = 1;

  BX_NEW_HDD_SUPPORT = 1;
  SEEK_SET           = 0;
  CONNER_CFA540A     = 0;

  BX_FLOPPY_NONE = 10; // floppy not present
  BX_FLOPPY_1_2  = 11; // 1.2M  5.25"
  BX_FLOPPY_1_44 = 12; // 1.44M 3.5"
  BX_FLOPPY_2_88 = 13; // 2.88M 3.5"
  BX_FLOPPY_720K = 14; // 720K  3.5"
  BX_FLOPPY_LAST = 14; // last one

  BX_EJECTED          = 10;
  BX_INSERTED         = 11;
//  BX_GET_FLOPPYB_TYPE = BX_FLOPPY_NONE;
  BX_FLOPPY_INSERTED  = BX_INSERTED;

  BX_RESET_SOFTWARE = 10;
  BX_RESET_HARDWARE = 11;

//  BX_DEBUG_FLOPPY = 0;
  BX_WITH_MACOS   = 0;

  BX_GUI_ENABLED   = 1;
  BX_INFO_ENABLED  = 0;
  BX_DEBUG_ENABLED = 0;
  BX_ERROR_ENABLED = 0;
  BX_LOG_ENABLED   = 0;

  BX_DEBUG_TO_FILE = 0;
  BX_USE_NEW_PIT   = 1;
  {$j+}
  CPU_SPEED        = 500000;   // 500000
  {$j-}

  BX_READ_AFTER = 83890000;

  BX_MAX_ERRORS      = 2000000;
  BX_MAX_IPS         = 1900000;
  BX_MAX_IPS_REFRESH = 3000;

//  HDD_READ_ONLY        = 0;
  KEYBOARD_DELAY       = 20000; // 20000
  KEYBOARD_PASTE_DELAY = 100000; //100000

  BX_USE_REALTIME_PIT = 0; //0

  BX_BOOT_A = 1;
  BX_BOOT_B = 0;
  BX_BOOT_C = 0;

  BX_MB_SIZE = 1024 * 1024;

type
  ArrayChar128  = array[0..128] of char;
  ArrayChar512  = array[0..512] of char;
  ArrayChar4096 = array[0..4096] of char;

  PConf = ^TConf;

  TConf = record
    Memory:      word;
    BootDevice:  byte;
//    FloppyType:  integer;
//    FloppyFile:  string;

    FloppyAType: Integer;
    FloppyAFile: string;
    FloppyAReadOnly: Boolean;

    FloppyBType: Integer;
    FloppyBFile: string;
    FloppyBReadOnly: Boolean;


    HddPresent:  boolean;
    HddFile:     string;
    HddCylinder: integer;
    HddHeads:    integer;
    HddSectors:  integer;
    CdPresent:   boolean;
    CdFile:      string;
    running:     boolean;
    usefpu:      boolean;
    BIOS:        string;
    VGA_BIOS:    string;
    Font:        string;
  end;

  precstate = ^recstate;

  recstate = record
    a0: longword;
    a1: longword;
    a2: longword;
    a3: longword;
    a4: longword;
    a5: longword;
    a6: longword;
    a7: longword;
    a8: longword;
    a9: longword;
  end;

  PBit8u    = ^byte;
  Bit8u     = byte;
  PBit8s    = ^shortint;
  Bit8s     = shortint;
  PBit16u   = ^word;
  Bit16u    = word;
  PBit16s   = ^smallint;
  Bit16s    = smallint;
  PBit32u   = ^longword;
  Bit32u    = longword;
  PBit32s   = ^integer;
  Bit32s    = integer;
  PBit64u   = ^Bit64u;
  Bit64u    = Uint64;
  PBit64s   = ^Bit64s;
  Bit64s    = int64;
  punsigned = ^unsigned;
  unsigned  = word;
  Bool      = word;
  Size_T    = longword;
  time_t    = longint;
  off_t     = Bit32u;
  ssize_t   = longword;
  puint8    = ^uint8;
  uint8     = bit8u;

  puint16 = ^uint16;
  uint16  = bit16u;

  puint32       = ^uint32;
  uint32        = bit32u;
  parray_memory = ^array_memory;
  array_memory  = array[0..256 * 1024] of Bit8u;

  Char256 = array[0..256] of char;

  HDC     = longword;
  HBITMAP = longword;

  array_buffer_disk   = array[0..2048] of Bit8u;
  array_buffer_floppy = array[0..512 + 2] of Bit8u;

const
  bx_parity_lookup: array[0..255] of Bool = (
    1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
    0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
    0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
    1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
    0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
    1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
    1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
    0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
    0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
    1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
    1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
    0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
    1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
    0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
    0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
    1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1);

var
  ThreadGui, ThreadGuiID: THandle;
  LastMessage: array[0..128] of char;
  FilesLog:  integer;
  drawCS, KeyCS, MouseCS, FakeCS: RTL_CRITICAL_SECTION;
  StartEvent: integer;
  OutLogTxt, LogFile: TextFile;
  EmsDir, WorkDir, KeybDir: string;
  WriteBXInfo: boolean = False;
  MouseEnabled: boolean = False;
  ReLoop:    boolean = False;
  ShowAgain: boolean = False;

  MEMORYMB:      word;
  BX_BOOT_DRIVE: word;
  BX_FLOPPY_PATH, HDD_FILE_DISK, BX_CD_FILEPATH: string;

  HDD_NUM_CYL, HDD_SECTOR_PER_TRACK, HDD_NUM_HEADS: word;
  DelphiWindow: THandle;
  mozaaMenu:    integer;

//GlobalDataFpuAddress:LongWord;

procedure SetConfiguration (const Cfg: PConf);
procedure SetWorkSpaceConfig(const aConf: TWorkSpaceConfig);
function LoadConfigFile (FName: string; Rec: PConf): boolean;
function SaveConfigFile (FName: string; Rec: PConf): boolean;
procedure SetDefaultConfig (Cfg: PConf);
procedure OutError (const idxError: integer);

implementation

uses IniFiles, SysUtils;

procedure OutError (const idxError: integer);
begin
  WriteLn('Fatal Error....');
  Halt;
end;

function CheckFile (s: string): string;
begin
  Result := s;
  if not FileExists(s) then
    raise Exception.Create('Can''t find file: ' + s);
end;

procedure SetDefaultConfig (cfg: PConf);
begin
  cfg.Memory := 32;
  cfg.BootDevice := BX_BOOT_DISKC;

  cfg.FloppyAType := BX_FLOPPY_1_44;
  cfg.FloppyAFile := WorkDir + 'disks\A.img';
  cfg.FloppyAReadOnly := False;

  cfg.FloppyBType := BX_FLOPPY_1_44;
  cfg.FloppyBFile := WorkDir + 'disks\A.img';
  cfg.FloppyBReadOnly := False;

  cfg.HddPresent := True;
  cfg.HddFile := WorkDir + 'disks\C.img';
  cfg.HddCylinder := 360;
  cfg.HddHeads := 2;
  cfg.HddSectors := 63;
  cfg.CdPresent := False;
  cfg.CdFile  := '';
  cfg.running := False;
  cfg.usefpu  := False;
end;

procedure SetConfiguration (const Cfg: PConf);
begin
  MemoryMB := Cfg.Memory;

  BX_BOOT_DRIVE := Cfg.BootDevice;

//  BX_FLOPPY_PATH := Cfg.FloppyFile;
  BX_C_PRESENT := integer(Cfg.HddPresent);
  HDD_FILE_DISK := Cfg.HddFile;
  HDD_NUM_CYL := Cfg.HddCylinder;
  HDD_NUM_HEADS := Cfg.HddHeads;
  HDD_SECTOR_PER_TRACK := Cfg.HddSectors;
  BX_CD_FILEPATH := Cfg.CdFile;
  BX_CD_STATE := Cfg.CdPresent;
  BX_SUPPORT_FPU := Cfg.usefpu;
end;

procedure SetWorkSpaceConfig(const aConf: TWorkSpaceConfig);
var
  i: Integer;
begin
  MEMORYMB := aConf.MemorySize;
  BX_BOOT_DRIVE := Ord(aConf.Boot); // проверить

  BX_C_PRESENT := 0;
  HDD_FILE_DISK := '';
  HDD_NUM_CYL := 0;
  HDD_NUM_HEADS := 0;
  HDD_SECTOR_PER_TRACK := 0;
  BX_CD_FILEPATH := '';
  BX_CD_STATE := False;

  for i := 0 to Pred(length(aConf.Devices)) do
    if aConf.Devices[i].DeviceType = dHDD then
    begin
      BX_C_PRESENT := 1;
      HDD_FILE_DISK := aConf.Devices[i].DeviceFileName;
      HDD_NUM_CYL := aConf.Devices[i].DiscConfig.Cyl;
      HDD_NUM_HEADS := aConf.Devices[i].DiscConfig.Heads;
      HDD_SECTOR_PER_TRACK := aConf.Devices[i].DiscConfig.Sectors;
      Break;
    end;
  for i := 0 to Pred(length(aConf.Devices)) do
    if aConf.Devices[i].DeviceType = dCDROM then
    begin
      BX_CD_STATE := True;
      BX_CD_FILEPATH := aConf.Devices[i].DeviceFileName;
      Break;
    end;
  BX_SUPPORT_FPU := aConf.UseFPU;
end;

function LoadConfigFile (FName: string; Rec: PConf): boolean;
var
  ini: TIniFile;
begin
  SetDefaultConfig(Rec);
  if FileExists(FName) then
  begin
    ini := TIniFile.Create(FName);

    Rec.Memory  := ini.ReadInteger('main', 'Memory', Rec.Memory);
    Rec.BootDevice := ini.ReadInteger('main', 'BootDevice', Rec.BootDevice);

    Rec.FloppyAType := ini.Readinteger('main', 'FloppyAType', Rec.FloppyAType);
    Rec.FloppyAFile := ini.ReadString('main', 'FloppyAFile', Rec.FloppyAFile);
    Rec.FloppyAReadOnly := ini.ReadBool('main', 'FloppyAReadOnly', Rec.FloppyAReadOnly);

    Rec.FloppyBType := ini.Readinteger('main', 'FloppyBType', Rec.FloppyBType);
    Rec.FloppyBFile := ini.ReadString('main', 'FloppyBFile', Rec.FloppyBFile);
    Rec.FloppyBReadOnly := ini.ReadBool('main', 'FloppyBReadOnly', Rec.FloppyBReadOnly);

    Rec.HddPresent := ini.ReadBool('main', 'HDDPresent', Rec.HddPresent);
    Rec.HddFile := ini.ReadString('main', 'HDDFile', Rec.HddFile);
    Rec.HddCylinder := ini.ReadInteger('main', 'HDDCylinder', Rec.HddCylinder);
    Rec.HddHeads := ini.ReadInteger('main', 'HDDHeads', Rec.HddHeads);
    Rec.HddSectors := ini.ReadInteger('main', 'HDDSectors', Rec.HddSectors);
    Rec.CdPresent := ini.ReadBool('main', 'CDPresent', Rec.CdPresent);
    Rec.CdFile  := ini.ReadString('main', 'CDFile', Rec.CdFile);
    Rec.usefpu  := ini.ReadBool('main', 'FPU', Rec.usefpu);
    Rec.BIOS := ini.ReadString('main', 'BIOS', Rec.BIOS);
    Rec.VGA_BIOS := ini.ReadString('main', 'VGA_BIOS', Rec.VGA_BIOS);
    Rec.Font := ini.ReadString('main', 'FONT', Rec.Font);
    ini.Free;
  end;
  Result := True;
end;


function SaveConfigFile (FName: string; Rec: PConf): boolean;
var
  Ini: TIniFile;
begin
  ini := TIniFile.Create(FName);
  try
    ini.WriteInteger('main', 'Memory', Rec.Memory);
    ini.WriteInteger('main', 'BootDevice', Rec.BootDevice);

    ini.Writeinteger('main', 'FloppyAType', Rec.FloppyAType);
    ini.WriteString('main', 'FloppyAFile', Rec.FloppyAFile);
    Ini.WriteBool('main', 'FloppyAReadOnly', Rec.FloppyAReadOnly);

    ini.Writeinteger('main', 'FloppyBType', Rec.FloppyBType);
    ini.WriteString('main', 'FloppyBFile', Rec.FloppyBFile);
    Ini.WriteBool('main', 'FloppyBReadOnly', Rec.FloppyBReadOnly);

    ini.WriteBool('main', 'HDDPresent', Rec.HddPresent);
    ini.WriteString('main', 'HDDFile', Rec.HddFile);
    ini.WriteInteger('main', 'HDDCylinder', Rec.HddCylinder);
    ini.WriteInteger('main', 'HDDHeads', Rec.HddHeads);
    ini.WriteInteger('main', 'HDDSectors', Rec.HddSectors);
    ini.WriteBool('main', 'CDPresent', Rec.CdPresent);
    ini.WriteString('main', 'CDFile', Rec.CdFile);
    ini.WriteBool('main', 'FPU', Rec.usefpu);
    ini.WriteString('main', 'BIOS', Rec.BIOS);
    ini.WriteString('main', 'VGA_BIOS', Rec.VGA_BIOS);
    ini.WriteString('main', 'FONT', Rec.Font);
  except
    {--}
  end;
  ini.Free;
  Result := True;
end;

end.
