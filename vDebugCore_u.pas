unit vDebugCore_u;

interface

uses
  Windows, Classes, SysUtils, WorkSpaceWizLogic_u;

type
  TDebugMode = (dmStop, dmRun, dmStepOver, dmStepInto, dmAnimate);

  TTVDbgLogThread = class(TThread)
  private
    FCanPush: boolean;
    procedure PushLog; // выкинуть лог наружу
  protected
    procedure Execute; override;
  public
  end;

  TVDebugThread = class(TThread)
  private
    FOnCommandStep: TNotifyEvent;
    FOnStartEmulator: TNotifyEvent;
    FOnStopEmulator: TNotifyEvent;

    procedure __OnCommandStep;
    procedure __OnStartEmulator;
    procedure __OnStopEmulator;
  private
    FDebugMode: TDebugMode;
    FNextEIP: Cardinal;
  private
    FGlobalConfig: TWorkSpaceConfig;
    // FLog: TTVDbgLogThread;
    procedure OnCPUStep(aInstruction: Pointer);
    // OnCPUStep
    // OnCPUBreakpoint
    // OnHDDBreakpoint
    // OnFloppyBreakpoint
    // OnOther
    // OnException
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    property OnCommandStep: TNotifyEvent read FOnCommandStep write FOnCommandStep;
    property OnStartEmulator: TNotifyEvent read FOnStartEmulator write FOnStartEmulator;
    property OnStopEmulator: TNotifyEvent read FOnStopEmulator write FOnStopEmulator;

    property GlobalConfig: TWorkSpaceConfig read FGlobalConfig write FGlobalConfig;

    procedure Run;
    procedure Pause;
    procedure Stop;
    procedure StepOver;
    procedure StepInto;
    procedure AnimateStepOver;
    procedure AnimateStepInto;

    constructor Create(const aConf: TWorkSpaceConfig); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Config, Gui32, cpu, iodev, Service, Memory, IniFiles, CreateIsoForm, frmConfig,
  floppy, HDD, cdrom;

{ TVDebugThread }

procedure TVDebugThread.AnimateStepInto;
begin
  FDebugMode := dmAnimate;
  Resume;
end;

procedure TVDebugThread.AnimateStepOver;
begin
  FDebugMode := dmAnimate;
  Resume;
end;

constructor TVDebugThread.Create(const aConf: TWorkSpaceConfig);
var
//  VMConf: TConf;
  _resetInfo: TCPUResetInfo;
  i: Integer;
begin
  // тут сделать вычитывание параметров из воркспэйса
  inherited Create(True);
  FGlobalConfig := aConf;
  FNextEIP := 0;
  FreeOnTerminate := True;

  FDebugMode := dmStop;
  // создание ядра эмулятора
//  LoadConfigFile(WorkDir + 'config\vm.ini', @VMConf);
//  SetConfiguration(@VMConf);
  SetWorkSpaceConfig(FGlobalConfig);

  bx_cpu := TCPU.Create;
  bx_cpu.OnCPUCommand := OnCPUStep;

  bx_pc_system:=TPC_System.Create;
  // объем ОЗУ из параметров
  sysmemory := TMEM_C.Create(FGlobalConfig.MemorySize * 1024 * 1024);
  for i := 0 to Pred(Length(FGlobalConfig.Modules)) do
    sysmemory.LoadInROM(FGlobalConfig.Modules[i].FileName,
                        FGlobalConfig.Modules[i].Address);

  bx_devices := bx_devices_c.Create;
  bx_devices.init(FGlobalConfig);
  bx_cpu.init(nil);
  InitSystem;
  InitNames;
  Gui32InitFontConfig(FGlobalConfig.FontFileName);
  InitFont;

  with _resetInfo, FGlobalConfig do
  begin
    val_EAX := Registers[crEAX];
    val_ECX := Registers[crECX];
    val_EDX := Registers[crEDX];
    val_EBX := Registers[crEBX];
    val_ESP := Registers[crESP];
    val_EBP := Registers[crEBP];
    val_ESI := Registers[crESI];
    val_EDI := Registers[crEDI];
    val_ES  := Registers[crES ];
    val_CS  := Registers[crCS ];
    val_SS  := Registers[crSS ];
    val_DS  := Registers[crDS ];
    val_FS  := Registers[crFS ];
    val_GS  := Registers[crGS ];
    val_DR0 := Registers[crDR0];
    val_DR1 := Registers[crDR1];
    val_DR2 := Registers[crDR2];
    val_DR3 := Registers[crDR3];
    val_DR6 := Registers[crDR6];
    val_DR7 := Registers[crDR7];
    val_CR0 := Registers[crCR0];
    val_CR1 := Registers[crCR1];
    val_CR2 := Registers[crCR2];
    val_CR3 := Registers[crCR3];
    val_CR4 := Registers[crCR4];
    val_EIP := Registers[crEIP];
  end;

  bx_cpu.reset(@_resetInfo);

  InitializeCriticalSection(FakeCS);
  InitializeCriticalSection(drawCS);
  InitializeCriticalSection(KeyCS);
  InitializeCriticalSection(MouseCS);
end;

destructor TVDebugThread.Destroy;
begin
  // разрушение ядра эмулятора
  DeleteCriticalSection(MouseCS);
  DeleteCriticalSection(drawCs);
  DeleteCriticalSection(KeyCs);
  DeleteCriticalSection(FakeCs);
  TerminateEmul;
  Gui32Stop;
  sysmemory.Free;
  bx_cpu.Free;
  bx_pc_system.Free;
  bx_devices.Free;
  inherited;
end;

procedure TVDebugThread.DoTerminate;
begin
  Synchronize(__OnStopEmulator);
  inherited;
end;

procedure TVDebugThread.Execute;
begin
  inherited;
  try
    try
      Synchronize(__OnStartEmulator);
      bx_cpu.Active := true;
      bx_cpu.cpu_loop;
    except
      on e: Exception do
      begin
  //      fMain.File1.Enabled := True;
        LogPanic(e.Message);
      end;
    end;
  finally
    Terminate;
  end;
end;

procedure TVDebugThread.OnCPUStep(aInstruction: Pointer);
var
  _stop: boolean;
  _instr: PInstruction_tag absolute aInstruction;
begin
  case FDebugMode of
    dmStop: ;
    dmRun: ;
    dmAnimate, dmStepOver, dmStepInto:
      begin
        if FNextEIP = 0 then
        begin
          // $2E0, $2E1, $2E2, $2E8, $F2, $F3
          if _instr^.b1 in [$E0, $E1, $E2, $E8, $F2, $F3] then
            FNextEIP := bx_cpu.EIP + _instr^.ilen
          else
            FNextEIP := 0;
        end;

        if (FDebugMode = dmStepInto) or (FDebugMode = dmStepOver) then
        begin
          if (FDebugMode = dmStepInto) or (FNextEIP = 0) or
             ((FDebugMode = dmStepOver) and (FNextEIP = bx_cpu.EIP)) then
          begin
            if (FDebugMode <> dmStepInto) then
              FNextEIP := 0;
            Synchronize(__OnCommandStep);
            suspend;
          end;
        end else
          Synchronize(__OnCommandStep);
      end;
  end;
end;

procedure TVDebugThread.Pause;
begin
  FDebugMode := dmStepOver;
end;

procedure TVDebugThread.Run;
begin
  FDebugMode := dmRun;
  Resume;
end;

procedure TVDebugThread.StepInto;
begin
  FDebugMode := dmStepInto;
  Resume;
end;

procedure TVDebugThread.StepOver;
begin
  FDebugMode := dmStepOver;
  Resume;
end;

procedure TVDebugThread.Stop;
begin
  bx_cpu.Active := False;
  FDebugMode := dmStop;
end;

procedure TVDebugThread.__OnCommandStep;
begin
  if Assigned(FOnCommandStep) then
    FOnCommandStep(Self);
end;

procedure TVDebugThread.__OnStartEmulator;
begin
  if Assigned(FOnStartEmulator) then
    FOnStartEmulator(Self);
end;

procedure TVDebugThread.__OnStopEmulator;
begin
  if Assigned(FOnStopEmulator) then
    FOnStopEmulator(Self);
end;

{ TTVDbgLogThread }

procedure TTVDbgLogThread.Execute;
begin
  inherited;
  while not Terminated do
  begin
    if FCanPush then
      Synchronize(PushLog);
    sleep(100);
  end;
end;

procedure TTVDbgLogThread.PushLog;
begin

end;

end.
