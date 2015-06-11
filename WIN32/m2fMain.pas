unit m2fMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ToolWin, ActnMan, ActnCtrls, ActnMenus,
  ActnList, ExtCtrls, ImgList, Config, StdCtrls, Buttons, thPerif;

const
  ErrChooseMessage = 'First choose a configuration file';

type
  TfMain1 = class (TForm)
    MainMenu1:    TMainMenu;
    File1:        TMenuItem;
    StartEmulation1: TMenuItem;
    StopEmulation1: TMenuItem;
    Em1:          TMenuItem;
    DlgOpen:      TOpenDialog;
    MainBar:      TStatusBar;
    ChangeConfiguration2: TMenuItem;
    ListImages:   TImageList;
    Exit1:        TMenuItem;
    EnableMouse1: TMenuItem;
    N3:           TMenuItem;
    N4:           TMenuItem;
    N5:           TMenuItem;
    N6:           TMenuItem;
    N7:           TMenuItem;
    N8:           TMenuItem;
    N9:           TMenuItem;
    N10:          TMenuItem;
    N11:          TMenuItem;
    N1:           TMenuItem;
    N12:          TMenuItem;
    N13:          TMenuItem;
    N14:          TMenuItem;
    N15:          TMenuItem;
    N16:          TMenuItem;
    N17:          TMenuItem;
    Reset1: TMenuItem;
    N2: TMenuItem;
    Readonly1: TMenuItem;
    Close1: TMenuItem;
    N18: TMenuItem;
    N19: TMenuItem;
    dlgSave: TSaveDialog;
    N20: TMenuItem;
    tmrMouse: TTimer;
    procedure FormCreate (Sender: TObject);
    procedure StartEmulation1Click (Sender: TObject);
    procedure StopEmulation1Click (Sender: TObject);
    procedure MainBarDrawPanel (StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure Exit1Click (Sender: TObject);
    procedure EnableMouse1Click (Sender: TObject);
    procedure ChangeConfiguration2Click (Sender: TObject);
    procedure FormClose (Sender: TObject; var Action: TCloseAction);
    procedure N6Click (Sender: TObject);
    procedure N8Click (Sender: TObject);
    procedure N7Click (Sender: TObject);
    procedure N9Click (Sender: TObject);
    procedure N13Click (Sender: TObject);
    procedure N15Click (Sender: TObject);
    procedure N16Click (Sender: TObject);
    procedure N17Click (Sender: TObject);
    procedure Reset1Click(Sender: TObject);
    procedure Readonly1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure N19Click(Sender: TObject);
    procedure tmrMouseTimer(Sender: TObject);
    procedure N20Click(Sender: TObject);
  private
    WorkDir, EmulDir: string;
    StopEvent:        THandle;
    THddOn, THddOff:  TBitmap;
    //THddLed:          TThPerif;
    LedHddVisible:    boolean;
    workPoint: TPoint;
    workKeyData: Cardinal;
    procedure InitVars;
    procedure LoadMenuEmuls;
    procedure CreateListItem (const AName: string);
  public
    procedure WMPaint (var Msg: TWMPaint); message WM_PAINT;
    procedure WMKeyDown (var Msg: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp (var Msg: TWMKeyUp); message WM_KEYUP;
    procedure CM_DIALOGKEY (var Message: TWMKey); message CM_DIALOGKEY;
    procedure WMSysKeyDown (var Msg: TWMSysKeyDown); message WM_SYSKEYDOWN;
    procedure WMSysKeyUp (var Msg: TWMSysKeyUp); message WM_SYSKEYUP;
    procedure WMChar (var Msg: TWMChar); message WM_CHAR;
    procedure WMMouseMouse (var Msg: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMMouseDblClick(var msg: TWMMouseMove); message WM_LBUTTONDBLCLK;
    procedure WMLMouseDown (var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLMouseUp (var Msg: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMRMouseDown (var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRMouseUp (var Msg: TWMRButtonUp); message WM_RBUTTONUP;
    procedure wmmenuinfo(var msg: TWMMenuSelect); //message WM_MENUSELECT;
    procedure LoadVMConfig;
    procedure SaveVMConfig;
  end;

  TMessageThread = class (TThread)
    procedure Execute; override;
  end;

const
  MAX_TIME_WAIT = 5000;

var
  fMain1:    TfMain1;
  KT:       TMessageThread;
  VMConfig: TConf;

  IsStarted: boolean;

implementation

uses Gui32, cpu, iodev, Service, Memory, IniFiles,
  CreateIsoForm, frmConfig, floppy, HDD, cdrom;


{$R *.dfm}

type
  TEmulThread = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

{ TfMain }


procedure TfMain1.LoadVMConfig;
begin
  LoadConfigFile(WorkDir + 'config\vm.ini', @VMConfig);
  SaveConfigFile(WorkDir + 'config\vm.ini', @VMConfig);
end;

procedure TfMain1.SaveVMConfig;
begin
  SaveConfigFile(WorkDir + 'config\vm.ini', @VMConfig);
end;

procedure TfMain1.InitVars;
begin
  WorkDir := ExtractFilePath(Application.ExeName);
  if not DirectoryExists(WorkDir + 'logs') then
    MkDir(WorkDir + 'logs');
  IsStarted := False;
end;

procedure TfMain1.FormClose (Sender: TObject; var Action: TCloseAction);
begin
  SaveVMConfig;
  StopEmulation1Click(sender);
  Application.Terminate;
end;

procedure TfMain1.FormCreate (Sender: TObject);
begin
  THddOn := TBitmap.Create;
  ListImages.GetBitmap(0, THddOn);
  THddOff := TBitmap.Create;
  InitVars;

  if not DirectoryExists(workdir + 'disks') then
    ForceDirectories(workdir + 'disks');

  if not DirectoryExists(workdir + 'snaps') then
    ForceDirectories(workdir + 'snaps');

  if not DirectoryExists(workdir + 'config') then
    ForceDirectories(workdir + 'config');

  LoadMenuEmuls;
  LoadVMConfig;

  workPoint.X := 0;
  workPoint.Y := 0;
  workKeyData := 0;
end;

procedure LedHdd (OnOff: boolean);
begin
  TryEnterCriticalSection(FakeCS);
//  fMain.LedHddVisible := OnOff;
//  fMain.MainBar.Refresh;
  LeaveCriticalSection(FakeCS);
end;

procedure TfMain1.StartEmulation1Click (Sender: TObject);
var
  I: integer;
  thread: TEmulThread;
begin
  MouseEnabled := False;

  if IsStarted then
    exit;

  LoadVMConfig;

  for I := 0 to 1028 do
    BxOpcodeInfo[I].Counter := 0;

  File1.Enabled := False;
  MainWnd := Handle;
  InitLogFiles;
  SetConfiguration(@VMConfig);
  //stoprun := False;
  LastMessage := '';
  ips_count := 0;
  m_ips := 0;
  LedHddVisible := False;

  Gui32Init(MainWnd, 0);
  thread := TEmulThread.Create;
  thread.Resume;
//  try
//    //THddLed := TThPerif.Init(LedHdd);
//    Gui32Init(MainWnd, 0);
//    bx_cpu := TCPU.Create;
//    bx_devices := bx_devices_c.Create;
//
//    bx_devices.init(nil, @VMConfig);
//    bx_cpu.init(nil);
//    InitSystem;
//    InitNames;
//    InitFont;
//    IsStarted := True;
//    bx_cpu.reset(0);
//    InitializeCriticalSection(FakeCS);
//    InitializeCriticalSection(drawCS);
//    InitializeCriticalSection(KeyCS);
//    InitializeCriticalSection(MouseCS);
//    StartEvent := CreateEvent(nil, False, False, nil);
//
//    try
//      try
//        bx_cpu.cpu_loop;
//      except
//        on e: Exception do
//        begin
//          fMain.File1.Enabled := True;
//          LogPanic(e.Message);
//        end;
//      end;
//    finally
//      DeleteCriticalSection(MouseCS);
//      DeleteCriticalSection(drawCs);
//      DeleteCriticalSection(KeyCs);
//      DeleteCriticalSection(FakeCs);
//      TerminateEmul;
//      Gui32Stop;
//      sysmemory.Free;
//      bx_cpu.Free;
//      bx_pc_system.Free;
//      bx_devices.Free;
//      SetEvent(fMain.StopEvent);
//    end;
//
//  finally
//    stoprun := True;
//    WaitForSingleObject(StopEvent, MAX_TIME_WAIT);
//    Invalidate;
//    File1.Enabled := True;
//    IsStarted  := False;
//  end;
end;

procedure TfMain1.WMPaint (var Msg: TWMPaint);
var
  ps: PAINTSTRUCT;
  VideoDC, MemDC: HDC;
  OldObject: THandle;
begin
// Отрисовка букв в модулях vga (строка 1727), Gui32 (text_update, DrawChar)
  if IsStarted then
  begin
    EnterCriticalSection(DrawCS);
    VideoDC := BeginPaint(Self.Handle, ps);
    MemDC := CreateCompatibleDC(VideoDC);

    OldObject := SelectObject(MemDC, MemoryBitmap);

    StretchBlt(VideoDC, ps.rcPaint.left, ps.rcPaint.top, ps.rcPaint.right -
       ps.rcPaint.left + 1,
       ps.rcPaint.bottom - ps.rcPaint.top + 1, MemDC,
       ps.rcPaint.left div stretch_factor, ps.rcPaint.top div stretch_factor,
      (ps.rcPaint.right - ps.rcPaint.left + 1) div stretch_factor,
      (ps.rcPaint.bottom - ps.rcPaint.top + 1) div stretch_factor, SRCCOPY);

    SelectObject(MemDC, OldObject);

    DeleteDC(MemDC);
    EndPaint(Self.Handle, ps);
    leavecriticalsection(DrawCs);
    Msg.Result := 0;
  end
  else
  begin
    inherited;
    Msg.Result := 1;
  end;
end;

procedure TfMain1.WMRMouseDown(var Msg: TWMRButtonDown);
begin
  if MouseEnabled then
  begin
    WMMouseMouse(Msg);
    Msg.Result := 0;
//    workKeyData := Msg.Keys;
//    processMouseXY(workPoint.X, workPoint.Y, Msg.Keys, 1);
  end;
  inherited;
end;

procedure TfMain1.WMRMouseUp(var Msg: TWMRButtonUp);
begin
  if MouseEnabled then
  begin
    WMMouseMouse(Msg);
    Msg.Result := 0;
//    workKeyData := Msg.Keys;
//    processMouseXY(workPoint.X, workPoint.Y, Msg.Keys, 1);
  end;
  inherited;
end;

{ TRunner }

procedure TfMain1.StopEmulation1Click (Sender: TObject);
begin
  Halt;
//  if IsStarted then
//    StopEmulation;
end;

procedure TfMain1.tmrMouseTimer(Sender: TObject);
var
  mpos: TPoint;
  old: TPoint;
begin
  if not MouseEnabled then
    Exit;

  old.x := Width div 2;
  old.Y := Height div 2;
  old := ClientToScreen(old);

  SetCursorPos(old.X, old.y);
end;

procedure TfMain1.WMKeyDown (var Msg: TWMKeyDown);
begin
  if IsStarted then
  begin
    if MouseEnabled then
    begin
      // отрубание режима мыши
      if (Msg.KeyData = 22740993) and (Msg.CharCode = 91) then
      begin
        MouseEnabled := false;
        tmrMouse.Enabled := false;
        ShowCursor(true);
      end;
    end;
    EnterCriticalSection(KeyCS);
    enq_key_event(HiWord(Msg.KeyData) and $01FF, BX_KEY_PRESSED);
    Msg.KeyData  := 0;
    Msg.CharCode := 0;
    leavecriticalsection(KeyCs);
    Msg.Result := 0;
  end
  else
    inherited;
end;

procedure TfMain1.WMKeyUp (var Msg: TWMKeyUp);
begin
  if IsStarted then
  begin
    EnterCriticalSection(KeyCS);
    enq_key_event(HiWord(Msg.KeyData) and $01FF, BX_KEY_RELEASED);
    Msg.KeyData  := 0;
    Msg.CharCode := 0;
    leavecriticalsection(KeyCs);
    Msg.Result := 0;
  end
  else
    inherited;
end;


procedure TfMain1.wmmenuinfo(var msg: TWMMenuSelect);
//  procedure SetStatusHint(const aHint: string);
//  begin
//    Self.MainBar.Panels[0].Text := aHint;
//  end;
begin
//  case msg.IDItem of
//    SC_CLOSE: SetStatusHint('Закрыть');
//    SC_MINIMIZE: SetStatusHint('Свернуть');
//    SC_RESTORE: SetStatusHint('Восстановить');
//    SC_MAXIMIZE: SetStatusHint('Развернуть');
//    SC_SIZE: SetStatusHint('Размер');
//    SC_MOVE: SetStatusHint('Переместить');
//    else SetStatusHint('');
//  end;
end;

procedure TfMain1.Close1Click(Sender: TObject);
begin
  HardDrive.close_harddrive;
end;

procedure TfMain1.CM_DIALOGKEY (var Message: TWMKey);
begin
  if IsStarted and (((Message.KeyData shr 16) and $01FF) = 15) then
  begin
    // Kill Delphi TAB handler event
    EnterCriticalSection(KeyCS);
    Message.KeyData := 0;
    Message.CharCode := 0;
    Message.Result := 0;
    leavecriticalsection(KeyCs);
  end
  else
    inherited;
end;

procedure TfMain1.WMSysKeyDown (var Msg: TWMSysKeyDown);
begin
  if IsStarted then
  begin
    EnterCriticalSection(KeyCS);
    enq_key_event(HiWord(Msg.KeyData) and $01FF, BX_KEY_PRESSED);
    Msg.KeyData  := 0;
    Msg.CharCode := 0;
    leavecriticalsection(KeyCs);
    Msg.Result := 0;
  end
  else
    inherited;
end;

procedure TfMain1.WMSysKeyUp (var Msg: TWMSysKeyUp);
begin
  if IsStarted then
  begin
    EnterCriticalSection(KeyCS);
    enq_key_event(HiWord(Msg.KeyData) and $01FF, BX_KEY_RELEASED);
    Msg.KeyData  := 0;
    Msg.CharCode := 0;
    leavecriticalsection(KeyCs);
    Msg.Result := 0;
  end
  else
    inherited;
end;


procedure TfMain1.WMChar (var Msg: TWMChar);
begin
  if IsStarted then
    Msg.Result := 0
  else
    inherited;
end;

procedure TfMain1.ChangeConfiguration2Click (Sender: TObject);
var
  CF: TConfigForm;
begin
  CF := TConfigForm.Create(nil);
  CF.ShowModal;
  CF.Free;
end;


procedure TfMain1.WMMouseMouse (var Msg: TWMMouseMove);
const
  MOUSE_SPEED = 2;
var
  mpos: TPoint;
  old: TPoint;
  i: Integer;
begin
  if MouseEnabled then
  begin

    old.x := Width div 2;
    old.Y := Height div 2;
    old := ClientToScreen(old);

    GetCursorPos(mpos);
    i := Integer(Msg.Keys <> 0);

    if ((mpos.x-old.X) <> 0) or ((old.Y-mpos.y) <> 0)then
    begin
      workPoint.X := Round(workPoint.X + (mpos.x-old.X) / MOUSE_SPEED);
      workPoint.Y := Round(workPoint.Y - (old.Y-mpos.y) / MOUSE_SPEED);
    //    processMouseXY(workPoint.X, workPoint.Y, Msg.Keys, 0);
    end;
    processMouseXY(workPoint.X, workPoint.Y, Msg.Keys, i);
    Msg.Result := 0;
  end else
  inherited;
end;

procedure TfMain1.WMMouseDblClick(var msg: TWMMouseMove);
begin
  if MouseEnabled then
  begin
    WMLMouseDown(msg);
    WMLMouseDown(msg);
  end;
  inherited;
end;

procedure TfMain1.WMLMouseDown (var Msg: TWMLButtonDown);
begin
  if MouseEnabled then
  begin
    WMMouseMouse(Msg);
    Msg.Result := 0;
//    workKeyData := Msg.Keys;
//    processMouseXY(workPoint.X, workPoint.Y, Msg.Keys, 1);
  end;
  inherited;
end;

procedure TfMain1.WMLMouseUp (var Msg: TWMLButtonUp);
begin
  if MouseEnabled then
  begin
    WMMouseMouse(Msg);
    Msg.Result := 0;
//    workKeyData := Msg.Keys;
//    processMouseXY(workPoint.X, workPoint.Y, Msg.Keys, 1);
  end;
  inherited;
end;

{ TMessageThread }

procedure TMessageThread.Execute;
begin
  while not Application.Terminated do
    Sleep(100);
//    Synchronize(Application.ProcessMessages);
end;

procedure TfMain1.MainBarDrawPanel (StatusBar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
begin
  if Panel.Index = 1 then
  begin
    if LedHddVisible then
      StatusBar.Canvas.Draw(Rect.Left, Rect.Top, THddOn)
    else
      StatusBar.Canvas.FillRect(Rect);
  end;
end;

procedure TfMain1.N13Click (Sender: TObject);
var
  od: TOpenDialog;
begin
  if not Assigned(bx_floppy) then
    exit;

  od := TOpenDialog.Create(nil);
  od.Filter := '*.img|*.img|*.iso|*.iso|*.ima|*.ima';
  if od.Execute then
  begin
    VMConfig.FloppyAFile := od.FileName;
    SetConfiguration(@VMConfig);
    SaveVMConfig;
    bx_floppy.evaluate_media(vmconfig.FloppyAType, PChar(vmconfig.FloppyAFile),
      @bx_floppy.s.media[0]);
    bx_floppy.s.media_present[0] := 1;
  end;
  od.Free;
end;

procedure TfMain1.N15Click (Sender: TObject);
var
  od: TOpenDialog;
begin
  if (not Assigned(HardDrive)) or (BX_CDROM_PRESENT <> 1) then
    exit;
  if HardDrive.s[1].cdrom.ready <> 0 then
  begin
    ShowMessage('Сначала извлеките компакт-диск!');
    exit;
  end;

  od := TOpenDialog.Create(nil);
  od.Filter := '*.img|*.img|*.iso|*.iso|*.ima|*.ima';

  if od.Execute then
  begin
    VMConfig.CDFile := od.FileName;
    SetConfiguration(@VMConfig);
    SaveVMConfig;

    HardDrive.s[1].cdrom.ready := 0;
    HardDrive.s[1].cdrom.cd.eject_cdrom;

    if (BX_CD_STATE = True) then
    begin
      if (HardDrive.s[1].cdrom.cd.insert_cdrom(BX_CD_FILEPATH)) then
      begin
        LogInfo(('Компакт-диск присутствует'));
        HardDrive.s[1].cdrom.ready := 1;
        HardDrive.s[1].cdrom.capacity := HardDrive.s[1].cdrom.cd.capacity();
      end
      else
      begin
        LogInfo(('Компакт-диск не обнаружен'));
        HardDrive.s[1].cdrom.ready := 0;
        //bx_options.cdromd.Oinserted^.set(BX_EJECTED);
      end;
    end;
  end;
  od.Free;
end;

procedure TfMain1.N16Click (Sender: TObject);
begin

  if (not Assigned(HardDrive)) or (BX_CDROM_PRESENT <> 1) then
    exit;

  HardDrive.s[1].cdrom.ready := 0;
  HardDrive.s[1].cdrom.cd.eject_cdrom;

end;

procedure TfMain1.N17Click (Sender: TObject);
begin
  bx_floppy.s.media_present[0] := 0;
end;

procedure TfMain1.N19Click(Sender: TObject);
var
  f: TFileStream;
begin
  if not dlgSave.Execute then
    exit;

  f := TFileStream.Create(dlgSave.FileName, fmCreate);
  f.WriteBuffer(sysmemory.vector[0], sysmemory.len);
  f.Free;
end;

procedure TfMain1.N20Click(Sender: TObject);
begin
  N20.Checked := not N20.Checked;
  bx_floppy.s.media[0].write_protected := word(N20.Checked);
end;

procedure TfMain1.N6Click (Sender: TObject);
begin
  if isStarted then
  begin
    if VMConfig.HddFile <> '' then
    begin
      if CopyFile(PChar(VMConfig.HddFile), PChar(WorkDir + 'snaps\hdd.img'),
        False) then
        ShowMessage('Снимок успещно создан')
      else
        ShowMessage('Снимок не был создан!');
    end
    else
    begin
      ShowMessage('Диск не подключен!');
    end;
  end
  else
    ShowMessage('Машина не запущена!');
end;

procedure TfMain1.N7Click (Sender: TObject);
begin
  if not isStarted then
  begin
    if VMConfig.HddFile <> '' then
    begin
      if FileExists(WorkDir + 'snaps\hdd.img') then
      begin
        if CopyFile(PChar(WorkDir + 'snaps\hdd.img'), PChar(VMConfig.HddFile),
          False) then
          ShowMessage('Диск успешно восстановлен')
        else
          ShowMessage('Диск не был восстановлен!');
      end
      else
        ShowMessage('Снимок не существует!');
    end
    else
    begin
      ShowMessage('Диск не подключен!');
    end;
  end
  else
    ShowMessage('Сначала остановите машину');
end;

procedure TfMain1.N8Click (Sender: TObject);
begin
  if isStarted then
  begin

    if VMConfig.FloppyAFile <> '' then
    begin
      if CopyFile(PChar(VMConfig.FloppyAFile), PChar(WorkDir + 'snaps\fdd.img'),
        False) then
        ShowMessage('Снимок успешно создан')
      else
        ShowMessage('Снимок не был создан!');
    end
    else
    begin
      ShowMessage('Диск не подключен!');
    end;
  end
  else
    ShowMessage('Машина не запущена!');
end;

procedure TfMain1.N9Click (Sender: TObject);
begin
  if not isStarted then
  begin
    if VMConfig.FloppyAFile <> '' then
    begin
      if FileExists(WorkDir + 'snaps\fdd.img') then
      begin
        if CopyFile(PChar(WorkDir + 'snaps\fdd.img'),
          PChar(VMConfig.FloppyAFile), False) then
          ShowMessage('Диск успещно восстановлен')
        else
          ShowMessage('Диск не был восстановлен!');
      end
      else
        ShowMessage('Снимок не существует!');
    end
    else
    begin
      ShowMessage('Диск не подключен!');
    end;
  end
  else
    ShowMessage('Сначала остановите машину');
end;

procedure TfMain1.Readonly1Click(Sender: TObject);
begin
  Readonly1.Checked := not Readonly1.Checked;
  HardDrive.s[0].hard_drive.WriteProtect := Readonly1.Checked;
  HardDrive.s[1].hard_drive.WriteProtect := Readonly1.Checked;
end;

procedure TfMain1.Reset1Click(Sender: TObject);
begin
//  StopEmulation1Click(Sender);
//  StartEmulation1Click(Sender);
  bx_pc_system.ResetSignal( PCS_SET ); (* XXX is self right? *)
  bx_cpu.reset(0);
end;

procedure TfMain1.CreateListItem (const AName: string);
begin
end;

procedure TfMain1.LoadMenuEmuls;
var
  T:  TSearchRec;
  _r: integer;
begin
  _r := FindFirst(EmulDir + '\*.ini', faANyFile, T);
  while _r = 0 do
  begin
    CreateListItem(t.Name);
    _r := FindNext(T);
  end;
  FindClose(T);
end;

procedure TfMain1.Exit1Click (Sender: TObject);
begin
  Close;
end;

procedure TfMain1.EnableMouse1Click (Sender: TObject);
begin
  MouseEnabled := True;
  tmrMouse.Enabled := true;
  ShowCursor(false);
end;

{ TEmulThread }

constructor TEmulThread.Create;
const
  MByte = 1024 * 1024;
begin
  inherited Create(true);
  FreeOnTerminate := true;

  bx_cpu := TCPU.Create;

  bx_pc_system:=TPC_System.Create;
  sysmemory:=TMEM_C.Create(MEMORYMB * MByte);
  sysmemory.load_ROM(romtype_pcbios, $f0000);
  sysmemory.load_ROM(romtype_vgabios, $c0000);

  bx_devices := bx_devices_c.Create;

  bx_devices.init(nil, @VMConfig);
  bx_cpu.init(nil);
  InitSystem;
  InitNames;
  InitFont;
  IsStarted := True;
  bx_cpu.reset(0);

  InitializeCriticalSection(FakeCS);
  InitializeCriticalSection(drawCS);
  InitializeCriticalSection(KeyCS);
  InitializeCriticalSection(MouseCS);
  StartEvent := CreateEvent(nil, False, False, nil);
end;

destructor TEmulThread.Destroy;
begin
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
  SetEvent(fMain1.StopEvent);
  inherited;
end;

procedure TEmulThread.Execute;
begin
  inherited;
  try
    try
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

end.
