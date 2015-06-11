unit DebugMain_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, Menus, vDebugCore_u, ImgList, ActnList, math;

type
  TfrmVDebug = class(TForm)
    sbMain: TStatusBar;
    clbrNavigator: TCoolBar;
    tlbGeneral: TToolBar;
    mmMain: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    btnNewWorkSpace: TToolButton;
    btnOpenWorkSpace: TToolButton;
    btnCloseWorkSpace: TToolButton;
    spr1: TToolButton;
    btnCPUStart: TToolButton;
    btnCPUPause: TToolButton;
    spr2: TToolButton;
    btnStepInto: TToolButton;
    btnStepOver: TToolButton;
    ilMain: TImageList;
    actlstDebug: TActionList;
    actNewWorkSpace: TAction;
    actOpenWorkSpace: TAction;
    actCloseWorkSpace: TAction;
    actCPUStart: TAction;
    actCPUPause: TAction;
    actStepInto: TAction;
    actStepOver: TAction;
    Debug1: TMenuItem;
    actCPUStart1: TMenuItem;
    N1: TMenuItem;
    actCPUPause1: TMenuItem;
    actStepInto1: TMenuItem;
    actStepOver1: TMenuItem;
    btnspr3: TToolButton;
    btnCPU: TToolButton;
    btnDisplay: TToolButton;
    btnVGA: TToolButton;
    actCPU: TAction;
    actDisplay: TAction;
    actVGA: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HandleActionsClick(Sender: TObject);
    procedure ChangeWindowsVisible(Sender: TObject);
  private
    { Private declarations }
    procedure OnStartEmulator(Sender: TObject);
    procedure OnStopEmulator(Sender: TObject);
    procedure OnCPUStep(Sender: TObject);
    //
    procedure NewWorkSpaceProc; inline;
  public
    DBGCore: TVDebugThread;
    procedure AfterConstruction; override;
    { Public declarations }
  end;

var
  frmVDebug: TfrmVDebug;

implementation

{$R *.dfm}

uses
  CPUWindow_u, Display_u, WorkSpaceWiz_u, WorkSpaceWizLogic_u, VGAWindow_u,
  CONFIG;

const
  BTN_NEW_WORKSPACE = 1;
  BTN_OPEN_WORKSPACE = 2;
  BTN_CLOSE_WORKSPACE = 3;
  BTN_CPU_START = 4;
  BTN_CPU_PAUSE = 5;
  BTN_STEP_INTO = 6;
  BTN_STEP_OVER = 7;

procedure TfrmVDebug.AfterConstruction;
begin
  inherited;
  actNewWorkSpace.Tag := BTN_NEW_WORKSPACE;
  actOpenWorkSpace.Tag := BTN_OPEN_WORKSPACE;
  actCloseWorkSpace.Tag := BTN_CLOSE_WORKSPACE;
  actCPUStart.Tag := BTN_CPU_START;
  actCPUPause.Tag := BTN_CPU_PAUSE;
  actStepInto.Tag := BTN_STEP_INTO;
  actStepOver.Tag := BTN_STEP_OVER;
end;

procedure TfrmVDebug.ChangeWindowsVisible(Sender: TObject);
var
  wnd: HWND;
  vis: boolean;
begin
  wnd := INVALID_HANDLE_VALUE;
  case (Sender as TAction).Tag of
    1: if CPUform <> nil then wnd := CPUform.Handle;
    2: if Displayform <> nil then wnd := Displayform.Handle;
    3: if VGAform <> nil then wnd := VGAform.Handle;
  end;
  if wnd <> INVALID_HANDLE_VALUE then
  begin
    vis := IsWindowVisible(wnd);
    ShowWindow(wnd, IfThen(vis, SW_HIDE, SW_SHOW));
  end;
end;

procedure TfrmVDebug.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  OnStopEmulator(DBGCore);
end;

procedure TfrmVDebug.HandleActionsClick(Sender: TObject);
begin
  case (Sender as TAction).Tag of
    BTN_NEW_WORKSPACE:
      begin
        NewWorkSpaceProc;
      end;
    BTN_OPEN_WORKSPACE:
      begin

      end;
    BTN_CLOSE_WORKSPACE:
      begin
        DBGCore.Stop;
        DBGCore.Terminate;
        //
        actCPUStart.Enabled := False;
        actCPUPause.Enabled := False;
        actStepInto.Enabled := False;
        actStepOver.Enabled := False;
      end;
    BTN_CPU_START:
      begin
        DBGCore.Run;
        // при обычном запуске лочим кнопки запуска и пошаговой трассировки, разлочиваем паузу
        actCPUStart.Enabled := False;
        actCPUPause.Enabled := True;
        actStepInto.Enabled := False;
        actStepOver.Enabled := False;
      end;
    BTN_CPU_PAUSE:
      begin
        DBGCore.Pause;
        // когда ставм на паузу, разлочиваем все, а паузу лочим
        actCPUStart.Enabled := True;
        actCPUPause.Enabled := False;
        actStepInto.Enabled := True;
        actStepOver.Enabled := True;
      end;
    BTN_STEP_INTO:
      begin
        DBGCore.StepInto;
      end;
    BTN_STEP_OVER:
      begin
        DBGCore.StepOver;
      end;
  end;
//  // показать окно с настройками
//  DBGCore := TVDebugThread.Create;
//  DBGCore.OnStartEmulator := OnStartEmulator;
//  DBGCore.OnStopEmulator := OnStopEmulator;
//  DBGCore.OnCommandStep := OnCPUStep;
//
//  btnCloseWorkSpace.Enabled := true;
//  btnCPUStart.Enabled := true;
//  btnStepInto.Enabled := True;
//  btnStepOver.Enabled := True;

//  DBGCore.StepInto;
//  btnStepInto.Enabled := False;
//  btnStepOver.Enabled := False;

//DBGCore.Pause;
//  DBGCore.Run;

//  DBGCore.Terminate;
//  DBGCore := nil;
//  btnCPUStart.Enabled := False;
//  btnStepInto.Enabled := False;
//  btnStepOver.Enabled := False;
//  btnCPUPause.Enabled := False;
end;

procedure TfrmVDebug.NewWorkSpaceProc;
var
  _workspace: TWorkSpaceConfig;
  _dir: string;
begin
  with _workspace do
  begin
    _dir := ExtractFilePath(paramstr(0));
    WorkSpaceFileName := _dir + 'Projects\NewWorkSpace.dws';
    WorkSpaceComment := 'New workspace';

    MemorySize := 128;
    SetLength(Modules, 2);
    Modules[0].FileName := _dir + 'bios\pc_bios.dat';
    Modules[0].Address := $F0000;
    Modules[1].FileName := _dir + 'bios\vga_bios.dat';
    Modules[1].Address := $C0000;

    FontFileName := _dir + 'bios\font.dat';

    SetLength(Devices, 3);
    Devices[0].DeviceType := dFDD;
    Devices[0].DeviceFileName := 'disks\win311.img';
    Devices[0].DeviceReadOnly := False;
    Devices[1].DeviceType := dFDD;
    Devices[1].DeviceFileName := 'disks\boot_dos.img';
    Devices[1].DeviceReadOnly := False;

    Devices[2].DeviceType := dHDD;
    Devices[2].DeviceFileName := 'disks\hdd256mb.img';
    Devices[2].DeviceReadOnly := False;
    Devices[2].DiscConfig.Cyl := 521;
    Devices[2].DiscConfig.Heads := 16;
    Devices[2].DiscConfig.Sectors := 63;

    //Devices[3].DeviceType := dCDROM;

    Registers[crEAX] := 0;
    Registers[crECX] := 0;
    Registers[crEDX] := 0;
    Registers[crEBX] := 0;
    Registers[crESP] := 0;
    Registers[crEBP] := 0;
    Registers[crESI] := 0;
    Registers[crEIP] := $0000FFF0;
    Registers[crES] := 0;
    Registers[crCS] := $f000;
    Registers[crSS] := 0;
    Registers[crDS] := 0;
    Registers[crFS] := 0;
    Registers[crGS] := 0;
    Registers[crCR0] := 0;
    Registers[crCR1] := 0;
    Registers[crCR2] := 0;
    Registers[crCR3] := 0;
    Registers[crCR4] := 0;
    Registers[crDR0] := 0;
    Registers[crDR1] := 0;
    Registers[crDR2] := 0;
    Registers[crDR3] := 0;
    Registers[crDR6] := $FFFF0FF0;
    Registers[crDR7] := $00000400;

    CPUSpeed := CPU_SPEED;
    CPUName := 'CPU1';

    // диски
    Boot := dHDD;
    UseFPU := False;
  end;
  if TWorkSpaceWizform.NewWorkspace(Self, _workspace) then
  begin
    // сохраним в указанную папку и скопируем туда всякие файлы
    DBGCore := TVDebugThread.Create(_workspace);
    DBGCore.OnStartEmulator := OnStartEmulator;
    DBGCore.OnStopEmulator := OnStopEmulator;
    DBGCore.OnCommandStep := OnCPUStep;
    // разлочим кнопки
    actCPUStart.Enabled := True;
    actCPUPause.Enabled := False;
    actStepInto.Enabled := True;
    actStepOver.Enabled := True;
  end;
end;

procedure TfrmVDebug.OnCPUStep(Sender: TObject);
begin
  if CPUform <> nil then
    CPUform.UpdateInfo;
  if VGAform <> nil then
    VGAform.UpdateInfo;
end;

procedure TfrmVDebug.OnStartEmulator(Sender: TObject);
begin
  if CPUform <> nil then
    CPUform.InitInfo;
  if Displayform <> nil then
  begin
    Displayform.InitVideo;
    Displayform.Start;
  end;
  if VGAform <> nil then
    VGAform.InitInfo;
end;

procedure TfrmVDebug.OnStopEmulator(Sender: TObject);
begin
  if CPUform <> nil then
    CPUform.DeleteInfo;
  if Displayform <> nil then
    Displayform.Stop;
  if VGAform <> nil then
    VGAform.ReleaseInfo;
end;

end.
