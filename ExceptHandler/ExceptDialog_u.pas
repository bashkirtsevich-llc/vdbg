unit ExceptDialog_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, JclSysInfo, jclDebug, ShellAPI;

type
  TGetInfoProc = procedure (const aFieldName, aFieldValue: string;
    aReserved: array of string) of object;

  TExceptionObject = class
  private
    FException: Exception;
    FExceptionAddr: DWORD;
    FCurrentPID: DWORD;
    //
    FTime: string;
    // user
    FUserName: string;
    FDomainName: string;
    FWorkGroup: string;
    FRegisteredCompany: string;
    FRegisteredOwner: string;
    FComputerName: string;
    // OS
    FWindowsVersion: TWindowsVersion;
    FWindowsEdition: TWindowsEdition;
    FNtProductType: TNtProductType;
    FWindowsVersionString: string;
    FWindowsEditionString: string;
    FWindowsProductString: string;
    FNtProductTypeString: string;
    FWindowsServicePackVersion: Integer;
    FWindowsServicePackVersionString: string;
    FNativeSystemInfo: TSystemInfo;
    FProcessorArchitecture: TProcessorArchitecture;
    FIsWindows64: Boolean;
    // modules
    FModules: TStringList;
    // processes
    FProcesses: TStringList;
    // tasks
    FTasks: TStringList;
    //
    FStackTrace: TStringList;
    //
    FThreads: TStringList;
    FDump: string;
  public
    procedure GetGeneralInfo(aProc: TGetInfoProc);
    procedure GetProcesses(aProc: TGetInfoProc);
    procedure GetTasks(aProc: TGetInfoProc);
    procedure GetModules(aProc: TGetInfoProc);
    procedure GetStackTrace(aProc: TGetInfoProc);
    procedure GetThreads(aProc: TGetInfoProc);
    function GetDump: string;

    function AsString: string;

    constructor Create(aException: Exception; aExceptAddr: dword;
      aStackTrace: TStringList; aThreads: TStringList; const aDump: string);
    destructor Destroy;
  end;

  TExceptionDialog = class(TForm)
    imgErrorIcon: TImage;
    lblExceptionmessage: TLabel;
    btnContinue: TButton;
    btnRestart: TButton;
    btnClose: TButton;
    btnSendReport: TButton;
    btnDetails: TButton;
    pgcMain: TPageControl;
    tsGeneral: TTabSheet;
    tsModules: TTabSheet;
    tsTasks: TTabSheet;
    tsProcesses: TTabSheet;
    tsThreads: TTabSheet;
    lvGeneral: TListView;
    lvProcesses: TListView;
    tsStackTrace: TTabSheet;
    tsDump: TTabSheet;
    lvTasks: TListView;
    lvModules: TListView;
    lvStackTrace: TListView;
    lvThreads: TListView;
    mmoDump: TMemo;
    lblCopyright: TLabel;
    procedure OnBtnsClick(Sender: TObject);
    procedure btnDetailsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    FDialogResult: Integer;
    FCanContinue: Boolean;
    procedure OnAddReportGeneralInfo(const aFieldName, aFieldValue: string;
      aReserved: array of string);
    procedure OnAddReportProcessesInfo(const aFieldName, aFieldValue: string;
      aReserved: array of string);
    procedure OnAddReportTaskInfo(const aFieldName, aFieldValue: string;
      aReserved: array of string);
    procedure OnAddReportModuleInfo(const aFieldName, aFieldValue: string;
      aReserved: array of string);
    procedure OnAddReportStackTraceInfo(const aFieldName, aFieldValue: string;
      aReserved: array of string);
    procedure OnAddReportThreadsInfo(const aFieldName, aFieldValue: string;
      aReserved: array of string);

    procedure SetCanContinue(aValue: Boolean);
  public
    { Public declarations }
    property CanContinue: Boolean read FCanContinue write SetCanContinue;
    function ShowBugReport(aReport: TExceptionObject): integer;
    procedure SendReport(aReport: TExceptionObject);
    procedure AfterConstruction; override;
  end;

const
  EXCEPT_CONTINE = 1;
  EXCEPT_RESTART = 2;
  EXCEPT_TERMINATE = 3;
  EXCEPT_SEND_REPORT = 4;

implementation

{$R *.dfm}

procedure TExceptionDialog.AfterConstruction;
begin
  inherited;
  FCanContinue := True;
  FDialogResult := -1;
  EnableMenuItem(GetSystemMenu(Handle, False), SC_CLOSE, 1);
  imgErrorIcon.Picture.Icon.Handle := LoadIcon(0, IDI_HAND);
  pgcMain.Visible := False;
  ClientHeight := pgcMain.Top + lblCopyright.Height + 8;
end;

procedure TExceptionDialog.btnDetailsClick(Sender: TObject);
begin
  // показать подробности
  pgcMain.Visible := not pgcMain.Visible;
  if pgcMain.Visible then
    ClientHeight := pgcMain.Top + pgcMain.Height + lblCopyright.Height + 8
  else
    ClientHeight := pgcMain.Top + lblCopyright.Height + 8;
end;

procedure TExceptionDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := FDialogResult <> -1;
end;

procedure TExceptionDialog.OnAddReportGeneralInfo(const aFieldName,
  aFieldValue: string; aReserved: array of string);
begin
  with lvGeneral.Items.Add do
  begin
    Caption := aFieldName;
    SubItems.Add(aFieldValue);
  end;
end;

procedure TExceptionDialog.OnAddReportModuleInfo(const aFieldName,
  aFieldValue: string; aReserved: array of string);
begin
  with lvModules.Items.Add do
  begin
    Caption := aFieldName;
    SubItems.Add(aFieldValue);
  end;
end;

procedure TExceptionDialog.OnAddReportProcessesInfo(const aFieldName,
  aFieldValue: string; aReserved: array of string);
begin
  with lvProcesses.Items.Add do
  begin
    Caption := aFieldName;
    SubItems.Add(aFieldValue);
  end;
end;

procedure TExceptionDialog.OnAddReportStackTraceInfo(const aFieldName,
  aFieldValue: string; aReserved: array of string);
begin
  with lvStackTrace.Items.Add do
  begin
    Caption := aFieldName;
    SubItems.Add(aFieldValue)
  end;
end;

procedure TExceptionDialog.OnAddReportTaskInfo(const aFieldName,
  aFieldValue: string; aReserved: array of string);
begin
  with lvTasks.Items.Add do
  begin
    Caption := aFieldName;
  end;
end;

procedure TExceptionDialog.OnAddReportThreadsInfo(const aFieldName,
  aFieldValue: string; aReserved: array of string);
begin
  with lvThreads.Items.Add do
  begin
    Caption := aFieldName;
  end;
end;

procedure TExceptionDialog.OnBtnsClick(Sender: TObject);
begin
  case (Sender as TButton).Tag of
    1: FDialogResult := EXCEPT_CONTINE;
    2: FDialogResult := EXCEPT_RESTART;
    3: FDialogResult := EXCEPT_TERMINATE;
    4: FDialogResult := EXCEPT_SEND_REPORT;
  end;
  Close;
end;

procedure TExceptionDialog.SendReport(aReport: TExceptionObject);
var
  list: TStringList;
  _path, _fname: string;
begin
  list := TStringList.Create;
  try
    list.Text := aReport.AsString;
    list.SaveToFile('bugreport.txt');
    _path := IncludeTrailingPathDelimiter(GetCurrentDir);
    _fname := _path + 'bugreport.txt';
    ShellExecute(0, 'open', 'notepad.exe', PChar(_fname), PChar(_path), SW_SHOWNORMAL);
  finally
    list.Free;
  end;
end;

procedure TExceptionDialog.SetCanContinue(aValue: Boolean);
begin
  if FCanContinue = aValue then Exit;
  FCanContinue := aValue;
  btnContinue.Enabled := aValue;
end;

function TExceptionDialog.ShowBugReport(aReport: TExceptionObject): integer;
begin
  if aReport <> nil then
  begin
    aReport.GetGeneralInfo(OnAddReportGeneralInfo);
    aReport.GetProcesses(OnAddReportProcessesInfo);
    aReport.GetTasks(OnAddReportTaskInfo);
    aReport.GetModules(OnAddReportModuleInfo);
    aReport.GetStackTrace(OnAddReportStackTraceInfo);
    aReport.GetThreads(OnAddReportThreadsInfo);
    mmoDump.Text := aReport.GetDump;
    ShowModal;
    Result := FDialogResult;
  end;
end;

{ TExceptionObject }

function TExceptionObject.AsString: string;
  procedure Add(const a1, a2: string);
  begin
    Result := Result + Format('%s:  '#9#9 + '%s'#13#10, [a1, a2]);
  end;
var
  i: Integer;
  s, _Address: string;
begin
  Result := '===General info==='#13#10;
  Add('Date/Time', FTime);
  Add('Process ID', IntToHex(FCurrentPID, 8));
  Add('User name', FUserName);
  Add('Domain name', FDomainName);
  Add('Workgroup', FWorkGroup);
  Add('Registered company', FRegisteredCompany);
  Add('Registered owner', FRegisteredOwner);
  Add('Computer name', FComputerName);

  Add('Exception address', IntToHex(FExceptionAddr, 8));
  if FException <> nil then
  begin
    Add('Exception class', FException.ClassName);
    Add('Exception message', FException.ToString);
  end;

  Result := Result + #13#10#13#10;

  Result := Result + '===Processes info==='#13#10;

  for i := 0 to Pred(FProcesses.Count) do
    Add(ExtractFileName(FProcesses[i]), ExtractFilePath(FProcesses[i]));

  Result := Result + #13#10#13#10;

  Result := Result + '===Tasks info==='#13#10;

  for i := 0 to Pred(FTasks.Count) do
    Add('Task #'+inttostr(i), FTasks[i]);

  Result := Result + #13#10#13#10;

  Result := Result + '===Modules info==='#13#10;

  for i := 0 to Pred(FModules.Count) do
    Add(ExtractFileName(FModules[i]), ExtractFilePath(FModules[i]));

  Result := Result + #13#10#13#10;

  Result := Result + '===Threads info==='#13#10;

  for i := 0 to Pred(FThreads.Count) do
    Result := Result + FThreads[i] + #13#10;

  Result := Result + #13#10#13#10;

  Result := Result + '===Stack trace==='#13#10;
  Result := Result + 'Address '#9#9 + 'Info'#13#10;
  for i := 0 to Pred(FStackTrace.Count) do
  begin
    s := FStackTrace[i];
    _Address := Copy(s, Pos('[', s) + 1, Pos(']', s) - Pos('[', s) - 1);
    Delete(s, Pos('[', s), Pos(']', s) - Pos('[', s) + 2);
    Add(_Address, s);
  end;

  Result := Result + #13#10#13#10;

  Result := Result + '===Dump info==='#13#10;
  Result := Result + GetDump;
end;

constructor TExceptionObject.Create(aException: Exception; aExceptAddr: DWORD;
  aStackTrace: TStringList; aThreads: TStringList; const aDump: string);
var
  size: Cardinal;
begin
  FTime := DateTimeToStr(Now);
  FException := aException;
  FExceptionAddr := aExceptAddr;
  FProcesses := TStringList.Create;
  FModules := TStringList.Create;
  FTasks := TStringList.Create;
  FStackTrace := aStackTrace;
  FThreads := aThreads;
  FDump := aDump;
  size := 255;
  SetLength(FComputerName, size);
  GetComputerName(@FComputerName[1], size);
  SetLength(FComputerName, size);
  FCurrentPID := GetCurrentProcessId;

  FUserName := GetLocalUserName;
  FDomainName := GetUserDomainName(FUserName);
  FWorkGroup := GetWorkGroupName;
  FRegisteredCompany := GetRegisteredCompany;
  FRegisteredOwner := GetRegisteredOwner;

  LoadedModulesList(FModules, GetCurrentProcessId);
  RunningProcessesList(FProcesses);
  GetTasksList(FTasks);

  FWindowsVersion := GetWindowsVersion;
  FWindowsEdition := GetWindowsEdition;
  FNtProductType := NtProductType;
  FWindowsVersionString := GetWindowsVersionString;
  FWindowsEditionString := GetWindowsEditionString;
  FWindowsProductString := GetWindowsProductString;
  FNtProductTypeString := NtProductTypeString;
  FWindowsServicePackVersion := GetWindowsServicePackVersion;
  FWindowsServicePackVersionString := GetWindowsServicePackVersionString;
  GetNativeSystemInfo(FNativeSystemInfo);
  FProcessorArchitecture := GetProcessorArchitecture;
  FIsWindows64 := IsWindows64;
end;

destructor TExceptionObject.Destroy;
begin
  FProcesses.Free;
  FModules.Free;
  FTasks.Free;
  FStackTrace.Free;
  FThreads.Free;
end;

function TExceptionObject.GetDump: string;
begin
  Result := FDump;
end;

procedure TExceptionObject.GetGeneralInfo(aProc: TGetInfoProc);
begin
  if @aProc = nil then exit;

  aProc('Date/Time', FTime, []);
  aProc('Process ID', IntToHex(FCurrentPID, 8), []);
  aProc('User name', FUserName, []);
  aProc('Domain name', FDomainName, []);
  aProc('Workgroup', FWorkGroup, []);
  aProc('Registered company', FRegisteredCompany, []);
  aProc('Registered owner', FRegisteredOwner, []);
  aProc('Computer name', FComputerName, []);

  aProc('Exception address', IntToHex(FExceptionAddr, 8), []);
  if FException <> nil then
  begin
    aProc('Exception class', FException.ClassName, []);
    aProc('Exception message', FException.ToString, []);
  end;
end;

procedure TExceptionObject.GetModules(aProc: TGetInfoProc);
var
  i: integer;
begin
  if @aProc = nil then exit;

  for i := 0 to Pred(FModules.Count) do
  begin
    aProc(ExtractFileName(FModules[i]), ExtractFilePath(FModules[i]), []);
  end;
end;

procedure TExceptionObject.GetProcesses(aProc: TGetInfoProc);
var
  i: Integer;
  _name, _path: string;
begin
  if @aProc = nil then exit;

  for i := 0 to Pred(FProcesses.Count) do
  begin
    _name := ExtractFileName(FProcesses[i]);
    _path := ExtractFilePath(FProcesses[i]);
    aProc(_name, _path, []);
  end;
end;

procedure TExceptionObject.GetStackTrace(aProc: TGetInfoProc);
var
  i: integer;
  _Address, s: string;
begin
  if @aProc = nil then exit;

  for i := 0 to Pred(FStackTrace.Count) do
  begin
    s := FStackTrace[i];
    _Address := Copy(s, Pos('[', s) + 1, Pos(']', s) - Pos('[', s) - 1);
    Delete(s, Pos('[', s), Pos(']', s) - Pos('[', s) + 2);
    aProc(_Address, s, []);
  end;
end;

procedure TExceptionObject.GetTasks(aProc: TGetInfoProc);
var
  i: integer;
begin
  if @aProc = nil then exit;

  for i := 0 to Pred(FTasks.Count) do
  begin
    aProc(FTasks[i], '', []);
  end;
end;

procedure TExceptionObject.GetThreads(aProc: TGetInfoProc);
var
  i: integer;
begin
  if @aProc = nil then exit;

  for i := 0 to Pred(FThreads.Count) do
  begin
    aProc(FThreads[i], '', []);
  end;
end;

end.
