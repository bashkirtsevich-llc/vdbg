unit ExceptHandler_u;

interface

implementation

uses
  Windows, SysUtils, Classes, Forms, ExceptDialog_u, ShellAPI, JclDebug,
  madDisasm;

type
  TOldCode = packed record
    One: DWORD;
    Two: Word;
  end;

  TFarJmp = packed record
    PuhsOp : Byte;
    PushArg: Pointer;
    RetOp  : Byte;
  end;

  THandleExceptProc = procedure (Sender: TObject) of object;

  TMADExceptThread = class(TThread)
  private
    JmpHandleExcept : TFarJmp;
    OldHandleExcept : TOldCode;
    HandleExceptAddr: Pointer;
    HandleExceptPtr : THandleExceptProc;
    Bytes           : DWORD;
    HasAppObj       : Boolean;
  protected
    procedure Execute; override;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

var
  MADThread: TMADExceptThread;
  InternalExceptAddr: pointer;

procedure HandleExcept(Sender: TObject);
var
  addr: Pointer;
  Info: TMemoryBasicInformation;
  exceptForm: TExceptionDialog;
  exceptReport: TExceptionObject;
  exceptObj: TObject;
  fileName, dump: string;
  threadList: TStringList;
  stackList: TStringList;
  canCont: Boolean;
begin
  addr := ExceptAddr;
  if addr = nil then
    addr := InternalExceptAddr;
  VirtualQuery(addr, Info, sizeof(Info));

  stackList := TStringList.Create;
  JclLastExceptStackListToStrings(stackList, True, True, True, true);

  with JclLastExceptStackList do
    if count > 0 then
      ParseFunction(items[0].CallerAddr, dump);

  threadList := TStringList.Create;
  with TJclThreadInfoList.Create do
  begin
    Gather(GetCurrentThreadId);
    threadList.Text := AsString;
  end;

  exceptObj := nil;
  exceptObj := ExceptObject;
  if (exceptObj = nil) and (Sender is Exception) then
  begin
    exceptObj := Sender;
    canCont := False;
  end else
    canCont := True;

  if (exceptObj <> nil) and (exceptObj is Exception) then
    if exceptObj is EAbort then exit;

  exceptForm := TExceptionDialog.Create(nil);
  try
    exceptForm.CanContinue := canCont;
    exceptReport := TExceptionObject.Create(Exception(exceptObj), DWORD(addr),
      stackList, threadList, dump);
    try
      case exceptForm.ShowBugReport(exceptReport) of
        EXCEPT_CONTINE:;
        EXCEPT_RESTART:
          begin
            SetLength(fileName, 255);
            GetModuleFileName(Cardinal(Info.AllocationBase), @fileName[1], Length(fileName));
            ShellExecute(0, 'open', PChar(fileName), nil, PChar(GetCurrentDir), SW_SHOW);
            Halt(0);
          end;
        EXCEPT_TERMINATE: Halt(0);
        EXCEPT_SEND_REPORT: exceptForm.SendReport(exceptReport);
      end;
    finally
      exceptForm.Free;
      exceptReport.Free;
    end;
  finally
    threadList.Free;
    stackList.Free;
  end;
end;

procedure ShowExcept(aExceptObject: TObject; aExceptAddr: Pointer);
begin
  InternalExceptAddr := aExceptAddr;
  HandleExcept(aExceptObject);
  InternalExceptAddr := nil;
end;

{ TMADExceptThread }

procedure TMADExceptThread.AfterConstruction;
begin
  inherited;
  ExceptProc := @ShowExcept;
  FreeOnTerminate := True;
  HandleExceptPtr := Application.HandleException;
  HandleExceptAddr := @HandleExceptPtr;
  ReadProcessMemory(INVALID_HANDLE_VALUE, HandleExceptAddr, @OldHandleExcept, SizeOf(TOldCode), Bytes);
  JmpHandleExcept.PuhsOp  := $68;
  JmpHandleExcept.PushArg := @HandleExcept;
  JmpHandleExcept.RetOp   := $C3;
end;

destructor TMADExceptThread.Destroy;
begin
  WriteProcessMemory(INVALID_HANDLE_VALUE, HandleExceptAddr, @OldHandleExcept, SizeOf(TOldCode), Bytes);
  inherited;
end;

procedure TMADExceptThread.Execute;
var
  _HandleExceptAddr: Pointer;
  _HandleExceptPtr : THandleExceptProc;
  _JmpHandleExcept : TFarJmp;
begin
  inherited;
  while not Terminated do
  begin
    if ExceptProc <> @ShowExcept then
      ExceptProc := @ShowExcept;

    if Application <> nil then
    begin
      _HandleExceptPtr := Application.HandleException;
      _HandleExceptAddr := @_HandleExceptPtr;
      ReadProcessMemory(INVALID_HANDLE_VALUE, _HandleExceptAddr, @_JmpHandleExcept, SizeOf(TFarJmp), Bytes);
      if _JmpHandleExcept.PushArg <> @HandleExceptAddr then
        WriteProcessMemory(INVALID_HANDLE_VALUE, HandleExceptAddr, @JmpHandleExcept, SizeOf(TFarJmp), Bytes);
      _JmpHandleExcept.PuhsOp := 0;
      _JmpHandleExcept.PushArg := nil;
      _JmpHandleExcept.RetOp := 0;
    end;
    Sleep(100);
  end;
end;

initialization
//  ExceptProc := @ShowExcept;

  Include(JclStackTrackingOptions, stRawMode);
  Include(JclStackTrackingOptions, stAllModules);
  Include(JclStackTrackingOptions, stTraceAllExceptions);
//  Include(JclStackTrackingOptions, stStaticModuleList);
  JclStartExceptionTracking;

  MADThread := TMADExceptThread.Create;
//  HasAppObj := Application <> nil;
//  if not HasAppObj then Exit; // paranoid
//
//  HandleExceptPtr := Application.HandleException;
//  HandleExceptAddr := @HandleExceptPtr;
//  ReadProcessMemory(INVALID_HANDLE_VALUE, HandleExceptAddr, @OldHandleExcept, SizeOf(TOldCode), Bytes);
//  JmpHandleExcept.PuhsOp  := $68;
//  JmpHandleExcept.PushArg := @HandleExcept;
//  JmpHandleExcept.RetOp   := $C3;
//  WriteProcessMemory(INVALID_HANDLE_VALUE, HandleExceptAddr, @JmpHandleExcept, SizeOf(TFarJmp), Bytes);

finalization
  JclStopExceptionTracking;
  MADThread.Terminate;
//  if not HasAppObj then Exit; // paranoid
//
//
//  WriteProcessMemory(INVALID_HANDLE_VALUE, HandleExceptAddr, @OldHandleExcept, SizeOf(TOldCode), Bytes);

end.
