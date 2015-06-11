{$include defines.pas}
unit Service;

interface

uses SysUtils, Gui32, Config;

type
  LogType       = (DEVLOG, PCILOG);
  ExceptionType = (BX_GP_EXCEPTION);

procedure LogPanic (S: string); // bx_panic
procedure LogInfo (S: string);
//procedure BX_DEBUG (S: string);
procedure LogError (S: string);
procedure Put (S: string);
procedure putchar (C: integer);
procedure panic (S: string);
procedure bx_log_info (S: string);
procedure exception2 (K: array of word);

procedure SetType (T: LogType);
procedure InitLogFiles;
procedure DoneLogFiles;
function  CountLinesTxtFile (filename: string): longword;
function  GetFileSize (const Name: string): longword;
procedure Blink (const s: string);

procedure WriteToConsole (s: string);
function ChsToInt (const C: TConf): int64;

implementation

uses cpu, Windows;

//{$ifndef beep}
//procedure Beep;
//  begin
//{$ifdef COMPILE_WIN32}
//  MessageBeep(0);
//{$endif}
//  end;
//
//{$endif}

procedure InitLogFiles;
var
  WorkDir: string;
begin
//  WorkDir := ExtractFilePath(ParamStr(0));
//  if FileExists(WorkDir + 'logs\log.txt')
//    then AssignFile(LogFile, WorkDir + 'logs\log.txt')
//    else Rewrite(LogFile, WorkDir + 'logs\log.txt');
//  Append(LogFile);
end;

procedure DoneLogFiles;
begin
  CloseFile(LogFile);
end;

procedure LogPanic (S: string);
begin
  // пока уберем
//  WriteLn(LogFile, Format('PANIC message  : [%d], %s', [bx_cpu.prog, S]));
  //FormLog.OutLog.Items.Add(s);
{$ifndef NO_PANIC_ERR}
  //DoneLogFiles;
  //WriteLn('PANIC! : ' + S);
  //raise Exception.Create(Format('Panic message : %s on %d',[S,bx_cpu.prog]));
  //readln;
{$endif}
end;

procedure LogInfo (S: string);
begin
{$if BX_INFO_ENABLED=1}
{$if BX_DEBUG_TO_FILE=1}
//  WriteLn(LogFile, Format('INFO message  : [%d], %s', [bx_cpu.prog, S]));
{$else}
 //FormLog.OutLog.Items.Add(Format('Info message  : [%d], %s',[bx_cpu.prog,S]));
{$ifend}
{$ifend}
end;

procedure BX_DEBUG (S: string);
begin
{$if BX_DEBUG_ENABLED=1}
{$if BX_DEBUG_TO_FILE=1}
//  if bx_cpu.Prog >= 200000000 then
//    WriteLn(LogFile, Format('DEBUG message  : [%d], %s', [bx_cpu.prog, S]));
{$else}
 FormLog.OutLog.Items.Add(Format('DEBUG message  : [%d], %s',[bx_cpu.prog,S]));
{$ifend}
{$ifend}
end;

procedure LogError (S: string);
begin
{$if BX_ERROR_ENABLED=1}
{$if BX_DEBUG_TO_FILE=1}
//  WriteLn(LogFile, Format('ERROR message  : [%d], %s', [bx_cpu.prog, S]));
{$else}
//  FormLog.OutLog.Items.Add(Format('ERROR message  : [%d], %s',[bx_cpu.prog,S]));
{$ifend}
{$ifend}
end;

procedure Put (S: string);
begin
  // ???????????
end;

procedure SetType (T: LogType);
begin
end;

procedure putchar (C: integer);
begin
  Write(C);
end;

procedure panic (S: string);
begin
end;

procedure bx_log_info (S: string);
begin
end;

procedure exception2 (K: array of word);
begin
{$if BX_DEBUG_ENABLED=1}
  //FormLog.DebugList.Items.Add('Exception2');
{$ifend}
  bx_cpu.Exception(K[0], K[1], K[2]);
end;

procedure WriteToConsole (s: string);
{$ifdef COMPILE_WIN32}
var
  wHandle: THandle;
  CharWritten: cardinal;
{$endif}
begin
{$ifdef COMPILE_WIN32}
  s:=s + #13#10;
  wHandle:=GetStdHandle(STD_OUTPUT_HANDLE);
  WriteConsole(wHandle, @LastMessage, Length(LastMessage), CharWritten, nil);
{$endif}
end;

function CountLinesTxtFile (filename: string): longword;
var
  fp: TextFile;
  I:  longword;
  S:  string;
begin
  I := 0;
  assignfile(fp, filename);
  reset(fp);
  while not EOF(fp) do
  begin
    ReadLn(fp, S);
    Inc(I);
  end;
  closefile(fp);
  Result := I;
end;

function ChsToInt (const C: TConf): int64;
begin
  Result := C.HddCylinder * C.HddHeads * C.HddSectors * 512;
end;

function GetFileSize (const Name: string): longword;
var
  f: file;
begin
  assignfile(f, Name);
  reset(f);
  Result := FileSize(f);
  CloseFile(f);
end;

procedure Blink (const s: string);
begin
  SetWindowText(MainWnd, PChar(s));
end;


end.
