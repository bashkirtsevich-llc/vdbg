{$ifdef LINUX}
{$include ../defines.pas}
{$else}
{$include ..\defines.pas}
{$endif}
unit TimeProfiler;

interface

uses SysUtils;

{$include ../defines.pas}

const
  TIME_PROF_STEP: longword = 1000000;

type

  PRecFile = ^TRecFile;

  TRecFile = record
    filename: array[0..256] of char;
    time:     integer;
    Size:     integer;
  end;

  TRecProfiler = record
    Progress: longword;
    Timing:   int64;
  end;

  PSearchRec = ^TSearchRec;

{$ifdef USE_TIME_PROFILER}
procedure InitProfiler;
procedure DoneProfiler;

var
  fpProfiler,LastFile:file of TRecProfiler;
  TimingDir,LastFileName:String;
  recProfiler:TRecProfiler;
  ProfilerResult:TextFile;
{$endif}

implementation

uses Service, Classes;

{$ifdef USE_TIME_PROFILER}

function CompareNames(Item1, Item2: Pointer): Integer;
begin
     Result:=Integer(PSearchRec(Item1)^.Time < PSearchRec(Item2)^.Time);
end;

function GetLastFileName(SourceDir:String):String;
var
  T:TList;
  f:PRecFile;
  Tf:TSearchRec;
  Res:Integer;
  I:Integer;
begin
  T:=TList.Create;
  new(f);
  res:=FindFirst(SourceDir,faAnyFile,Tf);
  StrPCopy(f^.filename,Format('%s\%s',[ExtractFilePath(SourceDir),tf.Name]));
  f^.time:=tf.Time;
  f^.Size:=tf.Size;
  T.Add(f);
  while res = 0 do
    begin
       res:=FindNext(tf);
       new(f);
        StrPCopy(f^.filename,Format('%s\%s',[ExtractFilePath(SourceDir),tf.Name]));
        f^.time:=tf.Time;
        f^.Size:=tf.Size;
       if res = 0 then T.Add(f);
    end;
  T.Sort(@CompareNames);
  Result := PSearchrec(T.Items[T.Count-1])^.Name;
  for I:= 0 to T.Count - 1 do
    begin
      if FileExists(PRecFile(T.items[I])^.filename) then
        if PRecFile(T.items[I])^.Size = 0 then
          DeleteFile(PRecFile(T.items[I])^.filename);
      Dispose(T.items[I]);
    end;
  T.Free;
end;

procedure InitProfiler;
var
  Year,Month,Day:Word;
  Hour, Min, Sec, MSec: Word;
  fName:String;
begin
  TimingDir:=ExtractFilePath(ParamStr(0));
  if DirectoryExists(TimingDir + '\timing') = false then
    MkDir(TimingDir + '\timing');
  TimingDir:=TimingDir + '\timing';
  DecodeDate(Date,Year,Month,Day);
  DecodeTime(Time,Hour,Min,Sec,MSec);
  fName:=format('%s\%d-%d-%d %d.%d.%d',[TimingDir,Year,Month,Day,Hour,Min,Sec]);
  LastFileName:=GetLastFileName(TimingDir + '\*.*');
  AssignFile(fpProfiler,fName);
  rewrite(fpProfiler);
  AssignFile(ProfilerResult,TimingDir + '\' + result.txt);
  Append(ProfilerResult);
end;

procedure DoneProfiler;
begin
  CloseFile(fpProfiler);
  CloseFile(ProfilerResult);
end;
{$endif}

end.
