unit serv_param;

interface

function IsThereParams: boolean;

implementation

uses Windows, SysUtils, Classes, Service;

function RawDiskToFile (const sourceDisk, targetFile: string): boolean;
const
  _bufSize = 10 * 1024;
var
  _handleSource, _handleTarget: integer;
  _countRead: longword;
  _buffer: array[0.._bufSize] of byte;
begin
  _handleSource := CreateFile(PChar(Format('\\.\%s', [sourceDisk])),
    GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING,
    FILE_FLAG_SEQUENTIAL_SCAN, 0);
  Result := (_handleSource = INVALID_HANDLE_VALUE);
  if not Result then
  begin
    _handleTarget := FileCreate(targetFile);
    if _handleTarget <> -1 then
    begin
      _countRead := _bufSize;
      while _countRead = _bufSize do
      begin
        ReadFile(_handleSource, _buffer, _bufSize, _countRead, nil);
        FileWrite(_handleTarget, _buffer, _countRead);
      end;
    end
    else
      Result := False;
  end;
  if _handleSource <> INVALID_HANDLE_VALUE then
    CloseHandle(_handleSource);

  if _handleTarget <> -1 then
    FileClose(_handleTarget);
end;

procedure CreateFileSize (const fname: string; fsize: longword);
var
  fHandle: integer;
  _written, I: integer;
  Buf: array[0..1024 + 1] of char;
begin
  fHandle := FileCreate(fname);
  FillChar(Buf, 1024, 0);
  I := 0;
  _written := 1024;
  while (I < fsize) and (_written = 1024) do
  begin
    _written := FileWrite(fHandle, Buf, 1024);
    if I mod 10 = 0 then
      Write('.');
    Inc(I);
  end;
  FileClose(fHandle);
end;

function IsThereParams: boolean;
var
  _s, _s1, _s2: string;
begin
  Result := True;
  _s := ParamStr(1);
  if _s[1] <> '-' then
    Result := False
  else
  begin
    _s := Copy(ParamStr(1), 2, 1);
    case _s[1] of
      'p':
      begin
        _s1 := ParamStr(2);
        _s2 := ParamStr(3);
        assert((Length(_s1) > 0) and ((Length(_s2) > 0)));
        RawDiskToFile(_s1, _s2);
      end;
      'd':
      begin
        _s1 := ParamStr(2);
        _s2 := ParamStr(3);
        assert((Length(_s1) > 0) and ((Length(_s2) > 0)));
        CreateFileSize(_s1, StrToInt(_s2));
      end;
    end;
  end;
end;

end.
