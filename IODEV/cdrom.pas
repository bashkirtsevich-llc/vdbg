unit cdrom;

interface

uses Config, Service;

const
  BX_CD_FRAMESIZE = 2048;

const
  CD_FRAMESIZE = 2048;

type
  TCFRomClass = class
    a       : integer;
    cdhandle: THandle;
    constructor Init (const s: string);
    destructor  Done;
    function  insert_cdrom (const dev: string): boolean;
    procedure eject_cdrom;
    function  read_toc (buf: PChar; length: integer; msf: word;
      start_track: integer): boolean;
    function  capacity: longword;
    procedure read_block (buf: puint8; lba: integer);
  end;

implementation

uses Windows, SysUtils;

constructor TCFRomClass.Init (const s: string);
begin
  inherited;
  cdhandle := INVALID_HANDLE_VALUE;
end;

function TCFRomClass.insert_cdrom (const dev: string): boolean;
begin
  cdHandle := CreateFile(PChar(dev), GENERIC_READ, 0, nil, OPEN_EXISTING,
    FILE_FLAG_RANDOM_ACCESS, 0);

  if cdHandle = INVALID_HANDLE_VALUE then
    LogPanic('Не могу инициализировать привод компакт дисков!');

  Result := not (cdHandle = INVALID_HANDLE_VALUE);
end;

procedure TCFRomClass.eject_cdrom;
begin
  if cdHandle <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(cdHandle);
    cdHandle := INVALID_HANDLE_VALUE;
  end;
end;

function TCFRomClass.read_toc (buf: PChar; length: integer; msf: word;
  start_track: integer): boolean;
begin
  LogInfo('read toc not implemented');
  Result := True;
end;

function TCFRomClass.capacity: longword;
begin
  Result := Windows.GetFileSize(cdHandle, nil);
end;

procedure TCFRomClass.read_block (buf: puint8; lba: integer);
var
  n: DWORD;
begin
  SetFilePointer(cdHandle, lba * BX_CD_FRAMESIZE, nil, FILE_BEGIN);
  ReadFile(cdHandle, buf^, BX_CD_FRAMESIZE, n, nil);
end;

destructor TCFRomClass.Done;
begin
  if cdHandle <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(cdHandle);
    cdHandle := INVALID_HANDLE_VALUE;
  end;
end;

end.
