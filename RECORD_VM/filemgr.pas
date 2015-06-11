unit filemgr;

interface

uses Windows;

const
  MAY_TOO_BIG = $c000;

type
  TIdentify = (idCPU);

  TNode = record
    Magic:  byte;
    Source: TIdentify;
    id:     longword;
    Size:   longword;
  end;

  Record_interface = class
  private
    fp_record:    THandle;
    fake_start_pointer, fake_end_pointer, fake_size: longword;
    idf:          TIdentify;
    prog_session: longword;
  public
    constructor Create (fs, fe: longword; id: TIdentify);
    destructor Destroy; override;
    function WriteNode: boolean;
    function ReadNode (Entry: Pointer): shortint;
  end;

implementation

constructor Record_interface.Create (fs, fe: longword; id: TIdentify);
  begin
    fake_start_pointer := fs;
    fake_end_pointer := fe;
    prog_session := 0;
    idf := id;
    fp_record := CreateFile('c:\rc_1.img', GENERIC_READ or GENERIC_WRITE,
      0, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  end;

function Record_interface.WriteNode: boolean;
  var
    K: TNode;
    P: Pointer;
    datawritten: longword;
    Res: longbool;
  begin
    K.Magic := $33;
    K.Source := idf;
    K.id := prog_session;
    K.Size := fake_end_pointer - fake_start_pointer;
    P := Pointer(@fake_start_pointer);
    Res := WriteFile(fp_record, K, SizeOf(K), datawritten, nil);
    if datawritten = SizeOf(K) then
      Result := WriteFile(fp_record, P^, K.Size, datawritten, nil)
    else
      Result := False;
  end;

destructor Record_interface.Destroy;
  begin
    if fp_record <> INVALID_HANDLE_VALUE then
      CloseHandle(fp_record);
  end;

function Record_interface.ReadNode (Entry: Pointer): shortint;
  var
    K: TNode;
    Readed: longword;
    Res: longbool;
    P: Pointer;
  begin
    Result := 0;
    if fp_record <> INVALID_HANDLE_VALUE then
      begin
      Res := ReadFile(fp_record, K, SizeOf(K), Readed, nil);
      if Readed = SizeOf(K) then
        begin
        if K.Size > MAY_TOO_BIG then
          Result := -2
        else
          begin
          GetMem(P, K.Size);
          Res := ReadFile(fp_record, P, K.Size, Readed, nil);
          if Readed < K.Size then
            Result := -1
          else
            begin
            //MoveMemory();
            end;
          end;
        end;
      end
    else
      Entry := nil;
  end;

end.
