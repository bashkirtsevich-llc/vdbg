unit CreateIsoForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFormCreateIso = class (TForm)
    rdDisk:       TRadioGroup;
    Label1:       TLabel;
    edIso:        TEdit;
    cmRip:        TButton;
    cmClose:      TButton;
    cmChooseFile: TButton;
    saveRip:      TSaveDialog;
    lblTot:       TLabel;
    cmStop:       TButton;
    procedure FormCreate (Sender: TObject);
    procedure cmCloseClick (Sender: TObject);
    procedure cmChooseFileClick (Sender: TObject);
    procedure cmRipClick (Sender: TObject);
    procedure cmStopClick (Sender: TObject);
  private
    stopRun: boolean;
    procedure fillRadioDrive;
    procedure ripDrive (const driveRoot, fileIso: string);
  public
    { Public declarations }
  end;

var
  FormCreateIso: TFormCreateIso;

implementation

{$R *.dfm}

{ TFormCreateIso }

uses config;

procedure TFormCreateIso.fillRadioDrive;
  var
    I: integer;
    rootDisk: string;
  begin
    for I := 0 to 25 do
      begin
      rootDisk := Chr(65 + I) + ':\';
      if GetDriveType(PChar(rootDisk)) = DRIVE_CDROM then
        begin
        rdDisk.Items.Add(rootDisk);
        end;
      end;
    if rdDisk.Items.Count > 0 then
      rdDisk.ItemIndex := 0;
  end;

procedure TFormCreateIso.FormCreate (Sender: TObject);
  begin
    fillRadioDrive;
  end;

procedure TFormCreateIso.cmCloseClick (Sender: TObject);
  begin
    Close;
  end;

procedure TFormCreateIso.cmChooseFileClick (Sender: TObject);
  begin
    if saverip.Execute then
      edIso.Text := saverip.FileName;
  end;

procedure TFormCreateIso.cmRipClick (Sender: TObject);
  begin
    cmStop.Enabled := True;
    ripDrive(Copy(rdDisk.Items[rdDisk.ItemIndex], 1, 2), edIso.Text);
    cmStop.Enabled := False;
  end;

procedure TFormCreateIso.ripDrive (const driveRoot, fileIso: string);
  const
    chunk_size = 4096 * 10;
  var
    buffer: PChar;
    _fileHandle, bytesReaded, Written: longword;
    _totBytes: int64;
    _r: boolean;
    fp: file of byte;
  begin
    cmrip.Enabled  := False;
    cmClose.Enabled := False;
    cmStop.Enabled := False;
    GetMem(buffer, chunk_size);
    _fileHandle := CreateFile(PChar('\\.\' + driveRoot), GENERIC_READ,
      0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0);
    bytesReaded := chunk_size;
    _totBytes := 0;
    _r := True;
    stopRun := False;
      try
      if _fileHandle = INVALID_HANDLE_VALUE then
        begin
        raise Exception.Create('Errore opening: ' + '\\.\' + driveRoot);
        exit;
        end;
      AssignFile(fp, fileIso);
      Rewrite(fp);

      Written := chunk_size;
      while (bytesReaded = chunk_size) and (_r) and (Written = chunk_size) and
        (not stopRun) do
        begin
        _r := ReadFile(_fileHandle, buffer^, chunk_size, bytesReaded, nil);
        BlockWrite(fp, buffer^, chunk_size, Written);
        Inc(_totBytes, bytesReaded);
        if _totBytes mod (chunk_size * 10) = 0 then
          begin
          lblTot.Caption := IntToStr(_totBytes);
          Application.ProcessMessages;
          end;
        end;
      finally
      CloseFile(fp);
      if _fileHandle <> INVALID_HANDLE_VALUE then
        CloseHandle(_fileHandle);
      if buffer <> nil then
        FreeMem(buffer);
      cmrip.Enabled  := True;
      cmClose.Enabled := True;
      cmStop.Enabled := True;
      end;
  end;

procedure TFormCreateIso.cmStopClick (Sender: TObject);
  begin
    stopRun := True;
  end;

end.
