unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FileCtrl, StdCtrls, ExtCtrls, MPHexEditor, MPHexEditorEx, Grids;

type
  TfmHexViewer = class(TForm)
    Panel1: TPanel;
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;
    FileListBox1: TFileListBox;
    Panel2: TPanel;
    btnPrint: TButton;
    btnFind: TButton;
    btntest: TButton;
    procedure FileListBox1Change(Sender: TObject);
    procedure MPHexEditor1InvalidKey(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure btntestClick(Sender: TObject);
  private
    MPHexEditor1: TMPHexEditorEx;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmHexViewer: TfmHexViewer;

implementation

uses uprinting, ufinddialog;

{$R *.DFM}

procedure TfmHexViewer.FileListBox1Change(Sender: TObject);
begin
//  with MPHexEditor1
//  do
//    if FileName <> FileListBox1.FileName then
//    begin
//      if not FileExists(FileListBox1.FileName) then
//      begin
//        CreateEmptyFile('Unnamed');
//        btnPrint.Enabled := False;
//        btnFind.Enabled := False;
//      end
//      else
//      begin
//        LoadFromFile(FileListBox1.FileName);
//        btnPrint.Enabled := DataSize > 0;
//        btnFind.Enabled := DataSize > 0;
//      end;
//    end;
end;

procedure TfmHexViewer.MPHexEditor1InvalidKey(Sender: TObject);
begin
  // invalid Key, Beep
  MessageBeep(MB_ICONASTERISK)
end;

procedure TfmHexViewer.FormCreate(Sender: TObject);
begin
  MPHexEditor1 := TMPHexEditorEx.Create(Self);
  MPHexEditor1.Parent := Self;
  MPHexEditor1.Left := 126;
  MPHexEditor1.Top := 0;
  MPHexEditor1.Width := 534;
  MPHexEditor1.Height := 304;

  MPHexEditor1.UnicodeChars := False;
//  MPHexEditor1.Cursor := crIBeam;
//  MPHexEditor1.BackupExtension := '.bak';
  MPHexEditor1.PrintOptions.MarginLeft := 20;
  MPHexEditor1.PrintOptions.MarginTop := 15;
  MPHexEditor1.PrintOptions.MarginRight := 25;
  MPHexEditor1.PrintOptions.MarginBottom := 25;
  MPHexEditor1.PrintOptions.PageHeader := '%f||%P page(s)';
  MPHexEditor1.PrintOptions.PageFooter := '%d %t|(%<)|%p/%P';
  MPHexEditor1.PrintOptions.Flags := [pfSelectionBold, pfMonochrome];
//  MPHexEditor1.PrintFont.Charset := DEFAULT_CHARSET;
//  MPHexEditor1.PrintFont.Color := clWindowText;
//  MPHexEditor1.PrintFont.Height := -15;
//  MPHexEditor1.PrintFont.Name := 'Courier New';
//  MPHexEditor1.PrintFont.Style := [];
  MPHexEditor1.Align := alClient;
  MPHexEditor1.Font.Charset := DEFAULT_CHARSET;
  MPHexEditor1.Font.Color := clWhite;
//  MPHexEditor1.Font.Height := 8;
//  MPHexEditor1.Font.Size := 6;
  MPHexEditor1.Font.Name := 'Terminal';
  MPHexEditor1.Font.Style := [];
  MPHexEditor1.ParentFont := False;
  MPHexEditor1.TabOrder := 1;
  MPHexEditor1.AutoBytesPerRow := True;
  MPHexEditor1.BytesPerRow := 14;
  MPHexEditor1.Translation := tkAsIs;
  MPHexEditor1.OffsetFormat := '-!10:0x|';
  MPHexEditor1.Colors.Background := clBlack;
  MPHexEditor1.Colors.ChangedBackground := clred;//11075583;
  MPHexEditor1.Colors.ChangedText := clMaroon;
  MPHexEditor1.Colors.CursorFrame := clNavy;
  MPHexEditor1.Colors.Offset := clSilver;//clBlack;
  MPHexEditor1.Colors.OddColumn := clWhite;
  MPHexEditor1.Colors.EvenColumn := clWhite;
  MPHexEditor1.Colors.CurrentOffsetBackground := clGray;
  MPHexEditor1.Colors.OffsetBackground := clBlack;
  MPHexEditor1.Colors.CurrentOffset := clWhite;
  MPHexEditor1.Colors.Grid := clWhite;
  MPHexEditor1.Colors.NonFocusCursorFrame := clAqua;
  MPHexEditor1.Colors.ActiveFieldBackground := clBlack;
  MPHexEditor1.FocusFrame := True;
  MPHexEditor1.DrawGridLines := False;
  MPHexEditor1.ReadOnlyView := True;

  // make sure that MYGRID is not defined in mphexeditor.pas
  Assert(MPHexEditor1 is TCustomGrid);
end;

procedure TfmHexViewer.btnPrintClick(Sender: TObject);
begin
  PrintPreview(MPHexEditor1);
end;

procedure TfmHexViewer.btntestClick(Sender: TObject);
var
  b: array of char;
begin
  // load the selected file into the hex editor
  try
//    SetLength(b, 512);
    MPHexEditor1.AttachMemory(nil, 0);
  finally
//    SetLength(b, 0);
  end;

end;

procedure TfmHexViewer.btnFindClick(Sender: TObject);
var
  sData, sTemp: string;
  ct: integer;
begin
  with TDlgFind.Create(Application) do
  try
    if ShowModal = mrOK then
    begin
      sData := edSearch.Text;
      if cbHex.Checked then
      begin
        // text is in hex digit format '0a 0c...', convert to binary
        sTemp := sData;
        SetLength(sData, Length(sTemp) div 2);
        ConvertHexToBin(PChar(sTemp), PChar(sData), Length(sTemp), False, ct);
        // in ct the amount of converted data bytes is stored, now set real
        // length of the converted data
        SetLength(sData, ct);
      end;
      if sData <> '' then
      begin
        // get first position to find (end of selection)
        with MPHexEditor1 do
        begin
          ct := Min(SelStart, SelEnd)+SelCount;
          // prepare find data
          PrepareFindReplaceData(sData, cbIgnoreCase.Checked, not cbHex.Checked);
          // find data
          ct := Find(PChar(sData), Length(sData), ct, DataSize -1,
            cbIgnoreCase.Checked);
          if ct = -1 then
            ShowMessage('Not found.')
          else
          begin
            // select
            SelStart := ct;
            SelEnd := SelStart +Length(sData)-1;
          end;
        end;
      end;
    end;
  finally
    Free;
  end;
end;

end.
