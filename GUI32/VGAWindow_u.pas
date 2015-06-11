unit VGAWindow_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MPHexEditor, ExtCtrls, StdCtrls, ActnList;

type
  TVGAform = class(TForm)
    pnlMemory: TPanel;
    lblMemory: TLabel;
    cbbMemory: TComboBox;
    edtxResolution: TLabeledEdit;
    edtyresolution: TLabeledEdit;
    actlstMain: TActionList;
    actGotoOffset: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbbMemoryChange(Sender: TObject);
    procedure actGotoOffsetExecute(Sender: TObject);
  private
    VGADumpView: TMPHexEditor;
    FActive: Boolean;
    { Private declarations }
  public
    { Public declarations }
    procedure InitInfo;
    procedure UpdateInfo;
    procedure ReleaseInfo;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

var
  VGAform: TVGAform;

implementation

{$R *.dfm}

uses
  vga;

{ TVGAform }

procedure TVGAform.actGotoOffsetExecute(Sender: TObject);
begin
//
end;

procedure TVGAform.AfterConstruction;
begin
  inherited;
  VGADumpView := TMPHexEditor.Create(self);
  VGADumpView.Align := alClient;
  VGADumpView.Parent := Self;

  VGADumpView.UnicodeChars := False;
  VGADumpView.ParentFont := true;
  VGADumpView.ParentFont := False;
  VGADumpView.TabOrder := 1;
  VGADumpView.BytesPerColumn := 1;
  VGADumpView.Translation := tkAsIs;
  VGADumpView.Colors.Background := clBlack;
  VGADumpView.Colors.ChangedBackground := clred;
  VGADumpView.Colors.ChangedText := clMaroon;
  VGADumpView.Colors.CursorFrame := clNavy;
  VGADumpView.Colors.Offset := clSilver;
  VGADumpView.Colors.OddColumn := clWhite;
  VGADumpView.Colors.EvenColumn := clWhite;
  VGADumpView.Colors.CurrentOffsetBackground := clGray;
  VGADumpView.Colors.OffsetBackground := clBlack;
  VGADumpView.Colors.CurrentOffset := clWhite;
  VGADumpView.Colors.Grid := clWhite;
  VGADumpView.Colors.NonFocusCursorFrame := clAqua;
  VGADumpView.Colors.ActiveFieldBackground := clBlack;
  VGADumpView.FocusFrame := True;
  VGADumpView.DrawGridLines := False;
  VGADumpView.ReadOnlyView := True;
  VGADumpView.BorderStyle := bsNone;
end;

procedure TVGAform.BeforeDestruction;
begin
  inherited;
  ReleaseInfo;
  VGADumpView.Free;
end;

procedure TVGAform.cbbMemoryChange(Sender: TObject);
begin
  if not FActive then
    Exit;

  VGADumpView.AttachMemory(nil, 0);
  VGADumpView.DataSize := 0;
  case cbbMemory.ItemIndex of
    {VGA } 0:
    begin
      VGADumpView.DataSize := Length(bx_vga.s.vga_memory);
      VGADumpView.AttachMemory(@bx_vga.s.vga_memory[0], Length(bx_vga.s.vga_memory));
      edtxResolution.Text := IntToStr(bx_vga.s.x_tilesize);
      edtyResolution.Text := IntToStr(bx_vga.s.y_tilesize);
    end;
    {VBE } 1:
    begin
      VGADumpView.DataSize := Length(bx_vga.s.vbe_memory);
      VGADumpView.AttachMemory(@bx_vga.s.vbe_memory[0], Length(bx_vga.s.vbe_memory));
      edtxResolution.Text := IntToStr(bx_vga.s.vbe_xres);
      edtyResolution.Text := IntToStr(bx_vga.s.vbe_yres);
    end;
    {Text} 2:
    begin
      VGADumpView.DataSize := Length(bx_vga.s.text_snapshot);
      VGADumpView.AttachMemory(@bx_vga.s.text_snapshot[0], Length(bx_vga.s.text_snapshot));
      edtxResolution.Clear;
      edtyResolution.Clear;
    end;
  end;
  VGADumpView.Invalidate;
end;

procedure TVGAform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;
  ShowWindow(Self.Handle, SW_HIDE);
end;

procedure TVGAform.InitInfo;
begin
  FActive := True;
end;

procedure TVGAform.ReleaseInfo;
begin
  FActive := False;
  VGADumpView.AttachMemory(nil, 0);
end;

procedure TVGAform.UpdateInfo;
begin
  cbbMemoryChange(cbbMemory);
end;

end.
