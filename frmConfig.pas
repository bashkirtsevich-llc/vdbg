unit frmConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TConfigForm = class (TForm)
    Button1:        TButton;
    Button2:        TButton;
    Button3:        TButton;
    Panel1:         TPanel;
    PageControl1:   TPageControl;
    TabSheet1:      TTabSheet;
    TabSheet2:      TTabSheet;
    TabSheet3:      TTabSheet;
    lbBoot:         TComboBox;
    Label2:         TLabel;
    Label1:         TLabel;
    lbMemory:       TEdit;
    lbSectors:      TEdit;
    Label7:         TLabel;
    Label6:         TLabel;
    lbHeads:        TEdit;
    lbCylinder:     TEdit;
    Label5:         TLabel;
    lbHDDFile:      TEdit;
    Label4:         TLabel;
    lbHDDPresent:   TCheckBox;
    lbDiskSize:     TEdit;
    Label10:        TLabel;
    TabSheet4:      TTabSheet;
    Button6:        TButton;
    Label8:         TLabel;
    lbCDROMFile:    TEdit;
    lbCDRomPresent: TCheckBox;
    Button4:        TButton;
    Button7:        TButton;
    Button8:        TButton;
    lbFPU:          TCheckBox;
    lbCDROMDrive:   TComboBox;
    Label11:        TLabel;
    lblBios: TLabel;
    lblVgaBios: TLabel;
    edtBios: TEdit;
    edtVgaBios: TEdit;
    btnBrowsBios: TButton;
    btnBrowsVgaBios: TButton;
    lblFont: TLabel;
    edtFont: TEdit;
    btnBrows3: TButton;
    grpFloppyA: TGroupBox;
    Label3: TLabel;
    lbFloppyAType: TComboBox;
    Label12: TLabel;
    lbFloppyADrive: TComboBox;
    Button9: TButton;
    Label9: TLabel;
    lbFloppyAFile: TEdit;
    Button5: TButton;
    grpFloppyB: TGroupBox;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    lbFloppyBType: TComboBox;
    lbFloppyBDrive: TComboBox;
    Button10: TButton;
    lbFloppyBFile: TEdit;
    Button11: TButton;
    chkFloppyAReadOnly: TCheckBox;
    chkFloppyBReadOnly: TCheckBox;
    procedure Button3Click (Sender: TObject);
    procedure FormActivate (Sender: TObject);
    procedure Button2Click (Sender: TObject);
    procedure lbCylinderChange (Sender: TObject);
    procedure Button4Click (Sender: TObject);
    procedure Button5Click (Sender: TObject);
    procedure Button6Click (Sender: TObject);
    procedure Button7Click (Sender: TObject);
    procedure Button8Click (Sender: TObject);
    procedure lbCDROMDriveSelect (Sender: TObject);
    procedure lbFloppyADriveSelect (Sender: TObject);
    procedure Button9Click (Sender: TObject);
    procedure btnBrowsBiosClick(Sender: TObject);
    procedure btnBrowsVgaBiosClick(Sender: TObject);
    procedure btnBrows3Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure createEmptyFile (fileName: string; sizeFile: int64);
  end;

var
  ConfigForm: TConfigForm;

implementation

uses CONFIG, m2fMain, CreateIsoForm;

{$R *.dfm}

procedure TConfigForm.btnBrows3Click(Sender: TObject);
var
  OD: TOpenDialog;
begin
  OD := TOpenDialog.Create(nil);
  OD.Title := 'Select VGA BIOS file';
  od.Filter := '*.dat|*.dat';

  if OD.Execute then
    edtFont.Text := OD.FileName;

  OD.Free;
end;

procedure TConfigForm.btnBrowsBiosClick(Sender: TObject);
var
  OD: TOpenDialog;
begin
  OD := TOpenDialog.Create(nil);
  OD.Title := 'Select BIOS file';
  od.Filter := '*.dat|*.dat';

  if OD.Execute then
    edtBios.Text := OD.FileName;

  OD.Free;
end;

procedure TConfigForm.btnBrowsVgaBiosClick(Sender: TObject);
var
  OD: TOpenDialog;
begin
  OD := TOpenDialog.Create(nil);
  OD.Title := 'Select VGA BIOS file';
  od.Filter := '*.dat|*.dat';

  if OD.Execute then
    edtVgaBios.Text := OD.FileName;

  OD.Free;
end;

procedure TConfigForm.Button11Click(Sender: TObject);
  var
    OD: TOpenDialog;
  begin

    OD := TOpenDialog.Create(nil);
    OD.Title := 'Select image file';
    od.Filter := '*.img|*.img|*.iso|*.iso|*.ima|*.ima';


    if OD.Execute then
      begin
      lbFloppyBFile.Text := OD.FileName;
      end;

    OD.Free;
  end;

procedure TConfigForm.Button2Click (Sender: TObject);
  begin
    VMConfig.Memory  := StrToIntDef(lbMemory.Text, 32);
    VMConfig.BootDevice := lbBoot.ItemIndex;

    VMConfig.FloppyAType := lbFloppyAType.ItemIndex + 10;
    VMConfig.FloppyAFile := lbFloppyAFile.Text;
    VMConfig.FloppyAReadOnly := chkFloppyAReadOnly.Checked;

    VMConfig.FloppyBType := lbFloppyBType.ItemIndex + 10;
    VMConfig.FloppyBFile := lbFloppyBFile.Text;
    VMConfig.FloppyBReadOnly := chkFloppyBReadOnly.Checked;

    VMConfig.HddPresent := lbHDDPresent.Checked;
    VMConfig.HddCylinder := StrToIntDef(lbCylinder.Text, 306);
    VMConfig.HddHeads := StrToIntDef(lbHeads.Text, 4);
    VMConfig.HddSectors := StrToIntDef(lbSectors.Text, 17);
    VMConfig.HddFile := lbHDDFile.Text;
    VMConfig.CdPresent := lbCDRomPresent.Checked;
    VMConfig.CdFile  := lbCDROMFile.Text;
    VMConfig.usefpu  := lbFPU.Checked;
    VMConfig.BIOS := edtBios.Text;
    VMConfig.VGA_BIOS := edtVgaBios.Text;
    VMConfig.Font := edtFont.Text;
    fMain1.SaveVMConfig;

    fMain1.LoadVMConfig;
    Button3.Click;
  end;

procedure TConfigForm.Button3Click (Sender: TObject);
  begin
    lbMemory.Text := Format('%d', [VMConfig.Memory]);
    lbBoot.ItemIndex := VMConfig.BootDevice;

    lbFloppyAType.ItemIndex := VMConfig.FloppyAType - 10;
    lbFloppyAFile.Text := VMConfig.FloppyAFile;
    chkFloppyAReadOnly.Checked := VMConfig.FloppyAReadOnly;

    lbFloppyBType.ItemIndex := VMConfig.FloppyBType - 10;
    lbFloppyBFile.Text := VMConfig.FloppyBFile;
    chkFloppyBReadOnly.Checked := VMConfig.FloppyBReadOnly;

    lbHDDPresent.Checked := VMConfig.HddPresent;
    lbCylinder.Text := Format('%d', [VMConfig.HddCylinder]);
    lbHeads.Text  := Format('%d', [VMConfig.HddHeads]);
    lbSectors.Text := Format('%d', [VMConfig.HddSectors]);
    lbHDDFile.Text := VMConfig.HddFile;
    lbCDRomPresent.Checked := VMConfig.CdPresent;
    lbCDROMFile.Text := VMConfig.CdFile;
    lbFPU.Checked := VMConfig.usefpu;
    edtBios.Text := VMConfig.BIOS;
    edtVgaBios.Text := VMConfig.VGA_BIOS;
    edtFont.Text := VMConfig.Font;

    lbCylinderChange(nil);
  end;

procedure TConfigForm.Button4Click (Sender: TObject);
  var
    OD: TOpenDialog;
  begin

    OD := TOpenDialog.Create(nil);
    OD.Title := 'Select image file';
    od.Filter := '*.img|*.img|*.iso|*.iso|*.ima|*.ima';


    if OD.Execute then
      begin
      lbHDDFile.Text := OD.FileName;
      end;

    OD.Free;

  end;

procedure TConfigForm.Button5Click (Sender: TObject);
  var
    OD: TOpenDialog;
  begin

    OD := TOpenDialog.Create(nil);
    OD.Title := 'Select image file';
    od.Filter := '*.img|*.img|*.iso|*.iso|*.ima|*.ima';


    if OD.Execute then
      begin
      lbFloppyAFile.Text := OD.FileName;
      end;

    OD.Free;
  end;

procedure TConfigForm.Button6Click (Sender: TObject);
  var
    OD: TOpenDialog;
  begin

    OD := TOpenDialog.Create(nil);
    OD.Title := 'Select image file';
    od.Filter := '*.img|*.img|*.iso|*.iso|*.ima|*.ima';


    if OD.Execute then
      begin
      lbCDROMFile.Text := OD.FileName;
      end;

    OD.Free;
  end;

procedure TConfigForm.Button7Click (Sender: TObject);
var
  bytes: integer;
begin
  bytes := StrToIntDef(lbCylinder.Text, 306) * StrToIntDef(lbHeads.Text, 4) *
    StrToIntDef(lbSectors.Text, 17) * BYTES_PER_SECTOR;

  if MessageDlg(Format('Создать пустой образ диска %s?',
    [ExtractFileName(lbHDDFile.Text)]), mtConfirmation, [mbYes, mbNo], 0) = idYes then
    CreateEmptyFile(lbHDDFile.Text, bytes);
end;

procedure TConfigForm.Button8Click (Sender: TObject);
  begin
    FormCreateIso := TFormCreateIso.Create(Self);
    FormCreateIso.ShowModal;
    FormCreateIso.Free;
  end;

procedure TConfigForm.Button9Click (Sender: TObject);
  begin
    if MessageDlg(Format('Создать пустой образ диска %s?',
      [ExtractFileName(lbFloppyAFile.Text)]), mtConfirmation, [mbYes, mbNo], 0) =
      idYes then
      CreateEmptyFile(lbFloppyAFile.Text, 1474560);
  end;

procedure TConfigForm.createEmptyFile (fileName: string; sizeFile: int64);
var
  countWrite, i: integer;
  zeroBuffer: array [0..511] of byte;
  f, modval:  integer;
begin
  for i := 0 to 511 do
    zeroBuffer[i] := 0;

  f := FileCreate(filename);


  countWrite := sizeFile div 512;
  ModVal := sizeFile mod 512;

  for I := 1 to countWrite do
    FileWrite(f, zeroBuffer[0], 512);

  if modval <> 0 then
    FileWrite(f, zeroBuffer[0], modval);


  FileClose(f);

  ShowMessage(Format('Файл %s был создан', [filename]));
end;

procedure TConfigForm.FormActivate (Sender: TObject);
  var
    dr, i, dt: integer;
  begin
    onactivate := nil;

    dr := getlogicaldrives;
    for i := 0 to 31 do
      if (dr shr i) and 1 = 1 then
        begin
        dt := getdrivetype(PChar(char(byte('A') + i) + ':\'));
        if (dt = DRIVE_CDROM) then
          lbCDROMDrive.Items.Append(char(byte('A') + i))
        else
          if (dt = DRIVE_REMOVABLE) then
            lbFloppyADrive.Items.Append(char(byte('A') + i));

        end;

    PageControl1.ActivePageIndex := 0;
    Button3.Click;
  end;

procedure TConfigForm.lbCDROMDriveSelect (Sender: TObject);
  begin
    lbCDROMFile.Text := Format('\\.\CDRom%d', [(Sender as TComboBox).ItemIndex]);
  end;

procedure TConfigForm.lbCylinderChange (Sender: TObject);
var
  bytes: integer;
begin

  bytes := StrToIntDef(lbCylinder.Text, 306) * StrToIntDef(lbHeads.Text, 4) *
    StrToIntDef(lbSectors.Text, 17) * BYTES_PER_SECTOR;
  LbDiskSize.Text := Format('%.f Mb', [bytes / 1024 / 1024]);

end;

procedure TConfigForm.lbFloppyADriveSelect (Sender: TObject);
  begin
    lbFloppyAFile.Text := Format('\\.\%s:',
      [(Sender as TComboBox).Items[(Sender as TComboBox).ItemIndex]]);
  end;

end.
