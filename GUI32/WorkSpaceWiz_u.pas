unit WorkSpaceWiz_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, WorkSpaceWizLogic_u, ExtCtrls, AddModule_u, Grids;

type
  TWorkSpaceWizform = class(TForm)
    pgcMain: TPageControl;
    btnCancel: TButton;
    tsGeneral: TTabSheet;
    tsCPU: TTabSheet;
    tsBoot: TTabSheet;
    tsDisks: TTabSheet;
    btnOk: TButton;
    grpFile: TGroupBox;
    edtWorkspaceName: TLabeledEdit;
    tsMemory: TTabSheet;
    btnWorkSpaceFName: TButton;
    grpMemSize: TGroupBox;
    tbMemSize: TTrackBar;
    edtSize: TEdit;
    lblMB: TLabel;
    grpBIOS: TGroupBox;
    lvModules: TListView;
    btnMemAddFile: TButton;
    btnMemDelFile: TButton;
    lblComment: TLabel;
    mmoComments: TMemo;
    dlgSaveWorkSpace: TSaveDialog;
    grpRegState: TGroupBox;
    sgridRegisters: TStringGrid;
    grpCPUConf: TGroupBox;
    edtCPUName: TLabeledEdit;
    lblSpeed: TLabel;
    tbCPUSpeed: TTrackBar;
    edtCPUSpeed: TEdit;
    tsVGA: TTabSheet;
    grpVGAFont: TGroupBox;
    edtVGAFont: TLabeledEdit;
    btnBrowsFont: TButton;
    dlgOpenVGAFont: TOpenDialog;
    grpBoot: TGroupBox;
    cbbBoot: TComboBox;
    lblBoot: TLabel;
    chkFPU: TCheckBox;
    grpDrives: TGroupBox;
    lstDriveTypes: TListBox;
    pnlDriveConf: TPanel;
    chkReadOnly: TCheckBox;
    lbledtDriveFileName: TLabeledEdit;
    chkMounted: TCheckBox;
    grpHDDConf: TGroupBox;
    lbledtCyl: TLabeledEdit;
    lbledtHeads: TLabeledEdit;
    lbledtSectors: TLabeledEdit;
    btnApply: TButton;
    procedure tbMemSizeChange(Sender: TObject);
    procedure btnMemDelFileClick(Sender: TObject);
    procedure btnMemAddFileClick(Sender: TObject);
    procedure btnWorkSpaceFNameClick(Sender: TObject);
    procedure tbCPUSpeedChange(Sender: TObject);
    procedure btnBrowsFontClick(Sender: TObject);
    procedure lstDriveTypesClick(Sender: TObject);
    procedure chkMountedClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
  private
    { Private declarations }
    // внутр. настройки на каждый диск
    FDriveConsfig: array[0..3] of packed record
      _Active: Boolean;
      _FileName: string;
      _ReadOnly: Boolean;
      //
      _Cyl,
      _Heads,
      _Sectors: Integer;
    end;
    procedure FillRegistersGrid(const aData: array of Cardinal);
    procedure GridToRegisters(var aData: array of Cardinal);
  public
    { Public declarations }
    class function NewWorkspace(aOwner: TComponent;
      var aWorkSpaceConfig: TWorkSpaceConfig): boolean;
  end;

var
  WorkSpaceWizform: TWorkSpaceWizform;

implementation

{$R *.dfm}

{ TWorkSpaceWizform }

procedure TWorkSpaceWizform.btnApplyClick(Sender: TObject);
var
  i: Integer;
begin
  i := lstDriveTypes.ItemIndex;
  if (i < 0) or (i > lstDriveTypes.Items.Count) then
    Exit;

  FDriveConsfig[i]._Active := chkMounted.Checked;
  FDriveConsfig[i]._FileName := lbledtDriveFileName.Text;
  FDriveConsfig[i]._ReadOnly := chkReadOnly.Checked;
  FDriveConsfig[i]._Cyl := StrToIntDef(lbledtCyl.Text, 0);
  FDriveConsfig[i]._Heads:= StrToIntDef(lbledtHeads.Text, 0);
  FDriveConsfig[i]._Sectors := StrToIntDef(lbledtSectors.Text, 0);
end;

procedure TWorkSpaceWizform.btnBrowsFontClick(Sender: TObject);
begin
  if dlgOpenVGAFont.Execute then
    edtVGAFont.Text := dlgOpenVGAFont.FileName;
end;

procedure TWorkSpaceWizform.btnMemAddFileClick(Sender: TObject);
var
  moduleinfo: TModuleStruct;
begin
  // Добавление
  FillChar(moduleinfo, sizeof(moduleinfo), 0);
  if TAddModuleform.AddModule(Self, moduleinfo) then
    with lvModules.Items.Add do
    begin
      Caption := moduleinfo.FileName;
      SubItems.Add(IntToHex(moduleinfo.Address, 8));
    end;
end;

procedure TWorkSpaceWizform.btnMemDelFileClick(Sender: TObject);
begin
  if lvModules.ItemIndex <> -1 then
    lvModules.Items.Delete(lvModules.ItemIndex);
end;

procedure TWorkSpaceWizform.btnWorkSpaceFNameClick(Sender: TObject);
begin
  if not dlgSaveWorkSpace.Execute then
    Exit;

  edtWorkspaceName.Text := dlgSaveWorkSpace.FileName;
end;

procedure TWorkSpaceWizform.chkMountedClick(Sender: TObject);
var
  b: Boolean;
begin
  b := chkMounted.Checked;
  lbledtDriveFileName.Enabled := b;
  chkReadOnly.Enabled := b;

end;

procedure TWorkSpaceWizform.FillRegistersGrid(const aData: array of Cardinal);
var
  _register: TCPURegister;
begin
  sgridRegisters.RowCount := integer(high(TCPURegister)) + 2;
  sgridRegisters.Cells[0, 0] := 'Регистр';
  sgridRegisters.Cells[1, 0] := 'Значение (HEX)';
  for _register := low(TCPURegister) to High(TCPURegister) do
  begin
    sgridRegisters.Cells[0, ord(_register) + 1] := CPURegisterNames[_register];
    sgridRegisters.Cells[1, ord(_register) + 1] := IntToHex(aData[ord(_register)], 8);
  end;
end;

procedure TWorkSpaceWizform.GridToRegisters(var aData: array of Cardinal);
var
  _register: TCPURegister;
begin
  for _register := low(TCPURegister) to High(TCPURegister) do
    aData[ord(_register)] := StrToIntDef('$' + sgridRegisters.Cells[1, ord(_register) + 1],
      aData[ord(_register)]);
end;

procedure TWorkSpaceWizform.lstDriveTypesClick(Sender: TObject);
var
  i: Integer;
begin
  i := lstDriveTypes.ItemIndex;
  if (i < 0) or (i > lstDriveTypes.Items.Count) then
    Exit;

  grpHDDConf.Visible := i = 2;

  chkMounted.Checked        := FDriveConsfig[i]._Active;
  lbledtDriveFileName.Text  := FDriveConsfig[i]._FileName;
  chkReadOnly.Checked       := FDriveConsfig[i]._ReadOnly;
  lbledtCyl.Text            := IntToStr(FDriveConsfig[i]._Cyl);
  lbledtHeads.Text          := IntToStr(FDriveConsfig[i]._Heads);
  lbledtSectors.Text        := IntToStr(FDriveConsfig[i]._Sectors);
end;

class function TWorkSpaceWizform.NewWorkspace(aOwner: TComponent;
  var aWorkSpaceConfig: TWorkSpaceConfig): boolean;
var
  frmWiz: TWorkSpaceWizform;
  i, a: Integer;
begin
  frmWiz := TWorkSpaceWizform.Create(aOwner);
  with frmWiz, aWorkSpaceConfig do
  begin
    Caption := 'Новый проект';
    edtWorkspaceName.Text := WorkSpaceFileName;
    mmoComments.Text := WorkSpaceComment;
    tbMemSize.Position := MemorySize;
    // modules
    for i := 0 to Pred(Length(Modules)) do
      with lvModules.Items.Add do
      begin
        Caption := Modules[i].FileName;
        SubItems.Add(IntToHex(Modules[i].Address, 8));
      end;
    //
    FillRegistersGrid(Registers);
    edtCPUName.Text := CPUName;
    tbCPUSpeed.Position := CPUSpeed;
    edtVGAFont.Text := FontFileName;
    chkFPU.Checked := UseFPU;
    //
    a := 0; // т.к. два флопаря
    for i := 0 to Pred(Length(Devices)) do
    begin
      if Devices[i].DeviceType = dFDD then
      begin
        FDriveConsfig[a]._Active := True;
        FDriveConsfig[a]._FileName := Devices[i].DeviceFileName;
        FDriveConsfig[a]._ReadOnly := Devices[i].DeviceReadOnly;
        Inc(a);
      end else
      if Devices[i].DeviceType = dHDD then
      begin
        FDriveConsfig[2]._Active := True;
        FDriveConsfig[2]._FileName := Devices[i].DeviceFileName;
        FDriveConsfig[2]._ReadOnly := Devices[i].DeviceReadOnly;

        FDriveConsfig[2]._Cyl := Devices[i].DiscConfig.Cyl;
        FDriveConsfig[2]._Heads := Devices[i].DiscConfig.Heads;
        FDriveConsfig[2]._Sectors := Devices[i].DiscConfig.Sectors;
      end else
      if Devices[i].DeviceType = dCDROM then
      begin
        FDriveConsfig[2]._Active := True;
        FDriveConsfig[2]._FileName := Devices[i].DeviceFileName;
      end;
    end;
    lstDriveTypes.ItemIndex := 0;
    lstDriveTypesClick(lstDriveTypes);
    //
    cbbBoot.ItemIndex := Ord(Boot);
  end;
  // вставим дефолтные параметры
  Result := frmWiz.ShowModal = mrOk;
  if Result then
  with frmWiz, aWorkSpaceConfig do
  begin
    WorkSpaceFileName := edtWorkspaceName.Text;
    WorkSpaceComment := mmoComments.Text;
    MemorySize := tbMemSize.Position;
    // modules
    //SetLength(Modules, 0);
    SetLength(Modules, lvModules.Items.Count);
    for i := 0 to Pred(lvModules.Items.Count) do
      with lvModules.Items[i] do
      begin
        Modules[i].FileName := Caption;
        Modules[i].Address := StrToIntDef('$' + SubItems[0], 0);
      end;
    //
    GridToRegisters(Registers);
    CPUName := edtCPUName.Text;
    CPUSpeed := tbCPUSpeed.Position;
    FontFileName := edtVGAFont.Text;
    UseFPU := chkFPU.Checked;
    //
    a := 0;
    for i := Low(FDriveConsfig) to High(FDriveConsfig) do
      if FDriveConsfig[i]._Active then
        inc(a);
    SetLength(Devices, a);
    a := 0;
    for i := Low(FDriveConsfig) to High(FDriveConsfig) do
      if FDriveConsfig[i]._Active then
      begin
        if i in [0, 1] then
          Devices[a].DeviceType := dFDD
        else
        if i = 2 then
        begin
          Devices[a].DeviceType := dHDD;
          Devices[a].DiscConfig.Cyl := FDriveConsfig[i]._Cyl;
          Devices[a].DiscConfig.Heads := FDriveConsfig[i]._Heads;
          Devices[a].DiscConfig.Sectors := FDriveConsfig[i]._Sectors;
        end
        else
        if i = 3 then
          Devices[a].DeviceType := dCDROM;
        Devices[a].DeviceFileName := FDriveConsfig[i]._FileName;
        Devices[a].DeviceReadOnly := FDriveConsfig[i]._ReadOnly;
        inc(a);
      end;
    Boot := TDevice(cbbBoot.ItemIndex);
  end;
end;

procedure TWorkSpaceWizform.tbCPUSpeedChange(Sender: TObject);
begin
  edtCPUSpeed.Text := IntToStr(tbCPUSpeed.Position);
end;

procedure TWorkSpaceWizform.tbMemSizeChange(Sender: TObject);
begin
  //
  edtSize.Text := IntToStr(tbMemSize.Position);
end;

end.
