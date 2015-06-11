unit AddModule_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, WorkSpaceWizLogic_u;

type
  TAddModuleform = class(TForm)
    edtFileName: TLabeledEdit;
    edtAddress: TLabeledEdit;
    btnOk: TButton;
    btnCancel: TButton;
    btnBrows: TButton;
    dlgOpenModule: TOpenDialog;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class function AddModule(aOwner: TComponent;
      var aInfo: TModuleStruct): boolean;
  end;

var
  AddModuleform: TAddModuleform;

implementation

{$R *.dfm}

{ TAddModuleform }

class function TAddModuleform.AddModule(aOwner: TComponent;
  var aInfo: TModuleStruct): boolean;
var
  dlg: TAddModuleform;
begin
  dlg := TAddModuleform.Create(aOwner);
  try
    dlg.edtFileName.Text := aInfo.FileName;
    dlg.edtAddress.Text := IntToHex(aInfo.Address, 8);
    Result := dlg.ShowModal = mrOk;
    if Result then
    begin
      aInfo.FileName := dlg.edtFileName.Text;
      aInfo.Address := StrToIntDef('$'+dlg.edtAddress.Text, 0);
    end;
  finally
    dlg.Free;
  end;
end;

procedure TAddModuleform.FormCreate(Sender: TObject);
begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
end;

end.
