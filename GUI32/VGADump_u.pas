unit VGADump_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TVGADumpform = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  VGADumpform: TVGADumpform;

implementation

{$R *.dfm}

procedure TVGADumpform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;
  ShowWindow(Self.Handle, SW_HIDE);
end;

end.
