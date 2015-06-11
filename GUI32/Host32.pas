unit Host32;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TServerForm = class (TForm)
    vga_out_text: TMemo;
  private
    Counter:      int64;
    ThreadHandle: THandle;
  public
    constructor Create (AOwner: TComponent); override;
    procedure StartThread;
    destructor Destroy; override;
  end;

var
  ServerForm: TServerForm;

implementation

uses Vga;

{$R *.dfm}

{ TServerForm }

procedure ThreadFunction; stdcall;
  begin
  end;

constructor TServerForm.Create (AOwner: TComponent);
  begin
    inherited;
    Counter := 0;
    StartThread;
  end;

destructor TServerForm.Destroy;
  begin
    inherited;
    ExitThread(ThreadHandle);
  end;

procedure TServerForm.StartThread;
  var
    Th: THandle;
  begin
    ThreadHandle := CreateThread(nil, 0, @ThreadFunction, nil, 0, Th);
  end;

end.
