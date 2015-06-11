unit Display_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, flame_u;

type
  TDisplayform = class(TForm)
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FActive: boolean;
//    Flame: TFlameThread;

    procedure WMPAINT(var aMsg: TWMPaint); message WM_PAINT;
    procedure WMKeyDown (var Msg: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp (var Msg: TWMKeyUp); message WM_KEYUP;
    procedure WMSysKeyDown (var Msg: TWMSysKeyDown); message WM_SYSKEYDOWN;
    procedure WMSysKeyUp (var Msg: TWMSysKeyUp); message WM_SYSKEYUP;
  public
    { Public declarations }
    procedure InitVideo;
    procedure Start;
    procedure Stop;
    procedure AfterConstruction; override;
  end;

var
  Displayform: TDisplayform;

implementation

{$R *.dfm}

uses
  gui32, CONFIG;

procedure TDisplayform.AfterConstruction;
begin
  inherited;
  FActive := False;
//  Flame := TFlameThread.Create(true);
//  Flame.FreeOnTerminate := True;
//  Flame.ConnectCanvas(Self.Canvas.Handle);
//  Flame.Resize(Self.Height, Self.Width);
//  Flame.Resume;
end;

procedure TDisplayform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;
  ShowWindow(Self.Handle, SW_HIDE);
end;

procedure TDisplayform.FormResize(Sender: TObject);
begin
//  Flame.Resize(Self.Height, Self.Width);
end;

procedure TDisplayform.InitVideo;
begin
  MainWnd := Handle;
  Gui32Init(MainWnd, 0);
end;

procedure TDisplayform.Start;
begin
  FActive := True;
//  Flame.Suspend;
end;

procedure TDisplayform.Stop;
begin
  FActive := False;
//  Flame.Resume;
end;

procedure TDisplayform.WMKeyDown(var Msg: TWMKeyDown);
begin
  if FActive then
  begin
    EnterCriticalSection(KeyCS);
    enq_key_event(HiWord(Msg.KeyData) and $01FF, BX_KEY_PRESSED);
    Msg.KeyData  := 0;
    Msg.CharCode := 0;
    leavecriticalsection(KeyCs);
    Msg.Result := 0;
  end
  else
    inherited;
end;

procedure TDisplayform.WMKeyUp(var Msg: TWMKeyUp);
begin
  if FActive then
  begin
    EnterCriticalSection(KeyCS);
    enq_key_event(HiWord(Msg.KeyData) and $01FF, BX_KEY_RELEASED);
    Msg.KeyData  := 0;
    Msg.CharCode := 0;
    leavecriticalsection(KeyCs);
    Msg.Result := 0;
  end
  else
    inherited;
end;

procedure TDisplayform.WMPAINT(var aMsg: TWMPaint);
var
  ps: PAINTSTRUCT;
  VideoDC, MemDC: HDC;
  OldObject: THandle;
begin
// Отрисовка букв в модулях vga (строка 1727), Gui32 (text_update, DrawChar)
  if not FActive then
  begin
    inherited;
    aMsg.Result := 1;
  end else
  begin
    EnterCriticalSection(DrawCS);
    VideoDC := BeginPaint(MainWnd, ps);
    MemDC := CreateCompatibleDC(VideoDC);

    OldObject := SelectObject(MemDC, MemoryBitmap);

    StretchBlt(VideoDC, ps.rcPaint.left, ps.rcPaint.top, ps.rcPaint.right -
       ps.rcPaint.left + 1,
       ps.rcPaint.bottom - ps.rcPaint.top + 1, MemDC,
       ps.rcPaint.left div stretch_factor, ps.rcPaint.top div stretch_factor,
      (ps.rcPaint.right - ps.rcPaint.left + 1) div stretch_factor,
      (ps.rcPaint.bottom - ps.rcPaint.top + 1) div stretch_factor, SRCCOPY);

    SelectObject(MemDC, OldObject);

    DeleteDC(MemDC);
    EndPaint(MainWnd, ps);
    leavecriticalsection(DrawCs);
    aMsg.Result := 0;
  end;
end;

procedure TDisplayform.WMSysKeyDown(var Msg: TWMSysKeyDown);
begin
  if FActive then
  begin
    EnterCriticalSection(KeyCS);
    enq_key_event(HiWord(Msg.KeyData) and $01FF, BX_KEY_PRESSED);
    Msg.KeyData  := 0;
    Msg.CharCode := 0;
    leavecriticalsection(KeyCs);
    Msg.Result := 0;
  end
  else
    inherited;
end;

procedure TDisplayform.WMSysKeyUp(var Msg: TWMSysKeyUp);
begin
  if FActive then
  begin
    EnterCriticalSection(KeyCS);
    enq_key_event(HiWord(Msg.KeyData) and $01FF, BX_KEY_RELEASED);
    Msg.KeyData  := 0;
    Msg.CharCode := 0;
    leavecriticalsection(KeyCs);
    Msg.Result := 0;
  end
  else
    inherited;
end;

end.
