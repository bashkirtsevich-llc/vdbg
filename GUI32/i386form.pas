unit i386form;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TEmulator = class (TForm)
    TimerFlashGui: TTimer;
    procedure FormPaint (Sender: TObject);
    procedure TimerFlashGuiTimer (Sender: TObject);
  private
    procedure KeyPressed (var Message: TMessage); message WM_KEYDOWN;
  public
    procedure StartEmulation;
    { Public declarations }
  end;

var
  Emulator: TEmulator;

implementation

uses Gui32, Config, cpu, iodev, initsystemnames, service, memory, cputime;

{$R *.dfm}

procedure TEmulator.FormPaint (Sender: TObject);
  begin
    PaintWindow32(Canvas.ClipRect, Canvas.Handle);
  end;

procedure TEmulator.StartEmulation;
  begin
    SetConfiguration(EmsDir + '\conf0.ini'); // fixed for this Mozaa version
    stoprun := False;
    LastMessage := '';
    ips_count := 0;
    m_ips := 0;
    Gui32Init(Handle, 0);
    bx_cpu := BX_CPU_C.Create;
    bx_devices := bx_devices_c.Create;
    bx_devices.init(nil);
    bx_cpu.init(nil);
    InitSystem;
    InitNames;
    timecpu := TTimeCpu.Create(self);
    bx_cpu.reset(0);
    TimerFlashGui.Enabled := True;
      try
        try
        bx_cpu.cpu_loop;
        except
        on e: Exception do
          begin
          BX_PANIC(e.Message);
          end;
        end;
      finally
      TimerFlashGui.Enabled := False;
      Gui32Stop;
      timecpu.Free;
      sysmemory.Free;
      bx_cpu.Free;
      bx_pc_system.Free;
      bx_devices.Free;
      end;
  end;

procedure TEmulator.TimerFlashGuiTimer (Sender: TObject);
  begin
    FlushGui;
  end;

procedure TEmulator.KeyPressed (var Message: TMessage);
  begin
    enq_key_event(Message.LParamHi and $01FF, BX_KEY_PRESSED);
  end;

end.
