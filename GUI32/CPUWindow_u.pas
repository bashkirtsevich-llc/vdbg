unit CPUWindow_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, MPHexEditor, vDBGControls, StdCtrls, ActnList;

type
  TCPUform = class(TForm)
    pnlTop: TPanel;
    pnlBottom: TPanel;
    splMiddle: TSplitter;
    pnlMemoryDump: TPanel;
    pnlStackTrace: TPanel;
    splDumpStack: TSplitter;
    pnlCPU: TPanel;
    pnlRegistersAndFlags: TPanel;
    splCPURegistersAndFlags: TSplitter;
    actlstMain: TActionList;
    actGotoOffset: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actGotoOffsetExecute(Sender: TObject);
  private
    { Private declarations }
    MemoryDumpView: TMPHexEditor;
    RegistersView: TDBGRegistersView;
    FlagsView: TDBGRegistersView;
    StackView: TDBGStackView;
    CPUView: TDBGCPUView;

    procedure OnCPUParseCode(aDataPtr: pointer; var aItemInfo: TDisasmItem);
    function OnGetVarVal(Sender: TObject; const Variable: string;
      var Value: Cardinal): Boolean;
  public
    { Public declarations }
    procedure InitInfo;
    procedure UpdateInfo;
    procedure DeleteInfo;
    procedure BeforeDestruction; override;
  end;

var
  CPUform: TCPUform;

implementation

{$R *.dfm}

uses
  CPU, Memory, GotoWindow_u;

procedure TCPUform.actGotoOffsetExecute(Sender: TObject);
var
  _offset: Cardinal;
begin
  if (not MemoryDumpView.Focused) and (not CPUView.Focused) then
    Exit;

  if not TGoToForm.GoToExpression(Self, nil, '', OnGetVarVal, _offset) then
    Exit;

  if MemoryDumpView.Focused then
    MemoryDumpView.Seek(_offset, soFromBeginning);
//  if CPUView.Focused then
//    CPUView.
end;

procedure TCPUform.BeforeDestruction;
begin
  inherited;
  DeleteInfo;
  MemoryDumpView.Free;
  RegistersView.Free;
  FlagsView.Free;
  StackView.Free;
  CPUView.Free;
end;

procedure TCPUform.DeleteInfo;
begin
  MemoryDumpView.AttachMemory(nil, 0);
end;

procedure TCPUform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;
  ShowWindow(Self.Handle, SW_HIDE);
end;

procedure TCPUform.FormCreate(Sender: TObject);
begin
  MemoryDumpView := TMPHexEditor.Create(Self);
  MemoryDumpView.Parent := pnlMemoryDump;
  MemoryDumpView.Align := alClient;

  MemoryDumpView.UnicodeChars := False;
  MemoryDumpView.Align := alClient;

  MemoryDumpView.ParentFont := true;

  MemoryDumpView.ParentFont := False;
  MemoryDumpView.TabOrder := 1;
//  MemoryDumpView.AutoBytesPerRow := False;
//  MemoryDumpView.BytesPerBlock := 1;
//  MemoryDumpView.BytesPerRow := 14;
  MemoryDumpView.BytesPerColumn := 1;
  MemoryDumpView.Translation := tkAsIs;
//  MemoryDumpView.OffsetFormat := '-!10:0x|';
  MemoryDumpView.Colors.Background := clBlack;
  MemoryDumpView.Colors.ChangedBackground := clred;
  MemoryDumpView.Colors.ChangedText := clMaroon;
  MemoryDumpView.Colors.CursorFrame := clNavy;
  MemoryDumpView.Colors.Offset := clSilver;
  MemoryDumpView.Colors.OddColumn := clWhite;
  MemoryDumpView.Colors.EvenColumn := clWhite;
  MemoryDumpView.Colors.CurrentOffsetBackground := clGray;
  MemoryDumpView.Colors.OffsetBackground := clBlack;
  MemoryDumpView.Colors.CurrentOffset := clWhite;
  MemoryDumpView.Colors.Grid := clWhite;
  MemoryDumpView.Colors.NonFocusCursorFrame := clAqua;
  MemoryDumpView.Colors.ActiveFieldBackground := clBlack;
  MemoryDumpView.FocusFrame := True;
  MemoryDumpView.DrawGridLines := False;
  MemoryDumpView.ReadOnlyView := True;
  MemoryDumpView.BorderStyle := bsNone;

  RegistersView := TDBGRegistersView.Create(Self);
  RegistersView.BorderStyle := bsNone;
  RegistersView.Parent := pnlRegistersAndFlags;
  RegistersView.Canvas.Font.Assign(RegistersView.Font);
  RegistersView.Align := alLeft;
  RegistersView.ColWidths[0] := RegistersView.Canvas.TextWidth('FFFF') + 2;
  RegistersView.ColWidths[1] := RegistersView.Canvas.TextWidth('FFFFFFFFF') + 2;
  RegistersView.Width := RegistersView.ColWidths[0] + RegistersView.ColWidths[1] + 4;

//  RegistersView.Options := RegistersView.Options + [goColSizing, goRowSizing];
  RegistersView.HighlightingColor := clMaroon;
  RegistersView.ParentFont := True;
  RegistersView.Canvas.Font.Assign(RegistersView.Font);
  RegistersView.Color := clBlack;
  RegistersView.ColCount := 2;
  RegistersView.DefaultRowHeight := RegistersView.Canvas.TextHeight('F') - 1;
  RegistersView.LinesColor := clBlack;

  //RegistersView.Values.AddLine('EAX').AddValue('FFFFFFFF').AddLine('EBX').AddValue('20').AddLine('EDX').AddValue('20').AddLine('ECX').AddValue('20').Commit;
  RegistersView.Values.Mode := 0;
  RegistersView.Values.AddLine('EAX', ' AX').AddValue('00000000')
                      .AddLine('ECX', ' CX').AddValue('00000000')
                      .AddLine('EDX', ' DX').AddValue('00000000')
                      .AddLine('EBX', ' BX').AddValue('00000000')
                      .AddLine('ESP', ' SP').AddValue('00000000')
                      .AddLine('EBP', ' BP').AddValue('00000000')
                      .AddLine('ESI', ' SI').AddValue('00000000')
                      .AddLine('EDI', ' DI').AddValue('00000000')
                      .AddLine('EIP', ' IP').AddValue('00000000')

                      .AddLine(' ES').AddValue('00000000')
                      .AddLine(' CS').AddValue('00000000')
                      .AddLine(' SS').AddValue('00000000')
                      .AddLine(' DS').AddValue('00000000')
                      .AddLine(' FS').AddValue('00000000')
                      .AddLine(' GS').AddValue('00000000')

                      .AddLine('CR0').AddValue('00000000')
                      .AddLine('CR1').AddValue('00000000')
                      .AddLine('CR2').AddValue('00000000')
                      .AddLine('CR3').AddValue('00000000')
                      .AddLine('CR4').AddValue('00000000')

                      .AddLine('DR0').AddValue('00000000')
                      .AddLine('DR1').AddValue('00000000')
                      .AddLine('DR2').AddValue('00000000')
                      .AddLine('DR3').AddValue('00000000')
                      .AddLine('DR6').AddValue('00000000')
                      .AddLine('DR7').AddValue('00000000')
                      .Commit;
  RegistersView.RowCount := RegistersView.Values.Count;


  FlagsView := TDBGRegistersView.Create(Self);
  FlagsView.BorderStyle := bsNone;
  FlagsView.Parent := pnlRegistersAndFlags;
  FlagsView.Align := alClient;
  FlagsView.ColWidths[0] := FlagsView.Canvas.TextWidth('FF') + 2;
  FlagsView.ColWidths[1] := FlagsView.ColWidths[0];
  FlagsView.Values.AddLine('C').AddValue('0')
                  .AddLine('P').AddValue('0')
                  .AddLine('A').AddValue('0')
                  .AddLine('Z').AddValue('0')
                  .AddLine('S').AddValue('0')
                  .AddLine('T').AddValue('0')
                  .AddLine('D').AddValue('0')
                  .AddLine('O').AddValue('0')
                  .Commit;
  FlagsView.RowCount := FlagsView.Values.Count;

  FlagsView.HighlightingColor := clMaroon;
  FlagsView.ParentFont := true;
  FlagsView.Color := clBlack;
  FlagsView.ColCount := 2;
  FlagsView.DefaultRowHeight := RegistersView.DefaultRowHeight;
  FlagsView.LinesColor := clBlack;
  FlagsView.Width := FlagsView.ColWidths[0] + FlagsView.ColWidths[1] + 4;


  StackView := TDBGStackView.Create(Self);
  StackView.Parent := pnlStackTrace;
  StackView.ParentFont := True;
  StackView.Align := alClient;
  StackView.BorderStyle := bsNone;
  StackView.Color := clBlack;
  StackView.HeadCellHighlightColor := clWhite;
  StackView.HeadCellHighlightFontColor := clGray;
  StackView.DefaultRowHeight := RegistersView.DefaultRowHeight;
  StackView.ColCount := 2;
  StackView.ColWidths[0] := StackView.Canvas.TextWidth('FFFFFFFFFF') + 2;
  StackView.HighlightingColor := clMaroon;

  // CPU
  CPUView := TDBGCPUView.Create(Self);
  CPUView.Parent := pnlCPU;
  CPUView.Align := alClient;
  CPUView.BorderStyle := bsNone;
  CPUView.ParentFont := True;
  CPUView.Color := clBlack;
  CPUView.HeadCellHighlightColor := clWhite;
  CPUView.HeadCellHighlightFontColor := clGray;
  CPUView.ColCount := 3;
  CPUView.ColWidths[0] := CPUView.Canvas.TextWidth('FFFFFFFFFF');
  CPUView.ColWidths[1] := CPUView.Canvas.TextWidth('FFFFFFFFFFFFFFFF');
  CPUView.ColWidths[2] := 300;
  CPUView.DefaultRowHeight := RegistersView.DefaultRowHeight;
  CPUView.HighlightingColor := clMaroon;
  CPUView.OnParseCode := OnCPUParseCode;
//  CPUView.Options := CPUView.Options + [goRowSelect];
end;

procedure TCPUform.InitInfo;
begin
  MemoryDumpView.DataSize := sysmemory.len;
  MemoryDumpView.AttachMemory(@sysmemory.vector[0], MemoryDumpView.DataSize);
  StackView.MemoryData := @sysmemory.vector[0];
  StackView.MemoryDataSize := sysmemory.len;
  RegistersView.Values.Mode := bx_cpu.Is32;
  if bx_cpu.Is32 = 1 then
  begin
    StackView.BytesPerRow := 4;
    StackView.ColWidths[1] := StackView.Canvas.TextWidth('FFFFFFFFFF') + 2;
  end
  else
  begin
    StackView.BytesPerRow := 2;
    StackView.ColWidths[1] := StackView.Canvas.TextWidth('FFFFF') + 2;
  end;
  CPUView.MemoryData := @sysmemory.vector[0];
  CPUView.MemoryDataSize := sysmemory.len;
  CPUView.CodeSegmentValue := bx_cpu.CS; // code segment
  CPUView.CodeSegmentSize := 100;
  CPUView.UpdateDisasm;
end;

procedure TCPUform.OnCPUParseCode(aDataPtr: pointer;
  var aItemInfo: TDisasmItem);
var
  _disasmStr,
  _opcodeStr: string;
  _opcodeLen: Integer;
begin
  // eip == aItemInfo.Address
  if bx_cpu.DisAssemble(aItemInfo.Address, aItemInfo.Address, aItemInfo.Segment,
                        _disasmStr, _opcodeStr, _opcodeLen) = 1 then
  begin
    aItemInfo.Size := _opcodeLen;
    aItemInfo.Opcode := _opcodeStr;
    aItemInfo.Disasm := _disasmStr;
  end;
end;

function TCPUform.OnGetVarVal(Sender: TObject; const Variable: string;
  var Value: Cardinal): Boolean;
begin
  Result := False;
  if (Variable = 'eax') then Value := bx_cpu.EAX;
  if (Variable = 'ax')  then Value := bx_cpu.AX;
  if (Variable = 'al')  then Value := bx_cpu.AL;
  if (Variable = 'ah')  then Value := bx_cpu.AH;
  if (Variable = '[eax]') then Value := bx_cpu.EAX;
  if (Variable = '[ax]')  then Value := bx_cpu.AX;
  if (Variable = '[al]')  then Value := bx_cpu.AL;
  if (Variable = '[ah]')  then Value := bx_cpu.AH;
  //
  if (Variable = 'ecx') then Value := bx_cpu.ECX;
  if (Variable = 'cx')  then Value := bx_cpu.CX;
  if (Variable = 'cl')  then Value := bx_cpu.CL;
  if (Variable = 'ch')  then Value := bx_cpu.CH;
  if (Variable = '[ecx]') then Value := bx_cpu.ECX;
  if (Variable = '[cx]')  then Value := bx_cpu.CX;
  if (Variable = '[cl]')  then Value := bx_cpu.CL;
  if (Variable = '[ch]')  then Value := bx_cpu.CH;
end;

procedure TCPUform.UpdateInfo;
begin
  RegistersView.Values.Mode := bx_cpu.Is32;
  RegistersView.Values.Commit;
  RegistersView.Values.Select('EAX').UpdateValue(IntToHex(bx_cpu.EAX, 8))
                      .Select('ECX').UpdateValue(IntToHex(bx_cpu.ECX, 8))
                      .Select('EDX').UpdateValue(IntToHex(bx_cpu.EDX, 8))
                      .Select('EBX').UpdateValue(IntToHex(bx_cpu.EBX, 8))
                      .Select('ESP').UpdateValue(IntToHex(bx_cpu.ESP, 8))
                      .Select('EBP').UpdateValue(IntToHex(bx_cpu.EBP, 8))
                      .Select('ESI').UpdateValue(IntToHex(bx_cpu.ESI, 8))
                      .Select('EDI').UpdateValue(IntToHex(bx_cpu.EDI, 8))
                      .Select('EIP').UpdateValue(IntToHex(bx_cpu.EIP, 8))

                      .Select(' ES').UpdateValue(IntToHex(bx_cpu.ES, 8))
                      .Select(' CS').UpdateValue(IntToHex(bx_cpu.CS, 8))
                      .Select(' SS').UpdateValue(IntToHex(bx_cpu.SS, 8))
                      .Select(' DS').UpdateValue(IntToHex(bx_cpu.DS, 8))
                      .Select(' FS').UpdateValue(IntToHex(bx_cpu.FS, 8))
                      .Select(' GS').UpdateValue(IntToHex(bx_cpu.GS, 8))
                      .Select('CR0').UpdateValue(IntToHex(bx_cpu.CR0, 8))
                      .Select('CR1').UpdateValue(IntToHex(bx_cpu.CR1, 8))
                      .Select('CR2').UpdateValue(IntToHex(bx_cpu.CR2, 8))
                      .Select('CR3').UpdateValue(IntToHex(bx_cpu.CR3, 8))
                      .Select('CR4').UpdateValue(IntToHex(bx_cpu.CR4, 8))

                      .Select('DR0').UpdateValue(IntToHex(bx_cpu.DR0, 8))
                      .Select('DR1').UpdateValue(IntToHex(bx_cpu.DR1, 8))
                      .Select('DR2').UpdateValue(IntToHex(bx_cpu.DR2, 8))
                      .Select('DR3').UpdateValue(IntToHex(bx_cpu.DR3, 8))
                      .Select('DR6').UpdateValue(IntToHex(bx_cpu.DR6, 8))
                      .Select('DR7').UpdateValue(IntToHex(bx_cpu.DR7, 8));
  FlagsView.Values.Commit;
  FlagsView.Values.Select('C').UpdateValue(IntToStr(bx_cpu.CF))
                  .Select('P').UpdateValue(IntToStr(bx_cpu.PF))
                  .Select('A').UpdateValue(IntToStr(bx_cpu.AF))
                  .Select('Z').UpdateValue(IntToStr(bx_cpu.ZF))
                  .Select('S').UpdateValue(IntToStr(bx_cpu.SF))
                  .Select('T').UpdateValue(IntToStr(bx_cpu.TF))
                  .Select('D').UpdateValue(IntToStr(bx_cpu.DF))
                  .Select('O').UpdateValue(IntToStr(bx_cpu.DF));
  StackView.StackHeadAddress := bx_cpu.StackHeadAddr;

  // CPU
  CPUView.CodeSegmentValue := bx_cpu.CS; // code segment
  CPUView.CodeSegmentSize := bx_cpu.sregs[SEG_REG_CS].cache.segment.limit;
  CPUView.UpdateDisasm;
  CPUView.CurrentLine := bx_cpu.InstrPointer;
  // memory
  MemoryDumpView.Invalidate;
end;

end.
