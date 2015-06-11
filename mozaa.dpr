program mozaa;

uses
  Windows,
  Forms,
  CONFIG in 'Cpu\CONFIG.pas',
  Service in 'Service\Service.pas',
  jumpfar in 'Cpu\jumpfar.pas',
  cpu in 'Cpu\cpu.pas',
  SysUtils,
  Iodev in 'IODEV\Iodev.pas',
  dma in 'IODEV\dma.pas',
  PIC in 'IODEV\PIC.pas',
  Unmapped in 'IODEV\Unmapped.pas',
  cmos in 'IODEV\cmos.pas',
  PCI in 'IODEV\PCI.pas',
  pit in 'IODEV\pit.pas',
  Memory in 'Cpu\Memory.pas',
  keyboard in 'IODEV\keyboard.pas',
  KeyMap in 'IODEV\KeyMap.pas',
  Scd in 'IODEV\Scd.pas',
  vga in 'IODEV\vga.pas',
  HDD in 'IODEV\HDD.pas',
  cdrom in 'IODEV\cdrom.pas',
  Gui32 in 'Gui32\Gui32.pas',
  floppy in 'IODEV\floppy.pas',
  pit_wrap in 'IODEV\pit_wrap.pas',
  pit82c54 in 'IODEV\pit82c54.pas',
  serv_param in 'Service\serv_param.pas',
  m2fMain in 'win32\m2fMain.pas' {fMain1},
  thPerif in 'win32\thPerif.pas',
  CreateIsoForm in 'win32\CreateIsoForm.pas' {FormCreateIso},
  frmConfig in 'frmConfig.pas' {ConfigForm},
  fpu in 'FPU\fpu.pas',
  DebugMain_u in 'GUI32\DebugMain_u.pas' {frmVDebug},
  vDebugCore_u in 'vDebugCore_u.pas',
  CPUWindow_u in 'GUI32\CPUWindow_u.pas' {CPUform},
  MPHexEditor in 'CTRLS\MPHexEditor.pas',
  mphexeditorex in 'CTRLS\mphexeditorex.pas',
  vDBGControls in 'CTRLS\vDBGControls.pas' {,
  ExceptDialog_u in 'ExceptHandler\ExceptDialog_u.pas',
  ExceptHandler_u in 'ExceptHandler\ExceptHandler_u.pas',
  madDisasm in 'ExceptHandler\madDisasm.pas',
  madStrings in 'ExceptHandler\madStrings.pas',
  madTools in 'ExceptHandler\madTools.pas',
  madTypes in 'ExceptHandler\madTypes.pas'},
  Display_u in 'GUI32\Display_u.pas' {Displayform},
  WorkSpaceWiz_u in 'GUI32\WorkSpaceWiz_u.pas' {WorkSpaceWizform},
  WorkSpaceWizLogic_u in 'GUI32\WorkSpaceWizLogic_u.pas',
  AddModule_u in 'GUI32\AddModule_u.pas' {AddModuleform},
  VGADump_u in 'GUI32\VGADump_u.pas' {VGADumpform},
  FastBMP in '..\..\..\Gui32\FastBMP.pas',
  flame_u in 'GUI32\flame_u.pas',
  FastRGB in '..\..\..\Gui32\FastRGB.pas',
  VGAWindow_u in 'GUI32\VGAWindow_u.pas' {VGAform},
  GotoWindow_u in 'GUI32\GotoWindow_u.pas' {GoToForm};

{.$R res\mozaa.res}
{$R WindowsXP.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ДОС Моза';
//  Application.CreateForm(TfMain1, fMain1);
  Application.CreateForm(TfrmVDebug, frmVDebug);
  Application.CreateForm(TfrmVDebug, frmVDebug);
  Application.CreateForm(TCPUform, CPUform);
  Application.CreateForm(TDisplayform, Displayform);
  Application.CreateForm(TAddModuleform, AddModuleform);
  Application.CreateForm(TVGAform, VGAform);

  //  Application.CreateForm(TGoToForm, GoToForm);
  //  Application.CreateForm(TWorkSpaceWizform, WorkSpaceWizform);
  //  fMain1.Show;
  Application.Run;
end.
