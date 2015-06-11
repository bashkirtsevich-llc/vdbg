unit WorkSpaceWizLogic_u;

interface

type
  TCPURegister = (
    crEAX, crECX, crEDX, crEBX, crESP, crEBP, crESI, crEDI,
    crES, crCS, crSS, crDS, crFS, crGS,
    crDR0, crDR1, crDR2, crDR3, crDR6, crDR7,
    crCR0, crCR1, crCR2, crCR3, crCR4, crEIP);

  TDevice = (dFDD, dHDD, dCDROM);

  TModuleStruct = packed record
    FileName: string;
    Address: Cardinal;
  end;

  TDiscConfig = packed record
    Cyl,
    Heads,
    Sectors: Cardinal;
  end;

  TDeviceStruct = packed record
    DeviceType: TDevice;
    DeviceFileName: string;
    DeviceReadOnly: boolean;
    DiscConfig: TDiscConfig;
  end;

  TWorkSpaceConfig = packed record
    // GENERAL
    WorkSpaceFileName: string;
    WorkSpaceComment: string;
    // MEMORY
    MemorySize: Integer; // in MBytes
    Modules: array of TModuleStruct;
    // CPU
    CPUName: string;
    Registers: array[TCPURegister] of Cardinal;
    CPUSpeed: integer; // * 1000
    UseFPU: Boolean;
    // VGA
    FontFileName: string;
    // DEVICES
    Devices: array of TDeviceStruct;
    // BOOT
    Boot: TDevice;
  end;

const
  CPURegisterNames: array[TCPURegister] of string = (
    'EAX', 'ECX', 'EDX', 'EBX', 'ESP', 'EBP', 'ESI', 'EDI',
    'ES', 'CS', 'SS', 'DS', 'FS', 'GS',
    'DR0', 'DR1', 'DR2', 'DR3', 'DR6', 'DR7',
    'CR0', 'CR1', 'CR2', 'CR3', 'CR4', 'EIP'
  );

implementation

end.
