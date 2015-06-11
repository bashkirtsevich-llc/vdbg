unit Configuration;

interface

type

  TBootDevice = (BootFromFloppy, BootFromHDD);
  TDeviceCfg  = (FloppyNone, FloppyA, HDDNone, HDDCPresent);

  TConfiguration = class
  private
    function CheckFName (Section: string): string;
  public
    BiosFile:    string;
    VGABiosFile: string;
    MemoryMB:    word;
    BootDevice:  TBootDevice;
    FloppyFile, HDDFile: string;
    DeviceCfg:   TDeviceCfg;
    constructor Create;
  end;

implementation

{ TConfiguration }

function TConfiguration.CheckFName (Section: string): string;
  begin

  end;

constructor TConfiguration.Create;
  begin

  end;

end.
