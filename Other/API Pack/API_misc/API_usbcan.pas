unit API_usbcan;

// component to use with ESD's CAN-USB-MINI device
// revision history
// r1.00, ari pikivirta
// * first revision, all basic functions should be implemented
//   package should contain also drivers for easy and quick start-up,
//   check out the drivers folder under the API_source

interface

uses
  SysUtils, Classes;

type
  TAPI_usbcan = class(TComponent)
  private
    fversion: string;
  protected
  public
    constructor create(aowner: tcomponent); override;
    destructor destroy; override;
  published
    property Version: string read fversion;
  end;

procedure Register;

implementation

const
  versioninfostring = 'r1.00/ari.pikivirta@kolumbus.fi';

{$r *.res}

//------------------------------------------------------------------------------
constructor TAPI_usbcan.create(aowner: tcomponent);
begin
  inherited create(aowner);
  fversion:=versioninfostring;
end;

//------------------------------------------------------------------------------
destructor TAPI_usbcan.destroy;
begin
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API', [TAPI_usbcan]);
end;

end.
 