unit TAPIServiceUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TTAPIService = class(TComponent)
  private
    FUseCanonical: Boolean;
    FAppName: string;
    FCalledParty: string;
    FComment: string;
    FPhoneNumber: string;
    FCountryCode: string;
    FCityCode: string;
    FISDNName: string;
    function GetCityCode: string;
    function GetCountryCode: string;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Execute;
    procedure RefreshCodes;
  published
    { Published declarations }
    property UseCanonical :Boolean read FUseCanonical write FUseCanonical;
    property AppName      :string read FAppName write FAppName;
    property CalledParty  :string read FCalledParty write FCalledParty;
    property Comment      :string read FComment write FComment;
    property PhoneNumber  :string read FPhoneNumber write FPhoneNumber;
    property CountryCode  :string read GetCountryCode stored False;
    property CityCode     :string read GetCityCode stored False;
    property ISDNName     :string read FISDNName write FISDNName;
  end;

procedure Register;

implementation

const
  TAPI_REPLY                  = WM_USER + 99;

  TAPIERR_CONNECTED           = 0;
  TAPIERR_DROPPED             = -1;
  TAPIERR_NOREQUESTRECIPIENT  = -2;
  TAPIERR_REQUESTQUEUEFULL    = -3;
  TAPIERR_INVALDESTADDRESS    = -4;
  TAPIERR_INVALWINDOWHANDLE   = -5;
  TAPIERR_INVALDEVICECLASS    = -6;
  TAPIERR_INVALDEVICEID       = -7;
  TAPIERR_DEVICECLASSUNAVAIL  = -8;
  TAPIERR_DEVICEIDUNAVAIL     = -9;
  TAPIERR_DEVICEINUSE         = -10;
  TAPIERR_DESTBUSY            = -11;
  TAPIERR_DESTNOANSWER        = -12;
  TAPIERR_DESTUNAVAIL         = -13;
  TAPIERR_UNKNOWNWINHANDLE    = -14;
  TAPIERR_UNKNOWNREQUESTID    = -15;
  TAPIERR_REQUESTFAILED       = -16;
  TAPIERR_REQUESTCANCELLED    = -17;
  TAPIERR_INVALPOINTER        = -18;

  TAPIMAXDESTADDRESSSIZE      = 80;
  TAPIMAXAPPNAMESIZE          = 40;
  TAPIMAXCALLEDPARTYSIZE      = 40;
  TAPIMAXCOMMENTSIZE          = 80;
  TAPIMAXDEVICECLASSSIZE      = 40;
  TAPIMAXDEVICEIDSIZE         = 40;

  tapi32                      = 'tapi32.dll';

function tapiRequestMakeCall(DestAddress, AppName, CalledParty, Comment :PChar)
  :Integer; stdcall; external tapi32 name 'tapiRequestMakeCall';
function tapiGetLocationInfo(CountryCode, CityCode :PChar) :Integer; stdcall;
  external tapi32 name 'tapiGetLocationInfo';

procedure Register;
begin
  RegisterComponents('MLR Sysco', [TTAPIService]);
end;

{ TTAPIService }

procedure TTAPIService.Execute;
var
  Res     :Integer;
  ErrMsg  :string;
  ToPhone :string;
  function BuildAreaCode :string;
  begin
    if CityCode <> '' then
      Result := '(' + CityCode + ') ' else
      Result := '';
  end;
  function BuildISDNName :string;
  begin
    if FISDNName <> '' then
      Result := '^' + FISDNName else
      Result := '';
  end;
begin
  if FUseCanonical then begin
    ToPhone := '+' + CountryCode + ' ' + BuildAreaCode + FPhoneNumber +
      BuildISDNName;
  end else
    ToPhone := FPhoneNumber;
  Res := tapiRequestMakeCall(PChar(ToPhone), PChar(FAppName), PChar(FCalledParty),
    PChar(FComment));
  ErrMsg := '';
  case Res of
    TAPIERR_NOREQUESTRECIPIENT:
      ErrMsg := 'There is no request recipient for the TAPI dialing request';
    TAPIERR_INVALDESTADDRESS:
      ErrMsg := 'The destination address is invalid';
    TAPIERR_REQUESTQUEUEFULL:
      ErrMsg := 'The TAPI request queue is full';
    TAPIERR_INVALPOINTER:
      ErrMsg := 'Invalid pointer address';
    0: ErrMsg := '';
  end;
  if ErrMsg <> '' then raise Exception.Create(ErrMsg);
end;

function TTAPIService.GetCityCode: string;
begin
  if FCityCode = '' then RefreshCodes;
  Result := FCityCode;
end;

function TTAPIService.GetCountryCode: string;
begin
  if FCountryCode = '' then RefreshCodes;
  Result := FCountryCode;
end;

procedure TTAPIService.RefreshCodes;
begin
  SetLength(FCountryCode, 10);
  SetLength(FCityCode, 10);
  if tapiGetLocationInfo(PChar(FCountryCode), PChar(FCityCode))
     = TAPIERR_REQUESTFAILED then
    raise Exception.Create('Could not retrieve the location information')
  else begin
    SetLength(FCountryCode, StrLen(PChar(FCountryCode)));
    SetLength(FCityCode, StrLen(PChar(FCityCode)));
  end;
end;

end.
