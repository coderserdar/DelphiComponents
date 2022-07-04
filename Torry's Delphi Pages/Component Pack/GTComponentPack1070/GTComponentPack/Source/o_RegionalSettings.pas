{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtRegionalSettings                             }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{                                                       }
{*******************************************************}
unit o_RegionalSettings;

interface
uses
  Classes
  ;

type
  TgtRegionalSettings = class(TComponent)
  private
    { Private declarations }
    FLocaleBuffer : PChar;
    FSysMonthNames: TStrings;
    FSysDayNames: TStrings;
    function GetSysCountryCode: Word;
    function GetSysAnsiCodePage: string;
    function GetSysLocalLanguageName: string;
    function GetSysLanguageID:Word;
    function GetSysDateSeparator: string;
    function GetSysLongDateFormat: string;
    function GetSysShortDateFormat: string;
    function GetSysTimeSeparator: string;
    function GetSysTimeFormat: string;
    function GetSysDecimalSeparator: string;
    function GetSysDigitGrouping: string;
    function GetSysThousandSeparator: string;
    function GetSysCountryName: string;
    function GetSysOemCodePage: string;
    function GetSysCurrencySymbol: string;
    function GetSysCurrencyDecimalSeparator: string;
    function GetSysCurrencyThousandSeparator: string;
    function GetSysCurrenctyDecimalDigits: string;
    function GetSysDecimalDigits: string;
  protected
    { Protected declarations }
    procedure GetMonthsAndDayNames;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent);override;
    destructor  Destroy;override;
  public
    property SysLanguageId               : Word     read GetSysLanguageID;
    property SysLocalLanguageName        : string   read GetSysLocalLanguageName;
    property SysCountryCode              : Word     read GetSysCountryCode;
    property SysCountryName              : string   read GetSysCountryName;
    property SysAnsiCodePage             : string   read GetSysAnsiCodePage;
    property SysOemCodePage              : string   read GetSysOemCodePage;
    property SysDateSeparator            : string   read GetSysDateSeparator;
    property SysTimeSeparator            : string   read GetSysTimeSeparator;
    property SysTimeFormat               : string   read GetSysTimeFormat;
    property SysShortDateFormat          : string   read GetSysShortDateFormat;
    property SysLongDateFormat           : string   read GetSysLongDateFormat;
    property SysDecimalSeparator         : string   read GetSysDecimalSeparator;
    property SysThousandSeparator        : string   read GetSysThousandSeparator;
    property SysDecimalDigits            : string   read GetSysDecimalDigits;
    property SysDigitGrouping            : string   read GetSysDigitGrouping;
    property SysCurrencySymbol           : string   read GetSysCurrencySymbol;
    property SysCurrencyDecimalSeparator : string   read GetSysCurrencyDecimalSeparator;
    property SysCurrencyThousandSeparator: string   read GetSysCurrencyThousandSeparator;
    property SysCurrencyDecimalDigits    : string   read GetSysCurrenctyDecimalDigits;
    property SysDayNames                 : TStrings read FSysDayNames;
    property SysMonthNames               : TStrings read FSysMonthNames;
  published
    { Published declarations}

  end;

implementation
uses
   Windows
  ,SysUtils
  ;

{ TgtRegionalSettings }
{------------------------------------------------------------------------------}
constructor TgtRegionalSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLocaleBuffer := StrAlloc(255);
  FSysDayNames  := TStringList.Create;
  FSysMonthNames:= TStringList.Create;
  GetMonthsAndDayNames;
end;
{------------------------------------------------------------------------------}
destructor TgtRegionalSettings.Destroy;
begin
  StrDispose(FLocaleBuffer);
  FreeAndNil(FSysDayNames);
  FreeAndNil(FSysMonthNames);
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtRegionalSettings.GetMonthsAndDayNames;
begin
 FSysDayNames.Clear;
 FSysMonthNames.Clear;

 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SDAYNAME1,FLocaleBuffer, 255);// Monday
 FSysDayNames.Add(FLocaleBuffer);
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SDAYNAME2,FLocaleBuffer, 255);// Tuesday
 FSysDayNames.Add(FLocaleBuffer);
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SDAYNAME3,FLocaleBuffer, 255);// Wednesday
 FSysDayNames.Add(FLocaleBuffer);
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SDAYNAME4,FLocaleBuffer, 255);// Thursday
 FSysDayNames.Add(FLocaleBuffer);
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SDAYNAME5,FLocaleBuffer, 255);// Friday
 FSysDayNames.Add(FLocaleBuffer);
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SDAYNAME6,FLocaleBuffer, 255);// Saturday
 FSysDayNames.Add(FLocaleBuffer);
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SDAYNAME7,FLocaleBuffer, 255);// Sunday
 FSysDayNames.Add(FLocaleBuffer);

 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SMONTHNAME1,FLocaleBuffer, 255);// January
 FSysMonthNames.Add(FLocaleBuffer);
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SMONTHNAME2,FLocaleBuffer, 255);// February
 FSysMonthNames.Add(FLocaleBuffer);
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SMONTHNAME3,FLocaleBuffer, 255);// March
 FSysMonthNames.Add(FLocaleBuffer);
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SMONTHNAME4,FLocaleBuffer, 255);// April
 FSysMonthNames.Add(FLocaleBuffer);
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SMONTHNAME5,FLocaleBuffer, 255);// May
 FSysMonthNames.Add(FLocaleBuffer);
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SMONTHNAME6,FLocaleBuffer, 255);// June
 FSysMonthNames.Add(FLocaleBuffer);
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SMONTHNAME7,FLocaleBuffer, 255);// July
 FSysMonthNames.Add(FLocaleBuffer);
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SMONTHNAME8,FLocaleBuffer, 255);// August
 FSysMonthNames.Add(FLocaleBuffer);
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SMONTHNAME9,FLocaleBuffer, 255);// September
 FSysMonthNames.Add(FLocaleBuffer);
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SMONTHNAME10,FLocaleBuffer, 255);// October
 FSysMonthNames.Add(FLocaleBuffer);
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SMONTHNAME11,FLocaleBuffer, 255);// November
 FSysMonthNames.Add(FLocaleBuffer);
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SMONTHNAME12,FLocaleBuffer, 255);// December
 FSysMonthNames.Add(FLocaleBuffer);
end;
{------------------------------------------------------------------------------}

// Getters - Setters \\
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysLocalLanguageName: string;
begin
  GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SLANGUAGE,FLocaleBuffer, 255);
  Result := FLocaleBuffer;
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysAnsiCodePage: string;
begin
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_IDEFAULTANSICODEPAGE,FLocaleBuffer, 255);
 Result := FLocaleBuffer;
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysOemCodePage: string;
begin
 GetLocaleInfo(GetSystemDefaultLCID, LOCALE_IDEFAULTCODEPAGE,FLocaleBuffer, 255);
 Result := FLocaleBuffer;
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysCountryCode: Word;
begin
 Result := GetLocaleInfo(GetSystemDefaultLCID, LOCALE_IDEFAULTCOUNTRY,FLocaleBuffer, 255);
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysCountryName: string;
begin
  GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SCOUNTRY,FLocaleBuffer, 255);
  Result := FLocaleBuffer;
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysLanguageID: Word;
begin
 Result := GetLocaleInfo(GetSystemDefaultLCID, LOCALE_IDEFAULTLANGUAGE,FLocaleBuffer, 255);
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysDateSeparator: string;
begin
  GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SDATE,FLocaleBuffer, 255);
  Result := FLocaleBuffer;
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysLongDateFormat: string;
begin
  GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SLONGDATE,FLocaleBuffer, 255);
  Result := FLocaleBuffer;
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysShortDateFormat: string;
begin
  GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SSHORTDATE,FLocaleBuffer, 255);
  Result := FLocaleBuffer;
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysTimeSeparator: string;
begin
  GetLocaleInfo(GetSystemDefaultLCID, LOCALE_STIME,FLocaleBuffer, 255);
  Result := FLocaleBuffer;
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysTimeFormat: string;
begin
  GetLocaleInfo(GetSystemDefaultLCID, LOCALE_STIMEFORMAT,FLocaleBuffer, 255);
  Result := FLocaleBuffer;
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysDecimalSeparator: string;
begin
  GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SDECIMAL,FLocaleBuffer, 255);
  Result := FLocaleBuffer;
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysDigitGrouping: string;
begin
  GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SGROUPING,FLocaleBuffer, 255);
  Result := FLocaleBuffer;
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysThousandSeparator: string;
begin
  GetLocaleInfo(GetSystemDefaultLCID, LOCALE_STHOUSAND,FLocaleBuffer, 255);
  Result := FLocaleBuffer;
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysDecimalDigits: string;
begin
  GetLocaleInfo(GetSystemDefaultLCID, LOCALE_IDIGITS,FLocaleBuffer, 255);
  Result := FLocaleBuffer;
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysCurrencySymbol: string;
begin
  GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SCURRENCY,FLocaleBuffer, 255);
  Result := FLocaleBuffer;
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysCurrencyDecimalSeparator: string;
begin
  GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SMONDECIMALSEP,FLocaleBuffer, 255);
  Result := FLocaleBuffer;
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysCurrencyThousandSeparator: string;
begin
  GetLocaleInfo(GetSystemDefaultLCID, LOCALE_SMONTHOUSANDSEP,FLocaleBuffer, 255);
  Result := FLocaleBuffer;
end;
{------------------------------------------------------------------------------}
function TgtRegionalSettings.GetSysCurrenctyDecimalDigits: string;
begin
  GetLocaleInfo(GetSystemDefaultLCID, LOCALE_ICURRDIGITS,FLocaleBuffer, 255);
  Result := FLocaleBuffer;
end;
{------------------------------------------------------------------------------}

end.

