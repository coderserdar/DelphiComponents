unit getwinver;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TVersion = (wtWindows95, wtWindows98, wtWindowsNT, wtWin32s, wtUnknown);

  TWinVer = class(TComponent)
  private
    FVersion : TVersion;
    function GetwinType: TVersion;
    { Private declarations }
  protected
    constructor Create (AOwner : TComponent);
    { Protected declarations }
  public
    { Public declarations }
  published
    property WindowsVer : TVersion Read GetWinType write FVersion;
    { Published declarations }
  end;

procedure Register;

implementation

constructor TWinVer.Create (AOwner : TComponent);
begin
  GetWinType;
  inherited;
end;

function TWinVer.GetWinType: TVersion;
var
  VersionInfo: TOSVersionInfo;
begin
  Result := wtUnknown;
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
  GetVersionEx(VersionInfo);
  case VersionInfo.dwPlatformId of
    VER_PLATFORM_WIN32S        : Result := wtWin32s;
    VER_PLATFORM_WIN32_WINDOWS : begin
                                   if VersionInfo.dwMinorVersion=0 then
                                      Result:= wtWindows95
                                   else
                                      Result:= wtWindows98
                                   end;
    VER_PLATFORM_WIN32_NT      : Result := wtWindowsNT;
  end;
  FVersion:=Result;
end;

procedure Register;
begin
  RegisterComponents('Cc', [TWinVer]);
end;

end.
