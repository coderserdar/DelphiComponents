unit ATxWMP;

interface

function IsWMP6Installed: boolean;
function IsWMP9Installed: boolean;

implementation

uses
  Windows, SysUtils, ComObj, ATxRegistry, ATxUtils;

function IsGUIDInstalled(const GUID: TGUID): boolean;
const
  sDefault = '';
begin
  Result:= GetRegKeyStr(HKEY_LOCAL_MACHINE,
    PChar('SOFTWARE\Classes\TypeLib\'+GUIDToString(GUID)+'\1.0\0\win32'),
    '', sDefault) <> sDefault;
end;

const
  LIBID_MediaPlayer: TGUID = '{22D6F304-B0F6-11D0-94AB-0080C74C7E95}';
  LIBID_MediaPlayer9: TGUID = '{6BF52A50-394A-11D3-B153-00C04F79FAA6}';

function IsWMP6Installed: boolean;
begin
  Result:= IsWindowsVista or IsGUIDInstalled(LIBID_MediaPlayer);
end;

function IsWMP9Installed: boolean;
begin
  Result:= IsWindowsVista or IsGUIDInstalled(LIBID_MediaPlayer9);
end;

end.
