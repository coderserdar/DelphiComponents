unit LsTheme;

interface

uses
  Windows, CommCtrl, SysUtils;

const
  // Windows XP string sort indicator
  HDF_SORTUP   = $0400;
  HDF_SORTDOWN = $0200;

  LVM_SETSELECTEDCOLUMN   = LVM_FIRST + 140;

  function ThemesEnabled: Boolean;

implementation

const
  themelib = 'uxtheme.dll';

var
  IsAppThemed: function: BOOL; stdcall;
  IsThemeActive: function: BOOL; stdcall;
  ThemeLibrary: THandle;
  ReferenceCount: Integer;
  ThemesAvailable,
  NewComCtrls: boolean;
  ComCtlVersion: Integer;

function InitThemeLibrary: Boolean;
begin
  Inc(ReferenceCount);
  if ThemeLibrary = 0 then
  begin
    ThemeLibrary := LoadLibrary(themelib);
    if ThemeLibrary > 0 then
    begin
      IsThemeActive := GetProcAddress(ThemeLibrary, 'IsThemeActive');
      IsAppThemed := GetProcAddress(ThemeLibrary, 'IsAppThemed');
    end;
  end;
  Result := ThemeLibrary > 0;
end;

procedure FreeThemeLibrary;
begin
  if ReferenceCount > 0 then
    Dec(ReferenceCount);
  if (ThemeLibrary <> 0) and (ReferenceCount = 0) then
  begin
    FreeLibrary(ThemeLibrary);
    ThemeLibrary := 0;
    IsThemeActive := nil;
    IsAppThemed := nil;
  end;
end;

function UseThemes: Boolean;
begin
  if (ThemeLibrary > 0) then
    Result := IsAppThemed and IsThemeActive
  else
    Result := False;
end;

function GetFileVersion(const AFileName: string): Cardinal;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := Cardinal(-1);
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  FileName := AFileName;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
          Result:= FI.dwFileVersionMS;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

function GetComCtlVersion: Integer;
begin
  if ComCtlVersion = 0 then
    ComCtlVersion := GetFileVersion(comctl32);
  Result := ComCtlVersion;
end;

function ThemesEnabled: Boolean;
begin
  Result := ThemesAvailable and UseThemes and NewComCtrls;
end;

initialization
  ThemesAvailable := InitThemeLibrary;
  NewComCtrls := GetComCtlVersion >= $00060000; //ComCtlVersionIE6
finalization
  while ReferenceCount > 0 do
    FreeThemeLibrary;
end.
