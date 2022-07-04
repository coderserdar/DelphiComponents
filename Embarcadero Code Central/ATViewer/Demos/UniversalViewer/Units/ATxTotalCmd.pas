unit ATxTotalCmd;

interface

function TCDefExe: string; //Path to Total Commander's executable (Totalcmd.exe)
function TCDefIni: string; //Path to TC's config file (named wincmd.ini by default)
function TCDefIniFTP: string; //Path to TC's FTP config file (named wcx_ftp.ini by default)

procedure SetTcIniFilename(const fn: string);
function GetTcIniKey(const section, key: string): string;
procedure SetTcIniKey(const section, key: string; const value: string);


implementation

uses
  Windows, SysUtils, IniFiles,
  ATxSProc, ATxRegistry, ATxUtils;

var
  FIniFile: TIniFile = nil;


procedure SetTcIniFilename(const fn: string);
begin
  if Assigned(FIniFile) then
    FreeAndNil(FIniFile);
  FIniFile:= TIniFile.Create(fn);
end;

function tcDefDir: string;
begin
  Result:= SExpandVars('%COMMANDER_PATH%');
  if SExpanded(Result) then Exit;

  Result:=
    GetRegKeyStr(HKEY_CURRENT_USER, 'Software\Ghisler\Total Commander', 'InstallDir',
    GetRegKeyStr(HKEY_LOCAL_MACHINE, 'Software\Ghisler\Total Commander', 'InstallDir', ''));
end;

function tcDefExe: string;
begin
  Result:= tcDefDir;
  if Result<>'' then
    Result:= Result + '\Totalcmd.exe';
end;

function tcDefIni: string;
begin
  Result:= SExpandVars('%COMMANDER_INI%');
  if SExpanded(Result) then Exit;

  Result:= 
    GetRegKeyStr(HKEY_CURRENT_USER, 'SOFTWARE\Ghisler\Total Commander', 'IniFileName',
    GetRegKeyStr(HKEY_LOCAL_MACHINE, 'SOFTWARE\Ghisler\Total Commander', 'IniFileName',
    ''));
  if Result='' then Exit;

  if Pos('\', Result)=0 then
    Insert('%windir%\', Result, 1);

  if Pos('.\', Result)=1 then
    SReplace(Result, '.', tcDefDir);

  Result:= SExpandVars(Result);
end;

function tcDefIniFtp: string;
begin
  Result:= 
    GetRegKeyStr(HKEY_CURRENT_USER, 'SOFTWARE\Ghisler\Total Commander', 'FtpIniName',
    GetRegKeyStr(HKEY_LOCAL_MACHINE, 'SOFTWARE\Ghisler\Total Commander', 'FtpIniName',
    ''));
  if Result='' then Exit;

  if Pos('\', Result)=0 then
    Insert('%windir%\', Result, 1);

  if Pos('.\', Result)=1 then
    SReplace(Result, '.', tcDefDir);

  Result:= SExpandVars(Result);
end;


//----------------------------------------------------------------------------
function ActualFileName(const section: string): string;
var
  S: string;
begin
  Result:= FIniFile.FileName;

  S:= SExpandVars(FIniFile.ReadString(section, 'RedirectSection', ''));

  if (S='') or (S='0') then
    Exit;

  if (S='1') then
    Result:= SExpandVars(FIniFile.ReadString('Configuration', 'AlternateUserIni', Result))
  else
    Result:= S;

  if Pos('\', Result)=0 then
    Result:= ExtractFilePath(FIniFile.FileName) + Result;
end;

function GetTcIniKey(const section, key: string): string;
var
  FN: string;
begin
  FN:= ActualFileName(section);
  if FN = FIniFile.FileName then
    Result:= FIniFile.ReadString(section, key, '')
  else
    with TIniFile.Create(FN) do
    try
      Result:= ReadString(section, key, '');
    finally
      Free;
    end;
end;

procedure SetTcIniKey(const section, key: string; const value: string);
var
  FN: string;
begin
  FN:= ActualFileName(section);
  if FN = FIniFile.FileName then
    FIniFile.WriteString(section, key, value)
  else
    with TIniFile.Create(FN) do
    try
      WriteString(section, key, value);
    finally
      Free;
    end;
end;


initialization

finalization
  
  if Assigned(FIniFile) then
    FreeAndNil(FIniFile);

end.
