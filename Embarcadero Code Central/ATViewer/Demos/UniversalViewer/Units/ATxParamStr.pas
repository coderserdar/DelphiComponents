unit ATxParamStr;

interface

function SParamCount: integer;
function SParamStrW(Index: integer): WideString;
function SParamExe: WideString;
function SParamDir: WideString;

function SLangFN(const Name: string): string;
function SIconsFN(const Name: string): string;


implementation

uses
  Windows, SysUtils,
  TntSystem, TntSysUtils, ATxSProc;

function SParamCount: integer;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := TntSystem.WideParamCount
  else
    Result := System.ParamCount;
end;

function SParamStrW(Index: integer): WideString;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := TntSystem.WideParamStr(Index)
  else
    Result := System.ParamStr(Index);
end;

function SParamExe: WideString;
begin
  Result := SParamStrW(0);
end;

function SParamDir: WideString;
begin
  Result := WideExtractFileDir(SParamExe);
end;


function SLangFN(const Name: string): string;
begin
  Result := SFormatW('%s\Language\%s.lng', [SParamDir, Name]);
end;

function SIconsFN(const Name: string): string;
begin
  Result := SFormatW('%s\Icons\%s.bmp', [SParamDir, Name]);
end;


end.
