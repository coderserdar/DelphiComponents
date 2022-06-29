// Written at 01/2004 by Alexey Torgashin
// Based on VersionInfo unit by Sortland Automasjon

unit ATxVersionInfo;

interface

const
  vsCompanyName       = 'CompanyName';
  vsFileDescription   = 'FileDescription';
  vsFileVersion       = 'FileVersion';
  vsInternalName      = 'InternalName';
  vsOriginalFilename  = 'OriginalFilename';
  vsProductName       = 'ProductName';
  vsProductVersion    = 'ProductVersion';
  vsLegalCopyright    = 'LegalCopyright';
  vsLegalTrademarks   = 'LegalTrademarks';
  vsComments          = 'Comments';
  vsPrivateBuild      = 'PrivateBuild';
  vsSpecialBuild      = 'SpecialBuild';

function FGetVersionInfo(const FileName: WideString; const vsKey: string): string;


implementation

uses
  Windows, SysUtils;

function SwapLong(L: Longint): Longint; assembler;
asm
  rol eax, 16;
end;

function FGetVersionInfo(const FileName: WideString; const vsKey: string): string;
var
  buf: pointer;
  bufSize, n: DWORD;
  OK: boolean;
  pInfo: PVSFixedFileInfo;
  p: pointer;
begin
  Result:= '';

  if Win32Platform = VER_PLATFORM_WIN32_NT
    then bufSize:= GetFileVersionInfoSizeW(PWChar(FileName), n)
    else bufSize:= GetFileVersionInfoSizeA(PAnsiChar(AnsiString(FileName)), n);

  if bufSize = 0 then Exit;
  GetMem(buf, bufSize);

  if Win32Platform = VER_PLATFORM_WIN32_NT
    then OK:= GetFileVersionInfoW(PWChar(FileName), 0, bufSize, buf)
    else OK:= GetFileVersionInfoA(PAnsiChar(AnsiString(FileName)), 0, bufSize, buf);

  if OK then
    if vsKey=''
      then
        begin
        if VerQueryValue(buf, '\', pointer(pInfo), n) then
          Result:= Format('%d.%d.%d.%d',
                     [HiWord(pInfo.dwFileVersionMS),
                      LoWord(pInfo.dwFileVersionMS),
                      HiWord(pInfo.dwFileVersionLS),
                      LoWord(pInfo.dwFileVersionLS)]);
        end
      else
        begin
        if VerQueryValue(buf, '\VarFileInfo\Translation', p, n) and
           VerQueryValue(buf, PChar(Format('\StringFileInfo\%.8x\%s',
                              [SwapLong(Longint(p^)), vsKey])), p, n) then
             Result:= string(PChar(p));
        end;

  FreeMem(buf, bufSize);
end;


end.
