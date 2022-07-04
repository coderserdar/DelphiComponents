unit ATxUnpack_Dll;

interface

function FUnpackAll(const fn, sdir: string): boolean;
function FUnpackSingle(const fn, sdir, fnSingle: string): boolean;


implementation

uses
  SysUtils, Unzip, UnzipDll, UnrarDll;

function FUnpackAll(const fn, sdir: string): boolean;
var
  s: string;
begin
  s := LowerCase(ExtractFileExt(fn));
  if s = '.zip' then
  begin
    UnzipInit;
    Result := UnzipAll(fn, sdir) = PK_OK;
    UnzipFree;
  end
  else
  if s = '.rar' then
  begin
    Result := UnrarAll(fn, sdir);
  end
  else
    Result := False;
end;


function FUnpackSingle(const fn, sdir, fnSingle: string): boolean;
var
  s: string;
begin
  s := LowerCase(ExtractFileExt(fn));
  if s = '.zip' then
  begin
    UnzipInit;
    Result := UnzipSingle(fn, sdir, [PChar(fnSingle)]) = PK_OK;
    UnzipFree;
  end
  else
  if s = '.rar' then
  begin
    Result := UnrarSingle(fn, sdir, fnSingle);
  end
  else
    Result := False;
end;


end.
