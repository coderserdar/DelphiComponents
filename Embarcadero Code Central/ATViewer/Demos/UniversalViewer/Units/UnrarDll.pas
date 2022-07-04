unit UnrarDll;

interface

function UnrarAll(const fn, dir: string): boolean;
function UnrarSingle(const fn, dir, fn1: string): boolean;


implementation

uses
  Windows, SysUtils, RAR, Classes;

function UnrarAll(const fn, dir: string): boolean;
var
  r: TRAR;
begin
  r := TRAR.Create(nil);
  try
    Result := r.OpenFile(fn);
    if Result then
      Result := r.Extract(dir, True, nil);
  finally
    FreeAndNil(r);
  end;
end;


function UnrarSingle(const fn, dir, fn1: string): boolean;
var
  r: TRAR;
  List: TStringList;
begin
  r := TRAR.Create(nil);
  List := TStringList.Create;
  List.Add(fn1);
  try
    Result := r.OpenFile(fn);
    if Result then
      Result := r.Extract(dir, False, List);
  finally
    FreeAndNil(r);
    FreeAndNil(List);
  end;
end;


end.
