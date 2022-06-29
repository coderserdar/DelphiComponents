unit ATxSHex;

interface

//Conversion from hex encoded string_ (for example, '10 20 AA FF': 4 chars) to normal string_.
//Hex string_ must contain 2*N hex digits. Spaces are ignored.
function SHexToNormal(const HexStr: string; var ResStr: string): Boolean;

//Conversion of string_ to hex form, digits are separated with spaces.
function SToHex(const S: string): string;

//Conversion from hex to Integer.
function HexToIntDef(const S: string; const Default: Int64): Int64;


implementation

uses
  SysUtils, ATxSProc;

function SHexDigitToInt(Hex: char; var Int: LongWord): Boolean;
var
  ch: char;
begin
  Result := True;
  Int := 0;
  ch := UpCase(Hex);
  case ch of
    '0'..'9':
      Int := Ord(ch) - Ord('0');
    'A'..'F':
      Int := Ord(ch) - Ord('A') + 10;
    else
      Result := False;
  end;
end;


function SHexWordToInt(const Hex: string; var Int: LongWord): Boolean;
var
  Int1, Int2: LongWord;
begin
  Result := False;
  if Length(Hex) = 1 then
    Result := SHexDigitToInt(Hex[1], Int)
  else
  if Length(Hex) = 2 then
  begin
    Result :=
      SHexDigitToInt(Hex[1], Int1) and
      SHexDigitToInt(Hex[2], Int2);
    if Result then
      Int := Int1 * 16 + Int2;
  end;
end;


function SHexToNormal(const HexStr: string; var ResStr: string): Boolean;
var
  S: string;
  Int: LongWord;
  i: Integer;
begin
  ResStr := '';
  Result := False;

  S := HexStr;
  SReplaceAll(S, ' ', '');

  if (Length(S) mod 2) > 0 then Exit;

  for i := 1 to Length(S) div 2 do
  begin
    if not SHexWordToInt(S[2 * i - 1] + S[2 * i], Int) then Exit;
    ResStr := ResStr + Chr(Int);
  end;

  Result := True;
end;


function SToHex(const S: string): string;
var
  i: Integer;
begin
  Result := '';

  for i := 1 to Length(S) do
    Result := Result + IntToHex(Ord(S[i]), 2) + ' ';

  if Result <> '' then
    Delete(Result, Length(Result), 1);
end;


function HexToIntDef(const S: string; const Default: Int64): Int64;
var
  i: Integer;
  N: LongWord;
begin
  Result := 0;
  for i := 1 to Length(S) do
  begin
    if not SHexDigitToInt(S[i], N) then
      begin Result := Default; Exit end;
    Result := Result * $10 + N;
  end;
end;

end.
