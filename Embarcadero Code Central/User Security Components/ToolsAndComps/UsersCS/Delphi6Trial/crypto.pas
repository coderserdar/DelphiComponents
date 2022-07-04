unit crypto;

interface

function Encrypt(InString: String): String;

function Decrypt(InString: String): String;

implementation

const
  StartKey	= 321;  {Start default key}
  MultKey	= 1;	{Mult default key}
  AddKey	= 3;	{Add default key}

function BinaryMethod(const aText: string; aKey: word): string;
var
   nInd: Integer;
begin
   Result := aText;
   aKey := aKey shr 8;
   if aKey = 0 then
     exit;
   for nInd := 1 to length(Result) do
     Result[nInd] := char(byte(Result[nInd]) xor aKey);
end;

function Encrypt(InString: String): String;
var
  str_local, str_result: String;
begin
{$R-}
{$Q-}
  str_local  := InString;
  str_result := BinaryMethod(str_local,StartKey);
  Result := str_result;
{$R+}
{$Q+}
end;

function Decrypt(InString: String): String;
var
  str_local, str_result: String;
begin
{$R-}
{$Q-}
  str_local  := InString;
  str_result := BinaryMethod(str_local,StartKey);
  Result := str_result;
{$R+}
{$Q+}
end;

end.
