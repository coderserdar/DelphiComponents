library u_crypto;

uses SysUtils,
     crypto in 'crypto.pas';

{$R *.RES}

const

  StartKey	= 321;  {Start default key}
  MultKey	  = 1;	{Mult default key}
  AddKey	  = 3;	{Add default key}

procedure EncryptIt(InString: PChar); stdcall;
var
  str_local: String;
begin
  str_local := InString;
  str_local := Encrypt(str_local);
  strcopy (InString, Pchar (str_local));
end;

procedure DecryptIt(InString : PChar); stdcall;
var
  str_local: String;
begin
  str_local := InString;
  str_local := Encrypt(str_local);
  strcopy (InString, Pchar (str_local));
end;

exports
  EncryptIt,
  DecryptIt;

begin

end.
