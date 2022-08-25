program testcrypt;

uses
  Forms,
  umain in 'umain.pas' {Form1},
  UDESCryp in 'udescryp.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
