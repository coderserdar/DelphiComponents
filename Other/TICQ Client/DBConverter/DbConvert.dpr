program DbConvert;
{(C) Alex Demchenko(alex@ritlabs.com)}

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  ICQWorks in '..\Component\ICQWorks.pas',
  ICQDb in '..\Component\ICQDb.pas',
  ICQLang in '..\Component\ICQLang.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
