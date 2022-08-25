program Whois;

uses
  Forms,
  Whois1 in 'Whois1.pas' {WhoisDemoForm},
  WhoisCli in 'WhoisCli.pas';

{$R *.RES}

begin
  Application.CreateForm(TWhoisDemoForm, WhoisDemoForm);
  Application.Run;
end.
