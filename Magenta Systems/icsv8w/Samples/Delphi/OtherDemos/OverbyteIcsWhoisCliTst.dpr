program OverbyteIcsWhoisCliTst;

uses
  Forms,
  OverbyteIcsWhoisCliTst1 in 'OverbyteIcsWhoisCliTst1.pas' {WhoisDemoForm};

{$R *.RES}

begin
  Application.CreateForm(TWhoisDemoForm, WhoisDemoForm);
  Application.Run;
end.
