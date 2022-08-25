program OverbyteIcsHttpRestTst;

uses
  Forms,
  OverbyteIcsHttpRestTst1 in 'OverbyteIcsHttpRestTst1.pas' {HttpRestForm},
  OverbyteIcsHttpRestTst2 in 'OverbyteIcsHttpRestTst2.pas' {FormObject},
  OverbyteIcsLogin in 'OverbyteIcsLogin.pas' {FormLogin};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'ICS HTTPS REST and OAuth Demo';
  Application.CreateForm(THttpRestForm, HttpRestForm);
  Application.CreateForm(TFormObject, FormObject);
  Application.Run;
end.
