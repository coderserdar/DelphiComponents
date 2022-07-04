program OverbyteIcsMailSnd;

uses
  Forms,
  OverbyteIcsMailSnd1 in 'OverbyteIcsMailSnd1.pas' {SmtpTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSmtpTestForm, SmtpTestForm);
  Application.Run;
end.
