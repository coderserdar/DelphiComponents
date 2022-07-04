program OverbyteIcsSslMailSnd;

uses
  Forms,
  OverbyteIcsSslMailSnd1 in 'OverbyteIcsSslMailSnd1.pas' {SslSmtpTestForm};

{$R *.RES}

begin
  Application.CreateForm(TSslSmtpTestForm, SslSmtpTestForm);
  Application.Run;
end.
