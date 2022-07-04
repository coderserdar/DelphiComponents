program OverbyteIcsSslFtpServ;

uses
  Forms,
  OverbyteIcsSslFtpServ1 in 'OverbyteIcsSslFtpServ1.pas' {SslFtpServerForm};

{$R *.RES}

begin
  Application.CreateForm(TSslFtpServerForm, SslFtpServerForm);
  Application.Run;
end.
