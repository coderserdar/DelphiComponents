program OverbyteIcsSslFtpServ;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsSslFtpServ1 in 'OverbyteIcsSslFtpServ1.pas' {SslFtpServerForm};

{$R *.RES}

begin
  Application.CreateForm(TSslFtpServerForm, SslFtpServerForm);
  Application.Run;
end.
