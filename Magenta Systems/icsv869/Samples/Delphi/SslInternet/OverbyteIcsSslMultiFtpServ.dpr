program OverbyteIcsSslMultiFtpServ;

uses
  Forms,
  OverbyteIcsSslMultiFtpServ1 in 'OverbyteIcsSslMultiFtpServ1.pas' {FtpServerForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFtpServerForm, FtpServerForm);
  Application.Run;
end.
