program OverbyteIcsFtpServ;

uses
  Forms,
  OverbyteIcsFtpServ1 in 'OverbyteIcsFtpServ1.pas' {FtpServerForm};

{$R *.RES}

begin
  Application.CreateForm(TFtpServerForm, FtpServerForm);
  Application.Run;
end.
