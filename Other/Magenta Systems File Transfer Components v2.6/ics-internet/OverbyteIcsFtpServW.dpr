program OverbyteIcsFtpServW;

uses
  Forms,
  OverbyteIcsFtpServW1 in 'OverbyteIcsFtpServW1.pas' {FtpServerForm};

{$R *.RES}

begin
  Application.CreateForm(TFtpServerForm, FtpServerForm);
  Application.Run;
end.
