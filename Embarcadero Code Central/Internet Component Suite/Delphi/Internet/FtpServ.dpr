program FtpServ;

uses
  Forms,
  FtpServ1 in 'FtpServ1.pas' {FtpServerForm};

{$R *.RES}

begin
  Application.CreateForm(TFtpServerForm, FtpServerForm);
  Application.Run;
end.
