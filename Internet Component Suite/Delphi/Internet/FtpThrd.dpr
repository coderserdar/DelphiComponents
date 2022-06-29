program ftpthrd;

uses
  Forms,
  FtpThrd1 in 'FtpThrd1.pas' {ThrdFtpForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TThrdFtpForm, ThrdFtpForm);
  Application.Run;
end.
