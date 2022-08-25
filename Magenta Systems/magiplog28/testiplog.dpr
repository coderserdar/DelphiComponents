program testiplog;

uses
  madExcept,
  madLinkDisAsm,
  madListModules,
  Forms,
  iplogmain in 'iplogmain.pas' {IpLogForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'IP Log Streaming Tester';
  Application.CreateForm(TIpLogForm, IpLogForm);
  Application.Run;
end.

