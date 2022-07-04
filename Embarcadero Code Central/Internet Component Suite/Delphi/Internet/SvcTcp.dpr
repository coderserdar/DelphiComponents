program SvcTcp;

uses
  SvcMgr,
  SvcTcp1 in 'SvcTcp1.pas' {IcsTcpSvc: TService},
  TcpCmd in 'TcpCmd.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TIcsTcpSvc, IcsTcpSvc);
  Application.Run;
end.
