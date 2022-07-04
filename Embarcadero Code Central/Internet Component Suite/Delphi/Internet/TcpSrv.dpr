program TcpSrv;

uses
  Forms,
  TcpSrv1 in 'TcpSrv1.pas' {TcpSrvForm};

{$R *.RES}

begin
  Application.CreateForm(TTcpSrvForm, TcpSrvForm);
  Application.Run;
end.
