program OverbyteIcsTcpSrv;

uses
  Forms,
  OverbyteIcsTcpSrv1 in 'OverbyteIcsTcpSrv1.pas' {TcpSrvForm};

{$R *.RES}

begin
  Application.CreateForm(TTcpSrvForm, TcpSrvForm);
  Application.Run;
end.
