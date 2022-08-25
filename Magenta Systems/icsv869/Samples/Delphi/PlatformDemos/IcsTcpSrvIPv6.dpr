program IcsTcpSrvIPv6;

uses
  FMX.Forms,
{$IF CompilerVersion < 25}
  FMX.StdCtrls in '..\..\FMX.StdCtrls.pas',
{$IFEND}
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  FMX.Types,
  IcsTcpSrv1IPv6 in 'IcsTcpSrv1IPv6.pas' {TcpSrvForm};

{$R *.res}

begin
  Application.Initialize;
  GlobalDisableFocusEffect := True;
  Application.CreateForm(TTcpSrvForm, TcpSrvForm);
  Application.Run;
end.
