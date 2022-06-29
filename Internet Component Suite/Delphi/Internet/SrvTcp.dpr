program SrvTcp;

uses
  Forms,
  SrvTcp1 in 'SrvTcp1.pas' {GetGroupsForm},
  TcpCmd in 'TcpCmd.pas';

{$R *.RES}

begin
{$IFNDEF VER80}
  Application.Initialize;
{$ENDIF}
  Application.CreateForm(TGetGroupsForm, GetGroupsForm);
  Application.Run;
end.
