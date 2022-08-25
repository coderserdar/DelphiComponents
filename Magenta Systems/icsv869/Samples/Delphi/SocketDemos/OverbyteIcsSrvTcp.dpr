program OverbyteIcsSrvTcp;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsSrvTcp1 in 'OverbyteIcsSrvTcp1.pas' {GetGroupsForm},
  OverbyteIcsTcpCmd in 'OverbyteIcsTcpCmd.pas';

{$R *.RES}

begin
{$IFNDEF VER80}
  Application.Initialize;
{$ENDIF}
  Application.CreateForm(TGetGroupsForm, GetGroupsForm);
  Application.Run;
end.
