program OverbyteIcsMtSrv;

{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsMtSrv1 in 'OverbyteIcsMtSrv1.pas' {ServerForm},
  OverbyteIcsMtSrv2 in 'OverbyteIcsMtSrv2.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TServerForm, ServerForm);
  Application.Run;
end.
