program OverbyteIcsTnSrv;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsTnSrv1 in 'OverbyteIcsTnSrv1.pas' {ServerForm},
  OverbyteIcsTnSrv2 in 'OverbyteIcsTnSrv2.pas' {ClientForm};

{$R *.RES}

begin
  Application.CreateForm(TServerForm, ServerForm);
  Application.Run;
end.
