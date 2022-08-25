program OverbyteIcsSslWebServ;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsSslWebServ1 in 'OverbyteIcsSslWebServ1.pas' {SslWebServForm};

{$R *.RES}

begin
  Application.CreateForm(TSslWebServForm, SslWebServForm);
  Application.Run;
end.
