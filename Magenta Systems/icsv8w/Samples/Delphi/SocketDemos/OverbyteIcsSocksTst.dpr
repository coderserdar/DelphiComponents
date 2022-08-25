program OverbyteIcsSocksTst;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsSocksTst1 in 'OverbyteIcsSocksTst1.pas' {SocksTestForm};

{$R *.RES}

begin
  Application.CreateForm(TSocksTestForm, SocksTestForm);
  Application.Run;
end.
