program OverbyteIcsPingTst;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsPingTst1 in 'OverbyteIcsPingTst1.pas' {PingTstForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPingTstForm, PingTstForm);
  Application.Run;
end.
