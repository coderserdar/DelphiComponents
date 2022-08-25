program OverbyteIcsBasNntp;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsBasNntp1 in 'OverbyteIcsBasNntp1.pas' {BasicNntpForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBasicNntpForm, BasicNntpForm);
  Application.Run;
end.
