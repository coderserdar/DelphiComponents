program OverbyteIcsFinger;

{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsFinger1 in 'OverbyteIcsFinger1.pas' {FingerDemoForm};

{$R *.RES}

begin
  Application.CreateForm(TFingerDemoForm, FingerDemoForm);
  Application.Run;
end.
