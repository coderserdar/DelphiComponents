program OverbyteIcsTnDemo;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsTnDemo1 in 'OverbyteIcsTnDemo1.pas' {TnDemoForm};

{$R *.RES}

begin
  Application.CreateForm(TTnDemoForm, TnDemoForm);
  Application.Run;
end.
