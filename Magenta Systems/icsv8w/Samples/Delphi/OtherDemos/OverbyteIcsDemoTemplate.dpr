program OverbyteIcsDemoTemplate;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsDemoTemplate1 in 'OverbyteIcsDemoTemplate1.pas' {TemplateForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTemplateForm, TemplateForm);
  Application.Run;
end.
