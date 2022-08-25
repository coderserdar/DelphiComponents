program OverbyteIcsHttpAsy;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsHttpAsy1 in 'OverbyteIcsHttpAsy1.pas' {HttpAsyForm};

{$R *.RES}

begin
  Application.CreateForm(THttpAsyForm, HttpAsyForm);
  Application.Run;
end.
