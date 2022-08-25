program OverbyteIcsHttpAsp;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsHttpAsp1 in 'OverbyteIcsHttpAsp1.pas' {HttpTestForm};

{$R *.RES}

begin
  Application.CreateForm(THttpTestForm, HttpTestForm);
  Application.Run;
end.
