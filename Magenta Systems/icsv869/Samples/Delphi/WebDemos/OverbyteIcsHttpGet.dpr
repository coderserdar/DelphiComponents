program OverbyteIcsHttpGet;

{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsHttpGet1 in 'OverbyteIcsHttpGet1.pas' {HttpGetForm};

{$R *.RES}

begin
  Application.CreateForm(THttpGetForm, HttpGetForm);
  Application.Run;
end.
