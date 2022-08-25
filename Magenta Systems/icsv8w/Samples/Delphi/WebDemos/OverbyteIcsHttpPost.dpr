program OverbyteIcsHttpPost;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsHttpPost1 in 'OverbyteIcsHttpPost1.pas' {HttpPostForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THttpPostForm, HttpPostForm);
  Application.Run;
end.
