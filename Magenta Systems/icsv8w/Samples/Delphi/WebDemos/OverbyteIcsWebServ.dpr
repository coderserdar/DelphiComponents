program OverbyteIcsWebServ;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsWebServ1 in 'OverbyteIcsWebServ1.pas' {WebServForm};

{$R *.RES}

begin
  Application.CreateForm(TWebServForm, WebServForm);
  Application.Run;
end.
