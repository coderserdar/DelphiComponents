program OverbyteIcsHttpPg;

{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsHttpPg1 in 'OverbyteIcsHttpPg1.pas' {HttpTestForm};

{$R *.RES}

begin
  Application.CreateForm(THttpTestForm, HttpTestForm);
  Application.Run;
end.
