program OverbyteIcsHttpTst;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsHttpTst1 in 'OverbyteIcsHttpTst1.pas' {HttpTestForm};

{$R *.RES}

begin
{$IFNDEF VER80}
  Application.CreateForm(THttpTestForm, HttpTestForm);
  {$ENDIF}
  Application.Run;
end.
