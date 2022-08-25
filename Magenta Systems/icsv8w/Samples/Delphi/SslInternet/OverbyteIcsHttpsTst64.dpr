program OverbyteIcsHttpsTst64;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsHttpsTst1 in 'OverbyteIcsHttpsTst1.pas' {HttpsTstForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THttpsTstForm, HttpsTstForm);
  Application.Run;
end.
