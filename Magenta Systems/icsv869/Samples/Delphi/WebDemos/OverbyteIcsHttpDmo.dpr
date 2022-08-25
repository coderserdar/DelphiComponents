program OverbyteIcsHttpDmo;



{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsHttpDmo1 in 'OverbyteIcsHttpDmo1.pas' {HttpToMemoForm},
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas';

{$R *.RES}

begin
  Application.CreateForm(THttpToMemoForm, HttpToMemoForm);
  Application.Run;
end.
