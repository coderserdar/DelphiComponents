program OverbyteIcsDllTst;

{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsDllTst1 in 'OverbyteIcsDllTst1.pas' {DllTestForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDllTestForm, DllTestForm);
  Application.Run;
end.
