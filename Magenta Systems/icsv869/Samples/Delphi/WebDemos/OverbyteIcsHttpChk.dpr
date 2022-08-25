program OverbyteIcsHttpChk;

{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsHttpChk1 in 'OverbyteIcsHttpChk1.pas' {CheckUrlForm};

{$R *.RES}

begin
  Application.CreateForm(TCheckUrlForm, CheckUrlForm);
  Application.Run;
end.
