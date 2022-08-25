program OverbyteIcsThrdSrvV3;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsThrdSrvV3_1 in 'OverbyteIcsThrdSrvV3_1.pas' {ThrdSrvForm};

{$R *.RES}

begin
  Application.CreateForm(TThrdSrvForm, ThrdSrvForm);
  Application.Run;
end.
