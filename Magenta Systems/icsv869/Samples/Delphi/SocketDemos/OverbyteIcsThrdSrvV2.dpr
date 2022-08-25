program OverbyteIcsThrdSrvV2;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsThrdSrvV2_1 in 'OverbyteIcsThrdSrvV2_1.pas' {TcpSrvForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTcpSrvForm, TcpSrvForm);
  Application.Run;
end.
