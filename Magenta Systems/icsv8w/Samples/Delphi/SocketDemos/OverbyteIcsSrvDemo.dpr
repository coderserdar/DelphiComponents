program OverbyteIcsSrvDemo;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsSrvDemo1 in 'OverbyteIcsSrvDemo1.pas' {SrvForm},
  OverbyteIcsSrvDemo2 in 'OverbyteIcsSrvDemo2.pas' {CliForm};

{$R *.RES}

begin
  Application.CreateForm(TSrvForm, SrvForm);
  Application.Run;
end.
