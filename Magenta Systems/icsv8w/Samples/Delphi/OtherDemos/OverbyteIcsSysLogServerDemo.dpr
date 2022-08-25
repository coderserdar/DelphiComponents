program OverbyteIcsSysLogServerDemo;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsSysLogServerDemo1 in 'OverbyteIcsSysLogServerDemo1.pas' {SysLogServerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSysLogServerForm, SysLogServerForm);
  Application.Run;
end.
