program OverbyteIcsSysLogClientDemo;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsSysLogClientDemo1 in 'OverbyteIcsSysLogClientDemo1.pas' {SysLogClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSysLogClientForm, SysLogClientForm);
  Application.Run;
end.
