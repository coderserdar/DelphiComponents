program OverbyteIcsServer5;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsSrv5 in 'OverbyteIcsSrv5.pas' {ServerForm};

{$R *.RES}

begin
  Application.CreateForm(TServerForm, ServerForm);
  Application.Run;
end.
