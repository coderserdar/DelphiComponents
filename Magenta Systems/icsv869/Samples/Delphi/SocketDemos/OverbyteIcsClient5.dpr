program OverbyteIcsClient5;

{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsCli5 in 'OverbyteIcsCli5.pas' {ClientForm};

{$R *.RES}

begin
  Application.CreateForm(TClientForm, ClientForm);
  Application.Run;
end.
