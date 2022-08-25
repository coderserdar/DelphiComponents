program OverbyteIcsClient7;

{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsCli7 in 'OverbyteIcsCli7.pas' {Cli7Form};

{$R *.RES}

begin
  Application.CreateForm(TCli7Form, Cli7Form);
  Application.Run;
end.
