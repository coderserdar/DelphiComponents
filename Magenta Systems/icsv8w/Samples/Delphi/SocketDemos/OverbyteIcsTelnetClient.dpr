program OverbyteIcsTelnetClient;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsTelnetClient1 in 'OverbyteIcsTelnetClient1.pas' {TelnetForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTelnetForm, TelnetForm);
  Application.Run;
end.
