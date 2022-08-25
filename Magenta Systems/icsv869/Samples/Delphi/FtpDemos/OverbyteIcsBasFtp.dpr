program OverbyteIcsBasFtp;

{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms, OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsBasFtp1 in 'OverbyteIcsBasFtp1.pas' {BasicFtpClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBasicFtpClientForm, BasicFtpClientForm);
  Application.Run;
end.
