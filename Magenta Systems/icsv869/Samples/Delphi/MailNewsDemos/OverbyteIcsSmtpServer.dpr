program OverbyteIcsSmtpServer;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsSmtpServ1 in 'OverbyteIcsSmtpServ1.pas' {SmtpSrvForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Test SMTP Server';
  Application.CreateForm(TSmtpSrvForm, SmtpSrvForm);
  Application.Run;
end.
