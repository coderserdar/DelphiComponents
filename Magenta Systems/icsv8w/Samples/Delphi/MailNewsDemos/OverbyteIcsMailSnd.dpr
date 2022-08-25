program OverbyteIcsMailSnd;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsMailSnd1 in 'OverbyteIcsMailSnd1.pas' {SmtpTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSmtpTestForm, SmtpTestForm);
  Application.Run;
end.
