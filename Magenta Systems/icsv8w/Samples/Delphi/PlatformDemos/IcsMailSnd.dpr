program IcsMailSnd;

uses
  FMX.Forms,
{$IF CompilerVersion < 25}
  FMX.StdCtrls in '..\..\FMX.StdCtrls.pas',
{$IFEND} 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  FMX.Types,
  IcsMailSnd1 in 'IcsMailSnd1.pas' {SmtpTestForm};

{$R *.res}

begin
  Application.Initialize;
  GlobalDisableFocusEffect := TRUE;
  Application.CreateForm(TSmtpTestForm, SmtpTestForm);
  Application.Run;
end.
