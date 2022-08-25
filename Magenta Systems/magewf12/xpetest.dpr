program xpetest;

uses
  Forms,
  xpemain in 'xpemain.pas' {MainForm},
  EWFAPI in 'EWFAPI.PAS';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
