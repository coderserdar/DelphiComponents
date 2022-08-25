program pstest;

uses
  Forms,
  psmain in 'psmain.pas' {MainForm},
  PStorageIntfs in 'PStorageIntfs.pas',
  PStorage in 'PStorage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
