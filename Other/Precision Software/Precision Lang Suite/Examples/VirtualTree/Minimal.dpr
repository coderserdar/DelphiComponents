program Minimal;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  plsLangMan in '..\..\Source\plsLangMan.pas',
  plsController in '..\..\Source\plsController.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

