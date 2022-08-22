program TestPath;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  PathPlanner in 'PathPlanner.pas',
  PathPlannerReg in 'PathPlannerReg.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
