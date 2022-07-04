program ResStr;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  ConstsRc in 'Constsrc.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
