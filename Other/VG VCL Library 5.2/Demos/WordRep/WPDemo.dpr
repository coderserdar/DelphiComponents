program WPDemo;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  vgWP in '..\..\vgWP.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
