program vgLabel;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  vgCtrls in '..\..\vgCtrls.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
