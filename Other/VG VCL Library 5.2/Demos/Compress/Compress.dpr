program Compress;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  vgZLib in '..\..\vgZLib.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
