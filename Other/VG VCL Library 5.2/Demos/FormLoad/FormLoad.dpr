program FormLoad;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  fSheet in 'fSheet.pas' {SheetForm},
  vgCtrls in '..\..\vgCtrls.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
