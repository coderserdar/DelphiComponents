program DBTree;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  ADm in 'ADm.pas' {dm: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(Tdm, dm);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
