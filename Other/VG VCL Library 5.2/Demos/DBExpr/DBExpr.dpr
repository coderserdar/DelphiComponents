program DBExpr;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  bDM in 'bDM.pas' {dm: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(Tdm, dm);
  Application.Run;
end.
