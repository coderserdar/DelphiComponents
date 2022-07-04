program PropStore;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  dData in 'dData.pas' {dm: TDataModule},
  fFrame in 'fFrame.pas' {StoredFrame: TFrame};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(Tdm, dm);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
