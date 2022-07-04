program adoxdemo;

uses
  Forms,
  main in 'main.pas' {MainForm},
  dm in 'dm.pas' {DataModule1: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
end.
