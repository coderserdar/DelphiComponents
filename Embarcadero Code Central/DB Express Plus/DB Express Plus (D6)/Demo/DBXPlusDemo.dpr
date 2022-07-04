program DBXPlusDemo;

uses
  Forms,
  dbExpressPlus in 'dbExpressPlus.pas' {F_DbExpPlus};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TF_DbExpPlus, F_DbExpPlus);
  Application.Run;
end.
