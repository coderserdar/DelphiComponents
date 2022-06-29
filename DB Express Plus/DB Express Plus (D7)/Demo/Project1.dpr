program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {F_DbExpPlus};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TF_DbExpPlus, F_DbExpPlus);
  Application.Run;
end.
