program delphvar;

uses
  Forms,
  varsmain in 'varsmain.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
