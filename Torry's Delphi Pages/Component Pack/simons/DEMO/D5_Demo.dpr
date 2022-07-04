program D5_Demo;

uses
  Forms,
  D5_Main in 'D5_Main.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Simons Komponenten - Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
