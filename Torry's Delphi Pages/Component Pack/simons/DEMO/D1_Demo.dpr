program D1_Demo;

uses
  Forms,
  D1_Main in 'D1_MAIN.PAS' {MainForm};

{$R *.RES}

begin
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
