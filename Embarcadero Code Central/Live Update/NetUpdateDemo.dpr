program NetUpdateDemo;

uses
  Forms,
  MainFRM in 'MainFRM.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
