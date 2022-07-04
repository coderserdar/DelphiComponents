program minicad;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Mini CAD';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
