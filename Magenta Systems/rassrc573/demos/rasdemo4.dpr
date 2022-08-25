program rasdemo4;

{$R 'ras4tmanifest.res' 'ras4tmanifest.rc'}

uses
  Forms,
  demo4 in 'demo4.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
