program rasdemo6;

{$R 'ras6tmanifest.res' 'ras6tmanifest.rc'}

uses
  Forms,
  demo6 in 'demo6.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
