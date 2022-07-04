program Project1;

uses
  Forms,
  fmMain in 'fmMain.pas' {Main},
  fmProgress in 'fmProgress.pas' {Progress},
  fmDisplayArchive in 'fmDisplayArchive.pas' {DisplayArchive};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TProgress, Progress);
  Application.CreateForm(TDisplayArchive, DisplayArchive);
  Application.Run;
end.
