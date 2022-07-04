program Demo;

uses
  Forms,
  fDemoMain in 'fDemoMain.pas' {fmDemoMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmDemoMain, fmDemoMain);
  Application.Run;
end.
