program firemonkey;

uses
  //FMX.Canvas.VPR,
  FMX.Forms,
  main in 'main.pas' {MainForm};

{$R *.res}

begin

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

end.
