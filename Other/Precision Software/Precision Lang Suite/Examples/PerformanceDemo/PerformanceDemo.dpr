program PerformanceDemo;

uses
  Forms,
  Main in 'Main.pas' {TfrmPerf};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Precision Language Suite Performance Demo';
  Application.CreateForm(TTfrmPerf, TfrmPerf);
  Application.Run;
end.
