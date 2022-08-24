program PerformanceDemo;

uses
  Forms, LResources, Interfaces,
  Main in 'Main.pas' {TfrmPerf};

{$IFDEF WINDOWS}{$R PerformanceDemo.rc}{$ENDIF}

begin
  {$I PerformanceDemo.lrs}
  Application.Initialize;
  Application.Title := 'Precision Language Suite Performance Demo';
  Application.CreateForm(TTfrmPerf, TfrmPerf);
  Application.Run;
end.
