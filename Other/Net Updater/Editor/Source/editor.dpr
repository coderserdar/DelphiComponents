program editor;

uses
  Forms,
  main in 'main.pas' {fmSFiles};

{$R *.res}

begin
  Application.Initialize;
  Application.HintPause := 500;
  Application.HintHidePause := -1;
  Application.CreateForm(TfmSFiles, fmSFiles);
  Application.Run;
end.
