program ArchBackup;

uses
  Forms,
  fmMain in 'fmMain.pas' {Main},
  fmInspect in 'fmInspect.pas' {Inspect},
  fmOptions in 'fmOptions.pas' {Options};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ArchBackup';
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TInspect, Inspect);
  Application.CreateForm(TOptions, Options);
  Application.Run;
end.
