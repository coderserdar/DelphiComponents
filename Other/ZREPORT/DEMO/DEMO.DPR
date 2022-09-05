program DEMO;

uses
  Forms,
  MAIN in 'MAIN.PAS' {fmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
