program DBErrors;

uses
  Forms,
  Main in 'Main.pas' {FmMain},
  DM1 in 'Dm1.pas' {DM};

{$R *.RES}

begin
  Application.CreateForm(TFmMain, FmMain);
  Application.CreateForm(TDM, DM);
  Application.Run;
end.
