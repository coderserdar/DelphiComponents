program ExFF2SimplePlanner;

uses
  Forms,
  ExFF2SimplePlannerU1 in 'ExFF2SimplePlannerU1.pas' {FrmSimplePlanner};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFrmSimplePlanner, FrmSimplePlanner);
  Application.Run;
end.
