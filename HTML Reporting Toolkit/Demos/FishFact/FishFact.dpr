program FishFact;

uses
  Forms,
  main in 'main.pas' {FactFrm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFactFrm, FactFrm);
  Application.Run;
end.
