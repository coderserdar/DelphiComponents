program Containers;

uses
  Forms,
  UnitContainer in 'UnitContainer.pas' {FormContainers},
  CnContainers in '..\..\Source\Common\CnContainers.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormContainers, FormContainers);
  Application.Run;
end.
