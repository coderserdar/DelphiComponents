program ObjectDatasetDemo;

uses
  ExceptionLog,
  Forms,
  ObjectDatasetClass in 'ObjectDatasetClass.pas',
  uObjectDatasetDemo in 'uObjectDatasetDemo.pas' {FrmObjectDatasetDemo},
  SnapBaseDataset in '..\..\SnapBaseDataset.pas',
  SnapObjectDataset in '..\..\SnapObjectDataset.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFrmObjectDatasetDemo, FrmObjectDatasetDemo);
  Application.Run;
end.
