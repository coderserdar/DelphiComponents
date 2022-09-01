program DemoVDS;

uses
  ExceptionLog,
  Forms,
  UnitMain in 'UnitMain.pas' {Form1},
  SnapVirtualDataset in '..\..\SnapVirtualDataset.pas',
  SnapBaseDataset in '..\..\SnapBaseDataset.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Demo Virtual Dataset';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
