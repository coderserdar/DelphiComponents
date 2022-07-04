program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {frmMain},
  Unit2 in 'Unit2.pas' {frmInspector},
  Unit3 in 'Unit3.pas' {frmDesign};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmInspector, frmInspector);
  Application.CreateForm(TfrmDesign, frmDesign);
  Application.Run;
end.
