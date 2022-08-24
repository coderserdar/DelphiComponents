program Controller;

uses
  FMX.Forms,
  Main in 'Main.pas' {frmMain},
  DM in 'DM.pas' {frmDM: TDataModule},
  Child in 'Child.pas' {frmChild},
  FMX.plsController in '..\..\Source\FMX.plsController.pas',
  FMX.plsLangMan in '..\..\Source\FMX.plsLangMan.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmDM, frmDM);
  Application.Run;
end.
