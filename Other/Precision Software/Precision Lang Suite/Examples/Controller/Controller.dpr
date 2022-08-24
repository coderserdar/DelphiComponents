program Controller;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  Child in 'Child.pas' {frmChild},
  DM in 'DM.pas' {frmDM: TDataModule},
  plsController in '..\..\Source\plsController.pas',
  plsLangMan in '..\..\Source\plsLangMan.pas',
  plsDialogs in '..\..\Source\plsDialogs.pas';

{$R *.res}

begin
  Application.Initialize;
  {$IFDEF UNICODE}
  Application.MainFormOnTaskbar := True;
  {$ENDIF}
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmDM, frmDM);
  Application.Run;
end.
