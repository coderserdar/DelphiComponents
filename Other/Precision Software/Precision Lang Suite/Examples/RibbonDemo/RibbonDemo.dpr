program RibbonDemo;
{ RibbonDemo Copyright (c) 2008 Embarcadero Technologies Inc. }
uses
  Forms,
  RibbonDemoMainForm in 'RibbonDemoMainForm.pas' {frmRibbonDemo},
  plsLangMan in '..\..\Source\plsLangMan.pas',
  plsController in '..\..\Source\plsController.pas',
  Themes;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmRibbonDemo, frmRibbonDemo);
  Application.Run;
end.
