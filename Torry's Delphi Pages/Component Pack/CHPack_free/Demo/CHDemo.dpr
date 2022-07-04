program CHDemo;

uses
  Forms,
  CHUnit in 'CHUnit.pas' {frmDemo},
  unit_Label in 'unit_Label.pas' {frmCHLabel},
  unit_Button in 'unit_Button.pas' {frmCHButton},
  unit_Multigradient in 'unit_Multigradient.pas' {frmCHMultigradient},
  unit_Edit in 'unit_Edit.pas' {frmCHEdit},
  unit_CheckBox in 'unit_CheckBox.pas' {frmCHCheckBox},
  unit_Panel in 'unit_Panel.pas' {frmCHPanel},
  unit_About in 'unit_About.pas' {frmAbout},
  unit_Image in 'unit_Image.pas' {frmCHImage},
  unit_Richedit in 'unit_Richedit.pas' {frmCHRichedit},
  unit_Mouse in 'unit_Mouse.pas' {frmCHMouse},
  unit_Form in 'unit_Form.pas' {frmCHForm},
  unit_RadioButton in 'unit_RadioButton.pas' {frmCHRadioButton},
  unit_Crypt in 'unit_Crypt.pas' {frmCHCrypt},
  unit_Movecontainer in 'unit_Movecontainer.pas' {frmCHMoveContainer},
  unit_AdvancedLabel in 'unit_AdvancedLabel.pas' {frmCHAdvancedLabel},
  unit_Form2 in 'unit_Form2.pas' {frmCHForm2},
  unit_Form3 in 'unit_Form3.pas' {frmCHForm3},
  unit_Form4 in 'unit_Form4.pas' {frmCHForm4},
  unit_Form5 in 'unit_Form5.pas' {frmCHForm5},
  unit_CHLed in 'unit_CHLed.pas' {frmCHLed},
  unit_Timer in 'unit_Timer.pas' {frmCHTimer},
  unit_CHPerformance in 'unit_CHPerformance.pas' {frmCHPerformance},
  unit_Trayicon in 'unit_Trayicon.pas' {frmCHTrayIcon},
  unit_shape in 'unit_shape.pas' {frmCHShape};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDemo, frmDemo);
  Application.Run;
end.
