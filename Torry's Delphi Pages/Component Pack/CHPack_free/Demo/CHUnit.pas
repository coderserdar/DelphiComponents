unit CHUnit;


{
  +++ IMPORTANT!! +++
  To compile this Demo itself, you must have installed the
  "CH-Component Pack" (www.Blue-Xplosion.de)
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, ComCtrls, Menus, StdCtrls,

  CHButton;

type
  TfrmDemo = class(TForm)
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    MainMenu1: TMainMenu;
    mmClose: TMenuItem;
    mmInfo: TMenuItem;
    lblCompo: TLabel;
    btnCHLabel: TCHButton;
    btnCHButton: TCHButton;
    btnCHEdit: TCHButton;
    btnCHCheckbox: TCHButton;
    btnCHRadiobutton: TCHButton;
    btnCHMultigradient: TCHButton;
    btnCHPanel: TCHButton;
    mmLanguage: TMenuItem;
    mmGerman: TMenuItem;
    mmEnglish: TMenuItem;
    btnCHImage: TCHButton;
    btnCHRichedit: TCHButton;
    btnCHThreadTimer: TCHButton;
    btnCHApplication: TCHButton;
    btnCHMouse: TCHButton;
    btnCHColorpicker: TCHButton;
    btnCHForm: TCHButton;
    btnCHCrypt: TCHButton;
    btnCHMoveContainer: TCHButton;
    btnCHLed: TCHButton;
    btnCHHighResTimer: TCHButton;
    btnCHTrayicon: TCHButton;
    btnCHAdvancedTimer: TCHButton;
    btnCHPerformance: TCHButton;
    memInfo: TMemo;
    btnCHLabelAdvanced: TCHButton;
    btnCHShape: TCHButton;
    btnCHLogfile: TCHButton;

    procedure mmCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCHLabelMouseEnter(Sender: TObject);
    procedure btnCHLabelMouseLeave(Sender: TObject);
    procedure btnCHLabelClick(Sender: TObject);
    procedure btnCHButtonClick(Sender: TObject);
    procedure btnCHMultigradientClick(Sender: TObject);
    procedure btnCHMultigradientMouseEnter(Sender: TObject);
    procedure btnCHMultigradientMouseLeave(Sender: TObject);
    procedure btnCHButtonMouseEnter(Sender: TObject);
    procedure btnCHButtonMouseLeave(Sender: TObject);
    procedure btnCHEditClick(Sender: TObject);
    procedure btnCHEditMouseEnter(Sender: TObject);
    procedure btnCHEditMouseLeave(Sender: TObject);
    procedure btnCHCheckboxClick(Sender: TObject);
    procedure btnCHCheckboxMouseEnter(Sender: TObject);
    procedure btnCHCheckboxMouseLeave(Sender: TObject);
    procedure btnCHRadiobuttonClick(Sender: TObject);
    procedure btnCHRadiobuttonMouseEnter(Sender: TObject);
    procedure btnCHRadiobuttonMouseLeave(Sender: TObject);
    procedure mmGermanClick(Sender: TObject);
    procedure mmEnglishClick(Sender: TObject);
    procedure mmInfoClick(Sender: TObject);
    procedure btnCHPanelMouseEnter(Sender: TObject);
    procedure btnCHPanelMouseLeave(Sender: TObject);
    procedure btnCHImageMouseEnter(Sender: TObject);
    procedure btnCHImageMouseLeave(Sender: TObject);
    procedure btnCHRicheditMouseEnter(Sender: TObject);
    procedure btnCHRicheditMouseLeave(Sender: TObject);
    procedure btnCHPanelClick(Sender: TObject);
    procedure btnCHImageClick(Sender: TObject);
    procedure btnCHRicheditClick(Sender: TObject);
    procedure btnCHThreadTimerMouseEnter(Sender: TObject);
    procedure btnCHThreadTimerMouseLeave(Sender: TObject);
    procedure btnCHThreadTimerClick(Sender: TObject);
    procedure btnCHColorpickerClick(Sender: TObject);
    procedure btnCHApplicationMouseEnter(Sender: TObject);
    procedure btnCHApplicationMouseLeave(Sender: TObject);
    procedure btnCHApplicationClick(Sender: TObject);
    procedure btnCHMouseClick(Sender: TObject);
    procedure btnCHMouseMouseLeave(Sender: TObject);
    procedure btnCHMouseMouseEnter(Sender: TObject);
    procedure btnCHColorpickerMouseEnter(Sender: TObject);
    procedure btnCHFormMouseEnter(Sender: TObject);
    procedure btnCHFormMouseLeave(Sender: TObject);
    procedure btnCHFormClick(Sender: TObject);
    procedure btnCHColorpickerMouseLeave(Sender: TObject);
    procedure btnCHCryptMouseEnter(Sender: TObject);
    procedure btnCHCryptClick(Sender: TObject);
    procedure btnCHCryptMouseLeave(Sender: TObject);
    procedure memInfoChange(Sender: TObject);
    procedure memInfoClick(Sender: TObject);
    procedure memInfoEnter(Sender: TObject);
    procedure btnCHMoveContainerClick(Sender: TObject);
    procedure btnCHLabelAdvancedClick(Sender: TObject);
    procedure btnCHLabelAdvancedMouseEnter(Sender: TObject);
    procedure btnCHLedClick(Sender: TObject);
    procedure btnCHHighResTimerClick(Sender: TObject);
    procedure btnCHAdvancedTimerClick(Sender: TObject);
    procedure btnCHTrayiconClick(Sender: TObject);
    procedure btnCHPerformanceClick(Sender: TObject);
    procedure btnCHLabelAdvancedMouseLeave(Sender: TObject);
    procedure btnCHMoveContainerMouseLeave(Sender: TObject);
    procedure btnCHLedMouseLeave(Sender: TObject);
    procedure btnCHHighResTimerMouseLeave(Sender: TObject);
    procedure btnCHTrayiconMouseLeave(Sender: TObject);
    procedure btnCHAdvancedTimerMouseLeave(Sender: TObject);
    procedure btnCHPerformanceMouseLeave(Sender: TObject);
    procedure btnCHMoveContainerMouseEnter(Sender: TObject);
    procedure btnCHLedMouseEnter(Sender: TObject);
    procedure btnCHHighResTimerMouseEnter(Sender: TObject);
    procedure btnCHTrayiconMouseEnter(Sender: TObject);
    procedure btnCHAdvancedTimerMouseEnter(Sender: TObject);
    procedure btnCHPerformanceMouseEnter(Sender: TObject);
    procedure btnCHLogfileMouseEnter(Sender: TObject);
    procedure btnCHLogfileMouseLeave(Sender: TObject);
    procedure btnCHShapeMouseEnter(Sender: TObject);
    procedure btnCHShapeMouseLeave(Sender: TObject);
    procedure btnCHLogfileClick(Sender: TObject);
    procedure btnCHShapeClick(Sender: TObject);

  private

  public
    bGerman : Boolean;
    sPath, sInfoDir : String;
    procedure ClearInfo;
  end;

var
  frmDemo: TfrmDemo;

implementation

uses unit_Label, unit_Button, unit_Multigradient, unit_CheckBox, unit_Edit,
  unit_RadioButton, unit_About, unit_Image, unit_Richedit,
  unit_Mouse, unit_Form, unit_Panel, _CHTypes, unit_Crypt,
  unit_Movecontainer, unit_CHLed, unit_AdvancedLabel, unit_CHPerformance,
  unit_Timer, unit_Trayicon, unit_shape;

{$R *.dfm}

procedure TfrmDemo.FormCreate(Sender: TObject);
begin
  bGerman := True;
  sPath := ExtractFilePath(Application.ExeName);
  sInfoDir := sPath + 'Description\';

  btnCHLabel.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHLABEL');
  btnCHLabel.Glyph.Alignment := gaCenter;

  btnCHLabelAdvanced.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHLABELADV');
  btnCHLabelAdvanced.Glyph.Alignment := gaCenter;

  btnCHButton.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHBUTTON');
  btnCHButton.Glyph.Alignment := gaCenter;

  btnCHEdit.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHEDIT');
  btnCHEdit.Glyph.Alignment := gaCenter;

  btnCHCheckbox.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHCHECKBOX');
  btnCHCheckbox.Glyph.Alignment := gaCenter;

  btnCHRadiobutton.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHRADIOBUTTON');
  btnCHRadiobutton.Glyph.Alignment := gaCenter;

  btnCHMultigradient.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHMULTIGRADIENT');
  btnCHMultigradient.Glyph.Alignment := gaCenter;

  btnCHPanel.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHPANEL');
  btnCHPanel.Glyph.Alignment := gaCenter;

  btnCHImage.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHIMAGE');
  btnCHImage.Glyph.Alignment := gaCenter;

  btnCHRichedit.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHRICHEDIT');
  btnCHRichedit.Glyph.Alignment := gaCenter;

  btnCHThreadTimer.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHTHREADTIMER');
  btnCHThreadTimer.Glyph.Alignment := gaCenter;

  btnCHApplication.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHAPPLICATION');
  btnCHApplication.Glyph.Alignment := gaCenter;

  btnCHForm.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHFORM');
  btnCHForm.Glyph.Alignment := gaCenter;

  btnCHMouse.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHMOUSE');
  btnCHMouse.Glyph.Alignment := gaCenter;

  btnCHColorpicker.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHCOLORPICKER');
  btnCHColorpicker.Glyph.Alignment := gaCenter;

  btnCHCrypt.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHCRYPT');
  btnCHCrypt.Glyph.Alignment := gaCenter;

  btnCHMoveContainer.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHMOVECONTAINER');
  btnCHMoveContainer.Glyph.Alignment := gaCenter;

  btnCHLed.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHLED');
  btnCHLed.Glyph.Alignment := gaCenter;

  btnCHHighResTimer.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHHIGHRESTIMER');
  btnCHHighResTimer.Glyph.Alignment := gaCenter;

  btnCHTrayicon.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHTRAYICON');
  btnCHTrayicon.Glyph.Alignment := gaCenter;

  btnCHAdvancedTimer.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHADVANCEDTIMER');
  btnCHAdvancedTimer.Glyph.Alignment := gaCenter;

  btnCHPerformance.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHPERFORMANCE');
  btnCHPerformance.Glyph.Alignment := gaCenter;

  btnCHShape.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHSHAPE');
  btnCHShape.Glyph.Alignment := gaCenter;

  btnCHLogfile.Glyph.Glyph.LoadFromResourceName(hInstance, 'CHLOGFILE');
  btnCHLogfile.Glyph.Alignment := gaCenter;
end;

procedure TfrmDemo.mmCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmDemo.mmGermanClick(Sender: TObject);
begin
  mmGerman.Checked := True;
  mmEnglish.Checked := False;
  bGerman := True;
end;

procedure TfrmDemo.mmEnglishClick(Sender: TObject);
begin
  mmGerman.Checked := False;
  mmEnglish.Checked := True;
  bGerman := False;
end;

procedure TfrmDemo.memInfoChange(Sender: TObject);
begin
  if memInfo.Focused then
    btnCHLabel.SetFocus;
end;

procedure TfrmDemo.memInfoClick(Sender: TObject);
begin
  if memInfo.Focused then
    btnCHLabel.SetFocus;
end;

procedure TfrmDemo.memInfoEnter(Sender: TObject);
begin
  btnCHLabel.SetFocus;
end;

procedure TfrmDemo.ClearInfo;
begin
  lblCompo.Caption := '';
  memInfo.Clear;
end;

procedure TfrmDemo.mmInfoClick(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;

procedure TfrmDemo.btnCHLabelClick(Sender: TObject);
begin
  frmCHLabel := TfrmCHLabel.Create(Self);
  frmCHLabel.ShowModal;
end;

procedure TfrmDemo.btnCHLabelMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHLabel';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'label_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'label_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!'
  end;
end;

procedure TfrmDemo.btnCHLabelMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHButtonClick(Sender: TObject);
begin
  frmCHButton := TfrmCHButton.Create(Self);
  frmCHButton.ShowModal;
end;

procedure TfrmDemo.btnCHButtonMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHButton';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'button_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'button_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!'
  end;
end;

procedure TfrmDemo.btnCHButtonMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHMultigradientClick(Sender: TObject);
begin
  frmCHMultigradient := TfrmCHMultigradient.Create(Self);
  frmCHMultigradient.ShowModal;
end;

procedure TfrmDemo.btnCHMultigradientMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHMultigradient';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'multigradient_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'multigradient_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!'
  end;
end;

procedure TfrmDemo.btnCHMultigradientMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHEditClick(Sender: TObject);
begin
  frmCHEdit := TfrmCHEdit.Create(Self);
  frmCHEdit.ShowModal;
end;

procedure TfrmDemo.btnCHEditMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHEdit';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'edit_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'edit_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!'
  end;
end;

procedure TfrmDemo.btnCHEditMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHCheckboxClick(Sender: TObject);
begin
  frmCHCheckBox := TfrmCHCheckBox.Create(Self);
  frmCHCheckBox.ShowModal;
end;

procedure TfrmDemo.btnCHCheckboxMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHCheckBox';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'checkbox_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'checkbox_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!'
  end;
end;

procedure TfrmDemo.btnCHCheckboxMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHRadiobuttonClick(Sender: TObject);
begin
  frmCHRadioButton := TfrmCHRadioButton.Create(self);
  frmCHRadioButton.ShowModal;
end;

procedure TfrmDemo.btnCHRadiobuttonMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHRadioButton';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'radiobutton_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'radiobutton_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!'
  end;
end;

procedure TfrmDemo.btnCHRadiobuttonMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHPanelClick(Sender: TObject);
begin
  frmCHPanel := TfrmCHPanel.Create(self);
  frmCHPanel.ShowModal;
end;

procedure TfrmDemo.btnCHPanelMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHPanel';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'panel_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'panel_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHPanelMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHImageClick(Sender: TObject);
begin
  frmCHImage := TfrmCHImage.Create(self);
  frmCHImage.ShowModal;
end;

procedure TfrmDemo.btnCHImageMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHImage';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'image_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'image_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHImageMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHRicheditClick(Sender: TObject);
begin
  frmCHRichedit := TfrmCHRichedit.Create(self);
  frmCHRichedit.ShowModal;
end;

procedure TfrmDemo.btnCHRicheditMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHRichedit';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'richedit_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'richedit_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHRicheditMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHThreadTimerClick(Sender: TObject);
begin
  frmCHTimer := TfrmCHTimer.Create(Self);
  frmCHTimer.ShowModal;
end;

procedure TfrmDemo.btnCHThreadTimerMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHThreadTimer';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'threadtimer_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'threadtimer_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHThreadTimerMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHApplicationClick(Sender: TObject);
begin
  ShowMessage('No Demo for CHApplication-Component');
end;

procedure TfrmDemo.btnCHApplicationMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHApplication';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'application_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'application_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHApplicationMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHMouseClick(Sender: TObject);
begin
  frmCHMouse := TfrmCHMouse.Create(self);
  frmCHMouse.ShowModal;
end;

procedure TfrmDemo.btnCHMouseMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHMouseMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHMouse';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'mouse_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'mouse_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHColorpickerClick(Sender: TObject);
begin
  ShowMessage('No Demo for CHColorPicker-Component');
end;

procedure TfrmDemo.btnCHColorpickerMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHColorpicker';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'colorpicker_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'colorpicker_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHFormMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHForm';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'form_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'form_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHFormMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHFormClick(Sender: TObject);
begin
  frmCHForm := TfrmCHForm.Create(Self);
  frmCHForm.Show;
end;

procedure TfrmDemo.btnCHColorpickerMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHCryptMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHCrypt';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'crypt_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'crypt_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHCryptClick(Sender: TObject);
begin
  frmCHCrypt := TfrmCHCrypt.Create(self);
  frmCHCrypt.Show;
end;

procedure TfrmDemo.btnCHCryptMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;


procedure TfrmDemo.btnCHMoveContainerClick(Sender: TObject);
begin
  frmCHMoveContainer := TfrmCHMoveContainer.Create(Self);
  frmCHMoveContainer.ShowModal;
end;

procedure TfrmDemo.btnCHLabelAdvancedClick(Sender: TObject);
begin
  frmCHAdvancedLabel := TfrmCHAdvancedLabel.Create(Self);
  frmCHAdvancedLabel.ShowModal;
end;

procedure TfrmDemo.btnCHLabelAdvancedMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHAdvancedLabel';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'advancedlabel_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'advancedlabel_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHLedClick(Sender: TObject);
begin
  frmCHLed := TfrmCHLed.Create(self);
  frmCHLed.Show;
end;

procedure TfrmDemo.btnCHHighResTimerClick(Sender: TObject);
begin
  frmCHTimer := TfrmCHTimer.Create(Self);
  frmCHTimer.ShowModal;
end;

procedure TfrmDemo.btnCHAdvancedTimerClick(Sender: TObject);
begin
  frmCHTimer := TfrmCHTimer.Create(Self);
  frmCHTimer.ShowModal;
end;

procedure TfrmDemo.btnCHTrayiconClick(Sender: TObject);
begin
  frmCHTrayIcon := TfrmCHTrayIcon.Create(self);
  frmCHTrayIcon.Show;
end;

procedure TfrmDemo.btnCHPerformanceClick(Sender: TObject);
begin
  frmCHPerformance := TfrmCHPerformance.Create(self);
  frmCHPerformance.ShowModal;
end;

procedure TfrmDemo.btnCHLabelAdvancedMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHMoveContainerMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHLedMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHHighResTimerMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHTrayiconMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHAdvancedTimerMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHPerformanceMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHMoveContainerMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHMoveContainer';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'movecontainer_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'movecontainer_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHLedMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHLed';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'led_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'led_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHHighResTimerMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHHighResTimer';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'highrestimer_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'highrestimer_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHTrayiconMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHTrayicon';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'trayicon_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'trayicon_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHAdvancedTimerMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHAdvancedTimer';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'advancedtimer_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'advancedtimer_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHPerformanceMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHPerformance';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'performance_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'performance_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHLogfileMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHLogfile';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'logfile_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'logfile_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHLogfileMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHShapeMouseEnter(Sender: TObject);
begin
  lblCompo.Caption := 'CHShape';
  try
    if bGerman then
      memInfo.Lines.LoadFromFile(sInfoDir + 'shape_deu.txt')
    else
      memInfo.Lines.LoadFromFile(sInfoDir + 'shape_eng.txt');
  except
    memInfo.Lines[0] := 'No description found!';
  end;
end;

procedure TfrmDemo.btnCHShapeMouseLeave(Sender: TObject);
begin
  ClearInfo;
end;

procedure TfrmDemo.btnCHLogfileClick(Sender: TObject);
begin
  ShowMessage('No Demo for CHLogfile-Component');
end;

procedure TfrmDemo.btnCHShapeClick(Sender: TObject);
begin
  frmCHShape := TfrmCHShape.Create(Self);
  frmCHShape.ShowModal;
end;

end.
