unit SettingCtrlsReg;

interface
uses
   Classes
  ,SettingCtrls

  ;


procedure Register;

implementation

uses
  SysUtils
  ;

procedure Register;
begin
  RegisterComponents('GT SettingControls',[TgtSettingsManager
                                       ,TgtCSCEdit
                                       ,TgtCSCCheckBox
                                       ,TgtCSCComboBox
                                       ,TgtCSCListBox
                                       ,TgtCSCMemo
                                       ,TgtCSCRadioButton
                                       ,TgtCSCLabeledEdit
                                       ,TgtCSCDateTimePicker
                                       ,TgtCSCTrackBar
                                       ,TgtCSCRadioGroup
                                       ,TgtCSCColorBox]);


end;


end.
