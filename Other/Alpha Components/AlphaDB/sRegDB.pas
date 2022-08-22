unit sRegDB;
{$I sDefs.inc}

interface

uses
  Classes;

procedure Register;

implementation


uses sDBEdit, sDBMemo, sDBComboBox, sDBLookupComboBox, sDBText, sDBRadioGroup, sDBRichEdit,
  sDBLookupListBox, sDBListBox, sDBCheckBox, sDBNavigator, sDBDateEdit,
  sDBCalcEdit, acDBTextFX, acDBCtrlGrid, acDBGrid, acDBComboEdit,
  acDBDecimalSpinEdit, acDBComboBoxEx, acDBListBoxEx, acDBBtns, acDBSlider;


procedure Register;
begin
  RegisterComponents('AlphaDBControls', [
    TsDBEdit, TsDBMemo, TsDBComboBox, TsDBLookupComboBox, TsDBText, TsDBListBox,
    TsDBLookupListBox, TsDBCheckBox, TsDBNavigator, TsDBDateEdit, TsDBRadioGroup,
    TsDBCalcEdit, TsDBRichEdit, TsDBTextFX, TsDBCtrlGrid, TsDBGrid, TsDBDecimalSpinEdit,
    TsDBComboBoxEx, TsDBListBoxEx, TsDBComboEdit, TsDBButton, TsDBBitBtn, TsDBSpeedButton, TsDBSlider]);
end;

end.
