unit Simonreg;

interface

{$I SRDefine.inc}

procedure Register;

implementation

uses Classes, EnhEdit, FileBtn, IniList, OvalBtn, RackCtls, SRCal,
     SRChkBox, SRColBtn, SRGrad, SRLabel, SRValEdt, SRWave, SRFntCtl
     {$IFDEF SR_Delphi2_Up}, SRClock{$ENDIF}
     {$IFDEF SR_Delphi3_Up}, SRDlgs{$ENDIF};

procedure Register;
begin
  RegisterComponents('Simon', [TButtonPanel, TScrewPanel,
                               TLEDButton, TOvalButton, TSRColorButton, TFileButton,
                               TSRCheckBox, TEnhancedCheckbox,
                               TEnhancedEdit, TLEDDisplay, TSRLabel,
                               TNumericEdit, TSliderEdit, TSRValueEdit,
                               TLEDMeter, TSRCalendar, TSRGradient,
                               TSRFontComboBox, TSRFontListBox, TSRWavePlayer]);
  {$IFDEF SR_Delphi2_Up}
  RegisterComponents('Simon', [TSRClock]);
  {$ENDIF}
  {$IFDEF SR_Delphi3_Up}
  RegisterComponents('Simon', [TCSVOpenDialog, TCSVSaveDialog,
                               TExtOpenDialog, TExtSaveDialog,
                               TSliderOpenDialog, TSliderSaveDialog]);
  {$ENDIF}
end;

end.
