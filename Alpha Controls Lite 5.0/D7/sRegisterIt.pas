unit sRegisterIt;
{$I sDefs.inc}

interface

uses
  Classes, sScrollBar, sLabel, sButton, sBitBtn, sSpeedButton, sPanel,
  sAlphaListBox, sGauge,
  {$IFNDEF ALITE}
    sHintManager, sToolBar, sColorSelect, sDialogs,
    sComboBox, sCurrencyEdit, sSpinEdit, sMemo, sRadioButton, sComboEdit,
    sPageControl, sCurrEdit, sToolEdit, sMonthCalendar,
    sBevel, sGroupBox, sStatusBar, sTrackBar, sCalculator,
    sMaskEdit, sComboBoxes, sSplitter, sFileListView,
    sEdit, sSkinManager, sSkinProvider, sTabControl, sDriveComboBox, sFontCtrls,
    sCheckBox, sScrollBox, sImageList, sCheckListBox, sRichEdit, sFileCtrl,
    sTreeView, sListView, sFrameAdapter, sGridAdapter, sUpDown, sFrameBar, sPathComboBox;
  {$ELSE}
    sEdit, sSkinManager, sSkinProvider, sComboBox, sCheckBox;
  {$ENDIF}

procedure Register;

implementation

uses Registry, Windows, sUtils, SysUtils{$IFNDEF ALITE}, sColorDialog{$ENDIF};

procedure Register;
{$IFNDEF ALITE}
var
  r : TRegistry;
{$ENDIF}
begin

{$IFNDEF ALITE}
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_CURRENT_USER;
  {$IFDEF DELPHI7}
    if r.OpenKey('SOFTWARE\Borland\Delphi\7.0\Palette', False) then begin
  {$ELSE}
    {$IFDEF DELPHI6}
      if r.OpenKey('SOFTWARE\Borland\Delphi\6.0\Palette', False) then begin
    {$ELSE}
      if r.OpenKey('SOFTWARE\Borland\Delphi\5.0\Palette', False) then begin
    {$ENDIF}
  {$ENDIF}
      r.DeleteValue('AlphaLite');
{      r.DeleteValue('AlphaStandard');
      r.DeleteValue('AlphaAdditional');
      r.DeleteValue('AlphaTools');}
      r.DeleteValue('sStyle');
      r.DeleteValue('sTools');

      r.DeleteValue('AlphaControls');

    end;
  finally
    FreeAndNil(r);
  end;

  RegisterComponents('AlphaStandard', [
    TsLabel, TsEdit, TsMemo, TsButton, TsCheckBox, TsRadioButton,
    TsListBox, TsComboBox, TsGroupBox, TsRadioGroup, TsPanel, TsBitBtn,
    TsSpeedButton, TsMaskEdit, TsBevel, TsScrollBox, TsCheckListBox, TsSplitter,
    TsTabControl, TsPageControl, TsRichEdit, TsTrackBar, TsUpDown, TsTreeView,
    TsListView, TsStatusBar, TsToolBar, TsGauge, TsSpinEdit]);

  RegisterComponents('AlphaAdditional', [
    TsWebLabel, TsDecimalSpinEdit, TsColorSelect, TsDragBar, TsComboBoxEx, TsColorBox, TsScrollBar,
    TsMonthCalendar, TsComboEdit, TsCurrencyEdit, TsDateEdit, TsCalcEdit, TsDirectoryEdit,
    TsFileNameEdit, TsFileListView, TsDriveComboBox, TsFilterComboBox, TsFontComboBox, TsFontListBox,
    TsFrameBar, TsPathComboBox, TsColorsPanel, TsStickyLabel, TsLabelFX]);

  RegisterComponents('AlphaTools', [
    TsSkinManager, TsSkinProvider, TsFrameAdapter, TsGridAdapter, TsHintManager,
    TsCalculator, TsOpenDialog, TsSaveDialog, TsOpenPictureDialog,
    TsSavePictureDialog{, TsImageList in progress}, TsColordialog]);

  RegisterNoIcon([TsTabSheet]);
  RegisterClasses([TsTabSheet, TsEditLabel, TsStdColorsPanel]);

{$ELSE}

  RegisterComponents('AlphaLite', [
    TsSkinManager, TsSkinProvider, TsEdit, TsCheckBox, TsPanel, TsButton,
    TsScrollBar, TsLabel, TsWebLabel, TsBitBtn, TsComboBox, TsListBox, TsGauge]);

{$ENDIF}

end;

end.
