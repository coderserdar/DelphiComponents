{================================================================================
Copyright (C) 1997-2001 Mills Enterprise

Unit     : rmRegister
Purpose  : This is the registration unit for all of the "rm" Controls.
Date     : 06-02-1999
Author   : Ryan J. Mills
Version  : 1.80
================================================================================}

unit rmRegister;

interface

{$I CompilerDefines.INC}

uses rmBaseEdit, rmSpeedBtns, rmScrnCtrls, rmBtnEdit, rmColorComboBox,
     rmComboBox, rmPanel, rmGauge, rmLabel, rmSplit, rmTaskBar, rmTrackBar, rmTabs3x,
     rmCalendar, rmSpin, rmFileDrop, rmColumns, rmAppEvents, rmTVComboBox, rmPathTreeView,
     rmDataStoragePropEdit, rmDataStorage, rmMDIBackground, rmCCTabs, rmCCTabsReg,
     rmCollectionListBox, rmTreeNonView, rmTreeNonViewEdit,
     rmSpinCombo, rmBtnCombo, rmCaptionButtons, rmCornerGrip, rmKeyBindings, rmKeyBindingPropEdit,
     rmDiff, rmBrowseFor, rmTrayIcon, rmImageListGraphic, rmEditGrid, rmListControl,
     rmHint, rmInspector, rmInspectorItems, rmInspectorEdit, rmOutlook, rmOutlookReg,
     rmOutlookExtras, rmDGT, rmMemoryDataSet, rmToolWin, rmToolWinFormExpt, rmCheckBox;

procedure Register;

implementation

{$ifdef BD6}
uses
  classes, DesignIntf, DesignEditors, ExptIntf;
{$else}
uses
  classes, DsgnIntf, ExptIntf;
{$endif}

const
     PalettePage = 'rmControls';

procedure Register;
begin
     RegisterCustomModule(TrmToolWinForm, TCustomModule);
     RegisterLibraryExpert(TrmToolWinFormExpert.create);

     RegisterClasses([TrmCustomEdit, TrmCustomSpeedButton, TrmTimerSpeedButton]);
     RegisterClasses([TrmCustomBtnEdit, TrmCustomCalendar, TrmCustomComboCalendar]);
     Registerclasses([TrmCustomSpinEdit, TrmListColumns, TrmListColumn]);
     RegisterClasses([TrmCustomApplicationEvents, TrmCustomLabel]);
     RegisterClasses([TrmCustomScreenTreeView, TrmCustomComboTreeView, TrmCustomScreenPathTreeView, TrmCustomComboPathTreeView]);
     RegisterClasses([TrmCustomDataStorage]);
     RegisterClasses([TrmCustomCCTabControl, TrmCCTabSheet]);
     RegisterClasses([TrmCustomScreenListBox, TrmCustomSpinCombo, TrmCustomBtnCombo]);
     RegisterClasses([TrmCustomComboBox, TrmHintWindow]);
     RegisterClasses([TrmCaptionButtonItem, TrmCaptionButtonsCollection]);
     RegisterClasses([TrmKeyBindingItem, TrmKeyBindingCollection]);
     RegisterClasses([TrmCustomDiffEngine, TrmCustomDiffViewer, TrmCustomImageListGraphic]);
     RegisterClasses([TrmOutLookPage, TrmCustomCheckBox]);

     RegisterComponents(PalettePage,[TrmSpeedButton, TrmSpinButton, TrmBtnEdit, TrmPanel]);
     RegisterComponents(PalettePage,[TrmColorComboBox, TrmComboBox, TrmNewComboBox, TrmComboTreeView, TrmComboPathTreeView]);
     RegisterComponents(PalettePage,[TrmGauge, TrmLabel, TrmSplitter, TrmTaskBar]);
     RegisterComponents(PalettePage,[TrmTrackBar, TrmTabSet, TrmCalendar, TrmComboCalendar]);
     RegisterComponents(PalettePage,[TrmSpinEdit, TrmFloatSpinEdit, TrmTimeSpinEdit, TrmSpinCombo, TrmBtnCombo]);
     RegisterComponents(PalettePage,[TrmFileDrop, TrmColumns, TrmApplicationEvents]);
     RegisterComponents(PalettePage,[TrmPathTreeView, TrmTreeNonView, TrmTextDataStorage, TrmBinaryDataStorage]);
     RegisterComponents(PalettePage,[TrmCCTabControl, TrmCCPageControl]);
     RegisterComponents(PalettePage,[TrmMDIBackground, TrmCollectionListBox]);
     RegisterComponents(PalettePage,[TrmCaptionButtons, TrmCornerGrip, TrmKeyBindings]);
     RegisterComponents(PalettePage,[TrmEditGrid, TrmEditDrawGrid, TrmBrowseForFolder, TrmTrayIcon]);
     RegisterComponents(PalettePage,[TrmImageListGraphic, TrmImageListGlyph]);
     RegisterComponents(PalettePage,[TrmListControl, TrmDiffEngine, TrmDiffViewer, TrmDiffMergeViewer, TrmDiffMap]);
     RegisterComponents(PalettePage,[TrmInspector, TrmOutLookControl, TrmOutlookButtonList, TrmOutlookActionLink]);
     RegisterComponents(PalettePage,[TrmDGTree, TrmMemoryDataSet, TrmCheckBox]);

     RegisterPropertyEditor(TypeInfo(TrmTreeNonViewNodes), TrmTreeNonView, 'Items', TrmTreeNonViewItemsProperty);
     RegisterPropertyEditor(TypeInfo(TrmDataStorageLongint), TrmCustomDataStorage, 'DataSize', TrmDataLongintProperty);
     RegisterPropertyEditor(TypeInfo(TrmTreeNonViewNodes), TrmInspector, 'Items', TrmInspectorItemsProperty);
     RegisterPropertyEditor(TypeInfo(TrmOutLookPage), TrmOutLookControl, 'ActivePage', TrmOutlookActivePageProperty);

     RegisterComponentEditor(TrmCustomDataStorage, TrmDataStorageEditor);
     RegisterComponentEditor(TrmCCPageControl, TrmCCPageControlEditor);
     RegisterComponentEditor(TrmCCTabSheet, TrmCCPageControlEditor);
     RegisterComponentEditor(TrmKeyBindings, TrmKeyBindingEditor);
     RegisterComponentEditor(TrmInspector, TrmInspectorEditor);
     RegisterComponentEditor(TrmOutLookControl, TrmOutlookControlEditor);
     RegisterComponentEditor(TrmOutLookPage, TrmOutlookControlEditor);
end;

end.
