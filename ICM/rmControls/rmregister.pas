{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmRegister
Purpose  : This is the registration unit for all of the "rm" Controls.
Date     : 06-02-1999
Author   : Ryan J. Mills
Version  : 1.90
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
     rmOutlookExtras, rmDGT, rmMemoryDataSet, rmToolWin, rmToolWinFormExpt, rmCheckBox,
     rmWordTree, rmClock, rmNoteBook2, rmNotebookReg, rmDayView, rmScrollableControl;

procedure Register;

implementation

{$ifdef D6_or_higher}
uses
  classes, DesignIntf, DesignEditors, ExptIntf;
{$else}
uses
  classes, DsgnIntf, ExptIntf;
{$endif}

const
     PalettePages : array[0..5] of string = ('rmStandard', 'rmCombos', 'rmEdits', 'rmEnhanced', 'rmNonVisual', 'rmAdvanced');

procedure Register;
begin
  //Module and Expert Registration
     RegisterCustomModule(TrmToolWinForm, TCustomModule);
     RegisterLibraryExpert(TrmToolWinFormExpert.create);

  //Classes Registration
     RegisterClasses([TrmCustomEdit, TrmCustomSpeedButton, TrmTimerSpeedButton, TrmCustomBtnEdit]);
     RegisterClasses([TrmCustomCalendar, TrmCustomComboCalendar, TrmCustomSpinEdit, TrmListColumns]);
     Registerclasses([TrmListColumn, TrmCustomApplicationEvents, TrmCustomLabel, TrmCustomScreenTreeView]);
     RegisterClasses([TrmCustomComboTreeView, TrmCustomScreenPathTreeView, TrmCustomComboPathTreeView, TrmCustomDataStorage]);
     RegisterClasses([TrmCustomCCTabControl, TrmCCTabSheet, TrmCustomScreenListBox, TrmCustomSpinCombo]);
     RegisterClasses([TrmCustomBtnCombo, TrmCustomComboBox, TrmHintWindow, TrmCaptionButtonItem]);
     RegisterClasses([TrmCaptionButtonsCollection, TrmKeyBindingItem, TrmKeyBindingCollection, TrmBindingStorage]);
     RegisterClasses([TrmCustomDiffEngine, TrmCustomDiffViewer, TrmCustomImageListGraphic, TrmOutLookPage, TrmCustomCheckBox]);
     RegisterClasses([TrmCustomClock, TrmNotebookPage, TrmCustomDayView, TrmCustomDayViewItem, TrmCustomScrollableControl]);

     RegisterNoIconProc([TrmDayViewItem]);

  //Components Registration
     //rmStandard
     RegisterComponents(PalettePages[0], [TrmLabel, TrmGauge, TrmTrackBar, TrmCalendar, TrmCheckBox]);
     RegisterComponents(PalettePages[0], [TrmListControl, TrmCollectionListBox, TrmImageListGraphic, TrmImageListGlyph]);
     RegisterComponents(PalettePages[0], [TrmSpinButton, TrmClock]);

     //rmCombos
     RegisterComponents(PalettePages[1], [TrmColorComboBox, TrmComboBox, TrmNewComboBox, TrmComboTreeView]);
     RegisterComponents(PalettePages[1], [TrmComboPathTreeView, TrmComboCalendar, TrmBtnCombo, TrmSpinCombo]);

     //rmEdits
     RegisterComponents(PalettePages[2],[TrmBtnEdit, TrmSpinEdit, TrmFloatSpinEdit, TrmTimeSpinEdit]);

     //rmEnhanced
     RegisterComponents(PalettePages[3],[TrmPathTreeView, TrmEditGrid, TrmEditDrawGrid, TrmTabSet]);
     RegisterComponents(PalettePages[3],[TrmCCTabControl, TrmCCPageControl, TrmSplitter, TrmSpeedButton]);
     RegisterComponents(PalettePages[3],[TrmPanel]);

     //rmNonVisual
     RegisterComponents(PalettePages[4],[TrmTreeNonView, TrmDGTree, TrmMemoryDataSet, TrmWordTree]);
     RegisterComponents(PalettePages[4],[TrmBrowseForFolder, TrmTrayIcon, TrmApplicationEvents]);
     RegisterComponents(PalettePages[4],[TrmFileDrop, TrmColumns, TrmKeyBindings, TrmCornerGrip]);

     //rmAdvanced
     RegisterComponents(PalettePages[5],[TrmTaskBar, TrmTextDataStorage, TrmBinaryDataStorage, TrmMDIBackground]);
     RegisterComponents(PalettePages[5],[TrmCaptionButtons, TrmDiffEngine, TrmDiffViewer, TrmDiffMergeViewer]);
     RegisterComponents(PalettePages[5],[TrmDiffMap, TrmInspector, TrmOutLookControl, TrmOutlookButtonList]);
     RegisterComponents(PalettePages[5],[TrmOutlookActionLink, TrmNoteBookControl, TrmDayView]);

     //Property and Component Editor Registration
     RegisterPropertyEditor(TypeInfo(TrmTreeNonViewNodes), TrmTreeNonView, 'Items', TrmTreeNonViewItemsProperty);
     RegisterPropertyEditor(TypeInfo(TrmDataStorageLongint), TrmCustomDataStorage, 'DataSize', TrmDataLongintProperty);
     RegisterPropertyEditor(TypeInfo(TrmTreeNonViewNodes), TrmInspector, 'Items', TrmInspectorItemsProperty);
     RegisterPropertyEditor(TypeInfo(TrmOutLookPage), TrmOutLookControl, 'ActivePage', TrmOutlookActivePageProperty);
     RegisterPropertyEditor(TypeInfo(TrmNoteBookPage), TrmNotebookControl, 'ActivePage', TrmNotebookActivePageProperty);

     RegisterComponentEditor(TrmCustomDataStorage, TrmDataStorageEditor);
     RegisterComponentEditor(TrmCCPageControl, TrmCCPageControlEditor);
     RegisterComponentEditor(TrmCCTabSheet, TrmCCPageControlEditor);
     RegisterComponentEditor(TrmKeyBindings, TrmKeyBindingEditor);
     RegisterComponentEditor(TrmInspector, TrmInspectorEditor);
     RegisterComponentEditor(TrmOutLookControl, TrmOutlookControlEditor);
     RegisterComponentEditor(TrmOutLookPage, TrmOutlookControlEditor);
     RegisterComponentEditor(TrmNotebookControl, TrmNotebookControlEditor);
     RegisterComponentEditor(TrmNoteBookPage, TrmNotebookControlEditor);
end;

end.
