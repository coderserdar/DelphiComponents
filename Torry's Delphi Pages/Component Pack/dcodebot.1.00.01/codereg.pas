
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit CodeReg;

interface

{$I STD.INC}

uses
  AutoComplete, Classes, InspectCtrls, KeybrdCtrls, ScrollCtrls,
  SuplCtrls, BtnEdit, BtnCtrls, ShlCtrls, PopCtrls, FolderCtrls, PaneCtrls,
  MemoCtrls, StretchCtrls, Balloon, PropEditors, StrEdit,
  {$IFDEF D6_UP}DesignIntf, DesignWindows, DesignEditors,
  {$IFDEF D8_UP}DesignMenus,{$ENDIF}
  {$ELSE}DsgnIntf,{$ENDIF} OpenTools, ToolsAPI;

procedure Register;

implementation

{$R CODEREG.DCR}

procedure Register;
begin
  RegisterComponents('Codebot',  [TAutoCompletion, TBackground, TBalloonHint,
    TBrushButton, TButtonEdit, TCaptionBox, TCheckListEdit, TColorGridButton,
    TDatePopupEdit, TDBDrawList, TDBReportView, TDrawList, TExpandableBox,
    TFolderView, TImageButton, TImageListEdit, TInspector, TIntegerEdit,
    TLargeShellImages, TListEdit, TMoneyEdit, TPaneControl, TPenButton,
    TShellBubbles, TShellEdit, TShellTree, TShellView, TSlideBar,
    TSmallShellImages, TThemeGlyphButton]);
  RegisterNoIcon([TPaneSheet]);
  RegisterPropertyEditor(TStrings.ClassInfo, nil, '', TDefaultStringsProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomImageButton, 'ImageIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomImageButton, 'ImageDisabledIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TFolderBars.ClassInfo, TFolderView, 'Folders', TFolderBarsProperty);
  RegisterPropertyEditor(TInspectorEditors.ClassInfo, TInspector, 'Editors',
    TInspectorEditorsProperty);
  RegisterComponentEditor(TBalloonHint, TBalloonHintEditor);
  RegisterComponentEditor(TLargeShellImages, TReadOnlyImageListEditor);
  RegisterComponentEditor(TSmallShellImages, TReadOnlyImageListEditor);
  RegisterComponentEditor(TFolderView, TFolderBarsEditor);
  RegisterComponentEditor(TInspector, TInspectorEditorsEditor);
  RegisterComponentEditor(TPaneControl, TPaneSheetEditor);
  RegisterComponentEditor(TPaneSheet, TPaneSheetEditor);
  { RegisterPackageWizard(TBalloonHintWizard.Create); }
end;

end.
