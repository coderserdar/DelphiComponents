
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit ModReg;

interface

{$I STD.INC}

uses
  AutoComplete, Classes, InspectCtrls, KeybrdCtrls, ScrollCtrls,
  SuplCtrls, BtnEdit, BtnCtrls, ShlCtrls, PopCtrls, FolderCtrls, PaneCtrls,
  MemoCtrls, StretchCtrls, PropEditors, GLCtrls,
  {$IFDEF D6_UP}DesignIntf, DesignWindows, DesignEditors, {$IFDEF D8_UP}DesignMenus,{$ENDIF}
  {$ELSE}DsgnIntf, {$ENDIF}OpenTools, StrEdit;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Codebot',  [TAutoCompletion, TBrushButton, TButtonEdit,
    TCaptionBox, TExpandableBox, TCheckListEdit,  TColorGridButton, TDrawList,
    TDragDrawList, TDBDrawList, TDBReportView, TFolderView, TImageButton,
    TInspector, TIntegerEdit, TMoneyEdit, TPaneControl, TPenButton, TShellTree,
    TShellView, TShellEdit, TListEdit, TDatePopupEdit, TSlideBar,
    TOpenGLControl, TBackground]);
  RegisterNoIcon([TPaneSheet]);
  RegisterPropertyEditor(TStrings.ClassInfo, nil, '', TDefaultStringsProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomImageButton, 'ImageIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomImageButton, 'ImageDisabledIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TInspectorEditors.ClassInfo, TInspector, 'Editors',
    TInspectorEditorsProperty);
  RegisterComponentEditor(TInspector, TInspectorEditorsEditor);
  RegisterComponentEditor(TPaneControl, TPaneSheetEditor);
  RegisterComponentEditor(TPaneSheet, TPaneSheetEditor);
end;

end.
