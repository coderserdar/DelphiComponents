unit SpTBXReg;

{==============================================================================
The contents of this file are subject to the SpTBXLib License; you may
not use or distribute this file except in compliance with the
SpTBXLib License. 
A copy of the SpTBXLib License may be found in SpTBXLib-LICENSE.txt or at:
  http://www.silverpointdevelopment.com/sptbxlib/SpTBXLib-LICENSE.htm

Alternatively, the contents of this file may be used under the terms of the
Mozilla Public License Version 1.1 (the "MPL v1.1"), in which case the provisions
of the MPL v1.1 are applicable instead of those in the SpTBXLib License.
A copy of the MPL v1.1 may be found in MPL-LICENSE.txt or at:
  http://www.mozilla.org/MPL/
  
If you wish to allow use of your version of this file only under the terms of
the MPL v1.1 and not to allow others to use your version of this file under the
SpTBXLib License, indicate your decision by deleting the provisions
above and replace them with the notice and other provisions required by the
MPL v1.1. If you do not delete the provisions above, a recipient may use your
version of this file under either the SpTBXLib License or the MPL v1.1.

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The initial developer of this code is Robert Lee.

Requirements:
For Delphi/C++Builder 2009 or newer:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
For Delphi/C++Builder 7-2007:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
  - Troy Wolbrink's TNT Unicode Controls
    http://www.tntware.com/delphicontrols/unicode/
==============================================================================}

interface

{$I TB2Ver.inc}

uses
  Windows, Classes, Controls, SysUtils, Graphics, ImgList, Dialogs,
  {$IFDEF JR_D6} DesignIntf, DesignEditors, VCLEditors, {$ELSE} DsgnIntf, {$ENDIF}
  TB2Reg, TB2Toolbar, TB2Item, TB2DsgnItemEditor,
  SpTBXItem, SpTBXTabs, SpTBXDkPanels, SpTBXFormPopupMenu,
  SpTBXControls, SpTBXEditors, SpTBXExtEditors, SpTBXPageScroller,
  SpTBXCustomizer, SpTBXMDIMRU;

type
  { TSpTBXItemsEditor }

  TSpTBXItemsEditor = class(TTBItemsEditor)
  public
    procedure Edit; override;
  end;

  { TSpTBXImageIndexEditor }

  TSpTBXImageIndexEditor = class(TIntegerProperty, ICustomPropertyListDrawing)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetImageListAt(Index: Integer): TCustomImageList; virtual;

    // ICustomPropertyListDrawing
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  end;

procedure Register;

implementation

uses
  Forms, TypInfo;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Helpers }

procedure SpEditFormAddItems(Sender: TTBItemEditForm; const ToolbarName, ItemsPrefix: string);
// Creates a new toolbar in the EditForm with all the items that have the same prefix.
var
  TB: TTBToolbar;
  Item: TTBCustomItem;
  NewItem: TTBItem;
  S: string;
  I, C: Integer;
begin
  if Sender.FindComponent(ToolbarName) <> nil then Exit;

  // Create our own toolbar in the editor
  TB := TTBToolbar.Create(Sender);
  TB.Top := Sender.Height;
  TB.Parent := Sender;
  TB.Align := alTop;
  TB.Images := Sender.ToolbarItems.SubMenuImages;
  TB.ShowHint := True;
  TB.Name := ToolbarName;

  C := Length(ItemsPrefix);
  for I := 0 to Sender.MoreMenu.Count - 1 do
  begin
    Item := Sender.MoreMenu.Items[I];
    if Item is TTBCustomItem then
    begin
      S := TTBCustomItemClass(Item.Tag).ClassName;
      if StrLComp(PChar(S), PChar(ItemsPrefix), C) = 0 then
      begin
        NewItem := TTBItem.Create(TB);
        TB.Items.Add(NewItem);
        NewItem.Caption := Item.Caption;
        NewItem.ImageIndex := Item.ImageIndex;
        NewItem.Tag := Item.Tag;
        NewItem.Hint := S;
        NewItem.OnClick := Item.OnClick;
      end;
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBItemsEditor }

procedure TSpTBXItemsEditor.Edit;
var
  Intf: ITBItems;
  F: TTBItemEditForm;
  I: Integer;
begin
  if Assigned(Component) and Component.GetInterface(ITBItems, Intf) then begin
    inherited;
    F := nil;
    if Screen.ActiveForm is TTBItemEditForm then
      F := Screen.ActiveForm as TTBItemEditForm
    else
      for I := 0 to Screen.FormCount - 1 do
        if Screen.Forms[I] is TTBItemEditForm then begin
          F := Screen.Forms[I] as TTBItemEditForm;
          Break;
        end;

    if Assigned(F) then
      SpEditFormAddItems(F, 'SpTBToolbar', 'TSpTBX');
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXImageIndexEditor }

function TSpTBXImageIndexEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

function TSpTBXImageIndexEditor.GetImageListAt(Index: Integer): TCustomImageList;
begin
  Result := TCustomImageList(TypInfo.GetObjectProp(GetComponent(Index), 'Images'));
end;

procedure TSpTBXImageIndexEditor.GetValues(Proc: TGetStrProc);
var
  ImgList: TCustomImageList;
  I: Integer;
begin
  ImgList := GetImageListAt(0);
  if Assigned(ImgList) then
    for I := 0 to ImgList.Count-1 do
      Proc(IntToStr(I));
end;

procedure TSpTBXImageIndexEditor.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  ImgList: TCustomImageList;
  X: Integer;
begin
  ImgList := GetImageListAt(0);
  ACanvas.FillRect(ARect);
  X := ARect.Left + 2;
  if Assigned(ImgList) then begin
    ImgList.Draw(ACanvas, X, ARect.Top + 2, StrToInt(Value));
    Inc(X, ImgList.Width);
  end;
  ACanvas.TextOut(X + 3, ARect.Top + 1, Value);
end;

procedure TSpTBXImageIndexEditor.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
var
  ImgList: TCustomImageList;
begin
  ImgList := GetImageListAt(0);
  AHeight := ACanvas.TextHeight(Value) + 2;
  if Assigned(ImgList) and (ImgList.Height + 4 > AHeight) then
    AHeight := ImgList.Height + 4;
end;

procedure TSpTBXImageIndexEditor.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  ImgList: TCustomImageList;
begin
  ImgList := GetImageListAt(0);
  AWidth := ACanvas.TextWidth(Value) + 4;
  if Assigned(ImgList) then
    Inc(AWidth, ImgList.Width);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM

procedure Register;
begin
  {$IFDEF JR_D9}
  ForceDemandLoadState(dlDisable);
  {$ENDIF}

  RegisterComponents('SpTBXLib', [TSpTBXDock, TSpTBXMultiDock, TSpTBXToolbar,
    TSpTBXToolWindow, TSpTBXDockablePanel, TSpTBXTabSet, TSpTBXTabControl, TSpTBXStatusBar,
    TSpTBXPopupMenu, TSpTBXFormPopupMenu, TSpTBXMDIHandler, TSpTBXTitleBar,
    TSpTBXLabel, TSpTBXCheckBox, TSpTBXRadioButton, TSpTBXButton, TSpTBXSpeedButton,
    TSpTBXProgressBar, TSpTBXTrackBar, TSpTBXSplitter, TSpTBXPanel, TSpTBXGroupBox,
    TSpTBXRadioGroup, TSpTBXEdit, TSpTBXButtonEdit, TSpTBXSpinEdit, TSpTBXColorEdit,
    TSpTBXComboBox, TSpTBXFontComboBox, TSpTBXListBox, TSpTBXCheckListBox, TSpTBXColorListBox,
    TSpTBXPageScroller, TSpTBXCustomizer]);

  RegisterClasses([TSpTBXTabSheet, TSpTBXCustomTabSet, TSpTBXCustomTabControl]);

  // TSpTBXItem
  RegisterNoIcon([TSpTBXItem]);
  RegisterClasses([TSpTBXItem]);
  TBRegisterItemClass(TSpTBXItem, 'New SpTBX Item', HInstance);
  // TSpTBXSubmenuItem
  RegisterNoIcon([TSpTBXSubmenuItem]);
  RegisterClasses([TSpTBXSubmenuItem]);
  TBRegisterItemClass(TSpTBXSubmenuItem, 'New SpTBX Submenu Item', HInstance);
  // TSpTBXSeparatorItem
  RegisterNoIcon([TSpTBXSeparatorItem]);
  RegisterClasses([TSpTBXSeparatorItem]);
  TBRegisterItemClass(TSpTBXSeparatorItem, 'New SpTBX Separator Item', HInstance);
  // TSpTBXSpacerItem
  RegisterNoIcon([TSpTBXRightAlignSpacerItem]);
  RegisterClasses([TSpTBXRightAlignSpacerItem]);
  TBRegisterItemClass(TSpTBXRightAlignSpacerItem, 'New SpTBX RightAlignSpacer Item', HInstance);
  // TSpTBXLabelItem
  RegisterNoIcon([TSpTBXLabelItem]);
  RegisterClasses([TSpTBXLabelItem]);
  TBRegisterItemClass(TSpTBXLabelItem, 'New SpTBX Label Item', HInstance);
  // TSpTBXSkinGroupItem
  RegisterNoIcon([TSpTBXSkinGroupItem]);
  RegisterClasses([TSpTBXSkinGroupItem]);
  TBRegisterItemClass(TSpTBXSkinGroupItem, 'New SpTBX SkinGroup Item', HInstance);
  // TSpTBXColorItem
  RegisterNoIcon([TSpTBXColorItem]);
  RegisterClasses([TSpTBXColorItem]);
  TBRegisterItemClass(TSpTBXColorItem, 'New SpTBX Color Item', HInstance);
  // TSpTBXTabItem
  RegisterNoIcon([TSpTBXTabItem]);
  RegisterClasses([TSpTBXTabItem]);
  TBRegisterItemClass(TSpTBXTabItem, 'New SpTBX Tab Item', HInstance);
  // TSpTBXEditItem
  RegisterNoIcon([TSpTBXEditItem]);
  RegisterClasses([TSpTBXEditItem]);
  TBRegisterItemClass(TSpTBXEditItem, 'New SpTBX Edit Item', HInstance);
  // TSpTBXSpinEditItem
  RegisterNoIcon([TSpTBXSpinEditItem]);
  RegisterClasses([TSpTBXSpinEditItem]);
  TBRegisterItemClass(TSpTBXSpinEditItem, 'New SpTBX Spin Edit Item', HInstance);
  // TSpTBXToolPalette
  RegisterNoIcon([TSpTBXToolPalette]);
  RegisterClasses([TSpTBXToolPalette]);
  TBRegisterItemClass(TSpTBXToolPalette, 'New SpTBX Tool Palette Item', HInstance);
  // TSpTBXColorPalette
  RegisterNoIcon([TSpTBXColorPalette]);
  RegisterClasses([TSpTBXColorPalette]);
  TBRegisterItemClass(TSpTBXColorPalette, 'New SpTBX Color Palette Item', HInstance);
  // TSpTBXMRUListItem
  RegisterNoIcon([TSpTBXMRUListItem]);
  RegisterClasses([TSpTBXMRUListItem]);
  TBRegisterItemClass(TSpTBXMRUListItem, 'New SpTBX MRU List Item', HInstance);
  // TSpTBXMDIWindowItem
  RegisterNoIcon([TSpTBXMDIWindowItem]);
  RegisterClasses([TSpTBXMDIWindowItem]);
  TBRegisterItemClass(TSpTBXMDIWindowItem, 'New SpTBX MDI Window Item', HInstance);

  // Register the components editor, the components must implement IItems interface
  RegisterComponentEditor(TSpTBXToolbar, TSpTBXItemsEditor);
  RegisterComponentEditor(TSpTBXCompoundItemsControl, TSpTBXItemsEditor);
  RegisterComponentEditor(TSpTBXDockablePanel, TSpTBXItemsEditor);
  RegisterComponentEditor(TSpTBXCustomizer, TSpTBXItemsEditor);
  RegisterComponentEditor(TSpTBXPopupMenu, TSpTBXItemsEditor);

  // Register the ImageIndex property editor for TSpTBXTextObject descendants, this is
  // needed to show the preview of images in the property editor combobox.
  RegisterPropertyEditor(TypeInfo(TImageIndex), TSpTBXTextObject, '', TSpTBXImageIndexEditor);
end;

end.
