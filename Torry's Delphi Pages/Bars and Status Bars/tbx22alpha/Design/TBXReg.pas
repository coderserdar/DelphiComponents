unit TBXReg;

// TBX Package
// Copyright 2001-2005 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// $Id: TBXReg.pas 129 2005-11-01 23:44:08Z Alex $

interface

{$I TB2Ver.inc}
{$I TBX.inc}

uses
  Windows, Classes, Controls, SysUtils, Graphics, ImgList, Dialogs,
  {$IFDEF JR_D6} DesignIntf, DesignEditors, VCLEditors, {$ELSE} DsgnIntf, {$ENDIF}
  TB2Reg, TB2Toolbar, TB2Item, TBX, TBXMDI, TBXSwitcher, TB2DsgnItemEditor,
  TBXExtItems, TBXLists, TBXDkPanels, TBXToolPals, TBXControls, TBXStatusBars,
  TBXGraphics, TBXImgListEdit;

procedure Register;

type
  TThemeProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{$IFDEF JR_D9}
  TTBXWideStringProperty = class(TWideStringProperty)
  end;
{$ELSE}
  TTBXWideStringProperty = class(TACPWideStringProperty)
  end;
{$ENDIF}

(*  TTBXWideCharProperty = class(TOrdinalProperty, ICustomPropertyDrawing)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    { IProperty }
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    { ICustomPropertyDrawing methods }
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  end; *)

  TTBXWideCaptionProperty = class(TTBXWideStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TTBXMultiLineWideStringProperty = class(TTBXWideStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TTBXMultiLineWideCaptionProperty = class(TTBXMultiLineWideStringProperty)
    function GetAttributes: TPropertyAttributes; override;
  end;

  TTBXStringListWProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

{$IFDEF JR_D5}
  TTBXLinkImageIndexPropertyEditor = class(TTBImageIndexPropertyEditor)
  public
    function GetImageListAt(Index: Integer): TCustomImageList; override;
  end;
{$ENDIF}

  TTBXStatusBarEditor = class(TDefaultEditor)
  protected
{$IFDEF JR_D6}
    procedure GetPanelsProp(const Prop: IProperty);
{$ELSE}
    procedure GetPanelsProp(Prop: TPropertyEditor);
{$ENDIF}
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TTBXItemsEditor = class(TTBItemsEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TTBXImageListEditor = class(TDefaultEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  Forms, TBXThemes, TBXStrEdit, TBXUtils, TypInfo, TB2Version, TB2ExtItems, TBXStrUtils,
  {TBXCharSelector,} RTLConsts, Consts;

type
  TTBXLinkAccess = class(TTBXCustomLink);
  TTBXButtonAccess = class(TTBXCustomButton);


{ TThemeProperty }

function TThemeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

procedure TThemeProperty.GetValues(Proc: TGetStrProc);
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  GetAvailableTBXThemes(SL);
  for I := 0 to SL.Count - 1 do Proc(SL[I]);
  SL.Free;
end;

{ TTBXWideCaptionProperty }

function TTBXWideCaptionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paAutoUpdate];
end;

{ TTBXMultiLineWideStringProperty }

procedure TTBXMultiLineWideStringProperty.Edit;
var
  Caption: WideString;
  Component: TPersistent;

  function AppendEOL(const S: WideString): WideString;
  begin
    if (Length(S) >= 2) and (S[Length(S) - 1] = #13) and (S[Length(S)] = #10) then Result := S + #13#10
    else Result := S;
  end;

  function RemoveEOL(const S: WideString): WideString;
  begin
    Result := S;
    if (Length(S) >= 2) and (S[Length(S) - 1] = #13) and (S[Length(S)] = #10) then SetLength(Result, Length(Result) - 2);
  end;

begin
  Component := GetComponent(0);
  if Component is TComponent then Caption := TComponent(Component).Name + '.' + GetName
  else Caption := GetName;
  Caption := 'MultiLine String Editor - ' + Caption;

  with TTBXStrEditDlg.Create(Application.Handle, Caption, True) do
  try
    EditText := AppendEOL(GetWideStrValue);
    if ShowModal = mrOk then SetWideStrValue(RemoveEOL(EditText));
  finally
    Free;
  end;
end;

function TTBXMultiLineWideStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TMultiLineWideCaptionProperty }

function TTBXMultiLineWideCaptionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paAutoUpdate];
end;

(*{ TTBXWideCharProperty }

procedure TTBXWideCharProperty.Edit;
var
  Value: WideChar;
begin
  Value := WideChar(GetOrdValue);
  if ShowCharSelectDialog(Value) then SetOrdValue(Integer(Value));
end;

function TTBXWideCharProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TTBXWideCharProperty.GetValue: string;
var
  Value: Integer;
begin
  Value := GetOrdValue;
  FmtStr(Result, '#%d', [Value]);
end;

procedure TTBXWideCharProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

procedure TTBXWideCharProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  Value: Integer;
  S: WideString;
begin
  Value := GetOrdValue;
  S := '#' + IntToStr(Value);
  if Value > 32 then S := S + ' (' + WideChar(Value) + ')';
  CanvasTextRectW(ACanvas, ARect, ARect.Left + 1, ARect.Top + 1, S);
end;

procedure TTBXWideCharProperty.SetValue(const Value: string);
var
  WValue: WideString;
  L: Longint;
begin
  WValue := Value;
  if Length(WValue) = 0 then L := 0 else
    if Length(WValue) = 1 then L := Ord(WValue[1]) else
      if WValue[1] = '#' then L := StrToInt(Copy(WValue, 2, Maxint)) else
        raise EPropertyError.CreateRes(@SInvalidPropertyValue);
  with GetTypeData(GetPropType)^ do
    if (L < MinValue) or (L > MaxValue) then
      raise EPropertyError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
  SetOrdValue(L);
end;     *)

{ TTBXStringListWProperty }

procedure TTBXStringListWProperty.Edit;
var
  Caption: WideString;
  Component: TComponent;
begin
  Component := TComponent(GetComponent(0));
  if Component is TComponent then Caption := TComponent(Component).Name + '.' + GetName
  else Caption := GetName;

  with TTBXStrEditDlg.Create(Application.Handle, Caption, True) do
  try
    EditText := TStringListW(GetOrdValue).Text;
    if ShowModal = mrOk then TStringListW(GetOrdValue).Text := EditText;
  finally
    Free;
  end;
end;

function TTBXStringListWProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

{$IFDEF JR_D5}
{ TTBXLinkImageIndexPropertyEditor }

function TTBXLinkImageIndexPropertyEditor.GetImageListAt(Index: Integer): TCustomImageList;
var
  C: TPersistent;
begin
  Result := nil;
  C := GetComponent(Index);
  if C is TTBXCustomLink then
    Result := TTBXLinkAccess(C).Images
  else if C is TTBXCustomButton then
    Result := TTBXButtonAccess(C).Images;
end;
{$ENDIF}

{ TTBXStatusBarEditor }

procedure TTBXStatusBarEditor.Edit;
var
{$IFDEF JR_D6}
  Components: IDesignerSelections;
{$ELSE}
  {$IFDEF JR_D5}
  Components: TDesignerSelectionList;
  {$ELSE}
  Components: TComponentList;
  {$ENDIF}
{$ENDIF}
begin
{$IFDEF JR_D6}
  Components := CreateSelectionList;
{$ELSE}
  {$IFDEF JR_D5}
  Components := TDesignerSelectionList.Create;
  {$ELSE}
  Components := TComponentList.Create;
  {$ENDIF}
{$ENDIF}
  try
    Components.Add(Component);
    GetComponentProperties(Components, [tkClass], Designer, GetPanelsProp);
  finally
{$IFNDEF JR_D6}
    Components.Free;
{$ENDIF}
  end;
end;

procedure TTBXStatusBarEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then Edit;
end;

function TTBXStatusBarEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then Result := '&Panels Editor...';
end;

function TTBXStatusBarEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF JR_D6}
procedure TTBXStatusBarEditor.GetPanelsProp(const Prop: IProperty);
begin
  if SameText(Prop.GetName, 'Panels') then Prop.Edit;
end;
{$ELSE}
procedure TTBXStatusBarEditor.GetPanelsProp(Prop: TPropertyEditor);
begin
  if CompareText(Prop.GetName, 'Panels') = 0 then Prop.Edit;
end;
{$ENDIF}

{ TTBXItemsEditor }

procedure TTBXItemsEditor.ExecuteVerb(Index: Integer);
const
  AboutText =
    '%s'#13#10 +
    '©2001–2005 Alex A. Denisov'#13#10 +
    'For conditions of distribution and use, see TBX documentation.'#13#10 +
    'Visit http://g32.org/tbx/ for the latest versions of TBX'#13#10 +
    #13#10 +
    'Running on'#13#10 +
    '%s'#13#10 +
    '©1998-2005 by Jordan Russell'#13#10 +
    'For conditions of distribution and use, see Toolbar2000 documentation.'#13#10 +
    #13#10 +
    'Visit http://www.jrsoftware.org/ for the latest versions of Toolbar2000'#13#10 +
    '';
begin
  case Index of
    0: Edit;
    1:
      begin
        MessageDlg(
          Format(AboutText,
          [TBXVersionText, Toolbar2000VersionPropText]),
          mtInformation, [mbOK], 0);
      end;
  end;
end;

{ TTBXImageListEditor }

procedure TTBXImageListEditor.Edit;
var
  Dlg: TTBXImageListDlg;
  L: TTBXImageList;
  Cmp: TTBXImageList;
begin
  Cmp := Component as TTBXImageList;
  L := TTBXImageList.Create(nil);
  try
    L.Assign(Cmp);
    Dlg := TTBXImageListDlg.Create;
    try
      Dlg.DIBList := L.DIBList;
      Dlg.ShowModal;
      if Dlg.ModalResult = mrOk then
      begin
        Cmp.Assign(L);
        Designer.Modified;
      end;
    finally
      Dlg.Free;
    end;
  finally
    L.Free;
  end;
end;

procedure TTBXImageListEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then Edit;
end;

function TTBXImageListEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TTBXImageListEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then Result := 'Edit...';
end;


{ THookObj }

type
  THookObj = class
    procedure HookProc(Sender: TTBItemEditForm);
  end;

var O: THookObj;

procedure THookObj.HookProc(Sender: TTBItemEditForm);
var
  TB: TTBToolbar;
  Item: TTBCustomItem;
  NewItem: TTBItem;
  S: string;
  I: Integer;
begin
  TB := TTBToolbar.Create(Sender);
  TB.Top := Sender.Height;
  TB.Parent := Sender;
  TB.Align := alTop;
  TB.Images := Sender.ToolbarItems.SubMenuImages;
  TB.ShowHint := True;

  for I := 0 to Sender.MoreMenu.Count - 1 do
  begin
    Item := Sender.MoreMenu.Items[I];
    if Item is TTBCustomItem then
    begin
      S := TTBCustomItemClass(Item.Tag).ClassName;
      if StrLComp(PChar(S), 'TTBX', 4) = 0 then
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

procedure Register;
begin
  RegisterComponents('Toolbar2000', [TTBXDock, TTBXMultiDock, TTBXToolbar,
    TTBXToolWindow, TTBXDockablePanel, TTBXPopupMenu, TTBXSwitcher, TTBXMRUList,
    TTBXMDIHandler, TTBXPageScroller, TTBXColorSet, TTBXAlignmentPanel,
    TTBXLabel, TTBXLink, TTBXButton, TTBXCheckBox, TTBXRadioButton, TTBXStatusBar,
    TTBXImageList]);

  RegisterNoIcon([TTBXItem, TTBXSubMenuItem, TTBXSeparatorItem,
    TTBXVisibilityToggleItem, TTBXLabelItem, TTBXMRUListItem, TTBXColorItem,
    TTBXMDIWindowItem, TTBXEditItem, TTBXSpinEditItem, TTBXDropDownItem,
    TTBXComboBoxItem, TTBXStringList, TTBXUndoList, TTBXToolPalette, TTBXColorPalette]);

  RegisterClasses([TTBXItem, TTBXSubMenuItem, TTBXSeparatorItem,
    TTBXVisibilityToggleItem, TTBXLabelItem, TTBXMRUListItem, TTBXColorItem,
    TTBXMDIWindowItem, TTBXEditItem, TTBXSpinEditItem, TTBXDropDownItem,
    TTBXComboBoxItem, TTBXStringList, TTBXUndoList, TTBXToolPalette, TTBXColorPalette]);

  RegisterComponentEditor(TTBXToolbar, TTBXItemsEditor);
  RegisterComponentEditor(TTBXPopupMenu, TTBXItemsEditor);
  RegisterPropertyEditor(TypeInfo(WideString), TTBCustomItem, 'Caption', TTBXMultiLineWideCaptionProperty);
  RegisterPropertyEditor(TypeInfo(WideString), TTBCustomItem, 'Hint', TTBXWideStringProperty);
  RegisterPropertyEditor(TypeInfo(WideString), TTBEditItem, 'EditCaption', TTBXWideCaptionProperty);
  RegisterPropertyEditor(TypeInfo(WideString), TTBEditItem, 'Text', TTBXWideCaptionProperty);
  RegisterPropertyEditor(TypeInfo(TStringListW), nil, '', TTBXStringListWProperty);
  RegisterPropertyEditor(TypeInfo(WideString), TTBXLabelItem, 'Caption', TTBXWideCaptionProperty);
  RegisterPropertyEditor(TypeInfo(WideString), TTBToolbar, 'Caption', TTBXWideCaptionProperty);
  RegisterPropertyEditor(TypeInfo(WideString), TTBXToolbar, 'Caption', TTBXWideCaptionProperty);
  RegisterPropertyEditor(TypeInfo(WideString), TTBToolbar, 'ChevronHint', TTBXMultiLineWideStringProperty);
  RegisterPropertyEditor(TypeInfo(WideString), TTBXToolbar, 'ChevronHint', TTBXMultiLineWideStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TTBXSwitcher, 'Theme', TThemeProperty);
  RegisterPropertyEditor(TypeInfo(WideString), TTBXTextObject, 'Caption', TTBXMultiLineWideCaptionProperty);
  RegisterPropertyEditor(TypeInfo(WideString), TTBXTextObject, 'Hint', TTBXWideStringProperty);

{$IFDEF JR_D5}
  RegisterPropertyEditor(TypeInfo(TImageIndex), TTBXCustomLink, 'ImageIndex', TTBXLinkImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TTBXCustomButton, 'ImageIndex', TTBXLinkImageIndexPropertyEditor);
{$ENDIF}

  RegisterComponentEditor(TTBXStatusBar, TTBXStatusBarEditor);
  RegisterComponentEditor(TTBXImageList, TTBXImageListEditor);

  TBRegisterItemClass(TTBXItem, 'New &TBX Item', HInstance);
  TBRegisterItemClass(TTBXSubMenuItem, 'New TBX Submenu Item', HInstance);
  TBRegisterItemClass(TTBXSeparatorItem, 'New TBX Separator Item', HInstance);
  TBRegisterItemClass(TTBXVisibilityToggleItem, 'New TBX Visibility Toggle Item', HInstance);
  TBRegisterItemClass(TTBXLabelItem, 'New TBX Label Item', HInstance);
  TBRegisterItemClass(TTBXMRUListItem, 'New TBX MRU List Item', HInstance);
  TBRegisterItemClass(TTBXColorItem, 'New TBX Color Item', HInstance);
  TBRegisterItemClass(TTBXMDIWindowItem, 'New TBX MDI Window Item', HInstance);
  TBRegisterItemClass(TTBXEditItem, 'New TBX Edit Item', HInstance);
  TBRegisterItemClass(TTBXSpinEditItem, 'New TBX Spin Edit Item', HInstance);
  TBRegisterItemClass(TTBXDropDownItem, 'New TBX Drop Down Item', HInstance);
  TBRegisterItemClass(TTBXComboBoxItem, 'New TBX Combo Box Item', HInstance);
  TBRegisterItemClass(TTBXStringList, 'New TBX String List', HInstance);
  TBRegisterItemClass(TTBXUndoList, 'New TBX Undo List', HInstance);
  TBRegisterItemClass(TTBXToolPalette, 'New TBX Tool Palette', HInstance);
  TBRegisterItemClass(TTBXColorPalette, 'New TBX Color Palette', HInstance);
end;

initialization
  O := THookObj.Create;
  TBUnregisterDsgnEditorHook(O.HookProc);
  TBRegisterDsgnEditorHook(O.HookProc);

finalization
  TBUnregisterDsgnEditorHook(O.HookProc);
  O.Free;

end.
