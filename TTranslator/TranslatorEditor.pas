{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: TranslatorEditor.pas,v 1.41 2003/04/17 14:21:00 laa Exp $ }

{-----------------------------------------------------------------------------
  TranslatorEditor    The visual editor for TTranslator translating form
                      properties and code strings. See more on
                      http://www.polycon.fi/translator

  What                TdlgStringsEditor

  Company             Polycon
  Authors             LGE
----------------------------------------------------------------------------}

unit TranslatorEditor;

interface
{$i common.inc}

{$ifndef D5_OR_HIGHER}
// The Excel Automation is not tested with version 3 and 4
{$define NO_EXCEL}
{$endif D5_OR_HIGHER}

{$ifdef PERSONALDELPHI}
// Delphi Personal doesn't have the server tab components
{$define NO_EXCEL}
{$endif PERSONALDELPHI}

uses
{$ifndef NO_EXCEL}
  ExcelEdit, // The Excel Automation is not tested with Delphi versions 3 and 4
{$endif NO_EXCEL}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, Menus, ComCtrls,
  Translator,
  DataEditor, DBUDataEditorCell;

type
  TClassInfo = class
  public
    ClassType : TClass;
    Properties : TStringList;

    constructor Create(ClassType : TClass; ExampleComp : TObject; Strings : IEditableTranslatedStrings);
    destructor Destroy; override;
  end;

  TdlgStringsEditor = class(TForm)
    Menu: TMainMenu;
    MenuLanguages: TMenuItem;
    MenuLanguageAdd: TMenuItem;
    menuLanguageRename: TMenuItem;
    MenuLanguageRemove: TMenuItem;
    MenuLanguagesSeparator: TMenuItem;
    MenuClose: TMenuItem;
    PageControl: TPageControl;
    tsComponentProperties: TTabSheet;
    tsCodeStrings: TTabSheet;
    pnlTop: TPanel;
    lblShowProperties: TLabel;
    cboShow: TComboBox;
    bvlTop: TBevel;
    pnlRight: TPanel;
    cmdAddString: TButton;
    cmdDeleteString: TButton;
    pnlBottom: TPanel;
    tsClassesProperties: TTabSheet;
    pnlAddedPropertiesEditor: TPanel;
    spltrCRLeft: TSplitter;
    spltrCRRight: TSplitter;
    TreeViewComponents: TTreeView;
    ListBoxProps: TListBox;
    pnlCPButtons: TPanel;
    ButtonAdd: TButton;
    ButtonRemove: TButton;
    pnlCodeStrings: TPanel;
    menuEdit: TMenuItem;
    menuEditCut: TMenuItem;
    menuEditCopy: TMenuItem;
    menuEditPaste: TMenuItem;
    cmdDuplicateString: TButton;
    PopupMenuCodeStrings: TPopupMenu;
    menuPopupCut: TMenuItem;
    menuPopupCopy: TMenuItem;
    menuPopupPaste: TMenuItem;
    N1: TMenuItem;
    menuPopupAddString: TMenuItem;
    menuPopupDuplicateString: TMenuItem;
    menuPopupDeleteString: TMenuItem;
    menuEditRemoveDeletedComps: TMenuItem;
    MenuEditSeparator1: TMenuItem;
    mnuExcelEdit: TMenuItem;
    cboUnits: TComboBox;
    Label1: TLabel;
    pnlPropertiesRightSide: TPanel;
    MenuEditSeparator2: TMenuItem;
    menuEditRemoveClientTranslations: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cboShowChange(Sender: TObject);
    procedure MenuLanguageAddClick(Sender: TObject);
    procedure menuLanguageRenameClick(Sender: TObject);
    procedure MenuLanguageRemoveClick(Sender: TObject);
    procedure cmdAddStringClick(Sender: TObject);
    procedure cmdDeleteStringClick(Sender: TObject);
    procedure MenuCloseClick(Sender: TObject);
    procedure MenuLanguagesClick(Sender: TObject);
    procedure RefreshListBoxProps;
    procedure BuildTreeNodeComponents( ATreeView : TTreeView );
    procedure DeleteTreeNodeItems( TreeView : TTreeView );
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonRemoveClick(Sender: TObject);
    procedure TreeViewComponentsChange(Sender: TObject; Node: TTreeNode);
    procedure PageControlChange(Sender: TObject);
    procedure menuEditCutClick(Sender: TObject);
    procedure menuEditCopyClick(Sender: TObject);
    procedure menuEditPasteClick(Sender: TObject);
    procedure menuEditAddStringClick(Sender: TObject);
    procedure cmdDuplicateStringClick(Sender: TObject);
    procedure menuEditDuplicateStringClick(Sender: TObject);
    procedure menuEditDeleteStringClick(Sender: TObject);
    procedure menuEditRemoveDeletedCompsClick(Sender: TObject);
    procedure cboUnitsChange(Sender: TObject);
    procedure mnuExcelEditClick(Sender: TObject);
    procedure ListBoxPropsDblClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure menuEditRemoveClientTranslationsClick(Sender: TObject);
  private
    FFirstPageChange : Boolean;
    FStrings : IEditableTranslatedStrings;
    FComponentPropertyEditor,
    FClassesPropertyEditor,
    FCodeStringEditor : TDataEditor;
{$ifndef NO_EXCEL}
    fExcelEdit : TExcelEdit;
{$endif NO_EXCEL}
    procedure UpdateUnitsCombo(Selection : String);
    procedure Initialize;
    procedure SetStrings(AStrings : IEditableTranslatedStrings);
    procedure CreateEditors;
    function ActiveEditor : TDataEditor;

    procedure ReadPosition;
    procedure StorePosition;
    procedure OnClassesPropertyEditorDblClick(Sender: TObject);
{$ifndef NO_EXCEL}
    procedure ExcelEditClose(Sender: TObject; var Action: TCloseAction);
{$endif NO_EXCEL}
    procedure SetOwnerModified(Sender: TObject; Cell : TDataInplaceEditorCell);
  public
    procedure RefreshEditor;
    procedure DestroyEditors;
    procedure ShowClient(AClient : TTranslatorClient);
    property Strings : IEditableTranslatedStrings read FStrings write SetStrings;
  end;

implementation

uses
{$ifndef NO_EXCEL}
  ExcelEditForm,
{$endif NO_EXCEL}

  Dialogs,

  Math, Registry,
  DataTypes, DataElements, DataEditorLib, GridEditor, LanguageEditor, TypInfo,
  DataClipboard, TranslatorFields;

const
  USED_REGISTRY_KEY = 'SOFTWARE\Polycon\Translator';
var
  FClipBoard : TDataClipboard;

{$R *.DFM}

type
  TTranslatorClientLink = class(TTranslatorClient);

{ TClassInfo }

constructor TClassInfo.Create(ClassType : TClass; ExampleComp : TObject; Strings : IEditableTranslatedStrings);
begin
  Self.ClassType := ClassType;
  Properties := TStringList.Create;
  Properties.Sorted := True;
  Properties.Duplicates := dupIgnore;

  Strings.FillClassPropertyList(ClassType, ExampleComp, Properties);
end;

destructor TClassInfo.Destroy;
begin
  inherited Destroy;
  Properties.Free;
end;

{ TdlgStringsEditor }

function TdlgStringsEditor.ActiveEditor : TDataEditor;
begin
  if PageControl.ActivePage = tsComponentProperties then
    Result := FComponentPropertyEditor
  else if PageControl.ActivePage = tsClassesProperties then
    Result := FClassesPropertyEditor
  else
    Result := FCodeStringEditor;
end;

procedure TdlgStringsEditor.CreateEditors;

  procedure LangFieldWidths(ADataEditor : TDataEditor; DispalyField : Boolean);
  var
    iField, FirstIndex : Integer;
    TotalFieldWidth : Integer;
  begin
    with ADataEditor.ActiveGridEditor, ADataEditor.ActiveGridEditor.Grid do
    begin
      if DispalyField then
        FirstIndex := GridIndexOfField[FStrings.LanguageFields[0].DisplayField]
      else
        FirstIndex := GridIndexOfField[FStrings.LanguageFields[0]];

      TotalFieldWidth := ADataEditor.Parent.Width - 36;
      for iField := 0 to ColCount - 1 do
        if (iField < FirstIndex) or (iField >= FirstIndex + FStrings.LanguageCount) then
          Dec(TotalFieldWidth, ColWidths[iField]);

      TotalFieldWidth := TotalFieldWidth div FStrings.LanguageCount;

      for iField := 0 to FStrings.LanguageCount - 1 do
        ColWidths[iField + FirstIndex] := TotalFieldWidth;
    end;
  end;

begin
  FStrings.Translations.ArrangeRows;

  FComponentPropertyEditor := TDataEditor.CreateWithStandardView(tsComponentProperties, FStrings.Translations, False,
                                                                FStrings.EditorStandardView, nil, nil);
  FComponentPropertyEditor.ShowKeys := False;
  FComponentPropertyEditor.SetFocusOnExec := False;
  FComponentPropertyEditor.ShowAllBooleansAsCheckBoxes := True;
  FComponentPropertyEditor.MarkRowCalcField := MarkRowField;
  FComponentPropertyEditor.ShowMarkRowCalcField := True;
  FComponentPropertyEditor.Execute;

  LangFieldWidths(FComponentPropertyEditor, True);
  FComponentPropertyEditor.UpdateType := utRowCol;

  FCodeStringEditor := TDataEditor.CreateDefault( pnlCodeStrings, FStrings.StringTranslations, False, nil, nil);
  FCodeStringEditor.ShowKeys := True;
  FCodeStringEditor.SetFocusOnExec := False;
  FCodeStringEditor.ShowAllBooleansAsCheckBoxes := True;
  FCodeStringEditor.ShowMarkRowCalcField := True;
  FCodeStringEditor.Execute;
  FCodeStringEditor.AllowMultipleAutoCreatedRows := True;
  FCodeStringEditor.AutoCreateRows := True;
  FCodeStringEditor.AutoDeleteRows := True;
  FCodeStringEditor.OnChangeValue := SetOwnerModified;

  LangFieldWidths(FCodeStringEditor, False);

  if ActiveEditor.ActivegridEditor.Grid.Showing then
    ActiveEditor.ActivegridEditor.Grid.SetFocus;

  FClassesPropertyEditor := TDataEditor.CreateDefault( pnlAddedPropertiesEditor, FStrings.AddedProperties, False, nil, nil);
  FClassesPropertyEditor.Definition.DefaultStandardView.SetReadOnly( [FieldClass, FieldProperty], True );
  FClassesPropertyEditor.ShowKeys := True;
  FClassesPropertyEditor.SetFocusOnExec := False;
  FClassesPropertyEditor.ShowAllBooleansAsCheckBoxes := True;
  FClassesPropertyEditor.ShowMarkRowCalcField := True;
  FClassesPropertyEditor.OnDblClick := OnClassesPropertyEditorDblClick;
  FClassesPropertyEditor.Execute;
  FClassesPropertyEditor.OnChangeValue := SetOwnerModified;
end;

procedure TdlgStringsEditor.SetOwnerModified(Sender: TObject; Cell : TDataInplaceEditorCell);
begin
  TTranslatorClientLink(FStrings.GetOwner).SetModified;
end;

{
procedure TdlgStringsEditor.InvalidateActiveEditor;
begin
  ActiveEditor.ActivegridEditor.InvalidateGrid;
end;
}

procedure TdlgStringsEditor.DestroyEditors;
begin
  FreeAndNil(FComponentPropertyEditor);
  FreeAndNil(FCodeStringEditor);
  FreeAndNil(FClassesPropertyEditor);
end;

procedure TdlgStringsEditor.RefreshEditor;
var
  OldSelection : PChar;
begin
  if FComponentPropertyEditor <> nil then
    FComponentPropertyEditor.ActiveGridEditor.Disable;

  OldSelection := Pointer(cboUnits.Items.Objects[cboUnits.ItemIndex]);
  UpdateUnitsCombo(OldSelection);
  cboUnitsChange(Self);

  if FComponentPropertyEditor <> nil then
    FComponentPropertyEditor.ActiveGridEditor.Enable;
end;

procedure TdlgStringsEditor.SetStrings(AStrings : IEditableTranslatedStrings);
begin
  if (FStrings = nil) or (AStrings = nil) then
  begin
    FStrings := AStrings;
    if FStrings <> nil then
      Initialize;
  end;
end;

procedure TdlgStringsEditor.UpdateUnitsCombo(Selection : String);
var
  i : Integer;
  GUIDStr : PChar;
  Sel : Integer;
  OldFirstPageChange : Boolean;
begin
  OldFirstPageChange := FFirstPageChange;
  FFirstPageChange := True; // Disable events


  for i := 0 to cboUnits.Items.Count - 1 do
    if cboUnits.Items.Objects[i] <> nil then
      FreeMem(Pointer(cboUnits.Items.Objects[i]));
  cboUnits.Items.Clear;

  cboUnits.Items.AddObject('All', nil);
  Sel := 0;

  for i := 0 to FStrings.UnitList.RowCount - 1 do
  begin
    GetMem(GUIDStr, Length(FStrings.UnitList.Rows[i].StringValue[FieldGUID]) + 1);
    StrPCopy(GUIDStr, FStrings.UnitList.Rows[i].StringValue[FieldGUID]);

    cboUnits.Items.AddObject(FStrings.UnitList.Rows[i].StringValue[FieldFormClass] + ' in ' +
                             FStrings.UnitList.Rows[i].StringValue[FieldUnit] + '.dfm',
                             TObject(GUIDStr));
    if Selection = FStrings.UnitList.Rows[i].StringValue[FieldGUID] then
      Sel := i;
  end;

  cboUnits.ItemIndex := Sel;
  FFirstPageChange := OldFirstPageChange;
end;

procedure TdlgStringsEditor.Initialize;
begin
{$ifdef NO_EXCEL}
  mnuExcelEdit.Visible := False;
{$endif NO_EXCEL}
  ReadPosition;

  FStrings.EditorWindow := Self;

  FFirstPageChange := True;
  PageControl.ActivePage := tsComponentProperties;

  cboShow.ItemIndex := Integer(FStrings.ShowProperties);

  CreateEditors;

  UpdateUnitsCombo('');
  FFirstPageChange := False;

  cmdDeleteString.Enabled := FStrings.StringTranslations.RowCount > 0;
  BuildTreeNodeComponents(TreeViewComponents);
  TreeViewComponents.FullExpand;
end;

procedure TdlgStringsEditor.ShowClient(AClient : TTranslatorClient);

begin


  cboUnitsChange(nil);

  Show;
end;

procedure TdlgStringsEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
{$ifndef NO_EXCEL}
  if Assigned(fExcelEdit) then
  begin
    fExcelEdit.DoClose;
    Action := caNone;
  end
  else
{$endif NO_EXCEL}
  begin
    if Self.WindowState = wsNormal then
      StorePosition;

    Action := caFree;
  end;
end;

procedure TdlgStringsEditor.StorePosition;
var
  reg : TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;

    if reg.OpenKey(USED_REGISTRY_KEY, True) then
    begin
      reg.WriteInteger( 'EditorTop', Self.Top );
      reg.WriteInteger( 'EditorHeight', Self.Height );
      reg.WriteInteger( 'EditorLeft', Self.Left );
      reg.WriteInteger( 'EditorWidth', Self.Width );
      reg.WriteInteger( 'LeftSplitter', TreeViewComponents.Width );
      reg.WriteInteger( 'RightSplitter', ListBoxProps.Width );
    end;
  finally
    reg.Free;
  end;
end;

procedure TdlgStringsEditor.ReadPosition;
var
  reg : TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;

    if reg.OpenKeyReadOnly(USED_REGISTRY_KEY) then
    begin
      if reg.ValueExists( 'EditorTop' ) then
        Self.Top := reg.ReadInteger( 'EditorTop' );
      if reg.ValueExists( 'EditorHeight' ) then
        Self.Height := reg.ReadInteger( 'EditorHeight' );
      if reg.ValueExists( 'EditorLeft' ) then
        Self.Left := reg.ReadInteger( 'EditorLeft' );
      if reg.ValueExists( 'EditorWidth' ) then
        Self.Width := reg.ReadInteger( 'EditorWidth' );
      if reg.ValueExists( 'LeftSplitter' ) then
        TreeViewComponents.Width  := Max(reg.ReadInteger('LeftSplitter'), 30);
      if reg.ValueExists( 'RightSplitter' ) then
        ListBoxProps.Width := Max(reg.ReadInteger('RightSplitter'), 30);
    end;
  finally
    reg.Free;
  end;
end;

procedure TdlgStringsEditor.cboShowChange(Sender: TObject);
begin
  FComponentPropertyEditor.ActiveGridEditor.Disable;
  if cboShow.ItemIndex = 0 then
    FStrings.ShowProperties := spAllAdded
  else
    FStrings.ShowProperties := spTranslatedOnly;
  FComponentPropertyEditor.ActiveGridEditor.Enable;
end;

procedure TdlgStringsEditor.MenuLanguageAddClick(Sender: TObject);
var
  LangEditor : TdlgLangEditor;
begin
  Application.CreateForm(TdlgLangEditor, LangEditor);
  DestroyEditors;

  try
    LangEditor.AddLanguage(FStrings);
  finally
    LangEditor.Release;
    CreateEditors;
  end;
end;

procedure TdlgStringsEditor.menuLanguageRenameClick(Sender: TObject);
var
  LangEditor : TdlgLangEditor;
begin
  Application.CreateForm(TdlgLangEditor, LangEditor);

  try
    LangEditor.RenameLanguage(FStrings);
  finally
    LangEditor.Release;

    // Repaint headers
    FComponentPropertyEditor.ActiveGridEditor.Grid.RedrawRow(0);
    FComponentPropertyEditor.ActiveGridEditor.Grid.RedrawRow(0);
  end;
end;

procedure TdlgStringsEditor.MenuLanguageRemoveClick(Sender: TObject);
var
  LangEditor : TdlgLangEditor;
begin
  Application.CreateForm(TdlgLangEditor, LangEditor);
  DestroyEditors;

  try
    LangEditor.RemoveLanguage(FStrings);
  finally
    LangEditor.Release;
    CreateEditors;
  end;
end;

procedure TdlgStringsEditor.cmdAddStringClick(Sender: TObject);
begin
  FCodeStringEditor.ActiveGridEditor.NewRow;
  cmdDeleteString.Enabled := True;
  FCodeStringEditor.SetFocus;

  TTranslatorClientLink(FStrings.GetOwner).SetModified;
end;

procedure TdlgStringsEditor.cmdDuplicateStringClick(Sender: TObject);
begin
  FCodeStringEditor.ActiveGridEditor.DuplicateRow;
  cmdDeleteString.Enabled := FCodeStringEditor.ActiveGridEditor.CanDeleteRows;
  FCodeStringEditor.SetFocus;

  TTranslatorClientLink(FStrings.GetOwner).SetModified;
end;

procedure TdlgStringsEditor.cmdDeleteStringClick(Sender: TObject);
begin
  if FCodeStringEditor.ActiveGridEditor.CanDeleteRows then
    FCodeStringEditor.ActiveGridEditor.DeleteRows;

  cmdDeleteString.Enabled := FCodeStringEditor.ActiveGridEditor.CanDeleteRows;

  TTranslatorClientLink(FStrings.GetOwner).SetModified;
end;

procedure TdlgStringsEditor.MenuCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TdlgStringsEditor.MenuLanguagesClick(Sender: TObject);
begin
  menuLanguageRename.Enabled := (FStrings.LanguageCount > 1);
end;

procedure TdlgStringsEditor.RefreshListBoxProps;

  function SelfSelected(AClass : String; AProperty : String) : Boolean;
  var
    ARow : TDataRow;
  begin
    ARow := TDataRow(FStrings.AddedProperties.LocateRow([AClass, AProperty]));
    if (ARow <> nil) then
      result := True
    else
      result := False;
  end;

  procedure AddProperties(SelectedClass : String; ANode : TTreeNode; AList : TStringList);
  var
    ClassInfo : TClassInfo;
    i : Integer;
  begin
    if ANode = nil then
      Exit;

    ClassInfo := TClassInfo(ANode.Data);

    if ClassInfo = nil then
      Exit;

    for i := 0 to ClassInfo.Properties.Count - 1 do
      if not SelfSelected(SelectedClass, ClassInfo.Properties[i]) then
        AList.Add(ClassInfo.Properties[i]);

    for i := 0 to ANode.Count - 1 do
      AddProperties(SelectedClass, ANode.Item[i], AList);
  end;

var
  i : Integer;
  ANode : TTreeNode;
  APropertyList : TStringList;
begin
  ListBoxProps.Items.Clear;

  ANode := TreeViewComponents.Selected;
  if (ANode = nil) or (ANode.Data = nil) then
    Exit;

  APropertyList := TStringList.Create;
  APropertyList.Sorted := True;
  APropertyList.Duplicates := dupIgnore;

  AddProperties(TClassInfo(ANode.Data).ClassType.ClassName, ANode, APropertyList);
  ListBoxProps.Items.Assign(APropertyList);
  APropertyList.Free;

  for i := 0 to ListBoxProps.Items.Count - 1 do
    ListBoxProps.Selected[i] := True;
end;

procedure TdlgStringsEditor.BuildTreeNodeComponents( ATreeView : TTreeView );
var
  ABaseTreeNode : TTreeNode;

  function CreateParentNode( AClass : TClass; ExampleObject : TObject ) : TTreeNode;
  var
    AParentClass : TClass;
    tmpTreeNode : TTreeNode;
    i : Integer;
  begin
    tmpTreeNode := nil;
    AParentClass := AClass.ClassParent;
    for i := 0 to ATreeView.Items.Count - 1 do
    begin
      tmpTreeNode := ATreeView.Items.Item[i];
      if TClassInfo(tmpTreeNode.Data).ClassType = AParentClass then
        break;
      tmpTreeNode := nil;
    end;
    if tmpTreeNode = nil then
    begin
      if AParentClass.ClassParent <> TComponent then  // Stop at TComponent!
        tmpTreeNode := CreateParentNode(AParentClass, ExampleObject)
      else
        tmpTreeNode := ABaseTreeNode;
      result := ATreeView.Items.AddChildObject(tmpTreeNode, AParentClass.ClassName, TClassInfo.Create(AParentClass, ExampleObject, FStrings));
    end
    else
      result := tmpTreeNode;
  end;

var
  i : Integer;
  AComponent : TComponent;
  AClassType : TClass;
  ATreeNode : TTreeNode;
  Nodes : TList;
  AList : TList;
begin
  DeleteTreeNodeItems(ATreeView);
  ABaseTreeNode := ATreeView.Items.AddObject(nil, TComponent.ClassName, TClassInfo.Create(TComponent, nil, FStrings)); // Base item is TComponent

  Nodes := TList.Create;
  AList := TList.Create;

  FStrings.FillComponentList(AList);

  for i := 0 to AList.Count - 1 do
  begin
    AComponent := TComponent(AList.Items[i]);
    if not (AComponent is TTranslatorClient) then
    begin
      AClassType := AComponent.ClassType;
      if Nodes.IndexOf(AClassType) = -1 then
      begin
        Nodes.Add(AClassType);
        ATreeNode := CreateParentNode(AClassType, AComponent);
        ATreeView.Items.AddChildObject(ATreeNode, AComponent.ClassName, TClassInfo.Create(AClassType, AComponent, FStrings));
      end;
    end;
  end;

  Nodes.Free;
  AList.Free;
end;

procedure TdlgStringsEditor.DeleteTreeNodeItems( TreeView : TTreeView );
var
  TreeNode : TTreeNode;
  i : Integer;
begin
  for i := TreeView.Items.Count - 1 downto 0 do
  begin
    TreeNode := TreeView.Items[i];
    if TreeNode.Data <> nil then
      TObject(TreeNode.Data).Free;
  end;
  TreeView.Items.Clear;
end;

procedure TdlgStringsEditor.ButtonAddClick(Sender: TObject);
var
  i : integer;
  ClassInfo : TClassInfo;
  NewRow : TDataRow;
begin
  if TreeViewComponents.Selected <> nil then
    ClassInfo := TClassInfo(TTreeNode(TreeViewComponents.Selected).Data)
  else
    Exit;

  if (ClassInfo = nil) or
     (ListBoxProps.Items.Count = 0) or
     (ListBoxProps.SelCount = 0) then
    Exit;

  FClassesPropertyEditor.ActiveGridEditor.Disable;

  for i := 0 to ListBoxProps.Items.Count - 1 do
  begin
    if ListBoxProps.Selected[i] then
    begin
      NewRow := TDataRow.Create(AddedPropertiesTable);
      NewRow.StringValue[FieldClass] := ClassInfo.ClassType.ClassName;
      NewRow.StringValue[FieldProperty] := ListBoxProps.Items[i];
      NewRow.BooleanValue[FieldDoTranslate] := True;
      NewRow.BooleanValue[FieldSubClasses] := True;
      FStrings.AddedProperties.PutRow(NewRow, paDontOverwriteKeys);
    end;
  end;
  RefreshListBoxProps;

  FClassesPropertyEditor.ActiveGridEditor.RowStorage.ArrangeRows;
  FClassesPropertyEditor.ActiveGridEditor.Enable;

  TTranslatorClientLink(FStrings.GetOwner).SetModified;
end;

procedure TdlgStringsEditor.ButtonRemoveClick(Sender: TObject);
begin
  FClassesPropertyEditor.ActiveGridEditor.DeleteRows;

  RefreshListBoxProps;

  TTranslatorClientLink(FStrings.GetOwner).SetModified;
end;

procedure TdlgStringsEditor.TreeViewComponentsChange(Sender: TObject;
  Node: TTreeNode);
begin
  RefreshListBoxProps;
end;

procedure TdlgStringsEditor.PageControlChange(Sender: TObject);
begin
  if (not FFirstPageChange) and
     (PageControl.ActivePage = tsComponentProperties) then
  begin
    if FComponentPropertyEditor <> nil then
      FComponentPropertyEditor.ActiveGridEditor.Disable;

    FStrings.RefreshLanguages;

    if FComponentPropertyEditor <> nil then
      FComponentPropertyEditor.ActiveGridEditor.Enable;
  end;
end;

procedure TdlgStringsEditor.menuEditCutClick(Sender: TObject);
begin
  ActiveEditor.ActiveGridEditor.Cut(FClipBoard);
end;

procedure TdlgStringsEditor.menuEditCopyClick(Sender: TObject);
begin
  ActiveEditor.ActiveGridEditor.Copy(FClipBoard);
end;

procedure TdlgStringsEditor.menuEditPasteClick(Sender: TObject);
begin
  ActiveEditor.ActiveGridEditor.Paste(FClipBoard);
end;

procedure TdlgStringsEditor.menuEditAddStringClick(Sender: TObject);
begin
  cmdAddStringClick(Sender);
end;

procedure TdlgStringsEditor.menuEditDuplicateStringClick(Sender: TObject);
begin
  cmdDuplicateStringClick(Sender);
end;

procedure TdlgStringsEditor.menuEditDeleteStringClick(Sender: TObject);
begin
  cmdDeleteStringClick(Sender);
end;

procedure TdlgStringsEditor.menuEditRemoveDeletedCompsClick(
  Sender: TObject);
begin
  if FComponentPropertyEditor <> nil then
    FComponentPropertyEditor.ActiveGridEditor.Disable;

  FStrings.UpdateVisibleItems(True);

  if FComponentPropertyEditor <> nil then
    FComponentPropertyEditor.ActiveGridEditor.Enable;
end;

procedure TdlgStringsEditor.cboUnitsChange(Sender: TObject);
var
  GUIDStr : PChar;
begin
  if FFirstPageChange then
    Exit;

  FComponentPropertyEditor.ActiveGridEditor.Disable;

  GUIDStr := Pointer(cboUnits.Items.Objects[cboUnits.ItemIndex]);
  if GUIDStr = nil then
    FStrings.CurrentEditorClientGUID := ''
  else
    FStrings.CurrentEditorClientGUID := GUIDStr;

  if FStrings.CurrentEditorClientGUID = '' then
  begin
    FComponentPropertyEditor.ActiveGridEditor.ShowField(FieldFormClass);
    FComponentPropertyEditor.ActiveGridEditor.ShowField(FieldUnit);
  end
  else
  begin
    FComponentPropertyEditor.ActiveGridEditor.HideField(FieldFormClass);
    FComponentPropertyEditor.ActiveGridEditor.HideField(FieldUnit);
  end;

  FComponentPropertyEditor.ActiveGridEditor.Enable;
end;

procedure TdlgStringsEditor.mnuExcelEditClick(Sender: TObject);
{$ifndef NO_EXCEL}
var
  ExcelEditForm : TfrmExcelEdit;
  Edit1 : TExactGridEditorExcelEdit;
  Edit2 : TRowStorageExcelEdit;
{$endif NO_EXCEL}
begin
{$ifndef NO_EXCEL}


  if MessageDlg('Editing in Excel is only available in the commercial version '+
     'of the Translator. In this GPL version you can only export the translations ' +
     'but not import it back. Continue anyway?', mtConfirmation, [mbOK,mbCancel],0) = mrOK then

  begin
    MenuLanguages.Enabled := False;
    MenuEdit.Enabled := False;

    // The User Interface
    ExcelEditForm := TfrmExcelEdit.Create( Self );
    ExcelEditForm.OnClose := ExcelEditClose;
    ExcelEditForm.FileListRegKey := USED_REGISTRY_KEY + '\ImportFiles';

    // The logical objects, Edit1 and Edit2 that handles one GridEditor each
    // (the components and the code strings) plus one that bundles the two and
    // handles the communication with the UI
    Edit1 := TExactGridEditorExcelEdit.Create(ExcelEditForm, FComponentPropertyEditor.RowStorage.DataTable, FComponentPropertyEditor.ActiveGridEditor );
    Edit1.ExcelTransferId := etiFieldDescription;
    Edit1.AllowedImportRules := [eirReplace];
    Edit1.AllowAddingRows := False;
    Edit1.ConsiderKeys.Clear;
    Edit1.ConsiderKeys.Add( FieldUnit );
    Edit1.ConsiderKeys.Add( FieldFormClass );
    Edit1.ConsiderKeys.Add( FieldClass );
    Edit1.ConsiderKeys.Add( FieldComponent );
    Edit1.ConsiderKeys.Add( FieldProperty );

    Edit2 := TGridEditorExcelEdit.Create(ExcelEditForm, FCodeStringEditor.RowStorage.DataTable, FCodeStringEditor.ActiveGridEditor );
    Edit2.ExcelTransferId := etiFieldDescription;
    fExcelEdit := TMultiExcelEdit.Create(ExcelEditForm, [Edit1,Edit2]);

    // Run the form non-modal
    ExcelEditForm.ShowMe( fExcelEdit );
    ExcelEditForm.OnClose := ExcelEditClose;
  end;
{$endif NO_EXCEL}
end;

{$ifndef NO_EXCEL}
procedure TdlgStringsEditor.ExcelEditClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;

  FreeAndNil( fExcelEdit );

  MenuLanguages.Enabled := True;
  MenuEdit.Enabled := True;
end;
{$endif NO_EXCEL}

procedure TdlgStringsEditor.ListBoxPropsDblClick(Sender: TObject);
begin
  //double click on on item is the same as clicking on the arrow
  ButtonAddClick(Sender);
end;

procedure TdlgStringsEditor.OnClassesPropertyEditorDblClick(Sender: TObject);
begin
  //double click on on item is the same as clicking on the arrow
  ButtonRemoveClick(Sender);
end;

procedure TdlgStringsEditor.FormActivate(Sender: TObject);
begin
  RefreshEditor;

  FStrings.UpdateLanguage;
  if FComponentPropertyEditor <> nil then
    FComponentPropertyEditor.ActiveGridEditor.InvalidateGrid;
end;

procedure TdlgStringsEditor.FormDeactivate(Sender: TObject);
begin
  if (FStrings <> nil) and (FComponentPropertyEditor <> nil) then
    FComponentPropertyEditor.ActiveGridEditor.CommitChanges;
end;

procedure TdlgStringsEditor.FormDestroy(Sender: TObject);
var
  i : Integer;
begin
  DestroyEditors;

  if FStrings <> nil then
  begin
    FStrings.CurrentEditorClientGUID := '';
    FStrings.EditorWindow := nil;
    FStrings := nil;
  end;

  FFirstPageChange := True; // Disable events
  for i := 0 to cboUnits.Items.Count - 1 do
    if cboUnits.Items.Objects[i] <> nil then
      FreeMem(Pointer(cboUnits.Items.Objects[i]));
  cboUnits.Items.Clear;

  DeleteTreeNodeItems( TreeViewComponents );
end;

procedure TdlgStringsEditor.menuEditRemoveClientTranslationsClick(
  Sender: TObject);

begin

end;

initialization
  FClipBoard := TDataClipboard.Create( True, False );

finalization
  FClipBoard.Free;

end.


