unit TB2DsgnItemEditor;

{
  Toolbar2000
  Copyright (C) 1998-2008 by Jordan Russell
  All rights reserved.

  The contents of this file are subject to the "Toolbar2000 License"; you may
  not use or distribute this file except in compliance with the
  "Toolbar2000 License". A copy of the "Toolbar2000 License" may be found in
  TB2k-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/TB2k-LICENSE.txt

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License (the "GPL"), in which case the provisions of the
  GPL are applicable instead of those in the "Toolbar2000 License". A copy of
  the GPL may be found in GPL-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/GPL-LICENSE.txt
  If you wish to allow use of your version of this file only under the terms of
  the GPL and not to allow others to use your version of this file under the
  "Toolbar2000 License", indicate your decision by deleting the provisions
  above and replace them with the notice and other provisions required by the
  GPL. If you do not delete the provisions above, a recipient may use your
  version of this file under either the "Toolbar2000 License" or the GPL.

  $jrsoftware: tb2k/Source/TB2DsgnItemEditor.pas,v 1.63 2008/09/25 18:49:31 jr Exp $
}

interface

{$I TB2Ver.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF CLR} System.ComponentModel, {$ENDIF}
  StdCtrls, ExtCtrls, Buttons, ComCtrls, ImgList, Menus,
  TB2Item, TB2Toolbar, TB2Dock,
  {$IFDEF JR_D6}
    DesignIntf, DesignWindows, DesignEditors;
  {$ELSE}
    DsgnIntf, DsgnWnds, LibIntf;
  {$ENDIF}

const
  CM_DEFERUPDATE = WM_USER + 100;

type
  TTBItemEditForm = class(TDesignWindow)
    TreeView: TTreeView;
    ListView: TListView;
    Splitter1: TSplitter;
    Toolbar: TTBToolbar;
    NewSubmenuButton: TTBItem;
    NewItemButton: TTBItem;
    NewSepButton: TTBItem;
    DeleteButton: TTBItem;
    TBSeparatorItem1: TTBSeparatorItem;
    TBPopupMenu1: TTBPopupMenu;
    TBItemContainer1: TTBItemContainer;
    ToolbarItems: TTBSubmenuItem;
    CopyButton: TTBItem;
    CutButton: TTBItem;
    PasteButton: TTBItem;
    MoreMenu: TTBSubmenuItem;
    TBSeparatorItem2: TTBSeparatorItem;
    TBSubmenuItem1: TTBSubmenuItem;
    TConvertMenu: TTBItem;
    TBSeparatorItem3: TTBSeparatorItem;
    MoveUpButton: TTBItem;
    MoveDownButton: TTBItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure NewSubmenuButtonClick(Sender: TObject);
    procedure NewItemButtonClick(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure DeleteButtonClick(Sender: TObject);
    procedure NewSepButtonClick(Sender: TObject);
    procedure ListViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeViewEnter(Sender: TObject);
    procedure TreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure CopyButtonClick(Sender: TObject);
    procedure ListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CutButtonClick(Sender: TObject);
    procedure PasteButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ListViewKeyPress(Sender: TObject; var Key: Char);
    procedure ListViewDblClick(Sender: TObject);
    procedure ListViewEnter(Sender: TObject);
    procedure TreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TConvertMenuClick(Sender: TObject);
    procedure TreeViewKeyPress(Sender: TObject; var Key: Char);
    procedure MoveUpButtonClick(Sender: TObject);
    procedure MoveDownButtonClick(Sender: TObject);
  private
    FParentComponent: TComponent;
    FRootItem, FSelParentItem: TTBCustomItem;
    FNotifyItemList: TList;
    FSettingSel, FRebuildingTree, FRebuildingList: Integer;
    function AddListViewItem(const Index: Integer;
      const Item: TTBCustomItem): TListItem;
    procedure CMDeferUpdate(var Message: TMessage); message CM_DEFERUPDATE;
    procedure Copy;
    procedure CreateNewItem(const AClass: TTBCustomItemClass);
    procedure Cut;
    procedure Delete;
    procedure DeleteItem(const Item: TTBCustomItem);
    function GetItemTreeCaption(AItem: TTBCustomItem): String;
    procedure GetSelItemList(const AList: TList);
    procedure ItemNotification(Ancestor: TTBCustomItem; Relayed: Boolean;
      Action: TTBItemChangedAction; Index: Integer; Item: TTBCustomItem);
    procedure MoreItemClick(Sender: TObject);
    procedure MoveItem(CurIndex, NewIndex: Integer);
    procedure Paste;
    procedure RebuildList;
    procedure RebuildTree;
    procedure SelectInObjectInspector(AList: TList);
    procedure SetSelParentItem(ASelParentItem: TTBCustomItem);
    function TreeViewDragHandler(Sender, Source: TObject; X, Y: Integer;
      Drop: Boolean): Boolean;
    procedure UnregisterAllNotifications;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function UniqueName(Component: TComponent): String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF JR_D6}
    function EditAction(Action: TEditAction): Boolean; override;
    {$ELSE}
    procedure EditAction(Action: TEditAction); override;
    {$ENDIF}
    function GetEditState: TEditState; override;
  end;

  TTBItemsEditor = class(TDefaultEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

  TTBItemsPropertyEditor = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
  end;

procedure TBRegisterItemClass(AClass: TTBCustomItemClass;
  const ACaption: String; ResInstance: HINST);

implementation

{$R *.DFM}

uses
  {$IFDEF CLR} System.Drawing, System.IO, System.Reflection, {$ENDIF}
  TypInfo, CommCtrl, TB2Version, TB2Common, TB2DsgnConverter;

type
{$IFNDEF JR_D5}
  TDesignerSelectionList = TComponentList;
{$ENDIF}
{$IFDEF JR_D6}
  TDesignerSelectionList = IDesignerSelections;
{$ENDIF}

  TItemClassInfo = class
    ItemClass: TTBCustomItemClass;
    Caption: String;
    ImageIndex: Integer;
  end;

var
  ItemClasses: TList;
  ItemImageList: TImageList;

{$IFNDEF JR_D6}
function CreateSelectionList: TDesignerSelectionList;
begin
  Result := TDesignerSelectionList.Create;
end;
{$ENDIF}

procedure FreeItemClasses;
var
  I: Integer;
  IC: TList;
begin
  if ItemClasses = nil then Exit;
  IC := ItemClasses;
  ItemClasses := nil;
  for I := IC.Count-1 downto 0 do
    TItemClassInfo(IC[I]).Free;
  IC.Free;
end;

{ Note: AFAIK, there is no need for a similar function on .NET since assemblies
  can't be unloaded. When a design-time package is uninstalled, it remains
  loaded until the IDE is restarted. }
{$IFNDEF CLR}
procedure UnregisterModuleItemClasses(AModule: {$IFDEF JR_D5} LongWord {$ELSE} Integer {$ENDIF});
var
  I: Integer;
  Info: TItemClassInfo;
begin
  I := 0;
  while I < ItemClasses.Count do begin
    Info := TItemClassInfo(ItemClasses[I]);
    if FindClassHInstance(Info.ItemClass) = AModule then begin
      ItemClasses.Delete(I);
      Info.Free;
    end
    else
      Inc(I);
  end;
  { Note: TTBItemEditForm also holds references to item classes, but since
    Delphi automatically closes all editor forms before compiling/removing
    a package, we don't need to remove them. }
end;
{$ENDIF}

{$IFNDEF CLR}
function LoadItemImage(Instance: HINST; const ResName: String): Integer;
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Handle := LoadBitmap(Instance, {$IFNDEF CLR}PChar{$ENDIF}(ResName));
    if Bmp.Handle = 0 then
      Result := -1
    else
      Result := ItemImageList.AddMasked(Bmp, Bmp.Canvas.Pixels[0, Bmp.Height-1]);
  finally
    Bmp.Free;
  end;
end;
{$ELSE}
function LoadItemImage(const AAssembly: System.Reflection.Assembly;
  const ResName: String): Integer;
var
  Bmp: TBitmap;
  ResStream: System.IO.Stream;
  ResBmp: System.Drawing.Bitmap;
begin
  Bmp := TBitmap.Create;
  try
    ResStream := AAssembly.GetManifestResourceStream(ResName);
    if ResStream = nil then begin
      Result := -1;
      Exit;
    end;
    try
      ResBmp := System.Drawing.Bitmap.Create(ResStream);
      try
        Bmp.LoadFromBitmap(ResBmp);
      finally
        ResBmp.Dispose;
      end;
    finally
      ResStream.Close;
    end;
    Result := ItemImageList.AddMasked(Bmp, Bmp.Canvas.Pixels[0, Bmp.Height-1]);
  finally
    Bmp.Free;
  end;
end;
{$ENDIF}

procedure TBRegisterItemClass(AClass: TTBCustomItemClass;
  const ACaption: String; ResInstance: HINST);
var
  I: Integer;
  Info: TItemClassInfo;
begin
  { Hack for Delphi.NET 2006 bug:
    If you start Delphi, open & rebuild the tb2k_dn10 package only, then open
    the Demo project, the IDE calls the Register procedure on tb2kdsgn_d10 a
    second time, without reloading either of the two packages. As a result,
    the TBRegisterItemClass calls are repeated. To avoid doubled items on the
    editor form's More menu, check if the class was already registered. }
  for I := 0 to ItemClasses.Count-1 do
    if TItemClassInfo(ItemClasses[I]).ItemClass = AClass then
      Exit;
  Info := TItemClassInfo.Create;
  Info.ItemClass := AClass;
  Info.Caption := ACaption;
  {$IFNDEF CLR}
  Info.ImageIndex := LoadItemImage(ResInstance,
    Uppercase(AClass.ClassName {$IFDEF JR_D9} , loInvariantLocale {$ENDIF}));
  {$ELSE}
  Info.ImageIndex := LoadItemImage(Assembly.GetCallingAssembly,
    AClass.ClassName + '.bmp');
  {$ENDIF}
  ItemClasses.Add(Info);
end;

function GetItemClassImage(AClass: TTBCustomItemClass): Integer;
var
  I: Integer;
  Info: TItemClassInfo;
begin
  for I := ItemClasses.Count-1 downto 0 do begin
    Info := TItemClassInfo(ItemClasses[I]);
    if AClass.InheritsFrom(Info.ItemClass) then begin
      Result := Info.ImageIndex;
      if Result >= 0 then
        Exit;
    end;
  end;
  if AClass.InheritsFrom(TTBSubmenuItem) then
    Result := 1
  else if AClass.InheritsFrom(TTBSeparatorItem) then
    Result := 2
  else
    Result := 0;
end;

procedure ShowEditForm(AParentComponent: TComponent; ARootItem: TTBCustomItem;
  const ADesigner: {$IFDEF JR_D6} IDesigner {$ELSE} IFormDesigner {$ENDIF});
var
  I: Integer;
  Form: TCustomForm;
  EditForm: TTBItemEditForm;
begin
  if Assigned(ARootItem.LinkSubitems) then begin
    case MessageDlg(Format('The LinkSubitems property is set to ''%s''. ' +
       'Would you like to edit that item instead?',
       [ARootItem.LinkSubitems.Name]), mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: begin
          AParentComponent := ARootItem.LinkSubitems;
          ARootItem := ARootItem.LinkSubitems;
        end;
      mrCancel: Exit;
    end;
  end;
  for I := 0 to Screen.FormCount-1 do begin
    Form := Screen.Forms[I];
    if Form is TTBItemEditForm then
      if TTBItemEditForm(Form).FRootItem = ARootItem then begin
        Form.Show;
        if Form.WindowState = wsMinimized then
          Form.WindowState := wsNormal;
        Exit;
      end;
  end;
  EditForm := TTBItemEditForm.Create(Application);
  try
    EditForm.Designer := ADesigner;
    EditForm.FParentComponent := AParentComponent;
    AParentComponent.FreeNotification(EditForm);
    EditForm.FRootItem := ARootItem;
    ARootItem.FreeNotification(EditForm);
    EditForm.FSelParentItem := ARootItem;
    EditForm.Caption := 'Editing ' + AParentComponent.Name;
    EditForm.RebuildTree;
    EditForm.RebuildList;
    {$IFDEF JR_D9}
    EditForm.PopupMode := pmExplicit;
    {$ENDIF}
    EditForm.Show;
  except
    EditForm.Free;
    raise;
  end;
end;

function IsSubmenuItem(Item: TTBCustomItem): Boolean;
begin
  Result := tbisSubitemsEditable in Item.GetItemStyle;
end;

procedure ShowVersion;
const
  AboutText =
    '%s'#13#10 +
    'Copyright (C) 1998-2008 by Jordan Russell'#13#10 +
    'For conditions of distribution and use, see LICENSE.TXT.'#13#10 +
    #13#10 +
    'Visit my web site for the latest versions of Toolbar2000:'#13#10 +
    'http://www.jrsoftware.org/';
begin
  MessageDlg(Format(AboutText, [Toolbar2000VersionPropText]), mtInformation,
    [mbOK], 0);
end;


{ TTBItemEditForm }

constructor TTBItemEditForm.Create(AOwner: TComponent);
var
  I: Integer;
  Info: TItemClassInfo;
  Item: TTBItem;
begin
  inherited;
  FNotifyItemList := TList.Create;
  ToolbarItems.SubMenuImages := ItemImageList;
  ListView.SmallImages := ItemImageList;
  { Populate the 'More' menu }
  for I := 0 to ItemClasses.Count-1 do begin
    Info := TItemClassInfo(ItemClasses[I]);
    Item := TTBItem.Create(Self);
    Item.Caption := Info.Caption;
    Item.ImageIndex := GetItemClassImage(Info.ItemClass);
    Item.Tag := {$IFNDEF CLR}Integer{$ELSE}TTag{$ENDIF}(Info.ItemClass);
    Item.OnClick := MoreItemClick;
    MoreMenu.Add(Item);
  end;
end;

destructor TTBItemEditForm.Destroy;
begin
  inherited;
  if Assigned(FNotifyItemList) then begin
    UnregisterAllNotifications;
    FNotifyItemList.Free;
    FNotifyItemList := nil;
  end;
end;

procedure TTBItemEditForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TTBItemEditForm.FormActivate(Sender: TObject);
begin
  SetSelParentItem(FSelParentItem);
end;

procedure TTBItemEditForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and
     ((AComponent = FParentComponent) or (AComponent = FRootItem)) then
    { Must use Free instead of Close, since Close causes the freeing of the
      form to be delayed until the next message. We have to destroy the form
      immediately, otherwise Delphi will crash when Compile is clicked on the
      TB2k package. }
    Free;
  {}{temp:}
  (*if (Operation = opRemove) and (FNotifyItemList.IndexOf(AComponent) <> -1) then begin
    outputdebugstring(pchar('Still in list: ' + AComponent.name));
    //beep;
  end;*)
end;

function TTBItemEditForm.UniqueName(Component: TComponent): String;
begin
  Result := Designer.UniqueName(Component.ClassName);
end;

function TTBItemEditForm.GetEditState: TEditState;
begin
  Result := [];
  if ActiveControl = ListView then begin
    if Assigned(ListView.Selected) then
      Result := [esCanDelete, esCanCut, esCanCopy];
    if ClipboardComponents then
      Include(Result, esCanPaste);
  end;
end;

{$IFDEF JR_D6}
function TTBItemEditForm.EditAction(Action: TEditAction): Boolean;
{$ELSE}
procedure TTBItemEditForm.EditAction(Action: TEditAction);
{$ENDIF}
begin
  {$IFDEF JR_D6}
  Result := True;
  {$ENDIF}
  case Action of
    eaCut: Cut;
    eaCopy: Copy;
    eaPaste: Paste;
    eaDelete: Delete;
  {$IFDEF JR_D6}
  else
    Result := False;
  {$ENDIF}
  end;
end;

procedure TTBItemEditForm.UnregisterAllNotifications;
var
  I: Integer;
begin
  for I := FNotifyItemList.Count-1 downto 0 do begin
    //outputdebugstring(pchar('Unregall: ' + TTBCustomItem(FNotifyItemList[I]).name));
    TTBCustomItem(FNotifyItemList[I]).UnregisterNotification(ItemNotification);
    FNotifyItemList.Delete(I);
  end;
end;

procedure TTBItemEditForm.ItemNotification(Ancestor: TTBCustomItem;
  Relayed: Boolean; Action: TTBItemChangedAction; Index: Integer;
  Item: TTBCustomItem);
var
  ListItem: TListItem;
  TreeNode: TTreeNode;
  I: Integer;
  C: String;
begin
  { Manipulate the list view when items are inserted, deleted, or their Caption
    changes }
  case Action of
    tbicInserted:
      begin
        if (Ancestor = FSelParentItem) and not Relayed then
          AddListViewItem(Index, Item);
        if IsSubmenuItem(Item) then
          RebuildTree;
      end;
    tbicDeleting:
      if (Ancestor = FSelParentItem) and not Relayed then begin
        ListItem := ListView.FindData(0, Item, True, False);
        if Assigned(ListItem) then
          ListItem.Delete;
      end;
    tbicInvalidateAndResize:
      if (Ancestor = FSelParentItem) and not Relayed then begin
        ListItem := ListView.FindData(0, Item, True, False);
        if Assigned(ListItem) and (ListItem.Caption <> TTBCustomItem(Item).Caption) then
          ListItem.Caption := TTBCustomItem(Item).Caption;
      end;
  end;
  { Update tree view when an item is deleted, or a Caption changes }
  if Action = tbicDeleting then begin
    I := FNotifyItemList.IndexOf(Item);
    if I <> -1 then begin
      //outputdebugstring(pchar('Deleting, so unreging: ' + item.name));
      TTBCustomItem(Item).UnregisterNotification(ItemNotification);
      FNotifyItemList.Delete(I);
    end;
  end;
  if Action in [tbicDeleting, tbicInvalidateAndResize, tbicNameChanged] then begin
    TreeNode := TreeView.Items.GetFirstNode;
    while Assigned(TreeNode) do begin
      if TreeNode.Data = Item then begin
        if Action = tbicDeleting then begin
          TreeNode.Delete;
          if FSelParentItem = Item then
            SetSelParentItem(TTBCustomItem(Item).Parent);
        end
        else begin
          { tbicInvalidateAndResize, tbicNameChanged: }
          C := GetItemTreeCaption(Item);
          if TreeNode.Text <> C then
            TreeNode.Text := C;
        end;
        Break;
      end;
      TreeNode := TreeNode.GetNext;
    end;
  end;
end;

function TTBItemEditForm.GetItemTreeCaption(AItem: TTBCustomItem): String;
begin
  if AItem <> FRootItem then begin
    Result := AItem.Caption;
    if Result = '' then
      Result := '[' + AItem.Name + ']';
  end
  else
    Result := '(Root)';
end;

procedure TTBItemEditForm.RebuildTree;

  procedure Recurse(const AParentItem: TTBCustomItem; const ATreeNode: TTreeNode;
    var FoundSelParentItem: TTreeNode);
  var
    I: Integer;
    NewNode: TTreeNode;
    ChildItem: TTBCustomItem;
  begin
    {}AParentItem.FreeNotification(Self);
    AParentItem.RegisterNotification(ItemNotification);
    FNotifyItemList.Add(AParentItem);
    NewNode := TreeView.Items.AddChild(ATreeNode, GetItemTreeCaption(AParentItem));
    NewNode.Data := AParentItem;
    if AParentItem = FSelParentItem then
      FoundSelParentItem := NewNode;
    for I := 0 to AParentItem.Count-1 do begin
      ChildItem := AParentItem[I];
      if IsSubmenuItem(ChildItem) then
        Recurse(ChildItem, NewNode, FoundSelParentItem);
    end;
  end;

var
  FoundSelParentItem: TTreeNode;
begin
  Inc(FRebuildingTree);
  try
    TreeView.Items.BeginUpdate;
    try
      TreeView.Items.Clear;
      UnregisterAllNotifications;
      FoundSelParentItem := nil;
      Recurse(FRootItem, nil, FoundSelParentItem);
      if FoundSelParentItem = nil then
        SetSelParentItem(FRootItem)
      else
        TreeView.Selected := FoundSelParentItem;
      TreeView.Items[0].Expand(True);
    finally
      TreeView.Items.EndUpdate;
    end;
  finally
    Dec(FRebuildingTree);
  end;
end;

function TTBItemEditForm.AddListViewItem(const Index: Integer;
  const Item: TTBCustomItem): TListItem;
begin
  Result := ListView.Items.Insert(Index);
  Result.Data := Item;
  if not(Item is TTBControlItem) then begin
    Result.Caption := Item.Caption;
    Result.Subitems.Add(Item.ClassName);
    Result.ImageIndex := GetItemClassImage(TTBCustomItemClass(Item.ClassType));
  end
  else begin
    Result.Caption := '(Control)';
    Result.Subitems.Add(Item.ClassName);
    Result.ImageIndex := -1;
  end;
end;

procedure TTBItemEditForm.RebuildList;
var
  ChildItem: TTBCustomItem;
  I: Integer;
begin
  Inc(FRebuildingList);
  try
    ListView.Items.BeginUpdate;
    try
      ListView.Items.Clear;
      if Assigned(FSelParentItem) then begin
        for I := 0 to FSelParentItem.Count-1 do begin
          ChildItem := FSelParentItem[I];
          { Check for csDestroying because deleting an item in the tree view
            causes the parent item to be selected, and the parent item won't
            get a notification that the item is deleting since notifications
            were already sent }
          if not(csDestroying in ChildItem.ComponentState) then
            AddListViewItem(I, ChildItem);
        end;
        { Add an empty item to the end }
        ListView.Items.Add.ImageIndex := -1;
      end;
    finally
      ListView.Items.EndUpdate;
    end;
    { Work around a strange TListView bug(?). Without this, the column header
      isn't painted properly. }
    if HandleAllocated then
      SetWindowPos(ListView.Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or
        SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
  finally
    Dec(FRebuildingList);
  end;
end;

procedure TTBItemEditForm.SelectInObjectInspector(AList: TList);
var
  CompList1, CompList2: TDesignerSelectionList;
  I: Integer;
  C: TComponent;
begin
  { Designer.SetSelections will make components appear selected on the form.
    It will also select the component in Object Inspector, but only if the
    form has the focus. TDesignWindow.SetSelection will select the component
    in Object Inspector regardless of whether the form has the focus. }
  CompList1 := CreateSelectionList;
  CompList2 := CreateSelectionList;
  for I := 0 to AList.Count-1 do begin
    C := TComponent(AList[I]);
    { Must check for csDestroying. If SetSelection is passed a component that's
      destroying, Delphi will crash. }
    if not(csDestroying in C.ComponentState) then begin
      CompList1.Add(C);
      CompList2.Add(C);
    end;
  end;
  if CompList1.Count = 0 then begin
    {$IFNDEF JR_D6}
    CompList1.Free;
    CompList2.Free;
    {$ENDIF}
  end
  else begin
    Designer.SetSelections(CompList1);
    { Note: Never pass an empty list to SetSelection or Delphi will crash }
    { History here:
      - 1.34: SetSelection call remarked out because it fixed Delphi 6 issue
        with random AV's after the editor was closed.
      - 1.38: SetSelection call restored because without it, Ctrl+X/C/V didn't
        work.
      - 1.40: SetSelection call disabled on Delphi 6 only because AV problem
        still seems to exist despite another change which I thought fixed it.
        On D6 it isn't necessary to call SetSelection for Ctrl+X/C/V to work.
        Note: Using "ComponentDesigner.SetSelection(Designer, nil, CompList2);"
        instead seems to fix the AV problem, but for consistency with Delphi's
        TMainMenu editor (which only selects items when its parent form is
        focused), I decided not to call SetSelection at all on D6.
    }
    {$IFNDEF JR_D6}
    SetSelection(CompList2);
    {$ENDIF}
  end;
end;

procedure TTBItemEditForm.GetSelItemList(const AList: TList);
var
  ListItem: TListItem;
begin
  ListItem := nil;
  while True do begin
    ListItem := ListView.GetNextItem(ListItem, sdAll, [isSelected]);
    if ListItem = nil then
      Break;
    if Assigned(ListItem.Data) then
      AList.Add(ListItem.Data);
  end;
end;

procedure TTBItemEditForm.SetSelParentItem(ASelParentItem: TTBCustomItem);
{ - Rebuilds the list view to match a new selection (ASelParentItem) in the
    tree view
  - Updates toolbar
  - Selects selected item(s) into Object Inspector }
var
  I: Integer;
  TreeNode: TTreeNode;
  ItemIsSelected: Boolean;
  List: TList;
begin
  if FSettingSel > 0 then
    Exit;
  List := TList.Create;
  Inc(FSettingSel);
  try
    if FSelParentItem <> ASelParentItem then begin
      FSelParentItem := ASelParentItem;
      NewSubmenuButton.Enabled := Assigned(ASelParentItem);
      NewItemButton.Enabled := Assigned(ASelParentItem);
      NewSepButton.Enabled := Assigned(ASelParentItem);
      for I := 0 to MoreMenu.Count-1 do
        MoreMenu[I].Enabled := Assigned(ASelParentItem);
      if not Assigned(TreeView.Selected) or (TreeView.Selected.Data <> FSelParentItem) then begin
        if FSelParentItem = nil then
          TreeView.Selected := nil
        else begin
          TreeNode := TreeView.Items.GetFirstNode;
          while Assigned(TreeNode) do begin
            if TreeNode.Data = FSelParentItem then begin
              TreeView.Selected := TreeNode;
              Break;
            end;
            TreeNode := TreeNode.GetNext;
          end;
        end;
      end;
      RebuildList;
    end;

    ItemIsSelected := (ActiveControl = ListView) and Assigned(ListView.Selected) and
      Assigned(ListView.Selected.Data);
    if ItemIsSelected then
      GetSelItemList(List);

    CutButton.Enabled := ItemIsSelected;
    CopyButton.Enabled := ItemIsSelected;
    PasteButton.Enabled := (ActiveControl = ListView);
    DeleteButton.Enabled := ItemIsSelected or
      ((ActiveControl = TreeView) and (FSelParentItem <> FRootItem));
    MoveUpButton.Enabled := ItemIsSelected and
      (FSelParentItem.IndexOf(TTBCustomItem(List.First)) > 0);
    MoveDownButton.Enabled := ItemIsSelected and
      (FSelParentItem.IndexOf(TTBCustomItem(List.Last)) < FSelParentItem.Count-1);

    if ActiveControl = ListView then begin
      if List.Count = 0 then
        { No item was selected, or the blank item was selected.
          Select the root item so it looks like no item was selected in
          Object Inspector }
        List.Add(FRootItem);
    end
    else if not Assigned(ASelParentItem) or (ASelParentItem = FRootItem) then
      List.Add(FParentComponent)
    else
      List.Add(ASelParentItem);
    SelectInObjectInspector(List);
  finally
    Dec(FSettingSel);
    List.Free;
  end;
end;

procedure TTBItemEditForm.Cut;
begin
  Copy;
  Delete;
end;

procedure TTBItemEditForm.Copy;
var
  SelList: TList;
  CompList: TDesignerSelectionList;
  I: Integer;
  Item: TTBCustomItem;
begin
  if ListView.Selected = nil then Exit;
  CompList := nil;
  SelList := TList.Create;
  try
    GetSelItemList(SelList);
    CompList := CreateSelectionList;
    for I := 0 to SelList.Count-1 do begin
      Item := TTBCustomItem(SelList[I]);
      if Item is TTBControlItem then
        raise EInvalidOperation.Create('Cannot cut or copy TTBControlItems');
      CompList.Add(Item);
    end;
    CopyComponents(FParentComponent.Owner, CompList);
  finally
    {$IFNDEF JR_D6}
    CompList.Free;
    {$ENDIF}
    SelList.Free;
  end;
end;

procedure TTBItemEditForm.Paste;
var
  CompList: TDesignerSelectionList;
begin
  if FSelParentItem = nil then Exit;
  CompList := CreateSelectionList;
  try
    PasteComponents(FParentComponent.Owner, FSelParentItem, CompList);
    if CompList.Count <> 0 then
      Designer.Modified;
  finally
    {$IFNDEF JR_D6}
    CompList.Free;
    {$ENDIF}
  end;
end;

procedure TTBItemEditForm.DeleteItem(const Item: TTBCustomItem);
begin
  if csAncestor in Item.ComponentState then
    raise EInvalidOperation.Create('Items introduced in an ancestor form cannot be deleted');
  //Designer.ValidateRename(Item, Item.Name, '');
  Item.Free;
  Designer.Modified;
end;

procedure TTBItemEditForm.Delete;
var
  List: TList;
  Item: TTBCustomItem;
  ListItem: TListItem;
begin
  List := TList.Create;
  try
    List.Add(FSelParentItem);
    SelectInObjectInspector(List);
  finally
    List.Free;
  end;
  FSelParentItem.ViewBeginUpdate;
  try
    while Assigned(ListView.Selected) do begin
      Item := TTBCustomItem(ListView.Selected.Data);
      if Item = nil then
        Break;
      DeleteItem(Item);
    end;
  finally
    FSelParentItem.ViewEndUpdate;
  end;
  { After deleting the items, select the item with the focus }
  ListItem := ListView.GetNextItem(nil, sdAll, [isFocused]);
  if Assigned(ListItem) then
    ListItem.Selected := True;
end;

procedure TTBItemEditForm.MoveItem(CurIndex, NewIndex: Integer);
var
  WasFocused: Boolean;
begin
  WasFocused := ListView.Items[CurIndex].Focused;

  FSelParentItem.Move(CurIndex, NewIndex);
  Designer.Modified;

  if WasFocused then
    ListView.Items[NewIndex].Focused := True;
  ListView.Items[NewIndex].Selected := True;
end;

procedure TTBItemEditForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
var
  NewSelectedParentItem: TTBCustomItem;
begin
  if (FRebuildingTree > 0) or (FSettingSel > 0) then Exit;
  if Node = nil then
    NewSelectedParentItem := nil
  else
    NewSelectedParentItem := TTBCustomItem(Node.Data);
  SetSelParentItem(NewSelectedParentItem);
end;

procedure TTBItemEditForm.TreeViewEnter(Sender: TObject);
{ When the tree view gets the focus, act as if the currently selected item
  was clicked. }
begin
  ListView.Selected := nil;
  SetSelParentItem(FSelParentItem);
end;

procedure TTBItemEditForm.CMDeferUpdate(var Message: TMessage);
begin
  SetSelParentItem(FSelParentItem);
end;

procedure TTBItemEditForm.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  Msg: TMsg;
begin
  if (FRebuildingList > 0) or (FSettingSel > 0) or (Change <> ctState) or
     (csDestroying in ListView.ComponentState) then
    Exit;
  if not PeekMessage(Msg, Handle, CM_DEFERUPDATE, CM_DEFERUPDATE,
     PM_NOREMOVE or PM_NOYIELD) then
    PostMessage(Handle, CM_DEFERUPDATE, 0, 0);
end;

procedure TTBItemEditForm.ListViewEnter(Sender: TObject);
begin
  { When list view gets the focus, update the toolbar }
  SetSelParentItem(FSelParentItem);
end;

procedure TTBItemEditForm.ListViewDblClick(Sender: TObject);
var
  SelItem: TTBCustomItem;
  PropCount, I: Integer;
  Props: {$IFNDEF CLR} PPropList {$ELSE} TPropList {$ENDIF};
  PropInfo: {$IFNDEF CLR} PPropInfo {$ELSE} TPropInfo {$ENDIF};
  MethodName: String;
  Method: TMethod;
begin
  SelItem := nil;
  if Assigned(ListView.Selected) then
    SelItem := TTBCustomItem(ListView.Selected.Data);
  if SelItem = nil then Exit;
  if IsSubmenuItem(SelItem) then begin
    SetSelParentItem(SelItem);
    Exit;
  end;
  {$IFNDEF CLR}
  PropCount := GetPropList(SelItem.ClassInfo, [tkMethod], nil);
  GetMem(Props, PropCount * SizeOf(PPropInfo));
  try
    GetPropList(SelItem.ClassInfo, [tkMethod], Props);
  {$ELSE}
    Props := GetPropList(SelItem.ClassInfo, [tkMethod]);
    PropCount := Length(Props);
  {$ENDIF}
    for I := PropCount-1 downto 0 do begin
      PropInfo := Props[I];
      {$IFNDEF CLR}
      if CompareText(String(PropInfo.Name), 'OnClick') = 0 then begin
      {$ELSE}
      if SameText(PropInfo.Name, 'OnClick', loInvariantLocale) then begin
      {$ENDIF}
        Method := GetMethodProp(SelItem, PropInfo);
        MethodName := Designer.GetMethodName(Method);
        if MethodName = '' then begin
          MethodName := SelItem.Name + 'Click';
          Method := Designer.CreateMethod(MethodName, GetTypeData(
            {$IFNDEF CLR} PropInfo.PropType^ {$ELSE} PropInfo.TypeInfo {$ENDIF}));
          SetMethodProp(SelItem, PropInfo, Method);
          Designer.Modified;
        end;
        if Designer.MethodExists(MethodName) then
          Designer.ShowMethod(MethodName);
        Break;
      end;
    end;
  {$IFNDEF CLR}
  finally
    FreeMem(Props);
  end;
  {$ENDIF}
end;

procedure TTBItemEditForm.ListViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN: begin
        Key := 0;
        ActivateInspector(#0);
      end;
    VK_INSERT: begin
        Key := 0;
        if ssCtrl in Shift then
          NewSubmenuButtonClick(Sender)
        else
          NewItemButtonClick(Sender);
      end;
    VK_DELETE: begin
        Key := 0;
        Delete;
      end;
  end;
end;

procedure TTBItemEditForm.TreeViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN: begin
        Key := 0;
        ActivateInspector(#0);
      end;
    VK_DELETE: begin
        Key := 0;
        DeleteButtonClick(Sender);
      end;
  end;
end;

procedure TTBItemEditForm.TreeViewKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key >= #33) and (Key <= #126) then begin
    ActivateInspector(Key);
    Key := #0;
  end
  else if Key = #13 then
    Key := #0;  { suppress beep }
end;

procedure TTBItemEditForm.ListViewKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = '-' then begin
    NewSepButtonClick(Sender);
    Key := #0;
  end
  else if (Key >= #33) and (Key <= #126) then begin
    ActivateInspector(Key);
    Key := #0;
  end;
end;

procedure TTBItemEditForm.ListViewDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
{ List item dragged over the list view }
var
  Item: TListItem;
begin
  Accept := False;
  if (Sender = ListView) and (Source = ListView) and (ListView.SelCount = 1) then begin
    Item := ListView.GetItemAt(X, Y);
    if Assigned(Item) and (Item <> ListView.Selected) then
      Accept := True;
  end;
end;

procedure TTBItemEditForm.ListViewDragDrop(Sender, Source: TObject; X,
  Y: Integer);
{ List item dropped onto another list item }
var
  ListItem: TListItem;
  Item: TTBCustomItem;
  NewIndex: Integer;
begin
  if (Sender = ListView) and (Source = ListView) and (ListView.SelCount = 1) then begin
    ListItem := ListView.GetItemAt(X, Y);
    if Assigned(ListItem) and (ListItem <> ListView.Selected) and Assigned(FSelParentItem) then begin
      NewIndex := FSelParentItem.IndexOf(TTBCustomItem(ListItem.Data));
      if NewIndex <> -1 then begin
        ListView.Items.BeginUpdate;
        { For good performance and to prevent Object Inspector flicker, increment
          FSettingSel to prevent calls to SetSelParentItem while moving items }
        Inc(FSettingSel);
        try
          Item := TTBCustomItem(ListView.Selected.Data);
          MoveItem(FSelParentItem.IndexOf(Item), NewIndex);
        finally
          Dec(FSettingSel);
          ListView.Items.EndUpdate;
        end;
        { After decrementing FSettingSel, now call SetSelParentItem, to update
          the toolbar buttons }
        SetSelParentItem(FSelParentItem);
      end;
    end;
  end;
end;

function TTBItemEditForm.TreeViewDragHandler(Sender, Source: TObject;
  X, Y: Integer; Drop: Boolean): Boolean;
var
  Node: TTreeNode;
  ListItem: TListItem;
  Item, NewParentItem: TTBCustomItem;
  ItemList: TList;
  I: Integer;
  NeedRebuildTree: Boolean;
begin
  Result := False;
  if (Sender = TreeView) and (Source = ListView) then begin
    Node := TreeView.GetNodeAt(X, Y);
    if Assigned(Node) and (Node <> TreeView.Selected) then begin
      NewParentItem := TTBCustomItem(Node.Data);
      ItemList := TList.Create;
      try
        ListItem := nil;
        while True do begin
          ListItem := ListView.GetNextItem(ListItem, sdAll, [isSelected]);
          if ListItem = nil then
            Break;
          Item := TTBCustomItem(ListItem.Data);
          if Assigned(Item) and (Item <> NewParentItem) and
             not Item.ContainsItem(NewParentItem) and
             not(Item is TTBControlItem) then begin
            Result := True;
            ItemList.Add(Item);
          end;
        end;
        if Drop then begin
          NeedRebuildTree := False;
          for I := 0 to ItemList.Count-1 do begin
            Item := TTBCustomItem(ItemList[I]);
            Item.Parent.Remove(Item);
            NewParentItem.Add(Item);
            Designer.Modified;
            if IsSubmenuItem(Item) then
              NeedRebuildTree := True;
          end;
          if NeedRebuildTree then
            RebuildTree;
        end;
      finally
        ItemList.Free;
      end;
    end;
  end;
end;

procedure TTBItemEditForm.TreeViewDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
{ List item dragged over the tree view }
begin
  Accept := TreeViewDragHandler(Sender, Source, X, Y, False);
end;

procedure TTBItemEditForm.TreeViewDragDrop(Sender, Source: TObject; X,
  Y: Integer);
{ List item dropped onto the tree view }
begin
  TreeViewDragHandler(Sender, Source, X, Y, True);
end;

procedure TTBItemEditForm.CreateNewItem(const AClass: TTBCustomItemClass);
var
  NewIndex: Integer;
  NewItem: TTBCustomItem;
  ListItem: TListItem;
begin
  if FSelParentItem = nil then Exit;
  NewIndex := -1;
  if (GetKeyState(VK_SHIFT) >= 0) and Assigned(ListView.Selected) then
    NewIndex := FSelParentItem.IndexOf(TTBCustomItem(ListView.Selected.Data));
  if NewIndex = -1 then
    NewIndex := FSelParentItem.Count;
  NewItem := AClass.Create(FParentComponent.Owner{Designer.Form});
  try
    NewItem.Name := Designer.UniqueName(NewItem.ClassName);
    FSelParentItem.Insert(NewIndex, NewItem);
  except
    NewItem.Free;
    raise;
  end;
  Designer.Modified;
  ListView.Selected := nil;
  ListItem := ListView.FindData(0, NewItem, True, False);
  if Assigned(ListItem) then begin
    ListItem.Selected := True;
    ListItem.Focused := True;
    ListItem.MakeVisible(False);
    ListView.SetFocus;
  end;
end;

procedure TTBItemEditForm.NewSubmenuButtonClick(Sender: TObject);
begin
  CreateNewItem(TTBSubmenuItem);
end;

procedure TTBItemEditForm.NewItemButtonClick(Sender: TObject);
begin
  CreateNewItem(TTBItem);
end;

procedure TTBItemEditForm.NewSepButtonClick(Sender: TObject);
begin
  CreateNewItem(TTBSeparatorItem);
end;

procedure TTBItemEditForm.MoreItemClick(Sender: TObject);
begin
  CreateNewItem(TTBCustomItemClass((Sender as TTBItem).Tag));
end;

procedure TTBItemEditForm.CutButtonClick(Sender: TObject);
begin
  Cut;
end;

procedure TTBItemEditForm.CopyButtonClick(Sender: TObject);
begin
  Copy;
end;

procedure TTBItemEditForm.PasteButtonClick(Sender: TObject);
begin
  Paste;
end;

procedure TTBItemEditForm.DeleteButtonClick(Sender: TObject);
begin
  if ActiveControl = ListView then
    Delete
  else if (ActiveControl = TreeView) and (FSelParentItem <> FRootItem) then
    DeleteItem(FSelParentItem);
end;

procedure TTBItemEditForm.MoveUpButtonClick(Sender: TObject);
var
  SelList: TList;
  I, J: Integer;
  Item: TTBCustomItem;
  ListItem: TListItem;
begin
  if FSelParentItem = nil then Exit;
  SelList := TList.Create;
  try
    GetSelItemList(SelList);
    if SelList.Count = 0 then Exit;

    ListView.Items.BeginUpdate;
    FSelParentItem.ViewBeginUpdate;
    { For good performance and to prevent Object Inspector flicker, increment
      FSettingSel to prevent calls to SetSelParentItem while moving items }
    Inc(FSettingSel);
    try
      for I := 0 to SelList.Count-1 do begin
        Item := TTBCustomItem(SelList[I]);
        J := FSelParentItem.IndexOf(Item);
        if J <> -1 then
          MoveItem(J, J-1);
      end;
      ListItem := ListView.FindData(0, SelList[0], True, False);
      if Assigned(ListItem) then
        ListItem.MakeVisible(False);
    finally
      Dec(FSettingSel);
      FSelParentItem.ViewEndUpdate;
      ListView.Items.EndUpdate;
    end;
    { After decrementing FSettingSel, now call SetSelParentItem, to update
      the toolbar buttons }
    SetSelParentItem(FSelParentItem);
  finally
    SelList.Free;
  end;
end;

procedure TTBItemEditForm.MoveDownButtonClick(Sender: TObject);
var
  SelList: TList;
  I, J: Integer;
  Item: TTBCustomItem;
  ListItem: TListItem;
begin
  if FSelParentItem = nil then Exit;
  SelList := TList.Create;
  try
    GetSelItemList(SelList);
    if SelList.Count = 0 then Exit;

    ListView.Items.BeginUpdate;
    FSelParentItem.ViewBeginUpdate;
    { For good performance and to prevent Object Inspector flicker, increment
      FSettingSel to prevent calls to SetSelParentItem while moving items }
    Inc(FSettingSel);
    try
      for I := SelList.Count-1 downto 0 do begin
        Item := TTBCustomItem(SelList[I]);
        J := FSelParentItem.IndexOf(Item);
        if J <> -1 then
          MoveItem(J, J+1);
      end;
      ListItem := ListView.FindData(0, SelList[SelList.Count-1], True, False);
      if Assigned(ListItem) then
        ListItem.MakeVisible(False);
    finally
      Dec(FSettingSel);
      FSelParentItem.ViewEndUpdate;
      ListView.Items.EndUpdate;
    end;
    { After decrementing FSettingSel, now call SetSelParentItem, to update
      the toolbar buttons }
    SetSelParentItem(FSelParentItem);
  finally
    SelList.Free;
  end;
end;

procedure TTBItemEditForm.TConvertMenuClick(Sender: TObject);
begin
  if FSelParentItem = nil then Exit;
  DoConvert(FSelParentItem, FParentComponent.Owner);
end;


{ TTBItemsEditor }

procedure TTBItemsEditor.Edit;
var
  Item: TTBCustomItem;
begin
  if Assigned(Component) then begin
    Item := TBGetItems(Component);
    if Assigned(Item) then
      ShowEditForm(Component, Item, Designer);
  end;
end;

procedure TTBItemsEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
    1: ShowVersion;
  end;
end;

function TTBItemsEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TTBItemsEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Edit...';
    1: Result := 'Version...';
  else
    Result := '';
  end;
end;


{ TTBItemsPropertyEditor }

procedure TTBItemsPropertyEditor.Edit;
var
  Editor: {$IFDEF JR_D6} IComponentEditor {$ELSE} TComponentEditor {$ENDIF};
begin
  if PropCount <> 1 then Exit;
  Editor := GetComponentEditor(GetComponent(0) as TComponent, Designer);
  try
    Editor.Edit;
  finally
    {$IFNDEF JR_D6}
    Editor.Free;
    {$ENDIF}
  end;
end;

function TTBItemsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly];
end;

function TTBItemsPropertyEditor.GetValue: String;
begin
  Result := '(TB2000 Items)';
end;

initialization
  ItemImageList := TImageList.Create(nil);
  {$IFNDEF CLR}
  ItemImageList.Handle := ImageList_LoadImage(HInstance, 'TB2_DSGNEDITORIMAGES',
    16, 0, clFuchsia, IMAGE_BITMAP, 0);
  {$ELSE}
  { Initialize the image list as plain ILC_COLOR (4-bit), because on Windows
    2000, at color depths > 16 (what TImageList's ILC_COLORDDB would give us
    when running on a true-color display), selected images are drawn with an
    ugly dithering effect }
  ItemImageList.Handle := ImageList_Create(16, 16, ILC_COLOR or ILC_MASK, 4, 4);
  LoadItemImage(Assembly.GetExecutingAssembly, 'TB2DsgnEditorImages.bmp');
  {$ENDIF}
  ItemClasses := TList.Create;
  {$IFNDEF CLR}
  AddModuleUnloadProc(UnregisterModuleItemClasses);
  {$ENDIF}
finalization
  {$IFNDEF CLR}
  RemoveModuleUnloadProc(UnregisterModuleItemClasses);
  {$ENDIF}
  FreeItemClasses;
  FreeAndNil(ItemImageList);
end.
