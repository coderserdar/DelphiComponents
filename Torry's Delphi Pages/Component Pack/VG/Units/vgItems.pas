{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         TItemListEditor                               }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgItems;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DesignIntf, DesignEditors, {$IFDEF _D5_}Contnrs,{$ENDIF} vgTools, Menus,
   Explorer, ComCtrls, vgCtrls, ExplCtrl, DesignWindows;

type
{$IFNDEF _D4_}
  IDesigner = TDesigner;
  IFormDesigner = TFormDesigner;
{$ENDIF}
  IFormDesigner = IDesigner;
  TDesignerSelectionList=IDesignerSelections;//TComponentList;

{ TItemsEditorForm }
  TItemsEditorForm = class(TDesignWindow)
    puMenu: TPopupMenu;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    miDelete: TMenuItem;
    miLine2: TMenuItem;
    miSelectAll: TMenuItem;
    lvItems: TExplorerListView;
    esItems: TExplorerSource;
    enRoot: TExplorerRootNode;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CutClick(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure PasteClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
    procedure lvItemsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FItemList: TItemList;
    FLocked: Boolean;
    procedure DoVerbClick(Sender: TObject);
    procedure SetItemList(Value: TItemList);
    procedure UpdateData;
    procedure UpdateStates;
    procedure UpdateSelected;
    procedure UpdateSelection(ASelection: {$IFDEF _D5_}TDesignerSelectionList{$ELSE}TComponentList{$ENDIF});
  protected
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Delete;
    procedure GetSelection(CompList: {$IFDEF _D5_}TDesignerSelectionList{$ELSE}TComponentList{$ENDIF});
    procedure Activated; override;
    function UniqueName(Component: TComponent): string; override;
  public
    { Public declarations }
    procedure ExecuteVerb(Index: Integer); virtual;
    procedure GetVerb(Index: Integer; var Caption: TCaption; var ShortCut: TShortCut); virtual;
    function GetVerbCount: Integer; virtual;
    procedure ComponentDeleted(AComponent: {$IFNDEF _D4_}IPersistent{$ELSE}TComponent{$ENDIF});
    procedure FormClosed(Form: TCustomForm);
    procedure FormModified;
    function GetEditState: TEditState; override;
    function EditAction(Action: TEditAction):Boolean; override;
    procedure SelectionChanged(const ADesigner:IDesigner; const ASelection: IDesignerSelections); override;
    property ItemList: TItemList read FItemList write SetItemList;
  end;

  TItemsEditorFormClass = class of TItemsEditorForm;

{ TItemsEditor }
  TItemsEditor = class(TComponentEditor)
  public
    function GetFormClass: TItemsEditorFormClass; virtual;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TExplorerItemNode }
  TExplorerItemNode = class(TExplorerInterfaceNode)
  private
    FItem: TItem;
    procedure SetItem(Value: TItem);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean; override;
    procedure DragDrop(List: TExplorerNodesList); override;
    property Item: TItem read FItem write SetItem;
  end;

implementation
uses vgUtils;

{$R *.DFM}

function FindItemsEditorForm(Component: TComponent): TItemsEditorForm;
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
    if (Screen.Forms[I] is TItemsEditorForm) and
      ((Screen.Forms[I] as TItemsEditorForm).ItemList = Component) then
    begin
      Result := TItemsEditorForm(Screen.Forms[I]);
      Exit;
    end;
  Result := nil;
end;

procedure ShowItemsEditorForm(Designer: IFormDesigner; Component: TComponent; FormClass: TItemsEditorFormClass);
var
  Form: TItemsEditorForm;
begin
  Form := FindItemsEditorForm(Component);

  if not Assigned(Form) then
  begin
    Form := FormClass.Create(Application);
    try
      Form.Designer := Designer;
      Form.ItemList := Component as TItemList;
    except
      Form.Free;
      raise;
    end;
  end;
  Form.Show;
  if Form.WindowState = wsMinimized then
    Form.WindowState := wsNormal;
end;

{ TItemsEditor }
procedure TItemsEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowItemsEditorForm(Self.Designer, Component, GetFormClass);
  end;
end;

function TItemsEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Items...';
  end;
end;

function TItemsEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TItemsEditor.GetFormClass: TItemsEditorFormClass;
begin
  Result := TItemsEditorForm;
end;

function Compare(Item1, Item2: Pointer): Integer;
var
  I1, I2: Integer;
begin
  I1 := TExplorerItemNode(Item1).Item.Index;
  I2 := TExplorerItemNode(Item2).Item.Index;
  if I1 > I2 then Result := 1 else if I1 < I2 then Result := -1 else Result := 0;
end;

{ TExplorerItemNode }
constructor TExplorerItemNode.Create(AOwner: TComponent);
begin
  inherited;
  SubItems.Add('');
  EnableDrag := True;
  EnableDrop := True;
end;

destructor TExplorerItemNode.Destroy;
begin
  SetItem(nil);
  inherited;
end;

procedure TExplorerItemNode.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FItem) then
  begin
    SetItem(nil);
    Destroy;
  end;
end;

function TExplorerItemNode.AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean;
begin
  Result := (ExplorerNodes is TExplorerItemNode) and (ExplorerNodes.Parent = Self.Parent);
end;

procedure TExplorerItemNode.DragDrop(List: TExplorerNodesList);
var
  I: Integer;
begin
  with (Owner as TItemsEditorForm) do
  begin
    FLocked := True;
    try
      for I := 0 to List.Count - 1 do
        TExplorerItemNode(List[I]).Item.Index := Self.Item.Index;
      Self.Parent.Sort(@Compare);
      Designer.Modified;
    finally
      FLocked := False;
    end;
  end;
end;

procedure TExplorerItemNode.SetItem(Value: TItem);
begin
  if (FItem <> Value) then
  begin
    BeginUpdate;
    try
      if Assigned(FItem) then
      begin
        Text := '';
        SubItems[0] := FItem.ClassName;
      end;
      FItem := Value;
      if Assigned(FItem) then
      begin
        Text := FItem.Name;
        SubItems[0] := FItem.ClassName;
        FreeNotification(FItem);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

{ TItemsEditorForm }
procedure TItemsEditorForm.DoVerbClick(Sender: TObject);
begin
  ExecuteVerb(TComponent(Sender).Tag);
end;

procedure TItemsEditorForm.SetItemList(Value: TItemList);
begin
  if FItemList <> Value then
  begin
    lvItems.Items.BeginUpdate;
    try
      FItemList := Value;
      UpdateData;
    finally
      lvItems.Items.EndUpdate;
    end;
  end;
end;

procedure TItemsEditorForm.UpdateSelected;
var
  CompList: IDesignerSelections;
begin
  if Assigned(ItemList) and Active and not FLocked then
  begin
    FLocked := True;
    try
      CompList := TDesignerSelections.Create;
      GetSelection(CompList);
      UpdateSelection(CompList);
      SetSelection(CompList);
    finally
      FLocked := False;
    end;
  end;
end;

procedure TItemsEditorForm.Activated;
begin
  UpdateSelected;
end;

function TItemsEditorForm.UniqueName(Component: TComponent): string;
var
  Temp: string;
begin
  Result := '';
  if (Component <> nil) then Temp := Component.ClassName
  else Temp := TItem.ClassName;
  if (UpCase(Temp[1]) = 'T') and (Length(Temp) > 1) then
    System.Delete(Temp, 1, 1);
  Result := Designer.UniqueName(Temp);
end;

procedure TItemsEditorForm.FormClosed(Form: TCustomForm);
begin
  if Form = Designer.Root then
  begin
    ItemList := nil;
    Free;
  end;
end;

{$IFNDEF _D4_}
procedure TItemsEditorForm.ComponentDeleted(AComponent: IPersistent);
begin
  if ExtractPersistent(AComponent) = ItemList then
{$ELSE}
procedure TItemsEditorForm.ComponentDeleted(AComponent: TComponent);
begin
  if AComponent = ItemList then
{$ENDIF}
  begin
    ItemList := nil;
    Close;
  end;
end;

procedure TItemsEditorForm.FormModified;
begin
  if not FLocked and not (csDestroying in ComponentState) then UpdateData;
end;

function TItemsEditorForm.GetEditState: TEditState;
begin
  Result := [];
  if miDelete.Enabled then
    Result := [esCanDelete, esCanCut, esCanCopy];

  if miPaste.Enabled then
    Include(Result, esCanPaste);
end;

function TItemsEditorForm.EditAction(Action: TEditAction):boolean;
begin
  case Action of
    eaCut: Cut;
    eaCopy: Copy;
    eaPaste: Paste;
    eaDelete: Delete;
  end;
  Result:=true;
end;

procedure TItemsEditorForm.FormCreate(Sender: TObject);
var
  I, Verbs: Integer;
  Item: TMenuItem;
  Caption: TCaption;
  ShortCut: TShortCut;
begin
  Verbs := GetVerbCount;
  if Verbs > 0 then
  begin
    puMenu.Items.Insert(0, NewLine);
    for I := Verbs - 1 downto 0 do
    begin
      GetVerb(I, Caption, ShortCut);
      Item := NewItem(Caption, ShortCut, False, True, DoVerbClick, 0, '');
      Item.Tag := I;
      puMenu.Items.Insert(0, Item);
    end;
  end;
end;

procedure TItemsEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TItemsEditorForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then ActivateInspector(#0);
end;

procedure TItemsEditorForm.UpdateData;

  function FindNode(Item: TItem): TExplorerItemNode;
  var
    I: Integer;
  begin
    for I := 0 to enRoot.Count - 1 do
    begin
      Result := TExplorerItemNode(enRoot[I]);
      if Result.Item = Item then Exit;
    end;
    Result := nil;
  end;

var
  I: Integer;
  Created: Boolean;
  Item: TItem;
  Node: TExplorerItemNode;
begin
  if Assigned(ItemList) then
  begin
    Caption := Format('%s.%s', [Designer.Root.Name, ItemList.Name]);
    enRoot.BeginExpand;
    try
      for I := 0 to ItemList.Count - 1 do
      begin
        Item := ItemList[I];
        Node := FindNode(Item);

        Created := not Assigned(Node);
        try
          if Created then
          begin
            Created := True;
            Node := TExplorerItemNode.Create(Self);
            Node.Item := Item;
            Node.Parent := enRoot;
          end else
            Node.Text := Item.Name;
        except
          if Created then Node.Free;
          raise;
        end;
      end;
      enRoot.Sort(@Compare);
    finally
      enRoot.EndExpand;
    end;
  end else begin
    Caption := Format('%s.none', [Designer.Root.Name]);
    enRoot.Clear;
  end;
  UpdateStates;
end;

procedure TItemsEditorForm.UpdateStates;
begin
  miDelete.Enabled := Assigned(lvItems.Selected);
  miCopy.Enabled := miDelete.Enabled;
  miCut.Enabled := miDelete.Enabled;
  miPaste.Enabled := ClipboardComponents;
end;

procedure TItemsEditorForm.CutClick(Sender: TObject);
begin
  Cut;
  UpdateData;
end;

procedure TItemsEditorForm.CopyClick(Sender: TObject);
begin
  Copy;
  UpdateStates;
end;

procedure TItemsEditorForm.PasteClick(Sender: TObject);
begin
  Paste;
  UpdateData;
end;

procedure TItemsEditorForm.DeleteClick(Sender: TObject);
begin
  Delete;
  UpdateData;
end;

procedure TItemsEditorForm.SelectAllClick(Sender: TObject);
var
  I: Integer;
  CompList: {$IFDEF _D5_}IDesignerSelections{$ELSE}TComponentList{$ENDIF};
begin
  CompList := {$IFDEF _D5_}TDesignerSelections{$ELSE}TComponentList{$ENDIF}.Create;
  try
    for I := 0 to enRoot.Count - 1 do
     CompList.Add(TExplorerItemNode(enRoot[I]).Item);
    SetSelection(CompList);
  except
//    CompList.Free;
    raise;
  end;
end;

procedure TItemsEditorForm.GetSelection(CompList: TDesignerSelectionList);
var
  I: Integer;
  Item: TExplorerListItem;
begin
  if not Assigned(ItemList) then Exit;

  for I := 0 to lvItems.Items.Count - 1 do
  begin
    Item := TExplorerListItem(lvItems.Items[I]);
    if Item.Selected then
      CompList.Add(TExplorerItemNode(Item.ExplorerNodes).Item);
  end;
end;

procedure TItemsEditorForm.UpdateSelection(ASelection: {$IFDEF _D5_}TDesignerSelectionList{$ELSE}TComponentList{$ENDIF});
var
  I, J: Integer;
  Select: Boolean;
  Node: TExplorerItemNode;
begin
  for I := 0 to enRoot.Count - 1 do
  begin
    Node := TExplorerItemNode(enRoot[I]);
    Select := False;
    for J := 0 to ASelection.Count - 1 do
      if Node.Item = ASelection[J] then
      begin
        Select := True;
        Break;
      end;

    lvItems.Items[I].Selected := Select;
  end;
  UpdateStates;
end;

procedure TItemsEditorForm.SelectionChanged(const ADesigner:IDesigner; const ASelection: IDesignerSelections);
begin
  if FLocked then Exit;
  FLocked := True;
  try
    UpdateSelection(ASelection);
  finally
    FLocked := False;
  end;
end;

procedure TItemsEditorForm.Cut;
begin
  Copy;
  DeleteClick(nil);
end;

procedure TItemsEditorForm.Copy;
var
  CompList: IDesignerSelections;
begin
  CompList := TDesignerSelections.Create;
  try
    GetSelection(CompList);
    CopyComponents(Designer.Root, CompList);
  finally
//    CompList.Free;
  end;
end;

procedure TItemsEditorForm.Paste;
var
  CompList: IDesignerSelections;
begin
  CompList := TDesignerSelections.Create;
  try
    enRoot.BeginExpand;
    try
      PasteComponents(Designer.Root, ItemList, CompList);
    finally
      enRoot.EndExpand;
    end;
  except
//    CompList.Free;
    raise;
  end;
  SetSelection(CompList);
end;

procedure TItemsEditorForm.Delete;
var
  I: Integer;
  CompList: IDesignerSelections;
begin
  FLocked := True;
  try
    CompList := TDesignerSelections.Create;
    try
      lvItems.Items.BeginUpdate;
      try
        GetSelection(CompList);
        for I := 0 to CompList.Count - 1 do CompList[I].Free;
      finally
        lvItems.Items.EndUpdate;
      end;
    finally
//      CompList.Free;
    end;
    CompList := TDesignerSelections.Create;
    CompList.Add(ItemList);
    SetSelection(CompList);
  finally
    FLocked := False;
  end;
  Designer.Modified;
end;

procedure TItemsEditorForm.lvItemsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if Change = ctState then UpdateSelected;
end;

procedure TItemsEditorForm.ExecuteVerb(Index: Integer);
begin
end;

procedure TItemsEditorForm.GetVerb(Index: Integer; var Caption: TCaption; var ShortCut: TShortCut);
begin
end;

function TItemsEditorForm.GetVerbCount: Integer;
begin
  Result := 0;
end;

end.
