unit DTDBTreeView;

interface

{$I DTDBTree.Inc}

uses
{$IFNDEF TR_DELPHI5}
  Variants,
{$ENDIF}
  SysUtils, Messages, Classes, Controls, VirtualTrees, DB, Windows, Contnrs,
  Menus, ActiveX, ComCtrls, ImgList, Types;

type
  TDTNodeItem = class
  private
    FNode: PVirtualNode;
    FKey: String;
    FCaption: String;
    FParam: Variant;
    FFieldValues: Variant;
  public
    constructor Create(AKey: String; ANode: PVirtualNode);
    property Caption: String read FCaption write FCaption;
    property Key: String read FKey;
    property FieldValues: Variant read FFieldValues write FFieldValues;
    property Param: Variant read FParam write FParam;
  end;

  PNodeItem = ^TDTNodeItem;

  TDTNodeList = class(TList)
  private
    function CompareIDs(ID1, ID2: String): Integer;
    function GetNodes(Index: Integer): PVirtualNode;
    function GetIDs(Index: Integer): String;
    function FindNewIndex(AID: String): Integer;
    procedure ClearItems;
  public
    destructor Destroy; override;
    property Nodes[Index: Integer]: PVirtualNode read GetNodes;
    property IDs[Index: Integer]: String read GetIDs;
    function Search(AID: String): Integer;
    function InsertItem(AID: String; ANode: PVirtualNode): TDTNodeItem;
    procedure DeleteItem(AID: String);
    function NodeByID(AID: String): PVirtualNode;
  end;

  TDTDBTreeView = class;

  TDTDBTreeViewLink = class(TDataLink)
  private
    FDBTree: TDTDBTreeView;
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure DataSetChanged; override;
  public
   property DBTree: TDTDBTreeView read FDBTree;
   constructor Create(ADBTree: TDTDBTreeView);
  end;

  TDTDBTreeFields = class(TPersistent)
  private
    FDBTreeView: TDTDBTreeView;
    FKeyFieldName: String;
    FParentFieldName: String;
    FParentOfRootValue: String;
    FListFieldName: String;
    FHasChildrenFieldName: String;
    function GetDBTreeView: TDTDBTreeView;
    procedure SetKeyFieldName(const Value: String);
    procedure SetListFieldName(const Value: String);
    procedure SetParentFieldName(const Value: String);
    procedure SetParentOfRootValue(const Value: String);
    function ValidFieldName(AFieldName: String): Boolean;
  public
    function FieldToKey(AFieldName: String; ADataSet: TDataSet = nil): String;
    property DBTreeView: TDTDBTreeView read GetDBTreeView;
  published
    property KeyFieldName: String read FKeyFieldName write SetKeyFieldName;
    property ParentFieldName: String read FParentFieldName write SetParentFieldName;
    property ListFieldName: String read FListFieldName write SetListFieldName;
    property HasChildrenFieldName: String read FHasChildrenFieldName write FHasChildrenFieldName;
    property ParentOfRootValue: String read FParentOfRootValue write SetParentOfRootValue;
  end;

  TDTDBTreeImages = class(TPersistent)
  private
    FHasChildrenImageIndex: Integer;
    FNoChildrenImageIndex: Integer;
    FHasChildrenSelectedIndex: Integer;
    FNoChildrenSelectedIndex: Integer;
  public
    constructor Create;
  published
    property HasChildrenImageIndex: Integer read FHasChildrenImageIndex write FHasChildrenImageIndex;
    property HasChildrenSelectedIndex: Integer read FHasChildrenSelectedIndex write FHasChildrenSelectedIndex;
    property NoChildrenImageIndex: Integer read FNoChildrenImageIndex write FNoChildrenImageIndex;
    property NoChildrenSelectedIndex: Integer read FNoChildrenSelectedIndex write FNoChildrenSelectedIndex;
  end;

  TOnDBTreeCreateNode = procedure(Sender: TDTDBTreeView; Node: PVirtualNode;
    DataSet: TDataSet) of object;
  TDTOnGetFieldValue = procedure(ADataSet: TDataSet;
    var HasChildrenValue: Boolean) of object;

  TDTDBVirtualTreeColumn = class(TVirtualTreeColumn)
  private
    FFieldName: String;
    procedure SetFieldName(const Value: String);
  published
    property FieldName: String read FFieldName write SetFieldName;
  end;

  TDTDBTreeView = class(TVirtualStringTree)
  private
    FDataLink: TDTDBTreeViewLink;
    FDraggedNode: PVirtualNode;
    FDBTreeFields: TDTDBTreeFields;
    FDBTreeImages: TDTDBTreeImages;
    FOnDBTreeCreateNode: TOnDBTreeCreateNode;
    FIntOperationCounter: Integer;
    FOnGetHasChildren: TDTOnGetFieldValue;
    FUseFilter: Boolean;
    procedure SetDBTreeImages(const Value: TDTDBTreeImages);
    function GetDataSource: TDataSource;
    function IsDataAvail: boolean;
    function NodeCaption(ANode: PVirtualNode): String;
    procedure SetDBTreeFields(const Value: TDTDBTreeFields);
    procedure DoGoUp;
    procedure DoGoDown;
    procedure UpdateDataSet;
    function GetFilterString(AParentValue: String): String;
    procedure SetOnDBTreeCreateNode(const Value: TOnDBTreeCreateNode);
    function GetInternalOperation: Boolean;
    procedure SetInternalOperation(const Value: Boolean);
    procedure SetOnGetHasChildren(const Value: TDTOnGetFieldValue);
    procedure NodeChanged(ANode: PVirtualNode);
  protected
    FNodes: TDTNodeList;
    FCloneDataSet: TDataSet;
    FEndOfTree: Boolean;
    procedure CheckRecordDeleted;
    procedure CheckRecordInserted; virtual;
    function ChildrenPresent(Node: PVirtualNode): Boolean; virtual;
    function CreateChild(ANode: PVirtualNode): PVirtualNode; virtual;
    procedure CreateCloneDataSet; virtual; abstract;
    procedure CreateRootNodes; virtual;
    procedure DataLinkRecordChanged(AField: TField);
    procedure DataLinkDataSetChanged; virtual;
    procedure DataLinkDataSetScrolled; virtual;
    procedure DestroyCloneDataSet; virtual;
    procedure DoChange(Node: PVirtualNode); override;
    procedure KeyChanged(AKey: String); virtual;
    procedure DoDragDrop(Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint;
      var Effect: Integer; Mode: TDropMode); override;
    function DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer): Boolean; override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; Text: WideString); override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var Text: WideString); override;
    procedure DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    function GetColumnClass: TVirtualTreeColumnClass; override;
    function GetListNode(Node: PVirtualNode): TDTNodeItem;
    function GetNodeByID(ID: variant): PVirtualNode;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SelectNode(Node: PVirtualNode);
    procedure SetCloneDataSetFilter(AParentValue: String); virtual;
    procedure SetDataSource(const Value: TDataSource); virtual;
    function TreeFieldChanged(ATreeField: String; AField: TField): Boolean;
    function ValidParent(AParentValue: String): Boolean;
    procedure UpdateCloneDataSet; virtual;
    property InternalOperation: Boolean read GetInternalOperation write SetInternalOperation;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    function VarArrayToKey(AArray: Variant): String;
    function KeyToVarArray(Key: String): Variant;
    function RecordHasChildren: Boolean; virtual;
    procedure GoTop;
    function CanGoUp(ADataSet: TDataSet = nil): Boolean;
    function CanGoDown: Boolean;
    procedure UpdateText;
    procedure GoUp;
    procedure GoDown;
    procedure Next;
    function EndOfTree: Boolean;
    property ListNode[Node: PVirtualNode]: TDTNodeItem read GetListNode;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DBTreeFields: TDTDBTreeFields read FDBTreeFields write SetDBTreeFields;
    property DBTreeImages: TDTDBTreeImages read FDBTreeImages write SetDBTreeImages;
    property UseFilter: Boolean read FUseFilter write FUseFilter;
    property OnDBTreeCreateNode: TOnDBTreeCreateNode read FOnDBTreeCreateNode write SetOnDBTreeCreateNode;
    property OnGetHasChildren: TDTOnGetFieldValue read FOnGetHasChildren write SetOnGetHasChildren;
  end;

implementation

uses Forms, DTCommon;

{ TTrNodeList }

function TDTNodeList.CompareIDs(ID1, ID2: String): Integer;
begin
  if ID1 = ID2 then
    Result := 0
  else if ID1 > ID2 then
    Result := 1
  else
    Result := -1;
end;

procedure TDTNodeList.DeleteItem(AID: String);
var
  Index: Integer;
begin
  Index := Search(AID);
  if Index <> -1 then
    Delete(Index);
end;

function TDTNodeList.FindNewIndex(AID: String): Integer;
var
  First : Integer;
  Last  : Integer;
  Pivot : Integer;
  Found : Boolean;

  function CheckFoundFound: Integer;
  begin
    Result := -1;
    if Count = 0 then
      Result := 0
    else if CompareIDs(AID, IDs[Last]) = 1 then
      Result := Last + 1
    else if CompareIDs(AID, IDs[First]) = -1 then
      Result := First
  end;

begin
    First := 0;
    Last := Count - 1;
    Found := False;
    Result := 0;
    while (First <= Last) and (not Found) do
    begin
      Result := CheckFoundFound;
      if Result = -1 then
      begin
        Pivot := (First + Last) div 2;
        case CompareIDs(AID, IDs[Pivot]) of
          -1: Last := Pivot - 1;
          0: raise Exception.CreateFmt(sDuplicateListID, [AID]);
          1: First := Pivot + 1;
        end;
      end
      else
        Found := True;
    end;
end;

function TDTNodeList.GetNodes(Index: Integer): PVirtualNode;
begin
  Result := TDTNodeItem(Items[Index]).FNode;
end;

function TDTNodeList.GetIDs(Index: Integer): String;
begin
  Result := TDTNodeItem(Items[Index]).FKey;
end;

function TDTNodeList.InsertItem(AID: String; ANode: PVirtualNode): TDTNodeItem;
var
  Index: Integer;
begin
  Index := FindNewIndex(AID);
  Result := TDTNodeItem.Create(AID, ANode);
  Insert(Index, Result);
end;

function TDTNodeList.Search(AID: String): Integer;
var
  First : Integer;
  Last  : Integer;
  Pivot : Integer;
  Found : Boolean;
begin
  First := 0;
  Last := Count - 1;
  Found := False;
  Result := -1;
  while (First <= Last) and (not Found) do
    begin
      Pivot := (First + Last) div 2;
      case CompareIDs(IDs[Pivot], AID) of
        0:
        begin
          Found := True;
          Result := Pivot;
        end;
        1: Last := Pivot - 1
        else First := Pivot + 1;
      end;
    end;
end;

function TDTNodeList.NodeByID(AID: String): PVirtualNode;
var
  Index: Integer;
begin
  Result := nil;
  Index := Search(AID);
  if Index <> -1 then
    Result := Nodes[Index];
end;

{ TDTNodeItem }

constructor TDTNodeItem.Create(AKey: String; ANode: PVirtualNode);
begin
  FKey := AKey;
  FNode := ANode;
end;

{ TDBTree }

function TDTDBTreeView.CreateChild(ANode: PVirtualNode): PVirtualNode;

  procedure AddFieldValues(var AFieldValues: Variant);
  var
    i: Integer;
  begin
    with Header.Columns do
      if Count > 0 then
      begin
        AFieldValues := VarArrayCreate([0, Count - 1], varVariant);
        for i := 0 to Count - 1 do
          with Items[i] as TDTDBVirtualTreeColumn do
            if FieldName <> '' then
              AFieldValues[i] := FCloneDataSet.FieldValues[FieldName]
            else
              AFieldValues[i] := Null;
      end;
  end;

var
  ChildData: PNodeItem;
  HasChldn: Boolean;
begin
  with FCloneDataSet, FDBTreeFields do
  begin
    Result := AddChild(ANode);
    ChildData := GetNodeData(Result);
    ChildData^ := FNodes.InsertItem(FieldToKey(KeyFieldName, FCloneDataSet), Result);
    ChildData^.Caption := VarToStr(FCloneDataSet.FieldValues[ListFieldName]);
    AddFieldValues(ChildData^.FFieldValues);
    if HasChildrenFieldName <> '' then
      HasChldn := FCloneDataSet.FieldByName(HasChildrenFieldName).AsBoolean
    else if Assigned(OnGetHasChildren) then
    begin
      OnGetHasChildren(FCloneDataSet, HasChldn)

    end
    else
      HasChldn := ChildrenPresent(Result);
    Self.HasChildren[Result] := HasChldn;
  end;
  if Assigned(OnDBTreeCreateNode) then
    OnDBTreeCreateNode(Self, Result, FCloneDataSet);
end;

procedure TDTDBTreeView.CheckRecordDeleted;
var
  ListNode: TDTNodeItem;
  BMark: TBookmarkStr;
  NewNode: PVirtualNode;
begin
  if InternalOperation then
    Exit;
  InternalOperation := True;
  try
    if (FocusedNode <> nil) then
    begin
      NewNode := nil;
      ListNode := GetListNode(FocusedNode);
      if Assigned(ListNode) then
        with DataSource.DataSet, FDBTreeFields do
        begin
          if not VarEquals(KeyToVarArray(ListNode.Key), FieldValues[KeyFieldName]) then
          begin
            BMark := Bookmark;
            try
              if not Locate(KeyFieldName, KeyToVarArray(ListNode.Key), []) then
              begin
                NewNode := GetNextSibling(FocusedNode);
                if NewNode = nil then
                begin
                  NewNode := GetPreviousSibling(FocusedNode);
                  if NewNode = nil then
                    NewNode := FocusedNode.Parent;
                end;
                DeleteNode(FocusedNode);
              end;
            finally
              if NewNode <> nil then
              begin
                NodeChanged(NewNode);
                SelectNode(NewNode)
              end
              else
                Bookmark := BMark;
            end;
          end;
        end;
    end;
  finally
    InternalOperation := False;
  end;
end;

function TDTDBTreeView.ChildrenPresent(Node: PVirtualNode): Boolean;
var
  ListNode: TDTNodeItem;
  Res: variant;
  BMark: TBookmarkStr;
  WasFiltered: Boolean;
begin
  Result := False; 
  if FCloneDataSet <> nil then
  begin
    ListNode := GetListNode(Node);
    if Assigned(ListNode) and (FCloneDataSet <> nil) then
      with FCloneDataSet, FDBTreeFields do
      try
        BMark := Bookmark;
        WasFiltered := Filtered;
        Filtered := False;
        Res := Lookup(ParentFieldName, KeyToVarArray(ListNode.Key), ParentFieldName);
        Result := not (VarType(Res) in [varNull]);
        Filtered := WasFiltered;
        Bookmark := BMark;
      finally
      end;
  end;
end;

constructor TDTDBTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDBTreeFields := TDTDBTreeFields.Create;
  FDBTreeFields.FDBTreeView := Self;
  FDBTreeImages := TDTDBTreeImages.Create;
  FNodes := TDTNodeList.Create;
  FDataLink := TDTDBTreeViewLink.Create(Self);
  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toUseBlendedImages];
  NodeDataSize := SizeOf(TDTNodeItem);
  RootNodeCount := 0;
  FUseFilter := True;
end;

procedure TDTDBTreeView.DataLinkDataSetScrolled;

  function FindParentValue: Variant;
  begin
  end;

  function InitParents(CurID: Variant): PVirtualNode;
  var
    Node : PVirtualNode;
    ParentValue: Variant;
  begin
    if VarToStr(CurID) = FDBTreeFields.ParentOfRootValue then
    begin
      Result := RootNode;
      Exit;
    end;
    Result := nil;
    Node := GetNodeByID(CurID);
    if Node <> nil then
      Result := Node
    else
    begin
      if FCloneDataSet <> nil then
        with FCloneDataSet, FDBTreeFields do
          ParentValue := Lookup(KeyFieldName, KeyToVarArray(CurID), ParentFieldName);
      if VarType(ParentValue) = varNull then
      begin
        Information(Format(sCanNotFindParent , [FDBTreeFields.KeyFieldName, VarToStr(CurID)]))
      end
      else
      begin
        Node := InitParents(VarArrayToKey(ParentValue));
        if Node <> nil then
        begin
          Expanded[Node] := True;
          Result := GetNodeByID(CurID);
        end;
      end;
    end;
  end;

var
  Node: PVirtualNode;
  CurNodeID: Variant;
begin
  FEndOfTree := False;
try
  if FCloneDataSet <> nil then
    if FCloneDataSet.RecordCount <> 0 then
    with FCloneDataSet, FDBTreeFields do
    begin
      InternalOperation := True;
      try
        UpdateCloneDataSet;
        CurNodeID := FieldToKey(KeyFieldName, FCloneDataSet);
        Node := GetNodeByID(CurNodeID);
        if Node <> nil then
          SelectNode(Node)
        else
        begin
          Node := InitParents(CurNodeID);
          SelectNode(Node);
        end;
      finally
        InternalOperation := False;
      end;
    end;
except

end;
end;

destructor TDTDBTreeView.Destroy;
begin
  FDataLink.DataSource := nil;
  DestroyCloneDataSet;
  FDBTreeFields.Free;
  FDBTreeImages.Free;
  FNodes.Free;
  FNodes := nil;
  FDataLink.Free;
  inherited;
end;

procedure TDTDBTreeView.DoChange(Node: PVirtualNode);
begin
  if not InternalOperation and (Node <> nil) then
    NodeChanged(Node);
  inherited DoChange(Node);
end;

function TDTDBTreeView.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
begin
  Result := nil;
  if Assigned(OnGetImageIndex) or Assigned(OnGetImageIndexEx) then
    inherited DoGetImageIndex(Node, Kind, Column, Ghosted, Index)
  else
  begin
    if HasChildren[Node] then                        
    begin
      if Kind = ikNormal then
        Index := FDBTreeImages.HasChildrenImageIndex
      else if Kind = ikSelected then
        Index := FDBTreeImages.HasChildrenSelectedIndex;
    end
    else
    begin
      if Kind = ikNormal then
        Index := FDBTreeImages.NoChildrenImageIndex
      else if Kind = ikSelected then
        Index := FDBTreeImages.NoChildrenSelectedIndex;
    end;
  end;
end;

procedure TDTDBTreeView.DoGetText(Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var Text: WideString);
var
  Item: TDTNodeItem;
begin
  if not (csDesigning in ComponentState) then
  try
    Item := GetListNode(Node);
    if Item <> nil then
    begin
      if Header.Columns.Count > 0 then
        Text := VarToStr(Item.FieldValues[Column])
      else
        Text := Item.Caption;
    end;
  except
  end;
end;

procedure TDTDBTreeView.DoInitChildren(Node: PVirtualNode;
  var ChildCount: Cardinal);
var
  CurData: PNodeItem;
  CCount: integer;
begin
  if Node = nil then
    Exit;
  if not (vsInitialized in Node.States) then
    Exit;
  if FCloneDataSet <> nil then
  begin
    BeginUpdate;
    CurData := GetNodeData(Node);
    CCount := 0;
    with FCloneDataSet, FDBTreeFields do
      if FUseFilter then
      begin
        SetCloneDataSetFilter(CurData^.Key);
        Filtered := True;
        First;
        while not Eof do
        begin
          CreateChild(Node);
          Inc(CCount);
          Next;
        end;
        Filtered := False;
      end
      else
      begin
        First;
        while not Eof do
        begin
          if ValidParent(CurData^.Key) then
          begin
            CreateChild(Node);
            Inc(CCount);
          end;
          Next;
        end;
      end;
    ChildCount := CCount;
    EndUpdate;
  end;
end;

function TDTDBTreeView.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDTDBTreeView.GetNodeByID(ID: variant): PVirtualNode;
var
  Index: Integer;
begin
  Result := nil;
  Index := FNodes.Search(VarToStr(ID));
  if Index <> -1 then
    Result := FNodes.Nodes[Index];
end;

procedure TDTDBTreeView.CreateRootNodes;
begin
  BeginUpdate;
  Clear;
  if FCloneDataSet <> nil then
    if FUseFilter then
    begin
      SetCloneDataSetFilter(FDBTreeFields.ParentOfRootValue);
      with FCloneDataSet do
      begin
        Filtered := True;
        First;
        while not Eof do
        begin
          CreateChild(nil);
          Next;
        end;
        Filtered := False;
      end;
    end
    else
    begin
      with FCloneDataSet do
      begin
        First;
        while not Eof do
        begin
          if ValidParent(DBTreeFields.ParentOfRootValue) then
            CreateChild(nil);
          Next;
        end;
      end;
    end;
  EndUpdate;
end;

function TDTDBTreeView.NodeCaption(ANode: PVirtualNode): String;
var
  Data: PNodeItem;
begin
  Result := '';
  Data := GetNodeData(ANode);
  if Assigned(Data) and Assigned(Data^) then
    Result := Trim(Data^.Caption);
end;

procedure TDTDBTreeView.DataLinkRecordChanged(AField: TField);
var
  Data: PNodeItem;
  NewNode, Node, ParentNode: PVirtualNode;
  CurID: Variant;
  HasChilden: Boolean;
begin
  if InternalOperation then
    Exit;
  if AField <> nil then
    with FDBTreeFields do
    begin
      InternalOperation := True;
      try
        if AnsiCompareText(AField.FieldName, ListFieldName) = 0 then
        begin
          if FocusedNode <> nil then
          begin
            Data := GetNodeData(FocusedNode);
            if Assigned(Data) and Assigned(Data^) then
              if Data^.Key = FieldToKey(KeyFieldName) then
              begin
                BeginUpdate;
                Data^.Caption := AField.Value;
                EndUpdate;
                SelectNode(FocusedNode);
              end;
          end;
        end;
{
        else if TreeFieldChanged(KeyFieldName, AField) then
        begin
          Data := GetNodeData(FocusedNode);
          DeleteNode(FocusedNode, True);
          CurCaption := Data^.FCaption;
          FNodes.DeleteItem(Data^.Key);
          with DataSource.DataSet, DBTreeFields do
            Data^ := FNodes.InsertItem(FieldToKey(KeyFieldName), FocusedNode);
          Data^.FCaption := CurCaption;
          SelectNode(FocusedNode);
          DoInitChildren(FocusedNode, ChCount);
        end
}
        if TreeFieldChanged(ParentFieldName, AField) then
        begin
          with DataSource.DataSet, DBTreeFields do
          begin
            CurID := FieldToKey(KeyFieldName);
            Node := GetNodeByID(CurID);
            if Node <> nil then
            begin
              HasChilden := Self.HasChildren[Node];
              ParentNode := GetNodeByID(FieldToKey(ParentFieldName));
              if ParentNode <> nil then
              begin
                DeleteNode(Node);
                if ParentNode <> nil then
                begin
                  NewNode := CreateChild(ParentNode);
                  Self.HasChildren[NewNode] := HasChilden;
                  Expanded[ParentNode] := True;
                  SelectNode(NewNode);
                end;
              end;
            end;
          end;
        end;
      finally
        InternalOperation := False;
      end;
    end;
end;

procedure TDTDBTreeView.SetDataSource(const Value: TDataSource);
begin
  if Value <> FDatalink.DataSource then
  begin
    FDataLink.DataSource := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

{ TDTDBTreeViewLink }

procedure TDTDBTreeViewLink.ActiveChanged;
var 
  CurCursor: TCursor;
begin
  with FDBTree do
  begin
    if InternalOperation then
      Exit;
    if csDesigning in ComponentState then
    begin
      if IsDataAvail and Active then
        RootNodeCount := 2
      else
        RootNodeCount := 0;
    end
    else
    begin
      if IsDataAvail and Active then
      begin
        CurCursor := Screen.Cursor;
        Screen.Cursor := crHourGlass;
        try
          CreateCloneDataSet;
          CreateRootNodes;
          DataLinkDataSetScrolled;
        finally
          Screen.Cursor := CurCursor;  
        end;
      end
      else
      begin
        BeginUpdate;
        Clear;
        EndUpdate;
        DestroyCloneDataSet;
      end;
    end;
  end;
end;

constructor TDTDBTreeViewLink.Create(ADBTree: TDTDBTreeView);
begin
 inherited Create;
 FDBTree := ADBTree;
 VisualControl := True;
end;


procedure TDTDBTreeViewLink.DataSetChanged;
begin
  inherited;
  FDBTree.DataLinkDataSetChanged;
end;

procedure TDTDBTreeViewLink.DataSetScrolled(Distance: Integer);
begin
  with FDBTree do
    DataLinkDataSetScrolled;
end;

procedure TDTDBTreeViewLink.RecordChanged(Field: TField);
begin
  inherited;
  with FDBTree do
    DataLinkRecordChanged(Field);
end;

procedure TDTDBTreeView.DoDragDrop(Source: TObject; DataObject: IDataObject;
  Formats: TFormatArray; Shift: TShiftState; Pt: TPoint;
  var Effect: Integer; Mode: TDropMode);
var
  NewParentNode: PVirtualNode;
  CurData: PNodeItem;
  ParentKey: Variant;
begin
  if not (csDesigning in ComponentState) then
//    if not Assigned(OnDragDrop) then
    begin
      Effect := DROPEFFECT_NONE;
      NewParentNode := GetNodeAt(Pt.X, Pt.Y);
      if (NewParentNode <> nil) and (FDraggedNode <> nil) and (NewParentNode <> FDraggedNode) then
        if Question(Format(sMoveNodeQuestion, [NodeCaption(FDraggedNode),  NodeCaption(NewParentNode)])) then
        begin
          CurData := GetNodeData(NewParentNode);
          if Assigned(CurData) and Assigned(CurData^) then
          begin
            ParentKey := KeyToVarArray(CurData^.Key);
            CurData := GetNodeData(FDraggedNode);
            if Assigned(CurData) and Assigned(CurData^) then
              with DataSource.DataSet, FDBTreeFields do
                if Locate(KeyFieldName, KeyToVarArray(CurData^.Key), []) then
                  begin
                    Edit;
                    FieldValues[FParentFieldName] := ParentKey;
//                    Post;
                  end;
          end;
        end;
    end;
  inherited DoDragDrop(Source, DataObject, Formats, Shift, Pt, Effect, Mode);    
end;

procedure TDTDBTreeView.DoStartDrag(var DragObject: TDragObject);
begin
  if not (csDesigning in ComponentState) then
    FDraggedNode := FocusedNode;
end;

function TDTDBTreeView.DoDragOver(Source: TObject; Shift: TShiftState;
  State: TDragState; Pt: TPoint; Mode: TDropMode;
  var Effect: Integer): Boolean;
begin
  Result := inherited DoDragOver(Source, Shift, State, Pt, Mode, Effect);
  if not Assigned(OnDragOver) then
  begin
    Result := False;
    if not (csDesigning in ComponentState) then
      if Source = Self then
        Result := True;
  end;
end;

procedure TDTDBTreeView.CheckRecordInserted;
var
  ParentNode, NewNode: PVirtualNode;
  BMark: TBookmarkStr;
begin
  if InternalOperation then
    Exit;
  InternalOperation := True;
  try
    with DataSource.DataSet, FDBTreeFields do
    begin
      if GetNodeByID(FieldToKey(KeyFieldName)) = nil then
      begin
        if FieldToKey(ParentFieldName) = ParentOfRootValue then
          ParentNode := RootNode
        else
          ParentNode := GetNodeByID(FieldToKey(ParentFieldName));
        if ParentNode <> nil then
        begin
          BMark := Bookmark;
          Expanded[ParentNode] := True;
          Bookmark := BMark;
          UpdateCloneDataSet;
          NewNode := CreateChild(ParentNode);
          SelectNode(NewNode);
          DataLinkDataSetScrolled;
        end;
      end;
    end;
  finally
    InternalOperation := False;
  end;
end;

procedure TDTDBTreeView.DestroyCloneDataSet;
begin
  if FCloneDataSet <> nil then
  begin
    FCloneDataSet.Close;
    FCloneDataSet.Free;
    FCloneDataSet := nil;
  end;
end;

procedure TDTDBTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (DataSource <> nil) then
    if AComponent = DataSource then
      DataSource := nil;
end;

procedure TDTDBTreeView.UpdateCloneDataSet;
begin
  if FCloneDataSet <> nil then
    with FDataLink.DataSet, FDBTreeFields do
      if not ControlsDisabled then
      begin
        FCloneDataSet.Filtered := False;
        FCloneDataSet.Locate(KeyFieldName, FieldValues[KeyFieldName], []);
      end;
end;

procedure TDTDBTreeView.SetCloneDataSetFilter(AParentValue: String);
begin
  if FCloneDataSet <> nil then
    FCloneDataSet.Filter := GetFilterString(AParentValue);
end;

procedure TDTDBTreeView.SetDBTreeFields(const Value: TDTDBTreeFields);
begin
  FDBTreeFields := Value;
end;

procedure TDTDBTreeView.SetDBTreeImages(const Value: TDTDBTreeImages);
begin
  FDBTreeImages := Value;
end;

procedure TDTDBTreeView.DoFreeNode(Node: PVirtualNode);
var
  Index: Integer;
  ListNode: TDTNodeItem;
begin
  if not (csDesigning in ComponentState) then
  begin
    ListNode := GetListNode(Node);
    if FNodes <> nil then
      if ListNode <> nil then
      begin
        Index := FNodes.Search(ListNode.Key);
        if Index <> -1 then
        begin
          FNodes.Delete(Index);
          ListNode.Free;
        end;
      end;
  end;
  inherited DoFreeNode(Node);
end;

function TDTDBTreeView.RecordHasChildren: Boolean;
var
  BMark: String;
  WasFiltered: Boolean;
  KeyValue: Variant;
begin
  if DBTreeFields.HasChildrenFieldName <> '' then
    Result := FDataLink.DataSet.FieldValues[DBTreeFields.HasChildrenFieldName]
  else
  with FCloneDataSet, FDBTreeFields do
    begin
      KeyValue := FieldValues[KeyFieldName];
      BMark := Bookmark;
      WasFiltered := Filtered;
      Filtered := False;
      Result := Locate(ParentFieldName, KeyValue, []);
      Filtered := WasFiltered;
      Bookmark := BMark;
    end;
end;

function TDTDBTreeView.GetListNode(Node: PVirtualNode): TDTNodeItem;
var
  CurData: ^TObject;
begin
  Result := nil;
  if FNodes.Count > 0 then
  begin
    CurData := GetNodeData(Node);
    if Assigned(CurData) then
      if CurData^ is TDTNodeItem then
        Result := CurData^ as TDTNodeItem;
  end;
end;

{ TDTDBTreeFields }

function TDTDBTreeFields.FieldToKey(AFieldName: String;
  ADataSet: TDataSet): String;
var
  i: Integer;
  FieldNames: TStringDynArray;
begin
  FieldNames := StringAsArray(AFieldName);
  Result := '';
  if ADataSet = nil then
    ADataSet := FDBTreeView.DataSource.DataSet;
  for i := Low(FieldNames) to High(FieldNames) do
  begin
    if i > 0 then
      Result := Result + ';';
    Result := Result + VarToStr(ADataSet.FieldValues[FieldNames[i]]);
  end;
end;

function TDTDBTreeFields.GetDBTreeView: TDTDBTreeView;
begin
  Result := FDBTreeView; 
end;

procedure TDTDBTreeView.KeyDown(var Key: Word; Shift: TShiftState);
begin
end;

procedure TDTDBTreeFields.SetKeyFieldName(const Value: String);
begin
  if FKeyFieldName <> Value then
  begin
    FKeyFieldName := Value;
    FDBTreeView.FDataLink.ActiveChanged;
  end;
end;

procedure TDTDBTreeFields.SetListFieldName(const Value: String);
begin
  if FListFieldName <> Value then
  begin
    FListFieldName := Value;
    FDBTreeView.FDataLink.ActiveChanged;
  end;
end;

procedure TDTDBTreeFields.SetParentFieldName(const Value: String);
begin
  if FParentFieldName <> Value then
  begin
    FParentFieldName := Value;
    FDBTreeView.FDataLink.ActiveChanged;
  end;
end;

procedure TDTDBTreeFields.SetParentOfRootValue(const Value: String);
begin
  if FParentOfRootValue <> Value then
  begin
    FParentOfRootValue := Value;
    FDBTreeView.FDataLink.ActiveChanged;
  end;
end;

destructor TDTNodeList.Destroy;
begin
  ClearItems;
  inherited Destroy;
end;

procedure TDTDBTreeView.Clear;
begin
  if FNodes <> nil then
    FNodes.ClearItems;
  inherited Clear;
end;

procedure TDTNodeList.ClearItems;
begin
  while Count > 0 do
    TDTNodeItem(Extract(Items[Count - 1])).Free;
end;

procedure TDTDBTreeView.SelectNode(Node: PVirtualNode);
begin
  Selected[FocusedNode] := False;
  FocusedNode := Node;
  Selected[Node] := True;
end;

function TDTDBTreeFields.ValidFieldName(AFieldName: String): Boolean;
var
  i: Integer;
  FieldNames: TStringDynArray;
begin
  Result := False;
  if (AFieldName <> '') and (FDBTreeView.DataSource <> nil) then
    if FDBTreeView.DataSource.DataSet <> nil then
    begin
      Result := True;
      FieldNames := StringAsArray(AFieldName);
      for i := Low(FieldNames) to High(FieldNames) do
        with FDBTreeView.DataSource.DataSet do
          if FindField(FieldNames[i]) = nil then
          begin
            Result := False;
            Break;
          end;
    end;
end;

{ TDTDBTreeImages }

constructor TDTDBTreeImages.Create;
begin
  FHasChildrenImageIndex := -1;
  FNoChildrenImageIndex := -1;
  FHasChildrenSelectedIndex := -1;
  FNoChildrenSelectedIndex := -1;
end;

procedure TDTDBTreeView.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  Text: WideString);
begin
  inherited DoNewText(Node, Column, Text);
  if not Assigned(OnNewText) then
    with DataSource.DataSet do
    begin
      Edit;
      FieldValues[DBTreeFields.ListFieldName] := Text;
      Post;
    end;
end;

function TDTDBTreeView.CanGoDown: Boolean;
begin
  Result := RecordHasChildren;
end;

function TDTDBTreeView.CanGoUp(ADataSet: TDataSet = nil): Boolean;
begin
  Result := False;
  if ADataSet = nil then
    ADataSet := DataSource.DataSet;
  with DBTreeFields do
    if FieldToKey(ParentFieldName, ADataSet) <> ParentOfRootValue then
      Result := True;
end;

function TDTDBTreeView.EndOfTree: Boolean;
begin
  Result := FEndOfTree;
end;

procedure TDTDBTreeView.GoDown;
begin
  if CanGoDown then
  begin
    UpdateCloneDataSet;  
    DoGoDown;
    UpdateDataSet;
  end;
end;

procedure TDTDBTreeView.GoTop;
begin
  with FCloneDataSet, FDBTreeFields do
  begin
    SetCloneDataSetFilter(ParentOfRootValue);
    Filtered := True;
    First;
  end;
  UpdateDataSet;
end;

procedure TDTDBTreeView.GoUp;
begin
  if CanGoUp then
  begin
    UpdateCloneDataSet;
    DoGoUp;
    UpdateDataSet;
  end;
end;

procedure TDTDBTreeView.Next;

  procedure PrepareGoNext;
  begin
    with DataSource.DataSet, FDBTreeFields do
    begin
      if not (FCloneDataSet.Filtered and (FCloneDataSet.Filter = GetFilterString(FieldValues[ParentFieldName]))) then
      begin
        SetCloneDataSetFilter(FieldToKey(ParentFieldName));
        FCloneDataSet.Filtered := True;
      end;
      if not VarEquals  (FieldValues[KeyFieldName], FCloneDataSet.FieldValues[KeyFieldName]) then
        FCloneDataSet.Locate(KeyFieldName, FieldValues[KeyFieldName], []);
    end;
  end;

var
  KeyValue: Variant;
  ParentValue: String;
begin
  if not EndOfTree then
  begin
    if CanGoDown then
      GoDown
    else
    begin
      with FCloneDataSet, FDBTreeFields do
      begin
        PrepareGoNext;
        KeyValue := FieldValues[KeyFieldName];
        ParentValue := FieldToKey(ParentFieldName, FCloneDataSet);
        Next;
        while Eof and CanGoUp(FCloneDataSet) do
        begin
          DoGoUp;
          Next;
        end;
        if Eof and not CanGoUp(FCloneDataSet) then
        begin
          SetCloneDataSetFilter(ParentValue);
          FCloneDataSet.Locate(KeyFieldName, KeyValue, []);
          FEndOfTree := True;
        end;
      end;
    end;
    UpdateDataSet;
  end;
end;

procedure TDTDBTreeView.DoGoDown;
begin
  with FCloneDataSet, FDBTreeFields do
  begin
    SetCloneDataSetFilter(FieldToKey(KeyFieldName, FCloneDataSet));
    Filtered := True;
    First;
  end;
end;

procedure TDTDBTreeView.DoGoUp;
var
  ParentValue: Variant;
begin
  with FDBTreeFields do
  begin
    ParentValue := FCloneDataSet.FieldValues[ParentFieldName];
    FCloneDataSet.Filtered := False;
    FCloneDataSet.Locate(KeyFieldName, ParentValue, []);
    SetCloneDataSetFilter(FieldToKey(ParentFieldName, FCloneDataSet));
    FCloneDataSet.Filtered := True;
    FCloneDataSet.Locate(KeyFieldName, ParentValue, []);
  end;
end;

procedure TDTDBTreeView.UpdateDataSet;
begin
  with FDBTreeFields do
    if not VarEquals(FCloneDataSet.FieldValues[KeyFieldName], FDataLink.DataSet.FieldValues[KeyFieldName]) then
      FDataLink.DataSet.Locate(KeyFieldName, FCloneDataSet.FieldValues[KeyFieldName], []);
end;

procedure TDTDBTreeView.UpdateText;
var
  Node: PVirtualNode;
  Data: PNodeItem;
begin
  with DataSource.DataSet, DBTreeFields do
  begin
    Node := GetNodeByID(FieldToKey(KeyFieldName));
    Data := GetNodeData(Node);
    if Assigned(Data) and Assigned(Data^) then
        if Data^.Key = FieldToKey(KeyFieldName) then
        begin
          BeginUpdate;
          Data^.Caption := VarToStr(FieldValues[ListFieldName]);
          EndUpdate;
        end;
  end;
end;

function TDTDBTreeView.GetFilterString(AParentValue: String): String;
var
  i: Integer;
  ParentFieldValues, ParentFieldNames: TStringDynArray;
begin
  Result := '';
  if FCloneDataSet <> nil then
    with FCloneDataSet, FDBTreeFields do
    begin
      if Self.DataSource.DataSet.Filtered then
        Result := '(' + Self.DataSource.DataSet.Filter + ') AND';
      ParentFieldValues := StringAsArray(AParentValue);
      ParentFieldNames := StringAsArray(ParentFieldName);
      for i := Low(ParentFieldNames) to High(ParentFieldNames) do
      begin
        if Result <> '' then
          Result := Result + 'AND';
        if ParentFieldValues[i] <> '' then
          Result := Result + '(' + ParentFieldNames[i] + '=' + ParentFieldValues[i] + ')';
      end;
    end;
end;

procedure TDTDBTreeView.SetOnDBTreeCreateNode(
  const Value: TOnDBTreeCreateNode);
begin
  FOnDBTreeCreateNode := Value;
end;

procedure TDTDBTreeView.SetOnGetHasChildren(const Value: TDTOnGetFieldValue);
begin
  FOnGetHasChildren := Value;
end;

function TDTDBTreeView.GetColumnClass: TVirtualTreeColumnClass;
begin
  Result := TDTDBVirtualTreeColumn;
end;

function TDTDBTreeView.IsDataAvail: boolean;
begin
  Result := Assigned(DataSource) and Assigned(DataSource.DataSet) and (DataSource.DataSet.Active);
  if Result then
    with DataSource.DataSet, FDBTreeFields do
      Result := Result and ValidFieldName(KeyFieldName) and ValidFieldName(ParentFieldName)
      and ValidFieldName(ListFieldName);
end;

{ TDTDBVirtualTreeColumn }

procedure TDTDBVirtualTreeColumn.SetFieldName(const Value: String);
begin
  FFieldName := Value;
end;

procedure TDTDBTreeView.DataLinkDataSetChanged;
begin
    if DataSource.DataSet.State = dsBrowse then
    begin
      CheckRecordDeleted;
      DataLinkDataSetScrolled;
      CheckRecordInserted;
    end;
end;

procedure TDTDBTreeView.KeyChanged(AKey: String);
begin
  try
    with FDataLink.DataSet, FDBTreeFields do
      Locate(KeyFieldName, KeyToVarArray(AKey),[]);
  except
  end;
end;

function TDTDBTreeView.GetInternalOperation: Boolean;
begin
  Result := FIntOperationCounter > 0;
end;

procedure TDTDBTreeView.SetInternalOperation(const Value: Boolean);
begin
  if Value then
    Inc(FIntOperationCounter)
  else
    Dec(FIntOperationCounter)
end;

function TDTDBTreeView.KeyToVarArray(Key: String): Variant;
var
  i: Integer;
  StrArray: TStringDynArray;
begin
  StrArray := StringAsArray(Key);
  if Length(StrArray) = 1 then
    Result := Key
  else
  begin
    Result := VarArrayCreate([0, Length(StrArray) - 1], varVariant);
    for i := Low(StrArray) to High(StrArray) do
      Result[i] := StrArray[i];
  end;
end;

function TDTDBTreeView.ValidParent(AParentValue: String): Boolean;
var
  ParentKey: String;
begin
  with DBTreeFields do
  begin
    ParentKey := VarArrayToKey(FCloneDataSet.FieldValues[ParentFieldName]);
    Result := SameKeys(ParentKey, AParentValue);
  end;
end;

function TDTDBTreeView.VarArrayToKey(AArray: Variant): String;
var
  i: Integer;
  Key: String;
begin
  Result := '';
  if VarArrayDimCount(AArray) = 0 then
    Result := VarToStr(AArray)
  else
    for i := VarArrayLowBound(AArray, 1) to VarArrayHighBound(AArray, 1) do
    begin
      Key := VarToStr(AArray[i]);
//      if Key = '' then
//        Key := sDefaultKey;
      Result := Result + Key;
      if i < VarArrayDimCount(AArray) then
        Result := Result + ';';
    end;
end;

function TDTDBTreeView.TreeFieldChanged(ATreeField: String; AField: TField): Boolean;
var
  i: Integer;
  TreeFields: TStringDynArray;
begin
  Result := False;
  TreeFields := StringAsArray(ATreeField);
  for i := Low(TreeFields) to High(TreeFields) do
    if AnsiCompareText(TreeFields[i], AField.FieldName) = 0 then
    begin
      Result := True;
      Break;
    end;
end;

procedure TDTDBTreeView.NodeChanged(ANode: PVirtualNode);
var
  ListNode: TDTNodeItem;
begin
  with FDataLink.DataSet, FDBTreeFields do
  begin
    ListNode := GetListNode(ANode);
    if Assigned(ListNode) then
      KeyChanged(ListNode.Key);
  end;
end;

end.
