{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         TvgDBTreeView, ver. 1.90                      }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L- }

unit vgDBTree;

interface

uses
  Messages, Classes, Controls, ComCtrls, vgCtrls, DB, vgDBUtl;

type
  TSetRangeEvent = procedure (Sender: TObject; DataSet: TDataSet; ParentID: string) of object;
  TCancelRangeEvent = procedure (Sender: TObject; DataSet: TDataSet) of object;
  TCreateNodeEvent = procedure (Sender: TObject; Node: TTreeNode; DataSet: TDataSet) of object;
  TNodeEvent = procedure (Sender: TObject; Node: TTreeNode) of object;
  TCompareNodeEvent = procedure (Sender: TObject; Node: TTreeNode;
    NodeID: string; var Equal: Boolean) of object;
  TProcessNodeEvent = procedure (Sender: TObject; Node: TTreeNode; var Process: Boolean) of object;

  TUpdateMode       = (umNormal, umFast);

  TvgDBTreeOption   = (toEmptySelect, toQuickSearch);
  TvgDBTreeOptions  = set of TvgDBTreeOption;

  TvgCustomDBTreeView = class(TvgCustomTreeView)
  private
    FBmk: TPKBookmark;
    FChanged: Boolean;
    FDataLink: TDataLink;
    FDataFields: array[0..2] of string;
    FOptions: TvgDBTreeOptions;
    FStreamedDataFields: array[0..2] of string;
    FRootID, FStreamedRootID: string;
    FDataSetFilter: string;
    FDataSetFiltered: Boolean;
    FSearchText: string;
    FSortType: TSortType;
    FOnSetRange: TSetRangeEvent;
    FOnCancelRange: TCancelRangeEvent;
    FOnCreateNode: TCreateNodeEvent;
    FOnCompareNode: TCompareNodeEvent;
    FOnDestroyNode: TNodeEvent;
    FOnUpdateNode: TNodeEvent;
    FOnProcessBranches: TProcessNodeEvent;
    function CanUpdateNodes: Boolean;
    function GetDataSource: TDataSource;
    function GetField(Index: Integer): TField;
    procedure SetDataField(Index: Integer; Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetDataSetFilter(Value: string);
    procedure SetDataSetFiltered(Value: Boolean);
    procedure SetRootID(Value: string);
    procedure SetSortType(Value: TSortType);
    procedure CheckFields;
    procedure CreateBranches(Node: TTreeNode; UpdateMode: TUpdateMode);
    procedure CreateRoot;
    procedure DestroyBranches(Node: TTreeNode);
    procedure DestroyNode(Node: TTreeNode);
    procedure DestroyRoot;
    procedure InternalUpdateNode(Node: TTreeNode; UpdateMode: TUpdateMode);
    procedure StartTimer;
    procedure StopTimer;
    procedure ClearSearch;
    procedure QuickSearch;
    function StoreRootID: Boolean;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Msg: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure WMRButtonDown(var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    procedure ActiveChanged; virtual;
    procedure BeginChange;
    procedure EndChange;
    procedure Collapse(Node: TTreeNode); override;
    function CreateNode: TTreeNode; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoSetRange(Node: TTreeNode); virtual;
    procedure DoCancelRange(Node: TTreeNode); virtual;
    procedure DoCreateNode(Node: TTreeNode; DataSet: TDataSet); virtual;
    procedure DoDestroyNode(Node: TTreeNode); virtual;
    procedure DoUpdateNode(Node: TTreeNode); virtual;
    function ProcessBranches(Node: TTreeNode): Boolean;
    procedure UpdateNode(Node: TTreeNode; UpdateMode: TUpdateMode);
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataFieldID: string index 0 read FDataFields[0] write SetDataField;
    property DataFieldParentID: string index 1 read FDataFields[1] write SetDataField;
    property DataFieldText: string index 2 read FDataFields[2] write SetDataField;
    property FieldID: TField index 0 read GetField;
    property FieldParentID: TField index 1 read GetField;
    property FieldText: TField index 2 read GetField;
    property Options: TvgDBTreeOptions read FOptions write FOptions;
    property RootID: string read FRootId write SetRootID stored StoreRootID;
    property DataSetFilter: string read FDataSetFilter write SetDataSetFilter;
    property DataSetFiltered: Boolean read FDataSetFiltered write SetDataSetFiltered;
    property OnSetRange: TSetRangeEvent read FOnSetRange write FOnSetRange;
    property OnCancelRange: TCancelRangeEvent read FOnCancelRange write FOnCancelRange;
    property OnCreateNode: TCreateNodeEvent read FOnCreateNode write FOnCreateNode;
    property OnCompareNode: TCompareNodeEvent read FOnCompareNode write FOnCompareNode;
    property OnDestroyNode: TNodeEvent read FOnDestroyNode write FOnDestroyNode;
    property OnProcessBranches: TProcessNodeEvent read FOnProcessBranches write FOnProcessBranches;
    property OnUpdateNode: TNodeEvent read FOnUpdateNode write FOnUpdateNode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultSetRange(DataSet: TDataSet; ParentID: string); virtual;
    procedure DefaultCancelRange(DataSet: TDataSet); virtual;
    function CompareNode(Node: TTreeNode; NodeID: string): Boolean; virtual;
    function LocateNode(StartNode: TTreeNode; NodeID: string): TTreeNode;
    function LookupNode(NodeID: string): TTreeNode;
    function LookupNodeEx(NodeID: string; Select: Boolean): TTreeNode;
    function LookupNodeText(const Text: string; Options: TLocateOptions; Select: Boolean): TTreeNode;
    procedure SelectNode(Node: TTreeNode);
    function UpdateCursorPos: Boolean;
    procedure UpdateNodes(Node: TTreeNode);
    property SortType read FSortType write SetSortType;
  end;

  TvgDBTreeNode = class(TTreeNode)
  private
    FID, FParentID: string;
    FBookmark: TBookmark;
  public
    destructor Destroy; override;
    function GotoBookmark: Boolean;
    property Bookmark: TBookmark read FBookmark write FBookmark;
    property ID: string read FID write FID;
    property ParentID: string read FParentID write FParentID;
  end;

  TvgDBTreeView = class(TvgCustomDBTreeView)
  public
    property FieldID;
    property FieldParentID;
    property FieldText;
    property Items;
  published
    property AutoExpand;
    property HotTrack;
    property RowSelect;
    property ToolTips;
    property RightClickSelect;
    property OnGetItemParams;
    property OnCustomDraw;
    property OnCustomDrawItem;
  { new properties }
    property DataSource;
    property DataFieldID;
    property DataFieldParentID;
    property DataFieldText;
    property Options;
    property RootID;
    property OnSetRange;
    property OnCancelRange;
    property OnCompareNode;
    property OnCreateNode;
    property OnDestroyNode;
    property OnProcessBranches;
    property OnUpdateNode;
  published
    property Align;
{$IFDEF _D3_}
  {$IFDEF _D4_}
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property ParentBiDiMode;
    property ChangeDelay;
    property Constraints;
    property DragKind;
  {$ENDIF}
    property ImeMode;
    property ImeName;
{$ENDIF}
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property Images;
    property Indent;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsing;
    property OnCollapsed;
    property OnCompare;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanding;
    property OnExpanded;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
{$IFDEF _D4_}
    property OnEndDock;
    property OnStartDock;
  {$IFDEF _D5_}
    property OnContextPopup;
  {$ENDIF}
{$ENDIF}
  end;

const
  NullValue      = 'Null';

implementation
uses Windows, SysUtils, CommCtrl, Forms{$IFNDEF _D3_}, DBTables{$ENDIF}, vgVCLUtl;

const
  TimerID: array [1..4] of Char = 'vgTV';

{ TvgDBTreeDataLink }

type
  TvgDBTreeDataLink = class(TDataLink)
  private
    FTreeView: TvgCustomDBTreeView;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
  end;

procedure TvgDBTreeDataLink.ActiveChanged;
begin
  FTreeView.ActiveChanged;
end;

procedure TvgDBTreeDataLink.DataSetChanged;
begin
end;

{ TvgDBTreeNode }

destructor TvgDBTreeNode.Destroy;
begin
{$IFNDEF _D4_}
  StrDispose(FBookmark);
{$ELSE}
  FreeMem(FBookmark);
{$ENDIF}
  FBookmark := nil;
  inherited;
end;

function TvgDBTreeNode.GotoBookmark: Boolean;
var
  Tree: TvgCustomDBTreeView;
begin
  try
    Tree := TvgCustomDBTreeView(Owner.Owner);
    Tree.FDataLink.DataSet.GotoBookmark(Bookmark);
    Result := True;
  except Result := False; end;
end;

{ TvgCustomDBTreeView }
constructor TvgCustomDBTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TvgDBTreeDataLink.Create;
{$IFDEF _D5_}
  TvgDBTreeDataLink(FDataLink).VisualControl := True;
{$ENDIF}
  FRootID := NullValue;
  FStreamedRootID := NullValue;
  TvgDBTreeDataLink(FDataLink).FTreeView := Self;
end;

destructor TvgCustomDBTreeView.Destroy;
begin
  SetDataSource(nil);
  inherited;
end;

procedure TvgCustomDBTreeView.ActiveChanged;
begin
  if FDataLink.Active then
  with FDataLink.DataSet do
  begin
    FDataSetFilter := Filter;
    FDataSetFiltered := Filtered;
    UpdateNodes(nil);
  end else begin
    DestroyRoot;
    if Assigned(FDataLink.DataSet) then
    with FDataLink.DataSet do
    begin
      Filter := FDataSetFilter;
      Filtered := FDataSetFiltered;
    end;
  end;
end;

procedure TvgCustomDBTreeView.BeginChange;
begin
  if not FChanged then
  begin
    FChanged := True;
    Items.BeginUpdate;
  end;
end;

procedure TvgCustomDBTreeView.EndChange;
begin
  if FChanged then
  begin
    FChanged := False;
    Items.EndUpdate;
  end;
end;

function TvgCustomDBTreeView.CanUpdateNodes: Boolean;
begin
  Result := FDataLink.Active and HandleAllocated;
end;

function TvgCustomDBTreeView.CompareNode(Node: TTreeNode; NodeID: string): Boolean;
begin
  if Assigned(FOnCompareNode) then
    FOnCompareNode(Self, Node, NodeID, Result)
  else
    Result := TvgDBTreeNode(Node).ID = NodeID;
end;

function TvgCustomDBTreeView.CreateNode: TTreeNode;
begin
  Result := TvgDBTreeNode.Create(Items);
end;

procedure TvgCustomDBTreeView.CreateWnd;
begin
  inherited;
  UpdateNodes(nil);
end;

procedure TvgCustomDBTreeView.DestroyWnd;
begin
  StopTimer;
  DestroyRoot;
  inherited;
end;

procedure TvgCustomDBTreeView.Loaded;
begin
  inherited;
  try
    RootID := FStreamedRootID;
    FDataFields[0] := FStreamedDataFields[0];
    FDataFields[1] := FStreamedDataFields[1];
    FDataFields[2] := FStreamedDataFields[2];
    UpdateNodes(nil);
  except
    if csDesigning in ComponentState then
      Application.HandleException(Self)
    else
      raise;
  end;
end;

procedure TvgCustomDBTreeView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = GetDataSource) then
    SetDataSource(nil);
end;

function TvgCustomDBTreeView.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TvgCustomDBTreeView.GetField(Index: Integer): TField;
begin
  Result := nil;
  if FDataLink.Active then
    Result := DataSource.DataSet.FieldByName(FDataFields[Index]);
end;

procedure TvgCustomDBTreeView.SetDataField(Index: Integer; Value: string);
begin
  Value := AnsiUpperCase(Value);
  if (csLoading in ComponentState) then
    FStreamedDataFields[Index] := Value
  else if (FDataFields[Index] <> Value) then
  begin
    DestroyRoot;
    FDataFields[Index] := Value;
    UpdateNodes(nil);
  end;
end;

procedure TvgCustomDBTreeView.SetDataSource(Value: TDataSource);
begin
  if FDataLink.DataSource <> Value then
  begin
    FDataLink.DataSource := Value;
    if Assigned(Value) then FreeNotification(Value);
  end;
end;

procedure TvgCustomDBTreeView.SetDataSetFilter(Value: string);
begin
  if (FDataSetFilter <> Value) then
  begin
    FDataSetFilter := Value;
    UpdateNodes(nil);
  end;
end;

procedure TvgCustomDBTreeView.SetDataSetFiltered(Value: Boolean);
begin
  FDataSetFiltered := Value;
end;

procedure TvgCustomDBTreeView.SetRootID(Value: string);
begin
  if (csLoading in ComponentState) then
    FStreamedRootID := Value
  else if (FRootID <> Value) then
  begin
    DestroyRoot;
    FRootID := Value;
    UpdateNodes(nil);
  end;
end;

procedure TvgCustomDBTreeView.SetSortType(Value: TSortType);
begin
  if (FSortType <> Value) then
  begin
    FSortType := Value;
    if (FSortType in [stText, stBoth]) then AlphaSort;
  end;
end;

procedure TvgCustomDBTreeView.CNNotify(var Message: TWMNotify);
  function GetNodeFromItem(const Item: TTVItem): TTreeNode;
  begin
    with Item do
      if (state and TVIF_PARAM) <> 0 then Result := Pointer(lParam)
      else Result := Items.GetNode(hItem);
  end;
var
  Node: TTreeNode;
begin
  inherited;
  with Message.NMHdr^ do
    case code of
      TVN_ITEMEXPANDING:
        with PNMTreeView(Pointer(Message.NMHdr))^ do
        begin
          Node := GetNodeFromItem(ItemNew);
          if (Action = TVE_EXPAND) and (Message.Result <> 1) then
            UpdateNodes(Node);
        end;
    end;
end;

procedure TvgCustomDBTreeView.Collapse(Node: TTreeNode);
begin
  try
    DestroyBranches(Node);
    CreateBranches(Node, umFast);
    inherited;
  finally
    EndChange;
  end;
  inherited;
end;

procedure TvgCustomDBTreeView.CheckFields;
begin
  FDataLink.DataSet.FieldByName(DataFieldID);
  FDataLink.DataSet.FieldByName(DataFieldParentID);
  FDataLink.DataSet.FieldByName(DataFieldText);
end;

procedure TvgCustomDBTreeView.CreateBranches(Node: TTreeNode; UpdateMode: TUpdateMode);
var
  ANode: TTreeNode;
  FNodeID: string;
begin
  if not ProcessBranches(Node) then Exit;

  if Assigned(Node) and (UpdateMode = umFast) then
  begin
    Node.HasChildren := True;
    Exit;
  end;

  if Assigned(Node) then
    FNodeID := TvgDBTreeNode(Node).ID else
    FNodeID := FRootID;

  with FDataLink.DataSet do
  begin
    DisableControls;
    try
      DoSetRange(Node);
      First;
      while not EOF do
      begin
        if not Assigned(Node) or Node.IsVisible and Node.Expanded then BeginChange;
        ANode := Items.AddChild(Node, FieldByName(DataFieldText).AsString);
        TvgDBTreeNode(ANode).Bookmark := GetBookmark;
        TvgDBTreeNode(ANode).ID := FieldByName(DataFieldID).AsString;
        TvgDBTreeNode(ANode).ParentID := FNodeID;
        DoCreateNode(ANode, FDataLink.DataSet);
        DoUpdateNode(ANode);
        Next;
      end;
      if (FSortType in [stText, stBoth]) then
        if (Node = nil) then AlphaSort else Node.AlphaSort;
    finally
      EnableControls;
    end;
  end;
end;

procedure TvgCustomDBTreeView.CreateRoot;
begin
  if (csLoading in ComponentState) or not (FDataLink.Active)
    or (FDataFields[0] = '') or (FDataFields[1] = '') or (FDataFields[2] = '') then Exit;

  with DataSource.DataSet do
  begin
    CheckFields;
    try
      CreateBranches(nil, umFast);
    finally
      DoCancelRange(nil);
    end;
  end;
end;

procedure TvgCustomDBTreeView.DestroyBranches(Node: TTreeNode);
var
  TmpNode: TTreeNode;
begin
  if Node.IsVisible and Node.Expanded then BeginChange;
  TmpNode := Node.GetFirstChild;
  while Assigned(TmpNode) do
  begin
    DestroyNode(TmpNode);
    TmpNode := Node.GetFirstChild;
  end;
  Node.HasChildren := False;
end;

procedure TvgCustomDBTreeView.DestroyNode(Node: TTreeNode);
begin
  if Node.IsVisible then BeginChange;
  DoDestroyNode(Node);
  DestroyBranches(Node);
  Node.Delete;
end;

procedure TvgCustomDBTreeView.DestroyRoot;
var
  Child, NextChild: TTreeNode;
begin
  ClearSearch;
  if HandleAllocated then
  try
    Child := Items.GetFirstNode;
    while Assigned(Child) do
    begin
      NextChild := Child.GetNextSibling;
      DestroyNode(Child);
      Child := NextChild;
    end;
  finally
    EndChange;
  end;
end;

procedure TvgCustomDBTreeView.DefaultSetRange(DataSet: TDataSet; ParentID: string);
var
  Filter, DefaultFilter: string;
begin
  if (FDataSetFilter <> '') and (FDataSetFiltered) then
    DefaultFilter := FDataSetFilter + ' AND ' else DefaultFilter := '';

  if ParentID <> NullValue then ParentID := '''' + ParentID + '''';

  FmtStr(Filter, '%s(%s = %s)', [DefaultFilter, DataFieldParentID, ParentID]);

  SetFilter(FDataLink.DataSet, Filter, True);
end;

procedure TvgCustomDBTreeView.DefaultCancelRange(DataSet: TDataSet);
begin
  SetFilter(DataSet, FDataSetFilter, FDataSetFiltered);
end;

procedure TvgCustomDBTreeView.DoSetRange(Node: TTreeNode);
var
  ParentID: string;
  DataSet: TDataSet;
begin
  DataSet := FDataLink.DataSet;

  if Assigned(Node) then
    ParentID := TvgDBTreeNode(Node).ID else
    ParentID := FRootID;

  DataSet.DisableControls;
  try
    if not Assigned(FBmk) then
      FBmk := GetPKBookmark(DataSet, DataFieldID);
    if Assigned(FOnSetRange) then
      FOnSetRange(Self, DataSet, ParentID) else
      DefaultSetRange(DataSet, ParentID);
  finally
    DataSet.EnableControls;
  end;
end;

procedure TvgCustomDBTreeView.DoCancelRange(Node: TTreeNode);
begin
  FDataLink.DataSet.DisableControls;
  try
    if Assigned(FOnCancelRange) then
      FOnCancelRange(Self, FDataLink.DataSet) else
      DefaultCancelRange(FDataLink.DataSet);
    SetToPKBookmark(FDataLink.DataSet, FBmk);
  finally
    FreePKBookmark(FBmk);
    FDataLink.DataSet.EnableControls;
  end;
end;

procedure TvgCustomDBTreeView.DoCreateNode(Node: TTreeNode; DataSet: TDataSet);
begin
  if Assigned(FOnCreateNode) then FOnCreateNode(Self, Node, DataSet);
end;

procedure TvgCustomDBTreeView.DoDestroyNode(Node: TTreeNode);
begin
  if Assigned(FOnDestroyNode) then FOnDestroyNode(Self, Node);
end;

procedure TvgCustomDBTreeView.DoUpdateNode(Node: TTreeNode);
begin
  if Assigned(FOnUpdateNode) then FOnUpdateNode(Self, Node);
end;

procedure TvgCustomDBTreeView.InternalUpdateNode(Node: TTreeNode; UpdateMode: TUpdateMode);
begin
  if not CanUpdateNodes then Exit;
  if Assigned(Node) then
  begin
    DestroyBranches(Node);
    if ProcessBranches(Node) then
      CreateBranches(Node, UpdateMode);
  end else begin
    DestroyRoot;
    CreateRoot;
  end;
  DoUpdateNode(Node);
end;

function TvgCustomDBTreeView.LocateNode(StartNode: TTreeNode; NodeID: string): TTreeNode;
begin
  if Assigned(StartNode) then
  begin
    Result := StartNode.GetFirstChild;
    while Assigned(Result) do
      if CompareNode(Result, NodeID) then Exit
      else Result := StartNode.GetNextChild(Result);
  end else begin
    Result := Items.GetFirstNode;
    while Assigned(Result) do
      if CompareNode(Result, NodeID) then Exit
      else Result := Result.GetNextSibling;
  end;
  Result := nil;
end;

function TvgCustomDBTreeView.LookupNode(NodeID: string): TTreeNode;
  function LocateNext(ANodeID: string; var AParentID: string): Boolean;
  var
    Field: TField;
  begin
    Result := FDataLink.DataSet.Locate(FDataFields[0], ANodeID, []);
    if Result then
    begin
      Field := FDataLink.DataSet.FieldByName(FDataFields[1]);
      if not Field.IsNull then
        AParentID := Field.AsString else AParentID := NullValue;
    end;
  end;
var
  Nodes: TStrings;
  NodeFound: Boolean;
  ParentID: string;
begin
  Result := nil;
  Nodes := TStringList.Create;
  Items.BeginUpdate;
  try
    { Creating FIFO }
    repeat
      NodeFound := LocateNext(NodeID, ParentID);
      if NodeFound then
      begin
        Nodes.Insert(0, NodeID);
        NodeID := ParentID;
        if (ParentID = FRootID) then Break;
      end;
    until not NodeFound;

    { Processing FIFO }
    while Nodes.Count > 0 do
    begin
      Result := LocateNode(Result, Nodes[0]);
      if Assigned(Result) then
      begin
        if Nodes.Count > 1 then
        begin
          UpdateNodes(Result);
          Result.Expanded := True;
        end;
        Nodes.Delete(0);
      end else
        Break;
    end;
  finally
    Items.EndUpdate;
    Nodes.Free;
  end;
end;

function TvgCustomDBTreeView.LookupNodeEx(NodeID: string; Select: Boolean): TTreeNode;
begin
  Result := LookupNode(NodeID);
  if Select and Assigned(Result) then SelectNode(Result);
end;

function TvgCustomDBTreeView.LookupNodeText(const Text: string; Options: TLocateOptions; Select: Boolean): TTreeNode;
begin
  Result := nil;
  if (Text <> '') then
  begin
    if FDataLink.DataSet.Locate(DataFieldText, Text, Options) then
      Result := LookupNodeEx(FieldID.AsString, Select);
  end;
end;

function TvgCustomDBTreeView.UpdateCursorPos: Boolean;
begin
  Result := False;
  if Assigned(Selected) then
    Result := TvgDBTreeNode(Selected).GotoBookmark;
end;

function TvgCustomDBTreeView.ProcessBranches(Node: TTreeNode): Boolean;
begin
  Result := True;
  if Assigned(FOnProcessBranches) then FOnProcessBranches(Self, Node, Result);
end;

procedure TvgCustomDBTreeView.UpdateNode(Node: TTreeNode; UpdateMode: TUpdateMode);
begin
  try
    InternalUpdateNode(Node, UpdateMode);
  finally
    EndChange;
  end;
end;

procedure TvgCustomDBTreeView.UpdateNodes(Node: TTreeNode);
var
  I: Integer;
  Nodes: TList;
  TmpNode, SaveNode: TTreeNode;
begin
  if not CanUpdateNodes then Exit;
  AppSetCursor(crHourglass);
  SaveNode := Node;

  try
    InternalUpdateNode(Node, umNormal);
    Nodes := TList.Create;
    try
      if Assigned(Node) then
      begin
        TmpNode := Node.GetFirstChild;
        while Assigned(TmpNode) do
        begin
          Nodes.Add(TmpNode);
          TmpNode := Node.GetNextChild(TmpNode);
        end;
      end else begin
        Node := Items.GetFirstNode;
        while Assigned(Node) do
        begin
          Nodes.Add(Node);
          Node := Node.GetNextSibling;
        end;
      end;

      for I := 0 to Nodes.Count - 1 do
      begin
        Node := Nodes[I];
        InternalUpdateNode(Node, umFast)
      end;
    finally
      Nodes.Free;
    end;
  finally
    EndChange;
    if Assigned(SaveNode) then
      DoCancelRange(SaveNode) else
      DoCancelRange(nil);
    AppRestoreCursor;
  end;
end;

procedure TvgCustomDBTreeView.SelectNode(Node: TTreeNode);
begin
  Selected := Node;
  if Assigned(Node) then Node.Focused := True;
end;

procedure TvgCustomDBTreeView.StartTimer;
begin
  StopTimer;
  if (toQuickSearch in FOptions) then
  begin
    SetTimer(Handle, Longint(TimerID), 300, nil);
  end;
end;

procedure TvgCustomDBTreeView.StopTimer;
begin
  if HandleAllocated then
    KillTimer(Handle, Longint(TimerID));
end;

procedure TvgCustomDBTreeView.ClearSearch;
begin
  FSearchText := '';
  StopTimer;
end;

procedure TvgCustomDBTreeView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_ESCAPE, VK_PRIOR..VK_DOWN] then ClearSearch;
  inherited;
end;

procedure TvgCustomDBTreeView.KeyPress(var Key: Char);
begin
  if (toQuickSearch in FOptions) then
  begin
    case Key of
      #8:
        if Length(FSearchText) > 0 then System.Delete(FSearchText, Length(FSearchText), 1);
      #9, #27:
        FSearchText := '';
      #32..#255:
          FSearchText := FSearchText + Key;
    end;
    if (FSearchText <> '') then StartTimer else StopTimer;
    Key := #0;
  end;
  inherited;
end;

procedure TvgCustomDBTreeView.QuickSearch;
begin
  if (FSearchText <> '') and CanUpdateNodes then
  begin
    LookupNodeText(FSearchText, [loCaseInsensitive, loPartialKey], True);
    FSearchText := '';
  end;
end;

function TvgCustomDBTreeView.StoreRootID: Boolean;
begin
  Result := FRootID <> NullValue;
end;

procedure TvgCustomDBTreeView.WMLButtonDown(var Msg: TWMLButtonDown);
var
  Node: TTreeNode;
begin
  ClearSearch;
  Node := GetNodeAt(Msg.XPos, Msg.YPos);
  if not Assigned(Node) and (toEmptySelect in FOptions) then SelectNode(Node);
  inherited;
end;

procedure TvgCustomDBTreeView.WMLButtonDblClk(var Msg: TWMLButtonDown);
var
  Node: TTreeNode;
begin
  Node := GetNodeAt(Msg.XPos, Msg.YPos);
  if Assigned(Node) then
  begin
    if (Node.Count = 0) then
    begin
      try
        InternalUpdateNode(Node, umFast);
        inherited;
      finally
        EndChange;
      end;
    end else
      inherited;
  end;
end;

procedure TvgCustomDBTreeView.WMRButtonDown(var Msg: TWMRButtonDown);
var
  Node: TTreeNode;
begin
  ClearSearch;
  Node := GetNodeAt(Msg.XPos, Msg.YPos);
  if not Assigned(Node) and (toEmptySelect in FOptions) then SelectNode(Node);
  inherited;
end;

procedure TvgCustomDBTreeView.WMTimer(var Message: TWMTimer);
begin
  if Message.TimerID = LongInt(TimerID) then
  begin
    StopTimer;
    QuickSearch;
  end;
  inherited;
end;

end.
