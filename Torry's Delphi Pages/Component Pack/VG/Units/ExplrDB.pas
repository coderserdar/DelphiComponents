{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Explorer library: DB Tree                     }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit ExplrDB;

interface
uses Classes, Explorer, DB, Variants;

type
  TExplorerDBTreeNode = class;

  TSetRangeEvent = procedure (ExplorerNodes: TExplorerNodes; DataSet: TDataSet;
    ParentID: string) of object;
  TCancelRangeEvent = procedure (Sender: TObject; DataSet: TDataSet) of object;
  THasBranchesEvent = procedure (Sender: TObject; ExplorerNodes: TExplorerDBTreeNode;
    var HasBranches: Boolean) of object;

  TFindNodeOption = (foCreateOnNeed, foAcceptParent, foSelect);
  TFindNodeOptions = set of TFindNodeOption;

{ TExplorerDBTreeRootNode }
  TExplorerDBTreeRootNode = class(TExplorerFolderNode)
  private
    FBmk: Pointer;
    FDataSetFilter: string;
    FDataSetFiltered: Boolean;
    FDataLink: TDataLink;
    FDataFields: array[0..2] of string;
    FStreamedDataFields: array[0..2] of string;
    FRootID, FStreamedRootID: string;
    FUpdateCursorPos: Boolean;
    FOnSetRange: TSetRangeEvent;
    FOnCancelRange: TCancelRangeEvent;
    FOnHasBranches: THasBranchesEvent;
    procedure CheckFields;
    procedure GetChildrenParams(Sender: TObject; ExplorerNodes: TExplorerNodes);
    function GetDataSource: TDataSource;
    function GetField(Index: Integer): TField;
    function IsRootIDStored: Boolean;
    procedure SetDataField(Index: Integer; Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetRootID(Value: string);
  protected
    procedure ActiveChanged;
    procedure DefaultSetRange(DataSet: TDataSet; ParentID: string); virtual;
    procedure DefaultCancelRange(DataSet: TDataSet);
    procedure DoSetRange(ExplorerNodes: TExplorerNodes; const ParentID: string); virtual;
    procedure DoCancelRange(ExplorerNodes: TExplorerNodes); virtual;
    procedure DoCreateNode(ExplorerNodes: TExplorerNodes); virtual;
    procedure DoCreateNodes(ExplorerNodes: TExplorerNodes; const ParentID: string);
    function HasBranches(ExplorerNodes: TExplorerDBTreeNode): Boolean;
    procedure InternalExpand; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanExpand(NodeTypes: TExplorerNodeTypes): Boolean; override;
    function FindDBTreeNode(Root: TExplorerNodes; ID: string;
      Options: TFindNodeOptions): TExplorerDBTreeNode;
    function GotoBookmark(ExplorerNodes: TExplorerDBTreeNode): Boolean;
    property FieldID: TField index 0 read GetField;
    property FieldParentID: TField index 1 read GetField;
    property FieldText: TField index 2 read GetField;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataFieldID: string index 0 read FDataFields[0] write SetDataField;
    property DataFieldParentID: string index 1 read FDataFields[1] write SetDataField;
    property DataFieldText: string index 2 read FDataFields[2] write SetDataField;
    property RootID: string read FRootID write SetRootID stored IsRootIDStored;
    property UpdateCursorPos: Boolean read FUpdateCursorPos write FUpdateCursorPos default False;
    property OnSetRange: TSetRangeEvent read FOnSetRange write FOnSetRange;
    property OnCancelRange: TCancelRangeEvent read FOnCancelRange write FOnCancelRange;
    property OnHasBranches: THasBranchesEvent read FOnHasBranches write FOnHasBranches;
  end;

{ TExplorerDBTreeNode }
  TExplorerDBTreeNode = class(TExplorerFolderNode)
  private
    FID: string;
    FBookmark: TBookmark;
  protected
    procedure InternalExpand; override;
    procedure InternalSelect; override;
  public
    destructor Destroy; override;
    function CanExpand(NodeTypes: TExplorerNodeTypes): Boolean; override;
    function GotoBookmark: Boolean;
    function IsStored: Boolean; override;
    property ID: string read FID;
    property Bookmark: TBookmark read FBookmark;
  end;

const
  NullValue = 'Null';

implementation
uses SysUtils, Forms, vgDBUtl;

type
  TDBTreeDataLink = class(TDataLink)
  private
    FRoot: TExplorerDBTreeRootNode;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
  end;

procedure TDBTreeDataLink.ActiveChanged;
begin
  FRoot.ActiveChanged;
end;

procedure TDBTreeDataLink.DataSetChanged;
begin
end;

{ TExplorerDBTreeRootNode }
constructor TExplorerDBTreeRootNode.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TDBTreeDataLink.Create;
  FRootID := NullValue;
  FStreamedRootID := NullValue;
  TDBTreeDataLink(FDataLink).FRoot := Self;
end;

destructor TExplorerDBTreeRootNode.Destroy;
begin
  SetDataSource(nil);
  FDataLink.Free;
  inherited;
end;

procedure TExplorerDBTreeRootNode.ActiveChanged;
begin
  Expanded := FDataLink.Active;
end;

function TExplorerDBTreeRootNode.CanExpand(NodeTypes: TExplorerNodeTypes): Boolean;
begin
  Result := FDataLink.Active;
end;

procedure TExplorerDBTreeRootNode.CheckFields;
begin
  FDataLink.DataSet.FieldByName(DataFieldID);
  FDataLink.DataSet.FieldByName(DataFieldParentID);
  FDataLink.DataSet.FieldByName(DataFieldText);
end;

procedure TExplorerDBTreeRootNode.DefaultSetRange(DataSet: TDataSet; ParentID: string);
var
  Filter, DefaultFilter: string;
begin
  if (FDataSetFilter <> '') and (FDataSetFiltered) then
    DefaultFilter := FDataSetFilter + ' and ' else DefaultFilter := '';

  if ParentID <> NullValue then ParentID := '''' + ParentID + '''';

  FmtStr(Filter, '%s(%s = %s)', [DefaultFilter, DataFieldParentID, ParentID]);

  SetFilter(FDataLink.DataSet, Filter, True);
end;

procedure TExplorerDBTreeRootNode.DefaultCancelRange(DataSet: TDataSet);
begin
  SetFilter(DataSet, FDataSetFilter, FDataSetFiltered);
end;

procedure TExplorerDBTreeRootNode.DoSetRange(ExplorerNodes: TExplorerNodes; const ParentID: string);
begin
  with FDataLink do
  begin
    DataSet.DisableControls;
    try
      FBmk := GetPKBookmark(DataSet, DataFieldID);
      if Assigned(FOnSetRange) then
        FOnSetRange(ExplorerNodes, DataSet, ParentID)
      else
        DefaultSetRange(DataSet, ParentID);
    finally
      DataSet.EnableControls;
    end;
  end;
end;

procedure TExplorerDBTreeRootNode.DoCancelRange(ExplorerNodes: TExplorerNodes);
begin
  with FDataLink do
  begin
    DataSet.DisableControls;
    try
      if Assigned(FOnCancelRange) then
        FOnCancelRange(Self, DataSet)
      else
        DefaultCancelRange(DataSet);
      SetToPKBookmark(DataSet, FBmk);
    finally
      FreePKBookmark(TPKBookmark(FBmk));
      DataSet.EnableControls;
    end;
  end;
end;

procedure TExplorerDBTreeRootNode.DoCreateNode(ExplorerNodes: TExplorerNodes);
var
  Child: TExplorerDBTreeNode;
begin
  Child := TExplorerDBTreeNode.Create(Self);
  try
    Child.FID := FieldID.AsString;
    Child.Text := FieldText.AsString;
    Child.FBookmark := FDataLink.DataSet.GetBookmark;
    Child.Sorted := Sorted;
    Child.Parent := ExplorerNodes;
    Child.OnGetChildrenParams := GetChildrenParams;
  except
    Free;
    raise;
  end;
end;

procedure TExplorerDBTreeRootNode.DoCreateNodes(ExplorerNodes: TExplorerNodes; const ParentID: string);
begin
  with FDataLink do
  begin
    DataSet.DisableControls;
    try
      DoSetRange(ExplorerNodes, ParentID);
      try
        DataSet.First;
        while not DataSet.EOF do
        begin
          DoCreateNode(ExplorerNodes);
          DataSet.Next;
        end;
      finally
        DoCancelRange(Self);
      end;
    finally
      DataSet.EnableControls;
    end;
  end;
end;

function TExplorerDBTreeRootNode.FindDBTreeNode(Root: TExplorerNodes; ID: string;
  Options: TFindNodeOptions): TExplorerDBTreeNode;
var
  Field: TField;

  function DataSetLocate(const ID: string; var AParentID: string): Boolean;
  begin
    Result := FDataLink.DataSet.Locate(FDataFields[0], ID, []);
    if Result then
    begin
      if not Field.IsNull then
        AParentID := Field.AsString else
        AParentID := NullValue;
    end;
  end;

  function CompareID(ExplorerNodes: TExplorerNodes; Data: Pointer): Boolean;
  begin
    Result := TExplorerDBTreeNode(ExplorerNodes).ID = string(Data);
  end;

var
  Nodes: TStrings;
  I: Integer;
  ParentID: string;
  Bmk: TPKBookmark;
begin
  Result := nil;
  if FDataLink.Active then
  begin
    FDataLink.DataSet.DisableControls;
    try
      Bmk := GetPKBookmark(FDataLink.DataSet, DataFieldID);
      try
        if foCreateOnNeed in Options then
        begin
          Field := FieldParentID;
          Nodes := TStringList.Create;
          try
            { FIFO - forward pass }
            while DataSetLocate(ID, ParentID) do
            begin
              Nodes.Add(ID);
              ID := ParentID;
              if (ParentID = FRootID) then Break;
            end;

            { FIFO - backward pass }
            for I := Nodes.Count - 1 downto 0 do
            begin
              Root.Expand;
              Root := FindChildExplorerNode(Root, Pointer(Nodes[I]), @CompareID);
              if Assigned(Root) then
                Result := TExplorerDBTreeNode(Root) else
                Break;
            end;
          finally
            Nodes.Free;
          end;
        end else
          Result := TExplorerDBTreeNode(FindExplorerNode(Root, Root <> Self, Pointer(ID), @CompareID));

        if not CompareID(Result, Pointer(ID)) and not (foAcceptParent in Options) then
          Result := nil;

        if (foSelect in Options) and Assigned(Result) then
        begin
          Result.Select;
          if not FUpdateCursorPos then Result.GotoBookmark;
        end else
          SetToPKBookmark(FDataLink.DataSet, Bmk);
      finally
        FreePKBookmark(Bmk);
      end;
    finally
      FDataLink.DataSet.EnableControls;
    end;
  end;
end;

procedure TExplorerDBTreeRootNode.GetChildrenParams(Sender: TObject; ExplorerNodes: TExplorerNodes);
begin
  DoGetChildrenParams(ExplorerNodes);
end;

function TExplorerDBTreeRootNode.GotoBookmark(ExplorerNodes: TExplorerDBTreeNode): Boolean;
begin
  try
    FDataLink.DataSet.GotoBookmark(ExplorerNodes.Bookmark);
    Result := True;
  except
    Result := False;
  end;
end;

function TExplorerDBTreeRootNode.HasBranches(ExplorerNodes: TExplorerDBTreeNode): Boolean;
begin
  Result := True;
  if Assigned(FOnHasBranches) then FOnHasBranches(Self, ExplorerNodes, Result);
end;

procedure TExplorerDBTreeRootNode.InternalExpand;
begin
  if FDataLink.Active then
  begin
    CheckFields;
    DoCreateNodes(Self, FRootID);
  end;
end;

function TExplorerDBTreeRootNode.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TExplorerDBTreeRootNode.GetField(Index: Integer): TField;
begin
  Result := nil;
  if FDataLink.Active then
    Result := DataSource.DataSet.FieldByName(FDataFields[Index]);
end;

function TExplorerDBTreeRootNode.IsRootIDStored: Boolean;
begin
  Result := not VarIsNull(FRootID);
end;

procedure TExplorerDBTreeRootNode.Loaded;
begin
  inherited;
  try
    FRootID := FStreamedRootID;
    FDataFields[0] := FStreamedDataFields[0];
    FDataFields[1] := FStreamedDataFields[1];
    FDataFields[2] := FStreamedDataFields[2];
  except
    if csDesigning in ComponentState then
      Application.HandleException(Self)
    else
      raise;
  end;
end;

procedure TExplorerDBTreeRootNode.SetDataField(Index: Integer; Value: string);
begin
  Value := AnsiUpperCase(Value);
  if (csLoading in ComponentState) then
    FStreamedDataFields[Index] := Value
  else if (FDataFields[Index] <> Value) then
  begin
    BeginExpand;
    try
      Clear;
      FDataFields[Index] := Value;
      ActiveChanged;
    finally
      EndExpand;
    end;
  end;
end;

procedure TExplorerDBTreeRootNode.SetDataSource(Value: TDataSource);
begin
  if FDataLink.DataSource <> Value then
  begin
    FDataLink.DataSource := Value;
    if Assigned(Value) then FreeNotification(Value);
  end;
end;

procedure TExplorerDBTreeRootNode.SetRootID(Value: string);
begin
  if (csLoading in ComponentState) then
    FStreamedRootID := Value
  else if (FRootID <> Value) then
  begin
    BeginExpand;
    try
      Clear;
      FRootID := Value;
      ActiveChanged;
    finally
      EndExpand;
    end;
  end;
end;

{ TExplorerDBTreeNode }
destructor TExplorerDBTreeNode.Destroy;
begin
{$IFNDEF _D4_}
  StrDispose(FBookmark);
{$ELSE}
  FreeMem(FBookmark);
{$ENDIF}
  FBookmark := nil;
  inherited;
end;

function TExplorerDBTreeNode.CanExpand(NodeTypes: TExplorerNodeTypes): Boolean;
begin
  Result := TExplorerDBTreeRootNode(Owner).HasBranches(Self);
end;

function TExplorerDBTreeNode.GotoBookmark: Boolean;
begin
  Result := TExplorerDBTreeRootNode(Owner).GotoBookmark(Self);
end;

procedure TExplorerDBTreeNode.InternalExpand;
begin
  TExplorerDBTreeRootNode(Owner).DoCreateNodes(Self, FID);
end;

procedure TExplorerDBTreeNode.InternalSelect;
begin
  if TExplorerDBTreeRootNode(Owner).FUpdateCursorPos then GotoBookmark;
end;

function TExplorerDBTreeNode.IsStored: Boolean;
begin
  Result := False;
end;

end.
