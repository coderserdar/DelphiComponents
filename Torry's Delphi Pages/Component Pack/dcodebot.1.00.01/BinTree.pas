unit BinTree;

interface

{ TBinaryTree class }

type
  PBinaryNode = ^TBinaryNode;
  TBinaryNode = record
    Balance: Integer;
    Left: PBinaryNode;
    Right: PBinaryNode;
    Data: Pointer;
  end;

  TCompareResult = (crLessThan, crGreaterThan, crEqual);
  TCompareEvent = function (Sender: TObject; CompareData: Pointer;
    TreeData: Pointer): TCompareResult of object;

  TReleaseDataEvent = procedure (Sender: TObject; Data: Pointer);

  TBinaryTree = class(TAwareObject)
  private
    FCount: Integer;
    FRoot: PBinaryNode;
    FOnCompare: TCompareEvent;
    FOnReleaseData: TReleaseDataEvent;
  protected
    procedure DoReleaseData(Node: PBinaryNode);
  public
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Data: Pointer);
    procedure Remove(Data: Pointer);
    function Find(Data: Pointer): Pointer;
    property Count: Integer read FCount;
    property OnCompare: TCompareEvent read FOnCompare write FOnCompare;
    property OnReleaseData: TReleaseDataEvent read FOnReleaseData write FOnReleaseData;
  end;

implementation

{ TBinaryTree }

type
  PPBinaryNode = ^PBinaryNode;

destructor TBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TBinaryTree.Add(Data: Pointer);
var
  InsertNode: PBinaryNode;

  procedure InternalAdd(var Node: PBinaryNode);
  begin
    if Assigned(Node) then
    else
      Node := InsertNode;
  end;

begin
  New(InsertNode);
  FillChar(InsertNode^,SizeOf(TBinaryNode),#0);
  InsertNode.Data := Data;
  InternalAdd(FRoot);
  Inc(FCount);
end;

procedure TBinaryTree.DoReleaseData(Node: PBinaryNode);
begin
  if Assigned(FOnReleaseData) then
    FOnReleaseData(Self, Node.Data);
  Dispose(Node);
end;

procedure TBinaryTree.Remove(Data: Pointer);
begin

end;

procedure TBinaryTree.Clear;

  procedure InternalClear(Node: PBinaryNode);
  begin
    if Assigned(Node) then
    begin
      InternalClear(Node.Left);
      InternalClear(Node.Right);
      DoReleaseData(Node);
    end;
  end;

begin
  InternalClear(FRoot);
  FRoot := nil;
  FCount := 0;
end;

function TBinaryTree.Find(Data: Pointer): Pointer;

  function InternalFind(Node: PBinaryNode);
  begin
    if Assigned(Node) then
      if Assigned(FOnCompare) then
      case FOnCompare(Self,Data,Node.Data) of
        crLessThan:
          Result := InternalFind(Node.Left)
        crGreaterThan:
          Result := InternalFind(Node.Right)
      end
      else
        Result := nil;
    Result := Node;
  end;

begin
  Result := InternalFind(FRoot);
  if Assigned(Result) then
    Result := PBinaryNode(Result).Data;
end;

end.
