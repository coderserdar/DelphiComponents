{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmDGT
Purpose  : To have a non-visual Directed Graph Tree component.
Date     : 04-10-2001
Author   : Ryan J. Mills
Version  : 1.90
Notes    : This unit was partially based upon the work of Patrick O'Keeffe.
================================================================================}

unit rmDGT;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  { TTreeNode }
  TAddMode = (taAddFirst, taAdd, taInsert);
  TNodeAttachMode = (naAdd, naAddFirst, naAddChild, naAddChildFirst, naInsert);

  PDGNodeInfo = ^TDGNodeInfo;
  TDGNodeInfo = packed record
    Count: Integer;
    Index : integer;
    Text: Char;
  end;

  TrmCustomDGTree = class;
  TrmDGTreeNodes = class;
  TrmDGTreeNode = class;

  TrmDGTreeNode = class(TPersistent)
  private
    FOwner: TrmDGTreeNodes;
    FText: Char;
    FData: Integer;
    FChildList : TList;
    FDeleting: Boolean;
    FParent: TrmDGTreeNode;
    function GetLevel: Integer;
    function GetParent: TrmDGTreeNode;
    procedure SetParent(Value : TrmDGTreeNode);
    function GetChildren: Boolean;
    function GetIndex: Integer;
    function GetItem(Index: Integer): TrmDGTreeNode;
    function GetCount: Integer;
    function GeTrmDGTree: TrmCustomDGTree;
    function IsEqual(Node: TrmDGTreeNode): Boolean;
    procedure ReadData(Stream: TStream; Info: PDGNodeInfo);
    procedure SetData(Value: Integer);
    procedure SetItem(Index: Integer; Value: TrmDGTreeNode);
    procedure SetText(const S: Char);
    procedure WriteData(Stream: TStream; Info: PDGNodeInfo);
  public
    constructor Create(AOwner: TrmDGTreeNodes);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Delete;
    procedure DeleteChildren;
    function GetFirstChild: TrmDGTreeNode;
    function GetLastChild: TrmDGTreeNode;
    function GetNext: TrmDGTreeNode;
    function GetNextChild(Value: TrmDGTreeNode): TrmDGTreeNode;
    function GetNextSibling: TrmDGTreeNode;
    function GetPrev: TrmDGTreeNode;
    function GetPrevChild(Value: TrmDGTreeNode): TrmDGTreeNode;
    function getPrevSibling: TrmDGTreeNode;
    function HasAsParent(Value: TrmDGTreeNode): Boolean;
    function IndexOf(Value: TrmDGTreeNode): Integer;
    function MoveTo(Destination: TrmDGTreeNode; Mode: TNodeAttachMode):TrmDGTreeNode;

    property Count: Integer read GetCount;
    property Data: Integer read FData write SetData;
    property Deleting: Boolean read FDeleting;
    property HasChildren: Boolean read GetChildren;
    property Index: Integer read GetIndex;
    property Item[Index: Integer]: TrmDGTreeNode read GetItem write SetItem; default;
    property Level: Integer read GetLevel;
    property Owner: TrmDGTreeNodes read FOwner;
    property Parent: TrmDGTreeNode read GetParent write SetParent;
    property DGTree: TrmCustomDGTree read GeTrmDGTree;
    property Text: Char read FText write SetText;
  end;

  TrmDGTreeNodes = class(TPersistent)
  private
    FOwner: TrmCustomDGTree;
    FRootNodeList : TList;
    function GetNodeFromIndex(Index: Integer): TrmDGTreeNode;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  protected
    function InternalAddObject(Node: TrmDGTreeNode; const S: Char;
      Ptr: Integer; AddMode: TAddMode): TrmDGTreeNode;
    procedure DefineProperties(Filer: TFiler); override;
    function GetCount: Integer;
    procedure SetItem(Index: Integer; Value: TrmDGTreeNode);
  public
    constructor Create(AOwner: TrmCustomDGTree);
    destructor Destroy; override;
    function AddChildFirst(Node: TrmDGTreeNode; const S: Char): TrmDGTreeNode;
    function AddChild(Node: TrmDGTreeNode; const S: Char): TrmDGTreeNode;
    function AddChildObjectFirst(Node: TrmDGTreeNode; const S: Char;
      Ptr: Integer): TrmDGTreeNode;
    function AddChildObject(Node: TrmDGTreeNode; const S: Char;
      Ptr: Integer): TrmDGTreeNode;
    function AddFirst(Node: TrmDGTreeNode; const S: Char): TrmDGTreeNode;
    function Add(Node: TrmDGTreeNode; const S: Char): TrmDGTreeNode;
    function AddObjectFirst(Node: TrmDGTreeNode; const S: Char;
      Ptr: Integer): TrmDGTreeNode;
    function AddObject(Node: TrmDGTreeNode; const S: Char;
      Ptr: Integer): TrmDGTreeNode;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure Delete(Node: TrmDGTreeNode);
    function GetFirstNode: TrmDGTreeNode;
    function Insert(Node: TrmDGTreeNode; const S: Char): TrmDGTreeNode;
    function InsertObject(Node: TrmDGTreeNode; const S: Char;
      Ptr: Integer): TrmDGTreeNode;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TrmDGTreeNode read GetNodeFromIndex; default;
    property Owner: TrmCustomDGTree read FOwner;
  end;

{ TDGCustomDGTree }

  TrmDGTreeEvent = procedure(Sender: TObject; Node: TrmDGTreeNode) of object;
  EDGTreeError = class(Exception);

  TrmCustomDGTree = class(TComponent)
  private
    FMemStream: TMemoryStream;
    FTreeNodes: TrmDGTreeNodes;
    FOnDeletion: TrmDGTreeEvent;
    procedure SetrmDGTreeNodes(Value: TrmDGTreeNodes);
  protected
    function CreateNode: TrmDGTreeNode; virtual;
    procedure Delete(Node: TrmDGTreeNode); dynamic;
    property Items: TrmDGTreeNodes read FTreeNodes write SeTrmDGTreeNodes;
    property OnDeletion: TrmDGTreeEvent read FOnDeletion write FOnDeletion;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TrmDGTree = class(TrmCustomDGTree)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property Items;
    property OnDeletion;
  end;

implementation

procedure DGTreeError(const Msg: string);
begin
  raise EDGTreeError.Create(Msg);
end;

constructor TrmDGTreeNode.Create(AOwner: TrmDGTreeNodes);
begin
  inherited Create;
  FOwner := AOwner;
  FChildList := TList.Create;
end;

destructor TrmDGTreeNode.Destroy;
begin
  FDeleting := True;
  FChildList.Free;
  inherited Destroy;
end;

function TrmDGTreeNode.GeTrmDGTree: TrmCustomDGTree;
begin
  Result := Owner.Owner;
end;

function TrmDGTreeNode.HasAsParent(Value: TrmDGTreeNode): Boolean;
begin
  if Value <> Nil then
  begin
    if Parent = nil then
      Result := False
    else
      if Parent = Value then
        Result := True
      else
        Result := Parent.HasAsParent(Value);
  end
  else
    Result := True;
end;

procedure TrmDGTreeNode.SetText(const S: Char);
begin
  FText := S;
end;

procedure TrmDGTreeNode.SetData(Value: Integer);
begin
  FData := Value;
end;

function TrmDGTreeNode.GetChildren: Boolean;
begin
  Result := FChildList.Count > 0;
end;

function TrmDGTreeNode.GetParent: TrmDGTreeNode;
begin
  Result := FParent;
end;

procedure TrmDGTreeNode.SetParent(Value : TrmDGTreeNode);
begin
  FParent := Value;
end;


function TrmDGTreeNode.GetNextSibling: TrmDGTreeNode;
var
  CurIdx : Integer;

begin
  if Parent <> nil then
  begin
    CurIdx := Parent.FChildList.IndexOf(Self);
    if (CurIdx + 1) < Parent.FChildList.Count then
      Result := Parent.FChildList.Items[CurIdx + 1]
    else
      Result := nil;
  end
  else
  begin
    CurIdx := Owner.FRootNodeList.IndexOf(Self);
    if (CurIdx + 1) < Owner.FRootNodeList.Count then
      Result := Owner.FRootNodeList.Items[CurIdx + 1]
    else
      Result := nil;
  end;    
end;

function TrmDGTreeNode.GetPrevSibling: TrmDGTreeNode;
var
  CurIdx : Integer;
begin
  if Parent <> nil then
  begin
    CurIdx := Parent.FChildList.IndexOf(Self);
    if (CurIdx - 1) < 0 then
      Result := Parent.FChildList.Items[CurIdx - 1]
    else
      Result := nil;
  end
  else
  begin
    CurIdx := Owner.FRootNodeList.IndexOf(Self);
    if (CurIdx - 1) < Owner.FRootNodeList.Count then
      Result := Owner.FRootNodeList.Items[CurIdx - 1]
    else
      Result := nil;
  end;
end;

function TrmDGTreeNode.GetNextChild(Value: TrmDGTreeNode): TrmDGTreeNode;
begin
  if Value <> nil then
    Result := Value.GetNextSibling
  else
    Result := nil;
end;

function TrmDGTreeNode.GetPrevChild(Value: TrmDGTreeNode): TrmDGTreeNode;
begin
  if Value <> nil then
    Result := Value.GetPrevSibling
  else
    Result := nil;
end;

function TrmDGTreeNode.GetFirstChild: TrmDGTreeNode;
begin
  if FChildList.Count > 0 then
  begin
    Result := FChildList.Items[0];
  end
  else
    Result := nil;
end;

function TrmDGTreeNode.GetLastChild: TrmDGTreeNode;
begin
  if FChildList.Count > 0 then
  begin
    Result := FChildList.Items[FChildList.Count - 1]
  end
  else
    Result := nil;
end;

function TrmDGTreeNode.GetNext: TrmDGTreeNode;
var
  N : TrmDGTreeNode;
  P : TrmDGTreeNode;

begin
  if HasChildren then
    N := GetFirstChild
  else
  begin
    N := GetNextSibling;
    if N = nil then
    begin
      P := Parent;
      while P <> nil do
      begin
        N := P.GetNextSibling;
        if N <> nil then
          Break;
        P := P.Parent;
      end;
    end;
  end;
  Result := N;
end;

function TrmDGTreeNode.GetPrev: TrmDGTreeNode;
var
  Node: TrmDGTreeNode;

begin
  Result := GetPrevSibling;
  if Result <> nil then
  begin
    Node := Result;
    repeat
      Result := Node;
      Node := Result.GetLastChild;
    until Node = nil;
  end
  else
    Result := Parent;
end;

function TrmDGTreeNode.GetIndex: Integer;
var
  Node: TrmDGTreeNode;

begin
  Result := -1;
  Node := Self;
  while Node <> nil do
  begin
    Inc(Result);
    Node := Node.GetPrevSibling;
  end;
end;

function TrmDGTreeNode.GetItem(Index: Integer): TrmDGTreeNode;
begin
  Result := GetFirstChild;
  while (Result <> nil) and (Index > 0) do
  begin
    Result := GetNextChild(Result);
    Dec(Index);
  end;
  if Result = nil then DGTreeError('List Index Out of Bounds');
end;

procedure TrmDGTreeNode.SetItem(Index: Integer; Value: TrmDGTreeNode);
begin
  item[Index].Assign(Value);
end;

function TrmDGTreeNode.IndexOf(Value: TrmDGTreeNode): Integer;
var
  Node: TrmDGTreeNode;
begin
  Result := -1;
  Node := GetFirstChild;
  while (Node <> nil) do
  begin
    Inc(Result);
    if Node = Value then Break;
    Node := GetNextChild(Node);
  end;
  if Node = nil then
    Result := -1;
end;

function TrmDGTreeNode.MoveTo(Destination: TrmDGTreeNode; Mode: TNodeAttachMode) : TrmDGTreeNode;
{var
  AddMode : TAddMode;
  node    : TrmDGTreeNode;
 }
begin
  Result := nil;
{  if (Destination = nil) or not Destination.HasAsParent(Self) then
    begin
      AddMode := taAdd;
      if (Destination <> nil) and not (Mode in [naAddChild, naAddChildFirst]) then
        Node := Destination.Parent else
        Node := Destination;
      case Mode of
        naAdd,
        naAddChild: AddMode := taAdd;
        naAddFirst,
        naAddChildFirst: AddMode := taAddFirst;
        naInsert:
          begin
            Destination := Destination.GetPrevSibling;
            if Destination = nil then AddMode := taAddFirst
            else AddMode := taInsert;
          end;
      end;

      result := owner.InternalAddObject(Destination, Text, data, AddMode);
      delete;
    end
  else
    result:=self;}
end;

function TrmDGTreeNode.GetCount: Integer;
var
  Node: TrmDGTreeNode;

begin
  Result := 0;
  Node := GetFirstChild;
  while Node <> nil do
  begin
    Inc(Result);
    Node := Node.GetNextChild(Node);
  end;
end;

function TrmDGTreeNode.GetLevel: Integer;
var
  Node: TrmDGTreeNode;

begin
  Result := 0;
  Node := Parent;
  while Node <> nil do
  begin
    Inc(Result);
    Node := Node.Parent;
  end;
end;

procedure TrmDGTreeNode.Delete;
begin
  if HasChildren then
    DeleteChildren;

  TrmCustomDGTree(Owner.Owner).Delete(Self);
  if Parent <> nil then
  begin
    Parent.FChildList.Delete(Parent.FChildList.IndexOf(Self));
    Parent.FChildList.Pack;
  end
  else
  begin
    Owner.FRootNodeList.Delete(Owner.FRootNodeList.IndexOf(Self));
    Owner.FRootNodeList.Pack;
  end;
  Free;
end;

procedure TrmDGTreeNode.DeleteChildren;
var
  Node: TrmDGTreeNode;

begin
  Node := GetFirstChild;
  while Node <> nil do
  begin
    Node.Delete;
    Node := GetFirstChild;
  end;
end;

procedure TrmDGTreeNode.Assign(Source: TPersistent);
var
  Node: TrmDGTreeNode;

begin
  if Source is TrmDGTreeNode then
  begin
    Node := TrmDGTreeNode(Source);
    Text := Node.Text;
    Data := Node.Data;
  end
  else
    inherited Assign(Source);
end;

function TrmDGTreeNode.IsEqual(Node: TrmDGTreeNode): Boolean;
begin
  Result := (Text = Node.Text) and (Data = Node.Data);
end;

procedure TrmDGTreeNode.ReadData(Stream: TStream; Info: PDGNodeInfo);
var
  I, Size, ItemCount: Integer;

begin
  Stream.ReadBuffer(Size, SizeOf(Size));
  Stream.ReadBuffer(Info^, Size);
  Text := Info^.Text;
  ItemCount := Info^.Count;
  Data := Info^.Index;
  for I := 0 to ItemCount - 1 do
    Owner.AddChild(Self, #0).ReadData(Stream, Info);
end;

procedure TrmDGTreeNode.WriteData(Stream: TStream; Info: PDGNodeInfo);
var
  I,
  Size,
  ItemCount: Integer;

begin
  Size := SizeOf(TDGNodeInfo);
  Info^.Text := Text;
  ItemCount := Count;
  Info^.Count := ItemCount;
  Info^.Index := Data;
  Stream.WriteBuffer(Size, SizeOf(Size));
  Stream.WriteBuffer(Info^, Size);
  for I := 0 to ItemCount - 1 do
    Item[I].WriteData(Stream, Info);
end;

{ TrmDGTreeNodes }

constructor TrmDGTreeNodes.Create(AOwner: TrmCustomDGTree);
begin
  inherited Create;
  FOwner := AOwner;
  FRootNodeList := TList.Create;
end;

destructor TrmDGTreeNodes.Destroy;
begin
  Clear;
  FRootNodeList.Free;
  inherited Destroy;
end;

function TrmDGTreeNodes.GetCount: Integer;
var
  N : TrmDGTreeNode;
begin
  N := GetFirstNode;
  Result := 0;
  while N <> nil do
  begin
    Result := Result + 1;
    N := N.GetNext;
  end;
end;

procedure TrmDGTreeNodes.Delete(Node: TrmDGTreeNode);
begin
  Node.Delete;
end;

procedure TrmDGTreeNodes.Clear;
var
  N : TrmDGTreeNode;

begin
  N := GetFirstNode;
  While N <> nil do
  begin
    N.Delete;
    N := GetFirstNode;
  end;  
end;

function TrmDGTreeNodes.AddChildFirst(Node: TrmDGTreeNode; const S: Char): TrmDGTreeNode;
begin
  Result := AddChildObjectFirst(Node, S, -1);
end;

function TrmDGTreeNodes.AddChildObjectFirst(Node: TrmDGTreeNode; const S: Char;
  Ptr: Integer): TrmDGTreeNode;
begin
  Result := InternalAddObject(Node, S, Ptr, taAddFirst);
end;

function TrmDGTreeNodes.AddChild(Node: TrmDGTreeNode; const S: char): TrmDGTreeNode;
begin
  Result := AddChildObject(Node, S, -1);
end;

function TrmDGTreeNodes.AddChildObject(Node: TrmDGTreeNode; const S: char;
  Ptr: integer): TrmDGTreeNode;
begin
  Result := InternalAddObject(Node, S, Ptr, taAdd);
end;

function TrmDGTreeNodes.AddFirst(Node: TrmDGTreeNode; const S: char): TrmDGTreeNode;
begin
  Result := AddObjectFirst(Node, S, -1);
end;

function TrmDGTreeNodes.AddObjectFirst(Node: TrmDGTreeNode; const S: char;
  Ptr: integer): TrmDGTreeNode;
begin
  if Node <> nil then Node := Node.Parent;
  Result := InternalAddObject(Node, S, Ptr, taAddFirst);
end;

function TrmDGTreeNodes.Add(Node: TrmDGTreeNode; const S: char): TrmDGTreeNode;
begin
  Result := AddObject(Node, S, -1);
end;

function TrmDGTreeNodes.AddObject(Node: TrmDGTreeNode; const S: char;
  Ptr: integer): TrmDGTreeNode;
begin
  if Node <> nil then Node := Node.Parent;
  Result := InternalAddObject(Node, S, Ptr, taAdd);
end;

function TrmDGTreeNodes.Insert(Node: TrmDGTreeNode; const S: char): TrmDGTreeNode;
begin
  Result := InsertObject(Node, S, -1);
end;

function TrmDGTreeNodes.InsertObject(Node: TrmDGTreeNode; const S: char; Ptr: Integer): TrmDGTreeNode;
var
  Parent : TrmDGTreeNode;
  AddMode : TAddMode;

begin
  AddMode := taInsert;
  if Node <> nil then
  begin
    Parent := Node.Parent;
    if Parent <> nil then
      Node := Node.GetPrevSibling;
    if Node = nil then
      AddMode := taAddFirst;
  end;
  Result := InternalAddObject(Node, S, Ptr, AddMode);
end;


function TrmDGTreeNodes.InternalAddObject(Node: TrmDGTreeNode; const S: char;
  Ptr: integer; AddMode: TAddMode): TrmDGTreeNode;
begin
  Result := Owner.CreateNode;
  try
    case AddMode of
      taAddFirst:
        begin
          if Node = nil then
          begin
            FRootNodeList.Insert(0, Result);
            Result.Parent := nil;
          end
          else
          begin
            Node.FChildList.Insert(0, Result);
            Result.Parent := Node;
          end;
          try
            Result.Data := Ptr;
            Result.Text := S;
          except
            raise;
          end;
        end;

      taAdd:
        begin
          if Node = nil then
          begin
            FRootNodeList.Add(Result);
            Result.Parent := nil;
          end
          else
          begin
            Node.FChildList.Add(Result);
            Result.Parent := Node;
          end;
          try
            Result.Data := Ptr;
            Result.Text := S;
          except
            raise;
          end;
        end;

      taInsert:
        begin


        end;
    end;
  except
    raise;
  end;
end;

function TrmDGTreeNodes.GetFirstNode: TrmDGTreeNode;
begin
  if FRootNodeList.Count = 0 then
    Result := nil
  else
    Result := FRootNodeList.Items[0];
end;

function TrmDGTreeNodes.GetNodeFromIndex(Index: Integer): TrmDGTreeNode;
var
  I: Integer;
begin
  Result := GetFirstNode;
  I := Index;
  while (I <> 0) and (Result <> nil) do
  begin
    Result := Result.GetNext;
    Dec(I);
  end;
  if Result = nil then
    DGTreeError('Index out of range');
end;

procedure TrmDGTreeNodes.SetItem(Index: Integer; Value: TrmDGTreeNode);
begin
  GetNodeFromIndex(Index).Assign(Value);
end;

procedure TrmDGTreeNodes.Assign(Source: TPersistent);
var
  TreeNodes: TrmDGTreeNodes;
  MemStream: TMemoryStream;
begin
  if Source is TrmDGTreeNodes then
  begin
    TreeNodes := TrmDGTreeNodes(Source);
    Clear;
    MemStream := TMemoryStream.Create;
    try
      TreeNodes.WriteData(MemStream);
      MemStream.Position := 0;
      ReadData(MemStream);
    finally
      MemStream.Free;
    end;
  end
  else inherited Assign(Source);
end;

procedure TrmDGTreeNodes.DefineProperties(Filer: TFiler);

  function WriteNodes: Boolean;
  var
    I: Integer;
    Nodes: TrmDGTreeNodes;
  begin
    Nodes := TrmDGTreeNodes(Filer.Ancestor);
    if Nodes = nil then
      Result := Count > 0
    else if Nodes.Count <> Count then
      Result := True
    else
    begin
      Result := False;
      for I := 0 to Count - 1 do
      begin
        Result := not Item[I].IsEqual(Nodes[I]);
        if Result then Break;
      end
    end;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, WriteNodes);
end;

procedure TrmDGTreeNodes.ReadData(Stream: TStream);
var
  I, Count: Integer;
  Info : TDGNodeInfo;

begin
  Clear;
  Stream.ReadBuffer(Count, SizeOf(Count));
  for I := 0 to Count - 1 do
    Add(nil, #0).ReadData(Stream, @Info);
end;

procedure TrmDGTreeNodes.WriteData(Stream: TStream);
var
  I: Integer;
  Node: TrmDGTreeNode;
  Info : TDGNodeInfo;

begin
  I := 0;
  Node := GetFirstNode;
  while Node <> nil do
  begin
    Inc(I);
    Node := Node.GetNextSibling;
  end;
  Stream.WriteBuffer(I, SizeOf(I));
  Node := GetFirstNode;
  while Node <> nil do
  begin
    Node.WriteData(Stream, @Info);
    Node := Node.GetNextSibling;
  end;
end;

{ TrmCustomDGTree }

constructor TrmCustomDGTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTreeNodes := TrmDGTreeNodes.Create(Self);
end;

destructor TrmCustomDGTree.Destroy;
begin
  Items.Free;
  FMemStream.Free;
  inherited Destroy;
end;

procedure TrmCustomDGTree.SetrmDGTreeNodes(Value: TrmDGTreeNodes);
begin
  Items.Assign(Value);
end;

procedure TrmCustomDGTree.Delete(Node: TrmDGTreeNode);
begin
  if Assigned(FOnDeletion) then
    FOnDeletion(Self, Node);
end;

function TrmCustomDGTree.CreateNode: TrmDGTreeNode;
begin
  Result := TrmDGTreeNode.Create(Items);
end;

end.
