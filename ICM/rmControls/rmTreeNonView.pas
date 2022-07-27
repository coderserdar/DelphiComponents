{============== ==================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmTreeNonView
Purpose  : To have a non-visual tree component.
Date     : 12-01-1999
Author   : Ryan J. Mills
Version  : 1.90
Notes    : This unit was originally based upon the work of Patrick O'Keeffe.
           It was at his request that I took the component over and rm'ified it.
================================================================================}

unit rmTreeNonView;

interface

{$I CompilerDefines.INC}

uses
   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   Contnrs;

type
   TAddMode = (taAddFirst, taAdd, taInsert) ;
   TNodeAttachMode = (naAdd, naAddFirst, naAddChild, naAddChildFirst, naInsert) ;

   PNodeInfo = ^TNodeInfo;
   TNodeInfo = packed record
      Count: Integer;
      Text: string[255];
   end;

   TrmCustomTreeNonView = class;
   TrmTreeNonViewNodes = class;
   TrmTreeNonViewNode = class;

   TrmHashData = class(TObject)
      Hash: longint;
      IDLength: Integer;
      Node: TrmTreeNonViewNode;
   end;

{ TrmTreeNonViewNode }

   TrmTreeNonViewNode = class(TPersistent)
   private
      FOwner: TrmTreeNonViewNodes;
      FText: string;
      FData: Pointer;
      FChildList: TList;
      FDeleting: Boolean;
      FParent: TrmTreeNonViewNode;
      fExpanded: boolean;
      fHashed : boolean;
      function GetLevel: Integer;
      function GetParent: TrmTreeNonViewNode;
      procedure SetParent(Value: TrmTreeNonViewNode) ;
      function GetChildren: Boolean;
      function GetIndex: Integer;
      function GetItem(Index: Integer) : TrmTreeNonViewNode;
      function GetCount: Integer;
      function GetrmTreeNonView: TrmCustomTreeNonView;
      function IsEqual(Node: TrmTreeNonViewNode) : Boolean;
      procedure ReadData(Stream: TStream; Info: PNodeInfo) ;
      procedure SetData(Value: Pointer) ;
      procedure SetItem(Index: Integer; Value: TrmTreeNonViewNode) ;
      procedure SetText(const S: string) ;
      procedure WriteData(Stream: TStream; Info: PNodeInfo) ;
      function GetItemCount: Integer;
      procedure RemoveHash;
      procedure RenewHash;
      property HasBeenHashed : boolean read fhashed write fhashed;
      function GetNodePath: string;
   public
      constructor Create(AOwner: TrmTreeNonViewNodes) ;
      destructor Destroy; override;
      procedure Assign(Source: TPersistent) ; override;
      procedure Delete;
      procedure DeleteChildren;
      function GetFirstChild: TrmTreeNonViewNode;
      function GetLastChild: TrmTreeNonViewNode;
      function GetNext: TrmTreeNonViewNode;
      function GetNextChild(Value: TrmTreeNonViewNode) : TrmTreeNonViewNode;
      function GetNextSibling: TrmTreeNonViewNode;
      function GetPrev: TrmTreeNonViewNode;
      function GetPrevChild(Value: TrmTreeNonViewNode) : TrmTreeNonViewNode;
      function getPrevSibling: TrmTreeNonViewNode;
      function HasAsParent(Value: TrmTreeNonViewNode) : Boolean;
      function IndexOf(Value: TrmTreeNonViewNode) : Integer;
      function MoveTo(Destination: TrmTreeNonViewNode; Mode: TNodeAttachMode) : TrmTreeNonViewNode;

      property Count: Integer read GetCount;
      property Data: Pointer read FData write SetData;
      property Deleting: Boolean read FDeleting;
      property HasChildren: Boolean read GetChildren;
      property Expanded: boolean read fExpanded write fExpanded default false;
      property Index: Integer read GetIndex;
      property Item[Index: Integer]: TrmTreeNonViewNode read GetItem write SetItem; default;
      property Level: Integer read GetLevel;
      property Owner: TrmTreeNonViewNodes read FOwner;
      property Parent: TrmTreeNonViewNode read GetParent write SetParent;
      property TreeNonView: TrmCustomTreeNonView read GetrmTreeNonView;
      property NodePath: string read GetNodePath;
      property Text: string read FText write SetText;
      property ItemCount: Integer read GetItemCount;
   end;

{ TrmTreeNonViewNodes }

   TrmTreeNonViewNodes = class(TPersistent)
   private
      FOwner: TrmCustomTreeNonView;
      FRootNodeList: TList;
      FHashList: TObjectList;
      function GetNodeFromIndex(Index: Integer) : TrmTreeNonViewNode;
      procedure ReadData(Stream: TStream) ;
      procedure WriteData(Stream: TStream) ;
      function HashValue(St: string) : LongInt;
      function LocateHashIndex(Path: string) : integer;
      procedure BinaryInsert(Path: string; Node: TrmTreeNonViewNode) ;
      procedure RemoveHash(Node: TrmTreeNonViewNode) ;
   protected
      function InternalAddObject(Node: TrmTreeNonViewNode; const S: string;
         Ptr: Pointer; AddMode: TAddMode) : TrmTreeNonViewNode;
      procedure DefineProperties(Filer: TFiler) ; override;
      function GetCount: Integer;
      procedure SetItem(Index: Integer; Value: TrmTreeNonViewNode) ;
   public
      procedure dumphash;
      constructor Create(AOwner: TrmCustomTreeNonView) ;
      destructor Destroy; override;
      function AddChildFirst(Node: TrmTreeNonViewNode; const S: string) : TrmTreeNonViewNode;
      function AddChild(Node: TrmTreeNonViewNode; const S: string) : TrmTreeNonViewNode;
      function AddChildObjectFirst(Node: TrmTreeNonViewNode; const S: string;
         Ptr: Pointer) : TrmTreeNonViewNode;
      function AddChildObject(Node: TrmTreeNonViewNode; const S: string;
         Ptr: Pointer) : TrmTreeNonViewNode;
      function AddFirst(Node: TrmTreeNonViewNode; const S: string) : TrmTreeNonViewNode;
      function Add(Node: TrmTreeNonViewNode; const S: string) : TrmTreeNonViewNode;
      function AddObjectFirst(Node: TrmTreeNonViewNode; const S: string;
         Ptr: Pointer) : TrmTreeNonViewNode;
      function AddObject(Node: TrmTreeNonViewNode; const S: string;
         Ptr: Pointer) : TrmTreeNonViewNode;
      procedure Assign(Source: TPersistent) ; override;
      procedure Clear;
      procedure Delete(Node: TrmTreeNonViewNode) ;
      function GetFirstNode: TrmTreeNonViewNode;
      function Insert(Node: TrmTreeNonViewNode; const S: string) : TrmTreeNonViewNode;
      function InsertObject(Node: TrmTreeNonViewNode; const S: string;
         Ptr: Pointer) : TrmTreeNonViewNode;
      function LocateNode(Path: string) : TrmTreeNonViewNode;
      property Count: Integer read GetCount;
      property Item[Index: Integer]: TrmTreeNonViewNode read GetNodeFromIndex; default;
      property Owner: TrmCustomTreeNonView read FOwner;
   end;

{ TrmCustomTreeNonView }

   TrmTreeNonViewEvent = procedure(Sender: TObject; Node: TrmTreeNonViewNode) of object;
   ErmTreeNonViewError = class(Exception) ;

   TrmCustomTreeNonView = class(TComponent)
   private
      FMemStream: TMemoryStream;
      FTreeNodes: TrmTreeNonViewNodes;
      FOnDeletion: TrmTreeNonViewEvent;
      fOnNodeTextChanged: TrmTreeNonViewEvent;
      FSepChar: Char;

      procedure SetrmTreeNonViewNodes(Value: TrmTreeNonViewNodes) ;
      function ParentName(s: string) : string;
      function ChildName(s: string) : string;
   protected
      function CreateNode: TrmTreeNonViewNode; virtual;
      procedure Delete(Node: TrmTreeNonViewNode) ; dynamic;

      property SepChar: Char read FSepChar write FSepChar;
      property Items: TrmTreeNonViewNodes read FTreeNodes write SetrmTreeNonViewNodes;
      property OnDeletion: TrmTreeNonViewEvent read FOnDeletion write FOnDeletion;
      property OnNodeTextChanged: TrmTreeNonViewEvent read fOnNodeTextChanged write fOnNodeTextChanged;
   public
      constructor Create(AOwner: TComponent) ; override;
      destructor Destroy; override;
      procedure LoadFromFile(const FileName: string) ;
      procedure LoadFromStream(Stream: TStream) ;
      procedure SaveToFile(const FileName: string) ;
      procedure SaveToStream(Stream: TStream) ;
      function AddPathNode(Node: TrmTreeNonViewNode; Path: string) : TrmTreeNonViewNode;
      function FindPathNode(Path: string) : TrmTreeNonViewNode;
      function NodePath(Node: TrmTreeNonViewNode) : string;
      procedure TextSort(ParentNode: TrmTreeNonViewNode; Recursive: boolean) ; virtual;
   end;

   TrmTreeNonView = class(TrmCustomTreeNonView)
   private
    { Private declarations }
   protected
    { Protected declarations }
   public
    { Public declarations }
   published
    { Published declarations }
      property SepChar;
      property Items;
      property OnDeletion;
      property OnNodeTextChanged;
   end;

implementation

uses Consts, rmLibrary;

procedure rmTreeNonViewError(const Msg: string) ;
begin
   raise ErmTreeNonViewError.Create(Msg) ;
end;

constructor TrmTreeNonViewNode.Create(AOwner: TrmTreeNonViewNodes) ;
begin
   inherited Create;
   FOwner := AOwner;
   FChildList := TList.Create;
   fExpanded := false;
   fHashed := false;
end;

destructor TrmTreeNonViewNode.Destroy;
begin
   FDeleting := True;
   Data := nil;
   FChildList.Free;
   inherited Destroy;
end;

function TrmTreeNonViewNode.GeTrmTreeNonView: TrmCustomTreeNonView;
begin
   Result := Owner.Owner;
end;

function TrmTreeNonViewNode.HasAsParent(Value: TrmTreeNonViewNode) : Boolean;
begin
   if Value <> nil then
   begin
      if Parent = nil then
         Result := False
      else if Parent = Value then
         Result := True
      else
         Result := Parent.HasAsParent(Value) ;
   end
   else
      Result := True;
end;

procedure TrmTreeNonViewNode.SetText(const S: string) ;
var
   fRemoved: boolean;
begin
   fRemoved := false;

   if not (FText = '') and (FText <> S) then
   begin
      Self.RemoveHash;
      fRemoved := true;
   end;

   FText := S;

   if fRemoved and not (fText = '') then
   begin
      Self.RenewHash;
   end;

   if assigned(TreeNonView.OnNodeTextChanged) then
      TreeNonView.OnNodeTextChanged(TreeNonView, self) ;
end;

procedure TrmTreeNonViewNode.SetData(Value: pointer) ;
begin
   FData := Value;
end;

function TrmTreeNonViewNode.GetChildren: Boolean;
begin
   Result := FChildList.Count > 0;
end;

function TrmTreeNonViewNode.GetParent: TrmTreeNonViewNode;
begin
   Result := FParent;
end;

procedure TrmTreeNonViewNode.SetParent(Value: TrmTreeNonViewNode) ;
var
   wHashed : boolean;
begin
   wHashed := HasBeenHashed;
   if wHashed then
      removeHash;

   if (fParent <> nil) then
      fParent.FChildList.delete(fParent.FChildList.indexOf(self) ) ;

   if (value <> nil) then
   begin
      FParent := Value;
      if fParent.FChildList.indexof(self) = -1 then
         fParent.FChildList.Add(self) ;
   end;

   if wHashed then
      RenewHash;
end;

function TrmTreeNonViewNode.GetNextSibling: TrmTreeNonViewNode;
var
   CurIdx: Integer;

begin
   if Parent <> nil then
   begin
      CurIdx := Parent.FChildList.IndexOf(Self) ;
      if (CurIdx + 1) < Parent.FChildList.Count then
         Result := Parent.FChildList.Items[CurIdx + 1]
      else
         Result := nil;
   end
   else
   begin
      CurIdx := Owner.FRootNodeList.IndexOf(Self) ;
      if (CurIdx + 1) < Owner.FRootNodeList.Count then
         Result := Owner.FRootNodeList.Items[CurIdx + 1]
      else
         Result := nil;
   end;
end;

function TrmTreeNonViewNode.GetPrevSibling: TrmTreeNonViewNode;
var
   CurIdx: Integer;
begin
   if Parent <> nil then
   begin
      CurIdx := Parent.FChildList.IndexOf(Self) ;
      if (CurIdx - 1) >= 0 then
         Result := Parent.FChildList.Items[CurIdx - 1]
      else
         Result := nil;
   end
   else
   begin
      CurIdx := Owner.FRootNodeList.IndexOf(Self) ;
      if (CurIdx - 1) >= Owner.FRootNodeList.Count then
         Result := Owner.FRootNodeList.Items[CurIdx - 1]
      else
         Result := nil;
   end;
end;

function TrmTreeNonViewNode.GetNextChild(Value: TrmTreeNonViewNode) : TrmTreeNonViewNode;
begin
   if Value <> nil then
      Result := Value.GetNextSibling
   else
      Result := nil;
end;

function TrmTreeNonViewNode.GetPrevChild(Value: TrmTreeNonViewNode) : TrmTreeNonViewNode;
begin
   if Value <> nil then
      Result := Value.GetPrevSibling
   else
      Result := nil;
end;

function TrmTreeNonViewNode.GetFirstChild: TrmTreeNonViewNode;
begin
   if FChildList.Count > 0 then
   begin
      Result := FChildList.Items[0];
   end
   else
      Result := nil;
end;

function TrmTreeNonViewNode.GetLastChild: TrmTreeNonViewNode;
begin
   if FChildList.Count > 0 then
   begin
      Result := FChildList.Items[FChildList.Count - 1]
   end
   else
      Result := nil;
end;

function TrmTreeNonViewNode.GetNext: TrmTreeNonViewNode;
var
   N: TrmTreeNonViewNode;
   P: TrmTreeNonViewNode;

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

function TrmTreeNonViewNode.GetPrev: TrmTreeNonViewNode;
var
   Node: TrmTreeNonViewNode;

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

function TrmTreeNonViewNode.GetIndex: Integer;
var
   Node: TrmTreeNonViewNode;

begin
   Result := -1;
   Node := parent;
   if Node = nil then
   begin
      if fowner <> nil then
         FOwner.FRootNodeList.indexof(self)
   end
   else
      result := parent.FChildList.indexof(self) ;
end;

function TrmTreeNonViewNode.GetItem(Index: Integer) : TrmTreeNonViewNode;
begin
   if (index >= 0) and (index < FChildList.count) then
      Result := fchildlist[index]
   else
   begin
      result := nil;
      rmTreeNonViewError('List Index Out of Bounds') ;
   end;
end;

procedure TrmTreeNonViewNode.SetItem(Index: Integer; Value: TrmTreeNonViewNode) ;
begin
   item[Index].Assign(Value) ;
end;

function TrmTreeNonViewNode.IndexOf(Value: TrmTreeNonViewNode) : Integer;
begin
   Result := fChildList.indexof(Value) ;
end;

function TrmTreeNonViewNode.MoveTo(Destination: TrmTreeNonViewNode; Mode: TNodeAttachMode) : TrmTreeNonViewNode;
var
   AddMode: TAddMode;
   node: TrmTreeNonViewNode;

begin
   Result := nil;
   if (Destination = nil) or not Destination.HasAsParent(Self) then
   begin
      AddMode := taAdd;
      if (Destination <> nil) and not (Mode in [naAddChild, naAddChildFirst]) then
         Node := Destination.Parent
      else
         Node := Destination;

      case Mode of
         naAdd,
            naAddChild: AddMode := taAdd;
         naAddFirst,
            naAddChildFirst: AddMode := taAddFirst;
         naInsert:
            begin
               Node := Destination.GetPrevSibling;
               if Node = nil then
                  AddMode := taAddFirst
               else
                  AddMode := taInsert;
            end;
      end;
      if node <> self then
      begin
         result := owner.InternalAddObject(node, Text, data, AddMode) ;
         delete;
      end;
   end
   else
      result := self;
end;

function TrmTreeNonViewNode.GetCount: Integer;
begin
   result := FChildList.Count;
end;

function TrmTreeNonViewNode.GetLevel: Integer;
var
   Node: TrmTreeNonViewNode;

begin
   Result := 0;
   Node := Parent;
   while Node <> nil do
   begin
      Inc(Result) ;
      Node := Node.Parent;
   end;
end;

procedure TrmTreeNonViewNode.Delete;
var
   wIndex : integer;
begin
   if HasChildren then
      DeleteChildren;

   Owner.RemoveHash(self) ;
   if Parent <> nil then
   begin
      wIndex := Parent.FChildList.IndexOf(Self);
      Parent.FChildList.Delete( wIndex );
      Parent.FChildList.Pack;
   end
   else
   begin
      wIndex := Owner.FRootNodeList.IndexOf(Self);
      Owner.FRootNodeList.Delete( wIndex ) ;
      Owner.FRootNodeList.Pack;
   end;
   TrmCustomTreeNonView(Owner.Owner) .Delete(Self) ;
   Free;
end;

procedure TrmTreeNonViewNode.DeleteChildren;
var
   Node: TrmTreeNonViewNode;

begin
   Node := GetLastChild;
   while Node <> nil do
   begin
      Node.Delete;
      Node := GetLastChild;
   end;
end;

procedure TrmTreeNonViewNode.Assign(Source: TPersistent) ;
var
   Node: TrmTreeNonViewNode;

begin
   if Source is TrmTreeNonViewNode then
   begin
      Node := TrmTreeNonViewNode(Source) ;
      Text := Node.Text;
      Data := Node.Data;
   end
   else
      inherited Assign(Source) ;
end;

function TrmTreeNonViewNode.IsEqual(Node: TrmTreeNonViewNode) : Boolean;
begin
   Result := (Text = Node.Text) and (Data = Node.Data) ;
end;

procedure TrmTreeNonViewNode.ReadData(Stream: TStream; Info: PNodeInfo) ;
var
   I, Size, ItemCount: Integer;
   ObjType: Integer;

begin
   Stream.ReadBuffer(Size, SizeOf(Size) ) ;
   Stream.ReadBuffer(Info^, Size) ;
   Text := Info^.Text;
   ItemCount := Info^.Count;
   Stream.ReadBuffer(ObjType, SizeOf(ObjType) ) ;
   case ObjType of
      0:
         begin
        //do nothing
         end;

      1:
         begin
            Data := Stream.ReadComponent(nil) ;
         end;
   end;
   for I := 0 to ItemCount - 1 do
      Owner.AddChild(Self, '') .ReadData(Stream, Info) ;
end;

procedure TrmTreeNonViewNode.WriteData(Stream: TStream; Info: PNodeInfo) ;
var
   I,
      Size,
      L,
      ItemCount,
      ObjType: Integer;

begin
   L := Length(Text) ;
   if L > 255 then
      L := 255;
   Size := SizeOf(TNodeInfo) + L - 255;
   Info^.Text := Text;
   ItemCount := Count;
   Info^.Count := ItemCount;
   Stream.WriteBuffer(Size, SizeOf(Size) ) ;
   Stream.WriteBuffer(Info^, Size) ;
   if Assigned(Self.Data) then
   begin
      try
         TObject(self.data) .classtype;
         if (TObject(Self.Data) is TComponent) then
         begin
            ObjType := 1;
            Stream.WriteBuffer(ObjType, SizeOf(ObjType) ) ;
            Stream.WriteComponent(TComponent(Data) ) ;
         end
         else
         begin
            ObjType := 0;
            Stream.WriteBuffer(ObjType, SizeOf(ObjType) ) ;
         end;
      except
         ObjType := 0;
         Stream.WriteBuffer(ObjType, SizeOf(ObjType) ) ;
      end;
   end
   else
   begin
      ObjType := 0;
      Stream.WriteBuffer(ObjType, SizeOf(ObjType) ) ;
   end;
   for I := 0 to ItemCount - 1 do
      Item[I].WriteData(Stream, Info) ;
end;

{ TrmTreeNonViewNodes }

constructor TrmTreeNonViewNodes.Create(AOwner: TrmCustomTreeNonView) ;
begin
   inherited Create;
   FOwner := AOwner;
   FRootNodeList := TList.Create;
   FHashList := TObjectList.Create;
   FHashList.OwnsObjects := true;
end;

destructor TrmTreeNonViewNodes.Destroy;
begin
   Clear;
   FRootNodeList.Free;
   FHashList.Free;
   inherited Destroy;
end;

function TrmTreeNonViewNodes.GetCount: Integer;
var
   Idx: Integer;

begin
   Result := FRootNodeList.Count;
   for Idx := 0 to FRootNodeList.Count - 1 do
   begin
      Result := Result + TrmTreeNonViewNode(FRootNodeList[Idx]) .ItemCount;
   end;
end;

procedure TrmTreeNonViewNodes.Delete(Node: TrmTreeNonViewNode) ;
var
   wIndex: integer;
begin
   wIndex := LocateHashIndex(Owner.NodePath(Node) ) ;
   if wIndex > -1 then
      FHashList.delete(wIndex) ;
   Node.Delete;
end;

procedure TrmTreeNonViewNodes.Clear;
var
   N: TrmTreeNonViewNode;

begin
   N := GetFirstNode;
   while N <> nil do
   begin
      N.Delete;
      N := GetFirstNode;
   end;
   FHashList.Clear;
end;

function TrmTreeNonViewNodes.AddChildFirst(Node: TrmTreeNonViewNode; const S: string) : TrmTreeNonViewNode;
begin
   Result := AddChildObjectFirst(Node, S, nil) ;
end;

function TrmTreeNonViewNodes.AddChildObjectFirst(Node: TrmTreeNonViewNode; const S: string;
   Ptr: Pointer) : TrmTreeNonViewNode;
begin
   Result := InternalAddObject(Node, S, Ptr, taAddFirst) ;
end;

function TrmTreeNonViewNodes.AddChild(Node: TrmTreeNonViewNode; const S: string) : TrmTreeNonViewNode;
begin
   Result := AddChildObject(Node, S, nil) ;
end;

function TrmTreeNonViewNodes.AddChildObject(Node: TrmTreeNonViewNode; const S: string;
   Ptr: Pointer) : TrmTreeNonViewNode;
begin
   Result := InternalAddObject(Node, S, Ptr, taAdd) ;
end;

function TrmTreeNonViewNodes.AddFirst(Node: TrmTreeNonViewNode; const S: string) : TrmTreeNonViewNode;
begin
   Result := AddObjectFirst(Node, S, nil) ;
end;

function TrmTreeNonViewNodes.AddObjectFirst(Node: TrmTreeNonViewNode; const S: string;
   Ptr: Pointer) : TrmTreeNonViewNode;
begin
   if Node <> nil then Node := Node.Parent;
   Result := InternalAddObject(Node, S, Ptr, taAddFirst) ;
end;

function TrmTreeNonViewNodes.Add(Node: TrmTreeNonViewNode; const S: string) : TrmTreeNonViewNode;
begin
   Result := AddObject(Node, S, nil) ;
end;

function TrmTreeNonViewNodes.AddObject(Node: TrmTreeNonViewNode; const S: string;
   Ptr: Pointer) : TrmTreeNonViewNode;
begin
   if Node <> nil then Node := Node.Parent;
   Result := InternalAddObject(Node, S, Ptr, taAdd) ;
end;

function TrmTreeNonViewNodes.Insert(Node: TrmTreeNonViewNode; const S: string) : TrmTreeNonViewNode;
begin
   Result := InsertObject(Node, S, nil) ;
end;

function TrmTreeNonViewNodes.InsertObject(Node: TrmTreeNonViewNode; const S: string; Ptr: Pointer) : TrmTreeNonViewNode;
var
   Parent: TrmTreeNonViewNode;
   AddMode: TAddMode;
   Target: TrmTreeNonViewNode;
begin
   AddMode := taInsert;
   Target := node;
   if Node <> nil then
   begin
      Parent := Node.Parent;
      if Parent <> nil then
         Node := Node.GetPrevSibling;
      if Node = nil then
      begin
         AddMode := taAddFirst;
         target := parent;
      end;
   end;
   Result := InternalAddObject(Target, S, Ptr, AddMode) ;
end;

function TrmTreeNonViewNodes.InternalAddObject(Node: TrmTreeNonViewNode; const S: string;
   Ptr: Pointer; AddMode: TAddMode) : TrmTreeNonViewNode;
var
   nvnParent: TrmTreeNonViewNode;
   nindex: integer;
begin
   Result := Owner.CreateNode;
   try
      case AddMode of
         taAddFirst:
            begin
               if Node = nil then
               begin
                  FRootNodeList.Insert(0, Result) ;
                  Result.Parent := nil;
               end
               else
               begin
                  Node.FChildList.Insert(0, Result) ;
                  Result.Parent := Node;
               end;
               try
                  Result.Data := Ptr;
                  Result.Text := S;
                  BinaryInsert(Owner.NodePath(Result) , Result) ;
               except
                  raise;
               end;
            end;

         taAdd:
            begin
               if Node = nil then
               begin
                  FRootNodeList.Add(Result) ;
                  Result.Parent := nil;
               end
               else
               begin
                  Node.FChildList.Add(Result) ;
                  Result.Parent := Node;
               end;
               try
                  Result.Data := Ptr;
                  Result.Text := S;
                  BinaryInsert(Owner.NodePath(Result) , Result) ;
               except
                  raise;
               end;
            end;

         taInsert:
            begin
               nvnParent := Node.Parent;
               if nvnParent = nil then
               begin
                  if Node = nil then
                  begin
                     FRootNodeList.Insert(0, Result) ;
                     Result.Parent := nil;
                  end
                  else
                  begin
                     nIndex := fRootNodeList.IndexOf(Node) ;
                     if nIndex <> -1 then
                     begin
                        fRootNodeList.Insert(nIndex, Result) ;
                        result.parent := nil;
                     end
                     else
                        rmTreeNonViewError('Unable to find Node reference') ;
                  end;
               end
               else
               begin
                  nIndex := nvnParent.FChildList.IndexOf(node) ;
                  if nIndex >= 0 then
                  begin
                     nvnParent.FChildList.Insert(nIndex, Result) ;
                     result.parent := nvnParent;
                  end
                  else
                     rmTreeNonViewError('Unable to find Node reference') ;
               end;

               try
                  Result.Data := Ptr;
                  Result.Text := S;
                  BinaryInsert(Owner.NodePath(Result) , Result) ;
               except
                  raise;
               end;
            end;
      end;
   except
      raise;
   end;
end;

function TrmTreeNonViewNodes.GetFirstNode: TrmTreeNonViewNode;
begin
   if FRootNodeList.Count = 0 then
      Result := nil
   else
      Result := FRootNodeList.Items[0];
end;

function TrmTreeNonViewNodes.GetNodeFromIndex(Index: Integer) : TrmTreeNonViewNode;
var
   I: Integer;
begin
   Result := GetFirstNode;
   I := Index;
   while (I <> 0) and (Result <> nil) do
   begin
      Result := Result.GetNext;
      Dec(I) ;
   end;
   if Result = nil then
      rmTreeNonViewError('Index out of range') ;
end;

procedure TrmTreeNonViewNodes.SetItem(Index: Integer; Value: TrmTreeNonViewNode) ;
begin
   GetNodeFromIndex(Index) .Assign(Value) ;
end;

procedure TrmTreeNonViewNodes.Assign(Source: TPersistent) ;
var
   TreeNodes: TrmTreeNonViewNodes;
   MemStream: TMemoryStream;
   wNode: TrmTreeNonViewNode;
begin
   if Source is TrmTreeNonViewNodes then
   begin
      TreeNodes := TrmTreeNonViewNodes(Source) ;
      Clear;
      MemStream := TMemoryStream.Create;
      try
         TreeNodes.WriteData(MemStream) ;
         MemStream.Position := 0;
         ReadData(MemStream) ;
      finally
         MemStream.Free;
      end;

      //Now that we've assigned all the nodes
      //we need to redo that hashlist
      wNode := Self.GetFirstNode;
      while wNode <> nil do
      begin
         wNode.RenewHash;
         wNode := wNode.GetNextSibling;
      end;
   end
   else
      inherited Assign(Source) ;
end;

procedure TrmTreeNonViewNodes.DefineProperties(Filer: TFiler) ;

   function WriteNodes: Boolean;
   var
      I: Integer;
      Nodes: TrmTreeNonViewNodes;
   begin
      Nodes := TrmTreeNonViewNodes(Filer.Ancestor) ;
      if Nodes = nil then
         Result := Count > 0
      else if Nodes.Count <> Count then
         Result := True
      else
      begin
         Result := False;
         for I := 0 to Count - 1 do
         begin
            Result := not Item[I].IsEqual(Nodes[I]) ;
            if Result then Break;
         end
      end;
   end;

begin
   inherited DefineProperties(Filer) ;
   Filer.DefineBinaryProperty('Data', ReadData, WriteData, WriteNodes) ;
end;

procedure TrmTreeNonViewNodes.ReadData(Stream: TStream) ;
var
   I, Count: Integer;
   Info: TNodeInfo;

begin
   Clear;
   Stream.ReadBuffer(Count, SizeOf(Count) ) ;
   for I := 0 to Count - 1 do
      Add(nil, '') .ReadData(Stream, @Info) ;
end;

procedure TrmTreeNonViewNodes.WriteData(Stream: TStream) ;
var
   I: Integer;
   Node: TrmTreeNonViewNode;
   Info: TNodeInfo;

begin
   I := 0;
   Node := GetFirstNode;
   while Node <> nil do
   begin
      Inc(I) ;
      Node := Node.GetNextSibling;
   end;
   Stream.WriteBuffer(I, SizeOf(I) ) ;
   Node := GetFirstNode;
   while Node <> nil do
   begin
      Node.WriteData(Stream, @Info) ;
      Node := Node.GetNextSibling;
   end;
end;

function TrmTreeNonViewNodes.HashValue(St: string) : Longint;
begin
   result := GetStrCRC32(St) ;
end;

function TrmTreeNonViewNodes.LocateHashIndex(Path: string) : integer;
var
   wHash: longint;
   wData: TrmHashData;
   First, Middle, Last, Temp: longint;
   wFound: boolean;
begin
   wHash := HashValue(Path) ;

   result := -1;
   First := 0;
   Last := FHashList.count - 1;
   wFound := false;
   middle := round((last + first) / 2) ;

   while (not wFound) and (first <= last) do
   begin
      middle := round((last + first) / 2) ;
      wData := TrmHashData(fHashlist[middle]) ;

      if wHash = wData.hash then
         wFound := true
      else
      begin
         if wHash < wData.hash then
            last := middle - 1
         else
            first := middle + 1;
      end;
   end;

   if wFound then
   begin
      Temp := middle;

      while (Middle > 0) and (Middle - 1 >= First) and (TrmHashData(FHashList[middle - 1]) .Hash = wHash) do
         dec(Middle) ;

      while (result = -1) and (Middle < FHashList.Count) and (Middle + 1 < Last) and (TrmHashData(FHashList[middle + 1]) .Hash = wHash) do
      begin
         wData := TrmHashData(FHashList[middle]) ;
         if (Owner.NodePath(wData.Node) = Path) then
            result := middle
         else
            inc(Middle) ;
      end;

      if result = -1 then
         result := temp;
   end;
end;

procedure TrmTreeNonViewNodes.BinaryInsert(Path: string; Node: TrmTreeNonViewNode) ;
var
   wHash: longint;
   wLen: integer;
   wData: TrmHashData;
   First, Middle, Last: longint;
   wFound: boolean;
begin
   wHash := HashValue(Path) ;
   wLen := Length(Path) ;

   First := 0;
   Last := FHashList.count - 1;
   wFound := false;

   while (not wFound) and (first <= last) do
   begin
      middle := round((last + first) / 2) ;
      wData := TrmHashData(fHashlist[middle]) ;

      if wHash = wData.hash then
         wFound := true
      else
      begin
         if wHash < wData.hash then
            last := middle - 1
         else
            first := middle + 1;
      end;
   end;

   if wFound then
   begin
      middle := round((last + first) / 2) ;
      wFound := false;

      while (Middle > 0) and (Middle - 1 >= First) and (TrmHashData(FHashList[middle - 1]) .Hash = wHash) do
         dec(Middle) ;

      while (not wfound) and (Middle < FHashList.Count) and (Middle + 1 < Last) and (TrmHashData(FHashList[middle + 1]) .Hash = wHash) do
      begin
         wData := TrmHashData(FHashList[middle]) ;
         if (Owner.NodePath(wData.Node) = Path) then
            wFound := true
         else
            inc(Middle) ;
      end;
      if not wFound then
         first := middle;
   end;

   if not wfound then
   begin
      wData := TrmHashData.create;
      wData.Hash := wHash;
      wData.IDLength := wLen;
      wData.Node := Node;
      fHashList.Insert(first, wData) ;
      Node.HasBeenHashed := true;
   end;
end;

function TrmTreeNonViewNodes.LocateNode(Path: string) : TrmTreeNonViewNode;
var
   wIndex: integer;
begin
   wIndex := LocateHashIndex(Path) ;
   if wIndex = -1 then
      result := nil
   else
      result := TrmHashData(FHashList[wIndex]) .Node;
end;

procedure TrmTreeNonViewNodes.RemoveHash(Node: TrmTreeNonViewNode) ;
var
   wIndex: integer;
begin
   wIndex := LocateHashIndex(Owner.NodePath(Node) ) ;
   if wIndex > -1 then
   begin
      FHashList.delete(wIndex) ;
      Node.HasBeenHashed := false;
   end;
end;

procedure TrmTreeNonViewNodes.dumphash;
var
   fstr: TextFile;
   loop: integer;
   wdata: trmhashdata;
begin
   AssignFile(fstr, '\nvhash.txt') ;
   rewrite(fstr) ;
   for loop := 0 to fhashlist.count - 1 do
   begin
      wData := Trmhashdata(fhashlist[loop]) ;
      writeln(fstr, owner.nodepath(wdata.node) ) ;
   end;
   closefile(fstr) ;
end;

{ TrmCustomTreeNonView }

constructor TrmCustomTreeNonView.Create(AOwner: TComponent) ;
begin
   inherited Create(AOwner) ;
   FSepChar := '/';
   FTreeNodes := TrmTreeNonViewNodes.Create(Self) ;
end;

destructor TrmCustomTreeNonView.Destroy;
begin
   Items.Free;
   FMemStream.Free;
   inherited Destroy;
end;

procedure TrmCustomTreeNonView.SetrmTreeNonViewNodes(Value: TrmTreeNonViewNodes) ;
begin
   Items.Assign(Value) ;
end;

procedure TrmCustomTreeNonView.Delete(Node: TrmTreeNonViewNode) ;
begin
   if Assigned(FOnDeletion) then
      FOnDeletion(Self, Node) ;
end;

function TrmCustomTreeNonView.CreateNode: TrmTreeNonViewNode;
begin
   Result := TrmTreeNonViewNode.Create(Items) ;
end;

procedure TrmCustomTreeNonView.LoadFromFile(const FileName: string) ;
var
   Stream: TStream;
begin
   Stream := TFileStream.Create(FileName, fmOpenRead) ;
   try
      LoadFromStream(Stream) ;
   finally
      Stream.Free;
   end;
end;

procedure TrmCustomTreeNonView.LoadFromStream(Stream: TStream) ;
begin

end;

procedure TrmCustomTreeNonView.SaveToFile(const FileName: string) ;
var
   Stream: TStream;
begin
   Stream := TFileStream.Create(FileName, fmCreate) ;
   try
      SaveToStream(Stream) ;
   finally
      Stream.Free;
   end;
end;

procedure TrmCustomTreeNonView.SaveToStream(Stream: TStream) ;
var
   N: TrmTreeNonViewNode;
   L: TStringList;

begin
   L := TStringList.Create;
   try
      N := Items.GetFirstNode;
      while N <> nil do
      begin
         L.Add(NodePath(N) ) ;
         N := N.GetNext;
      end;
      L.SaveToStream(Stream) ;
   finally
      L.Free;
   end;
end;

function TrmCustomTreeNonView.NodePath(Node: TrmTreeNonViewNode) : string;
var
   Temp: string;

begin
   Temp := '';

   while Node <> nil do
   begin
      Temp := FSepChar + Node.Text + Temp;
      Node := Node.Parent;
   end;
   Result := Temp;
end;

function TrmTreeNonViewNode.GetItemCount: Integer;
var
   Idx: Integer;

begin
   Result := FChildList.Count;
   for Idx := 0 to FChildList.Count - 1 do
   begin
      Result := Result + TrmTreeNonViewNode(FChildList[Idx]) .ItemCount;
   end;
end;

procedure TrmCustomTreeNonView.TextSort(ParentNode: TrmTreeNonViewNode;
   Recursive: boolean) ;
var
   Child: TrmTreeNonViewNode;
   WList, woList: TList;
   index: integer;
   found: boolean;

begin
   if ParentNode = nil then
      Child := FTreeNodes.GetFirstNode
   else
      Child := ParentNode.GetFirstChild;

   if assigned(child) then
   begin
      if child.parent = nil then
         woList := FTreeNodes.frootnodelist
      else
         woList := child.parent.FChildList;

      wList := TList.create;
      try
         while woList.count > 0 do
         begin
            wList.add(woList[0]) ;
            woList.delete(0) ;
         end;

         if Recursive then
            TextSort(TrmTreeNonViewNode(wList[0]) , recursive) ;

         woList.add(wList[0]) ;
         wList.delete(0) ;
         while wList.count > 0 do
         begin
            if Recursive then
               TextSort(TrmTreeNonViewNode(wList[0]) , recursive) ;

            index := 0;
            found := false;
            while index < woList.Count do
            begin
               if TrmTreeNonViewNode(wList[0]) .FText > TrmTreeNonViewNode(woList[index]) .fText then
                  inc(index)
               else
               begin
                  woList.Insert(index, wList[0]) ;
                  wList.delete(0) ;
                  found := true;
                  break;
               end;
            end;
            if not found then
            begin
               woList.add(wList[0]) ;
               wList.delete(0) ;
            end;
         end;
      finally
         wList.free;
      end;
   end;
end;

function TrmCustomTreeNonView.FindPathNode(Path: string) : TrmTreeNonViewNode;
begin
   result := Items.LocateNode(Path) ;
end;

function TrmCustomTreeNonView.AddPathNode(Node: TrmTreeNonViewNode; Path: string) : TrmTreeNonViewNode;
var
   wNode, wParent, wChild: TrmTreeNonViewNode;
   wPName, wCName: string;
begin
   result := nil;
   if path = '' then
      exit;

   if path[1] = sepchar then
      path := NodePath(Node) + path
   else
      path := nodepath(node) + sepchar + path;

   wNode := Items.LocateNode(Path) ;
   if wNode = nil then
   begin
      wPName := ParentName(Path) ;
      wCName := ChildName(Path) ;

      if (wPName = '') and (wCName = '') then
         exit;

      wParent := Items.LocateNode(wPName) ;
      if wParent = nil then
         wParent := AddPathNode(nil, wPname) ;
      wChild := Items.AddChild(wParent, wCName) ;
      result := wChild;
   end
   else
      result := wNode;
end;

function TrmCustomTreeNonView.ParentName(s: string) : string;
var
   wLen: integer;
begin
   wLen := length(s) ;
   if s[wLen] = SepChar then
   begin
      system.Delete(s, wLen, 1) ;
      dec(wLen) ;
   end;
   while (wlen > 0) and (s[wLen] <> sepchar) do
   begin
      system.Delete(s, wLen, 1) ;
      dec(wLen) ;
   end;
   if (wLen > 0) and (s[wLen] = SepChar) then
      system.Delete(s, wLen, 1) ;
   result := s;
end;

function TrmCustomTreeNonView.ChildName(s: string) : string;
var
   wLen: integer;
begin
   wLen := length(s) ;
   if s[wLen] = SepChar then
   begin
      system.Delete(s, wLen, 1) ;
      dec(wLen) ;
   end;
   while (wLen > 0) and (s[wLen] <> sepchar) do
      dec(wLen) ;
   system.delete(s, 1, wLen) ;
   result := s;
end;

procedure TrmTreeNonViewNode.RemoveHash;
var
   wNode: TrmTreeNonViewNode;
begin
   FOwner.RemoveHash(self) ;
   wNode := getFirstChild;
   while wNode <> nil do
   begin
      wNode.RemoveHash;
      wNode := wNode.getNextSibling;
   end;
end;

procedure TrmTreeNonViewNode.RenewHash;
var
   wNode: TrmTreeNonViewNode;
begin
   FOwner.BinaryInsert(FOwner.Owner.NodePath(self) , self) ;
   wNode := getFirstChild;
   while wNode <> nil do
   begin
      wNode.RenewHash;
      wNode := wNode.getNextSibling;
   end;
end;

function TrmTreeNonViewNode.GetNodePath: string;
begin
   Result := TreeNonView.NodePath(self) ;
end;

end.

