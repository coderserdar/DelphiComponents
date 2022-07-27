{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmWordTree
Purpose  : The TrmWordTree is a non-visual component provides a dictionary type
           word lookup interface.  You provide it with a list of words and then
           you can verify words against it or it will provide a list of similar
           words based on the soundex of the given word.
Date     : 05-01-01
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmWordTree;

interface

{$I CompilerDefines.INC}

uses
   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
   TAddMode = (taAddFirst, taAdd) ;
   TNodeAttachMode = (naAdd, naAddFirst, naAddChild, naAddChildFirst, naInsert) ;

   PWTNodeInfo = ^TWTNodeInfo;
   TWTNodeInfo = packed record
      Letter: Char;
      Count: Word;
      Complete: Boolean;
   end;

   TCustomrmWordTree = class;
   TrmWordTreeNodes = class;
   TrmWordTreeNode = class;

   TrmWordTreeNode = class(TPersistent)
   private
      FOwner: TrmWordTreeNodes;
      FParent: TrmWordTreeNode;
      FLetter: Char;
      FComplete: boolean;
      FChildList: TList;
      FDeleting: Boolean;
      function GetLevel: Integer;
      procedure SetParent(Value: TrmWordTreeNode) ;
      function GetChildren: Boolean;
      function GetIndex: Integer;
      function GetItem(Index: Integer) : TrmWordTreeNode;
      function GetCount: Integer;
      function GetWTTreeNonView: TCustomrmWordTree;
      procedure ReadData(Stream: TStream; Info: PWTNodeInfo) ;
      procedure SetItem(Index: Integer; Value: TrmWordTreeNode) ;
      procedure WriteData(Stream: TStream; Info: PWTNodeInfo) ;
      function IsEqual(Node: TrmWordTreeNode) : Boolean;
      function getWord: string;
   public
      constructor Create(AOwner: TrmWordTreeNodes) ;
      destructor Destroy; override;
      procedure Assign(Source: TPersistent) ; override;
      procedure Delete;
      procedure DeleteChildren;
      function GetFirstChild: TrmWordTreeNode;
      function GetLastChild: TrmWordTreeNode;
      function GetNext: TrmWordTreeNode;
      function GetNextChild(Value: TrmWordTreeNode) : TrmWordTreeNode;
      function GetNextSibling: TrmWordTreeNode;
      function GetPrev: TrmWordTreeNode;
      function GetPrevChild(Value: TrmWordTreeNode) : TrmWordTreeNode;
      function getPrevSibling: TrmWordTreeNode;
      function HasAsParent(Value: TrmWordTreeNode) : Boolean;
      function IndexOf(Value: TrmWordTreeNode) : Integer;

      property Count: Integer read GetCount;
      property Complete: boolean read FComplete write fComplete;
      property Deleting: Boolean read FDeleting;
      property HasChildren: Boolean read GetChildren;
      property Index: Integer read GetIndex;
      property Item[Index: Integer]: TrmWordTreeNode read GetItem write SetItem; default;
      property Level: Integer read GetLevel;
      property Owner: TrmWordTreeNodes read FOwner;
      property Parent: TrmWordTreeNode read FParent write SetParent;
      property WTTreeNonView: TCustomrmWordTree read GetWTTreeNonView;
      property Letter: Char read FLetter write FLetter;
      property Word : string read getWord;
   end;

   TrmWordTreeNodes = class(TPersistent)
   private
      FOwner: TCustomrmWordTree;
      FRootNodeList: TList;
      function GetNodeFromIndex(Index: Integer) : TrmWordTreeNode;
      procedure ReadData(Stream: TStream) ;
      procedure WriteData(Stream: TStream) ;
   protected
      function InternalAddObject(Node: TrmWordTreeNode; const Letter: Char; Complete: boolean; AddMode: TAddMode) : TrmWordTreeNode;
      procedure DefineProperties(Filer: TFiler) ; override;
      function GetCount: Integer;
      procedure SetItem(Index: Integer; Value: TrmWordTreeNode) ;
   public
      constructor Create(AOwner: TCustomrmWordTree) ;
      destructor Destroy; override;
      function AddChild(Node: TrmWordTreeNode; const Letter: Char) : TrmWordTreeNode;
      function Add(Node: TrmWordTreeNode; const Letter: Char) : TrmWordTreeNode;
      procedure Assign(Source: TPersistent) ; override;
      procedure Clear;
      procedure Delete(Node: TrmWordTreeNode) ;
      function GetFirstNode: TrmWordTreeNode;
      property Count: Integer read GetCount;
      property Item[Index: Integer]: TrmWordTreeNode read GetNodeFromIndex; default;
      property Owner: TCustomrmWordTree read FOwner;
   end;

{ TWTCustomrmWordTree }

   EWTTreeNonViewError = class(Exception) ;

   TCustomrmWordTree = class(TComponent)
   private
      FTreeNodes: TrmWordTreeNodes;
      procedure SetrmWordTreeNodes(Value: TrmWordTreeNodes) ;
   protected
      function CreateNode: TrmWordTreeNode; virtual;
      property Items: TrmWordTreeNodes read FTreeNodes write SeTrmWordTreeNodes;
   public
      constructor Create(AOwner: TComponent) ; override;
      destructor Destroy; override;
      function AddWord(word: string) : TrmWordTreeNode;
      function IsWord(word: string) : Boolean;
      function LocateWord(Word: string) : TrmWordTreeNode;
      procedure LoadPartialMatches(Word: String; MatchList: TStrings);
      procedure LoadSEMatches(Word: String; MatchList: TStrings);
   end;

   TrmWordTree = class(TCustomrmWordTree)
   private
    { Private declarations }
   protected
    { Protected declarations }
   public
    { Public declarations }
   published
    { Published declarations }
      property Items;
   end;

implementation

uses rmLibrary;

procedure WTTreeNonViewError(const Msg: string) ;
begin
   raise EWTTreeNonViewError.Create(Msg) ;
end;

constructor TrmWordTreeNode.Create(AOwner: TrmWordTreeNodes) ;
begin
   inherited Create;
   FOwner := AOwner;
   FChildList := TList.Create;
end;

destructor TrmWordTreeNode.Destroy;
begin
   FDeleting := True;
   FChildList.Free;
   inherited Destroy;
end;

function TrmWordTreeNode.GetWTTreeNonView: TCustomrmWordTree;
begin
   Result := Owner.Owner;
end;

function TrmWordTreeNode.HasAsParent(Value: TrmWordTreeNode) : Boolean;
begin
   if Value <> Nil then
   begin
      if Parent = nil then
         Result := False
      else
         if Parent = Value then
         Result := True
      else
         Result := Parent.HasAsParent(Value) ;
   end
   else
      Result := True;
end;

function TrmWordTreeNode.GetChildren: Boolean;
begin
   Result := FChildList.Count > 0;
end;

procedure TrmWordTreeNode.SetParent(Value: TrmWordTreeNode) ;
begin
   if (fParent <> nil) then
      fParent.FChildList.delete(fParent.FChildList.indexOf(self) ) ;

   if value <> nil then
   begin
      FParent := Value;
      if fParent.FChildList.indexof(self) = -1 then
         fParent.FChildList.Add(self) ;
   end;
end;

function TrmWordTreeNode.GetNextSibling: TrmWordTreeNode;
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

function TrmWordTreeNode.GetPrevSibling: TrmWordTreeNode;
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

function TrmWordTreeNode.GetNextChild(Value: TrmWordTreeNode) : TrmWordTreeNode;
begin
   if Value <> nil then
      Result := Value.GetNextSibling
   else
      Result := nil;
end;

function TrmWordTreeNode.GetPrevChild(Value: TrmWordTreeNode) : TrmWordTreeNode;
begin
   if Value <> nil then
      Result := Value.GetPrevSibling
   else
      Result := nil;
end;

function TrmWordTreeNode.GetFirstChild: TrmWordTreeNode;
begin
   if FChildList.Count > 0 then
      Result := FChildList.Items[0]
   else
      Result := nil;
end;

function TrmWordTreeNode.GetLastChild: TrmWordTreeNode;
begin
   if FChildList.Count > 0 then
      Result := FChildList.Items[FChildList.Count - 1]
   else
      Result := nil;
end;

function TrmWordTreeNode.GetNext: TrmWordTreeNode;
var
   N: TrmWordTreeNode;
   P: TrmWordTreeNode;

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

function TrmWordTreeNode.GetPrev: TrmWordTreeNode;
var
   Node: TrmWordTreeNode;

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

function TrmWordTreeNode.GetIndex: Integer;
var
   node : TrmWordTreeNode;
begin
   Result := -1;
   Node := parent;
   if Node = nil then
   begin
      if fowner <> nil then
         FOwner.FRootNodeList.indexof(self)
   end
   else
      result := parent.FChildList.indexof(self);
end;

function TrmWordTreeNode.GetItem(Index: Integer) : TrmWordTreeNode;
begin
   if (index >= 0) and (index < FChildList.count) then
      Result := fchildlist[index]
   else
   begin
      result := nil;
      WTTreeNonViewError('List Index Out of Bounds') ;
   end;
end;

procedure TrmWordTreeNode.SetItem(Index: Integer; Value: TrmWordTreeNode) ;
begin
   item[Index].Assign(Value) ;
end;

function TrmWordTreeNode.IndexOf(Value: TrmWordTreeNode) : Integer;
begin
   Result := fChildList.indexof(Value) ;
end;

function TrmWordTreeNode.GetCount: Integer;
begin
   result := FChildList.count;
end;

function TrmWordTreeNode.GetLevel: Integer;
var
   Node: TrmWordTreeNode;

begin
   Result := 0;
   Node := Parent;
   while Node <> nil do
   begin
      Inc(Result) ;
      Node := Node.Parent;
   end;
end;

procedure TrmWordTreeNode.Delete;
begin
   if HasChildren then
      DeleteChildren;

   if Parent <> nil then
   begin
      Parent.FChildList.Delete(Parent.FChildList.IndexOf(Self) ) ;
      Parent.FChildList.Pack;
   end
   else
   begin
      Owner.FRootNodeList.Delete(Owner.FRootNodeList.IndexOf(Self) ) ;
      Owner.FRootNodeList.Pack;
   end;
   Free;
end;

procedure TrmWordTreeNode.DeleteChildren;
var
   Node: TrmWordTreeNode;

begin
   Node := GetFirstChild;
   while Node <> nil do
   begin
      Node.Delete;
      Node := GetFirstChild;
   end;
end;

procedure TrmWordTreeNode.ReadData(Stream: TStream; Info: PWTNodeInfo) ;
var
   I, Size, ItemCount: Integer;

begin
   Stream.ReadBuffer(Size, SizeOf(Size) ) ;
   Stream.ReadBuffer(Info^, Size) ;
   Letter := Info^.Letter;
   ItemCount := Info^.Count;
   Complete := Info^.Complete;
   for I := 0 to ItemCount - 1 do
      Owner.AddChild(Self, #0) .ReadData(Stream, Info) ;
end;

procedure TrmWordTreeNode.WriteData(Stream: TStream; Info: PWTNodeInfo) ;
var
   I,
      Size,
      ItemCount: Integer;

begin
   Size := SizeOf(TWTNodeInfo) ;
   Info^.Letter := Letter;
   ItemCount := Count;
   Info^.Count := ItemCount;
   Info^.Complete := Complete;
   Stream.WriteBuffer(Size, SizeOf(Size) ) ;
   Stream.WriteBuffer(Info^, Size) ;
   for I := 0 to ItemCount - 1 do
      Item[I].WriteData(Stream, Info) ;
end;

{ TrmWordTreeNodes }

constructor TrmWordTreeNodes.Create(AOwner: TCustomrmWordTree) ;
begin
   inherited Create;
   FOwner := AOwner;
   FRootNodeList := TList.Create;
end;

destructor TrmWordTreeNodes.Destroy;
begin
   Clear;
   FRootNodeList.Free;
   inherited Destroy;
end;

function TrmWordTreeNodes.GetCount: Integer;
var
   N: TrmWordTreeNode;
begin
   N := GetFirstNode;
   Result := 0;
   while N <> nil do
   begin
      Result := Result + 1;
      N := N.GetNext;
   end;
end;

procedure TrmWordTreeNodes.Delete(Node: TrmWordTreeNode) ;
begin
   Node.Delete;
end;

procedure TrmWordTreeNodes.Clear;
var
   N: TrmWordTreeNode;

begin
   N := GetFirstNode;
   While N <> nil do
   begin
      N.Delete;
      N := GetFirstNode;
   end;
end;

function TrmWordTreeNodes.AddChild(Node: TrmWordTreeNode; const Letter: char) : TrmWordTreeNode;
begin
   Result := InternalAddObject(Node, Letter, False, taAdd) ;
end;

function TrmWordTreeNodes.Add(Node: TrmWordTreeNode; const Letter: char) : TrmWordTreeNode;
begin
   if Node <> nil then Node := Node.Parent;
   Result := InternalAddObject(Node, Letter, False, taAdd) ;
end;

function TrmWordTreeNodes.InternalAddObject(Node: TrmWordTreeNode; const Letter: char;
   Complete: boolean; AddMode: TAddMode) : TrmWordTreeNode;
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
                  Result.Complete := complete;
                  Result.Letter := Letter;
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
                  Result.Complete := complete;
                  Result.Letter := Letter;
               except
                  raise;
               end;
            end;
      end;
   except
      raise;
   end;
end;

function TrmWordTreeNodes.GetFirstNode: TrmWordTreeNode;
begin
   if FRootNodeList.Count = 0 then
      Result := nil
   else
      Result := FRootNodeList.Items[0];
end;

function TrmWordTreeNodes.GetNodeFromIndex(Index: Integer) : TrmWordTreeNode;
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
      WTTreeNonViewError('Index out of range') ;
end;

procedure TrmWordTreeNodes.SetItem(Index: Integer; Value: TrmWordTreeNode) ;
begin
   GetNodeFromIndex(Index) .Assign(Value) ;
end;

procedure TrmWordTreeNodes.Assign(Source: TPersistent) ;
var
   TreeNodes: TrmWordTreeNodes;
   MemStream: TMemoryStream;
begin
   if Source is TrmWordTreeNodes then
   begin
      TreeNodes := TrmWordTreeNodes(Source) ;
      Clear;
      MemStream := TMemoryStream.Create;
      try
         TreeNodes.WriteData(MemStream) ;
         MemStream.Position := 0;
         ReadData(MemStream) ;
      finally
         MemStream.Free;
      end;
   end
   else inherited Assign(Source) ;
end;

procedure TrmWordTreeNodes.DefineProperties(Filer: TFiler) ;

   function WriteNodes: Boolean;
   var
      I: Integer;
      Nodes: TrmWordTreeNodes;
   begin
      Nodes := TrmWordTreeNodes(Filer.Ancestor) ;
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

procedure TrmWordTreeNodes.ReadData(Stream: TStream) ;
var
   I, Count: Integer;
   Info: TWTNodeInfo;

begin
   Clear;
   Stream.ReadBuffer(Count, SizeOf(Count) ) ;
   for I := 0 to Count - 1 do
      Add(nil, #0) .ReadData(Stream, @Info) ;
end;

procedure TrmWordTreeNodes.WriteData(Stream: TStream) ;
var
   I: Integer;
   Node: TrmWordTreeNode;
   Info: TWTNodeInfo;

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

{ TCustomrmWordTree }

constructor TCustomrmWordTree.Create(AOwner: TComponent) ;
begin
   inherited Create(AOwner) ;
   FTreeNodes := TrmWordTreeNodes.Create(Self) ;
end;

destructor TCustomrmWordTree.Destroy;
begin
   Items.Free;
   inherited Destroy;
end;

procedure TCustomrmWordTree.SetrmWordTreeNodes(Value: TrmWordTreeNodes) ;
begin
   Items.Assign(Value) ;
end;

function TCustomrmWordTree.CreateNode: TrmWordTreeNode;
begin
   Result := TrmWordTreeNode.Create(Items) ;
end;

function TrmWordTreeNode.IsEqual(Node: TrmWordTreeNode) : Boolean;
begin
   Result := (Letter = Node.Letter) and (Complete = Node.Complete) ;
end;

procedure TrmWordTreeNode.Assign(Source: TPersistent) ;
var
   Node: TrmWordTreeNode;

begin
   if Source is TrmWordTreeNode then
   begin
      Node := TrmWordTreeNode(Source) ;
      Letter := Node.Letter;
      Complete := Node.Complete;
   end
   else
      inherited Assign(Source) ;
end;

function TCustomrmWordTree.AddWord(word: string) : TrmWordTreeNode;
var
   wNode, wLastNode: TrmWordTreeNode;
   wIndex, wLen: integer;
begin
   result := nil;
   if Word = '' then
      exit;

  wNode := LocateWord(word) ;

   if wNode = nil then
   begin

      wIndex := 1;
      wLen := Length(word);
      wLastNode := nil;
      wNode := FTreeNodes.GetFirstNode;
      while (wNode <> nil) and (wIndex <= wLen) do
      begin
         wLastNode := wNode;
         if wNode.Letter = Word[wIndex] then
         begin
            if wIndex < wLen then
            begin
               wNode := wNode.GetFirstChild;
               inc(wIndex) ;
            end
            else
               wNode := nil;
         end
         else
         begin
            if wNode.GetNextSibling = nil then
            begin
               wLastNode := wNode.parent;
               wNode := nil;
            end
            else
               wNode := wNode.GetNextSibling;
         end;
      end;

      if (wIndex <= wLen) then
      begin
         if wIndex > 1 then
            wNode := wLastNode
         else
            wNode := nil;
         while wIndex <= wLen do
         begin
            wNode := fTreeNodes.AddChild(wNode, word[wIndex]) ;
            inc(wIndex) ;
         end;
         wNode.Complete := true;
      end;
   end
   else
      wNode.Complete := true;

   result := wNode;
end;

function TCustomrmWordTree.IsWord(word: string) : Boolean;
var
   wNode : TrmWordTreeNode;
begin
   wNode := locateWord(word);
   result := (wNode <> nil) and (wNode.Complete);
end;

function TCustomrmWordTree.LocateWord(Word: string) : TrmWordTreeNode;
var
   wNode, wLastNode: TrmWordTreeNode;
   wIndex, wLen: integer;
begin
   result := nil;
   if Word = '' then
      exit;

   wIndex := 1;
   wLen := Length(word);
   wLastNode := nil;
   wNode := FTreeNodes.GetFirstNode;
   while (wNode <> nil) and (wIndex <= wLen) do
   begin
      wLastNode := wNode;
      if wNode.Letter = Word[wIndex] then
      begin
         if wIndex < wLen then
         begin
            wNode := wNode.GetFirstChild;
            inc(wIndex) ;
         end
         else
            wNode := nil;
      end
      else
         wNode := wNode.GetNextSibling;
   end;
   if assigned(wLastNode) then
   begin
      if wLastNode.Complete and (wLastNode.Word = Word) then
         result := wLastNode;
   end
end;

procedure TCustomrmWordTree.LoadPartialMatches(Word: String; MatchList: TStrings) ;

   procedure RecurseNodes(Node: TrmWordTreeNode) ;
   var
      wChild: TrmWordTreeNode;
   begin
      if Node <> nil then
      begin
         if Node.Complete then
            MatchList.Add(Node.Word) ;

         wChild := Node.GetFirstChild;
         while wChild <> nil do
         begin
            RecurseNodes(wChild) ;
            wChild := Node.GetNextChild(wChild) ;
         end;
      end;
   end;

var
   wLastNode, wNode: TrmWordTreeNode;
   wLen, wIndex: integer;
begin
   MatchList.clear;

   wNode := FTreeNodes.GetFirstNode;
   if Word = '' then
   begin
      While wNode <> nil do
      begin
         RecurseNodes(wNode) ;
         wNode := wNode.GetNextSibling;
      end;
   end
   else
   begin
      wIndex := 1;
      wLen := Length(word);
      wLastNode := nil;
      while (wNode <> nil) and (wIndex <= wLen) do
      begin
         wLastNode := wNode;
         if wNode.Letter = Word[wIndex] then
         begin
            if wIndex < wLen then
            begin
               wNode := wNode.GetFirstChild;
               inc(wIndex) ;
            end
            else
               wNode := nil;
         end
         else
            wNode := wNode.GetNextSibling;
      end;
      if assigned(wLastNode) then
         RecurseNodes(wLastNode) ;
   end;
end;

function TrmWordTreeNode.getWord: string;
var
   wNode : TrmWordTreeNode;
begin
   result := '';
   
   if not Complete then
      exit;

   wNode := self;
   while wNode <> nil do
   begin
       result := wNode.letter + result;
       wNode := wNode.Parent;
   end;
end;

procedure TCustomrmWordTree.LoadSEMatches(Word: String; MatchList: TStrings);
var
   wNode: TrmWordTreeNode;
   wChar : char;
   wWordSE : string;
   wSELen : integer;
   wloop : integer;
   wSEChar : string;
   wWordLen : integer;
   wUpWord : string;


   function TestWord(NodeWord:string):boolean;
   var
      wCount : integer;
      wloop : integer;
   begin
      result := false;

      if uppercase(NodeWord) = wUpWord then
         exit;

      if (wWordSE = soundex(NodeWord, true, wSELen)) then
      begin
         wcount := 0;
         for wloop := 1 to wSELen do
         begin
            if pos(wSEChar[wloop], NodeWord) = 0 then
               inc(wcount);
         end;
         result := (wCount < 2) and (abs(length(NodeWord) - wWordLen) < 2);
      end;
   end;

   procedure RecurseNodes(Node: TrmWordTreeNode) ;
   var
      wChild: TrmWordTreeNode;
   begin
      if Node <> nil then
      begin
         if Node.Complete and TestWord(Node.word) then
            MatchList.Add(Node.Word + ' ('+Soundex(Node.Word, true, wSELen)+')' ) ;

         wChild := Node.GetFirstChild;
         while wChild <> nil do
         begin
            RecurseNodes(wChild) ;
            wChild := Node.GetNextChild(wChild) ;
         end;
      end;
   end;

begin
   MatchList.Clear;

   word := trim(word);

   if word = '' then
      exit;

   wSELen := 0;
   wloop := length(word);
   wSEChar := '';
   while wloop > 1 do
   begin
      if not (word[wloop] in ['A','E','I','O','U']) then
      begin
         inc(wSELen);
         if (wSEChar = '') or ((wSEChar <> '') and (word[wloop] <> wSEChar[1])) then
            wSEChar := word[wloop] + wSEChar;
      end;

      dec(wLoop);
   end;

   if wSELen < 4 then
      wSELen := 4;

   wWordSE := Soundex(Word, true, wSELen);
   wChar := word[1];
   wNode := FTreeNodes.GetFirstNode;
   wWordLen := length(word);
   wUpWord := uppercase(word);

   While wNode <> nil do
   begin
      if lowercase(wNode.Letter) = lowercase(wChar) then
         RecurseNodes(wNode);
      wNode := wNode.GetNextSibling;
   end;
end;

end.

