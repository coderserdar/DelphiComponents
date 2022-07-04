unit OverbyteIcsAvlTrees;
{/////////////////////////////////////////////////////////////////////////////

Author:       Arno Garrels
Creation:     April 6, 2006
Description:  Implements a fast cache-like data storage based on two
              linked AVL-Trees for primary and secondary indexing.
              Primary key of type string has to be unique, secondary
              key of type TDateTime may have duplicates.
              AVL or balanced binary trees are extremely efficient
              data structures for searching data. Finding an element in
              65536 requires at most 16 compares.
              Uses an AVL-Tree as it is described in the book
              "Algorithms & Data Structures", Prof. Niklaus Wirth.
Version:      1.04
EMail:        Arno Garrels <arno.garrels@gmx.de>
Support:      Don't expect any support, however please report bugs/fixes.
Credits:      Many thanks to Benjamin Stadin <stadin@gmx.de>, without
              his help and his initial developed storage class I won't
              have written this unit.
Legal issues: This code is hereby placed in the public domain, with the
              wish that this text is not removed from the source.
              Software is distributed on an "AS IS" basis,
              WITHOUT WARRANTY OF ANY KIND, either express or implied.

History:
Apr  08, 2006  So far tested with Delphi 5, 7, 2006.
Apr  09, 2006  Function Insert updated.
Apr  09, 2006  New field Expires added.
June 05, 2006  Exchanged default TDateTime value 0 by MinDT.
Aug  13, 2008  TCacheTree uses own CompareStr(UnicodeString) and
               AnsiCompareText() rather than CompareText(), if no
               case-sensitive key searches shall be performed.
Mar  15, 2009  Fixed a memory leak with secondary duplicated index.
Mar  16, 2009  CompareStr ignored the first char (Unicode only), uses
               Windows.pas to avoid a compiler warning.

/////////////////////////////////////////////////////////////////////////////}

interface

uses
    Windows,
    SysUtils,
    Classes;

const
    MinDT = -657434.0;      { 01/01/0100 12:00:00.000 AM }

type
    TCompareFunction = function(const S1, S2: String): Integer;

    TBal = -1..1;

    TAvlTreeNode = class             // Base node should be overridden!!
    private
        Bal   : TBal;
        Left  : TAvlTreeNode;
        Right : TAvlTreeNode;
    end;

    TAvlTree = class(TObject)        // Base tree must be overridden!!
    private
        FFoundNode   : TAvlTreeNode;
        FRoot        : TAvlTreeNode;
        FCount       : Integer;
        procedure   InternalSearchAndInsert(Node: TAvlTreeNode; var P: TAvlTreeNode; var HChange: Boolean; var Found: Boolean); virtual;
    protected
        procedure   DeleteNode(Node: TAvlTreeNode; var P: TAvlTreeNode; var HChange: Boolean; var Ok: Boolean);
        function    SearchNode(Node: TAvlTreeNode; var P: TAvlTreeNode): TAvlTreeNode;
        function    SearchAndInsert(Node: TAvlTreeNode; var Found: Boolean): TAvlTreeNode; virtual;
        procedure   BalanceLeft(var P: TAvlTreeNode; var HChange: Boolean; DL: Boolean);
        procedure   BalanceRight(var P: TAvlTreeNode; var HChange: Boolean; DL: Boolean);
        procedure   ListNodes(var P: TAvlTreeNode; var Cancel: Boolean);
        procedure   InternalClear(CurNode: TAvlTreeNode); virtual;
        procedure   DoBeforeDelete(Node: TAvlTreeNode); virtual; 
        procedure   DoListNode(Node: TAvlTreeNode; var Cancel: Boolean); virtual; abstract;
        procedure   Clear; virtual;
        // Node1 < Node2 :-1  Node1=Node2 :0  Node1 > Node2 :+1
        function    Compare(Node1, Node2: TAvlTreeNode): Integer; virtual; abstract;
        procedure   CopyNode(Source, Destination: TAvlTreeNode);  virtual; abstract;
    public
        constructor Create; virtual;
        destructor  Destroy; override;
        function    Remove(Node: TAvlTreeNode): Boolean; virtual;
        function    Search(Node: TAvlTreeNode): TAvlTreeNode;
        procedure   ListTree;
        function    First: TAvlTreeNode;
        function    Last: TAvlTreeNode;
        property    Count: Integer read FCount;
        property    Root: TAvlTreeNode read FRoot;
    end;

    TSecIdxTree = class(TAvlTree)
    protected
        procedure   InternalClear(CurNode: TAvlTreeNode); override;
        procedure   Clear; override;
        procedure   DoListNode(Node: TAvlTreeNode; var Cancel: Boolean); override;
        function    Compare(Node1, Node2: TAvlTreeNode): Integer; override;
        procedure   CopyNode(Source, Destination: TAvlTreeNode);  override;
    public
        function    Remove(Node: TAvlTreeNode): Boolean; override;
    end;

    TCacheListEvent = procedure(Sender: TObject; const Key: String; TimeStamp: TDateTime; Data: Pointer; Len: Integer; Expires: TDateTime; var Cancel: Boolean) of object;
    TCacheFreeData = procedure(Sender: TObject; Data: Pointer; Len: Integer) of object;

    TCacheNode = class;    // forward
    TCacheIdxNode = class; // forward

    TCacheTree = class(TAvlTree)
    private
        FSecIdxTree     : TSecIdxTree;
        FCaseSensitive  : Boolean;
        FOnFreeData     : TCacheFreeData;
        FOnList         : TCacheListEvent;
        FTmpNode        : TCacheNode;
    protected
        procedure   DoBeforeDelete(Node: TAvlTreeNode); override;
        procedure   DoListNode(Node: TAvlTreeNode; var Cancel: Boolean); override;
        function    Compare(Node1, Node2: TAvlTreeNode): Integer; override;
        procedure   CopyNode(Source, Destination: TAvlTreeNode);  override;
        procedure   TriggerFreeData(Data: Pointer; Len: Integer); virtual;
        procedure   InternalClear(CurNode : TAvlTreeNode); override;
    public
        constructor Create(CaseSensitive: Boolean = True); reintroduce;
        destructor  Destroy; override;
        procedure   Clear; override;
        procedure   Insert(Key: String; Data: Pointer; Len: Integer; TimeStamp: TDateTime = MinDT; Expires: TDateTime = MinDT; UpdateTime: Boolean = True; UpdateData: Boolean = True);
        procedure   Flush(UpTo: TDateTime);
        function    Remove(Node: TAvlTreeNode): Boolean; override;
        function    RemoveKey(const Key: String): Boolean;
        function    Oldest: TCacheNode;
        function    FindKey(const AKey: String): TCacheNode;
        property    OnFreeData: TCacheFreeData read FOnFreeData write FOnFreeData;
        property    OnList: TCacheListEvent read FOnList write FOnList;
    end;

    TCacheNode = class(TAvlTreeNode)
    private
        FKey     : String;
        FData    : Pointer;
        FLen     : Integer;
        FIdxRef  : TCacheIdxNode;
    public
        constructor Create(Key: String; Data: Pointer; Len: Integer);
        destructor  Destroy; override;
        property    Key: String read FKey;
        property    Data: Pointer read FData;
        property    Len: Integer read FLen;
        property    IdxRef: TCacheIdxNode read FIdxRef;
    end;

    TSecIdxDuplicates = class; // forward

    TCacheIdxNode = class(TAvlTreeNode)
    private
        FCacheRef  : TCacheNode;
        FTimeStamp : TDateTime;   // index
        FExpires   : TDateTime;
        FDups      : TSecIdxDuplicates;
        FNext      : TCacheIdxNode;
        FPrev      : TCacheIdxNode;
    public
        constructor Create(ACacheRef: TCacheNode; ATimeStamp: TDateTime = MinDT; AExpires: TDateTime = MinDT);
        destructor  Destroy; override;
        property    TimeStamp: TDateTime read FTimeStamp;
        property    Expires: TDateTime read FExpires;
    end;

    // Manages duplicated secondary index nodes, implemented as a doubly-linked lists
    TSecIdxDuplicates = class
    private
        FLast  : TCacheIdxNode;
        FFirst : TCacheIdxNode;
    public
        constructor Create; virtual;
        procedure   Clear;
        procedure   InsertAfter(ANode, NewNode : TCacheIdxNode);
        procedure   InsertBefore(ANode, NewNode : TCacheIdxNode);
        procedure   InsertTop(NewNode : TCacheIdxNode);
        procedure   InsertEnd(NewNode : TCacheIdxNode);
        procedure   Remove(ANode: TCacheIdxNode);
    end;

implementation

/////////////////////////////////////////////////////////////////////////////
constructor TAvlTree.Create;
begin
    inherited Create;
    FRoot  := nil;
    FCount := 0;
end;


/////////////////////////////////////////////////////////////////////////////
destructor TAvlTree.Destroy;
begin
    Clear;
    inherited Destroy;
end;


/////////////////////////////////////////////////////////////////////////////
procedure TAvlTree.InternalSearchAndInsert(
    Node        : TAvlTreeNode;
    var P       : TAvlTreeNode;
    var HChange : Boolean;
    var Found   : Boolean);
var
    Cmp : Integer;
begin
    Found := False;
    if P = nil then
    begin
        P := Node;
        HChange := True;
        Inc(FCount);
        with P do
        begin
            if FRoot = nil then
                FRoot := P;
            Left   := nil;
            Right  := nil;
            Bal    := 0;
        end;
    end
    else begin
        Cmp := Compare(P, Node);
        if (Cmp > 0) then       //  < Current
        begin
            InternalSearchAndInsert(Node, P.Left, HChange, Found);
            if HChange and not Found then
                BalanceLeft(P, HChange, False);
        end
        else if (Cmp < 0) then  //  > Current
        begin
            InternalSearchAndInsert(Node, P.Right, HChange, Found);
            if HChange and not Found then
                BalanceRight(P, HChange, False);
        end
        else begin
            HChange    := False;
            Found      := True;
            FFoundNode := P;
        end;
    end;    
end;


/////////////////////////////////////////////////////////////////////////////
function TAvlTree.SearchNode(
    Node  : TAvlTreeNode;
    var P : TAvlTreeNode): TAvlTreeNode;
var
    Cmp : Integer;
begin
    Result := nil;
    if (P <> nil) then
    begin
        Cmp := Compare(P, Node);
        if Cmp = 0 then
            Result := P
        else begin
            if Cmp > 0 then
                Result := SearchNode(Node, P.Left)
            else
                Result := SearchNode(Node, P.Right)
        end;
    end;
end;


/////////////////////////////////////////////////////////////////////////////
procedure TAvlTree.BalanceRight(
    var P       : TAvlTreeNode;
    var HChange : Boolean;
    Dl          : Boolean);
var
    B1 : TAvlTreeNode;
    B2 : TAvlTreeNode;
begin
    {HChange = true, right branch has become less high}
    case P.Bal of
        -1 :
        begin
            P.Bal := 0;
            if not DL then HChange := False;
        end;
        0 :
        begin
            P.Bal := +1;
            if DL then HChange := False;
        end;
        +1 :
        begin    {Rebalance}
            B1 := P.Right;
            if (B1.Bal = +1) or ((B1.Bal = 0) and DL) then  // single RR rotation
            begin
                P.Right := B1.Left;
                B1.Left := P;
                if not DL then
                    P.Bal := 0
                else begin
                    if B1.Bal = 0 then
                    begin
                        P.Bal   := +1;
                        B1.Bal  := -1;
                        HChange := False;
                    end
                    else begin
                        P.Bal  := 0;
                        B1.Bal := 0;
                    end;
                end;
                P := B1;
            end
            else begin                                   // double RL rotation
                B2       := B1.Left;
                B1.Left  := B2.Right;
                B2.Right := B1;
                P.Right  := B2.Left;
                B2.Left  := P;

                if B2.Bal = +1 then P.Bal  := -1 else P.Bal  := 0;
                if B2.Bal = -1 then B1.Bal := +1 else B1.Bal := 0;
                P := B2;
                if DL then B2.Bal := 0;
            end;
            if not DL then
            begin
                P.Bal := 0;
                HChange := False;
            end;
        end;
    end; // case
end;


/////////////////////////////////////////////////////////////////////////////
procedure TAvlTree.BalanceLeft(
    var P       : TAvlTreeNode;
    var HChange : Boolean;
    DL: Boolean);
var
    B1,
    B2 : TAvlTreeNode;
begin
    {HChange = true, left branch has become less high}
    case P.Bal of
        1 :
        begin
            P.Bal := 0;
            if not DL then HChange:= False;
        end;
        0 :
        begin
            P.Bal := -1;
            if DL then  HChange := False;
        end;
        -1 :
        begin
            B1 := P.Left;
            if (B1.Bal = -1) or ((B1.Bal = 0) and DL) then // single LL rotation
            begin
                P.Left   := B1.Right;
                B1.Right := P;
                if not DL then
                    P.Bal := 0
                else begin
                    if B1.Bal = 0 then
                    begin
                        P.Bal  := -1;
                        B1.Bal := +1;
                        HChange := False;
                    end
                    else begin
                        P.Bal  := 0;
                        B1.Bal := 0;
                    end;
                end;
                P := B1;
            end
            else begin                                     // double LR rotation
                B2       := B1.Right;
                B1.Right := B2.Left;
                B2.Left  := B1;
                P.Left   := B2.Right;
                B2.Right := P;
                if B2.Bal = -1 then P.Bal  := +1 else P.Bal  := 0;
                if B2.Bal = +1 then B1.Bal := -1 else B1.Bal := 0;
                P := B2;
                if DL then B2.Bal := 0;
            end;
            if not DL then
            begin
                P.Bal := 0;
                HChange := False;
            end;
        end;
    end;
end;


/////////////////////////////////////////////////////////////////////////////
procedure TAvlTree.DeleteNode(
    Node        : TAvlTreeNode;
    var P       : TAvlTreeNode;
    var HChange : Boolean;
    var Ok      : Boolean);
var
    Q   : TAvlTreeNode;
    Cmp : Integer;

 procedure Del(var R: TAvlTreeNode; var HChange: Boolean);
 begin
    if R.Right <> nil then
    begin
        Del(R.Right, HChange);
        if HChange then BalanceLeft(R, HChange, True);
    end
    else begin
        CopyNode(R, Q);  { Source, Destination, Q.Key := R.Key; }
        Q := R;
        R := R.Left;
        HChange := True;
    end;
 end;

begin
    Ok := True;
    if (P = nil) then
    begin
        Ok := False;
        HChange := False;
        Exit;
    end;
    Cmp := Compare(P, Node);
    if (Cmp > 0) then
    begin
        DeleteNode(Node, P.Left, HChange, Ok);
        if HChange then BalanceRight(P, HChange, True);
    end
    else if (Cmp < 0) then
    begin
        DeleteNode(Node, P.Right, HChange, Ok);
        if HChange then BalanceLeft(P, HChange, True);
    end
    else begin // Remove Q
        Q := P;
        if Q.Right = nil then
        begin
            P := Q.Left;
            HChange := True;
        end
        else if (Q.Left = nil) then
        begin
            P := Q.Right;
            HChange := True;
        end
        else begin
            Del(Q.Left, HChange);
            if HChange then
                BalanceRight(P, HChange, True);
        end;
        DoBeforeDelete(Q);
        Dec(FCount);
        Q.Free;
        Q := nil;
    end;
end;


/////////////////////////////////////////////////////////////////////////////
procedure TAvlTree.DoBeforeDelete(Node: TAvlTreeNode);
begin
end;


/////////////////////////////////////////////////////////////////////////////
function TAvlTree.SearchAndInsert(
    Node      : TAvlTreeNode;
    var Found : Boolean): TAvlTreeNode;
var
    H : Boolean;
begin
    InternalSearchAndInsert(Node, FRoot, H, Found);
    if Found then
        Result := FFoundNode
    else
        Result := Node;
end;


/////////////////////////////////////////////////////////////////////////////
function TAvlTree.Remove(Node: TAvlTreeNode): Boolean;
var
    H : Boolean;
begin
    Result := False;
    DeleteNode(Node, FRoot, H, Result);
end;


/////////////////////////////////////////////////////////////////////////////
function TAvlTree.Search(Node: TAvlTreeNode): TAvlTreeNode;
begin
    Result := SearchNode(Node, FRoot);
end;


/////////////////////////////////////////////////////////////////////////////
procedure TAvlTree.ListNodes(var P: TAvlTreeNode; var Cancel: Boolean);
begin
    if P <> nil then
    begin
        if (P.Left <> nil) then
            ListNodes(P.Left, Cancel);
        if Cancel then Exit;
        DoListNode(P, Cancel);
        if Cancel then Exit;
        if (P.Right <> nil) then
            ListNodes(P.Right, Cancel);
    end;
end;


/////////////////////////////////////////////////////////////////////////////
procedure TAvlTree.ListTree;  // uses Node.ListNode recursively
var
    Cancel : Boolean;
begin
    Cancel := False;
    ListNodes(FRoot, Cancel);
end;


/////////////////////////////////////////////////////////////////////////////
function TAvlTree.First: TAvlTreeNode;
begin
    Result := FRoot;
    if Assigned(Result) then
        while Assigned(Result.Left) do
            Result := Result.Left;
end;


/////////////////////////////////////////////////////////////////////////////
function TAvlTree.Last: TAvlTreeNode;
begin
    Result := FRoot;
    if Assigned(Result) then
        while Assigned(Result.Right) do
            Result := Result.Right;
end;


/////////////////////////////////////////////////////////////////////////////
procedure TAvlTree.InternalClear(CurNode : TAvlTreeNode);
var
    P : TAvlTreeNode;
begin
    if CurNode = nil then
        Exit;
    InternalClear(CurNode.Left);
    P := CurNode.Right;
    CurNode.Free;
    //CurNode := nil;
    InternalClear(P);
end;


/////////////////////////////////////////////////////////////////////////////
procedure TAvlTree.Clear;
begin
    InternalClear(FRoot);
    FRoot := nil;
    FCount := 0;
end;


/////////////////////////////////////////////////////////////////////////////
{ TCacheTree }

{$IFDEF UNICODE}
function CompareStr(const S1, S2: UnicodeString): Integer;
var
    L1, L2, I : Integer;
    MinLen    : Integer;
    P1, P2    : PWideChar;
begin
    L1 := Length(S1);
    L2 := Length(S2);
    if L1 > L2 then
        MinLen := L2
    else
        MinLen := L1;
    P1 := Pointer(S1);
    P2 := Pointer(S2);
    for I := 0 to MinLen do
    begin
        if (P1[I] <> P2[I]) then
        begin
            Result := Ord(P1[I]) - Ord(P2[I]);
            Exit;
        end;
    end;
    Result := L1 - L2;
end;
{$ENDIF}


/////////////////////////////////////////////////////////////////////////////
constructor TCacheTree.Create(CaseSensitive: Boolean = True);
begin
    inherited Create;
    FCaseSensitive := CaseSensitive;
    FSecIdxTree    := TSecIdxTree.Create;
    FTmpNode       := TCacheNode.Create('', nil, 0);
end;


/////////////////////////////////////////////////////////////////////////////
destructor TCacheTree.Destroy;
begin
    FSecIdxTree.Free;
    FSecIdxTree := nil;
    FTmpNode.Free;
    inherited Destroy;
end;


/////////////////////////////////////////////////////////////////////////////
procedure TCacheTree.Insert(
    Key           : String;
    Data          : Pointer;
    Len           : Integer;
    TimeStamp     : TDateTime = MinDT;
    Expires       : TDateTime = MinDT;
    UpdateTime    : Boolean = True;
    UpdateData    : Boolean = True);
var
    CacheNode,
    ResNode : TCacheNode;
    NewIdx,
    ResIdx : TCacheIdxNode;
    Found : Boolean;
begin
    CacheNode := TCacheNode.Create(Key, Data, Len);
    FFoundNode := nil;
    ResNode := TCacheNode(SearchAndInsert(CacheNode, Found));
    if not Found then  // Primary key not found = new cache Node added
    begin
        NewIdx := TCacheIdxNode.Create(CacheNode, TimeStamp, Expires);
        ResIdx := TCacheIdxNode(FSecIdxTree.SearchAndInsert(NewIdx, Found));
        if not Found then   // New TimeStamp inserted
            CacheNode.FIdxRef := NewIdx
        else begin          // TimeStamp exists, add a duplicate
            if not Assigned(ResIdx.FDups) then
            begin
                ResIdx.FDups := TSecIdxDuplicates.Create;
                ResIdx.FDups.InsertEnd(ResIdx);
            end;
            ResIdx.FDups.InsertEnd(NewIdx);
            CacheNode.FIdxRef := NewIdx;
        end;
    end
    else begin // Primary key found - update data and secondary index
        if UpdateData then
        begin
            // Old data needs to be freed
            TriggerFreeData(ResNode.FData, ResNode.FLen);
            // Update Data
            ResNode.FData := Data;
            ResNode.FLen  := Len;
        end;
        if UpdateTime then
        begin
            //Update TimeStamp (delete and new)
            FSecIdxTree.Remove(ResNode.FIdxRef);
            NewIdx := TCacheIdxNode.Create(ResNode, TimeStamp, Expires);
            ResIdx := TCacheIdxNode(FSecIdxTree.SearchAndInsert(NewIdx, Found));
            if not Found then
                ResNode.FIdxRef := NewIdx
            else begin   // Time value exists, create a duplicate
                if not Assigned(ResIdx.FDups) then
                begin
                    ResIdx.FDups := TSecIdxDuplicates.Create;
                    ResIdx.FDups.InsertEnd(ResIdx);
                end;
                ResIdx.FDups.InsertEnd(NewIdx);
                ResNode.FIdxRef := NewIdx
            end;
        end;

        // not new
        FreeAndNil(CacheNode);
    end;
end;


/////////////////////////////////////////////////////////////////////////////
function TCacheTree.Remove(Node: TAvlTreeNode): Boolean;
begin
    Result := False;
    if not Assigned(Node) then
        Exit;
    Result := inherited Remove(Node); // Calls destructor
end;


/////////////////////////////////////////////////////////////////////////////
function TCacheTree.RemoveKey(const Key: String): Boolean;
begin
    FTmpNode.FKey := Key;
    Result := inherited Remove(FTmpNode); // Calls destructor
end;


/////////////////////////////////////////////////////////////////////////////
procedure TCacheTree.DoBeforeDelete(Node: TAvlTreeNode);
var
    IdxNode : TCacheIdxNode;
begin
    IdxNode := TCacheNode(Node).FIdxRef;
    FSecIdxTree.Remove(IdxNode);
    TriggerFreeData(TCacheNode(Node).FData, TCacheNode(Node).FLen);

    inherited DoBeforeDelete(Node);
end;


/////////////////////////////////////////////////////////////////////////////
procedure TCacheTree.DoListNode(Node: TAvlTreeNode; var Cancel: Boolean);
begin
    if Assigned(Node) and Assigned(FOnList) then
        FOnList(Self, TCacheNode(Node).FKey, TCacheNode(Node).FIdxRef.FTimeStamp,
                TCacheNode(Node).FData, TCacheNode(Node).FLen,
                TCacheNode(Node).IdxRef.FExpires, Cancel);
end;


/////////////////////////////////////////////////////////////////////////////
procedure TCacheTree.TriggerFreeData(Data: Pointer; Len: Integer);
begin
    if Assigned(FOnFreedata) and (Data <> nil) then
        FOnFreeData(Self, Data, Len);
end;


/////////////////////////////////////////////////////////////////////////////
// a < self :-1  a=self :0  a > self :+1
function TCacheTree.Compare(Node1, Node2: TAvlTreeNode): Integer;
begin
    if FCaseSensitive then
        Result := CompareStr(TCacheNode(Node1).FKey, TCacheNode(Node2).FKey)
    else
        Result := AnsiCompareText(TCacheNode(Node1).FKey, TCacheNode(Node2).FKey);

end;


/////////////////////////////////////////////////////////////////////////////
procedure TCacheTree.CopyNode(Source, Destination: TAvlTreeNode);
var
    IdxRef   : TCacheIdxNode;
    CacheRef : TCacheNode;
    Dest     : TCacheNode;
    Src      : TCacheNode;
    Data     : Pointer;
    Len      : Integer;
begin
    { Object Source will be deleted, Destination has still the Data and
      Key to be deleted, Destination is kept. }

    Dest := TCacheNode(Destination); // avoids many casts?
    Src  := TCacheNode(Source);

  //Swap referer-pointers
    IdxRef       := Src.FIdxRef;
    Src.FIdxRef  := Dest.FIdxRef;
    Dest.FIdxRef := IdxRef;

    CacheRef               := Src.FIdxRef.FCacheRef;
    Src.FIdxRef.FCacheRef  := Dest.FIdxRef.FCacheRef;
    Dest.FIdxRef.FCacheRef := CacheRef;

 // Swap data pointers
    Data := Src.FData;
    Src.FData  := Dest.FData;
    Dest.FData := Data;

 // Swap length
    Len       := Src.FLen;
    Src.FLen  := Dest.FLen;
    Dest.FLen := Len;

  //Copy key
    Dest.FKey  := Src.FKey;
end;


/////////////////////////////////////////////////////////////////////////////
function TCacheTree.Oldest: TCacheNode;
var
    AvlNode : TAvlTreeNode;
begin
    AvlNode := FSecIdxTree.First;
    if AvlNode <> nil then
        Result := TCacheIdxNode(FSecIdxTree.First).FCacheRef
    else
        Result := nil;
end;


/////////////////////////////////////////////////////////////////////////////
procedure TCacheTree.InternalClear(CurNode: TAvlTreeNode);
var
    P : TAvlTreeNode;
begin
    if CurNode = nil then Exit;
    InternalClear(CurNode.Left);
    P := CurNode.Right;
    TriggerFreeData(TCacheNode(CurNode).FData, TCacheNode(CurNode).FLen);
    CurNode.Free;
    InternalClear(P); 
end;


/////////////////////////////////////////////////////////////////////////////
procedure TCacheTree.Clear;
begin
    if Assigned(FSecIdxTree) then
        FSecIdxTree.Clear;
    inherited Clear;
end;


/////////////////////////////////////////////////////////////////////////////
function TCacheTree.FindKey(const AKey: String): TCacheNode;
begin
    FTmpNode.FKey := AKey;
    Result := TCacheNode(Search(FTmpNode));
end;


/////////////////////////////////////////////////////////////////////////////
procedure TCacheTree.Flush(UpTo: TDateTime);
var
    Node : TCacheNode;
begin
    Node := Oldest;
    if Node = nil then Exit;
    while (Node <> nil) and (Node.FIdxRef.FTimeStamp <= UpTo) do
    begin
        Remove(Node);
        Node := Oldest;
    end;
end;


/////////////////////////////////////////////////////////////////////////////
{ TCacheNode }

constructor TCacheNode.Create(Key: String; Data: Pointer; Len: Integer);
begin
    inherited Create;
    FData       := Data;
    FLen        := Len;
    Self.FKey   := Key;
    FIdxRef     := nil;
end;


/////////////////////////////////////////////////////////////////////////////
destructor TCacheNode.Destroy;
begin
    inherited Destroy;
end;


/////////////////////////////////////////////////////////////////////////////
{ TCacheIdxNode }

constructor TCacheIdxNode.Create(ACacheRef: TCacheNode;
  ATimeStamp: TDateTime = MinDT; AExpires: TDateTime = MinDT);
begin
    inherited Create;
    FNext     := nil;
    FPrev     := nil;
    FDups     := nil;
    if ATimeStamp = MinDT then
        FTimeStamp := Now
    else
        FTimeStamp := ATimeStamp;
    if AExpires = MinDT then
        FExpires := FTimeStamp
    else
        FExpires := AExpires;
    FCacheRef := ACacheRef;
end;


/////////////////////////////////////////////////////////////////////////////
destructor TCacheIdxNode.Destroy;
begin
    if Assigned(FDups) and (FDups.FFirst = nil) then
    begin
        FDups.Free;
        FDups := nil;
    end;
    inherited Destroy;
end;


/////////////////////////////////////////////////////////////////////////////
// a < self :-1  a=self :0  a > self :+1
function TSecIdxTree.Compare(Node1, Node2: TAvlTreeNode): Integer;
begin
    //if Abs(TCacheIdxNode(Node1).FTime - TCacheIdxNode(Node2).FTime) < OneMillisecond then
    if TCacheIdxNode(Node1).FTimeStamp = TCacheIdxNode(Node2).FTimeStamp then
        Result := 0
    else if TCacheIdxNode(Node1).FTimeStamp < TCacheIdxNode(Node2).FTimeStamp then
        Result := -1
    else
        Result := 1;
end;


/////////////////////////////////////////////////////////////////////////////
procedure TSecIdxTree.CopyNode(Source, Destination: TAvlTreeNode);
var
    TmpRef : TCacheNode;
    IdxRef : TCacheIdxNode;
    Dups   : TSecIdxDuplicates;
    Dest   : TCacheIdxNode;
    Src    : TCacheIdxNode;
begin
    { Object Source will be deleted, Destination has still the Data and
      Key to be deleted, Destination is kept. }

    Dest := TCacheIdxNode(Destination); // Avoids many casts
    Src  := TCacheIdxNode(Source);

  //Swap the referer-pointers
    TmpRef         := Src.FCacheRef;
    Src.FCacheRef  := Dest.FCacheRef;
    Dest.FCacheRef := TmpRef;

    IdxRef                 := Src.FCacheRef.FIdxRef;
    Src.FCacheRef.FIdxRef  := Dest.FCacheRef.FIdxRef;
    Dest.FCacheRef.FIdxRef := IdxRef; 

    Dups       := Src.FDups;
    Src.FDups  := Dest.FDups;
    Dest.FDups := Dups;

    if Assigned(Dups) then
    begin
        Dups.Remove(Src);
        Dups.InsertTop(Dest);
    end;

    Dest.FTimeStamp := Src.FTimeStamp;
    Dest.FExpires   := Src.FExpires;
end;


/////////////////////////////////////////////////////////////////////////////
procedure TSecIdxTree.InternalClear(CurNode : TAvlTreeNode);
var
    P       : TAvlTreeNode;
    Dup,
    Tmp     : TCacheIdxNode;
    Dups    : TSecIdxDuplicates;
    NilFlag : Boolean;
begin
    if CurNode = nil then
        Exit;
    InternalClear(CurNode.Left);
    P := CurNode.Right;
    NilFlag := False;
    Dups := TCacheIdxNode(CurNode).FDups;
    if Assigned(Dups) then
    begin
        NilFlag := Dups.FLast <> Dups.FFirst;
        Dup := Dups.FFirst;
        while Assigned(Dup) do
        begin
            Tmp := Dup.FNext;
            Dups.Remove(Dup);
            if Dup <> CurNode then
                Dup.Free;
            Dup := Tmp;
        end;
    end;
    if NilFlag then
        TCacheIdxNode(CurNode).FDups := nil;
    CurNode.Free;
    InternalClear(P);
end;


/////////////////////////////////////////////////////////////////////////////
procedure TSecIdxTree.Clear;
begin
    InternalClear(FRoot);
    FRoot  := nil;
    FCount := 0;
end;


/////////////////////////////////////////////////////////////////////////////
function TSecIdxTree.Remove(Node: TAvlTreeNode): Boolean;
var
    SecIdx : TCacheIdxNode;
begin
    Result := FALSE;
    if Node = nil then Exit;
    SecIdx := TCacheIdxNode(Node);
    if SecIdx.FDups <> nil then
    begin
        if SecIdx.FDups.FFirst.FNext = nil then
        begin
            SecIdx.FDups.Remove(SecIdx);
            Result := inherited Remove(SecIdx);
        end
        else begin
            { We have multiple duplicates and this is the first one. }
            if SecIdx = SecIdx.FDups.FFirst then
            begin
                SecIdx.FTimeStamp        := SecIdx.FNext.FTimeStamp;
                SecIdx.FExpires          := SecIdx.FNext.FExpires;
                SecIdx.FCacheRef         := SecIdx.FNext.FCacheRef;
                SecIdx.FCacheRef.FIdxRef := SecIdx;
                SecIdx := SecIdx.FNext;
            end;
            SecIdx.FDups.Remove(SecIdx);
            SecIdx.Free;
            Result := TRUE;
        end;
    end
    else
        Result := inherited Remove(SecIdx);
end;


/////////////////////////////////////////////////////////////////////////////
procedure TSecIdxTree.DoListNode(Node: TAvlTreeNode; var Cancel: Boolean);
begin
end;


/////////////////////////////////////////////////////////////////////////////
{ TSecIdxDuplicates }

constructor TSecIdxDuplicates.Create;
begin
    inherited Create;
    FLast  := nil;
    FFirst := nil;
end;


/////////////////////////////////////////////////////////////////////////////
procedure TSecIdxDuplicates.Clear;
begin
    FLast  := nil;
    FFirst := nil;
end;


/////////////////////////////////////////////////////////////////////////////
procedure TSecIdxDuplicates.InsertAfter(ANode, NewNode: TCacheIdxNode);
begin
    NewNode.FDups := Self;
    NewNode.FPrev := ANode;
    NewNode.FNext := ANode.FNext;
    if ANode.FNext = nil then
         FLast := NewNode
    else
         ANode.FNext.FPrev := NewNode;
    ANode.FNext := NewNode;
end;


/////////////////////////////////////////////////////////////////////////////
procedure TSecIdxDuplicates.InsertBefore(ANode, NewNode: TCacheIdxNode);
begin
    NewNode.FDups := Self;
    NewNode.FPrev := ANode.FPrev;
    NewNode.FNext := ANode;
    if ANode.FPrev = nil then
        FFirst := NewNode
    else
        ANode.FPrev.FNext := NewNode;
    ANode.FPrev   := NewNode;
end;


/////////////////////////////////////////////////////////////////////////////
procedure TSecIdxDuplicates.InsertTop(NewNode: TCacheIdxNode);
begin
    NewNode.FDups := Self;
    if FFirst = nil then
    begin
         FFirst := NewNode;
         FLast  := NewNode;
         NewNode.FPrev := nil;
         NewNode.FNext := nil;
    end
    else
         InsertBefore(FFirst, NewNode);
end;


/////////////////////////////////////////////////////////////////////////////
procedure TSecIdxDuplicates.InsertEnd(NewNode: TCacheIdxNode);
begin
    if FLast = nil then
        InsertTop(NewNode)
    else
        InsertAfter(FLast, NewNode);
end;


/////////////////////////////////////////////////////////////////////////////
procedure TSecIdxDuplicates.Remove(ANode: TCacheIdxNode);
begin
    if ANode.FPrev = nil then
        FFirst := ANode.FNext
    else
        ANode.FPrev.FNext := ANode.FNext;

    if ANode.FNext = nil then
        FLast := ANode.FPrev
    else
        ANode.FNext.FPrev := ANode.FPrev;
end;


/////////////////////////////////////////////////////////////////////////////

end.


