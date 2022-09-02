{*********************************************************}
{* FlashFiler: Hash table & calculation routines         *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}
{.$DEFINE CompileDebugCode}
unit ffhash;

interface

uses
  SysUtils,
  ffllbase;

type

  { forward declarations }
  TffBaseHashTable = class;

  TffHashIteratorFunction = procedure(aKey : longInt; aData : pointer;
                                      const cookie1, cookie2, cookie3 : TffWord32) of object;
    { Used by TffHash.Iterate.  Called for each item in the hash
      table. }

  TffHash64IteratorFunction = procedure(aKey : TffInt64; aData : pointer;
                                        const cookie1, cookie2, cookie3 : TffWord32) of object;
    { Used by TffHash64.Iterate.  Called for each item in the hash
      table. }
  { This type defines the kind of procedure called when the data associated
    with a hash table entry must be freed by the owning object. }
  TffDisposeDataProc = procedure(Sender : TffBaseHashTable; AData : Pointer) of object;


  { This class is used to store key/value pairs within the hash table. }
  { Assumption: The TffHashNode.ExtraData property is not used for
    any other purpose. }
  TffHashNode = class(TffObject)
    protected
      fhKey   : Pointer;
      fhNext  : TffHashNode; { The next node in this hash table slot. }
      fhValue : Pointer;
    public
      ExtraData : pointer;
    end;


  { This class is a simple hash table implementation.  It assumes the
    key values will be long integers and the associated data will be a
    pointer.  It assumes the owning object will properly destroy the
    data associated with each hash table entry by assigning a disposal
    function to the OnDispose property of this class.

    This implementation is thread-safe.
  }

  TffBaseHashTable = class(TffObject)
    protected {private}
      FAtMin : boolean;
      FCanShrink : boolean;
      FCount : Integer;
      FHashSizeIndex : integer;
      FMinSizeIndex  : Integer;
      FOnDisposeData : TffDisposeDataProc;
      FTable : TffPointerList;
    protected
      function fhAddPrim(aKey   : Pointer;
                         aValue : Pointer) : Boolean;
        {-Use this method to add an entry to the hash table.  Returns True if
          the key/value pair was added or False if the key is already in the
          hash table. }

      function fhCompareKey(const aKey1 : Pointer;
                            const aKey2 : Pointer) : Boolean; virtual;

      function fhCreateNode: TffHashNode; virtual;

      procedure fhDeletePrim(const AKey : Pointer;
                             const AInx : Integer);
        {-This method is used to delete an entry in the hash table.  aInx
          must specify the exact slot within the table containing the entry.
          This method will then run through the associated entry list and
          locate the exact hash node using aKey. }

      function fhFindPrim(const AKey  : Pointer;
                            var AInx  : Integer;
                            var ANode : TffHashNode) : Boolean;
        {-This method is used to find an entry within the hash table.
          It fills aInx with the index of the key within the hash table and
          aNode with a pointer to the hash node storing the entry. }

      procedure fhFreeKeyPrim(aKey : pointer); virtual; abstract;
        {-Use this method to free a key created for a TffHashNode.
          Called from fhDeletePrim. }

      function fhGetIndex(const AKey   : Pointer;
                          const ACount : Integer) : Integer; virtual; abstract;
        {calculate the index, ie hash, of the key}

      function fhMoveNodePrim(OldTable : TffPointerList;
                              OldNodeInx : integer;
                              Node : TffHashNode): Boolean;
        {-Used by fhResizeTable to move a node from an old table to the new,
          resized table.  Assumption: Resized table has enough room to hold
          the new node. }

      procedure fhResizeTable(const increase : boolean); virtual;
        {-Resize the table. If you want the table to increase to the next
          level of capacity, set increase to True.  If you want the table
          to decrease to the next level of capacity, set increase to False. }
    public
      constructor Create(initialSizeIndex : integer); virtual;
        {-This method creates and initializes the hash table.  initialSizeIndex
          specifies the index of array ffc_HashSizes that is to specify the
          initial number of slots within the hash table. }

      destructor Destroy; override;

      procedure Clear;
        {-Use this method to clear the hash table.  The OnDisposeData event is
          raised for each entry in case the caller needs to free the data
          associated with the entry.}

      property CanShrink : boolean read FCanShrink write FCanShrink;
        {-Use this property to indicate whether or not the hash table may
          be reduced in size when the number of items is less than 1/6 the
          number of slots. }

      property Count : Integer read FCount;
        {-Use this property to determine the number of entries in the hash
          table. }

      property OnDisposeData : TffDisposeDataProc
         read FOnDisposeData write FOnDisposeData;
        {-This event is raised when data associated with an entry must be
          destroyed by the calling object. }
    end;

  TffHash = class(TffBaseHashTable)
    protected
      procedure fhFreeKeyPrim(aKey : pointer); override;

      function fhGetIndex(const AKey   : Pointer;
                          const ACount : Integer) : Integer; override;
        {calculate the index, ie hash, of the key}

    public
      function Add(aKey   : Longint;
                   aValue : Pointer) : Boolean;
        {-Use this method to add an entry to the hash table.  Returns True if
          the key/value pair was added or False if the key is already in the
          hash table. }

      function Get(const AKey : Longint) : Pointer;
        {-Use this method to find an entry in the hash table. }

      procedure Iterate(const CallBack : TffHashIteratorFunction;
                        const cookie1, cookie2, cookie3 : longInt);
        {-Use this method to iterate through the entries in the hash table.
          Callback will be called once for each entry. }

      procedure IterateSafely(const CallBack : TffHashIteratorFunction;
                              const cookie1, cookie2, cookie3 : longInt);
        {-Use this method to iterate through the entries in the hash table.
          It is safe in the sense that it allows the Callback function to
          free the item that is the current subject of the iteration.
          Callback will be called once for each entry. }

      function Remove(const AKey : Longint) : Boolean;                 {!!.02}
        {-Use this method to remove an entry from the hash table.  The
          OnDisposeData event is raised in case the caller needs to free the
          data associated with the entry. }



      {$IFDEF CompileDebugCode}
      procedure DebugPrint(const AFileName : string);
        {-Use this method to dump the contents of the hash table during
          testing stage. }
      {$ENDIF}

  end;

  TffHash64 = class(TffBaseHashTable)
    protected
      function fhCompareKey(const aKey1 : Pointer;
                            const aKey2 : Pointer) : Boolean; override;

      procedure fhFreeKeyPrim(aKey : pointer); override;

      function fhGetIndex(const AKey   : Pointer;
                          const ACount : Integer) : Integer; override;
        {calculate the index, ie hash, of the key}

    public
      function Add(const AKey   : TffInt64;
                   AValue : Pointer) : Boolean;
        {-Use this method to add an entry to the hash table.  Returns True if
          the key/value pair was added or False if the key is already in the
          hash table. }

      function Get(const AKey : TffInt64) : Pointer;
        {-Use this method to find an entry in the hash table. }

      procedure Iterate(const CallBack : TffHash64IteratorFunction;
                        const cookie1, cookie2, cookie3 : longInt);
        {-Use this method to iterate through the entries in the hash table.
          Callback will be called once for each entry. }

      procedure IterateSafely(const CallBack : TffHash64IteratorFunction;
                              const cookie1, cookie2, cookie3 : longInt);
        {-Use this method to iterate through the entries in the hash table.
          It is safe in the sense that it allows the Callback function to
          free the item that is the current subject of the iteration.
          Callback will be called once for each entry. }

      procedure Remove(const AKey : TffInt64);
        {-Use this method to remove an entry from the hash table.  The
          OnDisposeData event is raised in case the caller needs to free the
          data associated with the entry. }

      {$IFDEF CompileDebugCode}
      procedure DebugPrint(const AFileName : string);
        {-Use this method to dump the contents of the hash table during
          testing stage. }
      {$ENDIF}

  end;


  { This class is a threadsafe version of TffHash.  This class allows multiple
    threads to have read access or one thread to have write access (i.e.,
    multiple read, exclusive write).  A thread is granted write access only if
    there are no reading threads or writing threads.}

  TffThreadHash = class(TffHash)
    protected {private}
      FPortal    : TffReadWritePortal;
    public

      constructor Create(initialSizeIndex : Integer); override;

      destructor Destroy; override;

      function BeginRead : TffThreadHash;
        {-A thread must call this method to gain read access to the list.
          Returns the instance of TffThreadList as a convenience. }

      function BeginWrite : TffThreadHash;
        {-A thread must call this method to gain write access to the list.
          Returns the instance of TffThreadList as a convenience.}

      procedure EndRead;
        {-A thread must call this method when it no longer needs read access
          to the list.  If it does not call this method, all writers will
          be perpetually blocked. }

      procedure EndWrite;
        {-A thread must call this method when it no longer needs write access
          to the list.  If it does not call this method, all readers and writers
          will be perpetualy blocked. }
  end;

  TffThreadHash64 = class(TffHash64)
    protected {private}
      FPortal    : TffReadWritePortal;
    public

      constructor Create(initialSizeIndex : Integer); override;

      destructor Destroy; override;

      function BeginRead : TffThreadHash64;
        {-A thread must call this method to gain read access to the list.
          Returns the instance of TffThreadList as a convenience. }

      function BeginWrite : TffThreadHash64;
        {-A thread must call this method to gain write access to the list.
          Returns the instance of TffThreadList as a convenience.}

      procedure EndRead;
        {-A thread must call this method when it no longer needs read access
          to the list.  If it does not call this method, all writers will
          be perpetually blocked. }

      procedure EndWrite;
        {-A thread must call this method when it no longer needs write access
          to the list.  If it does not call this method, all readers and writers
          will be perpetualy blocked. }
  end;


{The following algorithm is the UNIX ELF format hash. The code was
 converted and adapted from the one in C published in Dr Dobbs
 Journal, April 1996, in the article "Hashing Rehashed" by
 Andrew Binstock.}
function FFCalcELFHash(const Buffer; BufSize : Integer) : TffWord32;

function FFCalcShStrELFHash(const S : TffShStr) : TffWord32;

const
  { The following constants represent indexes into ffc_HashSizes array
    declared in the implementation section of this unit.  Use these constants
    to specify the initial size index for hash tables. }
  ffc_Size59 = 0;
  ffc_Size127 = 1;
  ffc_Size257 = 2;
  ffc_Size521 = 3;
  ffc_Size1049 = 4;
  ffc_Size2099 = 5;

implementation

{ The following array contains the legal hash table sizes.  Each is a prime
  number which allows for better spread of inserts within a hash table. }
const
  ffc_HashSizes : array[0..15] of integer =
    (   59,   127,   257,   521,   1049,    2099,    4201,    8419,
     16843, 33703, 67409, 134837, 269683, 539389, 1078787, 2157587);

const
  ffc_HashLoadFactor = 4;
    { When storing integer-ish items in a hash table, the hash table can
      quickly walk through a slot's chain of nodes in those cases where a slot
      contains more than one item.  As a result, we can load up the hash
      table with more items than slots.  This constant specifies how far the
      table may be overloaded.  The table won't be resized until this limit
      is reached.  The limit is defined as Number of Slots * Load Factor. }

{===TffBaseHashTable=================================================}
constructor TffBaseHashTable.Create(initialSizeIndex : integer);
begin
  inherited Create;

  FAtMin := False;
  FCount := 0;
  if initialSizeIndex > high(ffc_HashSizes) then
    initialSizeIndex := high(ffc_HashSizes);
  FHashSizeIndex := initialSizeIndex;
  FMinSizeIndex := FHashSizeIndex;
  FOnDisposeData := nil;
  FTable := TffPointerList.Create;
  FTable.Count := ffc_HashSizes[FHashSizeIndex];
end;
{--------}
function TffBaseHashTable.fhCreateNode: TffHashNode;
begin
  Result := TffHashNode.Create;
end;
{--------}
procedure TffBaseHashTable.Clear;
var
  i    : integer;
  Node : TffHashNode;
  Temp : TffHashNode;
begin
  for i := 0 to pred(FTable.Count) do begin
    Node := TffHashNode(FTable[i]);
    while assigned(Node) do begin
      Temp := Node;
      Node := Node.fhNext;
      if assigned(FOnDisposeData) then
        FOnDisposeData(Self,Temp.fhValue);
      {Temp.fhValue := nil;}
      fhFreeKeyPrim(Temp.fhKey);                                       {!!.01}
      Temp.Free;
    end;
    FTable[i] := nil;
  end;
  FCount := 0;
end;
{--------}
destructor TffBaseHashTable.Destroy;
begin
  Clear;
  FTable.Free;
  inherited Destroy;
end;
{--------}
function TffBaseHashTable.fhAddPrim(aKey   : Pointer;
                                    aValue : Pointer): Boolean;
var
  Inx  : integer;
  Node : TffHashNode;
begin
  if fhFindPrim(aKey, Inx, Node) then
    Result := false
  else begin
    Result := true;
    Node := fhCreateNode;
    Node.fhNext := TffHashNode(FTable[Inx]);
    Node.fhKey := aKey;
    Node.fhValue := aValue;
    FTable.List[Inx] := Node;
    inc(FCount);

    { Expand the table if we've reached our load limit. }
    if (FCount > (FTable.Count * ffc_HashLoadFactor)) then
      fhResizeTable(True);
  end;
end;
{--------}
function TffBaseHashTable.fhCompareKey(const aKey1 : Pointer;
                                       const aKey2 : Pointer) : Boolean;
begin
  Result := aKey1 = aKey2;
end;
{--------}
procedure TffBaseHashTable.fhDeletePrim(const aKey : Pointer;
                                        const aInx : Integer);
var
  Node : TffHashNode;
  NextNode : TffHashNode;
  PrevNode : TffHashNode;
begin
  Node := TffHashNode(FTable.List[aInx]);
  PrevNode := nil;
  while assigned(Node) and (not fhCompareKey(Node.fhKey, AKey)) do begin
    PrevNode := Node;
    Node := Node.fhNext;
  end;
  if assigned(Node) then begin
    if assigned(FOnDisposeData) then
      FOnDisposeData(Self, Node.fhValue);
    NextNode := Node.fhNext;
    {Node.fhValue := nil;}
    fhFreeKeyPrim(Node.fhKey);
    Node.Free;
    if assigned(PrevNode) then
      PrevNode.fhNext := NextNode
    else if assigned(NextNode) then
      FTable.List[aInx] := NextNode
    else
      FTable.List[aInx] := nil;
  end;
  dec(FCount);
end;
{--------}
function TffBaseHashTable.fhFindPrim(const AKey  : Pointer;
                                       var AInx  : Integer;
                                       var ANode : TffHashNode): Boolean;
var
  Node : TffHashNode;
begin
  {assume we won't find aKey}
  Result := false;
  aNode := nil;
  {calculate the index, ie hash, of the key}
  aInx := fhGetIndex(aKey, FTable.Count);
  {traverse the linked list at this entry, looking for the key in each
   node we encounter--a case-sensitive comparison}
  Node := TffHashNode(FTable[aInx]);
  while (Node <> nil) do begin
    if fhCompareKey(AKey, Node.fhKey) then begin
      Result := true;
      aNode := Node;
      Exit;
    end;
    Node := Node.fhNext;
  end;
end;
{--------}
function TffBaseHashTable.fhMoveNodePrim(OldTable : TffPointerList;
                                         OldNodeInx : integer;
                                         Node : TffHashNode): Boolean;
var
  Inx  : integer;
  NextNode : TffHashNode;
  PrevNode : TffHashNode;
  TmpNode : TffHashNode;
begin
  { Assumption: The node will not be found in the table because we are only
    being called during a resize. }

  { Assumption: Table does not need to be expanded since this method is
    called during table expansion. }

  { Remove the node from the old table. }
  TmpNode := TffHashNode(OldTable[OldNodeInx]);
  PrevNode := nil;
  while assigned(TmpNode) and
       (not fhCompareKey(TmpNode.fhKey, Node.fhKey)) do begin
    PrevNode := TmpNode;
    TmpNode := TmpNode.fhNext;
  end;
  if assigned(TmpNode) then begin
    NextNode := TmpNode.fhNext;
    if assigned(PrevNode) then
      PrevNode.fhNext := NextNode
    else if assigned(NextNode) then
      OldTable.List[OldNodeInx] := NextNode
    else
      OldTable.List[OldNodeInx] := nil;
  end;

  { Calculate the index, ie hash, of the key. }
  Inx := fhGetIndex(Node.fhKey, FTable.Count);

  { Insert the node into the new table. }
  Result := true;
  Node.fhNext := TffHashNode(FTable[Inx]);
  FTable.List[Inx] := Node;

end;
{--------}
procedure TffBaseHashTable.fhResizeTable(const increase : boolean);
var
  OldTable : TffPointerList;
  Count    : Integer;
  Node     : TffHashNode;
  NewSize  : Integer;
begin
  FAtMin := False;
  { Are we increasing or decreasing? }
  if increase then begin
    { Increasing. Have we reached the limits of the ffc_HashSizes array? }
    if FHashSizeIndex = high(ffc_HashSizes) then begin
      { Yes.  Double the current size and add one.  If divisible by 3 then
        add 2. }
      NewSize := (FTable.Count * 2) + 1;
      if NewSize mod 3 = 0 then
        inc(NewSize, 2);
    end
    else begin
      { No.  Move to the next size. }
      inc(FHashSizeIndex);
      NewSize := ffc_HashSizes[FHashSizeIndex];
    end;
  end
  else begin
    { Decreasing.  Have we reached our lower limit? }
    FAtMin := (FHashSizeIndex = FMinSizeIndex);
    if FAtMin then
      exit
    else begin
      dec(FHashSizeIndex);
      NewSize := ffc_HashSizes[FHashSizeIndex];
    end;
  end;

  { Expand the table. }
  OldTable := FTable;

  FTable := TffPointerList.Create;
  FTable.Count := NewSize;

  for Count := 0 to Pred(OldTable.Count) do begin
    Node := TffHashNode(OldTable.List[Count]);
    repeat
      if Assigned(Node) then
        fhMoveNodePrim(OldTable, Count, Node);
      Node := TffHashNode(OldTable.List[Count]);
    until (not assigned(Node));
  end;

  OldTable.Free;
end;
{====================================================================}


{===TffHash==========================================================}
function TffHash.Add(aKey   : LongInt;
                     aValue : Pointer): Boolean;
begin
  Result := fhAddPrim(pointer(aKey), aValue);
end;
{--------}
{$IFDEF CompileDebugCode}

procedure TffHash.DebugPrint(const AFileName: string);
var
  F    : text;
  i    : integer;
  Node : TffHashNode;
begin
  System.Assign(F, aFileName);
  System.Rewrite(F);

  for i := 0 to pred(FTable.Count) do begin
    writeln(F, '---', i, '---');
    Node := TffHashNode(FTable[i]);
    while assigned(Node) do begin
      writeln(F, Longint(Node.fhKey):10, intToStr(longInt(Node.fhValue)):20);
      Node := Node.fhNext;
    end;
  end;

  writeln(F);
  writeln(F, 'Count: ', Count, ' (mean: ', Count/FTable.Count:5:3, ')');

  System.Close(F);
end;
{$ENDIF}
{--------}
procedure TffHash.fhFreeKeyPrim(aKey : pointer);
begin
  { Do nothing. }
end;
{--------}
function TffHash.fhGetIndex(const AKey   : Pointer;
                            const ACount : Integer): Integer;
begin
  Result := Longint(AKey) mod ACount;
end;
{--------}
function TffHash.Get(const AKey: Integer): Pointer;
var
  Inx  : integer;
  Node : TffHashNode;
begin
  Result := nil;
  if fhFindPrim(Pointer(aKey), Inx, Node) then
    Result := Node.fhValue
end;
{--------}
procedure TffHash.Iterate(const CallBack : TffHashIteratorFunction;
                          const cookie1, cookie2, cookie3 : longInt);
var
  Count    : Integer;
  Node     : TffHashNode;
begin
  for Count := 0 to Pred(FTable.Count) do begin
    Node := TffHashNode(FTable[Count]);
    while assigned(Node) do begin
      CallBack(longInt(Node.fhKey), Node.fhValue, cookie1, cookie2, cookie3);
      Node := Node.fhNext;
    end;
  end;
end;
{--------}
procedure TffHash.IterateSafely(const CallBack : TffHashIteratorFunction;
                                const cookie1, cookie2, cookie3 : longInt);
var
  Count    : Integer;
  FirstNode : TffHashNode;
  NextNode : TffHashNode;
  Node : TffHashNode;
  PrevNode : TffHashNode;
begin
  { Assumption: The TffHashNode.ExtraData property is not used for
    any other purpose. }
  { String the nodes together. }
  FirstNode := nil;
  PrevNode := nil;
  for Count := 0 to Pred(FTable.Count) do begin
    Node := TffHashNode(FTable[Count]);
    while assigned(Node) do begin

      if FirstNode = nil then
        FirstNode := Node;

      if Assigned(PrevNode) then
        PrevNode.ExtraData := Node;

      PrevNode := Node;
      Node := Node.fhNext;
    end;
  end;

  { Iterate through the list of nodes. }
  Node := FirstNode;
  while assigned(Node) do begin
    NextNode := Node.ExtraData;
    Callback(longInt(Node.fhKey), Node.fhValue, cookie1, cookie2, cookie3);
    Node := NextNode;
  end;

end;
{--------}
function TffHash.Remove(const AKey: Longint) : Boolean;                {!!.02}
var
  Inx : integer;
  Node : TffHashNode;
begin
  if fhFindPrim(Pointer(aKey), Inx, Node) then begin
    fhDeletePrim(Pointer(aKey), Inx);

    { Shrink the table if:
      1. Shrinking is allowed.
      2. We are not at the minimum size already.
      3. We have some elements.
      4. We have some elements and we're under 1/6 full
    }
    if FCanShrink and (not FAtMin) and
      (FCount > 10) and ((FCount * 6) < FTable.Count) then
      fhResizeTable(False);
    Result := True;                                                    {!!.02}
  end                                                                  {!!.02}
  else                                                                 {!!.02}
    Result := False;                                                   {!!.02}
end;
{====================================================================}


{===TffHash64========================================================}
function TffHash64.Add(const aKey   : TffInt64;
                             aValue : Pointer): Boolean;
var
  keyPtr : pointer;
begin
  FFGetMem(keyPtr, sizeOf(TffInt64));
  TffInt64(keyPtr^) := aKey;
  Result := fhAddPrim(keyPtr, aValue);
  if not Result then
    FFFreeMem(keyPtr, SizeOf(TffInt64));
end;
{--------}
{$IFDEF CompileDebugCode}
procedure TffHash64.DebugPrint(const AFileName: string);
var
  F    : text;
  i    : integer;
  Node : TffHashNode;
begin
  System.Assign(F, aFileName);
  System.Rewrite(F);

  for i := 0 to pred(FTable.Count) do begin
    writeln(F, '---', i, '---');
    Node := TffHashNode(FTable[i]);
    while assigned(Node) do begin
      writeln(F, FFI64ToStr(PffInt64(Node.fhKey)^), intToStr(longInt(Node.fhValue)):20);
      Node := Node.fhNext;
    end;
  end;

  writeln(F);
  writeln(F, 'Count: ', Count, ' (mean: ', Count/FTable.Count:5:3, ')');

  System.Close(F);
end;
{$ENDIF}
{--------}
function TffHash64.fhCompareKey(const aKey1 : Pointer;
                                const aKey2 : Pointer) : Boolean;
begin
   Result := FFCmpI64(PffInt64(aKey1)^, PffInt64(aKey2)^) = 0;
end;
{--------}
procedure TffHash64.fhFreeKeyPrim(aKey : pointer);
begin
  FFFreeMem(aKey, sizeOf(TffInt64));
end;
{--------}
function TffHash64.fhGetIndex(const AKey   : Pointer;
                              const ACount : Integer): Integer;
var
  Int : Integer;
begin
  Int := ffI64ModInt(PffInt64(AKey)^, ACount);
  Result := Int;
end;
{--------}
function TffHash64.Get(const AKey : TffInt64) : Pointer;
var
  Inx  : integer;
  Node : TffHashNode;
begin
  Result := nil;
  if fhFindPrim(@aKey, Inx, Node) then
    Result := Node.fhValue
end;
{--------}
procedure TffHash64.Iterate(const CallBack : TffHash64IteratorFunction;
                            const cookie1, cookie2, cookie3 : longInt);
var
  Count    : Integer;
  Node     : TffHashNode;
begin
  for Count := 0 to Pred(FTable.Count) do begin
    Node := TffHashNode(FTable[Count]);
    while assigned(Node) do begin
      CallBack(TffInt64(Node.fhKey^), Node.fhValue, cookie1, cookie2, cookie3);
      Node := Node.fhNext;
    end;
  end;
end;
{--------}
procedure Tffhash64.IterateSafely(const CallBack : TffHash64IteratorFunction;
                                 const cookie1, cookie2, cookie3 : longInt);
var
  Count    : Integer;
  FirstNode : TffHashNode;
  NextNode : TffHashNode;
  Node : TffHashNode;
  PrevNode : TffHashNode;
begin
  { Assumption: The TffHashNode.ExtraData property is not used for
    any other purpose. }
  { String the nodes together. }
  FirstNode := nil;
  PrevNode := nil;
  for Count := 0 to Pred(FTable.Count) do begin
    Node := TffHashNode(FTable[Count]);
    while assigned(Node) do begin

      if FirstNode = nil then
        FirstNode := Node;

      if Assigned(PrevNode) then
        PrevNode.ExtraData := Node;

      PrevNode := Node;
      Node := Node.fhNext;
    end;
  end;

  { Iterate through the list of nodes. }
  Node := FirstNode;
  while assigned(Node) do begin
    NextNode := Node.ExtraData;
    Callback(TffInt64(Node.fhKey^), Node.fhValue, cookie1, cookie2, cookie3);
    Node := NextNode;
  end;

end;
{--------}
procedure TffHash64.Remove(const AKey : TffInt64);
var
  Inx : integer;
  Node : TffHashNode;
begin
  if fhFindPrim(@aKey, Inx, Node) then begin
    fhDeletePrim(@aKey, Inx);

    { Shrink the table if:
      1. Shrinking is allowed.
      2. We are not at the minimum size already.
      3. We have some elements.
      4. We have some elements and we're under 1/6 full
    }
    if FCanShrink and (not FAtMin) and
      (FCount > 10) and ((FCount * 6) < FTable.Count) then
      fhResizeTable(False);
  end;
end;
{====================================================================}


{===TffThreadHash====================================================}
function TffThreadHash.BeginRead : TffThreadHash;
begin
  if IsMultiThread then
    FPortal.BeginRead;
  Result := Self
end;
{--------}
function TffThreadHash.BeginWrite : TffThreadHash;
begin
  if IsMultiThread then
    FPortal.BeginWrite;
  Result := Self
end;
{--------}
constructor TffThreadHash.Create(initialSizeIndex : Integer);
begin
  inherited Create(initialSizeIndex);
  FPortal := TffReadWritePortal.Create;
end;
{--------}
destructor TffThreadHash.Destroy;
begin
  if Assigned(FPortal) then
    FPortal.Free;
  inherited Destroy;
end;
{--------}
procedure TffThreadHash.EndRead;
begin
  if IsMultiThread then
    FPortal.EndRead;
end;
{--------}
procedure TffThreadHash.EndWrite;
begin
  if IsMultiThread then
    FPortal.EndWrite;
end;
{====================================================================}


{===TffThreadHash64==================================================}
function TffThreadHash64.BeginRead : TffThreadHash64;
begin
  FPortal.BeginRead;
  Result := Self
end;
{--------}
function TffThreadHash64.BeginWrite : TffThreadHash64;
begin
  FPortal.BeginWrite;
  Result := Self
end;
{--------}
constructor TffThreadHash64.Create(initialSizeIndex : Integer);
begin
  inherited Create(initialSizeIndex);
  FPortal := TffReadWritePortal.Create;
end;
{--------}
destructor TffThreadHash64.Destroy;
begin
  if Assigned(FPortal) then
    FPortal.Free;
  inherited Destroy;
end;
{--------}
procedure TffThreadHash64.EndRead;
begin
  FPortal.EndRead;
end;
{--------}
procedure TffThreadHash64.EndWrite;
begin
  FPortal.EndWrite;
end;

{====================================================================}

(****
Note: the original C routine looked like this:

unsigned long ElfHash ( const unsigned char *name )
{
  unsigned long h = 0, g;
  while ( *name )
  {
    h = ( h << 4 ) + *name++;
    if ( g = h & 0xF0000000 )
      h ^= g >> 24;
    h &= ~g;
  }
  return h;
}
****)

{$Q-}                                                                 {!!.05}
function FFCalcELFHash(const Buffer; BufSize : integer) : TffWord32;
var
  BufAsBytes : TffByteArray absolute Buffer;
  G : TffWord32;
  i : integer;
begin
  Result := 0;
  for i := 0 to pred(BufSize) do begin
    Result := (Result shl 4) + BufAsBytes[i];
    G := Result and $F0000000;
    if (G <> 0) then
      Result := Result xor (G shr 24);
    Result := Result and (not G);
  end;
end;
{$Q+}                                                                 {!!.05}
{--------}
function FFCalcShStrELFHash(const S : TffShStr) : TffWord32;
begin
  Result := FFCalcELFHash(S[1], length(S));
end;

end.

