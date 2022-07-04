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
 * The Original Code is TurboPower XMLPartner
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
{*********************************************************}
{* XMLPartner Pro: XpHash.PAS                            *}
{*********************************************************}
{* XMLPartner Pro: Hash table & calculation routines     *}
{*********************************************************}

{$I XpDefine.inc}
unit XpHash;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF LINUX}
  Libc,
  Types,
{$ENDIF}
  Classes,
  SysUtils,
  XpBase;

type

  PXpByteArray = ^TXpByteArray;             {General array of bytes}
  TXpByteArray = array[0..65531] of byte;

  { The following types are used by TXpPointerList to store a list of Pointers. }
  PXpPointerArray = ^TXpPointerArray;
  TXpPointerArray =
     array [0..pred(MaxInt div sizeof(Pointer))] of Pointer;

  { This is an unsorted list type dealing only with Pointers.  Note that it is
    the responsibility of the application to free the memory referenced by the
    Pointer. }
  TXpPointerList = class(TPersistent)
    protected {private}
      FCapacity  : Longint;
      FCount     : Longint;
      FList      : PXpPointerArray;
    protected

      function AppendPrim(aPtr : Pointer) : Longint;
      procedure Grow;
      function GetCapacity : Longint;
      function GetCount : Longint;
      function GetPointer(aInx : Longint) : Pointer;
      function GetInternalAddress : Pointer;
      procedure SetCapacity(const C : Longint);
      procedure SetCount(const C : Longint);
      procedure SetPointer(aInx : Longint; aPtr : Pointer);

      procedure RemoveAtPrim(aInx : Longint);
        {-Removes an item from the list but does not free the item. }

    public
      constructor Create;
        {-create the list}
      destructor Destroy; override;
        {-destroy the list}
      procedure Assign(Source : TPersistent); override;
        {-assign another list's data to this one}
      function  Append(aPtr : Pointer) : boolean;
        {-append an item to the list; return true on success}
      procedure Empty;
        {-Empty the list of Pointers.  Note that the application is
          responsible for freeing the memory referenced by the Pointers. }

      procedure Insert(aPtr : Pointer; const L : Longint);
        {-Insert a pointer before the specified item. }

      function  IsEmpty : boolean;
        {-return true if the list is empty}

      procedure RemoveAt(aInx : Longint);
        {-Use this method to remove the Pointer at the specified position. }

      property Capacity : Longint
        {-the total capacity of the list}
         read GetCapacity write SetCapacity;

      property Count : Longint
        {-the number of items in the list}
         read GetCount write SetCount;

      property InternalAddress : Pointer read GetInternalAddress;
        {-Returns a Pointer to the internal list of Pointers.  Be careful with
          this.  It is to be used only when necessary. }

      property List : PXpPointerArray read FList;
        {-Provides direct access to the internal list of Pointers.  Use this
          only if you know what you are doing. }

      property Pointers[aInx : Longint] : Pointer
        {-the list of items}
         read GetPointer write SetPointer; default;
  end;

  { forward declarations }
  TXpBaseHash = class;
  TXpStrHash = class;

  TXpIntHashIteratorFunction = procedure(aKey : longInt; aData : Pointer;
                                      const cookie1, cookie2, cookie3 : DWORD) of object;
    { Used by TXpIntHash.Iterate.  Called for each item in the hash
      table. }

  { This type defines the kind of procedure called when the data associated
    with a hash table entry must be freed by the owning object. }
  TXpDisposeDataProc = procedure(Sender : TXpBaseHash; aData : Pointer) of object;
  TXpDisposeDataProcEx = procedure(Sender : TXpBaseHash; aData : Pointer);
  TXpStrDisposeDataProc = procedure(Sender : TXpStrHash; aData : Pointer) of object;


  { This class is used to store key/value pairs within an Integer hash table. }
  TXpIntHashNode = class(TObject)
    protected
      FKey   : Pointer;
      FNext  : TXpIntHashNode; { The next node in this hash table slot. }
      FValue : Pointer;
    public
      ExtraData : Pointer;
        { Extra Pointer available for application use. }
    end;

  { This class is used to store key/value pairs within a string hash table. }
  TXpStrHashNode = class(TObject)
    protected
      FKey   : DOMString;
      FNext  : TXpStrHashNode; { The next node in this hash table slot. }
      FValue : Pointer;
    public
      ExtraData : Pointer;
        { Extra Pointer available for application use. }
    end;

  { This class is a simple hash table implementation.  It assumes the
    key values will be long Integers and the associated data will be a
    Pointer.  It assumes the owning object will properly destroy the
    data associated with each hash table entry by assigning a disposal
    function to the OnDispose property of this class. }
  TXpBaseHash = class(TObject)
    protected {private}
      FAtMin : boolean;
      FCanShrink : boolean;
      FCount : Integer;
      FHashSizeIndex : Integer;
      FMinSizeIndex  : Integer;
      FOnDisposeData : TXpDisposeDataProc;
      FOnDisposeDataEx : TXpDisposeDataProcEx;
      FTable : TXpPointerList;
    protected
      function XhAddPrim(aKey   : Pointer;
                         aValue : Pointer) : Boolean;
        {-Use this method to add an entry to the hash table.  Returns True if
          the key/value pair was added or False if the key is already in the
          hash table. }

      function XhCompareKey(const aKey1 : Pointer;
                            const aKey2 : Pointer) : Boolean; virtual;

      function XhCreateNode: TXpIntHashNode; virtual;

      procedure XhDeletePrim(const aKey : Pointer;
                             const aInx : Longint);
        {-This method is used to delete an entry in the hash table.  aInx
          must specify the exact slot within the table containing the entry.
          This method will then run through the associated entry list and
          locate the exact hash node using aKey. }

      function XhFindPrim(const aKey  : Pointer;
                            var aInx  : Longint;
                            var aNode : TXpIntHashNode) : Boolean;
        {-This method is used to find an entry within the hash table.
          It fills aInx with the index of the key within the hash table and
          aNode with a Pointer to the hash node storing the entry. }

      procedure XhFreeKeyPrim(aKey : Pointer); virtual;
        {-Use this method to free a key created for a TXpIntHashNode.
          Called from XhDeletePrim. }

      function XhGetIndex(const aKey   : Pointer;
                          const aCount : Longint) : Longint; virtual;
        {calculate the index, ie hash, of the key}

      function XhMoveNodePrim(OldTable : TXpPointerList;
                              OldNodeInx : Longint;
                              Node : TXpIntHashNode): Boolean;
        {-Used by XhResizeTable to move a node from an old table to the new,
          resized table.  Assumption: Resized table has enough room to hold
          the new node. }

      procedure XhResizeTable(const increase : boolean); virtual;
        {-Resize the table. If you want the table to increase to the next
          level of capacity, set increase to True.  If you want the table
          to decrease to the next level of capacity, set increase to False. }
    public
      constructor Create(initialSizeIndex : Integer); virtual;
        {-This method creates and initializes the hash table.  initialSizeIndex
          specifies the index of array xpc_HashSizes that is to specify the
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

      property OnDisposeData : TXpDisposeDataProc
         read FOnDisposeData write FOnDisposeData;
        {-This event is raised when data associated with an entry must be
          destroyed by the calling object. }

      property OnDisposeDataEx : TXpDisposeDataProcEx
         read FOnDisposeDataEx write FOnDisposeDataEx;
        {-This event is raised when data associated with an entry must be
          destroyed by the calling object. This event differs from
          FOnDisposeData because this one is not a class object and the
          handler can be a free function.}
    end;

  TXpIntHash = class(TXpBaseHash)
    protected
      function XhGetIndex(const aKey   : Pointer;
                          const aCount : Longint) : Longint; override;
        {calculate the index, ie hash, of the key}

    public
      function Add(aKey   : Longint;
                   aValue : Pointer) : Boolean;
        {-Use this method to add an entry to the hash table.  Returns True if
          the key/value pair was added or False if the key is already in the
          hash table. }

      function Get(const aKey : Longint) : Pointer;
        {-Use this method to find an entry in the hash table. }

      procedure Iterate(const CallBack : TXpIntHashIteratorFunction;
                        const cookie1, cookie2, cookie3 : longInt);
        {-Use this method to iterate through the entries in the hash table.
          Callback will be called once for each entry. }

      procedure IterateSafely(const CallBack : TXpIntHashIteratorFunction;
                              const cookie1, cookie2, cookie3 : longInt);
        {-Use this method to iterate through the entries in the hash table.
          It is safe in the sense that it allows the Callback function to
          free the item that is the current subject of the iteration.
          Callback will be called once for each entry. }

      function Remove(const aKey : Longint) : Boolean;
        {-Use this method to remove an entry from the hash table.  The
          OnDisposeData event is raised in case the caller needs to free the
          data associated with the entry. }

      {$IFDEF CompileDebugCode}
      procedure DebugPrint(const aFileName : string);
        {-Use this method to dump the contents of the hash table during
          testing stage. }
      {$ENDIF}

  end;

  TXpStrHash = class(TPersistent)
    protected
      FAtMin : boolean;
      FCanShrink : boolean;
      FCount : Integer;
      FHashSizeIndex : Integer;
      FMinSizeIndex  : Integer;
      FOnDisposeData : TXpStrDisposeDataProc;
      FTable : TXpPointerList;

      function XhAddPrim(const aKey   : DOMString;                     {!!.57}
                         aValue : Pointer) : Boolean;
        {-Use this method to add an entry to the hash table.  Returns True if
          the key/value pair was added or False if the key is already in the
          hash table. }

      function XhCompareKey(const aKey1 : DOMString;
                            const aKey2 : DOMString) : Boolean; virtual;

      function XhCreateNode: TXpStrHashNode; virtual;

      procedure XhDeletePrim(const aKey : DOMString;
                             const aInx : Longint);
        {-This method is used to delete an entry in the hash table.  aInx
          must specify the exact slot within the table containing the entry.
          This method will then run through the associated entry list and
          locate the exact hash node using aKey. }

      function XhFindPrim(const aKey  : DOMString;
                            var aInx  : Longint;
                            var aNode : TXpStrHashNode) : Boolean;
        {-This method is used to find an entry within the hash table.
          It fills aInx with the index of the key within the hash table and
          aNode with a Pointer to the hash node storing the entry. }

      function XhGetIndex(const aKey   : DOMString;
                          const aCount : Longint) : Longint; virtual;
        {calculate the index, ie hash, of the key}

      function XhMoveNodePrim(OldTable : TXpPointerList;
                              OldNodeInx : Longint;
                              Node : TXpStrHashNode): Boolean;
        {-Used by XhResizeTable to move a node from an old table to the new,
          resized table.  Assumption: Resized table has enough room to hold
          the new node. }

      procedure XhResizeTable(const increase : boolean); virtual;
        {-Resize the table. If you want the table to increase to the next
          level of capacity, set increase to True.  If you want the table
          to decrease to the next level of capacity, set increase to False. }

    public
      constructor Create(initialSizeIndex : Integer); virtual;
        {-This method creates and initializes the hash table.  initialSizeIndex
          specifies the index of array xpc_HashSizes that is to specify the
          initial number of slots within the hash table. }

      destructor Destroy; override;

      function Add(const aKey   : DOMString;                           {!!.57}
                   aValue : Pointer) : Boolean;
        {-Use this method to add an entry to the hash table.  Returns True if
          the key/value pair was added or False if the key is already in the
          hash table. }

      procedure Clear;
        {-Use this method to clear the hash table.  The OnDisposeData event is
          raised for each entry in case the caller needs to free the data
          associated with the entry.}

      function Exists(const aKey : DOMString) : Boolean;
        {-Returns True if there is an entry in the hash table for the specified
          key. }

      function ExistsWithHash(const aKey : DOMString;
                              const aHash : DWORD): Boolean;
        {-Identical in functionality to Exists. Use this method when you
          must do a lot of hash table lookups using the same key value.
          Use function XpCalcElfHash to generate a value for the aHash
          parameter. The original key value must still be supplied via the
          aKey parameter since string comparison must still be performed for
          slots having a linked list of nodes (i.e., collision). }

      function Get(const aKey : DOMString) : Pointer;
        {-Use this method to find an entry in the hash table. }

      function Remove(const aKey : DOMString) : Boolean;
        {-Use this method to remove an entry from the hash table.  The
          OnDisposeData event is raised in case the caller needs to free the
          data associated with the entry. }

      {$IFDEF CompileDebugCode}
      procedure DebugPrint(const aFileName : string);
        {-Use this method to dump the contents of the hash table during
          testing stage. }
      {$ENDIF}

      property CanShrink : boolean read FCanShrink write FCanShrink;
        {-Use this property to indicate whether or not the hash table may
          be reduced in size when the number of items is less than 1/6 the
          number of slots. }

      property Count : Integer read FCount;
        {-Use this property to determine the number of entries in the hash
          table. }

      property OnDisposeData : TXpStrDisposeDataProc
         read FOnDisposeData write FOnDisposeData;
        {-This event is raised when data associated with an entry must be
          destroyed by the calling object. }
  end;


{The following algorithm is the UNIX ELF format hash. The code was
 converted and adapted from the one in C published in Dr Dobbs
 Journal, April 1996, in the article "Hashing Rehashed" by
 Andrew Binstock.}
function XpCalcElfHash(const Buffer; BufSize : Integer) : DWORD;

const
  { The following constants represent indexes into xpc_HashSizes array
    declared in the implementation section of this unit.  Use these constants
    to specify the initial size index for hash tables. }
  xpc_Size59 = 0;
  xpc_Size127 = 1;
  xpc_Size257 = 2;
  xpc_Size521 = 3;
  xpc_Size1049 = 4;
  xpc_Size2099 = 5;

  xpcInitialListSize = 64;
    { The initial size of TXpPointerList. }

implementation

{ The following array contains the legal hash table sizes.  Each is a prime
  number which allows for better spread of inserts within a hash table. }
const
  xpc_HashSizes : array[0..15] of Integer =
    (   59,   127,   257,   521,   1049,    2099,    4201,    8419,
     16843, 33703, 67409, 134837, 269683, 539389, 1078787, 2157587);

const
  xpcHashLoadFactor : Integer = 4;
    { When storing Integer-ish items in a hash table, the hash table can
      quickly walk through a slot's chain of nodes in those cases where a slot
      contains more than one item.  As a result, we can load up the hash
      table with more items than slots.  This constant specifies how far the
      table may be overloaded.  The table won't be resized until this limit
      is reached.  The limit is defined as Number of Slots * Load Factor. }

{===TXpPointerList===================================================}
constructor TXpPointerList.Create;
begin
  inherited Create;
  { Allocate space for the initial number of items. }
  GetMem(FList, xpcInitialListSize * sizeOf(Pointer));
  FillChar(FList^, xpcInitialListSize * sizeOf(Pointer), 0);
  FCapacity := xpcInitialListSize;
  FCount := 0;
end;
{--------}
destructor TXpPointerList.Destroy;
begin
  FreeMem(FList, FCapacity * sizeOf(Pointer));
  inherited Destroy;
end;
{--------}
function TXpPointerList.Append(aPtr : Pointer) : boolean;
begin
  Result := AppendPrim(aPtr) <> -1;
end;
{--------}
function TXpPointerList.AppendPrim(aPtr : Pointer) : Longint;
var
  L : Longint;
begin
  { Determine the insertion point. }
  L := FCount;
  if L >= 0 then begin
    { If we are at the limit then increase capacity. }
    if FCount = FCapacity then
      Grow;

    { If we are before the last element in the list, shift everything up. }
    if L < FCount then
      Move(FList^[L], FList^[L + 1], (FCount - L) * sizeOf(Pointer));

    FList^[L] := aPtr;
    inc(FCount);
  end;
  Result := L;
end;
{--------}
procedure TXpPointerList.Assign(Source : TPersistent);
var
  SrcList : TXpPointerList;
  i       : Longint;
begin
  if (Source is TXpPointerList) then begin
    Empty;
    SrcList := TXpPointerList(Source);
    for i := 0 to pred(SrcList.Count) do
      Append(SrcList.Pointers[i]);
  end
  else
    inherited Assign(Source);
end;
{--------}
procedure TXpPointerList.Empty;
begin
  { Did the array contain anything? }
  if FCount > 0 then
    { Yes. Zero it out. }
    FillChar(FList^, FCapacity * sizeOf(Pointer), 0);
  FCount := 0;
end;
{--------}
procedure TXpPointerList.Grow;
begin
  SetCapacity(FCapacity + xpcInitialListSize);
end;
{--------}
function TXpPointerList.GetCapacity : Longint;
begin
  Result := FCapacity;
end;
{--------}
function TXpPointerList.GetCount : Longint;
begin
  Result := FCount;
end;
{--------}
function TXpPointerList.GetPointer(aInx : Longint) : Pointer;
begin
  if (0 <= aInx) and (aInx < FCount) then
    Result := FList^[aInx]
  else
    Result := nil;
end;
{--------}
function TXpPointerList.GetInternalAddress : Pointer;
begin
  Result := Pointer(FList);
end;
{--------}
function  TXpPointerList.IsEmpty : boolean;
begin
  Result := Count = 0;
end;
{--------}
procedure TXpPointerList.Insert(aPtr : Pointer; const L : Longint);
begin
  if L >= 0 then begin
    { If we are at the limit then increase capacity. }
    if FCount = FCapacity then
      Grow;

    { If we are before the last element in the list, shift everything up. }
    if L < FCount then
      Move(FList^[L], FList^[L + 1], (FCount - L) * sizeOf(Pointer));

    FList^[L] := aPtr;
    inc(FCount);
  end;
end;
{--------}
procedure TXpPointerList.RemoveAt(aInx : Longint);
begin
  RemoveAtPrim(aInx);
end;
{--------}
procedure TXpPointerList.RemoveAtPrim(aInx : Longint);
begin
  if (0 <= aInx) and
     (aInx < FCount) then begin
    dec(FCount);
    if aInx < FCount then
     Move(FList^[aInx + 1], FList^[aInx],
         (FCount - aInx) * SizeOf(Pointer));
  end;
end;
{--------}
procedure TXpPointerList.SetCapacity(const C : Longint);
var
  NewList : PXpPointerArray;
begin
  if (C >= FCount) and (C <> FCapacity) then begin
    { Get a new block. }
    GetMem(NewList, C * sizeOf(Pointer));
    FillChar(NewList^, C * sizeOf(Pointer), 0);

    { Transfer the existing data. }
    Move(FList^, NewList^, FCount * SizeOf(Pointer));

    { Free the existing data. }
    FreeMem(FList, FCapacity * SizeOf(Pointer));
    FList := NewList;
    FCapacity := C;
  end;
end;
{--------}
procedure TXpPointerList.SetCount(const C : Longint);
begin
  { Do we need to grow the table? }
  if C > FCapacity then
    SetCapacity(C);
  FCount := C;
end;
{--------}
procedure TXpPointerList.SetPointer(aInx : Longint; aPtr : Pointer);
begin
  { Is the index within range? }
  if (0 <= aInx) and (aInx < FCount) then
    FList^[aInx] := aPtr;
end;
{====================================================================}

{===TXpBaseHash.=================================================}
constructor TXpBaseHash.Create(initialSizeIndex : Integer);
begin
  inherited Create;

  FAtMin := False;
  FCount := 0;
  if initialSizeIndex > high(xpc_HashSizes) then
    initialSizeIndex := high(xpc_HashSizes);
  FHashSizeIndex := initialSizeIndex;
  FMinSizeIndex := FHashSizeIndex;
  FOnDisposeData := nil;
  FOnDisposeDataEx := nil;
  FTable := TXpPointerList.Create;
  FTable.Count := xpc_HashSizes[FHashSizeIndex];
end;
{--------}
function TXpBaseHash.XhCreateNode: TXpIntHashNode;
begin
  Result := TXpIntHashNode.Create;
end;
{--------}
procedure TXpBaseHash.Clear;
var
  i    : Integer;
  Node : TXpIntHashNode;
  Temp : TXpIntHashNode;
begin
  for i := 0 to pred(FTable.Count) do begin
    Node := TXpIntHashNode(FTable[i]);
    while assigned(Node) do begin
      Temp := Node;
      Node := Node.FNext;
      if assigned(FOnDisposeData) then
        FOnDisposeData(Self, Temp.FValue)
      else if (Assigned(FOnDisposeDataEx)) then
        FOnDisposeDataEx(Self, Temp.FValue);
      {Temp.FValue := nil;}
      XhFreeKeyPrim(Temp.FKey);
      Temp.Free;
    end;
    FTable[i] := nil;
  end;
  FCount := 0;
end;
{--------}
destructor TXpBaseHash.Destroy;
begin
  Clear;
  FTable.Free;
  inherited Destroy;
end;
{--------}
function TXpBaseHash.XhAddPrim(aKey   : Pointer;
                               aValue : Pointer): Boolean;
var
  Inx  : Longint;
  Node : TXpIntHashNode;
begin
  if XhFindPrim(aKey, Inx, Node) then
    Result := false
  else begin
    Result := true;
    Node := XhCreateNode;
    Node.FNext := TXpIntHashNode(FTable[Inx]);
    Node.FKey := aKey;
    Node.FValue := aValue;
    FTable.List[Inx] := Node;
    inc(FCount);

    { Expand the table if we've reached our load limit. }
    if (FCount > (FTable.Count * xpcHashLoadFactor)) then
      XhResizeTable(True);
  end;
end;
{--------}
function TXpBaseHash.XhCompareKey(const aKey1 : Pointer;
                                  const aKey2 : Pointer) : Boolean;
begin
  Result := (aKey1 = aKey2);
end;
{--------}
procedure TXpBaseHash.XhDeletePrim(const aKey : Pointer;
                                   const aInx : Longint);
var
  Node : TXpIntHashNode;
  NextNode : TXpIntHashNode;
  PrevNode : TXpIntHashNode;
begin
  Node := TXpIntHashNode(FTable.List[aInx]);
  PrevNode := nil;
  while assigned(Node) and (not XhCompareKey(Node.FKey, aKey)) do begin
    PrevNode := Node;
    Node := Node.FNext;
  end;
  if assigned(Node) then begin
    if assigned(FOnDisposeData) then
      FOnDisposeData(Self, Node.FValue);
    NextNode := Node.FNext;
    {Node.FValue := nil;}
    XhFreeKeyPrim(Node.FKey);
    Node.Free;
    if assigned(PrevNode) then
      PrevNode.FNext := NextNode
    else if assigned(NextNode) then
      FTable.List[aInx] := NextNode
    else
      FTable.List[aInx] := nil;
  end;
  dec(FCount);
end;
{--------}
function TXpBaseHash.XhFindPrim(const aKey  : Pointer;
                                  var aInx  : Longint;
                                  var aNode : TXpIntHashNode): Boolean;
var
  Node : TXpIntHashNode;
begin
  {assume we won't find aKey}
  Result := false;
  aNode := nil;
  {calculate the index, ie hash, of the key}
  aInx := XhGetIndex(aKey, FTable.Count);
  {traverse the linked list at this entry, looking for the key in each
   node we encounter--a case-sensitive comparison}
  Node := TXpIntHashNode(FTable[aInx]);
  while (Node <> nil) do begin
    if XhCompareKey(aKey, Node.FKey) then begin
      Result := true;
      aNode := Node;
      Exit;
    end;
    Node := Node.FNext;
  end;
end;
{--------}
procedure TXpBaseHash.XhFreeKeyPrim(aKey : Pointer);
begin
  { Do nothing. }
end;
{--------}
function TXpBaseHash.XhGetIndex(const aKey   : Pointer;
                                const aCount : Longint) : Longint;
begin
  { Do nothing. }
  Result := -1;
end;
{--------}
function TXpBaseHash.XhMoveNodePrim(OldTable : TXpPointerList;
                                    OldNodeInx : Longint;
                                    Node : TXpIntHashNode): Boolean;
var
  Inx  : Longint;
  NextNode : TXpIntHashNode;
  PrevNode : TXpIntHashNode;
  TmpNode : TXpIntHashNode;
begin
  { Assumption: The node will not be found in the table because we are only
    being called during a resize. }

  { Assumption: Table does not need to be expanded since this method is
    called during table expansion. }

  { Remove the node from the old table. }
  TmpNode := TXpIntHashNode(OldTable[OldNodeInx]);
  PrevNode := nil;
  while assigned(TmpNode) and
       (not XhCompareKey(TmpNode.FKey, Node.FKey)) do begin
    PrevNode := TmpNode;
    TmpNode := TmpNode.FNext;
  end;
  if assigned(TmpNode) then begin
    NextNode := TmpNode.FNext;
    if assigned(PrevNode) then
      PrevNode.FNext := NextNode
    else if assigned(NextNode) then
      OldTable.List[OldNodeInx] := NextNode
    else
      OldTable.List[OldNodeInx] := nil;
  end;

  { Calculate the index, ie hash, of the key. }
  Inx := XhGetIndex(Node.FKey, FTable.Count);

  { Insert the node into the new table. }
  Result := true;
  Node.FNext := TXpIntHashNode(FTable[Inx]);
  FTable.List[Inx] := Node;

end;
{--------}
procedure TXpBaseHash.XhResizeTable(const increase : boolean);
var
  OldTable : TXpPointerList;
  Count    : Integer;
  Node     : TXpIntHashNode;
  NewSize  : Integer;
begin
  // Kylix 1 raises a warning that NewSize is never initialized.       {!!.55}
  // This warning is invalid.                                          {!!.55}
  FAtMin := False;
  { Are we increasing or decreasing? }
  if increase then begin
    { Increasing. Have we reached the limits of the xpc_HashSizes array? }
    if FHashSizeIndex = high(xpc_HashSizes) then begin
      { Yes.  Double the current size and add one.  If divisible by 3 then
        add 2. }
      NewSize := (FTable.Count * 2) + 1;
      if NewSize mod 3 = 0 then
        inc(NewSize, 2);
    end
    else begin
      { No.  Move to the next size. }
      inc(FHashSizeIndex);
      NewSize := xpc_HashSizes[FHashSizeIndex];
    end;
  end
  else begin
    { Decreasing.  Have we reached our lower limit? }
    FAtMin := (FHashSizeIndex = FMinSizeIndex);
    if FAtMin then
      exit
    else begin
      dec(FHashSizeIndex);
      NewSize := xpc_HashSizes[FHashSizeIndex];
    end;
  end;

  { Expand the table. }
  OldTable := FTable;

  FTable := TXpPointerList.Create;
  FTable.Count := NewSize;

  for Count := 0 to Pred(OldTable.Count) do begin
    Node := TXpIntHashNode(OldTable.List[Count]);
    repeat
      if Assigned(Node) then
        XhMoveNodePrim(OldTable, Count, Node);
      Node := TXpIntHashNode(OldTable.List[Count]);
    until (not assigned(Node));
  end;

  OldTable.Free;
end;
{====================================================================}


{===TXpIntHash==========================================================}
function TXpIntHash.Add(aKey   : LongInt;
                        aValue : Pointer): Boolean;
begin
  Result := XhAddPrim(pointer(aKey), aValue);
end;
{--------}
{$IFDEF CompileDebugCode}

procedure TXpIntHash.DebugPrint(const aFileName: string);
var
  F    : text;
  i    : Integer;
  Node : TXpIntHashNode;
begin
  System.Assign(F, aFileName);
  System.Rewrite(F);

  for i := 0 to pred(FTable.Count) do begin
    writeln(F, '---', i, '---');
    Node := TXpIntHashNode(FTable[i]);
    while assigned(Node) do begin
      writeln(F, Longint(Node.FKey):10, intToStr(longInt(Node.FValue)):20);
      Node := Node.FNext;
    end;
  end;

  writeln(F);
  writeln(F, 'Count: ', Count, ' (mean: ', Count/FTable.Count:5:3, ')');

  System.Close(F);
end;
{$ENDIF}
{--------}
function TXpIntHash.XhGetIndex(const aKey   : Pointer;
                               const aCount : Longint): Longint;
begin
  Result := Longint(aKey) mod aCount;
end;
{--------}
function TXpIntHash.Get(const aKey: Integer): Pointer;
var
  Inx  : Longint;
  Node : TXpIntHashNode;
begin
  Result := nil;
  if XhFindPrim(Pointer(aKey), Inx, Node) then
    Result := Node.FValue
end;
{--------}
procedure TXpIntHash.Iterate(const CallBack : TXpIntHashIteratorFunction;
                             const cookie1, cookie2, cookie3 : longInt);
var
  Count    : Integer;
  Node     : TXpIntHashNode;
begin
  for Count := 0 to Pred(FTable.Count) do begin
    Node := TXpIntHashNode(FTable[Count]);
    while assigned(Node) do begin
      CallBack(longInt(Node.FKey), Node.FValue, cookie1, cookie2, cookie3);
      Node := Node.FNext;
    end;
  end;
end;
{--------}
procedure TXpIntHash.IterateSafely(const CallBack : TXpIntHashIteratorFunction;
                                   const cookie1, cookie2, cookie3 : longInt);
var
  Count    : Integer;
  FirstNode : TXpIntHashNode;
  NextNode : TXpIntHashNode;
  Node : TXpIntHashNode;
  PrevNode : TXpIntHashNode;
begin
  { Assumption: The TXpIntHashNode.ExtraData property is not used for
    any other purpose. }
  { String the nodes together. }
  FirstNode := nil;
  PrevNode := nil;
  for Count := 0 to Pred(FTable.Count) do begin
    Node := TXpIntHashNode(FTable[Count]);
    while assigned(Node) do begin

      if FirstNode = nil then
        FirstNode := Node;

      if Assigned(PrevNode) then
        PrevNode.ExtraData := Node;

      PrevNode := Node;
      Node := Node.FNext;
    end;
  end;

  { Iterate through the list of nodes. }
  Node := FirstNode;
  while assigned(Node) do begin
    NextNode := Node.ExtraData;
    Callback(longInt(Node.FKey), Node.FValue, cookie1, cookie2, cookie3);
    Node := NextNode;
  end;

end;
{--------}
function TXpIntHash.Remove(const aKey: Longint) : Boolean;
var
  Inx  : Longint;
  Node : TXpIntHashNode;
begin
  if XhFindPrim(Pointer(aKey), Inx, Node) then begin
    XhDeletePrim(Pointer(aKey), Inx);

    { Shrink the table if:
      1. Shrinking is allowed.
      2. We are not at the minimum size already.
      3. We have some elements.
      4. We have some elements and we're under 1/6 full
    }
    if FCanShrink and (not FAtMin) and
      (FCount > 10) and ((FCount * 6) < FTable.Count) then
      XhResizeTable(False);
    Result := True;
  end
  else
    Result := False;
end;
{====================================================================}

{===TXpStrHash=======================================================}
constructor TXpStrHash.Create(initialSizeIndex : Integer);
begin
  inherited Create;
  FAtMin := False;
  FCount := 0;
  if initialSizeIndex > high(xpc_HashSizes) then
    initialSizeIndex := high(xpc_HashSizes);
  FHashSizeIndex := initialSizeIndex;
  FMinSizeIndex := FHashSizeIndex;
  FOnDisposeData := nil;
  FTable := TXpPointerList.Create;
  FTable.Count := xpc_HashSizes[FHashSizeIndex];
end;
{--------}
function TXpStrHash.Add(const aKey   : DOMString;                      {!!.57}
                        aValue : Pointer): Boolean;
begin
  Result := XhAddPrim(aKey, aValue);
end;
{--------}
procedure TXpStrHash.Clear;
var
  i    : Integer;
  Node : TXpStrHashNode;
  Temp : TXpStrHashNode;
begin
  for i := 0 to pred(FTable.Count) do begin
    Node := TXpStrHashNode(FTable[i]);
    while assigned(Node) do begin
      Temp := Node;
      Node := Node.FNext;
      if assigned(FOnDisposeData) then
        FOnDisposeData(Self, Temp.FValue);
      Temp.Free;
    end;
    FTable[i] := nil;
  end;
  FCount := 0;
end;
{--------}
destructor TXpStrHash.Destroy;
begin
  Clear;
  FTable.Free;
  inherited Destroy;
end;
{--------}
function TXpStrHash.Exists(const aKey : DOMString) : Boolean;
var
  Inx  : Longint;
  Node : TXpStrHashNode;
begin
  Result := XhFindPrim(aKey, Inx, Node);
end;
{--------}
function TXpStrHash.ExistsWithHash(const aKey : DOMString;
                                   const aHash : DWORD): Boolean;
var
  aInx : Longint;
  Node : TXpStrHashNode;
begin
  { Assume we won't find aKey. }
  Result := false;
  { Calculate the index of the key. }
  aInx := Longint(aHash) mod FTable.Count;
  { Traverse the linked list at this entry, looking for the key in each
    node we encounter--a case-sensitive comparison. }
  Node := TXpStrHashNode(FTable[aInx]);
  while (Node <> nil) do begin
    if XhCompareKey(aKey, Node.FKey) then begin
      Result := true;
      Exit;
    end;
    Node := Node.FNext;
  end;
end;
{--------}
function TXpStrHash.Get(const aKey: DOMString): Pointer;
var
  Inx  : Longint;
  Node : TXpStrHashNode;
begin
  Result := nil;
  if XhFindPrim(aKey, Inx, Node) then
    Result := Node.FValue
end;
{--------}
function TXpStrHash.Remove(const aKey: DOMString) : Boolean;
var
  Inx  : Longint;
  Node : TXpStrHashNode;
begin
  if XhFindPrim(aKey, Inx, Node) then begin
    XhDeletePrim(aKey, Inx);

    { Shrink the table if:
      1. Shrinking is allowed.
      2. We are not at the minimum size already.
      3. We have some elements.
      4. We have some elements and we're under 1/6 full
    }
    if FCanShrink and (not FAtMin) and
      (FCount > 10) and ((FCount * 6) < FTable.Count) then
      XhResizeTable(False);
    Result := True;
  end
  else
    Result := False;
end;
{--------}
function TXpStrHash.XhAddPrim(const aKey   : DOMString;                {!!.57}
                              aValue : Pointer): Boolean;
var
  Inx  : Longint;
  Node : TXpStrHashNode;
begin
  if XhFindPrim(aKey, Inx, Node) then
    Result := false
  else begin
    Result := true;
    Node := XhCreateNode;
    Node.FNext := TXpStrHashNode(FTable[Inx]);
    Node.FKey := aKey;
    Node.FValue := aValue;
    FTable.List[Inx] := Node;
    inc(FCount);

    { Expand the table if we've reached our load limit. }
    if (FCount > (FTable.Count * xpcHashLoadFactor)) then
      XhResizeTable(True);
  end;
end;
{--------}
function TXpStrHash.XhCompareKey(const aKey1 : DOMString;
                                 const aKey2 : DOMString) : Boolean;
begin
  Result := (aKey1 = aKey2);
end;
{--------}
function TXpStrHash.XhCreateNode: TXpStrHashNode;
begin
  Result := TXpStrHashNode.Create;
end;
{--------}
procedure TXpStrHash.XhDeletePrim(const aKey : DOMString;
                                  const aInx : Longint);
var
  Node : TXpStrHashNode;
  NextNode : TXpStrHashNode;
  PrevNode : TXpStrHashNode;
begin
  Node := TXpStrHashNode(FTable.List[aInx]);
  PrevNode := nil;
  while assigned(Node) and (not XhCompareKey(Node.FKey, aKey)) do begin
    PrevNode := Node;
    Node := Node.FNext;
  end;
  if assigned(Node) then begin
    if assigned(FOnDisposeData) then
      FOnDisposeData(Self, Node.FValue);
    NextNode := Node.FNext;
    Node.Free;
    if assigned(PrevNode) then
      PrevNode.FNext := NextNode
    else if assigned(NextNode) then
      FTable.List[aInx] := NextNode
    else
      FTable.List[aInx] := nil;
  end;
  dec(FCount);
end;
{--------}
function TXpStrHash.XhFindPrim(const aKey  : DOMString;
                                 var aInx  : Longint;
                                 var aNode : TXpStrHashNode): Boolean;
var
  Node : TXpStrHashNode;
begin
  {assume we won't find aKey}
  Result := false;
  aNode := nil;
  {calculate the index, ie hash, of the key}
  aInx := XhGetIndex(aKey, FTable.Count);
  {traverse the linked list at this entry, looking for the key in each
   node we encounter--a case-sensitive comparison}
  Node := TXpStrHashNode(FTable[aInx]);
  while (Node <> nil) do begin
    if XhCompareKey(aKey, Node.FKey) then begin
      Result := true;
      aNode := Node;
      Exit;
    end;
    Node := Node.FNext;
  end;
end;
{--------}
function TXpStrHash.XhGetIndex(const aKey   : DOMString;
                               const aCount : Longint) : Longint;
{Rewritten !!.55}
begin
  if aKey = '' then
    Result := 0
  else
    Result := Longint(XpCalcELFHash(aKey[1], Length(aKey) * 2)) mod aCount;
end;
{--------}
function TXpStrHash.XhMoveNodePrim(OldTable : TXpPointerList;
                                   OldNodeInx : Longint;
                                   Node : TXpStrHashNode): Boolean;
var
  Inx  : Longint;
  NextNode : TXpStrHashNode;
  PrevNode : TXpStrHashNode;
  TmpNode : TXpStrHashNode;
begin
  { Assumption: The node will not be found in the table because we are only
    being called during a resize. }

  { Assumption: Table does not need to be expanded since this method is
    called during table expansion. }

  { Remove the node from the old table. }
  TmpNode := TXpStrHashNode(OldTable[OldNodeInx]);
  PrevNode := nil;
  while assigned(TmpNode) and
       (not XhCompareKey(TmpNode.FKey, Node.FKey)) do begin
    PrevNode := TmpNode;
    TmpNode := TmpNode.FNext;
  end;
  if assigned(TmpNode) then begin
    NextNode := TmpNode.FNext;
    if assigned(PrevNode) then
      PrevNode.FNext := NextNode
    else if assigned(NextNode) then
      OldTable.List[OldNodeInx] := NextNode
    else
      OldTable.List[OldNodeInx] := nil;
  end;

  { Calculate the index, ie hash, of the key. }
  Inx := XhGetIndex(Node.FKey, FTable.Count);

  { Insert the node into the new table. }
  Result := true;
  Node.FNext := TXpStrHashNode(FTable[Inx]);
  FTable.List[Inx] := Node;

end;
{--------}
procedure TXpStrHash.XhResizeTable(const increase : boolean);
var
  OldTable : TXpPointerList;
  Count    : Integer;
  Node     : TXpStrHashNode;
  NewSize  : Integer;
begin
  // Kylix 1 raises a warning that NewSize is never initialized.       {!!.55}
  // This warning is invalid.                                          {!!.55}
  FAtMin := False;
  { Are we increasing or decreasing? }
  if increase then begin
    { Increasing. Have we reached the limits of the xpc_HashSizes array? }
    if FHashSizeIndex = high(xpc_HashSizes) then begin
      { Yes.  Double the current size and add one.  If divisible by 3 then
        add 2. }
      NewSize := (FTable.Count * 2) + 1;
      if NewSize mod 3 = 0 then
        inc(NewSize, 2);
    end
    else begin
      { No.  Move to the next size. }
      inc(FHashSizeIndex);
      NewSize := xpc_HashSizes[FHashSizeIndex];
    end;
  end
  else begin
    { Decreasing.  Have we reached our lower limit? }
    FAtMin := (FHashSizeIndex = FMinSizeIndex);
    if FAtMin then
      exit
    else begin
      dec(FHashSizeIndex);
      NewSize := xpc_HashSizes[FHashSizeIndex];
    end;
  end;

  { Expand the table. }
  OldTable := FTable;

  FTable := TXpPointerList.Create;
  FTable.Count := NewSize;

  for Count := 0 to Pred(OldTable.Count) do begin
    Node := TXpStrHashNode(OldTable.List[Count]);
    repeat
      if Assigned(Node) then
        XhMoveNodePrim(OldTable, Count, Node);
      Node := TXpStrHashNode(OldTable.List[Count]);
    until (not assigned(Node));
  end;

  OldTable.Free;
end;
{--------}
{$IFDEF CompileDebugCode}
procedure TXpStrHash.DebugPrint(const aFileName: string);
var
  F    : text;
  i    : Integer;
  Node : TXpIntHashNode;
begin
  System.Assign(F, aFileName);
  System.Rewrite(F);

  for i := 0 to pred(FTable.Count) do begin
    writeln(F, '---', i, '---');
    Node := TXpIntHashNode(FTable[i]);
    while assigned(Node) do begin
      writeln(F, Longint(Node.FKey):10, intToStr(longInt(Node.FValue)):20);
      Node := Node.FNext;
    end;
  end;

  writeln(F);
  writeln(F, 'Count: ', Count, ' (mean: ', Count/FTable.Count:5:3, ')');

  System.Close(F);
end;
{$ENDIF}
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

function XpCalcElfHash(const Buffer; BufSize : Integer) : DWORD;
var
  BufAsBytes : TXpByteArray absolute Buffer;
  G : DWORD;
  i : Integer;
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

end.

