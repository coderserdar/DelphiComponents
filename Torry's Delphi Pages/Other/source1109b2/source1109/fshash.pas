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

{$I fsdefine.inc}
{.$DEFINE CompileDebugCode}
Unit fshash;

Interface

Uses
  SysUtils,
  fsllbase;

Type
  PSlArray = ^TSlArray;
  TSlArray = Array[0..131071] Of Word;
  { forward declarations }
  TfsBaseHashTable = Class;

  TfsHashIteratorFunction = Procedure(aKey: Longint; aData: pointer;
    Const cookie1, cookie2, cookie3: TffWord32) Of Object;
  { Used by TfsHash.Iterate.  Called for each item in the hash
    table. }

  TfsHash64IteratorFunction = Procedure(aKey: TffInt64; aData: pointer;
    Const cookie1, cookie2, cookie3: TffWord32) Of Object;
  { Used by TfsHash64.Iterate.  Called for each item in the hash
    table. }
{ This type defines the kind of procedure called when the data associated
  with a hash table entry must be freed by the owning object. }
  TfsDisposeDataProc = Procedure(Sender: TfsBaseHashTable; AData: Pointer) Of Object;

  { This class is used to store key/value pairs within the hash table. }
  { Assumption: The TfsHashNode.ExtraData property is not used for
    any other purpose. }
  TfsHashNode = Class(TFSSpecObject)
  Protected
  Public
    fhKey: Pointer;
    fhNext: TfsHashNode; { The next node in this hash table slot. }
    fhValue: Pointer;

    ExtraData: pointer;
  End;

  { This class is a simple hash table implementation.  It assumes the
    key values will be long integers and the associated data will be a
    pointer.  It assumes the owning object will properly destroy the
    data associated with each hash table entry by assigning a disposal
    function to the OnDispose property of this class.

    This implementation is thread-safe.
  }

  TfsBaseHashTable = Class(TFSSpecObject)
  Protected {private}
    FAtMin: boolean;
    FCanShrink: boolean;
    FCount: Integer;
    FHashSizeIndex: Integer;
    FMinSizeIndex: Integer;
    FOnDisposeData: TfsDisposeDataProc;
    FTable: TfsPointerList;
  Protected
    Function fhAddPrim(aKey: Pointer;
      aValue: Pointer): Boolean;
    {-Use this method to add an entry to the hash table.  Returns True if
      the key/value pair was added or False if the key is already in the
      hash table. }

    Function fhCompareKey(Const aKey1: Pointer;
      Const aKey2: Pointer): Boolean; Virtual;

    Function fhCreateNode: TfsHashNode; Virtual;

    Procedure fhDeletePrim(Const AKey: Pointer;
      Const AInx: Integer);
    {-This method is used to delete an entry in the hash table.  aInx
      must specify the exact slot within the table containing the entry.
      This method will then run through the associated entry list and
      locate the exact hash node using aKey. }

    Function fhFindPrim(Const AKey: Pointer;
      Var AInx: Integer;
      Var ANode: TfsHashNode): Boolean;
    {-This method is used to find an entry within the hash table.
      It fills aInx with the index of the key within the hash table and
      aNode with a pointer to the hash node storing the entry. }

    Procedure fhFreeKeyPrim(aKey: pointer); Virtual; Abstract;
    {-Use this method to free a key created for a TfsHashNode.
      Called from fhDeletePrim. }

    Function fhGetIndex(Const AKey: Pointer;
      Const ACount: Integer): Integer; Virtual; Abstract;
    {calculate the index, ie hash, of the key}

    Function fhMoveNodePrim(OldTable: TfsPointerList;
      OldNodeInx: Integer;
      Node: TfsHashNode): Boolean;
    {-Used by fhResizeTable to move a node from an old table to the new,
      resized table.  Assumption: Resized table has enough room to hold
      the new node. }

    Procedure fhResizeTable(Const increase: boolean); Virtual;
    {-Resize the table. If you want the table to increase to the next
      level of capacity, set increase to True.  If you want the table
      to decrease to the next level of capacity, set increase to False. }
  Public
    Constructor Create(initialSizeIndex: Integer); Virtual;
    {-This method creates and initializes the hash table.  initialSizeIndex
      specifies the index of array fsc_HashSizes that is to specify the
      initial number of slots within the hash table. }

    Destructor Destroy; Override;


    Procedure Clear;
    {-Use this method to clear the hash table.  The OnDisposeData event is
      raised for each entry in case the caller needs to free the data
      associated with the entry.}
    Property Table: TfsPointerList read FTable write FTable;
    Property CanShrink: boolean Read FCanShrink Write FCanShrink;
    {-Use this property to indicate whether or not the hash table may
      be reduced in size when the number of items is less than 1/6 the
      number of slots. }

    Property Count: Integer Read FCount;
    {-Use this property to determine the number of entries in the hash
      table. }

    Property OnDisposeData: TfsDisposeDataProc
      Read FOnDisposeData Write FOnDisposeData;
    {-This event is raised when data associated with an entry must be
      destroyed by the calling object. }
  End;

  TfsHash = Class(TfsBaseHashTable)
  Protected
    Procedure fhFreeKeyPrim(aKey: pointer); Override;

    Function fhGetIndex(Const AKey: Pointer;
      Const ACount: Integer): Integer; Override;
    {calculate the index, ie hash, of the key}

  Public
    Function Add(aKey: Longint;
      aValue: Pointer): Boolean;
    {-Use this method to add an entry to the hash table.  Returns True if
      the key/value pair was added or False if the key is already in the
      hash table. }

    Function Get(Const AKey: Longint): Pointer;
    {-Use this method to find an entry in the hash table. }

    Procedure Iterate(Const CallBack: TfsHashIteratorFunction;
      Const cookie1, cookie2, cookie3: Longint);
    {-Use this method to iterate through the entries in the hash table.
      Callback will be called once for each entry. }

    Procedure IterateSafely(Const CallBack: TfsHashIteratorFunction;
      Const cookie1, cookie2, cookie3: Longint);
    {-Use this method to iterate through the entries in the hash table.
      It is safe in the sense that it allows the Callback function to
      free the item that is the current subject of the iteration.
      Callback will be called once for each entry. }

    Function Remove(Const AKey: Longint): Boolean; {!!.02}
    {-Use this method to remove an entry from the hash table.  The
      OnDisposeData event is raised in case the caller needs to free the
      data associated with the entry. }

    {$IFDEF CompileDebugCode}
    Procedure DebugPrint(Const AFileName: String);
    {-Use this method to dump the contents of the hash table during
      testing stage. }
    {$ENDIF}

  End;

  TfsHash64 = Class(TfsBaseHashTable)
  Protected
    Function fhCompareKey(Const aKey1: Pointer;
      Const aKey2: Pointer): Boolean; Override;

    Procedure fhFreeKeyPrim(aKey: pointer); Override;

    Function fhGetIndex(Const AKey: Pointer;
      Const ACount: Integer): Integer; Override;
    {calculate the index, ie hash, of the key}

  Public
    Function Add(Const AKey: TffInt64;
      AValue: Pointer): Boolean;
    {-Use this method to add an entry to the hash table.  Returns True if
      the key/value pair was added or False if the key is already in the
      hash table. }

    Function Get(Const AKey: TffInt64): Pointer;
    {-Use this method to find an entry in the hash table. }

    Procedure Iterate(Const CallBack: TfsHash64IteratorFunction;
      Const cookie1, cookie2, cookie3: Longint);
    {-Use this method to iterate through the entries in the hash table.
      Callback will be called once for each entry. }

    Procedure IterateSafely(Const CallBack: TfsHash64IteratorFunction;
      Const cookie1, cookie2, cookie3: Longint);
    {-Use this method to iterate through the entries in the hash table.
      It is safe in the sense that it allows the Callback function to
      free the item that is the current subject of the iteration.
      Callback will be called once for each entry. }

    Procedure Remove(Const AKey: TffInt64);
    {-Use this method to remove an entry from the hash table.  The
      OnDisposeData event is raised in case the caller needs to free the
      data associated with the entry. }
    function Find(Const AKey: TffInt64): boolean;

    {$IFDEF CompileDebugCode}
    Procedure DebugPrint(Const AFileName: String);
    {-Use this method to dump the contents of the hash table during
      testing stage. }
    {$ENDIF}

  End;

  { This class is a threadsafe version of TfsHash.  This class allows multiple
    threads to have read access or one thread to have write access (i.e.,
    multiple read, exclusive write).  A thread is granted write access only if
    there are no reading threads or writing threads.}

  TfsThreadHash = Class(TfsHash)
  Protected {private}
    FPortal: TfsReadWritePortal;
  Public

    Constructor Create(initialSizeIndex: Integer); Override;

    Destructor Destroy; Override;

    Function BeginRead: TfsThreadHash;
    {-A thread must call this method to gain read access to the list.
      Returns the instance of TffThreadList as a convenience. }

    Function BeginWrite: TfsThreadHash;
    {-A thread must call this method to gain write access to the list.
      Returns the instance of TffThreadList as a convenience.}

    Procedure EndRead;
    {-A thread must call this method when it no longer needs read access
      to the list.  If it does not call this method, all writers will
      be perpetually blocked. }

    Procedure EndWrite;
    {-A thread must call this method when it no longer needs write access
      to the list.  If it does not call this method, all readers and writers
      will be perpetualy blocked. }
  End;

  TfsThreadHash64 = Class(TfsHash64)
  Protected {private}
    FPortal: TfsReadWritePortal;
  Public

    Constructor Create(initialSizeIndex: Integer); Override;

    Destructor Destroy; Override;

    Function BeginRead: TfsThreadHash64;
    {-A thread must call this method to gain read access to the list.
      Returns the instance of TffThreadList as a convenience. }

    Function BeginWrite: TfsThreadHash64;
    {-A thread must call this method to gain write access to the list.
      Returns the instance of TffThreadList as a convenience.}

    Procedure EndRead;
    {-A thread must call this method when it no longer needs read access
      to the list.  If it does not call this method, all writers will
      be perpetually blocked. }

    Procedure EndWrite;
    {-A thread must call this method when it no longer needs write access
      to the list.  If it does not call this method, all readers and writers
      will be perpetualy blocked. }
  End;

  {The following algorithm is the UNIX ELF format hash. The code was
   converted and adapted from the one in C published in Dr Dobbs
   Journal, April 1996, in the article "Hashing Rehashed" by
   Andrew Binstock.}
Function FSCalcELFHash(Const Buffer; BufSize: Integer): TffWord32;

Function FSCalcShStrELFHash(Const S: TffShStr): TffWord32;
Function wspak(sString: AnsiString): AnsiString;
Function XorString(Const Key, Src: ShortString): ShortString;
Function HexToInt(Value: String): Int64;
Procedure DecodeBuffer(Var aBuffer; BufferSize, IDWord: Word);

Const
  { The following constants represent indexes into fsc_HashSizes array
    declared in the implementation section of this unit.  Use these constants
    to specify the initial size index for hash tables. }
  fsc_Size59 = 0;
  fsc_Size127 = 1;
  fsc_Size257 = 2;
  fsc_Size521 = 3;
  fsc_Size1049 = 4;
  fsc_Size2099 = 5;

Implementation

{ The following array contains the legal hash table sizes.  Each is a prime
  number which allows for better spread of inserts within a hash table. }
Const
  fsc_HashSizes: Array[0..15] Of Integer =
  (59, 127, 257, 521, 1049, 2099, 4201, 8419,
    16843, 33703, 67409, 134837, 269683, 539389, 1078787, 2157587);

Const
  fsc_HashLoadFactor = 4;
  { When storing integer-ish items in a hash table, the hash table can
    quickly walk through a slot's chain of nodes in those cases where a slot
    contains more than one item.  As a result, we can load up the hash
    table with more items than slots.  This constant specifies how far the
    table may be overloaded.  The table won't be resized until this limit
    is reached.  The limit is defined as Number of Slots * Load Factor. }

{===TfsBaseHashTable=================================================}

Constructor TfsBaseHashTable.Create(initialSizeIndex: Integer);
Begin
  Inherited Create;

  FAtMin := False;
  FCount := 0;
  If initialSizeIndex > high(fsc_HashSizes) Then
    initialSizeIndex := high(fsc_HashSizes);
  FHashSizeIndex := initialSizeIndex;
  FMinSizeIndex := FHashSizeIndex;
  FOnDisposeData := Nil;
  FTable := TfsPointerList.Create;
  FTable.Count := fsc_HashSizes[FHashSizeIndex];
End;
{--------}

Function TfsBaseHashTable.fhCreateNode: TfsHashNode;
Begin
  Result := TfsHashNode.Create;
End;
{--------}

Procedure TfsBaseHashTable.Clear;
Var
  i: Integer;
  Node: TfsHashNode;
  Temp: TfsHashNode;
Begin
  For i := 0 To pred(FTable.Count) Do
    Begin
      Node := TfsHashNode(FTable[i]);
      While assigned(Node) Do
        Begin
          Temp := Node;
          Node := Node.fhNext;
          If assigned(FOnDisposeData) Then
            FOnDisposeData(Self, Temp.fhValue);
          {Temp.fhValue := nil;}
          fhFreeKeyPrim(Temp.fhKey); {!!.01}
          Temp.Free;
        End;
      FTable[i] := Nil;
    End;
  FCount := 0;
End;
{--------}

Destructor TfsBaseHashTable.Destroy;
Begin
  Clear;
  FTable.Free;
  Inherited Destroy;
End;
{--------}

Function TfsBaseHashTable.fhAddPrim(aKey: Pointer;
  aValue: Pointer): Boolean;
Var
  Inx: Integer;
  Node: TfsHashNode;
Begin
  If fhFindPrim(aKey, Inx, Node) Then
    Result := False
  Else
    Begin
      Result := True;
      Node := fhCreateNode;
      Node.fhNext := TfsHashNode(FTable[Inx]);
      Node.fhKey := aKey;
      Node.fhValue := aValue;
      FTable.List[Inx] := Node;
      inc(FCount);

      { Expand the table if we've reached our load limit. }
      If (FCount > (FTable.Count * fsc_HashLoadFactor)) Then
        fhResizeTable(True);
    End;
End;
{--------}

Function TfsBaseHashTable.fhCompareKey(Const aKey1: Pointer;
  Const aKey2: Pointer): Boolean;
Begin
  Result := aKey1 = aKey2;
End;
{--------}

Procedure TfsBaseHashTable.fhDeletePrim(Const aKey: Pointer;
  Const aInx: Integer);
Var
  Node: TfsHashNode;
  NextNode: TfsHashNode;
  PrevNode: TfsHashNode;
Begin
  Node := TfsHashNode(FTable.List[aInx]);
  PrevNode := Nil;
  While assigned(Node) And (Not fhCompareKey(Node.fhKey, AKey)) Do
    Begin
      PrevNode := Node;
      Node := Node.fhNext;
    End;
  If assigned(Node) Then
    Begin
      If assigned(FOnDisposeData) Then
        FOnDisposeData(Self, Node.fhValue);
      NextNode := Node.fhNext;
      {Node.fhValue := nil;}
      fhFreeKeyPrim(Node.fhKey);
      Node.Free;
      If assigned(PrevNode) Then
        PrevNode.fhNext := NextNode
      Else If assigned(NextNode) Then
        FTable.List[aInx] := NextNode
      Else
        FTable.List[aInx] := Nil;
    End;
  dec(FCount);
End;
{--------}

Function TfsBaseHashTable.fhFindPrim(Const AKey: Pointer;
  Var AInx: Integer;
  Var ANode: TfsHashNode): Boolean;
Var
  Node: TfsHashNode;
Begin
  {assume we won't find aKey}
  Result := False;
  aNode := Nil;
  {calculate the index, ie hash, of the key}
  aInx := fhGetIndex(aKey, FTable.Count);
  {traverse the linked list at this entry, looking for the key in each
   node we encounter--a case-sensitive comparison}
  Node := TfsHashNode(FTable[aInx]);
  While (Node <> Nil) Do
    Begin
      If fhCompareKey(AKey, Node.fhKey) Then
        Begin
          Result := True;
          aNode := Node;
          Exit;
        End;
      Node := Node.fhNext;
    End;
End;
{--------}

Function TfsBaseHashTable.fhMoveNodePrim(OldTable: TfsPointerList;
  OldNodeInx: Integer;
  Node: TfsHashNode): Boolean;
Var
  Inx: Integer;
  NextNode: TfsHashNode;
  PrevNode: TfsHashNode;
  TmpNode: TfsHashNode;
Begin
  { Assumption: The node will not be found in the table because we are only
    being called during a resize. }

  { Assumption: Table does not need to be expanded since this method is
    called during table expansion. }

  { Remove the node from the old table. }
  TmpNode := TfsHashNode(OldTable[OldNodeInx]);
  PrevNode := Nil;
  While assigned(TmpNode) And
    (Not fhCompareKey(TmpNode.fhKey, Node.fhKey)) Do
    Begin
      PrevNode := TmpNode;
      TmpNode := TmpNode.fhNext;
    End;
  If assigned(TmpNode) Then
    Begin
      NextNode := TmpNode.fhNext;
      If assigned(PrevNode) Then
        PrevNode.fhNext := NextNode
      Else If assigned(NextNode) Then
        OldTable.List[OldNodeInx] := NextNode
      Else
        OldTable.List[OldNodeInx] := Nil;
    End;

  { Calculate the index, ie hash, of the key. }
  Inx := fhGetIndex(Node.fhKey, FTable.Count);

  { Insert the node into the new table. }
  Result := True;
  Node.fhNext := TfsHashNode(FTable[Inx]);
  FTable.List[Inx] := Node;

End;
{--------}

Procedure TfsBaseHashTable.fhResizeTable(Const increase: boolean);
Var
  OldTable: TfsPointerList;
  Count: Integer;
  Node: TfsHashNode;
  NewSize: Integer;
Begin
  FAtMin := False;
  { Are we increasing or decreasing? }
  If increase Then
    Begin
      { Increasing. Have we reached the limits of the fsc_HashSizes array? }
      If FHashSizeIndex = high(fsc_HashSizes) Then
        Begin
          { Yes.  Double the current size and add one.  If divisible by 3 then
            add 2. }
          NewSize := (FTable.Count * 2) + 1;
          If NewSize Mod 3 = 0 Then
            inc(NewSize, 2);
        End
      Else
        Begin
          { No.  Move to the next size. }
          inc(FHashSizeIndex);
          NewSize := fsc_HashSizes[FHashSizeIndex];
        End;
    End
  Else
    Begin
      { Decreasing.  Have we reached our lower limit? }
      FAtMin := (FHashSizeIndex = FMinSizeIndex);
      If FAtMin Then
        Exit
      Else
        Begin
          dec(FHashSizeIndex);
          NewSize := fsc_HashSizes[FHashSizeIndex];
        End;
    End;

  { Expand the table. }
  OldTable := FTable;

  FTable := TfsPointerList.Create;
  FTable.Count := NewSize;

  For Count := 0 To Pred(OldTable.Count) Do
    Begin
      Node := TfsHashNode(OldTable.List[Count]);
      Repeat
        If Assigned(Node) Then
          fhMoveNodePrim(OldTable, Count, Node);
        Node := TfsHashNode(OldTable.List[Count]);
      Until (Not assigned(Node));
    End;

  OldTable.Free;
End;
{====================================================================}

{===TfsHash==========================================================}

Function TfsHash.Add(aKey: Longint;
  aValue: Pointer): Boolean;
Begin
  Result := fhAddPrim(pointer(aKey), aValue);
End;
{--------}
{$IFDEF CompileDebugCode}

Procedure TfsHash.DebugPrint(Const AFileName: String);
Var
  F: text;
  i: Integer;
  Node: TfsHashNode;
Begin
  System.Assign(F, aFileName);
  System.Rewrite(F);

  For i := 0 To pred(FTable.Count) Do
    Begin
      writeln(F, '---', i, '---');
      Node := TfsHashNode(FTable[i]);
      While assigned(Node) Do
        Begin
          writeln(F, Longint(Node.fhKey): 10, IntToStr(Longint(Node.fhValue)): 20);
          Node := Node.fhNext;
        End;
    End;

  writeln(F);
  writeln(F, 'Count: ', Count, ' (mean: ', Count / FTable.Count: 5: 3, ')');

  System.Close(F);
End;
{$ENDIF}
{--------}

Procedure TfsHash.fhFreeKeyPrim(aKey: pointer);
Begin
  { Do nothing. }
End;
{--------}

Function TfsHash.fhGetIndex(Const AKey: Pointer;
  Const ACount: Integer): Integer;
Begin
  Result := Longint(AKey) Mod ACount;
End;
{--------}

Function TfsHash.Get(Const AKey: Integer): Pointer;
Var
  Inx: Integer;
  Node: TfsHashNode;
Begin
  Result := Nil;
  If fhFindPrim(Pointer(aKey), Inx, Node) Then
    Result := Node.fhValue
End;
{--------}

Procedure TfsHash.Iterate(Const CallBack: TfsHashIteratorFunction;
  Const cookie1, cookie2, cookie3: Longint);
Var
  Count: Integer;
  Node: TfsHashNode;
Begin
  For Count := 0 To Pred(FTable.Count) Do
    Begin
      Node := TfsHashNode(FTable[Count]);
      While assigned(Node) Do
        Begin
          CallBack(Longint(Node.fhKey), Node.fhValue, cookie1, cookie2, cookie3);
          Node := Node.fhNext;
        End;
    End;
End;
{--------}

Procedure TfsHash.IterateSafely(Const CallBack: TfsHashIteratorFunction;
  Const cookie1, cookie2, cookie3: Longint);
Var
  Count: Integer;
  FirstNode: TfsHashNode;
  NextNode: TfsHashNode;
  Node: TfsHashNode;
  PrevNode: TfsHashNode;
Begin
  { Assumption: The TfsHashNode.ExtraData property is not used for
    any other purpose. }
  { String the nodes together. }
  FirstNode := Nil;
  PrevNode := Nil;
  For Count := 0 To Pred(FTable.Count) Do
    Begin
      Node := TfsHashNode(FTable[Count]);
      While assigned(Node) Do
        Begin

          If FirstNode = Nil Then
            FirstNode := Node;

          If Assigned(PrevNode) Then
            PrevNode.ExtraData := Node;

          PrevNode := Node;
          Node := Node.fhNext;
        End;
    End;

  { Iterate through the list of nodes. }
  Node := FirstNode;
  While assigned(Node) Do
    Begin
      NextNode := Node.ExtraData;
      Callback(Longint(Node.fhKey), Node.fhValue, cookie1, cookie2, cookie3);
      Node := NextNode;
    End;

End;
{--------}

Function TfsHash.Remove(Const AKey: Longint): Boolean; {!!.02}
Var
  Inx: Integer;
  Node: TfsHashNode;
Begin
  If fhFindPrim(Pointer(aKey), Inx, Node) Then
    Begin
      fhDeletePrim(Pointer(aKey), Inx);

      { Shrink the table if:
        1. Shrinking is allowed.
        2. We are not at the minimum size already.
        3. We have some elements.
        4. We have some elements and we're under 1/6 full
      }
      If FCanShrink And (Not FAtMin) And
        (FCount > 10) And ((FCount * 6) < FTable.Count) Then
        fhResizeTable(False);
      Result := True; {!!.02}
    End {!!.02}
  Else {!!.02}
    Result := False; {!!.02}
End;
{====================================================================}

{===TfsHash64========================================================}

Function TfsHash64.Add(Const aKey: TffInt64;
  aValue: Pointer): Boolean;
Var
  keyPtr: pointer;
Begin
  FFGetMem(keyPtr, sizeOf(TffInt64));
  TffInt64(keyPtr^) := aKey;
  Result := fhAddPrim(keyPtr, aValue);
  If Not Result Then
    FFFreeMem(keyPtr, SizeOf(TffInt64));
End;
{--------}
{$IFDEF CompileDebugCode}

Procedure TfsHash64.DebugPrint(Const AFileName: String);
Var
  F: text;
  i: Integer;
  Node: TfsHashNode;
Begin
  System.Assign(F, aFileName);
  System.Rewrite(F);

  For i := 0 To pred(FTable.Count) Do
    Begin
      writeln(F, '---', i, '---');
      Node := TfsHashNode(FTable[i]);
      While assigned(Node) Do
        Begin
          writeln(F, FFI64ToStr(PffInt64(Node.fhKey)^), IntToStr(Longint(Node.fhValue)): 20);
          Node := Node.fhNext;
        End;
    End;

  writeln(F);
  writeln(F, 'Count: ', Count, ' (mean: ', Count / FTable.Count: 5: 3, ')');

  System.Close(F);
End;
{$ENDIF}
{--------}

Function TfsHash64.fhCompareKey(Const aKey1: Pointer;
  Const aKey2: Pointer): Boolean;
Begin
  Result := FFCmpI64(PffInt64(aKey1)^, PffInt64(aKey2)^) = 0;
End;
{--------}

Procedure TfsHash64.fhFreeKeyPrim(aKey: pointer);
Begin
  FFFreeMem(aKey, sizeOf(TffInt64));
End;
{--------}

Function TfsHash64.fhGetIndex(Const AKey: Pointer;
  Const ACount: Integer): Integer;
Var
  Int: Integer;
Begin
  Int := ffI64ModInt(PffInt64(AKey)^, ACount);
  Result := Int;
End;
{--------}

Function TfsHash64.Get(Const AKey: TffInt64): Pointer;
Var
  Inx: Integer;
  Node: TfsHashNode;
Begin
  Result := Nil;
  If fhFindPrim(@aKey, Inx, Node) Then
    Result := Node.fhValue
End;
{--------}

Procedure TfsHash64.Iterate(Const CallBack: TfsHash64IteratorFunction;
  Const cookie1, cookie2, cookie3: Longint);
Var
  Count: Integer;
  Node: TfsHashNode;
Begin
  For Count := 0 To Pred(FTable.Count) Do
    Begin
      Node := TfsHashNode(FTable[Count]);
      While assigned(Node) Do
        Begin
          CallBack(TffInt64(Node.fhKey^), Node.fhValue, cookie1, cookie2, cookie3);
          Node := Node.fhNext;
        End;
    End;
End;
{--------}

Procedure TfsHash64.IterateSafely(Const CallBack: TfsHash64IteratorFunction;
  Const cookie1, cookie2, cookie3: Longint);
Var
  Count: Integer;
  FirstNode: TfsHashNode;
  NextNode: TfsHashNode;
  Node: TfsHashNode;
  PrevNode: TfsHashNode;
Begin
  { Assumption: The TfsHashNode.ExtraData property is not used for
    any other purpose. }
  { String the nodes together. }
  FirstNode := Nil;
  PrevNode := Nil;
  For Count := 0 To Pred(FTable.Count) Do
    Begin
      Node := TfsHashNode(FTable[Count]);
      While assigned(Node) Do
        Begin

          If FirstNode = Nil Then
            FirstNode := Node;

          If Assigned(PrevNode) Then
            PrevNode.ExtraData := Node;

          PrevNode := Node;
          Node := Node.fhNext;
        End;
    End;

  { Iterate through the list of nodes. }
  Node := FirstNode;
  While assigned(Node) Do
    Begin
      NextNode := Node.ExtraData;
      Callback(TffInt64(Node.fhKey^), Node.fhValue, cookie1, cookie2, cookie3);
      Node := NextNode;
    End;

End;
{--------}

Procedure TfsHash64.Remove(Const AKey: TffInt64);
Var
  Inx: Integer;
  Node: TfsHashNode;
Begin
  If fhFindPrim(@aKey, Inx, Node) Then
    Begin
      fhDeletePrim(@aKey, Inx);

      { Shrink the table if:
        1. Shrinking is allowed.
        2. We are not at the minimum size already.
        3. We have some elements.
        4. We have some elements and we're under 1/6 full
      }
      If FCanShrink And (Not FAtMin) And
        (FCount > 10) And ((FCount * 6) < FTable.Count) Then
        fhResizeTable(False);
    End;
End;

function TfsHash64.Find(Const AKey: TffInt64): boolean;
Var
  Inx: Integer;
  Node: TfsHashNode;
Begin
  Result:= fhFindPrim(@aKey, Inx, Node);
End;
{====================================================================}

{===TfsThreadHash====================================================}

Function TfsThreadHash.BeginRead: TfsThreadHash;
Begin
  If IsMultiThread Then
    FPortal.BeginRead;
  Result := Self
End;
{--------}

Function TfsThreadHash.BeginWrite: TfsThreadHash;
Begin
  If IsMultiThread Then
    FPortal.BeginWrite;
  Result := Self
End;
{--------}

Constructor TfsThreadHash.Create(initialSizeIndex: Integer);
Begin
  Inherited Create(initialSizeIndex);
  FPortal := TfsReadWritePortal.Create;
End;
{--------}

Destructor TfsThreadHash.Destroy;
Begin
  If Assigned(FPortal) Then
    FPortal.Free;
  Inherited Destroy;
End;
{--------}

Procedure TfsThreadHash.EndRead;
Begin
  If IsMultiThread Then
    FPortal.EndRead;
End;
{--------}

Procedure TfsThreadHash.EndWrite;
Begin
  If IsMultiThread Then
    FPortal.EndWrite;
End;
{====================================================================}

{===TfsThreadHash64==================================================}

Function TfsThreadHash64.BeginRead: TfsThreadHash64;
Begin
  FPortal.BeginRead;
  Result := Self
End;
{--------}

Function TfsThreadHash64.BeginWrite: TfsThreadHash64;
Begin
  FPortal.BeginWrite;
  Result := Self
End;
{--------}

Constructor TfsThreadHash64.Create(initialSizeIndex: Integer);
Begin
  Inherited Create(initialSizeIndex);
  FPortal := TfsReadWritePortal.Create;
End;
{--------}

Destructor TfsThreadHash64.Destroy;
Begin
  If Assigned(FPortal) Then
    FPortal.Free;
  Inherited Destroy;
End;
{--------}

Procedure TfsThreadHash64.EndRead;
Begin
  FPortal.EndRead;
End;
{--------}

Procedure TfsThreadHash64.EndWrite;
Begin
  FPortal.EndWrite;
End;

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

Procedure DecodeBuffer(Var aBuffer; BufferSize, IDWord: Word);
Var
  i: Integer;
Begin
  For i := 0 To (BufferSize Div 2) - 1 Do
    TSlArray(aBuffer)[i] := TSlArray(aBuffer)[i] Xor IDWord;
End;

Function wspak(sString: AnsiString): AnsiString;
Var
  i: Integer;
Begin
  For i := Length(sString) Downto 1 Do
    Begin
      Result := Result + sString[i];
    End;
End;

Function XorString(Const Key, Src: ShortString): ShortString;
Var
  I: Integer;
Begin
  Result := Src;
  If Length(Key) > 0 Then
    For I := 1 To Length(Src) Do
      Result[I] := Chr(Byte(Key[1 + ((I - 1) Mod Length(Key))]) Xor Ord(Src[I]));
End;

{$Q-} {!!.05}

Function FSCalcELFHash(Const Buffer; BufSize: Integer): TffWord32;
Var
  BufAsBytes: TffByteArray Absolute Buffer;
  G: TffWord32;
  i: Integer;
Begin
  Result := 0;
  For i := 0 To pred(BufSize) Do
    Begin
      Result := (Result Shl 4) + BufAsBytes[i];
      G := Result And $F0000000;
      If (G <> 0) Then
        Result := Result Xor (G Shr 24);
      Result := Result And (Not G);
    End;
End;
{$Q+} {!!.05}
{--------}

Function HexToInt(Value: String): Int64;
Var
  I: Integer;
  base: Int64;

  Function HexDigitToInt(Ch: char): Integer;
  Var
    sb: Byte;
  Begin
    sb := ord(ch);
    If (sb >= ord('A')) And (sb <= ord('F')) Then
      Result := sb - ord('A') + 10
    Else If (sb >= ord('a')) And (sb <= ord('f')) Then
      Result := sb - ord('a') + 10
    Else If (sb >= ord('0')) And (sb <= ord('9')) Then
      Result := sb - ord('0')
    Else
      Raise Exception.Create(ch + ' is not a hex digit');
  End;
Begin
  Result := 0;
  Value := UpperCase(Value);
  base := 1;
  For I := Length(Value) Downto 1 Do
    Begin
      Result := Result + HexDigitToInt(Value[I]) * base;
      base := base * 16
    End;
End;

Function FSCalcShStrELFHash(Const S: TffShStr): TffWord32;
Var
  st: String;
Begin
  st := XorString('$', s);
  Result := FSCalcELFHash(St[1], length(St));
End;

End.

