// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  17581: mcmPriorityQueue.pas 
//
//    Rev 1.3    2014-02-02 21:10:06  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.2    15-06-2003 13:06:20  mcm    Version: IMG 1.3.4
// Added a Try.. Except in method remove to ensure proper behaviour if index is
// out of range. In this case Nil is returned.

//
//   Rev 1.1    27-01-2003 13:43:24  mcm

//
//   Rev 1.0    27-05-2002 16:22:24  mcm

unit mcmPriorityQueue;

interface

{$Include 'mcmDefines.pas'}

{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses {$IFNDEF GE_DXE2}
      SysUtils, Classes;
     {$ELSE}
      System.SysUtils, System.Classes;
     {$ENDIF}                         

//------------------------------------------------------------------------------
// Information Source: The Delphi Magazine, Issue 39, November 1998
// Original Author: Mr. Julian M. Bucknall
//------------------------------------------------------------------------------

type
  //----------------------------------------------------------------------------
  // Function taking two items to compare their priority.
  // This function is a parameter in the create method.
  // Returns: < 0 less than zero, if first item has less priority
  //          = 0 equal to zero, if items have same priority
  //          > 0 greater than zero, if second item has less priority.
  TmcmPriorityCompare = function(const Item1, Item2 : pointer) : integer of object;

type
  TPriorityQueueHandle = pointer;

//------------------------------------------------------------------------------
// Priority Queue.
//------------------------------------------------------------------------------

  TmcmPriorityQueue = class
    private
      FExternal : boolean;
      FList     : TList;
      FCompare  : TmcmPriorityCompare;
    protected
      function    GetCount : integer; // Get number of items in list.
      procedure   SortExternalList;
      procedure   SortUpTree(FromIndex : integer; Item : pointer);
      procedure   SortDownTree(FromIndex : integer; Item : pointer);
    public
      constructor Create(ACompare : TmcmPriorityCompare);
      constructor CreateList(ACompare : TmcmPriorityCompare;
                             AList    : TList);
      destructor  Destroy; override;
      procedure   Add(Item : pointer);
      function    Remove : pointer;

      // properties.
      property    Count : integer
        read      GetCount;
      property    List : TList
        read      FList;
  end;

//------------------------------------------------------------------------------
// Extended Priority Queue.
//------------------------------------------------------------------------------

  TmcmPriorityQueueEx = class
    private
      FList    : TList;
      FHandles : pointer;
      FCompare : TmcmPriorityCompare;
    protected
      function    GetCount : integer; // Get number of items in list.
      procedure   SortUpTree(FromIndex : integer; Handle : pointer);
      procedure   SortDownTree(FromIndex : integer; Handle : pointer);
    public
      constructor Create(ACompare : TmcmPriorityCompare);
      destructor  Destroy; override;
      function    Add(Item : pointer) : TPriorityQueueHandle;
      procedure   Delete(var Handle : TPriorityQueueHandle);
      function    Remove : pointer;
      procedure   Replace(Handle : TPriorityQueueHandle; Item : pointer);

      // properties.
      property    Count : integer
        read      GetCount;
      property    List : TList
        read      FList;
  end;

implementation

//------------------------------------------------------------------------------
// Linked list routines
//------------------------------------------------------------------------------

type PLinkedNode = ^TLinkedNode;
     TLinkedNode = packed record
                   NextNode : PLinkedNode;
                   PrevNode : PLinkedNode;
                   Item     : pointer;
                   Index    : integer;
     end;


function CreateLinkedList : PLinkedNode;
begin
  Result := AllocMem(SizeOf(TLinkedNode));
  Result^.NextNode := AllocMem(SizeOf(TLinkedNode));
  Result^.NextNode^.PrevNode := Result;
end; // CreateLinkedList.


procedure DestroyLinkedList(LinkedList : PLinkedNode);
var TempNode : PLinkedNode;
begin
  while (LinkedList <> Nil)
  do begin
     TempNode := LinkedList;
     LinkedList := LinkedList^.NextNode;
     FreeMem(TempNode, SizeOf(TLinkedNode));
  end;
end; // DestroyLinkedList.


function AddLinkedListNode(LinkedList : PLinkedNode; Item : pointer) : PLinkedNode;
begin
  Result := AllocMem(SizeOf(TLinkedNode));
  Result^.NextNode := LinkedList^.NextNode;
  Result^.PrevNode := LinkedList;
  LinkedList^.NextNode^.PrevNode := Result;
  LinkedList^.NextNode := Result;
  Result^.Item := Item;
end; // AddLinkedListNode.


procedure DeleteLinkedListNode(Node : PLinkedNode);
begin
  Node^.PrevNode^.NextNode := Node^.NextNode;
  Node^.NextNode^.PrevNode := Node^.PrevNode;
  FreeMem(Node, SizeOf(TLinkedNode));
end; // DeleteLinkedListNode.


//------------------------------------------------------------------------------
// Priority Queue.
//------------------------------------------------------------------------------

constructor TmcmPriorityQueue.Create(ACompare : TmcmPriorityCompare);
begin
  Inherited Create;
  FCompare  := ACompare;
  FList     := TList.Create;
  FExternal := False;
end; // TmcmPriorityQueue.Create.


constructor TmcmPriorityQueue.CreateList(ACompare : TmcmPriorityCompare;
                                         AList    : TList);
begin
  Inherited Create;
  FCompare  := ACompare;
  FList     := AList;
  FExternal := True;
  SortExternalList;
end; // TmcmPriorityQueue.CreateList.


destructor TmcmPriorityQueue.Destroy;
begin
  // Dispose of the priority queue.
  // Items remaining are not freed from memory!

  if Not(FExternal)
  then FList.Free;
  Inherited Destroy;
end; // TmcmPriorityQueue.Destroy.


procedure TmcmPriorityQueue.SortExternalList;
var i : integer;
begin
  for i := ((FList.Count - 2) div 2) downto 0
  do SortDownTree(i, FList[i]);
end; // TmcmPriorityQueue.SortExternalList.


procedure TmcmPriorityQueue.SortUpTree(FromIndex : integer; Item : pointer);
var ParentIndex  : integer;
begin
  // While the Item under consideration has larger priority than its parent,
  // swap it with it's parent and continue from the new position.
  // Note: the parent for the child at index N is at (N - 1) div 2
  ParentIndex := (FromIndex - 1) div 2;

  if (FromIndex > 0)
  then begin
       // While item has a parent, and it has higher priority than the parent ..
       while (FromIndex > 0) and
             (FCompare(Item, FList[ParentIndex]) > 0)
       do begin
          // Move parent down the tree.
          FList[FromIndex] := FList[ParentIndex];
          FromIndex := ParentIndex;
          ParentIndex := (FromIndex - 1) div 2;
       end;
  end;

  // Store item at the correct position
  FList[FromIndex] := Item;
end; // TmcmPriorityQueue.SortUpTree.


procedure TmcmPriorityQueue.SortDownTree(FromIndex : integer; Item : pointer);
var ListCount     : integer;
    ChildIndex    : integer;
begin
  // while the item under consideration has lower priority than one of its
  // children, swap it with the higher priority child and continue from its new
  // position.
  // Note: children for the parent at index N are at (2 * N + 1) and 2 * N + 2
  ListCount := FList.Count;

  // Calculate left child index.
  ChildIndex := Succ(FromIndex * 2);

  // While there is at least a left child ..
  while (ChildIndex < ListCount)
  do begin
     // If there is a right child, calculate the index of the higher priority
     // child.
     if (Succ(ChildIndex) < ListCount) and
        (FCompare(FList[ChildIndex], FList[Succ(ChildIndex)]) < 0)
     then inc(ChildIndex);

     // If our item is greater or equal to the larger child, we're done}
     if (FCompare(Item, FList[ChildIndex]) >= 0)
     then Break;

     // Otherwise move the higher priority child up the tree, and move item
     // down the tree and continue.
     FList[FromIndex] := FList[ChildIndex];
     FromIndex := ChildIndex;
     ChildIndex := Succ(FromIndex * 2);
  end;

  // Store item in the correct position.
  FList[FromIndex] := Item;
end; // TmcmPriorityQueue.SortDownTree.


function TmcmPriorityQueue.GetCount : integer;
begin
  Result := FList.Count;
end; // TmcmPriorityQueue.GetCount.


procedure TmcmPriorityQueue.Add(Item : pointer);
begin
  // Add an item to the priority queue, and return handle.

  // Increase list size
  FList.Count := FList.Count + 1;

  // Sort node upwards in the tree as far as it will go.
  SortUpTree(Pred(FList.Count), Item);
end; // TmcmPriorityQueue.Add.


function TmcmPriorityQueue.Remove : pointer;
begin
  // Remove and return the item with the highest priority.

  // Return the item at the root.
  try
    Result := FList[0];

    // Replace the root with the child at the lowest rightmost position,
    // and shrink the list.
    FList[0] := FList.Last;
    FList.Count := FList.Count - 1;

    // Now trickle down the root item as far as it will go.
    if (FList.Count > 0)
    then SortDownTree(0, FList[0]);
  except
    Result := Nil;
  end;
end; // TmcmPriorityQueue.Remove.


//------------------------------------------------------------------------------
// Extended Priority Queue.
//------------------------------------------------------------------------------

constructor TmcmPriorityQueueEx.Create(ACompare : TmcmPriorityCompare);
begin
  Inherited Create;
  FCompare := ACompare;
  FList    := TList.Create;
  FHandles := CreateLinkedList;
end; // TmcmPriorityQueueEx.Create.


destructor TmcmPriorityQueueEx.Destroy;
begin
  // Dispose of the priority queue.
  // Items remaining are not freed from memory!

  FList.Free;
  DestroyLinkedList(FHandles);
  Inherited Destroy;
end; // TmcmPriorityQueueEx.Destroy.


procedure TmcmPriorityQueueEx.SortUpTree(FromIndex : integer; Handle : pointer);
var ParentIndex  : integer;
    ParentHandle : PLinkedNode;
    MovingHandle : PLinkedNode absolute Handle;
begin
  // While the handle under consideration has larger priority than its parent,
  // swap it with it's parent and continue from the new position.
  // Note: the parent for the child at index N is at (N - 1) div 2
  if (FromIndex > 0)
  then begin
       ParentIndex := (FromIndex - 1) div 2;
       ParentHandle := PLinkedNode(FList[ParentIndex]);

       // While item has a parent, and it has higher priority than the parent ..
       while (FromIndex > 0) and
             (FCompare(MovingHandle^.Item, ParentHandle^.Item) > 0)
       do begin
          // Move parent down the tree.
          FList[FromIndex] := ParentHandle;
          ParentHandle^.Index := FromIndex;
          FromIndex := ParentIndex;
          ParentIndex := (FromIndex - 1) div 2;
          ParentHandle := PLinkedNode(FList[ParentIndex]);
       end;
  end;

  // Store item at the correct position
  FList[FromIndex] := MovingHandle;
  MovingHandle^.Index := FromIndex;
end; // TmcmPriorityQueueEx.SortUpTree.


procedure TmcmPriorityQueueEx.SortDownTree(FromIndex : integer; Handle : pointer);
var ListCount     : integer;
    ChildIndex    : integer;
    ChildHandle   : PLinkedNode;
    MovingHandle  : PLinkedNode absolute Handle;
begin
  // while the item under consideration has lower priority than one of its
  // children, swap it with the higher priority child and continue from its new
  // position.
  // Note: children for the parent at index N are at (2 * N + 1) and 2 * N + 2
  ListCount := FList.Count;

  // Calculate left child index.
  ChildIndex := Succ(FromIndex * 2);

  // While there is at least a left child ..
  while (ChildIndex < ListCount)
  do begin
     // If there is a right child, calculate the index of the higher priority
     // child.
     if (succ(ChildIndex) < ListCount) and
        (FCompare(PLinkedNode(FList[ChildIndex])^.Item,
                   PLinkedNode(FList[succ(ChildIndex)])^.Item) < 0)
     then inc(ChildIndex);

     // If our item is greater or equal to the larger child, we're done}
     ChildHandle := PLinkedNode(FList[ChildIndex]);
     if (FCompare(MovingHandle^.Item, ChildHandle^.Item) >= 0)
     then Break;

     // Otherwise move the higher priority child up the tree, and move item
     // down the tree and continue.
     FList[FromIndex] := ChildHandle;
     ChildHandle^.Index := FromIndex;
     FromIndex := ChildIndex;
     ChildIndex := succ(FromIndex * 2);
  end;

  // Store item in the correct position.
  FList[FromIndex] := MovingHandle;
  MovingHandle^.Index := FromIndex;
end; // TmcmPriorityQueueEx.SortDownTree.


function TmcmPriorityQueueEx.GetCount : integer;
begin
  Result := FList.Count;
end; // TmcmPriorityQueueEx.GetCount.


function TmcmPriorityQueueEx.Add(Item : pointer) : TPriorityQueueHandle;
var Handle : PLinkedNode;
begin
  // Add an item to the priority queue, and return handle.

  // Increase list size
  FList.Count := FList.Count + 1;

  // Create new node
  Handle := AddLinkedListNode(FHandles, Item);

  // Sort node upwards in the tree as far as it will go.
  if (FList.Count = 1)
  then begin
       FList[0] := Handle;
       Handle^.Index := 0;
  end
  else SortUpTree(Pred(FList.Count), Handle);

  Result := Handle;
end; // TmcmPriorityQueueEx.Add.


procedure TmcmPriorityQueueEx.Delete(var Handle : TPriorityQueueHandle);
var DeleteHandle : PLinkedNode absolute Handle;
    NewHandle    : PLinkedNode;
    HeapIndex    : integer;
    ParentIndex  : integer;
    ParentHandle : PLinkedNode;
begin
  // Delete an item referenced by its handle from the priority queue.
  // The handle is set to Nil on return.

  // Delete Handle
  HeapIndex := DeleteHandle^.Index;
  DeleteLinkedListNode(DeleteHandle);
  DeleteHandle := Nil;

  // Check if the deleted item was the last, if so - shrink heap.
  if (HeapIndex = Pred(FList.Count))
  then FList.Count := FList.Count - 1
  else begin
       // Replace the heap element with the child at the lowest, rightmost
       // position, then shrink list.
       NewHandle := FList.Last;
       FList[HeapIndex] := NewHandle;
       NewHandle^.Index := HeapIndex;
       FList.Count := FList.Count - 1;

       // Check whether the node can be moved up through the tree.
       if (HeapIndex > 0)
       then begin
            ParentIndex := (HeapIndex - 1) div 2;
            ParentHandle := PLinkedNode(FList[ParentIndex]);
            if (FCompare(NewHandle^.Item, ParentHandle^.Item) > 0)
            then begin
                 SortUpTree(HeapIndex, NewHandle);
                 Exit;
            end;
       end;

       // Otherwise trickle.
       if (FList.Count > 0)
       then SortDownTree(HeapIndex, FList[HeapIndex]);
  end;
end; // TmcmPriorityQueueEx.Delete.


function TmcmPriorityQueueEx.Remove : pointer;
var Handle : PLinkedNode;
begin
  // Remove and return the item with the highest priority.

  // Return the item at the root.
  Handle := FList[0];
  Result := Handle^.Item;
  DeleteLinkedListNode(Handle);

  // Replace the root with the child at the lowest rightmost position,
  // and shrink the list.
  Handle := FList.Last;
  FList[0] := Handle;
  Handle^.Index := 0;
  FList.Count := FList.Count - 1;

  // Now trickle down the root item as far as it will go.
  if (FList.Count > 0)
  then SortDownTree(0, Handle);
end; // TmcmPriorityQueueEx.Remove.


procedure TmcmPriorityQueueEx.Replace(Handle : TPriorityQueueHandle; Item : pointer);
var ReplaceHandle : PLinkedNode absolute Handle;
    ParentIndex   : integer;
    ParentHandle  : PLinkedNode;
begin
  // Replace the item referenced by the handle in the priority queue.

  // Replace item.
  ReplaceHandle^.Item := Item;

  // Check whether we can SortUpTree up.
  if (ReplaceHandle^.Index > 0)
  then begin
       ParentIndex := (ReplaceHandle^.Index - 1) div 2;
       ParentHandle := PLinkedNode(FList[ParentIndex]);
       if (FCompare(ReplaceHandle^.Item, ParentHandle^.Item) > 0)
       then begin
            SortUpTree(ReplaceHandle^.Index, ReplaceHandle);
            Exit;
       end;
  end;

  // Otherwise trickle down
  SortDownTree(ReplaceHandle^.Index, ReplaceHandle);
end; // TmcmPriorityQueueEx.Replace.


end.
