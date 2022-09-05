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
// $Log:  17539: mcmHashTable.pas 
//
//    Rev 1.5    2014-02-02 21:09:56  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.4    26-08-2009 23:03:18  mcm    Version: IMG 3.2
// Fixed unicode issues (PChar -> PAnsiChar)
//
//   Rev 1.3    29-09-2003 18:44:34  mcm    Version: IMG 1.6
// Added option to disable Range check.

//
//   Rev 1.2    25-07-2003 00:05:00  mcm
// Modified for use with LZW compression.

//
//   Rev 1.1    27-01-2003 13:52:06  mcm

//
//   Rev 1.0    27-05-2002 16:21:58  mcm

unit mcmHashTable;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Classes;
     {$ELSE}
      WinApi.Windows, System.Classes;
     {$ENDIF}


{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}
{$DEFINE USENODEMANAGER}

const
  CPageNodeCount = 100;

type
  THashKey = packed record {a 3-character signature string}
             case boolean of
             False : (AsLongint : longint);
             True  : (AsString  : array[0..2] of AnsiChar);
             end;

  PHashNode = ^THashNode;
  THashNode = packed record
              Next   : PHashNode;
              Key    : THashKey;
              Offset : longint;
              Code   : word;
              Len    : word;
              end;

  PHashNodePage = ^THashNodePage;
  THashNodePage = packed record
                  Next  : PHashNodePage;
                  Nodes : array[0..Pred(CPageNodeCount)] of THashNode;
                  end;

  THashKeyEnumProc = procedure(ExtraData : pointer; const Key : THashKey; Offset, Len, Code : longint) of object;

  TmcmHashTable = class(TPersistent)
  private
    FTableSize    : longint;
    FHashList     : TList;

    FFreeList     : PHashNode;
    FPageList     : PHashNodePage;
  protected
    procedure   AllocPage;
    procedure   PageFreeNode(Node : PHashNode);
    function    PageAllocNode : PHashNode;

    procedure   FreeChain(FromNode : PHashNode; IsParent : boolean);
  public
    constructor Create(TableSize : longint);
    destructor  Destroy; override;
    procedure   Clear;
    function    FindAll(const NewKey    : THashKey;
                              CutOffset : longint;
                              Action    : THashKeyEnumProc;
                              ExtraData : pointer) : boolean;
    procedure   Insert(const NewKey    : THashKey;
                             Offset    : longint;
                             Len       : word;
                             Code      : word);
  published
  end;

implementation


constructor TmcmHashTable.Create(TableSize : longint);
begin
  Inherited Create;
  FFreeList := Nil;
  FPageList := Nil;
  FTableSize := TableSize;
  FHashList  := TList.Create;
  FHashList.Count := FTableSize;
end; // TmcmHashTable.Create.


destructor TmcmHashTable.Destroy;
var TempPage : PHashNodePage;
begin
  if Assigned(FHashList)
  then begin
       Clear;
       FHashList.Free;
  end;

  // Destroy all the single node pages.
  TempPage := FPageList;
  while (TempPage <> Nil)
  do begin
     FPageList := TempPage^.Next;
     Dispose(TempPage);
     TempPage := FPageList;
  end;

  Inherited Destroy;
end; // TmcmHashTable.Destroy.


procedure TmcmHashTable.PageFreeNode(Node : PHashNode);
begin
  {$IFDEF USENODEMANAGER}
    // Add the node to the top of the free list
    Node^.Next := FFreeList;
    FFreeList := Node;
  {$ELSE}
    Dispose(Node);
  {$ENDIF}
end; // TmcmHashTable.PageFreeNode.


procedure TmcmHashTable.AllocPage;
var NewPage : PHashNodePage;
    i       : integer;
begin
  // Alloc a new page.
  New(NewPage);

  // Add it to the current list of pages.
  NewPage^.Next := FPageList;
  FPageList := NewPage;

  // Add all the nodes on the page to the free list.
  for i := 0 to Pred(CPageNodeCount)
  do PageFreeNode(@NewPage^.Nodes[i]);
end; // TmcmHashTable.AllocPage.


function TmcmHashTable.PageAllocNode : PHashNode;
begin
  {$IFDEF USENODEMANAGER}
    // If the free list is empty, allocate a new page of nodes.
    if (FFreeList = Nil)
    then AllocPage;

    // Return the first node on the free list.
    Result := FFreeList;
    FFreeList := Result^.Next;
  {$ELSE}
    New(Result);
  {$ENDIF}
end; // TmcmHashTable.PageAllocNode.


procedure TmcmHashTable.FreeChain(FromNode : PHashNode; IsParent : boolean);
var TmpNode : PHashNode;
begin
  if IsParent
  then begin
       TmpNode := FromNode^.Next;
       FromNode^.Next := Nil;
  end
  else TmpNode := FromNode;

  while (TmpNode <> Nil)
  do begin
     FromNode := TmpNode^.Next;
     PageFreeNode(TmpNode);
     TmpNode := FromNode;
  end;
end; // TmcmHashTable.FreeChain.


procedure TmcmHashTable.Clear;
var Index     : integer;
    ChainHead : PHashNode;
begin
  for Index := 0 to Pred(FTableSize)
  do begin
     ChainHead := PHashNode(FHashList[Index]);
     if (ChainHead <> Nil)
     then begin
          FreeChain(ChainHead, False);
          FHashList[Index] := Nil;
     end;
  end;
end; // TmcmHashTable.Clear.


function TmcmHashTable.FindAll(const NewKey    : THashKey;
                                     CutOffset : longint;
                                     Action    : THashKeyEnumProc;
                                     ExtraData : pointer) : boolean;
var Index      : integer;
    TmpNode    : PHashNode;
    ParentNode : PHashNode;
begin
  // Assume we wont find any.
  Result := False;

  // Calculate the hash table index for this key.
  Index := (NewKey.AsLongint shr 8) mod FTableSize;

  // Search the chain at this index.
  ParentNode := Nil;

  TmpNode := PHashNode(FHashList[Index]);
  while (TmpNode <> Nil)
  do begin
     // If this node has an offset that is less than the cutoff offset,
     // then remove the rest of this chain and exit.
     if (TmpNode^.Offset < CutOffset)
     then begin
          if (ParentNode = Nil)
          then begin
               FreeChain(TmpNode, False);
               FHashList[Index] := Nil;
          end
          else FreeChain(ParentNode, True);
          Exit;
     end;

     // If the node's key matches our key, call the action routine.
     if (TmpNode^.Key.AsLongint = NewKey.AsLongint)
     then begin
          Result := True;
          Action(ExtraData, NewKey, TmpNode^.Offset, TmpNode^.Len, TmpNode^.Code);
     end;

     // Go to the next node.
     ParentNode := TmpNode;
     TmpNode := ParentNode^.Next;
  end;
end; // TmcmHashTable.FindAll.


procedure TmcmHashTable.Insert(const NewKey    : THashKey;
                                     Offset    : longint;
                                     Len       : word;
                                     Code      : word);
var Index   : integer;
    NewNode : PHashNode;
begin
  // Calculate the hash table index for this key
  Index := (NewKey.AsLongint shr 8) mod FTableSize;

  // Allocate a new node and insert at the head of the chain at this
  // index in the hash table. This ensures that the nodes in the chain
  // are in reverse order of offset value
  NewNode := PageAllocNode;
  NewNode^.Key := NewKey;
  NewNode^.Offset := Offset;
  NewNode^.Len    := Len;
  NewNode^.Code   := Code;
  NewNode^.Next := FHashList[Index];
  FHashList[Index] := NewNode;
end; // TmcmHashTable.Insert.


{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.
