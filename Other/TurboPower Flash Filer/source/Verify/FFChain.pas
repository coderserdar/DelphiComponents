{*********************************************************}
{* FlashFiler: Chain manager                             *}
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

{ TODO::
  - Implement adding of block
  - Implement review of chains
}

unit FFChain;

interface

uses
  Classes,
  FFLLBase;

type
  TffChainMgr = class;  { forward declaration }
  TffChainItem = class; { forward declaration }

  TffRefMode = (rmNext, rmPrev, rmBoth);

  TffChain = class
  protected
    FOwner : TffChainMgr;
  public
    PrevChain : TffChain;
    NextChain : TffChain;
    HeadItem : TffChainItem;
    TailItem : TffChainItem;

    constructor Create(Owner : TffChainMgr);
    destructor Destroy; override;

    procedure AppendItem(NewItem : TffChainItem);
      { Append the specified item to the chain. }

    function FindItem(const ThisBlock : TffWord32) : TffChainItem;
      { Find an item with the given block number. }

    function FindItemPointingTo(const ThisBlock : TffWord32;
                                const Mode : TffRefMode) : TffChainItem;
      { Find an item pointing to the specified block number. }

    procedure InsertHead(NewHead : TffChainItem);
      { Insert a new head item into the chain. }

    procedure RemoveItem(Item : TffChainItem);
      { Remove the specified item from the chain. }

  end;

  TffChainItem = class
  protected
    FOwner : TffChain;
  public
    NextItem,
    PrevItem  : TffChainItem;
    ThisBlock : TffWord32;
    NextBlock : TffWord32;
    PrevBlock : TffWord32;

    constructor Create(Owner : TffChain);
    destructor Destroy; override;
  end;

  TffLinkCallback = procedure(const Block1Num, Block2Num : TffWord32) of object;
    { Called when two blocks are linked together. }

  TffMoveCallback = procedure(const BlockMoved, PrevBlock : TffWord32) of object;
    { Called when an orphan is moved to the end of the chain. }

  TffChainMgr = class
  protected
    FPopulated : Boolean;
    OrphanChain : TffChain;
    HeadChain : TffChain;
    TailChain : TffChain;

    procedure AppendChain(NewChain : TffChain);
    function GetHasOrphans : Boolean;
    function GetHasValidChain : Boolean;
    function GetLastBlockNumber : TffWord32;
    function GetLastNextBlockNumber : TffWord32;
    procedure RemoveReference(const BlockNum : TffWord32;
                                    Item : TffChainItem;
                              const AdjustChain : Boolean);

  public
    constructor Create;
    destructor Destroy; override;
    procedure AddBlock(const ThisBlock, NextBlock, PrevBlock : TffWord32);

    procedure Clear;
      { Removes the current chains from the chain manager. }

    function Describe : TStringList;
      { Returns a string list describing the chains. }

    function FindItem(const BlockNum : TffWord32;
                        var PrevBlock, NextBlock : TffWord32) : Boolean;
      { Use this method to determine if a block is listed in the chain manager.
        If it is not listed, this function returns False. Otherwise, this
        function returns True. It fills PrevBlock with the block number of
        the previous block in the chain (or ffc_W32NoValue if there is no
        previous block) and fills NextBlock with the block number of the next
        block (or ffc_W32NoValue for no next block). }

    procedure Fixup;
      { If there is only 1 block in the orphan chain & no blocks in other chains
        then we have the case where there is only 1 free block or 1 data block
        in the table. Move the orphan to its own chain. }

    procedure LinkChains(CallBack : TffLinkCallback);
      { Use this method to have the chain manager link together all of its
        chains. Does not affect the orphan chain. }

    procedure MoveOrphansToTail(Callback : TffMoveCallBack);
      { Use this method to have the chain manager append all of the orphans
        in the orphan chain to the last chain. }

    function Referenced(const BlockNum : TffWord32;
                        const RemoveRef : Boolean;
                          var ReferencingBlockNum : TffWord32) : Boolean;
      { Returns True if the specified BlockNum is referenced as a Prev or Next
        block in the chain manager. If it is referenced then this function
        returns True and places the block number of the referencing block in
        the ReferencingBlockNum param. If the RemoveRef parameter is set to True
        then the reference to the block number in the chain manager is set to
        the value ffc_W32NoValue. }

    property HasOrphans : Boolean
      read GetHasOrphans;

    property HasValidChain : Boolean
      read GetHasValidChain;

    property LastBlockNumber : TffWord32
      read GetLastBlockNumber;
      { Returns the block number of the last block. }

    property LastBlockNextBlockNumber : TffWord32
      read GetLastNextBlockNumber;
      { Returns the next block number of the last block in the chain. }

    property Populated : Boolean
      read FPopulated write FPopulated;
      { Returns True if the chain manager has been fully populated with data. }

  end;

implementation

uses
  SysUtils;

{===TffChainMgr======================================================}
constructor TffChainMgr.Create;
begin
  inherited;
  FPopulated := False;
end;
{--------}
destructor TffChainMgr.Destroy;
begin
  Clear;
  inherited;
end;
{--------}
procedure TffChainMgr.AddBlock(const ThisBlock, NextBlock, PrevBlock : TffWord32);
var
  Item,
  OrphanItem : TffChainItem;
  Chain,
  NewChain : TffChain;
begin
  { Create an item for the block. }
  Item := TffChainItem.Create(nil);
  Item.ThisBlock := ThisBlock;
  Item.NextBlock := NextBlock;
  Item.PrevBlock := PrevBlock;

  { Step 1: Does this block point to an orphan?  If so then grab the orphan.
    We may be able to move the new block and the orphan to an existing chain
    or at least start a new chain.}
  OrphanItem := nil;
  if OrphanChain <> nil then begin
    OrphanItem := OrphanChain.FindItem(NextBlock);
    { If found an orphan then remove it from the orphan chain. }
    if Assigned(OrphanItem) then
      OrphanChain.RemoveItem(OrphanItem);
  end;  { if }

  { Step 2: If this block didn't point to an orphan, see if an orphan points
    to this block. }
  if (OrphanItem = nil) and (OrphanChain <> nil) then begin
    OrphanItem := OrphanChain.FindItemPointingTo(ThisBlock, rmNext);
    if Assigned(OrphanItem) then begin
      { Remove the orphan from the orphan chain. }
      OrphanChain.RemoveItem(OrphanItem);

      { Start a new chain. }
      NewChain := TffChain.Create(Self);
      AppendChain(NewChain);

      { Add the orphan to the new chain. }
      NewChain.AppendItem(OrphanItem);

      { Add the new chain item to the new chain. }
      NewChain.AppendItem(Item);

      Exit;
    end;  { if }
  end;  { if }

  { Step 3 : If the block does not point to an orphan, does it point to the
   head of an existing chain?  If so then add it to the beginning of that
   chain. }
  if OrphanItem = nil then begin
    Chain := HeadChain;
    while Assigned(Chain) and (Chain.HeadItem.ThisBlock <> NextBlock) do
      Chain := Chain.NextChain;
    if Assigned(Chain) then begin
      Chain.InsertHead(Item);
      Exit;
    end;  { if }
  end;

  { Step 4 : If the block does not point to a head of an existing chain, does
     the tail of an existing chain point to the block?  If so then add it to the
     end of that chain.  Bring along an orphan if one was pulled in Step 1. }
  Chain := HeadChain;
  while Assigned(Chain) and (Chain.TailItem.NextBlock <> ThisBlock) do
    Chain := Chain.NextChain;

  if Assigned(Chain) then begin
    Chain.AppendItem(Item);
    if Assigned(OrphanItem) then
      Chain.AppendItem(OrphanItem);
  end
  else begin
    { There are no chains where a tail points to this block. If found an
      associated orphan in Step 1 then start a new chain. Otherwise, add this
      block to the list of orphans. }
    if Assigned(OrphanItem) then begin
      { Start a new chain. }
      NewChain := TffChain.Create(Self);
      AppendChain(NewChain);

      { Add the new chain item to the new chain. }
      NewChain.AppendItem(Item);

      { Add the orphan to the new chain. }
      NewChain.AppendItem(OrphanItem);
    end
    else begin
      if OrphanChain = nil then
        OrphanChain := TffChain.Create(Self);
      OrphanChain.AppendItem(Item);
    end;  { if..else }
  end;
end;
{--------}
procedure TffChainMgr.AppendChain(NewChain : TffChain);
begin
  if TailChain = nil then begin
    HeadChain := NewChain;
    TailChain := HeadChain;
  end
  else begin
    { Point the last chain to the new chain, and vice versa. }
    TailChain.NextChain := NewChain;
    NewChain.PrevChain := TailChain;
    TailChain := NewChain;
  end;
end;
{--------}
procedure TffChainMgr.Clear;
var
  Chain,
  NextChain : TffChain;
begin
  OrphanChain.Free;
  OrphanChain := nil;

  Chain := HeadChain;
  while Chain <> nil do begin
    NextChain := Chain.NextChain;
    Chain.Free;
    Chain := NextChain;
  end;  { while }
  HeadChain := nil;
  TailChain := nil;
end;
{--------}
function TffChainMgr.Describe : TStringList;
var
  Chain : TffChain;
  Item : TffChainItem;
  Inx,
  Count : Integer;
begin
  Result := TStringList.Create;
  try
    { Orphaned blocks }
    if (OrphanChain <> nil) and (OrphanChain.HeadItem <> nil) then begin
      Result.Add('Orphaned blocks:');
      Item := OrphanChain.HeadItem;
      while Item <> nil do begin
        Result.Add(Format('Block: %d, next block: %d, prev block: %d',
                          [Item.ThisBlock, Item.NextBlock, Item.PrevBlock]));
        Item := Item.NextItem;
      end;  { while }
    end
    else
      Result.Add('No orphaned blocks');

    { Other blocks. First, count the number of chains. }
    Count := 0;
    Chain := HeadChain;
    while Chain <> nil do begin
      inc(Count);
      Chain := Chain.NextChain;
    end;  { while }

    { Now step through the chains. }
    Result.Add('');
    if Count = 0 then
      Result.Add('No chains')
    else begin
      Chain := HeadChain;
      Inx := 0;
      while Chain <> nil do begin
        inc(Inx);
        Result.Add(Format('Chain %d of %d', [Inx, Count]));
        { Display information about the first block & the last block in the
          chain. }
        Item := Chain.HeadItem;
        if (Item <> nil) then begin
          if (Chain.HeadItem = Chain.TailItem) then begin
            Result.Add(Format('There is 1 block in this chain, block: %d, ' +
                              'next block: %d, prev Block: %d',
                              [Item.ThisBlock, Item.NextBlock, Item.PrevBlock]));
          end
          else begin
            Result.Add(Format('Head, block: %d, next block: %d, prev block: %d',
                              [Item.ThisBlock, Item.NextBlock, Item.PrevBlock]));
            Item := Chain.TailItem;
            Result.Add(Format('Tail, block: %d, next block: %d, prev block: %d',
                              [Item.ThisBlock, Item.NextBlock, Item.PrevBlock]));
          end;
        end;  { if }

        Chain := Chain.NextChain;
      end;  { while }
    end;

  except
    Result.Free;
    raise;
  end;
end;
{--------}
function TffChainMgr.FindItem(const BlockNum : TffWord32;
                                var PrevBlock, NextBlock : TffWord32) : Boolean;
var
  Item : TffChainItem;
  Chain : TffChain;
begin
  Result := False;
  PrevBlock := ffc_W32NoValue;
  NextBlock := ffc_W32NoValue;

  { Look in the orphans first. }
  Item := OrphanChain.FindItem(BlockNum);
  if Item = nil then begin
    { Not an orphan. Look in the other chains. }
    Chain := HeadChain;
    while (Chain <> nil) do begin
      Item := Chain.FindItem(BlockNum);
      if Item <> nil then begin
        Result := True;
        PrevBlock := Item.PrevBlock;
        NextBlock := Item.NextBlock;
        Break;
      end;  { if }
      Chain := Chain.NextChain;
    end;
  end
  else
    Result := True;
end;
{--------}
procedure TffChainMgr.Fixup;
var
  Item : TffChainItem;
  Chain : TffChain;
begin
  { If the orphan chain contains only 1 block & there are no other chains
    being managed then we have a valid chain with one block. Move the block
    from the orphan chain to a new chain. }
  if Assigned(OrphanChain) and
     Assigned(OrphanChain.HeadItem) and
     (OrphanChain.HeadItem = OrphanChain.TailItem) and
     (HeadChain = nil) then begin

    Item := OrphanChain.HeadItem;
    OrphanChain.RemoveItem(Item);

    Chain := TffChain.Create(Self);
    AppendChain(Chain);
    Chain.AppendItem(Item);
  end;  { if }
end;
{--------}
function TffChainMgr.GetHasOrphans : Boolean;
begin
  Result := (OrphanChain <> nil) and (OrphanChain.HeadItem <> nil);
end;
{--------}
function TffChainMgr.GetHasValidChain : Boolean;
begin
  { The chain is valid if the following conditions are met:
    There are no orphans
      - AND either of the following -
    1. There are no data blocks
       - OR -
    2. There is only 1 chain in the chain manager.
  }
  Result := (not GetHasOrphans) and
            (
              (HeadChain = nil) or
              
              ((HeadChain.HeadItem <> nil) and
               (HeadChain = TailChain)
              )
            );
end;
{--------}
function TffChainMgr.GetLastBlockNumber : TffWord32;
begin
  if Assigned(TailChain) and
     Assigned(TailChain.TailItem) then
    Result := TailChain.TailItem.ThisBlock
  else
    Result := ffc_W32NoValue;
end;
{--------}
function TffChainMgr.GetLastNextBlockNumber : TffWord32;
begin
  if Assigned(TailChain) and
     Assigned(TailChain.TailItem) then
    Result := TailChain.TailItem.NextBlock
  else
    Result := ffc_W32NoValue;
end;
{--------}
function TffChainMgr.Referenced(const BlockNum : TffWord32;
                                const RemoveRef : Boolean;
                                  var ReferencingBlockNum : TffWord32) : Boolean;
var
  Item : TffChainItem;
  Chain : TffChain;
begin
  Result := False;
  ReferencingBlockNum := ffc_W32NoValue;

  { Search the orphan chain. }
  if OrphanChain <> nil then begin
    Item := OrphanChain.FindItemPointingTo(BlockNum, rmBoth);
    if Item <> nil then begin
      Result := True;
      ReferencingBlockNum := Item.ThisBlock;
      if RemoveRef then
        RemoveReference(BlockNum, Item, False);
    end;  { if }
  end;  { if }

  if not Result then begin
    Chain := HeadChain;
    while Chain <> nil do begin
      Item := Chain.FindItemPointingTo(BlockNum, rmBoth);
      if Item <> nil then begin
        Result := True;
        ReferencingBlockNum := Item.ThisBlock;
        if RemoveRef then
          RemoveReference(BlockNum, Item, True);
        Break;
      end
      else
        Chain := Chain.NextChain;
    end;  { while }
  end;  { if..else }  
end;
{--------}
procedure TffChainMgr.LinkChains(CallBack : TffLinkCallback);
var
  NextChain,
  Chain : TffChain;
  Block1Num,
  Block2Num : TffWord32;
begin
  if HeadChain <> nil then begin
    Chain := HeadChain.NextChain;
    while Chain <> nil do begin
      NextChain := Chain.NextChain;
      Block1Num := HeadChain.TailItem.ThisBlock;
      Block2Num := Chain.HeadItem.ThisBlock;

      { Connect the last item in the head chain to the first item in the current
        chain. }
      HeadChain.TailItem.NextItem := Chain.HeadItem;
      HeadChain.TailItem.NextBlock := Chain.HeadItem.ThisBlock;

      { Point the first item in the current chain back to the head chain's tail
        item. }
      Chain.HeadItem.PrevItem := HeadChain.TailItem;
      Chain.HeadItem.PrevBlock := HeadChain.TailItem.ThisBlock;

      { Update the head chain's tail item. }
      HeadChain.TailItem := Chain.TailItem;

      if Assigned(CallBack) then
        CallBack(Block1Num, Block2Num);

      { Remove all associations the current chain has with its items. }
      Chain.HeadItem := nil;
      Chain.TailItem := nil;

      { Free the chain. }
      Chain.Free;

      { Move to the next chain. }
      Chain := NextChain;
    end;

    { There should be no more chains after the head chain. }
    HeadChain.NextChain := nil;
    TailChain := HeadChain;
  end;  { if }
end;
{--------}
procedure TffChainMgr.MoveOrphansToTail(Callback : TffMoveCallBack);
var
  BlockNum, PrevBlock : TffWord32;
  NextItem,
  Item : TffChainItem;
begin
  Item := OrphanChain.TailItem;
  while Item <> nil do begin
    NextItem := Item.NextItem;
    BlockNum := Item.ThisBlock;
    PrevBlock := TailChain.TailItem.ThisBlock;
    OrphanChain.RemoveItem(Item);
    TailChain.AppendItem(Item);
    if Assigned(Callback) then
      Callback(BlockNum, PrevBlock);
    Item := NextItem;
  end;  { while }
end;
{--------}
procedure TffChainMgr.RemoveReference(const BlockNum : TffWord32;
                                            Item : TffChainItem;
                                      const AdjustChain : Boolean);
begin
  if Item.PrevBlock = BlockNum then begin
    if AdjustChain and (Item.PrevItem <> nil) then begin
      Assert(false, 'Unhandled case. Please report to FlashFiler team.');
    end;
    Item.PrevBlock := ffc_W32NoValue;
  end
  else begin
    if AdjustChain and (Item.NextItem <> nil) then begin
      Assert(false, 'Unhandled case. Please report to FlashFiler team.');
    end;
    Item.NextBlock := ffc_W32NoValue;
  end;
end;
{====================================================================}

{===TffChain=========================================================}
constructor TffChain.Create(Owner : TffChainMgr);
begin
  inherited Create;
  FOwner := Owner;
end;
{--------}
destructor TffChain.Destroy;
var
  Item,
  NextItem : TffChainItem;
begin
  inherited;
  Item := HeadItem;
  while Item <> nil do begin
    NextItem := Item.NextItem;
    Item.Free;
    Item := NextItem;
  end;  { while }
end;
{--------}
procedure TffChain.AppendItem(NewItem : TffChainItem);
begin
  { If no tail then this chain is empty. }
  if TailItem = nil then begin
    HeadItem := NewItem;
    TailItem := NewItem;
  end
  else begin
    { Otherwise, append the item to the tail. }
    TailItem.NextItem := NewItem;
    NewItem.PrevItem := TailItem;
    TailItem := NewItem;
  end;
  NewItem.FOwner := Self;
end;
{--------}
function TffChain.FindItem(const ThisBlock : TffWord32) : TffChainItem;
begin
  Result := HeadItem;
  while (Result <> nil) and (Result.ThisBlock <> ThisBlock) do
    Result := Result.NextItem;
end;
{--------}
function TffChain.FindItemPointingTo(const ThisBlock : TffWord32;
                                     const Mode : TffRefMode) : TffChainItem;
begin
  Result := HeadItem;
  case Mode of
    rmNext :
      while (Result <> nil) and (Result.NextBlock <> ThisBlock) do
        Result := Result.NextItem;
    rmPrev :
      while (Result <> nil) and (Result.PrevBlock <> ThisBlock) do
        Result := Result.NextItem;
    rmBoth :
      while (Result <> nil) and (Result.NextBlock <> ThisBlock) and
            (Result.PrevBlock <> ThisBlock) do
        Result := Result.NextItem;
  end;  { case }
end;
{--------}
procedure TffChain.InsertHead(NewHead : TffChainItem);
begin
  if HeadItem = nil then begin
    HeadItem := NewHead;
    TailItem := NewHead;
  end
  else begin
    { Point the head to the new head, and vice versa. }
    HeadItem.PrevItem := NewHead;
    NewHead.NextItem := HeadItem;
    HeadItem := NewHead;
  end;
end;
{--------}
procedure TffChain.RemoveItem(Item : TffChainItem);
var
  CurItem : TffChainItem;
begin
  { If this is the head item then the next item is the new head. }
  if Item = HeadItem then begin
    HeadItem := Item.NextItem;
    { If there is a new head then set its prevItem to nil. }
    if Assigned(HeadItem) then
      HeadItem.PrevItem := nil
    else
      { Otherwise the chain is empty so set the tail to nil. }
      TailItem := nil;
  end
  { If this is not the head but it is the tail then the previous item is the
    new tail. }
  else if Item = TailItem then begin
    TailItem := Item.PrevItem;
    { If there is a new tail then set its NextItem to nil. }
    if Assigned(TailItem) then
      TailItem.NextItem := nil
    else
      { Otherwise the chain is empty so set the head to nil. }
      HeadItem := nil;
  end
  else begin
    { This item is somewhere between the head & tail. Scan for it. }
    CurItem := HeadItem;
    while CurItem <> Item do
      CurItem := CurItem.NextItem;
    if Assigned(CurItem) then begin
      { Point the previous item to the next item. }
      CurItem.PrevItem.NextItem := CurItem.NextItem;
      { Point the next item to the previous item. }
      CurItem.NextItem.PrevItem := CurItem.PrevItem;
    end;  { if }
  end;

  { Nil out the item's pointers. }
  Item.NextItem := nil;
  Item.PrevItem := nil;
  Item.FOwner := nil;
end;
{====================================================================}

{===TffChainItem=====================================================}
constructor TffChainItem.Create(Owner : TffChain);
begin
  inherited Create;
  FOwner := Owner;
end;
{--------}
destructor TffChainItem.Destroy;
begin
  inherited;
  { TODO }
end;
{====================================================================}

end.
