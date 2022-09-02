{*********************************************************}
{* FlashFiler: BLOB retrieval & verification routines    *}
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

unit ffsrblob;

interface

uses
  ffllbase,
  ffsrbase,
  ffsrlock;

{---Blob retrieval & verification routines---}
function ReadBLOBBlock(FI             : PffFileInfo;
                       TI             : PffTransInfo;
                 const anOffset       : TffInt64;
                   var aOffsetInBlock : TffWord32;                     {!!.11}
                   var aReleaseMethod : TffReleaseMethod)
                                      : PffBlock;
  { Reads the BLOB block containing the specified offset.  This method does
    not perform any locking so use it only when the block has previously
    been locked and the lock is still in effect. }
    
function ReadVfyBlobBlock(FI             : PffFileInfo;
                          TI             : PffTransInfo;
                    const aMarkDirty     : Boolean;
                    const anOffset       : TffInt64;
                      var aOffsetInBlock : TffWord32;                  {!!.11}
                      var aReleaseMethod : TffReleaseMethod)
                                         : PffBlock;

function ReadVfyBlobBlock2(FI             : PffFileInfo;
                           TI             : PffTransInfo;
                     const aMarkDirty     : Boolean;
                     const anOffset       : TffInt64;
                       var aBlockNum      : TffWord32;
                       var aOffsetInBlock : TffWord32;                 {!!.11}
                       var aReleaseMethod : TffReleaseMethod)
                                          : PffBlock;

function ReadVfyBlobBlock3(FI             : PffFileInfo;
                           TI             : PffTransInfo;
                     const aMarkDirty     : Boolean;
                     const anOffset       : TffInt64;
                       var aReleaseMethod : TffReleaseMethod)
                                          : PffBlock;

implementation

uses
  ffconst,
  ffllexcp,
  fftbbase;

{== Block verification routines ======================================}
function ReadBLOBBlock(FI             : PffFileInfo;
                       TI             : PffTransInfo;
                 const anOffset       : TffInt64;
                   var aOffsetInBlock : TffWord32;                     {!!.11}
                   var aReleaseMethod : TffReleaseMethod)
                                      : PffBlock;
var
  BlockNumber : TffWord32;
  BLOBBlock   : PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB absolute BLOBBlock;
  TempI64     : TffInt64;
begin
  { Assumptions: The block was previously read and successfully verified.
                 The block was previously locked and the lock is still in
                 effect. }
  with FI^ do begin

    { Get the BLOB block. }
    ffShiftI64R(anOffset, fiLog2BlockSize, TempI64);
    BlockNumber := TempI64.iLow;

    ffI64MinusInt(anOffset, (BlockNumber shl fiLog2BlockSize), TempI64);
    aOffsetInBlock := TempI64.iLow;
    BLOBBlock := FI^.fiBufMgr.GetBlock(FI, BlockNumber, TI, false, aReleaseMethod);
  end;
  Result := BLOBBlock;
end;
{--------}
function ReadVfyBlobBlock(FI             : PffFileInfo;
                          TI             : PffTransInfo;
                    const aMarkDirty     : boolean;
                    const anOffset       : TffInt64;
                      var aOffsetInBlock : TffWord32;                  {!!.11}
                      var aReleaseMethod : TffReleaseMethod)
                                         : PffBlock;
var
  BlockNumber : TffWord32;
  BLOBBlock   : PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB absolute BLOBBlock;
  TempI64     : TffInt64;
begin
  with FI^ do begin
    {verify the BLOB number}
    if not FFVerifyBLOBNr(anOffset, fiLog2BlockSize) then
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrBadBLOBNr, [FI^.fiName^, anOffset.iLow,
                                        anOffset.iHigh]);
    {now get the BLOB block}
    ffShiftI64R(anOffset, fiLog2BlockSize, TempI64);
    BlockNumber := TempI64.iLow;
    if (BlockNumber <= 0) or (BlockNumber >= fiUsedBlocks) then
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrBadBlockNr, [FI^.fiName^, BlockNumber]);
    ffI64MinusInt(anOffset, (BlockNumber shl fiLog2BlockSize), TempI64);
    aOffsetInBlock := TempI64.iLow;
    BLOBBlock := FFBMGetBlock(FI, TI, BlockNumber, aMarkDirty, aReleaseMethod);
    { Verify that it's a BLOB header block. }
    with BLOBBlockHdr^ do
      if (bhbSignature <> ffc_SigBLOBBlock) or
         (bhbThisBlock <> BlockNumber) then
        FFRaiseException(EffServerException, ffStrResServer,
                         fferrBadBLOBBlock, [FI^.fiName^, BlockNumber]);
  end;
  Result := BLOBBlock;
end;
{--------}
function ReadVfyBlobBlock2(FI             : PffFileInfo;
                           TI             : PffTransInfo;
                     const aMarkDirty     : boolean;
                     const anOffset       : TffInt64;
                       var aBlockNum      : TffWord32;
                       var aOffsetInBlock : TffWord32;                 {!!.11}
                       var aReleaseMethod : TffReleaseMethod)
                                          : PffBlock;
var
  BLOBBlock   : PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB absolute BLOBBlock;
  TempI64     : TffInt64;
begin
  with FI^ do begin
    {verify the BLOB number}
    if not FFVerifyBLOBNr(anOffset, fiLog2BlockSize) then
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrBadBLOBNr, [FI^.fiName^, anOffset.iLow,
                                        anOffset.iHigh]);
    {now get the BLOB block}
    ffShiftI64R(anOffset, fiLog2BlockSize, TempI64);
    aBlockNum := TempI64.iLow;
    if (aBlockNum <= 0) or (aBlockNum >= fiUsedBlocks) then
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrBadBlockNr, [FI^.fiName^, aBlockNum]);
    ffI64MinusInt(anOffset, (aBlockNum shl fiLog2BlockSize), TempI64);
    aOffsetInBlock := TempI64.iLow;
    BLOBBlock := FFBMGetBlock(FI, TI, aBlockNum, aMarkDirty, aReleaseMethod);
    {verify that it's a BLOB header block}
    with BLOBBlockHdr^ do
      if (bhbSignature <> ffc_SigBLOBBlock) or
         (bhbThisBlock <> aBlockNum) then
        FFRaiseException(EffServerException, ffStrResServer,
                         fferrBadBLOBBlock, [FI^.fiName^, aBlockNum]);
  end;
  Result := BLOBBlock;
end;
{--------}
function ReadVfyBlobBlock3(FI         : PffFileInfo;
                           TI         : PffTransInfo;
                     const aMarkDirty : boolean;
                     const anOffset   : TffInt64;
                       var aReleaseMethod : TffReleaseMethod) : PffBlock;
var
  BlockNumber : TffWord32;
  BLOBBlock   : PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB absolute BLOBBlock;
  TempI64     : TffInt64;
begin
  with FI^ do begin
    {verify the segment number}
    if not FFVerifyBLOBNr(anOffset, fiLog2BlockSize) then
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrBadBLOBSeg, [FI^.fiName^, anOffset.iLow,
                       anOffset.iHigh, '']);
    {get the BLOB block}
    ffShiftI64R(anOffset, fiLog2BlockSize, TempI64);
    BlockNumber := TempI64.iLow;
    if (BlockNumber <= 0) or (BlockNumber >= fiUsedBlocks) then
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrBadBlockNr, [FI^.fiName^, BlockNumber]);
    BLOBBlock := FFBMGetBlock(FI, TI, BlockNumber, aMarkDirty, aReleaseMethod);
    {verify that it's a BLOB block}
    with BLOBBlockHdr^ do
      if (bhbSignature <> ffc_SigBLOBBlock) or
         (bhbThisBlock <> BlockNumber) then
        FFRaiseException(EffServerException, ffStrResServer,
                         fferrBadBLOBBlock, [FI^.fiName^, BlockNumber]);
  end;
  Result := BLOBBlock;
end;
{====================================================================}

end.
