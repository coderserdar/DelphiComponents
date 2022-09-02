{*********************************************************}
{* FSSQL: BLOB retrieval & verification routines         *}
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

Unit fssrblob;

Interface

Uses
  fsllbase,
  fssrbase,
  fssrlock;

{---Blob retrieval & verification routines---}
Function ReadBLOBBlock(FI: PffFileInfo;
  TI: PffTransInfo;
  Const anOffset: TffInt64;
  Var aOffsetInBlock: TffWord32; {!!.11}
  Var aReleaseMethod: TffReleaseMethod)
  : PffBlock;
{ Reads the BLOB block containing the specified offset.  This method does
  not perform any locking so use it only when the block has previously
  been locked and the lock is still in effect. }

Function ReadVfyBlobBlock(FI: PffFileInfo;
  TI: PffTransInfo;
  Const aMarkDirty: Boolean;
  Const anOffset: TffInt64;
  Var aOffsetInBlock: TffWord32; {!!.11}
  Var aReleaseMethod: TffReleaseMethod)
  : PffBlock;

Function ReadVfyBlobBlock2(FI: PffFileInfo;
  TI: PffTransInfo;
  Const aMarkDirty: Boolean;
  Const anOffset: TffInt64;
  Var aBlockNum: TffWord32;
  Var aOffsetInBlock: TffWord32; {!!.11}
  Var aReleaseMethod: TffReleaseMethod)
  : PffBlock;

Function ReadVfyBlobBlock3(FI: PffFileInfo;
  TI: PffTransInfo;
  Const aMarkDirty: Boolean;
  Const anOffset: TffInt64;
  Var aReleaseMethod: TffReleaseMethod)
  : PffBlock;

Implementation

Uses
  fsconst,
  fsllexcp,
  fstablehelper;

{== Block verification routines ======================================}

Function ReadBLOBBlock(FI: PffFileInfo;
  TI: PffTransInfo;
  Const anOffset: TffInt64;
  Var aOffsetInBlock: TffWord32; {!!.11}
  Var aReleaseMethod: TffReleaseMethod)
  : PffBlock;
Var
  BlockNumber: TffWord32;
  BLOBBlock: PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB Absolute BLOBBlock;
  TempI64: TffInt64;
Begin
  { Assumptions: The block was previously read and successfully verified.
                 The block was previously locked and the lock is still in
                 effect. }
  With FI^ Do
    Begin

      { Get the BLOB block. }
      ffShiftI64R(anOffset, fiLog2BlockSize, TempI64);
      BlockNumber := TempI64.iLow;

      ffI64MinusInt(anOffset, (BlockNumber Shl fiLog2BlockSize), TempI64);
      aOffsetInBlock := TempI64.iLow;
      BLOBBlock := FI^.fiBufMgr.GetBlock(FI, BlockNumber, TI, False, aReleaseMethod, fsoNone);
    End;
  Result := BLOBBlock;
End;
{--------}

Function ReadVfyBlobBlock(FI: PffFileInfo;
  TI: PffTransInfo;
  Const aMarkDirty: boolean;
  Const anOffset: TffInt64;
  Var aOffsetInBlock: TffWord32; {!!.11}
  Var aReleaseMethod: TffReleaseMethod)
  : PffBlock;
Var
  BlockNumber: TffWord32;
  BLOBBlock: PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB Absolute BLOBBlock;
  TempI64: TffInt64;
Begin
  With FI^ Do
    Begin
      {verify the BLOB number}
      If Not FFVerifyBLOBNr(anOffset, fiLog2BlockSize) Then
        FSRaiseException(EfsServerException, fsStrResServer,
          fserrBadBLOBNr, [FI^.fiName^, anOffset.iLow,
          anOffset.iHigh]);
      {now get the BLOB block}
      ffShiftI64R(anOffset, fiLog2BlockSize, TempI64);
      BlockNumber := TempI64.iLow;
      If (BlockNumber <= 0) Or (BlockNumber >= fiUsedBlocks) Then
        FSRaiseException(EfsServerException, fsStrResServer,
          fserrBadBlockNr, [FI^.fiName^, BlockNumber]);
      ffI64MinusInt(anOffset, (BlockNumber Shl fiLog2BlockSize), TempI64);
      aOffsetInBlock := TempI64.iLow;
      BLOBBlock := FFBMGetBlock(FI, TI, BlockNumber, aMarkDirty, aReleaseMethod, fsoNone);
      { Verify that it's a BLOB header block. }
      With BLOBBlockHdr^ Do
        If (bhbSignature <> fsc_SigBLOBBlock) Or
          (bhbThisBlock <> BlockNumber) Then
          FSRaiseException(EfsServerException, fsStrResServer,
            fserrBadBLOBBlock, [FI^.fiName^, BlockNumber]);
    End;
  Result := BLOBBlock;
End;
{--------}

Function ReadVfyBlobBlock2(FI: PffFileInfo;
  TI: PffTransInfo;
  Const aMarkDirty: boolean;
  Const anOffset: TffInt64;
  Var aBlockNum: TffWord32;
  Var aOffsetInBlock: TffWord32; {!!.11}
  Var aReleaseMethod: TffReleaseMethod)
  : PffBlock;
Var
  BLOBBlock: PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB Absolute BLOBBlock;
  TempI64: TffInt64;
Begin
  With FI^ Do
    Begin
      {verify the BLOB number}
      If Not FFVerifyBLOBNr(anOffset, fiLog2BlockSize) Then
        FSRaiseException(EfsServerException, fsStrResServer,
          fserrBadBLOBNr, [FI^.fiName^, anOffset.iLow,
          anOffset.iHigh]);
      {now get the BLOB block}
      ffShiftI64R(anOffset, fiLog2BlockSize, TempI64);
      aBlockNum := TempI64.iLow;
      If (aBlockNum <= 0) Or (aBlockNum >= fiUsedBlocks) Then
        FSRaiseException(EfsServerException, fsStrResServer,
          fserrBadBlockNr, [FI^.fiName^, aBlockNum]);
      ffI64MinusInt(anOffset, (aBlockNum Shl fiLog2BlockSize), TempI64);
      aOffsetInBlock := TempI64.iLow;
      BLOBBlock := FFBMGetBlock(FI, TI, aBlockNum, aMarkDirty, aReleaseMethod, fsoNone);
      {verify that it's a BLOB header block}
      With BLOBBlockHdr^ Do
        If (bhbSignature <> fsc_SigBLOBBlock) Or
          (bhbThisBlock <> aBlockNum) Then
          FSRaiseException(EfsServerException, fsStrResServer,
            fserrBadBLOBBlock, [FI^.fiName^, aBlockNum]);
    End;
  Result := BLOBBlock;
End;
{--------}

Function ReadVfyBlobBlock3(FI: PffFileInfo;
  TI: PffTransInfo;
  Const aMarkDirty: boolean;
  Const anOffset: TffInt64;
  Var aReleaseMethod: TffReleaseMethod): PffBlock;
Var
  BlockNumber: TffWord32;
  BLOBBlock: PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB Absolute BLOBBlock;
  TempI64: TffInt64;
Begin
  With FI^ Do
    Begin
      {verify the segment number}
      If Not FFVerifyBLOBNr(anOffset, fiLog2BlockSize) Then
        FSRaiseException(EfsServerException, fsStrResServer,
          fserrBadBLOBSeg, [FI^.fiName^, anOffset.iLow,
          anOffset.iHigh, '']);
      {get the BLOB block}
      ffShiftI64R(anOffset, fiLog2BlockSize, TempI64);
      BlockNumber := TempI64.iLow;
      If (BlockNumber <= 0) Or (BlockNumber >= fiUsedBlocks) Then
        FSRaiseException(EfsServerException, fsStrResServer,
          fserrBadBlockNr, [FI^.fiName^, BlockNumber]);
      BLOBBlock := FFBMGetBlock(FI, TI, BlockNumber, aMarkDirty, aReleaseMethod, fsoNone);
      {verify that it's a BLOB block}
      With BLOBBlockHdr^ Do
        If (bhbSignature <> fsc_SigBLOBBlock) Or
          (bhbThisBlock <> BlockNumber) Then
          FSRaiseException(EfsServerException, fsStrResServer,
            fserrBadBLOBBlock, [FI^.fiName^, BlockNumber]);
    End;
  Result := BLOBBlock;
End;
{====================================================================}

End.

