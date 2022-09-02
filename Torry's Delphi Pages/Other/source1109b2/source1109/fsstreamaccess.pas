{*********************************************************}
{* FlashFiler: Table access to streams                   *}
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

Unit fsstreamaccess;

Interface

Uses
  Windows,
  SysUtils,
  Classes,
  fsconst,
  fsllbase,
  fssrmgr,
  fsllexcp,
  fssrbase,
  fssrlock,
  fsfile,
  fstablehelper;

Procedure FFTblDeleteStream(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aStreamNr: TffWord32);
{-Deletes a stream from the file}

Procedure FFTblReadStream(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aStreamNr: TffWord32;
  aStream: TStream);
{-Reads a stream from the file}
{ NOTE: the data is read to the current position of the stream}

Procedure FFTblWriteStream(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Var aStreamNr: TffWord32;
  aStream: TStream;
  aCreateNew: boolean;
  aStreamID: Longint);
{-Writes a stream to the file; optionally creates it as a new one}

Implementation

{===Helper routines==================================================}

Function AddNewStreamBlock(FI: PffFileInfo;
  TI: PffTransInfo;
  FileHeader: PffBlockHeaderFile;
  PrevStreamBlock: PffBlock;
  Var aReleaseMethod: TffReleaseMethod): PffBlock;
Var
  StreamBlock: PffBlock;
  StrmBlockHdr: PffBlockHeaderStream Absolute StreamBlock;
  PrevStrmBlockHdr: PffBlockHeaderStream Absolute PrevStreamBlock;
Begin
  {Note: assumes that PrevStreamBlock has been marked dirty}
  With FileHeader^ Do
    Begin
      {get a new block}
      StreamBlock := FFTblHlpGetNewBlock(FI, TI, aReleaseMethod, fsoNone);
      FillChar(StreamBlock^[fsc_BlockHeaderSizeStream],
        (bhfBlockSize - fsc_BlockHeaderSizeStream), 0);
      {set up the stream block header information}
      With StrmBlockHdr^ Do
        Begin
          bhsSignature := fsc_SigStreamBlock;
          bhsNextBlock := $FFFFFFFF;
          bhsLSN := 0;
          bhsNextStrmBlock := $FFFFFFFF;
          bhsStreamType := 0;
          bhsStreamLength := 0;
          bhsOwningStream := 0;
        End;
      {chain this block to the previous stream block}
      If Assigned(PrevStreamBlock) Then
        Begin
          PrevStrmBlockHdr^.bhsNextStrmBlock := StrmBlockHdr^.bhsThisBlock;
        End;
    End;
  Result := StreamBlock;
End;
{--------}

Function ReadVfyStreamBlock(FI: PffFileInfo;
  TI: PffTransInfo;
  Const aBlockNumber: TffWord32;
  Const aMarkDirty: boolean;
  Var aReleaseMethod: TffReleaseMethod): PffBlock;
Var
  StreamBlock: PffBlock;
  StrmBlockHdr: PffBlockHeaderStream Absolute StreamBlock;
Begin
  With FI^ Do
    Begin
      {verify the block number}
      If (aBlockNumber <= 0) Or (aBlockNumber >= fiUsedBlocks) Then
        FSRaiseException(EfsServerException, fsStrResServer, fserrBadBlockNr,
          [FI^.fiName^, aBlockNumber]);
      {now get the stream block}
      StreamBlock := FFBMGetBlock(FI, TI, aBlockNumber, aMarkDirty,
        aReleaseMethod, fsoNone);
      {verify that it's a stream block}
      If (StrmBlockHdr^.bhsSignature <> fsc_SigStreamBlock) Or
        (StrmBlockHdr^.bhsThisBlock <> aBlockNumber) Then
        FSRaiseException(EfsServerException, fsStrResServer, fserrBadStreamBlock,
          [FI^.fiName^, aBlockNumber]);
    End;
  Result := StreamBlock;
End;
{--------}

Procedure DeleteStreamPrim(FI: PffFileInfo;
  TI: PffTransInfo;
  FileHeader: PffBlockHeaderFile;
  aStreamNr: Longint;
  aKeep1stBlock: boolean);
Var
  StreamBlock: PffBlock;
  StrmBlockHdr: PffBlockHeaderStream Absolute StreamBlock;
  NextBlock: TffWord32;
  aReleaseMethod: TffReleaseMethod;
Begin

  { Assumption: File header block is exclusively locked. }

  { Read & verify the 1st block for the stream. }
  StreamBlock := ReadVfyStreamBlock(FI, TI, aStreamNr, True, aReleaseMethod);

  Try
    { Get the next block. }
    NextBlock := StrmBlockHdr^.bhsNextStrmBlock;

    { If required, delete this block. }
    If Not aKeep1stBlock Then
      FFTblHlpDeleteBlock(FI, FileHeader, StreamBlock);
  Finally
    aReleaseMethod(StreamBlock);
  End;

  { Delete the succeeding blocks. }
  While NextBlock <> fsc_W32NoValue Do
    Begin
      { Read & verify the next stream block. }
      StreamBlock := ReadVfyStreamBlock(FI, TI, NextBlock, True, aReleaseMethod);
      Try
        { Get the next stream block. }
        NextBlock := StrmBlockHdr^.bhsNextStrmBlock;

        { Add this block to the free blocks list. }
        FFTblHlpDeleteBlock(FI, FileHeader, StreamBlock);
      Finally
        aReleaseMethod(StreamBlock);
      End;
    End;
End;
{====================================================================}

{===Stream routines==================================================}

Procedure FFTblDeleteStream(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aStreamNr: TffWord32);
Var
  FileHeader: PffBlockHeaderFile;
  aReleaseMethod: TffReleaseMethod;
Begin
  {first get the file header, block 0}
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, True,
    aReleaseMethod, fsoNone));
  Try
    {now delete the entire chain of blocks in the stream}
    DeleteStreamPrim(aFi, aTI, FileHeader, aStreamNr, False);
  Finally
    aReleaseMethod(PffBlock(FileHeader));
  End;
End;
{--------}

Procedure FFTblReadStream(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aStreamNr: TffWord32;
  aStream: TStream);
Var
  StreamBlock: PffBlock;
  StrmBlockHdr: PffBlockHeaderStream Absolute StreamBlock;
  NextBlock: TffWord32;
  BytesToGo: Longint;
  BytesToCopy: Longint;
  MaxDataInBlock: Integer;
  ThisBlock: TffWord32;
  aStrmRelMethod: TffReleaseMethod;
Begin
  { Calculate the maximum size of each stream block. }
  MaxDataInBlock := aFI^.fiBlockSize - fsc_BlockHeaderSizeStream;

  { Read & verify the first block for the stream. }
  ThisBlock := aStreamNr;
  StreamBlock := ReadVfyStreamBlock(aFI, aTI, ThisBlock, False,
    aStrmRelMethod);
  Try
    BytesToGo := StrmBlockHdr^.bhsStreamLength;
    While (BytesToGo > 0) Do
      Begin
        { Copy the data from the block to the stream. }
        BytesToCopy := FFMinL(MaxDataInBlock, BytesToGo);
        aStream.Write(StreamBlock^[fsc_BlockHeaderSizeStream], BytesToCopy);
        dec(BytesToGo, BytesToCopy);

        { Calc the next stream block. }
        NextBlock := StrmBlockHdr^.bhsNextStrmBlock;

        If (BytesToGo <> 0) Then
          Begin
            ThisBlock := NextBlock;
            aStrmRelMethod(StreamBlock);
            { Read & verify the next block for the stream. }
            StreamBlock := ReadVfyStreamBlock(aFI, aTI, ThisBlock, False,
              aStrmRelMethod);
          End;
      End;
  Finally
    aStrmRelMethod(StreamBlock);
  End;
End;
{--------}

Procedure FFTblWriteStream(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Var aStreamNr: TffWord32;
  aStream: TStream;
  aCreateNew: boolean;
  aStreamID: Longint);
Var
  FileHeader: PffBlockHeaderFile;
  StreamBlock: PffBlock;
  StrmBlockHdr: PffBlockHeaderStream Absolute StreamBlock;
  BytesToGo: Longint;
  BytesToCopy: Longint;
  MaxDataInBlock: Integer;
  aFHRelMethod,
    aStrmRelMethod: TffReleaseMethod;
  PrevStreamBlock: PffBlock; {!!.01}
  PrevStrmBlockHdr: PffBlockHeaderStream; {!!.01}
  PrevStrmRelMethod: TffReleaseMethod; {!!.01}
Begin

  { Get the file header block. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, True,
    aFHRelMethod, fsoNone));
  Try
    { If we are rewriting the stream, delete all but the first block,
      then read that first block}
    If Not aCreateNew Then
      Begin
        DeleteStreamPrim(aFI, aTI, FileHeader, aStreamNr, True);
        StreamBlock := ReadVfyStreamBlock(aFI, aTI, aStreamNr, True,
          aStrmRelMethod);
      End
        { Otherwise we are creating a new stream, so get a new block. }
    Else
      Begin
        StreamBlock := AddNewStreamBlock(aFI, aTI, FileHeader, Nil,
          aStrmRelMethod);
        With StrmBlockHdr^ Do
          Begin
            aStreamNr := bhsThisBlock;
            bhsOwningStream := aStreamNr;
          End;
      End;
    StrmBlockHdr^.bhsStreamType := aStreamID;
    { Set the stream length and therefore the number of bytes to copy. }
    BytesToGo := aStream.Size;
    StrmBlockHdr^.bhsStreamLength := BytesToGo;
    MaxDataInBlock := FileHeader^.bhfBlockSize - fsc_BlockHeaderSizeStream;

    { Prepare the stream (position at start). }
    aStream.Seek(0, soFromBeginning);

    { Copy the stream data to this block and other blocks, as required. }
    BytesToCopy := FFMinL(MaxDataInBlock, BytesToGo);
    aStream.Read(StreamBlock^[fsc_BlockHeaderSizeStream], BytesToCopy);
    dec(BytesToGo, BytesToCopy);
    While (BytesToGo > 0) Do
      Begin
        PrevStreamBlock := StreamBlock; {!!.01}
        PrevStrmBlockHdr := StrmBlockHdr; {!!.01}
        PrevStrmRelMethod := aStrmRelMethod; {!!.01}
        {aStrmRelMethod(StreamBlock);}{!!.01 deleted}
        StreamBlock := AddNewStreamBlock(aFI, aTI, FileHeader, StreamBlock,
          aStrmRelMethod);
        PrevStrmBlockHdr^.bhsNextBlock := StrmBlockHdr^.bhsThisBlock; {!!.01}
        PrevStrmRelMethod(PrevStreamBlock); {!!.01}
        With StrmBlockHdr^ Do
          Begin
            bhsOwningStream := aStreamNr;
            bhsStreamType := aStreamID;
          End;
        BytesToCopy := FFMinL(MaxDataInBlock, BytesToGo);
        aStream.Read(StreamBlock^[fsc_BlockHeaderSizeStream], BytesToCopy);
        dec(BytesToGo, BytesToCopy);
      End;
  Finally
    aStrmRelMethod(StreamBlock);
    aFHRelMethod(PffBlock(FileHeader));
  End;
End;
{====================================================================}

End.

