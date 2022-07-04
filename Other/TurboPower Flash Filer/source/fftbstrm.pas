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

{$I ffdefine.inc}

unit fftbstrm;

interface

uses
  Windows,
  SysUtils,
  Classes,
  ffconst,
  ffllbase,
  ffsrmgr,
  ffllexcp,
  ffsrbase,
  ffsrlock,
  fffile,
  fftbbase;

procedure FFTblDeleteStream(aFI       : PffFileInfo;
                            aTI       : PffTransInfo;
                            aStreamNr : TffWord32);
  {-Deletes a stream from the file}

procedure FFTblReadStream(aFI       : PffFileInfo;
                          aTI       : PffTransInfo;
                          aStreamNr : TffWord32;
                          aStream   : TStream);
  {-Reads a stream from the file}
  { NOTE: the data is read to the current position of the stream}

procedure FFTblWriteStream(aFI       : PffFileInfo;
                           aTI       : PffTransInfo;
                       var aStreamNr : TffWord32;
                           aStream   : TStream;
                           aCreateNew: boolean;
                           aStreamID : Longint);
  {-Writes a stream to the file; optionally creates it as a new one}

implementation

{===Helper routines==================================================}
function AddNewStreamBlock(FI             : PffFileInfo;
                           TI             : PffTransInfo;
                           FileHeader     : PffBlockHeaderFile;
                           PrevStreamBlock: PffBlock;
                       var aReleaseMethod : TffReleaseMethod) : PffBlock;
var
  StreamBlock      : PffBlock;
  StrmBlockHdr     : PffBlockHeaderStream absolute StreamBlock;
  PrevStrmBlockHdr : PffBlockHeaderStream absolute PrevStreamBlock;
begin
  {Note: assumes that PrevStreamBlock has been marked dirty}
  with FileHeader^ do begin
    {get a new block}
    StreamBlock := FFTblHlpGetNewBlock(FI, TI, aReleaseMethod);
    FillChar(StreamBlock^[ffc_BlockHeaderSizeStream],
             (bhfBlockSize - ffc_BlockHeaderSizeStream), 0);
    {set up the stream block header information}
    with StrmBlockHdr^ do begin
      bhsSignature := ffc_SigStreamBlock;
      bhsNextBlock := $FFFFFFFF;
      bhsLSN := 0;
      bhsNextStrmBlock := $FFFFFFFF;
      bhsStreamType := 0;
      bhsStreamLength := 0;
      bhsOwningStream := 0;
    end;
    {chain this block to the previous stream block}
    if Assigned(PrevStreamBlock) then begin
      PrevStrmBlockHdr^.bhsNextStrmBlock := StrmBlockHdr^.bhsThisBlock;
    end;
  end;
  Result := StreamBlock;
end;
{--------}
function ReadVfyStreamBlock(FI           : PffFileInfo;
                            TI           : PffTransInfo;
                      const aBlockNumber : TffWord32;
                      const aMarkDirty   : boolean;
                        var aReleaseMethod : TffReleaseMethod) : PffBlock;
var
  StreamBlock : PffBlock;
  StrmBlockHdr: PffBlockHeaderStream absolute StreamBlock;
begin
  with FI^ do begin
    {verify the block number}
    if (aBlockNumber <= 0) or (aBlockNumber >= fiUsedBlocks) then
      FFRaiseException(EffServerException, ffStrResServer, fferrBadBlockNr,
                       [FI^.fiName^, aBlockNumber]);
    {now get the stream block}
    StreamBlock := FFBMGetBlock(FI, TI, aBlockNumber, aMarkDirty,
                                aReleaseMethod);
    {verify that it's a stream block}
    if (StrmBlockHdr^.bhsSignature <> ffc_SigStreamBlock) or
       (StrmBlockHdr^.bhsThisBlock <> aBlockNumber) then
      FFRaiseException(EffServerException, ffStrResServer, fferrBadStreamBlock,
                       [FI^.fiName^, aBlockNumber]);
  end;
  Result := StreamBlock;
end;
{--------}
procedure DeleteStreamPrim(FI           : PffFileInfo;
                           TI           : PffTransInfo;
                           FileHeader   : PffBlockHeaderFile;
                           aStreamNr    : Longint;
                           aKeep1stBlock: boolean);
var
  StreamBlock : PffBlock;
  StrmBlockHdr: PffBlockHeaderStream absolute StreamBlock;
  NextBlock   : TffWord32;
  aReleaseMethod : TffReleaseMethod;
begin

  { Assumption: File header block is exclusively locked. }

  { Read & verify the 1st block for the stream. }
  StreamBlock := ReadVfyStreamBlock(FI, TI, aStreamNr, true, aReleaseMethod);

  try
    { Get the next block. }
    NextBlock := StrmBlockHdr^.bhsNextStrmBlock;

    { If required, delete this block. }
    if not aKeep1stBlock then
      FFTblHlpDeleteBlock(FI, FileHeader, StreamBlock);
  finally
    aReleaseMethod(StreamBlock);
  end;

  { Delete the succeeding blocks. }
  while NextBlock <> ffc_W32NoValue do begin
    { Read & verify the next stream block. }
    StreamBlock := ReadVfyStreamBlock(FI, TI, NextBlock, true, aReleaseMethod);
    try
      { Get the next stream block. }
      NextBlock := StrmBlockHdr^.bhsNextStrmBlock;

      { Add this block to the free blocks list. }
      FFTblHlpDeleteBlock(FI, FileHeader, StreamBlock);
    finally
      aReleaseMethod(StreamBlock);
    end;
  end;
end;
{====================================================================}



{===Stream routines==================================================}
procedure FFTblDeleteStream(aFI       : PffFileInfo;
                            aTI       : PffTransInfo;
                            aStreamNr : TffWord32);
var
  FileHeader : PffBlockHeaderFile;
  aReleaseMethod : TffReleaseMethod;
begin
  {first get the file header, block 0}
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, true,
                                                aReleaseMethod));
  try
    {now delete the entire chain of blocks in the stream}
    DeleteStreamPrim(aFi, aTI, FileHeader, aStreamNr, false);
  finally
    aReleaseMethod(PffBlock(FileHeader));
  end;
end;
{--------}
procedure FFTblReadStream(aFI       : PffFileInfo;
                          aTI       : PffTransInfo;
                          aStreamNr : TffWord32;
                          aStream   : TStream);
var
  StreamBlock   : PffBlock;
  StrmBlockHdr  : PffBlockHeaderStream absolute StreamBlock;
  NextBlock     : TffWord32;
  BytesToGo     : Longint;
  BytesToCopy   : Longint;
  MaxDataInBlock: integer;
  ThisBlock     : TffWord32;
  aStrmRelMethod : TffReleaseMethod;
begin
  { Calculate the maximum size of each stream block. }
  MaxDataInBlock := aFI^.fiBlockSize - ffc_BlockHeaderSizeStream;

  { Read & verify the first block for the stream. }
  ThisBlock := aStreamNr;
  StreamBlock := ReadVfyStreamBlock(aFI, aTI, ThisBlock, false,
                                    aStrmRelMethod);
  try
    BytesToGo := StrmBlockHdr^.bhsStreamLength;
    while (BytesToGo > 0) do begin
      { Copy the data from the block to the stream. }
      BytesToCopy := FFMinL(MaxDataInBlock, BytesToGo);
      aStream.Write(StreamBlock^[ffc_BlockHeaderSizeStream], BytesToCopy);
      dec(BytesToGo, BytesToCopy);

      { Calc the next stream block. }
      NextBlock := StrmBlockHdr^.bhsNextStrmBlock;

      if (BytesToGo <> 0) then begin
        ThisBlock := NextBlock;
        aStrmRelMethod(StreamBlock);
        { Read & verify the next block for the stream. }
        StreamBlock := ReadVfyStreamBlock(aFI, aTI, ThisBlock, false,
                                          aStrmRelMethod);
      end;
    end;
  finally
    aStrmRelMethod(StreamBlock);
  end;
end;
{--------}
procedure FFTblWriteStream(aFI       : PffFileInfo;
                           aTI       : PffTransInfo;
                       var aStreamNr : TffWord32;
                           aStream   : TStream;
                           aCreateNew: boolean;
                           aStreamID : Longint);
var
  FileHeader     : PffBlockHeaderFile;
  StreamBlock    : PffBlock;
  StrmBlockHdr   : PffBlockHeaderStream absolute StreamBlock;
  BytesToGo      : Longint;
  BytesToCopy    : Longint;
  MaxDataInBlock : Integer;
  aFHRelMethod,
  aStrmRelMethod : TffReleaseMethod;
  PrevStreamBlock   : PffBlock;                                       {!!.01}
  PrevStrmBlockHdr  : PffBlockHeaderStream;                           {!!.01}
  PrevStrmRelMethod : TffReleaseMethod;                               {!!.01}
begin

  { Get the file header block. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, true,
                                                aFHRelMethod));
  try
    { If we are rewriting the stream, delete all but the first block,
      then read that first block}
    if not aCreateNew then begin
      DeleteStreamPrim(aFI, aTI, FileHeader, aStreamNr, true);
      StreamBlock := ReadVfyStreamBlock(aFI, aTI, aStreamNr, true,
                                        aStrmRelMethod);
    end
    { Otherwise we are creating a new stream, so get a new block. }
    else begin
      StreamBlock := AddNewStreamBlock(aFI, aTI, FileHeader, nil,
                                       aStrmRelMethod);
      with StrmBlockHdr^ do begin
        aStreamNr := bhsThisBlock;
        bhsOwningStream := aStreamNr;
      end;
    end;
    StrmBlockHdr^.bhsStreamType := aStreamID;
    { Set the stream length and therefore the number of bytes to copy. }
    BytesToGo := aStream.Size;
    StrmBlockHdr^.bhsStreamLength := BytesToGo;
    MaxDataInBlock := FileHeader^.bhfBlockSize - ffc_BlockHeaderSizeStream;

    { Prepare the stream (position at start). }
    aStream.Seek(0, soFromBeginning);

    { Copy the stream data to this block and other blocks, as required. }
    BytesToCopy := FFMinL(MaxDataInBlock, BytesToGo);
    aStream.Read(StreamBlock^[ffc_BlockHeaderSizeStream], BytesToCopy);
    dec(BytesToGo, BytesToCopy);
    while (BytesToGo > 0) do begin
      PrevStreamBlock := StreamBlock;                                 {!!.01}
      PrevStrmBlockHdr := StrmBlockHdr;                               {!!.01}
      PrevStrmRelMethod := aStrmRelMethod;                            {!!.01}
      {aStrmRelMethod(StreamBlock);}                                  {!!.01 deleted}
      StreamBlock := AddNewStreamBlock(aFI, aTI, FileHeader, StreamBlock,
                                       aStrmRelMethod);
      PrevStrmBlockHdr^.bhsNextBlock := StrmBlockHdr^.bhsThisBlock;   {!!.01}
      PrevStrmRelMethod(PrevStreamBlock);                             {!!.01}
      with StrmBlockHdr^ do begin
        bhsOwningStream := aStreamNr;
        bhsStreamType := aStreamID;
      end;
      BytesToCopy := FFMinL(MaxDataInBlock, BytesToGo);
      aStream.Read(StreamBlock^[ffc_BlockHeaderSizeStream], BytesToCopy);
      dec(BytesToGo, BytesToCopy);
    end;
  finally
    aStrmRelMethod(StreamBlock);
    aFHRelMethod(PffBlock(FileHeader));
  end;
end;
{====================================================================}

end.
