{*********************************************************}
{* FlashFiler: Table data access                         *}
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

{ Enabled the following DEFINE to instantiate an instance of TffEventLog
  for debugging purposes. }
{.$DEFINE DebugLog}

unit fftbdata;

interface

uses
  Windows,
  SysUtils,
  ffconst,
  ffllbase,
  {$IFDEF DebugLog}
  fflllog,
  {$ENDIF}
  ffsrmgr,
  ffllexcp,
  ffsrbase,
  ffsrlock,
  fffile,
  fftbbase;


{$IFDEF DebugLog}
var
  aLog : TffBaseLog;
{$ENDIF}

{---Record maintenance and access---}
procedure FFTblAddRecord(aFI      : PffFileInfo;
                         aTI      : PffTransInfo;
                     var aRefNr   : TffInt64;
                         aRecData : PffByteArray);
  {-Add a record to the table}
procedure FFTblDeleteRecord(aFI    : PffFileInfo;
                            aTI    : PffTransInfo;
                      const aRefNr : TffInt64);
  {-Delete a record from the table}
procedure FFTblReadNextRecord(aFI        : PffFileInfo;
                              aTI        : PffTransInfo;
                        const aFromRefNr : TffInt64;
                          var aRefNr     : TffInt64;
                              aRecData   : PffByteArray);
  {-Read the next record from the table, given the record from which to read.
    Note: to read the first record, pass 0 as FromRefNr; 0 is returned if
          no further records}
procedure FFTblReadPrevRecord(aFI        : PffFileInfo;
                              aTI        : PffTransInfo;
                        const aFromRefNr : TffInt64;
                          var aRefNr     : TffInt64;
                              aRecData   : PffByteArray);
  {-Read the previous record from the table, given the record from which to read.
    Note: to read the last record, pass 0 as FromRefNr; 0 is returned if
          no further records}
procedure FFTblReadRecord(aFI        : PffFileInfo;
                          aTI        : PffTransInfo;
                    const aRefNr     : TffInt64;
                          aRecData   : PffByteArray);
  {-Read a record from the table}
procedure FFTblUpdateRecord(aFI      : PffFileInfo;
                            aTI      : PffTransInfo;
                    const aRefNr     : TffInt64;
                          aRecData   : PffByteArray);
  {-Update a record in the table}

{---Record information---}
procedure FFTblGetRecordInfo(aFI    : PffFileInfo;
                             aTI    : PffTransInfo;
                         var aInfo  : TffRecordInfo);
  {-Get information about the records in a table}

function FFTblNextAutoIncValue(aFI : PffFileInfo; aTI : PffTransInfo) : TffWord32;
  {-Return the next autoinc value}
function FFTblReadAutoIncValue(aFI : PffFileInfo; aTI : PffTransInfo) : TffWord32;
  {-Return the current autoinc value; this is read-only, it does not modify
    the autoinc value. }
procedure FFTblDelAutoIncValue(aFI : PffFileInfo; aTI : PffTransInfo;
                               aValue : TffWord32);
  {-Deletes an autoinc value (if it was the last)}
procedure FFTblSetAutoIncValue(aFI : PffFileInfo; aTI : PffTransInfo;
                               aValue : TffWord32);
  {-Set the starting value for autoinc fields}


implementation

{$IFDEF DebugLog}
procedure Log(aMsg : string; args : array of const);
begin
  if aLog <> nil then
    aLog.WriteStringFmt(aMsg, args);
end;
{$ENDIF}

{===Verification routines============================================}
function ReadVfyRefNrDataBlock(FI             : PffFileInfo;
                               TI             : PffTransInfo;
                         const aRefNr         : TffInt64;
                         const aMarkDirty     : boolean;
                           var aOffsetInBlock : Longint;
                           var aReleaseMethod : TffReleaseMethod) : PffBlock;
var
  BlockNumber : TffWord32;
  RecordBlock : PffBlock;
  TempI64     : TffInt64;
  RecBlockHdr : PffBlockHeaderData absolute RecordBlock;
begin
  with FI^ do begin
    {verify the reference number}
    if not FFVerifyRefNr(aRefNr, fiLog2BlockSize, fiRecLenPlusTrailer) then
      FFRaiseException(EffServerException, ffStrResServer, fferrBadRefNr,
                       [FI^.fiName^, aRefNr.iLow, aRefNr.iHigh]);
    {now get the record block}
    ffShiftI64R(aRefNr, fiLog2BlockSize, TempI64);
    BlockNumber := TempI64.iLow;
    if (BlockNumber <= 0) or (BlockNumber >= fiUsedBlocks) then
      FFRaiseException(EffServerException, ffStrResServer, fferrBadBlockNr,
                       [FI^.fiName^, BlockNumber]);
    ffI64MinusInt(aRefNr, (BlockNumber shl fiLog2BlockSize), TempI64);
    aOffsetInBlock := TempI64.iLow;
    RecordBlock := FFBMGetBlock(FI, TI, BlockNumber, aMarkDirty, aReleaseMethod);

    {verify that it's a data block}
    if (RecBlockHdr^.bhdSignature <> ffc_SigDataBlock) or
       (RecBlockHdr^.bhdThisBlock <> BlockNumber) then
      FFRaiseException(EffServerException, ffStrResServer, fferrBadDataBlock,
                       [FI^.fiName^, BlockNumber]);
  end;
  Result := RecordBlock;
end;
{--------}
function ReadVfyDataBlock(FI           : PffFileInfo;
                          TI           : PffTransInfo;
                          aBlockNumber : TffWord32;
                    const aMarkDirty   : boolean;
                      var aReleaseMethod : TffReleaseMethod) : PffBlock;
var
  RecordBlock : PffBlock;
  RecBlockHdr : PffBlockHeaderData absolute RecordBlock;
begin
  with FI^ do begin
    {verify the block number}
    if (aBlockNumber <= 0) or (aBlockNumber >= fiUsedBlocks) then
      FFRaiseException(EffServerException, ffStrResServer, fferrBadBlockNr,
                       [FI^.fiName^, aBlockNumber]);
    {now get the record block}
    RecordBlock := FFBMGetBlock(FI, TI, aBlockNumber, aMarkDirty, aReleaseMethod);
    {verify that it's a data block}
    if (RecBlockHdr^.bhdSignature <> ffc_SigDataBlock) or
       (RecBlockHdr^.bhdThisBlock <> aBlockNumber) then
      FFRaiseException(EffServerException, ffStrResServer, fferrBadDataBlock,
                       [FI^.fiName^, aBlockNumber]);
  end;
  Result := RecordBlock;
end;
{====================================================================}


{===Helper routines==================================================}
function AddNewDataBlock(FI             : PffFileInfo;
                         TI             : PffTransInfo;
                         FileHeader     : PffBlockHeaderFile;
                     var aOffsetInBlock : Longint;
                     var aReleaseMethod : TffReleaseMethod) : PffBlock;
var
  i               : integer;
  BlockOffset     : TffInt64;
  ThisBlock       : TffWord32;
  ThisLink        : TffWord32;
  NextLink        : TffWord32;
  RecordBlock     : PffBlock;
  RecBlockHdr     : PffBlockHeaderData absolute RecordBlock;
  PrevRecBlock    : PffBlock;
  PrevRecBlockHdr : PffBlockHeaderData absolute PrevRecBlock;
  TempI64         : TffInt64;
  aChainRelMethod : TffReleaseMethod;
begin
  { Assumption: File header block obtained for write access. }

  { Note: this routine is only called if bhfDelRecCount = 0}
  with FileHeader^ do begin
    Assert(bhfDelRecCount = 0);
    { Get a new block. }
    RecordBlock := FFTblHlpGetNewBlock(FI, TI, aReleaseMethod);

    { Set up the data block header information. }
    with RecBlockHdr^ do begin
      ThisBlock := bhdThisBlock;
      TempI64.iLow := ThisBlock;
      TempI64.iHigh := 0;
      ffShiftI64L(TempI64, FI^.fiLog2BlockSize, BlockOffset);
      bhdSignature := ffc_SigDataBlock;
      bhdNextBlock := $FFFFFFFF;
      bhdLSN := 0;
      bhdRecCount := bhfRecsPerBlock;
      bhdRecLength := FI^.fiRecordLength;
      bhdNextDataBlock := $FFFFFFFF;
      bhdPrevDataBlock := bhfLastDataBlock;
    end;

    { Create the deleted record chain in the block. }
    aOffsetInBlock := ffc_BlockHeaderSizeData;
    ThisLink := ffc_BlockHeaderSizeData;
    NextLink := ffc_BlockHeaderSizeData + FI^.fiRecLenPlusTrailer;
    for i := 1 to pred(bhfRecsPerBlock) do begin
      ffI64AddInt(BlockOffset, NextLink, TempI64);
      PByte(@RecordBlock^[ThisLink])^ := $FF;
      PffInt64(@RecordBlock^[ThisLink + 1])^ := TempI64;
      ThisLink := NextLink;
      inc(NextLink, FI^.fiRecLenPlusTrailer);
    end;
    PByte(@RecordBlock^[ThisLink])^ := $FF;                            {!!.01}
    PffWord32(@RecordBlock^[ThisLink + 1])^ := $FFFFFFFF;
    PffWord32(@RecordBlock^[ThisLink + 5])^ := $FFFFFFFF;

    { Attach this chain of deleted records. }
    ffI64AddInt(BlockOffset, ffc_BlockHeaderSizeData, bhf1stDelRec);
    bhfDelRecCount := bhfRecsPerBlock;
    assert(bhfDelRecCount > 0);

    { Attach this block to the chain of data blocks. }
    if (bhfLastDataBlock = $FFFFFFFF) then
      bhf1stDataBlock := ThisBlock
    else begin
      PrevRecBlock := ReadVfyDataBlock(FI, TI, bhfLastDataBlock, ffc_MarkDirty,
                                       aChainRelMethod);
      PrevRecBlockHdr^.bhdNextDataBlock := ThisBlock;
      aChainRelMethod(PrevRecBlock);
    end;
    bhfLastDataBlock := ThisBlock;
  end;
  Result := RecordBlock;
end;
{====================================================================}


{===Record maintenance===============================================}
procedure FFTblAddRecord(aFI      : PffFileInfo;
                         aTI      : PffTransInfo;
                     var aRefNr   : TffInt64;
                         aRecData : PffByteArray);
var
  OffsetInBlock : Longint;
  DelLink       : PByte;
  FileHeader    : PffBlockHeaderFile;
  RecordBlock   : PffBlock;
  RecBlockHdr   : PffBlockHeaderData absolute RecordBlock;
  aFHRelMethod,
  aBlkRelMethod : TffReleaseMethod;
begin
  { Get the file header block & mark it dirty. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, ffc_MarkDirty,
                                                aFHRelMethod));
  with FileHeader^ do
    try
      { If no deleted records, add new block}
      if (bhfDelRecCount = 0) then
        RecordBlock := AddNewDataBlock(aFI, aTI, FileHeader, OffsetInBlock,
                                       aBlkRelMethod)
      {else read and verify the 1st deleted record's block}
      else
        RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, bhf1stDelRec,
                                             ffc_MarkDirty, OffsetInBlock,
                                             aBlkRelMethod);
      {mark the first deleted record as active, update the 1st
       deleted record reference}
      aRefNr := bhf1stDelRec;
      DelLink := @(RecordBlock^[OffsetInBlock]);
      bhf1stDelRec := PffInt64(@(RecordBlock^[OffsetInBlock + 1]))^;
      DelLink^ := 0;
      {copy the data from the record into the block}
      Move(aRecData^, RecordBlock^[OffsetInBlock + sizeof(byte)], bhfRecordLength);
      {decrement the number of deleted records,
       increment the number of active records}
      inc(bhfRecordCount);
      dec(bhfDelRecCount);
      aBlkRelMethod(RecordBlock);
    finally
      aFHRelMethod(PffBlock(FileHeader));
    end;
end;
{--------}
procedure FFTblDeleteRecord(aFI    : PffFileInfo;
                            aTI    : PffTransInfo;
                      const aRefNr : TffInt64);
var
  FileHeader     : PffBlockHeaderFile;
  OffsetInBlock  : Longint;
  RecordBlock    : PffBlock;
  RecBlockHdr    : PffBlockHeaderData absolute RecordBlock;
  DelLink        : PByte;
  DeletedRecOfs  : PffInt64;
  TempI64        : TffInt64;
  aFHRelMethod,
  aBlkRelMethod  : TffReleaseMethod;
begin
  TempI64.iLow := 0;
  TempI64.iHigh := 0;

  { Get an exclusive lock on the file header. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, ffc_MarkDirty,
                                                aFHRelMethod));
  with FileHeader^ do
    try
      {read and verify the record's block}
      RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, aRefNr, true,
                                           OffsetInBlock, aBLKRelMethod);
      try
        {verify that the record's not already deleted}
        DelLink := @RecordBlock^[OffsetInBlock];
        if (DelLink^ = $FF) then
          FFRaiseException(EffServerException, ffStrResServer, fferrRecDeleted,
                           [aFI^.fiName^, aRefNr.iLow, aRefNr.iHigh]);

        {mark this record as the start of the chain}
        DelLink^ := $FF;
        inc(DelLink, sizeOf(Byte));
        DeletedRecOfs := PffInt64(DelLink);
        DeletedRecOfs^ := bhf1stDelRec;

        { Zero out the remainder of the record. }
        Inc(DelLink, sizeOf(TffInt64));
        FillChar(DelLink^, aFI^.fiRecordLength - sizeOf(TffInt64), 0);

        {update the 1st deleted record reference}
        bhf1stDelRec := aRefNr;

        {increment the number of deleted records,
         decrement the number of active records}
        inc(bhfDelRecCount);
        assert(bhfDelRecCount > 0);
        dec(bhfRecordCount);
      finally
        aBlkRelMethod(RecordBlock);
      end;
    finally
      aFHRelMethod(PffBlock(FileHeader));
    end;
end;
{--------}
procedure FFTblReadNextRecord(aFI        : PffFileInfo;
                              aTI        : PffTransInfo;
                        const aFromRefNr : TffInt64;
                          var aRefNr     : TffInt64;
                              aRecData   : PffByteArray  );
var
  FileHeader    : PffBlockHeaderFile;
  FinalOffset   : Longint;
  FoundIt       : boolean;
  NextBlock     : TffWord32;
  OffsetInBlock : Longint;
  RecordBlock   : PffBlock;
  RecBlockHdr   : PffBlockHeaderData absolute RecordBlock;
  TempI64       : TffInt64;
  ThisBlock     : TffWord32;
  aFHRelMethod,
  aBlkRelMethod : TffReleaseMethod;
begin
  {first get the file header, block 0}
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, ffc_ReadOnly,
                                                aFHRelMethod));
  with FileHeader^ do
    try
      {cater for silly case}
      if (bhfRecordCount = 0) then begin
        ffInitI64(aRefNr);
        Exit;
      end;
      {read and verify the record's block, if reference is not zero}
      TempI64.iLow := 0;
      TempI64.iHigh := 0;
      if (ffCmpI64(aFromRefNr, TempI64) <> 0) then
        RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, aFromRefNr, ffc_ReadOnly,
                                             OffsetInBlock, aBlkRelMethod)
      {otherwise, get the first data block}
      else begin
        ThisBlock := bhf1stDataBlock;
        RecordBlock := ReadVfyDataBlock(aFI, aTI, ThisBlock, ffc_ReadOnly,
                                        aBlkRelMethod);
        {note: although this calculation of the offset is likely to result in
               a negative number, it'll be incremented immediately again}
        OffsetInBlock := ffc_BlockHeaderSizeData - aFI^.fiRecLenPlusTrailer;
      end;

      try
        { Keep reading records and blocks until we've found the next active
          record. }
        FinalOffset := ffc_BlockHeaderSizeData +
                       (bhfRecsPerBlock * aFI^.fiRecLenPlusTrailer);
        FoundIt := false;
        repeat
          inc(OffsetInBlock, aFI^.fiRecLenPlusTrailer);
          {have we reached the end of this block?}
          if (OffsetInBlock >= FinalOffset) then begin
            {if there is no next block, break out of loop}
            NextBlock := RecBlockHdr^.bhdNextDataBlock;

            if (NextBlock = $FFFFFFFF) then
              Break;{out of the repeat..until loop}

            ThisBlock := NextBlock;
            { Otherwise read the next block. }
            aBlkRelMethod(RecordBlock);
            RecordBlock := ReadVfyDataBlock(aFI, aTI, ThisBlock, ffc_ReadOnly,
                                            aBlkRelMethod);
            OffsetInBlock := ffc_BlockHeaderSizeData;
          end;
          if (PShortInt(@RecordBlock^[OffsetInBlock])^ = 0) then
            FoundIt := true;
        until FoundIt;
        if not FoundIt then begin
          aRefNr.iLow := 0;
          aRefNr.iHigh := 0;
        end else begin
          TempI64.iLow := RecBlockHdr^.bhdThisBlock;
          TempI64.iHigh := 0;
          ffShiftI64L(TempI64, aFI^.fiLog2BlockSize, TempI64);
          ffI64AddInt(TempI64, OffsetInBlock, aRefNr);
          if aRecData <> nil then                                      {!!.01}
            Move(RecordBlock^[OffsetInBlock+sizeof(byte)], aRecData^,  {!!.01}
                 aFI^.fiRecordLength);                                 {!!.01}
        end;
      finally
        if RecordBlock <> nil then                                     {!!.13}
          aBlkRelMethod(RecordBlock);
      end;
    finally
      aFHRelMethod(PffBlock(FileHeader));
    end;
end;
{--------}
procedure FFTblReadPrevRecord(aFI        : PffFileInfo;
                              aTI        : PffTransInfo;
                        const aFromRefNr : TffInt64;
                          var aRefNr     : TffInt64;
                              aRecData   : PffByteArray  );
var
  FileHeader    : PffBlockHeaderFile;
  FoundIt       : boolean;
  OffsetInBlock : Longint;
  PrevBlock     : TffWord32;
  RecordBlock   : PffBlock;
  RecBlockHdr   : PffBlockHeaderData absolute RecordBlock;
  TempI64       : TffInt64;
  aBlkRelMethod,
  aFHRelMethod : TffReleaseMethod;
begin
  {first get the file header, block 0}
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, ffc_ReadOnly,
                                                aFHRelMethod));
  with FileHeader^ do
    try
      {cater for silly case}
      if (bhfRecordCount = 0) then begin
        aRefNr.iLow := 0;
        aRefNr.iHigh := 0;
        Exit;
      end;
      {read and verify the record's block, if reference is not zero}
      TempI64.iLow := 0;
      TempI64.iHigh := 0;
      if (ffCmpI64(aFromRefNr, TempI64) <> 0) then
        RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, aFromRefNr, ffc_ReadOnly,
                                             OffsetInBlock, aBlkRelMethod)
      {otherwise, get the last data block}
      else begin
        RecordBlock := ReadVfyDataBlock(aFI, aTI, bhfLastDataBlock,
                                        ffc_ReadOnly, aBlkRelMethod);
        OffsetInBlock := ffc_BlockHeaderSizeData +
                         (bhfRecsPerBlock * aFI^.fiRecLenPlusTrailer);
      end;
      try
        { Keep reading records and blocks until we've found the previous active
          record. }
        FoundIt := False;
        repeat
          dec(OffsetInBlock, aFI^.fiRecLenPlusTrailer);
          {have we reached the end of this block?}
          if (OffsetInBlock < ffc_BlockHeaderSizeData) then begin

            PrevBlock := RecBlockHdr^.bhdPrevDataBlock;
            {if there is no previous block, break out of loop}

            if (PrevBlock = $FFFFFFFF) then
              Break;{out of the repeat..until loop}

            {otherwise read the next block}
            aBlkRelMethod(RecordBlock);
            RecordBlock := ReadVfyDataBlock(aFI, aTI, PrevBlock, ffc_ReadOnly,
                                            aBlkRelMethod);
            OffsetInBlock := ffc_BlockHeaderSizeData +
                             ((bhfRecsPerBlock - 1) * aFI^.fiRecLenPlusTrailer); {!!.11}
          end;
          if (PShortInt(@RecordBlock^[OffsetInBlock])^ = 0) then
            FoundIt := True;
        until FoundIt;
        if not FoundIt then begin
          aRefNr.iLow := 0;
          aRefNr.iHigh := 0
        end else begin
          TempI64.iLow := OffsetInBlock;
          TempI64.iHigh := 0;
          aRefNr := TempI64;
          ffI64AddInt(aRefNr,
                      (RecBlockHdr^.bhdThisBlock shl aFI^.fiLog2BlockSize),
                      aRefNr);
          Move(RecordBlock^[OffsetInBlock+sizeof(byte)], aRecData^,
               aFI^.fiRecordLength);
        end;
      finally
        aBlkRelMethod(RecordBlock);
      end;
    finally
      aFHRelMethod(PffBlock(FileHeader));
    end;
end;
{--------}
procedure FFTblReadRecord(aFI        : PffFileInfo;
                          aTI        : PffTransInfo;
                    const aRefNr     : TffInt64;
                          aRecData   : PffByteArray);
var
  DelLink       : PByte;
  OffsetInBlock : Longint;
  RecordBlock   : PffBlock;
  RecBlockHdr   : PffBlockHeaderData absolute RecordBlock;
  aRelMethod    : TffReleaseMethod;
begin
  with aFI^ do begin
    {read and verify the record's block}
    RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, aRefNr, ffc_ReadOnly,
                                         OffsetInBlock, aRelMethod);
    try
      {verify that the record's not deleted}
      DelLink := @RecordBlock^[OffsetInBlock];
      if (DelLink^ <> 0) then
        FFRaiseException(EffServerException, ffStrResServer, fferrRecDeleted,
                         [aFI^.fiName^, aRefNr.iLow, aRefNr.iHigh]);
      {copy the record from the block}
      Move(RecordBlock^[OffsetInBlock+sizeof(byte)], aRecData^, fiRecordLength);
    finally
      aRelMethod(RecordBlock);
    end;
  end;
end;
{--------}
procedure FFTblUpdateRecord(aFI      : PffFileInfo;
                            aTI      : PffTransInfo;
                      const aRefNr   : TffInt64;
                            aRecData : PffByteArray);
var
  DelLink       : PByte;
  OffsetInBlock : Longint;
  RecordBlock   : PffBlock;
  RecBlockHdr   : PffBlockHeaderData absolute RecordBlock;
  aRelMethod    : TffReleaseMethod;
begin
  {read and verify the record's block}
  RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, aRefNr, ffc_MarkDirty,
                                       OffsetInBlock, aRelMethod);
  try
    {verify that the record's not deleted}
    DelLink := @RecordBlock^[OffsetInBlock];
    if (DelLink^ <> 0) then
      FFRaiseException(EffServerException, ffStrResServer, fferrRecDeleted,
                      [aFI^.fiName^, aRefNr.iLow, aRefNr.iHigh]);
    {copy the record into the block}
    Move(aRecData^, RecordBlock^[OffsetInBlock+sizeof(Byte)], aFI^.fiRecordLength);
  finally
    aRelMethod(RecordBlock);
  end;
end;
{====================================================================}


{===Record information===============================================}
procedure FFTblGetRecordInfo(aFI    : PffFileInfo;
                             aTI    : PffTransInfo;
                         var aInfo  : TffRecordInfo);
var
  FileHeader : PffBlockHeaderFile;
  aRelMethod : TffReleaseMethod;
begin
  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, ffc_ReadOnly,
                                                aRelMethod));
  { Return the information required. }
  with FileHeader^, aInfo do begin
    riRecLength := bhfRecordLength;
    riRecCount := bhfRecordCount;
    riDelRecCount := bhfDelRecCount;
    riRecsPerBlock := bhfRecsPerBlock;
  end;

  aRelMethod(PffBlock(FileHeader));
end;
{--------}
function FFTblNextAutoIncValue(aFI : PffFileInfo; aTI : PffTransInfo) : TffWord32;
var
  FileHeader : PffBlockHeaderFile;
  aRelMethod : TffReleaseMethod;
begin
  { Assumption: Transaction started.  Transaction committed
    or rolled back higher up in the call stack. }

  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, ffc_MarkDirty,
                                                aRelMethod));

  { Return the information required. }
  with FileHeader^ do begin
    Result := succ(bhfAutoIncValue);
    bhfAutoIncValue := Result;
  end;

  aRelMethod(PffBlock(FileHeader));
end;
{--------}
function FFTblReadAutoIncValue(aFI : PffFileInfo; aTI : PffTransInfo) : TffWord32;
var
  FileHeader : PffBlockHeaderFile;
  aRelMethod : TffReleaseMethod;
begin
  { Assumption: Transaction started.  Transaction committed
    or rolled back higher up in the call stack. }

  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, ffc_ReadOnly,
                                                aRelMethod));

  { Return the information required. }
  Result := FileHeader^.bhfAutoIncValue;

  aRelMethod(PffBlock(FileHeader));
end;
{--------}
procedure FFTblDelAutoIncValue(aFI : PffFileInfo; aTI : PffTransInfo;
                               aValue : TffWord32);
var
  FileHeader : PffBlockHeaderFile;
  aRelMethod : TffReleaseMethod;
begin
  { Assumption: Transaction started. }

  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, ffc_MarkDirty,
                                                aRelMethod));

  { Return the information required. }
  with FileHeader^ do begin
    if (aValue = bhfAutoIncValue) then
      dec(bhfAutoIncValue);
  end;

  aRelMethod(PffBlock(FileHeader));
end;
{--------}
procedure FFTblSetAutoIncValue(aFI : PffFileInfo; aTI : PffTransInfo;
                               aValue : TffWord32);
var
  FileHeader : PffBlockHeaderFile;
  aRelMethod : TffReleaseMethod;
begin
  { Assumption: Transaction started. }

  { First get the file header, block 0, mark dirty. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, ffc_MarkDirty,
                                                aRelMethod));

  { Set the new seed value }
  FileHeader^.bhfAutoIncValue := aValue;
  aRelMethod(PffBlock(FileHeader));
end;
{====================================================================}

{$IFDEF DebugLog}
initialization
  aLog := TffEventLog.Create(nil);
  aLog.FileName := '.\fftbData.log';
  aLog.Enabled := True;

finalization
  aLog.Free;
{$ENDIF}

end.
