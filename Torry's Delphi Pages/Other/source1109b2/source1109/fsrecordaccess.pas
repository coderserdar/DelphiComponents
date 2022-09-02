{*********************************************************}
{* FSSQL: Table data access                              *}
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

{ Enabled the following DEFINE to instantiate an instance of TffEventLog
  for debugging purposes. }
{.$DEFINE DebugLog}

Unit fsrecordaccess;

Interface

Uses
  Windows,
  SysUtils,
  fsconst,
  fsllbase,
  fssrbde,
  {$IFDEF DebugLog}
  fslllog,
  {$ENDIF}
  fssrmgr,
  fsllexcp,
  fssrbase,
  fssrlock,
  fsfile,
  fstablehelper,
  fslldict,
  fsblobaccess,
  fshash;

{$IFDEF DebugLog}
Var
  aLog: TffBaseLog;
  {$ENDIF}

  {---Record maintenance and access---}
Function fsUnDeleteRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Var aRefNr: TffInt64;
  Var aRecData: PffByteArray): Boolean;

Procedure FFTblAddRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Var aRefNr: TffInt64;
  aRecData: PffByteArray;
  aDict: TFSInfoDict;
  aBlobFI: PffFileInfo;
  aFlag: Byte);
{-Add a record to the table}

Function FFTblDeleteRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aRefNr: TffInt64;
  aDict: TFSInfoDict;
  aBlobFI: PffFileInfo;
  aOldData: PffByteArray;
  aDeleteBlob: boolean): TffResult;
{-Delete a record from the table}

Procedure FFTblReadNextRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aFromRefNr: TffInt64;
  Var aRefNr: TffInt64;
  aRecData: PffByteArray;
  aUndelete: Boolean;
  OnlyDeleted: boolean;
  Var aFlag: Byte);
{-Read the next record from the table, given the record from which to read.
  Note: to read the first record, pass 0 as FromRefNr; 0 is returned if
        no further records}

Procedure FFTblReadPrevRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aFromRefNr: TffInt64;
  Var aRefNr: TffInt64;
  aRecData: PffByteArray;
  Var aFlag: Byte);
{-Read the previous record from the table, given the record from which to read.
  Note: to read the last record, pass 0 as FromRefNr; 0 is returned if
        no further records}

Procedure FFTblReadRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aRefNr: TffInt64;
  aRecData: PffByteArray;
  fiVersion: Longint;
  Var aFlag: Byte);
{-Read a record from the table}

Function FFTblUpdateRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aRefNr: TffInt64;
  aRecData: PffByteArray;
  aDict: TFSInfoDict;
  aBlobFI: PffFileInfo;
  fiVersion: Longint): TffResult;
{-Update a record in the table}

Procedure FFTblUpdateFlagRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aRefNr: TffInt64;
  aFlags: Byte;
  aChecked: boolean;
  fiVersion: Longint);

Procedure FFTblSetFlagsRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aRefNr: TffInt64;
  aFlags: Byte;
  fiVersion: Longint);

Function FFTblGetFlagsRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aRefNr: TffInt64;
  fiVersion: Longint): Byte;

{---Record information---}
Procedure FFTblGetRecordInfo(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Var aInfo: TffRecordInfo);
{-Get information about the records in a table}

Function FFTblNextAutoInc(aFI: PffFileInfo; aTI: PffTransInfo): Int64;
{-Return the next autoinc value}
Function FFTblLastAutoInc(aFI: PffFileInfo; aTI: PffTransInfo): Int64;
{-Return the next autoinc value}

Function FFTblReadAutoInc(aFI: PffFileInfo; aTI: PffTransInfo; Var aStep: Longint): Int64;
{-Return the current autoinc value; this is read-only, it does not modify
  the autoinc value. }
Function FFTblReadMaxRecords(aFI: PffFileInfo; aTI: PffTransInfo): Longint;
Function FFTblReadTableFlags(aFI: PffFileInfo; aTI: PffTransInfo): Word;
Function FFTblReadTablePassword(aFI: PffFileInfo; aTI: PffTransInfo): Longword;
Function FFTblReadTablePasswordRest(aFI: PffFileInfo; aTI: PffTransInfo): Longword;
Function FFTblReadTableDBID(aFI: PffFileInfo; aTI: PffTransInfo): Longword;
Function FFTblReadRecordCount(aFI: PffFileInfo; aTI: PffTransInfo): Longint;
Procedure FFTblDelAutoIncValue(aFI: PffFileInfo; aTI: PffTransInfo;
  aValue: Int64);
{-Deletes an autoinc value (if it was the last)}
Procedure FFTblSetAutoInc(aFI: PffFileInfo; aTI: PffTransInfo;
  aValue: Int64; Step: Longint);
Procedure FFTblSetMaxRecords(aFI: PffFileInfo; aTI: PffTransInfo;
  aValue: Longint);
Procedure FFTblSetTableFlags(aFI: PffFileInfo; aTI: PffTransInfo;
  aValue: Word);
Procedure FFTblSetTablePassword(aFI: PffFileInfo; aTI: PffTransInfo;
  aValue: Longword);
Procedure FFTblSetTablePasswordRest(aFI: PffFileInfo; aTI: PffTransInfo;
  aValue: Longword);
Procedure FFTblSetTableDBID(aFI: PffFileInfo; aTI: PffTransInfo;
  aValue: Longword);
{-Set the starting value for autoinc fields}

Implementation

{$IFDEF DebugLog}

Procedure Log(aMsg: String; args: Array Of Const);
Begin
  If aLog <> Nil Then
    aLog.WriteStringFmt(aMsg, args);
End;
{$ENDIF}

{===Verification routines============================================}

Function ReadVfyRefNrDataBlock(FI: PffFileInfo;
  TI: PffTransInfo;
  Const aRefNr: TffInt64;
  Const aMarkDirty: boolean;
  Var aOffsetInBlock: Longint;
  Var aReleaseMethod: TffReleaseMethod;
  aOperation: TfsOperation): PffBlock;
Var
  BlockNumber: TffWord32;
  RecordBlock: PffBlock;
  TempI64: TffInt64;
  RecBlockHdr: PffBlockHeaderData Absolute RecordBlock;
Begin
  With FI^ Do
    Begin
      {verify the reference number}
      If Not FFVerifyRefNr(aRefNr, fiLog2BlockSize, fiRecLenPlusTrailer) Then
        FSRaiseException(EfsServerException, fsStrResServer, fserrBadRefNr,
          [FI^.fiName^, aRefNr.iLow, aRefNr.iHigh]);
      {now get the record block}
      ffShiftI64R(aRefNr, fiLog2BlockSize, TempI64);
      BlockNumber := TempI64.iLow;
      If (BlockNumber <= 0) Or (BlockNumber >= fiUsedBlocks) Then
        FSRaiseException(EfsServerException, fsStrResServer, fserrBadBlockNr,
          [FI^.fiName^, BlockNumber]);
      ffI64MinusInt(aRefNr, (BlockNumber Shl fiLog2BlockSize), TempI64);
      aOffsetInBlock := TempI64.iLow;
      RecordBlock := FFBMGetBlock(FI, TI, BlockNumber, aMarkDirty, aReleaseMethod, aOperation);

      {verify that it's a data block}
      If (RecBlockHdr^.bhdSignature <> fsc_SigDataBlock) Or
        (RecBlockHdr^.bhdThisBlock <> BlockNumber) Then
        FSRaiseException(EfsServerException, fsStrResServer, fserrBadDataBlock,
          [FI^.fiName^, BlockNumber]);
    End;
  Result := RecordBlock;
End;
{--------}

Function ReadVfyDataBlock(FI: PffFileInfo;
  TI: PffTransInfo;
  aBlockNumber: TffWord32;
  Const aMarkDirty: boolean;
  Var aReleaseMethod: TffReleaseMethod;
  aOperation: TfsOperation): PffBlock;
Var
  RecordBlock: PffBlock;
  RecBlockHdr: PffBlockHeaderData Absolute RecordBlock;
Begin
  With FI^ Do
    Begin
      {verify the block number}
      If (aBlockNumber <= 0) Or (aBlockNumber >= fiUsedBlocks) Then
        FSRaiseException(EfsServerException, fsStrResServer, fserrBadBlockNr,
          [FI^.fiName^, aBlockNumber]);
      {now get the record block}
      RecordBlock := FFBMGetBlock(FI, TI, aBlockNumber, aMarkDirty, aReleaseMethod, aOperation);
      {verify that it's a data block}
      If (RecBlockHdr^.bhdSignature <> fsc_SigDataBlock) Or
        (RecBlockHdr^.bhdThisBlock <> aBlockNumber) Then
        FSRaiseException(EfsServerException, fsStrResServer, fserrBadDataBlock,
          [FI^.fiName^, aBlockNumber]);
    End;
  Result := RecordBlock;
End;
{====================================================================}

{===Helper routines==================================================}

Function AddNewDataBlock(FI: PffFileInfo;
  TI: PffTransInfo;
  FileHeader: PffBlockHeaderFile;
  Var aOffsetInBlock: Longint;
  Var aReleaseMethod: TffReleaseMethod;
  aOperation: TfsOperation): PffBlock;
Var
  i: Integer;
  BlockOffset: TffInt64;
  ThisBlock: TffWord32;
  ThisLink: TffWord32;
  NextLink: TffWord32;
  RecordBlock: PffBlock;
  RecBlockHdr: PffBlockHeaderData Absolute RecordBlock;
  PrevRecBlock: PffBlock;
  PrevRecBlockHdr: PffBlockHeaderData Absolute PrevRecBlock;
  TempI64: TffInt64;
  aChainRelMethod: TffReleaseMethod;
Begin
  { Assumption: File header block obtained for write access. }

  { Note: this routine is only called if bhfDelRecCount = 0}
  With FileHeader^ Do
    Begin
      If FileHeader^.bhfEngineDeleteType < edtUndeleteFull Then
        Assert(bhfDelRecCount = 0);
      { Get a new block. }
      RecordBlock := FFTblHlpGetNewBlock(FI, TI, aReleaseMethod, aOperation);

      { Set up the data block header information. }
      With RecBlockHdr^ Do
        Begin
          ThisBlock := bhdThisBlock;
          TempI64.iLow := ThisBlock;
          TempI64.iHigh := 0;
          ffShiftI64L(TempI64, FI^.fiLog2BlockSize, BlockOffset);
          bhdSignature := fsc_SigDataBlock;
          bhdNextBlock := $FFFFFFFF;
          bhdLSN := 0;
          bhdRecCount := bhfRecsPerBlock;
          bhdRecLength := FI^.fiRecordLength;
          bhdNextDataBlock := $FFFFFFFF;
          bhdPrevDataBlock := bhfLastDataBlock;
        End;

      { Create the deleted record chain in the block. }
      aOffsetInBlock := fsc_BlockHeaderSizeData;
      ThisLink := fsc_BlockHeaderSizeData;
      NextLink := fsc_BlockHeaderSizeData + FI^.fiRecLenPlusTrailer;
      // 1 is data, 2 empty, 4 deleted, 8,16,32,64,128 - 5 flags (NotDeleted, NotUpdated, NotVisible)
      For i := 1 To pred(bhfRecsPerBlock) Do
        Begin
          ffI64AddInt(BlockOffset, NextLink, TempI64);
          If bhfFSVersion <= 1058 Then
            PByte(@RecordBlock^[ThisLink])^ := $FF
          Else
            PByte(@RecordBlock^[ThisLink])^ := frEmptyRecord;

          If bhfVersionRecord = trUseVersion Then
            Begin
              PInt64(@RecordBlock^[ThisLink + 1])^ := 0;
              PffInt64(@RecordBlock^[ThisLink + 9])^ := TempI64;
              If FileHeader^.bhfEngineDeleteType >= edtUndeleteFull Then
                PffInt64(@RecordBlock^[ThisLink + 17])^ := TempI64;
            End
          Else
            Begin
              PffInt64(@RecordBlock^[ThisLink + 1])^ := TempI64;
              If FileHeader^.bhfEngineDeleteType >= edtUndeleteFull Then
                PffInt64(@RecordBlock^[ThisLink + 9])^ := TempI64;
            End;
          ThisLink := NextLink;
          inc(NextLink, FI^.fiRecLenPlusTrailer);
        End;

      If FileHeader^.bhfFSVersion <= 1058 Then
        PByte(@RecordBlock^[ThisLink])^ := $FF
      Else
        PByte(@RecordBlock^[ThisLink])^ := frEmptyRecord;

      If FileHeader^.bhfVersionRecord = trUseVersion Then
        Begin
          PInt64(@RecordBlock^[ThisLink + 1])^ := 0;
          PffWord32(@RecordBlock^[ThisLink + 9])^ := $FFFFFFFF;
          PffWord32(@RecordBlock^[ThisLink + 13])^ := $FFFFFFFF;
          If FileHeader^.bhfEngineDeleteType >= edtUndeleteFull Then
            Begin
              PffWord32(@RecordBlock^[ThisLink + 17])^ := $FFFFFFFF;
              PffWord32(@RecordBlock^[ThisLink + 21])^ := $FFFFFFFF;
            End;
        End
      Else
        Begin
          PffWord32(@RecordBlock^[ThisLink + 1])^ := $FFFFFFFF;
          PffWord32(@RecordBlock^[ThisLink + 5])^ := $FFFFFFFF;
          If FileHeader^.bhfEngineDeleteType >= edtUndeleteFull Then
            Begin
              PffWord32(@RecordBlock^[ThisLink + 9])^ := $FFFFFFFF;
              PffWord32(@RecordBlock^[ThisLink + 13])^ := $FFFFFFFF;
            End;
        End;

      { Attach this chain of deleted records. }
      ffI64AddInt(BlockOffset, fsc_BlockHeaderSizeData, bhfLastRec);
      If FileHeader^.bhfEngineDeleteType < edtUndeleteFull Then
        Begin
          ffI64AddInt(BlockOffset, fsc_BlockHeaderSizeData, bhf1stDelRec);
          bhfDelRecCount := bhfRecsPerBlock;
          assert(bhfDelRecCount > 0);
        End;

      { Attach this block to the chain of data blocks. }
      If (bhfLastDataBlock = $FFFFFFFF) Then
        bhf1stDataBlock := ThisBlock
      Else
        Begin
          PrevRecBlock := ReadVfyDataBlock(FI, TI, bhfLastDataBlock, fsc_MarkDirty,
            aChainRelMethod, fsoNone);
          PrevRecBlockHdr^.bhdNextDataBlock := ThisBlock;
          aChainRelMethod(PrevRecBlock);
        End;
      bhfLastDataBlock := ThisBlock;
    End;
  Result := RecordBlock;
End;
{====================================================================}

{===Record maintenance===============================================}

Procedure FFTblAddRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Var aRefNr: TffInt64;
  aRecData: PffByteArray;
  aDict: TFSInfoDict;
  aBlobFI: PffFileInfo;
  aFlag: Byte);
Var
  OffsetInBlock: Longint;
  DelLink: PByte;
  FileHeader: PffBlockHeaderFile;
  RecordBlock: PffBlock;
  aFHRelMethod,
    aBlkRelMethod: TffReleaseMethod;
  Data: PffByteArray;

  Procedure DeleteBLOBsForRecord(aData: PffByteArray);
  Var
    FldInx: Integer;
    FldDesc: PffFieldDescriptor;
    BLOBNr: TffInt64;
    IsNull: boolean;
  Begin
    With aDict Do
      Begin
        For FldInx := 0 To pred(FieldCount) Do
          Begin
            FldDesc := FieldDescriptor[FldInx];
            If (FldDesc^.fdType >= fstBLOB) And
              (FldDesc^.fdType <= ffcLastBLOBType) Then
              Begin
                GetRecordField(FldInx, aData, IsNull, @BLOBNr);
                If (Not IsNull) And (BLOBNr.iLow <> fsc_W32NoValue) Then
                  FFTblDeleteBLOB(aBlobFI, aTI, BLOBNr);
              End;
          End;
      End;
  End;
Begin
  { Get the file header block & mark it dirty. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty,
    aFHRelMethod, fsoNone));
  With FileHeader^ Do
    Try
      { If no deleted records, add new block}
      If (bhfDelRecCount = 0) Then
        Begin
          If FileHeader^.bhfEngineDeleteType >= edtUndeleteFull Then
            Begin
              If (bhfLastCountRec = bhfRecsPerBlock) Then
                Begin
                  RecordBlock := AddNewDataBlock(aFI, aTI, FileHeader, OffsetInBlock,
                    aBlkRelMethod, fsoInsert);
                  bhfLastCountRec := 1;
                End
              Else If ((bhfLastCountRec < bhfRecsPerBlock) And (bhfLastCountRec > 0)) Then
                Begin
                  RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, bhfLastRec,
                    fsc_MarkDirty, OffsetInBlock,
                    aBlkRelMethod, fsoInsert);
                  inc(bhfLastCountRec);
                End
              Else
                Begin
                  RecordBlock := AddNewDataBlock(aFI, aTI, FileHeader, OffsetInBlock,
                    aBlkRelMethod, fsoInsert);
                  bhfLastCountRec := 1;
                End;
            End
          Else
            Begin
              RecordBlock := AddNewDataBlock(aFI, aTI, FileHeader, OffsetInBlock,
                aBlkRelMethod, fsoInsert);
              bhfLastCountRec := 1;
            End;
        End
      Else
        Begin
          If FileHeader^.bhfEngineDeleteType >= edtUndeleteFull Then
            Begin
              If (bhfLastCountRec = bhfRecsPerBlock) Then
                Begin
                  RecordBlock := AddNewDataBlock(aFI, aTI, FileHeader, OffsetInBlock,
                    aBlkRelMethod, fsoInsert);
                  bhfLastCountRec := 1;
                End
              Else
                Begin
                  RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, bhfLastRec,
                    fsc_MarkDirty, OffsetInBlock,
                    aBlkRelMethod, fsoInsert);
                  inc(bhfLastCountRec);
                End;
            End
          Else
            RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, bhf1stDelRec,
              fsc_MarkDirty, OffsetInBlock,
              aBlkRelMethod, fsoInsert);
        End;
      {mark the first deleted record as active, update the 1st
       deleted record reference}
      If FileHeader^.bhfEngineDeleteType >= edtUndeleteFull Then
        aRefNr := bhfLastRec
      Else
        aRefNr := bhf1stDelRec;
      DelLink := @(RecordBlock^[OffsetInBlock]);

      If getflags(DelLink^, frDeleteRecord) And (bhfEngineDeleteType In [edtUndeleteIfPossibleAndBlob]) Then
        Begin
          ffgetmem(data, bhfRecordLength);
          Try
            Move(RecordBlock^[OffsetInBlock + (bhfRecLenPlusTrailer - bhfRecordLength)], Data^, bhfRecordLength);
            DeleteBLOBsForRecord(Data);
          Finally
            fffreemem(data, bhfRecordLength);
          End;
        End;

      If bhfVersionRecord = trUseVersion Then
        Begin
          PInt64(@RecordBlock^[OffsetInBlock + 1])^ := 1;
          If FileHeader^.bhfEngineDeleteType < edtUndeleteFull Then
            bhf1stDelRec := PffInt64(@(RecordBlock^[OffsetInBlock + 9]))^;
          bhfLastRec := PffInt64(@(RecordBlock^[OffsetInBlock + 17]))^;
        End
      Else
        Begin
          If FileHeader^.bhfEngineDeleteType < edtUndeleteFull Then
            bhf1stDelRec := PffInt64(@(RecordBlock^[OffsetInBlock + 1]))^;
          bhfLastRec := PffInt64(@(RecordBlock^[OffsetInBlock + 9]))^;
        End;

      If bhfFSVersion <= 1058 Then
        DelLink^ := 0
      Else
        DelLink^ := frDataRecord; // is data

      If AFlag > 0 Then
        DelLink^ := DelLink^ + AFlag;

      {copy the data from the record into the block}
      Move(aRecData^, RecordBlock^[OffsetInBlock + (bhfRecLenPlusTrailer - bhfRecordLength)], bhfRecordLength);
      {decrement the number of deleted records,
       increment the number of active records}
      inc(bhfRecordCount);
      If FileHeader^.bhfEngineDeleteType < edtUndeleteFull Then
        dec(bhfDelRecCount);
      aBlkRelMethod(RecordBlock);
    Finally
      aFHRelMethod(PffBlock(FileHeader));
    End;
End;
{--------}

Function FFTblDeleteRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aRefNr: TffInt64;
  aDict: TFSInfoDict;
  aBlobFI: PffFileInfo;
  aOldData: PffByteArray;
  aDeleteBlob: Boolean): TffResult;
Var
  FileHeader: PffBlockHeaderFile;
  OffsetInBlock: Longint;
  RecordBlock: PffBlock;
  DelLink: PByte;
  DeletedRecOfs: PffInt64;
  TempI64: TffInt64;
  aFHRelMethod,
    aBlkRelMethod: TffReleaseMethod;

  RecBlockHdr: PffBlockHeaderData Absolute RecordBlock;
  PrevRecBlock: PffBlock;
  PrevRecBlockHdr: PffBlockHeaderData Absolute PrevRecBlock;

  b: Byte;

  Procedure DeleteBLOBsForRecord(aData: PffByteArray);
  Var
    FldInx: Integer;
    FldDesc: PffFieldDescriptor;
    BLOBNr: TffInt64;
    IsNull: boolean;
  Begin
    If aDict.EngineDeleteType In [edtNotUndelete, edtUndeleteIfPossibleNotBlob] Then
      Begin
        With aDict Do
          Begin
            For FldInx := 0 To pred(FieldCount) Do
              Begin
                FldDesc := FieldDescriptor[FldInx];
                If (FldDesc^.fdType >= fstBLOB) And
                  (FldDesc^.fdType <= ffcLastBLOBType) Then
                  Begin
                    GetRecordField(FldInx, aData, IsNull, @BLOBNr);
                    If (Not IsNull) And (BLOBNr.iLow <> fsc_W32NoValue) Then
                      Begin
                        FFTblDeleteBLOB(aBlobFI, aTI, BLOBNr);
                        If aDict.EngineDeleteType In [edtUndeleteIfPossibleNotBlob] Then
                          Begin
                            SetRecordFieldNull(FldInx, aData, True);
                            FFTblUpdateRecord(aFI, aTi, aRefNr, aData, aDict, aBlobFi, aFI^.fiFSVersion);
                          End;
                      End;
                  End;
              End;
          End;
      End;
  End;

Begin
  TempI64.iLow := 0;
  TempI64.iHigh := 0;
  Result := 0;
  { Get an exclusive lock on the file header. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty,
    aFHRelMethod, fsoNone));
  With FileHeader^ Do
    Try
      {read and verify the record's block}
      RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, aRefNr, True,
        OffsetInBlock, aBLKRelMethod, fsoDelete);
      Try
        {verify that the record's not already deleted}
        DelLink := @RecordBlock^[OffsetInBlock];

        If bhfFSVersion <= 1058 Then
          Begin
            If (DelLink^ = $FF) Then
              FSRaiseException(EfsServerException, fsStrResServer, fserrRecDeleted,
                [aFI^.fiName^, aRefNr.iLow, aRefNr.iHigh]);
          End
        Else
          Begin
            If GetFlags(DelLink^, frDeleteRecord) Then
              FSRaiseException(EfsServerException, fsStrResServer, fserrRecDeleted,
                [aFI^.fiName^, aRefNr.iLow, aRefNr.iHigh]);
          End;

        If Not GetFlags(DelLink^, frProtectDeleteRecord) Then
          Begin
            If aDeleteBlob Then
              DeleteBLOBsForRecord(aOldData);
            {mark this record as the start of the chain}
            If bhfFSVersion <= 1058 Then
              DelLink^ := $FF
            Else
              Begin
                b := DelLink^;
                DelLink^ := frDeleteRecord;
                If GetFlags(b, frProtectUpdateRecord) Then
                  DelLink^ := DelLink^ + frProtectUpdateRecord;
              End;

            inc(DelLink, sizeOf(Byte));

            If bhfVersionRecord = trUseVersion Then
              Inc(DelLink, sizeOf(Int64));

            DeletedRecOfs := PffInt64(DelLink);
            DeletedRecOfs^ := bhf1stDelRec;
            { Zero out the remainder of the record. }
            If bhfEngineDeleteType = edtNotUndelete Then
              Begin
                Inc(DelLink, sizeOf(TffInt64));
                FillChar(DelLink^, aFI^.fiRecordLength - sizeOf(TffInt64), 0);
              End;
            {update the 1st deleted record reference}
            bhf1stDelRec := aRefNr;
            {increment the number of deleted records,
             decrement the number of active records}
            inc(bhfDelRecCount);
            assert(bhfDelRecCount > 0);
            dec(bhfRecordCount);
          End
        Else
          Result := DBIERR_NOTSUFFFIELDRIGHTS;
      Finally
        aBlkRelMethod(RecordBlock);
      End;
    Finally
      aFHRelMethod(PffBlock(FileHeader));
    End;
End;
{--------}

Function fsUnDeleteRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Var aRefNr: TffInt64;
  Var aRecData: PffByteArray): Boolean;
Var
  OffsetInBlock: Longint;
  DelLink: PByte;
  FileHeader: PffBlockHeaderFile;
  RecordBlock: PffBlock;
  aFHRelMethod,
    aBlkRelMethod: TffReleaseMethod;
  b: Byte;

Begin
  Result := False;
  { Get the file header block & mark it dirty. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty,
    aFHRelMethod, fsoNone));
  With FileHeader^ Do
    Try
      If (bhfFSVersion >= 1059) And (bhfEngineDeleteType In [edtUndeleteIfPossibleNotBlob, edtUndeleteIfPossibleAndBlob,
        edtUndeleteFull]) Then
        Begin
          { If no deleted records, add new block}
          If (bhfDelRecCount = 0) Then
            Result := False
          Else
            Begin
              RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, bhf1stDelRec,
                fsc_MarkDirty, OffsetInBlock,
                aBlkRelMethod, fsoInsert);
              Try
                {mark the first deleted record as active, update the 1st
                 deleted record reference}
                DelLink := @(RecordBlock^[OffsetInBlock]);

                If GetFlags(DelLink^, frDeleteRecord) Then
                  Begin
                    aRefNr := bhf1stDelRec;
                    If bhfVersionRecord = trUseVersion Then
                      bhf1stDelRec := PffInt64(@(RecordBlock^[OffsetInBlock + 9]))^
                    Else
                      bhf1stDelRec := PffInt64(@(RecordBlock^[OffsetInBlock + 1]))^;
                    b := DelLink^;
                    DelLink^ := frDataRecord + frUndeletedRecord; // is data and undelete

                    If GetFlags(b, frProtectUpdateRecord) Then
                      DelLink^ := DelLink^ + frProtectUpdateRecord;
                    b := DelLink^;
                    If GetFlags(b, frMarkAsBadRecord) Then
                      DelLink^ := DelLink^ + frMarkAsBadRecord;

                    {copy the data from the record into the block}
                    Move(RecordBlock^[OffsetInBlock + (bhfRecLenPlusTrailer - bhfRecordLength)], aRecData^, bhfRecordLength);
                    {decrement the number of deleted records,
                     increment the number of active records}
                    inc(bhfRecordCount);
                    dec(bhfDelRecCount);
                    Result := True;
                  End;
              Finally
                If assigned(RecordBlock) Then
                  aBlkRelMethod(RecordBlock);
              End;
            End;
        End;
    Finally
      aFHRelMethod(PffBlock(FileHeader));
    End;
End;

Function FFTblUpdateRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aRefNr: TffInt64;
  aRecData: PffByteArray;
  aDict: TFSInfoDict;
  aBlobFI: PffFileInfo;
  fiVersion: Longint): TffResult;
Var
  DelLink: PByte;
  OffsetInBlock: Longint;
  RecordBlock: PffBlock;
  RecBlockHdr: PffBlockHeaderData Absolute RecordBlock;
  aRelMethod: TffReleaseMethod;
  af: Int64;

Begin
  Result := 0;
  {read and verify the record's block}
  RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, aRefNr, fsc_MarkDirty,
    OffsetInBlock, aRelMethod, fsoUpdate);
  Try
    {verify that the record's not deleted}
    DelLink := @RecordBlock^[OffsetInBlock];
    // Len := aFI^.fiRecLenPlusTrailer - aFI^.fiRecordLength;
    If fiVersion <= 1058 Then
      Begin
        If (DelLink^ = $FF) Then
          FSRaiseException(EfsServerException, fsStrResServer, fserrRecDeleted,
            [aFI^.fiName^, aRefNr.iLow, aRefNr.iHigh]);
      End
    Else
      Begin
        If GetFlags(DelLink^, frDeleteRecord) Then
          FSRaiseException(EfsServerException, fsStrResServer, fserrRecDeleted,
            [aFI^.fiName^, aRefNr.iLow, aRefNr.iHigh]);
      End;

    If Not GetFlags(DelLink^, frProtectUpdateRecord) Then // not update
      Begin
        {If len In [5, 13, 21] then
        Begin
          DelLink := @RecordBlock^[OffsetInBlock + 1];
          prec := pLongWord(DelLink);
          If pRec^ = 4294967295 then
            pRec^ := 0
          Else
            pRec^ := pRec^ + 1;
        End ;}
        af := DelLink^;
        If GetFlags(af, frUndeletedRecord) Then // reset undelete
          setflags(af, frUndeletedRecord, False);
        setflags(af, frDataRecord, True);
        DelLink^ := Byte(af);
        {copy the record into the block}
        Move(aRecData^, RecordBlock^[OffsetInBlock + (aFI^.fiRecLenPlusTrailer - aFI^.fiRecordLength)], aFI^.fiRecordLength);
      End
    Else
      Result := DBIERR_NOTSUFFFIELDRIGHTS;
  Finally
    aRelMethod(RecordBlock);
  End;
End;

Procedure FFTblUpdateFlagRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aRefNr: TffInt64;
  aFlags: Byte;
  aChecked: boolean;
  fiVersion: Longint);

Var
  DelLink: PByte;
  OffsetInBlock: Longint;
  RecordBlock: PffBlock;
  RecBlockHdr: PffBlockHeaderData Absolute RecordBlock;
  aRelMethod: TffReleaseMethod;
  aValue: Int64;

Begin
  {read and verify the record's block}
  RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, aRefNr, fsc_MarkDirty,
    OffsetInBlock, aRelMethod, fsoUpdate);
  Try
    {verify that the record's not deleted}
    DelLink := @RecordBlock^[OffsetInBlock];
    If fiVersion <= 1058 Then
      Begin
        If (DelLink^ = $FF) Then
          FSRaiseException(EfsServerException, fsStrResServer, fserrRecDeleted,
            [aFI^.fiName^, aRefNr.iLow, aRefNr.iHigh]);
      End
    Else
      Begin
        If GetFlags(DelLink^, frDeleteRecord) Then
          FSRaiseException(EfsServerException, fsStrResServer, fserrRecDeleted,
            [aFI^.fiName^, aRefNr.iLow, aRefNr.iHigh]);

        aValue := DelLink^;
        SetFlags(aValue, aFlags, aChecked);
        DelLink^ := Byte(aValue);
      End;
  Finally
    aRelMethod(RecordBlock);
  End;
End;

Procedure FFTblSetFlagsRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aRefNr: TffInt64;
  aFlags: Byte;
  fiVersion: Longint);
Var
  DelLink: PByte;
  OffsetInBlock: Longint;
  RecordBlock: PffBlock;
  RecBlockHdr: PffBlockHeaderData Absolute RecordBlock;
  aRelMethod: TffReleaseMethod;
  af: Int64;

Begin
  {read and verify the record's block}
  RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, aRefNr, fsc_MarkDirty,
    OffsetInBlock, aRelMethod, fsoUpdate);
  Try
    {verify that the record's not deleted}
    DelLink := @RecordBlock^[OffsetInBlock];
    If fiVersion <= 1058 Then
      Begin
        If (DelLink^ = $FF) Then
          FSRaiseException(EfsServerException, fsStrResServer, fserrRecDeleted,
            [aFI^.fiName^, aRefNr.iLow, aRefNr.iHigh]);
      End
    Else
      Begin
        If GetFlags(DelLink^, frDeleteRecord) Then
          FSRaiseException(EfsServerException, fsStrResServer, fserrRecDeleted,
            [aFI^.fiName^, aRefNr.iLow, aRefNr.iHigh]);

        af := aFlags;
        If GetFlags(af, frDeleteRecord) Then // reset delete
          setflags(af, frDeleteRecord, False);
        If GetFlags(af, frUndeletedRecord) Then // reset undelete
          setflags(af, frUndeletedRecord, False);
        setflags(af, frDataRecord, True); // set normal
        DelLink^ := Byte(af); // set flag
      End;
  Finally
    aRelMethod(RecordBlock);
  End;
End;

Function FFTblGetFlagsRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aRefNr: TffInt64;
  fiVersion: Longint): Byte;
Var
  DelLink: PByte;
  OffsetInBlock: Longint;
  RecordBlock: PffBlock;
  RecBlockHdr: PffBlockHeaderData Absolute RecordBlock;
  aRelMethod: TffReleaseMethod;
Begin
  Result := 0;
  {read and verify the record's block}
  RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, aRefNr, fsc_MarkDirty,
    OffsetInBlock, aRelMethod, fsoUpdate);
  Try
    {verify that the record's not deleted}
    DelLink := @RecordBlock^[OffsetInBlock];
    If fiVersion <= 1058 Then
      Begin
        If (DelLink^ = $FF) Then
          FSRaiseException(EfsServerException, fsStrResServer, fserrRecDeleted,
            [aFI^.fiName^, aRefNr.iLow, aRefNr.iHigh])
        Else
          Result := DelLink^;
      End
    Else
      Begin
        If GetFlags(DelLink^, frDeleteRecord) Then
          FSRaiseException(EfsServerException, fsStrResServer, fserrRecDeleted,
            [aFI^.fiName^, aRefNr.iLow, aRefNr.iHigh])
        Else
          Result := DelLink^;
      End;
  Finally
    aRelMethod(RecordBlock);
  End;
End;

Procedure FFTblReadRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aRefNr: TffInt64;
  aRecData: PffByteArray;
  fiVersion: Longint;
  Var aFlag: Byte);
Var
  DelLink: PByte;
  OffsetInBlock: Longint;
  RecordBlock: PffBlock;
  RecBlockHdr: PffBlockHeaderData Absolute RecordBlock;
  aRelMethod: TffReleaseMethod;
Begin
  aFlag := 0;
  With aFI^ Do
    Begin
      {read and verify the record's block}
      RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, aRefNr, fsc_ReadOnly,
        OffsetInBlock, aRelMethod, fsoRead);
      Try
        {verify that the record's not deleted}
        DelLink := @RecordBlock^[OffsetInBlock];
        If fiVersion <= 1058 Then
          Begin
            If (DelLink^ = $FF) Then
              FSRaiseException(EfsServerException, fsStrResServer, fserrRecDeleted,
                [aFI^.fiName^, aRefNr.iLow, aRefNr.iHigh]);
          End
        Else
          Begin
            If GetFlags(DelLink^, frDeleteRecord) Then
              FSRaiseException(EfsServerException, fsStrResServer, fserrRecDeleted,
                [aFI^.fiName^, aRefNr.iLow, aRefNr.iHigh]);
          End;

        aFlag := DelLink^;
        {copy the record from the block}
        Move(RecordBlock^[OffsetInBlock + (aFI^.fiRecLenPlusTrailer - aFI^.fiRecordLength)], aRecData^, fiRecordLength);
      Finally
        aRelMethod(RecordBlock);
      End;
    End;
End;

Procedure FFTblReadNextRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aFromRefNr: TffInt64;
  Var aRefNr: TffInt64;
  aRecData: PffByteArray;
  aUndelete: boolean;
  OnlyDeleted: boolean;
  Var aFlag: Byte);
Var
  FileHeader: PffBlockHeaderFile;
  FinalOffset: Longint;
  FoundIt: boolean;
  NextBlock: TffWord32;
  OffsetInBlock: Longint;
  RecordBlock: PffBlock;
  RecBlockHdr: PffBlockHeaderData Absolute RecordBlock;
  TempI64: TffInt64;
  ThisBlock: TffWord32;
  aFHRelMethod,
    aBlkRelMethod: TffReleaseMethod;
Begin
  aFlag := 0;
  {first get the file header, block 0}
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_ReadOnly,
    aFHRelMethod, fsoNone));
  With FileHeader^ Do
    Try
      {cater for silly case}
      If aUndelete Then
        Begin
          If (bhfRecordCount = 0) And (bhfDelRecCount = 0) Then
            Begin
              ffInitI64(aRefNr);
              Exit;
            End;
        End
      Else
        Begin
          If (bhfRecordCount = 0) Then
            Begin
              ffInitI64(aRefNr);
              Exit;
            End;
        End;
      {read and verify the record's block, if reference is not zero}
      TempI64.iLow := 0;
      TempI64.iHigh := 0;
      If (ffCmpI64(aFromRefNr, TempI64) <> 0) Then
        RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, aFromRefNr, fsc_ReadOnly,
          OffsetInBlock, aBlkRelMethod, fsoRead)
          {otherwise, get the first data block}
      Else
        Begin
          ThisBlock := bhf1stDataBlock;
          RecordBlock := ReadVfyDataBlock(aFI, aTI, ThisBlock, fsc_ReadOnly,
            aBlkRelMethod, fsoRead);
          {note: although this calculation of the offset is likely to result in
                 a negative number, it'll be incremented immediately again}
          OffsetInBlock := fsc_BlockHeaderSizeData - aFI^.fiRecLenPlusTrailer;
        End;

      Try
        { Keep reading records and blocks until we've found the next active
          record. }
        FinalOffset := fsc_BlockHeaderSizeData +
          (bhfRecsPerBlock * aFI^.fiRecLenPlusTrailer);
        FoundIt := False;
        Repeat
          inc(OffsetInBlock, aFI^.fiRecLenPlusTrailer);
          {have we reached the end of this block?}
          If (OffsetInBlock >= FinalOffset) Then
            Begin
              {if there is no next block, break out of loop}
              NextBlock := RecBlockHdr^.bhdNextDataBlock;

              If (NextBlock = $FFFFFFFF) Then
                Break; {out of the repeat..until loop}

              ThisBlock := NextBlock;
              { Otherwise read the next block. }
              aBlkRelMethod(RecordBlock);
              RecordBlock := ReadVfyDataBlock(aFI, aTI, ThisBlock, fsc_ReadOnly,
                aBlkRelMethod, fsoRead);
              OffsetInBlock := fsc_BlockHeaderSizeData;
            End;
          If aUndelete Then
            Begin
              If bhfFSVersion >= 1059 Then
                Begin
                  If OnlyDeleted Then
                    Begin
                      If getflags(PByte(@RecordBlock^[OffsetInBlock])^, frDeleteRecord) Then
                        FoundIt := True;
                    End
                  Else
                    Begin
                      If (getflags(PByte(@RecordBlock^[OffsetInBlock])^, frDataRecord) Or
                        getflags(PByte(@RecordBlock^[OffsetInBlock])^, frDeleteRecord) Or
                        getflags(PByte(@RecordBlock^[OffsetInBlock])^, frUndeletedRecord))
                        And Not (PByte(@RecordBlock^[OffsetInBlock])^ = 255) Then
                        FoundIt := True;
                    End;
                End
              Else If ((PShortInt(@RecordBlock^[OffsetInBlock])^ = 0) Or
                getflags(PByte(@RecordBlock^[OffsetInBlock])^, frDataRecord) Or
                getflags(PByte(@RecordBlock^[OffsetInBlock])^, frDeleteRecord) Or
                getflags(PByte(@RecordBlock^[OffsetInBlock])^, frUndeletedRecord))
                And Not (PByte(@RecordBlock^[OffsetInBlock])^ = 255) Then
                FoundIt := True;
            End
          Else
            Begin
              If bhfFSVersion >= 1059 Then
                Begin
                  If (getflags(PByte(@RecordBlock^[OffsetInBlock])^, frDataRecord) Or
                    getflags(PByte(@RecordBlock^[OffsetInBlock])^, frUndeletedRecord))
                    And Not (PByte(@RecordBlock^[OffsetInBlock])^ = 255) Then
                    FoundIt := True;
                End
              Else If ((PShortInt(@RecordBlock^[OffsetInBlock])^ = 0) Or
                getflags(PByte(@RecordBlock^[OffsetInBlock])^, frDataRecord) Or
                getflags(PByte(@RecordBlock^[OffsetInBlock])^, frUndeletedRecord))
                And Not (PByte(@RecordBlock^[OffsetInBlock])^ = 255) Then
                FoundIt := True;
            End;

        Until FoundIt;

        aFlag := 0;
        If Not FoundIt Then
          Begin
            aRefNr.iLow := 0;
            aRefNr.iHigh := 0;
          End
        Else
          Begin
            TempI64.iLow := RecBlockHdr^.bhdThisBlock;
            TempI64.iHigh := 0;
            ffShiftI64L(TempI64, aFI^.fiLog2BlockSize, TempI64);
            ffI64AddInt(TempI64, OffsetInBlock, aRefNr);
            If getflags(PByte(@RecordBlock^[OffsetInBlock])^, frDeleteRecord) Then
              aFlag := frUndeletedRecord;
            If getflags(PByte(@RecordBlock^[OffsetInBlock])^, frUndeletedRecord) Then
              aFlag := aFlag + frUndeletedRecord;
            If getflags(PByte(@RecordBlock^[OffsetInBlock])^, frProtectDeleteRecord) Then
              aFlag := aFlag + frProtectDeleteRecord;
            If getflags(PByte(@RecordBlock^[OffsetInBlock])^, frProtectUpdateRecord) Then
              aFlag := aFlag + frProtectUpdateRecord;
            If getflags(PByte(@RecordBlock^[OffsetInBlock])^, frMarkAsBadRecord) Then
              aFlag := aFlag + frMarkAsBadRecord;

            If aRecData <> Nil Then
              Move(RecordBlock^[OffsetInBlock + (aFI^.fiRecLenPlusTrailer - aFI^.fiRecordLength)], aRecData^,
                aFI^.fiRecordLength);
          End;
      Finally
        If RecordBlock <> Nil Then
          aBlkRelMethod(RecordBlock);
      End;
    Finally
      aFHRelMethod(PffBlock(FileHeader));
    End;
End;
{--------}

Procedure FFTblReadPrevRecord(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aFromRefNr: TffInt64;
  Var aRefNr: TffInt64;
  aRecData: PffByteArray;
  Var aFlag: Byte);
Var
  FileHeader: PffBlockHeaderFile;
  FoundIt: boolean;
  OffsetInBlock: Longint;
  PrevBlock: TffWord32;
  RecordBlock: PffBlock;
  RecBlockHdr: PffBlockHeaderData Absolute RecordBlock;
  TempI64: TffInt64;
  aBlkRelMethod,
    aFHRelMethod: TffReleaseMethod;
Begin
  aflag := 0;
  {first get the file header, block 0}
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_ReadOnly,
    aFHRelMethod, fsoNone));
  With FileHeader^ Do
    Try
      {cater for silly case}
      If (bhfRecordCount = 0) Then
        Begin
          aRefNr.iLow := 0;
          aRefNr.iHigh := 0;
          Exit;
        End;
      {read and verify the record's block, if reference is not zero}
      TempI64.iLow := 0;
      TempI64.iHigh := 0;
      If (ffCmpI64(aFromRefNr, TempI64) <> 0) Then
        RecordBlock := ReadVfyRefNrDataBlock(aFI, aTI, aFromRefNr, fsc_ReadOnly,
          OffsetInBlock, aBlkRelMethod, fsoRead)
          {otherwise, get the last data block}
      Else
        Begin
          RecordBlock := ReadVfyDataBlock(aFI, aTI, bhfLastDataBlock,
            fsc_ReadOnly, aBlkRelMethod, fsoRead);
          OffsetInBlock := fsc_BlockHeaderSizeData +
            (bhfRecsPerBlock * aFI^.fiRecLenPlusTrailer);
        End;
      Try
        { Keep reading records and blocks until we've found the previous active
          record. }
        FoundIt := False;
        Repeat
          dec(OffsetInBlock, aFI^.fiRecLenPlusTrailer);
          {have we reached the end of this block?}
          If (OffsetInBlock < fsc_BlockHeaderSizeData) Then
            Begin

              PrevBlock := RecBlockHdr^.bhdPrevDataBlock;
              {if there is no previous block, break out of loop}

              If (PrevBlock = $FFFFFFFF) Then
                Break; {out of the repeat..until loop}

              {otherwise read the next block}
              aBlkRelMethod(RecordBlock);
              RecordBlock := ReadVfyDataBlock(aFI, aTI, PrevBlock, fsc_ReadOnly,
                aBlkRelMethod, fsoRead);
              OffsetInBlock := fsc_BlockHeaderSizeData +
                ((bhfRecsPerBlock - 1) * aFI^.fiRecLenPlusTrailer); {!!.11}
            End;
          If bhfFSVersion >= 1059 Then
            Begin
              If (getflags(PByte(@RecordBlock^[OffsetInBlock])^, frDataRecord) Or
                getflags(PByte(@RecordBlock^[OffsetInBlock])^, frUndeletedRecord))
                And Not (PByte(@RecordBlock^[OffsetInBlock])^ = 255) Then
                FoundIt := True;
            End
          Else If ((PShortInt(@RecordBlock^[OffsetInBlock])^ = 0) Or
            getflags(PByte(@RecordBlock^[OffsetInBlock])^, frDataRecord) Or
            getflags(PByte(@RecordBlock^[OffsetInBlock])^, frUndeletedRecord))
            And Not (PByte(@RecordBlock^[OffsetInBlock])^ = 255) Then
            FoundIt := True;

        Until FoundIt;
        If Not FoundIt Then
          Begin
            aRefNr.iLow := 0;
            aRefNr.iHigh := 0
          End
        Else
          Begin
            TempI64.iLow := OffsetInBlock;
            TempI64.iHigh := 0;
            aRefNr := TempI64;
            ffI64AddInt(aRefNr, (RecBlockHdr^.bhdThisBlock Shl aFI^.fiLog2BlockSize), aRefNr);
            If getflags(PByte(@RecordBlock^[OffsetInBlock])^, frUndeletedRecord) Then
              aFlag := frUndeletedRecord
            Else
              AFlag := 0;
            If getflags(PByte(@RecordBlock^[OffsetInBlock])^, frProtectDeleteRecord) Then
              aFlag := aFlag + frProtectDeleteRecord;
            If getflags(PByte(@RecordBlock^[OffsetInBlock])^, frProtectUpdateRecord) Then
              aFlag := aFlag + frProtectUpdateRecord;
            If getflags(PByte(@RecordBlock^[OffsetInBlock])^, frMarkAsBadRecord) Then
              aFlag := aFlag + frMarkAsBadRecord;

            Move(RecordBlock^[OffsetInBlock + (aFI^.fiRecLenPlusTrailer - aFI^.fiRecordLength)], aRecData^,
              aFI^.fiRecordLength);
          End;
      Finally
        aBlkRelMethod(RecordBlock);
      End;
    Finally
      aFHRelMethod(PffBlock(FileHeader));
    End;
End;
{--------}

{====================================================================}

{===Record information===============================================}

Procedure FFTblGetRecordInfo(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Var aInfo: TffRecordInfo);
Var
  FileHeader: PffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
Begin
  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_ReadOnly,
    aRelMethod, fsoNone));
  { Return the information required. }
  With FileHeader^, aInfo Do
    Begin
      riRecLength := bhfRecordLength;
      riRecCount := bhfRecordCount;
      riDelRecCount := bhfDelRecCount;
      riRecsPerBlock := bhfRecsPerBlock;
    End;

  aRelMethod(PffBlock(FileHeader));
End;
{--------}

Function FFTblNextAutoInc(aFI: PffFileInfo; aTI: PffTransInfo): Int64;
Var
  FileHeader: PffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
Begin
  { Assumption: Transaction started.  Transaction committed
    or rolled back higher up in the call stack. }

  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty,
    aRelMethod, fsoNone));

  { Return the information required. }
  With FileHeader^ Do
    Begin
      Result := bhfAutoInc64Value + bhfAutoInc64StepValue;
      bhfAutoInc64Value := Result;
    End;

  aRelMethod(PffBlock(FileHeader));
End;

Function FFTblLastAutoInc(aFI: PffFileInfo; aTI: PffTransInfo): Int64;
Var
  FileHeader: PffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
Begin
  { Assumption: Transaction started.  Transaction committed
    or rolled back higher up in the call stack. }

  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty,
    aRelMethod, fsoNone));

  { Return the information required. }
  With FileHeader^ Do
    Begin
      Result := bhfAutoInc64Value;
    End;

  aRelMethod(PffBlock(FileHeader));
End;
{--------}

Function FFTblReadAutoInc(aFI: PffFileInfo; aTI: PffTransInfo; Var aStep: Longint): Int64;
Var
  FileHeader: PffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
Begin
  { Assumption: Transaction started.  Transaction committed
    or rolled back higher up in the call stack. }

  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_ReadOnly,
    aRelMethod, fsoNone));

  { Return the information required. }
  Result := FileHeader^.bhfAutoInc64Value;
  aStep := FileHeader^.bhfAutoInc64StepValue;
  aRelMethod(PffBlock(FileHeader));
End;

Function FFTblReadMaxRecords(aFI: PffFileInfo; aTI: PffTransInfo): Longint;
Var
  FileHeader: PffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
Begin
  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_ReadOnly,
    aRelMethod, fsoNone));

  { Return the information required. }
  Result := FileHeader^.bhfMaxRecords;
  aRelMethod(PffBlock(FileHeader));
End;

Function FFTblReadTableFlags(aFI: PffFileInfo; aTI: PffTransInfo): Word;
Var
  FileHeader: PffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
Begin
  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_ReadOnly,
    aRelMethod, fsoNone));

  { Return the information required. }
  Result := FileHeader^.bhfTableFlags;
  aRelMethod(PffBlock(FileHeader));
End;

Function FFTblReadRecordCount(aFI: PffFileInfo; aTI: PffTransInfo): Longint;
Var
  FileHeader: PffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
Begin
  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_ReadOnly,
    aRelMethod, fsoNone));

  { Return the information required. }
  Result := FileHeader^.bhfRecordCount;
  aRelMethod(PffBlock(FileHeader));
End;

{--------}

Procedure FFTblDelAutoIncValue(aFI: PffFileInfo; aTI: PffTransInfo;
  aValue: Int64);
Var
  FileHeader: PffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
Begin
  { Assumption: Transaction started. }

  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty,
    aRelMethod, fsoNone));

  { Return the information required. }
  With FileHeader^ Do
    Begin
      If (aValue = bhfAutoInc64Value) Then
        dec(bhfAutoInc64Value);
    End;

  aRelMethod(PffBlock(FileHeader));
End;
{--------}

Procedure FFTblSetAutoInc(aFI: PffFileInfo; aTI: PffTransInfo;
  aValue: Int64; Step: Longint);
Var
  FileHeader: PffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
  Rflags: Word;
Begin
  { Assumption: Transaction started. }

  { First get the file header, block 0, mark dirty. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty,
    aRelMethod, fsoNone));
  Rflags := FileHeader^.bhfTableFlags;
  If ((Rflags And fsTableDontChangeAutoInc) = 0) Then
    Begin
      //Result := DBIERR_NOTSUFFFIELDRIGHTS;
  { Set the new seed value }
      FileHeader^.bhfAutoInc64Value := aValue;
      FileHeader^.bhfAutoInc64StepValue := Step;
    End;
  aRelMethod(PffBlock(FileHeader));
End;

Procedure FFTblSetMaxRecords(aFI: PffFileInfo; aTI: PffTransInfo;
  aValue: Longint);
Var
  FileHeader: PffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
  Rflags: Word;
Begin
  { Assumption: Transaction started. }

  { First get the file header, block 0, mark dirty. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty,
    aRelMethod, fsoNone));
  Rflags := FileHeader^.bhfTableFlags;
  If ((Rflags And fsTableDontChangeMaxRecords) = 0) Then
    Begin
      { Set the new seed value }
      FileHeader^.bhfMaxRecords := aValue;
    End;
  aRelMethod(PffBlock(FileHeader));
End;

Procedure FFTblSetTableFlags(aFI: PffFileInfo; aTI: PffTransInfo;
  aValue: Word);
Var
  FileHeader: PffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
Begin
  { Assumption: Transaction started. }

  { First get the file header, block 0, mark dirty. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty,
    aRelMethod, fsoNone));

  { Set the new seed value }
  FileHeader^.bhfTableFlags := aValue;
  aRelMethod(PffBlock(FileHeader));
End;

Procedure FFTblSetTablePassword(aFI: PffFileInfo; aTI: PffTransInfo;
  aValue: Longword);
Var
  FileHeader: PffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
  S1: String;
  I: Integer;

Begin
  If aValue = 4294967295 Then
    FSRaiseException(EfsException, fsStrResServer, 50100,
      [aFI^.fiName^, 4294967295]);
  { Assumption: Transaction started. }
  { First get the file header, block 0, mark dirty. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty, aRelMethod, fsoNone));
  Try
    DecodeBuffer(FileHeader^.bhfInfoSec1.bhfPasswdTable, SizeOf(FileHeader^.bhfInfoSec1.bhfPasswdTable), 1588);
    DecodeBuffer(FileHeader^.bhfInfoSec1, SizeOf(FileHeader^.bhfInfoSec1), 1188);

    For i := 1 To 16 Do
      Begin
        FileHeader^.bhfInfoSec1.bhfPasswdTable[i] := inttohex(i, 2)[2];
        FileHeader^.bhfInfoSec1.bhfPasswdTableBlanc[i] := inttohex(i, 2)[2];
      End;
    FileHeader^.bhfInfoSec1.bhfPasswdT[1] := 'P';
    FileHeader^.bhfInfoSec1.bhfPasswdT[2] := 'X';
    FileHeader^.bhfInfoSec1.bhfPasswdT[3] := 'P';
    FileHeader^.bhfInfoSec1.bhfPasswdT[4] := 'V';
    FileHeader^.bhfInfoSec1.bhfPasswdT[5] := '1';
    FileHeader^.bhfInfoSec1.bhfPasswdT[6] := '@';
    FileHeader^.bhfInfoSec1.bhfPasswdT[7] := '$';
    FileHeader^.bhfInfoSec1.bhfPasswdT[8] := 'Z';
    S1 := '';
    If aValue = 0 Then
      Begin
        // empty password
        S1 := inttohex(FSCalcShStrELFHash(DateTimeToStr(Now)), 8);
        S1 := S1 + inttohex(FSCalcShStrELFHash(Inttohex(FileHeader^.bhfRecordLength, 8)), 8);
        S1 := S1 + inttohex(FSCalcShStrELFHash(Inttohex(FileHeader^.bhfFieldCount, 8)), 8);
        S1 := S1 + inttohex(FSCalcShStrELFHash(Inttohex(FileHeader^.bhfIndexCount, 8)), 8);
        S1 := S1 + inttohex(FSCalcShStrELFHash(Inttohex(FileHeader^.bhfBlockSize, 8)), 8);
        S1 := 'XZ' + inttohex(FSCalcShStrELFHash(S1), 14);

        For i := 16 Downto 1 Do
          FileHeader^.bhfInfoSec1.bhfPasswdTable[i] := S1[17 - i];
        For i := 16 Downto 1 Do
          FileHeader^.bhfInfoSec1.bhfPasswdTableBlanc[i] := S1[17 - i];
      End
    Else
      Begin
        S1 := inttohex(aValue, 16);

        For i := 16 Downto 1 Do
          FileHeader^.bhfInfoSec1.bhfPasswdTable[i] := S1[17 - i];
        For i := 16 Downto 1 Do
          FileHeader^.bhfInfoSec1.bhfPasswdTableBlanc[i] := S1[17 - i];
        FileHeader^.bhfInfoSec1.bhfPasswdTableBlanc[7] := '9';
        FileHeader^.bhfInfoSec1.bhfPasswdTableBlanc[8] := 'X';
        FileHeader^.bhfInfoSec1.bhfPasswdTableBlanc[9] := '9';
        FileHeader^.bhfInfoSec1.bhfPasswdTableBlanc[10] := 'Y';
      End;
    DecodeBuffer(FileHeader^.bhfInfoSec1, SizeOf(FileHeader^.bhfInfoSec1), 1188);
    DecodeBuffer(FileHeader^.bhfInfoSec1.bhfPasswdTable, SizeOf(FileHeader^.bhfInfoSec1.bhfPasswdTable), 1588);
  Finally
    aRelMethod(PffBlock(FileHeader));
  End;
End;

Function FFTblReadTablePassword(aFI: PffFileInfo; aTI: PffTransInfo): Longword;
Var
  FileHeader: PffBlockHeaderFile;
  FileHeader1: TffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
  tmp: Tctr8c;
  tmp1: Tctr8c;
  tmp2: Tctr8c;
  j: Integer;

  Procedure ReadPasswd;
  Var
    S1, S2: String;
    I: Integer;
  Begin
    S1 := '';
    S2 := '';
    For i := 16 Downto 1 Do
      S1 := S1 + FileHeader1.bhfInfoSec1.bhfPasswdTable[i];
    For i := 16 Downto 1 Do
      S2 := S2 + FileHeader1.bhfInfoSec1.bhfPasswdTableBlanc[i];
    If S1 = S2 Then
      Result := 0
    Else
      Begin
        Try
          Result := HexToInt(S1);
        Except
          Result := 4294967295;
          FSRaiseException(EfsException, fsStrResServer, 50100,
            [aFI^.fiName^, 4294967295]);
        End;
      End;
  End;

Begin
  Result := 0;
  If aFI^.fiForServer Then Exit;
  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_ReadOnly, aRelMethod, fsoNone));
  Try
    Move(FileHeader^, FileHeader1, SizeOf(TffBlockHeaderFile));
    tmp[1] := 'P';
    tmp[2] := 'X';
    tmp[3] := 'P';
    tmp[4] := 'V';
    tmp[5] := '1';
    tmp[6] := '@';
    tmp[7] := '$';
    tmp[8] := 'Z';
    For j := 1 To 8 Do
      tmp2[j] := #0;
    tmp1 := FileHeader1.bhfInfoSec1.bhfPasswdT;
    DecodeBuffer(FileHeader1.bhfInfoSec1.bhfPasswdTable, SizeOf(FileHeader1.bhfInfoSec1.bhfPasswdTable), 1588);
    DecodeBuffer(FileHeader1.bhfInfoSec1, SizeOf(FileHeader1.bhfInfoSec1), 1188);
    If (FileHeader1.bhfFSVersion >= 1045) Then
      Begin
        If (tmp = FileHeader1.bhfInfoSec1.bhfPasswdT) Then
          ReadPasswd
        Else
          Begin
            Result := 4294967295;
            FSRaiseException(EfsException, fsStrResServer, 50100,
              [aFI^.fiName^, 4294967295]);
          End;
      End
    Else If (FileHeader1.bhfFSVersion < 1045) Then
      Begin
        If (tmp = FileHeader1.bhfInfoSec1.bhfPasswdT) Then
          ReadPasswd
        Else If tmp1 <> tmp2 Then
          Begin
            Result := 4294967295;
            FSRaiseException(EfsException, fsStrResServer, 50100,
              [aFI^.fiName^, 4294967295]);
          End;
      End;
  Finally
    aRelMethod(PffBlock(FileHeader));
  End;
End;

Procedure FFTblSetTablePasswordRest(aFI: PffFileInfo; aTI: PffTransInfo;
  aValue: Longword);
Var
  FileHeader: PffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
  S1: String;
  I: Integer;

Begin
  If aValue = 4294967295 Then
    FSRaiseException(EfsException, fsStrResServer, 50100,
      [aFI^.fiName^, 4294967295]);
  { Assumption: Transaction started. }
  { First get the file header, block 0, mark dirty. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty, aRelMethod, fsoNone));
  Try
    DecodeBuffer(FileHeader^.bhfInfoSec2.bhfPasswdRest, SizeOf(FileHeader^.bhfInfoSec2.bhfPasswdRest), 1589);
    DecodeBuffer(FileHeader^.bhfInfoSec2, SizeOf(FileHeader^.bhfInfoSec2), 1189);

    For i := 1 To 16 Do
      Begin
        FileHeader^.bhfInfoSec2.bhfPasswdRest[i] := inttohex(i, 2)[2];
        FileHeader^.bhfInfoSec2.bhfPasswdRestBlanc[i] := inttohex(i, 2)[2];
      End;
    FileHeader^.bhfInfoSec2.bhfPasswdT[1] := 'P';
    FileHeader^.bhfInfoSec2.bhfPasswdT[2] := 'X';
    FileHeader^.bhfInfoSec2.bhfPasswdT[3] := 'P';
    FileHeader^.bhfInfoSec2.bhfPasswdT[4] := 'V';
    FileHeader^.bhfInfoSec2.bhfPasswdT[5] := '1';
    FileHeader^.bhfInfoSec2.bhfPasswdT[6] := '@';
    FileHeader^.bhfInfoSec2.bhfPasswdT[7] := '$';
    FileHeader^.bhfInfoSec2.bhfPasswdT[8] := 'Z';
    S1 := '';
    If aValue = 0 Then
      Begin
        // empty password
        S1 := inttohex(FSCalcShStrELFHash(DateTimeToStr(Now)), 8);
        S1 := S1 + inttohex(FSCalcShStrELFHash(Inttohex(FileHeader^.bhfRecordLength, 8)), 8);
        S1 := S1 + inttohex(FSCalcShStrELFHash(Inttohex(FileHeader^.bhfFieldCount, 8)), 8);
        S1 := S1 + inttohex(FSCalcShStrELFHash(Inttohex(FileHeader^.bhfIndexCount, 8)), 8);
        S1 := S1 + inttohex(FSCalcShStrELFHash(Inttohex(FileHeader^.bhfBlockSize, 8)), 8);
        S1 := 'XZ' + inttohex(FSCalcShStrELFHash(S1), 14);

        For i := 16 Downto 1 Do
          FileHeader^.bhfInfoSec2.bhfPasswdRest[i] := S1[17 - i];
        For i := 16 Downto 1 Do
          FileHeader^.bhfInfoSec2.bhfPasswdRestBlanc[i] := S1[17 - i];
      End
    Else
      Begin
        S1 := inttohex(aValue, 16);

        For i := 16 Downto 1 Do
          FileHeader^.bhfInfoSec2.bhfPasswdRest[i] := S1[17 - i];
        For i := 16 Downto 1 Do
          FileHeader^.bhfInfoSec2.bhfPasswdRestBlanc[i] := S1[17 - i];
        FileHeader^.bhfInfoSec2.bhfPasswdRestBlanc[7] := '9';
        FileHeader^.bhfInfoSec2.bhfPasswdRestBlanc[8] := 'X';
        FileHeader^.bhfInfoSec2.bhfPasswdRestBlanc[9] := '9';
        FileHeader^.bhfInfoSec2.bhfPasswdRestBlanc[10] := 'Y';
      End;
    DecodeBuffer(FileHeader^.bhfInfoSec2, SizeOf(FileHeader^.bhfInfoSec2), 1189);
    DecodeBuffer(FileHeader^.bhfInfoSec2.bhfPasswdRest, SizeOf(FileHeader^.bhfInfoSec2.bhfPasswdRest), 1589);
  Finally
    aRelMethod(PffBlock(FileHeader));
  End;
End;

Function FFTblReadTablePasswordRest(aFI: PffFileInfo; aTI: PffTransInfo): Longword;
Var
  FileHeader: PffBlockHeaderFile;
  FileHeader1: TffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
  tmp: Tctr8c;
  tmp1: Tctr8c;
  tmp2: Tctr8c;
  j: Integer;

  Procedure ReadPasswd;
  Var
    S1, S2: String;
    I: Integer;
  Begin
    S1 := '';
    S2 := '';
    For i := 16 Downto 1 Do
      S1 := S1 + FileHeader1.bhfInfoSec2.bhfPasswdRest[i];
    For i := 16 Downto 1 Do
      S2 := S2 + FileHeader1.bhfInfoSec2.bhfPasswdRestBlanc[i];
    If S1 = S2 Then
      Result := 0
    Else
      Begin
        Try
          Result := HexToInt(S1);
        Except
          Result := 4294967295;
          FSRaiseException(EfsException, fsStrResServer, 50100,
            [aFI^.fiName^, 4294967295]);
        End;
      End;
  End;

Begin
  Result := 0;
  If aFI^.fiForServer Then Exit;
  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_ReadOnly, aRelMethod, fsoNone));
  Try
    Move(FileHeader^, FileHeader1, SizeOf(TffBlockHeaderFile));
    tmp[1] := 'P';
    tmp[2] := 'X';
    tmp[3] := 'P';
    tmp[4] := 'V';
    tmp[5] := '1';
    tmp[6] := '@';
    tmp[7] := '$';
    tmp[8] := 'Z';
    For j := 1 To 8 Do
      tmp2[j] := #0;
    tmp1 := FileHeader1.bhfInfoSec2.bhfPasswdT;
    DecodeBuffer(FileHeader1.bhfInfoSec2.bhfPasswdRest, SizeOf(FileHeader1.bhfInfoSec2.bhfPasswdRest), 1589);
    DecodeBuffer(FileHeader1.bhfInfoSec2, SizeOf(FileHeader1.bhfInfoSec2), 1189);
    If (FileHeader1.bhfFSVersion >= 1045) Then
      Begin
        If (tmp = FileHeader1.bhfInfoSec2.bhfPasswdT) Then
          ReadPasswd
        Else
          Begin
            Result := 4294967295;
            FSRaiseException(EfsException, fsStrResServer, 50100,
              [aFI^.fiName^, 4294967295]);
          End;
      End
    Else If (FileHeader1.bhfFSVersion < 1045) Then
      Begin
        If (tmp = FileHeader1.bhfInfoSec2.bhfPasswdT) Then
          ReadPasswd
        Else If tmp1 <> tmp2 Then
          Begin
            Result := 4294967295;
            FSRaiseException(EfsException, fsStrResServer, 50100,
              [aFI^.fiName^, 4294967295]);
          End;
      End;
  Finally
    aRelMethod(PffBlock(FileHeader));
  End;
End;

Function FFTblReadTableDBID(aFI: PffFileInfo; aTI: PffTransInfo): Longword;
Var
  FileHeader: PffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
  {
  DecodeBuffer(FileHeader^.bhfInfoSec3, SizeOf(FileHeader^.bhfInfoSec3), 1128);
          DecodeBuffer(FileHeader^.bhfInfoSec4, SizeOf(FileHeader^.bhfInfoSec4), 1028);
  }

  Procedure ReadHID;
    {
    Var
      S1, S2, S3, S4: String;
      I, j: Integer;
      tmp1: TArray92;
    }
  Begin
    {S1 := '';
    S2 := '';
    tmp1 := FileHeader^.bhfSignatureDBID;
    For i := 1 To 23 Do
      Begin
        FileHeader^.bhfSignatureDBID[i] := tmp1[i];
        FileHeader^.bhfSignatureDBID[i + 23] := tmp1[i + 23];
        FileHeader^.bhfSignatureDBID[i + 23 + 23] := tmp1[i + 23 + 23];
        FileHeader^.bhfSignatureDBID[i + 23 + 23 + 23] := tmp1[i + 23 + 23 + 23];
      End ;
    Try
      J := HexToInt(FileHeader^.bhfSignatureDBID[1] + FileHeader^.bhfSignatureDBID[2]);
    Except
      Result := 4294967295;
      FSRaiseException(EfsException, fsStrResServer, 50100,
        [aFI^.fiName^, 4294967295]);
    End ;
    For i := 3 To j + 2 Do
      S1 := S1 + FileHeader^.bhfSignatureDBID[i];

    Try
      Result := HexToInt(S1);
    Except
      Result := 4294967295;
      FSRaiseException(EfsException, fsStrResServer, 50100,
        [aFI^.fiName^, 4294967295]);
    End ;  }
  End;

Begin
  Result := 0;
  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_ReadOnly, aRelMethod, fsoNone));
  Try
    {If (((FileHeader^.bhfUsedOldTable And 256) <> 0) And ((FileHeader^.bhfUsedOldTable And 2048) <> 0)
      And ((FileHeader^.bhfUsedOldTable And 128) <> 0))
      Or (FileHeader^.bhfFSVersion >= 1045) then
      ReadHID
    Else
      Result := 0;}
  Finally
    aRelMethod(PffBlock(FileHeader));
  End;
End;

Procedure FFTblSetTableDBID(aFI: PffFileInfo; aTI: PffTransInfo;
  aValue: Longword);
Var
  FileHeader: PffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
  //  S1, S2, S3, S4: String;
  //  I, j: Integer;
  //  i6: Int64;
  //  Tmp1: TArray92;
Begin
  // High(LongWord) = 4,294,967,295
  {$IFDEF DCC6OrLater}
  If aValue = High(Longword) Then
    FSRaiseException(EfsException,
      fsStrResServer, 50101,
      [aFI^.fiName^, High(Longword)]);
  {$ELSE}
  If aValue = 4294967295 Then
    FSRaiseException(EfsException,
      fsStrResServer, 50101,
      [aFI^.fiName^, 4294967295]);
  {$ENDIF}
  { Assumption: Transaction started. }
  { First get the file header, block 0, mark dirty. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty, aRelMethod, fsoNone));
  Try
    {FileHeader^.bhfUsedOldTable := FileHeader^.bhfUsedOldTable + 128 + 256 + 2048;
    For i := 1 To 92 Do
      FileHeader^.bhfSignatureDBID[i] := inttohex(i, 2)[2];
    If aValue > 0 then
      Begin
        S1 := inttohex(aValue, 16);
        // size passwd
        S4 := IntToHex(length(S1), 2);

        S2 := S4 + S1;
        S3 := S2 + '+&%$#@!X';
        j := length(S2);
        For i := 1 To j Do
          FileHeader^.bhfSignatureDBID[i] := S2[i];
        tmp1 := FileHeader^.bhfSignatureDBID;
        For i := 1 To 23 Do
          Begin
            FileHeader^.bhfSignatureDBID[i] := tmp1[i];
            FileHeader^.bhfSignatureDBID[i + 23] := tmp1[i + 23];
            FileHeader^.bhfSignatureDBID[i + 23 + 23] := tmp1[i + 23 + 23];
            FileHeader^.bhfSignatureDBID[i + 23 + 23 + 23] := tmp1[i + 23 + 23 + 23];
          End ;
      End ; }
  Finally
    aRelMethod(PffBlock(FileHeader));
  End;
End;

{====================================================================}

{$IFDEF DebugLog}
Initialization
  aLog := TffEventLog.Create(Nil);
  aLog.FileName := '.\fstbData.lg';
  aLog.Enabled := True;

Finalization
  aLog.Free;
  {$ENDIF}

End.

