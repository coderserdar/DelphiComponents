{*********************************************************}
{* FlashFiler: Table access - general & helper routines  *}
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

Unit fstablehelper;

Interface

Uses
  Windows,
  SysUtils,
  Classes,
  fsconst,
  fsllbase,
  fssrlock,
  fssrmgr,
  fsllexcp,
  fssrbase,
  fsfile,
  fssrintf,
  fslldict;

{---Field comparison routine---}
Function FFKeyCompareField(Const Key1, Key2;
  FieldType: TfsFieldType;
  FieldLen: Integer;
  NoCase: Boolean;
  PartLen: integer = 0): Integer;
{-Treat Key1 and Key2 as Filer typed fields, compare}

{---Table helper routines---}
Function FFTblHlpGetNewBlock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Var aReleaseMethod: TffReleaseMethod;
  aOperation: TfsOperation)
  : PffBlock;
{-Return a new block, pre-marked as dirty.  Exclusively locks the
  file header block and recycled block (if one is available). }
Procedure FFTblHlpDelete(Const aPath: TffPath;
  Const aTableName: TfsTableName;
  Const aDict: TFSInfoDict);
{-Delete all files associated with a table}
Procedure FFTblHlpDeleteBlock(aFI: PffFileInfo;
  aFileHeader: PffBlockHeaderFile;
  aBlock: PffBlock);
{-Delete the block, add it to the deleted block chain.  Assumes the block
  has been exclusively locked. }

Procedure FFTblHlpRename(Const aPath: TffPath;
  Const aTableName: TfsTableName;
  Const aNewName: TfsTableName;
  Const aDict: TFSInfoDict);
{-Renames all files associated with a table}

{---Buffer manager access routines---}
Function FFBMAddBlock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aBlockNum: TffWord32;
  Var aReleaseMethod: TffReleaseMethod;
  aOperation: TfsOperation): PffBlock;
{-Return address of a new block}
Procedure FFBMDirtyBlock(aFI: PffFileInfo;
  Const aBlockNum: TffWord32;
  aTI: PffTransInfo;
  Var aModifiableBlock: PffBlock);
{-Mark a block dirty. The block *must* be in the buffer already.
  Returns a pointer to the modifiable copy of the block in output variable
  aModifiableBlock. The calling function *MUST*
  use the modifiable copy instead of the read-only copy it currently
  possesses. }
Function FFBMGetBlock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aBlockNum: TffWord32;
  Const aMarkDirty: Boolean;
  Var aReleaseMethod: TffReleaseMethod;
  aOperation: TfsOperation): PffBlock;
{-Retrieves the specified file block.  If aMarkDirty is False then returns
  a pointer to the read-only copy of the block.  If aMarkDirty is True then
  returns a modifiable copy of the block. }
Function FFBMGetFileHeaderBlock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aMarkDirty: Boolean;
  Var aReleaseMethod: TffReleaseMethod): PffBlock;
{-Reads a file header (block 0) into buffer, returns address.
  Note: this routine verifies the header block to be a valid header
        block, so it must be used IMMEDIATELY after opening a file;
        also sets the block size for the file}
Procedure FFBMRemoveFile(aFI: PffFileInfo);
{-Mark a file's blocks in the buffer manager as available for reuse}
Procedure FFBMUnlockBlock(aFI: PffFileInfo; aBlockNum: TffWord32);
{-Unlock a block making it available for reuse immediately.}

{---Lock manager access routines---}
Procedure FSAcquireRecordLock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aRefNum: TffInt64;
  Const aLockType: TfsSrcLockType;
  Const aDatabaseID: TffDatabaseID; {!!.10}
  Const aCursorID: TffCursorID; {!!.02}
  Const aConditional: Boolean;
  Const aUser: Boolean); {!!.02}
{ Use this procedure to obtain a lock on a record.  If the lock is not
  granted then an exception is raised. }
Procedure FSUserAcquireRecordLock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aRefNum: TffInt64;
  Const aLockType: TfsSrcLockType;
  Const aDatabaseID: TffDatabaseID;
  Const aCursorID: TffCursorID;
  Const aConditional: Boolean;
  Const aUserLock: TfsUserRecLocking;
  Const aUser: Boolean);
{ For User. Use this procedure to obtain a lock on a record.  If the lock is not
granted then an exception is raised. }
{Begin !!.10}
Procedure FFRelaxRecordLock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aCursorID: TffCursorID;
  Const aRefNum: TffInt64);
{ Used by data modification operations to make a modified record available
  to other cursors within the same transaction. }
{End !!.10}

Procedure FFRelRecordLock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aRefNum: TffInt64;
  Const aDatabaseID: TffDatabaseID; {!!.10}
  Const aCursorID: TffCursorID);
{ Use this procedure to release an existing record lock. }

Procedure FFRelRecordLockAll(aFI: PffFileInfo; aTI: PffTransInfo);

Implementation

Uses
  fsserverclass;

Resourcestring
  ffcRecord = 'record %d:%d (high:low)';

  {== Field comparison routine =========================================}

Function FFKeyCompareField(Const Key1, Key2;
  FieldType: TfsFieldType;
  FieldLen: Integer;
  NoCase: Boolean;
  PartLen: integer = 0)
  : Integer;
Var
  CompareData: TffCompareData;
  Ch1, Ch2: AnsiChar;
Begin
  CompareData.cdPartLen := PartLen;
  {we'll use an ascending type comparison here, our caller will flip
   the sign of our result if required}
  CompareData.cdAscend := True;
  CompareData.cdNoCase := NoCase;
  Case FieldType Of
    fstBoolean:
      Begin
        {assumption: True is greater than False}
        If boolean(Key1) Then
          If boolean(Key2) Then
            Result := 0
          Else
            Result := 1
        Else If boolean(Key2) Then
          Result := -1
        Else
          Result := 0;
      End;
    fstSingleChar:
      Begin
        If NoCase Then
          Begin
            Ch1 := upcase(AnsiChar(Key1));
            Ch2 := upcase(AnsiChar(Key2));
            Result := FFCmpB(Byte(Ch1), Byte(Ch2));
          End
        Else
          Result := FFCmpB(Byte(Key1), Byte(Key2));
      End;
    fstSingleWideChar:
      Begin
        CompareData.cdKeyLen := 1;
        Result := FFKeyCompareWideChar(Key1, Key2, @CompareData);
      End;
    fstUInt8:
      Begin
        Result := FFCmpB(Byte(Key1), Byte(Key2));
      End;
    fstUInt16:
      Begin
        Result := FFCmpW(TffWord16(Key1), TffWord16(Key2));
      End;
    fstUInt32:
      Begin
        Result := FFCmpDW(TffWord32(Key1), TffWord32(Key2));
      End;
    fstInt8:
      Begin
        Result := FFCmpI8(Shortint(Key1), Shortint(Key2));
      End;
    fstInt16:
      Begin
        Result := FFCmpI16(Smallint(Key1), Smallint(Key2));
      End;
    fstInt32:
      Begin
        Result := FFCmpI32(Longint(Key1), Longint(Key2));
      End;
    fstAutoInc32:
      Begin
        Result := FFCmpDW(Longint(Key1), Longint(Key2));
      End;
    fstSingle:
      Begin
        If Single(Key1) = Single(Key2) Then
          Result := 0
        Else If Single(Key1) > Single(Key2) Then
          Result := 1
        Else
          Result := -1;
      End;
    fstDouble:
      Begin
        If Double(Key1) = Double(Key2) Then
          Result := 0
        Else If Double(Key1) > Double(Key2) Then
          Result := 1
        Else
          Result := -1;
      End;
    fstExtended:
      Begin
        If Extended(Key1) = Extended(Key2) Then
          Result := 0
        Else If Extended(Key1) > Extended(Key2) Then
          Result := 1
        Else
          Result := -1;
      End;
    fstInt64, fstAutoInc64, fstRecVersion:
      Begin
        If Int64(Key1) = Int64(Key2) Then
          Result := 0
        Else If Int64(Key1) > Int64(Key2) Then
          Result := 1
        Else
          Result := -1;
      End;
    fstCurrency:
      Begin
        If Int64(Key1) = Int64(Key2) Then
          Result := 0
        Else If Int64(Key1) > Int64(Key2) Then
          Result := 1
        Else
          Result := -1;
      End;
    //fstBcd :;
    fstDate,
      fstTime:
      Begin
        Result := FFCmpI32(Longint(Key1), Longint(Key2));
      End;
    fstDateTime:
      Begin
        If Double(Key1) = Double(Key2) Then
          Result := 0
        Else If Double(Key1) > Double(Key2) Then
          Result := 1
        Else
          Result := -1;
      End;
    fstBLOB,
      fstBLOBMemo,
      fstBLOBGraphic:
      Begin
        Result := 0; {a spurious value}
        FSRaiseExceptionNoData(EfsServerException, fsStrResServer, fserrCannotCompare);
      End;
    fstArrayUInt8:
      Begin
        CompareData.cdKeyLen := FieldLen;
        Result := FFKeyCompareBytes(Key1, Key2, @CompareData);
      End;
    fstArrayUInt16:
      Begin
        CompareData.cdKeyLen := FieldLen;
        Result := FFKeyCompareWord(Key1, Key2, @CompareData);
      End;
    fstArrayInt32:
      Begin
        CompareData.cdKeyLen := FieldLen;
        Result := FFKeyCompareInt(Key1, Key2, @CompareData);
      End;
    fstArrayDouble:
      Begin
        CompareData.cdKeyLen := FieldLen;
        Result := FFKeyCompareDouble(Key1, Key2, @CompareData);
      End;
    fstShortString:
      Begin
        CompareData.cdKeyLen := FieldLen-1;
        Result := FFKeyCompareAnsiStr(Key1, Key2, @CompareData);
      End;
    fstNullString,fstVarNullString:
      Begin
        CompareData.cdKeyLen := FieldLen;
        Result := FFKeyCompareAnsiStrZ(Key1, Key2, @CompareData);
      End;
    fstWideString,fstVarWideString {, fstUnicode}:
      Begin
        CompareData.cdKeyLen := FieldLen Div sizeof(WideChar);
        Result := FFKeyCompareWideStr(Key1, Key2, @CompareData);
      End;
    Else
      Result := 0; {a spurious value}
      FSRaiseExceptionNoData(EfsServerException, fsStrResServer, fserrBadFieldType);
  End; {case}
End;
{=====================================================================}

{== Internal Table Helper routines ===================================}

Function FFTblHlpGetNewBlock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Var aReleaseMethod: TffReleaseMethod;
  aOperation: TfsOperation) {!!.11}
: PffBlock;
Var
  aFileHeader: PffBlockHeaderFile;
  BlockHdr: PffBlockHeaderFile Absolute Result; {a convenient typecast}
  BlockNumber: Longint;
  aFHRelMethod: TffReleaseMethod;
Begin

  aFileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI,
    aTI,
    0,
    True,
    aFHRelMethod, fsoNone));
  Try
    {find a new block}
    With aFileHeader^ Do
      Begin
        {if there are no deleted blocks...}
        If (bhfAvailBlocks = 0) Then
          Begin
            {Have we reached the max # of blocks? }
            If (bhfUsedBlocks = aFI^.fiMaxBlocks) Then
              FSRaiseExceptionNoData(EfsServerException,
                fsStrResServer,
                fserrTableFull);
            {Begin !!.11}
            If ((TfsSrcDatabase(aTI^.tirTrans.DatabaseID).CheckSpace) And
              (Not (fffaTemporary In aFI^.fiAttributes)) And {!!.12}
              (FFGetDiskFreeSpace(ExtractFileDir(aFI^.fiName^)) <
              aFI^.fiBlockSizeK)) Then
              FSRaiseExceptionNoData(EfsServerException,
                fsStrResServer,
                fserrDiskFull);
            {End !!.11}

                    {get a brand new block}
            BlockNumber := bhfUsedBlocks;
            {Note: We don't need to lock the new block because the file's
                   header block is exclusively locked.  This prevents
                   other threads from adding the same block to the file
                   and doesn't allow them to read it since they don't know
                   about it yet. }
            Result := FFBMAddBlock(aFI, aTI, BlockNumber, aReleaseMethod, aOperation);
            inc(bhfUsedBlocks);
            aFI^.fiUsedBlocks := bhfUsedBlocks;
            If Not (fffaTemporary In aFI^.fiAttributes) Then {!!.12}
              aTI^.tirTrans.NewSpace := aTI^.tirTrans.NewSpace + aFI^.fiBlockSizeK; {!!.11}
          End
        Else {...there are some deleted blocks}
          Begin
            { Reuse the first in the deleted block chain. }
            BlockNumber := bhf1stFreeBlock;
            Result := FFBMGetBlock(aFI,
              aTI,
              BlockNumber,
              True,
              aReleaseMethod,
              aOperation);
            bhf1stFreeBlock := BlockHdr^.bhfNextBlock;
            dec(bhfAvailBlocks);
          End;
      End;
    {set the only field in the block header we can}
    BlockHdr^.bhfThisBlock := BlockNumber;
  Finally
    aFHRelMethod(PffBlock(aFileHeader));
  End;
End;
{--------}

Procedure FFTblHlpDelete(Const aPath: TffPath;
  Const aTableName: TfsTableName;
  Const aDict: TFSInfoDict);
Var
  SL: TFSSpecStringList;
  i: Integer;
  TblName: TfsTableName;
  FullName: TffFullFileName;
Begin
  SL := TFSSpecStringList.Create;
  Try
    If (FFExtractExtension(aTableName) = fsc_ExtForData) Then
      TblName := FFExtractFileName(aTableName)
    Else
      TblName := aTableName;

    For i := 0 To Pred(aDict.FileCount) Do
      Begin
        If i = 0 Then
          { Force file extension. }
          FullName := FFMakeFullFileName(aPath,
            FFMakeFileNameExt(TblName,
            fsc_ExtForData))
        Else
          { Use file extension from data dictionary. }
          FullName := FFMakeFullFileName(aPath,
            FFMakeFileNameExt(TblName,
            aDict.FileExt[i]));
        FFDeleteFile(FullName);
      End;
  Finally
    SL.Free;
  End;
End;
{--------}

Procedure FFTblHlpDeleteBlock(aFI: PffFileInfo;
  aFileHeader: PffBlockHeaderFile;
  aBlock: PffBlock);
Var
  BlockHdr: PffBlockHeaderFile Absolute aBlock; {a convenient typecast}
Begin
  { Assumption: File header block & aBlock have been exclusively locked. }

  {add it to the deleted block chain}
  With aFileHeader^ Do
    Begin
      {destroy the info in the block header}
      BlockHdr^.bhfSignature := fsc_SigFreeBlock;
      BlockHdr^.bhfNextBlock := bhf1stFreeBlock;
      {add it to the deleted block chain}
      bhf1stFreeBlock := BlockHdr^.bhfThisBlock;
      inc(bhfAvailBlocks);
    End;
End;
{--------}

Procedure FFTblHlpRename(Const aPath: TffPath;
  Const aTableName: TfsTableName;
  Const aNewName: TfsTableName;
  Const aDict: TFSInfoDict);
Var
  SL: TFSSpecStringList;
  i: Integer;
  TblName: TfsTableName;
  NewName: TfsTableName;
  FullName: TffFullFileName;
  FullNewName: TffFullFileName;
Begin
  SL := TFSSpecStringList.Create;
  Try
    If (FFExtractExtension(aTableName) = fsc_ExtForData) Then
      TblName := FFExtractFileName(aTableName)
    Else
      TblName := aTableName;

    If (FFExtractExtension(aNewName) = fsc_ExtForData) Then
      NewName := FFExtractFileName(aNewname)
    Else
      NewName := aNewName;

    For i := 0 To Pred(aDict.FileCount) Do
      Begin
        If i = 0 Then
          Begin
            { Force file extension. }
            FullName := FFMakeFullFileName(aPath,
              FFMakeFileNameExt(TblName,
              fsc_ExtForData));
            FullNewName := FFMakeFullFileName(aPath,
              FFMakeFileNameExt(NewName,
              fsc_ExtForData));
          End
        Else
          Begin
            { Use file extension from data dictionary. }
            FullName := FFMakeFullFileName(aPath,
              FFMakeFileNameExt(TblName,
              aDict.FileExt[i]));
            FullNewName := FFMakeFullFileName(aPath,
              FFMakeFileNameExt(NewName,
              aDict.FileExt[i]));
          End;
        FFRenameFile(FullName, FullNewName);
      End;
  Finally
    SL.Free;
  End;
End;
{====================================================================}

{===Buffer manager access routines===================================}

Function FFBMAddBlock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aBlockNum: TffWord32;
  Var aReleaseMethod: TffReleaseMethod;
  aOperation: TfsOperation): PffBlock;
Begin
  Assert(assigned(aFI) And assigned(aFI^.fiBufMgr));
  Result := aFI^.fiBufMgr.AddBlock(aFI, aTI, aBlockNum, aReleaseMethod, aOperation)
End;
{--------}

Procedure FFBMDirtyBlock(aFI: PffFileInfo;
  Const aBlockNum: TffWord32;
  aTI: PffTransInfo;
  Var aModifiableBlock: PffBlock);
Begin
  Assert(assigned(aFI));
  aFI^.fiBufMgr.DirtyBlock(aFI, aBlockNum, aTI, aModifiableBlock);
End;
{--------}

Function FFBMGetBlock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aBlockNum: TffWord32;
  Const aMarkDirty: Boolean;
  Var aReleaseMethod: TffReleaseMethod;
  aOperation: TfsOperation): PffBlock;
Begin
  Assert(aTI <> Nil, 'No transaction specified.'); {!!.03}
  If Assigned(aFI) And Assigned(aFI^.fiBufMgr) Then
    Result := aFI^.fiBufMgr.GetBlock(aFI, aBlockNum, aTI, aMarkDirty,
      aReleaseMethod, aOperation)
  Else
    Result := Nil;
End;
{--------}

Function FFBMGetFileHeaderBlock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aMarkDirty: Boolean;
  Var aReleaseMethod: TffReleaseMethod): PffBlock;
Begin
  If Assigned(aFI) And Assigned(aFI^.fiBufMgr) Then
    Result := aFI^.fiBufMgr.AddFile(aFI, aTI, aMarkDirty, aReleaseMethod)
  Else
    Result := Nil;
End;
{--------}

Procedure FFBMRemoveFile(aFI: PffFileInfo);
Begin
  If Assigned(aFI) And Assigned(aFI^.fiBufMgr) Then
    aFI^.fiBufMgr.RemoveFile(aFI);
End;
{--------}

Procedure FFBMUnlockBlock(aFI: PffFileInfo; aBlockNum: TffWord32);
Begin
  If Assigned(aFI) And Assigned(aFI^.fiBufMgr) Then
    aFI^.fiBufMgr.UnlockBlock(aFI, aBlockNum);
End;
{====================================================================}

{===Lock manager access routines=====================================}

Procedure FSAcquireRecordLock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aRefNum: TffInt64;
  Const aLockType: TfsSrcLockType;
  Const aDatabaseID: TffDatabaseID; {!!.10}
  Const aCursorID: TffCursorID; {!!.02}
  Const aConditional: Boolean;
  Const aUser: Boolean); {!!.02}
Var
  LockStatus: TffLockRequestStatus;
  RecStr: String;
  RetryUntil: DWORD;
  TickCount: DWORD;
  Db: TfsSrcDatabase;
  i: integer;
Begin
  //xxlock
  Db := TfsSrcDatabase(aDatabaseID);
  //If Db.TransLocking = tlOptimistic Then Exit;
  { We support only exclusive locking of records. }
  If (aFI^.fiExclOwner = aCursorID) Or (aLockType <> ffsltExclusive) Then
    Exit;
  If aUser Then
    If aLockType = ffsltExclusive Then
      Begin
        i := TfsLockManager(aTI^.tirLockMgr).FindMyRecordLock
          (aRefNum, aFI, aTI^.tirTrans, aDatabaseID);
        If i > 0 Then Exit;
      End;
  RetryUntil := FFGetRetry;
  TickCount := GetTickCount;

  { Do we have any time left?  Note that we are doing this check to avoid
    the situation where we ask for a lock and pass a negative timeout. }
  If (RetryUntil > TickCount) And
    ((RetryUntil - TickCount) >= 5) Then
    Begin
      { Obtain an exclusive lock on the record. }
      LockStatus := TfsLockManager(aTI^.tirLockMgr).AcquireRecordLock
        (aRefNum, aFI,
        aLockType,
        aConditional, {!!.02}
        (RetryUntil - TickCount),
        aTI^.tirTrans,
        aDatabaseID, {!!.10}
        aCursorID,
        Db.RecLocking);

      { Raise an exception if something went awry. }
      If LockStatus <> fflrsGranted Then
        RecStr := format(ffcRecord, [aRefNum.iHigh, aRefNum.iLow]);
      Case LockStatus Of
        fflrsTimeout:
          FSRaiseException(EfsServerException, fsStrResServer, fserrLockTimeout,
            [FFMapLockToName(aLockType), RecStr, aFI^.fiName^]);
        fflrsRejected:
          FSRaiseException(EfsServerException, fsStrResServer, fserrLockRejected,
            [FFMapLockToName(aLockType), RecStr, aFI^.fiName^]);
      End; { case }
    End
  Else
    { No.  Assume we will time out waiting for the resource. }
    FSRaiseExceptionNoData(EfsServerException, fsStrResServer,
      fserrGeneralTimeout);
End;

Procedure FSUserAcquireRecordLock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aRefNum: TffInt64;
  Const aLockType: TfsSrcLockType;
  Const aDatabaseID: TffDatabaseID;
  Const aCursorID: TffCursorID;
  Const aConditional: Boolean;
  Const aUserLock: TfsUserRecLocking;
  Const aUser: Boolean);
Var
  LockStatus: TffLockRequestStatus;
  RecStr: String;
  RetryUntil: DWORD;
  TickCount: DWORD;
  Db: TfsSrcDatabase;
  DataBaseLock: TfsdataBaseRecLocking;
  i: integer;
Begin
  { We support only exclusive locking of records. }
  If (aFI^.fiExclOwner = aCursorID) Or (aLockType <> ffsltExclusive) Then
    Exit;
  If aUser Then
    If aLockType = ffsltExclusive Then
      Begin
        i := TfsLockManager(aTI^.tirLockMgr).FindMyRecordLock
          (aRefNum, aFI, aTI^.tirTrans, aDatabaseID);
        If i > 0 Then Exit;
      End;
  Db := TfsSrcDatabase(aDatabaseID);
  DataBaseLock := Db.RecLocking;
  Case aUserLock Of
    tluDatabase: DataBaseLock := Db.RecLocking;
    tluOptimisticNoWait: DataBaseLock := tlOptimisticNoWait;
    tluOptimisticWait: DataBaseLock := tlOptimisticWait;
    tluPessimisticNoWait: DataBaseLock := tlPessimisticNoWait;
    tluPessimisticWait: DataBaseLock := tlPessimisticWait;
  End;
  RetryUntil := FFGetRetry;
  TickCount := GetTickCount;

  { Do we have any time left?  Note that we are doing this check to avoid
    the situation where we ask for a lock and pass a negative timeout. }
  If (RetryUntil > TickCount) And
    ((RetryUntil - TickCount) >= 5) Then
    Begin
      { Obtain an exclusive lock on the record. }
      LockStatus := TfsLockManager(aTI^.tirLockMgr).AcquireRecordLock
        (aRefNum, aFI,
        aLockType,
        aConditional, {!!.02}
        (RetryUntil - TickCount),
        aTI^.tirTrans,
        aDatabaseID, {!!.10}
        aCursorID,
        DataBaseLock);

      { Raise an exception if something went awry. }
      If LockStatus <> fflrsGranted Then
        RecStr := format(ffcRecord, [aRefNum.iHigh, aRefNum.iLow]);
      Case LockStatus Of
        fflrsTimeout:
          FSRaiseException(EfsServerException, fsStrResServer, fserrLockTimeout,
            [FFMapLockToName(aLockType), RecStr, aFI^.fiName^]);
        fflrsRejected:
          FSRaiseException(EfsServerException, fsStrResServer, fserrLockRejected,
            [FFMapLockToName(aLockType), RecStr, aFI^.fiName^]);
      End; { case }
    End
  Else
    { No.  Assume we will time out waiting for the resource. }
    FSRaiseExceptionNoData(EfsServerException, fsStrResServer,
      fserrGeneralTimeout);
End;

{Begin !!.10}
{--------}

Procedure FFRelaxRecordLock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aCursorID: TffCursorID;
  Const aRefNum: TffInt64);
Begin
  If (aFI^.fiExclOwner = aCursorID) Then
    Exit;

  TfsLockManager(aTI^.tirLockMgr).RelaxRecordLock
    (aRefNum, aFI, aTI^.tirTrans.DatabaseID);
End;
{End !!.10}
{--------}

Procedure FFRelRecordLock(aFI: PffFileInfo; aTI: PffTransInfo;
  Const aRefNum: TffInt64;
  Const aDatabaseID: TffDatabaseID; {!!.10}
  Const aCursorID: TffCursorID);
Begin
  If (aFI^.fiExclOwner = aCursorID) Then
    Exit;

  TfsLockManager(aTI^.tirLockMgr).ReleaseRecordLock
    (aRefNum, aFI, aTI^.tirTrans, aDatabaseID);
End;

Procedure FFRelRecordLockAll(aFI: PffFileInfo; aTI: PffTransInfo);
Begin
  TfsLockManager(aTI^.tirLockMgr).ReleaseTransactionLocksInTable(aFI, aTI^.tirTrans, True);
End;
{====================================================================}

End.

