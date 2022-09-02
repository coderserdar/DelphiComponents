{*********************************************************}
{* FlashFiler: rebuild index include file                *}
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

Function TFSServer.TableRebuildIndex(aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName;
  Const aIndexName: TffName;
  aIndexID: Longint;
  Var aRebuildID: Longint): TffResult;

Var
  DB: TfsSrcDatabase;
  RebuildParamsPtr: PffSrRebuildParams;
  CursorID: TffCursorID;
  RecordInfo: TffRecordInfo;
  StartedTrans: Boolean;
  TransID: TffTransID;
Begin
  If IsReadOnly Then
    Begin {!!.01 - Start}
      Result := DBIERR_TABLEREADONLY;
      Exit;
    End
  Else {!!.01 - End}
    Result := DBIERR_NONE;
  aRebuildID := -1;
  StartedTrans := False;
  RebuildParamsPtr := Nil;
  Try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If Result <> DBIERR_NONE Then
      Exit;
    Try
      Result := DB.NotifyExtenders(ffeaBeforeRebuildInx, ffeaTabRebuildInxFail);
      { Exit if the extenders give us an error. }
      If Result <> DBIERR_NONE Then
        Exit;

      FFGetMem(RebuildParamsPtr, SizeOf(RebuildParamsPtr^)); {!!.13}
      Try
        FillChar(RebuildParamsPtr^, SizeOf(RebuildParamsPtr^), 0);
        With RebuildParamsPtr^ Do
          Begin
            rpDB := TfsSrcDatabase.Create(DB.Engine,
              DB.Session,
              DB.Folder,
              DB.Alias,
              DB.OpenMode,
              DB.ShareMode,
              DB.Timeout,
              DB.CheckSpace,
              DB.TransIsolation,
              DB.RecLocking); {!!.11}
            rpDB.State := ffosActive;
            { Update the folder's reference count. }
            DB.Folder.IncRefCount;
            rpTableName := aTableName;
            rpIndexName := aIndexName;
            rpIndexID := aIndexID;

            Try
              { Open the table for exclusive write access; TableRebuildIndexPrim
                is responsible for closing the cursor. }
              Result := TableOpen(rpDB.DatabaseID,
                aTableName, False, aIndexName, aIndexID,
                omReadWrite, smExclusive, rpDB.Timeout, CursorID,
                Nil, False);
              If Result <> DBIERR_NONE Then Abort;
              seCheckCursorIDAndGet(CursorID, TfsSrBaseCursor(rpCursor));
              rpCursor.State := ffosActive;
              rpCursor.CloseTable := True;
              Try
                { Start an implicit, read-only transaction. }
                If Not assigned(rpDB.Transaction) Then
                  Begin
                    Result := seTransactionStart(rpDB, False,
                      fscl_TrImplicit, TransID);
                    StartedTrans := (Result = DBIERR_NONE);
                  End;

                If Result <> DBIERR_NONE Then Abort;

                { Get the total nondeleted records in the table }
                FFTblGetRecordInfo(rpCursor.Table.Files[0],
                  rpDB.TransactionInfo, RecordInfo);

                If StartedTrans Then
                  Begin
                    seTransactionCommit(rpDB);
                    StartedTrans := False;
                  End;

                rpRebuildStatus := RebuildRegister
                  (TfsSrcClient(rpDB.Client).ClientID,
                  RecordInfo.riRecCount);
                aRebuildID := rpRebuildStatus.RebuildID;

                { Lock the table; TableRebuildIndexPrim is responsible for
                  releasing the lock. }
                Result := TableLockAcquire(CursorID, ffltWriteLock);
                If Result <> DBIERR_NONE Then Abort;
                Try
                  { Create a separate thread for the reindex operation }
                  TfsSrcReindexThread.Create(Self, RebuildParamsPtr, False, False, False);

                  { The thread constructor is responsible for deallocating this
                    memory block }
                  RebuildParamsPtr := Nil;
                Except
                  TableLockRelease(CursorID, False);
                  Raise;
                End;
              Except
                rpCursor.State := ffosInactive;
                CursorClose(CursorID);
                Raise;
              End;
            Except
              rpDB.State := ffosInactive;
              RebuildDeregister(aRebuildID);
              Raise;
            End;
          End;
      Except
        Raise;
      End;
    Finally
      DB.Deactivate;
    End;
  Except
    On E: Exception Do
      Begin
        If Result = DBIERR_NONE Then
          Result := ConvertServerException(E, FEventLog);
        {Begin !!.13}
        If Assigned(RebuildParamsPtr) Then
          Begin
            If StartedTrans Then
              seTransactionRollback(RebuildParamsPtr^.rpDB);
            FFFreeMem(RebuildParamsPtr, SizeOf(RebuildParamsPtr^));
          End; { if }
        {End !!.13}
      End;
  End;
End;
{--------}

Function TFSServer.seTableRebuildIndexPrim(aRebuildParamsPtr: PffSrRebuildParams): TffResult;
Const
  { Action intervals }
//  aiFlush     = 10;   { every x records, flush dirty pages }         {Deleted !!.05}
  aiSnapshot = 10; { every x records, update the status snapshot }
  aiYield = 10; { every x records, yield for other messages (16-bit) }
Var
  aiFlush: Integer; {!!.05}
  RecordInfo: TffRecordInfo;
  RecordBuf: PffByteArray;
  BufLength: Longint;
  Dict: TFSInfoServerDict;
  OurTable: TfsSrcTable;
  OurIndexID: Longint;
  OurIndexFileNumber: Longint;
  RefNr: TffInt64;
  Compare: TffKeyCompareFunc;
  OurKey: PffByteArray;
  BuildKey: TffKeyBuildFunc;
  CmpData: TffCompareData;
  TransID: TffTransID;

  RecordsRead: Longint;
  RecordsWritten: Longint;

  NextFlushPoint: Longint;
  NextSnapshotPoint: Longint;
  {$IFNDEF ThreadedRebuilds}
  NextYieldPoint: Longint;
  {$ENDIF}

  IsComposite: Boolean;
  aFlag: Byte;
Begin
  Result := DBIERR_NONE;
  FFSetRetry(0); {!!.03}

  With aRebuildParamsPtr^ Do
    Begin
      RecordsRead := 0;
      RecordsWritten := 0;
      Try
        Try
          Try
            Try

              rpCursor.Timeout := 0;
              rpDB.Timeout := 0;

              { Capture data dictionary, etc }
              OurIndexID := rpCursor.IndexID;
              OurTable := TfsSrcTable(rpCursor.Table);
              Dict := OurTable.Dictionary;
              OurIndexFileNumber := Dict.IndexFileNumber[OurIndexID];

              { Set up the compare method for the index }
              If (Dict.IndexType[OurIndexID] = itComposite) Then
                Begin
                  Compare := FSKeyCompareComposite;
                  IsComposite := True;
                End
              Else
                Begin
                  Compare := OurTable.stGetUserCompareKey(OurIndexID);
                  IsComposite := False;
                End;

              { Start transaction -- to ensure all data changes are written to
                the file }
              Result := seTransactionStart(rpDB, False, False, TransID);
              If Result <> DBIERR_NONE Then Exit;
              Try
                With CmpData Do
                  Begin
                    cdKeyLen := Dict.IndexKeyLength[OurIndexID];
                    cdIndex := OurIndexID;
                    cdDict := Dict;
                    cdFldCnt := 0;
                    cdPartLen := 0;
                    cdAscend := Dict.IndexIsAscending[OurIndexID];
                    cdNoCase := Dict.IndexIsCaseInsensitive[OurIndexID];
                  End;

                { Remove all the keys in the existing index }
                rpCursor.ClearIndex;
                If (Result <> DBIERR_NONE) Then
                  FSRaiseExceptionNoData(EfsServerException, fsStrResServer, fserrUnknownCursor);
                { Post the dirty pages for the deleted keys }
                If seTransactionCommitSubset(rpDB) <> DBIERR_NONE Then
                  FSRaiseExceptionNoData(EfsServerException, fsStrResServer,
                    fserrTransactionFailed);

                { Get the size of the record buffer }
                FFTblGetRecordInfo(OurTable.Files[0],
                  rpDB.TransactionInfo, RecordInfo);
                BufLength := RecordInfo.riRecLength;

                { Allocate a record buffer }
                FFGetMem(RecordBuf, BufLength);
                {Begin !!.05}
                              { Figure out how many records are to be processed before
                                flushing. }
                aiFlush := (fscl_1MB Div BufLength);
                {End !!.05}
                Try

                  { Allocate key buffer }
                  FFGetMem(OurKey, CmpData.cdKeyLen);
                  Try
                    NextFlushPoint := aiFlush;
                    NextSnapshotPoint := aiSnapshot;
                    {$IFNDEF ThreadedRebuilds}
                    NextYieldPoint := aiYield;
                    {$ENDIF}

                    RefNr.iLow := 0;
                    RefNr.iHigh := 0;

                    { Loop through all the nondeleted records... }
                    OurTable.GetNextRecordSeq(rpDB.TransactionInfo,
                      RefNr, RecordBuf, False, False, AFlag);
                    While (Not (RefNr.iLow = 0) And (RefNr.iHigh = 0)) Do
                      Begin
                        Inc(RecordsRead);

                        { Reindexing the Sequential Access Index }
                        If OurIndexID = 0 Then
                          Begin
                            If OurTable.stInsertKeyPrim
                              (OurIndexFileNumber,
                              rpDB.TransactionInfo,
                              RefNr,
                              PffByteArray(@RefNr),
                              FFKeyCompareI64,
                              @CmpData) Then
                              Inc(RecordsWritten)
                            Else
                              FSRaiseExceptionNoData(EfsServerException, fsStrResServer,
                                fserrKeyPresent);
                          End
                        Else
                          Begin

                            { Reindexing a composite index }
                            If IsComposite Then
                              Begin
                                Result := OurTable.stBuildCompositeKey(OurIndexID,
                                  RecordBuf,
                                  OurKey,
                                  0,
                                  0);
                              End

                                { Reindexing a user-defined index }
                            Else
                              Begin
                                BuildKey := OurTable.stGetUserBuildKey(OurIndexID);
                                If Not BuildKey(OurIndexID,
                                  RecordBuf,
                                  OurKey^,
                                  CmpData.cdKeyLen) Then
                                  Result := DBIERR_KEYVIOL;
                              End;
                            If Result <> DBIERR_NONE Then
                              Abort;

                            If OurTable.stInsertKeyPrim
                              (OurIndexFileNumber,
                              rpDB.TransactionInfo,
                              RefNr,
                              OurKey,
                              Compare,
                              @CmpData) Then
                              Inc(RecordsWritten)
                            Else
                              Abort;
                          End;

                        { See if it's time to flush our work so far }
                        If RecordsRead >= NextFlushPoint Then
                          Begin
                            Inc(NextFlushPoint, aiFlush);
                            If seTransactionCommitSubset(rpDB) <> DBIERR_NONE Then
                              FSRaiseExceptionNoData(EfsServerException,
                                fsStrResServer,
                                fserrTransactionFailed);
                          End;

                        { See if it's time to update the status packet }
                        If RecordsRead >= NextSnapshotPoint Then
                          Begin
                            Inc(NextSnapshotPoint, aiSnapshot);
                            rpRebuildStatus.MakeSnapshot(RecordsRead,
                              RecordsWritten,
                              DBIERR_NONE);
                          End;

                        {$IFNDEF ThreadedRebuilds}
                        { See if it's time to yield for other messages }
                        If RecordsRead >= NextYieldPoint Then
                          Begin
                            Inc(NextYieldPoint, aiYield);
                            Application.ProcessMessages;
                          End;
                        {$ENDIF}

                        OurTable.GetNextRecordSeq(rpDB.TransactionInfo,
                          RefNr, RecordBuf, False, False, AFlag);
                      End;
                  Finally
                    FFFreeMem(OurKey, CmpData.cdKeyLen);
                  End;
                Finally
                  FFFreeMem(RecordBuf, BufLength);
                End;
              Finally
                If seTransactionCommit(rpDB) <> DBIERR_NONE Then
                  FSRaiseExceptionNoData(EfsServerException,
                    fsStrResServer,
                    fserrTransactionFailed);
              End;
            Finally
              rpCursor.RelTableLock(False);
            End;
          Finally
            rpCursor.State := ffosInactive;
            CursorClose(rpCursor.CursorID);
            rpDB.State := ffosInactive;
            rpDB.Free;
          End;
        Except
          On E: Exception Do
            Begin
              If Result = DBIERR_NONE Then
                Result := ConvertServerExceptionEx(E, FEventLog, {!!.01}
                  bseGetReadOnly); {!!.01}
            End;
        End;
      Finally
        rpRebuildStatus.MakeSnapshot(RecordsRead,
          RecordsWritten,
          Result);
        RebuildDeregister(rpRebuildStatus.RebuildID);
      End;
    End;
End;

