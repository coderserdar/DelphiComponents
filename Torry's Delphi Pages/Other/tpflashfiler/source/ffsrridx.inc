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

function TffServerEngine.TableRebuildIndex(aDatabaseID : TffDatabaseID;
                                     const aTableName  : TffTableName;
                                     const aIndexName  : TffName;
                                           aIndexID    : LongInt;
                                       var aRebuildID  : LongInt): TffResult;

var
  DB               : TffSrDatabase;
  RebuildParamsPtr : PffSrRebuildParams;
  CursorID         : TffCursorID;
  RecordInfo       : TffRecordInfo;
  StartedTrans     : Boolean;
  TransID          : TffTransID;
begin
  if IsReadOnly then begin                                             {!!.01 - Start}
    Result := DBIERR_TABLEREADONLY;
    Exit;
  end else                                                             {!!.01 - End}
    Result := DBIERR_NONE;
  aRebuildID := -1;
  StartedTrans := False;
  RebuildParamsPtr := nil;
  try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if Result <> DBIERR_NONE then
      Exit;
    try
      Result := DB.NotifyExtenders(ffeaBeforeRebuildInx, ffeaTabRebuildInxFail);
      { Exit if the extenders give us an error. }
      if Result <> DBIERR_NONE then
        Exit;

      FFGetMem(RebuildParamsPtr, SizeOf(RebuildParamsPtr^));           {!!.13}
      try
        FillChar(RebuildParamsPtr^, SizeOf(RebuildParamsPtr^), 0);
        with RebuildParamsPtr^ do begin
          rpDB := TffSrDatabase.Create(DB.Engine,
                                       DB.Session,
                                       DB.Folder,
                                       DB.Alias,
                                       DB.OpenMode,
                                       DB.ShareMode,
                                       DB.Timeout,
                                       DB.CheckSpace);                 {!!.11}
          rpDB.State := ffosActive;
          { Update the folder's reference count. }
          DB.Folder.IncRefCount;
          rpTableName := aTableName;
          rpIndexName := aIndexName;
          rpIndexID := aIndexID;

          try
            { Open the table for exclusive write access; TableRebuildIndexPrim
              is responsible for closing the cursor. }
            Result := TableOpen(rpDB.DatabaseID,
                                aTableName, false, aIndexName, aIndexID,
                                omReadWrite, smExclusive, rpDB.Timeout, CursorID,
                                nil);
            if Result <> DBIERR_NONE then Abort;
            seCheckCursorIDAndGet(CursorID, TffSrBaseCursor(rpCursor));
            rpCursor.State := ffosActive;
            rpCursor.CloseTable := True;
            try
              { Start an implicit, read-only transaction. }
              if not assigned(rpDB.Transaction) then begin
                Result := seTransactionStart(rpDB, false,
                                             ffcl_TrImplicit, TransID);
                StartedTrans := (Result = DBIERR_NONE);
              end;

              if Result <> DBIERR_NONE then Abort;

              { Get the total nondeleted records in the table }
              FFTblGetRecordInfo(rpCursor.Table.Files[0],
                                 rpDB.TransactionInfo, RecordInfo);

              if StartedTrans then begin
                seTransactionCommit(rpDB);
                StartedTrans := False;
              end;

              rpRebuildStatus := RebuildRegister
                                   (TffSrClient(rpDB.Client).ClientID,
                                    RecordInfo.riRecCount);
              aRebuildID := rpRebuildStatus.RebuildID;

              { Lock the table; TableRebuildIndexPrim is responsible for
                releasing the lock. }
              Result := TableLockAcquire(CursorID, ffltWriteLock);
              if Result <> DBIERR_NONE then Abort;
              try
                { Create a separate thread for the reindex operation }
                TffSrReindexThread.Create(Self, RebuildParamsPtr);

                { The thread constructor is responsible for deallocating this
                  memory block }
                RebuildParamsPtr := nil;
              except
                TableLockRelease(CursorID, false);
                raise;
              end;
            except
              rpCursor.State := ffosInactive;
              CursorClose(CursorID);
              raise;
            end;
          except
            rpDB.State := ffosInactive;
            RebuildDeregister(aRebuildID);
            raise;
          end;
        end;
      except
        raise;
      end;
    finally
      DB.Deactivate;
    end;
  except
    on E : Exception do begin
      if Result = DBIERR_NONE then
        Result := ConvertServerException(E, FEventLog);
{Begin !!.13}
      if Assigned(RebuildParamsPtr) then begin
        if StartedTrans then
          seTransactionRollback(RebuildParamsPtr^.rpDB);
        FFFreeMem(RebuildParamsPtr, SizeOf(RebuildParamsPtr^));
      end;  { if }
{End !!.13}
    end;
  end;
end;
{--------}
function TffServerEngine.seTableRebuildIndexPrim(aRebuildParamsPtr: PffSrRebuildParams): TffResult;
const
  { Action intervals }
//  aiFlush     = 10;   { every x records, flush dirty pages }         {Deleted !!.05}
  aiSnapshot  = 10;   { every x records, update the status snapshot }
  aiYield     = 10;   { every x records, yield for other messages (16-bit) }
var
  aiFlush : integer;                                                   {!!.05}
  RecordInfo: TffRecordInfo;
  RecordBuf: PffByteArray;
  BufLength: LongInt;
  Dict: TffServerDataDict;
  OurTable: TffSrTable;
  OurIndexID: longint;
  OurIndexFileNumber: longint;
  RefNr: TffInt64;
  Compare: TffKeyCompareFunc;
  OurKey: PffByteArray;
  BuildKey: TffKeyBuildFunc;
  CmpData : TffCompareData;
  TransID : TffTransID;

  RecordsRead: LongInt;
  RecordsWritten: LongInt;

  NextFlushPoint: LongInt;
  NextSnapshotPoint: LongInt;
  {$IFNDEF ThreadedRebuilds}
  NextYieldPoint: LongInt;
  {$ENDIF}

  IsComposite : Boolean;
begin
  Result := DBIERR_NONE;
  FFSetRetry(0);                                                       {!!.03}

  with aRebuildParamsPtr^ do begin
    RecordsRead := 0;
    RecordsWritten := 0;
    try
      try
        try
          try

            rpCursor.Timeout := 0;
            rpDB.Timeout := 0;

            { Capture data dictionary, etc }
            OurIndexID := rpCursor.IndexID;
            OurTable := TffSrTable(rpCursor.Table);
            Dict := OurTable.Dictionary;
            OurIndexFileNumber := Dict.IndexFileNumber[OurIndexID];

            { Set up the compare method for the index }
            if (Dict.IndexType[OurIndexID] = itComposite) then begin
              Compare := FFKeyCompareComposite;
              IsComposite := true;
            end
            else begin
              Compare := OurTable.stGetUserCompareKey(OurIndexID);
              IsComposite := false;
            end;

            { Start transaction -- to ensure all data changes are written to
              the file }
            Result := seTransactionStart(rpDB, False, False, TransID);
            if Result <> DBIERR_NONE then Exit;
            try
              with CmpData do begin
                cdKeyLen := Dict.IndexKeyLength[OurIndexID];
                cdIndex := OurIndexID;
                cdDict := Dict;
                cdFldCnt := 0;
                cdPartLen := 0;
                cdAscend := Dict.IndexIsAscending[OurIndexID];
                cdNoCase := Dict.IndexIsCaseInsensitive[OurIndexID];
              end;

              { Remove all the keys in the existing index }
              rpCursor.ClearIndex;
              if (Result <> DBIERR_NONE) then
                FFRaiseExceptionNoData(EffServerException, ffStrResServer, fferrUnknownCursor);
              { Post the dirty pages for the deleted keys }
              if seTransactionCommitSubset(rpDB) <> DBIERR_NONE then
                FFRaiseExceptionNoData(EffServerException, ffStrResServer,
                                       fferrTransactionFailed);

              { Get the size of the record buffer }
              FFTblGetRecordInfo(OurTable.Files[0],
                                 rpDB.TransactionInfo, RecordInfo);
              BufLength := RecordInfo.riRecLength;

              { Allocate a record buffer }
              FFGetMem(RecordBuf, BufLength);
{Begin !!.05}
              { Figure out how many records are to be processed before
                flushing. }
              aiFlush := (ffcl_1MB div BufLength);
{End !!.05}
              try

                { Allocate key buffer }
                FFGetMem(OurKey, CmpData.cdKeyLen);
                try
                  NextFlushPoint := aiFlush;
                  NextSnapshotPoint := aiSnapshot;
                  {$IFNDEF ThreadedRebuilds}
                  NextYieldPoint := aiYield;
                  {$ENDIF}

                  RefNr.iLow := 0;
                  RefNr.iHigh := 0;

                  { Loop through all the nondeleted records... }
                  OurTable.GetNextRecordSeq(rpDB.TransactionInfo,
                                            RefNr, RecordBuf);
                  while (not (RefNr.iLow = 0) and (RefNr.iHigh = 0)) do begin
                    Inc(RecordsRead);

                    { Reindexing the Sequential Access Index }
                    if OurIndexID = 0 then begin
                      if OurTable.stInsertKeyPrim
                                    (OurIndexFileNumber,
                                     rpDB.TransactionInfo,
                                     RefNr,
                                     PffByteArray(@RefNr),
                                     FFKeyCompareI64,
                                     @CmpData) then
                        Inc(RecordsWritten)
                      else
                        FFRaiseExceptionNoData(EffServerException, ffStrResServer,
                                               fferrKeyPresent);
                    end
                    else begin

                      { Reindexing a composite index }
                      if IsComposite then begin
                        Result := OurTable.stBuildCompositeKey(OurIndexID,
                                                               RecordBuf,
                                                               OurKey,
                                                               0,
                                                               0);
                      end

                      { Reindexing a user-defined index }
                      else begin
                        BuildKey := OurTable.stGetUserBuildKey(OurIndexID);
                        if not BuildKey(OurIndexID,
                                        RecordBuf,
                                        OurKey^,
                                        CmpData.cdKeyLen) then
                          Result := DBIERR_KEYVIOL;
                      end;
                      if Result <> DBIERR_NONE then
                        Abort;

                      if OurTable.stInsertKeyPrim
                                    (OurIndexFileNumber,
                                     rpDB.TransactionInfo,
                                     RefNr,
                                     OurKey,
                                     Compare,
                                     @CmpData) then
                        Inc(RecordsWritten)
                      else
                        Abort;
                    end;

                    { See if it's time to flush our work so far }
                    if RecordsRead >= NextFlushPoint then begin
                      Inc(NextFlushPoint, aiFlush);
                      if seTransactionCommitSubset(rpDB) <> DBIERR_NONE then
                        FFRaiseExceptionNoData(EffServerException,
                                               ffStrResServer,
                                               fferrTransactionFailed);
                    end;

                    { See if it's time to update the status packet }
                    if RecordsRead >= NextSnapshotPoint then begin
                      Inc(NextSnapshotPoint, aiSnapshot);
                      rpRebuildStatus.MakeSnapshot(RecordsRead,
                                                   RecordsWritten,
                                                   DBIERR_NONE);
                    end;

                    {$IFNDEF ThreadedRebuilds}
                      { See if it's time to yield for other messages }
                      if RecordsRead >= NextYieldPoint then begin
                        Inc(NextYieldPoint, aiYield);
                        Application.ProcessMessages;
                      end;
                    {$ENDIF}

                    OurTable.GetNextRecordSeq(rpDB.TransactionInfo,
                                              RefNr, RecordBuf);
                  end;
                finally
                  FFFreeMem(OurKey, CmpData.cdKeyLen);
                end;
              finally
                FFFreeMem(RecordBuf, BufLength);
              end;
            finally
              if seTransactionCommit(rpDB) <> DBIERR_NONE then
                FFRaiseExceptionNoData(EffServerException,
                                       ffStrResServer,
                                       fferrTransactionFailed);
            end;
          finally
            rpCursor.RelTableLock(false);
          end;
        finally
          rpCursor.State := ffosInactive;
          CursorClose(rpCursor.CursorID);
          rpDB.State := ffosInactive;
          rpDB.Free;
        end;
      except
        on E : Exception do begin
          if Result = DBIERR_NONE then
            Result := ConvertServerExceptionEx(E, FEventLog,           {!!.01}
                                               bseGetReadOnly);        {!!.01}
        end;
      end;
    finally
      rpRebuildStatus.MakeSnapshot(RecordsRead,
                                   RecordsWritten,
                                   Result);
      RebuildDeregister(rpRebuildStatus.RebuildID);
    end;
  end;
end;

