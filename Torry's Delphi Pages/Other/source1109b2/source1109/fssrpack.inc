Const
  { The name given the old table while we are replacing it with the new
    table. }
  ffcPackBaseName = '_PACK';
  ffcSaveBaseName = '_PACKSV';

Function TFSServer.TablePack(aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName;
  Var aRebuildID: Longint; UndeleteRecords: Boolean; OnlyDeleted: boolean): TffResult;
Var
  DB: TfsSrcDatabase;
  RebuildParamsPtr: PffSrRebuildParams;
  SourceDict: TFSInfoServerDict;
  TargetDict: TFSInfoServerDict;
  TargetBasename: TffFileName;
  RecordInfo: TffRecordInfo;
  CursorID: TffCursorID;
  I: Integer;
  Inx: Integer;
  KeyProcItem: TfsKeyProcItem;
  StartedTrans: boolean;
  TransID: TffTransID;
  SrcTblVersionChanged: Boolean; {!!.11}

  Function SetTargetBasename(Path: TffPath; Root: TffFileName): TffFileName;
  Var
    I: Integer;
  Begin
    I := 0;
    Repeat
      Inc(I);
      Result := Root + IntToStr(I);
    Until Not FFFileExists(FFMakeFullFilename(Path,
      FFMakeFileNameExt(Result,
      fsc_ExtForData)));
  End;

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
      Result := DB.NotifyExtenders(ffeaBeforeTabPack, ffeaTabPackFail);
      { Exit if the extenders give us an error. }
      If Result <> DBIERR_NONE Then
        Exit;

      FFGetMem(RebuildParamsPtr, SizeOf(RebuildParamsPtr^)); {!!.13}
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
          rpIndexName := '';
          rpIndexID := 0;

          Try
            { Open the table for exclusive write access; TablePackPrim
              is responsible for closing the cursor. }

            SrcTblVersionChanged := False;
            If rpDB.Folder.PackSrcTableVersion <> 0 Then
              Begin
                SrcTblVersionChanged := True;
                rpDB.Folder.ExistingTableVersion :=
                  rpDB.Folder.PackSrcTableVersion;
              End;

            Result := TableOpen(rpDB.DatabaseID,
              aTableName, False, '', 0, omReadWrite,
              smExclusive, DB.Timeout, CursorID, Nil, False);

            If SrcTblVersionChanged Then
              rpDB.Folder.ExistingTableVersion := 0;

            If Result <> DBIERR_NONE Then
              Abort;

            seCheckCursorIDAndGet(CursorID, TfsSrBaseCursor(rpCursor));
            rpCursor.State := ffosActive;
            rpCursor.CloseTable := True;

            Try
              { Start an implicit, read-only transaction. }
              If Not assigned(DB.Transaction) Then
                Begin
                  Result := seTransactionStart(rpDB, False,
                    fscl_TrImplicit, TransID);
                  StartedTrans := (Result = DBIERR_NONE);
                End;

              If Result <> DBIERR_NONE Then
                Abort;

              { Get the total nondeleted records in the table }
              FFTblGetRecordInfo(rpCursor.Table.Files[0],
                rpDB.TransactionInfo,
                RecordInfo);
              If StartedTrans Then
                Begin
                  seTransactionCommit(rpDB);
                  StartedTrans := False;
                End;
              rpRebuildStatus := RebuildRegister
                (TfsSrcClient(rpDB.Client).ClientID,
                RecordInfo.riRecCount);
              aRebuildID := rpRebuildStatus.RebuildID;

              { Setup the destination file(s).  Use a basename of _PACK<x>
                where <x> starts and 1 and is incremented upward until a
                nonexistant filename is found. }
              TargetBasename := SetTargetBasename(DB.Folder.Path,
                ffcPackBaseName);

              { Capture the data dictionary }
              SourceDict := rpCursor.Table.Dictionary;
              TargetDict := TFSInfoServerDict.Create(SourceDict.BlockSize);
              Try

                { Setup the new (temporary) table }
                TargetDict.Assign(SourceDict);
                Result := TableBuild(rpDB.DatabaseID, False, TargetBasename,
                  False, TargetDict);
                If Result <> DBIERR_NONE Then
                  Abort;
                Try

                  { Bind the user-defined index routines (if any) to the
                    target table }
                  For I := 1 To TargetDict.IndexCount - 1 Do
                    If (TargetDict.IndexType[I] = itUdfDll) Then
                      With Configuration Do
                        Begin
                          With rpCursor.Table Do
                            Inx := KeyProcList.KeyProcIndex(Folder.Path,
                              BaseName, i);
                          If (Inx <> -1) Then
                            Begin
                              KeyProcItem := KeyProcList[Inx];
                              With KeyProcItem Do
                                Begin
                                  Link;
                                  AddKeyProc(DB.Folder.Path, TargetBasename, I,
                                    DLLName, BuildKeyName,
                                    CompareKeyName);
                                End;
                            End
                          Else
                            FSRaiseExceptionNoData(EfsServerException,
                              fsStrResServer,
                              fserrResolveTableLinks);
                        End;
                  Try

                    { Open the destination table for exclusive write access;
                      TablePackPrim is responsible for closing the cursor }
                    Result := TableOpen(rpDB.DatabaseID,
                      TargetBasename, False,
                      '', 0, omReadWrite, smExclusive,
                      DB.Timeout, CursorID, Nil, False);
                    If Result <> DBIERR_NONE Then
                      Abort;
                    seCheckCursorIDAndGet(CursorID,
                      TfsSrBaseCursor(rpTargetCursor));
                    rpTargetCursor.State := ffosActive;
                    rpTargetCursor.Table.AddAttribute(fffaBLOBChainSafe); {!!.03}
                    rpTargetCursor.CloseTable := True;
                  Finally

                    { Get rid of the temporary user-defined index bindings }
                    For I := 1 To TargetDict.IndexCount - 1 Do
                      If (TargetDict.IndexType[I] = itUdfDll) Then
                        With Configuration Do
                          If KeyProcList.KeyProcExists(DB.Folder.Path,
                            TargetBaseName, I) Then
                            KeyProcList.DeleteKeyProc(DB.Folder.Path,
                              TargetBaseName, I);
                  End;

                  Try
                    { Create a separate thread for the pack operation }
                    TfsSrcPackThread.Create(Self, RebuildParamsPtr, False, UndeleteRecords, OnlyDeleted);

                    { The thread constructor is responsible for
                      deallocating this memory block }
                    RebuildParamsPtr := Nil;
                  Except
                    rpTargetCursor.State := ffosInactive;
                    CursorClose(rpTargetCursor.CursorID);
                    Raise;
                  End;
                Except
                  { Clean up the files }
                  TableDelete(rpDB.DatabaseID, TargetBasename);
                  Raise;
                End;
                {Begin !!.05}
              Finally
                TargetDict.Free;
              End;
              {End !!.05}
            Except
              rpCursor.State := ffosInactive;
              CursorClose(rpCursor.CursorID);
              Raise;
            End;
          Except
            rpDB.State := ffosInactive;
            RebuildDeregister(aRebuildID);
            Raise;
          End;
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

Function TFSServer.seTablePackPrim(aRebuildParamsPtr: PffSrRebuildParams; aRangeError: boolean;
UndeleteRecords: Boolean; OnlyDeleted: boolean): TffResult;
Const
  { Action intervals }
  aiSnapshot = 10; { every x records, update the status snapshot }
  aiYield = 10; { every x records, yield for other messages }

  {Begin !!.03}
  BLOBBufLen = fscl_1MB; { size of BLOB transfer buffer }
  RecBuf = fscl_1MB; { max memory for records before flushing }
  MaxBLOBBytes = 10 * fscl_1MB; { max # of BLOB bytes that may be copied before
  committing the transaction. }
{End !!.03}
Var
  aiFlush: Integer;
  BLOBBytesCopied: Integer; {!!.03}
  RecordBuf: PffByteArray;
  BufLength: Longint;
  SaveBaseName: TfsTableName;
  TargetRecordBuf: PffByteArray;
  TargetBufLength: Longint;
  IsNull: Boolean;
  RAMTrigger: Longint; {!!.03}
  RefNr: TffInt64;
  SourceName: TfsTableName;
  TargetName: TfsTableName;
  Restructuring: Boolean;
  BLOBBuffer: Pointer;
  AutoIncField: Integer;
  AutoIncFieldType: TfsFieldType;
  AutoIncHighValue64: Int64;
  AutoStep: Longint;
  AutoInc: Int64;
  ThisAutoIncValue64: Int64;
  AutoIncHighValue32: TffWord32;
  ThisAutoIncValue32: TffWord32;
  TransID: TffTransID;

  RecordsRead: Longint;
  RecordsWritten: Longint;

  NextFlushPoint: Longint;
  NextSnapshotPoint: Longint;
  Mx: Longint;
  Ps, Ps1,ps2, ps3: Longword;
  aflag: byte;
  aRefNr: tffInt64;
  {$IFNDEF ThreadedRebuilds}
  {NextYieldPoint: LongInt;}{!!.03}
  {$ENDIF}

  Procedure FindAutoIncField;
  Var
    TargetAutoInc: Integer;
  Begin
    With aRebuildParamsPtr^ Do
      If rpTargetCursor.Table.Dictionary.HasAutoIncField(TargetAutoInc) Then
        Begin
          AutoIncField := TargetAutoInc;
          AutoIncFieldType := rpTargetCursor.Table.Dictionary.FieldType[TargetAutoInc];
        End;
  End;

  Procedure AllocateBLOBTransferBuffer;
    { Don't allocate a BLOB transfer buffer unless we actually
      need one (i.e., there may not be any BLOBs in this record }
  Begin
    If Not Assigned(BLOBBuffer) Then
      FFGetMem(BLOBBuffer, BLOBBufLen);
  End;

  Procedure ReleaseBLOBTransferBuffer;
  Begin
    If Assigned(BLOBBuffer) Then
      FFFreeMem(BLOBBuffer, BLOBBufLen);
  End;

  Function CopyBLOBs(Var BLOBBytesCopied: Integer): TffResult; {!!.03}
  Var
    I: Integer;
    SourceBLOBNr: TffInt64;
    TargetBLOBNr: TffInt64;
    IsNull: Boolean;
  Begin
    Result := DBIERR_NONE;

    With aRebuildParamsPtr^ Do
      With rpCursor.Table.Dictionary Do
        For I := 0 To FieldCount - 1 Do

          { Find all the BLOB fields in the record }
          If (FieldType[I] >= fstBLOB) And
            (FieldType[I] <= ffcLastBLOBType) Then
            Begin
              GetRecordField(I, RecordBuf, IsNull, @SourceBLOBNr);
              If IsNull Then
                With rpTargetCursor.Table.Dictionary Do
                  SetRecordField(I, TargetRecordBuf, Nil)
              Else
                Begin
                  AllocateBLOBTransferBuffer;

                  {Begin !!.03}
                  inc(BLOBBytesCopied, rpCursor.BLOBGetLength(SourceBLOBNr, Result));
                  If Result <> DBIERR_NONE Then
                    Abort;
                  {End !!.03}

                  Result := rpTargetCursor.bcBLOBCopy
                    (rpCursor, SourceBLOBNr, TargetBLOBNr);
                  If Result <> DBIERR_NONE Then
                    Abort;

                  With rpTargetCursor.Table.Dictionary Do
                    SetRecordField(I, TargetRecordBuf, @TargetBlobNr);
                End;
            End;
  End;

  Function FillTargetBuffer(aSourceCursor, aTargetCursor: TfsSrcCursor;
    aSourceBuf, aTargetBuf: PffByteArray;
    aFieldMap: TfsSrcFieldMapList; {!!.03}
    Var BLOBBytesCopied: Integer): TffResult; {!!.03}
    { Copies the fields from the source buffer into the target buffer }
  Var
    I: Integer;
    IsNull: Boolean;
    BLOBNr: TffInt64;
    SourceBLOBNr: TffInt64; {!!.03}
  Begin
    { The FieldMap has already been validated so we can assume all the
      fieldnames and datatype matches are legal. }
    { WARNING: the above assumption is no longer valid. Some matches are now
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    checked for during restructure and an exception is raised below if invalid. }{!!.10}
    Result := DBIERR_NONE;

    { Initialize the output buffer.  All fields will be null by default,
      therefore we do not have to explicitly handle any new fields added
      by the restructure. }
    aTargetCursor.Table.Dictionary.InitRecord(aTargetBuf);

    { Loop through all the fields to be converted }
    For I := 0 To aFieldMap.Count - 1 Do
      With aFieldMap Do
        Begin
          {Begin !!.03}
          If TargetField[I].FieldType In [fstBLOB..ffcLastBLOBType] Then
            Begin
              AllocateBLOBTransferBuffer;
              { Obtain the length of the source BLOB. }
              If (SourceField[I].FieldType In [fstBLOB..ffcLastBLOBType]) Then
                Begin {!!.13}
                  aSourceCursor.Table.Dictionary.GetRecordField(SourceField[I].Number,
                    aSourceBuf,
                    IsNull, @SourceBLOBNr);
                  If (Not IsNull) Then
                    inc(BLOBBytesCopied,
                      aSourceCursor.BLOBGetLength(SourceBLOBNr, Result));
                  If Result <> DBIERR_NONE Then
                    Break;
                End
              Else {!!.13}
                inc(BLOBBytesCopied, aFieldMap.SourceField[I].FieldLength); {!!.13}
            End;
          {End !!.03}

          Result := seConvertSingleField(aSourceBuf,
            aTargetBuf,
            aSourceCursor.CursorID,
            aTargetCursor.CursorID,
            SourceField[I].Number,
            TargetField[I].Number,
            BLOBBuffer,
            BLOBBufLen,
            aRangeError);
          If Result <> DBIERR_NONE Then
            {Begin !!.10}
            If Result = DBIERR_INVALIDFLDXFORM Then
              FSRaiseException(EfsException, fsStrResServer, fserrBadFieldXform,
                [SourceField[I].Name + ' (' +
                GetEnumName(TypeInfo(TfsFieldType), Integer(SourceField[I].FieldType)) + ')',
                  TargetField[I].Name + ' (' +
                  GetEnumName(TypeInfo(TfsFieldType), Integer(TargetField[I].FieldType)) + ')'])

            Else
              {End !!.10}
              Break;
        End;

    { Check for fields not converted that may be "required" (i.e., added
      fields that have the "required" flag set) }
    With aTargetCursor.Table.Dictionary Do
      Begin
        For I := 0 To FieldCount - 1 Do
          Begin
            GetRecordField(I, aTargetBuf, IsNull, Nil);
            If IsNull And FieldRequired[I] Then
              Begin

                { Clear the null flag, the record buffer is already set to zero }
                FFClearBit(PffByteArray(@aTargetBuf^[LogicalRecordLength]), I);

                { For BLOBs we have to create an empty BLOB }
                If FieldType[I] In [fstBLOB..ffcLastBLOBType] Then
                  Begin
                    AllocateBLOBTransferBuffer;
                    Result := aTargetCursor.BLOBAdd(BLOBNr);
                    SetRecordField(I, aTargetBuf, @BLOBNr);
                    If Result = DBIERR_NONE Then
                      Result := aTargetCursor.BLOBWrite(BLOBNr, 0, 0, BLOBBuffer);
                  End;
              End;
          End;
      End;
  End;

  Function SetTargetBasename(Path: TffPath; Root: TffFileName): TffFileName;
  Var
    I: Integer;
  Begin
    I := 0;
    Repeat
      Inc(I);
      Result := Root + IntToStr(I);
    Until Not FFFileExists(FFMakeFullFilename(Path,
      FFMakeFileNameExt(Result,
      fsc_ExtForData)));
  End;

Begin
  Result := DBIERR_NONE;
  AutoIncField := -1;
  AutoStep := 1;
  AutoIncHighValue32 := 0;
  AutoIncHighValue64 := 0;
  BLOBBytesCopied := 0; {!!.03}
  RAMTrigger := seBufMgr.MaxRAM + (seBufMgr.MaxRAM Div 10); {!!.03}
  RecordsRead := 0;
  RecordsWritten := 0;
  FFSetRetry(0);
  With aRebuildParamsPtr^ Do
    Begin
      Try
        Try
          rpCursor.Timeout := 0;
          rpTargetCursor.Timeout := 0;
          rpDb.Timeout := 0;
          TargetRecordBuf := Nil;
          TargetBufLength := 0;

          { Find first AutoInc field, if any }
          FindAutoIncField;

          { Decide if we are performing a restructure as well }
          Restructuring := Assigned(rpFieldMap);
          If Restructuring Then
            Begin

              { Allocate an output record buffer }
              TargetBufLength := rpTargetCursor.Table.Dictionary.RecordLength;
              FFGetMem(TargetRecordBuf, TargetBufLength);
            End;

          Try
            Try
              Try
                Try
                  BLOBBuffer := Nil;

                  SourceName := rpCursor.Table.Basename;
                  TargetName := rpTargetCursor.Table.Basename;

                  { Allocate a record buffer }
                  BufLength := rpCursor.Table.Dictionary.RecordLength;
                  FFGetMem(RecordBuf, BufLength);

                  { Figure out how many records are to be processed before
                    flushing. }
                  aiFlush := (RecBuf Div BufLength);

                  Try
                    Try

                      { For packs, TargetRecordBuf points to the input buffer as well }
                      If Not Assigned(TargetRecordBuf) Then
                        TargetRecordBuf := RecordBuf;

                      Result := seTransactionStart(rpDB, False, False, TransID);
                      If Result <> DBIERR_NONE Then
                        Abort;
                      Try
                        NextFlushPoint := aiFlush;
                        NextSnapshotPoint := aiSnapshot;
                        {$IFNDEF ThreadedRebuilds}
                        {NextYieldPoint := aiYield;}{!!.03}
                        {$ENDIF}

                        RefNr.iLow := 0;
                        RefNr.iHigh := 0;

                        { Loop through all the nondeleted records... }
                        rpCursor.Table.GetNextRecordSeq(rpDB.TransactionInfo,
                          RefNr, RecordBuf, UndeleteRecords, OnlyDeleted, aFlag);

                        AutoInc := FFTblReadAutoInc(rpCursor.Table.Files[0], rpDB.TransactionInfo, AutoStep);
                        Mx := FFTblReadMaxRecords(rpCursor.Table.Files[0], rpDB.TransactionInfo);
                        Ps := FFTblReadTablePassword(rpCursor.Table.Files[0], rpDB.TransactionInfo);
                        Ps3 := FFTblReadTablePasswordRest(rpCursor.Table.Files[0], rpDB.TransactionInfo);
                        Ps1 := FFTblReadTableDBID(rpCursor.Table.Files[0], rpDB.TransactionInfo);
                        Ps2:= FFTblReadTableFlags(rpCursor.Table.Files[0], rpDB.TransactionInfo);

                        FFTblSetTablePassword(rpTargetCursor.Table.Files[0], rpDB.TransactionInfo, ps);
                        FFTblSetTablePasswordRest(rpTargetCursor.Table.Files[0], rpDB.TransactionInfo, ps3);
                        FFTblSetMaxRecords(rpTargetCursor.Table.Files[0], rpDB.TransactionInfo,mx);
                        FFTblSetTableFlags(rpTargetCursor.Table.Files[0], rpDB.TransactionInfo,ps2);
                        FFTblSetTableDBID(rpTargetCursor.Table.Files[0], rpDB.TransactionInfo,ps1);

                        If AutoIncField <> -1 Then
                          Begin
                            If AutoIncFieldType = fstAutoInc32 Then
                              FFTblSetAutoInc(rpTargetCursor.Table.Files[0], rpDB.TransactionInfo, AutoInc, AutoStep)
                            Else If AutoIncFieldType = fstAutoInc64 Then
                              FFTblSetAutoInc(rpTargetCursor.Table.Files[0], rpDB.TransactionInfo, AutoInc, AutoStep);
                          End;
                        While Not ((RefNr.iLow = 0) And (RefNr.iHigh = 0)) Do
                          Begin
                            Inc(RecordsRead);

                            { Copy record to the new output file, restructuring
                              along the way if needed. }
                            If Restructuring Then
                              Begin
                                FillTargetBuffer(rpCursor,
                                  rpTargetCursor,
                                  RecordBuf,
                                  TargetRecordBuf,
                                  rpFieldMap, {!!.03}
                                  BLOBBytesCopied); {!!.03}
                              End
                            Else
                              Begin

                                { Copy BLOBs, if any, to the new table }
                                Result := CopyBLOBs(BLOBBytesCopied); {!!.03}
                                If Result <> DBIERR_NONE Then
                                  Abort;
                              End;

                            { Add the record into the new table }
                            Result := rpTargetCursor.InsertRecord(TargetRecordBuf, ffsltExclusive, aflag, arefNr);
                            If Result <> DBIERR_NONE Then
                              Abort;

                            If AutoIncField <> -1 Then
                              Begin
                                If AutoIncFieldType = fstAutoInc32 Then
                                  Begin
                                    rpTargetCursor.Table.Dictionary.GetRecordField(AutoIncField,
                                      TargetRecordBuf,
                                      IsNull,
                                      @ThisAutoIncValue32);
                                    If Not IsNull And (ThisAutoIncValue32 > AutoIncHighValue32) Then
                                      AutoIncHighValue32 := ThisAutoIncValue32;
                                  End
                                Else If AutoIncFieldType = fstAutoInc64 Then
                                  Begin
                                    rpTargetCursor.Table.Dictionary.GetRecordField(AutoIncField,
                                      TargetRecordBuf,
                                      IsNull,
                                      @ThisAutoIncValue64);
                                    If Not IsNull And (ThisAutoIncValue64 > AutoIncHighValue64) Then
                                      AutoIncHighValue64 := ThisAutoIncValue64;
                                  End;
                              End;

                            Inc(RecordsWritten);

                            { See if it's time to flush our work so far }
                            If (RecordsRead >= NextFlushPoint) Or {!!.03}
                            (BLOBBytesCopied >= MaxBLOBBytes) Or {!!.03}
                            (seBufMgr.RAMUsed >= RAMTrigger) Then
                              Begin {!!.03}

                                If (RecordsRead >= NextFlushPoint) Then {!!.13}
                                  Inc(NextFlushPoint, aiFlush);
                                BLOBBytesCopied := 0; {!!.03}
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

                            rpCursor.Table.GetNextRecordSeq(rpDB.TransactionInfo,
                              RefNr, RecordBuf, UndeleteRecords, OnlyDeleted, aFlag);
                          End;

                        { Save all data changes }
                        If seTransactionCommit(rpDB) <> DBIERR_NONE Then
                          FSRaiseExceptionNoData(EfsServerException,
                            fsStrResServer,
                            fserrTransactionFailed);
                        {Begin !!.01}
                      Except
                        seTransactionRollback(rpDB);
                        Raise;
                        {End !!.01}
                      End;
                    Finally
                      ReleaseBLOBTransferBuffer;
                    End;
                  Finally
                    { Deallocate the record buffer }
                    FFFreeMem(RecordBuf, BufLength);
                  End;
                Finally
                  { Close the target cursor }
                  rpTargetCursor.State := ffosInactive;
                  CursorClose(rpTargetCursor.CursorID);
                End;
              Finally
                { Close the source cursor }
                rpCursor.State := ffosInactive;
                CursorClose(rpCursor.CursorID);
              End;
            Except
              { An error occurred somewhere in the process; clean up the files}
              TableDelete(rpDB.DatabaseID, TargetName);
              Raise;
            End;

            { Replace the original file with the working file.
              First step: Rename the old table.  We will restore it if an
                error occurs. }
            SaveBaseName := SetTargetBaseName(rpDB.Folder.Path, ffcSaveBaseName);
            Result := TableRename(rpDB.DatabaseID, SourceName, SaveBaseName);
            If Result = DBIERR_NONE Then
              Begin
                Try
                  { Rename the new table to the old table. }
                  Result := TableRename(rpDB.DatabaseID, TargetName, SourceName);
                Except
                  { If an exception occurs then put the original table back in its
                    place. }
                  TableRename(rpDB.DatabaseID, SaveBaseName, SourceName);
                  Raise;
                End;
                { Everything worked so far.  Delete the original table. }
                Result := TableDelete(rpDB.DatabaseID, SaveBaseName);
              End;
          Finally
            rpDB.State := ffosInactive;
            rpDB.Free;
            { Release the output record buffer, if allocated separately from
              the input record buffer }
            If Restructuring Then
              FFFreeMem(TargetRecordBuf, TargetBufLength);
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
        { Shut down the rebuild status indicator }
        rpRebuildStatus.MakeSnapshot(RecordsRead,
          RecordsWritten,
          Result);
        RebuildDeregister(rpRebuildStatus.RebuildID);
      End;
    End;
End;