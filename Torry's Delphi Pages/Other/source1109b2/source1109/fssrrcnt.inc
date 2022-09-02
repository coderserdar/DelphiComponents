Function TFSServer.TableGetRecCountAsync(aCursorID: TffCursorID;
  Var aTaskID: Longint): TffResult;
Var
  Cursor: TfsSrBaseCursor;
  RebuildParamsPtr: PffSrRebuildParams;
  RecordInfo: TffRecordInfo;
Begin
  Result := DBIERR_NONE;
  aTaskID := -1;

  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    Try
      If Result <> DBIERR_NONE Then
        Exit;
      FFGetMem(RebuildParamsPtr, SizeOf(RebuildParamsPtr^)); {!!.13}
      Try
        FillChar(RebuildParamsPtr^, SizeOf(RebuildParamsPtr^), 0);
        With RebuildParamsPtr^ Do
          Begin
            rpDB := Nil;
            rpTableName := '';
            rpIndexName := '';
            rpIndexID := 0;

            Try
              rpCursor := TfsSrcCursor(Cursor.CloneCursor(omReadOnly));
              Try
                rpCursor.State := ffosActive;
                { Get the total nondeleted records in the table }
                FFTblGetRecordInfo(rpCursor.Table.Files[0],
                  rpCursor.Database.TransactionInfo,
                  RecordInfo);
                rpRebuildStatus := RebuildRegister
                  (TfsSrcClient(rpCursor.Database.Client).ClientID,
                  RecordInfo.riRecCount);
                aTaskID := rpRebuildStatus.RebuildID;

                { Create a separate thread for the pack operation }
                TfsSrcGetRecordCountThread.Create(Self, RebuildParamsPtr, False, False, False);

                { The thread constructor (32-bit) or message handler (16-bit)
                  are responsible for deallocating this memory block }
                RebuildParamsPtr := Nil;
              Except
                rpCursor.State := ffosInactive;
                CursorClose(rpCursor.CursorID);
                Raise;
              End;
            Except
              RebuildDeregister(aTaskID);
              Raise;
            End;
          End;
      Except
        If Assigned(RebuildParamsPtr) Then {!!.13}
          FFFreeMem(RebuildParamsPtr, SizeOf(RebuildParamsPtr^)); {!!.13}
        Raise;
      End;
    Finally
      Cursor.Deactivate;
    End;
  Except
    On E: Exception Do
      Begin
        If Result = DBIERR_NONE Then
          Result := ConvertServerException(E, FEventLog);
      End;
  End;
End;

Type
  TfsSrcCursorHacker = Class(TfsSrcCursor);

Function TFSServer.seTableGetRecordCountPrim(aRebuildParamsPtr: PffSrRebuildParams): TffResult;
Const
  { Action intervals }
  aiSnapshot = 128; { every x records, update the status snapshot }
Var
  Action: TffSearchKeyAction;
  KeyCompareResult: Integer;
  Info: TffRecordInfo;
  RecordsRead: Longint;
  RecordsMatched: Longint;
  NextSnapshotPoint: Longint;
Begin
  Result := DBIERR_NONE;
  aRebuildParamsPtr^.rpCursor.AcqContentLock(fsclmRead);
  Try
    With aRebuildParamsPtr^, TfsSrcCursorHacker(rpCursor) Do
      Begin
        rpCursor.Timeout := 0;
        NextSnapshotPoint := aiSnapshot;
        RecordsRead := 0;
        RecordsMatched := 0;
        Try
          If bcHasRange Or Assigned(bcFilter) Then
            Begin

              SetToBegin;
              If bcHasRange And bcRng1Valid Then
                Begin
                  {position at start of range}
                  If bcRng1Incl Then
                    Action := skaGreaterEqual
                  Else
                    Action := skaGreater;
                  {note: the following FindKey call will always return true in
                   this case}
                  Move(bcRng1Key^, bcCurKey^, scKeyLen);
                  With bcCompareData Do
                    Begin
                      cdFldCnt := bcRng1FldCnt;
                      cdPartLen := bcRng1PtlLen;
                    End;
                  Table.FindKey(bcKID, bcInfo.RefNr, Database.TransactionInfo,
                    bcCurKey, bcInfo.KeyPath, Action);
                  {check whether the keypath was positioned at EOF, if so the
                   start of the range is at EOF, so it's not likely we'll find a
                   'next' key or any keys at all <g>}
                  If (bcInfo.KeyPath.kpPos = kppEOF) Then
                    Begin
                      {note the reset of the cursor position still occurs}
                      Exit;
                    End;
                  {make sure that the keypath is on the crack before the key so that
                   the next key call in a minute returns the right record}
                  If (bcInfo.KeyPath.kpPos = kppOnKey) Then
                    bcInfo.KeyPath.kpPos := kppOnCrackBefore;
                End;
              {while not EOF or other error do}
              While (Result = DBIERR_NONE) Do
                Begin
                  {readnext key}
                  Result := Table.GetNextKey(bcKID, bcInfo.RefNr,
                    Database.TransactionInfo,
                    bcCurKey, bcInfo.KeyPath);
                  If (Result = DBIERR_NONE) Then
                    Begin
                      {check that we're in range if required}
                      If bcHasRange And bcRng2Valid Then
                        Begin
                          {check whether beyond end of range}
                          With bcCompareData Do
                            Begin
                              cdFldCnt := bcRng2FldCnt;
                              cdPartLen := bcRng2PtlLen;
                            End;
                          KeyCompareResult :=
                            Table.CompareKeysForCursor(bcKID, bcCurKey, bcRng2Key);
                          If (KeyCompareResult > 0) Or
                            ((KeyCompareResult = 0) And (Not bcRng2Incl)) Then
                            Begin
                              Result := DBIERR_EOF;
                            End
                          Else {key is in range}
                            Begin
                              If Assigned(bcFilter) Then
                                Begin
                                  Table.GetRecordNoLock(Database.TransactionInfo,
                                    bcInfo.RefNr,
                                    bcRecordData);
                                  If bcFilter.MatchesRecord(bcRecordData) Then
                                    inc(RecordsMatched);
                                End
                              Else
                                inc(RecordsMatched);
                            End;
                        End
                      Else {end of range = end of index path}
                        Begin
                          If Assigned(bcFilter) Then
                            Begin
                              Table.GetRecordNoLock(Database.TransactionInfo,
                                bcInfo.RefNr, bcRecordData);
                              If bcFilter.MatchesRecord(bcRecordData) Then
                                inc(RecordsMatched);
                            End
                          Else
                            Inc(RecordsMatched);
                        End;
                      inc(RecordsRead);

                      { See if it's time to update the status packet }
                      If RecordsRead >= NextSnapshotPoint Then
                        Begin
                          Inc(NextSnapshotPoint, aiSnapshot);
                          rpRebuildStatus.MakeSnapshot(RecordsRead,
                            RecordsMatched,
                            DBIERR_NONE);
                        End;
                    End;
                End;
            End
          Else
            Begin
              FFTblGetRecordInfo(Table.Files[0], Database.TransactionInfo, Info);
              RecordsMatched := Info.riRecCount;
              RecordsRead := Info.riRecCount;
            End;
          Result := DBIERR_NONE;
        Finally
          { Shut down the rebuild status indicator }
          rpRebuildStatus.MakeSnapshot(RecordsRead,
            RecordsMatched,
            Result);
          RebuildDeregister(rpRebuildStatus.RebuildID);
        End;
      End;
  Finally
    aRebuildParamsPtr^.rpCursor.RelContentLock(fsclmRead);
    aRebuildParamsPtr^.rpCursor.Deactivate;
    CursorClose(aRebuildParamsPtr^.rpCursor.CursorID); {!!.10}
  End;
End;

