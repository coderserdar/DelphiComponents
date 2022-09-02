Function TFSServer.TableRestructure(aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName;
  aDictionary: TFSInfoDict;
  aFieldMap: TFSSpecStringList;
  Var aRebuildID: Longint;
  aRangeError: boolean): TffResult;
Var
  DB: TfsSrcDatabase;
  RebuildParamsPtr: PffSrRebuildParams;
  TargetBasename: TffFileName;
  RecordInfo: TffRecordInfo;
  CursorID: TffCursorID;
  SourceClosed: Boolean;
  SourceDeleted: Boolean;
  I, L: Integer;
  Inx: Integer;
  KeyProcItem: TfsKeyProcItem;
  StartedTrans: boolean;
  Rflags: Word;
  Passwd: LongWord;

  Function ValidateFieldMap(aSourceCursorID,
    aTargetCursorID: Longint;
    aFieldExMap: TfsSrcFieldMapList): TffResult;
  Var
    I: Integer;
  Begin
    Result := DBIERR_NONE;
    With aFieldExMap Do
      Begin
        For I := 0 To Count - 1 Do
          Begin
            Result := seConvertSingleField(Nil,
              Nil,
              aSourceCursorID,
              aTargetCursorID,
              SourceField[I].Number,
              TargetField[I].Number,
              Nil,
              0,
              aRangeError);
            If Result <> DBIERR_NONE Then
              FSRaiseException(EfsServerException, fsStrResServer, fserrBadFieldXform, [SourceField[I].Name, TargetField[I].Name]);
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
    SourceClosed := False;
    SourceDeleted := False;
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If Result <> DBIERR_NONE Then
      Exit;
    Try
      Result := DB.NotifyExtenders(ffeaBeforeTabRestruct, ffeaTabRestructFail);
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
          { Open the table for exclusive write access; TablePackPrim
            is responsible for closing the cursor. }
          Result := TableOpen(rpDB.DatabaseID,
            aTableName, False, '', 0,
            omReadWrite, smExclusive, DB.Timeout, CursorID,
            Nil, False);
          If Result <> DBIERR_NONE Then Abort;

          seCheckCursorIDAndGet(CursorID, TfsSrBaseCursor(rpCursor));
          rpCursor.State := ffosActive;
          rpCursor.CloseTable := True;
          Rflags := FFTblReadTableFlags(rpCursor.Table.Files[0], rpDB.TransactionInfo);
          If ((Rflags And fsTableDontRestructure) <> 0) Then
            Begin
              Result := DBIERR_NOTSUFFFIELDRIGHTS;
              Abort;
            End;

          Passwd := 0;
          Passwd :=FFTblReadTablePasswordRest(rpCursor.Table.Files[0], rpDB.TransactionInfo);
          If Passwd > 0 Then
            If Not DB.Session.TablePasswordList.Exists(IntToStr(Passwd)) Then
              Begin
                Result := DBIERR_NOTSUFFFIELDRIGHTS;
                Abort;
            End;
          { Get the total nondeleted records in the table }
          FFTblGetRecordInfo(rpCursor.Table.Files[0],
            rpDB.TransactionInfo,
            RecordInfo);
          Try

            { Check the source and target indexes.  Can't preserve data if the
              sequence of user-defined indexes has been altered. }
            If Assigned(aFieldMap) Then
              With rpCursor.Table.Dictionary Do
                Begin
                  If IndexCount > aDictionary.IndexCount Then
                    Begin
                      L := aDictionary.IndexCount;
                      { Deleting user-defined indexes is OK }
                    End
                  Else
                    Begin
                      L := IndexCount;
                      { see if we've added user-defined indexes }
                      For I := L To aDictionary.IndexCount - 1 Do
                        If (aDictionary.IndexType[I] = itUdfDll) And
                          (RecordInfo.riRecCount > 0) Then
                          Begin {!!.13}
                            Result := DBIERR_INVALIDRESTROP;
                            Abort; { to trigger exception handlers }
                          End;
                    End;

                  { See if there have been changes to existing user-defined indexes }
                  For I := 1 To L - 1 Do
                    If (aDictionary.IndexType[I] = itUdfDll) And
                      ((IndexType[I] <> itUdfDll) Or
                      (IndexKeyLength[I] <> aDictionary.IndexKeyLength[I])) Then
                      Begin
                        Result := DBIERR_INVALIDRESTROP;
                        Abort; { to trigger exception handlers }
                      End;
                End;

            { Setup the destination file(s).  Use a basename of _REST<x>
              where <x> starts at 1 and is incremented upward until a
              nonexistant filename is found. }
            TargetBasename := SetTargetBasename(DB.Folder.Path, '_REST');

            { Setup the new (temporary) table }
            Result := TableBuild(rpDB.DatabaseID, False, TargetBasename, False,
              aDictionary);
            If Result <> DBIERR_NONE Then Abort;
            Try

              { If a field map is given to us, then pass info on so the
                data can be reorganized.  Otherwise, we just restructure
                the table and lose all data. }

              If Assigned(aFieldMap) Then
                Begin

                  { The underlying primitive method is responsible for
                    releasing this memory block }
                  rpFieldMap := TfsSrcFieldMapList.Create(rpCursor.Table.Dictionary,
                    aDictionary);
                  Try

                    { Bind the user-defined index routines (if any) to the
                      target table }
                    For I := 1 To aDictionary.IndexCount - 1 Do
                      If (aDictionary.IndexType[I] = itUdfDll) Then
                        With Configuration Do
                          Begin
                            With rpCursor.Table Do
                              Inx := KeyProcList.KeyProcIndex(Folder.Path, BaseName,
                                i);
                            If (Inx <> -1) Then
                              Begin
                                KeyProcItem := KeyProcList[Inx];
                                With KeyProcItem Do
                                  Begin
                                    Link;
                                    AddKeyProc(DB.Folder.Path, TargetBasename, I,
                                      DLLName, BuildKeyName, CompareKeyName);
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
                        TargetBasename, False, '', 0,
                        omReadWrite, smExclusive, DB.Timeout,
                        CursorID, Nil, False);
                      If Result <> DBIERR_NONE Then Abort;
                      seCheckCursorIDAndGet(CursorID,
                        TfsSrBaseCursor(rpTargetCursor));

                      rpTargetCursor.State := ffosActive;
                      rpTargetCursor.Table.AddAttribute(fffaBLOBChainSafe); {!!.03}

                      rpTargetCursor.CloseTable := True;

                    Finally

                      { Get rid of the temporary user-defined index bindings }
                      For I := 1 To aDictionary.IndexCount - 1 Do
                        If (aDictionary.IndexType[I] = itUdfDll) Then
                          With Configuration Do
                            If KeyProcList.KeyProcExists(DB.Folder.Path,
                              TargetBaseName, I) Then
                              KeyProcList.DeleteKeyProc(DB.Folder.Path,
                                TargetBaseName, I);
                    End;

                    Try

                      { Expand the field map from string list to structured list }
                      Result := rpFieldMap.AddStringList(aFieldMap);
                      If Result <> DBIERR_NONE Then Abort;

                      { Validate the field map; translate into extended field map }
                      ValidateFieldMap(rpCursor.CursorID,
                        rpTargetCursor.CursorID,
                        rpFieldMap);

                      rpRebuildStatus := RebuildRegister
                        (TfsSrcClient(rpDB.Client).ClientID,
                        RecordInfo.riRecCount);
                      Try
                        aRebuildID := rpRebuildStatus.RebuildID;

                        { Create a separate thread for the restructure operation }
                        TfsSrcRestructureThread.Create(Self, RebuildParamsPtr, aRangeError, False, False);

                        { The thread constructor is responsible for deallocating
                          this memory block }
                        RebuildParamsPtr := Nil;
                      Except
                        RebuildDeregister(aRebuildID);
                        Raise;
                      End;
                    Except
                      rpTargetCursor.State := ffosInactive;
                      CursorClose(rpTargetCursor.CursorID);
                      Raise;
                    End;
                  Except
                    rpFieldMap.Free;
                    Raise;
                  End;
                End { if Assigned(aFieldMap) }
              Else
                Begin
                  { No field map was given, so we just restructure the
                    table and lose the data }

                  { Close the source cursor; the locks are cleared for us }
                  rpCursor.State := ffosInactive;
                  CursorClose(rpCursor.CursorID);
                  SourceClosed := True; { Defuse the exception handler }

                  { Delete the original files, rename the working file }
                  Result := TableDelete(rpDB.DatabaseID, aTableName);
                  SourceDeleted := True; { Defuse the exception handler }

                  { Rename the temporary work table to the original tablename }
                  If Result = DBIERR_NONE Then
                    Result := TableRename(rpDB.DatabaseID, TargetBasename, aTableName);

                  { Deallocate the parameters buffer }
                  FFFreeMem(RebuildParamsPtr, SizeOf(RebuildParamsPtr^)); {!!.13}
                  RebuildParamsPtr := Nil; { Defuse the exception handler }
                End; { if not Assigned(aFieldMap) }

              { Clean up after ourselves if there are exeptions }

            Except
              { Clean up the files }
              If Not SourceDeleted Then
                seTableDeletePrim(rpDB, TargetBaseName);
              Raise;
            End;
          Except
            rpCursor.State := ffosInactive;
            If Not SourceClosed Then
              CursorClose(rpCursor.CursorID);
            rpDB.State := ffosInactive;
            Raise;
          End;
        End;
    Finally
      If RebuildParamsPtr <> Nil Then
        Begin
          Try
            With RebuildParamsPtr^ Do
              Begin
                If rpCursor <> Nil Then
                  Begin
                    rpCursor.State := ffosInactive;
                    CursorClose(rpCursor.CursorID);
                    SourceClosed := True; { Defuse the exception handler }
                  End;
              End;
          Finally
            FFFreeMem(RebuildParamsPtr, SizeOf(RebuildParamsPtr^)); {!!.13}
            RebuildParamsPtr := Nil;
          End;
        End;
      DB.Deactivate;
    End;
  Except
    On E: Exception Do
      Begin
        If Result = DBIERR_NONE Then
          Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly); {!!.01}
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

