{*********************************************************}
{* FlashFiler: table restructure include file            *}
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

function TffServerEngine.TableRestructure(aDatabaseID : TffDatabaseID;
                                    const aTableName  : TffTableName;
                                          aDictionary : TffDataDictionary;
                                          aFieldMap   : TffStringList;
                                      var aRebuildID  : LongInt): TffResult;
var
  DB : TffSrDatabase;
  RebuildParamsPtr: PffSrRebuildParams;
  TargetBasename: TffFileName;
  RecordInfo: TffRecordInfo;
  CursorID: TffCursorID;
  SourceClosed: Boolean;
  SourceDeleted: Boolean;
  I, L: Integer;
  Inx: Integer;
  KeyProcItem : TffKeyProcItem;
  StartedTrans : boolean;

  function ValidateFieldMap(aSourceCursorID,
                            aTargetCursorID: LongInt;
                            aFieldExMap: TffSrFieldMapList): TffResult;
  var
    I: Integer;
  begin
    Result := DBIERR_NONE;
    with aFieldExMap do begin
      for I := 0 to Count - 1 do begin
        Result := seConvertSingleField(nil,
                                       nil,
                                       aSourceCursorID,
                                       aTargetCursorID,
                                       SourceField[I].Number,
                                       TargetField[I].Number,
                                       nil,
                                       0);
        if Result <> DBIERR_NONE then
          FFRaiseException(EffServerException, ffStrResServer, fferrBadFieldXform, [SourceField[I].Name, TargetField[I].Name]);
      end;
    end;
  end;

  function SetTargetBasename(Path: TffPath; Root: TffFileName): TffFileName;
  var
    I: Integer;
  begin
    I := 0;
    repeat
      Inc(I);
      Result := Root + IntToStr(I);
    until not FFFileExists(FFMakeFullFilename(Path,
                                              FFMakeFileNameExt(Result,
                                                                ffc_ExtForData)));
  end;

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
    SourceClosed := False;
    SourceDeleted := False;
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if Result <> DBIERR_NONE then
      Exit;
    try
      Result := DB.NotifyExtenders(ffeaBeforeTabRestruct, ffeaTabRestructFail);
      { Exit if the extenders give us an error. }
      if Result <> DBIERR_NONE then
        exit;

      FFGetMem(RebuildParamsPtr, SizeOf(RebuildParamsPtr^));           {!!.13}
      FillChar(RebuildParamsPtr^, SizeOf(RebuildParamsPtr^), 0);
      with RebuildParamsPtr^ do begin
        rpDB := TffSrDatabase.Create(DB.Engine,
                                     DB.Session,
                                     DB.Folder,
                                     DB.Alias,
                                     DB.OpenMode,
                                     DB.ShareMode,
                                     DB.Timeout,
                                     DB.CheckSpace);                   {!!.11}
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
                            nil);
        if Result <> DBIERR_NONE then Abort;
        seCheckCursorIDAndGet(CursorID, TffSrBaseCursor(rpCursor));
        rpCursor.State := ffosActive;
        rpCursor.CloseTable := True;
        { Get the total nondeleted records in the table }
        FFTblGetRecordInfo(rpCursor.Table.Files[0],
                           rpDB.TransactionInfo,
                           RecordInfo);
        try

          { Check the source and target indexes.  Can't preserve data if the
            sequence of user-defined indexes has been altered. }
          if Assigned(aFieldMap) then
            with rpCursor.Table.Dictionary do begin
              if IndexCount > aDictionary.IndexCount then begin
                L := aDictionary.IndexCount;
                { Deleting user-defined indexes is OK }
              end
              else begin
                L := IndexCount;
                { see if we've added user-defined indexes }
                for I := L to aDictionary.IndexCount - 1 do
                  if (aDictionary.IndexType[I] = itUserDefined) and
                     (RecordInfo.riRecCount > 0) then begin            {!!.13}
                    Result := DBIERR_INVALIDRESTROP;
                    Abort;  { to trigger exception handlers }
                  end;
              end;

              { See if there have been changes to existing user-defined indexes }
              for I := 1 to L - 1 do
                if (aDictionary.IndexType[I] = itUserDefined) and
                   ((IndexType[I] <> itUserDefined) or
                    (IndexKeyLength[I] <> aDictionary.IndexKeyLength[I])) then begin
                  Result := DBIERR_INVALIDRESTROP;
                  Abort;  { to trigger exception handlers }
                end;
            end;

          { Setup the destination file(s).  Use a basename of _REST<x>
            where <x> starts at 1 and is incremented upward until a
            nonexistant filename is found. }
          TargetBasename := SetTargetBasename(DB.Folder.Path, '_REST');

          { Setup the new (temporary) table }
          Result := TableBuild(rpDB.DatabaseID, False, TargetBasename, False,
                               aDictionary);
          if Result <> DBIERR_NONE then Abort;
          try

            { If a field map is given to us, then pass info on so the
              data can be reorganized.  Otherwise, we just restructure
              the table and lose all data. }

            if Assigned(aFieldMap) then begin

              { The underlying primitive method is responsible for
                releasing this memory block }
              rpFieldMap := TffSrFieldMapList.Create(rpCursor.Table.Dictionary,
                                                     aDictionary);
              try

                { Bind the user-defined index routines (if any) to the
                  target table }
                for I := 1 to aDictionary.IndexCount - 1 do
                  if (aDictionary.IndexType[I] = itUserDefined) then
                    with Configuration do begin
                      with rpCursor.Table do
                        Inx := KeyProcList.KeyProcIndex(Folder.Path, BaseName,
                                                        i);
                      if (Inx <> -1) then begin
                        KeyProcItem := KeyProcList[Inx];
                        with KeyProcItem do begin
                          Link;
                          AddKeyProc(DB.Folder.Path, TargetBasename, I,
                                        DLLName, BuildKeyName, CompareKeyName);
                        end;
                      end else
                        FFRaiseExceptionNoData(EffServerException,
                                               ffStrResServer,
                                               fferrResolveTableLinks);
                    end;
                try

                  { Open the destination table for exclusive write access;
                    TablePackPrim is responsible for closing the cursor }
                  Result := TableOpen(rpDB.DatabaseID,
                                      TargetBasename, False, '', 0,
                                      omReadWrite, smExclusive, DB.Timeout,
                                      CursorID, nil);
                  if Result <> DBIERR_NONE then Abort;
                  seCheckCursorIDAndGet(CursorID,
                                        TffSrBaseCursor(rpTargetCursor));
                  rpTargetCursor.State := ffosActive;
                  rpTargetCursor.Table.AddAttribute(fffaBLOBChainSafe); {!!.03}
                  rpTargetCursor.CloseTable := True;
                finally

                  { Get rid of the temporary user-defined index bindings }
                  for I := 1 to aDictionary.IndexCount - 1 do
                    if (aDictionary.IndexType[I] = itUserDefined) then
                      with Configuration do
                        if KeyProcList.KeyProcExists(DB.Folder.Path,
                                                     TargetBaseName, I) then
                          KeyProcList.DeleteKeyProc(DB.Folder.Path,
                                                    TargetBaseName, I);
                end;

                try

                  { Expand the field map from string list to structured list }
                  Result := rpFieldMap.AddStringList(aFieldMap);
                  if Result <> DBIERR_NONE then Abort;

                  { Validate the field map; translate into extended field map }
                  ValidateFieldMap(rpCursor.CursorID,
                                   rpTargetCursor.CursorID,
                                   rpFieldMap);

                  rpRebuildStatus := RebuildRegister
                                       (TffSrClient(rpDB.Client).ClientID,
                                        RecordInfo.riRecCount);
                  try
                    aRebuildID := rpRebuildStatus.RebuildID;

                    { Create a separate thread for the restructure operation }
                    TffSrRestructureThread.Create(Self, RebuildParamsPtr);

                    { The thread constructor is responsible for deallocating
                      this memory block }
                    RebuildParamsPtr := nil;
                  except
                    RebuildDeregister(aRebuildID);
                    raise;
                  end;
                except
                  rpTargetCursor.State := ffosInactive;
                  CursorClose(rpTargetCursor.CursorID);
                  raise;
                end;
              except
                rpFieldMap.Free;
                raise;
              end;
            end { if Assigned(aFieldMap) }
            else begin
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
              if Result = DBIERR_NONE then
                Result := TableRename(rpDB.DatabaseID, TargetBasename, aTableName);

              { Deallocate the parameters buffer }
              FFFreeMem(RebuildParamsPtr, SizeOf(RebuildParamsPtr^)); {!!.13}
              RebuildParamsPtr := nil; { Defuse the exception handler }
            end;{ if not Assigned(aFieldMap) }

          { Clean up after ourselves if there are exeptions }

          except
            { Clean up the files }
            if not SourceDeleted then
              seTableDeletePrim(rpDB, TargetBaseName);
            raise;
          end;
        except
          rpCursor.State := ffosInactive;
          if not SourceClosed then
            CursorClose(rpCursor.CursorID);
          rpDB.State := ffosInactive;
          raise;
        end;
      end;
    finally
      DB.Deactivate;
    end;
  except
    on E : Exception do begin
      if Result = DBIERR_NONE then
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);  {!!.01}
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
