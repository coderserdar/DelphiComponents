{*********************************************************}
{* FlashFiler: pack table include file                   *}
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

const
  { The name given the old table while we are replacing it with the new
    table. }
  ffcPackBaseName = '_PACK';
  ffcSaveBaseName = '_PACKSV';

function TffServerEngine.TablePack(aDatabaseID : TffDatabaseID;
                             const aTableName  : TffTableName;
                               var aRebuildID  : LongInt): TffResult;
var
  DB : TffSrDatabase;
  RebuildParamsPtr: PffSrRebuildParams;
  SourceDict: TffServerDataDict;
  TargetDict: TffServerDataDict;
  TargetBasename: TffFileName;
  RecordInfo: TffRecordInfo;
  CursorID: TffCursorID;
  I: Integer;
  Inx: Integer;
  KeyProcItem : TffKeyProcItem;
  StartedTrans : boolean;
  TransID : TffTransID;
  SrcTblVersionChanged : Boolean;                                      {!!.11}

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
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if Result <> DBIERR_NONE then
      Exit;
    try
      Result := DB.NotifyExtenders(ffeaBeforeTabPack, ffeaTabPackFail);
      { Exit if the extenders give us an error. }
      if Result <> DBIERR_NONE then
        Exit;

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

        try
          { Open the table for exclusive write access; TablePackPrim
            is responsible for closing the cursor. }
{Begin !!.11}
          SrcTblVersionChanged := False;
          if rpDB.Folder.PackSrcTableVersion <> 0 then begin
            SrcTblVersionChanged := True;
            rpDB.Folder.ExistingTableVersion :=
              rpDB.Folder.PackSrcTableVersion;
          end;
{End !!.11}
          Result := TableOpen(rpDB.DatabaseID,
                              aTableName, False, '', 0, omReadWrite,
                              smExclusive, DB.Timeout, CursorID, nil);
{Begin !!.11}
          if SrcTblVersionChanged then
            rpDB.Folder.ExistingTableVersion := 0;
{End !!.11}
          if Result <> DBIERR_NONE then Abort;

          seCheckCursorIDAndGet(CursorID, TffSrBaseCursor(rpCursor));
          rpCursor.State := ffosActive;
          rpCursor.CloseTable := True;

          try
            { Start an implicit, read-only transaction. }
            if not assigned(DB.Transaction) then begin
              Result := seTransactionStart(rpDB, false,
                                           ffcl_TrImplicit, TransID);
              StartedTrans := (Result = DBIERR_NONE);
            end;

            if Result <> DBIERR_NONE then Abort;

            { Get the total nondeleted records in the table }
            FFTblGetRecordInfo(rpCursor.Table.Files[0],
                               rpDB.TransactionInfo,
                               RecordInfo);
            if StartedTrans then begin
              seTransactionCommit(rpDB);
              StartedTrans := False;
            end;
            rpRebuildStatus := RebuildRegister
                                 (TffSrClient(rpDB.Client).ClientID,
                                  RecordInfo.riRecCount);
            aRebuildID := rpRebuildStatus.RebuildID;

            { Setup the destination file(s).  Use a basename of _PACK<x>
              where <x> starts and 1 and is incremented upward until a
              nonexistant filename is found. }
            TargetBasename := SetTargetBasename(DB.Folder.Path,
                                                ffcPackBaseName);

            { Capture the data dictionary }
            SourceDict := rpCursor.Table.Dictionary;
            TargetDict := TffServerDataDict.Create(SourceDict.BlockSize);
            try

              { Setup the new (temporary) table }
              TargetDict.Assign(SourceDict);
              Result := TableBuild(rpDB.DatabaseID, False, TargetBasename,
                                   False, TargetDict);
              if Result <> DBIERR_NONE then Abort;
              try

                { Bind the user-defined index routines (if any) to the
                  target table }
                for I := 1 to TargetDict.IndexCount - 1 do
                  if (TargetDict.IndexType[I] = itUserDefined) then
                    with Configuration do begin
                      with rpCursor.Table do
                        Inx := KeyProcList.KeyProcIndex(Folder.Path,
                                                        BaseName, i);
                      if (Inx <> -1) then begin
                        KeyProcItem := KeyProcList[Inx];
                        with KeyProcItem do begin
                          Link;
                          AddKeyProc(DB.Folder.Path, TargetBasename, I,
                                        DLLName, BuildKeyName,
                                        CompareKeyName);
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
                                      TargetBasename, False,
                                      '', 0, omReadWrite, smExclusive,
                                      DB.Timeout, CursorID, nil);
                  if Result <> DBIERR_NONE then Abort;
                  seCheckCursorIDAndGet(CursorID,
                                        TffSrBaseCursor(rpTargetCursor));
                  rpTargetCursor.State := ffosActive;
                  rpTargetCursor.Table.AddAttribute(fffaBLOBChainSafe); {!!.03}
                  rpTargetCursor.CloseTable := True;
                finally

                  { Get rid of the temporary user-defined index bindings }
                  for I := 1 to TargetDict.IndexCount - 1 do
                    if (TargetDict.IndexType[I] = itUserDefined) then
                      with Configuration do
                        if KeyProcList.KeyProcExists(DB.Folder.Path,
                                                     TargetBaseName, I) then
                          KeyProcList.DeleteKeyProc(DB.Folder.Path,
                                                    TargetBaseName, I);
                end;

                try
                  { Create a separate thread for the pack operation }
                  TffSrPackThread.Create(Self, RebuildParamsPtr);

                  { The thread constructor is responsible for
                    deallocating this memory block }
                  RebuildParamsPtr := nil;
                except
                  rpTargetCursor.State := ffosInactive;
                  CursorClose(rpTargetCursor.CursorID);
                  raise;
                end;
              except
                { Clean up the files }
                TableDelete(rpDB.DatabaseID, TargetBasename);
                raise;
              end;
{Begin !!.05}
            finally
              TargetDict.Free;
            end;
{End !!.05}
          except
            rpCursor.State := ffosInactive;
            CursorClose(rpCursor.CursorID);
            raise;
          end;
        except
          rpDB.State := ffosInactive;
          RebuildDeregister(aRebuildID);
          raise;
        end;
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

function TffServerEngine.seTablePackPrim(aRebuildParamsPtr: PffSrRebuildParams): TffResult;
const
  { Action intervals }
  aiSnapshot  = 10;   { every x records, update the status snapshot }
  aiYield     = 10;   { every x records, yield for other messages }

{Begin !!.03}
  BLOBBufLen   = ffcl_1MB;      { size of BLOB transfer buffer }
  RecBuf       = ffcl_1MB;      { max memory for records before flushing }
  MaxBLOBBytes = 10 * ffcl_1MB; { max # of BLOB bytes that may be copied before
                                  committing the transaction. }
{End !!.03}
var
  aiFlush : integer;
  BLOBBytesCopied : Integer;                                           {!!.03}
  RecordBuf: PffByteArray;
  BufLength: LongInt;
  SaveBaseName : TffTableName;
  TargetRecordBuf: PffByteArray;
  TargetBufLength: LongInt;
  IsNull: Boolean;
  RAMTrigger : Longint;                                                {!!.03}
  RefNr: TffInt64;
  SourceName: TffTableName;
  TargetName: TffTableName;
  Restructuring: Boolean;
  BLOBBuffer: Pointer;
  AutoIncField: Integer;
  AutoIncHighValue: TffWord32;
  ThisAutoIncValue: TffWord32;
  TransID : TffTransID;

  RecordsRead: LongInt;
  RecordsWritten: LongInt;

  NextFlushPoint: LongInt;
  NextSnapshotPoint: LongInt;
  {$IFNDEF ThreadedRebuilds}
  {NextYieldPoint: LongInt;}                                          {!!.03}
  {$ENDIF}

  procedure FindAutoIncField;
  var
    TargetAutoInc: Integer;
  begin
    with aRebuildParamsPtr^ do
      if rpTargetCursor.Table.Dictionary.HasAutoIncField(TargetAutoInc) then
        AutoIncField := TargetAutoInc;
  end;

  procedure AllocateBLOBTransferBuffer;
  { Don't allocate a BLOB transfer buffer unless we actually
    need one (i.e., there may not be any BLOBs in this record }
  begin
    if not Assigned(BLOBBuffer) then
      FFGetMem(BLOBBuffer, BLOBBufLen);                     
  end;

  procedure ReleaseBLOBTransferBuffer;
  begin
    if Assigned(BLOBBuffer) then
      FFFreeMem(BLOBBuffer, BLOBBufLen);
  end;

  function CopyBLOBs(var BLOBBytesCopied : Integer): TffResult;        {!!.03}
  var
    I: Integer;
    SourceBLOBNr: TffInt64;
    TargetBLOBNr: TffInt64;
    IsNull: Boolean;
  begin
    Result := DBIERR_NONE;

    with aRebuildParamsPtr^ do
      with rpCursor.Table.Dictionary do
        for I := 0 to FieldCount - 1 do

          { Find all the BLOB fields in the record }
          if (FieldType[I] >= fftBLOB) and
             (FieldType[I] <= ffcLastBLOBType) then begin
            GetRecordField(I, RecordBuf, IsNull, @SourceBLOBNr);
            if IsNull then
              with rpTargetCursor.Table.Dictionary do
                SetRecordField(I, TargetRecordBuf, nil)
            else begin
              AllocateBLOBTransferBuffer;

{Begin !!.03}
              inc(BLOBBytesCopied, rpCursor.BLOBGetLength(SourceBLOBNr, Result));
              if Result <> DBIERR_NONE then Abort;
{End !!.03}

              Result := rpTargetCursor.bcBLOBCopy
                          (rpCursor, SourceBLOBNr, TargetBLOBNr);
              if Result <> DBIERR_NONE then Abort;

              with rpTargetCursor.Table.Dictionary do
                SetRecordField(I, TargetRecordBuf, @TargetBlobNr);
            end;
          end;
  end;

  function FillTargetBuffer(aSourceCursor, aTargetCursor: TffSrCursor;
                            aSourceBuf, aTargetBuf: PffByteArray;
                            aFieldMap: TffSrFieldMapList;              {!!.03}
                            var BLOBBytesCopied : Integer): TffResult; {!!.03}
  { Copies the fields from the source buffer into the target buffer }
  var
    I: Integer;
    IsNull: Boolean;
    BLOBNr: TffInt64;
    SourceBLOBNr: TffInt64;                                            {!!.03}
  begin
    { The FieldMap has already been validated so we can assume all the
      fieldnames and datatype matches are legal. }
    { WARNING: the above assumption is no longer valid. Some matches are now
      checked for during restructure and an exception is raised below if invalid. }    {!!.10}
    Result := DBIERR_NONE;

    { Initialize the output buffer.  All fields will be null by default,
      therefore we do not have to explicitly handle any new fields added
      by the restructure. }
    aTargetCursor.Table.Dictionary.InitRecord(aTargetBuf);

    { Loop through all the fields to be converted }
    for I := 0 to aFieldMap.Count - 1 do
      with aFieldMap do begin
{Begin !!.03}
        if TargetField[I].FieldType in [fftBLOB..ffcLastBLOBType] then begin
          AllocateBLOBTransferBuffer;
          { Obtain the length of the source BLOB. }
          if (SourceField[I].FieldType in [fftBLOB..ffcLastBLOBType]) then begin {!!.13}
            aSourceCursor.Table.Dictionary.GetRecordField(SourceField[I].Number,
                                                          aSourceBuf,
                                                          IsNull, @SourceBLOBNr);
            if (not IsNull) then
              inc(BLOBBytesCopied,
                  aSourceCursor.BLOBGetLength(SourceBLOBNr, Result));
            if Result <> DBIERR_NONE then Break;
          end
          else                                                         {!!.13}
            inc(BLOBBytesCopied, aFieldMap.SourceField[I].FieldLength);{!!.13}
        end;
{End !!.03}

        Result := seConvertSingleField(aSourceBuf,
                                       aTargetBuf,
                                       aSourceCursor.CursorID,
                                       aTargetCursor.CursorID,
                                       SourceField[I].Number,
                                       TargetField[I].Number,
                                       BLOBBuffer,
                                       BLOBBufLen);
        if Result <> DBIERR_NONE then
        {Begin !!.10}
        if Result = DBIERR_INVALIDFLDXFORM then
          FFRaiseException(EffException, ffStrResServer, fferrBadFieldXform,
                          [SourceField[I].Name+' ('+
                           GetEnumName(TypeInfo(TffFieldtype), Integer(SourceField[I].FieldType))+')',
                           TargetField[I].Name+' ('+
                           GetEnumName(TypeInfo(TffFieldtype), Integer(TargetField[I].FieldType))+')'])

        else
        {End !!.10}
          Break;
      end;

    { Check for fields not converted that may be "required" (i.e., added
      fields that have the "required" flag set) }
    with aTargetCursor.Table.Dictionary do begin
      for I := 0 to FieldCount - 1 do begin
        GetRecordField(I, aTargetBuf, IsNull, nil);
        if IsNull and FieldRequired[I] then begin

          { Clear the null flag, the record buffer is already set to zero }
          FFClearBit(PffByteArray(@aTargetBuf^[LogicalRecordLength]), I);

          { For BLOBs we have to create an empty BLOB }
          if FieldType[I] in [fftBLOB..ffcLastBLOBType] then begin
            AllocateBLOBTransferBuffer;
            Result := aTargetCursor.BLOBAdd(BLOBNr);
            SetRecordField(I, aTargetBuf, @BLOBNr);
            if Result = DBIERR_NONE then
              Result := aTargetCursor.BLOBWrite(BLOBNr, 0, 0, BLOBBuffer);
          end;
        end;
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
  Result := DBIERR_NONE;
  AutoIncField := -1;
  AutoIncHighValue := 0;
  BLOBBytesCopied := 0;                                                {!!.03}
  RAMTrigger := seBufMgr.MaxRAM + (seBufMgr.MaxRAM div 10);            {!!.03}
  RecordsRead := 0;
  RecordsWritten := 0;
  FFSetRetry(0);
  with aRebuildParamsPtr^ do begin
    try
      try
        rpCursor.Timeout := 0;
        rpTargetCursor.Timeout := 0;
        rpDb.Timeout := 0;
        TargetRecordBuf := nil;
        TargetBufLength := 0;

        { Find first AutoInc field, if any }                  
        FindAutoIncField;                                     

        { Decide if we are performing a restructure as well }
        Restructuring := Assigned(rpFieldMap);
        if Restructuring then begin

          { Allocate an output record buffer }
          TargetBufLength := rpTargetCursor.Table.Dictionary.RecordLength;
          FFGetMem(TargetRecordBuf, TargetBufLength);         
        end;

        try
          try
            try
              try
                BLOBBuffer := nil;

                SourceName := rpCursor.Table.Basename;
                TargetName := rpTargetCursor.Table.Basename;


                { Allocate a record buffer }
                BufLength := rpCursor.Table.Dictionary.RecordLength;
                FFGetMem(RecordBuf, BufLength);

                { Figure out how many records are to be processed before
                  flushing. }
                aiFlush := (RecBuf div BufLength);

                try
                  try

                    { For packs, TargetRecordBuf points to the input buffer as well }
                    if not Assigned(TargetRecordBuf) then
                      TargetRecordBuf := RecordBuf;

                    Result := seTransactionStart(rpDB, False, False, TransID);
                    if Result <> DBIERR_NONE then Abort;
                    try
                      NextFlushPoint := aiFlush;
                      NextSnapshotPoint := aiSnapshot;
                      {$IFNDEF ThreadedRebuilds}
                      {NextYieldPoint := aiYield;}                    {!!.03}
                      {$ENDIF}

                      RefNr.iLow  := 0;
                      RefNr.iHigh := 0;

                      { Loop through all the nondeleted records... }
                      rpCursor.Table.GetNextRecordSeq(rpDB.TransactionInfo,
                                                      RefNr, RecordBuf);
                      while not ((RefNr.iLow = 0) and (RefNr.iHigh = 0)) do begin
                        Inc(RecordsRead);

                        { Copy record to the new output file, restructuring
                          along the way if needed. }
                        if Restructuring then begin
                          FillTargetBuffer(rpCursor,
                                           rpTargetCursor,
                                           RecordBuf,
                                           TargetRecordBuf,
                                           rpFieldMap,                  {!!.03}
                                           BLOBBytesCopied);            {!!.03}
                        end
                        else begin

                          { Copy BLOBs, if any, to the new table }
                          Result := CopyBLOBs(BLOBBytesCopied);        {!!.03}
                          if Result <> DBIERR_NONE then Abort;
                        end;

                        { Add the record into the new table }
                        Result := rpTargetCursor.InsertRecord(TargetRecordBuf,
                                                              ffsltExclusive);
                        if Result <> DBIERR_NONE then
                          Abort;

                        { For fftAutoInc targets, keep track of the largest value observed }
                        if AutoIncField <> -1 then begin
                          rpTargetCursor.Table.Dictionary.GetRecordField(AutoIncField,
                                                                         TargetRecordBuf,
                                                                         IsNull,
                                                                         @ThisAutoIncValue);
                          if not IsNull and (ThisAutoIncValue > AutoIncHighValue) then
                            AutoIncHighValue := ThisAutoIncValue;
                        end;

                        Inc(RecordsWritten);

                        { See if it's time to flush our work so far }
                        if (RecordsRead >= NextFlushPoint) or           {!!.03}
                           (BLOBBytesCopied >= MaxBLOBBytes) or         {!!.03}
                           (seBufMgr.RAMUsed >= RAMTrigger ) then begin {!!.03}

                          if (RecordsRead >= NextFlushPoint) then       {!!.13}
                            Inc(NextFlushPoint, aiFlush);
                          BLOBBytesCopied := 0;                         {!!.03}
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

{Deleted !!.01}
                        {$IFNDEF ThreadedRebuilds}
                          { See if it's time to yield for other messages }
{                          if RecordsRead >= NextYieldPoint then begin
                            Inc(NextYieldPoint, aiYield);
                            Application.ProcessMessages;
                          end;}
                        {$ENDIF}

                        rpCursor.Table.GetNextRecordSeq(rpDB.TransactionInfo,
                                                        RefNr, RecordBuf);
                      end;

                      { Post the autoinc value if needed }
                      if AutoIncField <> -1 then
                        FFTblSetAutoIncValue(rpTargetCursor.Table.Files[0],
                                             rpDB.TransactionInfo,
                                             AutoIncHighValue);
//                    finally                                          {Deleted !!.01}
                      { Save all data changes }
                      if seTransactionCommit(rpDB) <> DBIERR_NONE then
                        FFRaiseExceptionNoData(EffServerException,
                                               ffStrResServer,
                                               fferrTransactionFailed);
{Begin !!.01}
                    except
                      seTransactionRollback(rpDB);
                      raise;
{End !!.01}
                    end;
                  finally
                    ReleaseBLOBTransferBuffer;
                  end;
                finally
                  { Deallocate the record buffer }
                  FFFreeMem(RecordBuf, BufLength);
                end;
              finally
                { Close the target cursor }
                rpTargetCursor.State := ffosInactive;
                CursorClose(rpTargetCursor.CursorID);
              end;
            finally
              { Close the source cursor }
              rpCursor.State := ffosInactive;
              CursorClose(rpCursor.CursorID);
            end;
          except
            { An error occurred somewhere in the process; clean up the files}
            TableDelete(rpDB.DatabaseID, TargetName);
            raise;
          end;

          { Replace the original file with the working file.
            First step: Rename the old table.  We will restore it if an
              error occurs. }
          SaveBaseName := SetTargetBaseName(rpDB.Folder.Path, ffcSaveBaseName);
          Result := TableRename(rpDB.DatabaseID, SourceName, SaveBaseName);
          if Result = DBIERR_NONE then begin
            try
              { Rename the new table to the old table. }
              Result := TableRename(rpDB.DatabaseID, TargetName, SourceName);
            except
              { If an exception occurs then put the original table back in its
                place. }
              TableRename(rpDB.DatabaseID, SaveBaseName, SourceName);
              raise;
            end;
            { Everything worked so far.  Delete the original table. }
            Result := TableDelete(rpDB.DatabaseID, SaveBaseName);
          end;
        finally
          rpDB.State := ffosInactive;
          rpDB.Free;
          { Release the output record buffer, if allocated separately from
            the input record buffer }
          if Restructuring then
            FFFreeMem(TargetRecordBuf, TargetBufLength);
        end;
      except
        on E : Exception do begin
          if Result = DBIERR_NONE then
            Result := ConvertServerExceptionEx(E, FEventLog,           {!!.01}
                                               bseGetReadOnly);        {!!.01}
        end;
      end;
    finally
      { Shut down the rebuild status indicator }
      rpRebuildStatus.MakeSnapshot(RecordsRead,
                                   RecordsWritten,
                                   Result);
      RebuildDeregister(rpRebuildStatus.RebuildID);
    end;
  end;
end;

