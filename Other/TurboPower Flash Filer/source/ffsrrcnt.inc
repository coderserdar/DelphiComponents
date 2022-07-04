{*********************************************************}
{* FlashFiler: async get record count include file       *}
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
 * Thorsten Engler
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2002
 * the Initial Developer. All Rights Reserved.
 * Used with permission.
 *
 * Modified from the original to fit the width of your screen
 * & to be compatible with FlashFiler 2.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

function TffServerEngine.TableGetRecCountAsync(aCursorID : TffCursorID;
                                           var aTaskID : Longint) : TffResult;   
var
  Cursor : TffSrBaseCursor;
  RebuildParamsPtr: PffSrRebuildParams;
  RecordInfo: TffRecordInfo;
begin
  Result := DBIERR_NONE;
  aTaskID := -1;

  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    try
      if Result <> DBIERR_NONE then
        Exit;
      FFGetMem(RebuildParamsPtr, SizeOf(RebuildParamsPtr^));           {!!.13}
      try
        FillChar(RebuildParamsPtr^, SizeOf(RebuildParamsPtr^), 0);
        with RebuildParamsPtr^ do begin
          rpDB := nil;
          rpTableName := '';
          rpIndexName := '';
          rpIndexID := 0;

          try
            rpCursor := TffSrCursor(Cursor.CloneCursor(omReadOnly));
            try
              rpCursor.State := ffosActive;
              { Get the total nondeleted records in the table }
              FFTblGetRecordInfo(rpCursor.Table.Files[0],
                                 rpCursor.Database.TransactionInfo,
                                 RecordInfo);
              rpRebuildStatus := RebuildRegister
                                   (TffSrClient(rpCursor.Database.Client).ClientID,
                                    RecordInfo.riRecCount);
              aTaskID := rpRebuildStatus.RebuildID;

              { Create a separate thread for the pack operation }
              TffSrGetRecordCountThread.Create(Self, RebuildParamsPtr);

              { The thread constructor (32-bit) or message handler (16-bit)
                are responsible for deallocating this memory block }
              RebuildParamsPtr := nil;
            except
              rpCursor.State := ffosInactive;
              CursorClose(rpCursor.CursorID);
              raise;
            end;
          except
            RebuildDeregister(aTaskID);
            raise;
          end;
        end;
      except
        if Assigned(RebuildParamsPtr) then                             {!!.13}
          FFFreeMem(RebuildParamsPtr, SizeOf(RebuildParamsPtr^));      {!!.13}
        raise;
      end;
    finally
      Cursor.Deactivate;
    end;
  except
    on E : Exception do begin
      if Result = DBIERR_NONE then
        Result := ConvertServerException(E, FEventLog);
    end;
  end;
end;

type
  TffSrCursorHacker=class(TffSrCursor);

function TffServerEngine.seTableGetRecordCountPrim(aRebuildParamsPtr: PffSrRebuildParams): TffResult;
const
  { Action intervals }
  aiSnapshot  = 128;   { every x records, update the status snapshot }
var
  Action           : TffSearchKeyAction;
  KeyCompareResult : integer;
  Info             : TffRecordInfo;
  RecordsRead      : Longint;
  RecordsMatched   : Longint;
  NextSnapshotPoint: Longint;
begin
  Result := DBIERR_NONE;
  aRebuildParamsPtr^.rpCursor.AcqContentLock(ffclmRead);
  try
    with aRebuildParamsPtr^, TffSrCursorHacker(rpCursor) do begin
      rpCursor.Timeout := 0;
      NextSnapshotPoint := aiSnapshot;
      RecordsRead := 0;
      RecordsMatched := 0;
      try
        if bcHasRange or Assigned(bcFilter) then begin

          SetToBegin;
          if bcHasRange and bcRng1Valid then begin
            {position at start of range}
            if bcRng1Incl then
              Action := skaGreaterEqual
            else
              Action := skaGreater;
            {note: the following FindKey call will always return true in
             this case}
            Move(bcRng1Key^, bcCurKey^, scKeyLen);
            with bcCompareData do begin
              cdFldCnt := bcRng1FldCnt;
              cdPartLen := bcRng1PtlLen;
            end;
            Table.FindKey(bcKID, bcInfo.RefNr, Database.TransactionInfo,
                          bcCurKey, bcInfo.KeyPath, Action);
            {check whether the keypath was positioned at EOF, if so the
             start of the range is at EOF, so it's not likely we'll find a
             'next' key or any keys at all <g>}
            if (bcInfo.KeyPath.kpPos = kppEOF) then begin
              {note the reset of the cursor position still occurs}
              Exit;
            end;
            {make sure that the keypath is on the crack before the key so that
             the next key call in a minute returns the right record}
            if (bcInfo.KeyPath.kpPos = kppOnKey) then
              bcInfo.KeyPath.kpPos := kppOnCrackBefore;
          end;
          {while not EOF or other error do}
          while (Result = DBIERR_NONE) do begin
            {readnext key}
            Result := Table.GetNextKey(bcKID, bcInfo.RefNr,
                                       Database.TransactionInfo,
                                       bcCurKey, bcInfo.KeyPath);
            if (Result = DBIERR_NONE) then begin
              {check that we're in range if required}
              if bcHasRange and bcRng2Valid then begin
                {check whether beyond end of range}
                with bcCompareData do begin
                  cdFldCnt := bcRng2FldCnt;
                  cdPartLen := bcRng2PtlLen;
                end;
                KeyCompareResult :=
                   Table.CompareKeysForCursor(bcKID, bcCurKey, bcRng2Key);
                if (KeyCompareResult > 0) or
                  ((KeyCompareResult = 0) and (not bcRng2Incl)) then begin
                  Result := DBIERR_EOF;
                end
                else {key is in range} begin
                  if Assigned(bcFilter) then begin
                    Table.GetRecordNoLock(Database.TransactionInfo,
                                          bcInfo.RefNr,
                                          bcRecordData);
                    if bcFilter.MatchesRecord(bcRecordData) then
                      inc(RecordsMatched);
                  end else
                  inc(RecordsMatched);
                end;
              end
              else {end of range = end of index path} begin
                if Assigned(bcFilter) then begin
                  Table.GetRecordNoLock(Database.TransactionInfo,
                                        bcInfo.RefNr, bcRecordData);
                  if bcFilter.MatchesRecord(bcRecordData) then
                    inc(RecordsMatched);
                end else
                Inc(RecordsMatched);
              end;
              inc(RecordsRead);

              { See if it's time to update the status packet }
              if RecordsRead >= NextSnapshotPoint then begin
                Inc(NextSnapshotPoint, aiSnapshot);
                rpRebuildStatus.MakeSnapshot(RecordsRead,
                                             RecordsMatched,
                                             DBIERR_NONE);
              end;
            end;
          end;
        end
        else begin
          FFTblGetRecordInfo(Table.Files[0], Database.TransactionInfo, Info);
          RecordsMatched := Info.riRecCount;
          RecordsRead    := Info.riRecCount;
        end;
        Result:= DBIERR_NONE;
      finally
        { Shut down the rebuild status indicator }
        rpRebuildStatus.MakeSnapshot(RecordsRead,
                                     RecordsMatched,
                                     Result);
        RebuildDeregister(rpRebuildStatus.RebuildID);
      end;
    end;
  finally
    aRebuildParamsPtr^.rpCursor.RelContentLock(ffclmRead);
    aRebuildParamsPtr^.rpCursor.Deactivate;
    CursorClose(aRebuildParamsPtr^.rpCursor.CursorID);                 {!!.10}
  end;
end;
