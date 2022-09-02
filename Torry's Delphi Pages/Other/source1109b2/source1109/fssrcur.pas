{*********************************************************}
{* FlashFiler: Server cursor classes                     *}
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

Unit fssrcur;

Interface

Uses
  fsllbase,
  fslldict,
  fslltemp,
  fssrbase,
  fssrbde,
  fsserverclass,
  fssrfltr,
  fssrfold,
  fssrlock,
  fsdictserveraccess,
  fsindexaccess;

Type

  { This class manages a set of data stored in a logical table. It is very
    similar to TfsSrcTable. However, the data is not indexed.
    To create a new simple table, you must do the following:

    1. Call TfsSrcSimpleTable.Create.
    2. Have the table build its files by calling its BuildFiles method.
    3. Have the table open its fils by calling its OpenFiles method. }
  TfsSrcSimpleTable = Class(TfsSrcBaseTable)
  Public
    Procedure AddIndex(Const aIndexDesc: TffIndexDescriptor;
      aTI: PffTransInfo); Override;
    Procedure BuildFiles(aTI: PffTransInfo;
      aForServer: boolean;
      aDictionary: TFSInfoDict;
      aAttribs: TffFileAttributes;
      aStore: TfsBaseTempStorage); Override;
    Function BuildKeyForRecord(aIndexID: Integer;
      aData: PffByteArray;
      aKey: PffByteArray;
      aFieldCount: Integer;
      aPartialLen: Integer): TffResult; Override;
    Function CompareKeysForCursor(Var aKID: TffKeyIndexData;
      aKey1: PffByteArray;
      aKey2: PffByteArray): Integer; Override;
    Function DeleteRecord(aTI: PffTransInfo;
      Const aCursorID: TffCursorID;
      Const aRefNr: TffInt64;
      Const aLockObtained: Boolean;
      Var aBTreeChanged: Boolean) {!!.05}
    : TffResult; Override;
    Procedure DropIndex(aTI: PffTransInfo; aIndexID: Longint); Override;
    Function FindKey(Var aKID: TffKeyIndexData;
      Var aRefNr: TffInt64;
      aTI: PffTransInfo;
      aKey: PffByteArray;
      Var aKeyPath: TffKeyPath;
      aAction: TffSearchKeyAction): boolean; Override;
    Function GetNextKey(Var aKID: TffKeyIndexData;
      Var aRefNr: TffInt64;
      aTI: PffTransInfo;
      aKey: PffByteArray;
      Var aKeyPath: TffKeyPath): TffResult; Override;
    Function GetNextRecord(aTI: PffTransInfo;
      Const aDatabaseID: TffDatabaseID; {!!.10}
      Const aCursorID: TffCursorID; {!!.10}
      Var aKID: TffKeyIndexData;
      Var aRefNr: TffInt64;
      aKey: PffByteArray;
      Var aKeyPath: TffKeyPath;
      aData: PffByteArray;
      Const aLockType: TfsSrcLockType;
      Var aFlag: Byte): TffResult; Override; {!!.10}
    Function GetPriorRecord(aTI: PffTransInfo;
      Const aDatabaseID: TffDatabaseID; {!!.10}
      Const aCursorID: TffCursorID; {!!.10}
      Var aKID: TffKeyIndexData;
      Var aRefNr: TffInt64;
      aKey: PffByteArray;
      Var aKeyPath: TffKeyPath;
      aData: PffByteArray;
      Const aLockType: TfsSrcLockType;
      Var aFlag: Byte): TffResult; Override; {!!.10}
    Function InsertRecord(aTI: PffTransInfo;
      aCursorID: TffCursorID;
      aData: PffByteArray;
      aLockType: TfsSrcLockType;
      Var aNewRefNr: TffInt64; aFlag: Byte): TffResult; Override;
    Function InsertRecordNoDefault(aTI: PffTransInfo; {!!.10}
      aCursorID: TffCursorID;
      aData: PffByteArray;
      aLockType: TfsSrcLockType;
      Var aNewRefNr: TffInt64; aFlag: Byte): TffResult; Override;
    Procedure MakeKIDForCursor(aIndexID: Integer; Var aKID: TffKeyIndexData); Override;
    Procedure OpenFiles(aTI: PffTransInfo; aForServer: boolean;
      aAttribs: TffFileAttributes); Override;
    Function PutRecord(aTI: PffTransInfo;
      aCursorID: TffCursorID;
      aRefNr: TffInt64;
      aData: PffByteArray;
      aRelLock: boolean; {!!.05}
      aUserLockType: TfsUserRecLocking;
      Var aKeyChanged: Boolean): TffResult; Override; {!!.05}

  End;

  { Use this cursor class to manage sets of data that do not require indices. }
  TfsSrcSimpleCursor = Class(TfsSrBaseCursor)
  Protected
    Procedure bcTableOpenPreconditions(aTable: TfsSrcBaseTable;
      Const aIndexName: String;
      Var aIndexID: Longint;
      Const aOpenMode: TffOpenMode); Override;
    { Used by Create method to verify a thread may open a table. }

  Public
    Constructor Create(anEngine: TFSServer;
      aDatabase: TfsSrcDatabase;
      Const aTimeout: Longint); Override;

    Destructor Destroy; Override;
    Function AddIndexToTable(Const aIndexDesc: TffIndexDescriptor): TffResult; Override;
    Function CheckBookmark(aBookmark: PffByteArray): TffResult; Override;
    Procedure ClearIndex; Override;
    Function CloneCursor(aOpenMode: TffOpenMode): TfsSrBaseCursor; Override;
    Function CompareBookmarks(aBookmark1, aBookmark2: PffByteArray;
      Var CmpResult: Longint): TffResult; Override;
    Function DropIndexFromTable(Const aIndexName: TffDictItemName;
      aIndexID: Longint): TffResult; Override;
    Function ExtractKey(aData: PffByteArray; aKey: PffByteArray): TffResult; Override;
    Function GetBookmark(aBookmark: PffByteArray): TffResult; Override;
    Function GetBookmarkSize: Integer; Override;
    Function GetNextRecord(aData: PffByteArray; aLockType: TfsSrcLockType; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult; Override;
    Function GetPriorRecord(aData: PffByteArray; aLockType: TfsSrcLockType; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult; Override;
    Function GetRecordCount(Var aRecCount: Longword): TffResult; Override;
    Function GetRecordForKey(aDirectKey: boolean;
      aFieldCount: Integer;
      aPartialLen: Integer;
      aKeyData: PffByteArray;
      aData: PffByteArray;
      aFirstCall: Boolean): TffResult; Override;
    Function InsertRecord(aData: PffByteArray; aLockType: TfsSrcLockType; aFlag: Byte; Var aRefNr: TffInt64): TffResult; Override;
    Function InsertRecordNoDefault(aData: PffByteArray; aLockType: TfsSrcLockType; aFlag: Byte; Var aRefNr: TffInt64): TffResult; Override; {!!.10}
    Function IsInRange(aKey: PffByteArray): Integer; Override;
    Function ModifyRecord(aData: PffByteArray; aRelLock: boolean; aUserLockType: TfsUserRecLocking; aFlag: Byte; aSet, Use: Boolean): TffResult;
      Override;
    Procedure Open(Const aTableName: TfsTableName;
      Const aIndexName: TffName;
      Const aIndexID: Longint;
      Const aOpenMode: TffOpenMode;
      aShareMode: TffShareMode;
      aForServer: boolean;
      Const aExclContLock: Boolean; {!!.10}
      aAttribs: TffFileAttributes; SysOpen: Boolean); Override;
    Procedure ResetRange; Override;
    Function SetRange(aDirectKey: boolean;
      aFieldCount1: Integer;
      aPartialLen1: Integer;
      aKeyData1: PffByteArray;
      aKeyIncl1: boolean;
      aFieldCount2: Integer;
      aPartialLen2: Integer;
      aKeyData2: PffByteArray;
      aKeyIncl2: boolean): TffResult; Override;
    Procedure SetToBegin; Override;
    Function SetToBookmark(aBookmark: PffByteArray): TffResult; Override;
    Function SetToCursor(aCursor: TfsSrBaseCursor): TffResult; Override;
    Procedure SetToEnd; Override;
    Function SetToKey(aSearchAction: TffSearchKeyAction;
      aDirectKey: boolean;
      aFieldCount: Integer;
      aPartialLen: Integer;
      aKeyData: PffByteArray): TffResult; Override;
    Function SwitchToIndex(aIndexID: Integer;
      aPosnOnRec: boolean): TffResult; Override;
  End;

  { Use this class to create a cursor representing a query's result set.
    Do not directly create TfsSrcSimpleCursor. }

  TfsSrcSQLSimpleTable = Class(TfsSrcBaseTable)
  Public
    Procedure AddIndex(Const aIndexDesc: TffIndexDescriptor;
      aTI: PffTransInfo); Override;
    Procedure BuildFiles(aTI: PffTransInfo;
      aForServer: boolean;
      aDictionary: TFSInfoDict;
      aAttribs: TffFileAttributes;
      aStore: TfsBaseTempStorage); Override;
    Function BuildKeyForRecord(aIndexID: Integer;
      aData: PffByteArray;
      aKey: PffByteArray;
      aFieldCount: Integer;
      aPartialLen: Integer): TffResult; Override;
    Function CompareKeysForCursor(Var aKID: TffKeyIndexData;
      aKey1: PffByteArray;
      aKey2: PffByteArray): Integer; Override;
    Function DeleteRecord(aTI: PffTransInfo;
      Const aCursorID: TffCursorID;
      Const aRefNr: TffInt64;
      Const aLockObtained: Boolean;
      Var aBTreeChanged: Boolean) {!!.05}
    : TffResult; Override;
    Procedure DropIndex(aTI: PffTransInfo; aIndexID: Longint); Override;
    Function FindKey(Var aKID: TffKeyIndexData;
      Var aRefNr: TffInt64;
      aTI: PffTransInfo;
      aKey: PffByteArray;
      Var aKeyPath: TffKeyPath;
      aAction: TffSearchKeyAction): boolean; Override;
    Function GetNextKey(Var aKID: TffKeyIndexData;
      Var aRefNr: TffInt64;
      aTI: PffTransInfo;
      aKey: PffByteArray;
      Var aKeyPath: TffKeyPath): TffResult; Override;
    Function GetNextRecord(aTI: PffTransInfo;
      Const aDatabaseID: TffDatabaseID; {!!.10}
      Const aCursorID: TffCursorID; {!!.10}
      Var aKID: TffKeyIndexData;
      Var aRefNr: TffInt64;
      aKey: PffByteArray;
      Var aKeyPath: TffKeyPath;
      aData: PffByteArray;
      Const aLockType: TfsSrcLockType;
      Var aFlag: Byte): TffResult; Override; {!!.10}
    Function GetPriorRecord(aTI: PffTransInfo;
      Const aDatabaseID: TffDatabaseID; {!!.10}
      Const aCursorID: TffCursorID; {!!.10}
      Var aKID: TffKeyIndexData;
      Var aRefNr: TffInt64;
      aKey: PffByteArray;
      Var aKeyPath: TffKeyPath;
      aData: PffByteArray;
      Const aLockType: TfsSrcLockType;
      Var aFlag: Byte): TffResult; Override; {!!.10}
    Function InsertRecord(aTI: PffTransInfo;
      aCursorID: TffCursorID;
      aData: PffByteArray;
      aLockType: TfsSrcLockType;
      Var aNewRefNr: TffInt64; aFlag: Byte): TffResult; Override;
    Function InsertRecordNoDefault(aTI: PffTransInfo; {!!.10}
      aCursorID: TffCursorID;
      aData: PffByteArray;
      aLockType: TfsSrcLockType;
      Var aNewRefNr: TffInt64; aFlag: Byte): TffResult; Override;
    Procedure MakeKIDForCursor(aIndexID: Integer; Var aKID: TffKeyIndexData); Override;
    Procedure OpenFiles(aTI: PffTransInfo; aForServer: boolean;
      aAttribs: TffFileAttributes); Override;
    Function PutRecord(aTI: PffTransInfo;
      aCursorID: TffCursorID;
      aRefNr: TffInt64;
      aData: PffByteArray;
      aRelLock: boolean; {!!.05}
      aUserLockType: TfsUserRecLocking;
      Var aKeyChanged: Boolean): TffResult; Override; {!!.05}

  End;

  { Use this cursor class to manage sets of data that do not require indices. }
  TfsSrcSQLSimpleCursor = Class(TfsSrBaseCursor)
  Protected
    Procedure bcTableOpenPreconditions(aTable: TfsSrcBaseTable;
      Const aIndexName: String;
      Var aIndexID: Longint;
      Const aOpenMode: TffOpenMode); Override;
    { Used by Create method to verify a thread may open a table. }

  Public
    Constructor Create(anEngine: TFSServer;
      aDatabase: TfsSrcDatabase;
      Const aTimeout: Longint); Override;

    Destructor Destroy; Override;
    Function AddIndexToTable(Const aIndexDesc: TffIndexDescriptor): TffResult; Override;
    Function CheckBookmark(aBookmark: PffByteArray): TffResult; Override;
    Procedure ClearIndex; Override;
    Function CloneCursor(aOpenMode: TffOpenMode): TfsSrBaseCursor; Override;
    Function CompareBookmarks(aBookmark1, aBookmark2: PffByteArray;
      Var CmpResult: Longint): TffResult; Override;
    Function DropIndexFromTable(Const aIndexName: TffDictItemName;
      aIndexID: Longint): TffResult; Override;
    Function ExtractKey(aData: PffByteArray; aKey: PffByteArray): TffResult; Override;
    Function GetBookmark(aBookmark: PffByteArray): TffResult; Override;
    Function GetBookmarkSize: Integer; Override;
    Function GetNextRecord(aData: PffByteArray; aLockType: TfsSrcLockType; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult; Override;
    Function GetPriorRecord(aData: PffByteArray; aLockType: TfsSrcLockType; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult; Override;
    Function GetRecordCount(Var aRecCount: Longword): TffResult; Override;
    Function GetSetPosition(aValue: Longint; aData: PffByteArray; aLockType: TfsSrcLockType;
      Var aFlag: Byte; Var aRecNo: Longword;
      Var aRefNr: TffInt64;
      aInfoGetSetPosition: TInfoGetSetPosition;
      aSet: Boolean): TffResult; Override;
    Function GetRecordForKey(aDirectKey: boolean;
      aFieldCount: Integer;
      aPartialLen: Integer;
      aKeyData: PffByteArray;
      aData: PffByteArray;
      aFirstCall: Boolean): TffResult; Override;
    Function InsertRecord(aData: PffByteArray; aLockType: TfsSrcLockType; aFlag: Byte; Var aRefNr: TffInt64): TffResult; Override;
    Function InsertRecordNoDefault(aData: PffByteArray; aLockType: TfsSrcLockType; aFlag: Byte; Var aRefNr: TffInt64): TffResult; Override; {!!.10}
    Function IsInRange(aKey: PffByteArray): Integer; Override;
    Function ModifyRecord(aData: PffByteArray; aRelLock: boolean; aUserLockType: TfsUserRecLocking; aFlag: Byte; aSet, Use: Boolean): TffResult;
      Override;
    Procedure Open(Const aTableName: TfsTableName;
      Const aIndexName: TffName;
      Const aIndexID: Longint;
      Const aOpenMode: TffOpenMode;
      aShareMode: TffShareMode;
      aForServer: boolean;
      Const aExclContLock: Boolean; {!!.10}
      aAttribs: TffFileAttributes; SysOpen: Boolean); Override;
    Procedure ResetRange; Override;
    Function SetRange(aDirectKey: boolean;
      aFieldCount1: Integer;
      aPartialLen1: Integer;
      aKeyData1: PffByteArray;
      aKeyIncl1: boolean;
      aFieldCount2: Integer;
      aPartialLen2: Integer;
      aKeyData2: PffByteArray;
      aKeyIncl2: boolean): TffResult; Override;
    Procedure SetToBegin; Override;
    Function SetToBookmark(aBookmark: PffByteArray): TffResult; Override;
    Function SetToCursor(aCursor: TfsSrBaseCursor): TffResult; Override;
    Procedure SetToEnd; Override;
    Function SetToKey(aSearchAction: TffSearchKeyAction;
      aDirectKey: boolean;
      aFieldCount: Integer;
      aPartialLen: Integer;
      aKeyData: PffByteArray): TffResult; Override;
    Function SwitchToIndex(aIndexID: Integer;
      aPosnOnRec: boolean): TffResult; Override;
  End;

  TfsSimpleSQLResultSet = Class(TfsSrcSQLSimpleCursor);
  TfsSharedSQLResultSet = Class(TfsSrcSimpleCursor);

Implementation

Uses
  SysUtils,
  fsconst,
  fshash,
  fslleng,
  fsllexcp,
  fstablehelper,
  fsblobaccess,
  fsutil,
  fsfuninterp,
  fsrecordaccess;

{===TfsSrcSQLTable====================================================}

Procedure TfsSrcSimpleTable.AddIndex(Const aIndexDesc: TffIndexDescriptor;
  aTI: PffTransInfo);
Begin
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [btBaseName^, Self.ClassName]);
End;
{--------}

Procedure TfsSrcSimpleTable.BuildFiles(aTI: PffTransInfo;
  aForServer: boolean;
  aDictionary: TFSInfoDict;
  aAttribs: TffFileAttributes;
  aStore: TfsBaseTempStorage);
Var
  FileInx: Integer;
  DataFile: PffFileInfo;
  FileCnt: Integer; {dup for speed}
Begin
  { Create the data file. }
  btFiles.Count := 1;
  btFiles[0] := FFAllocFileInfo(FFMakeFullFileName(Folder.Path, BaseName),
    fsc_ExtForData, btBufMgr);

  { Validate the dictionary. }
  aDictionary.CheckValid;

  { Assimilate the dictionary. }
  btDictionary.ForceOffReadOnly;
  btDictionary.Assign(aDictionary);
  btDictionary.BindIndexHelpers;

  With PffFileInfo(btFiles[0])^ Do
    Begin
      fiAttributes := aAttribs;
      fiForServer := aForServer;
      If aForServer Then
        fiEncrypted := btEngine.Configuration.GeneralInfo^.giEncrypt
      Else
        fiEncrypted := aDictionary.IsEncrypted;
      If aDictionary.EngineDeleteType In [edtUndeleteIfPossibleNotBlob, edtUndeleteIfPossibleAndBlob] Then
        fiRecLenPlusTrailer := btDictionary.RecordLength + SizeOf(Byte) + sizeof(TffInt64) // extra info for delete
      Else If aDictionary.EngineDeleteType In [edtUndeleteFull] Then
        fiRecLenPlusTrailer := btDictionary.RecordLength + SizeOf(Byte) + sizeof(TffInt64) + sizeof(TffInt64) // extra info for delete
      Else
        fiRecLenPlusTrailer := btDictionary.RecordLength + SizeOf(Byte);
      If fiVersionRecord = trUseVersion Then
        fiRecLenPlusTrailer := fiRecLenPlusTrailer + Sizeof(Int64); // for recversion
      fiRecordLength := btDictionary.RecordLength;
      fiTempStore := aStore;
    End;

  { Get the file count for this table (for speed reasons, etc). }
  FileCnt := Dictionary.FileCount;
  FileCount := FileCnt;

  { Get the data file for speed reasons. }
  DataFile := Files[0];

  { Scan through the secondary files. This table supports separate BLOB
    files but not separate index files. }
  For FileInx := 0 To pred(FileCnt) Do
    Begin
      If Dictionary.FileType[FileInx] = ftIndexFile Then
        FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
          [btBaseName^, Self.ClassName]);
      btCreateFile(FileInx, aTI, btDictionary.FileExt[FileInx], aForServer,
        aAttribs, aStore);
    End;

  { Write the dictionary to the data file. }
  Dictionary.WriteToFile(DataFile, aTI);
  {Begin !!.11}
  Files[Dictionary.BLOBFileNumber].fiBLOBrscMgr :=
    TffBaseBLOBResourceMgr.GetMgr(Files[Dictionary.BLOBFileNumber]);
  btBLOBEngine := TffBaseBLOBEngine.GetEngine(Files[Dictionary.BLOBFileNumber]);
  {End !!.11}
  Files[Dictionary.BLOBFileNumber].fiMaxSegSize :=
    FFCalcMaxBLOBSegSize(Files[Dictionary.BLOBFileNumber]);

End;
{--------}

Function TfsSrcSimpleTable.BuildKeyForRecord(aIndexID: Integer;
  aData: PffByteArray;
  aKey: PffByteArray;
  aFieldCount: Integer;
  aPartialLen: Integer): TffResult;
Begin
  Result := DBIERR_INVALIDINDEXCREATE;
End;
{--------}

Function TfsSrcSimpleTable.CompareKeysForCursor(Var aKID: TffKeyIndexData;
  aKey1: PffByteArray;
  aKey2: PffByteArray): Integer;
Begin
  Result := DBIERR_INVALIDINDEXCREATE;
End;
{--------}

Function TfsSrcSimpleTable.DeleteRecord(aTI: PffTransInfo;
  Const aCursorID: TffCursorID;
  Const aRefNr: TffInt64;
  Const aLockObtained: Boolean;
  Var aBTreeChanged: Boolean) {!!.05}
: TffResult;
Var
  OldData: PffByteArray;
  RecLen: Integer;
  Rflags: Word;
  af: Byte;
Begin
  Rflags := FFTblReadTableFlags(Files[0], aTI);
  If ((Rflags And fsTableDontDeleteRecord) <> 0) Then
    Begin
      Result := DBIERR_NOTSUFFFIELDRIGHTS;
      Exit;
    End;

  RecLen := btDictionary.RecordLength;
  FFGetMem(OldData, RecLen);
  Result := DBIERR_NONE;

  { If we have yet to lock the record then do so. }
  If (Not aLockObtained) Then
    FSAcquireRecordLock(Files[0], aTI, aRefNr, ffsltExclusive, {!!.10}
      aTI^.tirTrans.DatabaseID, {!!.10}
      aCursorID, False, False); {!!.02} {!!.10}
  { Note: We leave all such locks active until the transaction is committed. }

  Try
    FFTblReadRecord(Files[0], aTI, aRefNr, OldData, Self.Files[0].fiFSVersion, af);
    If GetFlags(af, 16) Then
      Result := DBIERR_NOTSUFFFIELDRIGHTS
    Else
      FFTblDeleteRecord(Files[0], aTI, aRefNr, Dictionary, Files[Dictionary.BLOBFileNumber], OldData, True);
  Finally
    btInformCursors(aCursorID, roDelete, aRefNr, 0);
    FFFreeMem(OldData, RecLen);
  End; {try..finally}
End;
{--------}

Procedure TfsSrcSimpleTable.DropIndex(aTI: PffTransInfo;
  aIndexID: Longint);
Begin
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [btBaseName^, Self.ClassName]);
End;
{--------}

Function TfsSrcSimpleTable.FindKey(Var aKID: TffKeyIndexData;
  Var aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath;
  aAction: TffSearchKeyAction): boolean;
Begin
  ffInitI64(aRefNr);
  Result := False;
End;
{--------}

Function TfsSrcSimpleTable.GetNextKey(Var aKID: TffKeyIndexData;
  Var aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath): TffResult;
Begin
  Result := DBIERR_INVALIDINDEXCREATE;
End;
{--------}

Function TfsSrcSimpleTable.GetNextRecord(aTI: PffTransInfo;
  Const aDatabaseID: TffDatabaseID; {!!.10}
  Const aCursorID: TffCursorID; {!!.10}
  Var aKID: TffKeyIndexData;
  Var aRefNr: TffInt64;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath;
  aData: PffByteArray;
  Const aLockType: TfsSrcLockType;
  Var aFlag: Byte): TffResult;
Begin
  Result := DBIERR_NONE;
  GetNextRecordSeq(aTI, aRefNr, aData, False, False, aFlag);
End;
{--------}

Function TfsSrcSimpleTable.GetPriorRecord(aTI: PffTransInfo;
  Const aDatabaseID: TffDatabaseID; {!!.10}
  Const aCursorID: TffCursorID; {!!.10}
  Var aKID: TffKeyIndexData;
  Var aRefNr: TffInt64;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath;
  aData: PffByteArray;
  Const aLockType: TfsSrcLockType;
  Var aFlag: Byte): TffResult; {!!.10}
Begin
  Result := DBIERR_NONE;
  GetPrevRecordSeq(aTI, aRefNr, aData, aFlag);
End;
{--------}

Function TfsSrcSimpleTable.InsertRecord(aTI: PffTransInfo;
  aCursorID: TffCursorID;
  aData: PffByteArray;
  aLockType: TfsSrcLockType;
  Var aNewRefNr: TffInt64;
  aFlag: Byte): TffResult;
Var
  RefNr: TffInt64;
  Rflags: Word;
  RMax, RCount: Longword;
Begin
  RefNr.iLow := 0;
  RefNr.iHigh := 0;
  Result := DBIERR_NONE;

  Rflags := FFTblReadTableFlags(Files[0], aTI);
  If ((Rflags And fsTableDontInsertRecord) <> 0) Then
    Begin
      Result := DBIERR_NOTSUFFFIELDRIGHTS;
      Exit;
    End;

  RMax := FFTblReadMaxRecords(Files[0], aTI);
  If RMax > 0 Then
    Begin
      RCount := FFTblReadRecordCount(Files[0], aTI);
      If RCount + 1 > RMax Then
        Begin
          Result := DBIERR_TABLEFULL;
          Exit;
        End;
    End;
  If Not Dictionary.CheckRequiredRecordFields(aData) Then
    Result := DBIERR_REQDERR
  Else
    Begin
      {we need to add the default field values}
      If Dictionary.DefaultFieldCount > 0 Then
        Begin
          Dictionary.FUserName := TfsSrBaseCursor(aCursorID).Client.ClientName;
          Dictionary.SetDefaultFieldUpdateValues(aData, Nil);
        End;

      { Updating the autoinc value obtains an exclusive lock on block 0 which
        prevents other cursors from inserting the same or additional records
        until we are done. }
      btUpdateAutoInc(aTI, aData);
      FFTblAddRecord(Files[0], aTI, RefNr, aData, Dictionary, Files[Dictionary.BLOBFileNumber], AFlag);
      aNewRefNr := RefNr;
      FSAcquireRecordLock(Files[0], aTI, aNewRefNr, aLockType,
        aTI^.tirTrans.DatabaseID,
        aCursorID, False, False);
    End;
End;
{--------}

Function TfsSrcSimpleTable.InsertRecordNoDefault(aTI: PffTransInfo; {!!.10}
  aCursorID: TffCursorID;
  aData: PffByteArray;
  aLockType: TfsSrcLockType;
  Var aNewRefNr: TffInt64;
  aFlag: Byte): TffResult;
Var
  RefNr: TffInt64;
  Rflags: Word;
  RMax, RCount: Longword;
Begin
  Result := DBIERR_NONE;
  RefNr.iLow := 0;
  RefNr.iHigh := 0;

  Rflags := FFTblReadTableFlags(Files[0], aTI);
  If ((Rflags And fsTableDontInsertRecord) <> 0) Then
    Begin
      Result := DBIERR_NOTSUFFFIELDRIGHTS;
      Exit;
    End;

  RMax := FFTblReadMaxRecords(Files[0], aTI);
  If RMax > 0 Then
    Begin
      RCount := FFTblReadRecordCount(Files[0], aTI);
      If RCount + 1 > RMax Then
        Begin
          Result := DBIERR_TABLEFULL;
          Exit;
        End;
    End;
  If Not Dictionary.CheckRequiredRecordFields(aData) Then
    Result := DBIERR_REQDERR
  Else
    Begin
      { Updating the autoinc value obtains an exclusive lock on block 0 which
        prevents other cursors from inserting the same or additional records
        until we are done. }
      btUpdateAutoInc(aTI, aData);
      FFTblAddRecord(Files[0], aTI, RefNr, aData, Dictionary, Files[Dictionary.BLOBFileNumber], AFlag);
      aNewRefNr := RefNr;
      FSAcquireRecordLock(Files[0], aTI, aNewRefNr, aLockType,
        aTI^.tirTrans.DatabaseID,
        aCursorID, False, False);
    End;
End;
{--------}

Procedure TfsSrcSimpleTable.MakeKIDForCursor(aIndexID: Integer; Var aKID: TffKeyIndexData);
Begin
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [btBaseName^, Self.ClassName]);
End;
{--------}

Procedure TfsSrcSimpleTable.OpenFiles(aTI: PffTransInfo; aForServer: boolean;
  aAttribs: TffFileAttributes);
Var
  FileInx: Integer;
Begin
  { Are any of the files marked as index files? If so then raise an exception
    because this class does not support indexes. Assume that file 0 is the
    data file. }
  For FileInx := 1 To pred(Dictionary.FileCount) Do
    If Dictionary.FileType[FileInx] = ftIndexFile Then
      FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
        [btBaseName^, Self.ClassName]);

  { If we've made it this far then open the files. }
  Inherited OpenFiles(aTI, aForServer, aAttribs);
End;
{--------}

Function TfsSrcSimpleTable.PutRecord(aTI: PffTransInfo;
  aCursorID: TffCursorID;
  aRefNr: TffInt64;
  aData: PffByteArray;
  aRelLock: boolean; {!!.05}
  aUserLockType: TfsUserRecLocking;
  Var aKeyChanged: Boolean): TffResult; {!!.05}
Var
  RecLen: Integer;
  OldData: PffByteArray;
  Rflags: Word;
  af: Byte;
Begin
  Rflags := FFTblReadTableFlags(Files[0], aTI);
  If ((Rflags And fsTableDontModifyRecord) <> 0) Then
    Begin
      Result := DBIERR_NOTSUFFFIELDRIGHTS;
      Exit;
    End;

  { Assumption: By the time we have reached this point, the transaction has
    acquired a content lock on the table and we are the only ones who are
    going to be modifying the record. }

  aKeyChanged := False; {!!.05}
  RecLen := 0;
  If Not Dictionary.CheckRequiredRecordFields(aData) Then
    Begin
      Result := DBIERR_REQDERR;
      Exit;
    End;
  {If Dictionary.DefaultFieldCount > 0 Then
    Begin
      Dictionary.FUserName := TfsSrBaseCursor(aCursorID).Client.ClientName;
      Dictionary.SetDefaultFieldValues(aData, uUpdate);
    End;}
  Result := DBIERR_NONE;

  Try
    Try
      RecLen := Dictionary.RecordLength;
      FFGetMem(OldData, RecLen);

      FFTblReadRecord(Files[0], aTI, aRefNr, OldData, Self.Files[0].fiFSVersion, af);
      If GetFlags(af, 32) Then
        Result := DBIERR_NOTSUFFFIELDRIGHTS
      Else
        Begin
          { Acquire an exclusive lock. }
          If aUserLockType <> tluDatabase Then
            FSUserAcquireRecordLock(Files[0], aTI, aRefNr, ffsltExclusive,
              aTI^.tirTrans.DatabaseID, aCursorID, False, aUserLockType, False)
          Else
            FSAcquireRecordLock(Files[0], aTI, aRefNr, ffsltExclusive,
              aTI^.tirTrans.DatabaseID, aCursorID, False, False);

          FFTblUpdateRecord(Files[0], aTI, aRefNr, aData, Dictionary, Files[Dictionary.BLOBFileNumber], Self.Files[0].fiFSVersion);
        End;
    Except
      FFRelRecordLock(Files[0], aTI, aRefNr, aTI^.tirTrans.DatabaseID, {!!.10}
        aCursorID); {!!.10}
    End;
  Finally
    FFFreeMem(OldData, RecLen);
  End; {try..finally}
End;
{====================================================================}

{===TfsSrcSimpleCursor================================================}

Constructor TfsSrcSimpleCursor.Create(anEngine: TFSServer;
  aDatabase: TfsSrcDatabase;
  Const aTimeout: Longint);
Begin
  bcTableClass := TfsSrcSimpleTable;
  Inherited Create(anEngine, aDatabase, aTimeout);
End;
{--------}

Destructor TfsSrcSimpleCursor.Destroy;
Begin
  { Free the table locks held by the cursor. }
  If Assigned(bcTable) Then
    bcTable.RelLock(CursorID, True);

  Inherited Destroy;
End;
{--------}

Function TfsSrcSimpleCursor.AddIndexToTable(Const aIndexDesc: TffIndexDescriptor): TffResult;
Begin
  Result := DBIERR_INVALIDINDEXCREATE;
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Procedure TfsSrcSimpleCursor.bcTableOpenPreconditions(aTable: TfsSrcBaseTable;
  Const aIndexName: String;
  Var aIndexID: Longint;
  Const aOpenMode: TffOpenMode);
Begin
  { Ignore the index information. }

  { If the table's data file is open in read-only mode it means the
    physical file is read-only: hence this call's openmode must be
    read-only as well. }
  If (aTable.Files[0]^.fiOpenMode = omReadOnly) And
    (aOpenMode <> omReadOnly) Then
    FSRaiseException(EfsException, fsStrResServer, fserrCursorReadOnly,
      [aTable.BaseName]);
End;
{--------}

Function TfsSrcSimpleCursor.CheckBookmark(aBookmark: PffByteArray): TffResult;
Var
  CheckHash: Longint;
Begin
  Result := DBIERR_INVALIDBOOKMARK;
  If (aBookmark = Nil) Then
    Exit;
  With PfsSrBookmark(aBookmark)^ Do
    Begin
      CheckHash := FSCalcELFHash(sbIndexID,
        fscl_FixedBookmarkSize - SizeOf(sbHash));
      If (sbHash <> CheckHash) Then
        Exit;
    End;
  Result := DBIERR_NONE;
End;
{--------}

Procedure TfsSrcSimpleCursor.ClearIndex;
Begin
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Function TfsSrcSimpleCursor.CloneCursor(aOpenMode: TffOpenMode): TfsSrBaseCursor;
Begin
  {NOTE: we are not checking rights for this action because the client
         had the rights to open the cursor}

  { Resolve the open mode. }
  If (bcOpenMode = omReadOnly) Then
    aOpenMode := omReadOnly;

  AcqContentLock(fsclmRead);
  Try
    { Create the cursor. }
    {$IFDEF DCC7OrLater}
    {$WARNINGS OFF}
    {$ENDIF}
    Result := TfsSrcSimpleCursor.Create(bcEngine, bcDatabase, soTimeout);
    {$IFDEF DCC7OrLater}
    {$WARNINGS ON} 
    {$ENDIF}

    Result.Open(bcTable.BaseName,
      '',
      bcIndexID,
      aOpenMode,
      smShared,
      bcTable.IsServerTable,
      False,
      bcTable.Files[0]^.fiAttributes,
      True);

    { Set up all of the misc fields. }
    Result.CursorInfo := bcInfo;
    If Assigned(bcFilter) Then
      Result.SetFilter(bcFilter.Expression, bcFilter.Timeout);
  Finally
    RelContentLock(fsclmRead);
  End;
End;
{--------}

Function TfsSrcSimpleCursor.CompareBookmarks(aBookmark1, aBookmark2: PffByteArray;
  Var CmpResult: Longint): TffResult;
Var
  BM1: PfsSrBookmark Absolute aBookmark1;
  BM2: PfsSrBookmark Absolute aBookmark2;
Begin
  Result := CheckBookmark(aBookmark1);
  If (Result = DBIERR_NONE) Then
    Result := CheckBookmark(aBookmark2);
  If (Result <> DBIERR_NONE) Then
    Exit;
  Case BM1^.sbPos Of
    cpUnknown: CmpResult := -1;
    cpBOF: If (BM2^.sbPos = cpBOF) Then
        CmpResult := 0
      Else
        CmpResult := -1;
    cpEOF: If (BM2^.sbPos = cpEOF) Then
        CmpResult := 0
      Else
        CmpResult := 1;
    Else
      { Bookmark 1 is on a crack or on a record. }
      Case BM2^.sbPos Of
        cpUnknown: CmpResult := 1;
        cpBOF: CmpResult := 1;
        cpEOF: CmpResult := -1;
        Else
          { Bookmark 2 is also on a crack or on a record.  Check the reference
            numbers.}
          CmpResult := ffCmpI64(BM1^.sbRefNr, BM2^.sbRefNr);
      End; {case}
  End; {case}
End;
{--------}

Function TfsSrcSimpleCursor.DropIndexFromTable(Const aIndexName: TffDictItemName;
  aIndexID: Longint): TffResult;
Begin
  Result := DBIERR_INVALIDINDEXCREATE;
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Function TfsSrcSimpleCursor.ExtractKey(aData: PffByteArray; aKey: PffByteArray): TffResult;
Begin
  Result := DBIERR_INVALIDINDEXCREATE;
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Function TfsSrcSimpleCursor.GetBookmark(aBookmark: PffByteArray): TffResult;
Begin
  Result := DBIERR_NONE;
  AcqContentLock(fsclmRead);
  Try
    FillChar(PfsSrBookmark(aBookmark)^, fscl_FixedBookmarkSize, 0);
    With PfsSrBookmark(aBookmark)^ Do
      Begin
        sbRefNr := bcInfo.RefNr;
        sbPos := bcInfo.Pos;
        sbKeyValid := bcInfo.KeyValid;
        sbHash := FSCalcELFHash(sbIndexID, fscl_FixedBookmarkSize - sizeof(sbHash));
      End;
  Finally
    RelContentLock(fsclmRead);
  End;
End;
{--------}

Function TfsSrcSimpleCursor.GetBookmarkSize: Integer;
Begin
  Result := fscl_FixedBookmarkSize;
End;
{--------}

Function TfsSrcSimpleCursor.GetNextRecord(aData: PffByteArray; aLockType: TfsSrcLockType; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Begin

  { If we are at EOF, then obviously there's no next record. }
  If (bcInfo.pos = cpEOF) Then
    Begin
      Result := DBIERR_EOF;
      aRefNr := bcInfo.refnr;
      Exit;
    End;

  AcqContentLock(fsclmRead);
  Try

    { Make sure that we have somewhere to read the record into. }
    If (aData = Nil) Then
      aData := bcRecordData;

    If Assigned(bcFilter) Then
      bcFilter.BeginTimeout;

    Result := DBIERR_NONE;
    Repeat
      bcTable.GetNextRecordSeq(bcDatabase.TransactionInfo, bcInfo.RefNr, aData, False, False, AFlag);

      If ffI64IsZero(bcInfo.RefNr) Then
        Begin
          Result := DBIERR_EOF;
          SetToEnd;
          aRefNr := bcInfo.refnr;
          Exit;
        End;

      { In theory we're on a record. }
      bcInfo.Deleted := False;
      bcInfo.KeyValid := True;
      bcInfo.Pos := cpOnRecord;
    Until (Result <> DBIERR_NONE) Or Not Assigned(bcFilter) Or
      bcFilter.MatchesRecord(aData) Or bcFilter.CheckTimeout(Result);

    { Place the lock if needed... record will not be read again. }
    If (Result = DBIERR_NONE) And (aLockType <> ffsltNone) Then
      Result := Table.GetRecord(bcDatabase.TransactionInfo,
        bcDatabase.DatabaseID, CursorID, {!!.10}
        bcInfo.RefNr, Nil, aLockType, tluDatabase, False, False, aFlag); {!!.02}
    aRefNr := bcInfo.refnr;
    If Result > 0 Then
      Begin
        aRefNr.iLow := 0;
        aRefNr.iHigh := 0;
      End;
  Finally
    RelContentLock(fsclmRead);
  End;
End;
{--------}

Function TfsSrcSimpleCursor.GetPriorRecord(aData: PffByteArray; aLockType: TfsSrcLockType; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Begin

  { If we are at BOF, then obviously there's no prior record. }
  If (bcInfo.pos = cpBOF) Then
    Begin
      Result := DBIERR_BOF;
      aRefNr := bcInfo.refnr;
      Exit;
    End;

  AcqContentLock(fsclmRead);
  Try

    { Make sure that we have somewhere to read the record into. }
    If (aData = Nil) Then
      aData := bcRecordData;

    { Get the previous record. }
    If Assigned(bcFilter) Then
      bcFilter.BeginTimeout;

    Result := DBIERR_NONE;
    Repeat
      bcTable.GetPrevRecordSeq(bcDatabase.TransactionInfo, bcInfo.RefNr,
        aData, aFlag);
      If FFI64IsZero(bcInfo.RefNr) Then
        Begin
          Result := DBIERR_BOF;
          SetToBegin;
          aRefNr := bcInfo.refnr;
          Exit;
        End;

      { In theory we're on a record. }
      bcInfo.Deleted := False;
      bcInfo.KeyValid := True;
      bcInfo.Pos := cpOnRecord;

    Until (Result <> DBIERR_NONE) Or Not Assigned(bcFilter) Or
      bcFilter.MatchesRecord(aData) Or bcFilter.CheckTimeout(Result);

    { Place the lock if needed... record will not be read again. }
    If (Result = DBIERR_NONE) And (aLockType <> ffsltNone) Then
      Result := bcTable.GetRecord(bcDatabase.TransactionInfo, {!!.10}
        bcDatabase.DatabaseID, {!!.10}
        CursorID, {!!.10}
        bcInfo.refNr, Nil, aLockType, tluDatabase, False, False, aFlag); {!!.02}
    aRefNr := bcInfo.refnr;
    If Result > 0 Then
      Begin
        aRefNr.iLow := 0;
        aRefNr.iHigh := 0;
      End;
  Finally
    RelContentLock(fsclmRead);
  End;

End;
{--------}

Function TfsSrcSimpleCursor.GetRecordCount(Var aRecCount: Longword): TffResult;
Var
  aData: PffByteArray;
  Info: TffRecordInfo;
  aFlag: Byte;
  aRefNr: TffInt64;
Begin
  Result := DBIERR_NONE;
  AcqContentLock(fsclmRead);
  Try
    { Is a filter active? }
    If Assigned(bcFilter) Then
      Begin
        { Yes. Set count to zero. We are going to scan through the records. }
        aRecCount := 0;
        { Save the current position. }
        bcSaveCurInfo;
        FFGetZeroMem(aData, bcRecordLen);
        Try
          {BOF}
          SetToBegin;

          { While not EOF or other error do. }
          While (Result = DBIERR_NONE) Do
            Begin
              Result := GetNextRecord(aData, ffsltNone, aFlag, aRefNr);
              If (Result = DBIERR_NONE) Then
                inc(aRecCount);
            End;
          Result := DBIERR_NONE;
        Finally
          FFFreeMem(aData, bcRecordLen);
          { Reset current position. }
          bcRestoreCurInfo;
        End;
      End
    Else
      Begin
        FFTblGetRecordInfo(bcTable.Files[0], bcDatabase.TransactionInfo, Info);
        aRecCount := Info.riRecCount;
      End;
  Finally
    RelContentLock(fsclmRead);
  End;
End;
{--------}

Function TfsSrcSimpleCursor.GetRecordForKey(aDirectKey: boolean;
  aFieldCount: Integer;
  aPartialLen: Integer;
  aKeyData: PffByteArray;
  aData: PffByteArray;
  aFirstCall: Boolean): TffResult;
Begin
  Result := DBIERR_INVALIDINDEXCREATE;
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Function TfsSrcSimpleCursor.InsertRecord(aData: PffByteArray; aLockType: TfsSrcLockType; aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Var
  NewRefNr: TffInt64;

Begin
  aRefNr.iLow := 0;
  aRefNr.iHigh := 0;
  { Notify extenders. }
  Result := NotifyExtenders(ffeaBeforeRecInsert, ffeaInsertRecFail);
  If Result = DBIERR_NONE Then
    Begin
      AcqContentLock(fsclmWrite);

      bcTable.Dictionary.FUserName := Self.Client.ClientName;
      Result := bcTable.InsertRecord(bcDatabase.TransactionInfo,
        CursorID, aData, aLockType, NewRefNr, 0);
      If (Result = DBIERR_NONE) Then
        Begin
          bcInfo.pos := cpOnRecord;
          bcInfo.refNr := NewRefNr;
          bcInfo.Deleted := False;
          bcInfo.KeyValid := True;
          aRefNr := bcInfo.refnr;
          { Notify extenders of successful insert. }
          NotifyExtenders(ffeaAfterRecInsert, ffeaNoAction);
        End
      Else
        Begin
          aRefNr.iLow := 0;
          aRefNr.iHigh := 0;
          { Notify extenders of failed insert. }
          NotifyExtenders(ffeaInsertRecFail, ffeaNoAction);
        End;
    End;
End;
{--------}

Function TfsSrcSimpleCursor.InsertRecordNoDefault(aData: PffByteArray; {!!.10}
  aLockType: TfsSrcLockType; aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Var
  NewRefNr: TffInt64;

Begin
  aRefNr.iLow := 0;
  aRefNr.iHigh := 0;
  { Notify extenders. }
  Result := NotifyExtenders(ffeaBeforeRecInsert, ffeaInsertRecFail);
  If Result = DBIERR_NONE Then
    Begin
      AcqContentLock(fsclmWrite);
      Result := bcTable.InsertRecordNoDefault(bcDatabase.TransactionInfo, {!!.10}
        CursorID, aData, aLockType, NewRefNr, 0);
      If (Result = DBIERR_NONE) Then
        Begin
          bcInfo.pos := cpOnRecord;
          bcInfo.refNr := NewRefNr;
          bcInfo.Deleted := False;
          bcInfo.KeyValid := True;
          aRefNr := bcInfo.refnr;
          { Notify extenders of successful insert. }
          NotifyExtenders(ffeaAfterRecInsert, ffeaNoAction);
        End
      Else
        Begin
          aRefNr.iLow := 0;
          aRefNr.iHigh := 0;
          { Notify extenders of failed insert. }
          NotifyExtenders(ffeaInsertRecFail, ffeaNoAction);
        End;
    End;
End;
{--------}

Function TfsSrcSimpleCursor.IsInRange(aKey: PffByteArray): Integer;
Begin
  { This class does not support ranges. }
  Result := 0;
  FSRaiseException(EfsException, fsStrResServer, fserrRangeNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Function TfsSrcSimpleCursor.ModifyRecord(aData: PffByteArray; aRelLock: boolean; aUserLockType: TfsUserRecLocking; aFlag: Byte; aSet, Use: Boolean):
  TffResult;
Var
  aKeyChanged: Boolean;

Begin
  Result := 0;
  If Use Then Exit;
  { Note: By this time, any other cursor deleting or modifying the record ahead
    of us has completed and has set bcInfo.Deleted.  We can be assured of this
    because TFSServer.RecordDelete calls Cursor.EnsureWritable(true) which
    obtains a lock on the record to be deleted.  We won't get that lock until
    the other cursor has finished. }

  { Has this record already been deleted? }
  If bcInfo.Deleted Then
    Begin
      { Yes. }
      Result := DBIERR_KEYORRECDELETED;
      Exit;
    End;

  { Are we on a record? }
  If (bcInfo.Pos <> cpOnRecord) Then
    Begin
      { No. }
      Case bcInfo.Pos Of
        cpBOF: Result := DBIERR_BOF;
        cpEOF: Result := DBIERR_EOF;
        Else
          Result := DBIERR_NOCURRREC;
      End;
      Exit;
    End;

  { Notify extenders. }
  Result := NotifyExtenders(ffeaBeforeRecUpdate, ffeaUpdateRecFail);
  If (Result = DBIERR_NONE) Then
    Begin
      AcqContentLock(fsclmWrite);
      bcTable.Dictionary.FUserName := Self.Client.ClientName;
      Result := bcTable.PutRecord(bcDatabase.TransactionInfo, CursorID, bcInfo.refNr,
        aData, aRelLock, aUserLockType, aKeyChanged); {!!.05}
      If (Result = DBIERR_NONE) Then
        Begin
          bcInfo.KeyValid := True;
          bcInfo.pos := cpOnRecord;
          { Notify extenders of successful update. }
          NotifyExtenders(ffeaAfterRecUpdate, ffeaNoAction);
        End
      Else
        { Notify extenders of failed update. }
        NotifyExtenders(ffeaUpdateRecFail, ffeaNoAction);
    End;
End;
{--------}

Procedure TfsSrcSimpleCursor.Open(Const aTableName: TfsTableName;
  Const aIndexName: TffName;
  Const aIndexID: Longint;
  Const aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  aForServer: boolean;
  Const aExclContLock: Boolean; {!!.10}
  aAttribs: TffFileAttributes; SysOpen: Boolean);
Begin
  Inherited Open(aTableName, aIndexName, aIndexID, aOpenMode, aShareMode,
    aForServer, aExclContLock, aAttribs, True); {!!.10}
  SetToBegin;
End;
{--------}

Procedure TfsSrcSimpleCursor.ResetRange;
Begin
  FSRaiseException(EfsException, fsStrResServer, fserrRangeNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Function TfsSrcSimpleCursor.SetRange(aDirectKey: boolean;
  aFieldCount1: Integer;
  aPartialLen1: Integer;
  aKeyData1: PffByteArray;
  aKeyIncl1: boolean;
  aFieldCount2: Integer;
  aPartialLen2: Integer;
  aKeyData2: PffByteArray;
  aKeyIncl2: boolean): TffResult;
Begin
  Result := DBIERR_FS_RangeNotSupported;
  FSRaiseException(EfsException, fsStrResServer, fserrRangeNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Procedure TfsSrcSimpleCursor.SetToBegin;
Begin
  AcqContentLock(fsclmRead);
  Try
    bcInfo.Deleted := False;
    bcInfo.KeyValid := False;
    bcInfo.Pos := cpBOF;
    FFInitI64(bcInfo.RefNr);
  Finally
    RelContentLock(fsclmRead);
  End;
End;
{--------}

Function TfsSrcSimpleCursor.SetToBookmark(aBookmark: PffByteArray): TffResult;
Var
  aFlag: Byte;
Begin
  Result := CheckBookmark(aBookmark);
  If (Result = DBIERR_NONE) Then
    Begin
      AcqContentLock(fsclmRead);
      Try
        { Initialize the key path. }
        FFInitKeyPath(bcInfo.KeyPath);
        With PfsSrBookmark(aBookmark)^ Do
          Begin
            bcInfo.Pos := sbPos;
            bcInfo.RefNr := sbRefNr;
            bcInfo.KeyValid := sbKeyValid;
            bcInfo.Deleted := False;

            { Does the record still exist? }
            Try
              bcTable.GetRecord(bcDatabase.TransactionInfo, {!!.10}
                bcDatabase.DatabaseID, {!!.10}
                CursorID, sbRefNr, {!!.10}
                bcRecordData, ffsltNone, tluDatabase, False, False, aflag); {!!.02}
            Except
              On E: EfsException Do
                Begin
                  If E.ErrorCode = fserrRecDeleted Then
                    Begin
                      bcInfo.Pos := cpOnCrack;
                      bcInfo.Deleted := True;
                    End
                  Else
                    Begin
                      SetToBegin;
                      Result := DBIERR_INVALIDBOOKMARK;
                    End;
                End
              Else
                Begin
                  SetToBegin;
                  Result := DBIERR_INVALIDBOOKMARK;
                End;
            End;
          End; { with }
      Finally
        RelContentLock(fsclmRead);
      End;
    End;
End;
{--------}

Function TfsSrcSimpleCursor.SetToCursor(aCursor: TfsSrBaseCursor): TffResult;
Begin
  Result := DBIERR_NONE;
  If (aCursor.Table <> Table) Then
    Begin
      Result := DBIERR_DIFFERENTTABLES;
      Exit;
    End;

  AcqContentLock(fsclmRead);
  Try
    bcInfo := aCursor.CursorInfo;
  Finally
    RelContentLock(fsclmRead);
  End;
End;
{--------}

Procedure TfsSrcSimpleCursor.SetToEnd;
Begin
  AcqContentLock(fsclmRead);
  Try
    bcInfo.Pos := cpEOF;
    bcInfo.KeyValid := False;
    FFInitI64(bcInfo.refNr);
    bcInfo.Deleted := False;
  Finally
    RelContentLock(fsclmRead);
  End;
End;
{--------}

Function TfsSrcSimpleCursor.SetToKey(aSearchAction: TffSearchKeyAction;
  aDirectKey: boolean;
  aFieldCount: Integer;
  aPartialLen: Integer;
  aKeyData: PffByteArray): TffResult;
Begin
  { To set to a specific record, specify a value for the RefNr property. }
  Result := DBIERR_INVALIDINDEXCREATE;
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Function TfsSrcSimpleCursor.SwitchToIndex(aIndexID: Integer;
  aPosnOnRec: boolean): TffResult;
Begin
  Result := DBIERR_INVALIDINDEXCREATE;
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{====================================================================}

{===TfsSrcSQLSQLTable====================================================}

Procedure TfsSrcSQLSimpleTable.AddIndex(Const aIndexDesc: TffIndexDescriptor;
  aTI: PffTransInfo);
Begin
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [btBaseName^, Self.ClassName]);
End;
{--------}

Procedure TfsSrcSQLSimpleTable.BuildFiles(aTI: PffTransInfo;
  aForServer: boolean;
  aDictionary: TFSInfoDict;
  aAttribs: TffFileAttributes;
  aStore: TfsBaseTempStorage);
Var
  FileInx: Integer;
  DataFile: PffFileInfo;
  FileCnt: Integer; {dup for speed}
Begin
  { Create the data file. }
  btFiles.Count := 1;
  btFiles[0] := FFAllocFileInfo(FFMakeFullFileName(Folder.Path, BaseName),
    fsc_ExtForData, btBufMgr);

  { Validate the dictionary. }
  aDictionary.CheckValid;

  { Assimilate the dictionary. }
  btDictionary.ForceOffReadOnly;
  btDictionary.Assign(aDictionary);
  btDictionary.BindIndexHelpers;

  With PffFileInfo(btFiles[0])^ Do
    Begin
      fiAttributes := aAttribs;
      fiForServer := aForServer;

      If aForServer Then
        fiEncrypted := btEngine.Configuration.GeneralInfo^.giEncrypt
      Else
        fiEncrypted := aDictionary.IsEncrypted;

      If aDictionary.EngineDeleteType In [edtUndeleteIfPossibleNotBlob, edtUndeleteIfPossibleAndBlob] Then
        fiRecLenPlusTrailer := btDictionary.RecordLength + SizeOf(Byte) + sizeof(TffInt64) // extra info for delete
      Else If aDictionary.EngineDeleteType In [edtUndeleteFull] Then
        fiRecLenPlusTrailer := btDictionary.RecordLength + SizeOf(Byte) + sizeof(TffInt64) + sizeof(TffInt64) // extra info for delete
      Else
        fiRecLenPlusTrailer := btDictionary.RecordLength + SizeOf(Byte);
      If fiVersionRecord = trUseVersion Then
        fiRecLenPlusTrailer := fiRecLenPlusTrailer + Sizeof(Longword); // for recversion
      fiRecordLength := btDictionary.RecordLength;
      fiTempStore := aStore;
    End;

  { Get the file count for this table (for speed reasons, etc). }
  FileCnt := Dictionary.FileCount;
  FileCount := FileCnt;

  { Get the data file for speed reasons. }
  DataFile := Files[0];

  { Scan through the secondary files. This table supports separate BLOB
    files but not separate index files. }
  For FileInx := 0 To pred(FileCnt) Do
    Begin
      If Dictionary.FileType[FileInx] = ftIndexFile Then
        FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
          [btBaseName^, Self.ClassName]);
      btCreateFile(FileInx, aTI, btDictionary.FileExt[FileInx], aForServer,
        aAttribs, aStore);
    End;

  { Write the dictionary to the data file. }
  Dictionary.WriteToFile(DataFile, aTI);
  {Begin !!.11}
  Files[Dictionary.BLOBFileNumber].fiBLOBrscMgr :=
    TffBaseBLOBResourceMgr.GetMgr(Files[Dictionary.BLOBFileNumber]);
  btBLOBEngine := TffBaseBLOBEngine.GetEngine(Files[Dictionary.BLOBFileNumber]);
  {End !!.11}
  Files[Dictionary.BLOBFileNumber].fiMaxSegSize :=
    FFCalcMaxBLOBSegSize(Files[Dictionary.BLOBFileNumber]);

End;
{--------}

Function TfsSrcSQLSimpleTable.BuildKeyForRecord(aIndexID: Integer;
  aData: PffByteArray;
  aKey: PffByteArray;
  aFieldCount: Integer;
  aPartialLen: Integer): TffResult;
Begin
  Result := DBIERR_INVALIDINDEXCREATE;
End;
{--------}

Function TfsSrcSQLSimpleTable.CompareKeysForCursor(Var aKID: TffKeyIndexData;
  aKey1: PffByteArray;
  aKey2: PffByteArray): Integer;
Begin
  Result := DBIERR_INVALIDINDEXCREATE;
End;
{--------}

Function TfsSrcSQLSimpleTable.DeleteRecord(aTI: PffTransInfo;
  Const aCursorID: TffCursorID;
  Const aRefNr: TffInt64;
  Const aLockObtained: Boolean;
  Var aBTreeChanged: Boolean)
: TffResult;
Var
  OldData: PffByteArray;
  RecLen: Integer;
  af: Byte;

Begin
  RecLen := btDictionary.RecordLength;
  FFGetMem(OldData, RecLen);
  Result := DBIERR_NONE;

  Try
    FFTblReadRecord(Files[0], aTI, aRefNr, OldData, Self.Files[0].fiFSVersion, af);
    If GetFlags(af, 16) Then
      Result := DBIERR_NOTSUFFFIELDRIGHTS
    Else
      FFTblDeleteRecord(Files[0], aTI, aRefNr, Dictionary, Files[Dictionary.BLOBFileNumber], OldData, True);
  Finally
    btInformCursors(aCursorID, roDelete, aRefNr, 0);
    FFFreeMem(OldData, RecLen);
  End; {try..finally}
End;
{--------}

Procedure TfsSrcSQLSimpleTable.DropIndex(aTI: PffTransInfo;
  aIndexID: Longint);
Begin
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [btBaseName^, Self.ClassName]);
End;
{--------}

Function TfsSrcSQLSimpleTable.FindKey(Var aKID: TffKeyIndexData;
  Var aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath;
  aAction: TffSearchKeyAction): boolean;
Begin
  ffInitI64(aRefNr);
  Result := False;
End;
{--------}

Function TfsSrcSQLSimpleTable.GetNextKey(Var aKID: TffKeyIndexData;
  Var aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath): TffResult;
Begin
  Result := DBIERR_INVALIDINDEXCREATE;
End;
{--------}

Function TfsSrcSQLSimpleTable.GetNextRecord(aTI: PffTransInfo;
  Const aDatabaseID: TffDatabaseID; {!!.10}
  Const aCursorID: TffCursorID; {!!.10}
  Var aKID: TffKeyIndexData;
  Var aRefNr: TffInt64;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath;
  aData: PffByteArray;
  Const aLockType: TfsSrcLockType;
  Var aFlag: Byte): TffResult;
Begin
  Result := DBIERR_NONE;
  GetNextRecordSeq(aTI, aRefNr, aData, False, False, AFlag);
End;
{--------}

Function TfsSrcSQLSimpleTable.GetPriorRecord(aTI: PffTransInfo;
  Const aDatabaseID: TffDatabaseID; {!!.10}
  Const aCursorID: TffCursorID; {!!.10}
  Var aKID: TffKeyIndexData;
  Var aRefNr: TffInt64;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath;
  aData: PffByteArray;
  Const aLockType: TfsSrcLockType;
  Var aFlag: Byte): TffResult; {!!.10}
Begin
  Result := DBIERR_NONE;
  GetPrevRecordSeq(aTI, aRefNr, aData, aFlag);
End;
{--------}

Function TfsSrcSQLSimpleTable.InsertRecord(aTI: PffTransInfo;
  aCursorID: TffCursorID;
  aData: PffByteArray;
  aLockType: TfsSrcLockType;
  Var aNewRefNr: TffInt64;
  aFlag: Byte): TffResult;
Var 
  RefNr: TffInt64;

Begin
  RefNr.iLow := 0;
  RefNr.iHigh := 0;
  Result := DBIERR_NONE;

  If Not Dictionary.CheckRequiredRecordFields(aData) Then
    Result := DBIERR_REQDERR
  Else
    Begin
      {we need to add the default field values}
      If Dictionary.DefaultFieldCount > 0 Then
        Begin
          Dictionary.FUserName := TfsSrBaseCursor(aCursorID).Client.ClientName;
          Dictionary.SetDefaultFieldUpdateValues(aData, Nil);
        End;

      { Updating the autoinc value obtains an exclusive lock on block 0 which
        prevents other cursors from inserting the same or additional records
        until we are done. }
      btUpdateAutoInc(aTI, aData);
      FFTblAddRecord(Files[0], aTI, RefNr, aData, Dictionary, Files[Dictionary.BLOBFileNumber], AFlag);
      aNewRefNr := RefNr;
    End;
End;
{--------}

Function TfsSrcSQLSimpleTable.InsertRecordNoDefault(aTI: PffTransInfo; {!!.10}
  aCursorID: TffCursorID;
  aData: PffByteArray;
  aLockType: TfsSrcLockType;
  Var aNewRefNr: TffInt64;
  aFlag: Byte): TffResult;
Var
  RefNr: TffInt64;

Begin
  Result := DBIERR_NONE;
  RefNr.iLow := 0;
  RefNr.iHigh := 0;

  If Not Dictionary.CheckRequiredRecordFields(aData) Then
    Result := DBIERR_REQDERR
  Else
    Begin
      { Updating the autoinc value obtains an exclusive lock on block 0 which
        prevents other cursors from inserting the same or additional records
        until we are done. }
      btUpdateAutoInc(aTI, aData);
      FFTblAddRecord(Files[0], aTI, RefNr, aData, Dictionary, Files[Dictionary.BLOBFileNumber], AFlag);
      aNewRefNr := RefNr;
    End;
End;
{--------}

Procedure TfsSrcSQLSimpleTable.MakeKIDForCursor(aIndexID: Integer; Var aKID: TffKeyIndexData);
Begin
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [btBaseName^, Self.ClassName]);
End;
{--------}

Procedure TfsSrcSQLSimpleTable.OpenFiles(aTI: PffTransInfo; aForServer: boolean;
  aAttribs: TffFileAttributes);
Var
  FileInx: Integer;
Begin
  { Are any of the files marked as index files? If so then raise an exception
    because this class does not support indexes. Assume that file 0 is the
    data file. }
  For FileInx := 1 To pred(Dictionary.FileCount) Do
    If Dictionary.FileType[FileInx] = ftIndexFile Then
      FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
        [btBaseName^, Self.ClassName]);

  { If we've made it this far then open the files. }
  Inherited OpenFiles(aTI, aForServer, aAttribs);
End;
{--------}

Function TfsSrcSQLSimpleTable.PutRecord(aTI: PffTransInfo;
  aCursorID: TffCursorID;
  aRefNr: TffInt64;
  aData: PffByteArray;
  aRelLock: boolean;
  aUserLockType: TfsUserRecLocking;
  Var aKeyChanged: Boolean): TffResult; 
Var
  RecLen: Integer;
  OldData: PffByteArray;
  af: Byte;

Begin
    Result := DBIERR_NONE;

    { Assumption: By the time we have reached this point, the transaction has
      acquired a content lock on the table and we are the only ones who are
      going to be modifying the record. }

  aKeyChanged := False; {!!.05}
  RecLen := 0;
  If Not Dictionary.CheckRequiredRecordFields(aData) Then
    Begin
      Result := DBIERR_REQDERR;
      Exit;
    End;

  Try
    Try
      RecLen := Dictionary.RecordLength;
      FFGetMem(OldData, RecLen);
      FFTblReadRecord(Files[0], aTI, aRefNr, OldData, Self.Files[0].fiFSVersion, af);
      If GetFlags(af, 32) Then
        Result := DBIERR_NOTSUFFFIELDRIGHTS
      Else
        FFTblUpdateRecord(Files[0], aTI, aRefNr, aData, Dictionary, Files[Dictionary.BLOBFileNumber], Self.Files[0].fiFSVersion);
    Except
    raise;
    End;
  Finally
    FFFreeMem(OldData, RecLen);
  End; {try..finally}
End;
{====================================================================}

{===TfsSrcSQLSimpleCursor================================================}

Constructor TfsSrcSQLSimpleCursor.Create(anEngine: TFSServer;
  aDatabase: TfsSrcDatabase;
  Const aTimeout: Longint);
Begin
  bcTableClass := TfsSrcSQLSimpleTable;
  Inherited Create(anEngine, aDatabase, aTimeout);
End;
{--------}

Destructor TfsSrcSQLSimpleCursor.Destroy;
Begin
  { Free the table locks held by the cursor. }
  If Assigned(bcTable) Then
    bcTable.RelLock(CursorID, True);

  Inherited Destroy;
End;
{--------}

Function TfsSrcSQLSimpleCursor.AddIndexToTable(Const aIndexDesc: TffIndexDescriptor): TffResult;
Begin
  Result := DBIERR_INVALIDINDEXCREATE;
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Procedure TfsSrcSQLSimpleCursor.bcTableOpenPreconditions(aTable: TfsSrcBaseTable;
  Const aIndexName: String;
  Var aIndexID: Longint;
  Const aOpenMode: TffOpenMode);
Begin
  { Ignore the index information. }

  { If the table's data file is open in read-only mode it means the
    physical file is read-only: hence this call's openmode must be
    read-only as well. }
  If (aTable.Files[0]^.fiOpenMode = omReadOnly) And
    (aOpenMode <> omReadOnly) Then
    FSRaiseException(EfsException, fsStrResServer, fserrCursorReadOnly,
      [aTable.BaseName]);
End;
{--------}

Function TfsSrcSQLSimpleCursor.CheckBookmark(aBookmark: PffByteArray): TffResult;
Var
  CheckHash: Longint;
Begin
  Result := DBIERR_INVALIDBOOKMARK;
  If (aBookmark = Nil) Then
    Exit;
  With PfsSrBookmark(aBookmark)^ Do
    Begin
      CheckHash := FSCalcELFHash(sbIndexID,
        fscl_FixedBookmarkSize - SizeOf(sbHash));
      If (sbHash <> CheckHash) Then
        Exit;
    End;
  Result := DBIERR_NONE;
End;
{--------}

Procedure TfsSrcSQLSimpleCursor.ClearIndex;
Begin
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Function TfsSrcSQLSimpleCursor.CloneCursor(aOpenMode: TffOpenMode): TfsSrBaseCursor;
Begin
  {NOTE: we are not checking rights for this action because the client
         had the rights to open the cursor}

  { Resolve the open mode. }
  If (bcOpenMode = omReadOnly) Then
    aOpenMode := omReadOnly;

  { Create the cursor. }
  {$IFDEF DCC7OrLater}
  {$WARNINGS OFF}
  {$ENDIF}
  Result := TfsSrcSQLSimpleCursor.Create(bcEngine, bcDatabase, soTimeout);
  {$IFDEF DCC7OrLater}
  {$WARNINGS ON}
  {$ENDIF}

  Result.Open(bcTable.BaseName, '', bcIndexID, aOpenMode, smShared,
    bcTable.IsServerTable, False, bcTable.Files[0]^.fiAttributes, True);

  { Set up all of the misc fields. }
  Result.CursorInfo := bcInfo;
  If Assigned(bcFilter) Then
    Result.SetFilter(bcFilter.Expression, bcFilter.Timeout);
End;
{--------}

Function TfsSrcSQLSimpleCursor.CompareBookmarks(aBookmark1, aBookmark2: PffByteArray;
  Var CmpResult: Longint): TffResult;
Var
  BM1: PfsSrBookmark Absolute aBookmark1;
  BM2: PfsSrBookmark Absolute aBookmark2;
Begin
  Result := CheckBookmark(aBookmark1);
  If (Result = DBIERR_NONE) Then
    Result := CheckBookmark(aBookmark2);
  If (Result <> DBIERR_NONE) Then
    Exit;
  Case BM1^.sbPos Of
    cpUnknown: CmpResult := -1;
    cpBOF: If (BM2^.sbPos = cpBOF) Then
        CmpResult := 0
      Else
        CmpResult := -1;
    cpEOF: If (BM2^.sbPos = cpEOF) Then
        CmpResult := 0
      Else
        CmpResult := 1;
    Else
      { Bookmark 1 is on a crack or on a record. }
      Case BM2^.sbPos Of
        cpUnknown: CmpResult := 1;
        cpBOF: CmpResult := 1;
        cpEOF: CmpResult := -1;
        Else
          { Bookmark 2 is also on a crack or on a record.  Check the reference
            numbers.}
          CmpResult := ffCmpI64(BM1^.sbRefNr, BM2^.sbRefNr);
      End; {case}
  End; {case}
End;
{--------}

Function TfsSrcSQLSimpleCursor.DropIndexFromTable(Const aIndexName: TffDictItemName;
  aIndexID: Longint): TffResult;
Begin
  Result := DBIERR_INVALIDINDEXCREATE;
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Function TfsSrcSQLSimpleCursor.ExtractKey(aData: PffByteArray; aKey: PffByteArray): TffResult;
Begin
  Result := DBIERR_INVALIDINDEXCREATE;
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Function TfsSrcSQLSimpleCursor.GetBookmark(aBookmark: PffByteArray): TffResult;
Begin
  Result := DBIERR_NONE;
  FillChar(PfsSrBookmark(aBookmark)^, fscl_FixedBookmarkSize, 0);
  With PfsSrBookmark(aBookmark)^ Do
    Begin
      sbRefNr := bcInfo.RefNr;
      sbPos := bcInfo.Pos;
      sbKeyValid := bcInfo.KeyValid;
      sbHash := FSCalcELFHash(sbIndexID, fscl_FixedBookmarkSize - sizeof(sbHash));
    End;
End;
{--------}

Function TfsSrcSQLSimpleCursor.GetBookmarkSize: Integer;
Begin
  Result := fscl_FixedBookmarkSize;
End;
{--------}

Function TfsSrcSQLSimpleCursor.GetNextRecord(aData: PffByteArray; aLockType: TfsSrcLockType; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Begin
  { If we are at EOF, then obviously there's no next record. }
  If (bcInfo.pos = cpEOF) Then
    Begin
      Result := DBIERR_EOF;
      aRefNr := bcInfo.refnr;
      Exit;
    End;

  { Make sure that we have somewhere to read the record into. }
  If (aData = Nil) Then
    aData := bcRecordData;

  If Assigned(bcFilter) Then
    bcFilter.BeginTimeout;

  Result := DBIERR_NONE;
  Repeat
    bcTable.GetNextRecordSeq(bcDatabase.TransactionInfo, bcInfo.RefNr, aData, False, False, AFlag);

    If ffI64IsZero(bcInfo.RefNr) Then
      Begin
        Result := DBIERR_EOF;
        SetToEnd;
        aRefNr := bcInfo.refnr;
        Exit;
      End;

    { In theory we're on a record. }
    bcInfo.Deleted := False;
    bcInfo.KeyValid := True;
    bcInfo.Pos := cpOnRecord;
  Until (Result <> DBIERR_NONE) Or Not Assigned(bcFilter) Or
    bcFilter.MatchesRecord(aData) Or bcFilter.CheckTimeout(Result);

  { Place the lock if needed... record will not be read again. }
  If (Result = DBIERR_NONE) And (aLockType <> ffsltNone) Then
    Result := Table.GetRecord(bcDatabase.TransactionInfo,
      bcDatabase.DatabaseID, CursorID, {!!.10}
      bcInfo.RefNr, Nil, aLockType, tluDatabase, False, False, aFlag); {!!.02}
  aRefNr := bcInfo.refnr;
  If Result > 0 Then
    Begin
      aRefNr.iLow := 0;
      aRefNr.iHigh := 0;
    End;
End;
{--------}

Function TfsSrcSQLSimpleCursor.GetPriorRecord(aData: PffByteArray; aLockType: TfsSrcLockType; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Begin
  { If we are at BOF, then obviously there's no prior record. }
  If (bcInfo.pos = cpBOF) Then
    Begin
      Result := DBIERR_BOF;
      aRefNr := bcInfo.refnr;
      Exit;
    End;

  { Make sure that we have somewhere to read the record into. }
  If (aData = Nil) Then
    aData := bcRecordData;

  { Get the previous record. }
  If Assigned(bcFilter) Then
    bcFilter.BeginTimeout;

  Result := DBIERR_NONE;
  Repeat
    bcTable.GetPrevRecordSeq(bcDatabase.TransactionInfo, bcInfo.RefNr,
      aData, aFlag);
    If FFI64IsZero(bcInfo.RefNr) Then
      Begin
        Result := DBIERR_BOF;
        SetToBegin;
        aRefNr := bcInfo.refnr;
        Exit;
      End;

    { In theory we're on a record. }
    bcInfo.Deleted := False;
    bcInfo.KeyValid := True;
    bcInfo.Pos := cpOnRecord;

  Until (Result <> DBIERR_NONE) Or Not Assigned(bcFilter) Or
    bcFilter.MatchesRecord(aData) Or bcFilter.CheckTimeout(Result);

  { Place the lock if needed... record will not be read again. }
  If (Result = DBIERR_NONE) And (aLockType <> ffsltNone) Then
    Result := bcTable.GetRecord(bcDatabase.TransactionInfo, {!!.10}
      bcDatabase.DatabaseID, {!!.10}
      CursorID, {!!.10}
      bcInfo.refNr, Nil, aLockType, tluDatabase, False, False, aFlag); {!!.02}
  aRefNr := bcInfo.refnr;
  If Result > 0 Then
    Begin
      aRefNr.iLow := 0;
      aRefNr.iHigh := 0;
    End;
End;
{--------}

Function TfsSrcSQLSimpleCursor.GetRecordCount(Var aRecCount: Longword): TffResult;
Var
  aData: PffByteArray;
  Info: TffRecordInfo;
  aFlag: Byte;
  aRefNr: TffInt64;
Begin
  Result := DBIERR_NONE;
  { Is a filter active? }
  If Assigned(bcFilter) Then
    Begin
      { Yes. Set count to zero. We are going to scan through the records. }
      aRecCount := 0;
      { Save the current position. }
      bcSaveCurInfo;
      FFGetZeroMem(aData, bcRecordLen);
      Try
        {BOF}
        SetToBegin;

        { While not EOF or other error do. }
        While (Result = DBIERR_NONE) Do
          Begin
            Result := GetNextRecord(aData, ffsltNone, aFlag, aRefNr);
            If (Result = DBIERR_NONE) Then
              inc(aRecCount);
          End;
        Result := DBIERR_NONE;
      Finally
        FFFreeMem(aData, bcRecordLen);
        { Reset current position. }
        bcRestoreCurInfo;
      End;
    End
  Else
    Begin
      FFTblGetRecordInfo(bcTable.Files[0], bcDatabase.TransactionInfo, Info);
      aRecCount := Info.riRecCount;
    End;
End;
{--------}

Function TfsSrcSQLSimpleCursor.GetSetPosition(aValue: Longint; aData: PffByteArray; aLockType: TfsSrcLockType;
  Var aFlag: Byte; Var aRecNo: Longword;
  Var aRefNr: TffInt64;
  aInfoGetSetPosition: TInfoGetSetPosition;
  aSet: Boolean): TffResult;

  Function GotoRec(Var aRecCount: Longword): TffResult;
  Var
    aFlag: Byte;

  Begin
    Result := DBIERR_NONE;
    aRecCount := 0;
    If aValue < 0 Then
      If (bcInfo.pos = cpBOF) Then
        Begin
          Result := DBIERR_BOF;
          Exit;
        End;
    If aValue > 0 Then
      If (bcInfo.pos = cpEOF) Then
        Begin
          Result := DBIERR_EOF;
          Exit;
        End;

    bcSaveCurInfo;
    FFGetZeroMem(aData, bcRecordLen);
    Try

      While ((Result = DBIERR_NONE) And (aRecCount < Longword(abs(avalue) * 1))) Do
        Begin
          If aValue < 0 Then
            Result := GetPriorRecord(aData, ffsltNone, aFlag, aRefNr)
          Else
            Result := GetNextRecord(aData, ffsltNone, aFlag, aRefNr);

          If (Result = DBIERR_NONE) Then
            inc(aRecCount);
        End;
      Result := DBIERR_NONE;
      If (aRecCount > 0) Then
        Begin
          Result := bcTable.GetRecord(bcDatabase.TransactionInfo,
            bcDatabase.DatabaseID,
            CursorID,
            bcInfo.refNr, Nil, aLockType, tluDatabase, False, False, aflag);
        End;
    Finally
      aRefNr := bcInfo.refNr;
      If (aRecCount = 0) Or Not aSet Then
        Begin
          bcRestoreCurInfo;
        End;
      FFFreeMem(aData, bcRecordLen);
    End;

  End;
Begin
  If aInfoGetSetPosition = imMoveBy Then
    Result := GotoRec(aRecNo)
  Else // other - not yet
    Result := GotoRec(aRecNo);

End;

Function TfsSrcSQLSimpleCursor.GetRecordForKey(aDirectKey: boolean;
  aFieldCount: Integer;
  aPartialLen: Integer;
  aKeyData: PffByteArray;
  aData: PffByteArray;
  aFirstCall: Boolean): TffResult;
Begin
  Result := DBIERR_INVALIDINDEXCREATE;
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Function TfsSrcSQLSimpleCursor.InsertRecord(aData: PffByteArray; aLockType: TfsSrcLockType; aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Var
  NewRefNr: TffInt64;

Begin
  aRefNr.iLow := 0;
  aRefNr.iHigh := 0;
  { Notify extenders. }
  Result := NotifyExtenders(ffeaBeforeRecInsert, ffeaInsertRecFail);
  If Result = DBIERR_NONE Then
    Begin
      bcTable.Dictionary.FUserName := Self.Client.ClientName;
      Result := bcTable.InsertRecord(bcDatabase.TransactionInfo,
        CursorID, aData, aLockType, NewRefNr, AFlag);
      If (Result = DBIERR_NONE) Then
        Begin
          bcInfo.pos := cpOnRecord;
          bcInfo.refNr := NewRefNr;
          bcInfo.Deleted := False;
          bcInfo.KeyValid := True;
          aRefNr := bcInfo.refnr;
          { Notify extenders of successful insert. }
          NotifyExtenders(ffeaAfterRecInsert, ffeaNoAction);
        End
      Else
        Begin
          aRefNr.iLow := 0;
          aRefNr.iHigh := 0;
          { Notify extenders of failed insert. }
          NotifyExtenders(ffeaInsertRecFail, ffeaNoAction);
        End;
    End;
End;
{--------}

Function TfsSrcSQLSimpleCursor.InsertRecordNoDefault(aData: PffByteArray;
  aLockType: TfsSrcLockType; aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Var
  NewRefNr: TffInt64;

Begin
  aRefNr.iLow := 0;
  aRefNr.iHigh := 0;
  { Notify extenders. }
  Result := NotifyExtenders(ffeaBeforeRecInsert, ffeaInsertRecFail);
  If Result = DBIERR_NONE Then
    Begin
      Result := bcTable.InsertRecordNoDefault(bcDatabase.TransactionInfo, {!!.10}
        CursorID, aData, aLockType, NewRefNr, AFlag);
      If (Result = DBIERR_NONE) Then
        Begin
          bcInfo.pos := cpOnRecord;
          bcInfo.refNr := NewRefNr;
          bcInfo.Deleted := False;
          bcInfo.KeyValid := True;
          aRefNr := bcInfo.refnr;
          { Notify extenders of successful insert. }
          NotifyExtenders(ffeaAfterRecInsert, ffeaNoAction);
        End
      Else
        Begin
          aRefNr.iLow := 0;
          aRefNr.iHigh := 0;
          { Notify extenders of failed insert. }
          NotifyExtenders(ffeaInsertRecFail, ffeaNoAction);
        End;
    End;
End;
{--------}

Function TfsSrcSQLSimpleCursor.IsInRange(aKey: PffByteArray): Integer;
Begin
  { This class does not support ranges. }
  Result := 0;
  FSRaiseException(EfsException, fsStrResServer, fserrRangeNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Function TfsSrcSQLSimpleCursor.ModifyRecord(aData: PffByteArray; aRelLock: boolean; aUserLockType: TfsUserRecLocking; aFlag: Byte; aSet, Use:
  Boolean): TffResult;
Var
  aKeyChanged: Boolean; 
Begin
  Result := 0;
  If Use Then Exit;
  { Note: By this time, any other cursor deleting or modifying the record ahead
    of us has completed and has set bcInfo.Deleted.  We can be assured of this
    because TFSServer.RecordDelete calls Cursor.EnsureWritable(true) which
    obtains a lock on the record to be deleted.  We won't get that lock until
    the other cursor has finished. }

  { Has this record already been deleted? }
  If bcInfo.Deleted Then
    Begin
      { Yes. }
      Result := DBIERR_KEYORRECDELETED;
      Exit;
    End;

  { Are we on a record? }
  If (bcInfo.Pos <> cpOnRecord) Then
    Begin
      { No. }
      Case bcInfo.Pos Of
        cpBOF: Result := DBIERR_BOF;
        cpEOF: Result := DBIERR_EOF;
        Else
          Result := DBIERR_NOCURRREC;
      End;
      Exit;
    End;

  { Notify extenders. }
  Result := NotifyExtenders(ffeaBeforeRecUpdate, ffeaUpdateRecFail);
  If (Result = DBIERR_NONE) Then
    Begin
      bcTable.Dictionary.FUserName := Self.Client.ClientName;
      Result := bcTable.PutRecord(bcDatabase.TransactionInfo, CursorID, bcInfo.refNr,
        aData, aRelLock, tluDatabase, aKeyChanged); {!!.05}
      If (Result = DBIERR_NONE) Then
        Begin
          bcInfo.KeyValid := True;
          bcInfo.pos := cpOnRecord;
          { Notify extenders of successful update. }
          NotifyExtenders(ffeaAfterRecUpdate, ffeaNoAction);
        End
      Else
        { Notify extenders of failed update. }
        NotifyExtenders(ffeaUpdateRecFail, ffeaNoAction);
    End;
End;
{--------}

Procedure TfsSrcSQLSimpleCursor.Open(Const aTableName: TfsTableName;
  Const aIndexName: TffName;
  Const aIndexID: Longint;
  Const aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  aForServer: boolean;
  Const aExclContLock: Boolean; {!!.10}
  aAttribs: TffFileAttributes; SysOpen: Boolean);
Begin
  Inherited Open(aTableName, aIndexName, aIndexID, aOpenMode, aShareMode,
    aForServer, aExclContLock, aAttribs, True); {!!.10}
  SetToBegin;
End;
{--------}

Procedure TfsSrcSQLSimpleCursor.ResetRange;
Begin
  FSRaiseException(EfsException, fsStrResServer, fserrRangeNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Function TfsSrcSQLSimpleCursor.SetRange(aDirectKey: boolean;
  aFieldCount1: Integer;
  aPartialLen1: Integer;
  aKeyData1: PffByteArray;
  aKeyIncl1: boolean;
  aFieldCount2: Integer;
  aPartialLen2: Integer;
  aKeyData2: PffByteArray;
  aKeyIncl2: boolean): TffResult;
Begin
  Result := DBIERR_FS_RangeNotSupported;
  FSRaiseException(EfsException, fsStrResServer, fserrRangeNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Procedure TfsSrcSQLSimpleCursor.SetToBegin;
Begin
  bcInfo.Deleted := False;
  bcInfo.KeyValid := False;
  bcInfo.Pos := cpBOF;
  FFInitI64(bcInfo.RefNr);
End;
{--------}

Function TfsSrcSQLSimpleCursor.SetToBookmark(aBookmark: PffByteArray): TffResult;
Var
  aFlag: Byte;
Begin
  Result := CheckBookmark(aBookmark);
  If (Result = DBIERR_NONE) Then
    Begin
      { Initialize the key path. }
      FFInitKeyPath(bcInfo.KeyPath);
      With PfsSrBookmark(aBookmark)^ Do
        Begin
          bcInfo.Pos := sbPos;
          bcInfo.RefNr := sbRefNr;
          bcInfo.KeyValid := sbKeyValid;
          bcInfo.Deleted := False;

          { Does the record still exist? }
          Try
            bcTable.GetRecord(bcDatabase.TransactionInfo, {!!.10}
              bcDatabase.DatabaseID, {!!.10}
              CursorID, sbRefNr, {!!.10}
              bcRecordData, ffsltNone, tluDatabase, False, False, aFlag); {!!.02}
          Except
            On E: EfsException Do
              Begin
                If E.ErrorCode = fserrRecDeleted Then
                  Begin
                    bcInfo.Pos := cpOnCrack;
                    bcInfo.Deleted := True;
                  End
                Else
                  Begin
                    SetToBegin;
                    Result := DBIERR_INVALIDBOOKMARK;
                  End;
              End
            Else
              Begin
                SetToBegin;
                Result := DBIERR_INVALIDBOOKMARK;
              End;
          End;
        End; { with }
    End;
End;
{--------}

Function TfsSrcSQLSimpleCursor.SetToCursor(aCursor: TfsSrBaseCursor): TffResult;
Begin
  Result := DBIERR_NONE;
  If (aCursor.Table <> Table) Then
    Begin
      Result := DBIERR_DIFFERENTTABLES;
      Exit;
    End;
  bcInfo := aCursor.CursorInfo;
End;
{--------}

Procedure TfsSrcSQLSimpleCursor.SetToEnd;
Begin
  bcInfo.Pos := cpEOF;
  bcInfo.KeyValid := False;
  FFInitI64(bcInfo.refNr);
  bcInfo.Deleted := False;
End;
{--------}

Function TfsSrcSQLSimpleCursor.SetToKey(aSearchAction: TffSearchKeyAction;
  aDirectKey: boolean;
  aFieldCount: Integer;
  aPartialLen: Integer;
  aKeyData: PffByteArray): TffResult;
Begin
  { To set to a specific record, specify a value for the RefNr property. }
  Result := DBIERR_INVALIDINDEXCREATE;
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{--------}

Function TfsSrcSQLSimpleCursor.SwitchToIndex(aIndexID: Integer;
  aPosnOnRec: boolean): TffResult;
Begin
  Result := DBIERR_INVALIDINDEXCREATE;
  FSRaiseException(EfsException, fsStrResServer, fserrIndexNotSupported,
    [bcTable.BaseName, Self.ClassName]);
End;
{====================================================================}
End.

