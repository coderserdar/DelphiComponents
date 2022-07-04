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

{$I ffdefine.inc}

unit ffsrcur;

interface

uses
  ffllbase,
  fflldict,
  fflltemp,
  ffsrbase,
  ffsrbde,
  ffsreng,
  ffsrfltr,
  ffsrfold,
  ffsrlock,
  fftbdict,
  fftbindx;

type

  { This class manages a set of data stored in a logical table. It is very
    similar to TffSrTable. However, the data is not indexed.
    To create a new simple table, you must do the following:

    1. Call TffSrSimpleTable.Create.
    2. Have the table build its files by calling its BuildFiles method.
    3. Have the table open its fils by calling its OpenFiles method. }
  TffSrSimpleTable = class(TffSrBaseTable)
    public
      procedure AddIndex(const aIndexDesc : TffIndexDescriptor;
                               aTI : PffTransInfo); override;
      procedure BuildFiles(aTI         : PffTransInfo;
                           aForServer  : boolean;
                           aDictionary : TffDataDictionary;
                           aAttribs    : TffFileAttributes;
                           aStore      : TffBaseTempStorage); override;
      function BuildKeyForRecord(aIndexID    : integer;
                                 aData       : PffByteArray;
                                 aKey        : PffByteArray;
                                 aFieldCount : integer;
                                 aPartialLen : integer) : TffResult; override;
      function CompareKeysForCursor(var aKID  : TffKeyIndexData;
                                        aKey1 : PffByteArray;
                                        aKey2 : PffByteArray) : integer; override;
      function DeleteRecord(aTI           : PffTransInfo;
                      const aCursorID     : TffCursorID;
                      const aRefNr        : TffInt64;
                      const aLockObtained : Boolean;
                        var aBTreeChanged : Boolean)                   {!!.05}
                                          : TffResult; override;
      procedure DropIndex(aTI : PffTransInfo; aIndexID : Longint); override;
      function FindKey(var aKID        : TffKeyIndexData;
                       var aRefNr      : TffInt64;
                           aTI         : PffTransInfo;
                           aKey        : PffByteArray;
                       var aKeyPath    : TffKeyPath;
                           aAction     : TffSearchKeyAction) : boolean; override;
      function GetNextKey(var aKID       : TffKeyIndexData;
                          var aRefNr     : TffInt64;
                              aTI        : PffTransInfo;
                              aKey       : PffByteArray;
                          var aKeyPath   : TffKeyPath) : TffResult; override;
      function GetNextRecord(aTI        : PffTransInfo;
                       const aDatabaseID : TffDatabaseID;              {!!.10}
                       const aCursorID  : TffCursorID;                 {!!.10}
                         var aKID       : TffKeyIndexData;
                         var aRefNr     : TffInt64;
                             aKey       : PffByteArray;
                         var aKeyPath   : TffKeyPath;
                             aData      : PffByteArray;
                       const aLockType  : TffSrLockType) : TffResult; override; {!!.10}
      function GetPriorRecord(aTI        : PffTransInfo;
                        const aDatabaseID : TffDatabaseID;             {!!.10}
                        const aCursorID  : TffCursorID;                {!!.10}
                          var aKID       : TffKeyIndexData;
                          var aRefNr     : TffInt64;
                              aKey       : PffByteArray;
                          var aKeyPath   : TffKeyPath;
                              aData      : PffByteArray;
                        const aLockType  : TffSrLockType) : TffResult; override; {!!.10}
      function InsertRecord(aTI        : PffTransInfo;
                            aCursorID  : TffCursorID;
                            aData      : PffByteArray;
                            aLockType  : TffSrLockType;
                        var aNewRefNr  : TffInt64) : TffResult; override;
      function InsertRecordNoDefault(aTI        : PffTransInfo;        {!!.10}
                                     aCursorID  : TffCursorID;
                                     aData      : PffByteArray;
                                     aLockType  : TffSrLockType;
                                 var aNewRefNr  : TffInt64) : TffResult; override;
      procedure MakeKIDForCursor(aIndexID : integer; var aKID : TffKeyIndexData); override;
      procedure OpenFiles(aTI : PffTransInfo; aForServer : boolean;
                          aAttribs : TffFileAttributes); override;
      function PutRecord(aTI       : PffTransInfo;
                         aCursorID : TffCursorID;
                         aRefNr    : TffInt64;
                         aData     : PffByteArray;
                         aRelLock  : boolean;                          {!!.05}
                     var aKeyChanged : Boolean) : TffResult; override; {!!.05}

    end;

  { Use this cursor class to manage sets of data that do not require indices. }
  TffSrSimpleCursor = class(TffSrBaseCursor)
    protected
      procedure bcTableOpenPreconditions(aTable     : TffSrBaseTable;
                                  const aIndexName : string;
                                    var aIndexID   : Longint;
                                  const aOpenMode  : TffOpenMode); override;
        { Used by Create method to verify a thread may open a table. }

    public
      constructor Create(anEngine   : TffServerEngine;
                         aDatabase  : TffSrDatabase;
                   const aTimeout   : Longint); override;

      destructor Destroy; override;
      function AddIndexToTable(const aIndexDesc : TffIndexDescriptor) : TffResult; override;
      function CheckBookmark(aBookmark : PffByteArray) : TffResult; override;
      procedure ClearIndex; override;
      function CloneCursor(aOpenMode : TffOpenMode) : TffSrBaseCursor; override;
      function CompareBookmarks(aBookmark1, aBookmark2 : PffByteArray;
                             var CmpResult : Longint) : TffResult; override;
      function DropIndexFromTable(const aIndexName : TffDictItemName;
                                         aIndexID   : Longint) : TffResult; override;
      function ExtractKey(aData : PffByteArray; aKey : PffByteArray) : TffResult; override;
      function GetBookmark(aBookmark : PffByteArray) : TffResult; override;
      function GetBookmarkSize : integer; override;
      function GetNextRecord(aData : PffByteArray; aLockType : TffSrLockType) : TffResult; override;
      function GetPriorRecord(aData : PffByteArray; aLockType : TffSrLockType) : TffResult; override;
      function GetRecordCount(var aRecCount : Longint) : TffResult; override;
      function GetRecordForKey(aDirectKey  : boolean;
                                aFieldCount : integer;
                                aPartialLen : integer;
                                aKeyData    : PffByteArray;
                                aData       : PffByteArray;
                                aFirstCall  : Boolean) : TffResult; override;
      function InsertRecord(aData : PffByteArray; aLockType : TffSrLockType) : TffResult; override;
      function InsertRecordNoDefault(aData : PffByteArray; aLockType : TffSrLockType) : TffResult; override;{!!.10}
      function IsInRange(aKey : PffByteArray) : integer; override;
      function ModifyRecord(aData : PffByteArray; aRelLock : boolean) : TffResult; override;
      procedure Open(const aTableName : TffTableName;
                     const aIndexName : TffName;
                     const aIndexID   : Longint;
                     const aOpenMode  : TffOpenMode;
                           aShareMode : TffShareMode;
                           aForServer : boolean;
                     const aExclContLock : Boolean;                    {!!.10}
                           aAttribs   : TffFileAttributes); override;
      procedure ResetRange; override;
      function SetRange(aDirectKey : boolean;
                         aFieldCount1 : integer;
                         aPartialLen1 : integer;
                         aKeyData1    : PffByteArray;
                         aKeyIncl1    : boolean;
                         aFieldCount2 : integer;
                         aPartialLen2 : integer;
                         aKeyData2    : PffByteArray;
                         aKeyIncl2    : boolean) : TffResult; override;
      procedure SetToBegin;  override;
      function SetToBookmark(aBookmark : PffByteArray) : TffResult; override;
      function SetToCursor(aCursor : TffSrBaseCursor) : TffResult; override;
      procedure SetToEnd; override;
      function SetToKey(aSearchAction : TffSearchKeyAction;
                         aDirectKey    : boolean;
                         aFieldCount   : integer;
                         aPartialLen   : integer;
                         aKeyData      : PffByteArray) : TffResult; override;
      function SwitchToIndex(aIndexID   : integer;
                              aPosnOnRec : boolean) : TffResult; override;
  end;

  { Use this class to create a cursor representing a query's result set.
    Do not directly create TffSrSimpleCursor. }
  TffSrSQLResultSet = class(TffSrSimpleCursor);

implementation

uses
  ffconst,
  ffhash,
  fflleng,
  ffllexcp,
  fftbbase,
  fftbBLOB,                                                            {!!.11}
  fftbdata;

{===TffSrSQLTable====================================================}
procedure TffSrSimpleTable.AddIndex(const aIndexDesc : TffIndexDescriptor;
                                    aTI : PffTransInfo);
begin
  FFRaiseException(EffException, ffStrResServer, fferrIndexNotSupported,
                   [btBaseName^, Self.ClassName]);
end;
{--------}
procedure TffSrSimpleTable.BuildFiles(aTI         : PffTransInfo;
                                      aForServer  : boolean;
                                      aDictionary : TffDataDictionary;
                                      aAttribs    : TffFileAttributes;
                                      aStore      : TffBaseTempStorage);
var
  FileInx  : integer;
  DataFile : PffFileInfo;
  FileCnt  : integer; {dup for speed}
begin
  { Create the data file. }
  btFiles.Count := 1;
  btFiles[0] := FFAllocFileInfo(FFMakeFullFileName(Folder.Path, BaseName),
                               ffc_ExtForData, btBufMgr);

  { Validate the dictionary. }
  aDictionary.CheckValid;

  { Assimilate the dictionary. }
  btDictionary.ForceOffReadOnly;
  btDictionary.Assign(aDictionary);
  btDictionary.BindIndexHelpers;

  with PffFileInfo(btFiles[0])^ do begin
    fiAttributes := aAttribs;
    fiForServer := aForServer;
    fiEncrypted := btEngine.Configuration.GeneralInfo^.giAllowEncrypt and
                   aDictionary.IsEncrypted;
    fiRecLenPlusTrailer := btDictionary.RecordLength + SizeOf(Byte);
    fiRecordLength := btDictionary.RecordLength;
    fiTempStore := aStore;
  end;

  { Get the file count for this table (for speed reasons, etc). }
  FileCnt := Dictionary.FileCount;
  FileCount := FileCnt;

  { Get the data file for speed reasons. }
  DataFile := Files[0];

  { Scan through the secondary files. This table supports separate BLOB
    files but not separate index files. }
  for FileInx := 0 to pred(FileCnt) do begin
    if Dictionary.FileType[FileInx] = ftIndexFile then
      FFRaiseException(EffException, ffStrResServer, fferrIndexNotSupported,
                       [btBaseName^, Self.ClassName]);
    btCreateFile(FileInx, aTI, btDictionary.FileExt[FileInx], aForServer,
                 aAttribs, aStore);
  end;

  { Write the dictionary to the data file. }
  Dictionary.WriteToFile(DataFile, aTI);
{Begin !!.11}
  Files[Dictionary.BLOBFileNumber].fiBLOBrscMgr :=
    TffBaseBLOBResourceMgr.GetMgr(Files[Dictionary.BLOBFileNumber]);
  btBLOBEngine := TffBaseBLOBEngine.GetEngine(Files[Dictionary.BLOBFileNumber]);
{End !!.11}
  Files[Dictionary.BLOBFileNumber].fiMaxSegSize :=
    FFCalcMaxBLOBSegSize(Files[Dictionary.BLOBFileNumber]);

end;
{--------}
function TffSrSimpleTable.BuildKeyForRecord(aIndexID    : integer;
                                            aData       : PffByteArray;
                                            aKey        : PffByteArray;
                                            aFieldCount : integer;
                                            aPartialLen : integer) : TffResult;
begin
  Result := DBIERR_INVALIDINDEXCREATE;
end;
{--------}
function TffSrSimpleTable.CompareKeysForCursor(var aKID  : TffKeyIndexData;
                                               aKey1 : PffByteArray;
                                               aKey2 : PffByteArray) : integer;
begin
  Result := DBIERR_INVALIDINDEXCREATE;
end;
{--------}
function TffSrSimpleTable.DeleteRecord(aTI           : PffTransInfo;
                                 const aCursorID     : TffCursorID;
                                 const aRefNr        : TffInt64;
                                 const aLockObtained : Boolean;
                                   var aBTreeChanged : Boolean)        {!!.05}
                                                     : TffResult;
var
  OldData : PffByteArray;
  RecLen  : integer;
begin
  RecLen := btDictionary.RecordLength;
  FFGetMem(OldData, RecLen);
  Result := DBIERR_NONE;

  { If we have yet to lock the record then do so. }
  if (not aLockObtained) then
    FFAcqRecordLock(Files[0], aTI, aRefNr, ffsltExclusive,             {!!.10}
                    aTI^.tirTrans.DatabaseID,                          {!!.10}
                    aCursorID, false);                                 {!!.02}{!!.10}
    { Note: We leave all such locks active until the transaction is committed. }

  try
    FFTblReadRecord(Files[0], aTI, aRefNr, OldData);
    btDeleteBLOBsForRecord(aTI, OldData);
    FFTblDeleteRecord(Files[0], aTI, aRefNr);
  finally
    btInformCursors(aCursorID, roDelete, aRefNr, 0);
    FFFreeMem(OldData, RecLen);
  end;{try..finally}
end;
{--------}
procedure TffSrSimpleTable.DropIndex(aTI      : PffTransInfo;
                                     aIndexID : Longint);
begin
  FFRaiseException(EffException, ffStrResServer, fferrIndexNotSupported,
                   [btBaseName^, Self.ClassName]);
end;
{--------}
function TffSrSimpleTable.FindKey(var aKID        : TffKeyIndexData;
                                  var aRefNr      : TffInt64;
                                      aTI         : PffTransInfo;
                                      aKey        : PffByteArray;
                                  var aKeyPath    : TffKeyPath;
                                      aAction     : TffSearchKeyAction) : boolean;
begin
  ffInitI64(aRefNr);
  Result := False;
end;
{--------}
function TffSrSimpleTable.GetNextKey(var aKID       : TffKeyIndexData;
                                     var aRefNr     : TffInt64;
                                         aTI        : PffTransInfo;
                                         aKey       : PffByteArray;
                                     var aKeyPath   : TffKeyPath) : TffResult;
begin
  Result := DBIERR_INVALIDINDEXCREATE;
end;
{--------}
function TffSrSimpleTable.GetNextRecord(aTI        : PffTransInfo;
                                  const aDatabaseID : TffDatabaseID;   {!!.10}
                                  const aCursorID  : TffCursorID;      {!!.10}
                                    var aKID       : TffKeyIndexData;
                                    var aRefNr     : TffInt64;
                                        aKey       : PffByteArray;
                                    var aKeyPath   : TffKeyPath;
                                        aData      : PffByteArray;
                                  const aLockType  : TffSrLockType) : TffResult;
begin
  Result := DBIERR_NONE;
  GetNextRecordSeq(aTI, aRefNr, aData);
end;
{--------}
function TffSrSimpleTable.GetPriorRecord(aTI        : PffTransInfo;
                                   const aDatabaseID : TffDatabaseID;  {!!.10}
                                   const aCursorID  : TffCursorID;     {!!.10}
                                     var aKID       : TffKeyIndexData;
                                     var aRefNr     : TffInt64;
                                         aKey       : PffByteArray;
                                     var aKeyPath   : TffKeyPath;
                                         aData      : PffByteArray;
                                   const aLockType  : TffSrLockType) : TffResult; {!!.10}
begin
  Result := DBIERR_NONE;
  GetPrevRecordSeq(aTI, aRefNr, aData);
end;
{--------}
function TffSrSimpleTable.InsertRecord(aTI        : PffTransInfo;
                                       aCursorID  : TffCursorID;
                                       aData      : PffByteArray;
                                       aLockType  : TffSrLockType;
                                   var aNewRefNr  : TffInt64) : TffResult;
var
  RefNr : TffInt64;
begin
  RefNr.iLow := 0;
  RefNr.iHigh := 0;
  Result := DBIERR_NONE;
  if not Dictionary.CheckRequiredRecordFields(aData) then
    Result := DBIERR_REQDERR
  else begin
    {we need to add the default field values}
    if Dictionary.DefaultFieldCount > 0 then
      Dictionary.SetDefaultFieldValues(aData);

    { Updating the autoinc value obtains an exclusive lock on block 0 which
      prevents other cursors from inserting the same or additional records
      until we are done. }
    btUpdateAutoInc(aTI, aData);
    FFTblAddRecord(Files[0], aTI, RefNr, aData);
    aNewRefNr := RefNr;
    FFAcqRecordLock(Files[0], aTI, aNewRefNr, aLockType,               {!!.10}
                    aTI^.tirTrans.DatabaseID,                          {!!.10}
                    aCursorID, false);                                 {!!.02}{!!.10}
  end;
end;
{--------}
function TffSrSimpleTable.InsertRecordNoDefault(aTI        : PffTransInfo;{!!.10}
                                                aCursorID  : TffCursorID;
                                                aData      : PffByteArray;
                                                aLockType  : TffSrLockType;
                                            var aNewRefNr  : TffInt64) : TffResult;
var
  RefNr : TffInt64;
begin
  RefNr.iLow := 0;
  RefNr.iHigh := 0;
  Result := DBIERR_NONE;
  if not Dictionary.CheckRequiredRecordFields(aData) then
    Result := DBIERR_REQDERR
  else begin
    { Updating the autoinc value obtains an exclusive lock on block 0 which
      prevents other cursors from inserting the same or additional records
      until we are done. }
    btUpdateAutoInc(aTI, aData);
    FFTblAddRecord(Files[0], aTI, RefNr, aData);
    aNewRefNr := RefNr;
    FFAcqRecordLock(Files[0], aTI, aNewRefNr, aLockType,               {!!.10}
                    aTI^.tirTrans.DatabaseID,                          {!!.10}
                    aCursorID, false);                                 {!!.02}{!!.10}
  end;
end;
{--------}
procedure TffSrSimpleTable.MakeKIDForCursor(aIndexID : integer; var aKID : TffKeyIndexData);
begin
  FFRaiseException(EffException, ffStrResServer, fferrIndexNotSupported,
                   [btBaseName^, Self.ClassName]);
end;
{--------}
procedure TffSrSimpleTable.OpenFiles(aTI : PffTransInfo; aForServer : boolean;
                                     aAttribs : TffFileAttributes);
var
  FileInx  : integer;
begin
  { Are any of the files marked as index files? If so then raise an exception
    because this class does not support indexes. Assume that file 0 is the
    data file. }
  for FileInx := 1 to pred(Dictionary.FileCount) do
    if Dictionary.FileType[FileInx] = ftIndexFile then
      FFRaiseException(EffException, ffStrResServer, fferrIndexNotSupported,
                       [btBaseName^, Self.ClassName]);

  { If we've made it this far then open the files. }
  inherited OpenFiles(aTI, aForServer, aAttribs);
end;
{--------}
function TffSrSimpleTable.PutRecord(aTI       : PffTransInfo;
                                    aCursorID : TffCursorID;
                                    aRefNr    : TffInt64;
                                    aData     : PffByteArray;
                                    aRelLock  : boolean;                                   {!!.05}
                                var aKeyChanged : Boolean) : TffResult; {!!.05}
var
  RecLen : integer;
  OldData: PffByteArray;
begin

  { Assumption: By the time we have reached this point, the transaction has
    acquired a content lock on the table and we are the only ones who are
    going to be modifying the record. }

  aKeyChanged := False;                                                {!!.05}
  RecLen := 0;
  if not Dictionary.CheckRequiredRecordFields(aData) then begin
    Result := DBIERR_REQDERR;
    Exit;
  end;

  Result := DBIERR_NONE;
  try
    try
      RecLen := Dictionary.RecordLength;
      FFGetMem(OldData, RecLen);

      FFTblReadRecord(Files[0], aTI, aRefNr, OldData);

      { Acquire an exclusive lock. }
      FFAcqRecordLock(Files[0], aTI, aRefNr, ffsltExclusive,            {!!.10}
                      aTI^.tirTrans.DatabaseID,                         {!!.10}
                      aCursorID, false);                                {!!.02}{!!.10}

      FFTblUpdateRecord(Files[0], aTI, aRefNr, aData);
    except
      FFRelRecordLock(Files[0], aTI, aRefNr, aTI^.tirTrans.DatabaseID, {!!.10}
                      aCursorID);                                      {!!.10}
    end;
  finally
    FFFreeMem(OldData, RecLen);
  end;{try..finally}
end;
{====================================================================}

{===TffSrSimpleCursor================================================}
constructor TffSrSimpleCursor.Create(anEngine   : TffServerEngine;
                                     aDatabase  : TffSrDatabase;
                               const aTimeout   : Longint);
begin
  bcTableClass := TffSrSimpleTable;
  inherited Create(anEngine, aDatabase, aTimeout);
end;
{--------}
destructor TffSrSimpleCursor.Destroy;
begin
  { Free the table locks held by the cursor. }
  if Assigned(bcTable) then
    bcTable.RelLock(CursorID, True);

  inherited Destroy;
end;
{--------}
function TffSrSimpleCursor.AddIndexToTable(const aIndexDesc : TffIndexDescriptor) : TffResult;
begin
  Result := DBIERR_INVALIDINDEXCREATE;
  FFRaiseException(EffException, ffStrResServer, fferrIndexNotSupported,
                   [bcTable.BaseName, Self.ClassName]);
end;
{--------}
procedure TffSrSimpleCursor.bcTableOpenPreconditions(aTable     : TffSrBaseTable;
                                               const aIndexName : string;
                                                 var aIndexID   : Longint;
                                               const aOpenMode  : TffOpenMode);
begin
  { Ignore the index information. }

  { If the table's data file is open in read-only mode it means the
    physical file is read-only: hence this call's openmode must be
    read-only as well. }
  if (aTable.Files[0]^.fiOpenMode = omReadOnly) and
     (aOpenMode <> omReadOnly) then
    FFRaiseException(EffException, ffStrResServer, fferrCursorReadOnly,
                     [aTable.BaseName]);
end;
{--------}
function TffSrSimpleCursor.CheckBookmark(aBookmark : PffByteArray) : TffResult;
var
  CheckHash : Longint;
begin
  Result := DBIERR_INVALIDBOOKMARK;
  if (aBookmark = nil) then
    Exit;
  with PffSrBookmark(aBookmark)^ do begin
    CheckHash := FFCalcElfHash(sbIndexID,
                               ffcl_FixedBookmarkSize - SizeOf(sbHash));
    if (sbHash <> CheckHash) then
      Exit;
  end;
  Result := DBIERR_NONE;
end;
{--------}
procedure TffSrSimpleCursor.ClearIndex;
begin
  FFRaiseException(EffException, ffStrResServer, fferrIndexNotSupported,
                   [bcTable.BaseName, Self.ClassName]);
end;
{--------}
function TffSrSimpleCursor.CloneCursor(aOpenMode : TffOpenMode) : TffSrBaseCursor;
begin
  {NOTE: we are not checking rights for this action because the client
         had the rights to open the cursor}

  { Resolve the open mode. }
  if (bcOpenMode = omReadOnly) then
    aOpenMode := omReadOnly;

  AcqContentLock(ffclmRead);
  try
    { Create the cursor. }
    Result := TffSrSimpleCursor.Create(bcEngine, bcDatabase, soTimeout);
    Result.Open(bcTable.BaseName, '', bcIndexID, aOpenMode, smShared,
                bcTable.IsServerTable, False, bcTable.Files[0]^.fiAttributes);

    { Set up all of the misc fields. }
    Result.CursorInfo := bcInfo;
    if Assigned(bcFilter) then
      Result.SetFilter(bcFilter.Expression,bcFilter.Timeout);
  finally
    RelContentLock(ffclmRead);
  end;
end;
{--------}
function TffSrSimpleCursor.CompareBookmarks(aBookmark1, aBookmark2 : PffByteArray;
                                        var CmpResult : Longint) : TffResult;
var
  BM1 : PffSrBookmark absolute aBookmark1;
  BM2 : PffSrBookmark absolute aBookmark2;
begin
  Result := CheckBookmark(aBookmark1);
  if (Result = DBIERR_NONE) then
    Result := CheckBookmark(aBookmark2);
  if (Result <> DBIERR_NONE) then
    Exit;
  case BM1^.sbPos of
    cpUnknown : CmpResult := -1;
    cpBOF     : if (BM2^.sbPos = cpBOF) then
                  CmpResult := 0
                else
                  CmpResult := -1;
    cpEOF     : if (BM2^.sbPos = cpEOF) then
                  CmpResult := 0
                else
                  CmpResult := 1;
  else
    { Bookmark 1 is on a crack or on a record. }
    case BM2^.sbPos of
      cpUnknown : CmpResult := 1;
      cpBOF     : CmpResult := 1;
      cpEOF     : CmpResult := -1;
    else
      { Bookmark 2 is also on a crack or on a record.  Check the reference
        numbers.}
      CmpResult := ffCmpI64(BM1^.sbRefNr, BM2^.sbRefNr);
    end; {case}
  end; {case}
end;
{--------}
function TffSrSimpleCursor.DropIndexFromTable(const aIndexName : TffDictItemName;
                                   aIndexID   : Longint) : TffResult;
begin
  Result := DBIERR_INVALIDINDEXCREATE;
  FFRaiseException(EffException, ffStrResServer, fferrIndexNotSupported,
                   [bcTable.BaseName, Self.ClassName]);
end;
{--------}
function TffSrSimpleCursor.ExtractKey(aData : PffByteArray; aKey : PffByteArray) : TffResult;
begin
  Result := DBIERR_INVALIDINDEXCREATE;
  FFRaiseException(EffException, ffStrResServer, fferrIndexNotSupported,
                   [bcTable.BaseName, Self.ClassName]);
end;
{--------}
function TffSrSimpleCursor.GetBookmark(aBookmark : PffByteArray) : TffResult;
begin
  Result := DBIERR_NONE;
  AcqContentLock(ffclmRead);
  try
    FillChar(PffSrBookmark(aBookmark)^, ffcl_FixedBookmarkSize, 0);
    with PffSrBookmark(aBookmark)^ do begin
      sbRefNr := bcInfo.RefNr;
      sbPos := bcInfo.Pos;
      sbKeyValid := bcInfo.KeyValid;
      sbHash := FFCalcElfHash(sbIndexID, ffcl_FixedBookmarkSize - sizeof(sbHash));
    end;
  finally
    RelContentLock(ffclmRead);
  end;
end;
{--------}
function TffSrSimpleCursor.GetBookmarkSize : integer;
begin
  Result := ffcl_FixedBookmarkSize;
end;
{--------}
function TffSrSimpleCursor.GetNextRecord(aData : PffByteArray; aLockType : TffSrLockType) : TffResult;
begin

  { If we are at EOF, then obviously there's no next record. }
  if (bcInfo.pos = cpEOF) then begin
    Result := DBIERR_EOF;
    Exit;
  end;

  AcqContentLock(ffclmRead);
  try

    { Make sure that we have somewhere to read the record into. }
    if (aData = nil) then
      aData := bcRecordData;

    if Assigned(bcFilter) then
      bcFilter.BeginTimeout;

    Result := DBIERR_NONE;
    repeat
      bcTable.GetNextRecordSeq(bcDatabase.TransactionInfo, bcInfo.RefNr, aData);

      if ffI64IsZero(bcInfo.RefNr) then begin
        Result := DBIERR_EOF;
        SetToEnd;
        Exit;
      end;

      { In theory we're on a record. }
      bcInfo.Deleted := false;
      bcInfo.KeyValid := true;
      bcInfo.Pos := cpOnRecord;
    until (Result <> DBIERR_NONE) or not Assigned(bcFilter) or
          bcFilter.MatchesRecord(aData) or bcFilter.CheckTimeout(Result);

    { Place the lock if needed... record will not be read again. }
    if (Result = DBIERR_NONE) and (aLockType <> ffsltNone) then
      Result := Table.GetRecord(bcDatabase.TransactionInfo,
                                bcDatabase.DatabaseID, CursorID,       {!!.10}
                                bcInfo.RefNr, nil, aLockType, false, false);  {!!.02}
  finally
    RelContentLock(ffclmRead);
  end;
end;
{--------}
function TffSrSimpleCursor.GetPriorRecord(aData : PffByteArray; aLockType : TffSrLockType) : TffResult;
begin

  { If we are at BOF, then obviously there's no prior record. }
  if (bcInfo.pos = cpBOF) then begin
    Result := DBIERR_BOF;
    Exit;
  end;

  AcqContentLock(ffclmRead);
  try

    { Make sure that we have somewhere to read the record into. }
    if (aData = nil) then
      aData := bcRecordData;

    { Get the previous record. }
    if Assigned(bcFilter) then
      bcFilter.BeginTimeout;

    Result := DBIERR_NONE;
    repeat
      bcTable.GetPrevRecordSeq(bcDatabase.TransactionInfo, bcInfo.RefNr,
                              aData);
      if FFI64IsZero(bcInfo.RefNr) then begin
        Result := DBIERR_BOF;
        SetToBegin;
        Exit;
      end;

      { In theory we're on a record. }
      bcInfo.Deleted := false;
      bcInfo.KeyValid := true;
      bcInfo.Pos := cpOnRecord;

    until (Result <> DBIERR_NONE) or not Assigned(bcFilter) or
          bcFilter.MatchesRecord(aData) or bcFilter.CheckTimeout(Result);

    { Place the lock if needed... record will not be read again. }
    if (Result = DBIERR_NONE) and (aLockType <> ffsltNone) then
      Result := bcTable.GetRecord(bcDatabase.TransactionInfo,          {!!.10}
                                  bcDatabase.DatabaseID,               {!!.10}
                                  CursorID,                            {!!.10}
                                  bcInfo.refNr, nil, aLockType, false, false); {!!.02}
  finally
    RelContentLock(ffclmRead);
  end;

end;
{--------}
function TffSrSimpleCursor.GetRecordCount(var aRecCount : Longint) : TffResult;
var
  aData : PffByteArray;
  Info  : TffRecordInfo;
begin
  Result := DBIERR_NONE;
  AcqContentLock(ffclmRead);
  try
    { Is a filter active? }
    if Assigned(bcFilter) then begin
      { Yes. Set count to zero. We are going to scan through the records. }
      aRecCount := 0;
      { Save the current position. }
      bcSaveCurInfo;
      FFGetZeroMem(aData, bcRecordLen);
      try
        {BOF}
        SetToBegin;

        { While not EOF or other error do. }
        while (Result = DBIERR_NONE) do begin
          Result := GetNextRecord(aData, ffsltNone);
          if (Result = DBIERR_NONE) then
            inc(aRecCount);
        end;
        Result := DBIERR_NONE;
      finally
        FFFreeMem(aData, bcRecordLen);
        { Reset current position. }
        bcRestoreCurInfo;
      end;
    end
    else begin
      FFTblGetRecordInfo(bcTable.Files[0], bcDatabase.TransactionInfo, Info);
      aRecCount := Info.riRecCount;
    end;
  finally
    RelContentLock(ffclmRead);
  end;
end;
{--------}
function TffSrSimpleCursor.GetRecordForKey(aDirectKey  : boolean;
                                           aFieldCount : integer;
                                           aPartialLen : integer;
                                           aKeyData    : PffByteArray;
                                           aData       : PffByteArray;
                                           aFirstCall  : Boolean) : TffResult;
begin
  Result := DBIERR_INVALIDINDEXCREATE;
  FFRaiseException(EffException, ffStrResServer, fferrIndexNotSupported,
                   [bcTable.BaseName, Self.ClassName]);
end;
{--------}
function TffSrSimpleCursor.InsertRecord(aData : PffByteArray; aLockType : TffSrLockType) : TffResult;
var
  NewRefNr : TffInt64;
begin
  { Notify extenders. }
  Result := NotifyExtenders(ffeaBeforeRecInsert, ffeaInsertRecFail);

  if Result = DBIERR_NONE then begin
    AcqContentLock(ffclmWrite);
    Result := bcTable.InsertRecord(bcDatabase.TransactionInfo,
                                  CursorID, aData, aLockType, NewRefNr);
    if (Result = DBIERR_NONE) then begin
      bcInfo.pos := cpOnRecord;
      bcInfo.refNr := NewRefNr;
      bcInfo.Deleted := false;
      bcInfo.KeyValid := True;
      { Notify extenders of successful insert. }
      NotifyExtenders(ffeaAfterRecInsert, ffeaNoAction);
    end else
      { Notify extenders of failed insert. }
      NotifyExtenders(ffeaInsertRecFail, ffeaNoAction);
  end;
end;
{--------}
function TffSrSimpleCursor.InsertRecordNoDefault(aData : PffByteArray;{!!.10}
                                                 aLockType : TffSrLockType) : TffResult;
var
  NewRefNr : TffInt64;
begin
  { Notify extenders. }
  Result := NotifyExtenders(ffeaBeforeRecInsert, ffeaInsertRecFail);

  if Result = DBIERR_NONE then begin
    AcqContentLock(ffclmWrite);
    Result := bcTable.InsertRecordNoDefault(bcDatabase.TransactionInfo,{!!.10}
                                            CursorID, aData, aLockType, NewRefNr);
    if (Result = DBIERR_NONE) then begin
      bcInfo.pos := cpOnRecord;
      bcInfo.refNr := NewRefNr;
      bcInfo.Deleted := false;
      bcInfo.KeyValid := True;
      { Notify extenders of successful insert. }
      NotifyExtenders(ffeaAfterRecInsert, ffeaNoAction);
    end else
      { Notify extenders of failed insert. }
      NotifyExtenders(ffeaInsertRecFail, ffeaNoAction);
  end;
end;
{--------}
function TffSrSimpleCursor.IsInRange(aKey : PffByteArray) : integer;
begin
  { This class does not support ranges. }
  Result := 0;
  FFRaiseException(EffException, ffStrResServer, fferrRangeNotSupported,
                   [bcTable.BaseName, Self.ClassName]);
end;
{--------}
function TffSrSimpleCursor.ModifyRecord(aData : PffByteArray; aRelLock : boolean) : TffResult;
var                                                                    {!!.05}
  aKeyChanged: Boolean;                                                {!!.05}
begin

  { Note: By this time, any other cursor deleting or modifying the record ahead
    of us has completed and has set bcInfo.Deleted.  We can be assured of this
    because TffServerEngine.RecordDelete calls Cursor.EnsureWritable(true) which
    obtains a lock on the record to be deleted.  We won't get that lock until
    the other cursor has finished. }

  { Has this record already been deleted? }
  if bcInfo.Deleted then begin
    { Yes. }
    Result := DBIERR_KEYORRECDELETED;
    Exit;
  end;

  { Are we on a record? }
  if (bcInfo.Pos <> cpOnRecord) then begin
    { No. }
    case bcInfo.Pos of
      cpBOF :  Result := DBIERR_BOF;
      cpEOF :  Result := DBIERR_EOF;
    else
      Result := DBIERR_NOCURRREC;
    end;
    Exit;
  end;

  { Notify extenders. }
  Result := NotifyExtenders(ffeaBeforeRecUpdate, ffeaUpdateRecFail);
  if Result = DBIERR_NONE then begin
    AcqContentLock(ffclmWrite);
    Result := bcTable.PutRecord(bcDatabase.TransactionInfo, CursorID, bcInfo.refNr,
                               aData, aRelLock, aKeyChanged);          {!!.05}
    if (Result = DBIERR_NONE) then begin
      bcInfo.KeyValid := True;
      bcInfo.pos := cpOnRecord;
      { Notify extenders of successful update. }
      NotifyExtenders(ffeaAfterRecUpdate, ffeaNoAction);
    end else
      { Notify extenders of failed update. }
      NotifyExtenders(ffeaUpdateRecFail, ffeaNoAction);
  end;
end;
{--------}
procedure TffSrSimpleCursor.Open(const aTableName : TffTableName;
                                 const aIndexName : TffName;
                                 const aIndexID   : Longint;
                                 const aOpenMode  : TffOpenMode;
                                       aShareMode : TffShareMode;
                                       aForServer : boolean;
                                 const aExclContLock : Boolean;        {!!.10}
                                       aAttribs   : TffFileAttributes);
begin
  inherited Open(aTableName, aIndexName, aIndexID, aOpenMode, aShareMode,
                 aForServer, aExclContLock, aAttribs);                 {!!.10}
  SetToBegin;
end;
{--------}
procedure TffSrSimpleCursor.ResetRange;
begin
  FFRaiseException(EffException, ffStrResServer, fferrRangeNotSupported,
                   [bcTable.BaseName, Self.ClassName]);
end;
{--------}
function TffSrSimpleCursor.SetRange(aDirectKey : boolean;
                   aFieldCount1 : integer;
                   aPartialLen1 : integer;
                   aKeyData1    : PffByteArray;
                   aKeyIncl1    : boolean;
                   aFieldCount2 : integer;
                   aPartialLen2 : integer;
                   aKeyData2    : PffByteArray;
                   aKeyIncl2    : boolean) : TffResult;
begin
  Result := DBIERR_FF_RangeNotSupported;
  FFRaiseException(EffException, ffStrResServer, fferrRangeNotSupported,
                   [bcTable.BaseName, Self.ClassName]);
end;
{--------}
procedure TffSrSimpleCursor.SetToBegin;
begin
  AcqContentLock(ffclmRead);
  try
    bcInfo.Deleted := False;
    bcInfo.KeyValid := False;
    bcInfo.Pos := cpBOF;
    FFInitI64(bcInfo.RefNr);
  finally
    RelContentLock(ffclmRead);
  end;
end;
{--------}
function TffSrSimpleCursor.SetToBookmark(aBookmark : PffByteArray) : TffResult;
begin
  Result := CheckBookmark(aBookmark);
  if (Result = DBIERR_NONE) then begin
    AcqContentLock(ffclmRead);
    try
      { Initialize the key path. }
      FFInitKeyPath(bcInfo.KeyPath);
      with PffSrBookmark(aBookmark)^ do begin
        bcInfo.Pos := sbPos;
        bcInfo.RefNr := sbRefNr;
        bcInfo.KeyValid := sbKeyValid;
        bcInfo.Deleted := false;

        { Does the record still exist? }
        try
          bcTable.GetRecord(bcDatabase.TransactionInfo,                {!!.10}
                            bcDatabase.DatabaseID,                     {!!.10}
                            CursorID, sbRefNr,                         {!!.10}
                            bcRecordData, ffsltNone, false, false);    {!!.02}
        except
          on E:EffException do begin
            if E.ErrorCode = fferrRecDeleted then begin
              bcInfo.Pos := cpOnCrack;
              bcInfo.Deleted := True;
            end
            else begin
              SetToBegin;
              Result := DBIERR_INVALIDBOOKMARK;
            end;
          end
          else begin
            SetToBegin;
            Result := DBIERR_INVALIDBOOKMARK;
          end;
        end;
      end;  { with }
    finally
      RelContentLock(ffclmRead);
    end;
  end;
end;
{--------}
function TffSrSimpleCursor.SetToCursor(aCursor : TffSrBaseCursor) : TffResult;
begin
  Result := DBIERR_NONE;
  if (aCursor.Table <> Table) then begin
    Result := DBIERR_DIFFERENTTABLES;
    Exit;
  end;

  AcqContentLock(ffclmRead);
  try
    bcInfo := aCursor.CursorInfo;
  finally
    RelContentLock(ffclmRead);
  end;
end;
{--------}
procedure TffSrSimpleCursor.SetToEnd;
begin
  AcqContentLock(ffclmRead);
  try
    bcInfo.Pos := cpEOF;
    bcInfo.KeyValid := False;
    FFInitI64(bcInfo.refNr);
    bcInfo.Deleted := false;
  finally
    RelContentLock(ffclmRead);
  end;
end;
{--------}
function TffSrSimpleCursor.SetToKey(aSearchAction : TffSearchKeyAction;
                   aDirectKey    : boolean;
                   aFieldCount   : integer;
                   aPartialLen   : integer;
                   aKeyData      : PffByteArray) : TffResult;
begin
  { To set to a specific record, specify a value for the RefNr property. }
  Result := DBIERR_INVALIDINDEXCREATE;
  FFRaiseException(EffException, ffStrResServer, fferrIndexNotSupported,
                   [bcTable.BaseName, Self.ClassName]);
end;
{--------}
function TffSrSimpleCursor.SwitchToIndex(aIndexID   : integer;
                        aPosnOnRec : boolean) : TffResult;
begin
  Result := DBIERR_INVALIDINDEXCREATE;
  FFRaiseException(EffException, ffStrResServer, fferrIndexNotSupported,
                   [bcTable.BaseName, Self.ClassName]);
end;
{====================================================================}
end.
