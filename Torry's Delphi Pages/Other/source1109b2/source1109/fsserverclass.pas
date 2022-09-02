{*********************************************************}
{* FSSQL: Server Class Definitions                       *}
{* Programming: Krzysztof Winnicki                       *}
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
 * The next programing by Krzysztof Winnicki
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
{Notes:
   1. The perform dynamic link call has been commented out in the
      server engine create.

   2. Server-side objects are freed when a client requests the object be closed
      (e.g., SessionRemove) & all of its dependent objects report they be
      closed.  For example, a TfsSrcDatabase can be closed only if no other
      thread is using the TfsSrcDatabase and its associated cursors report they
      are inactive.

      If a server-side object cannot be freed when the close request is received
      from the client then the server's garbage collection thread will
      eventually free the object.

   3. When adding new TFSServer methods, please follow these guidelines:

      a. All steps should be wrapped with a Try..Except block.  At a minimum,
         the Try..Except block must do the following:

           try
             ...
           except
             on E : Exception do begin
               Result := ConvertServerException(E, btEngine.EventLog);
             end;
         end;

         This ensures the client is returned an error code that it
         understands.

      b. If you call any of the CheckxxxIDAndGet methods, the remaining
         code should be wrapped with a try..finally block.  The Finally
         section should call "xxxx.Deactivate".  Why?  Because the
         CheckxxxxIDAndGet methods mark the relevant object as Active to make
         sure it is not freed by another thread.  Once the operation has
         completed, the object must be marked as Inactive so that it may
         be closed and freed at a later time.

}

{$I FsDEFINE.INC}

{ Enable the following define to debug RAM pages. }
{.$DEFINE RAMPageCheck}

{ Enable the following to debug the deleted record count. }
{.$DEFINE DebugDelCount}

{ Diasable the following to retrieve files using DatabaseTableList that
  are not FSSQL  Tables. }
{$DEFINE OnlyRetrieveTables} {!!.01}

Unit fsserverclass;

Interface

Uses
  Windows,
  SysUtils,
  Classes,
  Forms,
  FsStDate,
  FsConst,
  FsLLBase,
  FsLLEng,
  FsLLDict,
  FsLLThrd,
  FsSrMgr,
  FsLLExcp,
  FsLLLog,
  FsLLProt,
  FsLLTemp,
  FsLLUNC,
  FsHash,
  FsNetMsg,
  FsSrBase,
  FsFile,
  FsSqlBas,
  FsSrIntf,
  FsSrBDE,
  FsSrCfg,
  FsSrFMap,
  FsSrIntm,
  FsSrStat,
  FsSrCvEx,
  FsSrFold,
  fsindexhelper,
  FsSrLock,
  FsSrTran,
  FsSrFltr,
  Fsconvff,
  fstablehelper,
  fsrecordaccess,
  fsblobaccess,
  fsdictserveraccess,
  fsindexaccess,
  fszlib,
  {$IFDEF DCC6OrLater}
  Variants,
  {$ENDIF}
  fsfunInterp;

{===Read/Write alias data from table=================================}
Const
  fsc_SavPrefix = 'SAV';
  fsc_StdPrefix = 'TAB';
  fsc_TmpPrefix = 'XXS';

  fsc_IndexSuffix = '0001';
  fsc_IndexTableName = 'TAB0002';
  fsc_SavedIndexTableName = 'SAV0002';
  fsc_TempIndexTableName = 'XXS0002';
  fsc_AliasScript = 'alias.fc$';
  fsc_ClientShutdownTime: TffWord32 = 10000;

  fsc_StartTranWithDelay: DWORD = 10;
  { Used with TransactionStartWith. If a lock cannot be immediately obtained
    then the operation will be retried every fsc_StartTranWithDelay
    milliseconds. }

Type
  PStDate = ^TStDate;
  PStTime = ^TStTime;

  TfsCursorPosition = ({Positions of a cursor in an index}
    cpUnknown, {..unknown: must be resolved asap}
    cpBOF, {..prior to first record}
    cpEOF, {..after last record}
    cpOnCrack, {..in between two records}
    cpOnRecord); {..on a record somewhere}

  TfsRecOp = ({Record update operations}
    roInsert, {..insertion}
    roDelete, {..deletion}
    roModify); {..modification}

Type
  PfsSrBookmark = ^TfsSrBookmark;
  TfsSrBookmark = Packed Record
    sbHash: Longint; {validity check}
    sbIndexID: Longint;
    sbPos: TfsCursorPosition;
    sbKeyValid: boolean;
    sbFill1: Array[0..1] Of Byte; {to DWORD align}
    sbRefNr: TffInt64;
    sbKeyLen: Longint;
    sbKey: Array[0..1] Of Byte;
  End;

Type
  TFSServer = Class; {forward declaration}
  TfsSrcTableClass = Class Of TfsSrcBaseTable; {forward declaration}
  TfsSrcBaseTable = Class; {forward declaration}
  TfsSrcDatabase = Class; {forward declaration}
  TfsSrcSession = Class; {forward declaration}
  TfsSrcClient = Class; {forward declaration}
  TfsSrcStmtList = Class; {forward declaration}
  TfsSrBaseCursor = Class; {forward declaration}
  TfsSrcCursorClass = Class Of TfsSrBaseCursor; {forward declaration}

  TCursorInterpretator = Class(TFunInterpretator)
  Public
    // source execute
    SrcCursor: TfsSrBaseCursor;
    Engine: TFSServer;
    ClientID: TffClientID;
    DatabaseID: TffDatabaseID;
    Timeout: Longint;
    OpenMode: TffOpenMode;
    fStmtID: TffSqlStmtID;

    CursorProcedure: TffCursorId;

    OldRecord: PffByteArray;
    NewRecord: PffByteArray;

    ListEventsTransaction: TFSSpecStringList;
    ListEventsGlobalTransaction: TFSSpecStringList;

    InternalCursorList, InternalFields: TIntVariables;

    Procedure ErrorString(Const S: String);
    Function CreateTemporaryTableWithoutIndex(CursorName: String): TffCursorId;
    Function iExecSQLGetNewOld(Field: String; iRecord: PffByteArray; OldRec: Boolean): Variant;
    Function iExecSQLGetNewOldSql(Field: String; iRecord: PffByteArray; OldRec: Boolean): Variant;
    Procedure iExecSQLSetNewOld(Field: String; Value: Variant; Var iRecord: PffByteArray);
    // for parser
    Function iExecSQL(CursorName, Sql: String): TffCursorId; Virtual;
    Function iExecModifySQL(Sql: String): TffResult; Virtual;
    Function iExecSQLClose(CursorName: String): boolean; Virtual;
    Function iExecSQLClearRow(CursorName: String): boolean; Virtual;
    Function iExecSQLSort(CursorName, FieldCsv, OrderCsv: String): Boolean; Virtual;
    Function iExecSQLTableName(CursorName: String): String; Virtual;
    Function iExecSQLNext(CursorName: String): Boolean; Virtual;
    Function iExecSQLPrior(CursorName: String): Boolean; Virtual;
    Function iExecSQLFirst(CursorName: String): Boolean; Virtual;
    Function iExecSQLLast(CursorName: String): Boolean; Virtual;
    Function iExecSQLEof(CursorName: String): boolean; Virtual;
    Function iExecSQLBof(CursorName: String): Boolean; Virtual;
    Function iExecSQLEmpty(CursorName: String): Boolean; Virtual;
    Function iExecSQLGetValue(CursorName, Field: String): Variant; Virtual;
    Function iExecSQLGetValueSql(CursorName, Field: String): Variant; Virtual;
    Function iExecSQLInsert(CursorName: String): Boolean; Virtual;
    Function iExecSQLDelete(CursorName: String): Boolean; Virtual;
    Function iExecSQLUpdate(CursorName, Field: String; Value: Variant): Boolean; Virtual;
    Function iExecSQLRecordCount(CursorName: String): Longword; Virtual;
    Function iExecSQLRollbackTransaction(CursorName: String): Boolean; Virtual;
    Function iExecSQLCommitTransaction(CursorName: String): Boolean; Virtual;
    Function iExecSQLStartTransaction(CursorName: String): Boolean; Virtual;
    Function iExecSQLInTransaction(CursorName: String): Boolean; Virtual;
    Function iExecSQLFieldExists(CursorName, Field: String): Boolean; Virtual;
    Function iExecSQLFieldType(CursorName, Field: String): Integer; Virtual;
    // Function iExecSQLGetOldValue(Field: String): Variant;  Virtual;
    // Procedure iExecSQLSetNewValue(Field: String; Value: Variant); Virtual;

    Constructor Create;
    Destructor Destroy; Override;
    Procedure Clear;
    Procedure GetValue(Const Name: String; Var Value: Variant); Override;
    Procedure SetValue(Const Name: String; Value: Variant); Override;
    Procedure GetSetValueFunction(Const Name: String; p1, p2, p3, p4, p5, p6, p7, p8, p9, p10: Variant;
      Var Value: Variant); Override;
  End;

  { This type identifies the state of a TfsServerObject.  Given the
    multi-threaded nature of the server engine, it is possible for thread A
    to be using an object while thread B processes a command that would result
    in the closing and freeing of the object.  For example, in thread A a
    cursor is waiting to obtain an exclusive page lock.  While the cursor
    is waiting, the client times out and issues a CloseCursor command to
    the server.  Thread B processes the CloseCursor command.  Thread B
    must see that the cursor is active and thread B must not free the cursor.
    Doing so would cause an access violation as soon as thread A tries to
    use the cursor once more. }
  TfsServerObjectState = (ffosInactive, ffosActive, ffosClosePending,
    ffosClosing);
  { ffosInactive - The object is not being used by a thread.
    ffosActive   - The object is being used by a thread.
    ffosClosePending - Thread A is using the object but thread B wants
      to free the object.  Thread A is responsible for freeing the object
      once it has finished its operation.
    ffosClosing - The object is being freed by a thread. }

{ Contains the essential properties and methods for a server object (e.g.,
  client, session, database, cursor).  Before a thread can use a server object
  it must call the Activate method.  If the object can be used then the
  Activate method returns True.

  When a thread has finished using a server object, it must call the
  Deactivate method.

  When a thread wants to close and free an object, it must call the
  Close method.  If the Close method returns True then the thread must
  call TfsServerObject.Free. }
  TfsServerObject = Class(TfsSelfListItem)
  Protected
    soClient: TfsSrcClient; {!!.10}
    { This is a reference to the server object's parent TfsSrcClient.
      It is instantiated for TfsSrcDatabase, TfsSrcBaseTable,
      and TfsSrBaseCursor. }
    soLock: TfsPadlock;
    { Padlock used to prevent re-entrancy on a per-client basis.
      This lock is instantiated only for TfsServerObjects of type
      TfsSrcClient. }
    soState: TfsServerObjectState;
    soTimeout: Longint;
  Public
    Constructor Create(Const aTimeout: Longint);
    Destructor Destroy; Override;

    Function Activate: boolean;
    { This method must be called before a thread can use a server object.
      If State is ffosInactive then sets State to ffosActive and returns
      True.  Otherwise returns False. }

    Function CanClose(Const Mark: boolean): boolean; Virtual;
    { When a server object is to be freed, call this method.  If the
      object can be freed this method returns True otherwise it returns
      False.  If the Mark parameter is True then the object's state is
      set to ffosClosing. }

    Procedure Deactivate;
    { When a thread has finished using a server object, it must call this
      method.
      If State is ffosShutdownPending then the object frees itself.
      If State is ffosActive then switches to ffosInactive.
      If State is ffosShuttingDown then does nothing with the assumption
      that another thread will finish the object's shutdown. }

    Procedure ForceClose; Virtual;
    { Sets the client's state to ffosClosing so that it will free itself
      when the server next requests the client to be removed. }

    Procedure RequestClose; Virtual; {!!.03}
    { If an object cannot be closed (i.e., CanClose returns False) then
      call this method to submit a request to close the object. }

    Function ShouldClose: boolean; Virtual;
    { When a server object is ready to be freed (i.e., State = ffosClosing),
      this method returns True. }

  { Properties }

    Property Client: TfsSrcClient Read soClient; {!!.10}
    { The object's parent client object. }

    Property State: TfsServerObjectState Read soState Write soState;
    { The current state of the object. }

    Property Timeout: Longint Read soTimeout Write soTimeout;
    { The object's timeout value. }
  End;

  { This is the base class for lists of TfsServerObjects. }
  TfsServerObjectList = Class(TFSSpecObject)
  Protected {private}
    solList: TFSSpecThreadList;
  Protected
  Public

    Constructor Create; Virtual; {!!.01}

    Destructor Destroy; Override;

    Procedure BeginRead;
    { A thread must call this method to gain read access to the list. }

    Procedure BeginWrite;
    { A thread must call this method to gain write access to the list. }

    Function CanClose(Const Mark: boolean): boolean; Virtual;
    { Used to determine if all the server objects in the list can be
      closed.  Returns True if all can be closed otherwise returns False. }

    Procedure EndRead;
    { A thread must call this method when it no longer needs read access
      to the list.  If it does not call this method, all writers will
      be perpetually blocked. }

    Procedure EndWrite;
    { A thread must call this method when it no longer needs write access
      to the list.  If it does not call this method, all readers and writers
      will be perpetualy blocked. }

    Procedure ForceClose; Virtual;
    { Use this method to force all objects within the list to set themselves
      to a ffosClosing state. }

{Begin !!.06}
    Function HasClosableState(Const Mark: Boolean): boolean;
    { Use this method to determine if objects have a closable state. Ignores
      all other facets of the object. If the Mark parameter is True and all
      objects in the list can be closed then sets all objects with state
      ffosInactive to ffosClosing. }
{End !!.06}

    Procedure RemoveUnused; Virtual;
    { Use this method to free objects that could not be freed at the time
      they were closed. }

{Begin !!.03}
    Procedure RequestClose; Virtual;
    { Use this method to request a close on all objects contained in the
      list. }
{End !!.03}

    Function ShouldClose: boolean; Virtual;
    { Use this method to determine if all the objects in the list should
      be closed. }

  End;

  TfsSrcCursorInfo = Packed Record
    Deleted: boolean;
    { If true then the record referenced by this information has been
      deleted. }
    KeyPath: TffKeyPath;
    {This is a trail into the current index that leads us to a
     specific record, crack between two records, EOF, or BOF}
    KeyValid: boolean;
    {This variable is set to True when we position to the
     next or previous record, reposition to an existing record,
     retrieve a record for a key, or position to a bookmark that is on a
     valid record.

     When this variable is True, we can rely upon the key stored in
     variable bcCurKey.

     This variable is set to False when we insert a record, modify a
     record, or otherwise need to force a recalculation of the key
     path to a record (e.g., TfsSrcCursor.SetToBegin,
     TfsSrcCursor.SwitchToIndex). }
    Pos: TfsCursorPosition;
    { This tells us whether the cursor is on a specific record, at BOF,
      at EOF, or on a crack between two records. }
    RefNr: TffInt64;
    { Reference number of the current record.  This is its physical position
      within the file.  For example, if RefNr = 128,556 then the record
      starts at position 128,556 within the data file. }
  End;

  TfsContentLockMode = (fsclmCommit, fsclmRead, fsclmWrite);
  { Used by cursor to indicate what type of content lock is needed. }

  TfsSrcCopyRecordsProc = Procedure(aSrcCursor: TfsSrBaseCursor;
    aSrcRecord: PffByteArray;
    aCookie1, aCookie2: Longint;
    Var include: boolean) Of Object;
  { Defines the event handler for the CopyRecords method.
    SrcCursor is the cursor from which the record is being copied.
    aSrcRecord is the record to be copied.
    Set include to True if the record is to be copied, otherwise set it to
    False. }

  {TfsRecordFilterAndRangeList = Class(TList)
  Private
    fCurrPos: Longint;
    Cursor: TffCursorID;
  Public
    Constructor Create(aCursor: TffCursorID);
    Function GetNext: TffResult;
    Function GetPrior: TffResult;
    Procedure SetToEnd;
    Procedure SetToBegin;
    Function Eof;
    Function Bof;

    Property CurrPos: Longint Read fCurrPos Write fCurrPos;
  End; }

  { Use the following type to describe how columns within a simple table should
    be sorted. }
  TfsOrderByDirection = (fsobAscending, fsobDescending);

  PfsOrderByArray = ^TfsOrderByArray;
  TfsOrderByArray = Array[0..fscl_MaxIndexFlds] Of TfsOrderByDirection;

  { Defines the standard interface for a cursor. Note that once you create a
    cursor, you must call its Open method to open a cursor for an existing
    table.  If the table does not yet exist, use the Build method to create
    the table and open the cursor on the new table. }
  TfsSrBaseCursor = Class(TfsServerObject)
  Protected {private}
    bcTableClass: TfsSrcTableClass;
    { The type of table to be created by the cursor.
      Be sure to initialize it to the appropriate value in the
      inherited constructors before calling one of the TfsSrBaseCursor
      constructors. }

    bcBLOBCursors: TFSNormalList; { List of cursors for which we have
    dereferenced BLOB links. }
    bcCloseTable: Boolean; { Set to True if the cursor is to close
    its table when the cursor is freed.
    Standard cursors leave the table open
    because other clients may need to access
    the same table. SQL cursors close the
    table right away because the result set
    is typically for only one client. }
    bcCloseWTrans: Boolean; {!!.05}
    bcDatabase: TfsSrcDatabase;
    bcEngine: TFSServer; {the engine with which this cursor is
    associated }
    bcExclOwner: Boolean; {If True then cursor has exclusively
    opened the table. }
    bcExtenders: TFSNormalList;
    {-List of engine extenders associated with this cursor. }

    bcIndexID: Longint;
    {Begin !!.03}
    bcLockedRefNum: TffInt64; { Last record locked via GetRecord
    method. The cursor tracks this to
    ensure that a record lock obtained
    via TfsTable.Edit, while an implicit
    transaction is in effect, will be
    unlocked if the client abruptly
    terminates. }
{End !!.03}
    bcNumReadLocks: Integer; { Number of open read locks.} {!!.05}
    bcTable: TfsSrcBaseTable;
    bcTempStore: TfsBaseTempStorage;

    bcKID: TffKeyIndexData; {work field for index access}
    bcCompareData: TffCompareData; {ditto}
    bcCurKey: PffByteArray; {current key}
    bcFilter: TfsSrcFilter; {filter object}
    bcFilterSav: TfsSrcFilter; {overridden filter}
    bcHasRange: Boolean; {whether range is active}
    bcInfo: TfsSrcCursorInfo; {the cursor's current position, key path,
    reference number, etc. }
{Begin !!.06}
    bcInfoLock: TfsPadlock; {Used to prevent transaction from
    clearing a cursor's key path while the
    cursor is navigating to next or prev
    record. }
{End !!.06}
    bcOpenMode: TffOpenMode;
    bcRecordData: PffByteArray; {work record data area}
    bcRecordLen: Integer; {record length}
    bcRng1Valid: Boolean; {is low range point valid?}
    bcRng2Valid: Boolean; {is high range point valid?}
    bcRng1Key: PffByteArray; {range start key}
    bcRng2Key: PffByteArray; {range end key}
    bcRng1FldCnt: Integer; {range start field count}
    bcRng2FldCnt: Integer; {range end field count}
    bcRng1PtlLen: Integer; {range start partial length}
    bcRng2PtlLen: Integer; {range end partial length}
    bcRng1Incl: Boolean; {range includes start key}
    bcRng2Incl: Boolean; {range includes end key}
    bcSavedInfo: TfsSrcCursorInfo; {temporary work area for bcSaveCurInfo &
    bcRestoreCurInfo }

    bcNewRecBuff: PffByteArray; { exclusively used by extenders }
    bcOldRecBuff: PffByteArray; { exclusively used by extenders }

    bcNeedNestedTransaction: Boolean; {If set to true all operations on the
    cursor use a nested transaction if needed}
    bcInterpretator: TCursorInterpretator;

    Procedure bcAddExtender(anExtender: TFSBaseEngineExtender);
    {-Use this method to add an extender to the list of extenders
      interested in a cursor. }

    Function bcBLOBCopy(aSrcCursor: TfsSrBaseCursor;
      Const aBLOBNr: TffInt64;
      Var aDestBLOBNr: TffInt64): TffResult;
    { Used to copy a BLOB from one cursor to another. }

    Function bcBLOBLinkGetLength(Const aTableName: TfsTableName;
      Const aBLOBNr: TffInt64;
      Var aLength: Longint): TffResult; Virtual;
    {-Used to obtain the length of a BLOB referenced by a BLOB link within
      a record of this cursor's result set. }

    Function bcBLOBLinkRead(Const aTableName: TfsTableName;
      Const aBLOBNr: TffInt64;
      Const aOffset: TffWord32; {!!.06}
      Const aLen: TffWord32; {!!.06}
      Var aBLOB;
      Var aBytesRead: TffWord32) {!!.06}
    : TffResult;
    {-Used to read a BLOB referenced by a BLOB link within a record of this
      cursor's result set. }

    Function bcCheckExclusiveReadWrite: TffResult; Virtual;
    {-Verifies the cursor has exclusive read-write access to the table. }

    Function bcFindBLOBCursor(Const aTableName: TfsTableName): TfsSrBaseCursor; Virtual;
    {-Finds a BLOB cursor based upon a table name. }

    Function bcGetAttribs: TffFileAttributes; Virtual;

    Function bcGetCursorID: TffCursorID; Virtual;

    Function bcGetPosition: TfsCursorPosition;

    Function bcGetRefNr: TffInt64;

    Procedure bcInit(Const aOpenMode: TffOpenMode;
      Const aShareMode: TffShareMode;
      Const aExclContLock: Boolean); Virtual; {!!.10}
    {-Called from a cursor constructor. Performs misc. initializations. }

    Procedure bcInvalidateCurKey;
    Function bcIsCurKeyPathValid: boolean;
    Function bcIsCurKeyValid: boolean;
    Procedure bcRebuildKeyPath; {!!.05 - Moved from TfsSrcCursor.scRebuildKeyPath}
    { If the cursor has a valid key, this method rebuilds the cursor's key
      path. }

    Procedure bcTableOpenPreconditions(aTable: TfsSrcBaseTable;
      Const aIndexName: String;
      Var aIndexID: Longint;
      Const aOpenMode: TffOpenMode); Virtual; Abstract;
    { Used by Create method to verify a thread may open a table. }

    Procedure bcTableOpenPrim(aDatabase: TfsSrcDatabase;
      Const aTableName: TfsTableName;
      Const aOpenMode: TffOpenMode;
      Const aShareMode: TffShareMode;
      Const aForServer: boolean;
      Const aAttribs: TffFileAttributes); Virtual;
    { Primitive engine method for opening a table. }

    Procedure bcRecordUpdated(aOp: TfsRecOp;
      aRefNr: TffInt64;
      aIndexID: Integer); Virtual;
    { Called when another cursor has updated a record in the same
      table.  Gives this cursor a chance to update its internal
      information (e.g., whether or not the current record has been
      deleted, key path status). }

    Procedure bcRestoreCurInfo; Virtual;
    { Restore the cursor's position, reference number, key, etc.
      as saved via scSaveCurValues. }

    Procedure bcSaveCurInfo; Virtual;
    { Save the cursor's current position, reference number, key, etc. }

    Function bcGetDictionary: TFSInfoDict; Virtual;

  Public
    SqlTableProxy: TObject; // TFFSqlTableProxy;
    TableName: TfsTableName;
    Constructor Create(anEngine: TFSServer;
      aDatabase: TfsSrcDatabase;
      Const aTimeout: Longint); Virtual;

    Destructor Destroy; Override;

    {Begin !!.10}
    Procedure AcqContentLock(Const aMode: TfsContentLockMode; Const aUserLockType: TfsUserRecLocking = tluDatabase); Virtual;
    { Acquire unconditional content lock. }
    Function AcqExclContentLock: TffResult; Virtual;
    { Acquire conditional content lock (i.e., the lock is obtained only if
      it can be immediately granted). }
{End !!.10}

    { Used by threads to obtain a content lock. }
    Function AddIndexToTable(Const aIndexDesc: TffIndexDescriptor): TffResult; Virtual; Abstract;
    Procedure AppendNewRecord(aData: PffByteArray); Virtual;

    { BLOB methods }
    Function BLOBAdd(Var aBLOBNr: TffInt64): TffResult; Virtual;

    Function BLOBLinkAdd(Const aTableName: TfsTableName;
      Const aTableBLOBNr: TffInt64;
      Var aBLOBNr: TffInt64): TffResult; Virtual;
    { Adds a link to a BLOB in another table to the cursor's table. }

    Procedure Build(Const aTableName: TfsTableName;
      aDict: TFSInfoDict;
      Const aOpenMode: TffOpenMode;
      aShareMode: TffShareMode;
      aForServer: boolean;
      aOverWrite: boolean;
      aAttribs: TffFileAttributes;
      aStoreSize: TffWord32); Virtual;
    { Use this method to open a cursor for a table that does not
      yet exist. This method uses aDict to create the table. This method
      then opens the cursor and positions to the Sequential Access Index
      (i.e., index 0). }

    Function CanClose(Const Mark: boolean): boolean; Override; {New !!.01}
    { A cursor can close if it is not active & is not involved in a
      transaction. }

    Function FileBLOBAdd(Const aFileName: TffFullFileName;
      Var aBLOBNr: TffInt64): TffResult; Virtual;

    Function BLOBDelete(Const aBLOBNr: TffInt64): TffResult; Virtual;

    Function BLOBFree(aBLOBNr: TffInt64): TffResult; Virtual;

    Function BLOBGetLength(aBLOBNr: TffInt64;
      Var aFBError: TffResult): Longint; Virtual;
    Function IsDeletedBLOB(aBLOBNr: TffInt64;
      Var aFBError: TffResult): Boolean; Virtual;

    Function BLOBIsLink(aBLOBNr: TffInt64; {!!.11 - New}
      Var aSrcTableName: TfsTableName;
      Var aSrcTableBLOBNr: TffInt64)
      : Boolean;

    {Begin !!.03}
    Function BLOBListSegments(aBLOBNr: TffInt64;
      aStream: TStream): TffResult; Virtual;
    {End !!.03}
    Function BLOBRead(aBLOBNr: TffInt64;
      aOffset: TffWord32; {!!.06}
      aLen: TffWord32; {!!.06}
      Var aBLOB;
      Var aBytesRead: TffWord32) {!!.06}
    : TffResult; Virtual;

    Function BLOBTruncate(aBLOBNr: TffInt64;
      aLen: TffWord32): TffResult; Virtual;

    Function BLOBWrite(Const aBLOBNr: TffInt64;
      aOffset: TffWord32;
      aLen: TffWord32;
      Var aBLOB): TffResult; Virtual;

    Function CheckBookmark(aBookmark: PffByteArray): TffResult; Virtual; Abstract;
    Procedure ClearIndex; Virtual; Abstract;
    Function CloneCursor(aOpenMode: TffOpenMode): TfsSrBaseCursor; Virtual; Abstract;
    Function CompareBookmarks(aBookmark1, aBookmark2: PffByteArray;
      Var CmpResult: Longint): TffResult; Virtual; Abstract;
    Function CopyRecords(aSrcCursor: TfsSrBaseCursor; aBLOBCopyMode: TffBLOBCopyMode;
      aCallback: TfsSrcCopyRecordsProc;
      aCookie1, aCookie2, CountPerTrans: Longint): TffResult; Virtual;
    { Use this method to copy all records from a source cursor to this
      cursor. Copies only those records matching the range and/or filter
      applied to the source cursor.

      Requirement: The source and destination cursors must have compatible
      dictionaries. The dictionaries must have the same field order, field
      type, length, units, and decimal places.

      If a record contains BLOBs, they are handled based upon the
      aBLOBCopyMode parameter. If mode is ffbcmNoCopy then the BLOB fields
      are set to NULL in the destination record. If mode is ffbcmCopyFull
      then the BLOBs are copied wholesale to the destination cursor.
      If mode is ffbcmCreateLink then the destination cursor is given a
      link to the BLOB in the source cursor.

      Use aCallback to have a validation routine called for each r4ecord
      that is copied. The validation routine has the opportunity to
      inspect the record and tell this routine whether or not to copy the
      record. }

    Function CopyRecordParts(aSrcCursor: TfsSrBaseCursor;
      aFields: PffLongintArray;
      aNumFields: Integer;
      aBLOBCopyMode: TffBLOBCopyMode;
      aCallback: TfsSrcCopyRecordsProc;
      aCookie1, aCookie2, CountPerTrans: Longint): TffResult; Virtual;
    { Similar to the CopyRecords method except this method allows you to
      copy specific fields from the source cursor. aFields identifies the
      fields to be copied. Each element of aFields is a field number in
      the source cursor's dictionary (base zero). The fields are copied in
      the order specified.

      The destination cursor's dictionary must have fields that match the
      specified fields in the source dictionary except that they must be
      in the order specified by aFields.
    }
    Function DeleteRecord(aData: PffByteArray): TffResult; Virtual;
    {Begin !!.06}
    Function DeleteRecords(CountPerTrans: Longint): TffResult; Virtual;
    { Delete all records in the cursor's result set, taking into account
      the active filter and/or range. }
{End !!.06}
    Function DropIndexFromTable(Const aIndexName: TffDictItemName;
      aIndexID: Longint): TffResult; Virtual; Abstract;

    Function Empty: TffResult; Virtual;
    Function EnsureWritable(aCheckCurRec, aConditionalLock: boolean; aUserLockType: TfsUserRecLocking): TffResult; Virtual;
    { Ensures the cursor is writable.  If aCheckCurRec is true, this method
     attempts to obtain an Exclusive, Commit duration lock on the
     record. If aConditionalLock is also True then the method succeeds only
     if it is able to immediately obtain the Exclusive lock. }
    Function ExtractKey(aData: PffByteArray; aKey: PffByteArray): TffResult; Virtual; Abstract;
    Function GetBookmark(aBookmark: PffByteArray): TffResult; Virtual; Abstract;
    Function GetBookmarkSize: Integer; Virtual; Abstract;
    Function GetRecord(aData: PffByteArray; aLockType: TfsSrcLockType; aUserLockType: TfsUserRecLocking;
      Var aFlag: Byte; Var aRefNr: TffInt64; Const aUser: Boolean):
      TffResult; Virtual;
    Function GetRecordCount(Var aRecCount: Longword): TffResult; Virtual; Abstract;
    Function GetNextRecord(aData: PffByteArray; aLockType: TfsSrcLockType; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult; Virtual; Abstract;
    Function GetPriorRecord(aData: PffByteArray; aLockType: TfsSrcLockType; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult; Virtual; Abstract;
    Function GetRecordField(aField: Integer;
      aRecordBuffer: PffByteArray;
      Var isNull: boolean;
      aFieldBuffer: pointer): TffResult; Virtual;
    { Obtain the value of a field. }
    Function GetRecordForKey(aDirectKey: boolean;
      aFieldCount: Integer;
      aPartialLen: Integer;
      aKeyData: PffByteArray;
      aData: PffByteArray;
      aFirstCall: Boolean): TffResult; Virtual; Abstract;

    Function HasRecordLocks: Boolean; Virtual;
    { Returns True if there are any record locks on the table. }
    Function RecordCountLocks: TffWord32; Virtual;
    { Returns record count locks on the table. }
    Procedure AssignRecordCountLocks(aList: TList); Virtual;
    { Assign record refnr locks on the table. }

    Function GetSetPosition(aValue: Longint; aData: PffByteArray; aLockType: TfsSrcLockType;
      Var aFlag: Byte; Var aRecNo: Longword;
      Var aRefNr: TffInt64;
      aInfoGetSetPosition: TInfoGetSetPosition;
      aSet: Boolean): TffResult; Virtual; Abstract;
    Function UndeleteRecord(aData: PffByteArray; aLockType: TfsSrcLockType; Var aRefNr: TffInt64): TffResult; Virtual; Abstract;
    Function InsertRecord(aData: PffByteArray; aLockType: TfsSrcLockType; aFlag: Byte; Var aRefNr: TffInt64): TffResult; Virtual; Abstract;
    Function InsertRecordNoDefault(aData: PffByteArray; aLockType: TfsSrcLockType; aFlag: Byte; Var aRefNr: TffInt64): TffResult; Virtual; Abstract;
    {!!.10}
    Function IsInRange(aKey: PffByteArray): Integer; Virtual; Abstract;
    Function IsRecordLocked(aLockType: TfsSrcLockType): Boolean; Virtual;
    Function WhoRecordLocked(aLockType: TfsSrcLockType): String; Virtual;
    {Begin !!.03}
    Procedure ListBLOBFreeSpace(aTI: PffTransInfo;
      Const aInMemory: Boolean;
      aStream: TStream);
    {End !!.03}
    Function OverrideFilter(aExpression: pCANExpr;
      aTimeout: TffWord32): TffResult; Virtual;
    Function ModifyRecord(aData: PffByteArray; aRelLock: Boolean; aUserLockType: TfsUserRecLocking; aFlag: Byte; aSet, Use: Boolean)
      : TffResult; Virtual; Abstract;
    Function NotifyExtenders(Const anAction: TffEngineAction;
      Const aFailAction: TffEngineAction)
      : TffResult;
    {-Notifies all extenders associated with the cursor about the
      specified action.  If ignoreErrCode is True then error codes
      returned by extenders are ignored.  If failures occur it will
      be taken care of before going back to the calling method.}
    Procedure Open(Const aTableName: TfsTableName;
      Const aIndexName: TffName;
      Const aIndexID: Longint;
      Const aOpenMode: TffOpenMode;
      aShareMode: TffShareMode;
      aForServer: Boolean;
      Const aExclContLock: Boolean; {!!.10}
      aAttribs: TffFileAttributes; SysOpen: Boolean); Virtual;
    { Use this method to open a cursor for a table that exists. }

    Procedure ReadAutoInc(Var aValue: Int64; Var aStep: Longint); Virtual;
    Procedure ReadMaxRecords(Var aValue: Longword); Virtual;
    Procedure ReadTableFlags(Var aValue: Word); Virtual;
    Procedure ReadTablePassword(Var aValue: Longword); Virtual;
    Procedure ReadTablePasswordRest(Var aValue: Longword); Virtual;
    Procedure ReadTableDBID(Var aValue: Longword); Virtual;
    Procedure RelContentLock(aMode: TfsContentLockMode); Virtual;
    Procedure RelRecordLock(aAllLocks: Boolean); Virtual;
    Procedure RelTableLock(aAllLocks: Boolean); Virtual;
    Procedure RemoveIfUnused; Virtual; {!!.05}
    Procedure ResetRange; Virtual; Abstract;
    Function RestoreFilter: TffResult; Virtual;
    Procedure SetAutoInc(aValue: Int64; aStep: Longint); Virtual;

    Function NextAutoInc: Int64; Virtual; // next value
    Function ReadLastAutoInc: Int64; Virtual; // Last value

    Procedure SetMaxRecords(aValue: Longword); Virtual;
    Procedure SetTableFlags(aValue: Word); Virtual;
    Procedure SetTablePassword(aValue: Longword); Virtual;
    Procedure SetTablePasswordRest(aValue: Longword); Virtual;
    Procedure SetTableDBID(aValue: Longword); Virtual;
    Function SetFilter(aExpression: pCANExpr;
      aTimeout: TffWord32): TffResult; Virtual;
    Function SetRange(aDirectKey: Boolean;
      aFieldCount1: Integer;
      aPartialLen1: Integer;
      aKeyData1: PffByteArray;
      aKeyIncl1: Boolean;
      aFieldCount2: Integer;
      aPartialLen2: Integer;
      aKeyData2: PffByteArray;
      aKeyIncl2: Boolean): TffResult; Virtual; Abstract;
    Procedure SetToBegin; Virtual; Abstract;
    Function SetToBookmark(aBookmark: PffByteArray): TffResult; Virtual; Abstract;
    Function SetToCursor(aCursor: TfsSrBaseCursor): TffResult; Virtual; Abstract;
    Procedure SetToEnd; Virtual; Abstract;
    Function SetToKey(aSearchAction: TffSearchKeyAction;
      aDirectKey: boolean;
      aFieldCount: Integer;
      aPartialLen: Integer;
      aKeyData: PffByteArray): TffResult; Virtual; Abstract;
    Function ShouldClose: boolean; Override; {New !!.01}
    { A cursor can close if it is not involved in a transaction. }
    Function SortRecords(aFieldsArray: TffFieldList;
      Const aOrderByArray: TfsOrderByArray;
      Const aNumFields: Integer): TffResult; Virtual;
    { Use this method to physically sort the records within a table.
      Parameters:
        aFieldsArray - Array of field numbers on which the table is being
                       sorted. Field numbers correspond to the fields in
                       the table's dictionary. Each element in this array
                       must have a corresponding element in aOrderByArray.
        aOrderByArray - Array of order by indicators, one for each field on
                        which the table is being sorted. Each element in
                        this array has a corresponding element in
                        aFieldsArray.
        aNumFields - The number of fields on which the table is being
                     sorted.
    }
    Function SwitchToIndex(aIndexID: Integer;
      aPosnOnRec: boolean): TffResult; Virtual; Abstract;

    Function ExecScript(cScript: TStringList; Var OldRecord: PffByteArray; Var NewRecord: PffByteArray;
      ListEventsTransaction, ListEventsGlobalTransaction: TfsSrcTransaction): TffResult;
    Function ExecProcedure(ProcName, ProcParam: String; Var CursorID: TffCursorID): TffResult;
    { Properties }
    Property Attribs: TffFileAttributes Read bcGetAttribs;
    { Returns the file attributes attached to the table's data file. }
    Property CloseTable: boolean Read bcCloseTable Write bcCloseTable;
    { Set this property to True if the cursor is to close its table when
      the cursor is freed. This is useful for SQL cursors which generate
      temporary tables applicable to only one client. }
    Property CursorID: TffCursorID Read bcGetCursorID;
    Property CursorInfo: TfsSrcCursorInfo Read bcInfo Write bcInfo;
    Property Database: TfsSrcDatabase Read bcDatabase;
    Property Dictionary: TFSInfoDict Read bcGetDictionary;
    Property Engine: TFSServer Read bcEngine;
    Property ExclOwner: boolean Read bcExclOwner Write bcExclOwner;
    Property Extenders: TFSNormalList Read bcExtenders; {!!.02}
    Property Filter: TfsSrcFilter Read bcFilter;
    Property IndexID: Longint Read bcIndexID;
    Property Position: TfsCursorPosition Read bcGetPosition;
    Property RefNr: TffInt64 Read bcGetRefNr;
    { Returns the reference number of the current record. }
//    property ServerEngine : TFSServer read bcEngine;           {Deleted !!.03}
    Property Table: TfsSrcBaseTable Read bcTable;

    { Used exclusively by extenders, these might not reflect actual values }
    Property NewRecordBuffer: PffByteArray Read bcNewRecBuff;
    Property OldRecordBuffer: PffByteArray Read bcOldRecBuff;

    Property NeedNestedTransaction: Boolean {!!.03}
    Read bcNeedNestedTransaction {!!.03}
    Write bcNeedNestedTransaction; {!!.03}
    Property Interpretator: TCursorInterpretator Read bcInterpretator Write bcInterpretator;
  End;

  TfsSrcCursor = Class(TfsSrBaseCursor)
  Protected {private}
    scKeyLen: Integer; {key length for cursor's index}
  Protected

    Procedure bcInit(Const aOpenMode: TffOpenMode;
      Const aShareMode: TffShareMode;
      Const aExclContLock: Boolean); Override; {!!.10}
    Procedure bcTableOpenPreconditions(aTable: TfsSrcBaseTable;
      Const aIndexName: String;
      Var aIndexID: Longint;
      Const aOpenMode: TffOpenMode); Override;
    { Used by Create method to verify a thread may open a table. }

    Procedure scRebuildCurKey(aRecData: PffByteArray;
      aLockObtained: boolean);
    { Rebuilds the cursor's key from the specified record buffer.  If
      aRecData is nil then this method reads the record from the data file
      & rebuilds the key from the retrieved record.
      If you have already obtained a lock on the current record, set
      aLockObtained := True.  Doing so skips an unnecessary lock request. }
//      procedure scRebuildKeyPath;                                    {!!.05 - moved to TfsSrBaseCursor.bcRebuildKeyPath}
//        { If the cursor has a valid key, this method rebuilds the cursor's key
//          path. }
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

    Function GetSetPosition(aValue: Longint; aData: PffByteArray; aLockType: TfsSrcLockType;
      Var aFlag: Byte; Var aRecNo: Longword;
      Var aRefNr: TffInt64;
      aInfoGetSetPosition: TInfoGetSetPosition;
      aSet: Boolean): TffResult; Override;

    Function UndeleteRecord(aData: PffByteArray; aLockType: TfsSrcLockType; Var aRefNr: TffInt64): TffResult; Override;
    Function InsertRecord(aData: PffByteArray; aLockType: TfsSrcLockType; aFlag: Byte; Var aRefNr: TffInt64): TffResult; Override;
    Function InsertRecordNoDefault(aData: PffByteArray; aLockType: TfsSrcLockType; aFlag: Byte; Var aRefNr: TffInt64): TffResult; Override; {!!.10}
    Function IsInRange(aKey: PffByteArray): Integer; Override;
    Function ModifyRecord(aData: PffByteArray; aRelLock: boolean; aUserLockType: TfsUserRecLocking; aFlag: Byte; aSet, Use: Boolean): TffResult;
      Override;
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

  TfsSrcCursorList = Class(TfsServerObjectList)
  Protected {private}
  Protected
    Function GetCursorItem(Find: TfsListFindType; Value: Longint): TfsSrBaseCursor;
  Public
    Procedure AddCursor(aCursor: TfsSrBaseCursor);

    Function CursorCount: Integer;
    { Returns the number of cursors in the list. }

    Procedure DeleteCursor(aCursorID: TffCursorID);
    { Removes a cursor from the list and frees the cursor. }

    Procedure RemoveCursor(aCursorID: TffCursorID);
    { Removes a cursor from the list but does not free the cursor. }

    Property Cursor[Find: TfsListFindType;
    Value: Longint]: TfsSrBaseCursor
    Read GetCursorItem; Default;

  End;

  { Describes the interface for the representation of a physical table. }
  TfsSrcBaseTable = Class(TfsSelfListItem)
  Protected
    btBaseName: PffShStr;
    btBLOBEngine: TffBaseBLOBEngine; {!!.11}
    btBufMgr: TfsBufferManager;
    btCursorList: TfsSrcCursorList;
    btDictionary: TFSInfoServerDict;
    btEngine: TFSServer;
    btFiles: TfsVCLList;
    btFolder: TfsSrcFolder;
    btForServer: Boolean;
    btContentLocks: TfsLockContainer;
    btClientLocks: TfsLockContainer;
    btOpenIntents: Longint;
    btPortal: TfsReadWritePortal;
    btTableFlags, btMaxRecords: Longint;
    btTableUndelete: boolean;
    {Begin !!.03}
    Procedure btCommitBLOBMgr;
    { Commits the changes made by the BLOB resource manager to its
      in-memory list. }
{End !!.03}
    Procedure btCreateFile(aFileInx: Integer;
      aTI: PffTransInfo;
      Const aExtension: TffExtension;
      aForServer: boolean;
      aAttribs: TffFileAttributes;
      aStore: TfsBaseTempStorage); Virtual;
    Procedure btDeleteBLOBsForRecord(aFI: PffFileInfo; aTI: PffTransInfo;
      aData: PffByteArray; Const aRefNr: TffInt64); Virtual;
    Function btGetBaseName: TfsTableName; Virtual;
    Function btGetCursorList: TfsSrcCursorList; Virtual;
    Function btGetDictionary: TFSInfoServerDict; Virtual;
    Function btGetFile(Inx: Integer): PffFileInfo; Virtual;
    Function btGetFileCount: Integer; Virtual;
    Function btGetFolder: TfsSrcFolder; Virtual;
    Procedure btInformCursors(aSrcCursorID: TffCursorID;
      aOp: TfsRecOp;
      aRefNr: TffInt64;
      aIndexID: Integer); Virtual;
    Function btGetOpenIntents: Longint; Virtual;
    {Begin !!.03}
    Procedure btRollbackBLOBMgr;
    { Rolls back the changes made by the BLOB resource manager to its
      in-memory list. }
{End !!.03}
    Procedure btSetFile(Inx: Integer; FI: PffFileInfo); Virtual;
    Procedure btSetFileCount(FC: Integer); Virtual;
    Procedure btTableUpdated(aDatabaseID: TffDatabaseID); Virtual;
    Procedure btUpdateAutoInc(aTI: PffTransInfo; aData: PffByteArray); Virtual;
  Public
    Constructor Create(anEngine: TFSServer;
      Const aBaseName: TfsTableName;
      aFolder: TfsSrcFolder;
      aBufMgr: TfsBufferManager;
      Const aOpenMode: TffOpenMode); Virtual;
    Destructor Destroy; Override;

    Function btNextAutoInc(aTI: PffTransInfo): Int64; Virtual;
    Procedure AcqClientLock(aCursorID: Longint;
      Const aLockType: TfsSrcLockType;
      Const aConditional: Boolean); Virtual;

    Procedure AcqContentLock(aTrans: TfsSrcTransaction;
      Const aLockType: TfsSrcLockType;
      Const aConditional: boolean;
      Const aUserLockType: TfsUserRecLocking = tluDatabase); Virtual;
    {Begin !!.10}
    Function AcqExclContentLock(aTrans: TfsSrcTransaction): TffResult; Virtual;
    {End !!.10}
    Procedure AcqLock(Const aCursorID: TffCursorID;
      Const aLockType: TfsSrcLockType); Virtual;
    {Begin !!.03}
    Procedure AddAttribute(Const anAttrib: TffFileAttribute);
    { Add an attribute to the table's FF-specific file attributes. }
{End !!.03}
    Procedure AddIndex(Const aIndexDesc: TffIndexDescriptor;
      aTI: PffTransInfo); Virtual; Abstract;
    Procedure BeginCommit; Virtual;
    { Before a transaction commits, a thread must call this method.
      This ensures that all readers have finished with the table before
      the table is updated.  When done committing, the thread must call
      TfsSrcTable.EndCommit. }
    Procedure BeginRead; Virtual;
    { Threads that are not in a transaction & needing to read data from
      the table must call this method prior to reading.  When done
      reading the thread must call TfsSrcTable.EndRead. }
    Procedure BuildFiles(aTI: PffTransInfo;
      aForServer: boolean;
      aDictionary: TFSInfoDict;
      aAttribs: TffFileAttributes;
      aStore: TfsBaseTempStorage); Virtual; Abstract;
    Function BuildKeyForRecord(aIndexID: Integer;
      aData: PffByteArray;
      aKey: PffByteArray;
      aFieldCount: Integer;
      aPartialLen: Integer): TffResult; Virtual; Abstract;
    Procedure CloseFiles(commitChanges: boolean; aTI: PffTransInfo); Virtual;
    Procedure CommitChanges(aTI: PffTransInfo); Virtual;
    Function CompareKeysForCursor(Var aKID: TffKeyIndexData;
      aKey1: PffByteArray;
      aKey2: PffByteArray)
      : Integer; Virtual; Abstract;
    Function DeleteRecord(aTI: PffTransInfo;
      Const aCursorID: TffCursorID;
      Const aRefNr: TffInt64;
      Const aLockObtained: Boolean;
      Var aBTreeChanged: Boolean) {!!.05}
    : TffResult; Virtual; Abstract;
    Procedure DeregisterOpenIntent; Virtual;
    { Use this function to deregister intent to open.  Should only be
      called if RegisterIntentOpen was previously called. }

    Procedure DropIndex(aTI: PffTransInfo; aIndexID: Longint); Virtual; Abstract;
    Function EmptyFiles(aTI: PffTransInfo): TffResult; Virtual;
    Procedure EndCommit(aDatabaseID: TffDatabaseID); Virtual;
    { Call this method after calling BeginCommit and finishing the commit
      operation. }
    Procedure EndRead; Virtual;
    { Call this method after calling BeginRead and finishing the read
      operation. }
    Function FindKey(Var aKID: TffKeyIndexData;
      Var aRefNr: TffInt64;
      aTI: PffTransInfo;
      aKey: PffByteArray;
      Var aKeyPath: TffKeyPath;
      aAction: TffSearchKeyAction): boolean; Virtual; Abstract;
    Function GetNextKey(Var aKID: TffKeyIndexData;
      Var aRefNr: TffInt64;
      aTI: PffTransInfo;
      aKey: PffByteArray;
      Var aKeyPath: TffKeyPath): TffResult; Virtual; Abstract;
    Function GetPrevKey(Var aKID: TffKeyIndexData;
      Var aRefNr: TffInt64;
      aTI: PffTransInfo;
      aKey: PffByteArray;
      Var aKeyPath: TffKeyPath): TffResult; Virtual; Abstract;
    Function GetNextRecord(aTI: PffTransInfo;
      Const aDatabaseID: TffDatabaseID; {!!.10}
      Const aCursorID: TffCursorID; {!!.10}
      Var aKID: TffKeyIndexData;
      Var aRefNr: TffInt64;
      aKey: PffByteArray;
      Var aKeyPath: TffKeyPath;
      aData: PffByteArray;
      Const aLockType: TfsSrcLockType;
      Var aFlag: Byte): TffResult; Virtual; Abstract;
    Procedure GetNextRecordSeq(aTI: PffTransInfo;
      Var aRefNr: TffInt64;
      aData: PffByteArray;
      aUndelete: boolean;
      OnlyDeleted: boolean;
      Var aFlag: Byte); Virtual;
    Procedure GetPrevRecordSeq(aTI: PffTransInfo;
      Var aRefNr: TffInt64;
      aData: PffByteArray;
      Var aFlag: Byte); Virtual;
    Function GetPriorRecord(aTI: PffTransInfo;
      Const aDatabaseID: TffDatabaseID; {!!.10}
      Const aCursorID: TffCursorID; {!!.10}
      Var aKID: TffKeyIndexData;
      Var aRefNr: TffInt64;
      aKey: PffByteArray;
      Var aKeyPath: TffKeyPath;
      aData: PffByteArray;
      Const aLockType: TfsSrcLockType;
      Var aFlag: Byte): TffResult; Virtual; Abstract; {!!.10}
    Function GetRecord(aTI: PffTransInfo;
      Const aDatabaseID: TffDatabaseID; {!!.10}
      Const aCursorID: TffCursorID; {!!.10}
      aRefNr: TffInt64;
      aData: PffByteArray;
      Const aLockType: TfsSrcLockType; {!!.10}
      Const aUserLockType: TfsUserRecLocking;
      Const aLockObtained: boolean; {!!.10}
      Const aConditional: boolean;
      Var aFlag: Byte): TffResult; Virtual; {!!.10}
    { Use this method to retrieve a record from the data file.
      If a lock has already been obtained via TfsSrcTable.GetRecordLock
      then set aLockObtained := True.  Doing so skips an unnecessary
      lock request. }
    Procedure GetRecordLock(aTI: PffTransInfo;
      Const aDatabaseID: TffDatabaseID; {!!.10}
      Const aCursorID: TffCursorID; {!!.10}
      Const aRefNr: TffInt64; {!!.10}
      Const aLockType: TfsSrcLockType;
      Const aUserLockType: TfsUserRecLocking;
      Const aUser: Boolean); Virtual; {!!.10}
    {Begin !!.10}
    Procedure GetRecordNoLock(aTI: PffTransInfo;
      aRefNr: TffInt64;
      aData: PffByteArray);
    { Retrieve a record without obtaining any type of lock. }
{End !!.10}
    Function HasClientLock(Const aCursorID: TffCursorID): boolean; Virtual;
    { Returns True if the specified cursor has a client lock (i.e.,
      TfsTable.LockTable). }
    Function HasLock(Const aCursorID: TffCursorID;
      Const aLockType: TfsSrcLockType): boolean; Virtual;
    { Returns True if the specified cursor has an open lock of the specified
      type on the table. }

    Function HasRecordLocks: Boolean;
    { Returns True if there are any record locks on the table. }
    Function RecordCountLocks: TffWord32;
    { Returns record count locks on the table. }
    Procedure AssignRecordCountLocks(aList: TList);
    { Assign record refnr locks on the table. }

    Function InsertRecord(aTI: PffTransInfo;
      aCursorID: TffCursorID;
      aData: PffByteArray;
      aLockType: TfsSrcLockType;
      Var aNewRefNr: TffInt64; aFlag: Byte): TffResult; Virtual; Abstract;
    Function UndeleteRecord(aTI: PffTransInfo;
      aCursorID: TffCursorID;
      Var aData: PffByteArray;
      aLockType: TfsSrcLockType;
      Var aNewRefNr: TffInt64): TffResult; Virtual; Abstract;
    Function InsertRecordNoDefault(aTI: PffTransInfo; {!!.10}
      aCursorID: TffCursorID;
      aData: PffByteArray;
      aLockType: TfsSrcLockType;
      Var aNewRefNr: TffInt64; aFlag: Byte): TffResult; Virtual; Abstract;
    Function IsContentLockedBy(aTrans: TfsSrcTransaction): boolean; Virtual;
    { Returns True if the table's contents are locked by the specified
      transaction. This returns True whether the lock is a read lock or
      a write lock. }
    Function IsRecordLocked(aTI: PffTransInfo;
      aCursorID: TffCursorID;
      aRefNr: TffInt64;
      aLockType: TfsSrcLockType): Boolean; Virtual;
    Function WhoRecordLocked(aTI: PffTransInfo;
      aCursorID: TffCursorID;
      aRefNr: TffInt64;
      aLockType: TfsSrcLockType): String; Virtual;
    Function IsServerTable: boolean; Virtual;
    { Returns True if this table is a server table. }
    Procedure MakeKIDForCursor(aIndexID: Integer; Var aKID: TffKeyIndexData); Virtual; Abstract;
    Procedure OpenFiles(aTI: PffTransInfo; aForServer: boolean;
      aAttribs: TffFileAttributes); Virtual;
    Function PutRecord(aTI: PffTransInfo;
      aCursorID: TffCursorID;
      aRefNr: TffInt64;
      aData: PffByteArray;
      aRelLock: boolean; {!!.05}
      aUserLockType: TfsUserRecLocking;
      Var aKeyChanged: Boolean): TffResult; Virtual; Abstract; {!!.05}

    Function PutSetFlagRecord(aTI: PffTransInfo;
      aCursorID: TffCursorID;
      aRefNr: TffInt64;
      aData: PffByteArray;
      aFlag: Byte; aSet, Use: Boolean): TffResult; Virtual; Abstract;

    Procedure RegisterOpenIntent; Virtual;
    { Use this method to register intent to open a table. }
{Begin !!.10}
    Procedure RelaxRecordLock(aTI: PffTransInfo;
      aCursorID: TffCursorID;
      aRefNr: TffInt64); Virtual;
    {End !!.10}
    Procedure RelClientLock(aCursorID: Longint; aRemoveAll: Boolean); Virtual;
    Procedure RelContentLock(aTrans: TfsSrcTransaction); Virtual;
    Procedure RelLock(Const aCursorID: TffCursorID;
      Const aAllLocks: boolean); Virtual;
    Procedure RelRecordLock(aTI: PffTransInfo;
      aDatabaseID: TffDatabaseID; {!!.10}
      aCursorID: TffCursorID;
      aRefNr: TffInt64); Virtual;
    Procedure RemoveLocksForCursor(Const aDatabaseID: TffDatabaseID; {!!.10}
      Const aCursorID: TffCursorID;
      Const aRefNr: TffInt64;
      aTI: PffTransInfo); Virtual;

    {Begin !!.03}
    Procedure ListBLOBFreeSpace(aTI: PffTransInfo;
      Const aInMemory: Boolean;
      aStream: TStream);
    {End !!.03}

    Procedure SetAttributes(Const fileAttribs: TffFileAttributes); Virtual;
    { Sets the file attributes on all files of a table instance. This
      should only be called when the table is first opened. }

    Procedure SetExclOwner(Const aCursorID: TffCursorID); Virtual;
    { Marks each file managed by a table as exclusively owned by the
      specified cursor. Only call this method when the table has been
      exclusively opened by the cursor. }

    Property BaseName: TfsTableName Read btGetBaseName;
    Property ClientLocks: TfsLockContainer Read btClientLocks; {!!.11}
    Property CursorList: TfsSrcCursorList Read btGetCursorList;
    Property Dictionary: TFSInfoServerDict Read btGetDictionary;
    Property FileCount: Integer Read btGetFileCount Write btSetFileCount;
    Property Files[Inx: Integer]: PffFileInfo Read btGetFile Write btSetFile;
    Property Folder: TfsSrcFolder Read btGetFolder;
    Property OpenIntents: Longint Read btGetOpenIntents;
    Property TableFlags: Longint Read btTableFlags Write btTableFlags;
    Property MaxRecords: Longint Read btMaxRecords Write btMaxRecords;
    { The number of threads that have registered their intent to open this
      table. }
    Property TableID: Longint Read KeyAsInt;
  End;

  { Represents a table opened by one or more cursors.  Only one
    instance of this class is created and the instance is freed when all
    cursors have closed the table.

    Table locks are acquired using the parent folder's lock manager.  This
    means that each client opening a table obtains some kind of lock on the
    table.  The following types of locks are used:

      ffsltExclusive - Used to obtain exclusive read-write access to a table.
      ffsltShare - Used to obtain read-only access to a table.
      ffsltIntentS - Used to obtain read-write access to a table.

    Since client A may open a table in read-only mode while clients B, C, & D
    may open a table in non-exclusive read-write mode, we use the ffsltShare
    & ffsltIntentS locks to represent non-exclusive read-write and read-only
    modes.  ffsltShare and ffsltIntentS are compatible locks so any number
    of clients may concurrently access the table.

    If a client wants to open the table exclusively, their request for a
    ffsltExclusive lock will wait until all non-exclusive read-write and
    read-only clients have released their locks.

    Conversely, a client wanting to open the table in read-only or
    non-exclusive read-write mode must wait until a client granted Exclusive
    access to the table has released its lock.

    Notes on LockTable and UnlockTable:
    Just as in the BDE, a client may lock a table for reading or writing.
    Pertinent rules:
    1. Table locking is as described in the previous paragraphs.
    2. If a table is read-locked then no client may edit a record.
    3. If a table is write-locked then only the client obtaining the lock
       may edit records.
  }
  TfsSrcTable = Class(TfsSrcBaseTable)
  Protected
    //      stUseInternalRollback : boolean;                               {!!.03}

    stUserBuildKey: TfsVCLList;
    stUserCompareKey: TfsVCLList;

    Function stGetBuiltCompositeKey(aIndexID: Integer;
      aData: PffByteArray;
      aKeyLen: Longint;
      Var aKey: PffByteArray): TffResult;
    Function stBuildCompositeKey(aIndexID: Integer;
      aData: PffByteArray;
      aKey: PffByteArray;
      aFieldCount: Integer;
      aLastFldLen: Integer): TffResult;
    Function stDeleteKeyPrim(aInxFile: Integer;
      aTI: PffTransInfo;
      aRefNr: TffInt64;
      aKey: PffByteArray;
      aCompare: TffKeyCompareFunc;
      aCmpData: PffCompareData;
      Var aBTreeChanged: Boolean): Boolean; {!!.05}
    Function stDeleteKeysForRecord(aTI: PffTransInfo;
      aRefNr: TffInt64;
      aData: PffByteArray;
      Var aBTreeChanged: Boolean) {!!.05}
    : TffResult;
    Function stGetUserBuildKey(aIndexID: Integer): TffKeyBuildFunc;
    Function stGetUserCompareKey(aIndexID: Integer): TffKeyCompareFunc;
    Function stInsertKeyPrim(aInxFile: Integer;
      aTI: PffTransInfo;
      aRefNr: TffInt64;
      aKey: PffByteArray;
      aCompare: TffKeyCompareFunc;
      aCmpData: PffCompareData): boolean;
    Function stInsertKeysForRecord(aTI: PffTransInfo;
      aRefNr: TffInt64;
      aData: PffByteArray): TffResult;
    Function stUpdateKeysForRecord(aCursorID: TffCursorID;
      aTI: PffTransInfo;
      aRefNr: TffInt64;
      aData,
      aOldData: PffByteArray; {!!.05}
      Var aKeyChanged: Boolean): TffResult; {!!.05}
  Public
    Constructor Create(anEngine: TFSServer;
      Const aBaseName: TfsTableName;
      aFolder: TfsSrcFolder;
      aBufMgr: TfsBufferManager;
      Const aOpenMode: TffOpenMode); Override;
    Destructor Destroy; Override;

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
    Function GetPrevKey(Var aKID: TffKeyIndexData;
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
    Function UndeleteRecord(aTI: PffTransInfo;
      aCursorID: TffCursorID;
      Var aData: PffByteArray;
      aLockType: TfsSrcLockType;
      Var aNewRefNr: TffInt64): TffResult; Override;
    Function InsertRecordNoDefault(aTI: PffTransInfo; {!!.10}
      aCursorID: TffCursorID;
      aData: PffByteArray;
      aLockType: TfsSrcLockType;
      Var aNewRefNr: TffInt64; aFlag: Byte): TffResult; Override;
    Procedure MakeKIDForCursor(aIndexID: Integer; Var aKID: TffKeyIndexData); Override;
    Function PutRecord(aTI: PffTransInfo;
      aCursorID: TffCursorID;
      aRefNr: TffInt64;
      aData: PffByteArray;
      aRelLock: boolean; {!!.05}
      aUserLockType: TfsUserRecLocking;
      Var aKeyChanged: Boolean): TffResult; Override; {!!.05}

    Function PutSetFlagRecord(aTI: PffTransInfo;
      aCursorID: TffCursorID;
      aRefNr: TffInt64;
      aData: PffByteArray;
      aFlag: Byte; aSet, Use: Boolean): TffResult; Override;
    Procedure RemoveDynamicLinks;
    Procedure ResolveDynamicLinks;

    Property BaseName: TfsTableName Read btGetBaseName;
    Property CursorList: TfsSrcCursorList Read btGetCursorList;
    Property Dictionary: TFSInfoServerDict Read btGetDictionary;
    Property FileCount: Integer Read btGetFileCount Write btSetFileCount;
    Property Files[Inx: Integer]: PffFileInfo Read btGetFile Write btSetFile;
    Property Folder: TfsSrcFolder Read btGetFolder;
    Property OpenIntents: Longint Read btOpenIntents;
    { The number of threads that have registered their intent to open this
      table. }
    Property TableID: Longint Read KeyAsInt;

    //      property UseInternalRollback : boolean read stUseInternalRollback write stUseInternalRollback; {!!.03}

  End;

  { The following class may be used to access system tables (e.g., FFSALIAS,
    FFSUSER, etc.). }
  TfsSrcSystemTable = Class(TfsSrcTable)
  Public
    Function IsServerTable: boolean; Override;
  End;

  TfsSrcTableList = Class(TFSSpecObject)
  Protected {private}
    tlList: TFSSpecThreadList;
    FOwner: TFSServer; {!!.06}
  Protected
    Function GetTableItem(Find: TfsListFindType; Value: Longint): TfsSrcBaseTable;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure AddTable(aTable: TfsSrcBaseTable);

    Function BeginRead: TfsSrcTableList;
    {-A thread must call this method to gain read access to the list.
      Returns the instance of this object as a convenience. }

    Function BeginWrite: TfsSrcTableList;
    {-A thread must call this method to gain write access to the list.
      Returns the instance of this object as a convenience.}

    Procedure DeleteTable(aTableID: Longint);

    Procedure EndRead;
    {-A thread must call this method when it no longer needs read access
      to the list.  If it does not call this method, all writers will
      be perpetually blocked. }

    Procedure EndWrite;
    {-A thread must call this method when it no longer needs write access
      to the list.  If it does not call this method, all readers and writers
      will be perpetualy blocked. }

    Function GetTableFromName(Const aTableName: TfsTableName): TfsSrcBaseTable;
    Procedure RemoveIfUnused(aTable: TfsSrcBaseTable);
    Procedure RemoveUnusedTables;
    Function TableCount: Integer;

    Property Owner: TFSServer {!!.06}
    Read FOwner Write FOwner; {!!.06}
    Property Table[Find: TfsListFindType;
    Value: Longint]: TfsSrcBaseTable
    Read GetTableItem; Default;
  End;

  { An instance of this class mirrors an instance of TffDatabase in the client
    application.  If multiple clients open the same database, there will be
    one instance of TfsSrcDatabase per client.

    A TfsSrcDatabase may have one active transaction however there may be
    multiple concurrent transactions on a physical database. }
  TfsSrcDatabase = Class(TfsServerObject)
  Protected {private}
    dbAlias: PffShStr;
    dbCheckSpace: Boolean; {!!.11}
    dbCursorList: TfsSrcCursorList;
    dbEngine: TFSServer;
    dbExtenders: TFSSpecThreadList;
    dbFolder: TfsSrcFolder;
    dbOpenMode: TffOpenMode;
    dbSession: TfsSrcSession;
    dbShareMode: TffShareMode;
    dbStmtList: TfsSrcStmtList; {!!.10}
    dbTI: PffTransInfo;
    {-Transaction-specific information used for locking. }
    dbTrans: TfsSrcTransaction;
    {-The active transaction for this database. }
    dbTransIsolation: TfsTransIsolation; // = (tiRepeatableRead);//, tiSerializable);
    dbRecLocking: TfsDataBaseRecLocking; // = (tlOptimistic, tlpessimistic);

  Protected
    Procedure dbAddExtender(anExtender: TFSBaseEngineExtender);
    Function dbGetAlias: TffName;
    Function dbGetDatabaseID: TffDatabaseID;
    Function dbGetTransID: TffTransID;
    {-Returns the ID of the transaction associated with the cursor. }

    Function dbGetTransLSN: TffWord32;
    {-Returns the LSN of the cursor's transaction. }

{Begin !!.11}
    Procedure dbSetExistingTableVersion(Const Version: Longint);
    { *** WARNING: This procedure is provided for testing & utility
          purposes only. Do not use it unless you really know what you're
          doing. That means you! ***}
    Procedure dbSetNewTableVersion(Const Version: Longint);
    { *** WARNING: This procedure is provided for testing & utility
          purposes only. Do not use it unless you really know what you're
          doing. That means you! ***}
    Procedure dbSetPackSrcTableVersion(Const Version: Longint);
    { *** WARNING: This procedure is provided for testing & utility
          purposes only. Do not use it unless you really know what you're
          doing. That means you! ***}
{End !!.11}

    Procedure dbSetTrans(aTransaction: TfsSrcTransaction); Virtual;

  Public
    Constructor Create(anEngine: TFSServer;
      aSession: TfsSrcSession;
      aFolder: TfsSrcFolder;
      anAlias: TffName;
      aOpenMode: TffOpenMode;
      aShareMode: TffShareMode;
      aTimeout: Longint;
      aCheckSpace: Boolean;
      aTransIsolation: TfsTransIsolation;
      aRecLocking: TfsDataBaseRecLocking); {!!.11}
    Destructor Destroy; Override;

    Function CanClose(Const Mark: boolean): boolean; Override;

    Procedure ForceClose; Override;

    Function NotifyExtenders(Const anAction: TffEngineAction;
      Const aFailAction: TffEngineAction): TffResult;
    {-Notifies all extenders associated with the cursor about the
      specified action.  If ignoreErrCode is True then error codes
      returned by extenders are ignored.  If failures occur it will
      be taken care of before going back to the calling method.}

    Procedure RequestClose; Override; {!!.03}

    Function ShouldClose: boolean; Override;

    Property Alias: TffName Read dbGetAlias;
    {-The alias for which this database was opened. }
    Property CheckSpace: Boolean {!!.11}
    Read dbCheckSpace; {!!.11}
    Property CursorList: TfsSrcCursorList Read dbCursorList;
    Property DatabaseID: TffDatabaseID Read dbGetDatabaseID;
    Property Engine: TFSServer Read dbEngine;
    Property Folder: TfsSrcFolder Read dbFolder;
    Property OpenMode: TffOpenMode Read dbOpenMode;
    Property Session: TfsSrcSession Read dbSession;
    Property ShareMode: TffShareMode Read dbShareMode;
    Property StmtList: TfsSrcStmtList Read dbStmtList; {!!.10}
    Property Transaction: TfsSrcTransaction Read dbTrans Write dbSetTrans;
    { The transaction associated with the cursor. }
    Property TransactionID: TffTransID Read dbGetTransID;
    { The transaction active for this cursor.  If no transaction is
      active then returns zero. }
    Property TransactionInfo: PffTransInfo Read dbTI;
    { Returns a pointer to the cursor's transaction information. }
    Property TransactionLSN: TffWord32 Read dbGetTransLSN;
    { Returns the LSN of the transaction associated with the cursor. }
//    property ServerEngine : TFSServer read dbEngine;           {Deleted !!.03}
    Property TransIsolation: TfsTransIsolation Read dbTransIsolation Write dbTransIsolation;
    Property RecLocking: TfsDataBaseRecLocking Read dbRecLocking Write dbRecLocking;
  End;

  TfsSrcDatabaseList = Class(TfsServerObjectList)
  Protected {private}
  Protected
    Function GetDatabaseItem(Find: TfsListFindType; Value: Longint): TfsSrcDatabase;
  Public
    Procedure AddDatabase(aDatabase: TfsSrcDatabase);

    Function DatabaseCount: Integer;
    Procedure DeleteDatabase(aDatabaseID: Longint);

    Function GetDatabaseForFolder(aFolder: TfsSrcFolder): TfsSrcDatabase;

    Property Database[Find: TfsListFindType;
    Value: Longint]: TfsSrcDatabase Read GetDatabaseItem; Default;
  End;

  TfsSrcSession = Class(TfsServerObject)
  Protected {private}
    ssDatabaseList: TfsSrcDatabaseList;
    ssIsDefault: boolean;
    ssTablePasswordList: TFSSpecStringList;
  Protected
    Function ssGetSessionID: TffSessionID;
  Public
    Constructor Create(aClient: TfsSrcClient; Const aIsDef: boolean;
      Const aTimeout: Longint);
    Destructor Destroy; Override;

    Function CanClose(Const Mark: boolean): boolean; Override;

    Procedure ForceClose; Override;

    Procedure RequestClose; Override; {!!.03}

    Function ShouldClose: boolean; Override;

    Property DatabaseList: TfsSrcDatabaseList Read ssDatabaseList;
    Property IsDefault: boolean Read ssIsDefault;
    Property SessionID: TffSessionID Read ssGetSessionID;
    Property TablePasswordList: TFSSpecStringList Read ssTablePasswordList Write ssTablePasswordList;
  End;

  TfsSrcSessionList = Class(TfsServerObjectList)
  Protected {private}
    slDefSess: TfsSrcSession;
    slCurSess: TfsSrcSession;
  Protected
    Function slGetCurSess: TfsSrcSession;
    Function slGetSessionItem(Find: TfsListFindType; Value: Longint): TfsSrcSession;
    Procedure slSetCurSess(CS: TfsSrcSession);
  Public
    Procedure AddSession(aSession: TfsSrcSession);

    Procedure DeleteSession(aSessionID: Longint);
    Function SessionCount: Integer;
    Procedure SetDefaultSession(aSession: TfsSrcSession);
    Property CurrentSession: TfsSrcSession Read slGetCurSess Write slSetCurSess;
    Property Session[Find: TfsListFindType;
    Value: Longint]: TfsSrcSession Read slGetSessionItem;
  End;

  {Begin !!.10}
  TfsBasePreparedStmt = Class(TfsServerObject)
  Protected
    bpsClientID: TffClientID;
    bpsDatabaseID: TffDatabaseID;
    bpsEngine: TFSServer;
  Public
    Procedure Bind; Virtual; Abstract; {!!.11}
    Function Execute(Var aLiveResult: Boolean;
      Var aCursorID: TffCursorID;
      Var aRowsAffected: Integer;
      Var aRecordsRead: Integer): TffResult; Virtual; Abstract;

    Function Parse(aQuery: PChar): Boolean; Virtual; Abstract;

    Property ClientID: TffClientID
      Read bpsClientID;
    { ID of owning client. }

    Property DatabaseID: TffDatabaseID
      Read bpsDatabaseID;
    { ID of owning database. }

    Property Engine: TFSServer
      Read bpsEngine;

    Property Handle: Longint
      Read KeyAsInt;
    { Statement handle. }
  End;

  TfsSrcStmtList = Class(TfsServerObjectList)
  Protected
    Function GetStmt(Find: TfsListFindType; Value: Longint): TfsBasePreparedStmt;
  Public
    Procedure AddStmt(aStmt: TfsBasePreparedStmt);

    Procedure DeleteStmt(aStmtID: TffSQLStmtID);

    Procedure RemoveForClient(Const aClientID: TffClientID);
    {-Removes all prepared statements associated with a particular client. }

    Function StmtCount: Integer;

    Property Stmt[Find: TfsListFindType;
    Value: Longint]: TfsBasePreparedStmt
    Read GetStmt; Default;
  End;
  {End !!.10}

  TfsSrcClient = Class(TfsServerObject)
  Protected {private}
    clAccepted: boolean;
    clClientName: PffShStr;
    clEngine: TFSServer;
    clExtenders: TFSSpecThreadList;
    clSessionList: TfsSrcSessionList;
    clUserID: TffName;
    clFirst: TffName;
    clLast: TffName;
    clRights: TffUserRights;
    clExport, clImport: boolean;
    clFirstSession: TfsSrcSession; {!!.03}
    clClientVersion: Longint; {!!.11}
  Protected
    Function clGetClientID: TffClientID;
    Function clGetClientName: TffNetName;
  Public
    Constructor Create(aClientID: Longint;
      Const aClientName: TffNetName;
      Const aTimeout: Longint;
      Const aClientVersion: Longint; {!!.11}
      aUser: TfsUserItem;
      anEngine: TFSServer);
    Destructor Destroy; Override;

    Procedure AddClientExtender(anExtender: TFSBaseEngineExtender);
    {-Use this method to add an extender to the list of extenders
      interested in clients. }

    Function CanClose(Const Mark: boolean): boolean; Override;

    Procedure ForceClose; Override;

    Function NotifyExtenders(Const anAction: TffEngineAction;
      Const aFailAction: TffEngineAction): TffResult;
    {-Use this method to notify client extenders about a client-related
      action. }

    Procedure RequestClose; Override; {!!.03}

    Function ShouldClose: boolean; Override;

    Property Accepted: boolean Read clAccepted Write clAccepted;
    Property ExportData: boolean Read clExport Write clExport;
    Property ImportData: boolean Read clImport Write clImport;
    { Returns True if the client was accepted by the client extender(s). }
    Property ClientID: TffClientID Read clGetClientID;
    Property ClientVersion: Longint Read clClientVersion; {!!.11}
    Property ClientName: TffNetName Read clGetClientName;
    Property Rights: TffUserRights Read clRights;
    Property SessionList: TfsSrcSessionList Read clSessionList;
  End;

  TfsSrcClientList = Class(TfsServerObjectList)
  Protected {private}
  Protected
    Function GetClientItem(Find: TfsListFindType; Value: Longint): TfsSrcClient;
    Procedure SetClientItem(Inx: Integer; CI: TfsSrcClient);
  Public
    Procedure AddClient(aClient: TfsSrcClient);

    Function ClientCount: Integer;
    Procedure DeleteClient(aClientID: Longint);

    Property Client[Find: TfsListFindType;
    Value: Longint]: TfsSrcClient Read GetClientItem;
  End;

  PffSrRebuildParams = ^TfsSrcRebuildParams;
  TfsSrcRebuildParams = Record
    rpDB: TfsSrcDatabase;
    rpTableName: TfsTableName;
    rpIndexName: TffName;
    rpIndexID: Longint;
    rpRebuildStatus: TfsSrcRebuildStatus;
    rpCursor: TfsSrcCursor;
    rpTargetCursor: TfsSrcCursor;
    rpFieldMap: TfsSrcFieldMapList;
  End;

  TFSServer = Class(TfsIntermediateServerEngine)
  Private
  Protected {public}
    seCursorClass: TfsSrcCursorClass; {!!.06}
    seBufMgr: TfsBufferManager;
    seCanLog: Boolean; { If True then can write to event log. }
    seClientHash: TfsHash; {!!.02}
    seConfig: TfsServerConfiguration;
    seConfigLoaded: Boolean; { True if config tables have been loaded. }
    seGarbageThread: TfsTimerThread;
    seLastFlush: DWORD; {!!.01}
    seRebuildList: TfsSrcRebuildStatusList;
    seStartTime: DWORD; {!!.10}
    seUniqueID: TGUID; {!!.10}
    seClientList: TfsSrcClientList;
    seTempPath: String;
    seConfigFile: String;
    { The location of the server tables for this server engine.
      IMPORTANT NOTE: When retrieving this value, use the TempPath property
      or the seGetTempPath method directly as this method determines the
      correct config dir for the server if the config dir has not been
      specified (i.e., is set to ''). }
    seCursorList: TfsSrcCursorList;
    seDatabaseList: TfsSrcDatabaseList;
    seFolderList: TfsSrcFolderList;
    seOnRecoveryCheck: TNotifyEvent;
    { Handler called when it is time to check for recovery. }
    seScriptFile: TffFullFileName;
    seSessionList: TfsSrcSessionList;
    seSQLEngine: TFSBaseSQLEngine;
    seTableList: TfsSrcTableList;

    seEvtClientDone: TFSNormalEvent;
    {This event is used to notify a server when a client is done
     processing during server shutdown. This event is nill except
     when shutting down.}

    Function seTransactionCommitSubset(Const aDB: TfsSrcDatabase): TffResult;
    {Begin !!.11}
    Function seClientAddPrim(Var aClientID: TffClientID;
      Const aClientName: TffNetName;
      Const aUserID: TffName;
      Const aTimeout: Longint;
      Const aClientVersion: Longint;
      Var aHash: TffWord32;
      Var aRights: TffUserRights;
      Var aSecurityEnabled: boolean): TffResult;
    {End !!.11}
    Procedure seClientRemovePrim(Const aClient: TfsSrcClient);
    Function seConvertSingleField(aSourceBuf,
      aTargetBuf: PffByteArray;
      aSourceCursorID,
      aTargetCursorID: Longint;
      aSourceFldNr,
      aTargetFldNr: Integer;
      aBLOBBuffer: Pointer;
      aBLOBBufLen: Longint;
      aRangError: boolean): TffResult;
    Function seDatabaseAliasListPrim(aList: TList): TffResult;
    Function seDatabaseDeleteAliasPrim(aAlias: TffName): TffResult;
    Function seDatabaseGetAliasPathPrim(aAlias: TffName;
      Var aPath: TffPath): TffResult;
    Function seDeleteTable(Const aDB: TfsSrcDatabase;
      Const aTableName: TfsTableName): TffResult;
    Function seGetConfig: TfsServerConfiguration;
    Function seGetDictionary(Const aDB: TfsSrcDatabase;
      Const aTableName: TfsTableName;
      Var aDict: TFSInfoDict): TffResult;
    Function seIsServerTable(Const aTableName: TfsTableName): Boolean;
    Function seGetCollectFrequency: Longint;
    Function seGetCollectGarbage: Boolean;
    Function seGetTempPath: String;
    Function seGetConfigFile: String;
    Function seGetMaxRAM: Longint;
    Function seGetSecurityEnabled: Boolean;
    Function seGetScriptFile: String; {!!.11}
    Procedure seSetCollectFrequency(aFreq: Longint);
    Procedure seSetCollectGarbage(aValue: Boolean);
    Procedure seSetTempPath(Const aPath: String);
    Procedure seSetConfigFile(Const aFile: String);
    Function seGetClearCachePerCount: Longint;
    Function seGetClearCacheIfUpdate: Boolean;

    Function seGetCloseInactiveTablesAfterCommitOrRoolback: Boolean;

    Function seGetCloseInactiveTables: Boolean;
    Function seGetClearCache: Boolean;

    Procedure seSetClearCachePerCount(Const aValue: Longint);
    Procedure seSetCloseInactiveTablesAfterCommitOrRoolback(Const aValue: Boolean);
    Procedure seSetCloseInactiveTables(Const aValue: Boolean);
    Procedure seSetClearCacheIfUpdate(Const aValue: Boolean);
    Procedure seSetClearCache(Const aValue: Boolean);
    Procedure seSetMaxRAM(Const aValue: Longint); {!!.01}
    Procedure seSetSecurityEnabled(aValue: Boolean);
    Procedure seSetScriptFile(Const aFile: String); {!!.11}
    Function seTableBuildPrim(aDB: TfsSrcDatabase;
      aOverwrite: Boolean;
      Const aTableName: TfsTableName;
      aForServer: Boolean;
      aDict: TFSInfoDict): TffResult;
    Function seTableDeletePrim(DB: TfsSrcDatabase;
      Const aTableName: TfsTableName): TffResult;
    Function seTableExistsPrim(aDB: TfsSrcDatabase; {!!.11}
      Const aTableName: TfsTableName): Boolean; {!!.11}
    Function seTablePackPrim(aRebuildParamsPtr: PffSrRebuildParams; aRangeError: Boolean; UndeleteRecords: Boolean; OnlyDeleted: boolean): TffResult;
    Function seTableRebuildIndexPrim(aRebuildParamsPtr: PffSrRebuildParams): TffResult;
    Function seTableGetRecordCountPrim(aRebuildParamsPtr: PffSrRebuildParams): TffResult; { !!.10}

  Protected
    {validation and checking}

    { The Check*IDAndGet routines are responsible for checking the
      engine state to make sure it is ffesStarted.  The
      seCheck*IDAndGet avoid checking the engine state.
      WARNING: Ensure changes are made to Check*IDAndGet and
               seCheck*IDAndGet }
//    function CheckClientIDAndGet(aClientID : TffClientID;            {!!.01 - Start}
//                             var aClient   : TfsSrcClient)            {Moved to Public section}
//                                           : TffResult;              {!!.01 - End}
    Function seCheckClientIDAndGet(aClientID: TffClientID;
      Var aClient: TfsSrcClient): TffResult;
    //    function CheckSessionIDAndGet(aClientID  : TffClientID;          {!!.01 - Start}
    //                                  aSessionID : TffSessionID;         {Moved to Public section}
    //                              var aClient    : TfsSrcClient;          {!!.01 - End}
    //                              var aSession   : TfsSrcSession) : TffResult;
    Function seCheckSessionIDAndGet(aSessionID: TffSessionID;
      Var aSession: TfsSrcSession): TffResult;
    //    function CheckTransactionIDAndGet(aTransactionID : TffTransID;   {!!.01 - Start}
    //                                  var aTrans         : TfsSrcTransaction) {Moved to Public section}
    //                                                     : TffResult;    {!!.01 - End}
    Function seCheckCursorIDAndGet(aCursorID: TffCursorID;
      Var aCursor: TfsSrBaseCursor): TffResult;
    {-Find the cursor specified by aCursorID. }

    Function seCheckDatabaseIDAndGet(aDatabaseID: TffDatabaseID;
      Var aDatabase: TfsSrcDatabase): TffResult;
    Function GetTableInstance(aFolder: TfsSrcFolder;
      Const aTableName: TfsTableName): TfsSrcBaseTable;
    Function IsTableNameOpen(aFolder: TfsSrcFolder;
      Const aTableName: TfsTableName): boolean;

    {rebuild status related stuff}
    Function RebuildRegister(aClientID: TffClientID;
      aTotalRecords: Longint): TfsSrcRebuildStatus;
    Procedure RebuildDeregister(aRebuildID: Longint);

    Function seBLOBCopy(aSrc, aTgt: TfsSrBaseCursor;
      aSourceBLOBNr, aTargetBLOBNr: TffInt64;
      aBuffer: pointer;
      aBufLen: Longint;
      aSourceCompress, aTargetCompress: TDataCompLevel): TffResult;
    Procedure BCompress(Stream: TMemoryStream; aTgt: TfsSrBaseCursor; aTargetBLOBNr: TffInt64; aTargetCompress: TDataCompLevel);
    Function seDatabaseAddAliasPrim(Const aAlias: TffName;
      Const aPath: TffPath;
      aCheckSpace: Boolean) {!!.11}
    : TffResult;
    Function seDatabaseOpenPrim(Session: TfsSrcSession;
      Folder: TfsSrcFolder;
      anAlias: TffName;
      aOpenMode: TffOpenMode;
      aShareMode: TffShareMode;
      aTimeout: Longint;
      aCheckSpace: Boolean;
      aTransIsolation: TfsTransIsolation;
      aRecLocking: TfsDataBaseRecLocking) {!!.11}
    : TfsSrcDatabase;
    {-Used by the public DatabaseOpenxx methods and used to open system
      tables. }

    Function seTableRenamePrim(DB: TfsSrcDatabase;
      Const aOldName, aNewName: TffName): TffResult;

    Function RecordGetNextSeq(aCursorID: TffCursorID; Var aRefNr: TffInt64; aData: PffByteArray): TffResult;

    {index stuff}
    Function IndexClear(aCursorID: TffCursorID): TffResult;

    {misc stuff}
    Procedure CreateAdminUser;
    {-create the default administrator user}

  Protected

    {State methods}
    Procedure scInitialize; Override;
    Procedure scPrepareForShutdown; Override;
    Procedure scShutdown; Override;
    Procedure scStartup; Override;

    { Property methods }
    Function bseGetAutoSaveCfg: Boolean; Override;
    Function bseGetReadOnly: Boolean; Override;
    Procedure bseSetAutoSaveCfg(aValue: Boolean); Override; {!!.01}
    Procedure bseSetReadOnly(aValue: Boolean); Override; {!!.01}
    Procedure lcSetEventLog(anEventLog: TFSBaseLog); Override;
    Procedure lcSetLogEnabled(Const aEnabled: boolean); Override;

    { Misc }

    Procedure seCleanRebuildList(Const aClientID: TffClientID); Virtual;
    {-Remove all entries in the rebuild status list for the specified
      client. }

    Procedure seCollectGarbage(Const aTimerEventCookie: Longint); Virtual;
    {-Looks for clients, sessions, databases, cursors, tables, &
      folders that should be closed & freed. }

    Procedure seForce(Const aMsg: String; {!!.06 - Start}
      args: Array Of Const;
      ReadOnly: Boolean); Virtual; {!!.06 - End}
    {-Use this method to log a formatted string to the event log. Writes to
      the log whether or not logging is enabled. }

    Function seGetServerName: TffNetName;
    {-Returns the server's name from its configuration. }

    Procedure seSetLoggingState;
    {-Called whenever something is changed that would affect logging.
      Sets a boolean flag that tells the logging routines whether or not
      they can log.  We centralize the logic here so that the logging
      routines don't have to do the checks each time they are called. }

    Procedure seSetSQLEngine(anEngine: TFSBaseSQLEngine);
    {-Used to set the SQLEngine property of the server engine. }

  {script stuff}
    Function CalcPriorityIndex(Const PriorityStr: TffShStr): Integer;
    Function CalcKeyIndex(Const KeyStr: TffShStr): Integer;
    Function ValBoolean(Const BoolStr: TffShStr;
      Var BoolValue: boolean): boolean;
    Procedure ProcessAliasScript;
    {process the FFALIAS.SC$ script file to autocreate aliases}
    Procedure ProcessFullScript(Const ScriptFileName: TffFullFileName);
    {process a server script file to set general info & aliases}
    Procedure ProcessScriptCommand(Const KeyStr, ValueStr: TffShStr;
      Var DeleteScript: Boolean);
    Function seInTransaction(Const aDatabaseID: TffDatabaseID;
      Var aTransLevel: Longint): TffResult;
    Function seTransactionStart(Const aDB: TfsSrcDatabase;
      Const aFailSafe, aImplicit: boolean;
      Var aTransactionID: TffTransID): TffResult;
    {-starts a transaction based on aImplicit setting}
    Function seTransactionCommit(aDB: TfsSrcDatabase; aRemoveFile: Boolean = False): TffResult;
    Function seTransactionRollback(aDB: TfsSrcDatabase): TffResult;
    Function seTransactionCorrupted(Const aDatabaseID: TffDatabaseID): TffResult;

    Procedure LoadAliasData(aStream: TStream = Nil; aVersion: Integer = 0);
    {-read the aliases - databases}
    Procedure LoadGeneralInfo(aStream: TStream = Nil; aVersion: Integer = 0);
    {-read the general info}
    Procedure LoadUserIndex(aStream: TStream = Nil; aVersion: Integer = 0);
    {-read the user-defined index}
    Procedure LoadUserData(aStream: TStream = Nil; aVersion: Integer = 0);
    {-read the user}

    Function SaveAliasData(aStream: TStream = Nil): TffResult;
    {-write the aliases}
    Function SaveGeneralInfo(aStream: TStream = Nil): TffResult;
    {-write the general info}
    Function SaveUserIndex(aStream: TStream = Nil): TffResult;
    {-write the user-defined index}
    Function SaveUserData(aStream: TStream = Nil): TffResult;
    {-write the user}
  Public
    {creation/destruction}
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp; {!!.11}
      Const AData: TffWord32); Override; {!!.11}
    { When the freeing of seSQLEngine is detected, this method
      sets seSQLEngine to nil to avoid using the freed TFSBaseSQLEngine. }

  { Event logging }
    Procedure Log(Const aMsg: String); Override;
    {-Use this method to log a string to the event log. }

    Procedure LogAll(Const Msgs: Array Of String); Override;
    {-Use this method to log multiple strings to the event log. }

    Procedure LogFmt(Const aMsg: String; args: Array Of Const); Override;
    {-Use this method to log a formatted string to the event log. }

  { Object validation }
    Function CheckCursorIDAndGet(aCursorID: TffCursorID;
      Var aCursor: TfsSrBaseCursor)
      : TffResult;
    {-Find the cursor specified by aCursorID. }
    Function CheckDatabaseIDAndGet(aDatabaseID: TffDatabaseID;
      Var aDatabase: TfsSrcDatabase)
      : TffResult;
    {-Find the database specified by aDatabaseID. }
    Function CheckClientIDAndGet(aClientID: TffClientID; {!!.01 - Start}
      Var aClient: TfsSrcClient) {Moved from Public section}
    : TffResult;
    Function CheckSessionIDAndGet(aClientID: TffClientID; {Moved from Public section}
      aSessionID: TffSessionID;
      Var aClient: TfsSrcClient;
      Var aSession: TfsSrcSession)
      : TffResult;
    Function CheckTransactionIDAndGet(aTransactionID: TffTransID; {Moved from Public section}
      Var aTrans: TfsSrcTransaction)
      : TffResult; {!!.01 - End}

    Procedure GetServerNames(aList: TStrings;
      aTimeout: Longint); Override;

    {transaction tracking}
    Function InTransaction(Const aDatabaseID: TffDatabaseID; Var aTransLevel: Longint): TffResult; Override;
    Function TransactionCorrupted(Const aDatabaseID: TffDatabaseID): TffResult; Override;
    Function TransactionCommit(Const aDatabaseID: TffDatabaseID; aRemoveFile: Boolean = False): TffResult; Override;
    {Begin !!.01}
    Function TransactionCommitSQL(Const aDatabaseID: TffDatabaseID;
      Const notifyExtenders: Boolean): TffResult;
    { Commit transaction for SQL engine. Does not reset timeout and controls
      extender notification. }
{End !!.01}
    Function TransactionCommitSubset(Const aDatabaseID: TffDatabaseID): TffResult;
    Function TransactionRollback(Const aDatabaseID: TffDatabaseID): TffResult; Override;
    {Begin !!.01}
    Function TransactionRollbackSQL(Const aDatabaseID: TffDatabaseID;
      Const notifyExtenders: Boolean): TffResult;
    { Rollback transaction for SQL engine. Does not reset timeout and
      controls extender notification. }
{End !!.01}
    Function TransactionStart(Const aDatabaseID: TffDatabaseID;
      Const aFailSafe: boolean): TffResult; Override;
    {-starts an explicit transaction}
{Begin !!.01}
    Function TransactionStartSQL(Const aDatabaseID: TffDatabaseID;
      Const notifyExtenders: boolean): TffResult;
    { For use by the SQL engine. Starts a transaction without resetting
      the timeout & controls notification of extenders. }
{End !!.01}

{Begin !!.10}
    Function TransactionStartWith(Const aDatabaseID: TffDatabaseID;
      Const aFailSafe: Boolean;
      Const aCursorIDs: TfsPointerList): TffResult; Override;
    {End !!.10}

          {client related stuff}
    Function ClientAdd(Var aClientID: TffClientID;
      Const aClientName: TffNetName;
      Const aUserID: TffName;
      Const aTimeout: Longint;
      Var aHash: TffWord32;
      Var aRights: TffUserRights;
      Var aSecurityEnabled: boolean): TffResult; Override;
    {Begin !!.11}
    Function ClientAddEx(Var aClientID: TffClientID;
      Const aClientName: TffNetName;
      Const aUserID: TffName;
      Const aTimeout: Longint;
      Const aClientVersion: Longint;
      Var aHash: TffWord32;
      Var aRights: TffUserRights;
      Var aSecurityEnabled: boolean): TffResult; Override;
    { Same as ClientAdd but client version is supplied via the aClientVersion
      parameter. }
{End !!.11}
    Function ClientRemove(aClientID: TffClientID): TffResult; Override;
    Function ClientSetTimeout(Const aClientID: TffClientID;
      Const aTimeout: Longint): TffResult; Override;

    {client session related stuff}
    Function SessionAdd(Const aClientID: TffClientID; Const timeout: Longint;
      Var aSessionID: TffSessionID;
      aData: Pointer;
      aDataLength: Longint = 0): TffResult; Override;
    Function SessionCloseInactiveTables(aClientID: TffClientID): TffResult; Override; {!!.06}
    Function SessionCount(aClientID: TffClientID; Var aCount: Integer): TffResult; Override;
    Function SessionGetCurrent(aClientID: TffClientID; Var aSessionID: TffSessionID): TffResult; Override;
    Function SessionRemove(aClientID: TffClientID; aSessionID: TffSessionID): TffResult; Override;
    Function SessionSetCurrent(aClientID: TffClientID; aSessionID: TffSessionID): TffResult; Override;
    Function SessionSetTimeout(Const aClientID: TffClientID;
      Const aSessionID: TffSessionID;
      Const aTimeout: Longint): TffResult; Override;

    {database related stuff}
    Function DatabaseAddAlias(Const aAlias: TffName;
      Const aPath: TffPath;
      aCheckSpace: Boolean; {!!.11}
      Const aClientID: TffClientID)
      : TffResult; Override;
    Function DatabaseAliasList(aList: TList;
      aClientID: TffClientID): TffResult; Override;
    Function RecoveryAliasList(aList: TList;
      aClientID: TffClientID): TffResult; Override;
    {-Return a list of database aliases for use by a journal recovery
      engine. The functionality of this method is identical to
      DatabaseAliasList except that it does not require the server engine
      to be started. }
    Function DatabaseChgAliasPath(aAlias: TffName;
      aNewPath: TffPath;
      aCheckSpace: Boolean; {!!.11}
      aClientID: TffClientID)
      : TffResult; Override;
    Function DatabaseClose(aDatabaseID: TffDatabaseID): TffResult; Override;
    Function DatabaseDeleteAlias(aAlias: TffName;
      aClientID: TffClientID): TffResult; Override;
    Function DatabaseGetAliasPath(aAlias: TffName;
      Var aPath: TffPath;
      aClientID: TffClientID): TffResult; Override;
    Function DatabaseGetFreeSpace(Const aDatabaseID: TffDatabaseID;
      Var aFreeSpace: Int64): TffResult; Override;
    Function DatabaseModifyAlias(Const aClientID: TffClientID;
      Const aAlias: TffName;
      Const aNewName: TffName;
      Const aNewPath: TffPath;
      aCheckSpace: Boolean) {!!.11}
    : TffResult; Override;
    Function DatabaseOpen(aClientID: TffClientID;
      Const aAlias: TffName;
      Const aOpenMode: TffOpenMode;
      Const aShareMode: TffShareMode;
      Const aTimeout: Longint;
      Const aTransIsolation: TfsTransIsolation;
      Const aRecLocking: TfsDataBaseRecLocking;
      Var aDatabaseID: TffDatabaseID): TffResult; Override;
    Function DatabaseOpenNoAlias(aClientID: TffClientID;
      Const aPath: TffPath;
      Const aOpenMode: TffOpenMode;
      Const aShareMode: TffShareMode;
      Const aTimeout: Longint;
      Const aTransIsolation: TfsTransIsolation;
      Const aRecLocking: TfsDataBaseRecLocking;
      Var aDatabaseID: TffDatabaseID): TffResult; Override;
    Function DatabaseSetTimeout(Const aDatabaseID: TffDatabaseID;
      Const aTimeout: Longint): TffResult; Override;
    Function DatabaseTableExists(aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName;
      Var aExists: Boolean): TffResult; Override;
    Function DatabaseTableList(aDatabaseID: TffDatabaseID;
      Const aMask: TffFileNameExt;
      aList: TList): TffResult; Override;
    Function DatabaseTableLockedExclusive(aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName;
      Var aLocked: Boolean): TffResult; Override;
    {rebuild status related stuff}
    Function RebuildGetStatus(aRebuildID: Longint;
      Const aClientID: TffClientID;
      Var aIsPresent: boolean;
      Var aStatus: TffRebuildStatus): TffResult; Override;

    {table related stuff}
    Function TableAddIndex(Const aDatabaseID: TffDatabaseID;
      Const aCursorID: TffCursorID;
      Const aTableName: TfsTableName;
      Const aIndexDesc: TffIndexDescriptor): TffResult; Override;
    Function TableBuild(aDatabaseID: TffDatabaseID;
      aOverWrite: boolean;
      Const aTableName: TfsTableName;
      aForServer: boolean;
      aDictionary: TFSInfoDict): TffResult; Override;
    Function TableDelete(aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName): TffResult; Override;
    Function TableDropIndex(aDatabaseID: TffDatabaseID;
      aCursorID: TffCursorID;
      Const aTableName: TfsTableName;
      Const aIndexName: TffDictItemName;
      aIndexID: Longint): TffResult; Override;
    Function TableEmpty(aDatabaseID: TffDatabaseID;
      aCursorID: TffCursorID;
      Const aTableName: TfsTableName): TffResult; Override;
    Function TableGetAutoInc(aCursorID: TffCursorID;
      Var aValue: Int64; Var aStep: Longint): TffResult; Override;
    Function TableGetMaxRecords(aCursorID: TffCursorID;
      Var aValue: Longword): TffResult; Override;
    Function TableGetTableFlags(aCursorID: TffCursorID;
      Var aValue: Word): TffResult; Override;
    Function TableGetTablePassword(aCursorID: TffCursorID;
      Var aValue: Longword): TffResult; Override;
    Function TableGetTablePasswordRest(aCursorID: TffCursorID;
      Var aValue: Longword): TffResult; Override;
    Function TableGetTableDBID(aCursorID: TffCursorID;
      Var aValue: Longword): TffResult; Override;
    Function TableGetDictionary(aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName;
      aForServer: boolean;
      aStream: TStream): TffResult; Override;
    Function TableGetRecCount(aCursorID: TffCursorID;
      Var aRecCount: Longword): TffResult; Override;
    Function TableGetRecCountAsync(aCursorID: TffCursorID; {!!.10}
      Var aTaskID: Longint): TffResult; Override; {!!.10}
    Function TableOpen(Const aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName;
      Const aForServer: boolean;
      Const aIndexName: TffName;
      aIndexID: Longint;
      Const aOpenMode: TffOpenMode;
      aShareMode: TffShareMode;
      Const aTimeout: Longint;
      Var aCursorID: TffCursorID;
      aStream: TStream;
      aSysOpen: boolean = True): TffResult; Override;
    Function TablePack(aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName;
      Var aRebuildID: Longint; UndeleteRecords: boolean; OnlyDeleted: boolean): TffResult; Override;
    Function TableRebuildIndex(aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName;
      Const aIndexName: TffName;
      aIndexID: Longint;
      Var aRebuildID: Longint): TffResult; Override;
    Function TableRename(aDatabaseID: TffDatabaseID; Const aOldName, aNewName: TffName): TffResult; Override;
    Function TableRestructure(aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName;
      aDictionary: TFSInfoDict;
      aFieldMap: TFSSpecStringList;
      Var aRebuildID: Longint;
      aRangeError: boolean): TffResult; Override;
    Function TableSetAutoInc(aCursorID: TffCursorID;
      aValue: Int64; aStep: Longint): TffResult; Override;
    Function TableSetMaxRecords(aCursorID: TffCursorID;
      aValue: Longint): TffResult; Override;
    Function TableSetTableFlags(aCursorID: TffCursorID;
      aValue: Word): TffResult; Override;
    Function TableSetTablePassword(aCursorID: TffCursorID;
      aValue: Longword): TffResult; Override;
    Function TableSetTablePasswordRest(aCursorID: TffCursorID;
      aValue: Longword): TffResult; Override;
    Function TableSetTableDBID(aCursorID: TffCursorID;
      aValue: Longword): TffResult; Override;
    {Begin !!.11}
    Function TableVersion(aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName;
      Var aVersion: Longint): TffResult; Override;
    {End !!.11}

          {table locks via cursor}
    Function TableIsLocked(aCursorID: TffCursorID; aLockType: TffLockType;
      Var aIsLocked: boolean): TffResult; Override;
    Function TableLockAcquire(aCursorID: TffCursorID; aLockType: TffLockType): TffResult; Override;
    Function TableLockRelease(aCursorID: TffCursorID; aAllLocks: Boolean): TffResult; Override;

    {cursor stuff}
    Function CursorClone(aCursorID: TffCursorID; aOpenMode: TffOpenMode;
      Var aNewCursorID: TffCursorID): TffResult; Override;
    Function CursorClose(aCursorID: TffCursorID): TffResult; Override;
    Function CursorCompareBookmarks(aCursorID: TffCursorID;
      aBookmark1,
      aBookmark2: PffByteArray;
      Var aCompResult: Longint): TffResult; Override;
    {Begin !!.02}
    Function CursorCopyRecords(aSrcCursorID,
      aDestCursorID: TffCursorID;
      aCopyBLOBs: Boolean; CountPerTrans: Longint): TffResult; Override;
    {End !!.02}
    Function CursorDeleteRecords(aCursorID: TffCursorID; CountPerTrans: Longint): TffResult; Override; {!!.06}
    Function CursorGetBookmark(aCursorID: TffCursorID; aBookmark: PffByteArray): TffResult; Override;

    Function CursorGetBookmarkSize(aCursorID: TffCursorID;
      Var aSize: Integer): TffResult; Override;
    Function CursorOverrideFilter(aCursorID: Longint;
      aExpression: pCANExpr;
      aTimeout: TffWord32): TffResult; Override;
    Function CursorResetRange(aCursorID: TffCursorID): TffResult; Override;
    Function CursorRestoreFilter(aCursorID: Longint): TffResult; Override;
    Function CursorSetRange(aCursorID: TffCursorID;
      aDirectKey: boolean;
      aFieldCount1: Integer;
      aPartialLen1: Integer;
      aKeyData1: PffByteArray;
      aKeyIncl1: boolean;
      aFieldCount2: Integer;
      aPartialLen2: Integer;
      aKeyData2: PffByteArray;
      aKeyIncl2: boolean): TffResult; Override;
    Function CursorSetTimeout(Const aCursorID: TffCursorID;
      Const aTimeout: Longint): TffResult; Override;
    Function CursorSetToBegin(aCursorID: TffCursorID): TffResult; Override;
    Function CursorSetToBookmark(aCursorID: TffCursorID; aBookmark: PffByteArray): TffResult; Override;
    Function CursorSetToCursor(aDestCursorID: TffCursorID; aSrcCursorID: TffCursorID): TffResult; Override;
    Function CursorSetToEnd(aCursorID: TffCursorID): TffResult; Override;
    Function CursorSetToKey(aCursorID: TffCursorID;
      aSearchAction: TffSearchKeyAction;
      aDirectKey: boolean;
      aFieldCount: Integer;
      aPartialLen: Integer;
      aKeyData: PffByteArray): TffResult; Override;
    Function CursorSwitchToIndex(aCursorID: TffCursorID;
      aIndexName: TffDictItemName;
      aIndexID: Integer;
      aPosnOnRec: boolean): TffResult; Override;
    Function CursorSetFilter(aCursorID: TffCursorID;
      aExpression: pCANExpr;
      aTimeout: TffWord32): TffResult; Override;

    {Begin !!.03}
    Function CursorListBLOBFreeSpace(aCursorID: TffCursorID;
      Const aInMemory: Boolean;
      aStream: TStream): TffResult; Override;
    {End !!.03}

          {record stuff}
    Function RecordDelete(aCursorID: TffCursorID; aData: PffByteArray): TffResult; Override;
    Function RecordDeleteBatch(aCursorID: TffCursorID;
      aBMCount: Longint;
      aBMLen: Longint;
      aData: PffByteArray;
      aErrors: PffLongintArray): TffResult; Override;
    Function RecordExtractKey(aCursorID: TffCursorID; aData: PffByteArray; aKey: PffByteArray): TffResult; Override;
    Function RecordGet(aCursorID: TffCursorID; aLockType: TffLockType; aUserLockType: TfsUserRecLocking; aData: PffByteArray; Var aFlag: Byte;
      Var aRefNr: TffInt64; Const aUser: Boolean): TffResult; Override;
    Function RecordGetBatch(aCursorID: TffCursorID;
      aRecCount: Longint;
      aRecLen: Longint;
      Var aRecRead: Longint;
      aData: PffByteArray;
      Var aError: TffResult): TffResult; Override;
    Function RecordGetForKey(aCursorID: TffCursorID;
      aDirectKey: boolean;
      aFieldCount: Integer;
      aPartialLen: Integer;
      aKeyData: PffByteArray;
      aData: PffByteArray;
      aFirstCall: Boolean): TffResult; Override;

    Function RecordGetSetPosition(aValue: Longint; aCursorID: TffCursorID;
      aLockType: TffLockType;
      aData: PffByteArray;
      Var aFlag: Byte;
      Var aRecNo: Longword;
      Var aRefNr: TffInt64;
      aInfoGetSetPosition: TInfoGetSetPosition;
      aSet: Boolean): TffResult; Override;

    Function RecordGetNext(aCursorID: TffCursorID; aLockType: TffLockType; aData: PffByteArray; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
      Override;
    Function RecordGetPrior(aCursorID: TffCursorID; aLockType: TffLockType; aData: PffByteArray; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
      Override;
    Function RecordInsert(aCursorID: TffCursorID; aLockType: TffLockType; aData: PffByteArray; aUndelete: Boolean; Var aRefNr: TffInt64): TffResult;
      Override;
    Function RecordInsertBatch(aCursorID: TffCursorID;
      aRecCount: Longint;
      aRecLen: Longint;
      aData: PffByteArray;
      aErrors: PffLongintArray): TffResult; Override;
    Function RecordIsLocked(aCursorID: TffCursorID; aLockType: TffLockType;
      Var aIsLocked: boolean): TffResult; Override;
    Function RecordModify(aCursorID: TffCursorID; aData: PffByteArray;
      aRelLock: Boolean; aUserLockType: TfsUserRecLocking; aFlag: Byte; aSet, Use: Boolean): TffResult; Override;
    Function RecordRelLock(aCursorID: TffCursorID; aAllLocks: Boolean): TffResult; Override;

    {BLOB stuff}
    Function BLOBCreate(aCursorID: TffCursorID;
      Var aBlobNr: TffInt64): TffResult; Override;
    Function BLOBDelete(aCursorID: TffCursorID; aBLOBNr: TffInt64): TffResult; Override;
    {Begin !!.03}
    Function BLOBListSegments(aCursorID: TffCursorID;
      aBLOBNr: TffInt64;
      aStream: TStream): TffResult; Override;
    {End !!.03}
    Function BLOBRead(aCursorID: TffCursorID;
      aFieldNo: TffWord32;
      aBLOBNr: TffInt64;
      aOffset: TffWord32; {!!.06}
      aLen: TffWord32; {!!.06}
      Var aBLOB;
      Var aBytesRead: TffWord32) {!!.06}
    : TffResult; Override;
    Function BLOBFree(aCursorID: TffCursorID; aBLOBNr: TffInt64;
      ReadOnly: boolean): TffResult; Override;
    Function BLOBGetLength(aCursorID: TffCursorID; aBLOBNr: TffInt64;
      Var aLength: Longint): TffResult; Override;
    Function BLOBTruncate(aCursorID: TffCursorID; aBLOBNr: TffInt64;
      aBLOBLength: Longint): TffResult; Override;
    Function BLOBWrite(aCursorID: TffCursorID;
      aFieldNo: TffWord32;
      aBLOBNr: TffInt64;
      aOffset: Longint;
      aLen: Longint;
      Var aBLOB): TffResult; Override;
    Function FileBLOBAdd(aCursorID: TffCursorID;
      Const aFileName: TffFullFileName;
      Var aBLOBNr: TffInt64): TffResult; Override;

    {query stuff}
    Function SQLAlloc(aClientID: TffClientID;
      aDatabaseID: TffDatabaseID;
      aTimeout: Longint;
      Var aStmtID: TffSqlStmtID): TffResult; Override;
    Function SQLExec(aStmtID: TffSqlStmtID;
      aOpenMode: TffOpenMode;
      Var aCursorID: TffCursorID;
      aStream: TStream): TffResult; Override;
    Function SQLExecDirect(aClientID: TffClientID;
      aDatabaseID: TffDatabaseID;
      aQueryText: PChar;
      aTimeout: Longint;
      aOpenMode: TffOpenMode;
      Var aCursorID: TffCursorID;
      aStream: TStream): TffResult; Override;
    Function SQLFree(aStmtID: TffSqlStmtID): TffResult; Override;
    Function SQLPrepare(aStmtID: TffSqlStmtID;
      aQueryText: PChar;
      aStream: TStream): TffResult; Override;
    Function SQLSetParams(aStmtID: TffSqlStmtID;
      aNumParams: Word;
      aParamDescs: Pointer;
      aDataBuffer: PffByteArray;
      aDataLen: Integer;
      aStream: TStream): TffResult; Override;

    {misc stuff}
    Function GetServerDateTime(Var aDateTime: TDateTime): TffResult; Override;
    {begin !!.10}
    Function GetServerSystemTime(Var aSystemTime: TSystemTime)
      : TffResult; Override;
    Function GetServerGUID(Var aGUID: TGUID)
      : TffResult; Override;
    Function GetServerID(Var aUniqueID: TGUID)
      : TffResult; Override;
    Function GetServerStatistics(Var aStats: TfsServerStatistics)
      : TffResult; Override;
    Function GetCommandHandlerStatistics(Const aCmdHandlerIdx: Integer;
      Var aStats: TfsCommandHandlerStatistics)
      : TffResult; Override;
    Function GetTransportStatistics(Const aCmdHandlerIdx: Integer;
      Const aTransportIdx: Integer;
      Var aStats: TfsTransportStatistics)
      : TffResult; Override;
    {end !!.10}

    Procedure LoadConfiguration(aCfg: String = '');
    Function SaveConfiguration(aCfg: String = ''): TffResult;
    {-Reads in the server configuration tables and processes the
      server script file (if present). }

  {properties}
    Property BufferManager: TfsBufferManager Read seBufMgr;
    Property ClientList: TfsSrcClientList Read seClientList;
    Property Configuration: TfsServerConfiguration
      Read seGetConfig;
    Property CursorList: TfsSrcCursorList Read seCursorList;
    Property DatabaseList: TfsSrcDatabaseList Read seDatabaseList;
    Property FolderList: TfsSrcFolderList Read seFolderList;
    Property ServerName: TffNetName Read seGetServerName;
    Property SessionList: TfsSrcSessionList Read seSessionList;
    Property TableList: TfsSrcTableList Read seTableList;
    Property CursorClass: TfsSrcCursorClass {!!.06}
    Read seCursorClass
      Write seCursorClass;
    Property SQLEngine: TFSBaseSQLEngine Read seSQLEngine;
    //Write seSetSQLEngine;

  Published

    Property CollectGarbageEnabled: Boolean
      Read seGetCollectGarbage
      Write seSetCollectGarbage
      Default True; {!!.01}
    { If True then the server engine is to perform garbage collection. }

    Property CollectFrequency: Longint
      Read seGetCollectFrequency
      Write seSetCollectFrequency
      Default fscl_CollectionFrequency; {!!.01}
    { The number of milliseconds between each garbage collection run by the
      server engine. }
    Property CollectCloseInactiveTables: Boolean Read seGetCloseInactiveTables Write sesetCloseInactiveTables Default True;
    Property CollectClearCache: Boolean Read seGetClearCache Write sesetClearCache Default True;

    Property TempPath: String {!!.10}
    Read seGetTempPath
      Write seSetTempPath;
    Property ConfigFile: String Read seGetConfigFile Write seSetConfigFile;
    Property MaxRAM: Longint Read seGetMaxRAM Write seSetMaxRAM Default 10;
    Property SecurityEnabled: Boolean
      Read seGetSecurityEnabled
      Write seSetSecurityEnabled;

    Property ClearCachePerCount: Longint Read seGetClearCachePerCount Write seSetClearCachePerCount Default 10000;
    Property ClearCacheIfUpdate: Boolean Read seGetClearCacheIfUpdate Write sesetClearCacheIfUpdate Default False;
    Property CloseInactiveTablesAfterCommitOrRoolback: Boolean Read seGetCloseInactiveTablesAfterCommitOrRoolback Write
      sesetCloseInactiveTablesAfterCommitOrRoolback Default True;

    Property OnRecoveryCheck: TNotifyEvent
      Read seOnRecoveryCheck
      Write seOnRecoveryCheck;
    { Called when the server engine is initializing and it is time to
      check for recovery of fail-safe transactions. }

    Property ScriptFile: String {!!.11}
    Read seGetScriptFile
      Write seSetScriptFile;

  End;

Var
  fsc_AdminUserID: String[11];
  fsc_AdminPasswd: TffWord32;
  fsc_QuestUserID: String[11];
  fsc_QuestPasswd: TffWord32;

Implementation

Uses
  TypInfo,
  Activex,
  fsllcomp,
  fsllcomm,
  fssrjour, {!!.06}
  fssqldb,
  fssrsort,
  fsutil,
  fssrcur,
  fssqleng;

Const
  fsc_NumBLOBBytesToCopy = fscl_1MB;
  { When copying BLOBs from one cursor to another, this is the initial number
    of bytes to read from the source BLOB. }
  fscl_FlushRate = 5 * 60 * 1000; {!!.01}
  { Flush memory pools and other pools every 5 minutes. }{!!.01}

Resourcestring
  fscTable = 'table %s';
  fscTableContent = 'content of table ''%s''';

  {===Utility functions================================================}

Function FFMapLock(Const aClientLock: TffLockType;
  Const isTableLock: boolean): TfsSrcLockType;
{-Map a client lock type to a server lock type. }
Begin
  Result := ffsltNone;
  If isTableLock Then
    Case aClientLock Of
      ffltNoLock: Result := ffsltShare;
      ffltReadLock: Result := ffsltIntentS;
      ffltWriteLock: Result := ffsltExclusive;
    End { case }
  Else If aClientLock = ffltWriteLock Then
    Result := ffsltExclusive
  Else
    Result := ffsltNone;
End;

{====================================================================}

Type
  { Base thread class for rebuild operations }
  TfsSrcRebuildBaseThread = Class(TfsThread)
  Protected { private }
  Protected
    rthServerEngine: TFSServer;
    rthParams: PffSrRebuildParams;
  Public
    rthRangeError: Boolean;
    rthUndeleteRecords, rhtOnlyDeleted: boolean;
    Constructor Create(aServerEngine: TFSServer;
      aRebuildParamsPtr: PffSrRebuildParams; aRangeError, UndeleteRecords, OnlyDeleted: Boolean);
    Destructor Destroy; Override;
  End;

  { Thread class for table reindexing operation }
  TfsSrcReindexThread = Class(TfsSrcRebuildBaseThread)
  Protected
    Procedure Execute; Override;
  End;

  { Thread class for table packing operation }
  TfsSrcPackThread = Class(TfsSrcRebuildBaseThread)
  Protected
    Procedure Execute; Override;
  End;

  { Thread class for table restructure operation }
  TfsSrcRestructureThread = Class(TfsSrcRebuildBaseThread)
  Protected
    Procedure Execute; Override;
  End;

  {Begin !!.10}
    { Thread class for asynchronous record count }
  TfsSrcGetRecordCountThread = Class(TfsSrcRebuildBaseThread)
  Protected
    Procedure Execute; Override;
  End;
  {End !!.10}

  {===TfsSrcReindexThread====================================================}

Constructor TfsSrcRebuildBaseThread.Create(
  aServerEngine: TFSServer;
  aRebuildParamsPtr: PffSrRebuildParams;
  aRangeError, UndeleteRecords, OnlyDeleted: Boolean);
Begin
  rthServerEngine := aServerEngine;
  rthParams := aRebuildParamsPtr;
  rthRangeError := aRangeError;
  rthUndeleteRecords := UndeleteRecords;
  rhtOnlyDeleted := OnlyDeleted;
  Inherited Create(False);
  FreeOnTerminate := True;
End;

Destructor TfsSrcRebuildBaseThread.Destroy;
Begin
  {Begin !!.13}
  If Assigned(rthParams.rpFieldMap) Then
    rthParams.rpFieldMap.Free;
  FFFreeMem(rthParams, SizeOf(rthParams^));
  {End !!.13}
  Inherited Destroy;
End;
{--------}

Procedure TfsSrcReindexThread.Execute;
Begin
  rthServerEngine.seTableRebuildIndexPrim(rthParams); {!!.13}
End;
{--------}

Procedure TfsSrcPackThread.Execute;
Begin
  rthServerEngine.seTablePackPrim(rthParams, False, rthUndeleteRecords, rhtOnlyDeleted); {!!.13}
End;
{--------}

Procedure TfsSrcRestructureThread.Execute;
Begin
  { Because we are passing a field map within the rebuild parameters,
    TablePackPrim knows that we are doing a restructure. }
  rthServerEngine.seTablePackPrim(rthParams, rthRangeError, False, False);
End;
{Begin !!.10}
{--------}

Procedure TfsSrcGetRecordCountThread.Execute;
Begin
  rthServerEngine.seTableGetRecordCountPrim(rthParams); {!!.13}
End;
{End !!.10}
{====================================================================}

{===TfsServerObject==================================================}

Constructor TfsServerObject.Create(Const aTimeout: Longint);
Begin
  Inherited Create;
  soState := ffosInactive;
  soTimeout := aTimeout;
End;
{--------}

Destructor TfsServerObject.Destroy;
Begin
  Inherited Destroy;
End;
{--------}

Function TfsServerObject.Activate: boolean;
Begin
  If soState In [ffosInactive, ffosActive] Then
    Begin
      If soClient = Nil Then
        soLock.Lock
      Else
        soClient.soLock.Lock;
      soState := ffosActive;
      Result := True;
    End
  Else
    Result := False;
End;
{--------}

Function TfsServerObject.CanClose(Const Mark: boolean): boolean;
Begin
  Result := (soState = ffosInactive) Or (soState = ffosClosing);
  { Note: If the state is ffosClosePending then the object is active &
          will be freed once it has completed.  Until then we have to
          leave it alone. }
  If (soState = ffosInactive) And Mark Then
    soState := ffosClosing;
End;
{--------}

Procedure TfsServerObject.Deactivate;
Begin
  Case soState Of
    ffosActive:
      soState := ffosInactive;
    ffosClosePending:
      Begin
        soState := ffosClosing;
        If Self.CanClose(True) Then
          Self.Free;
      End;
  End; { case }
  If soClient = Nil Then
    soLock.Unlock
  Else
    soClient.soLock.Unlock;
End;
{--------}

Procedure TfsServerObject.ForceClose;
Begin
  soState := ffosClosing;
End;
{--------}

Procedure TfsServerObject.RequestClose;
Begin
  If soState = ffosActive Then
    soState := ffosClosePending
  Else If soState = ffosInactive Then
    soState := ffosClosing;
End;
{--------}

Function TfsServerObject.ShouldClose: boolean;
Begin
  Result := (soState = ffosClosing);
End;
{====================================================================}

{===TfsServerObjectList==============================================}

Constructor TfsServerObjectList.Create;
Begin
  Inherited Create;
  solList := TFSSpecThreadList.Create;
End;
{--------}

Destructor TfsServerObjectList.Destroy;
Begin
  solList.Free;
  Inherited Destroy;
End;
{--------}

Procedure TfsServerObjectList.BeginRead;
Begin
  solList.BeginRead;
End;
{--------}

Procedure TfsServerObjectList.BeginWrite;
Begin
  solList.BeginWrite;
End;
{--------}

Function TfsServerObjectList.CanClose(Const Mark: boolean): boolean;
Var
  Inx: Longint;
Begin
  Result := True;
  For Inx := 0 To pred(solList.Count) Do
    Begin
      { If any one of the objects cannot be closed then return False.
        Note we have the option to tell each Inactive object to mark itself
        as closed.  This makes sure the object is unavailable until we actually
        free it. Note that we must call CanClose on each object. If we break
        out of this loop early, an object that should be closed may never
        be marked as closable. }
      If (Not TfsServerObject(solList[Inx]).CanClose(Mark)) Then
        Result := False;
    End;
End;
{--------}

Procedure TfsServerObjectList.EndRead;
Begin
  solList.EndRead;
End;
{--------}

Procedure TfsServerObjectList.EndWrite;
Begin
  solList.EndWrite;
End;
{--------}

Procedure TfsServerObjectList.ForceClose;
Var
  Inx: Longint;
Begin
  For Inx := 0 To pred(solList.Count) Do
    TfsServerObject(solList[Inx]).ForceClose;
End;
{Begin !!.06}
{--------}

Function TfsServerObjectList.HasClosableState(Const Mark: Boolean): boolean;
Var
  Inx: Longint;
Begin
  Result := True;
  For Inx := 0 To pred(solList.Count) Do
    Begin
      { If any one of the objects cannot be closed then return False. }
      If Not (TfsServerObject(solList[Inx]).State In
        [ffosInactive, ffosClosing]) Then
        Begin
          Result := False;
          Break;
        End;
    End;

  { If all objects are in a closable state and we are to mark them as being
    closed then do so. }
  If Result And Mark Then
    For Inx := 0 To pred(solList.Count) Do
      If TfsServerObject(solList[Inx]).State = ffosInactive Then
        TfsServerObject(solList[Inx]).State := ffosClosing;

End;
{End !!.06}
{--------}

Procedure TfsServerObjectList.RemoveUnused;
Var
  Index: Longint;
Begin
  solList.BeginWrite;
  Try
    For Index := pred(solList.Count) Downto 0 Do
      {Begin !!.05}
      Try
        If TfsServerObject(solList[Index]).ShouldClose Then
          solList.DeleteAt(Index);
      Except
        { If an exception occurred then it is most likely because the object
          has already been deleted. Remove the invalid object from the list. }
        solList.RemoveAt(Index);
      End;
    {End !!.05}
  Finally
    solList.EndWrite;
  End;
End;
{Begin !!.03}
{--------}

Procedure TfsServerObjectList.RequestClose;
Var
  Inx: Longint;
Begin
  For Inx := 0 To pred(solList.Count) Do
    TfsServerObject(solList[Inx]).RequestClose;
End;
{End !!.03}
{--------}

Function TfsServerObjectList.ShouldClose: boolean;
Var
  Inx: Longint;
Begin
  Result := True;
  For Inx := 0 To pred(solList.Count) Do
    Begin
      { If any one of the objects cannot be closed then return False. }
      If (Not TfsServerObject(solList[Inx]).ShouldClose) Then
        Begin
          Result := False;
          break;
        End;
    End;
End;
{====================================================================}

{===TfsSrBaseCursor==================================================}

Constructor TfsSrBaseCursor.Create(anEngine: TFSServer;
  aDatabase: TfsSrcDatabase;
  Const aTimeout: Longint);
Begin
  Inherited Create(aTimeout);
  soClient := aDatabase.Client;
  bcCloseTable := False;
  bcCloseWTrans := False; {!!.05}
  bcDatabase := aDatabase;
  bcEngine := anEngine;
  bcExclOwner := False;
  bcExtenders := Nil;
  bcInfoLock := TfsPadlock.Create; {!!.06}
  bcTable := Nil;
  bcTempStore := Nil;
  bcNumReadLocks := 0; {!!.05}
  bcInterpretator := TCursorInterpretator.Create;
End;
{Begin !!.01}
{--------}

Function TfsSrBaseCursor.CanClose(Const Mark: Boolean): Boolean;
Begin
  { Cursor can be closed if it is not in a transaction or if the table is
    temporary. }
  Result := (bcDatabase.Transaction = Nil) Or
    (fffaTemporary In bcTable.Files[0].fiAttributes);
  If Result Then
    Result := Inherited CanClose(Mark) {!!.05 - Start}
  Else If (bcDatabase.Transaction <> Nil) Then
    bcCloseWTrans := True; {!!.05 - End}
End;
{End !!.01}
{--------}

Procedure TfsSrBaseCursor.Open(Const aTableName: TfsTableName;
  Const aIndexName: TffName;
  Const aIndexID: Longint;
  Const aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  aForServer: boolean;
  Const aExclContLock: Boolean; {!!.10}
  aAttribs: TffFileAttributes; SysOpen: Boolean);
Var
  aFlag: Byte;
  aLockType: TfsSrcLockType;
  NewTable: boolean;
  OpenIntentRegistered: Boolean;
  CurEr: TffResult;
  Passwd: Longword;
  aRefNr: TffInt64;
  CACTIVE, tmpS: String; // czy aktywny
  RELATIONSOURCE: String; // aTableName
  Flags: Word;
  MaxR: Longword;

  TransBegin: boolean;

  Procedure OpenConstraint;
  Var
    SearchPath: TffPath;
    Cur: TfsSrBaseCursor;
    RELATIONNAME: String; // Cascadedelete ...
    RELATIONPOSITION: Integer;
    RELATIONSOURCEFIELD: String; // aTableName field
    RELATIONDESTINATION: String; //destTableName
    RELATIONDESTINATIONFIELD: String; //destTableName  field
    BEFORECONSTRAINT: String;
    AFTERCONSTRAINT: String;

    Procedure GetTable(cur: TfsSrBaseCursor);
    Var
      OldData: PffByteArray;
      RecLen: Integer;
      FieldBuffer: PffByteArray;
      IsNull: boolean;
      SQLResult, n: TffResult;
      Rec: Variant;
      iFld: Integer;
      Cstr: String;
    Begin
      Rec := null;
      If cur <> Nil Then
        Begin
          Self.table.Dictionary.CascadeDelete.Clear;
          Self.table.Dictionary.CascadeDeleteNull.Clear;
          Self.table.Dictionary.CascadeUpdateNull.Clear;
          Self.table.Dictionary.CascadeInsert.Clear;
          Self.table.Dictionary.CascadeUpdate.Clear;
          Self.table.Dictionary.RestrictUpdate.Clear;
          Self.table.Dictionary.RestrictDelete.Clear;
          Self.table.Dictionary.NoActionUpdate.Clear;
          Cur.SetToBegin;
          RecLen := cur.Dictionary.RecordLength;
          FFGetMem(OldData, RecLen);
          Try
            TransBegin := False;
            Try
              If cur.Database.TransactionID = 0 Then
                Begin
                  cur.Engine.TransactionStartSQL(cur.DataBase.DatabaseID, False);
                  TransBegin := True;
                End;
              n := 0;
              SQLResult := 0;
              n := cur.GetNextRecord(Nil, ffsltNone, aflag, arefnr);
              If n = 0 Then
                While (Not (cur.Position = cpEOF)) Do
                  Begin
                    Cstr := '';
                    CACTIVE := '';
                    RELATIONNAME := '';
                    RELATIONPOSITION := 0;
                    RELATIONSOURCE := '';
                    RELATIONSOURCEFIELD := '';
                    RELATIONDESTINATION := '';
                    RELATIONDESTINATIONFIELD := '';
                    BEFORECONSTRAINT := '';
                    AFTERCONSTRAINT := '';
                    SQLResult := cur.GetRecord(OldData, ffsltNone, tluDatabase, aflag, arefnr, False);
                    For iFld := 0 To 7 Do
                      Begin
                        FFGetMem(FieldBuffer, cur.Dictionary.FieldLength[iFld]);
                        Try
                          If SQLResult = DBIERR_NONE Then
                            Begin
                              cur.Dictionary.GetRecordField(iFld, OldData, IsNull, FieldBuffer);
                              If IsNull Then
                                Rec := Null
                              Else
                                Begin
                                  Rec := fsCurGetValue(Cur.CursorID, iFld, OldData, FieldBuffer, False);
                                  Case iFld Of
                                    0: CACTIVE := UpperCase(Trim(Rec));
                                    2: RELATIONNAME := UpperCase(Trim(Rec));
                                    3: RELATIONPOSITION := Rec;
                                    4: RELATIONSOURCE := UpperCase(Trim(Rec));
                                    5: RELATIONSOURCEFIELD := UpperCase(Trim(Rec));
                                    6: RELATIONDESTINATION := UpperCase(Trim(Rec));
                                    7: RELATIONDESTINATIONFIELD := UpperCase(Trim(Rec));
                                    //9: BEFORECONSTRAINT:= fsGetBlobValue( Cur.CursorID, 9, OldData );
                                   //10: AFTERCONSTRAINT:=  fsGetBlobValue( Cur.CursorID, 10, OldData );
                                  End;
                                End;
                            End;
                        Finally
                          FFFreeMem(FieldBuffer, cur.Dictionary.FieldLength[iFld]);
                        End;
                      End; //For iFld := 0 To 6 Do
                    If (CACTIVE = 'A') And (RELATIONSOURCE = AnsiUpperCase(ATableName)) Then
                      Begin
                        // funName, tblName, SfldName, DfldName
                        Cstr := RELATIONNAME + ';' + RELATIONDESTINATION + ';' + RELATIONSOURCEFIELD + ';' + RELATIONDESTINATIONFIELD;
                        If Length(Cstr) > 3 Then
                          Begin
                            If RELATIONNAME = 'CASCADEDELETE' Then
                              Self.table.Dictionary.CascadeDelete.Add(Cstr)
                            Else If RELATIONNAME = 'CASCADEDELETENULL' Then
                              Self.table.Dictionary.CascadeDeleteNull.Add(Cstr)
                            Else If RELATIONNAME = 'CASCADEINSERT' Then
                              Self.table.Dictionary.CascadeInsert.Add(Cstr)
                            Else If RELATIONNAME = 'CASCADEUPDATE' Then
                              Self.table.Dictionary.CascadeUpdate.Add(Cstr)
                            Else If RELATIONNAME = 'CASCADEUPDATENULL' Then
                              Self.table.Dictionary.CascadeUpdateNull.Add(Cstr)
                            Else If RELATIONNAME = 'RESTRICTUPDATE' Then
                              Self.table.Dictionary.RestrictUpdate.Add(Cstr)
                            Else If RELATIONNAME = 'RESTRICTDELETE' Then
                              Self.table.Dictionary.RestrictDelete.Add(Cstr)
                            Else If RELATIONNAME = 'TARGETREQUIRED' Then
                              Self.table.Dictionary.NoActionUpdate.Add(Cstr);
                          End;
                      End
                    Else If (CACTIVE <> 'A') Then
                      break;

                    // next
                    n := cur.GetNextRecord(Nil, ffsltNone, aflag, arefnr);
                  End;
            Except
              If TransBegin Then
                Begin
                  TransBegin := False;
                  cur.Engine.TransactionRollbackSQL(cur.DataBase.DatabaseID, False);
                End;
            End;

          Finally
            FFFreeMem(OldData, RecLen);
            If TransBegin Then
              cur.Engine.TransactionCommitSQL(cur.DataBase.DatabaseID, False);
          End;
        End;
    End;
  Begin
    If Self.Engine.Configuration.GeneralInfo^.giEnabledReferential Then
      Begin
        SearchPath := bcDatabase.Folder.Path;
        If (SearchPath[length(SearchPath)] <> '\') Then
          FFShStrAddChar(SearchPath, '\');
        If FFFileExists(SearchPath + FFForceExtension('sys$constraints', fsc_ExtForData)) Then
          Begin
            Cur := Self.Engine.CursorClass.Create(Self.Engine, Self.DataBase, Timeout);
            If Cur <> Nil Then
              Begin
                Try
                  CurEr := 0;
                  Cur.Open('sys$constraints', 'RELATION', 0, omReadOnly, smShared, False, False, [], False);
                Except
                  CurEr := DBIERR_NOCURRREC;
                  If Assigned(Cur) Then
                    Cur.free;
                  cur := Nil;
                  Raise;
                End;
                If (CurEr = 0) Then
                  GetTable(Cur);
                If Assigned(Cur) Then
                  Cur.free;
                cur := Nil;
              End;
          End;
      End;
  End;

  Procedure OpenTrigers;
  Var
    SearchPath: TffPath;
    Cur: TfsSrBaseCursor;
    CACTIVE: String;
    CTABLENAME: String;
    CMETODENAME: String;

    Procedure GetTable(cur: TfsSrBaseCursor);
    Var
      OldData: PffByteArray;
      RecLen: Integer;
      FieldBuffer: PffByteArray;
      IsNull: boolean;
      SQLResult, n: TffResult;
      Rec: Variant;
      iFld: Integer;
      Cstr: String;
    Begin
      Rec := null;
      If cur <> Nil Then
        Begin
          Self.table.Dictionary.TrigersBeforeInsert.Clear;
          Self.table.Dictionary.TrigersBeforeInsertOperation.Clear;
          Self.table.Dictionary.TrigersAfterInsert.Clear;
          Self.table.Dictionary.TrigersAfterInsertOperation.Clear;

          Self.table.Dictionary.TrigersBeforeUpdate.Clear;
          Self.table.Dictionary.TrigersBeforeUpdateOperation.Clear;
          Self.table.Dictionary.TrigersAfterUpdate.Clear;
          Self.table.Dictionary.TrigersAfterUpdateOperation.Clear;

          Self.table.Dictionary.TrigersBeforeDelete.Clear;
          Self.table.Dictionary.TrigersBeforeDeleteOperation.Clear;
          Self.table.Dictionary.TrigersAfterDelete.Clear;
          Self.table.Dictionary.TrigersAfterDeleteOperation.Clear;

          Cur.SetToBegin;
          RecLen := cur.Dictionary.RecordLength;
          FFGetMem(OldData, RecLen);
          Try
            TransBegin := False;
            Try
              If cur.Database.TransactionID = 0 Then
                Begin
                  cur.Engine.TransactionStartSQL(cur.DataBase.DatabaseID, False);
                  TransBegin := True;
                End;
              n := 0;
              SQLResult := 0;
              n := cur.GetNextRecord(Nil, ffsltNone, aflag, arefnr);
              If n = 0 Then
                While (Not (cur.Position = cpEOF)) Do
                  Begin
                    Cstr := '';
                    CACTIVE := ''; //ACTIVE
                    CTABLENAME := '';
                    CMETODENAME := '';

                    SQLResult := cur.GetRecord(OldData, ffsltNone, tluDatabase, aflag, arefnr, False);
                    For iFld := 0 To 3 Do
                      Begin
                        FFGetMem(FieldBuffer, cur.Dictionary.FieldLength[iFld]);
                        Try
                          If SQLResult = DBIERR_NONE Then
                            Begin
                              cur.Dictionary.GetRecordField(iFld, OldData, IsNull, FieldBuffer);
                              If IsNull Then
                                Rec := Null
                              Else
                                Begin
                                  Rec := fsCurGetValue(Cur.CursorID, iFld, OldData, FieldBuffer, False);
                                  Case iFld Of
                                    0: CACTIVE := UpperCase(Trim(Rec));
                                    2: CTABLENAME := UpperCase(Trim(Rec));
                                    3: CMETODENAME := UpperCase(Trim(Rec));
                                    //3: RELATIONSOURCE := UpperCase(Trim(Rec));//code
                                  End;
                                End;
                            End;
                        Finally
                          FFFreeMem(FieldBuffer, cur.Dictionary.FieldLength[iFld]);
                        End;
                      End; //For iFld := 0 To 2 Do
                    If (CACTIVE = 'A') And ((CTABLENAME = AnsiUpperCase(ATableName)) Or (CTABLENAME = '')) Then
                      Begin
                        Begin
                          tmpS := Trim(fsGetBlobValue(Cur.CursorID, 5, OldData));
                          If CMETODENAME = 'BEFOREINSERT' Then
                            Begin
                              If tmpS <> '' Then
                                Self.table.Dictionary.TrigersBeforeInsert.Add(tmpS)
                            End
                          Else If CMETODENAME = 'AFTERINSERT' Then
                            Begin
                              If tmpS <> '' Then
                                Self.table.Dictionary.TrigersAfterInsert.Add(tmpS)
                            End
                          Else If CMETODENAME = 'BEFOREUPDATE' Then
                            Begin
                              If tmpS <> '' Then
                                Self.table.Dictionary.TrigersBeforeUpdate.Add(tmpS)
                            End
                          Else If CMETODENAME = 'AFTERUPDATE' Then
                            Begin
                              If tmpS <> '' Then
                                Self.table.Dictionary.TrigersAfterUpdate.Add(tmpS)
                            End
                          Else If CMETODENAME = 'BEFOREDELETE' Then
                            Begin
                              If tmpS <> '' Then
                                Self.table.Dictionary.TrigersBeforeDelete.Add(tmpS)
                            End
                          Else If CMETODENAME = 'AFTERDELETE' Then
                            Begin
                              If tmpS <> '' Then
                                Self.table.Dictionary.TrigersAfterDelete.Add(tmpS)
                            End
                          Else If CMETODENAME = 'BEFOREINSERTOPERATION' Then
                            Begin
                              If tmpS <> '' Then
                                Self.table.Dictionary.TrigersBeforeInsertOperation.Add(tmpS)
                            End
                          Else If CMETODENAME = 'AFTERINSERTOPERATION' Then
                            Begin
                              If tmpS <> '' Then
                                Self.table.Dictionary.TrigersAfterInsertOperation.Add(tmpS)
                            End
                          Else If CMETODENAME = 'BEFOREUPDATEOPERATION' Then
                            Begin
                              If tmpS <> '' Then
                                Self.table.Dictionary.TrigersBeforeUpdateOperation.Add(tmpS)
                            End
                          Else If CMETODENAME = 'AFTERUPDATEOPERATION' Then
                            Begin
                              If tmpS <> '' Then
                                Self.table.Dictionary.TrigersAfterUpdateOperation.Add(tmpS)
                            End
                          Else If CMETODENAME = 'BEFOREDELETEOPERATION' Then
                            Begin
                              If tmpS <> '' Then
                                Self.table.Dictionary.TrigersBeforeDeleteOperation.Add(tmpS)
                            End
                          Else If CMETODENAME = 'AFTERDELETEOPERATION' Then
                            Begin
                              If tmpS <> '' Then
                                Self.table.Dictionary.TrigersAfterDeleteOperation.Add(tmpS);
                            End;
                        End;
                      End
                    Else If (CACTIVE <> 'A') Then
                      break;
                    // next
                    n := cur.GetNextRecord(Nil, ffsltNone, aflag, arefnr);
                  End;
            Except
              If TransBegin Then
                Begin
                  TransBegin := False;
                  cur.Engine.TransactionRollbackSQL(cur.DataBase.DatabaseID, False);
                End;
            End;

          Finally
            FFFreeMem(OldData, RecLen);
            If TransBegin Then
              cur.Engine.TransactionCommitSQL(cur.DataBase.DatabaseID, False);
          End;
        End;
    End;
  Begin
    If Self.Engine.Configuration.GeneralInfo^.giEnabledTrigers Then
      Begin
        SearchPath := bcDatabase.Folder.Path;
        If (SearchPath[length(SearchPath)] <> '\') Then
          FFShStrAddChar(SearchPath, '\');
        If FFFileExists(SearchPath + FFForceExtension('sys$triggers', fsc_ExtForData)) Then
          Begin
            Cur := Self.Engine.CursorClass.Create(Self.Engine, Self.DataBase, Timeout);
            If Cur <> Nil Then
              Begin
                Try
                  CurEr := 0;
                  Cur.Open('sys$triggers', 'RELATION', 0, omReadOnly, smShared, False, False, [], False);
                Except
                  CurEr := DBIERR_NOCURRREC;
                  If Assigned(Cur) Then
                    Cur.free;
                  cur := Nil;
                  Raise;
                End;
                If (CurEr = 0) Then
                  GetTable(Cur);
                If Assigned(Cur) Then
                  Cur.free;
                cur := Nil;
              End;
          End;
      End;
  End;

Begin
  bcIndexID := aIndexID;
  TableName := AnsiUpperCase(aTableName);
  NewTable := False;
  OpenIntentRegistered := False;
  { The cursor references an instance of TfsSrcBaseTable. Multiple cursors may
    reference that same instance of TfsSrcBaseTable since only 1 instance of
    TfsSrcBaseTable is created per physical table (saves on file handles).

    So we must determine whether the table has already been opened by
    another cursor.  But first, we must obtain write access on the engine's
    table list.  Why?

    1. If the table has not been opened, we don't want two threads trying
       to open it at the same time.  Should that occur, we would wind
       up with duplicate tables in our table list.

    2. If the table is already open, we don't thread A closing the table
       while thread B is trying to "open" the table.  Good recipe for an
       access violation.

    Complication: If the table is open and locked, and this thread wants
      to open the table in an incompatible lock mode, we must make sure
      the table list is available to other threads.  Otherwise, we will
      freeze the server. }

  bcEngine.TableList.BeginWrite;
  Try
    { Try & find the open table in the engine's table list. If it exists already
      then reference the existing table. }
    bcTable := Nil;
    bcTable := bcEngine.GetTableInstance(bcDatabase.Folder, aTableName);

    { Is the table open? }
    If assigned(bcTable) Then
      Begin
        { Yes. Register our intent to open the table.  This prevents another
          thread from freeing the table. }
        bcTable.RegisterOpenIntent;
        OpenIntentRegistered := True;

        { Release our lock on the table list.  We must do so because our
          request for a lock on the table may cause this thread to wait.
          Retaining the lock in such a situation would freeze any threads
          wanting access to the table list. }
        bcEngine.TableList.EndWrite;

        { Determine the type of lock for the table, based upon the Open mode and
          Share mode. }
        If (aShareMode = smExclusive) Then
          aLockType := ffsltExclusive
        Else If (aOpenMode = omReadOnly) Then
          { Table is to be opened as Read-only. }
          aLockType := ffsltShare
        Else
          { Table is to be opened as Read-Write. }
          aLockType := ffsltIntentS;

        { Acquire the lock.  We will return from this call when the lock
          is granted.  Otherwise an exception will be raised (i.e., another
          thread has the table locked exclusively and isn't giving it
          up). }
        bcTable.AcqLock(CursorID, aLockType);
      End
    Else
      Begin
        { No, it is not open. Open it now. }
        Try
          bcTableOpenPrim(bcDatabase, aTableName, aOpenMode, aShareMode,
            aForServer, aAttribs);
        Except
          bcEngine.TableList.EndWrite;
          Raise;
        End;

        NewTable := True;
        bcTable.RegisterOpenIntent; {!!.01}
        OpenIntentRegistered := True; {!!.01}
      End;

    { Make sure we meet all requirements for opening the table. }
    Try
      bcTableOpenPreconditions(bcTable, aIndexName, bcIndexID, aOpenMode);
    Except
      { If we created a new table then get rid of it. }
      If NewTable Then
        Begin
          If OpenIntentRegistered Then {!!.02}
            bcTable.DeregisterOpenIntent; {!!.02}
          bcTable.Free;
          bcTable := Nil;
        End;
      Raise;
    End;

    { Add the newly opened table to the server table list. }
    If NewTable Then
      bcEngine.TableList.AddTable(bcTable);

    bcInit(aOpenMode, aShareMode, aExclContLock); {Moved !!.01}

  Finally
    { If the table was not already opened then we still have the tableList
      locked.  Unlock it. }

    If NewTable Then
      bcEngine.TableList.EndWrite; {!!.01}
    If (bcTable <> Nil) And OpenIntentRegistered Then {!!.02}
      { If we registered our intent to open then deregister our intent. }
      bcTable.DeregisterOpenIntent;
    {1.0000 opening table system KW FSSQL}
    If SysOpen And NewTable Then
      Begin
        Try
          If (TableName <> 'SYS$CONSTRAINTS') And (TableName <> 'SYS$TRIGGERS') Then
            Begin
              OpenConstraint;
              OpenTrigers;
            End;
        Except
          Raise;
        End;
      End;
    If (Not aForServer) Then
      //and (TableName <> 'SYS$CONSTRAINTS') And (TableName <> 'SYS$TRIGGERS') Then
      Begin
        Passwd := 0;
        Self.ReadTablePassword(Passwd);
        If Passwd > 0 Then
          If Not Self.bcDatabase.Session.TablePasswordList.Exists(IntToStr(Passwd)) Then
            FSRaiseException(EfsException, fsStrResServer, 50100,
              [TableName, bcDatabase.Alias]);
        If (bcTable <> Nil) Then
          Begin
            Self.ReadTableFlags(Flags);
            bcTable.TableFlags := Flags;
            Self.ReadMaxRecords(MaxR);
            bcTable.MaxRecords := MaxR;
          End;
      End;
    Self.Dictionary.IsAnyRound := bcTable.Dictionary.IsAnyRound;
    Self.Dictionary.IsAnyEmptyAsNull := bcTable.Dictionary.IsAnyEmptyAsNull;
    Self.Dictionary.IsAnyDefault := bcTable.Dictionary.IsAnyDefault;
    Self.Dictionary.IsRecVersion := bcTable.Dictionary.IsRecVersion;
  End;
End;
{--------}

Procedure TfsSrBaseCursor.Build(Const aTableName: TfsTableName;
  aDict: TFSInfoDict;
  Const aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  aForServer: boolean;
  aOverWrite: boolean;
  aAttribs: TffFileAttributes;
  aStoreSize: TffWord32);
Var
  aLockType: TfsSrcLockType;
  aTransID: TffTransID;
  OpenIntentRegistered: Boolean; {!!.10}
  TableDataFile: TffFileNameExt;
  TmpTableName: TfsTableName;
Begin
  bcIndexID := 0;
  OpenIntentRegistered := False; {!!.10}
  TmpTableName := aTableName;
  If (fffaTemporary In aAttribs) Then
    Begin
      { Requirement: If the temporary file attribute is specified, the table must
        have a block size of 64k. This is due to temporary storage (unit FFLLTEMP)
        being restricted to 64k blocks of data. }
      If (aDict.BlockSize < (64 * 1024)) Then
        aDict.BlockSize := (64 * 1024);

      { If no tablename specified then generate a unique table name. }
      If TmpTableName = '' Then
        TmpTableName := IntToStr(Longint(Self));
    End;
  TableName := AnsiUpperCase(TmpTableName);
  { Obtain write access to the table list.  Our purpose is to make sure
    the table is not opened.  By obtaining write access, we prevent other
    threads from creating or opening the table. }
  bcEngine.TableList.BeginWrite;
  Try

    { Was a tablename specified? }
    If aTableName <> '' Then
      Begin
        { Yes. It is possible the table may already exist.
          Try and find the open table in our list. If it exists already
          obviously there's an error (we can't build a new table when it's
          already open). }
        bcTable := bcEngine.GetTableInstance(bcDatabase.Folder, TmpTableName);
        If assigned(bcTable) Then
          FSRaiseException(EfsException, fsStrResServer, fserrTableOpen,
            [TmpTableName]);

        { The table name must be a valid file name without extension. }
        If Not FFVerifyFileName(TmpTableName) Then
          FSRaiseException(EfsException, fsStrResServer, fserrInvalidTableName,
            [TmpTableName]);

        { Is this a temporary table? }
        If Not (fffaTemporary In aAttribs) Then
          Begin
            { No. The table's data file cannot exist within the database. }
            TableDataFile := FFMakeFileNameExt(TmpTableName, fsc_ExtForData);
            If FFFileExists(FFMakeFullFileName(bcDatabase.Folder.Path,
              TableDataFile)) Then
              Begin
                If aOverWrite Then
                  { We want to overwrite this table - we have to delete it first. }
                  bcEngine.seDeleteTable(bcDatabase, TmpTableName)
                Else
                  FSRaiseException(EfsException, fsStrResServer, fserrTableExists,
                    [TmpTableName]);
              End;
          End;
      End;

    { Is this cursor to have its own temporary storage? }
    If aStoreSize > 0 Then
      bcTempStore := fscTempStorageClass.Create(bcEngine.TempPath,
        aStoreSize, 64 * 1024, Engine.Configuration.GeneralInfo^.giEncryptTempStorage)
    Else
      bcTempStore := Nil;

    { Create the table. }
    bcTable := bcTableClass.Create(bcEngine, TmpTableName, bcDatabase.Folder,
      bcEngine.BufferManager, omReadWrite);

    Try
      bcTable.RegisterOpenIntent; {!!.10}
      OpenIntentRegistered := True; {!!.10}
      bcEngine.seTransactionStart(bcDatabase, False, True, aTransID);
      Try
        { Create the files comprising the table. }
        bcTable.BuildFiles(bcDatabase.TransactionInfo, aForServer, aDict,
          aAttribs, bcTempStore);
        bcEngine.seTransactionCommit(bcDatabase);
      Except
        bcEngine.seTransactionRollback(bcDatabase);
        Raise;
      End;

      { Acquire the right type of lock on the table. }
      If aShareMode = smExclusive Then
        aLockType := ffsltExclusive
      Else If aOpenMode = omReadOnly Then
        aLockType := ffsltShare
      Else
        aLockType := ffsltIntentS;

      bcTable.AcqLock(CursorID, aLockType);

      bcTableOpenPreconditions(bcTable, '', bcIndexID, aOpenMode);
    Except
      { Destroy the table object. This will close all the files. }
      bcTable.DeregisterOpenIntent;
      bcTable.Free;
      bcTable := Nil;
      Raise;
    End; {try..finally}

    bcEngine.TableList.AddTable(bcTable);
    bcInit(aOpenMode, aShareMode, False); {!!.10}
    Self.Dictionary.IsAnyRound := aDict.IsAnyRound;
    Self.Dictionary.IsAnyEmptyAsNull := aDict.IsAnyEmptyAsNull;
    Self.Dictionary.IsAnyDefault := aDict.IsAnyDefault;
    Self.Dictionary.IsRecVersion := aDict.IsRecVersion;
  Finally
    bcEngine.TableList.EndWrite;
    If assigned(bcTable) And OpenIntentRegistered Then {!!.10}
      bcTable.DeregisterOpenIntent; {!!.10}
  End;
End;
{--------}

Destructor TfsSrBaseCursor.Destroy;
Var
  anExtender: TFSBaseEngineExtender;
  anIndex: Longint;
Begin
  bcEngine.TableList.BeginWrite; {!!.10}
  Try
    { Assumption: If cursor is being closed in the context of a transaction then
      the changes made to the table should be saved. We will retain the cursor's
      locks in the lock manager so that no other cursors can access those
      records. The changes to the table will stay in memory until the transaction
      commits or rolls back. }

    { If still have a record locked from TfsTable.Edit then release the lock. }
    If Not FFI64IsZero(bcLockedRefNum) Then
      bcTable.RemoveLocksForCursor(bcDatabase.DatabaseID,
        CursorID, bcLockedRefNum,
        bcDatabase.TransactionInfo);

    If (bcRecordData <> Nil) Then {!!.01}
      FFFreeMem(bcRecordData, bcRecordLen); {!!.01}

    If bcBLOBCursors <> Nil Then
      bcBLOBCursors.Free;
    bcBLOBCursors := Nil;
    If bcExclOwner Then
      Begin
        bcTable.SetExclOwner(fsc_W32NoValue);
        bcExclOwner := False;
      End;

    If assigned(bcExtenders) Then
      Begin
        For anIndex := pred(bcExtenders.Count) Downto 0 Do
          Begin
            anExtender := TFSBaseEngineExtender
              (TfsIntListItem(bcExtenders[anIndex]).KeyAsInt);
            anExtender.Free;
          End;
        bcExtenders.Free;
      End;

    If bcTable <> Nil Then
      Begin
        bcTable.CursorList.BeginWrite;
        Try
          bcTable.CursorList.RemoveCursor(CursorID);
        Finally
          bcTable.CursorList.EndWrite;
        End;
        If bcCloseTable Then
          bcEngine.TableList.RemoveIfUnused(bcTable);
      End;

    bcTempStore.Free;
    bcInfoLock.Free; {!!.06}

  Finally
    bcEngine.TableList.EndWrite; {!!.10}
    bcInterpretator.free;
    Inherited Destroy;
  End;
End;
{--------}

Procedure TfsSrBaseCursor.AcqContentLock(Const aMode: TfsContentLockMode; Const aUserLockType: TfsUserRecLocking = tluDatabase); {!!.10}
{ NOTE:: If you change this method then look at AcqContentLockCond for similar
  changes. }
Begin
  //xxlock
  If (fffaBLOBChainSafe In bcGetAttribs) Or {!!.05}
  (bcExclOwner And (Not bcTable.Dictionary.HasBLOBFields)) Then {!!.03} {!!.05}
    Exit;
  Assert(assigned(bcDatabase.Transaction) Or
    (aMode = fsclmRead));

  { Is a transaction active? }
  If assigned(bcDatabase.Transaction) Then
    { Yes.  Call the appropriate table method. }
    Case aMode Of
      fsclmCommit:
        bcTable.BeginCommit;
      fsclmRead:
        bcTable.AcqContentLock(bcDatabase.Transaction, ffsltShare, False, aUserLockType);
      fsclmWrite:
        bcTable.AcqContentLock(bcDatabase.Transaction, ffsltExclusive, False, aUserLockType);
    End { case }
  Else
    Begin {!!.05 - Start}
      { No transaction.  This should be a reader thread that wants read access. }
      If (bcNumReadLocks = 0) Then
        bcTable.BeginRead;
      InterlockedIncrement(bcNumReadLocks);
    End; {!!.05 - End}
End;
{Begin !!.10}
{--------}

Function TfsSrBaseCursor.AcqExclContentLock: TffResult;
{ NOTE:: If you change this method then look at AcqContentLock for similar
  changes. }
Begin
  If Not ((fffaBLOBChainSafe In bcGetAttribs) Or
    (bcExclOwner And (Not bcTable.Dictionary.HasBLOBFields))) Then
    Begin
      Assert(assigned(bcDatabase.Transaction));
      Result := bcTable.AcqExclContentLock(bcDatabase.Transaction);
    End
  Else
    Result := DBIERR_NONE;
End;
{End !!.10}
{--------}

Procedure TfsSrBaseCursor.AppendNewRecord(aData: PffByteArray);
Var
  aRefNr: TffInt64;
Begin
  AcqContentLock(fsclmWrite, tluDatabase);
  InsertRecord(aData, ffsltExclusive, 0, aRefNr);
End;
{--------}

Procedure TfsSrBaseCursor.bcAddExtender(anExtender: TFSBaseEngineExtender);
Var
  anItem: TfsIntListItem;
Begin
  If assigned(anExtender) Then
    Begin
      If Not assigned(bcExtenders) Then
        bcExtenders := TFSNormalList.Create;
      anItem := TfsIntListItem.Create(Longint(anExtender));
      bcExtenders.Insert(anItem);
    End;
End;
{--------}

Function TfsSrBaseCursor.bcBLOBCopy(aSrcCursor: TfsSrBaseCursor;
  Const aBLOBNr: TffInt64;
  Var aDestBLOBNr: TffInt64)
  : TffResult;
Var
  aBLOB: PffByteArray;
  aBytesRead,
    aLen,
    aOffset: TffWord32; {!!.06}
  FileName: TffFullFileName;
Begin
  Result := DBIERR_NONE;
  { Assumption: Transaction has already been started by a calling routine. }

  { Is this a file BLOB? }
  If FFTblGetFileNameBLOB
    (aSrcCursor.bcTable.btFiles[aSrcCursor.Dictionary.BLOBFileNumber],
    bcDatabase.TransactionInfo, aBLOBNr, FileName) Then
    Begin
      FFTblAddFileBLOB(bcTable.btFiles[Dictionary.BLOBFileNumber],
        bcDatabase.TransactionInfo,
        FileName, aDestBLOBNr);
    End
  Else
    Begin
      aBytesRead := 0;
      aOffset := 0;
      {Begin !!.12}
      aLen := FFMinI(aSrcCursor.BLOBGetLength(aBLOBNr, Result),
        fsc_NumBLOBBytesToCopy);
      If Result = DBIERR_NONE Then
        Begin
          {End !!.12}
          FFGetMem(aBLOB, aLen);
          Try
            { Create the BLOB in the destination cursor. }
            Result := BLOBAdd(aDestBLOBNr);
            If Result = DBIERR_NONE Then
              Repeat
                Result := aSrcCursor.BLOBRead(aBLOBNr, aOffset, aLen, aBLOB^,
                  aBytesRead);
                If aBytesRead > 0 Then
                  Begin
                    Result := BLOBWrite(aDestBLOBNr, aOffset, aBytesRead, aBLOB^);
                    inc(aOffset, aBytesRead);
                  End;
              Until (aBytesRead = 0) Or (Result <> DBIERR_NONE);
          Finally
            FFFreeMem(aBLOB, aLen);
          End;
        End; { if } {!!.12}
    End;
End;
{--------}

Function TfsSrBaseCursor.bcBLOBLinkGetLength(Const aTableName: TfsTableName;
  Const aBLOBNr: TffInt64;
  Var aLength: Longint): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Cursor := bcFindBLOBCursor(aTableName);
  Assert(Assigned(Cursor));
  aLength := Cursor.BLOBGetLength(aBLOBNr, Result);
End;
{--------}

Function TfsSrBaseCursor.bcBLOBLinkRead(Const aTableName: TfsTableName;
  Const aBLOBNr: TffInt64;
  Const aOffset: TffWord32; {!!.06}
  Const aLen: TffWord32; {!!.06}
  Var aBLOB;
  Var aBytesRead: TffWord32) {!!.06}
: TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Cursor := bcFindBLOBCursor(aTableName);
  Assert(Assigned(Cursor));
  Result := Cursor.BLOBRead(aBLOBNr, aOffset, aLen, aBLOB, aBytesRead);
End;
{--------}

Function TfsSrBaseCursor.bcCheckExclusiveReadWrite: TffResult;
Begin
  Result := DBIERR_NONE;

  { The cursor must have Exclusive access to the table. }
  If (Not bcExclOwner) Then
    Result := DBIERR_NEEDEXCLACCESS
  Else If (bcOpenMode = omReadOnly) And {!!.06}
  Not (fffaTemporary In bcTable.Files[0].fiAttributes) Then {!!.06}
    { The cursor must be in read-write mode. Temporary files are excluded
      from this rule. }
    Result := DBIERR_TABLEREADONLY;

End;
{--------}

Function TfsSrBaseCursor.bcFindBLOBCursor(Const aTableName: TfsTableName)
  : TfsSrBaseCursor;
Var
  Inx: Longint;
  UTableName: TfsTableName;
Begin
  Result := Nil;
  UTableName := Uppercase(aTableName);
  { Do we have any BLOB cursors yet? }
  If bcBLOBCursors = Nil Then
    { No. Instantiate. }
    bcBLOBCursors := TFSNormalList.Create;

  { Have we opened a cursor for the referenced table? }
  For Inx := 0 To pred(bcBLOBCursors.Count) Do
    Begin
      If UpperCase(TfsSrBaseCursor(bcBLOBCursors[Inx]).bcTable.BaseName) =
        UTableName Then
        Begin
          Result := TfsSrBaseCursor(bcBLOBCursors[Inx]);
          break;
        End;
    End;

  { Did we find a cursor? }
  If Result = Nil Then
    Begin
      { No. Create one. }
      { Limitation: BLOB links can refer only to standard cursors, not to
        SQL result sets. }
      Result := bcEngine.CursorClass.Create(bcEngine, {!!.06}
        bcDatabase,
        Timeout);
      Result.Open(aTableName, '', 0, omReadOnly, smShared, False, False, [], True); {!!.01}
      bcBLOBCursors.Insert(Result);
    End;
End;
{--------}

Function TfsSrBaseCursor.bcGetAttribs: TffFileAttributes;
Begin
  Result := bcTable.Files[0]^.fiAttributes;
End;
{--------}

Function TfsSrBaseCursor.bcGetCursorID: TffCursorID;
Begin
  Result := TffCursorID(Self);
End;
{--------}

Function TfsSrBaseCursor.bcGetPosition: TfsCursorPosition;
Begin
  Result := bcInfo.Pos;
End;
{--------}

Function TfsSrBaseCursor.bcGetRefNr: TffInt64;
Begin
  Result := bcInfo.RefNr;
End;
{--------}

Procedure TfsSrBaseCursor.bcInit(Const aOpenMode: TffOpenMode;
  Const aShareMode: TffShareMode;
  Const aExclContLock: Boolean); {!!.10}
Var
  anIndex: Longint;
  aMonitor: TFSBaseEngineMonitor;
  anExtender: TFSBaseEngineExtender;
  MonitorList: TFSNormalList;
Begin

  { Assumption: This routine only called once a table has been successfully
    opened by the cursor. }
  bcFilter := Nil;
  bcFilterSav := Nil;
  bcNewRecBuff := Nil;
  bcOldRecBuff := Nil;

  { Miscellaneous. }
  If bcEngine.Configuration.GeneralInfo^.giReadOnly Then
    bcOpenMode := omReadOnly
  Else
    bcOpenMode := aOpenMode;
  FreeOnRemove := True;

  { Add ourself to the cursor lists in the table and database. }
  bcTable.CursorList.BeginWrite;
  Try
    bcTable.CursorList.AddCursor(Self);
  Finally
    bcTable.CursorList.EndWrite;
  End;

  bcDatabase.CursorList.BeginWrite;
  Try
    bcDatabase.CursorList.AddCursor(Self);
  Finally
    bcDatabase.CursorList.EndWrite;
  End;

  { If there are any monitors interested in cursors then see if they
    are interested in this cursor. }
  MonitorList := bcEngine.GetInterestedMonitors(TfsSrBaseCursor);
  If assigned(MonitorList) Then
    Begin
      For anIndex := 0 To pred(MonitorList.Count) Do
        Begin
          aMonitor := TFSBaseEngineMonitor
            (TfsIntListItem(MonitorList[anIndex]).KeyAsInt);
          Try
            anExtender := aMonitor.Interested(Self);
            If assigned(anExtender) Then
              bcAddExtender(anExtender);
          Except
            On E: Exception Do
              bcEngine.seForce('Monitor [%s] exception, bcInit: %s', {!!.06 - Start}
                [aMonitor.ClassName, E.message],
                bcEngine.bseGetReadOnly); {!!.06 - End}
          End;
        End;
      MonitorList.Free;
    End;

  { Get memory for a record data scratch pad. }
  bcRecordLen := bcTable.Dictionary.RecordLength;
  FFGetMem(bcRecordData, bcRecordLen);

  { If the cursor is the exclusive owner of the table then mark this
    fact. }
  If aShareMode = smExclusive Then
    Begin
      bcTable.SetExclOwner(CursorID);
      bcExclOwner := True;
    End;

  {Begin !!.10}
  If aExclContLock Then
    bcTable.AcqContentLock(bcDatabase.Transaction, ffsltExclusive, False, tluDatabase);
  {End !!.10}

End;
{--------}

Procedure TfsSrBaseCursor.bcInvalidateCurKey;
Begin
  bcInfo.KeyValid := False;
End;
{--------}

Function TfsSrBaseCursor.bcIsCurKeyPathValid: boolean;
Begin
  Result := (bcInfo.KeyPath.kpPos <> kppUnknown);
End;
{--------}

Function TfsSrBaseCursor.bcIsCurKeyValid: boolean;
Begin
  Result := bcInfo.KeyValid;
End;
{--------}

Procedure TfsSrBaseCursor.bcRecordUpdated(aOp: TfsRecOp;
  aRefNr: TffInt64;
  aIndexID: Integer);
Begin
  { A cursor is affected by another cursor's operations as follows:

    1. When a cursor inserts a record, it may cause a Structural Modification
       Operation (SMO) in the indices.  Other cursors open on the same
       table may now have invalid key paths.

       In FF 1.x, this routine would reset the key path.  In FF 2.x, we
       leave the key path as is.  The next time the cursor moves to the next
       or previous record, the indexing code will see that the key path has
       been modified and rebuild the key path.

       *** We do not call this routine for a record insertion. ***

    2. When a cursor deletes a record, it may cause an SMO in the indices.  As
       mentioned for inserts, we will rely upon the indexing code to rebuild
       the key path.

       If another cursor is positioned on the deleted record, we must make sure
       the cursor knows the record has been deleted.  This routine sets the
       bcInfo.Deleted flag and positions the cursor to OnCrack.  When this
       notification occurs, any cursors wanting to do something with the record
       will be blocked while waiting for a lock on the record so this should
       be a safe operation.

    3. When a cursor modifies a record, it may cause an SMO in zero or more
       indicies.  As mentioned for inserts, we will rely upon the indexing
       code to rebuild the key path.

       If another cursor is positioned on the modified record, we must make
       it look like the record has been deleted.  This routine sets the
       bcInfo.Deleted flag and positions the cursor to OnCrack.  When this
       notification occurs, any cursors wanting to do something with the record
       will be blocked while waiting for a lock on the record so this should
       be a safe operation.

    In general, this method is thread-safe. It is called only for those cursors
    that belong to the same database as the cursor performing the insert,
    update, or delete. Those cursors should be in the same client thread
    and only one request from that client thread is executed on the server
    at any given time. So operations should not be active for any of the
    other cursors belonging to the same database.
  }

  Case aOp Of
    roDelete:
      If (FFCmpI64(aRefNr, bcInfo.RefNr) = 0) And
        (bcInfo.Pos = cpOnRecord) Then
        Begin
          bcInfo.Deleted := True;
          If bcIsCurKeyPathValid Then
            Begin
              Assert(bcInfo.KeyPath.kpCount > 0);
              bcInfo.Pos := cpOnCrack;
              bcInfo.KeyPath.kpPos := kppOnCrackBefore;
            End
          Else
            bcInfo.KeyPath.kpPos := kppUnknown;
        End;
    roModify:
      If (aIndexID = IndexID) And
        (FFCmpI64(aRefNr, bcInfo.RefNr) = 0) And
        (bcInfo.Pos = cpOnRecord) Then
        Begin
          bcInfo.Deleted := True;
          If bcIsCurKeyPathValid Then
            Begin
              Assert(bcInfo.KeyPath.kpCount > 0);
              bcInfo.Pos := cpOnCrack;
              bcInfo.KeyPath.kpPos := kppOnCrackBefore;
            End
          Else
            bcInfo.KeyPath.kpPos := kppUnknown;
        End;
  End; {case}
End;
{--------}

Procedure TfsSrBaseCursor.bcRestoreCurInfo;
Begin
  bcInfo := bcSavedInfo;
End;
{--------}

Procedure TfsSrBaseCursor.bcSaveCurInfo;
Begin
  bcSavedInfo := bcInfo;
End;
{--------}

Procedure TfsSrBaseCursor.bcTableOpenPrim(aDatabase: TfsSrcDatabase;
  Const aTableName: TfsTableName;
  Const aOpenMode: TffOpenMode;
  Const aShareMode: TffShareMode;
  Const aForServer: boolean;
  Const aAttribs: TffFileAttributes);
Var
  aLockType: TfsSrcLockType;
  TableDataFile: TffFileNameExt;
Begin

  { The table name must be a valid file name without extension. }
  If Not FFVerifyFileName(aTableName) Then
    FSRaiseException(EfsException, fsStrResServer, fserrInvalidTableName,
      [aTableName]);

  { The table's data file must exist within the database. }
  TableDataFile := FFMakeFileNameExt(aTableName, fsc_ExtForData);
  If Not FFFileExists(FFMakeFullFileName(aDatabase.Folder.Path, TableDataFile)) Then
    FSRaiseException(EfsException, fsStrResServer, fserrUnknownTable,
      [TableDataFile, aDatabase.Alias]);

  { Create the table instance. }
  bcTable := bcTableClass.Create(aDatabase.Engine, aTableName, {!!.03}
    aDatabase.Folder,
    aDatabase.Engine.BufferManager, aOpenMode); {!!.03}

  Try
    { Acquire the right type of lock on the table. }
    If aShareMode = smExclusive Then
      aLockType := ffsltExclusive
    Else If aOpenMode = omReadOnly Then
      aLockType := ffsltShare
    Else
      aLockType := ffsltIntentS;

    bcTable.AcqLock(CursorID, aLockType);

    { Open up the files in the table, making sure that all of them
      are in FF format. }
    bcTable.OpenFiles(aDatabase.TransactionInfo, aForServer, aAttribs);
    TfsSrcTable(bcTable).ResolveDynamicLinks; {!!.06}
    bcTable.SetAttributes(aAttribs);
  Except
    bcTable.Free;
    bcTable := Nil;
    Raise;
  End;
End;
{--------}

Function TfsSrBaseCursor.BLOBAdd(Var aBLOBNr: TffInt64): TffResult;
Begin
  Result := NotifyExtenders(ffeaBeforeBLOBCreate, ffeaBLOBCreateFail);

  If Result = DBIERR_NONE Then
    Try
      AcqContentLock(fsclmWrite, tluDatabase);
      FFTblAddBLOB(bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
        bcDatabase.TransactionInfo, aBLOBNr);
      NotifyExtenders(ffeaAfterBLOBCreate, ffeaNoAction);
    Except
      NotifyExtenders(ffeaBLOBCreateFail, ffeaNoAction);
      Raise;
    End;
End;
{--------}

Function TfsSrBaseCursor.BLOBLinkAdd(Const aTableName: TfsTableName;
  Const aTableBLOBNr: TffInt64;
  Var aBLOBNr: TffInt64): TffResult;
Begin
  Result := NotifyExtenders(ffeaBeforeBLOBLinkAdd, ffeaBLOBLinkAddFail);

  If Result = DBIERR_NONE Then
    Try
      AcqContentLock(fsclmWrite, tluDatabase);
      FFTblAddBLOBLink(bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
        bcDatabase.TransactionInfo, aTableName, aTableBLOBNr,
        aBLOBNr);
      NotifyExtenders(ffeaAfterBLOBLinkAdd, ffeaNoAction);
    Except
      NotifyExtenders(ffeaBLOBLinkAddFail, ffeaNoAction);
      Raise;
    End;
End;
{--------}

Function TfsSrBaseCursor.FileBLOBAdd(Const aFileName: TffFullFileName;
  Var aBLOBNr: TffInt64): TffResult;
Begin
  Result := NotifyExtenders(ffeaBeforeFileBLOBAdd, ffeaFileBLOBAddFail);

  If Result = DBIERR_NONE Then
    Try
      AcqContentLock(fsclmWrite, tluDatabase);
      FFTblAddFileBLOB(bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
        bcDatabase.TransactionInfo, aFileName, aBLOBNr);
      NotifyExtenders(ffeaAfterFileBLOBAdd, ffeaNoAction);
    Except
      NotifyExtenders(ffeaFileBLOBAddFail, ffeaNoAction);
      Raise;
    End;
End;
{--------}

Function TfsSrBaseCursor.BLOBDelete(Const aBLOBNr: TffInt64): TffResult;
Begin
  Result := NotifyExtenders(ffeaBeforeBLOBDelete, ffeaBLOBDeleteFail);
  If Result = DBIERR_NONE Then
    Try
      If Dictionary.EngineDeleteType In [edtNotUndelete, edtUndeleteIfPossibleNotBlob] Then
        Begin
          AcqContentLock(fsclmWrite, tluDatabase);
          FFTblDeleteBLOB(bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
            bcDatabase.TransactionInfo, aBLOBNr);
        End;
      NotifyExtenders(ffeaAfterBLOBDelete, ffeaNoAction);
    Except
      NotifyExtenders(ffeaBLOBDeleteFail, ffeaNoAction);
      Raise;
    End;
End;
{--------}

Function TfsSrBaseCursor.BLOBFree(aBLOBNr: TffInt64): TffResult;
Begin
  Result := NotifyExtenders(ffeaBeforeBLOBFree, ffeaBLOBFreeFail);

  If Result = DBIERR_NONE Then
    Try
      AcqContentLock(fsclmWrite, tluDatabase);
      If FFTblFreeBLOB(bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
        bcDatabase.TransactionInfo, aBLOBNr) Then
        Result := DBIERR_BLOBMODIFIED;
      NotifyExtenders(ffeaAfterBLOBFree, ffeaNoAction);
    Except
      NotifyExtenders(ffeaBLOBFreeFail, ffeaNoAction);
      Raise;
    End;
End;
{--------}

Function TfsSrBaseCursor.BLOBGetLength(aBLOBNr: TffInt64;
  Var aFBError: TffResult): Longint;
Begin
  Result := -1;
  aFBError := NotifyExtenders(ffeaBeforeBLOBGetLength, ffeaBLOBGetLengthFail);

  If aFBError = DBIERR_NONE Then
    Try
      AcqContentLock(fsclmRead, tluDatabase);
      Try
        Result := FFTblGetBLOBLength(bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
          bcDatabase.TransactionInfo,
          aBLOBNr,
          bcBLOBLinkGetLength,
          aFBError);
        NotifyExtenders(ffeaAfterBLOBGetLength, ffeaNoAction);
      Finally
        RelContentLock(fsclmRead);
      End;
    Except
      NotifyExtenders(ffeaBLOBGetLengthFail, ffeaNoAction);
      Raise;
    End;
End;

Function TfsSrBaseCursor.IsDeletedBLOB(aBLOBNr: TffInt64;
  Var aFBError: TffResult): Boolean;
Begin
  Result := False;
  aFBError := NotifyExtenders(ffeaBeforeBLOBGetLength, ffeaBLOBGetLengthFail);

  If aFBError = DBIERR_NONE Then
    Try
      AcqContentLock(fsclmRead, tluDatabase);
      Try
        Result := FFTblIsDeletedBLOB(bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
          bcDatabase.TransactionInfo,
          aBLOBNr,
          aFBError);
        NotifyExtenders(ffeaAfterBLOBGetLength, ffeaNoAction);
      Finally
        RelContentLock(fsclmRead);
      End;
    Except
      NotifyExtenders(ffeaBLOBGetLengthFail, ffeaNoAction);
      Raise;
    End;
End;
{Begin !!.03}
{--------}

Function TfsSrBaseCursor.BLOBIsLink(aBLOBNr: TffInt64; {!!.11 - Start}
  Var aSrcTableName: TfsTableName;
  Var aSrcTableBLOBNr: TffInt64)
  : Boolean;
Begin
  Result := FFTblIsBLOBLink(bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
    bcDatabase.TransactionInfo,
    aBLOBNr,
    aSrcTableName,
    aSrcTableBLOBNr);
End;
{--------}{!!.11 - End}

Function TfsSrBaseCursor.BLOBListSegments(aBLOBNr: TffInt64;
  aStream: TStream)
  : TffResult;
Begin
  Result := DBIERR_NONE;
  AcqContentLock(fsclmRead, tluDatabase);
  Try
    FFTblListBLOBSegments(bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
      bcDatabase.TransactionInfo, aBLOBNr,
      aStream);
  Finally
    RelContentLock(fsclmRead);
  End;
End;
{End !!.03}
{--------}

Function TfsSrBaseCursor.BLOBRead(aBLOBNr: TffInt64;
  aOffset: TffWord32; {!!.06}
  aLen: TffWord32; {!!.06}
  Var aBLOB;
  Var aBytesRead: TffWord32) {!!.06}
: TffResult;
Begin
  Result := NotifyExtenders(ffeaBeforeBLOBRead, ffeaBLOBReadFail);

  If Result = DBIERR_NONE Then
    Try
      AcqContentLock(fsclmRead, tluDatabase);
      Try
        {Begin !!.11}
        bcTable.btBLOBEngine.Read
          (bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
          bcDatabase.TransactionInfo,
          aBLOBNr,
          aOffset,
          aLen,
          bcBLOBLinkRead,
          aBLOB,
          aBytesRead,
          Result);
        {End !!.11}
        NotifyExtenders(ffeaAfterBLOBRead, ffeaNoAction);
      Finally
        RelContentLock(fsclmRead);
      End;
    Except
      NotifyExtenders(ffeaBLOBReadFail, ffeaNoAction);
      Raise;
    End;
End;
{--------}

Function TfsSrBaseCursor.BLOBTruncate(aBLOBNr: TffInt64;
  aLen: TffWord32): TffResult;
Begin
  Result := NotifyExtenders(ffeaBeforeBLOBTruncate, ffeaBLOBTruncateFail);

  If Result = DBIERR_NONE Then
    Try
      AcqContentLock(fsclmWrite, tluDatabase);
      //If FFVerifyBLOBNr(aBLOBNr, bcTable.Files[bcTable.Dictionary.BLOBFileNumber].fiLog2BlockSize) Then
        //begin
        //if not IsDeletedBLOB(aBLOBNr, aResult) then
      bcTable.btBLOBEngine.Truncate
        (bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
        bcDatabase.TransactionInfo, aBLOBNr, aLen);
      //end;
      NotifyExtenders(ffeaAfterBLOBTruncate, ffeaNoAction);
    Except
      NotifyExtenders(ffeaBLOBTruncateFail, ffeaNoAction);
      Raise;
    End;
End;
{--------}

Function TfsSrBaseCursor.BLOBWrite(Const aBLOBNr: TffInt64;
  aOffset: TffWord32;
  aLen: TffWord32;
  Var aBLOB): TffResult;
Begin
  Result := NotifyExtenders(ffeaBeforeBLOBWrite, ffeaBLOBWriteFail);

  If Result = DBIERR_NONE Then
    Try
      AcqContentLock(fsclmWrite);
      bcTable.btBLOBEngine.Write
        (bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
        bcDatabase.TransactionInfo, aBLOBNr, aOffset, aLen,
        aBLOB);
      NotifyExtenders(ffeaAfterBLOBWrite, ffeaNoAction);
    Except
      On E: Exception Do
        Begin
          NotifyExtenders(ffeaBLOBWriteFail, ffeaNoAction);
          Raise;
        End;
    End;
End;
{--------}

Function TfsSrBaseCursor.CopyRecords(aSrcCursor: TfsSrBaseCursor;
  aBLOBCopyMode: TffBLOBCopyMode;
  aCallback: TfsSrcCopyRecordsProc;
  aCookie1, aCookie2, CountPerTrans: Longint): TffResult;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  aAutoIncField: Integer; {!!.02}
  aAutoIncHigh: Int64; {!!.02}
  aStep, aCPerTrans: Longint;
  aThisAutoInc: Int64; {!!.02}
  aBLOBFields: TfsPointerList;
  aBLOBNr,
    aSrcBLOBNr: TffInt64;
  aInx,
    aOffset: Integer;
  aRecord: PffByteArray;
  aTableName: TfsTableName;
  aTransID: TffTransID;
  aVal: PffByteArray;
  Include,
    IsNull: boolean;
Begin
  aFlag := 0;
  aCPerTrans := 0;
  Result := NotifyExtenders(ffeaBeforeCopyTable, ffeaCopyTableFail);
  If Result = DBIERR_NONE Then
    Begin
      aVal := Nil;
      aAutoIncHigh := 0;
      aStep := 1;
      {Begin !!.02}
        { Does the target have an autoinc field? }
      If Dictionary.HasAutoIncField(aAutoIncField) Then
        { Yes. Get the current seed value. }
        ReadAutoInc(aAutoIncHigh, aStep)
      Else
        { No. Flag it. }
        aAutoIncField := -1;
      {End !!.02}
          { Requirement: The cursors must be pointing to different tables. }
      If bcTable = aSrcCursor.Table Then
        FSRaiseExceptionNoData(EfsException, fsStrResServer, fserrSameTable);

      aTableName := aSrcCursor.Table.BaseName;
      aBLOBFields := TfsPointerList.Create;
      Try
        { Requirement: The dictionary field types and sizes must match. }
        If Not bcTable.Dictionary.HasSameFields(aSrcCursor.Dictionary, aBLOBFields) Then
          FSRaiseExceptionNoData(EfsException, fsStrResServer, fserrIncompatDict);

        { Save the position of each cursor. }
        bcSaveCurInfo;
        aSrcCursor.bcSaveCurInfo;
        { Create a record buffer. }
        FFGetMem(aRecord, bcTable.Dictionary.RecordLength);
        Try
          { Position the source cursor to BOF. }
          aSrcCursor.SetToBegin;

          { Start a transaction. It will be nested if a transaction is already
            in progress. }
          Result := bcEngine.seTransactionStart(bcDatabase, False, True, aTransID);
          Try
            While (Result = DBIERR_NONE) Do
              Begin

                { Grab a record from the source cursor. }
                Result := aSrcCursor.GetNextRecord(aRecord, ffsltNone, aflag, arefnr);
                If Result = DBIERR_NONE Then
                  Begin

                    { Was a callback function specified? }
                    Include := True;
                    If assigned(aCallback) Then
                      aCallback(aSrcCursor, aRecord, aCookie1, aCookie2, Include);

                    If Include Then
                      Begin
                        { Any BLOB fields? }
                        If aBLOBFields.Count > 0 Then
                          Begin
                            { Yes. Copy or link as necessary. }
                            For aInx := 0 To pred(aBLOBFields.Count) Do
                              Begin
                                aOffset := bcTable.Dictionary.FieldOffset[Integer(aBLOBFields[aInx])];
                                { Is the BLOB field null? }
                                aSrcCursor.Dictionary.GetRecordField(Integer(aBLOBFields[aInx]),
                                  aRecord, IsNull, aVal);
                                If Not IsNull Then
                                  Begin
                                    Case aBLOBCopyMode Of
                                      ffbcmNoCopy:
                                        bcTable.Dictionary.SetRecordField
                                          (Integer(aBLOBFields[aInx]), aRecord, Nil);
                                      ffbcmCopyFull:
                                        Begin
                                          aSrcBLOBNr := PffInt64(@aRecord^[aOffset])^;
                                          Result := bcBLOBCopy(aSrcCursor, aSrcBLOBNr, aBLOBNr);
                                          If Result = DBIERR_NONE Then
                                            PffInt64(@aRecord^[aOffset])^ := aBLOBNr
                                          Else
                                            break;
                                        End;
                                      Else { link the BLOBs }
                                        { Get the BLOB reference out of the record. }
                                        aSrcBLOBNr := PffInt64(@aRecord^[aOffset])^;
                                        { Add a BLOB link. }
                                        BLOBLinkAdd(aTableName, aSrcBLOBNr, aBLOBNr);
                                        { Update the BLOB reference in the record. }
                                        PffInt64(@aRecord^[aOffset])^ := aBLOBNr;
                                    End; { case }
                                  End; { if BLOB field not null }
                              End; { for }
                          End;
                        Result := InsertRecord(aRecord, ffsltNone, 0, aRefNr);
                        Inc(aCPerTrans);
                        { If the target has an autoinc field then keep track of the
                          highest value. }
                        If (Result = DBIERR_NONE) And (aAutoIncField > -1) Then
                          Begin
                            Dictionary.GetRecordField(aAutoIncField,
                              aRecord, IsNull, @aThisAutoInc);
                            If Not IsNull And (aThisAutoInc > aAutoIncHigh) Then
                              aAutoIncHigh := aThisAutoInc;
                          End;

                        If CountPerTrans > 0 Then
                          Begin
                            If aCPerTrans >= CountPerTrans Then
                              Begin
                                aCPerTrans := 0;
                                Result := bcEngine.seTransactionCommit(bcDatabase);
                                If Result = DBIERR_NONE Then
                                  Begin
                                    Result := bcEngine.seTransactionStart(bcDatabase, False, True, aTransID);
                                    If Result <> DBIERR_NONE Then
                                      System.break;
                                  End
                                Else
                                  Begin
                                    bcEngine.seTransactionRollback(bcDatabase);
                                    System.break;
                                  End;
                              End;
                          End;
                      End;
                  End; { if }
              End; { while }
            {Begin !!.02}
            If Result = DBIERR_EOF Then
              Begin
                { If the destination has an autoinc field then update the seed
                  value. }
                If aAutoIncField <> -1 Then
                  FFTblSetAutoInc(Table.Files[0], Database.TransactionInfo, aAutoIncHigh, aStep);
                Result := bcEngine.seTransactionCommit(bcDatabase);
              End
                {End !!.02}
            Else
              bcEngine.seTransactionRollback(bcDatabase);
          Except
            bcEngine.seTransactionRollback(bcDatabase);
            Raise;
          End;
        Finally
          { Free the record buffer. }
          FFFreeMem(aRecord, bcTable.Dictionary.RecordLength);
          { Restore the position of each cursor. }
          bcRestoreCurInfo;
          aSrcCursor.bcRestoreCurInfo;
        End;
      Finally
        aBLOBFields.Free;
      End;
    End;
  NotifyExtenders(ffeaAfterCopyTable, ffeaNoAction);
End;
{--------}

Function TfsSrBaseCursor.CopyRecordParts(aSrcCursor: TfsSrBaseCursor;
  aFields: PffLongintArray;
  aNumFields: Integer;
  aBLOBCopyMode: TffBLOBCopyMode;
  aCallback: TfsSrcCopyRecordsProc;
  aCookie1, aCookie2, CountPerTrans: Longint): TffResult;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  aBLOBFields: TfsPointerList;
  aInx: Integer;
  aDestRec, aSrcRec: PffByteArray;
  aSrcBLOBNr, aBLOBNr: TffInt64;
  aOffset: Integer;
  aCPerTrans: Longint;
  aTableName: TfsTableName;
  aTransID: TffTransID;
  aVal: PffByteArray;
  DestLen: Integer;
  Include: boolean;
  IsNull: boolean;
Begin
  aCPerTrans := 0;
  aFlag := 0;
  { Requirement: The cursors must be pointing to different tables. }
  If bcTable = aSrcCursor.Table Then
    FSRaiseExceptionNoData(EfsException, fsStrResServer, fserrSameTable);

  Result := NotifyExtenders(ffeaBeforeCopyTable, ffeaCopyTableFail);
  If Result = DBIERR_NONE Then
    Begin
      aTableName := aSrcCursor.Table.BaseName;
      aBLOBFields := TfsPointerList.Create;
      Try
        { Requirement: The dictionary field types and sizes must match. }
        If Not bcTable.Dictionary.HasSameFieldsEx(aSrcCursor.Dictionary, aFields,
          aNumFields, aBLOBFields) Then
          FSRaiseExceptionNoData(EfsException, fsStrResServer, fserrIncompatDict);

        { Save the position of each cursor. }
        bcSaveCurInfo;
        aSrcCursor.bcSaveCurInfo;

        { Create record buffers. }
        DestLen := bcTable.Dictionary.RecordLength;
        FFGetMem(aDestRec, DestLen);
        FFGetMem(aSrcRec, aSrcCursor.Dictionary.RecordLength);
        FFGetMem(aVal, aSrcCursor.Dictionary.BlockSize);
        Try
          { Position the source cursor to BOF. }
          aSrcCursor.SetToBegin;

          { Start a transaction. It will be nested if a transaction is already
            in progress. }
          Result := bcEngine.seTransactionStart(bcDatabase, False, True, aTransID);
          Try
            While (Result = DBIERR_NONE) Do
              Begin

                { Grab a record from the source cursor. }
                Result := aSrcCursor.GetNextRecord(aSrcRec, ffsltNone, aflag, arefnr);
                If Result = DBIERR_NONE Then
                  Begin

                    { Was a callback function specified? }
                    Include := True;
                    If assigned(aCallback) Then
                      aCallback(aSrcCursor, aSrcRec, aCookie1, aCookie2, Include);

                    If Include Then
                      Begin
                        { Build the destination record. }
                        FillChar(aDestRec^, destLen, 0);
                        For aInx := 0 To pred(aNumFields) Do
                          Begin
                            aSrcCursor.Dictionary.GetRecordField(aFields^[aInx],
                              aSrcRec, IsNull, aVal);
                            If IsNull Then
                              bcTable.Dictionary.SetRecordField(aInx, aDestRec, Nil)
                            Else
                              Begin
                                { Is this a BLOB field? }
                                If bcTable.Dictionary.FieldType[aInx] In
                                  [fstBLOB..fstBLOBGraphic] Then
                                  Begin
                                    aOffset := aSrcCursor.Dictionary.FieldOffset[aFields^[aInx]];
                                    { Yes. How is it to be handled? }
                                    Case aBLOBCopyMode Of
                                      ffbcmNoCopy:
                                        bcTable.Dictionary.SetRecordField(aInx, aDestRec, Nil);
                                      ffbcmCopyFull:
                                        Begin
                                          aSrcBLOBNr := PffInt64(@aSrcRec^[aOffset])^;
                                          Result := bcBLOBCopy(aSrcCursor, aSrcBLOBNr, aBLOBNr);
                                          If Result = DBIERR_NONE Then
                                            PffInt64(@aDestRec^[aOffset])^ := aBLOBNr
                                          Else
                                            break;
                                        End;
                                      Else { link the BLOBs }
                                        { Get the BLOB reference out of the record. }
                                        aSrcBLOBNr := PffInt64(@aSrcRec^[aOffset])^;
                                        { Add a BLOB link. }
                                        BLOBLinkAdd(aTableName, aSrcBLOBNr, aBLOBNr);
                                        { Update the BLOB reference in the record. }
                                        PffInt64(@aDestRec^[aOffset])^ := aBLOBNr;
                                    End; { case }
                                  End
                                Else
                                  bcTable.Dictionary.SetRecordField(aInx, aDestRec, aVal);
                              End;
                          End;
                        { Insert the record. }
                        Result := InsertRecord(aDestRec, ffsltNone, 0, aRefNr);
                        inc(aCPerTrans);
                        If CountPerTrans > 0 Then
                          Begin
                            If aCPerTrans >= CountPerTrans Then
                              Begin
                                aCPerTrans := 0;
                                Result := bcEngine.seTransactionCommit(bcDatabase);
                                If Result = DBIERR_NONE Then
                                  Begin
                                    Result := bcEngine.seTransactionStart(bcDatabase, False, True, aTransID);
                                    If Result <> DBIERR_NONE Then
                                      System.break;
                                  End
                                Else
                                  Begin
                                    bcEngine.seTransactionRollback(bcDatabase);
                                    System.break;
                                  End;
                              End;
                          End;
                      End;
                  End; { if }
              End; { while }
            If Result = DBIERR_EOF Then
              Result := bcEngine.seTransactionCommit(bcDatabase)
            Else
              bcEngine.seTransactionRollback(bcDatabase);
          Except
            bcEngine.seTransactionRollback(bcDatabase);
            Raise;
          End;
        Finally
          { Free the record buffers. }
          FFFreeMem(aSrcRec, aSrcCursor.Dictionary.RecordLength);
          FFFreeMem(aDestRec, DestLen);
          FFFreeMem(aVal, aSrcCursor.Dictionary.BlockSize);
          { Restore the position of each cursor. }
          bcRestoreCurInfo;
          aSrcCursor.bcRestoreCurInfo;
        End;
      Finally
        aBLOBFields.Free;
      End;
    End;
  NotifyExtenders(ffeaAfterCopyTable, ffeaNoAction);
End;
{--------}

Function ExtractFunc(Const Func: String; Var Pos: Integer): String;
Var
  I: Integer;
Begin
  I := Pos;
  While (I <= Length(Func)) And (Func[I] <> ';') Do
    Inc(I);
  Result := Trim(Copy(Func, Pos, I - Pos));
  If (I <= Length(Func)) And (Func[I] = ';') Then
    Inc(I);
  Pos := I;
End;

Procedure MakeFunc(Def: String; Var funName, tblName, SfldName, DfldName, DfldValue: String);
Var
  s: String;
  iPos, n: Integer;
Begin
  iPos := 1;
  n := 0;
  While iPos <= Length(Def) Do
    Begin
      s := ExtractFunc(def, iPos);
      If s <> '' Then
        Begin
          Case n Of
            0: funName := s;
            1: tblName := s;
            2: SfldName := s;
            3: DfldName := s;
            4: DfldValue := s;
          End;
        End;
      inc(n);
    End;
End;

Function TfsSrBaseCursor.ExecScript(cScript: TStringList; Var OldRecord: PffByteArray; Var NewRecord: PffByteArray;
  ListEventsTransaction, ListEventsGlobalTransaction: TfsSrcTransaction): TffResult;
Var
  sl, sl1, sl2: TStringList;
  iScr: Integer;
Begin
  Result := 0;
  If cScript.Count > 0 Then
    Begin
      sl := TStringList.Create;
      sl1 := TStringList.Create;
      sl2 := TStringList.Create;
      Interpretator.ErrorResult := 0;
      Try
        Interpretator.ClientID := Self.client.ClientID;
        Interpretator.DatabaseID := Self.DataBase.DatabaseID;
        Interpretator.Timeout := Self.client.Timeout;
        Interpretator.OpenMode := Self.bcOpenMode;
        Interpretator.engine := Self.Engine;
        Interpretator.SrcCursor := Self;
        Interpretator.OldRecord := OldRecord;
        Interpretator.NewRecord := NewRecord;
        If ListEventsTransaction <> Nil Then
          Interpretator.ListEventsTransaction := ListEventsTransaction.ListEventsTransaction;
        If ListEventsGlobalTransaction <> Nil Then
          Interpretator.ListEventsGlobalTransaction := ListEventsGlobalTransaction.ListEventsGlobalTransaction;

        For iScr := 0 To cScript.Count - 1 Do
          Begin
            sl.Clear;
            sl1.Clear;
            sl2.Clear;
            sl.text := cScript[iScr];
            Interpretator.PrepareScript(sl, sl1, sl2);
            //sl1.SaveToFile('c:\aa.txt');
            If interpretator.ErrorResult = 0 Then
              Begin
                Try
                  Interpretator.ExecuteScript(sl1);
                Except
                  Result := interpretator.ErrorResult;
                End;
                Result := interpretator.ErrorResult;
                If interpretator.ErrorResult = 0 Then
                  Begin
                    OldRecord := Interpretator.OldRecord;
                    NewRecord := Interpretator.NewRecord;
                  End;
              End
            Else
              Result := interpretator.ErrorResult;
            If Result <> 0 Then Break;
          End;
      Finally
        interpretator.Clear;
        sl.Free;
        sl1.Free;
        sl2.Free;
      End;
    End;
End;

Function TfsSrBaseCursor.ExecProcedure(ProcName, ProcParam: String; Var CursorID: TffCursorID): TffResult;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  sl, sl1, sl2: TStringList;
  Interpretator: TCursorInterpretator;
  TransBegin: boolean;
  CurEr: TffResult;
  aProcString: String;
  DBS: TfsSrcDatabase;

  Function TextProcedure(Const sProc: String): String;
  Var
    SearchPath: TffPath;
    Cur: TfsSrBaseCursor;
    CACTIVE: String;
    CTABLENAME: String;
    CMETODENAME: String;
    ProcName: String;

    Procedure GetTable(cur: TfsSrBaseCursor);
    Var
      OldData: PffByteArray;
      RecLen: Integer;
      FieldBuffer: PffByteArray;
      IsNull: boolean;
      SQLResult, n: TffResult;
      Rec: Variant;
      iFld: Integer;
      Cstr: String;
      TransBegin: boolean;
    Begin
      Rec := null;
      ProcName := sProc;
      If cur <> Nil Then
        Begin
          Cur.SetToBegin;
          RecLen := cur.Dictionary.RecordLength;
          FFGetMem(OldData, RecLen);
          Try
            TransBegin := False;
            Try
              If cur.Database.TransactionID = 0 Then
                Begin
                  cur.Engine.TransactionStartSQL(cur.DataBase.DatabaseID, False);
                  TransBegin := True;
                End;
              n := 0;
              SQLResult := 0;
              n := cur.GetNextRecord(Nil, ffsltNone, aflag, arefnr);
              If n = 0 Then
                While (Not (cur.Position = cpEOF)) Do
                  Begin
                    Cstr := '';
                    CACTIVE := ''; //ACTIVE
                    CTABLENAME := '';
                    CMETODENAME := '';

                    SQLResult := cur.GetRecord(OldData, ffsltNone, tluDatabase, aflag, arefnr, False);
                    For iFld := 0 To 1 Do
                      Begin
                        FFGetMem(FieldBuffer, cur.Dictionary.FieldLength[iFld]);
                        Try
                          If SQLResult = DBIERR_NONE Then
                            Begin
                              cur.Dictionary.GetRecordField(iFld, OldData, IsNull, FieldBuffer);
                              If IsNull Then
                                Rec := Null
                              Else
                                Begin
                                  Rec := fsCurGetValue(Cur.CursorID, iFld, OldData, FieldBuffer, False);
                                  Case iFld Of
                                    0: CACTIVE := UpperCase(Trim(Rec));
                                    1: CTABLENAME := UpperCase(Trim(Rec));
                                  End;
                                End;
                            End;
                        Finally
                          FFFreeMem(FieldBuffer, cur.Dictionary.FieldLength[iFld]);
                        End;
                      End; //For iFld := 0 To 2 Do
                    If (CACTIVE = 'A') And (CTABLENAME = AnsiUpperCase(ProcName)) Then
                      TextProcedure := fsGetBlobValue(Cur.CursorID, 2, OldData)
                    Else If (CACTIVE <> 'A') Then
                      break;
                    // next
                    n := cur.GetNextRecord(Nil, ffsltNone, aflag, arefnr);
                  End;
            Except
              If TransBegin Then
                Begin
                  TransBegin := False;
                  cur.Engine.TransactionRollbackSQL(cur.DataBase.DatabaseID, False);
                End;
            End;
          Finally
            FFFreeMem(OldData, RecLen);
            If TransBegin Then
              cur.Engine.TransactionCommitSQL(cur.DataBase.DatabaseID, False);
          End;
        End;
    End;
  Begin
    Result := '';
    DBS := Self.Database;
    SearchPath := dbs.Folder.Path;
    If (SearchPath[length(SearchPath)] <> '\') Then
      FFShStrAddChar(SearchPath, '\');
    If FFFileExists(SearchPath + FFForceExtension('sys$procedures', fsc_ExtForData)) Then
      Begin
        Cur := Self.Engine.CursorClass.Create(Self.Engine, dbs, 10000);
        If Cur <> Nil Then
          Begin
            Try
              CurEr := 0;
              Cur.Open('sys$procedures', 'RELATION', 0, omReadOnly, smShared, False, False, [], False);
            Except
              CurEr := DBIERR_NOCURRREC;
              If Assigned(Cur) Then
                Cur.free;
              cur := Nil;
              Raise;
            End;
            If (CurEr = 0) Then
              GetTable(Cur);
            If Assigned(Cur) Then
              Cur.free;
            cur := Nil;
          End;
      End;
  End;

  Procedure AddParamVariable(Const s: String);
  Var
    c, d, oi, ci, i: Integer;
    AddS: String;

    Function VrRemoveQuotes(Const St: String): String;
    Var
      S: String;
    Begin
      S := St;
      If (Length(s) > 2) And (s[1] = '"') Then
        Begin
          If (Length(s) > 2) And (s[1] = '"') And (s[Length(s)] = '"') Then
            Result := Copy(s, 2, Length(s) - 2)
          Else
            Result := s;
        End
      Else
        Begin
          If (Length(s) > 2) And (s[1] = '''') And (s[Length(s)] = '''') Then
            Result := Copy(s, 2, Length(s) - 2)
          Else
            Result := s;
        End;
    End;

  Begin
    c := 1;
    d := 1;
    i := 1;
    oi := 2;
    ci := 1;
    Repeat
      Inc(i);
      If s[i] = '''' Then
        If d = 1 Then
          Inc(d)
        Else
          d := 1;
      If d = 1 Then
        Begin
          If s[i] = '(' Then
            Inc(c)
          Else If s[i] = ')' Then
            Dec(c);
          If (s[i] = ',') And (c = 1) Then
            Begin
              AddS := VrRemoveQuotes(Trim(Copy(s, oi, i - oi)));
              Interpretator.InternalVariables['_@#$-PARAM' + IntToStr(ci)] := '[' + AddS + ']';
              oi := i + 1;
              Inc(ci);
            End;
        End;
    Until (c = 0) Or (i >= Length(s));
    AddS := VrRemoveQuotes(Trim(Copy(s, oi, i - oi)));
    Interpretator.InternalVariables['_@#$-PARAM' + IntToStr(ci)] := '[' + AddS + ']';
    If c <> 0 Then Exit;
  End;

Begin
  Result := 0;
  TransBegin := False;
  aProcString := '';
  If ProcName <> '' Then
    aProcString := TextProcedure(ProcName);
  If aProcString <> '' Then
    Begin
      sl := TStringList.Create;
      sl1 := TStringList.Create;
      sl2 := TStringList.Create;
      Interpretator := TCursorInterpretator.Create;
      Interpretator.ErrorResult := 0;
      Try
        Interpretator.ClientID := Self.client.ClientID;
        Interpretator.DatabaseID := Self.DataBase.DatabaseID;
        Interpretator.Timeout := Self.client.Timeout;
        Interpretator.OpenMode := Self.bcOpenMode;
        Interpretator.engine := Self.Engine;

        Interpretator.SrcCursor := Self;
        Interpretator.OldRecord := Nil;
        Interpretator.NewRecord := Nil;

        sl.Clear;
        sl1.Clear;
        sl2.Clear;
        sl.text := aProcString;
        Interpretator.PrepareScript(sl, sl1, sl2);
        If interpretator.ErrorResult = 0 Then
          Begin
            Try
              If Self.Database.TransactionID = 0 Then
                Begin
                  Engine.TransactionStartSQL(Self.DataBase.DatabaseID, False);
                  TransBegin := True;
                End;
              If Self.DataBase.TransactionInfo <> Nil Then
                If Self.DataBase.TransactionInfo.tirTrans <> Nil Then
                  Begin
                    If Self.DataBase.TransactionInfo.tirTrans.ListEventsTransaction <> Nil Then
                      Interpretator.ListEventsTransaction := Self.DataBase.TransactionInfo.tirTrans.ListEventsTransaction;
                    If Self.DataBase.TransactionInfo.tirTrans.ListEventsGlobalTransaction <> Nil Then
                      Interpretator.ListEventsGlobalTransaction := Self.DataBase.TransactionInfo.tirTrans.ListEventsGlobalTransaction;
                  End;
              // Add Input Variable
              If ProcParam <> '' Then
                AddParamVariable(ProcParam);
              Interpretator.ExecuteScript(sl1);
            Except
              Result := interpretator.ErrorResult;
              If TransBegin Then
                Engine.TransactionRollbackSQL(Self.DataBase.DatabaseID, False);
            End;
            Result := interpretator.ErrorResult;
            If interpretator.ErrorResult = 0 Then
              CursorID := interpretator.CursorProcedure;
          End
        Else
          Result := interpretator.ErrorResult;

      Finally
        interpretator.free;
        sl.Free;
        sl1.Free;
        sl2.Free;
        Try
          If interpretator.ErrorResult = 0 Then
            If TransBegin Then
              Engine.TransactionCommitSQL(Self.DataBase.DatabaseID, False);
        Except
        End;
      End;
    End;
End;

{Begin !!.06}
{--------}

Function TfsSrBaseCursor.DeleteRecords(CountPerTrans: Longint): TffResult;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  aRecord: PffByteArray;
  aTransID: TffTransID;
  Cpos: Longint;

  Function GetInTransaction: boolean;
  Var
    TrLevel: Longint;
    err: TffResult;
  Begin
    Result := False;
    TrLevel := -1;
    err := Engine.InTransaction(bcDatabase.DatabaseID, TrLevel);
    If err = DBIERR_NOACTIVETRAN Then
      Result := False
    Else
      Begin
        DeleteRecords := err;
        Assert(Err = 0);
        Result := TrLevel >= 0;
      End;
  End;
Begin
  aFlag := 0;
  { Create a record buffer. }
  FFGetMem(aRecord, bcTable.Dictionary.RecordLength);
  Try
    { Position to BOF. }
    SetToBegin;

    { Start a transaction. It will be nested if a transaction is already
      in progress. }
    Result := bcEngine.seTransactionStart(bcDatabase, False, True, aTransID);
    Try
      Cpos := 0;
      While (Result = DBIERR_NONE) Do
        Begin

          { If on a record then get it otherwise move to the next
            record. }
          If bcInfo.Pos = cpOnRecord Then
            Begin
              Result := GetRecord(aRecord, ffsltExclusive, tluDatabase, aflag, arefnr, False);
              { Is a filter active? }
              If Result = DBIERR_NOCURRREC Then
                { Yes. The current record didn't match the filter. Find the next
                  record that matches the filter. }
                Result := GetNextRecord(aRecord, ffsltExclusive, aflag, arefnr);
            End
          Else
            Result := GetNextRecord(aRecord, ffsltExclusive, aflag, arefnr);
          If Result = DBIERR_NONE Then
            Begin
              Result := DeleteRecord(aRecord);
              If Result = DBIERR_NOTSUFFFIELDRIGHTS Then
                Result := GetNextRecord(aRecord, ffsltExclusive, aflag, arefnr);
            End;

          If Result = DBIERR_NONE Then
            Begin
              If CountPerTrans > 0 Then
                Begin
                  Inc(Cpos);
                  If Cpos = CountPerTrans Then
                    Begin
                      Cpos := 0;
                      Result := bcEngine.seTransactionCommit(bcDatabase);
                      If Result <> DBIERR_NONE Then
                        Begin
                          Result := bcEngine.seTransactionRollback(bcDatabase);
                          System.break;
                        End
                      Else
                        Begin
                          Result := bcEngine.seTransactionStart(bcDatabase, False, True, aTransID);
                          If Result <> DBIERR_NONE Then
                            System.break;
                        End;
                    End;
                End;
            End;
        End; { while }
      If Result = DBIERR_EOF Then
        Begin
          If GetInTransaction Then
            Result := bcEngine.seTransactionCommit(bcDatabase);
        End
      Else
        Begin
          bcEngine.seTransactionRollback(bcDatabase);
        End;
    Except
      bcEngine.seTransactionRollback(bcDatabase);
      Raise;
    End;
  Finally
    FFFreeMem(aRecord, bcTable.Dictionary.RecordLength);
  End;
End;
{End !!.06}
{--------}

Function TfsSrBaseCursor.Empty: TffResult;
Begin
  { Requirement: Transaction must be started. }
  Assert(Assigned(bcDatabase.Transaction));

  If ((Table.TableFlags And fsTableDontEmptyTable) <> 0) Then
    Begin
      Result := DBIERR_NOTSUFFFIELDRIGHTS;
      Exit;
    End;
  { The cursor must have Exclusive Read-Write access. }
  Result := bcCheckExclusiveReadWrite;
  If Result <> DBIERR_NONE Then
    Exit;

  { Get the table to empty itself. }
  AcqContentLock(fsclmWrite);
  Result := bcTable.EmptyFiles(Database.TransactionInfo);
End;
{--------}

Function TfsSrBaseCursor.EnsureWritable(aCheckCurRec, aConditionalLock: Boolean; aUserLockType: TfsUserRecLocking): TffResult;
Var
  aFlag: Byte;
Begin
  aflag := 0;
  { The cursor must have been opened in read-write mode. }
  If (bcOpenMode = omReadOnly) Then
    Begin
      Result := DBIERR_TABLEREADONLY;
      Exit;
    End;

  { There cannot be any type of lock on the table (unless its ours and
    is a write lock). }
  If Table.btClientLocks.Count > 0 Then
    If Table.btClientLocks.SummaryMode = ffsltExclusive Then
      Begin
        If Not Table.HasClientLock(CursorID) Then
          Begin
            Result := DBIERR_FILELOCKED;
            Exit;
          End;
      End
    Else
      Begin
        Result := DBIERR_FILELOCKED;
        Exit;
      End;

  { Make sure the cursor is positioned on a record. }
  If aCheckCurRec Then
    Begin
      If (bcInfo.pos <> cpOnRecord) Then
        Begin
          If bcInfo.Deleted Then
            Result := DBIERR_KEYORRECDELETED
          Else
            Result := DBIERR_NOCURRREC;
          Exit;
        End;

      AcqContentLock(fsclmRead, aUserLockType);
      Try
        Table.GetRecord(Database.TransactionInfo, {!!.10}
          bcDatabase.DatabaseID, {!!.10}
          CursorID, bcInfo.refNr, {!!.10}
          bcRecordData, ffsltExclusive, aUserLockType, False, aConditionalLock, aflag); {!!.02}
        { Note: We assume we can ask for an Exclusive lock because this
          method is passed True only from the Engine.RecordDelete and
          Engine.RecordModify methods. }
        If assigned(bcFilter) And {!!.02}
        (Not bcFilter.MatchesRecord(bcRecordData)) Then
          Begin {!!.02}
            If bcInfo.Deleted Then
              Result := DBIERR_KEYORRECDELETED
            Else
              Result := DBIERR_NOCURRREC;
            Exit;
          End;
      Finally
        RelContentLock(fsclmRead);
      End;
      //    end;                                                             {Deleted !!.02}
    End;

  { There must have been a transaction started for our owning database. }
  If Not assigned(Database.Transaction) Then
    Begin
      Result := DBIERR_NOACTIVETRAN;
      Exit;
    End;

  Result := DBIERR_NONE;

End;
{--------}

Procedure TfsSrBaseCursor.ReadAutoInc(Var aValue: Int64; Var aStep: Longint);
Begin
  AcqContentLock(fsclmRead);
  Try
    aValue := FFTblReadAutoInc(bcTable.Files[0], bcDatabase.TransactionInfo, aStep);
  Finally
    RelContentLock(fsclmRead);
  End;
End;

Procedure TfsSrBaseCursor.ReadMaxRecords(Var aValue: Longword);
Begin
  AcqContentLock(fsclmRead);
  Try
    aValue := FFTblReadMaxRecords(bcTable.Files[0], bcDatabase.TransactionInfo);
  Finally
    RelContentLock(fsclmRead);
  End;
End;

Procedure TfsSrBaseCursor.ReadTableFlags(Var aValue: Word);
Begin
  AcqContentLock(fsclmRead);
  Try
    aValue := FFTblReadTableFlags(bcTable.Files[0], bcDatabase.TransactionInfo);
  Finally
    RelContentLock(fsclmRead);
  End;
End;

Procedure TfsSrBaseCursor.ReadTablePassword(Var aValue: Longword);
Begin
  AcqContentLock(fsclmRead);
  Try
    aValue := FFTblReadTablePassword(bcTable.Files[0], bcDatabase.TransactionInfo);
  Finally
    RelContentLock(fsclmRead);
  End;
End;

Procedure TfsSrBaseCursor.ReadTablePasswordRest(Var aValue: Longword);
Begin
  AcqContentLock(fsclmRead);
  Try
    aValue := FFTblReadTablePasswordRest(bcTable.Files[0], bcDatabase.TransactionInfo);
  Finally
    RelContentLock(fsclmRead);
  End;
End;

Procedure TfsSrBaseCursor.ReadTableDBID(Var aValue: Longword);
Begin
  AcqContentLock(fsclmRead);
  Try
    aValue := FFTblReadTableDBID(bcTable.Files[0], bcDatabase.TransactionInfo);
  Finally
    RelContentLock(fsclmRead);
  End;
End;
{--------}

Function TfsSrBaseCursor.bcGetDictionary: TFSInfoDict;
Begin
  Result := Table.Dictionary;
End;
{--------}

Function TfsSrBaseCursor.GetRecord(aData: PffByteArray; aLockType: TfsSrcLockType; aUserLockType: TfsUserRecLocking;
  Var aFlag: Byte; Var aRefNr: TffInt64; Const aUser: Boolean): TffResult;
Begin
  aRefnr.iLow := 0;
  aRefnr.iHigh := 0;
  { Request a lock on the record prior to our testing any logic.  We must
    make sure that a delete in progress has finished before we make
    any decisions. }
{Begin !!.03}{Begin !!.06}
  If (bcInfo.pos = cpOnRecord) And (aLockType <> ffsltNone) Then
    Begin
      { If there is a write lock on the table then return an error. }
      If (bcTable.btClientLocks.Count > 0) Then
        { If table is write locked but not by this client then cannot obtain
          a lock on the record. If table is read locked by any client then cannot
          obtain a lock on the record. }
        If Table.btClientLocks.SummaryMode = ffsltExclusive Then
          Begin
            If (Not bcTable.HasClientLock(CursorID)) Then
              Begin
                Result := DBIERR_FILELOCKED;
                Exit;
              End;
          End
        Else
          Begin
            Result := DBIERR_FILELOCKED;
            Exit;
          End;

      { Made it this far. Obtain the record lock. }
      Table.GetRecordLock(bcDatabase.TransactionInfo, bcDatabase.DatabaseID,
        CursorID, bcInfo.refNr, aLockType, aUserLockType, aUser);
      bcLockedRefNum := bcInfo.refNr;
      aRefnr := bcInfo.refNr;
    End; { if }
  {End !!.03}{End !!.06}

  If (bcInfo.pos = cpOnRecord) Then
    Begin
      AcqContentLock(fsclmRead);
      bcInfoLock.Lock; {!!.06}
      Try
        Result := Table.GetRecord(bcDatabase.TransactionInfo, {!!.10}
          bcDatabase.DatabaseID, {!!.10}
          CursorID, {!!.10}
          bcInfo.refNr, aData, aLockType, aUserLockType, True, False, aFlag); {!!.02}
        aRefnr := bcInfo.refNr;
        If Assigned(bcFilter) Then
          Begin
            If Not bcFilter.MatchesRecord(aData) Then
              Begin
                { Release the record lock. }
                Table.RelRecordLock(bcDatabase.TransactionInfo, {!!.10}
                  bcDatabase.DatabaseID, {!!.10}
                  CursorID, bcInfo.RefNr); {!!.10}
                If bcInfo.Deleted Then
                  Result := DBIERR_KEYORRECDELETED
                Else
                  Result := DBIERR_NOCURRREC;
                Exit;
              End;
          End;
        {Begin !!.02}
        If (Result = DBIERR_NONE) And (aData <> Nil) Then
          Move(aData^, bcRecordData^, bcRecordLen);
        {End !!.02}
      Finally
        bcInfoLock.Unlock; {!!.06}
        RelContentLock(fsclmRead);
      End;
    End
  Else If bcInfo.pos = cpEOF Then
    Result := DBIERR_EOF
      //Else If bcInfo.pos = cpBOF Then
//  Result := DBIERR_BOF
  Else
    Begin
      If bcInfo.Deleted Then
        Result := DBIERR_KEYORRECDELETED
      Else
        Result := DBIERR_NOCURRREC;
    End;
End;
{--------}

Function TfsSrBaseCursor.GetRecordField(aField: Integer;
  aRecordBuffer: PffByteArray;
  Var isNull: boolean;
  aFieldBuffer: pointer): TffResult;
Begin
  Result := DBIERR_NONE;
  bcTable.Dictionary.GetRecordField(aField, aRecordBuffer, isNull, aFieldBuffer);
End;
{--------}

Function TfsSrBaseCursor.HasRecordLocks: Boolean;
Begin
  Result := bcTable.HasRecordLocks;
End;

Function TfsSrBaseCursor.RecordCountLocks: TffWord32;
Begin
  Result := bcTable.RecordCountLocks;
End;

Procedure TfsSrBaseCursor.AssignRecordCountLocks(aList: TList);
Begin
  bcTable.AssignRecordCountLocks(aList);
End;

{
Function TFSSqlTableProxy.GetRecordByID(ID: TffInt64;
  Const LockType: TfsSrcLockType): TffResult;
Var
  aRefNr: TffInt64;
Begin
  Result := TfsSrBaseCursor(FCursorID).SetToKey(skaEqual, True, 1, 0, @ID);
  If Result = DBIERR_NONE Then
    Result := TfsSrBaseCursor(FCursorID).GetNextRecord(RecordBuffer, LockType, RecFlag, aRefNr);
End;
}

Function TfsSrBaseCursor.IsRecordLocked(aLockType: TfsSrcLockType): Boolean;
Begin
  Result := bcTable.IsRecordLocked(Database.TransactionInfo, CursorID,
    bcInfo.refNr, aLockType);
End;

Function TfsSrBaseCursor.WhoRecordLocked(aLockType: TfsSrcLockType): String;
Begin
  Result := bcTable.WhoRecordLocked(Database.TransactionInfo, CursorID,
    bcInfo.refNr, aLockType);
End;
{Begin !!.03}
{--------}

Procedure TfsSrBaseCursor.ListBLOBFreeSpace(aTI: PffTransInfo;
  Const aInMemory: Boolean;
  aStream: TStream);
Begin
  Assert(bcTable <> Nil);
  bcTable.ListBLOBFreeSpace(aTI, aInMemory, aStream);
End;
{End !!.03}
{--------}

Function TfsSrBaseCursor.NotifyExtenders(Const anAction: TffEngineAction;
  Const aFailAction: TffEngineAction): TffResult;
Var
  anExtender: TFSBaseEngineExtender;
  anIndex: Longint;
  anIndex2: Longint;
Begin
  Result := DBIERR_NONE;
  If assigned(bcExtenders) Then
    For anIndex := 0 To pred(bcExtenders.Count) Do
      Begin
        anExtender := TFSBaseEngineExtender
          (TfsIntListItem(bcExtenders[anIndex]).KeyAsInt);
        If (anAction In anExtender.InterestedActions) Or
          (anExtender.InterestedActions = []) Then
          Begin
            Result := anExtender.Notify(Self, anAction); {!!.06}
            {since we aren't ignoring Notify's error code, we must
             capture it.  If an extender reports an error we will not
             process the rest of the extenders and we will notify the
             previous extenders that we are "undoing" the previous action}
            If Result <> DBIERR_NONE Then
              Begin
                For anIndex2 := 0 To pred(anIndex) Do
                  Begin
                    anExtender := TFSBaseEngineExtender
                      (TfsIntListItem(bcExtenders[anIndex2]).KeyAsInt);
                    anExtender.Notify(Self, aFailAction);
                  End;
                break;
              End;
          End;
      End;
End;
{--------}

Function TfsSrBaseCursor.OverrideFilter(aExpression: pCANExpr;
  aTimeout: TffWord32)
  : TffResult;
Begin
  Result := DBIERR_NONE;
  Try
    bcFilterSav := bcFilter;
    bcFilter := Nil;
    If Assigned(aExpression) Then
      bcFilter := TfsSrcFilter.Create(Self, bcTable.Dictionary, {!!.11}
        aExpression,
        aTimeout);
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E,
          bcEngine.FEventLog,
          bcEngine.bseGetReadOnly);
      End;
  End;
End;
{--------}

Procedure TfsSrBaseCursor.RelContentLock(aMode: TfsContentLockMode);
Begin
  If (fffaBLOBChainSafe In bcGetAttribs) Or {!!.05}
  (bcExclOwner And (Not bcTable.Dictionary.HasBLOBFields)) Then {!!.03} {!!.05}
    Exit;

  Assert(assigned(bcDatabase.Transaction) Or (aMode = fsclmRead));

  If assigned(bcDatabase.Transaction) Then
    Begin
      { Content locks obtained by a transaction via AcqContentLock are freed when
        the transaction's locks are released. }
      If aMode = fsclmCommit Then
        bcTable.EndCommit(bcDatabase.DatabaseID);
    End
  Else
    Begin {!!.05 - Start}
      InterlockedDecrement(bcNumReadLocks);
      { If the number of read locks ever goes below 0, it's outta whack.}
      //xxlock
      Assert(bcNumReadLocks >= 0);
      If (bcNumReadLocks = 0) Then
        bcTable.EndRead;
    End; {!!.05 - End}
End;
{--------}

Procedure TfsSrBaseCursor.RelRecordLock(aAllLocks: boolean);

  Procedure RelRecordAll;
  Var
    iResult: TffResult;
    SavedKey: PffByteArray;
  Begin
    iResult := DBIERR_NONE;
    SetToBegin;
    AcqContentLock(fsclmRead);
    Try
      {set count to zero}
      {save the current position}
      bcSaveCurInfo;
      FFGetMem(SavedKey, bcKID.kidCompareData^.cdKeyLen);
      Try
        Move(bcCurKey^, SavedKey^, bcKID.kidCompareData^.cdKeyLen);
        {while not EOF or other error do}
        While (iResult = DBIERR_NONE) Do
          Begin
            { Check for timeout. }
            If FFGetRetry < GetTickCount Then
              FSRaiseExceptionNoData(EfsServerException, fsStrResServer,
                fserrGeneralTimeout);
            {readnext key}
            iResult := Table.GetNextKey(bcKID, bcInfo.refNr, Database.TransactionInfo,
              bcCurKey, bcInfo.KeyPath);
            If (iResult = DBIERR_NONE) Then
              Begin
                {check that we're in range if required}
                If Not FFI64IsZero(bcInfo.refNr) Then
                  Begin
                    bcTable.RemoveLocksForCursor(bcDatabase.DatabaseID,
                      CursorID, bcInfo.refNr,
                      bcDatabase.TransactionInfo);
                    If FFCmpI64(bcInfo.refNr, bcLockedRefNum) = 0 Then
                      FFInitI64(bcLockedRefNum);
                  End;
              End;
          End;
        {endwhile}
      Finally
        {reset current position}
        bcRestoreCurInfo;
        Move(SavedKey^, bcCurKey^, bcKID.kidCompareData^.cdKeyLen);
        FFFreeMem(SavedKey, bcKID.kidCompareData^.cdKeyLen);
      End;
    Finally
      RelContentLock(fsclmRead);
    End;
  End;

Begin
  If aAllLocks Then
    Begin
      //If bcDatabase.Transaction <> Nil Then
      //  bcTable.RelLock(CursorID, True)
      //Else
      RelRecordAll;
    End
  Else If Not FFI64IsZero(bcInfo.refNr) Then
    Begin
      bcTable.RemoveLocksForCursor(bcDatabase.DatabaseID,
        CursorID, bcInfo.refNr,
        bcDatabase.TransactionInfo);
      If FFCmpI64(bcInfo.refNr, bcLockedRefNum) = 0 Then
        FFInitI64(bcLockedRefNum);
    End;
End;
{--------}

Procedure TfsSrBaseCursor.RelTableLock(aAllLocks: Boolean);
Begin
  bcTable.RelLock(CursorID, aAllLocks);
End;
{--------}

Procedure TfsSrBaseCursor.RemoveIfUnused;
Begin
  If (State = ffosClosing) Then
    Free;
End; {!!.05 - End added}
{--------}

Function TfsSrBaseCursor.RestoreFilter: TffResult;
Begin
  Result := DBIERR_NONE;
  Try
    bcFilter.Free;
    bcFilter := bcFilterSav;
    bcFilterSav := Nil;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E,
          bcEngine.FEventLog,
          bcEngine.bseGetReadOnly);
      End;
  End;
End;
{--------}

Procedure TfsSrBaseCursor.SetAutoInc(aValue: Int64; aStep: Longint);
Begin
  AcqContentLock(fsclmWrite);
  FFTblSetAutoInc(bcTable.Files[0], bcDatabase.TransactionInfo, aValue, aStep);
End;

Function TfsSrBaseCursor.NextAutoInc: Int64;
Begin
  AcqContentLock(fsclmWrite);
  Result := FFTblNextAutoInc(bcTable.Files[0], bcDatabase.TransactionInfo);
End;

Function TfsSrBaseCursor.ReadLastAutoInc: Int64;
Begin
  AcqContentLock(fsclmWrite);
  Result := FFTblLastAutoInc(bcTable.Files[0], bcDatabase.TransactionInfo);
End;

Procedure TfsSrBaseCursor.SetMaxRecords(aValue: Longword);
Begin
  AcqContentLock(fsclmWrite);
  FFTblSetMaxRecords(bcTable.Files[0], bcDatabase.TransactionInfo, aValue);
End;

Procedure TfsSrBaseCursor.SetTableFlags(aValue: Word);
Begin
  AcqContentLock(fsclmWrite);
  FFTblSetTableFlags(bcTable.Files[0], bcDatabase.TransactionInfo, aValue);
End;

Procedure TfsSrBaseCursor.SetTablePassword(aValue: Longword);
Begin
  AcqContentLock(fsclmWrite);
  FFTblSetTablePassword(bcTable.Files[0], bcDatabase.TransactionInfo, aValue);
End;

Procedure TfsSrBaseCursor.SetTablePasswordRest(aValue: Longword);
Begin
  AcqContentLock(fsclmWrite);
  FFTblSetTablePasswordRest(bcTable.Files[0], bcDatabase.TransactionInfo, aValue);
End;

Procedure TfsSrBaseCursor.SetTableDBID(aValue: Longword);
Begin
  AcqContentLock(fsclmWrite);
  FFTblSetTableDBID(bcTable.Files[0], bcDatabase.TransactionInfo, aValue);
End;
{--------}

Function TfsSrBaseCursor.SetFilter(aExpression: pCANExpr;
  aTimeout: TffWord32): TffResult;

Begin
  Result := DBIERR_NONE;
  Try
    bcFilter.Free;
    bcFilter := Nil;
    If Assigned(aExpression) Then
      bcFilter := TfsSrcFilter.Create(Self, bcTable.Dictionary, {!!.11}
        aExpression, aTimeout); {!!.11}
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, bcEngine.EventLog, bcEngine.bseGetReadOnly);
      End;
  End;
End;
{Begin !!.01}
{--------}

Function TfsSrBaseCursor.ShouldClose: boolean;
Begin
  Result := (bcDatabase.Transaction = Nil) And (soState = ffosClosing);
End;
{End !!.01}
{--------}

Function TfsSrBaseCursor.SortRecords(aFieldsArray: TffFieldList;
  Const aOrderByArray: TfsOrderByArray;
  Const aNumFields: Integer): TffResult;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  aRecord: PffByteArray;
  aTransID: TffTransID;
  RecLen: Longint;
  SortEngine: TffSrBaseSortEngine;
Begin
  aflag := 0;
  { The cursor must have Exclusive Read-Write access. }
  Result := bcCheckExclusiveReadWrite;
  If Result <> DBIERR_NONE Then
    Exit;
  { Create the sort engine. }
  SortEngine := ffcSortEngineClass.Create(bcEngine, bcDatabase, aFieldsArray,
    aOrderByArray, aNumFields,
    bcTable.Dictionary, bcIndexID);
  SortEngine.FUserName := Self.Client.clUserID;
  RecLen := bcTable.Dictionary.RecordLength;
  FFGetMem(aRecord, RecLen);
  Try
    { Start a transaction. }
    bcEngine.seTransactionStart(bcDatabase, False, True, aTransID);
    Try
      { Position to the beginning of the table. }
      Result := DBIERR_NONE;
      SetToBegin;

      { Walk through the records, posting them to the sort engine. }
      While (Result = DBIERR_NONE) Do
        Begin
          Result := GetNextRecord(aRecord, ffsltNone, aflag, arefnr);
          If Result = DBIERR_NONE Then
            Begin
              SortEngine.Put(aRecord);
            End;
        End;
      bcEngine.seTransactionCommit(bcDatabase);
    Except
      bcEngine.seTransactionRollback(bcDatabase);
      Raise;
    End;

    bcEngine.seTransactionStart(bcDatabase, False, True, aTransID);
    Try
      { Position to start of table. We will overwrite existing records
        in order to preserve BLOB data. }
      Result := DBIERR_NONE;
      SetToBegin;
      { Read the records back from the sort engine. }
      While (Result = DBIERR_NONE) Do
        Begin
          If SortEngine.Get(aRecord) Then
            Begin
              GetNextRecord(Nil, ffsltNone, aflag, arefnr);
              Result := ModifyRecord(aRecord, True, tluDatabase, 0, False, False);
            End
          Else
            break;
        End;
      //      end;
      {End !!.01}
      bcEngine.seTransactionCommit(bcDatabase);
    Except
      { Rollback if an exception occurs. }
      bcEngine.seTransactionRollback(bcDatabase);
      Raise;
    End;
  Finally
    FFFreeMem(aRecord, RecLen);
    SortEngine.Free;
  End;

  SetToBegin;

End;
{====================================================================}

{===TfsSrcCursor======================================================}

Constructor TfsSrcCursor.Create(anEngine: TFSServer;
  aDatabase: TfsSrcDatabase;
  Const aTimeout: Longint);
Begin
  bcTableClass := TfsSrcTable;
  Inherited Create(anEngine, aDatabase, aTimeout);
End;
{--------}

Destructor TfsSrcCursor.Destroy;
Begin

  { Notify extenders. }
  NotifyExtenders(ffeaBeforeCursorClose, ffeaNoAction);

  {Begin !!.02}
  bcEngine.TableList.BeginRead;
  Try
    { If we exclusively opened the table then remove the mark from the
      table. }
    If bcExclOwner Then
      Begin
        bcTable.SetExclOwner(fsc_W32NoValue);
        bcExclOwner := False;
      End;

    { Free the table locks held by the cursor. }
    If assigned(bcTable) Then
      bcTable.RelLock(CursorID, True);
  Finally
    bcEngine.TableList.EndRead;
  End;
  {End !!.02}

  If (bcRng1Key <> Nil) Then
    Begin
      FFFreeMem(bcRng1Key, scKeyLen);
      bcRng1Key := Nil;
    End;
  If (bcRng2Key <> Nil) Then
    Begin
      FFFreeMem(bcRng2Key, scKeyLen);
      bcRng2Key := Nil;
    End;
  If (bcCurKey <> Nil) Then
    Begin
      FFFreeMem(bcCurKey, scKeyLen);
      bcCurKey := Nil;
    End;
  bcFilter.Free;
  bcFilter := Nil;
  bcFilterSav.Free;
  bcFilterSav := Nil;
  Inherited Destroy;
End;
{--------}

Function TfsSrcCursor.AddIndexToTable(Const aIndexDesc: TffIndexDescriptor): TffResult;
Begin

  { The cursor must have Exclusive Read-Write access. }
  Result := bcCheckExclusiveReadWrite;
  If Result <> DBIERR_NONE Then
    Exit;

  { The index descriptor cannot be a user-defined index. }
  If (aIndexDesc.idCount = -1) Then
    Begin
      Result := DBIERR_INVALIDINDEXTYPE;
      Exit;
    End;

  { The index descriptor must be valid. }
  If Not Table.Dictionary.IsIndexDescValid(aIndexDesc) Then
    Begin
      Result := DBIERR_INVALIDIDXDESC;
      Exit;
    End;

  { The index name cannot already exist. }
  If (Table.Dictionary.GetIndexFromName(aIndexDesc.idName) <> -1) Then
    Begin
      Result := DBIERR_INDEXEXISTS;
      Exit;
    End;

  { There must be room for the new index. }
  If (Table.Dictionary.IndexCount = fscl_MaxIndexes) Then
    Begin
      Result := DBIERR_INDEXLIMIT;
      Exit;
    End;

  { Let the table do its stuff. }
  Result := DBIERR_NONE;
  AcqContentLock(fsclmWrite);
  Table.AddIndex(aIndexDesc, Database.TransactionInfo)
End;
{--------}

Procedure TfsSrcCursor.bcInit(Const aOpenMode: TffOpenMode;
  Const aShareMode: TffShareMode;
  Const aExclContLock: Boolean); {!!.10}
Begin
  Inherited bcInit(aOpenMode, aShareMode, aExclContLock); {!!.10}

  { Resolve any special build key and compare key routine links
    (i.e., user-defined indexes) for the new table. }
  {TfsSrcTable(bcTable).ResolveDynamicLinks;}{!!.06}

  { Get our work areas for the key. }
  bcKID.kidCompareData := @bcCompareData;
  scKeyLen := bcTable.Dictionary.IndexKeyLength[bcIndexID];
  FFGetMem(bcCurKey, scKeyLen);
  FFGetMem(bcRng1Key, scKeyLen);
  FFGetMem(bcRng2Key, scKeyLen);

  { Initialise our key index data record. }
  bcTable.MakeKIDForCursor(bcIndexID, bcKID);

  { Set up the position of the cursor to BOF. }
  SetToBegin;
End;
{--------}

Procedure TfsSrcCursor.bcTableOpenPreconditions(aTable: TfsSrcBaseTable;
  Const aIndexName: String;
  Var aIndexID: Longint;
  Const aOpenMode: TffOpenMode);
Begin

  { Validate the index information; if the index name is non-blank
    it must exist and will supercede the index number; if the index
    name is blank the index number must exist}
  If (aIndexName <> '') Then
    Begin
      aIndexID := aTable.Dictionary.GetIndexFromName(aIndexName);
      If (aIndexID = -1) Then
        FSRaiseException(EfsException, fsStrResServer, fserrUnknownIndex,
          [aTable.BaseName, aIndexName, aIndexID]);
    End
  Else If (0 > aIndexID) Or (aIndexID >= aTable.Dictionary.IndexCount) Then
    FSRaiseException(EfsException, fsStrResServer, fserrUnknownIndex,
      [aTable.BaseName, aIndexName, aIndexID]);

  { If the table's data file is open in read-only mode it means the
    physical file is read-only: hence this call's openmode must be
    read-only as well. }
  If (aTable.Files[0]^.fiOpenMode = omReadOnly) And
    (aOpenMode <> omReadOnly) Then
    FSRaiseException(EfsException, fsStrResServer, fserrCursorReadOnly,
      [aTable.BaseName]);

End;
{--------}

Function TfsSrcCursor.CheckBookmark(aBookmark: PffByteArray): TffResult;
Var
  CheckHash: Longint;
Begin
  Result := DBIERR_INVALIDBOOKMARK;
  If (aBookmark = Nil) Then
    Exit;
  With PfsSrBookmark(aBookmark)^ Do
    Begin
      If (sbIndexID <> IndexID) Then
        Exit;
      If (sbKeyLen <> scKeyLen) Then
        Exit;
      CheckHash := FSCalcELFHash(sbIndexID,
        fscl_FixedBookmarkSize - sizeof(sbHash) + sbKeyLen);
      If (sbHash <> CheckHash) Then
        Exit;
    End;
  Result := DBIERR_NONE;
End;
{--------}

Procedure TfsSrcCursor.ClearIndex;
Begin
  With bcCompareData Do
    Begin
      cdFldCnt := 0;
      cdPartLen := 0;
    End;
  AcqContentLock(fsclmWrite);
  FFTblDeleteAllKeys(Database.TransactionInfo, bcKID);
End;
{--------}

Function TfsSrcCursor.CloneCursor(aOpenMode: TffOpenMode): TfsSrBaseCursor;
Begin
  {NOTE: we are not checking rights for this action because the client
         had the rights to open the cursor}

  { Resolve the open mode. }
  If (bcOpenMode = omReadOnly) Then
    aOpenMode := omReadOnly;

  { Create the cursor. }
  Result := bcEngine.CursorClass.Create(bcEngine, {!!.06}
    bcDatabase,
    soTimeout);
  Result.Open(bcTable.BaseName, '', bcIndexID, aOpenMode, smShared,
    bcTable.IsServerTable, False, [], True);

  AcqContentLock(fsclmRead);
  Try
    { Set up all of the position, range, etc, fields. }
    Result.bcKID := bcKID;
    Result.bcKID.kidCompareData := @Result.bcCompareData;
    Result.bcCompareData := bcCompareData;
    Result.bcInfo := bcInfo;
    If bcInfo.KeyValid Then
      Move(bcCurKey^, Result.bcCurKey^, scKeyLen);
    Result.bcHasRange := bcHasRange;
    If bcHasRange Then
      Begin
        Result.bcRng1Valid := bcRng1Valid;
        If bcRng1Valid Then
          Begin
            Move(bcRng1Key^, Result.bcRng1Key^, scKeyLen);
            Result.bcRng1FldCnt := bcRng1FldCnt;
            Result.bcRng1PtlLen := bcRng1PtlLen;
            Result.bcRng1Incl := bcRng1Incl;
          End;
        Result.bcRng2Valid := bcRng2Valid;
        If bcRng2Valid Then
          Begin
            Move(bcRng2Key^, Result.bcRng2Key^, scKeyLen);
            Result.bcRng2FldCnt := bcRng2FldCnt;
            Result.bcRng2PtlLen := bcRng2PtlLen;
            Result.bcRng2Incl := bcRng2Incl;
          End;
      End;
    If Assigned(bcFilter) Then
      Result.SetFilter(bcFilter.Expression, bcFilter.Timeout);
  Finally
    RelContentLock(fsclmRead);
  End;
End;
{--------}

Function TfsSrcCursor.CompareBookmarks(aBookmark1, aBookmark2: PffByteArray;
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
    cpBOF:
      If (BM2^.sbPos = cpBOF) Then
        CmpResult := 0
      Else
        CmpResult := -1;
    cpEOF:
      If (BM2^.sbPos = cpEOF) Then
        CmpResult := 0
      Else
        CmpResult := 1;
    Else
      {bookmark 1 is on a crack or on a record}
      Case BM2^.sbPos Of
        cpUnknown: CmpResult := 1;
        cpBOF: CmpResult := 1;
        cpEOF: CmpResult := -1;
        Else
          {bookmark 2 is also on a crack or on a record}
          {check the reference numbers, if equal the key ought to be}
          If (ffCmpI64(BM1^.sbRefNr, BM2^.sbRefNr) = 0) Then
            CmpResult := 0
          Else
            Begin
              {compare the keys}
              With bcCompareData Do
                Begin
                  cdFldCnt := 0;
                  cdPartLen := 0;
                End;
              CmpResult := Table.CompareKeysForCursor(bcKID,
                PffByteArray(@BM1^.sbKey),
                PffByteArray(@BM2^.sbKey));
              If (CmpResult = 0) Then
                If (ffCmpI64(BM1^.sbRefNr, BM2^.sbRefNr) = -1) Then
                  CmpResult := -1
                Else
                  CmpResult := 1;
            End;
      End; {case}
  End; {case}
End;
{--------}

Function TfsSrcCursor.DropIndexFromTable(Const aIndexName: TffDictItemName;
  aIndexID: Longint): TffResult;
Var
  CompareData: TffCompareData;
  KID: TffKeyIndexData;
Begin

  {if the index name is set, convert to an index ID}
  If (aIndexName <> '') Then
    aIndexID := Table.Dictionary.GetIndexFromName(aIndexName);

  {check the index number (count index 0 as invalid as well)}
  If (aIndexID <= 0) Or (aIndexID >= Table.Dictionary.IndexCount) Then
    Begin
      Result := DBIERR_NOSUCHINDEX;
      Exit;
    End;

  {the index number cannot be our index number}
  If (aIndexID = IndexID) Then
    Begin
      Result := DBIERR_ACTIVEINDEX;
      Exit;
    End;

  { The cursor must have Exclusive Read-Write access. }
  Result := bcCheckExclusiveReadWrite;
  If Result <> DBIERR_NONE Then
    Exit;

  { Delete all the keys and then drop the index. }
  Result := DBIERR_NONE;
  KID.kidCompareData := @CompareData;
  Table.MakeKIDForCursor(aIndexID, KID);
  AcqContentLock(fsclmWrite);
  FFTblDeleteAllKeys(Database.TransactionInfo, KID);
  Table.DropIndex(Database.TransactionInfo, aIndexID);

End;
{--------}

Function TfsSrcCursor.ExtractKey(aData: PffByteArray; aKey: PffByteArray): TffResult;
Var
  aFlag: Byte;
Begin
  aflag := 0;
  Result := DBIERR_NOCURRREC;
  If (aData = Nil) And (bcInfo.pos = cpOnRecord) Then
    Begin
      aData := bcRecordData;
      AcqContentLock(fsclmRead);
      Try
        Table.GetRecord(bcDatabase.TransactionInfo, {!!.10}
          bcDatabase.DatabaseID, {!!.10}
          CursorID, bcInfo.refNr, aData, {!!.10}
          ffsltNone, tluDatabase, False, False, aflag); {!!.02}
      Finally
        RelContentLock(fsclmRead);
      End;
      If Assigned(bcFilter) Then
        If Not bcFilter.MatchesRecord(aData) Then
          aData := Nil;
    End;
  If (aData <> Nil) Then
    Begin
      Result := Table.BuildKeyForRecord(IndexID, aData, aKey, 0, 0);
    End;
End;
{--------}

Function TfsSrcCursor.GetBookmark(aBookmark: PffByteArray): TffResult;
Begin
  Result := DBIERR_NONE;
  AcqContentLock(fsclmRead);
  Try
    FillChar(PfsSrBookmark(aBookmark)^, fscl_FixedBookmarkSize, 0);
    With PfsSrBookmark(aBookmark)^ Do
      Begin
        sbIndexID := IndexID;
        sbPos := bcInfo.pos;
        sbKeyValid := bcInfo.KeyValid;
        sbRefNr := bcInfo.refNr;
        sbKeyLen := scKeyLen;
        If bcInfo.KeyValid Then
          Move(bcCurKey^, sbKey, scKeyLen)
        Else
          FillChar(sbKey, scKeyLen, 0);
        sbHash := FSCalcELFHash(sbIndexID,
          fscl_FixedBookmarkSize - sizeof(sbHash) + sbKeyLen);
      End;
  Finally
    RelContentLock(fsclmRead);
  End;
End;
{--------}

Function TfsSrcCursor.GetBookmarkSize: Integer;
Begin
  Result := fscl_FixedBookmarkSize + scKeyLen;
End;
{--------}

Function TfsSrcCursor.GetNextRecord(aData: PffByteArray;
  aLockType: TfsSrcLockType; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Var
  KeyCompareResult: Integer;
  Action: TffSearchKeyAction;
Begin
  aRefnr.iLow := 0;
  aRefnr.iHigh := 0;
  { If we are at EOF, then obviously there's no next record. }
  If (bcInfo.pos = cpEOF) Then
    Begin
      Result := DBIERR_EOF;
      aRefnr := bcInfo.refNr;
      Exit;
    End;

  AcqContentLock(fsclmRead);
  bcInfoLock.Lock; {!!.06}
  Try
    { If our position is at BOF and we have a range active, position the
      index at the start of the range}
    If (bcInfo.pos = cpBOF) And bcHasRange And bcRng1Valid Then
      Begin
        { Position at start of range. }
        If bcRng1Incl Then
          Action := skaGreaterEqual
        Else
          Action := skaGreater;
        { Note: the following call will always return true in this case. }
        Move(bcRng1Key^, bcCurKey^, scKeyLen);
        With bcCompareData Do
          Begin
            cdFldCnt := bcRng1FldCnt;
            cdPartLen := bcRng1PtlLen;
          End;
        Table.FindKey(bcKID, bcInfo.refNr, Database.TransactionInfo,
          bcCurKey, bcInfo.KeyPath, Action);

        { Is the keypath positioned at EOF? }
        If (bcInfo.KeyPath.kpPos = kppEOF) Then
          Begin
            {Yes.  The start of the range is at EOF, so it's not likely we'll find a
             'next record <g>. }
            Result := DBIERR_EOF;
            SetToEnd;
            aRefnr := bcInfo.refNr;
            Exit;
          End;

        { Make sure the keypath is on the crack before the key so that the next
          key call returns the right record. }
        If (bcInfo.KeyPath.kpPos = kppOnKey) Then
          Begin
            Assert(bcInfo.keyPath.kpCount > 0);
            bcInfo.KeyPath.kpPos := kppOnCrackBefore;
          End;

      End
        { Otherwise do we need to rebuild the key path? }
    Else If (Not bcIsCurKeyPathValid) Then
      bcRebuildKeyPath; {!!.05}

    { Make sure that we have somewhere to read the record into. }
    If (aData = Nil) Then
      aData := bcRecordData;

    { Get the next record. }
    With bcCompareData Do
      Begin
        cdFldCnt := 0;
        cdPartLen := 0;
      End;

    If Assigned(bcFilter) Then
      bcFilter.BeginTimeout;
    Repeat
      Result := bcTable.GetNextRecord(bcDatabase.TransactionInfo,
        bcDatabase.DatabaseID, {!!.10}
        CursorID, bcKID, bcInfo.refNr, bcCurKey,
        bcInfo.KeyPath, aData, aLockType, aFlag);
      If (Result <> DBIERR_NONE) Then
        Begin
          If (Result = DBIERR_EOF) Then
            Begin
              SetToEnd;
              aRefnr := bcInfo.refNr;
            End;
          Exit;
        End;
      {in theory we're on a record}
      bcInfo.Deleted := False;
      bcInfo.KeyValid := True;
      bcInfo.pos := cpOnRecord;
      {check that we're in range if required}
      If bcHasRange And bcRng2Valid Then
        Begin
          {check whether beyond end of range}
          With bcCompareData Do
            Begin
              cdFldCnt := bcRng2FldCnt;
              cdPartLen := bcRng2PtlLen;
            End;
          KeyCompareResult := bcTable.CompareKeysForCursor(bcKID, bcCurKey, bcRng2Key);
          If (KeyCompareResult > 0) Or
            ((KeyCompareResult = 0) And (Not bcRng2Incl)) Then
            Begin
              Result := DBIERR_EOF;
              SetToEnd;
              aRefnr := bcInfo.refNr;
            End;
        End;
      {Begin !!.03}
    Until (Result <> DBIERR_NONE) Or ((Not Assigned(bcFilter) Or
      bcFilter.MatchesRecord(aData) Or bcFilter.CheckTimeout(Result)) And
      (Not Assigned(bcFilterSav) Or bcFilterSav.MatchesRecord(aData)));
    {End !!.03}

        { Place the lock if needed... record will not be read again}
    {Begin !!.02}
    If (Result = DBIERR_NONE) Then
      Begin
        aRefnr := bcInfo.refNr;
        If aData <> bcRecordData Then
          Move(aData^, bcRecordData^, bcRecordLen);
        If (aLockType <> ffsltNone) Then
          Result := bcTable.GetRecord(bcDatabase.TransactionInfo, {!!.10}
            bcDatabase.DatabaseID, {!!.10}
            CursorID, {!!.10}
            bcInfo.refNr, Nil, aLockType, tluDatabase, False, False, aflag); {!!.02}
        aRefnr := bcInfo.refNr;
      End
    Else
      Begin
        aRefnr.iLow := 0;
        aRefnr.iHigh := 0;
      End;
    {End !!.02}
  Finally
    bcInfoLock.Unlock; {!!.06}
    RelContentLock(fsclmRead);
  End;
End;
{--------}

Function TfsSrcCursor.GetPriorRecord(aData: PffByteArray; aLockType: TfsSrcLockType; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Var
  KeyCompareResult: Integer;
  Action: TffSearchKeyAction;
Begin
  aRefnr.iLow := 0;
  aRefnr.iHigh := 0;
  { If we are at BOF, then obviously there's no prior record. }
  If (bcInfo.pos = cpBOF) Then
    Begin
      Result := DBIERR_BOF;
      aRefnr := bcInfo.refNr;
      Exit;
    End;

  AcqContentLock(fsclmRead);
  bcInfoLock.Lock; {!!.06}
  Try
    { If our position is at EOF and we have a range active, position the
      index at the end of the range. }
    If (bcInfo.pos = cpEOF) And bcHasRange And bcRng2Valid Then
      Begin
        { Position at end of range. }
        If bcRng2Incl Then
          Action := skaGreater
        Else
          Action := skaGreaterEqual;

        { Note: the following call will always return true in this case. }
        Move(bcRng2Key^, bcCurKey^, scKeyLen);
        With bcCompareData Do
          Begin
            cdFldCnt := bcRng2FldCnt;
            cdPartLen := bcRng2PtlLen;
          End;
        bcTable.FindKey(bcKID, bcInfo.refNr, bcDatabase.TransactionInfo,
          bcCurKey, bcInfo.KeyPath, Action);
      End
        { Otherwise, do we need to rebuild the key path? }
    Else If (Not bcIsCurKeyPathValid) Then
      bcRebuildKeyPath; {!!.05}

    { Make sure that we have somewhere to read the record into. }
    If (aData = Nil) Then
      aData := bcRecordData;

    { Get the previous record. }
    With bcCompareData Do
      Begin
        cdFldCnt := 0;
        cdPartLen := 0;
      End;

    If Assigned(bcFilter) Then
      bcFilter.BeginTimeout;
    Repeat
      Result := bcTable.GetPriorRecord(bcDatabase.TransactionInfo,
        bcDatabase.DatabaseID, {!!.10}
        CursorID, bcKID,
        bcInfo.refNr, bcCurKey,
        bcInfo.KeyPath, aData, ffsltNone, aflag);
      If (Result <> DBIERR_NONE) Then
        Begin
          If (Result = DBIERR_BOF) Then
            Begin
              SetToBegin;
              aRefnr := bcInfo.refNr;
            End;
          Exit;
        End;

      { In theory we're on a record. }
      bcInfo.Deleted := False;
      bcInfo.KeyValid := True;
      bcInfo.pos := cpOnRecord;

      { Check that we're in range if required. }
      If bcHasRange And bcRng1Valid Then
        Begin
          {check whether beyond start of range}
          With bcCompareData Do
            Begin
              cdFldCnt := bcRng1FldCnt;
              cdPartLen := bcRng1PtlLen;
            End;
          KeyCompareResult := bcTable.CompareKeysForCursor(bcKID, bcCurKey, bcRng1Key);
          If (KeyCompareResult < 0) Or
            ((KeyCompareResult = 0) And (Not bcRng1Incl)) Then
            Begin
              Result := DBIERR_BOF;
              SetToBegin;
              aRefnr := bcInfo.refNr;
            End;
        End;
    Until (Result <> DBIERR_NONE) Or ((Not Assigned(bcFilter) Or
      bcFilter.MatchesRecord(aData) Or bcFilter.CheckTimeout(Result)) And
      (Not Assigned(bcFilterSav) Or bcFilterSav.MatchesRecord(aData)));

    //Until (Result <> DBIERR_NONE) Or Not Assigned(bcFilter) Or
      //bcFilter.MatchesRecord(aData) Or bcFilter.CheckTimeout(Result);

    { Place the lock if needed... record will not be read again. }
{Begin !!.02}
    If (Result = DBIERR_NONE) Then
      Begin
        aRefnr := bcInfo.refNr;
        If aData <> bcRecordData Then
          Move(aData^, bcRecordData^, bcRecordLen);
        If (aLockType <> ffsltNone) Then
          Result := bcTable.GetRecord(bcDatabase.TransactionInfo, {!!.10}
            bcDatabase.DatabaseID, {!!.10}
            CursorID, {!!.10}
            bcInfo.refNr, Nil, aLockType, tluDatabase, False, False, aflag); {!!.02}
        aRefnr := bcInfo.refNr;
      End
    Else
      Begin
        aRefnr.iLow := 0;
        aRefnr.iHigh := 0;
      End;
    {End !!.02}
  Finally
    bcInfoLock.Unlock; {!!.06}
    RelContentLock(fsclmRead);
  End;

End;
{--------}

Function TfsSrcCursor.GetRecordCount(Var aRecCount: Longword): TffResult;
Var
  aFlag: Byte;
  Action: TffSearchKeyAction;
  KeyCompareResult: Integer;
  SavedKey: PffByteArray;
  Info: TffRecordInfo;
Begin
  Result := DBIERR_NONE;
  AcqContentLock(fsclmRead);
  Try
    If bcHasRange Or Assigned(bcFilter) Then
      Begin
        {set count to zero}
        aRecCount := 0;
        {save the current position}
        bcSaveCurInfo;
        FFGetMem(SavedKey, bcKID.kidCompareData^.cdKeyLen); {!!.06}
        Try
          Move(bcCurKey^, SavedKey^, bcKID.kidCompareData^.cdKeyLen);
          {BOF}
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
              Table.FindKey(bcKID, bcInfo.refNr, Database.TransactionInfo, bcCurKey,
                bcInfo.KeyPath, Action);
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
                Begin
                  Assert(bcInfo.KeyPath.kpCount > 0);
                  bcInfo.KeyPath.kpPos := kppOnCrackBefore;
                End;
            End;
          {while not EOF or other error do}
          While (Result = DBIERR_NONE) Do
            Begin
              {Begin !!.05}
                        { Check for timeout. }
              If FFGetRetry < GetTickCount Then
                FSRaiseExceptionNoData(EfsServerException, fsStrResServer,
                  fserrGeneralTimeout);
              {End !!.05}
                        {readnext key}
              Result := Table.GetNextKey(bcKID, bcInfo.refNr, Database.TransactionInfo,
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
                              Table.GetRecord(bcDatabase.TransactionInfo, {!!.10}
                                bcDatabase.DatabaseID, {!!.10}
                                CursorID, bcInfo.refNr, {!!.10}
                                bcRecordData, ffsltNone, tluDatabase, False, False, aflag); {!!.02}
                              If bcFilter.MatchesRecord(bcRecordData) Then
                                inc(aRecCount);
                            End
                          Else
                            inc(aRecCount);
                        End;
                    End
                  Else {end of range = end of index path}
                    Begin
                      If Assigned(bcFilter) Then
                        Begin
                          Table.GetRecord(bcDatabase.TransactionInfo, {!!.10}
                            bcDatabase.DatabaseID, {!!.10}
                            CursorID, bcInfo.refNr, {!!.10}
                            bcRecordData, ffsltNone, tluDatabase, False, False, aflag); {!!.02}
                          If bcFilter.MatchesRecord(bcRecordData) Then
                            inc(aRecCount);
                        End
                      Else
                        inc(aRecCount);
                    End;
                End;
            End;
          Result := DBIERR_NONE;
          {endwhile}
        Finally
          {reset current position}
          bcRestoreCurInfo;
          Move(SavedKey^, bcCurKey^, bcKID.kidCompareData^.cdKeyLen);
          FFFreeMem(SavedKey, bcKID.kidCompareData^.cdKeyLen); {!!.06}
        End;
      End
    Else
      Begin
        FFTblGetRecordInfo(Table.Files[0], Database.TransactionInfo, Info);
        aRecCount := Info.riRecCount;
      End;
  Finally
    RelContentLock(fsclmRead);
  End;
End;
{--------}

Function TfsSrcCursor.GetRecordForKey(aDirectKey: boolean;
  aFieldCount: Integer;
  aPartialLen: Integer;
  aKeyData: PffByteArray;
  aData: PffByteArray;
  aFirstCall: Boolean): TffResult;
Var
  aFlag: Byte;
  Action: TffSearchKeyAction;
  aTI: PffTransInfo;
  RecFound: boolean;
  KeyToFind: PffByteArray;
  TmpCompareData: TffCompareData; {!!.11}
Begin
  aflag := 0;
  {calculate the key}
  If aDirectKey Then
    Move(aKeyData^, bcCurKey^, scKeyLen)
  Else If (IndexID = 0) Then
    Begin
      Result := DBIERR_INVALIDINDEXTYPE;
      Exit;
    End
  Else
    Begin
      Result := Table.BuildKeyForRecord(IndexID, aKeyData, bcCurKey, aFieldCount,
        aPartialLen);
      If (Result <> DBIERR_NONE) Then
        Exit;
    End;

  AcqContentLock(fsclmRead);
  bcInfoLock.Lock; {!!.06}
  Try
    {now position the index on that exact key or the one that partially
     matches it}
    If aFirstCall Then
      Begin
        FFInitKeyPath(bcInfo.KeyPath);
        bcInfo.refNr.iLow := 0;
        bcInfo.refNr.iHigh := 0;
        bcInfo.Deleted := False;
      End;
    Action := skaEqual;
    {try to find the exact or partial key}
    With bcCompareData Do
      Begin
        cdFldCnt := aFieldCount;
        cdPartLen := aPartialLen;
      End;

    Result := DBIERR_NONE;
    aTI := Database.TransactionInfo;
    KeyToFind := Nil;
    Try
      // we need a copy of the key
      If Assigned(bcFilter) Or (Not aFirstCall) Then
        Begin
          FFGetMem(KeyToFind, scKeyLen);
          Move(bcCurKey^, KeyToFind^, scKeyLen)
        End;

      If Assigned(bcFilter) Then
        Begin
          If aData = Nil Then
            aData := bcRecordData;
          bcFilter.BeginTimeout;
        End;
      Repeat
        If aFirstCall Then
          Begin
            RecFound := Table.FindKey(bcKID, bcInfo.refNr, aTI,
              bcCurKey, bcInfo.KeyPath, Action);
            aFirstCall := False;
          End
        Else
          Begin
            RecFound := (Table.GetNextKey(bcKID, bcInfo.refNr, aTI,
              bcCurKey, bcInfo.KeyPath) = DBIERR_NONE) And
              (Table.CompareKeysForCursor(bcKID, bcCurKey, KeyToFind) = 0);
          End;

        If RecFound Then
          Begin
            TmpCompareData := bcCompareData; {!!.11}
            If IsInRange(bcCurKey) <> 0 Then
              Begin {!!.11}
                bcCompareData := TmpCompareData; {!!.11}
                Result := DBIERR_RECNOTFOUND; {!!.11}
              End
            Else
              Begin {!!.11}
                bcCompareData := TmpCompareData; {!!.11}
                If Assigned(aData) Then
                  Table.GetRecord(aTI, bcDatabase.DatabaseID, {!!.10}
                    CursorID, bcInfo.refNr, aData, ffsltNone, tluDatabase, {!!.10}
                    False, False, aflag); {!!.02}
                bcInfo.KeyValid := True;
                bcInfo.pos := cpOnRecord;
              End
          End
        Else
          Result := DBIERR_RECNOTFOUND;
      Until (Result <> DBIERR_NONE) Or {!!.11}
      ((Not Assigned(bcFilter)) Or {!!.11}
        bcFilter.MatchesRecord(aData) Or {!!.11}
        bcFilter.CheckTimeout(Result)); {!!.11}

      { If we didn't find the key then set to the end of the dataset. }
      If Result = DBIERR_RECNOTFOUND Then
        SetToEnd;
    Finally
      If Assigned(KeyToFind) Then
        FFFreeMem(KeyToFind, scKeyLen);
    End;
  Finally
    bcInfoLock.Unlock; {!!.06}
    RelContentLock(fsclmRead);
  End;
End;
{--------}

Function TfsSrcCursor.GetSetPosition(aValue: Longint; aData: PffByteArray; aLockType: TfsSrcLockType;
  Var aFlag: Byte; Var aRecNo: Longword;
  Var aRefNr: TffInt64;
  aInfoGetSetPosition: TInfoGetSetPosition;
  aSet: Boolean): TffResult;
Var
  aTmpCount: Longword;

  Function GotoRec(Var aRecCount: Longword): TffResult;
  Var
    aFlag: Byte;
    Action: TffSearchKeyAction;
    KeyCompareResult: Integer;
    SavedKey: PffByteArray;
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
    If aInfoGetSetPosition = imPosition Then
      aTmpCount := 4294967295
    Else
      aTmpCount := abs(aValue);

    AcqContentLock(fsclmRead);
    Try
      {set count to zero}
      aRecCount := 0;
      {save the current position}
      bcSaveCurInfo;
      FFGetMem(SavedKey, bcKID.kidCompareData^.cdKeyLen);
      Try
        Move(bcCurKey^, SavedKey^, bcKID.kidCompareData^.cdKeyLen);
        If (aValue < 0) Then
          Begin
            If bcHasRange And bcRng2Valid Then
              Begin
                {position at start of range}
                If bcRng2Incl Then
                  Action := skaGreater
                Else
                  Action := skaGreaterEqual;
                {note: the following FindKey call will always return true in
                 this case}
                Move(bcRng2Key^, bcCurKey^, scKeyLen);
                With bcCompareData Do
                  Begin
                    cdFldCnt := bcRng2FldCnt;
                    cdPartLen := bcRng2PtlLen;
                  End;
                Table.FindKey(bcKID, bcInfo.refNr, Database.TransactionInfo, bcCurKey,
                  bcInfo.KeyPath, Action);
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
                  Begin
                    Assert(bcInfo.KeyPath.kpCount > 0);
                    bcInfo.KeyPath.kpPos := kppOnCrackBefore;
                  End;
              End;
          End
        Else
          Begin
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
                Table.FindKey(bcKID, bcInfo.refNr, Database.TransactionInfo, bcCurKey,
                  bcInfo.KeyPath, Action);
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
                  Begin
                    Assert(bcInfo.KeyPath.kpCount > 0);
                    bcInfo.KeyPath.kpPos := kppOnCrackBefore;
                  End;
              End;
          End;
        {while not EOF or other error do}
        While (Result = DBIERR_NONE) And (aRecCount < aTmpCount) Do
          Begin
            { Check for timeout. }
            If FFGetRetry < GetTickCount Then
              FSRaiseExceptionNoData(EfsServerException, fsStrResServer,
                fserrGeneralTimeout);
            {readnext key}
            If (aValue < 0) {Or (aInfoGetSetPosition = imPosition)} Then
              Result := Table.GetPrevKey(bcKID, bcInfo.refNr, Database.TransactionInfo,
                bcCurKey, bcInfo.KeyPath)
            Else
              Result := Table.GetNextKey(bcKID, bcInfo.refNr, Database.TransactionInfo,
                bcCurKey, bcInfo.KeyPath);

            If (Result = DBIERR_NONE) Then
              Begin
                If (aValue < 0) Then
                  Begin
                    {check that we're in range if required}
                    If bcHasRange And bcRng1Valid Then
                      Begin
                        {check whether beyond end of range}
                        With bcCompareData Do
                          Begin
                            cdFldCnt := bcRng1FldCnt;
                            cdPartLen := bcRng1PtlLen;
                          End;
                        KeyCompareResult := bcTable.CompareKeysForCursor(bcKID, bcCurKey, bcRng1Key);
                        If (KeyCompareResult < 0) Or
                          ((KeyCompareResult = 0) And (Not bcRng1Incl)) Then
                          Begin
                            Result := DBIERR_BOF;
                          End
                        Else {key is in range}
                          Begin
                            If Assigned(bcFilter) Then
                              Begin
                                Table.GetRecord(bcDatabase.TransactionInfo, {!!.10}
                                  bcDatabase.DatabaseID, {!!.10}
                                  CursorID, bcInfo.refNr, {!!.10}
                                  bcRecordData, ffsltNone, tluDatabase, False, False, aflag); {!!.02}
                                If bcFilter.MatchesRecord(bcRecordData) Then
                                  inc(aRecCount);
                              End
                            Else
                              inc(aRecCount);
                          End;
                      End
                    Else {end of range = end of index path}
                      Begin
                        If Assigned(bcFilter) Then
                          Begin
                            Table.GetRecord(bcDatabase.TransactionInfo, {!!.10}
                              bcDatabase.DatabaseID, {!!.10}
                              CursorID, bcInfo.refNr, {!!.10}
                              bcRecordData, ffsltNone, tluDatabase, False, False, aflag); {!!.02}
                            If bcFilter.MatchesRecord(bcRecordData) Then
                              inc(aRecCount);
                          End
                        Else
                          inc(aRecCount);
                      End;
                  End
                Else
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
                                Table.GetRecord(bcDatabase.TransactionInfo, {!!.10}
                                  bcDatabase.DatabaseID, {!!.10}
                                  CursorID, bcInfo.refNr, {!!.10}
                                  bcRecordData, ffsltNone, tluDatabase, False, False, aflag); {!!.02}
                                If bcFilter.MatchesRecord(bcRecordData) Then
                                  inc(aRecCount);
                              End
                            Else
                              inc(aRecCount);
                          End;
                      End
                    Else {end of range = end of index path}
                      Begin
                        If Assigned(bcFilter) Then
                          Begin
                            Table.GetRecord(bcDatabase.TransactionInfo, {!!.10}
                              bcDatabase.DatabaseID, {!!.10}
                              CursorID, bcInfo.refNr, {!!.10}
                              bcRecordData, ffsltNone, tluDatabase, False, False, aflag); {!!.02}
                            If bcFilter.MatchesRecord(bcRecordData) Then
                              inc(aRecCount);
                          End
                        Else
                          inc(aRecCount);
                      End;
                  End;
              End;
          End;
        Result := DBIERR_NONE;
        If (aRecCount > 0) And aSet Then
          Begin
            Result := bcTable.GetRecord(bcDatabase.TransactionInfo,
              bcDatabase.DatabaseID,
              CursorID,
              bcInfo.refNr, Nil, aLockType, tluDatabase, False, False, aflag);
          End;
        {endwhile}
      Finally
        {reset current position}
        aRefNr := bcInfo.refNr;
        If (aRecCount = 0) Or Not aSet Then
          Begin
            bcRestoreCurInfo;
            Move(SavedKey^, bcCurKey^, bcKID.kidCompareData^.cdKeyLen);
          End;
        FFFreeMem(SavedKey, bcKID.kidCompareData^.cdKeyLen);
      End;
    Finally
      RelContentLock(fsclmRead);
    End;
  End;

Begin
  If aInfoGetSetPosition = imMoveBy Then
    Result := GotoRec(aRecNo)
  Else // not yet other
    Result := GotoRec(aRecNo);
End;

Function TfsSrcCursor.UndeleteRecord(aData: PffByteArray;
  aLockType: TfsSrcLockType; Var aRefNr: TffInt64): TffResult;
Var
  NewRefNr: TffInt64;
  SavInfo: TfsSrcCursorInfo;
  SavKey: PffByteArray;
Begin
  { Notify extenders. }
  Result := NotifyExtenders(ffeaBeforeRecInsert, ffeaInsertRecFail);

  If Result = DBIERR_NONE Then
    Begin
      AcqContentLock(fsclmWrite);
      If Result = DBIERR_NONE Then
        Result := bcTable.UndeleteRecord(bcDatabase.TransactionInfo,
          CursorID, aData, aLockType, NewRefNr);
      If (Result = DBIERR_NONE) Then
        Begin
          bcNewRecBuff := aData;
          aRefNr := NewRefNr;
          SavKey := Nil;
          Try
            If (Result = DBIERR_NONE) Then
              Begin
                { If a range is active then save the current key & cursor information.
                  We may need to reposition the cursor to its original position if
                  the inserted record does not fit into the range. }
                If bcHasRange Then
                  Begin
                    FFGetMem(SavKey, scKeyLen);
                    Move(bcCurKey^, SavKey^, scKeyLen);
                    SavInfo := bcInfo;
                  End;

                FFInitKeyPath(bcInfo.KeyPath);
                bcInfo.pos := cpOnRecord;
                bcInfo.refNr := NewRefNr;
                bcInfo.Deleted := False;
                scRebuildCurKey(aData, True);
                If bcHasRange And (IsInRange(bcCurKey) <> 0) Then
                  Begin
                    bcInfo := SavInfo;
                    Move(SavKey^, bcCurKey^, scKeyLen);
                  End;
                bcTable.RelaxRecordLock(bcDatabase.TransactionInfo, CursorID,
                  bcInfo.RefNr);

                { Notify extenders of successful insert. }
                NotifyExtenders(ffeaAfterRecInsert, ffeaNoAction);
              End
            Else
              Begin
                NotifyExtenders(ffeaInsertRecFail, ffeaNoAction);
              End;
          Finally
            If SavKey <> Nil Then {!!.12}
              FFFreeMem(SavKey, scKeyLen); {!!.12}
            bcNewRecBuff := Nil;
          End;
        End
      Else
        Begin
          aRefNr.iLow := 0;
          aRefNr.IHigh := 0;
        End;
    End;
End;

Function TfsSrcCursor.InsertRecord(aData: PffByteArray;
  aLockType: TfsSrcLockType; aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Var
  NewRefNr: TffInt64;
  SavInfo: TfsSrcCursorInfo; {!!.12}
  SavKey: PffByteArray; {!!.12}

  Cursor, tq: TfsSrBaseCursor;
  fi, iField: Integer;
  atCursorID: TffCursorID;
  sCo, sq, se: String;
  tblName, funName, SfldName, DfldName, DfldValue: String;

  BufnewStr: TffShStr;
  FieldBuffer: PffByteArray;
  oldnul, newNul: boolean;
  st: TffSqlStmtID;
  Str: TMemoryStream;
  Tr: TffResult;
  e: Extended;
  i64: Int64;
  aiRefNr: TffInt64;

  Function NoAction: TffResult;
  Var
    iRes: Integer;
    FRecordsRead: Integer;
  Begin
    Cursor := Self;
    Result := DBIERR_NONE;
    If Cursor.Table.Dictionary.NoActionUpdate.Count > 0 Then
      Begin
        Str := TMemoryStream.Create;
        st := 0;
        Result := Cursor.Engine.SQLAlloc(Cursor.Client.ClientID, Cursor.DataBase.DatabaseID, Cursor.client.Timeout, st);
        Try
          If Result = DBIERR_NONE Then
            Begin
              atCursorID := Cursor.CursorID;
              For iRes := 0 To Cursor.Table.Dictionary.NoActionUpdate.Count - 1 Do
                Begin
                  sCo := Cursor.Table.Dictionary.NoActionUpdate[iRes];
                  MakeFunc(sCo, funName, tblName, SfldName, DfldName, DfldValue);
                  fi := Cursor.Table.Dictionary.FieldIndex(SfldName);
                  If (fi > -1) Then
                    Begin
                      FFGetMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                      Try
                        // new data
                        Cursor.Table.Dictionary.GetRecordField(fi, aData, newNul, FieldBuffer);
                        // buffer to str
                        BufnewStr := '';
                        BufnewStr := fsCurGetValueSql(Cursor.CursorID, fi, aData, FieldBuffer, False, newNul);
                        // sql
                        If newNul Then
                          sq := 'select ' + DfldName + ' from ' + tblName + ' where ' + DfldName + ' IS ' + BufnewStr
                        Else
                          sq := 'select ' + DfldName + ' from ' + tblName + ' where ' + DfldName + ' = ' + BufnewStr;
                      Finally
                        FFFreeMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                      End;
                      Result := Cursor.Engine.SQLPrepare(st, pointer(sq), Str);
                      If Result = DBIERR_NONE Then
                        Begin
                          Str.Clear;
                          Str.Position := 0;
                          Try
                            Result := Cursor.Engine.SQLExec(st, omReadOnly, atCursorID, Str);
                            If Result = DBIERR_NONE Then
                              Begin
                                FRecordsRead := 0;
                                TfsSrBaseCursor(atCursorID).SetToBegin;
                                If TfsSrBaseCursor(atCursorID).GetNextRecord(Nil, ffsltNone, aFlag, airefNr) = 0 Then
                                  FRecordsRead := 1;
                                tq := TfsSrBaseCursor(atCursorID);
                                If tq <> Nil Then
                                  Begin
                                    tq.CloseTable := True;
                                    Cursor.Engine.CursorClose(atCursorID);
                                  End;
                                //TfsSrBaseCursor(atCursorID).GetRecordCount(FRecordsRead);
                                If FRecordsRead = 0 Then
                                  Begin
                                    Result := 50001;
                                    System.Break;
                                  End;
                              End
                            Else
                              Begin
                                System.Break;
                              End;
                          Finally
                            tq := TfsSrBaseCursor(atCursorID);
                            If tq <> Nil Then
                              Begin
                                tq.CloseTable := True;
                                Cursor.Engine.CursorClose(atCursorID);
                              End;
                          End;
                        End
                      Else
                        Begin
                          System.Break;
                        End;
                    End;
                End; // for
            End;
        Finally
          Str.free;
          Cursor.Engine.sqlfree(st);
        End;
      End;
  End;

Begin
  aRefNr.iLow := 0;
  aRefNr.iHigh := 0;
  { Notify extenders. }
  bcNewRecBuff := aData;
  SavKey := Nil; {!!.12}
  Try
    Result := NotifyExtenders(ffeaBeforeRecInsert, ffeaInsertRecFail);
    If Result = DBIERR_NONE Then
      If Engine.Configuration.GeneralInfo^.giEnabledReferential Then
        Result := NOACTION;
    If Result = DBIERR_NONE Then
      Begin
        //If Self.Database.RecLocking = tlPessimistic Then
        AcqContentLock(fsclmWrite);
        Try
          If Engine.Configuration.GeneralInfo^.giEnabledTrigers Then
            If Table.Dictionary.TrigersBeforeInsertOperation.Count > 0 Then
              Begin
                Result := ExecScript(Table.Dictionary.TrigersBeforeInsertOperation, aData, aData,
                  bcDatabase.TransactionInfo.tirTrans,
                  bcDatabase.TransactionInfo.tirTrans);
                If (Result <> DBIERR_NONE) Then
                  If (Result <> 50015) Then
                    Result := 50003;
              End;
        Except
          Result := 50003;
        End;
        // before insert
        Try
          If (Result = DBIERR_NONE) Then
            If Engine.Configuration.GeneralInfo^.giEnabledTrigers Then
              If Table.Dictionary.TrigersBeforeInsert.Count > 0 Then
                Begin
                  Result := ExecScript(Table.Dictionary.TrigersBeforeInsert, aData, aData,
                    bcDatabase.TransactionInfo.tirTrans,
                    bcDatabase.TransactionInfo.tirTrans);
                  If (Result <> DBIERR_NONE) Then
                    If (Result <> 50015) Then
                      Result := 50003;
                End;
        Except
          Result := 50003;
        End;

        If (Result = DBIERR_NONE) Then
          Begin
            // test all fields for emptyAsNull
            If Self.Dictionary.IsAnyEmptyAsNull Or Self.Dictionary.IsAnyRound Or Self.Dictionary.IsRecVersion Then
              For iField := 0 To Self.Dictionary.FieldCount - 1 Do
                Begin
                  //fftSingle..fftCurrency
                  Try
                    Case Self.Dictionary.FieldDescriptor^[iField]^.fdType Of
                      fstShortString, fstSingleWideChar, fstSingleChar, fstNullString, fstVarNullString, fstWideString, fstVarWideString:
                        If Self.Dictionary.FieldDescriptor^[iField]^.fdEmptyAsNull Then
                          Begin
                            FFGetMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                            Try
                              Self.Dictionary.GetRecordField(iField, aData, oldNul, FieldBuffer);
                              If Not oldNul Then
                                Begin
                                  se := fsCurGetValue(Self.CursorID, iField, aData, FieldBuffer, False);
                                  If se = '' Then
                                    Self.Dictionary.SetRecordFieldNull(iField, aData, True);
                                End;
                            Finally
                              FFFreeMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                            End;
                          End;

                      fstSingle..fstCurrency:
                        If Self.Dictionary.FieldDescriptor^[iField]^.fdRound <> rNone Then
                          Begin
                            FFGetMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                            Try
                              Self.Dictionary.GetRecordField(iField, aData, oldNul, FieldBuffer);
                              If Not oldNul Then
                                Begin
                                  E := fsCurGetValueE(Self.CursorID, iField, aData, FieldBuffer);
                                  If E > 0 Then
                                    Begin
                                      E := RoundExtended(E, Self.Dictionary.FieldDescriptor^[iField]^.fdDecPl,
                                        Self.Dictionary.FieldDescriptor^[iField]^.fdRound);
                                      fsCurSetValueE(E, Self.CursorID, iField, aData, FieldBuffer);
                                    End;
                                End;
                            Finally
                              FFFreeMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                            End;
                          End;
                      fstRecVersion:
                        Begin
                          FFGetMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                          Try
                            Self.Dictionary.GetRecordField(iField, aData, oldNul, FieldBuffer);
                            If Not oldNul Then
                              Begin
                                i64 := pint64(FieldBuffer)^;
                                pint64(FieldBuffer)^ := i64; // + Self.Dictionary.FieldDescriptor^[iField]^.fdDecPl;
                                Self.Dictionary.SetRecordField(iField, aData, FieldBuffer);
                              End
                            Else
                              Begin
                                pint64(FieldBuffer)^ := Self.Dictionary.FieldDescriptor^[iField]^.fdUnits;
                                Self.Dictionary.SetRecordField(iField, aData, FieldBuffer);
                              End;
                          Finally
                            FFFreeMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                          End;
                        End;
                    End;
                  Except
                    Result := ERRCODE_WRITEERR;
                    Raise;
                  End;
                End;

            If (Result = DBIERR_NONE) Then
              Result := bcTable.InsertRecord(bcDatabase.TransactionInfo,
                CursorID, aData, aLockType, NewRefNr, AFlag);
          End;

        Try
          If Engine.Configuration.GeneralInfo^.giEnabledTrigers Then
            If (Result = DBIERR_NONE) Then
              If Table.Dictionary.TrigersAfterInsert.Count > 0 Then
                Begin
                  Result := ExecScript(Table.Dictionary.TrigersAfterInsert, aData, aData,
                    bcDatabase.TransactionInfo.tirTrans,
                    bcDatabase.TransactionInfo.tirTrans);
                  If (Result <> DBIERR_NONE) Then
                    If (Result <> 50015) Then Result := 50004;
                End;
        Except
          Result := 50004;
        End;
        Tr := Result;

        Try
          If Engine.Configuration.GeneralInfo^.giEnabledTrigers Then
            If Table.Dictionary.TrigersAfterInsertOperation.Count > 0 Then
              Begin
                Result := ExecScript(Table.Dictionary.TrigersAfterInsertOperation, aData, aData,
                  bcDatabase.TransactionInfo.tirTrans,
                  bcDatabase.TransactionInfo.tirTrans);
                If (Result <> DBIERR_NONE) Or (Tr <> DBIERR_NONE) Then
                  If (Result <> 50015) Then Result := 50004;
              End;
        Except
          Result := 50004;
        End;

        If (Result = DBIERR_NONE) Then
          Begin
            {Begin !!.12}
                    { If a range is active then save the current key & cursor information.
                      We may need to reposition the cursor to its original position if
                      the inserted record does not fit into the range. }
            If bcHasRange Then
              Begin
                FFGetMem(SavKey, scKeyLen);
                Move(bcCurKey^, SavKey^, scKeyLen);
                SavInfo := bcInfo;
              End;

            FFInitKeyPath(bcInfo.KeyPath);
            bcInfo.pos := cpOnRecord;
            bcInfo.refNr := NewRefNr;
            bcInfo.Deleted := False;
            scRebuildCurKey(aData, True);
            If bcHasRange And (IsInRange(bcCurKey) <> 0) Then
              Begin
                bcInfo := SavInfo;
                Move(SavKey^, bcCurKey^, scKeyLen);
              End;
            bcTable.RelaxRecordLock(bcDatabase.TransactionInfo, CursorID,
              bcInfo.RefNr);
            aRefnr := bcInfo.refNr;
            { Notify extenders of successful insert. }
            NotifyExtenders(ffeaAfterRecInsert, ffeaNoAction);
          End
        Else
          Begin
            aRefnr.iLow := 0;
            aRefnr.iHigh := 0;
            NotifyExtenders(ffeaInsertRecFail, ffeaNoAction);
          End;
      End;
  Finally
    If SavKey <> Nil Then {!!.12}
      FFFreeMem(SavKey, scKeyLen); {!!.12}
    bcNewRecBuff := Nil;
  End;
End;
{--------}

Function TfsSrcCursor.InsertRecordNoDefault(aData: PffByteArray; {!!.10}
  aLockType: TfsSrcLockType; aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Var
  NewRefNr: TffInt64;
  Tr: TffResult;

  Cursor, tq: TfsSrBaseCursor;
  fi, iField: Integer;
  atCursorID: TffCursorID;
  sCo, sq, se: String;
  tblName, funName, SfldName, DfldName, DfldValue: String;

  BufnewStr: TffShStr;
  FieldBuffer: PffByteArray;
  oldnul, newNul: boolean;
  st: TffSqlStmtID;
  Str: TMemoryStream;
  e: Extended;
  i64: Int64;
  aiRefNr: TffInt64;

  Function NoAction: TffResult;
  Var
    iRes: Integer;
    FRecordsRead: Integer;
  Begin
    Cursor := Self;
    Result := DBIERR_NONE;
    If Cursor.Table.Dictionary.NoActionUpdate.Count > 0 Then
      Begin
        Str := TMemoryStream.Create;
        st := 0;
        Result := Cursor.Engine.SQLAlloc(Cursor.Client.ClientID, Cursor.DataBase.DatabaseID, Cursor.client.Timeout, st);
        Try
          If Result = DBIERR_NONE Then
            Begin
              atCursorID := Cursor.CursorID;
              For iRes := 0 To Cursor.Table.Dictionary.NoActionUpdate.Count - 1 Do
                Begin
                  sCo := Cursor.Table.Dictionary.NoActionUpdate[iRes];
                  MakeFunc(sCo, funName, tblName, SfldName, DfldName, DfldValue);
                  fi := Cursor.Table.Dictionary.FieldIndex(SfldName);
                  If (fi > -1) Then
                    Begin
                      FFGetMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                      Try
                        // new data
                        Cursor.Table.Dictionary.GetRecordField(fi, aData, newNul, FieldBuffer);
                        // buffer to str
                        BufnewStr := '';
                        BufnewStr := fsCurGetValueSql(Cursor.CursorID, fi, aData, FieldBuffer, False, newNul);
                        // sql
                        If newNul Then
                          sq := 'select ' + DfldName + ' from ' + tblName + ' where ' + DfldName + ' IS ' + BufnewStr
                        Else
                          sq := 'select ' + DfldName + ' from ' + tblName + ' where ' + DfldName + ' = ' + BufnewStr;
                      Finally
                        FFFreeMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                      End;
                      Result := Cursor.Engine.SQLPrepare(st, pointer(sq), Str);
                      If Result = DBIERR_NONE Then
                        Begin
                          Str.Clear;
                          Str.Position := 0;
                          Try
                            Result := Cursor.Engine.SQLExec(st, omReadOnly, atCursorID, Str);
                            If Result = DBIERR_NONE Then
                              Begin
                                FRecordsRead := 0;
                                TfsSrBaseCursor(atCursorID).SetToBegin;
                                If TfsSrBaseCursor(atCursorID).GetNextRecord(Nil, ffsltNone, aFlag, airefNr) = 0 Then
                                  FRecordsRead := 1;
                                tq := TfsSrBaseCursor(atCursorID);
                                If tq <> Nil Then
                                  Begin
                                    tq.CloseTable := True;
                                    Cursor.Engine.CursorClose(atCursorID);
                                  End;
                                //TfsSrBaseCursor(atCursorID).GetRecordCount(FRecordsRead);
                                If FRecordsRead = 0 Then
                                  Begin
                                    Result := 50001;
                                    System.Break;
                                  End;
                              End
                            Else
                              Begin
                                System.Break;
                              End;
                          Finally
                            tq := TfsSrBaseCursor(atCursorID);
                            If tq <> Nil Then
                              Begin
                                tq.CloseTable := True;
                                Cursor.Engine.CursorClose(atCursorID);
                              End;
                          End;
                        End
                      Else
                        Begin
                          System.Break;
                        End;
                    End;
                End; // for
            End;
        Finally
          Str.free;
          Cursor.Engine.sqlfree(st);
        End;
      End;
  End;

Begin
  aRefNr.iLow := 0;
  aRefNr.iHigh := 0;
  { Notify extenders. }
  bcNewRecBuff := aData;
  Try
    Result := NotifyExtenders(ffeaBeforeRecInsert, ffeaInsertRecFail);
    If Result = DBIERR_NONE Then
      If Engine.Configuration.GeneralInfo^.giEnabledReferential Then
        Result := NOACTION;
    If Result = DBIERR_NONE Then
      Begin
        //If Self.Database.RecLocking = tlPessimistic Then
        AcqContentLock(fsclmWrite);
        Try
          If Engine.Configuration.GeneralInfo^.giEnabledTrigers Then
            If Table.Dictionary.TrigersBeforeInsertOperation.Count > 0 Then
              Begin
                Result := ExecScript(Table.Dictionary.TrigersBeforeInsertOperation, aData, aData,
                  bcDatabase.TransactionInfo.tirTrans,
                  bcDatabase.TransactionInfo.tirTrans);
                If (Result <> DBIERR_NONE) Then
                  If (Result <> 50015) Then Result := 50003;
              End;
        Except
          Result := 50003;
        End;
        // before insert
        Try
          If (Result = DBIERR_NONE) Then
            If Engine.Configuration.GeneralInfo^.giEnabledTrigers Then
              If Table.Dictionary.TrigersBeforeInsert.Count > 0 Then
                Begin
                  Result := ExecScript(Table.Dictionary.TrigersBeforeInsert, aData, aData,
                    bcDatabase.TransactionInfo.tirTrans,
                    bcDatabase.TransactionInfo.tirTrans);
                  If (Result <> DBIERR_NONE) Then
                    If (Result <> 50015) Then Result := 50003;
                End;
        Except
          Result := 50003;
        End;

        If (Result = DBIERR_NONE) Then
          Begin
            // test all fields for emptyAsNull
            If Self.Dictionary.IsAnyEmptyAsNull Or Self.Dictionary.IsAnyRound Or
              Self.Dictionary.IsAnyDefault Or Self.Dictionary.IsRecVersion Then
              For iField := 0 To Self.Dictionary.FieldCount - 1 Do
                Begin
                  //fstSingle..fstCurrency
                  Try
                    Case Self.Dictionary.FieldDescriptor^[iField]^.fdType Of
                      fstShortString, fstSingleWideChar, fstSingleChar, fstNullString, fstVarNullString, fstWideString, fstVarWideString:
                        If Self.Dictionary.FieldDescriptor^[iField]^.fdEmptyAsNull Then
                          Begin
                            FFGetMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                            Try
                              Self.Dictionary.GetRecordField(iField, aData, oldNul, FieldBuffer);
                              If Not oldNul Then
                                Begin
                                  se := fsCurGetValue(Self.CursorID, iField, aData, FieldBuffer, False);
                                  If se = '' Then
                                    Self.Dictionary.SetRecordFieldNull(iField, aData, True);
                                End;
                            Finally
                              FFFreeMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                            End;
                          End;

                      fstSingle..fstCurrency:
                        If Self.Dictionary.FieldDescriptor^[iField]^.fdRound <> rNone Then
                          Begin
                            FFGetMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                            Try
                              Self.Dictionary.GetRecordField(iField, aData, oldNul, FieldBuffer);
                              If Not oldNul Then
                                Begin
                                  E := fsCurGetValueE(Self.CursorID, iField, aData, FieldBuffer);
                                  If E > 0 Then
                                    Begin
                                      E := RoundExtended(E, Self.Dictionary.FieldDescriptor^[iField]^.fdDecPl,
                                        Self.Dictionary.FieldDescriptor^[iField]^.fdRound);
                                      fsCurSetValueE(E, Self.CursorID, iField, aData, FieldBuffer);
                                    End;
                                End;
                            Finally
                              FFFreeMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                            End;
                          End;
                      fstRecVersion:
                        Begin
                          FFGetMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                          Try
                            Self.Dictionary.GetRecordField(iField, aData, oldNul, FieldBuffer);
                            If Not oldNul Then
                              Begin
                                i64 := pint64(FieldBuffer)^;
                                pint64(FieldBuffer)^ := i64; // + Self.Dictionary.FieldDescriptor^[iField]^.fdDecPl;
                                Self.Dictionary.SetRecordField(iField, aData, FieldBuffer);
                              End
                            Else
                              Begin
                                pint64(FieldBuffer)^ := Self.Dictionary.FieldDescriptor^[iField]^.fdUnits;
                                Self.Dictionary.SetRecordField(iField, aData, FieldBuffer);
                              End;
                          Finally
                            FFFreeMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                          End;
                        End;
                    End;
                    If Dictionary.DefaultFieldCount > 0 Then
                      Begin
                        FFGetMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                        Try
                          Self.Dictionary.GetRecordField(iField, aData, oldNul, FieldBuffer);
                          If Self.Dictionary.FieldDescriptor^[iField]^.fdDefaultUpdate = duIFNULL Then
                            Begin
                              If oldNul And (Self.Dictionary.FieldDescriptor^[iField]^.fdDefaultUpdate = duIFNULL) Then
                                Self.Dictionary.SetDefaultFieldUpdateValue(aData, iField);
                            End
                          Else
                            Begin
                              If Self.Dictionary.FieldDescriptor^[iField]^.fdDefaultUpdate = duALWAYS Then
                                Self.Dictionary.SetDefaultFieldUpdateValue(aData, iField);
                            End;
                        Finally
                          FFFreeMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                        End;
                      End;
                  Except
                    Result := ERRCODE_WRITEERR;
                    Raise;
                  End;
                End;

            If (Result = DBIERR_NONE) Then
              Result := bcTable.InsertRecordNoDefault(bcDatabase.TransactionInfo, {!!.10}
                CursorID, aData, aLockType, NewRefNr, AFlag);

          End;

        Try
          If Engine.Configuration.GeneralInfo^.giEnabledTrigers Then
            If (Result = DBIERR_NONE) Then
              If Table.Dictionary.TrigersAfterInsert.Count > 0 Then
                Begin
                  Result := ExecScript(Table.Dictionary.TrigersAfterInsert, aData, aData,
                    bcDatabase.TransactionInfo.tirTrans,
                    bcDatabase.TransactionInfo.tirTrans);
                  If (Result <> DBIERR_NONE) Then
                    If (Result <> 50015) Then Result := 50004;
                End;
        Except
          Result := 50004;
        End;
        Tr := Result;

        Try
          If Engine.Configuration.GeneralInfo^.giEnabledTrigers Then
            If Table.Dictionary.TrigersAfterInsertOperation.Count > 0 Then
              Begin
                Result := ExecScript(Table.Dictionary.TrigersAfterInsertOperation, aData, aData,
                  bcDatabase.TransactionInfo.tirTrans,
                  bcDatabase.TransactionInfo.tirTrans);
                If (Result <> DBIERR_NONE) Or (Tr <> DBIERR_NONE) Then
                  If (Result <> 50015) Then Result := 50004;
              End;
        Except
          Result := 50004;
        End;

        If (Result = DBIERR_NONE) Then
          Begin
            FFInitKeyPath(bcInfo.KeyPath);
            bcInfo.pos := cpOnRecord;
            bcInfo.refNr := NewRefNr;
            bcInfo.Deleted := False;
            scRebuildCurKey(aData, True);
            //If Self.Database.RecLocking = tlPessimistic Then
            bcTable.RelaxRecordLock(bcDatabase.TransactionInfo, CursorID, {!!.10}
              bcInfo.RefNr); {!!.10}
            aRefnr := bcInfo.refNr;
            { Notify extenders of successful insert. }
            NotifyExtenders(ffeaAfterRecInsert, ffeaNoAction);
          End
        Else
          Begin
            aRefnr.iLow := 0;
            aRefnr.iHigh := 0;
            { Notify extenders of failed insert. }
            NotifyExtenders(ffeaInsertRecFail, ffeaNoAction);
          End;
      End;
  Finally
    bcNewRecBuff := Nil;
  End;
End;
{--------}

Function TfsSrcCursor.IsInRange(aKey: PffByteArray): Integer;
Var
  KeyCompareResult: Integer;
Begin
  Result := 0;
  If Not bcHasRange Then
    Exit;
  If bcRng1Valid Then
    Begin
      With bcCompareData Do
        Begin
          cdFldCnt := bcRng1FldCnt;
          cdPartLen := bcRng1PtlLen;
        End;
      KeyCompareResult := Table.CompareKeysForCursor(bcKID, aKey, bcRng1Key);
      If (KeyCompareResult < 0) Then
        Begin
          Result := -1;
          Exit;
        End;
      If (KeyCompareResult = 0) Then
        Begin
          If Not bcRng1Incl Then
            Result := -1;
          Exit;
        End;
    End;
  If bcRng2Valid Then
    Begin
      With bcCompareData Do
        Begin
          cdFldCnt := bcRng2FldCnt;
          cdPartLen := bcRng2PtlLen;
        End;
      KeyCompareResult := Table.CompareKeysForCursor(bcKID, aKey, bcRng2Key);
      If (KeyCompareResult > 0) Then
        Begin
          Result := 1;
          Exit;
        End;
      If (KeyCompareResult = 0) Then
        Begin
          If Not bcRng2Incl Then
            Result := 1;
          Exit;
        End;
    End;
End;

Function TfsSrBaseCursor.DeleteRecord(aData: PffByteArray): TffResult;
Var
  aFlag: Byte;
  BTreeChanged: Boolean; {!!.05}
  LockedRefNr: TffInt64; {!!.05}
  Cursor, tq: TfsSrBaseCursor;
  OldData: PffByteArray;
  RecLen, fi, iCon: Integer;
  atCursorID, SelfCursorID: TffCursorID;
  sCo, sq: String;
  tblName, funName, SfldName, DfldName, DfldValue: String;

  BufStr: TffShStr;
  FieldBuffer: PffByteArray;
  IsNull: boolean;
  st: TffSqlStmtID;
  Tr: TffResult;
  aRefNr: TffInt64;

  Function RestrictDelete: TffResult;
  Var
    iRes: Integer;
    FRecordsRead: Integer;
  Begin
    Result := DBIERR_NONE;
    SelfCursorID := Cursor.CursorID;
    If Cursor.Table.Dictionary.RestrictDelete.Count > 0 Then
      Begin
        Try
          st := 0;
          atCursorID := 0;
          Result := Cursor.Engine.SQLAlloc(Cursor.Client.ClientID, Cursor.DataBase.DatabaseID, Cursor.client.Timeout, st);
          If Result = DBIERR_NONE Then
            Begin
              For iRes := 0 To Cursor.Table.Dictionary.RestrictDelete.Count - 1 Do
                Begin
                  sCo := Cursor.Table.Dictionary.RestrictDelete[iRes];
                  MakeFunc(sCo, funName, tblName, SfldName, DfldName, DfldValue);
                  fi := Cursor.Table.Dictionary.FieldIndex(SfldName);
                  If (fi > -1) Then
                    Begin
                      FFGetMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                      Try
                        // move data to buffer
                        Cursor.Table.Dictionary.GetRecordField(fi, oldData, IsNull, FieldBuffer);
                        // buffer to str
                        BufStr := fsCurGetValueSql(Cursor.CursorID, fi, oldData, FieldBuffer, False, IsNull);
                        // sql
                        If IsNull Then
                          sq := 'select ' + DfldName + ' from ' + tblName + ' where ' + DfldName + ' IS ' + BufStr
                        Else
                          sq := 'select ' + DfldName + ' from ' + tblName + ' where ' + DfldName + ' = ' + BufStr;
                      Finally
                        FFFreeMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                      End;

                      Result := Cursor.Engine.SQLPrepare(st, pointer(sq), Nil);
                      If Result = DBIERR_NONE Then
                        Begin
                          atCursorID := 0;
                          Result := Cursor.Engine.SQLExec(st, omReadOnly, atCursorID, Nil);
                          If Result = DBIERR_NONE Then
                            Begin
                              FRecordsRead := 0;
                              TfsSrBaseCursor(atCursorID).SetToBegin;
                              If TfsSrBaseCursor(atCursorID).GetNextRecord(Nil, ffsltNone, aflag, arefnr) = 0 Then
                                FRecordsRead := 1;
                              tq := TfsSrBaseCursor(atCursorID);
                              If tq <> Nil Then
                                Begin
                                  tq.CloseTable := True;
                                  Cursor.Engine.CursorClose(atCursorID);
                                  Cursor.Engine.sqlfree(st);
                                End;
                              //TfsSrBaseCursor(atCursorID).GetRecordCount(FRecordsRead);
                              If FRecordsRead > 0 Then
                                Begin
                                  Result := 50000;
                                  System.Break;
                                End;
                            End
                          Else
                            Begin
                              tq := TfsSrBaseCursor(atCursorID);
                              If tq <> Nil Then
                                Begin
                                  tq.CloseTable := True;
                                  Cursor.Engine.CursorClose(atCursorID);
                                  Cursor.Engine.sqlfree(st);
                                End;
                              System.Break;
                            End;
                        End
                      Else
                        Begin
                          tq := TfsSrBaseCursor(atCursorID);
                          If tq <> Nil Then
                            Begin
                              tq.CloseTable := True;
                              Cursor.Engine.CursorClose(atCursorID);
                              Cursor.Engine.sqlfree(st);
                            End;
                          System.Break;
                        End;
                    End;
                End;
            End;
        Finally
          tq := TfsSrBaseCursor(atCursorID);
          If tq <> Nil Then
            Begin
              tq.CloseTable := True;
              Cursor.Engine.CursorClose(atCursorID);
            End;
          Cursor.Engine.sqlfree(st);
        End;
      End;
  End;

Begin
  aflag := 0;
  Result := DBIERR_NONE; {!!.01}
  Cursor := Self;
  { Are we on a record? }
  If (bcInfo.Pos <> cpOnRecord) Then
    Begin
      { No. }
      If bcInfo.Deleted Then
        Result := DBIERR_KEYORRECDELETED
      Else
        Result := DBIERR_NOCURRREC;
      Exit;
    End;

  { Note: By this time, any other cursor deleting the record ahead of us has
    completed and has set bcInfo.Deleted.  We can be assured of this because
    TFSServer.RecordDelete calls Cursor.EnsureWritable(true) which
    obtains a lock on the record to be deleted.  We won't get that lock until
    the other cursor has finished. }

  Try {!!.01}
    { Has this record already been deleted? }
    If bcInfo.Deleted Then
      Begin
        { Yes. }
        Result := DBIERR_KEYORRECDELETED;
        Exit;
      End;
    RecLen := Dictionary.RecordLength;
    FFGetMem(OldData, RecLen);
    Try
      AcqContentLock(fsclmWrite);
      If (aData = Nil) And {!!.02}
      ((bcFilter <> Nil) Or (bcExtenders <> Nil)) Then {!!.02}
        aData := bcRecordData;
      If (aData <> Nil) Then
        Begin
          Table.GetRecord(bcDatabase.TransactionInfo, bcDatabase.DatabaseID,
            CursorID, bcInfo.refNr, aData, ffsltExclusive, tluDatabase, True, False, aflag);
          If Assigned(bcFilter) Then
            If Not bcFilter.MatchesRecord(aData) Then
              Begin
                { Release the record lock. }
                If bcInfo.Deleted Then
                  Result := DBIERR_KEYORRECDELETED
                Else
                  Result := DBIERR_NOCURRREC;
                Exit;
              End;
        End;

      { Notify extenders. }
      bcOldRecBuff := aData;
      Try
        Result := NotifyExtenders(ffeaBeforeRecDelete, ffeaDeleteRecFail);
        { If the extenders object, we can't continue. }
        If Result = DBIERR_NONE Then
          Begin
            BTreeChanged := False; {!!.05 - Start}
            LockedRefNr := bcInfo.refNr; {!!.05}
            Table.GetRecord(bcDatabase.TransactionInfo, bcDatabase.DatabaseID, {!!.10}
              CursorID, bcInfo.refNr, OldData, ffsltNone, tluDatabase, True, False, aflag);

            Try
              If Engine.Configuration.GeneralInfo^.giEnabledTrigers Then
                If Table.Dictionary.TrigersBeforeDeleteOperation.Count > 0 Then
                  Begin
                    Result := ExecScript(Table.Dictionary.TrigersBeforeDeleteOperation, aData, aData,
                      Database.TransactionInfo.tirTrans,
                      bcDatabase.TransactionInfo.tirTrans);
                    If (Result <> DBIERR_NONE) Then
                      If (Result <> 50013) Then Result := 50007;
                  End;
            Except
              Result := 50007;
            End;

            If Result = DBIERR_NONE Then
              If Engine.Configuration.GeneralInfo^.giEnabledReferential Then
                Result := RestrictDelete;

            If Result = DBIERR_NONE Then
              Begin
                If Engine.Configuration.GeneralInfo^.giEnabledReferential And
                  (Cursor.Table.Dictionary.CascadeDelete.Count > 0) Then
                  Begin
                    // add to References on cascade delete k winnicki 31-10-2004 20:00
                    sCo := '';
                    Try
                      st := 0;
                      Result := Cursor.Engine.SQLAlloc(Cursor.Client.ClientID, Cursor.DataBase.DatabaseID, Cursor.client.Timeout, st);

                      If Result = DBIERR_NONE Then
                        Begin
                          Try
                            atCursorID := 0;
                            For iCon := 0 To Cursor.Table.Dictionary.CascadeDelete.Count - 1 Do
                              Begin
                                sCo := Cursor.Table.Dictionary.CascadeDelete[iCon];
                                MakeFunc(sCo, funName, tblName, SfldName, DfldName, DfldValue);
                                fi := Cursor.Table.Dictionary.FieldIndex(SfldName);
                                If (fi > -1) Then
                                  Begin
                                    FFGetMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                                    Try
                                      // move data to buffer
                                      Cursor.Table.Dictionary.GetRecordField(fi, oldData, IsNull, FieldBuffer);
                                      // buffer to str
                                      BufStr := fsCurGetValueSql(Cursor.CursorID, fi, oldData, FieldBuffer, False, IsNull);
                                      // sql
                                      If IsNull Then
                                        sq := 'delete  from ' + tblName + ' where ' + DfldName + ' IS ' + BufStr
                                      Else
                                        sq := 'delete  from ' + tblName + ' where ' + DfldName + ' = ' + BufStr;
                                    Finally
                                      FFFreeMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                                    End;

                                    Result := Cursor.Engine.SQLPrepare(st, pointer(sq), Nil);
                                    If Result = DBIERR_NONE Then
                                      Begin
                                        Result := Cursor.Engine.SQLExec(st, omReadWrite, atCursorID, Nil);
                                        If Result <> DBIERR_NONE Then
                                          Begin
                                            Cursor.Engine.sqlfree(st);
                                            System.Break;
                                          End;
                                      End
                                    Else
                                      Begin
                                        Cursor.Engine.sqlfree(st);
                                        System.Break;
                                      End;
                                  End;
                              End;
                          Finally
                            Cursor.Engine.sqlfree(st);
                          End;
                        End;
                    Finally
                      tq := TfsSrBaseCursor(atCursorID);
                      If tq <> Nil Then
                        Begin
                          tq.CloseTable := True;
                          Cursor.Engine.CursorClose(atCursorID);
                        End;
                      Cursor.Engine.sqlfree(st);
                    End;
                  End;

                If (Result = DBIERR_NONE) Then
                  If Engine.Configuration.GeneralInfo^.giEnabledReferential And
                    (Cursor.Table.Dictionary.CascadeDeleteNull.Count > 0) Then
                    Begin
                      // add to References on cascadedeletenull k winnicki 15-1-2006 20:00
                      sCo := '';
                      Try
                        st := 0;
                        Result := Cursor.Engine.SQLAlloc(Cursor.Client.ClientID, Cursor.DataBase.DatabaseID, Cursor.client.Timeout, st);

                        If Result = DBIERR_NONE Then
                          Begin
                            Try
                              atCursorID := 0;
                              For iCon := 0 To Cursor.Table.Dictionary.CascadeDeleteNull.Count - 1 Do
                                Begin
                                  sCo := Cursor.Table.Dictionary.CascadeDeleteNull[iCon];
                                  MakeFunc(sCo, funName, tblName, SfldName, DfldName, DfldValue);
                                  fi := Cursor.Table.Dictionary.FieldIndex(SfldName);
                                  If (fi > -1) Then
                                    Begin
                                      FFGetMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                                      Try
                                        // move data to buffer
                                        Cursor.Table.Dictionary.GetRecordField(fi, oldData, IsNull, FieldBuffer);
                                        // buffer to str
                                        BufStr := fsCurGetValueSql(Cursor.CursorID, fi, oldData, FieldBuffer, False, IsNull);
                                        // sql
                                        sq := 'Update ' + tblName + ' set ' + DfldName + ' = ' + 'NULL';
                                        If IsNull Then
                                          sq := sq + ' where ' + DfldName + ' IS ' + BufStr
                                        Else
                                          sq := sq + ' where ' + DfldName + ' = ' + BufStr;
                                      Finally
                                        FFFreeMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                                      End;

                                      Result := Cursor.Engine.SQLPrepare(st, pointer(sq), Nil);
                                      If Result = DBIERR_NONE Then
                                        Begin
                                          Result := Cursor.Engine.SQLExec(st, omReadWrite, atCursorID, Nil);
                                          If Result <> DBIERR_NONE Then
                                            Begin
                                              Cursor.Engine.sqlfree(st);
                                              System.Break;
                                            End;
                                        End
                                      Else
                                        Begin
                                          Cursor.Engine.sqlfree(st);
                                          System.Break;
                                        End;
                                    End;
                                End;
                            Finally
                              Cursor.Engine.sqlfree(st);
                            End;
                          End;
                      Finally
                        tq := TfsSrBaseCursor(atCursorID);
                        If tq <> Nil Then
                          Begin
                            tq.CloseTable := True;
                            Cursor.Engine.CursorClose(atCursorID);
                          End;
                        Cursor.Engine.sqlfree(st);
                      End;
                    End;

                Try
                  If (Result = DBIERR_NONE) Then
                    If Engine.Configuration.GeneralInfo^.giEnabledTrigers Then
                      If Table.Dictionary.TrigersBeforeDelete.Count > 0 Then
                        Begin
                          Result := ExecScript(Table.Dictionary.TrigersBeforeDelete, aData, aData,
                            Database.TransactionInfo.tirTrans,
                            Database.TransactionInfo.tirTrans);
                          If (Result <> DBIERR_NONE) Then
                            If (Result <> 50013) Then Result := 50007;
                        End;
                Except
                  Result := 50007;
                End;

                If (Result = DBIERR_NONE) Then
                  Result := Table.DeleteRecord(Database.TransactionInfo, CursorID,
                    bcInfo.refNr, True, BTreeChanged);

                Try
                  If Engine.Configuration.GeneralInfo^.giEnabledTrigers Then
                    If (Result = DBIERR_NONE) Then
                      If Table.Dictionary.TrigersAfterDelete.Count > 0 Then
                        Begin
                          Result := ExecScript(Table.Dictionary.TrigersAfterDelete, aData, aData,
                            Database.TransactionInfo.tirTrans,
                            Database.TransactionInfo.tirTrans);
                          If (Result <> DBIERR_NONE) Then
                            If (Result <> 50013) Then Result := 50008;
                        End;
                Except
                  Result := 50008;
                End;

              End;
            Tr := Result;
            // TRIGGER AFTEROPERATION
            Try
              If Engine.Configuration.GeneralInfo^.giEnabledTrigers Then
                If Table.Dictionary.TrigersAfterDeleteOperation.Count > 0 Then
                  Begin
                    Result := ExecScript(Table.Dictionary.TrigersAfterDeleteOperation, aData, aData,
                      Database.TransactionInfo.tirTrans,
                      Database.TransactionInfo.tirTrans);
                    If (Result <> DBIERR_NONE) Or (Tr <> DBIERR_NONE) Then
                      If (Result <> 50013) Then Result := 50008;
                  End;
            Except
              Result := 50008;
            End;

            If (Result = DBIERR_NONE) Or (Result = DBIERR_NOTSUFFFIELDRIGHTS) Then
              Begin
                bcTable.RelaxRecordLock(bcDatabase.TransactionInfo, CursorID,
                  bcInfo.RefNr);
                If Result = DBIERR_NONE Then
                  Begin
                    If bcInfo.KeyPath.kpPos = kppUnknown Then
                      bcInfo.Pos := cpUnknown
                    Else If (BTreeChanged) Then
                      Begin
                        bcRebuildKeyPath;
                      End
                    Else If (bcInfo.KeyPath.kpPos = kppOnKey) Then
                      Begin
                        bcInfo.KeyPath.kpPos := kppOnCrackBefore;
                        bcInfo.Deleted := True;
                        bcInfo.Pos := cpOnCrack;
                      End;
                  End;
                { Notify extenders of successful delete. }
                NotifyExtenders(ffeaAfterRecDelete, ffeaNoAction);
              End
            Else
              { Notify extenders of failed delete. }
              NotifyExtenders(ffeaDeleteRecFail, ffeaNoAction);
          End;
      Finally
        bcOldRecBuff := Nil;
      End;
      {Begin !!.01}
    Finally
      { Release the record lock if an error occurred or we are in an implicit
        transaction. }

      If (Result <> DBIERR_NONE) Or
        bcDatabase.Transaction.IsImplicit Then
        Begin
          Table.RelRecordLock(bcDatabase.TransactionInfo, {!!.10}
            bcDatabase.DatabaseID,
            CursorID, LockedRefNr);
          { Did an edit occur just prior to the delete? }
          If Not FFI64IsZero(bcLockedRefNum) Then
            Begin
              Table.RelRecordLock(bcDatabase.TransactionInfo, {!!.10}
                bcDatabase.DatabaseID, {!!.10}
                CursorID, bcLockedRefNum); {!!.10}
              FFInitI64(bcLockedRefNum);
            End;
        End;
    End;
  Finally
    FFfreeMem(OldData, RecLen);
  End;
End;

Function TfsSrcCursor.ModifyRecord(aData: PffByteArray; aRelLock: boolean; aUserLockType: TfsUserRecLocking; aFlag: Byte; aSet, Use: Boolean):
  TffResult;
Var
  aFlag1: Byte;
  aKeyChanged: Boolean; {!!.05}
  SavKey: PffByteArray; {!!.05}
  OldData: PffByteArray;
  Cursor: TfsSrBaseCursor;
  fi, iCon: Integer;
  atCursorID: TffCursorID;
  sCo, sq, se: String;
  tblName, funName, SfldName, DfldName, DfldValue: String;
  tq: TfsSrBaseCursor;
  iField: Integer;
  BufStr, BufnewStr: TffShStr;
  FieldBuffer: PffByteArray;
  oldNul, newnul: boolean;
  st: TffSqlStmtID;
  Str: TMemoryStream;
  Tr: TffResult;
  E: Extended;
  i64: Int64;
  aRefNr: TffInt64;

  Procedure CheckEmptyOrErrorBLOBs;
  Var
    FldInx: Integer;
    FldDesc: PffFieldDescriptor;
    BLOBNr: TffInt64;
    IsNull: boolean;
    aResult: TffResult;
  Begin
    With bctable.Dictionary Do
      Begin
        For FldInx := 0 To pred(FieldCount) Do
          Begin
            FldDesc := FieldDescriptor[FldInx];
            If (FldDesc^.fdType >= fstBLOB) And
              (FldDesc^.fdType <= ffcLastBLOBType) Then
              Begin
                GetRecordField(FldInx, aData, IsNull, @BLOBNr);
                If (Not IsNull) And (BLOBNr.iLow <> fsc_W32NoValue) Then
                  Begin
                    IsNull := IsDeletedBLOB(BLOBNr, aResult);
                    If IsNull Then
                      SetRecordFieldNull(FldInx, aData, True);
                  End
                Else
                  Begin // isnull new rec

                    GetRecordField(FldInx, oldData, IsNull, @BLOBNr);
                    If Not FFVerifyBLOBNr(BLOBNr,
                      bcTable.Files[bcTable.Dictionary.BLOBFileNumber].fiLog2BlockSize) Then
                      Begin
                        // refresh info blob
                        Table.GetRecord(bcDatabase.TransactionInfo, bcDatabase.DatabaseID,
                          CursorID, bcInfo.refNr, OldData, ffsltNone, tluDatabase, True, False, aflag1);
                        GetRecordField(FldInx, oldData, IsNull, @BLOBNr);
                      End;
                    // if not isnull blob (change other client)
                    If (Not IsNull) And (BLOBNr.iLow <> fsc_W32NoValue) Then
                      Begin
                        IsNull := IsDeletedBLOB(BLOBNr, aResult);
                        If IsNull Then
                          SetRecordFieldNull(FldInx, aData, True)
                        Else
                          Self.Dictionary.SetRecordField(FldInx, aData, @BLOBNr);
                      End
                  End;
              End;
          End;
      End;
  End;

  Function RestrictUpdate: TffResult;
  Var
    iRes: Integer;
    FRecordsRead: Integer;
  Begin
    Result := DBIERR_NONE;
    If Cursor.Table.Dictionary.RestrictUpdate.Count > 0 Then
      Begin
        Str := TMemoryStream.Create;
        st := 0;
        Result := Cursor.Engine.SQLAlloc(Cursor.Client.ClientID, Cursor.DataBase.DatabaseID, Cursor.client.Timeout, st);
        Try
          If Result = DBIERR_NONE Then
            Begin
              atCursorID := Cursor.CursorID;
              For iRes := 0 To Cursor.Table.Dictionary.RestrictUpdate.Count - 1 Do
                Begin
                  sCo := Cursor.Table.Dictionary.RestrictUpdate[iRes];
                  MakeFunc(sCo, funName, tblName, SfldName, DfldName, DfldValue);
                  fi := Cursor.Table.Dictionary.FieldIndex(SfldName);
                  If (fi > -1) Then
                    Begin
                      FFGetMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                      Try
                        // move data to buffer
                        Cursor.Table.Dictionary.GetRecordField(fi, oldData, oldNul, FieldBuffer);
                        // buffer to str
                        BufStr := '';
                        BufStr := fsCurGetValueSql(Cursor.CursorID, fi, oldData, FieldBuffer, False, oldNul);
                        // new data
                        Cursor.Table.Dictionary.GetRecordField(fi, aData, newNul, FieldBuffer);
                        // buffer to str
                        BufnewStr := '';
                        BufnewStr := fsCurGetValueSql(Cursor.CursorID, fi, aData, FieldBuffer, False, newNul);
                        // sql
                        sq := 'select ' + DfldName + ' from ' + tblName + ' where ' + DfldName + ' = ' + BufStr;
                      Finally
                        FFFreeMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                      End;
                      If Not oldNul Then
                        If (BufnewStr <> BufStr) Then
                          Begin
                            Result := Cursor.Engine.SQLPrepare(st, pointer(sq), Str);
                            If Result = DBIERR_NONE Then
                              Begin
                                Str.Clear;
                                Str.Position := 0;
                                Try
                                  Result := Cursor.Engine.SQLExec(st, omReadOnly, atCursorID, Str);
                                  If Result = DBIERR_NONE Then
                                    Begin
                                      FRecordsRead := 0;
                                      TfsSrBaseCursor(atCursorID).SetToBegin;
                                      If TfsSrBaseCursor(atCursorID).GetNextRecord(Nil, ffsltNone, aflag1, arefnr) = 0 Then
                                        FRecordsRead := 1;
                                      tq := TfsSrBaseCursor(atCursorID);
                                      If tq <> Nil Then
                                        Begin
                                          tq.CloseTable := True;
                                          Cursor.Engine.CursorClose(atCursorID);
                                        End;
                                      //TfsSrBaseCursor(atCursorID).GetRecordCount(FRecordsRead);
                                      If FRecordsRead > 0 Then
                                        Begin
                                          Result := 50001;
                                          System.Break;
                                        End;
                                    End
                                  Else
                                    Begin
                                      System.Break;
                                    End;
                                Finally
                                  tq := TfsSrBaseCursor(atCursorID);
                                  If tq <> Nil Then
                                    Begin
                                      tq.CloseTable := True;
                                      Cursor.Engine.CursorClose(atCursorID);
                                    End;
                                End;
                              End
                            Else
                              Begin
                                System.Break;
                              End;
                          End;
                    End;
                End; // for
            End;
        Finally
          Str.free;
          Cursor.Engine.sqlfree(st);
        End;
      End;
  End;

  Function NoAction: TffResult;
  Var
    iRes: Integer;
    FRecordsRead: Integer;
  Begin
    Result := DBIERR_NONE;
    If Cursor.Table.Dictionary.NoActionUpdate.Count > 0 Then
      Begin
        Str := TMemoryStream.Create;
        st := 0;
        Result := Cursor.Engine.SQLAlloc(Cursor.Client.ClientID, Cursor.DataBase.DatabaseID, Cursor.client.Timeout, st);
        Try
          If Result = DBIERR_NONE Then
            Begin
              atCursorID := Cursor.CursorID;
              For iRes := 0 To Cursor.Table.Dictionary.NoActionUpdate.Count - 1 Do
                Begin
                  sCo := Cursor.Table.Dictionary.NoActionUpdate[iRes];
                  MakeFunc(sCo, funName, tblName, SfldName, DfldName, DfldValue);
                  fi := Cursor.Table.Dictionary.FieldIndex(SfldName);
                  If (fi > -1) Then
                    Begin
                      FFGetMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                      Try
                        // move data to buffer
                        Cursor.Table.Dictionary.GetRecordField(fi, oldData, oldNul, FieldBuffer);
                        // buffer to str
                        BufStr := '';
                        BufStr := fsCurGetValueSql(Cursor.CursorID, fi, oldData, FieldBuffer, False, oldNul);
                        // new data
                        Cursor.Table.Dictionary.GetRecordField(fi, aData, newNul, FieldBuffer);
                        // buffer to str
                        BufnewStr := '';
                        BufnewStr := fsCurGetValueSql(Cursor.CursorID, fi, aData, FieldBuffer, False, newNul);
                        // sql
                        If newNul Then
                          sq := 'select ' + DfldName + ' from ' + tblName + ' where ' + DfldName + ' IS ' + BufnewStr
                        Else
                          sq := 'select ' + DfldName + ' from ' + tblName + ' where ' + DfldName + ' = ' + BufnewStr;
                      Finally
                        FFFreeMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                      End;
                      Result := Cursor.Engine.SQLPrepare(st, pointer(sq), Str);
                      If Result = DBIERR_NONE Then
                        Begin
                          Str.Clear;
                          Str.Position := 0;
                          Try
                            Result := Cursor.Engine.SQLExec(st, omReadOnly, atCursorID, Str);
                            If Result = DBIERR_NONE Then
                              Begin
                                FRecordsRead := 0;
                                TfsSrBaseCursor(atCursorID).SetToBegin;
                                If TfsSrBaseCursor(atCursorID).GetNextRecord(Nil, ffsltNone, aflag1, arefnr) = 0 Then
                                  FRecordsRead := 1;
                                tq := TfsSrBaseCursor(atCursorID);
                                If tq <> Nil Then
                                  Begin
                                    tq.CloseTable := True;
                                    Cursor.Engine.CursorClose(atCursorID);
                                  End;
                                //TfsSrBaseCursor(atCursorID).GetRecordCount(FRecordsRead);
                                If FRecordsRead = 0 Then
                                  Begin
                                    Result := 50001;
                                    System.Break;
                                  End;
                              End
                            Else
                              Begin
                                System.Break;
                              End;
                          Finally
                            tq := TfsSrBaseCursor(atCursorID);
                            If tq <> Nil Then
                              Begin
                                tq.CloseTable := True;
                                Cursor.Engine.CursorClose(atCursorID);
                              End;
                          End;
                        End
                      Else
                        Begin
                          System.Break;
                        End;
                    End;
                End; // for
            End;
        Finally
          Str.free;
          Cursor.Engine.sqlfree(st);
        End;
      End;
  End;

Begin
  aflag1 := 0;
  { Note: By this time, any other cursor deleting or modifying the record ahead
    of us has completed and has set bcInfo.Deleted.  We can be assured of this
    because TFSServer.RecordDelete calls Cursor.EnsureWritable(true) which
    obtains a lock on the record to be deleted.  We won't get that lock until
    the other cursor has finished. }
  Cursor := Self;
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
  FFGetMem(bcOldRecBuff, bcRecordLen); {!!.02}
  FFGetMem(OldData, bcRecordLen); {!!.02}

  bcNewRecBuff := aData;
  Try
    Move(bcRecordData^, bcOldRecBuff^, bcRecordLen); {!!.02}
    Result := NotifyExtenders(ffeaBeforeRecUpdate, ffeaUpdateRecFail);
    If Result = DBIERR_NONE Then
      Begin
        //xxlock
        //If Self.Database.RecLocking = tlPessimistic Then
        AcqContentLock(fsclmWrite);
        Table.GetRecord(bcDatabase.TransactionInfo, bcDatabase.DatabaseID,
          CursorID, bcInfo.refNr, OldData, ffsltNone, tluDatabase, True, False, aflag1);

        If use Then
          Begin
            Result := NotifyExtenders(ffeaBeforeProtectRow, ffeaProtectRowFail);
            If Result = DBIERR_NONE Then
              Begin
                Result := bcTable.PutSetFlagRecord(bcDatabase.TransactionInfo, CursorID,
                  bcInfo.refNr, aData, aFlag, aSet, Use);
                If Result = DBIERR_NONE Then
                  Begin
                    NotifyExtenders(ffeaAfterProtectRow, ffeaNoAction);
                    bcTable.RelaxRecordLock(bcDatabase.TransactionInfo, CursorID,
                      bcInfo.RefNr);
                  End
                Else
                  NotifyExtenders(ffeaProtectRowFail, ffeaNoAction);
              End
            Else
              Begin
                If Result = DBIERR_NOTSUFFTABLERIGHTS Then
                  Result := DBIERR_NOTSUFFFIELDRIGHTS;
                NotifyExtenders(ffeaProtectRowFail, ffeaNoAction);
              End;
          End
        Else
          Begin
            //fi:=FFCmpBytes(bcRecordData , OldData,bcRecordLen);
            //if bcRecordData <> OldData then Result := DBIERR_RECNOTFOUND else begin
            Try
              If Engine.Configuration.GeneralInfo^.giEnabledTrigers Then
                If Table.Dictionary.TrigersBeforeUpdateOperation.Count > 0 Then
                  Begin
                    Result := ExecScript(Table.Dictionary.TrigersBeforeUpdateOperation, OldData, aData,
                      bcDatabase.TransactionInfo.tirTrans,
                      bcDatabase.TransactionInfo.tirTrans);
                    If (Result <> DBIERR_NONE) Then
                      If (Result <> 50014) Then Result := 50005;
                  End;
            Except
              Result := 50005;
            End;

            {Begin !!.05}
            If Result = DBIERR_NONE Then
              If Engine.Configuration.GeneralInfo^.giEnabledReferential Then
                Result := NoAction;
            If Result = DBIERR_NONE Then
              If Engine.Configuration.GeneralInfo^.giEnabledReferential Then
                Result := RestrictUpdate; // add to References on cascade update k winnicki 31-10-2004 20:00

            If Result = DBIERR_NONE Then
              Begin
                If Engine.Configuration.GeneralInfo^.giEnabledReferential And
                  (Cursor.Table.Dictionary.CascadeUpdate.Count > 0) Then
                  Begin
                    sCo := '';
                    Try
                      st := 0;
                      Result := Cursor.Engine.SQLAlloc(Cursor.Client.ClientID, Cursor.DataBase.DatabaseID, Cursor.client.Timeout, st);
                      If Result = DBIERR_NONE Then
                        Begin
                          atCursorID := Cursor.CursorID;
                          For iCon := 0 To Cursor.Table.Dictionary.CascadeUpdate.Count - 1 Do
                            Begin
                              sCo := Cursor.Table.Dictionary.CascadeUpdate[iCon];
                              MakeFunc(sCo, funName, tblName, SfldName, DfldName, DfldValue);
                              fi := Cursor.Table.Dictionary.FieldIndex(SfldName);
                              If (fi > -1) Then
                                Begin
                                  FFGetMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                                  Try
                                    // move data to buffer
                                    Cursor.Table.Dictionary.GetRecordField(fi, oldData, oldNul, FieldBuffer);
                                    // buffer to str
                                    BufStr := '';
                                    BufStr := fsCurGetValueSql(Cursor.CursorID, fi, oldData, FieldBuffer, False, oldNul);
                                    // new data
                                    Cursor.Table.Dictionary.GetRecordField(fi, aData, NewNul, FieldBuffer);
                                    // buffer to str
                                    BufnewStr := '';
                                    BufnewStr := fsCurGetValueSql(Cursor.CursorID, fi, aData, FieldBuffer, False, newNul);
                                  Finally
                                    FFFreeMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                                  End;
                                  // if old <> new
                                  If BufStr <> BufNewStr Then
                                    Begin
                                      // sql
                                      sq := 'update ' + tblName + ' set ' + DfldName + ' = ' + BufnewStr +
                                        ' where ' + DfldName + ' = ' + BufStr;

                                      Result := Cursor.Engine.SQLPrepare(st, pointer(sq), Str);
                                      If Result = DBIERR_NONE Then
                                        Begin
                                          Result := Cursor.Engine.SQLExec(st, omReadWrite, atCursorID, Str);
                                          If Result <> DBIERR_NONE Then
                                            System.Break;
                                        End
                                      Else
                                        System.Break;
                                    End;
                                End;
                            End;
                        End;
                    Finally
                      Cursor.Engine.sqlfree(st);
                    End;
                  End;

                If Result = DBIERR_NONE Then
                  Begin
                    If Engine.Configuration.GeneralInfo^.giEnabledReferential And
                      (Cursor.Table.Dictionary.CascadeUpdateNull.Count > 0) Then
                      Begin
                        sCo := '';
                        Try
                          st := 0;
                          Result := Cursor.Engine.SQLAlloc(Cursor.Client.ClientID, Cursor.DataBase.DatabaseID, Cursor.client.Timeout, st);
                          If Result = DBIERR_NONE Then
                            Begin
                              atCursorID := Cursor.CursorID;
                              For iCon := 0 To Cursor.Table.Dictionary.CascadeUpdateNull.Count - 1 Do
                                Begin
                                  sCo := Cursor.Table.Dictionary.CascadeUpdateNull[iCon];
                                  MakeFunc(sCo, funName, tblName, SfldName, DfldName, DfldValue);
                                  fi := Cursor.Table.Dictionary.FieldIndex(SfldName);
                                  If (fi > -1) Then
                                    Begin
                                      FFGetMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                                      Try
                                        // move data to buffer
                                        Cursor.Table.Dictionary.GetRecordField(fi, oldData, oldNul, FieldBuffer);
                                        // buffer to str
                                        BufStr := '';
                                        BufStr := fsCurGetValueSql(Cursor.CursorID, fi, oldData, FieldBuffer, False, oldNul);
                                        // new data
                                        Cursor.Table.Dictionary.GetRecordField(fi, aData, NewNul, FieldBuffer);
                                        // buffer to str
                                        BufnewStr := '';
                                        BufnewStr := fsCurGetValueSql(Cursor.CursorID, fi, aData, FieldBuffer, False, newNul);
                                      Finally
                                        FFFreeMem(FieldBuffer, Cursor.Table.Dictionary.FieldLength[fi]);
                                      End;
                                      // if old <> new
                                      If BufStr <> BufNewStr Then
                                        Begin
                                          // sql
                                          sq := 'update ' + tblName + ' set ' + DfldName + ' = NULL' +
                                            ' where ' + DfldName + ' = ' + BufStr;

                                          Result := Cursor.Engine.SQLPrepare(st, pointer(sq), Str);
                                          If Result = DBIERR_NONE Then
                                            Begin
                                              Result := Cursor.Engine.SQLExec(st, omReadWrite, atCursorID, Str);
                                              If Result <> DBIERR_NONE Then
                                                System.Break;
                                            End
                                          Else
                                            System.Break;
                                        End;
                                    End;
                                End;
                            End;
                        Finally
                          Cursor.Engine.sqlfree(st);
                        End;
                      End;
                  End;

                Table.GetRecord(bcDatabase.TransactionInfo, bcDatabase.DatabaseID, {!!.10}
                  CursorID, bcInfo.refNr, OldData, ffsltNone, tluDatabase, True, False, aflag1);

                Try
                  If (Result = DBIERR_NONE) Then
                    If Engine.Configuration.GeneralInfo^.giEnabledTrigers Then
                      If Table.Dictionary.TrigersBeforeUpdate.Count > 0 Then
                        Begin
                          Result := ExecScript(Table.Dictionary.TrigersBeforeUpdate, OldData, aData,
                            bcDatabase.TransactionInfo.tirTrans,
                            bcDatabase.TransactionInfo.tirTrans);
                          If (Result <> DBIERR_NONE) Then
                            If (Result <> 50014) Then Result := 50005;
                        End;
                Except
                  Result := 50005;
                End;

                If (Result = DBIERR_NONE) Then
                  Begin
                    // test all fields for emptyAsNull
                    If Self.Dictionary.IsAnyEmptyAsNull Or Self.Dictionary.IsAnyRound Or
                      Self.Dictionary.IsAnyDefault Or Self.Dictionary.IsRecVersion Then
                      For iField := 0 To Self.Dictionary.FieldCount - 1 Do
                        Begin
                          //fstSingle..fstCurrency
                          Try
                            Case Self.Dictionary.FieldDescriptor^[iField]^.fdType Of
                              fstShortString, fstSingleWideChar, fstSingleChar, fstNullString, fstvarNullString, fstWideString, fstVarWideString:
                                If Self.Dictionary.FieldDescriptor^[iField]^.fdEmptyAsNull Then
                                  Begin
                                    FFGetMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                                    Try
                                      Self.Dictionary.GetRecordField(iField, aData, oldNul, FieldBuffer);
                                      If Not oldNul Then
                                        Begin
                                          se := fsCurGetValue(Self.CursorID, iField, aData, FieldBuffer, False);
                                          If se = '' Then
                                            Self.Dictionary.SetRecordFieldNull(iField, aData, True);
                                        End;
                                    Finally
                                      FFFreeMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                                    End;
                                  End;

                              fstSingle..fstCurrency:
                                If Self.Dictionary.FieldDescriptor^[iField]^.fdRound <> rNone Then
                                  Begin
                                    FFGetMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                                    Try
                                      Self.Dictionary.GetRecordField(iField, aData, oldNul, FieldBuffer);
                                      If Not oldNul Then
                                        Begin
                                          E := fsCurGetValueE(Self.CursorID, iField, aData, FieldBuffer);
                                          If E > 0 Then
                                            Begin
                                              E := RoundExtended(E, Self.Dictionary.FieldDescriptor^[iField]^.fdDecPl,
                                                Self.Dictionary.FieldDescriptor^[iField]^.fdRound);
                                              fsCurSetValueE(E, Self.CursorID, iField, aData, FieldBuffer);
                                            End;
                                        End;
                                    Finally
                                      FFFreeMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                                    End;
                                  End;
                              fstRecVersion:
                                Begin
                                  FFGetMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                                  Try
                                    Self.Dictionary.GetRecordField(iField, aData, oldNul, FieldBuffer);
                                    If oldNul Then
                                      Begin
                                        pint64(FieldBuffer)^ := Self.Dictionary.FieldDescriptor^[iField]^.fdUnits;
                                        Self.Dictionary.SetRecordField(iField, aData, FieldBuffer);
                                      End
                                    Else
                                      Begin
                                        Self.Dictionary.GetRecordField(iField, OldData, oldNul, FieldBuffer);
                                        If Not oldNul Then
                                          Begin
                                            i64 := pint64(FieldBuffer)^;
                                            pint64(FieldBuffer)^ := i64 + Self.Dictionary.FieldDescriptor^[iField]^.fdDecPl;
                                            Self.Dictionary.SetRecordField(iField, aData, FieldBuffer);
                                          End
                                        Else
                                          Begin
                                            pint64(FieldBuffer)^ := Self.Dictionary.FieldDescriptor^[iField]^.fdUnits;
                                            Self.Dictionary.SetRecordField(iField, aData, FieldBuffer);
                                          End;
                                      End;
                                  Finally
                                    FFFreeMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                                  End;
                                End;
                            End;
                            If Dictionary.DefaultFieldCount > 0 Then
                              Begin
                                FFGetMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                                Try
                                  Self.Dictionary.GetRecordField(iField, aData, oldNul, FieldBuffer);
                                  If Self.Dictionary.FieldDescriptor^[iField]^.fdDefaultUpdate = duIFNULL Then
                                    Begin
                                      If oldNul And (Self.Dictionary.FieldDescriptor^[iField]^.fdDefaultUpdate = duIFNULL) Then
                                        Self.Dictionary.SetDefaultFieldUpdateValue(aData, iField);
                                    End
                                  Else
                                    Begin
                                      If Self.Dictionary.FieldDescriptor^[iField]^.fdDefaultUpdate = duALWAYS Then
                                        Self.Dictionary.SetDefaultFieldUpdateValue(aData, iField);
                                    End;
                                Finally
                                  FFFreeMem(FieldBuffer, Self.Dictionary.FieldLength[iField]);
                                End;
                              End;
                          Except
                            Result := ERRCODE_WRITEERR;
                            Raise;
                          End;
                        End;
                    If (Result = DBIERR_NONE) Then
                      Begin
                        CheckEmptyOrErrorBLOBs;
                        Result := bcTable.PutRecord(bcDatabase.TransactionInfo, CursorID,
                          bcInfo.refNr, aData, aRelLock, aUserLockType, aKeyChanged);
                      End;
                  End;

                Try
                  If Engine.Configuration.GeneralInfo^.giEnabledTrigers Then
                    If (Result = DBIERR_NONE) Then
                      If Table.Dictionary.TrigersAfterUpdate.Count > 0 Then
                        Begin
                          Result := ExecScript(Table.Dictionary.TrigersAfterUpdate, OldData, aData,
                            bcDatabase.TransactionInfo.tirTrans,
                            bcDatabase.TransactionInfo.tirTrans);
                          If (Result <> DBIERR_NONE) Then
                            If (Result <> 50014) Then Result := 50006;
                        End;
                Except
                  Result := 50006;
                End;
              End;
            Tr := Result;
            Try
              If Engine.Configuration.GeneralInfo^.giEnabledTrigers Then
                If Table.Dictionary.TrigersAfterUpdateOperation.Count > 0 Then
                  Begin
                    Result := ExecScript(Table.Dictionary.TrigersAfterUpdateOperation, OldData, aData,
                      bcDatabase.TransactionInfo.tirTrans,
                      bcDatabase.TransactionInfo.tirTrans);
                    If (Result <> DBIERR_NONE) Or (Tr <> DBIERR_NONE) Then
                      If (Result <> 50014) Then Result := 50006;
                  End;
            Except
              Result := 50006;
            End;

            If (Result = DBIERR_NONE) Then
              Begin
                bcTable.RelaxRecordLock(bcDatabase.TransactionInfo, CursorID, {!!.10}
                  bcInfo.RefNr); {!!.10}
                { Was the key for the current index changed? }
                SavKey := Nil;
                If aKeyChanged Then
                  Begin
                    { Yes. Save the current key & rebuild it so that we may reposition to
                      the record. }
                    FFGetMem(SavKey, scKeyLen);
                    Try
                      Move(bcCurKey^, SavKey^, scKeyLen);
                      scRebuildCurKey(aData, True);
                      { Does the new key fall outside of the current range? }
                      If IsInRange(bcCurKey) <> 0 Then
                        { Yes. Restore the old key. The cursor will be repositioned to the
                          next record. }
                        Move(SavKey^, bcCurKey^, scKeyLen);
                    Finally
                      FFFreeMem(SavKey, scKeyLen);
                    End;
                  End;

                FFInitKeyPath(bcInfo.KeyPath);
                bcInfo.pos := cpOnRecord;
                bcRebuildKeyPath;

                { Notify extenders of successful update. }
                NotifyExtenders(ffeaAfterRecUpdate, ffeaNoAction);
              End
            Else
              Begin
                { Notify extenders of failed update. }
                NotifyExtenders(ffeaUpdateRecFail, ffeaNoAction);
              End;
          End; // for normal
      End;
  Finally
    FFFreeMem(bcOldRecBuff, bcRecordLen);
    FFFreeMem(OldData, bcRecordLen);
    bcOldRecBuff := Nil;
    bcNewRecBuff := Nil;
    //bcTable.RelaxRecordLock(bcDatabase.TransactionInfo, CursorID, bcInfo.RefNr);
  End;
End;
{--------}

Procedure TfsSrcCursor.ResetRange;
Begin
  bcHasRange := False;
End;
{--------}

Procedure TfsSrcCursor.scRebuildCurKey(aRecData: PffByteArray;
  aLockObtained: boolean);
Var
  aFlag: Byte;
Begin
  aflag := 0;
  bcInvalidateCurKey;
  If (IndexID = 0) Then
    Begin
      Move(bcInfo.refNr, bcCurKey^, scKeyLen);
      bcInfo.KeyValid := True;
    End
  Else
    Begin
      AcqContentLock(fsclmRead);
      Try
        { If we have been passed the record buffer then use it otherwise
          read the record from the file. }
        If assigned(aRecData) Then
          Move(aRecData^, bcRecordData^, Table.Files[0]^.fiRecordLength)
        Else
          Table.GetRecord(bcDatabase.TransactionInfo, {!!.10}
            bcDatabase.DatabaseID, {!!.10}
            CursorID, bcInfo.refNr, {!!.10}
            bcRecordData, ffsltNone, tluDatabase, aLockObtained, False, aflag); {!!.02}

        {calculate the key for this record}
        bcInfo.KeyValid :=
          (Table.BuildKeyForRecord(IndexID, bcRecordData, bcCurKey, 0, 0) = DBIERR_NONE);
      Finally
        RelContentLock(fsclmRead);
      End;
    End;
End;
{--------}

Procedure TfsSrBaseCursor.bcRebuildKeyPath; {!!.05 - Moved from TfsSrcCursor.scRebuildKeyPath}
Var
  InRangeResult: Integer;
Begin

  { Assumption: Content read lock already obtained. }

  { If we have a valid key, calculate the actual key path by finding that key
    within the current index. }
  If bcIsCurKeyValid Then
    Begin
      FFInitKeyPath(bcInfo.KeyPath);
      With bcCompareData Do
        Begin
          cdFldCnt := 0;
          cdPartLen := 0;
        End;
      If Table.FindKey(bcKID, bcInfo.refNr, bcDatabase.TransactionInfo, bcCurKey,
        bcInfo.KeyPath, skaGreaterEqual) Then
        Begin
          { Does the key fit within the current range? }
          InRangeResult := IsInRange(bcCurKey);
          If InRangeResult <> 0 Then
            bcInfo.pos := cpOnCrack
          Else
            { Make sure that the current position is set to reflect the
              keypath's position. }
            Case bcInfo.KeyPath.kpPos Of
              kppBOF: SetToBegin;
              kppOnCrackBefore,
                kppOnCrackAfter: bcInfo.pos := cpOnCrack;
              kppEOF: SetToEnd;
            End; {case}
        End; { if }
    End;
End;
{--------}

Function TfsSrcCursor.SetRange(aDirectKey: boolean;
  aFieldCount1: Integer;
  aPartialLen1: Integer;
  aKeyData1: PffByteArray;
  aKeyIncl1: boolean;
  aFieldCount2: Integer;
  aPartialLen2: Integer;
  aKeyData2: PffByteArray;
  aKeyIncl2: boolean): TffResult;
Begin
  Result := DBIERR_NONE;
  {we now have a range}
  bcRng1Valid := (aKeyData1 <> Nil);
  bcRng2Valid := (aKeyData2 <> Nil);
  {calculate the keys}
  If aDirectKey Then
    Begin
      If bcRng1Valid Then
        Move(aKeyData1^, bcRng1Key^, scKeyLen);
      If bcRng2Valid Then
        Move(aKeyData2^, bcRng2Key^, scKeyLen);
    End
  Else
    Begin
      If bcRng1Valid Then
        Result := Table.BuildKeyForRecord(IndexID, aKeyData1, bcRng1Key,
          aFieldCount1, aPartialLen1);
      If (Result = DBIERR_NONE) And bcRng2Valid Then
        Result := Table.BuildKeyForRecord(IndexID, aKeyData2, bcRng2Key,
          aFieldCount2, aPartialLen2);
      If (Result <> DBIERR_NONE) Then
        Exit;
    End;
  {store the other fields}
  If bcRng1Valid Then
    Begin
      bcRng1FldCnt := aFieldCount1;
      bcRng1PtlLen := aPartialLen1;
      bcRng1Incl := aKeyIncl1;
    End;
  If bcRng2Valid Then
    Begin
      bcRng2FldCnt := aFieldCount2;
      bcRng2PtlLen := aPartialLen2;
      bcRng2Incl := aKeyIncl2;
    End;
  {position ourselves at BOF}
  SetToBegin;
  bcHasRange := True;
End;
{--------}

Procedure TfsSrcCursor.SetToBegin;
Begin
  AcqContentLock(fsclmRead);
  Try
    bcInfo.pos := cpBOF;
    FFSetKeyPathToBOF(bcInfo.KeyPath);
    bcInvalidateCurKey;
    ffInitI64(bcInfo.refNr);
    bcInfo.Deleted := False;
  Finally
    RelContentLock(fsclmRead);
  End;
End;
{--------}

Function TfsSrcCursor.SetToBookmark(aBookmark: PffByteArray): TffResult;
Begin
  Result := CheckBookmark(aBookmark);
  If (Result = DBIERR_NONE) Then
    Begin

      { Requirement: The cursor must be on the same index as the bookmark. }
      If IndexID <> PfsSrBookmark(aBookmark)^.sbIndexID Then
        Begin
          Result := DBIERR_INVALIDBOOKMARK;
          Exit;
        End;

      AcqContentLock(fsclmRead);
      bcInfoLock.Lock; {!!.06}
      Try
        { Initialize the key path. }
        FFInitKeyPath(bcInfo.KeyPath);
        With PfsSrBookmark(aBookmark)^ Do
          Begin
            bcInfo.pos := sbPos;
            bcInfo.refNr := sbRefNr;
            bcInfo.KeyValid := sbKeyValid;
            bcInfo.Deleted := False;
            If sbKeyValid Then
              Move(sbKey, bcCurKey^, sbKeyLen);
            Try
              { See if the record still exists by rebuilding the key path. }
              bcRebuildKeyPath; {!!.05}

              { Does the record still exist? }
              If (ffCmpI64(bcInfo.refNr, sbRefNr) <> 0) Then
                Begin
                  { No.  Position the cursor to the crack before the record. }
                  bcInfo.pos := cpOnCrack;
                  bcInfo.refNr := sbRefNr;
                  If (bcInfo.KeyPath.kpPos = kppOnKey) Then
                    Begin
                      Assert(bcInfo.KeyPath.kpCount > 0);
                      bcInfo.KeyPath.kpPos := kppOnCrackBefore;
                    End;
                  bcInfo.Deleted := True;
                End;
            Except
              SetToBegin;
              Result := DBIERR_INVALIDBOOKMARK;
            End;
          End;
      Finally
        bcInfoLock.Unlock; {!!.06}
        RelContentLock(fsclmRead);
      End;
    End;
End;
{--------}

Function TfsSrcCursor.SetToCursor(aCursor: TfsSrBaseCursor): TffResult;
Var
  InRangeResult: Integer;
Begin
  Result := DBIERR_NONE;
  If (aCursor.Table <> Table) Then
    Begin
      Result := DBIERR_DIFFERENTTABLES;
      Exit;
    End;

  AcqContentLock(fsclmRead);
  Try
    { If the cursors are using the same index, copy over the source cursor's
      information as is.}
    If (aCursor.IndexID = IndexID) Then
      Begin
        bcInfo := aCursor.bcInfo;
        If bcInfo.KeyValid Then
          Move(aCursor.bcCurKey^, bcCurKey^, scKeyLen);
        { If this cursor has a range applied and the record to which it is
          positioning does not fit within the range, position the cursor on crack. }
        If (bcInfo.pos In [cpOnRecord, cpOnCrack]) And bcInfo.KeyValid Then
          Begin
            InRangeResult := IsInRange(bcCurKey);
            If InRangeResult <> 0 Then
              aCursor.bcInfo.Pos := cpOnCrack;
          End;
      End
    Else
      Begin
        { Otherwise, the cursor's are on different indices. }

        {  If the source cursor is not on a record then return an error.  This
          could happen if the source cursor was not originally on a record or
          the record has been deleted by the time we were granted a lock on the
          record. }
        If (aCursor.bcInfo.pos <> cpOnRecord) Then
          Begin
            If bcInfo.Deleted Then
              Result := DBIERR_KEYORRECDELETED
            Else
              Result := DBIERR_NOCURRREC;
            Exit;
          End;

        { Otherwise, position this cursor to the same record as the source cursor.
          We can use the source cursor's refNr as is.  We don't need to figure out
          the key path.  However, we do need to rebuild this cursor's key based
          upon the current index. }
        bcInfo.pos := cpOnRecord;
        bcInfo.refNr := aCursor.bcInfo.refNr;
        FFInitKeyPath(bcInfo.KeyPath);
        scRebuildCurKey(Nil, True);
        bcRebuildKeyPath; {!!.05}
      End;
  Finally
    RelContentLock(fsclmRead);
  End;
End;
{--------}

Procedure TfsSrcCursor.SetToEnd;
Begin
  AcqContentLock(fsclmRead);
  Try
    bcInfo.pos := cpEOF;
    FFSetKeyPathToEOF(bcInfo.KeyPath);
    bcInvalidateCurKey;
    ffInitI64(bcInfo.refNr);
    bcInfo.Deleted := False;
  Finally
    RelContentLock(fsclmRead);
  End;
End;
{--------}

Function TfsSrcCursor.SetToKey(aSearchAction: TffSearchKeyAction;
  aDirectKey: boolean;
  aFieldCount: Integer;
  aPartialLen: Integer;
  aKeyData: PffByteArray): TffResult;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  aTI: PffTransInfo;
  InRangeResult: Integer;
Begin
  aflag := 0;
  {calculate the key}
  If aDirectKey Then
    Move(aKeyData^, bcCurKey^, scKeyLen)
  Else
    Begin
      Result := Table.BuildKeyForRecord(IndexID, aKeyData, bcCurKey,
        aFieldCount, aPartialLen);
      If (Result <> DBIERR_NONE) Then
        Exit;
    End;

  AcqContentLock(fsclmRead);
  bcInfoLock.Lock; {!!.06}
  Try
    {now position the index on that key or the one that partially
     matches it}
    FFInitKeyPath(bcInfo.KeyPath);
    ffInitI64(bcInfo.refNr);
    bcInfo.Deleted := False;
    aTI := Database.TransactionInfo;
    {try to find the key according to the search action}
    With bcCompareData Do
      Begin
        cdFldCnt := aFieldCount;
        cdPartLen := aPartialLen;
      End;
    If Table.FindKey(bcKID, bcInfo.refNr, aTI, bcCurKey, bcInfo.KeyPath,
      aSearchAction) Then
      Begin
        {we found it}
        Result := DBIERR_NONE;
        {if we're at EOF, set all current key variables and exit}
        If (bcInfo.KeyPath.kpPos = kppEOF) Then
          Begin
            SetToEnd;
            Exit;
          End;
        {but did we? better see whether we're in the current range}
        InRangeResult := IsInRange(bcCurKey);
        {the key we found is before the start of the range: position
         ourselves at BOF, and only signal an error if the search action
         was "search for equal"}
        If (InRangeResult < 0) Then
          Begin
            If aSearchAction = skaEqual Then
              Result := DBIERR_RECNOTFOUND;
            SetToBegin;
            Exit;
          End;
        {the key we found is after the end of the range: position
         ourselves at EOF, and only signal an error if the search action
         was "search for equal"}
        If (InRangeResult > 0) Then
          Begin
            If aSearchAction = skaEqual Then
              Result := DBIERR_RECNOTFOUND;
            SetToEnd;
            Exit;
          End;
        If Assigned(bcFilter) Then
          Begin
            Table.GetRecord(aTI, bcDatabase.DatabaseID, {!!.10}
              CursorID, bcInfo.refNr, bcRecordData, ffsltNone, tluDatabase, {!!.10}
              False, False, aflag); {!!.02}
            If Not bcFilter.MatchesRecord(bcRecordData) Then
              Begin
                If aSearchAction = skaEqual Then
                  Result := DBIERR_RECNOTFOUND
                Else
                  Begin {begin !!.11}
                    Repeat
                      Result := bcTable.GetNextRecord(aTI,
                        bcDatabase.DatabaseID,
                        CursorID, bcKID, bcInfo.refNr,
                        bcCurKey, bcInfo.KeyPath, bcRecordData,
                        ffsltNone, aflag);
                      If (Result <> DBIERR_NONE) Then
                        Begin
                          If (Result = DBIERR_EOF) Then
                            SetToEnd;
                          Exit;
                        End;
                      {in theory we're on a record}
                      bcInfo.Deleted := False;
                      bcInfo.KeyValid := True;
                      bcInfo.pos := cpOnRecord;
                    Until (Result <> DBIERR_NONE) Or
                      (Not Assigned(bcFilter) Or bcFilter.MatchesRecord(bcRecordData)
                      Or bcFilter.CheckTimeout(Result));
                  End; {end !!.11}
                If Result = DBIERR_FS_FilterTimeout Then
                  Exit;
                If Result <> DBIERR_NONE Then
                  Begin
                    SetToEnd;
                    Exit;
                  End;
              End;
          End;
        {SetToKey is supposed to leave the position on the crack before
         the record, so make sure}
        bcInfo.KeyValid := True;
        bcInfo.pos := cpOnCrack;
        If (bcInfo.KeyPath.kpPos = kppOnKey) Then
          Begin
            Assert(bcInfo.KeyPath.kpCount > 0);
            bcInfo.KeyPath.kpPos := kppOnCrackBefore;
          End;
      End
    Else {we didn't find the key}
      Begin
        {if the search action was "search for equal", signal an error and
         position ourselves at BOF}
        If aSearchAction = skaEqual Then
          Begin
            Result := DBIERR_RECNOTFOUND;
            SetToBegin;
            Exit;
          End;
        {otherwise we're fine}
        Result := DBIERR_NONE;
        {if we're at EOF, set all current key variables and exit}
        If (bcInfo.KeyPath.kpPos = kppEOF) Then
          Begin
            SetToEnd;
            Exit;
          End;
        {check whether we're in the current range or not}
        InRangeResult := IsInRange(bcCurKey);
        If InRangeResult <> 0 Then
          Begin
            bcInfo.Pos := cpOnCrack;
            Exit;
          End;

        If Assigned(bcFilter) Then
          Begin
            Table.GetRecord(aTI, bcDatabase.DatabaseID, {!!.10}
              CursorID, bcInfo.refNr, bcRecordData, ffsltNone, tluDatabase, {!!.10}
              False, False, aflag); {!!.02}
            If Not bcFilter.MatchesRecord(bcRecordData) Then
              Begin
                Result := GetNextRecord(bcRecordData, ffsltNone, aflag, arefnr);
                If Result = DBIERR_FS_FilterTimeout Then
                  Exit;
                If Result <> DBIERR_NONE Then
                  Begin
                    SetToEnd;
                    Exit;
                  End;
              End;
          End;
        {otherwise set all current key variables}
        bcInfo.KeyValid := True;
        bcInfo.pos := cpOnCrack;
      End;
  Finally
    bcInfoLock.Unlock; {!!.06}
    RelContentLock(fsclmRead);
  End;
End;
{--------}

Function TfsSrcCursor.SwitchToIndex(aIndexID: Integer;
  aPosnOnRec: boolean): TffResult;
Begin
  {Assumption: aIndexID has been validated}
  Result := DBIERR_NONE;

  If aPosnOnRec And (bcInfo.pos <> cpOnRecord) Then
    Begin
      If bcInfo.Deleted Then
        Result := DBIERR_KEYORRECDELETED
      Else
        Result := DBIERR_NOCURRREC;
      Exit;
    End;

  AcqContentLock(fsclmRead);
  Try
    {set the index}
    bcIndexID := aIndexID;
    {free the key buffers}
    FFFreeMem(bcCurKey, scKeyLen);
    FFFreeMem(bcRng1Key, scKeyLen);
    FFFreeMem(bcRng2Key, scKeyLen);
    {we lose our range}
    bcHasRange := False;
    {get our work areas for the key}
    scKeyLen := bcTable.Dictionary.IndexKeyLength[aIndexID];
    FFGetMem(bcCurKey, scKeyLen);
    FFGetMem(bcRng1Key, scKeyLen);
    FFGetMem(bcRng2Key, scKeyLen);
    {initialise our key index data record}
    bcTable.MakeKIDForCursor(aIndexID, bcKID);
    { Set up the position of the cursor to the current record or BOF. }
    If aPosnOnRec Then
      Begin
        { Note that we've already checked that bcInfo.pos is cpOnRecord. }
        scRebuildCurKey(Nil, False);
        bcRebuildKeyPath; {!!.05}
      End
    Else
      SetToBegin;
  Finally
    RelContentLock(fsclmRead);
  End;
End;
{====================================================================}

{===TfsSrcCursorList==================================================}

Procedure TfsSrcCursorList.AddCursor(aCursor: TfsSrBaseCursor);
Begin
  solList.Insert(aCursor);
End;
{--------}

Function TfsSrcCursorList.CursorCount: Integer;
Begin
  Assert(Assigned(solList));
  Result := solList.Count;
End;
{--------}

Procedure TfsSrcCursorList.DeleteCursor(aCursorID: TffCursorID);
Begin
  solList.Delete(aCursorID);
End;
{--------}

Function TfsSrcCursorList.GetCursorItem(Find: TfsListFindType; Value: Longint): TfsSrBaseCursor;
Var
  Inx: Integer;
Begin
  Result := Nil;
  If (Find = ftFromID) Then
    Begin
      Inx := solList.Index(Value);
      If (Inx <> -1) Then
        Result := TfsSrBaseCursor(solList[Inx]);
    End
  Else {Find = ftFromIndex}
    Result := TfsSrBaseCursor(solList[Value]);
End;
{--------}

Procedure TfsSrcCursorList.RemoveCursor(aCursorID: TffCursorID);
Begin
  solList.Remove(aCursorID);
End;
{====================================================================}

{===TfsSrcBaseTable===================================================}

Constructor TfsSrcBaseTable.Create(anEngine: TFSServer;
  Const aBaseName: TfsTableName;
  aFolder: TfsSrcFolder;
  aBufMgr: TfsBufferManager;
  Const aOpenMode: TffOpenMode);
Begin
  Inherited Create;
  btBaseName := FFShStrAlloc(aBaseName);
  btBufMgr := aBufMgr;
  btEngine := anEngine;
  btFolder := aFolder;
  btTableFlags := 0;
  btMaxRecords := 0;
  btTableUndelete := False;
  {create the data dictionary, it'll be empty for now}
  btDictionary := TFSInfoServerDict.Create(4096);
  btDictionary.SetBaseName(aBaseName);
  {create the list of file info records, set the capacity to 8,
   generally tables will have less than this number of files}
  btFiles := TfsVCLList.Create;
  btFiles.Capacity := 8;
  {create the cursor list}
  btCursorList := TfsSrcCursorList.Create;
  btContentLocks := TfsLockContainer.Create;
  btClientLocks := TfsLockContainer.Create;
  btPortal := TfsReadWritePortal.Create;
End;
{--------}

Destructor TfsSrcBaseTable.Destroy;
Begin
  Try {!!.06}
    CloseFiles(False, Nil);
  Finally {!!.06}
    btCursorList.Free;
    btFiles.Free;
    btDictionary.Free;
    btContentLocks.Free;
    btClientLocks.Free;
    btPortal.Free;
    FFShStrFree(btBaseName);
    Inherited Destroy;
  End; {!!.06}
End;
{--------}

Procedure TfsSrcBaseTable.AcqClientLock(aCursorID: Longint;
  Const aLockType: TfsSrcLockType;
  Const aConditional: Boolean);
Var
  LockStatus: TffLockRequestStatus;
  RetryUntil: DWORD;
  TblStr: String;
  TickCount: DWORD;
Begin

  RetryUntil := FFGetRetry;
  TickCount := GetTickCount;

  { Do we have any time left?  Note that we are doing this check to avoid
    the situation where we ask for a lock and pass a negative timeout. }
  If (RetryUntil > TickCount) And
    ((RetryUntil - TickCount) >= 5) Then
    Begin
      { If there are record locks already on the table then raise an
        exception. }
      If HasRecordLocks Then
        FSRaiseException(EfsServerException, fsStrResServer, fserrLockRejected,
          [FFMapLockToName(ffsltExclusive), '',
          PffFileInfo(btFiles[0])^.fiName^]);

      { Obtain an exclusive lock on the table content. }
      LockStatus := Folder.LockMgr.AcquireClientLock(btClientLocks,
        aCursorID,
        (RetryUntil - TickCount),
        aLockType);

      { Raise an exception if something went awry. }
      If LockStatus <> fflrsGranted Then
        TblStr := format(fscTableContent, [btBaseName^]);
      Case LockStatus Of
        fflrsTimeout:
          FSRaiseException(EfsServerException, fsStrResServer, fserrTableLockTimeout,
            [FFMapLockToName(aLockType), TblStr]);
        fflrsRejected:
          FSRaiseException(EfsServerException, fsStrResServer, fserrLockRejected,
            [FFMapLockToName(aLockType), TblStr, '']);
      End; { case }
    End
  Else
    { No.  Assume we will time out waiting for the resource. }
    FSRaiseExceptionNoData(EfsServerException, fsStrResServer,
      fserrGeneralTimeout);
End;
{--------}

Procedure TfsSrcBaseTable.AcqContentLock(aTrans: TfsSrcTransaction;
  Const aLockType: TfsSrcLockType;
  Const aConditional: Boolean;
  Const aUserLockType: TfsUserRecLocking = tluDatabase);
Var
  LockStatus: TffLockRequestStatus;
  RetryUntil: DWORD;
  TblStr: String;
  TickCount: DWORD;
  TranLockType: TfsSrcLockType; {!!.03}
Begin
  //xxlock
  {Begin !!.03}
    { Does the transaction have a lock container? }
  If assigned(aTrans.TransLockContainer) Then
    Begin
      { Yes. Does it already have a sufficient lock on this table? }
      TranLockType := TfsTransContainer(aTrans.TransLockContainer).TableContentLockType(btContentLocks);
      If TranLockType >= aLockType Then
        { Yes. Exit. We don't need to request another lock since we have one
          already. }
        Exit;

      { Does this transaction already have a share lock on this table & is now
        requesting an exclusive lock? }
      If (TranLockType = ffsltShare) And
        (aLockType = ffsltExclusive) And
        (btContentLocks.Count > 1) Then
        Begin
          { Yes. Does another transaction currently have a share lock on this
            table and is already waiting for an exclusive lock? }
          btContentLocks.BeginRead;
          Try
            If btContentLocks.SimpleDeadlock Then
              { Yes. We have a simple deadlock situation. Raise a deadlock exception
                so that this transaction is rolled back. This will free up its
                share lock which may allow the other transaction to continue
                processing. }
              FSRaiseException(EfsServerException, fsStrResServer,
                fserrDeadlock,
                [FFMapLockToName(aLockType),
                Format(fscTableContent, [btBaseName^]),
                  aTrans.TransactionID]);
          Finally
            btContentLocks.EndRead;
          End;
        End;
    End;
  {End !!.03}

  RetryUntil := FFGetRetry;
  TickCount := GetTickCount;

  { Do we have any time left?  Note that we are doing this check to avoid
    the situation where we ask for a lock and pass a negative timeout. }
  If (RetryUntil > TickCount) And
    ((RetryUntil - TickCount) >= 5) Then
    Begin

      { Obtain an exclusive lock on the table content. }
      LockStatus := Folder.LockMgr.AcquireContentLock(btContentLocks,
        Self,
        aTrans,
        aConditional,
        (RetryUntil - TickCount),
        aLockType);

      { Raise an exception if something went awry. }
      If LockStatus <> fflrsGranted Then
        TblStr := format(fscTableContent, [btBaseName^]);
      Case LockStatus Of
        fflrsTimeout:
          FSRaiseException(EfsServerException, fsStrResServer, fserrTableLockTimeout,
            [FFMapLockToName(aLockType), TblStr]);
        fflrsRejected:
          FSRaiseException(EfsServerException, fsStrResServer, fserrLockRejected,
            [FFMapLockToName(aLockType), TblStr, '']);
      End; { case }
    End
  Else
    { No.  Assume we will time out waiting for the resource. }
    FSRaiseExceptionNoData(EfsServerException, fsStrResServer,
      fserrGeneralTimeout);
End;
{Begin !!.10}
{--------}

Function TfsSrcBaseTable.AcqExclContentLock(aTrans: TfsSrcTransaction): TffResult;
Var
  LockStatus: TffLockRequestStatus;
Begin
  { Obtain an exclusive lock on the table content. }
  LockStatus := Folder.LockMgr.AcquireContentLock(btContentLocks,
    Self,
    aTrans,
    True,
    0,
    ffsltExclusive);

  { Set the result. }
  Case LockStatus Of
    fflrsGranted: Result := DBIERR_NONE;
    fflrsTimeout: Result := fserrLockTimeout;
    fflrsRejected: Result := fserrLockRejected;
    Else
      Result := DBIERR_FS_Unknown;
  End; { case }
End;
{End !!.10}
{--------}

Procedure TfsSrcBaseTable.AcqLock(Const aCursorID: TffCursorID;
  Const aLockType: TfsSrcLockType);
Var
  LockStatus: TffLockRequestStatus;
  RetryUntil: DWORD;
  TblStr: String;
  TickCount: DWORD;
Begin

  RetryUntil := FFGetRetry;
  TickCount := GetTickCount;

  { Do we have any time left?  Note that we are doing this check to avoid
    the situation where we ask for a lock and pass a negative timeout. }
  If (RetryUntil > TickCount) And
    ((RetryUntil - TickCount) >= 5) Then
    Begin

      { Obtain an exclusive lock on the file header. }
      LockStatus := Folder.LockMgr.AcquireTableLock(TableID, aLockType,
        False,
        (RetryUntil - TickCount),
        aCursorID);
      { Raise an exception if something went awry. }
      If LockStatus <> fflrsGranted Then
        TblStr := format(fscTable, [btBaseName^]);
      Case LockStatus Of
        fflrsTimeout:
          FSRaiseException(EfsServerException, fsStrResServer, fserrTableLockTimeout,
            [FFMapLockToName(aLockType), TblStr]);
        fflrsRejected:
          FSRaiseException(EfsServerException, fsStrResServer, fserrLockRejected,
            [FFMapLockToName(aLockType), TblStr, '']);
      End; { case }
    End
  Else
    { No.  Assume we will time out waiting for the resource. }
    FSRaiseExceptionNoData(EfsServerException, fsStrResServer,
      fserrGeneralTimeout);
End;
{Begin !!.03}
{--------}

Procedure TfsSrcBaseTable.AddAttribute(Const anAttrib: TffFileAttribute);
Var
  Index: Longint;
Begin
  For Index := 0 To pred(FileCount) Do
    include(Files[Index].fiAttributes, anAttrib);
End;
{End !!.03}
{--------}

Procedure TfsSrcBaseTable.BeginCommit;
Begin
  btPortal.BeginWrite;
End;
{--------}

Procedure TfsSrcBaseTable.BeginRead;
Begin
  btPortal.BeginRead;
End;
{Begin !!.03}
{--------}

Procedure TfsSrcBaseTable.btCommitBLOBMgr;
Var
  anInx: Longint;
Begin
  For anInx := 0 To pred(FileCount) Do
    If Files[anInx].fiBLOBrscMgr <> Nil Then
      Files[anInx].fiBLOBrscMgr.Commit;
End;
{End !!.03}
{--------}

Function XorEncode(Const Key, Source: String): String;
Var
  I: Integer;
  C: Byte;
Begin
  Result := '';
  For I := 1 To Length(Source) Do
    Begin
      If Length(Key) > 0 Then
        C := Byte(Key[1 + ((I - 1) Mod Length(Key))]) Xor Byte(Source[I])
      Else
        C := Byte(Source[I]);
      Result := Result + AnsiLowerCase(IntToHex(C, 2));
    End;
End;

Function XorDecode(Const Key, Source: String): String;
Var
  I: Integer;
  C: Char;
Begin
  Result := '';
  For I := 0 To Length(Source) Div 2 - 1 Do
    Begin
      C := Chr(StrToIntDef('$' + Copy(Source, (I * 2) + 1, 2), Ord(' ')));
      If Length(Key) > 0 Then
        C := Chr(Byte(Key[1 + (I Mod Length(Key))]) Xor Byte(C));
      Result := Result + C;
    End;
End;

Procedure TfsSrcBaseTable.btCreateFile(aFileInx: Integer;
  aTI: PffTransInfo;
  Const aExtension: TffExtension;
  aForServer: Boolean;
  aAttribs: TffFileAttributes;
  aStore: TfsBaseTempStorage);
Var
  RecLen, i: Integer;
  S1, S2, S3, S4: String;
  BlockSize: Longint;
  FI: PffFileInfo;
  FileHeader: PffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
Begin
  {ASSUMPTION: btFiles.Count has already been set to the correct number
               of files so that the aFileInx'th element of the btFiles
               array can be set
               btFiles[aFileInx] is nil, except for aFileInx=0}

  {create the file inforec (note that the descriptor for file 0, the
   data file, has already been created)}
  If (aFileInx <> 0) Then
    Begin
      Files[aFileInx] := FFAllocFileInfo(FFMakeFullFileName(Folder.Path, BaseName),
        aExtension, btBufMgr);
      With Files[aFileInx]^ Do
        Begin
          fiAttributes := aAttribs;
          fiForServer := aForServer;
          If aForServer Then
            fiEncrypted := btEngine.Configuration.GeneralInfo^.giEncrypt
          Else
            fiEncrypted := btDictionary.IsEncrypted;
          fiTempStore := aStore;
        End;
    End;

  FI := Files[aFileInx];

  { Create the file on disk. }
  RecLen := Dictionary.RecordLength;
  BlockSize := Dictionary.FileBlockSize[aFileInx];
  FFOpenFile(FI, omReadWrite, smExclusive, True, True);
  Try
    {patch up the file's block size for the buffer manager}
    FI^.fiBlockSize := BlockSize;
    FI^.fiBlockSizeK := BlockSize Div 1024; {!!.11}
    FI^.fiLog2BlockSize := FFCalcLog2BlockSize(BlockSize);
    {add a new block for the new header}
    FileHeader := PffBlockHeaderFile(btBufMgr.AddBlock(FI, aTI, 0, aRelMethod, fsoNone));
    {set up the file header information}
    With FileHeader^ Do
      Begin
        bhfSignature := fsc_SigHeaderBlock;
        bhfNextBlock := $FFFFFFFF;
        bhfThisBlock := 0;
        bhfLSN := 0;
        bhfBlockSize := BlockSize;
        If fi^.fiForServer Then
          bhfEncrypted := ord(
            btEngine.Configuration.GeneralInfo^.giEncrypt)
        Else
          bhfEncrypted := ord(Dictionary.IsEncrypted);
        bhfLog2BlockSize := FFCalcLog2BlockSize(BlockSize);
        bhfUsedBlocks := 1; {ie this header block}
        bhfAvailBlocks := 0;
        bhf1stFreeBlock := $FFFFFFFF;
        bhfRecordCount := 0;
        bhfDelRecCount := 0;
        bhf1stDelRec.iLow := $FFFFFFFF;
        bhfLastRec.iLow := $FFFFFFFF;
        bhfRecordLength := RecLen;
        bhfVersionRecord := Dictionary.VersionRecord;
        bhfEngineDeleteType := Dictionary.EngineDeleteType; //edtNotUndelete;//edtUndeleteIfPossible;
        If bhfEngineDeleteType In [edtUndeleteIfPossibleNotBlob, edtUndeleteIfPossibleAndBlob] Then
          bhfRecLenPlusTrailer := bhfRecordLength + SizeOf(Byte) + sizeof(TffInt64) // extra info for delete
        Else If bhfEngineDeleteType In [edtUndeleteFull] Then
          bhfRecLenPlusTrailer := bhfRecordLength + SizeOf(Byte) + sizeof(TffInt64) + sizeof(TffInt64) // extra info for delete
        Else
          bhfRecLenPlusTrailer := bhfRecordLength + SizeOf(Byte);
        If bhfVersionRecord = trUseVersion Then
          bhfRecLenPlusTrailer := bhfRecLenPlusTrailer + Sizeof(Int64); // for recversion
        bhfLastCountRec := 0;
        bhfRecsPerBlock := (BlockSize - fsc_BlockHeaderSizeData) Div bhfRecLenPlusTrailer;
        bhf1stDataBlock := $FFFFFFFF;
        bhfLastDataBlock := $FFFFFFFF;
        bhfBLOBCount := 0;
        bhfDelBLOBHead.iLow := $FFFFFFFF;
        bhfDelBLOBTail.iLow := $FFFFFFFF;
        bhfAutoInc64Value := 0;
        bhfIndexCount := Dictionary.IndexCount;
        bhfHasSeqIndex := 1;
        bhfIndexHeader := fsc_W32NoValue;
        bhfDataDict := 0;
        bhfFieldCount := Dictionary.FieldCount;
        bhfFSVersion := btFolder.NewTableVersion;
        bhfres3 := 0;
        bhfres1 := 0;
        bhfres2 := 0;
        bhfUseSecureProtectRecord := 0;
        bhfTableFlags := 0;
        For i := 1 To 92 Do
          bhfReservedC[i] := '0';
        FileHeader^.bhfInfoSec1.bhfPasswdT[1] := 'P';
        FileHeader^.bhfInfoSec1.bhfPasswdT[2] := 'X';
        FileHeader^.bhfInfoSec1.bhfPasswdT[3] := 'P';
        FileHeader^.bhfInfoSec1.bhfPasswdT[4] := 'V';
        FileHeader^.bhfInfoSec1.bhfPasswdT[5] := '1';
        FileHeader^.bhfInfoSec1.bhfPasswdT[6] := '@';
        FileHeader^.bhfInfoSec1.bhfPasswdT[7] := '$';
        FileHeader^.bhfInfoSec1.bhfPasswdT[8] := 'Z';
        FileHeader^.bhfInfoSec2.bhfPasswdT := FileHeader^.bhfInfoSec1.bhfPasswdT;
        FileHeader^.bhfInfoSec3.bhfPasswdT := FileHeader^.bhfInfoSec1.bhfPasswdT;
        FileHeader^.bhfInfoSec4.bhfPasswdT := FileHeader^.bhfInfoSec1.bhfPasswdT;
        S1 := '';
        S3 := '';
        S2 := '';
        S4 := '';
        // empty password passwd
        S1 := inttohex(FSCalcShStrELFHash(DateTimeToStr(Now)), 8);
        S1 := S1 + inttohex(FSCalcShStrELFHash(Inttohex(FileHeader^.bhfRecordLength, 8)), 8);
        S1 := S1 + inttohex(FSCalcShStrELFHash(Inttohex(FileHeader^.bhfFieldCount, 8)), 8);
        S1 := S1 + inttohex(FSCalcShStrELFHash(Inttohex(FileHeader^.bhfIndexCount, 8)), 8);
        S1 := S1 + inttohex(FSCalcShStrELFHash(Inttohex(FileHeader^.bhfBlockSize, 8)), 8);
        S4 := 'ER' + inttohex(FSCalcShStrELFHash(S1), 14);
        S3 := 'GE' + inttohex(FSCalcShStrELFHash(S1), 14);
        S2 := 'EZ' + inttohex(FSCalcShStrELFHash(S1), 14);
        S1 := 'XZ' + inttohex(FSCalcShStrELFHash(S1), 14);

        For i := 1 To 16 Do
          Begin
            FileHeader^.bhfInfoSec1.bhfPasswdTable[i] := inttohex(i, 2)[2];
            FileHeader^.bhfInfoSec1.bhfPasswdTableBlanc[i] := inttohex(i, 2)[2];
            FileHeader^.bhfInfoSec2.bhfPasswdRest[i] := inttohex(i, 2)[2];
            FileHeader^.bhfInfoSec2.bhfPasswdRestBlanc[i] := inttohex(i, 2)[2];
            FileHeader^.bhfInfoSec3.bhfSignatureDBID[i] := inttohex(i, 2)[2];
            FileHeader^.bhfInfoSec3.bhfSignatureDBIDB[i] := inttohex(i, 2)[2];
            FileHeader^.bhfInfoSec4.bhfUserSignature[i] := inttohex(i, 2)[2];
            FileHeader^.bhfInfoSec4.bhfUserSignatureB[i] := inttohex(i, 2)[2];
          End;

        For i := 16 Downto 1 Do
          FileHeader^.bhfInfoSec1.bhfPasswdTable[i] := S1[17 - i];
        For i := 16 Downto 1 Do
          FileHeader^.bhfInfoSec1.bhfPasswdTableBlanc[i] := S1[17 - i];
        DecodeBuffer(FileHeader^.bhfInfoSec1, SizeOf(FileHeader^.bhfInfoSec1), 1188);
        DecodeBuffer(FileHeader^.bhfInfoSec1.bhfPasswdTable, SizeOf(FileHeader^.bhfInfoSec1.bhfPasswdTable), 1588);

        For i := 16 Downto 1 Do
          FileHeader^.bhfInfoSec2.bhfPasswdRest[i] := S2[17 - i];
        For i := 16 Downto 1 Do
          FileHeader^.bhfInfoSec2.bhfPasswdRestBlanc[i] := S2[17 - i];
        DecodeBuffer(FileHeader^.bhfInfoSec2, SizeOf(FileHeader^.bhfInfoSec2), 1189);
        DecodeBuffer(FileHeader^.bhfInfoSec2.bhfPasswdRest, SizeOf(FileHeader^.bhfInfoSec2.bhfPasswdRest), 1589);

        For i := 16 Downto 1 Do
          FileHeader^.bhfInfoSec3.bhfSignatureDBID[i] := S3[17 - i];
        For i := 16 Downto 1 Do
          FileHeader^.bhfInfoSec3.bhfSignatureDBIDB[i] := S3[17 - i];
        DecodeBuffer(FileHeader^.bhfInfoSec3, SizeOf(FileHeader^.bhfInfoSec3), 1128);
        DecodeBuffer(FileHeader^.bhfInfoSec3.bhfSignatureDBID, SizeOf(FileHeader^.bhfInfoSec3.bhfSignatureDBID), 1029);

        For i := 16 Downto 1 Do
          FileHeader^.bhfInfoSec4.bhfUserSignature[i] := S4[17 - i];
        For i := 16 Downto 1 Do
          FileHeader^.bhfInfoSec4.bhfUserSignatureB[i] := S4[17 - i];
        DecodeBuffer(FileHeader^.bhfInfoSec4, SizeOf(FileHeader^.bhfInfoSec4), 1228);
        DecodeBuffer(FileHeader^.bhfInfoSec4.bhfUserSignature, SizeOf(FileHeader^.bhfInfoSec4.bhfUserSignature), 1069);

        Try
          bhOEMCodePage := 0; //GetOEMCP
          bhCodePage := GetACP; // ansi codepage
        Except
          bhCodePage := 0; // is default system
          bhOEMCodePage := 0;
        End;
        Try
          bhLocale := GetUserDefaultLCID;
        Except
          bhLocale := 0;
        End;

        bhfAllocPageCount := 0;
        bhfBeforeEndAllocCount := 0;
        bhfAllocPageType := aptNone;

        For i := 1 To 601 Do
          bhfReserved4[i] := 0;

        bhfLastRecordID := 0;
        bhfAutoInc64StepValue := 1;
        bhfMaxRecords := 0;
      End;
    aRelMethod(PffBlock(FileHeader));
  Except
    aRelMethod(PffBlock(FileHeader));
    btBufMgr.RemoveFile(FI);
    FFCloseFile(FI);
    If (aFileInx <> 0) Then
      Begin
        FFFreeFileInfo(FI);
        btFiles[aFileInx] := Nil;
      End;
    Raise;
  End; {try..except}
End;
{--------}

Procedure TfsSrcBaseTable.btDeleteBLOBsForRecord(aFI: PffFileInfo; aTI: PffTransInfo;
  aData: PffByteArray; Const aRefNr: TffInt64);
Var
  FldInx: Integer;
  FldDesc: PffFieldDescriptor;
  BLOBNr: TffInt64;
  IsNull: boolean;
Begin
  If Dictionary.EngineDeleteType In [edtNotUndelete, edtUndeleteIfPossibleNotBlob] Then
    Begin
      With Dictionary Do
        Begin
          For FldInx := 0 To pred(FieldCount) Do
            Begin
              FldDesc := FieldDescriptor[FldInx];
              If (FldDesc^.fdType >= fstBLOB) And
                (FldDesc^.fdType <= ffcLastBLOBType) Then
                Begin
                  GetRecordField(FldInx, aData, IsNull, @BLOBNr);
                  If (Not IsNull) And (BLOBNr.iLow <> fsc_W32NoValue) Then
                    Begin
                      FFTblDeleteBLOB(Files[BLOBFileNumber], aTI, BLOBNr);
                      If Dictionary.EngineDeleteType In [edtUndeleteIfPossibleNotBlob] Then
                        Begin
                          SetRecordFieldNull(FldInx, aData, True);
                          FFTblUpdateRecord(aFI, aTi, aRefNr, aData, Dictionary, Files[Dictionary.BLOBFileNumber], Self.Files[0].fiFSVersion);
                        End;
                    End;
                End;
            End;
        End;
    End;
End;
{--------}

Function TfsSrcBaseTable.btGetBaseName: TfsTableName;
Begin
  Result := btBaseName^;
End;
{--------}

Function TfsSrcBaseTable.btGetCursorList: TfsSrcCursorList;
Begin
  Result := btCursorList;
End;
{--------}

Function TfsSrcBaseTable.btGetDictionary: TFSInfoServerDict;
Begin
  Result := btDictionary;
End;
{--------}

Function TfsSrcBaseTable.btGetFile(Inx: Integer): PffFileInfo;
Begin
  If (0 <= Inx) And (Inx < btFiles.Count) Then
    Result := PffFileInfo(btFiles[Inx])
  Else
    Result := Nil;
End;
{--------}

Function TfsSrcBaseTable.btGetFileCount: Integer;
Begin
  Result := btFiles.Count;
End;
{--------}

Function TfsSrcBaseTable.btGetFolder: TfsSrcFolder;
Begin
  Result := btFolder;
End;
{--------}

Procedure TfsSrcBaseTable.btInformCursors(aSrcCursorID: TffCursorID;
  aOp: TfsRecOp;
  aRefNr: TffInt64;
  aIndexID: Integer);
Var
  Inx: Integer;
  Cursor, SrcCursor: TfsSrBaseCursor;
Begin
  SrcCursor := TfsSrBaseCursor(aSrcCursorID);
  CursorList.BeginRead;
  Try
    For Inx := 0 To pred(CursorList.CursorCount) Do
      Begin
        Cursor := CursorList[ftFromIndex, Inx];
        { Is the cursor within the context of our transaction? }
        If (Cursor.Database = SrcCursor.Database) And
          (Cursor.CursorID <> aSrcCursorID) Then
          Cursor.bcRecordUpdated(aOp, aRefNr, aIndexID);
      End;
  Finally
    CursorList.EndRead;
  End;
End;
{--------}

Function TfsSrcBaseTable.btGetOpenIntents: Longint;
Begin
  Result := btOpenIntents;
End;
{Begin !!.03}
{--------}

Procedure TfsSrcBaseTable.btRollbackBLOBMgr; {!!.05 - Rewritten}
Begin
  Files[btDictionary.BLOBFileNumber].fiBLOBrscMgr.RollBack;
End; {!!.05 - End rewritten}
{End !!.03}
{--------}

Procedure TfsSrcBaseTable.btSetFile(Inx: Integer; FI: PffFileInfo);
Begin
  btFiles[Inx] := FI;
End;
{--------}

Procedure TfsSrcBaseTable.btSetFileCount(FC: Integer);
Begin
  If (FC <> btFiles.Count) Then
    btFiles.Count := FC;
End;
{--------}

Procedure TfsSrcBaseTable.btTableUpdated(aDatabaseID: TffDatabaseID);
Var
  Inx: Integer;
  Cursor: TfsSrBaseCursor;
  Database: TfsSrcDatabase;
Begin
  { The purpose of this routine is to invalidate the key path of any
    other cursors attached to this table. We do this because an operation
    may have caused a Structural Modification Operation (SMO) in the index
    used by the cursor and the key path is no longer valid.

    This method is thread-safe for the following reasons:

    1. A server thread committing a transaction must gain write access to the
       table being modified. No other threads will be performing any read or
       write operations on that table until the transaction has committed.

    2. This routine attempts to activate a cursor if the cursor belongs to
       another client. If a thread is in the middle of an operation pertaining
       to the cursor's client (e.g., RecordGetNext) then this routine will not
       be able to update the cursor until the other thread has finished, and
       vice versa.

    Future: We could get rid of this if the index structure was such that all
    keys were in leaf pages. Then the cursor could just check the LSN of its
    current leaf page to see if it should reset its key path. }
  Database := TfsSrcDatabase(aDatabaseID);
  CursorList.BeginRead;
  Try
    For Inx := 0 To pred(CursorList.CursorCount) Do
      Begin
        Cursor := CursorList[ftFromIndex, Inx];
        { Is this cursor attached to another database? }
        If (Cursor.Database <> Database) Then
          Begin
            {Begin !!.06}
            Cursor.bcInfoLock.Lock;
            Try
              FFInitKeyPath(Cursor.bcInfo.KeyPath);
            Finally
              Cursor.bcInfoLock.Unlock;
            End;
            {End !!.06}
          End;
      End;
  Finally
    CursorList.EndRead;
  End;
End;
{--------}

Procedure TfsSrcBaseTable.btUpdateAutoInc(aTI: PffTransInfo; aData: PffByteArray);
Var
  FldInx: Integer;
  AutoIncValue: Longint;
  AutoIncValue64: Int64;
  IsNull: boolean;
Begin
  With Dictionary Do
    Begin
      If HasAutoIncField(FldInx) Then
        Begin
          If FieldDescriptor^[FldInx]^.fdType In [fstAutoInc64] Then
            Begin
              GetRecordField(FldInx, aData, IsNull, @AutoIncValue64);
              If IsNull Or (AutoIncValue64 = 0) Then
                Begin
                  If IsNull Or (AutoIncValue64 = 0) Then
                    Begin
                      AutoIncValue64 := FFTblNextAutoInc(Files[0], aTI);
                      SetRecordField(FldInx, aData, @AutoIncValue64);
                    End;
                End;
            End
          Else
            Begin
              GetRecordField(FldInx, aData, IsNull, @AutoIncValue);
              If IsNull Or (AutoIncValue = 0) Then
                Begin
                  AutoIncValue := FFTblNextAutoInc(Files[0], aTI);
                  SetRecordField(FldInx, aData, @AutoIncValue);
                End;
            End;
        End;
    End;
End;

Function TfsSrcBaseTable.btNextAutoInc(aTI: PffTransInfo): Int64;
Begin
  Result := FFTblNextAutoInc(Files[0], aTI);
End;

{--------}

Procedure TfsSrcBaseTable.CloseFiles(commitChanges: boolean; aTI: PffTransInfo);
Var
  FileInx: Integer;
  TempFile: PffFileInfo;
Begin
  For FileInx := 0 To pred(FileCount) Do
    Begin
      TempFile := Files[FileInx];
      If (TempFile <> Nil) Then
        Begin
          If FFFileIsOpen(TempFile) Then
            Begin
              If commitChanges Then
                TempFile^.fiBufMgr.CommitFileChanges(TempFile, aTI^.tirTrans);
              FFCloseFile(TempFile);
            End;
          TempFile^.fiBufMgr.RemoveFile(TempFile);
          FFFreeFileInfo(TempFile);
        End;
    End;
End;
{--------}

Procedure TfsSrcBaseTable.CommitChanges(aTI: PffTransInfo);
Var
  FileInx: Integer;
  TempFile: PffFileInfo;
Begin
  For FileInx := 0 To pred(FileCount) Do
    Begin
      TempFile := Files[FileInx];
      If (TempFile <> Nil) And
        FFFileIsOpen(TempFile) Then
        TempFile^.fiBufMgr.CommitFileChanges(TempFile, aTI^.tirTrans);
    End;
End;
{--------}

Procedure TfsSrcBaseTable.DeregisterOpenIntent;
Begin
  If btOpenIntents > 0 Then
    dec(btOpenIntents);
End;
{--------}

Function TfsSrcBaseTable.EmptyFiles(aTI: PffTransInfo): TffResult;
Var
  aAttribs: TffFileAttributes;
  aStore: TfsBaseTempStorage;
  TempDict: TFSInfoServerDict;
  Mx: Longint;
  Ps, Ps1, ps2, ps3: Longword;
Begin
  Result := DBIERR_NONE;
  { Preserve the existing attributes. Assume that each file managed by the
    table has the same set of attributes. }
  aAttribs := Files[0]^.fiAttributes;
  aStore := TfsBaseTempStorage(Files[0]^.fiTempStore);
  TempDict := TFSInfoServerDict.Create(Dictionary.BlockSize);
  Mx := FFTblReadMaxRecords(Files[0], aTI);
  Ps := FFTblReadTablePassword(Files[0], aTI);
  Ps3 := FFTblReadTablePasswordRest(Files[0], aTI);
  Ps1 := FFTblReadTableDBID(Files[0], aTI);
  Ps2 := FFTblReadTableFlags(Files[0], aTI);

  Try
    TempDict.Assign(Dictionary);
    { Flush out any changes related to this file.  They will be eliminated
      when we rebuild the file but we want to make sure they are no longer
      part of an explicit transaction. }
    CloseFiles(True, aTI);
    BuildFiles(aTI, False, TempDict, aAttribs, aStore);

    { Is this a temporary file? }
    If Not (fffaTemporary In aAttribs) Then
      Begin
        { No. Commit the changes to the file. }
        CloseFiles(True, aTI);
        OpenFiles(aTI, False, aAttribs);
        FFTblSetTablePassword(Files[0], aTI, ps);
        FFTblSetTablePasswordRest(Files[0], aTI, ps3);
        FFTblSetMaxRecords(Files[0], aTI, mx);
        FFTblSetTableFlags(Files[0], aTI, ps2);
        FFTblSetTableDBID(Files[0], aTI, ps1);

      End;
  Finally
    TempDict.Free;
  End;
End;
{--------}

Procedure TfsSrcBaseTable.EndCommit(aDatabaseID: TffDatabaseID);
Begin
  btTableUpdated(aDatabaseID);
  btPortal.EndWrite;
End;
{--------}

Procedure TfsSrcBaseTable.EndRead;
Begin
  btPortal.EndRead;
End;
{--------}

Procedure TfsSrcBaseTable.GetNextRecordSeq(aTI: PffTransInfo;
  Var aRefNr: TffInt64;
  aData: PffByteArray;
  aUndelete: boolean;
  OnlyDeleted: boolean;
  Var aFlag: Byte);
Begin
  FFTblReadNextRecord(Files[0], aTI, aRefNr, aRefNr, aData, aUndelete, OnlyDeleted, aFlag);
End;
{--------}

Procedure TfsSrcBaseTable.GetPrevRecordSeq(aTI: PffTransInfo;
  Var aRefNr: TffInt64;
  aData: PffByteArray;
  Var aFlag: Byte);
Begin
  FFTblReadPrevRecord(Files[0], aTI, aRefNr, aRefNr, aData, aFlag);
End;
{--------}

Function TfsSrcBaseTable.GetRecord(aTI: PffTransInfo;
  Const aDatabaseID: TffDatabaseID; {!!.10}
  Const aCursorID: TffCursorID; {!!.10}
  aRefNr: TffInt64;
  aData: PffByteArray;
  Const aLockType: TfsSrcLockType; {!!.10}
  Const aUserLockType: TfsUserRecLocking;
  Const aLockObtained: boolean; {!!.02} {!!.10}
  Const aConditional: Boolean;
  Var aFlag: Byte): TffResult; {!!.02} {!!.10}
Begin

  Result := DBIERR_NONE;

  { Acquire a lock on the record. }
  If (Not aLockObtained) Then
    Begin
      If aUserLockType <> tluDatabase Then
        FSUserAcquireRecordLock(Files[0], aTI, aRefNr, aLockType, aDatabaseID,
          aCursorID, aConditional, aUserLockType, False)
      Else
        FSAcquireRecordLock(Files[0], aTI, aRefNr, aLockType, aDatabaseID,
          aCursorID, aConditional, False);
    End;

  Try
    If Assigned(aData) Then
      FFTblReadRecord(Files[0], aTI, aRefNr, aData, Self.Files[0].fiFSVersion, aFlag);
  Except
    If aLockType <> ffsltNone Then
      FFRelRecordLock(Files[0], aTI, aRefNr, aDatabaseID, aCursorID);
    Raise;
  End;
End;
{--------}

Procedure TfsSrcBaseTable.GetRecordLock(aTI: PffTransInfo;
  Const aDatabaseID: TffDatabaseID; {!!.10}
  Const aCursorID: TffCursorID; {!!.10}
  Const aRefNr: TffInt64; {!!.10}
  Const aLockType: TfsSrcLockType;
  Const aUserLockType: TfsUserRecLocking;
  Const aUser: Boolean); {!!.10}
Begin
  { Acquire a lock on the record. }
  If aUserLockType <> tluDatabase Then
    FSUserAcquireRecordLock(Files[0], aTI, aRefNr, aLockType, aDatabaseID,
      aCursorID, False, aUserLockType, aUser)
  Else
    FSAcquireRecordLock(Files[0], aTI, aRefNr, aLockType, aDatabaseID,
      aCursorID, False, aUser);
End;
{Begin !!.10}
{--------}

Procedure TfsSrcBaseTable.GetRecordNoLock(aTI: PffTransInfo;
  aRefNr: TffInt64;
  aData: PffByteArray);
Var
  af: Byte;
Begin
  If Assigned(aData) Then
    FFTblReadRecord(Files[0], aTI, aRefNr, aData, Self.Files[0].fiFSVersion, af);
End;
{End !!.10}
{--------}

Function TfsSrcBaseTable.HasClientLock(Const aCursorID: TffCursorID): boolean;
Begin
  Result := Folder.LockMgr.HasClientLock(btClientLocks, aCursorID);
End;
{--------}

Function TfsSrcBaseTable.HasLock(Const aCursorID: TffCursorID;
  Const aLockType: TfsSrcLockType): boolean;
Begin
  If (aLockType = ffsltNone) Then
    Result := True
  Else
    Result := Folder.LockMgr.IsTableLockedBy(TableID, aCursorID, aLockType);
End;
{Begin !!.06}
{--------}

Function TfsSrcBaseTable.HasRecordLocks: Boolean;
Var
  RecordLocks: TfsThreadHash64;
Begin
  RecordLocks := PffFileInfo(btFiles[0])^.fiRecordLocks;
  Result := (RecordLocks <> Nil) And (RecordLocks.Count > 0);
End;

Function TfsSrcBaseTable.RecordCountLocks: TffWord32;
Var
  RecordLocks: TfsThreadHash64;
Begin
  Result := 0;
  RecordLocks := PffFileInfo(btFiles[0])^.fiRecordLocks;
  If (RecordLocks <> Nil) Then
    Result := RecordLocks.Count;
End;

Procedure TfsSrcBaseTable.AssignRecordCountLocks(aList: TList);
Var
  RecordLocks: TfsThreadHash64;

  Procedure Add;
  Var
    i: Integer;
    Node: TfsHashNode;
    Temp: TfsHashNode;
  Begin
    RecordLocks.BeginRead;
    Try
      For i := 0 To pred(RecordLocks.Table.Count) Do
        Begin
          Node := TfsHashNode(RecordLocks.Table[i]);
          While assigned(Node) Do
            Begin
              Temp := Node;
              Node := Node.fhNext;
              //a:= tffInt64(Temp.fhKey^); // test
              aList.Add(Temp.fhKey);
            End;
        End;
    Finally
      RecordLocks.EndRead;
    End;
  End;
Begin
  aList.Clear;
  RecordLocks := PffFileInfo(btFiles[0])^.fiRecordLocks;
  If (RecordLocks <> Nil) Then
    If RecordLocks.Count > 0 Then
      Add;
End;

{--------}

Function TfsSrcBaseTable.IsContentLockedBy(aTrans: TfsSrcTransaction): boolean;
Begin
  Result := Folder.LockMgr.IsContentLockedBy(btContentLocks, aTrans);
End;
{--------}

Function TfsSrcBaseTable.IsRecordLocked(aTI: PffTransInfo;
  aCursorID: TffCursorID;
  aRefNr: TffInt64;
  aLockType: TfsSrcLockType): Boolean;
Begin
  Result := Folder.LockMgr.IsRecordLocked(aRefNr, Files[0]);
End;

Function TfsSrcBaseTable.WhoRecordLocked(aTI: PffTransInfo;
  aCursorID: TffCursorID;
  aRefNr: TffInt64;
  aLockType: TfsSrcLockType): String;
Begin
  Result := Folder.LockMgr.WhoRecordLocked(aRefNr, Files[0]);
End;
{--------}

Function TfsSrcBaseTable.IsServerTable: boolean;
Begin
  Result := btForServer;
End;
{Begin !!.03}
{--------}

Procedure TfsSrcBaseTable.ListBLOBFreeSpace(aTI: PffTransInfo;
  Const aInMemory: Boolean;
  aStream: TStream);
Var
  anInx: Longint;
  aStr: String;
Begin
  For anInx := 0 To pred(FileCount) Do
    If Files[anInx].fiBLOBrscMgr <> Nil Then
      Begin
        aStr := Files[anInx].fiName^ + #13#10;
        If anInx > 0 Then
          aStr := #13#10 + aStr;
        aStream.Write(aStr[1], Length(aStr));
        Files[anInx].fiBLOBrscMgr.ListFreeSpace(Files[anInx], aTI, aInMemory,
          aStream);
      End;
End;
{End !!.03}
{--------}

Procedure TfsSrcBaseTable.OpenFiles(aTI: PffTransInfo; aForServer: boolean;
  aAttribs: TffFileAttributes);
Var
  FileInx: Integer;
  FileCnt: Integer;
  DataFile: PffFileInfo;
  Page: PffBlock;
  TempFile: PffFileInfo;
  State: Integer;
  aRelMethod: TffReleaseMethod;
Begin
  State := 0;
  FileCnt := 0;
  TempFile := Nil;
  Try
    { Allocate the first file inforec: it'll be for the data file. }
    btFiles.Count := 1;
    btFiles[0] := FFAllocFileInfo(FFMakeFullFileName(Folder.Path, BaseName),
      fsc_ExtForData,
      btBufMgr);
    State := 25;
    PffFileInfo(btFiles[0])^.fiAttributes := aAttribs;
    PffFileInfo(btFiles[0])^.fiForServer := aForServer;

    { Open up the data file. }
    DataFile := Files[0];
    FFOpenFile(DataFile, omReadWrite, smExclusive, aForServer, False);
    State := 50;

    { Make sure it's a proper FS file: try to load the header record,
      make it fixed (this'll also check the encryption level). }
    Page := btBufMgr.AddFile(DataFile, aTI, False, aRelMethod);
    {Begin !!.11}
        { Adjust in-memory version if overridden via folder. }
    If btFolder.ExistingTableVersion <> 0 Then
      Files[0].fiFSVersion := btFolder.ExistingTableVersion;
    {End !!.11}
    aRelMethod(Page);

    { Read the data dictionary. }
    Dictionary.ReadFromFile(DataFile, aTI);
    Dictionary.BindIndexHelpers;

    { Set up the count of files in the Files array. }
    FileCnt := Dictionary.FileCount;
    FileCount := FileCnt;
    For FileInx := 1 To pred(FileCnt) Do
      Begin
        Files[FileInx] := Nil;
      End;
    { Now read through the Dictionary's file list and allocate the
      file inforecs, obviously don't do file 0 since it's been done
      already. }
    State := 100;
    For FileInx := 1 To pred(FileCnt) Do
      Begin
        Files[FileInx] := FFAllocFileInfo(FFMakeFullFileName(Folder.Path, BaseName),
          Dictionary.FileExt[FileInx],
          btBufMgr);
        PffFileInfo(btFiles[FileInx])^.fiAttributes := aAttribs;
        PffFileInfo(btFiles[FileInx])^.fiForServer := aForServer;
      End;

    { Now open up all the new files, ie excepting file 0 which is
      already open (it was opened to read the data dictionary); read
      the header record from each file as well, as a security check
      to see whether the file is in FF format. }
    State := 200;
    For FileInx := 1 To pred(FileCnt) Do
      Begin
        TempFile := Files[FileInx];
        FFOpenFile(TempFile,
          DataFile^.fiOpenMode, DataFile^.fiShareMode,
          DataFile^.fiWriteThru, False);
        Page := btBufMgr.AddFile(TempFile, aTI, False, aRelMethod);
        aRelMethod(Page);
      End;
    {Begin !!.11}
    Files[Dictionary.BLOBFileNumber].fiBLOBrscMgr :=
      TffBaseBLOBResourceMgr.GetMgr(Files[Dictionary.BLOBFileNumber]);
    btBLOBEngine := TffBaseBLOBEngine.GetEngine(Files[Dictionary.BLOBFileNumber]);
    {End !!.11}
    State := 300;
    btForServer := aForServer;
  Except
    If (State = 300) Then
      {BLOB Resource Mgr created}
      Files[Dictionary.BLOBFileNumber].fiBLOBrscMgr.Free;
    If (State >= 200) Then
      Begin
        {some files are open, all file inforecs are created}
        For FileInx := 1 To pred(FileCnt) Do
          Begin
            TempFile := Files[FileInx];
            If FFFileIsOpen(TempFile) Then
              FFCloseFile(TempFile);
            TempFile^.fiBufMgr.RemoveFile(TempFile);
          End;
      End;
    If (State >= 100) Then
      Begin
        {at least some of the inforecs have been created}
        For FileInx := 1 To pred(FileCnt) Do
          Begin
            TempFile := Files[FileInx];
            FFFreeFileInfo(TempFile);
          End;
      End;
    If (State >= 50) Then
      Begin
        {at least the data file is open}
        TempFile := Files[0];
        If FFFileIsOpen(TempFile) Then
          FFCloseFile(TempFile);
        TempFile^.fiBufMgr.RemoveFile(TempFile);
      End;
    If (State >= 25) Then
      Begin
        {at least the data file inforec has been allocated}
        TempFile := Files[0];
        FFFreeFileInfo(TempFile);
      End;
    If (State >= 0) Then
      Begin
        {empty the files list}
        FileCount := 0;
      End;
    Raise;
  End; {try..except}
End;
{--------}

Procedure TfsSrcBaseTable.RegisterOpenIntent;
Begin
  inc(btOpenIntents);
End;
{--------}

Procedure TfsSrcBaseTable.RelClientLock(aCursorID: Longint; aRemoveAll: Boolean);
Begin
  If (Not aRemoveAll) Then {!!.03}
    Folder.LockMgr.ReleaseClientLock(btClientLocks, aCursorID)
  Else
    Folder.LockMgr.ReleaseClientLockAll(btClientLocks, aCursorID);
End;
{--------}

Procedure TfsSrcBaseTable.RelContentLock(aTrans: TfsSrcTransaction);
Begin
  Folder.LockMgr.ReleaseContentLock(btContentLocks, aTrans);
End;
{--------}

Procedure TfsSrcBaseTable.RelLock(Const aCursorID: TffCursorID;
  Const aAllLocks: Boolean);
Begin
  If aAllLocks Then
    Folder.LockMgr.ReleaseTableLockAll(TableID, aCursorID)
  Else
    Folder.LockMgr.ReleaseTableLock(TableID, aCursorID);
End;
{Begin !!.10}
{--------}

Procedure TfsSrcBaseTable.RelaxRecordLock(aTI: PffTransInfo;
  aCursorID: TffCursorID;
  aRefNr: TffInt64);
Begin
  FFRelaxRecordLock(Files[0], aTI, aCursorID, aRefNr);
End;
{End !!.10}
{--------}

Procedure TfsSrcBaseTable.RelRecordLock(aTI: PffTransInfo;
  aDatabaseID: TffDatabaseID; {!!.10}
  aCursorID: TffCursorID;
  aRefNr: TffInt64);
Begin
  FFRelRecordLock(Files[0], aTI, aRefNr, aDatabaseID, aCursorID); {!!.10}
End;
{--------}

Procedure TfsSrcBaseTable.RemoveLocksForCursor(Const aDatabaseID: TffDatabaseID; {!!.10}
  Const aCursorID: TffCursorID;
  Const aRefNr: TffInt64;
  aTI: PffTransInfo);
Begin
  { In FF 1, if aRefNr = 0 then all of a cursor's locks were
    released. We do not have such a need for FF2 since the only time a cursor
    has record locks is if it is in a transaction and has acquired exclusive
    locks on one or more records.  When the transaction is committed or rolled
    back, the record locks are released. }
  FFRelRecordLock(Files[0], aTI, aRefNr, aDatabaseID, aCursorID); {!!.10}
End;
{--------}

Procedure TfsSrcBaseTable.SetAttributes(Const fileAttribs: TffFileAttributes);
Var
  Index: Longint;
Begin
  For Index := 0 To pred(FileCount) Do
    Files[Index].fiAttributes := fileAttribs;
End;
{--------}

Procedure TfsSrcBaseTable.SetExclOwner(Const aCursorID: TffCursorID);
Var
  Index: Longint;
Begin
  For Index := 0 To pred(FileCount) Do
    Files[Index].fiExclOwner := aCursorId;
End;
{====================================================================}

{===TfsSrcTable=======================================================}

Constructor TfsSrcTable.Create(anEngine: TFSServer;
  Const aBaseName: TfsTableName;
  aFolder: TfsSrcFolder;
  aBufMgr: TfsBufferManager;
  Const aOpenMode: TffOpenMode);
Begin
  Inherited Create(anEngine, aBaseName, aFolder, aBufMgr, aOpenMode);
  {create the user routine arrays}
  stUserBuildKey := TfsVCLList.Create;
  stUserCompareKey := TfsVCLList.Create;
  {miscellaneous}
  FreeOnRemove := True;
  //  stUseInternalRollback := False;                                    {!!.03}
End;
{--------}

Destructor TfsSrcTable.Destroy;
Begin
  stUserCompareKey.Free;
  stUserBuildKey.Free;
  RemoveDynamicLinks;
  Inherited Destroy;
End;
{--------}

Procedure TfsSrcTable.AddIndex(Const aIndexDesc: TffIndexDescriptor;
  aTI: PffTransInfo);
Var
  IndexInx: Integer;
Begin
  {assumption: aIndexDesc has been validated}
  IndexInx := Dictionary.IndexCount;
  With aIndexDesc Do
    Dictionary.AddIndex(idName, idDesc, idFile, idCount, idFields, idFieldsAscDesc, idFieldsCase,
      idFieldsSize, idFieldsNullTop, idFieldsFlags,
      idFieldIHlprs, idDups);
  Dictionary.BindIndexHelpers;
  Dictionary.WriteToFile(Files[0], aTI);
  FFTblAddIndex(Files[aIndexDesc.idFile],
    aTI,
    IndexInx,
    Dictionary.IndexKeyLength[IndexInx],
    aIndexDesc.idDups,
    IndexInx = 0);
End;
{--------}

Procedure TfsSrcTable.BuildFiles(aTI: PffTransInfo;
  aForServer: boolean;
  aDictionary: TFSInfoDict;
  aAttribs: TffFileAttributes;
  aStore: TfsBaseTempStorage);
Var
  FileInx: Integer;
  IndexInx: Integer;
  DataFile: PffFileInfo;
  FileCnt: Integer; {dup for speed}
Begin
  {allocate the first file inforec now: it'll be for the data file}
  btFiles.Count := 1;
  btFiles[0] := FFAllocFileInfo(FFMakeFullFileName(Folder.Path, BaseName),
    fsc_ExtForData, btBufMgr);
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
      fiRecordLength := aDictionary.RecordLength;
      fiTempStore := aStore;
    End;

  { Validate the dictionary. }
  aDictionary.CheckValid;

  { Assimilate the dictionary. }
  btDictionary.ForceOffReadOnly;
  btDictionary.Assign(aDictionary);
  btDictionary.BindIndexHelpers;

  { Get the file count for this table (for speed reasons, etc). }
  FileCnt := Dictionary.FileCount;
  FileCount := FileCnt;

  { Get the data file for speed reasons. }
  DataFile := Files[0];

  { Build all the files and assume that all will contain indexes. }
  For FileInx := 0 To pred(FileCnt) Do
    Begin
      btCreateFile(FileInx, aTI, btDictionary.FileExt[FileInx], aForServer,
        aAttribs, aStore);
      FFTblPrepareIndexes(btFiles[FileInx], aTI);
    End;

  { Write the dictionary. }
  Dictionary.WriteToFile(DataFile, aTI);

  { Add the indexes to their associated index files. }
  With btDictionary Do
    Begin
      For IndexInx := 0 To pred(IndexCount) Do
        Begin
          FFTblAddIndex(Files[IndexFileNumber[IndexInx]],
            aTI,
            IndexInx,
            IndexKeyLength[IndexInx],
            IndexAllowDups[IndexInx],
            IndexInx = 0);
        End;
    End;

  {Begin !!.11}
  Files[btDictionary.BLOBFileNumber].fiBLOBrscMgr :=
    TffBaseBLOBResourceMgr.GetMgr(Files[Dictionary.BLOBFileNumber]);
  btBLOBEngine := TffBaseBLOBEngine.GetEngine(Files[btDictionary.BLOBFileNumber]);
  {End !!.11}
  Files[btDictionary.BLOBFileNumber].fiMaxSegSize :=
    FFCalcMaxBLOBSegSize(Files[btDictionary.BLOBFileNumber]);

End;
{--------}

Function TfsSrcTable.BuildKeyForRecord(aIndexID: Integer;
  aData: PffByteArray;
  aKey: PffByteArray;
  aFieldCount: Integer;
  aPartialLen: Integer): TffResult;
Var
  BuildKey: TffKeyBuildFunc;
  LenKeyToGen: Integer;
Begin
  If (Dictionary.IndexType[aIndexID] = itComposite) Then
    Begin
      Result := stBuildCompositeKey(aIndexID, aData, aKey, aFieldCount, aPartialLen);
    End
  Else {user-defined index}
    Begin
      BuildKey := stGetUserBuildKey(aIndexID);
      If (aFieldCount = 0) And (aPartialLen = 0) Then
        LenKeyToGen := Dictionary.IndexKeyLength[aIndexID]
      Else
        LenKeyToGen := aPartialLen;
      If Not BuildKey(aIndexID, aData, aKey^, LenKeyToGen) Then
        Result := DBIERR_KEYVIOL
      Else
        Result := DBIERR_NONE;
    End;
End;
{--------}

Function TfsSrcTable.CompareKeysForCursor(Var aKID: TffKeyIndexData;
  aKey1: PffByteArray;
  aKey2: PffByteArray): Integer;
Var
  CompareKey: TffKeyCompareFunc;
Begin
  With aKID, kidCompareData^ Do
    Begin
      If (kidIndexType = itComposite) Then
        Begin
          Result := FSKeyCompareComposite(aKey1^, aKey2^, kidCompareData);
        End
      Else If (kidIndexType = itSysRef) Then //If (kidIndex = 0) Then
        Begin
          Result := FFCmpDW(PffWord32(aKey1)^, PffWord32(aKey2)^);
        End
      Else If (kidIndexType = itUdfDll) Then
        Begin
          CompareKey := stGetUserCompareKey(kidIndex);
          Result := CompareKey(aKey1^, aKey2^, kidCompareData);
        End;
    End;
End;
{--------}
{--------}

Procedure TfsSrcTable.DropIndex(aTI: PffTransInfo; aIndexID: Longint);
Var
  i: Integer;
Begin
  Dictionary.RemoveIndex(aIndexID);
  Dictionary.WriteToFile(Files[0], aTI);
  For i := 0 To pred(Dictionary.FileCount) Do
    FFTblDeleteIndex(Files[i], aTI, aIndexID);
End;
{--------}

Function TfsSrcTable.FindKey(Var aKID: TffKeyIndexData;
  Var aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath;
  aAction: TffSearchKeyAction): boolean;
Begin
  Result := FFTblFindKey(aKID, aRefNr, aTI, aKey, aKeyPath, aAction);
End;
{--------}

Function TfsSrcTable.GetNextKey(Var aKID: TffKeyIndexData;
  Var aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath): TffResult;
Begin
  If FFTblNextKey(aKID, aRefNr, aTI, aKey, aKeyPath) Then
    Result := DBIERR_NONE
  Else
    Result := DBIERR_EOF;
End;

Function TfsSrcTable.GetPrevKey(Var aKID: TffKeyIndexData;
  Var aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath): TffResult;
Begin
  If FFTblPrevKey(aKID, aRefNr, aTI, aKey, aKeyPath) Then
    Result := DBIERR_NONE
  Else
    Result := DBIERR_BOF;
End;
{--------}

Function TfsSrcTable.GetNextRecord(aTI: PffTransInfo;
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

  Try
    If FFTblNextKey(aKID, aRefNr, aTI, aKey, aKeyPath) Then
      Begin
        FSAcquireRecordLock(Files[0], aTI, aRefNr, aLockType, {!!.10}
          aDatabaseID, aCursorID, False, False); {!!.10}
        FFTblReadRecord(Files[0], aTI, aRefNr, aData, Self.Files[0].fiFSVersion, aflag);
      End
    Else
      Result := DBIERR_EOF;
  Except
    If aLockType <> ffsltNone Then
      FFRelRecordLock(Files[0], aTI, aRefNr, aDatabaseID, aCursorID); {!!.10}
    Raise;
  End;
End;
{--------}

Function TfsSrcTable.GetPriorRecord(aTI: PffTransInfo;
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

  Try
    If FFTblPrevKey(aKID, aRefNr, aTI, aKey, aKeyPath) Then
      Begin
        FSAcquireRecordLock(Files[0], aTI, aRefNr, aLockType, {!!.10}
          aDatabaseID, aCursorID, False, False); {!!.10}
        FFTblReadRecord(Files[0], aTI, aRefNr, aData, Self.Files[0].fiFSVersion, aflag);
      End
    Else
      Result := DBIERR_BOF;
  Except
    If aLockType <> ffsltNone Then
      FFRelRecordLock(Files[0], aTI, aRefNr, aDatabaseID, aCursorID); {!!.10}
    Raise;
  End;
End;
{--------}

Function TfsSrcTable.DeleteRecord(aTI: PffTransInfo;
  Const aCursorID: TffCursorID;
  Const aRefNr: TffInt64;
  Const aLockObtained: Boolean;
  Var aBTreeChanged: Boolean): TffResult; {!!.05}
Var
  OldData: PffByteArray;
  RecLen: Integer;
  Rflags: Word;
  af: Byte;
Begin
  Rflags := Self.TableFlags;
  If ((Rflags And fsTableDontDeleteRecord) <> 0) Then
    Begin
      Result := DBIERR_NOTSUFFFIELDRIGHTS;
      Exit;
    End;

  RecLen := Dictionary.RecordLength;
  FFGetMem(OldData, RecLen);
  Try
    If (Not aLockObtained) Then
      FSAcquireRecordLock(Files[0], aTI, aRefNr, ffsltExclusive,
        aTI^.tirTrans.DatabaseID, {!!.10}
        aCursorID, False, False); {!!.02} {!!.10}
    { Note: We leave all such locks active until the transaction is committed. }

    FFTblReadRecord(Files[0], aTI, aRefNr, OldData, Self.Files[0].fiFSVersion, af);
    If GetFlags(af, 16) Then
      Result := DBIERR_NOTSUFFFIELDRIGHTS
    Else
      Begin
        Dictionary.FUserName := TfsSrBaseCursor(aCursorID).Client.ClientName;

        Result := stDeleteKeysForRecord(aTI, aRefNr, OldData, aBTreeChanged);

        If (Result <> DBIERR_NONE) Then
          Begin
            If Assigned(OldData) Then
              FFFreeMem(OldData, RecLen);
            Exit;
          End;

        Result := FFTblDeleteRecord(Files[0], aTI, aRefNr, Dictionary, Files[Dictionary.BLOBFileNumber], OldData, True);

        If (Result <> DBIERR_NONE) Then
          Begin
            If Assigned(OldData) Then
              FFFreeMem(OldData, RecLen);
            Exit;
          End;
      End;
  Finally
    If Assigned(OldData) Then
      FFFreeMem(OldData, RecLen);
    btInformCursors(aCursorID, roDelete, aRefNr, 0);
  End; {try..finally}
End;

Function TfsSrcTable.InsertRecord(aTI: PffTransInfo;
  aCursorID: TffCursorID;
  aData: PffByteArray;
  aLockType: TfsSrcLockType;
  Var aNewRefNr: TffInt64;
  aFlag: Byte): TffResult;
Var
  RefNr: TffInt64;
  RMax, RCount: Longword;
  Rflags: Word;
  req: boolean;
Begin
  RefNr.iLow := 0;
  RefNr.iHigh := 0;
  Rflags := Self.TableFlags;
  If ((Rflags And fsTableDontInsertRecord) <> 0) Then
    Begin
      Result := DBIERR_NOTSUFFFIELDRIGHTS;
      Exit;
    End;
  RMax := Self.MaxRecords;
  If RMax > 0 Then
    Begin
      RCount := FFTblReadRecordCount(Files[0], aTI);
      If RCount + 1 > RMax Then
        Begin
          Result := DBIERR_TABLEFULL;
          Exit;
        End;
    End;

  req := Dictionary.CheckRequiredRecordFields(aData);
  If Not req Then
    Result := DBIERR_REQDERR
  Else
    Begin
      Dictionary.FUserName := TfsSrBaseCursor(aCursorID).Client.ClientName;
      {we need to add the default field values}
      If Dictionary.DefaultFieldCount > 0 Then
        Dictionary.SetDefaultFieldUpdateValues(aData, aData);

      { Updating the autoinc value obtains an exclusive lock on block 0 which
        prevents other cursors from inserting the same or additional records
        until we are done. }
      btUpdateAutoInc(aTI, aData);
      FFTblAddRecord(Files[0], aTI, RefNr, aData, Dictionary, Files[Dictionary.BLOBFileNumber], AFlag);
      {initialize result to an invalid value}
      Result := -1;
      Try
        aNewRefNr := RefNr;
        Result := stInsertKeysForRecord(aTI, RefNr, aData);
        If (Result = DBIERR_NONE) Then
          FSAcquireRecordLock(Files[0], aTI, aNewRefNr, aLockType, {!!.10}
            aTI^.tirTrans.DatabaseID, aCursorID, False, False); {!!.10}
      Finally
        { If the insertion of the keys was not successful and we
          are to cleanup after ourselves then remove the inserted record. }
        If (Result <> DBIERR_NONE) Then
          Begin {!!.11}
            FFTblDeleteRecord(Files[0], aTI, RefNr, Dictionary, Files[Dictionary.BLOBFileNumber], Nil, False);
            RefNr.iLow := 0;
            RefNr.iHigh := 0;
          End;
      End;
    End;
End;

Function TfsSrcTable.UnDeleteRecord(aTI: PffTransInfo;
  aCursorID: TffCursorID;
  Var aData: PffByteArray;
  aLockType: TfsSrcLockType;
  Var aNewRefNr: TffInt64): TffResult;
Var
  RefNr: TffInt64;
  RMax, RCount: Longword;
  Rflags: Word;
Begin
  RefNr.iLow := 0;
  RefNr.iHigh := 0;
  Rflags := Self.TableFlags;
  If ((Rflags And fsTableDontInsertRecord) <> 0) Then
    Begin
      Result := DBIERR_NOTSUFFFIELDRIGHTS;
      Exit;
    End;
  RMax := Self.MaxRecords;
  If RMax > 0 Then
    Begin
      RCount := FFTblReadRecordCount(Files[0], aTI);
      If RCount + 1 > RMax Then
        Begin
          Result := DBIERR_TABLEFULL;
          Exit;
        End;
    End;
  If fsUnDeleteRecord(Files[0], aTI, RefNr, aData) Then
    Begin
      Result := DBIERR_NONE;
      Try
        aNewRefNr := RefNr;
        Result := stInsertKeysForRecord(aTI, RefNr, aData);
        If (Result = DBIERR_NONE) Then
          FSAcquireRecordLock(Files[0], aTI, aNewRefNr, aLockType,
            aTI^.tirTrans.DatabaseID, aCursorID, False, False);
      Finally
        If (Result <> DBIERR_NONE) Then
          Begin
            FFTblDeleteRecord(Files[0], aTI, RefNr, Dictionary, Files[Dictionary.BLOBFileNumber], Nil, False);
            RefNr.iLow := 0;
            RefNr.iHigh := 0;
          End;
      End;
    End
  Else
    Result := DBIERR_EOF;
End;

{--------}

Function TfsSrcTable.InsertRecordNoDefault(aTI: PffTransInfo; {!!.10}
  aCursorID: TffCursorID;
  aData: PffByteArray;
  aLockType: TfsSrcLockType;
  Var aNewRefNr: TffInt64;
  aFlag: Byte): TffResult;
Var
  RefNr: TffInt64;
  RMax, RCount: Longword;
  Rflags: Word;
  req: Boolean;
Begin
  RefNr.iLow := 0;
  RefNr.iHigh := 0;

  Rflags := Self.TableFlags;
  If ((Rflags And fsTableDontInsertRecord) <> 0) Then
    Begin
      Result := DBIERR_NOTSUFFFIELDRIGHTS;
      Exit;
    End;

  RMax := Self.MaxRecords;
  If RMax > 0 Then
    Begin
      RCount := FFTblReadRecordCount(Files[0], aTI);
      If RCount + 1 > RMax Then
        Begin
          Result := DBIERR_TABLEFULL;
          Exit;
        End;
    End;
  req := Dictionary.CheckRequiredRecordFields(aData);
  If Not req Then
    Result := DBIERR_REQDERR
  Else
    Begin
      { Updating the autoinc value obtains an exclusive lock on block 0 which
        prevents other cursors from inserting the same or additional records
        until we are done. }
      btUpdateAutoInc(aTI, aData);
      Dictionary.FUserName := TfsSrBaseCursor(aCursorID).Client.ClientName;
      FFTblAddRecord(Files[0], aTI, RefNr, aData, Dictionary, Files[Dictionary.BLOBFileNumber], AFlag);
      {initialize result to an invalid value}
      Result := -1;
      Try
        aNewRefNr := RefNr;
        Result := stInsertKeysForRecord(aTI, RefNr, aData);
        If (Result = DBIERR_NONE) Then
          FSAcquireRecordLock(Files[0], aTI, aNewRefNr, aLockType, {!!.10}
            aTI^.tirTrans.DatabaseID, aCursorID, False, False); {!!.10}
      Finally
        { If the insertion of the keys was not successful and we
          are to cleanup after ourselves then remove the inserted record. }
        If (Result <> DBIERR_NONE) Then
          Begin {!!.11}
            FFTblDeleteRecord(Files[0], aTI, RefNr, Dictionary, Files[Dictionary.BLOBFileNumber], Nil, False);
            RefNr.iLow := 0;
            RefNr.iHigh := 0;
          End;
      End;
    End;
End;
{--------}

Function TfsSrcTable.PutRecord(aTI: PffTransInfo;
  aCursorID: TffCursorID;
  aRefNr: TffInt64;
  aData: PffByteArray;
  aRelLock: boolean; {!!.05}
  aUserLockType: TfsUserRecLocking;
  Var aKeyChanged: Boolean): TffResult; {!!.05}
Var
  OldData: PffByteArray;
  RecLen: Integer;
  Rflags: Word;
  af: Byte;
Begin
  OldData := Nil;
  Rflags := Self.TableFlags;
  If ((Rflags And fsTableDontModifyRecord) <> 0) Then
    Begin
      Result := DBIERR_NOTSUFFFIELDRIGHTS;
      Exit;
    End;

  { Assumption: By the time we have reached this point, the transaction has
    acquired a content lock on the table and we are the only ones who can
    modify the record. }
  If Not Dictionary.CheckRequiredRecordFields(aData) Then
    Begin
      Result := DBIERR_REQDERR;
      Exit;
    End;
  Result := DBIERR_NONE;
  {!!.11}
  RecLen := Dictionary.RecordLength;
  FFGetMem(OldData, RecLen);
  Try
    FFTblReadRecord(Files[0], aTI, aRefNr, OldData, Self.Files[0].fiFSVersion, af);
    If GetFlags(af, 32) Then
      Result := DBIERR_NOTSUFFFIELDRIGHTS
    Else
      Begin
        Dictionary.FUserName := TfsSrBaseCursor(aCursorID).Client.ClientName;
        //xxlock
        { Acquire an exclusive lock. }
        If aUserLockType <> tluDatabase Then
          FSUserAcquireRecordLock(Files[0], aTI, aRefNr, ffsltExclusive,
            aTI^.tirTrans.DatabaseID, aCursorID, False, aUserLockType, False)
        Else
          FSAcquireRecordLock(Files[0], aTI, aRefNr, ffsltExclusive,
            aTI^.tirTrans.DatabaseID, aCursorID, False, False);
        Try

          { There's no need to update index 0, the refnr has not changed. }
          Result := stUpdateKeysForRecord(aCursorID, aTI, aRefNr, aData, OldData, aKeyChanged); {!!.05}

          If (Result <> DBIERR_NONE) Then
            Begin
              If Assigned(OldData) Then
                FFFreeMem(OldData, RecLen);
              FFRelRecordLock(Files[0], aTI, aRefNr, aTI^.tirTrans.DatabaseID, {!!.10}
                aCursorID); {!!.10}
              Exit;
            End;

          Result := FFTblUpdateRecord(Files[0], aTI, aRefNr, aData, Dictionary, Files[Dictionary.BLOBFileNumber], Self.Files[0].fiFSVersion);

          If (Result <> DBIERR_NONE) Then
            Begin
              If Assigned(OldData) Then
                FFFreeMem(OldData, RecLen);
              FFRelRecordLock(Files[0], aTI, aRefNr, aTI^.tirTrans.DatabaseID, {!!.10}
                aCursorID); {!!.10}
              Exit;
            End;
        Except
          If Assigned(OldData) Then
            FFFreeMem(OldData, RecLen);
          FFRelRecordLock(Files[0], aTI, aRefNr, aTI^.tirTrans.DatabaseID, {!!.10}
            aCursorID); {!!.10}
          Raise; {!!.01}
        End;
      End;
  Finally
    If Assigned(OldData) Then
      FFFreeMem(OldData, RecLen);
    FFRelRecordLock(Files[0], aTI, aRefNr, aTI^.tirTrans.DatabaseID, {!!.10}
      aCursorID);
  End;
End;

Function TfsSrcTable.PutSetFlagRecord(aTI: PffTransInfo;
  aCursorID: TffCursorID;
  aRefNr: TffInt64;
  aData: PffByteArray;
  aFlag: Byte; aSet, Use: Boolean): TffResult;
Var
  OldData: PffByteArray;
  RecLen: Integer;
  Rflags: Word;
  af: Byte;
Begin
  OldData := Nil;
  Rflags := Self.TableFlags;
  If ((Rflags And fsTableDontModifyRecord) <> 0) Then
    Begin
      Result := DBIERR_NOTSUFFFIELDRIGHTS;
      Exit;
    End;

  If ((Rflags And fsTableDontProtectRow) <> 0) Then
    Begin
      Result := DBIERR_NOTSUFFFIELDRIGHTS;
      Exit;
    End;

  { Assumption: By the time we have reached this point, the transaction has
    acquired a content lock on the table and we are the only ones who can
    modify the record. }
  If Not Dictionary.CheckRequiredRecordFields(aData) Then
    Begin
      Result := DBIERR_REQDERR;
      Exit;
    End;
  Result := DBIERR_NONE;
  {!!.11}
  RecLen := Dictionary.RecordLength;
  FFGetMem(OldData, RecLen);
  Try
    FFTblReadRecord(Files[0], aTI, aRefNr, OldData, Self.Files[0].fiFSVersion, af);

    FSAcquireRecordLock(Files[0], aTI, aRefNr, ffsltExclusive,
      aTI^.tirTrans.DatabaseID, aCursorID, False, False);
    Try

      FFTblUpdateFlagRecord(Files[0], aTI, aRefNr, aFlag, aSet, Self.Files[0].fiFSVersion);

    Except
      If Assigned(OldData) Then
        FFFreeMem(OldData, RecLen);
      FFRelRecordLock(Files[0], aTI, aRefNr, aTI^.tirTrans.DatabaseID, {!!.10}
        aCursorID); {!!.10}
      Raise; {!!.01}
    End;
  Finally
    If Assigned(OldData) Then
      FFFreeMem(OldData, RecLen);
    FFRelRecordLock(Files[0], aTI, aRefNr, aTI^.tirTrans.DatabaseID, {!!.10}
      aCursorID);
  End;
End;

Procedure TfsSrcTable.MakeKIDForCursor(aIndexID: Integer; Var aKID: TffKeyIndexData);
Begin
  With Dictionary, aKID, kidCompareData^ Do
    Begin
      kidFI := Files[IndexFileNumber[aIndexID]];
      kidIndex := aIndexID;
      If IndexType[aIndexID] = itSysRef Then
        Begin
          kidCompare := FFKeyCompareI64;
          kidIndexType := itSysRef;
        End
      Else If (IndexType[aIndexID] = itComposite) Then
        Begin
          kidCompare := FSKeyCompareComposite;
          kidIndexType := itComposite;
        End
      Else
        Begin
          kidCompare := stGetUserCompareKey(aIndexID);
          kidIndexType := itUdfDll;
        End;
      cdKeyLen := IndexKeyLength[aIndexID];
      cdDict := pointer(Dictionary);
      cdIndex := aIndexID;
      cdFldCnt := 0; {for completeness}
      cdPartLen := 0; {for completeness}
      cdAscend := IndexIsAscending[aIndexID];
      cdNoCase := IndexIsCaseInsensitive[aIndexID];
    End;
End;
{--------}

Procedure TfsSrcTable.RemoveDynamicLinks;
Var
  i: Integer;
  KeyProcItem: TfsKeyProcItem;
  Inx: Integer;
Begin
  {unlink user-defined indexes}
  With btEngine.Configuration Do
    Begin
      For i := 1 To pred(Dictionary.IndexCount) Do
        Begin
          If (Dictionary.IndexType[i] = itUdfDll) Then
            Begin
              Inx := KeyProcList.KeyProcIndex(Folder.Path, BaseName, i);
              If (Inx <> -1) Then
                Begin
                  KeyProcItem := KeyProcList[Inx];
                  KeyProcItem.Unlink;
                End;
            End;
        End;
    End;
End;
{--------}

Procedure TfsSrcTable.ResolveDynamicLinks;
Var
  i: Integer;
  KeyProcItem: TfsKeyProcItem;
  Inx: Integer;
Begin
  stUserBuildKey.Clear;
  stUserCompareKey.Clear;
  {add nil pointers for index 0 as this can never be a user-defined
   index}
  stUserBuildKey.Add(Nil);
  stUserCompareKey.Add(Nil);
  {fill the arrays with data for each index}
  For i := 1 To pred(Dictionary.IndexCount) Do
    Begin
      If (Dictionary.IndexType[i] = itComposite) Then
        Begin
          stUserBuildKey.Add(Nil);
          stUserCompareKey.Add(Nil);
        End
      Else {it's a user-defined index}
        Begin
          With btEngine.Configuration Do
            Begin
              Inx := KeyProcList.KeyProcIndex(Folder.Path, BaseName, i);
              If (Inx <> -1) Then
                Begin
                  KeyProcItem := KeyProcList[Inx];
                  If KeyProcItem.Link Then
                    Begin
                      stUserBuildKey.Add(pointer(@KeyProcItem.BuildKey));
                      stUserCompareKey.Add(pointer(@KeyProcItem.CompareKey));
                    End
                  Else
                    FSRaiseExceptionNoData(EfsServerException,
                      fsStrResServer,
                      fserrResolveTableLinks);
                End
              Else
                FSRaiseExceptionNoData(EfsServerException, fsStrResServer, fserrResolveTableLinks);
            End;
        End;
    End;
End;
{--------}

Function TfsSrcTable.stGetBuiltCompositeKey(aIndexID: Integer;
  aData: PffByteArray;
  aKeyLen: Longint;
  Var aKey: PffByteArray): TffResult;
Var
  WorkKey: PffByteArray;
Begin
  FFGetMem(WorkKey, aKeyLen);
  Try
    Result := stBuildCompositeKey(aIndexID, aData, WorkKey, 0, 0);
    If (Result <> DBIERR_NONE) Then
      FFFreeMem(WorkKey, aKeyLen) {!!.06}
    Else
      aKey := WorkKey;
  Except
    FFFreeMem(WorkKey, aKeyLen);
    Raise;
  End; {try..except}
End;
{--------}

Function TfsSrcTable.stBuildCompositeKey(aIndexID: Integer;
  aData: PffByteArray;
  aKey: PffByteArray;
  aFieldCount: Integer;
  aLastFldLen: Integer): TffResult;
Var
  KeyOffset: Integer;
  IndexDscrptr: PffIndexDescriptor;
  FieldDesc: PffFieldDescriptor;
  FieldNumber: Integer;
  LenToUse, FieldsSize: Integer;
  FldCnt: Integer;
Begin
  Result := DBIERR_NONE;
  KeyOffset := 0;
  IndexDscrptr := Dictionary.IndexDescriptor[aIndexID];
  With IndexDscrptr^ Do
    Begin
      {clear the entire key}
      FSInitKey(aKey, idKeyLen, idCount);
      {calculate the number of complete fields we can use}
      If (aFieldCount = 0) Then
        If (aLastFldLen = 0) Then
          FldCnt := idCount
        Else {partial key}
          FldCnt := 0
      Else If (aLastFldLen = 0) Then
        FldCnt := FFMinI(aFieldCount, idCount)
      Else {partial key}
        FldCnt := FFMinI(aFieldCount, pred(idCount));

      {build using complete fields}
      If (FldCnt > 0) Then
        For FieldNumber := 0 To pred(FldCnt) Do
          Begin
            FieldDesc := Dictionary.FieldDescriptor[idFields[FieldNumber]];
            With FieldDesc^ Do
              Begin
                If Not Dictionary.IsRecordFieldNull(idFields[FieldNumber], aData) Then
                  Begin
                    If (IndexDscrptr^.idFieldsSize[FieldNumber] > 0) And
                      (FieldDesc^.fdType In [fstWideString, fstVarWideString]) Then
                      Move(aData^[fdOffset], aKey^[KeyOffset], (IndexDscrptr^.idFieldsSize[FieldNumber] + 1) * sizeof(WideChar))
                    Else If (IndexDscrptr^.idFieldsSize[FieldNumber] > 0) And
                      (FieldDesc^.fdType In [fstShortString,
                      fstVarNullString, fstNullString]) Then
                      Move(aData^[fdOffset], aKey^[KeyOffset], IndexDscrptr^.idFieldsSize[FieldNumber] + 1)
                    Else
                      Move(aData^[fdOffset], aKey^[KeyOffset], fdLength);
                    FSSetKeyFieldNonNull(aKey, idKeyLen, idCount, FieldNumber);
                  End;
                If (IndexDscrptr^.idFieldsSize[FieldNumber] > 0) And
                  (FieldDesc^.fdType In [fstWideString, fstVarWideString]) Then
                  inc(KeyOffset, (IndexDscrptr^.idFieldsSize[FieldNumber] + 1) * sizeof(WideChar))
                Else If (IndexDscrptr^.idFieldsSize[FieldNumber] > 0) And
                  (FieldDesc^.fdType In [fstShortString,
                  fstVarNullString, fstNullString]) Then
                  inc(KeyOffset, IndexDscrptr^.idFieldsSize[FieldNumber] + 1)
                Else
                  inc(KeyOffset, fdLength);
              End;
          End;

      {add the last partial field if required - must be string}
      If (aLastFldLen <> 0) Then
        Begin
          FieldNumber := idFields[FldCnt];
          FieldsSize := IndexDscrptr^.idFieldsSize[FldCnt];
          If FieldsSize > 0 Then
            Begin
              If aLastFldLen > FieldsSize Then
                aLastFldLen := FieldsSize;
            End;
          If Not Dictionary.IsRecordFieldNull(FieldNumber, aData) Then
            Begin
              FieldDesc := Dictionary.FieldDescriptor[FieldNumber];
              With FieldDesc^ Do
                If (fdType >= fstShortString) Then
                  Begin
                    If (fdType In [fstWideString, fstVarWideString {, fstUnicode}]) Then
                      LenToUse := sizeof(WideChar) * aLastFldLen
                    Else
                      LenToUse := aLastFldLen;
                    {If (fdType = fstShortString) Then
                      Begin
                        Move(aData^[fdOffset], aKey^[KeyOffset], LenToUse + 1);
                        aKey^[KeyOffset] := LenToUse;
                      End
                    Else }
                    Move(aData^[fdOffset], aKey^[KeyOffset], LenToUse);
                    FSSetKeyFieldNonNull(aKey, idKeyLen, idCount, FldCnt);
                  End
                Else
                  Result := DBIERR_INVALIDFLDTYPE;
            End;
        End;
    End;
End;
{--------}

Function TfsSrcTable.stDeleteKeyPrim(aInxFile: Integer;
  aTI: PffTransInfo;
  aRefNr: TffInt64;
  aKey: PffByteArray;
  aCompare: TffKeyCompareFunc;
  aCmpData: PffCompareData;
  Var aBTreeChanged: Boolean) {!!.05}
: Boolean;
Var
  KID: TffKeyIndexData;
Begin
  With KID Do
    Begin
      kidFI := Files[aInxFile];
      kidIndex := aCmpData^.cdIndex;
      kidCompare := aCompare;
      kidCompareData := aCmpData;
    End;
  Result := FFTblDeleteKey(aTI, aKey, aRefNr, KID, aBTreeChanged); {!!.05}
End;
{--------}

Function TfsSrcTable.stDeleteKeysForRecord(aTI: PffTransInfo;
  aRefNr: TffInt64;
  aData: PffByteArray;
  Var aBTreeChanged: Boolean) {!!.05}
: TffResult;
Var
  IndexNumber: Integer;
  IndexDscrptr: PffIndexDescriptor;
  Key: PffByteArray;
  BuildKey: TffKeyBuildFunc;
  Compare: TffKeyCompareFunc;
  CmpData: TffCompareData;
  tmpBtreeChanged: Boolean; {!!.05}
Begin
  Result := DBIERR_NONE;
  With CmpData Do
    Begin
      cdDict := pointer(Dictionary);
      cdIndex := 0;
      cdFldCnt := 0;
      cdPartLen := 0;
      cdAscend := True; {for index 0}
      cdNoCase := True; {for index 0}
    End;
  aBTreeChanged := True; {!!.05}
  With Dictionary Do
    Begin
      If Not stDeleteKeyPrim(0, aTI, aRefNr, PffByteArray(@aRefNr),
        FFKeyCompareI64, @CmpData,
        tmpBTreeChanged) Then
        Begin {!!!.05}
          Result := DBIERR_KEYVIOL;
          Exit;
        End;
      aBTreeChanged := tmpBtreeChanged; {!!.05}
      For IndexNumber := 1 To pred(IndexCount) Do
        Begin
          IndexDscrptr := IndexDescriptor[IndexNumber];
          With IndexDscrptr^ Do
            Begin
              If (idCount <> -1) Then
                Begin {a composite index}
                  CmpData.cdIndex := IndexNumber;
                  CmpData.cdAscend := Boolean(IndexDscrptr^.idFieldsAscDesc[0]);
                  CmpData.cdNoCase := Boolean(IndexDscrptr^.idFieldsCase[0]);
                  CmpData.cdKeyLen := idKeyLen;
                  Result := stGetBuiltCompositeKey(IndexNumber, aData, idKeyLen, Key);
                  If (Result <> DBIERR_NONE) Then
                    Exit;
                  Try
                    If Not stDeleteKeyPrim(idFile, aTI, aRefNr,
                      Key, FSKeyCompareComposite,
                      @CmpData, tmpBTreeChanged) Then
                      Begin {!!.05}
                        Result := DBIERR_KEYVIOL;
                        Exit;
                      End;
                    If tmpBtreeChanged Then {!!.05}
                      aBTreeChanged := True; {!!.05}
                  Finally
                    FFFreeMem(Key, CmpData.cdKeyLen);
                  End; {try..finally}
                End
              Else {a user-defined index}
                Begin
                  CmpData.cdIndex := IndexNumber;
                  CmpData.cdAscend := True;
                  CmpData.cdNoCase := True;
                  CmpData.cdKeyLen := idKeyLen;
                  FFGetMem(Key, CmpData.cdKeyLen);
                  Try
                    BuildKey := stGetUserBuildKey(IndexNumber);
                    Compare := stGetUserCompareKey(IndexNumber);
                    If BuildKey(IndexNumber, aData, Key^, CmpData.cdKeyLen) Then
                      If Not stDeleteKeyPrim(idFile, aTI, aRefNr,
                        Key, Compare, @CmpData,
                        tmpBTreeChanged) Then
                        Begin {!!.05}
                          Result := DBIERR_KEYVIOL;
                          Exit;
                        End;
                    If tmpBtreeChanged Then {!!.05}
                      aBTreeChanged := True; {!!.05}
                  Finally
                    FFFreeMem(Key, CmpData.cdKeyLen);
                  End; {try..finally}
                End;
            End;
        End;
    End;
End;
{--------}

Function TfsSrcTable.stGetUserBuildKey(aIndexID: Integer): TffKeyBuildFunc;
Begin
  If (0 <= aIndexID) And (aIndexID < stUserBuildKey.Count) Then
    @Result := stUserBuildKey[aIndexID]
  Else
    Result := Nil;
End;
{--------}

Function TfsSrcTable.stGetUserCompareKey(aIndexID: Integer): TffKeyCompareFunc;
Begin
  If (0 <= aIndexID) And (aIndexID < stUserCompareKey.Count) Then
    @Result := stUserCompareKey[aIndexID]
  Else
    Result := Nil;
End;
{--------}

Function TfsSrcTable.stInsertKeyPrim(aInxFile: Integer;
  aTI: PffTransInfo;
  aRefNr: TffInt64;
  aKey: PffByteArray;
  aCompare: TffKeyCompareFunc;
  aCmpData: PffCompareData): boolean;
Var
  KID: TffKeyIndexData;
Begin
  With KID Do
    Begin
      kidFI := Files[aInxFile];
      kidIndex := aCmpData^.cdIndex;
      kidCompare := aCompare;
      kidCompareData := aCmpData;
    End;
  Result := FFTblInsertKey(KID, aRefNr, aTI, aKey);
End;
{--------}

Function TfsSrcTable.stInsertKeysForRecord(aTI: PffTransInfo;
  aRefNr: TffInt64;
  aData: PffByteArray): TffResult;
Var
  IndexNumber: Integer;
  IndexDscrptr: PffIndexDescriptor;
  Key: PffByteArray;
  BuildKey: TffKeyBuildFunc;
  Compare: TffKeyCompareFunc;
  CmpData: TffCompareData;
  BTreeChanged: Boolean; {!!.05}

  Procedure RollBackInsertKeys(LastIndexAdded: Integer);
  Var
    IndexNumber: Integer;
    Key2: PffByteArray; {!!.03}
  Begin
    { Remove any keys that were successfully added before the error occurred. }
    With Dictionary Do
      Begin
        For IndexNumber := LastIndexAdded Downto 1 Do
          Begin
            IndexDscrptr := IndexDescriptor[IndexNumber];
            With IndexDscrptr^ Do
              Begin
                If (idCount <> -1) Then
                  Begin {a composite index}
                    CmpData.cdIndex := IndexNumber;
                    CmpData.cdAscend := Boolean(IndexDscrptr^.idFieldsAscDesc[0]);
                    CmpData.cdNoCase := Boolean(IndexDscrptr^.idFieldsCase[0]);
                    CmpData.cdKeyLen := idKeyLen;
                    Result := stGetBuiltCompositeKey(IndexNumber, aData,
                      idKeyLen, Key2); {!!.03}
                    If (Result = DBIERR_NONE) Then
                      Try
                        stDeleteKeyPrim(idFile, aTI, aRefNr, Key2, {!!.03}
                          FSKeyCompareComposite, @CmpData,
                          BTreeChanged); {!!.05}
                      Finally
                        FFFreeMem(Key2, CmpData.cdKeyLen); {!!.03}
                      End; {try..finally}
                  End
                Else {a user-defined index}
                  Begin
                    CmpData.cdIndex := IndexNumber;
                    CmpData.cdAscend := True;
                    CmpData.cdNoCase := True;
                    CmpData.cdKeyLen := idKeyLen;
                    FFGetMem(Key, CmpData.cdKeyLen);
                    Try
                      BuildKey := stGetUserBuildKey(IndexNumber);
                      Compare := stGetUserCompareKey(IndexNumber);
                      If BuildKey(IndexNumber, aData, Key2^, CmpData.cdKeyLen) Then {!!.03}
                        stInsertKeyPrim(idFile, aTI, aRefNr,
                          Key2, Compare, @CmpData); {!!.03}
                    Finally
                      FFFreeMem(Key2, CmpData.cdKeyLen); {!!.03}
                    End; {try..finally}
                  End;
              End;
          End;
        {delete the internal RefNr key}
        With CmpData Do
          Begin
            cdDict := pointer(Dictionary);
            cdIndex := 0;
            cdFldCnt := 0;
            cdPartLen := 0;
            cdAscend := True; {for index 0}
            cdNoCase := True; {for index 0}
          End;
        stDeleteKeyPrim(0, aTI, aRefNr, PffByteArray(@aRefNr),
          FFKeyCompareI64, @CmpData, BTreeChanged); {!!.05}
      End;
  End;

Begin
  Result := DBIERR_NONE;
  With CmpData Do
    Begin
      cdDict := pointer(Dictionary);
      cdIndex := 0;
      cdFldCnt := 0;
      cdPartLen := 0;
      cdAscend := True; {for index 0}
      cdNoCase := True; {for index 0}
    End;
  With Dictionary Do
    Begin
      If Not stInsertKeyPrim(0, aTI, aRefNr, PffByteArray(@aRefNr),
        FFKeyCompareI64, @CmpData) Then
        Begin
          Result := DBIERR_KEYVIOL;
          Exit;
        End;
      For IndexNumber := 1 To pred(IndexCount) Do
        Begin
          IndexDscrptr := IndexDescriptor[IndexNumber];
          With IndexDscrptr^ Do
            Begin
              If (idCount <> -1) Then
                Begin {a composite index}
                  CmpData.cdIndex := IndexNumber;
                  CmpData.cdAscend := Boolean(IndexDscrptr^.idFieldsAscDesc[0]);
                  CmpData.cdNoCase := Boolean(IndexDscrptr^.idFieldsCase[0]);
                  CmpData.cdKeyLen := idKeyLen;
                  Result := stGetBuiltCompositeKey(IndexNumber, aData, idKeyLen, Key);
                  If (Result <> DBIERR_NONE) Then
                    Exit;
                  Try
                    If Not stInsertKeyPrim(idFile, aTI, aRefNr, Key,
                      FSKeyCompareComposite, @CmpData) Then
                      Begin
                        //              if UseInternalRollBack then                            {Deleted !!.11}
                        RollBackInsertKeys(Pred(IndexNumber));
                        Result := DBIERR_KEYVIOL;
                        Exit;
                      End;
                  Finally
                    FFFreeMem(Key, idKeyLen); {!!.06}
                  End; {try..finally}
                End
              Else {a user-defined index}
                Begin
                  CmpData.cdIndex := IndexNumber;
                  CmpData.cdAscend := True;
                  CmpData.cdNoCase := True;
                  CmpData.cdKeyLen := idKeyLen;
                  FFGetMem(Key, CmpData.cdKeyLen);
                  Try
                    BuildKey := stGetUserBuildKey(IndexNumber);
                    Compare := stGetUserCompareKey(IndexNumber);
                    If BuildKey(IndexNumber, aData, Key^, CmpData.cdKeyLen) Then
                      If Not stInsertKeyPrim(idFile, aTI, aRefNr,
                        Key, Compare, @CmpData) Then
                        Begin
                          //                if UseInternalRollBack then                          {Deleted !!.11}
                          RollBackInsertKeys(Pred(IndexNumber));
                          Result := DBIERR_KEYVIOL;
                          Exit;
                        End;
                  Finally
                    FFFreeMem(Key, CmpData.cdKeyLen);
                  End; {try..finally}
                End;
            End;
        End;
    End;
End;
{--------}

Function TfsSrcTable.stUpdateKeysForRecord(aCursorID: TffCursorID;
  aTI: PffTransInfo;
  aRefNr: TffInt64;
  aData,
  aOldData: PffByteArray; {!!.05}
  Var aKeyChanged: Boolean): TffResult; {!!.05}
{Reorganized !!.10}
Var
  IndexNumber: Integer;
  CurrentIndexNum: Integer; {!!.05}
  IndexDscrptr: PffIndexDescriptor;
  OldKey: PffByteArray;
  NewKey: PffByteArray;
  CompResult: Integer;
  BuildKey: TffKeyBuildFunc;
  Compare: TffKeyCompareFunc;
  CmpData: TffCompareData;
  OldKeyBuilt: Boolean;
  NewKeyBuilt: Boolean;
  IndexChanged: Array[1..255] Of Boolean;

  Procedure RollbackUpdateKeys(LastIndexUpdated: Integer;
    DoLastInsertOnly: Boolean);
  Var
    OldKey2: PffByteArray;
    NewKey2: PffByteArray;
    IndexNumber2: Integer;
  Begin
    For IndexNumber2 := LastIndexUpdated Downto 1 Do
      Begin
        IndexDscrptr := Dictionary.IndexDescriptor[IndexNumber2];
        OldKey2 := Nil;
        NewKey2 := Nil;
        CmpData.cdIndex := IndexNumber2;
        CmpData.cdAscend := Boolean(IndexDscrptr^.idFieldsAscDesc[0]);
        CmpData.cdNoCase := Boolean(IndexDscrptr^.idFieldsCase[0]);
        CmpData.cdKeyLen := IndexDscrptr^.idKeyLen;
        With IndexDscrptr^ Do
          Try
            If (idCount <> -1) Then
              Begin {a composite index}
                Result := stGetBuiltCompositeKey(IndexNumber2, aOldData,
                  idKeyLen, OldKey2);
                If (Result = DBIERR_NONE) Then
                  Result := stGetBuiltCompositeKey(IndexNumber2, aData,
                    idKeyLen, NewKey2);
                If (Result <> DBIERR_NONE) Then
                  Continue; {carry on with the next index in case of error}
                CompResult := FSKeyCompareComposite(OldKey2^, NewKey2^, @CmpData);
                If (CompResult <> 0) Then
                  Begin
                    If (Not DoLastInsertOnly) Then
                      {Remove the NewKey on this index}
                      stDeleteKeyPrim(idFile, aTI, aRefNr, NewKey2,
                        FSKeyCompareComposite, @CmpData,
                        IndexChanged[IndexNumber2]); {!!.05}
                    {Restore the OldKey value on this index}
                    stInsertKeyPrim(idFile, aTI, aRefNr, OldKey2,
                      FSKeyCompareComposite, @CmpData);
                  End;
              End
            Else {a user-defined index}
              Begin
                CmpData.cdAscend := True;
                CmpData.cdNoCase := True;
                BuildKey := stGetUserBuildKey(IndexNumber2);
                Compare := stGetUserCompareKey(IndexNumber2);
                FFGetMem(OldKey2, CmpData.cdKeyLen);
                FFGetMem(NewKey2, CmpData.cdKeyLen);
                OldKeyBuilt := BuildKey(IndexNumber2, aOldData,
                  OldKey2^, CmpData.cdKeyLen);
                NewKeyBuilt := BuildKey(IndexNumber2, aData,
                  NewKey2^, CmpData.cdKeyLen);
                If OldKeyBuilt And NewKeyBuilt Then
                  CompResult := Compare(OldKey2^, NewKey2^, @CmpData)
                Else If (OldKeyBuilt Or NewKeyBuilt) Then
                  CompResult := 1 {value doesn't matter so long as it's <> 0}
                Else
                  CompResult := 0;
                If (CompResult <> 0) Then
                  Begin
                    If NewKeyBuilt And (Not DoLastInsertOnly) Then
                      {Remove the NewKey on this index}
                      stDeleteKeyPrim(idFile, aTI, aRefNr,
                        NewKey2, Compare, @CmpData,
                        IndexChanged[IndexNumber2]); {!!.05}
                    If OldKeyBuilt Then
                      {Restore the OldKey value on this index}
                      stInsertKeyPrim(idFile, aTI, aRefNr,
                        OldKey2, Compare, @CmpData);
                  End;
              End; { if }
          Finally
            If Assigned(NewKey2) Then
              FFFreeMem(NewKey2, CmpData.cdKeyLen);
            If Assigned(OldKey2) Then
              FFFreeMem(OldKey2, CmpData.cdKeyLen);
          End; {try..finally}
      End; { for }
  End;
Begin
  Result := DBIERR_NONE;
  CurrentIndexNum := TfsSrBaseCursor(aCursorID).IndexID; {!!.05}
  aKeyChanged := False; {!!.05}
  With CmpData Do
    Begin
      cdDict := pointer(Dictionary);
      cdFldCnt := 0;
      cdPartLen := 0;
    End;
  With Dictionary Do
    Try
      For IndexNumber := 1 To pred(IndexCount) Do
        Begin
          IndexChanged[IndexNumber] := False;
          IndexDscrptr := IndexDescriptor[IndexNumber];
          OldKey := Nil;
          NewKey := Nil;
          CmpData.cdIndex := IndexNumber;
          CmpData.cdAscend := Boolean(IndexDscrptr^.idFieldsAscDesc[0]);
          CmpData.cdNoCase := Boolean(IndexDscrptr^.idFieldsCase[0]);
          CmpData.cdKeyLen := IndexDscrptr^.idKeyLen;
          With IndexDscrptr^ Do
            Try
              If (idCount <> -1) Then
                Begin {a composite index}
                  Result := stGetBuiltCompositeKey(IndexNumber, aOldData, idKeyLen, OldKey);
                  If (Result = DBIERR_NONE) Then
                    Result := stGetBuiltCompositeKey(IndexNumber, aData, idKeyLen, NewKey);
                  If (Result <> DBIERR_NONE) Then
                    Exit;
                  CompResult := FSKeyCompareComposite(OldKey^, NewKey^, @CmpData);
                  If (CompResult <> 0) Then
                    Begin
                      If (IndexNumber = CurrentIndexNum) Then {!!.05}
                        aKeyChanged := True; {!!.05}
                      If Not stDeleteKeyPrim(idFile, aTI, aRefNr, OldKey,
                        FSKeyCompareComposite, @CmpData,
                        IndexChanged[IndexNumber]) Then
                        Begin {!!.05}
                          Result := DBIERR_KEYVIOL;
                          Exit;
                        End;
                      If Not stInsertKeyPrim(idFile, aTI, aRefNr, NewKey,
                        FSKeyCompareComposite, @CmpData) Then
                        Begin
                          //                if UseInternalRollBack then                          {Deleted !!.11}
                          RollbackUpdateKeys(IndexNumber, True);
                          Result := DBIERR_KEYVIOL;
                          Exit;
                        End;
                      IndexChanged[IndexNumber] := True; {!!.06}
                    End;
                End
              Else {a user-defined index}
                Begin
                  CmpData.cdAscend := True;
                  CmpData.cdNoCase := True;
                  BuildKey := stGetUserBuildKey(IndexNumber);
                  Compare := stGetUserCompareKey(IndexNumber);
                  FFGetMem(OldKey, CmpData.cdKeyLen);
                  FFGetMem(NewKey, CmpData.cdKeyLen);
                  OldKeyBuilt := BuildKey(IndexNumber, aOldData, OldKey^, CmpData.cdKeyLen);
                  NewKeyBuilt := BuildKey(IndexNumber, aData, NewKey^, CmpData.cdKeyLen);
                  If OldKeyBuilt And NewKeyBuilt Then
                    CompResult := Compare(OldKey^, NewKey^, @CmpData)
                  Else If (OldKeyBuilt Or NewKeyBuilt) Then
                    CompResult := 1 {value doesn't matter so long as it's <> 0}
                  Else
                    CompResult := 0;
                  If (CompResult <> 0) Then
                    Begin
                      If (IndexNumber = CurrentIndexNum) Then {!!.05}
                        aKeyChanged := True; {!!.05}
                      If OldKeyBuilt Then
                        If Not stDeleteKeyPrim(idFile, aTI, aRefNr,
                          OldKey, Compare, @CmpData,
                          IndexChanged[IndexNumber]) Then
                          Begin {!!.05}
                            //                  if UseInternalRollBack then                        {Deleted !!.11}
                            RollbackUpdateKeys(Pred(IndexNumber), False);
                            Result := DBIERR_KEYVIOL;
                            Exit;
                          End;
                      If NewKeyBuilt Then
                        If Not stInsertKeyPrim(idFile, aTI, aRefNr,
                          NewKey, Compare, @CmpData) Then
                          Begin
                            //                  if UseInternalRollBack then                        {Deleted !!.11}
                            RollbackUpdateKeys(IndexNumber, True);
                            Result := DBIERR_KEYVIOL;
                            Exit;
                          End;
                      IndexChanged[IndexNumber] := True;
                    End;
                End; { if }
            Finally
              If Assigned(NewKey) Then
                FFFreeMem(NewKey, idKeyLen);
              If Assigned(OldKey) Then
                FFFreeMem(OldKey, idKeyLen);
            End; {try..finally}
        End; { for }
    Finally {with dictionary do try...}
      {Inform other cursors at end when we are sure everything worked}
      If Result = DBIERR_NONE Then
        Begin
          For IndexNumber := 1 To pred(IndexCount) Do
            If IndexChanged[IndexNumber] Then
              btInformCursors(aCursorID, roModify, aRefNr, IndexNumber);
        End;
    End; { with dictionary do }
End;
{====================================================================}

{===TfsSrcSystemTable=================================================}

Function TfsSrcSystemTable.IsServerTable: boolean;
Begin
  Result := True;
End;
{====================================================================}

{===TfsSrcTableList===================================================}

Constructor TfsSrcTableList.Create;
Begin
  Inherited Create;
  tlList := TFSSpecThreadList.Create;
End;
{--------}

Destructor TfsSrcTableList.Destroy;
Begin
  tlList.Free;
  Inherited Destroy;
End;
{--------}

Procedure TfsSrcTableList.AddTable(aTable: TfsSrcBaseTable);
Begin
  tlList.Insert(aTable);
End;
{--------}

Function TfsSrcTableList.BeginRead: TfsSrcTableList;
Begin
  tlList.BeginRead;
  Result := Self;
End;
{--------}

Function TfsSrcTableList.BeginWrite: TfsSrcTableList;
Begin
  tlList.BeginWrite;
  Result := Self;
End;
{--------}

Procedure TfsSrcTableList.DeleteTable(aTableID: Longint);
Begin
  tlList.Delete(aTableID);
End;
{--------}

Procedure TfsSrcTableList.EndRead;
Begin
  tlList.EndRead;
End;
{--------}

Procedure TfsSrcTableList.EndWrite;
Begin
  tlList.EndWrite;
End;
{--------}

Function TfsSrcTableList.GetTableFromName(Const aTableName: TfsTableName): TfsSrcBaseTable;
Var
  Inx: Integer;
Begin
  For Inx := 0 To pred(tlList.Count) Do
    Begin
      Result := TfsSrcTable(tlList[Inx]);
      If (FFCmpShStrUC(Result.BaseName, aTableName, 255) = 0) Then
        Exit;
    End;
  Result := Nil;
End;
{--------}

Function TfsSrcTableList.GetTableItem(Find: TfsListFindType; Value: Longint): TfsSrcBaseTable;
Var
  Inx: Integer;
Begin
  Result := Nil;
  If (Find = ftFromID) Then
    Begin
      Inx := tlList.Index(Value);
      If (Inx <> -1) Then
        Result := TfsSrcTable(tlList[Inx]);
    End
  Else {Find = ftFromIndex}
    Begin
      If (0 <= Value) And (Value < tlList.Count) Then
        Result := TfsSrcTable(tlList[Value]);
    End;
End;
{--------}

Procedure TfsSrcTableList.RemoveIfUnused(aTable: TfsSrcBaseTable);
Begin
  { Assumption: TableList has not been write locked by the calling routine. }
  tlList.BeginWrite;
  Try
    If (aTable.CursorList.CursorCount = 0) And
      (aTable.OpenIntents = 0) Then
      Begin
        aTable.Free;
      End;
  Finally
    tlList.EndWrite;
  End;
End;
{--------}

Procedure TfsSrcTableList.RemoveUnusedTables;
Var
  Inx: Integer;
  Table: TfsSrcTable;
Begin
  { Assumption: TableList has not been write locked by the calling routine. }
  tlList.BeginWrite;
  Try
    For Inx := pred(TableCount) Downto 0 Do
      Begin
        Table := TfsSrcTable(tlList[Inx]);
        If (Table.CursorList.CursorCount = 0) And
          (Table.OpenIntents = 0) Then
          Try
            Table.Free;
          Except
            On E: Exception Do
              If FOwner <> Nil Then
                FOwner.seForce('Exception removing unused table: %s',
                  [E.Message],
                  FOwner.bseGetReadOnly);
          End;
      End;
  Finally
    tlList.EndWrite;
  End;
End;
{--------}

Function TfsSrcTableList.TableCount: Integer;
Begin
  Result := tlList.Count;
End;
{=====================================================================}

{== TfsSrcDatabase ====================================================}

Constructor TfsSrcDatabase.Create(anEngine: TFSServer;
  aSession: TfsSrcSession;
  aFolder: TfsSrcFolder;
  anAlias: TffName;
  aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  aTimeout: Longint;
  aCheckSpace: Boolean;
  aTransIsolation: TfsTransIsolation;
  aRecLocking: TfsDataBaseRecLocking); {!!.11}
Var {!!.11}
  OSVerInfo: TOSVersionInfo; {!!.11}
Begin
  Inherited Create(aTimeout);
  dbTransIsolation := aTransIsolation;
  dbRecLocking := aRecLocking;
  dbAlias := FFShStrAlloc(anAlias);
  dbEngine := anEngine;
  dbExtenders := Nil;
  soClient := aSession.Client;
  dbCursorList := TfsSrcCursorList.Create;
  dbFolder := aFolder;
  //  FDeadlocked := False;
  dbOpenMode := aOpenMode;
  dbSession := aSession;
  dbShareMode := aShareMode;
  dbStmtList := TfsSrcStmtList.Create; {!!.10}
  { Initialize the transaction information. }
  FFGetZeroMem(dbTI, SizeOf(TffTransInfo));
  With dbTI^ Do
    Begin
      tirTrans := Nil;
      tirLockMgr := dbFolder.LockMgr;
    End;
  dbTrans := Nil;
  FreeOnRemove := True;
  Session.DatabaseList.BeginWrite;
  Try
    Session.DatabaseList.AddDatabase(Self);
  Finally
    Session.DatabaseList.EndWrite;
  End;

  OSVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo); {!!.11 - Start}
  If ((aCheckSpace) And
    (GetVersionEx(OSVerInfo))) Then
    dbCheckSpace := ((OSVerInfo.dwPlatformId = VER_PLATFORM_WIN32_NT) Or
      ((OSVerInfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS) And
      (OSVerInfo.dwBuildNumber > 1000)))
  Else
    dbCheckSpace := False; {!!.11 - End}
End;
{--------}

Destructor TfsSrcDatabase.Destroy;
Var
  anIndex: Longint;
  anExtender: TFSBaseEngineExtender;

Begin
  { If a transaction is active then the transaction must be rolled back. }
  If assigned(dbTrans) Then
    dbEngine.seTransactionRollback(Self);

  { Free all registered extenders. }
  If assigned(dbExtenders) Then
    Begin
      For anIndex := pred(dbExtenders.Count) Downto 0 Do
        Begin
          anExtender := TFSBaseEngineExtender
            (TfsIntListItem(dbExtenders[anIndex]).KeyAsInt);
          anExtender.Free;
        End;
      dbExtenders.Free;
    End;

  FFShStrFree(dbAlias);
  {Begin !!.10}
  For anIndex := pred(dbStmtList.StmtCount) Downto 0 Do
    dbEngine.SQLEngine.FreeStmt(dbStmtList.Stmt[ftFromIndex, anIndex].Handle);
  dbStmtList.Free;
  {End !!.10}
  dbCursorList.Free;
  Folder.DecRefCount;
  dbFolder := Nil;
  FFFreeMem(dbTI, SizeOf(TffTransInfo));
  Inherited Destroy;
End;
{--------}

Function TfsSrcDatabase.CanClose(Const Mark: boolean): boolean;
Begin
  CursorList.BeginRead;
  dbStmtList.BeginRead; {!!.10}
  Try
    Result := (Inherited CanClose(Mark)) And {!!.06}
    CursorList.HasClosableState(Mark) And {!!.06} {!!.10}
    dbStmtList.CanClose(Mark); {!!.10}
  Finally
    dbStmtList.EndRead; {!!.10}
    CursorList.EndRead;
  End;
End;
{--------}

Procedure TfsSrcDatabase.ForceClose;
Begin
  Inherited ForceClose;

  {Begin !!.01}
    { If a transaction is active then the transaction must be rolled back. }
  If assigned(dbTrans) Then
    dbEngine.seTransactionRollback(Self);
  {End !!.01}

  CursorList.BeginRead;
  dbStmtList.BeginRead; {!!.10}
  Try
    CursorList.ForceClose;
    dbStmtList.ForceClose; {!!.10}
  Finally
    dbStmtList.EndRead; {!!.10}
    CursorList.EndRead;
  End;
End;
{--------}

Function TfsSrcDatabase.NotifyExtenders(Const anAction: TffEngineAction;
  Const aFailAction: TffEngineAction): TffResult;
Var
  anExtender: TFSBaseEngineExtender;
  anIndex: Longint;
  anIndex2: Longint;
Begin
  Result := DBIERR_NONE;
  If assigned(dbExtenders) Then
    For anIndex := 0 To pred(dbExtenders.Count) Do
      Begin
        anExtender := TFSBaseEngineExtender
          (TfsIntListItem(dbExtenders[anIndex]).KeyAsInt);
        If (anAction In anExtender.InterestedActions) Or
          (anExtender.InterestedActions = []) Then
          Begin
            Result := anExtender.Notify(Self, anAction); {!!.06}
            {since we aren't ignoring Notify's error code, we must
             capture it.  If an extender reports an error we will not
             process the rest of the extenders and we will notify the
             previous extenders that we are "undoing" the previous action}
            If Result <> DBIERR_NONE Then
              Begin
                For anIndex2 := 0 To pred(anIndex) Do
                  Begin
                    anExtender := TFSBaseEngineExtender
                      (TfsIntListItem(dbExtenders[anIndex2]).KeyAsInt);
                    anExtender.Notify(Self, aFailAction);
                  End;
                break;
              End;
          End;
      End;
End;
{--------}

Procedure TfsSrcDatabase.dbAddExtender(anExtender: TFSBaseEngineExtender);
Var
  anItem: TfsIntListItem;
Begin
  If assigned(anExtender) Then
    Begin
      If Not assigned(dbExtenders) Then
        dbExtenders := TFSSpecThreadList.Create;
      anItem := TfsIntListItem.Create(Longint(anExtender));
      dbExtenders.Insert(anItem);
    End;
End;
{--------}

Function TfsSrcDatabase.dbGetAlias: TffName;
Begin
  Result := dbAlias^;
End;
{--------}

Function TfsSrcDatabase.dbGetTransID: TffTransID;
Begin
  If assigned(dbTrans) Then
    Result := dbTrans.TransactionID
  Else
    Result := 0;
End;
{--------}

Function TfsSrcDatabase.dbGetTransLSN: TffWord32;
Begin
  If assigned(dbTrans) Then
    Result := dbTrans.LSN
  Else
    Result := 0;
End;
{Begin !!.11}
{--------}

Procedure TfsSrcDatabase.dbSetExistingTableVersion(Const Version: Longint);
Begin
  dbFolder.ExistingTableVersion := Version;
End;
{--------}

Procedure TfsSrcDatabase.dbSetNewTableVersion(Const Version: Longint);
Begin
  dbFolder.NewTableVersion := Version;
End;
{--------}

Procedure TfsSrcDatabase.dbSetPackSrcTableVersion(Const Version: Longint);
Begin
  dbFolder.PackSrcTableVersion := Version;
End;
{End !!.11}
{--------}

Procedure TfsSrcDatabase.dbSetTrans(aTransaction: TfsSrcTransaction);
Begin
  dbTrans := aTransaction;
  dbTI^.tirTrans := aTransaction;
End;
{--------}

Function TfsSrcDatabase.dbGetDatabaseID: TffDatabaseID;
Begin
  Result := TffDatabaseID(Self);
End;
{Begin !!.03}
{--------}

Procedure TfsSrcDatabase.RequestClose;
Begin
  CursorList.BeginRead;
  dbStmtList.BeginRead; {!!.10}
  Try
    Inherited RequestClose;
    CursorList.RequestClose;
    dbStmtList.RequestClose; {!!.10}
  Finally
    dbStmtList.EndRead; {!!.10}
    CursorList.EndRead;
  End;
End;
{End !!.03}
{--------}

Function TfsSrcDatabase.ShouldClose: boolean;
{Begin !!.01}
Var
  aCursor: TfsSrBaseCursor;
  aStmt: TfsBasePreparedStmt; {!!.10}
  anInx: Longint;
Begin
  Result := Inherited ShouldClose;
  { Database can close?  }
  If Result Then
    Begin
      { Yes. Lock the cursor list for read-only access. }
      CursorList.BeginRead;
      dbStmtList.BeginRead; {!!.10}
      Try
        { Is a transaction active? }
        If assigned(dbTrans) Then
          Begin
            { Yes. See if state of all cursors will allow us to rollback the
              transaction. }
    {Begin !!.10}
            For anInx := 0 To pred(dbStmtList.StmtCount) Do
              Begin
                aStmt := dbStmtList.Stmt[ftFromIndex, anInx];
                If aStmt.State <> ffosClosing Then
                  Begin
                    Result := False;
                    Break;
                  End;
              End;
            If Result Then
              {End !!.10}
              For anInx := 0 To pred(CursorList.CursorCount) Do
                Begin
                  aCursor := CursorList.Cursor[ftFromIndex, anInx];
                  If aCursor.State <> ffosClosing Then
                    Begin
                      Result := False;
                      Break;
                    End;
                End;
            If Result Then
              dbEngine.seTransactionRollback(Self);
          End
        Else
          { No transaction is active. See if cursors may be closed. }
          Result := Result And CursorList.ShouldClose And {!!.10}
          dbStmtList.ShouldClose; {!!.10}
      Finally
        dbStmtList.EndRead; {!!.10}
        CursorList.EndRead;
      End;
    End;
  {End !!.01}
End;
{====================================================================}

{===TfsSrcDatabaseList================================================}

Procedure TfsSrcDatabaseList.AddDatabase(aDatabase: TfsSrcDatabase);
Begin
  solList.Insert(aDatabase);
End;
{--------}

Function TfsSrcDatabaseList.DatabaseCount: Integer;
Begin
  Result := solList.Count;
End;
{--------}

Procedure TfsSrcDatabaseList.DeleteDatabase(aDatabaseID: Longint);
Begin
  solList.Delete(aDatabaseID);
End;
{--------}

Function TfsSrcDatabaseList.GetDatabaseForFolder(aFolder: TfsSrcFolder): TfsSrcDatabase;
Var
  Inx: Integer;
Begin
  For Inx := 0 To pred(solList.Count) Do
    Begin
      Result := TfsSrcDatabase(solList[Inx]);
      If (Result.Folder = aFolder) Then
        Exit;
    End;
  Result := Nil;
End;
{--------}

Function TfsSrcDatabaseList.GetDatabaseItem(Find: TfsListFindType; Value: Longint): TfsSrcDatabase;
Var
  Inx: Integer;
Begin
  Result := Nil;
  If (Find = ftFromID) Then
    Begin
      Inx := solList.Index(Value);
      If (Inx <> -1) Then
        Result := TfsSrcDatabase(solList[Inx]);
    End
  Else {Find = ftFromIndex}
    Begin
      If (0 <= Value) And (Value < solList.Count) Then
        Result := TfsSrcDatabase(solList[Value]);
    End;
End;
{====================================================================}

{===TfsSrcSession===============================================}

Constructor TfsSrcSession.Create(aClient: TfsSrcClient;
  Const aIsDef: boolean;
  Const aTimeout: Longint);
Begin
  Inherited Create(aTimeout);
  soClient := aClient;
  ssDatabaseList := TfsSrcDatabaseList.Create;
  ssIsDefault := aIsDef;
  FreeOnRemove := True;
  ssTablePasswordList := TFSSpecStringList.Create;
  aClient.SessionList.BeginWrite;
  Try
    aClient.SessionList.AddSession(Self);
  Finally
    aClient.SessionList.EndWrite;
  End;
End;
{--------}

Destructor TfsSrcSession.Destroy;
Begin
  ssDatabaseList.Free;
  ssTablePasswordList.free;
  Inherited Destroy;
End;
{--------}

Function TfsSrcSession.CanClose(Const Mark: boolean): boolean;
Begin
  DatabaseList.BeginRead;
  Try
    Result := (Inherited CanClose(Mark)) And DatabaseList.CanClose(Mark);
  Finally
    DatabaseList.EndRead;
  End;
End;
{--------}

Procedure TfsSrcSession.ForceClose;
Begin
  Inherited ForceClose;
  DatabaseList.BeginRead;
  Try
    DatabaseList.ForceClose;
  Finally
    DatabaseList.EndRead;
  End;
End;
{--------}

Function TfsSrcSession.ssGetSessionID: TffSessionID;
Begin
  Result := TffSessionID(Self);
End;
{Begin !!.03}
{--------}

Procedure TfsSrcSession.RequestClose;
Begin
  DatabaseList.BeginRead;
  Try
    Inherited RequestClose;
    DatabaseList.RequestClose;
  Finally
    DatabaseList.EndRead;
  End;
End;
{End !!.03}
{--------}

Function TfsSrcSession.ShouldClose: boolean;
Begin
  DatabaseList.BeginRead;
  Try
    Result := (Inherited ShouldClose) And DatabaseList.ShouldClose;
  Finally
    DatabaseList.EndRead;
  End;
End;
{====================================================================}

{Begin !!.10}
{===TfsSrcStmtList====================================================}

Procedure TfsSrcStmtList.AddStmt(aStmt: TfsBasePreparedStmt);
Begin
  solList.Insert(aStmt);
End;
{--------}

Function TfsSrcStmtList.StmtCount: Integer;
Begin
  Result := solList.Count;
End;
{--------}

Procedure TfsSrcStmtList.DeleteStmt(aStmtID: TffSQLStmtID);
Begin
  solList.Delete(aStmtID);
End;
{--------}

Function TfsSrcStmtList.GetStmt(Find: TfsListFindType; Value: Longint): TfsBasePreparedStmt;
Var
  Inx: Integer;
Begin
  Result := Nil;
  If (Find = ftFromID) Then
    Begin
      Inx := solList.Index(Value);
      If (Inx <> -1) Then
        Result := TfsBasePreparedStmt(solList[Inx]);
    End
  Else {Find = ftFromIndex}
    Begin
      If (0 <= Value) And (Value < solList.Count) Then
        Result := TfsBasePreparedStmt(solList[Value]);
    End;
End;
{--------}

Procedure TfsSrcStmtList.RemoveForClient(Const aClientID: TffClientID);
Var
  anInx: Longint;
Begin
  With solList.BeginWrite Do
    Try
      For anInx := Pred(solList.Count) Downto 0 Do
        Begin
          If TfsBasePreparedStmt(solList[anInx]).ClientID = aClientID Then
            solList.DeleteAt(anInx);
        End;
    Finally
      solList.EndWrite;
    End;
End;
{====================================================================}
{End !!.10}

{===TfsSrcSessionList====================================================}

Procedure TfsSrcSessionList.AddSession(aSession: TfsSrcSession);
Begin
  solList.Insert(aSession);
End;
{--------}

Procedure TfsSrcSessionList.DeleteSession(aSessionID: Longint);
Begin
  solList.Delete(aSessionID);
End;
{--------}

Function TfsSrcSessionList.slGetCurSess: TfsSrcSession;
Begin
  Result := slCurSess;
End;
{--------}

Function TfsSrcSessionList.slGetSessionItem(Find: TfsListFindType; Value: Longint): TfsSrcSession;
Var
  Inx: Longint;
Begin
  Result := Nil;
  If (Find = ftFromID) Then
    Begin
      Inx := solList.Index(Value);
      If (Inx <> -1) Then
        Result := TfsSrcSession(solList[Inx]);
    End
  Else {Find = ftFromIndex}  If (0 <= Value) And (Value < solList.Count) Then
    Result := TfsSrcSession(solList[Value]);
End;
{--------}

Function TfsSrcSessionList.SessionCount: Integer;
Begin
  Result := solList.Count;
End;
{--------}

Procedure TfsSrcSessionList.slSetCurSess(CS: TfsSrcSession);
Begin
  If (slCurSess = Nil) Then
    slCurSess := slDefSess;
  If (slCurSess <> CS) Then
    If (CS = Nil) Then {CS=nil means the default session}
      slCurSess := slDefSess
    Else
      slCurSess := CS;
End;
{--------}

Procedure TfsSrcSessionList.SetDefaultSession(aSession: TfsSrcSession);
Begin
  slDefSess := aSession;
  CurrentSession := Nil;
End;
{====================================================================}

{===TfsSrcClient=====================================================}

Constructor TfsSrcClient.Create(aClientID: Longint;
  Const aClientName: TffNetName;
  Const aTimeout: Longint;
  Const aClientVersion: Longint; {!!.11}
  aUser: TfsUserItem;
  anEngine: TFSServer);
//var                                                                  {Deleted !!.03}
//  DefSess : TfsSrcSession;                                            {Deleted !!.03}
Begin
  Inherited Create(aTimeout);
  clAccepted := False;
  clClientName := FFShStrAlloc(aClientName);
  clClientVersion := aClientVersion; {!!.11}
  clEngine := anEngine;
  clExtenders := Nil;
  soLock := TfsPadlock.Create;
  clSessionList := TfsSrcSessionList.Create;
  clFirstSession := TfsSrcSession.Create(Self, True, timeout); {!!.03}
  SessionList.BeginWrite;
  Try
    SessionList.SetDefaultSession(clFirstSession); {!!.03}
  Finally
    SessionList.EndWrite;
  End;
  FreeOnRemove := True;
  {Note: we do NOT save the reference to the user object, these get
         destroyed and rebuilt ad hoc}
  If (aUser <> Nil) Then
    With aUser Do
      Begin
        clUserID := UserID;
        clFirst := FirstName;
        clLast := LastName;
        clRights := Rights;
      End;
End;
{--------}

Destructor TfsSrcClient.Destroy;
Var
  anExtender: TFSBaseEngineExtender;
  anIndex: Longint;
Begin

  Try {!!.03}
    { Notify the extenders. }
    If clAccepted Then
      NotifyExtenders(ffeaBeforeRemoveClient, ffeaNoAction);

    { Get rid of the rebuild status info associated with this client. }
    clEngine.seCleanRebuildList(ClientID);

    { Free all registered extenders. }
    If assigned(clExtenders) Then
      Begin
        For anIndex := pred(clExtenders.Count) Downto 0 Do
          Begin
            anExtender := TFSBaseEngineExtender
              (TfsIntListItem(clExtenders[anIndex]).KeyAsInt);
            anExtender.Free;
          End;
        clExtenders.Free;
      End;

    clSessionList.Free;
    FFShStrFree(clClientName);
    soLock.Free;
  Finally {!!.03}
    Inherited Destroy;
  End; {!!.03}
End;
{--------}

Procedure TfsSrcClient.AddClientExtender(anExtender: TFSBaseEngineExtender);
Var
  anItem: TfsIntListItem;
Begin
  If assigned(anExtender) Then
    Begin
      If Not assigned(clExtenders) Then
        clExtenders := TFSSpecThreadList.Create;
      anItem := TfsIntListItem.Create(Longint(anExtender));
      clExtenders.Insert(anItem);
    End;
End;
{--------}

Function TfsSrcClient.CanClose(Const Mark: boolean): boolean;
Begin
  SessionList.BeginRead;
  Try
    Result := (Inherited CanClose(Mark)) And SessionList.CanClose(Mark);
  Finally
    SessionList.EndRead;
  End;
End;
{--------}

Function TfsSrcClient.clGetClientID: TffClientID;
Begin
  Result := TffClientID(Self);
End;
{--------}

Procedure TfsSrcClient.ForceClose;
Begin
  Inherited ForceClose;
  SessionList.BeginRead;
  Try
    SessionList.ForceClose;
  Finally
    SessionList.EndRead;
  End;
End;
{--------}

Function TfsSrcClient.clGetClientName: TffNetName;
Begin
  Result := clClientName^;
End;
{--------}

Function TfsSrcClient.NotifyExtenders(Const anAction: TffEngineAction;
  Const aFailAction: TffEngineAction): TffResult;
Var
  anExtender: TFSBaseEngineExtender;
  anIndex: Longint;
  anIndex2: Longint;
Begin
  Result := DBIERR_NONE;
  If assigned(clExtenders) Then
    For anIndex := 0 To pred(clExtenders.Count) Do
      Begin
        anExtender := TFSBaseEngineExtender
          (TfsIntListItem(clExtenders[anIndex]).KeyAsInt);
        If (anAction In anExtender.InterestedActions) Or
          (anExtender.InterestedActions = []) Then
          Begin
            Result := anExtender.Notify(Self, anAction);
            { If an extender reports a failure, subsequent extenders will not be
              notified of the action. }
            If Result <> DBIERR_NONE Then
              Begin
                For anIndex2 := 0 To pred(anIndex) Do
                  Begin
                    anExtender := TFSBaseEngineExtender(TfsIntListItem(clExtenders[anIndex2]).KeyAsInt);
                    anExtender.Notify(Self, aFailAction);
                  End;
                break;
              End;
          End;
      End;
End;
{Begin !!.03}
{--------}

Procedure TfsSrcClient.RequestClose;
Begin
  SessionList.BeginRead;
  Try
    Inherited RequestClose;
    SessionList.RequestClose;
  Finally
    SessionList.EndRead;
  End;
End;
{End !!.03}
{--------}

Function TfsSrcClient.ShouldClose: boolean;
Begin
  SessionList.BeginRead;
  Try
    Result := (Inherited ShouldClose) And SessionList.ShouldClose;
  Finally
    SessionList.EndRead;
  End;
End;
{====================================================================}

{===TfsSrcClientList====================================================}

Procedure TfsSrcClientList.AddClient(aClient: TfsSrcClient);
Begin
  solList.Insert(aClient)
End;
{--------}

Function TfsSrcClientList.ClientCount: Integer;
Begin
  Result := solList.Count;
End;
{--------}

Procedure TfsSrcClientList.DeleteClient(aClientID: Longint);
Begin
  solList.Delete(aClientID);
End;
{--------}

Function TfsSrcClientList.GetClientItem(Find: TfsListFindType; Value: Longint): TfsSrcClient;
Var
  Inx: Integer;
Begin
  Result := Nil;
  If (Find = ftFromID) Then
    Begin
      Inx := solList.Index(Value);
      If (Inx <> -1) Then
        Result := TfsSrcClient(solList[Inx]);
    End
  Else {Find = ftFromIndex}  If (0 <= Value) And (Value < solList.Count) Then
    Result := TfsSrcClient(solList[Value]);
End;
{--------}

Procedure TfsSrcClientList.SetClientItem(Inx: Integer; CI: TfsSrcClient);
Begin
  solList[Inx] := CI;
End;
{=====================================================================}

{===TFSServer===================================================}

Constructor TFSServer.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  CursorClass := TfsSrcCursor; {!!.06}

  FileProcsInitialize;
  //fSecurityEnabled:= true;
  seCanLog := False;

  seClientHash := TfsHash.Create(fsc_Size127); {!!.02}

  {create the configuration object}
  seConfig := TfsServerConfiguration.Create;
  seConfigLoaded := False;

  {create the client list, the open database list, the open table
   list, the transaction list}
  seClientList := TfsSrcClientList.Create;
  seSessionList := TfsSrcSessionList.Create;
  seDatabaseList := TfsSrcDatabaseList.Create;
  seTableList := TfsSrcTableList.Create;
  seTableList.Owner := Self; {!!.06}
  seCursorList := TfsSrcCursorList.Create;

  seTempPath := '';
  seConfigFile := '';
  seFolderList := TfsSrcFolderList.Create;
  seRebuildList := TfsSrcRebuildStatusList.Create;

  { Create the buffer manager. Temporary storage size will be updated after
    reading FLSINFO. }
  seBufMgr := TfsBufferManager.Create(TempPath, fscl_TempStorageSize, False);

  { Ensure the seEvtClientDone is set to nil. }
  seEvtClientDone := Nil;
  seOnRecoveryCheck := Nil;
  seScriptFile := '';
  seSQLEngine := TFSSQLEngine.Create(Nil);
End;
{--------}

Destructor TFSServer.Destroy;
Begin
  { Tell garbage collector to end. }
  If assigned(seGarbageThread) Then
    Begin
      seGarbageThread.DieDieDie;
      seGarbageThread.WaitFor;
      seGarbageThread.Free;
    End;

  { Make sure we are shutdown. }
  State := fsesInactive;

  FFNotifyDependents(ffn_Destroy);
  //If Assigned(seSQLEngine) Then
  //  seSQLEngine.FFRemoveDependent(Self);
  TFSSQLEngine(seSQLEngine).free;
  seCursorList.Free;
  seTableList.Free;
  seDatabaseList.Free;
  seSessionList.Free;
  seClientList.Free;
  seFolderList.Free;
  seConfig.Free;
  seBufMgr.Free;
  seRebuildList.Free;
  seClientHash.Free;

  Inherited Destroy;
End;
{--------}

Procedure TFSServer.FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp;
  Const AData: TffWord32);
Var
  RecalcLogFlag: boolean;
Begin
  RecalcLogFlag := (AFrom = FEventLog);
  Inherited;
  If (AFrom = seSQLEngine) And (AOp In [ffn_Destroy, ffn_Remove]) Then
    Begin
      seSQLEngine.FFRemoveDependent(Self);
      seSQLEngine := Nil;
    End;

  If RecalcLogFlag Then
    seSetLoggingState;
End;
{--------}

Procedure TFSServer.scInitialize;
Begin
  LogAll(['FSSQL Server initializing...',
    format('  Version: %5.3f ',
      [fsVersionNumber / 1000])]);
  LoadConfiguration;

  {Begin !!.06}
  Log('Performing recovery check...');
  If assigned(seOnRecoveryCheck) Then
    seOnRecoveryCheck(Self)
  Else
    With FFRecoveryClass.Create Do
      Try
        Check(Self);
      Finally
        Free;
      End;
  Log('Finished recovery check...');
  {End !!.06}

    { Perform garbage collection? }
  If Configuration.GeneralInfo^.giCollectEnabled Then
    { Yes. Start the garbage collector thread. }
    seGarbageThread := TfsTimerThread.Create
      (Configuration.GeneralInfo^.giCollectFreq,
      seCollectGarbage, 0, False);

  seLastFlush := GetTickCount; {!!.01}

  {$IFDEF DebugDelCount}
  FFTBDATA.aLog := FEventLog;
  {$ENDIF}
  {$IFDEF RAMPageCheck}
  FFSRBASE.aLog := FEventLog;
  {$ENDIF}
End;
{--------}

Procedure TFSServer.scPrepareForShutdown;
Var
  aClient: TfsSrcClient;
  ClientDoneEvent: TFSNormalEvent;
  i: Integer;
Begin
  Log('FSSQL Server preparing for shutdown.');

  { Kill the garbage collection thread. }
  If assigned(seGarbageThread) Then
    seGarbageThread.DieDieDie;

  { Ask the SQL engine to get rid of any remaining prepared statements. }
  If Assigned(seSQLEngine) Then
    seSQLEngine.RequestClose;

  If ClientList.ClientCount > 0 Then
    { Attempt to clear out those clients in a "closing" state. }
    seCollectGarbage(0);

  FFNotifyDependents(ffn_Deactivate); {!!.03}

  If ClientList.ClientCount > 0 Then
    Begin
      {Create an event to wait on the clients to finish what they're
       doing. We will give them a chance to signal us that they're done
       and then we'll just cut them off.}
      ClientDoneEvent := TFSNormalEvent.Create;
      Try
        seEvtClientDone := ClientDoneEvent;
        Try
          ClientDoneEvent.WaitFor(fsc_ClientShutdownTime);
        Except
          For i := Pred(ClientList.ClientCount) Downto 0 Do
            Begin
              Try
                aClient := ClientList.Client[ftFromIndex, i];
                aClient.ForceClose;
                seClientRemovePrim(aClient);
              Except
              End;
            End;
        End;
      Finally
        seEvtClientDone := Nil;
        ClientDoneEvent.Free;
      End;
    End;
End;
{--------}

Procedure TFSServer.scStartup;
Begin
  Log('FSSQL Server started.');

  seStartTime := GetTickCount; {!!.10}
  CoCreateGUID(seUniqueID); {!!.10}
End;
{--------}

Procedure TFSServer.scShutDown;
Begin
  Log('FS Server shutting down.');
End;
{--------}

Procedure TFSServer.seCleanRebuildList(Const aClientID: TffClientID);
Begin
  If assigned(seRebuildList) Then
    seRebuildList.DeleteAllForClient(aClientID);
End;
{--------}

Procedure TFSServer.seCollectGarbage(Const aTimerEventCookie: Longint);
Begin
  Try
    If assigned(seSQLEngine) Then
      seSQLEngine.CollectGarbage;
    ClientList.RemoveUnused;
    If Configuration.GeneralInfo^.giCloseInactiveTables Then
      TableList.RemoveUnusedTAbles;
    FolderList.RemoveUnusedFolders;
    If Configuration.GeneralInfo^.giClearCache Then
      seBufMgr.bmRemoveExcessPages;
    { Time to flush pools? }
    If (GetTickCount - seLastFlush) >= fscl_FlushRate Then
      Begin
        FSLockContainerPool.Flush;
        FFSemPool.Flush;
        seBufMgr.Lock;
        Try
          seBufMgr.FlushPools([]);
        Finally
          seBufMgr.Unlock;
        End;
        seLastFlush := GetTickCount;
      End;
  Except
    On E: EfsException Do {!!.01}
      seForce('Error in garbage collection: %s',
        [E.Message],
        bseGetReadOnly);
  End;
End;
{--------}

Function TFSServer.seDatabaseAddAliasPrim(Const aAlias: TffName;
  Const aPath: TffPath;
  aCheckSpace: Boolean) {!!.11}
: TffResult;
Begin
  { Assumption: Thread-safeness enforced at a higher level. }

  { Does the alias already exist? }
  If seConfig.AliasList.AliasExists(aAlias) Then
    { No.  Return error code. }
    Result := DBIERR_NAMENOTUNIQUE
  Else
    Begin
      { Yes. Add the new Alias and its path. }
      seConfig.AddAlias(aAlias, aPath, aCheckSpace); {!!.11}
      Result := DBIERR_NONE;
    End;
End;
{--------}

Function TFSServer.seDeleteTable(Const aDB: TfsSrcDatabase;
  Const aTableName: TfsTableName)
  : TffResult;
Var
  Dict: TFSInfoDict;
Begin
  Dict := TFSInfoDict.Create(4096);
  ;
  Try
    Result := seGetDictionary(aDB, aTableName, Dict);
    { Retrieved the dictionary? }
    If Result = DBIERR_NONE Then
      Begin
        { Yes. Delete the files specified by the dictionary. }
        FFTblHlpDelete(aDB.Folder.Path, aTableName, Dict);
        Result := DBIERR_NONE;
      End
    Else If (Result <> DBIERR_INVALIDTABLENAME) And
      (Result <> DBIERR_NOSUCHTABLE) Then
      { No. Assuming the result code is not one of the above errors then the
        file exists but has no dictionary. Delete the data file. }
      FFDeleteFile(FFMakeFullFileName(aDB.Folder.Path,
        FFMakeFileNameExt(aTableName,
        fsc_ExtForData)));
  Finally
    Dict.Free;
  End;
End;
{--------}

Function TFSServer.seGetCollectFrequency: Longint;
Begin
  Result := Configuration.GeneralInfo^.giCollectFreq;
End;
{--------}

Function TFSServer.seGetClearCachePerCount: Longint;
Begin
  Result := Configuration.GeneralInfo^.giClearCachePerCount;
End;

Function TFSServer.seGetCloseInactiveTablesAfterCommitOrRoolback: Boolean;
Begin
  Result := Configuration.GeneralInfo^.giCloseInactiveTablesAfterCommitOrRoolback;
End;

Function TFSServer.seGetCloseInactiveTables: Boolean;
Begin
  Result := Configuration.GeneralInfo^.giCloseInactiveTables;
End;

Function TFSServer.seGetClearCacheIfUpdate: Boolean;
Begin
  Result := Configuration.GeneralInfo^.giClearCacheIfUpdate;
End;

Function TFSServer.seGetClearCache: Boolean;
Begin
  Result := Configuration.GeneralInfo^.giClearCache;
End;

Function TFSServer.seGetCollectGarbage: Boolean;
Begin
  Result := Configuration.GeneralInfo^.giCollectEnabled;
End;
{--------}

Function TFSServer.seGetConfig: TfsServerConfiguration;
Begin
  If (Not seConfigLoaded) Then
    LoadConfiguration;
  Result := seConfig;
End;
{Begin !!.01}
{--------}

Function TFSServer.seGetMaxRAM: Longint;
Begin
  Result := Configuration.GeneralInfo^.giMaxRAM;
End;

Function TFSServer.seGetSecurityEnabled: Boolean;
Begin
  Result := Configuration.GeneralInfo^.giIsSecure;
End;
{--------}

Function TFSServer.seGetScriptFile: String; {!!.11}
Begin
  Result := seScriptFile;
End;
{--------}

Function TFSServer.seIsServerTable(Const aTableName: TfsTableName): boolean;
Var
  aPrefix, aSuffix: TfsTableName;
Begin
  Result := False;
  aPrefix := Uppercase(Copy(aTableName, 1, 3));
  { Is this prefixed with characters normally used for server tables? }
  If (aPrefix = fsc_SavPrefix) Or
    (aPrefix = fsc_StdPrefix) Or
    (aPrefix = fsc_TmpPrefix) Then
    Begin
      aSuffix := Uppercase(Copy(aTableName, 4, 5));
      Result := (aSuffix = fsc_IndexSuffix);
    End;
End;
{--------}

Function TFSServer.seGetDictionary(Const aDB: TfsSrcDatabase;
  Const aTableName: TfsTableName;
  Var aDict: TFSInfoDict): TffResult;
Var
  Table: TfsSrcTable;
  TableDataFile: TffFileNameExt;
Begin
  Result := DBIERR_NONE;
  Assert(assigned(aDB));
  Try
    Table := TfsSrcTable(GetTableInstance(aDB.Folder, aTableName));
    If Table = Nil Then
      Begin
        If Not FFVerifyFileName(aTableName) Then
          Begin
            Result := DBIERR_INVALIDTABLENAME;
            Exit;
          End;
        TableDataFile := FFMakeFileNameExt(aTableName, fsc_ExtForData);
        If Not FFFileExists(FFMakeFullFileName(aDB.Folder.Path, TableDataFile)) Then
          Begin
            Result := DBIERR_NOSUCHTABLE;
            Exit;
          End;
        Table := TfsSrcTable.Create(Self, aTableName, aDB.Folder, seBufMgr, omReadOnly);
        Try
          Table.OpenFiles(aDB.dbTI, seIsServerTable(aTableName), []);
          aDict.Assign(Table.Dictionary);
        Finally
          Table.Free;
        End;
      End
    Else
      aDict.Assign(Table.Dictionary);
  Except
    On E: Exception Do
      Result := ConvertServerException(E, EventLog);
  End;
End;
{--------}

Function TFSServer.seGetServerName: TffNetName;
Begin
  Result := seConfig.GeneralInfo^.giServerName;
End;
{--------}

Procedure TFSServer.seSetLoggingState;
Begin
  seCanLog := FLogEnabled And assigned(FEventLog) And (Not IsReadOnly);
End;
{--------}

Procedure TFSServer.seSetCollectFrequency(aFreq: Longint);
Begin
  Configuration.GeneralInfo^.giCollectFreq := aFreq;
End;
{--------}

Procedure TFSServer.seSetCollectGarbage(aValue: Boolean);
Begin
  Configuration.GeneralInfo^.giCollectEnabled := aValue;
End;
{--------}

Procedure TFSServer.seSetTempPath(Const aPath: String);
Begin
  seTempPath := Trim(aPath);
End;

Procedure TFSServer.seSetConfigFile(Const aFile: String);
Begin
  seConfigFile := Trim(aFile);
End;

Procedure TFSServer.seSetClearCachePerCount(Const aValue: Longint);
Begin
  Configuration.GeneralInfo^.giClearCachePerCount := aValue;
  seBufMgr.ClearCachePerCount := aValue;
End;

Procedure TFSServer.seSetCloseInactiveTablesAfterCommitOrRoolback(Const aValue: Boolean);
Begin
  Configuration.GeneralInfo^.giCloseInactiveTablesAfterCommitOrRoolback := aValue;
End;

Procedure TFSServer.seSetCloseInactiveTables(Const aValue: Boolean);
Begin
  Configuration.GeneralInfo^.giCloseInactiveTables := aValue;
End;

Procedure TFSServer.seSetClearCacheIfUpdate(Const aValue: Boolean);
Begin
  Configuration.GeneralInfo^.giClearCacheIfUpdate := aValue;
  seBufMgr.ClearCacheIfUpdate := aValue;
End;

Procedure TFSServer.seSetClearCache(Const aValue: Boolean);
Begin
  Configuration.GeneralInfo^.giClearCache := aValue;
End;

Procedure TFSServer.seSetSecurityEnabled(aValue: Boolean);
Begin
  Configuration.GeneralInfo^.giIsSecure := aValue;
End;

Procedure TFSServer.seSetMaxRAM(Const aValue: Longint);
Begin
  Configuration.GeneralInfo^.giMaxRAM := aValue;
  seBufMgr.MaxRAM := aValue;
End;

{--------}

Procedure TFSServer.seSetScriptFile(Const aFile: String); {!!.11}
Begin
  seScriptFile := aFile;
End;
{--------}

Function TFSServer.seGetTempPath: String;
Begin
  If (csDesigning In ComponentState) Then
    Result := seTempPath
  Else If (seTempPath = '') Then
    Begin
      Result := FFExtractPath(Application.ExeName);
      If (Result[Length(Result)] <> '\') Then
        Result := Result + '\';
    End
  Else If Not fsDirectoryExists(seTempPath) Then
    Begin
      Result := FFExtractPath(Application.ExeName);
      If (Result[Length(Result)] <> '\') Then
        Result := Result + '\';
    End
  Else
    Result := seTempPath;
End;

Function TFSServer.seGetConfigFile: String;
Begin
  If (csDesigning In ComponentState) Then
    Result := seConfigFile
  Else If (seConfigFile = '') Then
    Begin
      Result := FFExtractPath(Application.ExeName);
      If (Result[Length(Result)] <> '\') Then
        Result := Result + '\';
      Result := Result + 'server.settings';
    End
  Else
    Result := seConfigFile;
End;

{--------}

Procedure TFSServer.seSetSQLEngine(anEngine: TFSBaseSQLEngine);
Begin
  If seSQLEngine = anEngine Then
    Exit;

  If assigned(seSQLEngine) Then
    seSQLEngine.FFRemoveDependent(Self); {!!.11}

  If assigned(anEngine) Then
    anEngine.FFAddDependent(Self); {!!.11}

  seSQLEngine := anEngine;

End;
{--------}

Procedure TFSServer.Log(Const aMsg: String);
Begin
  If seCanLog Then
    FEventLog.WriteString(aMsg);
End;
{--------}

Procedure TFSServer.LogAll(Const Msgs: Array Of String);
Begin
  If seCanLog Then
    FEventLog.WriteStrings(Msgs);
End;
{--------}

Procedure TFSServer.LogFmt(Const aMsg: String; args: Array Of Const);
Begin
  If seCanLog Then
    FEventLog.WriteString(format(aMsg, args));
End;
{--------}

Procedure TFSServer.seForce(Const aMsg: String; {!!.06 - Start}
  args: Array Of Const;
  ReadOnly: Boolean);
Begin
  If ((FEventLog <> Nil) And
    (Not ReadOnly)) Then {!!.06 - End}
    FEventLog.WriteString(Format(aMsg, args));
End;
{--------}

Function TFSServer.BLOBCreate(aCursorID: TffCursorID;
  Var aBLOBNr: TffInt64): TffResult;
{Restructured !!.10}
Var
  Cursor: TfsSrBaseCursor;
  StartedTrans: boolean;
  TransID: TffTransID;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If Result = DBIERR_NONE Then
    Try
      StartedTrans := False;
      Try
        FFSetRetry(Cursor.Timeout);
        If Result = DBIERR_NONE Then
          Begin
            Result := Cursor.EnsureWritable(False, False, tluDatabase); {!!.02}
            If (Result = DBIERR_NOACTIVETRAN) Or
              Cursor.NeedNestedTransaction Then
              Begin {!!.03}
                Result := seTransactionStart(Cursor.Database, False,
                  fscl_TrImplicit, TransID);
                StartedTrans := (Result = DBIERR_NONE);
              End;

            If (Result = DBIERR_NONE) Then
              Begin
                Result := Cursor.BLOBAdd(aBLOBNr);
                If StartedTrans Then
                  If Result = DBIERR_NONE Then
                    seTransactionCommit(Cursor.Database)
                  Else
                    seTransactionRollback(Cursor.Database);
              End;
          End;
      Except
        On E: Exception Do
          Begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            If StartedTrans Then
              seTransactionRollback(Cursor.Database);
          End;
      End; {try..except}
    Finally
      Cursor.Deactivate;
    End;
End;
{--------}

Function TFSServer.BLOBDelete(aCursorID: TffCursorID; aBLOBNr: TffInt64): TffResult;
Var
  Cursor: TfsSrBaseCursor;
  StartedTrans: boolean;
  TransID: TffTransID;
  {Restructured !!.10}
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If (Result = DBIERR_NONE) Then
    Begin
      If ((Cursor.Table.TableFlags And fsTableDontDeleteRecord) <> 0) Then
        Begin
          Result := DBIERR_NOTSUFFFIELDRIGHTS;
          Cursor.Deactivate;
          Exit;
        End;
    End;
  If (Result = DBIERR_NONE) Then
    Try
      StartedTrans := False;
      Try
        FFSetRetry(Cursor.Timeout);
        If Result = DBIERR_NONE Then
          Begin
            Result := Cursor.EnsureWritable(False, False, tluDatabase); {!!.02}

            If (Result = DBIERR_NOACTIVETRAN) Or
              Cursor.NeedNestedTransaction Then
              Begin {!!.03}
                Result := seTransactionStart(Cursor.Database, False,
                  fscl_TrImplicit, TransID);
                StartedTrans := (Result = DBIERR_NONE);
              End;
            If (Result = DBIERR_NONE) Then
              Begin
                Result := Cursor.BLOBDelete(aBLOBNr);
                If StartedTrans Then
                  If Result = DBIERR_NONE Then
                    seTransactionCommit(Cursor.Database)
                  Else
                    seTransactionRollback(Cursor.Database);
              End;
          End;
      Except
        On E: Exception Do
          Begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            If StartedTrans Then
              seTransactionRollback(Cursor.Database);
          End;
      End; {try..except}
    Finally
      Cursor.Deactivate;
    End;
End;
{--------}

Function TFSServer.BLOBFree(aCursorID: TffCursorID;
  aBLOBNr: TffInt64;
  ReadOnly: boolean): TffResult;
{Restructured !!.10}
Var
  Cursor: TfsSrBaseCursor;
  StartedTrans: boolean;
  TransID: TffTransID;
Begin
  { If the BLOB was opened in read-only mode then nothing to do. }
  If ReadOnly Then
    Begin
      Result := DBIERR_NONE;
      Exit;
    End;

  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If (Result = DBIERR_NONE) Then
    Try
      StartedTrans := False;
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.EnsureWritable(False, False, tluDatabase); {!!.02}
        If (Result = DBIERR_NOACTIVETRAN) Or
          Cursor.NeedNestedTransaction Then
          Begin {!!.03}
            Result := seTransactionStart(Cursor.Database, False,
              fscl_TrImplicit, TransID);
            StartedTrans := (Result = DBIERR_NONE);
          End;

        If (Result = DBIERR_NONE) Then
          Begin
            Result := Cursor.BLOBFree(aBLOBNr);
            If StartedTrans Then
              If (Result = DBIERR_NONE) Or {!!.01}
              (Result = DBIERR_BLOBMODIFIED) Then {!!.01}
                seTransactionCommit(Cursor.Database)
              Else
                seTransactionRollback(Cursor.Database);
          End; { if }
      Except
        On E: Exception Do
          Begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            If StartedTrans Then
              seTransactionRollback(Cursor.Database);
          End;
      End; {try..except}
    Finally
      Cursor.Deactivate;
    End;
End;
{--------}

Function TFSServer.BLOBGetLength(aCursorID: TffCursorID; aBLOBNr: TffInt64;
  Var aLength: Longint): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If Result = DBIERR_NONE Then
      Try
        FFSetRetry(Cursor.Timeout);
        If (Result = DBIERR_NONE) Then
          aLength := Cursor.BLOBGetLength(aBLOBNr, Result);
      Finally
        Cursor.Deactivate;
      End; { try..finally }
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{Begin !!.03}
{--------}

Function TFSServer.BLOBListSegments(aCursorID: TffCursorID;
  aBLOBNr: TffInt64;
  aStream: TStream): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        If Result = DBIERR_NONE Then
          Result := Cursor.BLOBListSegments(aBLOBNr, aStream);
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{End !!.03}
{--------}

Function TFSServer.BLOBRead(aCursorID: TffCursorID;
  aFieldNo: TffWord32;
  aBLOBNr: TffInt64;
  aOffset: TffWord32; {!!.06}
  aLen: TffWord32; {!!.06}
  Var aBLOB;
  Var aBytesRead: TffWord32) {!!.06}
: TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        If Result = DBIERR_NONE Then
          Begin
            Result := Cursor.BLOBRead(aBLOBNr, aOffset, aLen, aBLOB, aBytesRead);
          End;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.BLOBTruncate(aCursorID: TffCursorID;
  aBLOBNr: TffInt64;
  aBLOBLength: Longint): TffResult;
{Restructured !!.10}
Var
  Cursor: TfsSrBaseCursor;
  StartedTrans: boolean;
  TransID: TffTransID;
  aRecord: PffByteArray;
  aFlag: Byte;
  aRefNr: TffInt64;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If (Result = DBIERR_NONE) Then
    Begin
      If ((Cursor.Table.TableFlags And fsTableDontModifyRecord) <> 0) Then
        Begin
          Result := DBIERR_NOTSUFFFIELDRIGHTS;
          Cursor.Deactivate;
          Exit;
        End;

      FFGetMem(aRecord, Cursor.Table.Dictionary.RecordLength);
      Try
        Result := Cursor.getRecord(aRecord, ffsltNone, tluDatabase, aFlag, arefnr, False);
        If (getflags(aFlag, frProtectUpdateRecord)) Then
          Begin
            Result := DBIERR_NOTSUFFFIELDRIGHTS;
            FFFreeMem(aRecord, Cursor.Table.Dictionary.RecordLength);
            Cursor.Deactivate;
            Exit;
          End;
      Finally
        FFFreeMem(aRecord, Cursor.Table.Dictionary.RecordLength);
      End;
    End;

  If Result = DBIERR_NONE Then
    Try
      StartedTrans := False;
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.EnsureWritable(False, False, tluDatabase); {!!.02}
        If (Result = DBIERR_NOACTIVETRAN) Or
          Cursor.NeedNestedTransaction Then
          Begin {!!.03}
            Result := seTransactionStart(Cursor.Database, False,
              fscl_TrImplicit, TransID);
            StartedTrans := (Result = DBIERR_NONE);
          End;
        If (Result = DBIERR_NONE) Then
          Begin
            Result := Cursor.BLOBTruncate(aBLOBNr, aBLOBLength);
            If StartedTrans Then
              If Result = DBIERR_NONE Then
                seTransactionCommit(Cursor.Database)
              Else
                seTransactionRollback(Cursor.Database);
          End; { if }
      Except
        On E: Exception Do
          Begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            If StartedTrans Then
              seTransactionRollback(Cursor.Database);
          End;
      End; {try..except}
    Finally
      Cursor.Deactivate;
    End;
End;
{--------}

Function TFSServer.BLOBWrite(aCursorID: TffCursorID;
  aFieldNo: TffWord32;
  aBLOBNr: TffInt64;
  aOffset: Longint;
  aLen: Longint;
  Var aBLOB): TffResult;
{Restructured !!.10}
Var
  Cursor: TfsSrBaseCursor;
  StartedTrans: boolean;
  TransID: TffTransID;
Begin
  Result := DBIERR_NONE; {!!.01 - Start}
  If aLen = 0 Then
    Exit; {!!.01 - End}

  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If (Result = DBIERR_NONE) Then
    Begin
      If ((Cursor.Table.TableFlags And fsTableDontModifyRecord) <> 0) Then
        Begin
          Result := DBIERR_NOTSUFFFIELDRIGHTS;
          Cursor.Deactivate;
          Exit;
        End;
    End;

  If Result = DBIERR_NONE Then
    Try
      StartedTrans := False;
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.EnsureWritable(False, False, tluDatabase); {!!.02}
        If (Result = DBIERR_NOACTIVETRAN) Or
          Cursor.NeedNestedTransaction Then
          Begin {!!.03}
            Result := seTransactionStart(Cursor.Database, False,
              fscl_TrImplicit, TransID);
            StartedTrans := (Result = DBIERR_NONE);
          End;
        If (Result = DBIERR_NONE) Then
          Begin
            Result := Cursor.BLOBWrite(aBLOBNr, aOffset, aLen, aBLOB);
            If StartedTrans Then
              If Result = DBIERR_NONE Then
                seTransactionCommit(Cursor.Database)
              Else
                seTransactionRollback(Cursor.Database);
          End;
      Except
        On E: Exception Do
          Begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            If StartedTrans Then
              seTransactionRollback(Cursor.Database);
          End;
      End; {try..except}
    Finally
      Cursor.Deactivate;
    End;
End;
{--------}

Function TFSServer.FileBLOBAdd(aCursorID: TffCursorID;
  Const aFileName: TffFullFileName;
  Var aBLOBNr: TffInt64): TffResult;
{Restructured !!.10}
Var
  Cursor: TfsSrBaseCursor;
  StartedTrans: boolean;
  TransID: TffTransID;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If Result = DBIERR_NONE Then
    Try
      StartedTrans := False;
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.EnsureWritable(False, False, tluDatabase); {!!.02}
        If (Result = DBIERR_NOACTIVETRAN) Or
          Cursor.NeedNestedTransaction Then
          Begin {!!.03}
            Result := seTransactionStart(Cursor.Database, False,
              fscl_TrImplicit, TransID);
            StartedTrans := (Result = DBIERR_NONE);
          End;
        If (Result = DBIERR_NONE) Then
          Begin
            Result := Cursor.FileBLOBAdd(aFileName, aBLOBNr);
            If StartedTrans Then
              If Result = DBIERR_NONE Then
                seTransactionCommit(Cursor.Database)
              Else
                seTransactionRollback(Cursor.Database);
          End;
      Except
        On E: Exception Do
          Begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            If StartedTrans Then
              seTransactionRollback(Cursor.Database);
          End;
      End; {try..except}
    Finally
      Cursor.Deactivate;
    End;
End;
{--------}

Function TFSServer.CheckClientIDAndGet(aClientID: TffClientID;
  Var aClient: TfsSrcClient): TffResult;
Begin
  If State <> fsesStarted Then
    Begin
      Result := DBIERR_FS_ServerUnavail;
      Exit;
    End;

  Result := seCheckClientIDAndGet(aClientID, aClient);
  If Result = DBIERR_NONE Then
    Begin
      Result := DBIERR_FS_UnknownClient;
      If aClient.Activate Then
        Result := DBIERR_NONE;
    End;
End;
{--------}

Function TFSServer.seCheckClientIDAndGet(aClientID: TffClientID;
  Var aClient: TfsSrcClient): TffResult;
Begin
  Result := DBIERR_FS_UnknownClient;
  Try
    If TObject(aClientID) Is TfsSrcClient Then
      Begin
        aClient := TfsSrcClient(aClientID);
        Result := DBIERR_NONE;
      End;
  Except
    { An exception may be raised if the ID is bogus.  Swallow the exception.}
  End;
End;
{--------}

Function TFSServer.CheckCursorIDAndGet(aCursorID: TffCursorID;
  Var aCursor: TfsSrBaseCursor): TffResult;
Begin
  If State <> fsesStarted Then
    Begin
      Result := DBIERR_FS_ServerUnavail;
      Exit;
    End;

  Result := seCheckCursorIDAndGet(aCursorID, aCursor);
  If Result = DBIERR_NONE Then
    Begin
      Result := DBIERR_FS_UnknownCursor;
      If aCursor.Activate Then
        Result := DBIERR_NONE;
    End;
End;
{--------}

Function TFSServer.seCheckCursorIDAndGet(aCursorID: TffCursorID;
  Var aCursor: TfsSrBaseCursor): TffResult;
Begin
  Result := DBIERR_FS_UnknownCursor;
  Try
    If TObject(aCursorID) Is TfsSrBaseCursor Then
      Begin
        aCursor := TfsSrBaseCursor(aCursorID);
        Result := DBIERR_NONE;
      End;
  Except
    { An exception may be raised if the ID is bogus.  Swallow the exception.}
  End;
End;
{--------}

Function TFSServer.CheckDatabaseIDAndGet(aDatabaseID: TffDatabaseID;
  Var aDatabase: TfsSrcDatabase): TffResult;
Begin
  If State <> fsesStarted Then
    Begin
      Result := DBIERR_FS_ServerUnavail;
      Exit;
    End;

  Result := seCheckDatabaseIDAndGet(aDatabaseID, aDatabase);
  If Result = DBIERR_NONE Then
    Begin
      Result := DBIERR_FS_UnknownDB;
      If aDatabase.Activate Then
        Result := DBIERR_NONE;
    End;
End;
{--------}

Function TFSServer.seCheckDatabaseIDAndGet(aDatabaseID: TffDatabaseID;
  Var aDatabase: TfsSrcDatabase): TffResult;
Begin
  Result := DBIERR_FS_UnknownDB;
  Try
    If TObject(aDatabaseID) Is TfsSrcDatabase Then
      Begin
        aDatabase := TfsSrcDatabase(aDatabaseID);
        Result := DBIERR_NONE;
      End;
  Except
    { An exception may be raised if the ID is bogus.  Swallow the exception.}
  End;
End;
{--------}

Function TFSServer.CheckTransactionIDAndGet(aTransactionID: TffTransID;
  Var aTrans: TfsSrcTransaction): TffResult;
Begin
  If State <> fsesStarted Then
    Begin
      Result := DBIERR_FS_ServerUnavail;
      Exit;
    End;

  Result := DBIERR_INVALIDHNDL;
  Try
    If TObject(aTransactionID) Is TfsSrcTransaction Then
      Begin
        aTrans := TfsSrcTransaction(aTransactionID);
        Result := DBIERR_NONE;
      End;
  Except
    { An exception may be raised if the ID is bogus.  Swallow the exception.}
  End;
End;
{Begin !!.11}
{--------}

Function TFSServer.ClientAdd(Var aClientID: TffClientID;
  Const aClientName: TffNetName;
  Const aUserID: TffName;
  Const aTimeout: Longint;
  Var aHash: TffWord32;
  Var aRights: TffUserRights;
  Var aSecurityEnabled: boolean): TffResult;
Begin
  Result := seClientAddPrim(aClientID, aClientName, aUserID, aTimeout,
    fsVersionNumber, aHash, aRights, aSecurityEnabled);
End;
{--------}

Function TFSServer.ClientAddEx(Var aClientID: TffClientID;
  Const aClientName: TffNetName;
  Const aUserID: TffName;
  Const aTimeout: Longint;
  Const aClientVersion: Longint;
  Var aHash: TffWord32;
  Var aRights: TffUserRights;
  Var aSecurityEnabled: boolean): TffResult;
Begin
  Result := seClientAddPrim(aClientID, aClientName, aUserID, aTimeout,
    aClientVersion, aHash, aRights, aSecurityEnabled);
End;
{--------}

Function TFSServer.seClientAddPrim(Var aClientID: TffClientID;
  Const aClientName: TffNetName;
  Const aUserID: TffName;
  Const aTimeout: Longint;
  Const aClientVersion: Longint;
  Var aHash: TffWord32;
  Var aRights: TffUserRights;
  Var aSecurityEnabled: boolean): TffResult;
Var
  aMonitor: TFSBaseEngineMonitor;
  anExtender: TFSBaseEngineExtender;
  anIndex, anMaxDupUser: Longint;
  MonitorList: TFSNormalList;
  NewClient, CheckClient: TfsSrcClient;
  User: TfsUserItem;
Begin
  FFSetRetry(aTimeout); { Probably not needed but let's do it just in case. }
  aClientID := fsc_NoClientID;
  aSecurityEnabled := seConfig.GeneralInfo^.giIsSecure;
  Try
    If seConfig.GeneralInfo^.giIsSecure And
      (seConfig.UserList.Count <> 0) Then
      Begin
        If Not seConfig.UserList.UserExists(aUserID) Then
          Begin
            Result := DBIERR_INVALIDUSRPASS;
            Exit;
          End;
        With seConfig.UserList Do
          Begin
            User := UserItem[UserIndex(aUserID)];
            aHash := PasswordHash[aUserID];
            aRights := User.Rights;
          End;
      End
    Else
      Begin
        User := Nil;
        aHash := 0;
      End;
    NewClient := TfsSrcClient.Create(aClientID, aClientName, aTimeout,
      aClientVersion, User, Self);

    { If there are any monitors interested in client then see if they
      are interested in this client. }
    MonitorList := GetInterestedMonitors(TfsSrcClient);
    If assigned(MonitorList) Then
      Begin
        Try
          For anIndex := 0 To pred(MonitorList.Count) Do
            Begin
              aMonitor := TFSBaseEngineMonitor
                (TfsIntListItem(MonitorList[anIndex]).KeyAsInt);
              Try
                anExtender := aMonitor.Interested(NewClient);
                If assigned(anExtender) Then
                  NewClient.AddClientExtender(anExtender);
              Except
                On E: Exception Do
                  seForce('Monitor [%s] exception, ClientAdd: %s', {!!.06 - Start}
                    [aMonitor.ClassName, E.message],
                    bseGetReadOnly); {!!.06 - End}
              End;
            End;
        Finally
          MonitorList.Free;
        End;
      End;

    { Now notify the extenders about the client.  If somebody complains
      then disallow the client. }
    Result := NewClient.NotifyExtenders(ffeaAfterCreateClient, ffeaNoAction);
    // count client
    If Result <> DBIERR_NONE Then
      Begin
        NewClient.Free;
        Exit;
      End
    Else
      Begin
        // Check for maxclients
        // if Admin then ignore - no admin rights
        If UpperCase(NewClient.ClientName) <> UpperCase(fsc_AdminUserID) Then
          If Configuration.GeneralInfo^.giMaxClients > 0 Then
            If ClientList.ClientCount + 1 > Configuration.GeneralInfo^.giMaxClients Then
              Begin
                NewClient.Free;
                Result := DBIERR_FS_UnknownClient;
                Exit;
              End;

        If Result <> DBIERR_NONE Then Exit;

        // Check if exists clients
        anMaxDupUser := 1;
        For anIndex := 0 To ClientList.ClientCount - 1 Do
          Begin
            CheckClient := ClientList.Client[ftFromIndex, anIndex];
            If AnsiUpperCase(CheckClient.ClientName) = AnsiUpperCase(NewClient.ClientName) Then
              Inc(anMaxDupUser);
          End;
        If Configuration.GeneralInfo^.giMaxDuplicateUsers >= 1 Then
          If anMaxDupUser > Configuration.GeneralInfo^.giMaxDuplicateUsers Then
            Begin
              NewClient.Free;
              Result := DBIERR_FS_UnknownClient;
              //Send Error
              //giSendErrorIfLoginDuplicateUser: Boolean;
              Exit;
            End;
        If Result <> DBIERR_NONE Then Exit;

        NewClient.Accepted := True;
        Try
          ClientList.BeginWrite;
          Try
            ClientList.AddClient(NewClient);
            seClientHash.Add(NewClient.ClientID, Nil); {!!.02}
          Finally
            ClientList.EndWrite;
          End;
          {add the default session to our session list}
          SessionList.BeginWrite;
          Try
            { Assumption: No need to lock NewClient.SessionList since
              we have not confirmed creation of client to the client. }
            SessionList.AddSession(NewClient.SessionList.Session[ftFromIndex, 0]);
          Finally
            SessionList.EndWrite;
          End;
        Except
          NewClient.Free;
          Raise;
        End; {try..except}
        aClientID := NewClient.ClientID;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Procedure TFSServer.seClientRemovePrim(Const aClient: TfsSrcClient);
Begin
  If aClient.CanClose(True) Then
    Begin
      ClientList.DeleteClient(aClient.ClientID);
      TableList.RemoveUnusedTables;
      FolderList.RemoveUnusedFolders;
      {If the server is waiting on us to finish, let it know we're
       done so it can move on.}
      If ((Assigned(seEvtClientDone)) And
        (ClientList.ClientCount = 0)) Then
        seEvtClientDone.SignalEvent;
    End
  Else
    aClient.RequestClose;
End;
{--------}

Function TFSServer.ClientRemove(aClientID: TffClientID): TffResult;
Var
  Client: TfsSrcClient;
Begin
  Try
    { Note: We lock the client list because we may have 2 threads trying to
      do a remove for the same client.  Thread A could be processing the
      RemoveClient request from the remote client while thread B could be
      processing a remote client hangup (i.e., initiated from transport level).}
    ClientList.BeginWrite;
    Try
      {Begin !!.02}
              { Is the client is listed in the hash table? }
      If Not seClientHash.Remove(aClientID) Then
        Begin
          { No. The client has already been removed. }
          Result := DBIERR_FS_UnknownClient;
          Exit;
        End;
      {End !!.02}

            { Find the client object.  Note that we will always get an exception on
              the 2nd removal request for each client. The exception is swallowed
              in seCheckClientIDAndGet. We get the exception because the client is
              already freed. We live with the exception because we don't want to
              pay the cost of doing a sequential scan through the list of clients.
              This could be onerous when hundreds of clients are connected to the
              server. }
      Result := seCheckClientIDAndGet(aClientID, Client);
      If Result = DBIERR_NONE Then
        Begin
          FFSetRetry(Client.Timeout);
          seClientRemovePrim(Client);
        End;
    Finally
      ClientList.EndWrite;
    End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.ClientSetTimeout(Const aClientID: TffClientID;
  Const aTimeout: Longint): TffResult;
Var
  Client: TfsSrcClient;
Begin
  Try
    Result := CheckClientIDAndGet(aClientID, Client);
    If Result = DBIERR_NONE Then
      Try
        Client.Timeout := aTimeout;
      Finally
        Client.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Procedure TFSServer.BCompress(Stream: TMemoryStream; aTgt: TfsSrBaseCursor;
  aTargetBLOBNr: TffInt64; aTargetCompress: TDataCompLevel);
Var
  aOutput: TMemoryStream;
Begin
  Case aTargetCompress Of
    blNone:
      Begin
        //                  ss
      End;
    blFastest:
      Begin
        aOutput := TMemoryStream.Create;
        Try
          aOutput.Position := 0;
          Stream.Position := 0;
          If Stream.Size > 0 Then
            fsZCompress(zcFastest, Stream, aOutput);
          aOutput.Position := 0;
          aTgt.BLOBWrite(aTargetBLOBNr, 0, aOutput.size, aOutput.memory^);
        Finally
          aOutput.free;
        End;
      End;
    blDefault:
      Begin
        aOutput := TMemoryStream.Create;
        Try
          aOutput.Position := 0;
          Stream.Position := 0;
          If Stream.Size > 0 Then
            fsZCompress(zcDefault, Stream, aOutput);
          aOutput.Position := 0;
          aTgt.BLOBWrite(aTargetBLOBNr, 0, aOutput.size, aOutput.memory^);
        Finally
          aOutput.free;
        End;
      End;
    blMax:
      Begin
        aOutput := TMemoryStream.Create;
        Try
          aOutput.Position := 0;
          Stream.Position := 0;
          If Stream.Size > 0 Then
            fsZCompress(zcMax, Stream, aOutput);
          aOutput.Position := 0;
          aTgt.BLOBWrite(aTargetBLOBNr, 0, aOutput.size, aOutput.memory^);
        Finally
          aOutput.free;
        End;
      End;
  End;
End;

Function TFSServer.seBLOBCopy(aSrc,
  aTgt: TfsSrBaseCursor;
  aSourceBLOBNr,
  aTargetBLOBNr: TffInt64;
  aBuffer: Pointer;
  aBufLen: Longint;
  aSourceCompress, aTargetCompress: TDataCompLevel): TffResult;
Var
  SourceLen: Longint;
  SegmentLen: Longint;
  BytesRead: TffWord32; {!!.06}
  Offset: Longint;
  FileName: TffFullFileName;
  Source, Target: TMemoryStream;

  Procedure CopyNormal;
  Begin
    Offset := 0;
    SegmentLen := FFMinI(aBufLen, SourceLen);
    While Offset < SourceLen Do
      Begin
        Result := aSrc.BLOBRead(aSourceBLOBNr, Offset, SegmentLen, aBuffer^,
          BytesRead);
        If Result <> DBIERR_NONE Then
          Exit;

        Result := aTgt.BLOBWrite(aTargetBLOBNr, Offset, BytesRead, aBuffer^);
        If Result <> DBIERR_NONE Then
          Exit;

        Inc(Offset, BytesRead);
      End;
  End;
Begin

  With aSrc.Table Do
    Begin

      { Assumption: Transaction has already been started by a calling routine. }

      { See if we have a file BLOB }
      If FFTblGetFileNameBLOB(Files[Dictionary.BLOBFileNumber],
        aSrc.Database.TransactionInfo,
        aSourceBLOBNr, FileName) Then
        Begin
          FFTblAddFileBLOB(Files[Dictionary.BLOBFileNumber],
            aSrc.Database.TransactionInfo,
            FileName, aTargetBLOBNr);
          Result := DBIERR_NONE;
        End
      Else
        Begin

          { Otherwise copy the BLOB in segments based on the size of the
            given transfer buffer }
          SourceLen := aSrc.BLOBGetLength(aSourceBLOBNr, Result);
          If Result <> DBIERR_NONE Then
            Exit;
          //aSourceCompress, aTargetCompress
          If aSourceCompress = aTargetCompress Then
            CopyNormal
          Else
            Begin
              If aSourceCompress <> blNone Then
                Begin
                  Source := TMemoryStream.Create;
                  Target := TMemoryStream.Create;
                  Try
                    Source.position := 0;
                    Target.position := 0;
                    Offset := 0;
                    SegmentLen := FFMinI(aBufLen, SourceLen);
                    While Offset < SourceLen Do
                      Begin
                        Result := aSrc.BLOBRead(aSourceBLOBNr, Offset, SegmentLen, aBuffer^,
                          BytesRead);
                        If Result <> DBIERR_NONE Then
                          Exit;
                        If BytesRead > 0 Then
                          Source.writebuffer(abuffer^, BytesRead);
                        Inc(Offset, BytesRead);
                      End; { while }

                    If SourceLen > 0 Then
                      Begin
                        Source.position := 0;
                        Target.Position := 0;
                        fsZDecompress(Source, Target);
                        Source.position := 0;
                        Target.position := 0;
                        If aTargetCompress <> blNone Then
                          Begin
                            If Target.Size = 0 Then
                              Result := aTgt.BLOBWrite(aTargetBLOBNr, 0, 0, Target.memory^)
                            Else
                              BCompress(Target, aTgt, aTargetBLOBNr, aTargetCompress);
                          End
                        Else
                          Begin
                            If Target.Size = 0 Then
                              Result := aTgt.BLOBWrite(aTargetBLOBNr, 0, 0, Target.memory^)
                            Else
                              Result := aTgt.BLOBWrite(aTargetBLOBNr, 0, Target.size, Target.memory^);
                          End;
                      End;
                  Finally
                    Source.free;
                    Target.free;
                  End;
                End
              Else
                Begin //aSourceCompress = blNone
                  If aTargetCompress <> blNone Then
                    Begin
                      Source := TMemoryStream.Create;
                      Try
                        Source.position := 0;
                        Offset := 0;
                        SegmentLen := FFMinI(aBufLen, SourceLen);
                        While Offset < SourceLen Do
                          Begin
                            Result := aSrc.BLOBRead(aSourceBLOBNr, Offset, SegmentLen, aBuffer^,
                              BytesRead);
                            If Result <> DBIERR_NONE Then
                              Exit;
                            If BytesRead > 0 Then
                              Source.writebuffer(abuffer^, BytesRead);
                            Inc(Offset, BytesRead);
                          End; { while }

                        Source.position := 0;
                        If SourceLen > 0 Then
                          BCompress(Source, aTgt, aTargetBLOBNr, aTargetCompress)
                        Else
                          Result := aTgt.BLOBWrite(aTargetBLOBNr, 0, Source.size, Source.memory^);
                      Finally
                        Source.free;
                      End;
                    End
                  Else // target, source = none
                    CopyNormal;
                End;
            End;
        End;
    End; { with }
End;
{--------}

Function TFSServer.SessionAdd(Const aClientID: TffClientID;
  Const timeout: Longint;
  Var aSessionID: TffSessionID;
  aData: Pointer;
  aDataLength: Longint = 0): TffResult;
Var
  Client: TfsSrcClient;
  Session: TfsSrcSession;
  Stream: TMemoryStream;
  tl: TStringList;
  i, j, k: Integer;

  Function fsReadString(aStream: TStream): String;
  Var
    s: String;
    n: Integer;
  Begin
    Result := '';
    Stream.Read(n, 4);
    If n > 0 Then
      Begin
        SetLength(s, n);
        Stream.Read(s[1], n);
        Result := s;
      End;
  End;
Begin
  Try
    Result := CheckClientIDAndGet(aClientID, Client);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Client.Timeout); { Just in case }
        SessionList.BeginWrite;
        Try
          // add password list to session
          tl := TStringList.Create;
          If aDataLength > 0 Then
            Begin
              Stream := TMemoryStream.Create;
              Try
                Stream.WriteBuffer(aData^, aDataLength);
                Stream.position := 0;
                Stream.Read(j, 4);
                tl.BeginUpdate;
                If j > 0 Then
                  For i := 0 To j - 1 Do
                    Begin
                      Stream.Read(k, 4);
                      tl.add(IntToStr(k));
                    End;
                tl.EndUpdate;
              Finally
                Stream.free;
              End;
            End;

          If Assigned(Client.clFirstSession) Then
            Begin
              Session := Client.clFirstSession;
              Session.TablePasswordList.Assign(tl);
              Client.clFirstSession := Nil;
              // add table password
            End
          Else
            Begin
              Session := TfsSrcSession.Create(Client, False, timeout);
              Session.TablePasswordList.Assign(tl);
              SessionList.AddSession(Session);
            End;
          {End !!.03}
        Finally
          SessionList.EndWrite;
          tl.free;
        End;
        aSessionID := Session.SessionID;
      Finally
        Client.Deactivate;
      End
    Else
      aSessionID := 0;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{Begin !!.06}
{--------}

Function TFSServer.SessionCloseInactiveTables(aClientID: TffClientID): TffResult;
Var
  Client: TfsSrcClient;
Begin
  Try
    Result := CheckClientIDAndGet(aClientID, Client);
    If (Result = DBIERR_NONE) Then
      Try
        TableList.RemoveUnusedTAbles;
      Finally
        Client.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.SessionCount(aClientID: TffClientID;
  Var aCount: Integer): TffResult;
Var
  Client: TfsSrcClient;
Begin
  Try
    Result := CheckClientIDAndGet(aClientID, Client);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Client.Timeout); { Just in case }
        Client.SessionList.BeginRead;
        Try
          aCount := Client.SessionList.SessionCount
        Finally
          Client.SessionList.EndRead;
        End
      Finally
        Client.Deactivate;
      End
    Else
      aCount := 0;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.SessionGetCurrent(aClientID: TffClientID;
  Var aSessionID: TffSessionID): TffResult;
Var
  Client: TfsSrcClient;
  aSession: TfsSrcSession;
Begin
  Try
    aSessionID := 0;
    Result := CheckClientIDAndGet(aClientID, Client);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Client.Timeout); { just in case }
        Client.SessionList.BeginRead;
        Try
          aSession := Client.SessionList.CurrentSession;
        Finally
          Client.SessionList.EndRead;
        End;
        If assigned(aSession) Then
          aSessionID := aSession.SessionID;
      Finally
        Client.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.SessionRemove(aClientID: TffClientID;
  aSessionID: TffSessionID): TffResult;
Var
  Session: TfsSrcSession;
Begin
  Try
    Result := seCheckSessionIDAndGet(aSessionID, Session);
    If (Result = DBIERR_NONE) Then
      Begin
        FFSetRetry(Session.Timeout); { just in case }
        If Session.CanClose(True) Then
          Begin
            Session.Free;
            TableList.RemoveUnusedTables;
          End
        Else
          Session.RequestClose;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.SessionSetCurrent(aClientID: TffClientID;
  aSessionID: TffSessionID): TffResult;
Var
  Client: TfsSrcClient;
  aSession: TfsSrcSession;
Begin
  Try
    Result := CheckSessionIDAndGet(aClientID, aSessionID, Client, aSession);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Client.Timeout); { just in case }
        Client.SessionList.BeginWrite;
        Try
          Client.SessionList.CurrentSession := aSession;
        Finally
          Client.SessionList.EndWrite;
        End;
      Finally
        Client.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.SessionSetTimeout(Const aClientID: TffClientID;
  Const aSessionID: TffSessionID;
  Const aTimeout: Longint): TffResult;
Var
  Client: TfsSrcClient;
  Session: TfsSrcSession;
Begin
  Try
    Result := CheckSessionIDAndGet(aClientID, aSessionID, Client, Session);
    If Result = DBIERR_NONE Then
      Try
        Session.Timeout := aTimeout;
      Finally
        Client.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.CursorClone(aCursorID: TffCursorID;
  aOpenMode: TffOpenMode;
  Var aNewCursorID: TffCursorID)
  : TffResult;

Var
  aCursor, {!!.03}
  aNewCursor: TfsSrBaseCursor; {!!.03}
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, aCursor);
    If (Result = DBIERR_NONE) Then
      Begin {!!.06 - Start}
        FFSetRetry(aCursor.Timeout);
        aNewCursor := aCursor.CloneCursor(aOpenMode); {!!.03}
        CursorList.BeginWrite;
        Try
          CursorList.AddCursor(aNewCursor); {!!.03}
          aNewCursorID := aNewCursor.CursorID; {!!.03}
        Finally
          CursorList.EndWrite;
          aCursor.Deactivate;
        End; { try..finally }
      End; { if } {!!.06 - End}
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.CursorClose(aCursorID: TffCursorID): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := seCheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Begin
        FFSetRetry(Cursor.Timeout);
        If Cursor.CanClose(True) Then
          Cursor.Free
        Else
          Cursor.RequestClose;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.CursorCompareBookmarks(aCursorID: TffCursorID;
  aBookmark1,
  aBookmark2: PffByteArray;
  Var aCompResult: Longint): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
        If Result = DBIERR_NONE Then
          Result := Cursor.CompareBookmarks(aBookmark1, aBookmark2, aCompResult);
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{Begin !!.02}
{--------}

Function TFSServer.CursorCopyRecords(aSrcCursorID,
  aDestCursorID: TffCursorID;
  aCopyBLOBs: Boolean; CountPerTrans: Longint): TffResult;
Var
  aBLOBCopyMode: TffBLOBCopyMode;
  SrcCursor,
    DestCursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aSrcCursorID, SrcCursor);
    { If the extenders object, we can't continue. }
    If (Result = DBIERR_NONE) Then
      Try
        Result := CheckCursorIDAndGet(aDestCursorID, DestCursor);
        If (Result = DBIERR_NONE) Then
          Try
            FFSetRetry(DestCursor.Timeout); {!!.10}
            If aCopyBLOBs Then
              aBLOBCopyMode := ffbcmCopyFull
            Else
              aBLOBCopyMode := ffbcmNoCopy;
            Result := DestCursor.CopyRecords(SrcCursor, aBLOBCopyMode, Nil,
              0, 0, CountPerTrans);
          Finally
            DestCursor.Deactivate;
          End;
      Finally
        SrcCursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{End !!.02}
{Begin !!.06}
{--------}

Function TFSServer.CursorDeleteRecords(aCursorID: TffCursorID; CountPerTrans: Longint): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout); {!!.10}
        Result := Cursor.DeleteRecords(CountPerTrans);
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{End !!.02}
{--------}

Function TFSServer.CursorGetBookmark(aCursorID: TffCursorID;
  aBookmark: PffByteArray): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout); { just in case }
        Result := Cursor.GetBookmark(aBookmark);
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.CursorGetBookmarkSize(aCursorID: TffCursorID;
  Var aSize: Integer): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout); { just in case }
        aSize := Cursor.GetBookmarkSize;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{Begin !!.03}
{--------}

Function TFSServer.CursorListBLOBFreeSpace(aCursorID: TffCursorID;
  Const aInMemory: Boolean;
  aStream: TStream): TffResult;
Var
  Cursor: TfsSrBaseCursor;
  StartedTrans: Boolean;
  TransID: TffTransID;
Begin
  StartedTrans := False;
  Try
    {get the cursor}
    Result := CheckCursorIDAndGet(aCursorID, Cursor);

    If (Result = DBIERR_NONE) Then
      Try
        If Cursor.Database.Transaction = Nil Then
          Begin
            Result := seTransactionStart(Cursor.Database, False,
              fscl_TrImplicit, TransID);
            StartedTrans := (Result = DBIERR_NONE);
          End;
        FFSetRetry(Cursor.Timeout);
        Cursor.ListBLOBFreeSpace(Cursor.Database.TransactionInfo, aInMemory,
          aStream);
      Finally
        If StartedTrans Then
          seTransactionRollback(Cursor.Database);
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{End !!.03}
{--------}

Function TFSServer.CursorOverrideFilter(aCursorID: Longint;
  aExpression: pCANExpr;
  aTimeout: TffWord32): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    {get the cursor}
    Result := CheckCursorIDAndGet(aCursorID, Cursor);

    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.OverrideFilter(aExpression, aTimeout);
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.CursorResetRange(aCursorID: TffCursorID): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        If (Cursor.IndexID = 0) Then
          Result := DBIERR_NOASSOCINDEX
        Else
          Begin
            FFSetRetry(Cursor.Timeout); { just in case }
            Cursor.ResetRange;
          End;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.CursorRestoreFilter(aCursorID: Longint): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    {get the cursor}
    Result := CheckCursorIDAndGet(aCursorID, Cursor);

    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.RestoreFilter;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.CursorSetFilter(aCursorID: TffCursorID;
  aExpression: pCANExpr;
  aTimeout: TffWord32): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    {get the cursor}
    Result := CheckCursorIDAndGet(aCursorID, Cursor);

    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        If aExpression^.iTotalSize <= SizeOf(CANExpr) Then {!!.01}
          aExpression := Nil; {!!.01}
        Result := Cursor.SetFilter(aExpression, aTimeout);
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.CursorSetRange(aCursorID: TffCursorID;
  aDirectKey: boolean;
  aFieldCount1: Integer;
  aPartialLen1: Integer;
  aKeyData1: PffByteArray;
  aKeyIncl1: boolean;
  aFieldCount2: Integer;
  aPartialLen2: Integer;
  aKeyData2: PffByteArray;
  aKeyIncl2: boolean): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        If (Cursor.IndexID = 0) Then
          Result := DBIERR_NOASSOCINDEX
        Else
          Begin
            FFSetRetry(Cursor.Timeout); { just in case }
            Result := Cursor.SetRange(aDirectKey,
              aFieldCount1, aPartialLen1, aKeyData1, aKeyIncl1,
              aFieldCount2, aPartialLen2, aKeyData2, aKeyIncl2);
          End;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.CursorSetTimeout(Const aCursorID: TffCursorID;
  Const aTimeout: Longint): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If Result = DBIERR_NONE Then
      Try
        Cursor.Timeout := aTimeout;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.CursorSetToBegin(aCursorID: TffCursorID): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Cursor.SetToBegin;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.CursorSetToBookmark(aCursorID: TffCursorID;
  aBookmark: PffByteArray): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.SetToBookmark(aBookmark);
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.CursorSetToCursor(aDestCursorID: TffCursorID; aSrcCursorID: TffCursorID): TffResult;
Var
  DestCursor: TfsSrBaseCursor;
  SrcCursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aDestCursorID, DestCursor);
    If (Result = DBIERR_NONE) Then
      Try
        Result := seCheckCursorIDAndGet(aSrcCursorID, SrcCursor);
        { We call the primitive seCheckCursorIDAndGet here because
          the client was just locked by the call to get the destination
          cursor. }
        If (Result = DBIERR_NONE) Then
          Begin
            FFSetRetry(DestCursor.Timeout);
            Result := DestCursor.SetToCursor(SrcCursor);
          End;
      Finally
        DestCursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.CursorSetToEnd(aCursorID: TffCursorID): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Cursor.SetToEnd;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.CursorSetToKey(aCursorID: TffCursorID;
  aSearchAction: TffSearchKeyAction;
  aDirectKey: boolean;
  aFieldCount: Integer;
  aPartialLen: Integer;
  aKeyData: PffByteArray): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        If (Cursor.IndexID = 0) Then
          Result := DBIERR_NOASSOCINDEX
        Else
          Begin
            FFSetRetry(Cursor.Timeout);
            Result := Cursor.SetToKey(aSearchAction, aDirectKey,
              aFieldCount, aPartialLen, aKeyData);
          End;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.CursorSwitchToIndex(aCursorID: TffCursorID;
  aIndexName: TffDictItemName;
  aIndexID: Integer;
  aPosnOnRec: boolean): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    {get the cursor}
    Result := CheckCursorIDAndGet(aCursorID, Cursor);

    If (Result = DBIERR_NONE) Then
      Try
        {validate the index information; if the index name is non-blank
         it must exist and will supercede the index number; if the index
         name is blank the index number must exist}
        If (aIndexName <> '') Then
          Begin
            aIndexID := Cursor.Table.Dictionary.GetIndexFromName(aIndexName);
            If (aIndexID = -1) Then
              Result := DBIERR_NOSUCHINDEX;
          End
        Else If (0 > aIndexID) Or
          (aIndexID >= Cursor.Table.Dictionary.IndexCount) Then
          Result := DBIERR_NOSUCHINDEX;

        {switch indexes}
        If (Result = DBIERR_NONE) Then
          If (aIndexID <> Cursor.IndexID) Then
            Begin
              FFSetRetry(Cursor.Timeout);
              Result := Cursor.SwitchToIndex(aIndexID, aPosnOnRec);
            End;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.DatabaseAddAlias(Const aAlias: TffName;
  Const aPath: TffPath;
  aCheckSpace: Boolean; {!!.11}
  Const aClientID: TffClientID)
  : TffResult;
Var
  Client: TfsSrcClient;
Begin
  Try
    Result := CheckClientIDAndGet(aClientID, Client);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Client.Timeout);
        Result := Client.NotifyExtenders(ffeaBeforeDBInsert, ffeaDBInsertFail);
        If (Result = DBIERR_NONE) Then
          Begin
            seConfig.AliasList.BeginWrite;
            Try
              Result := seDatabaseAddAliasPrim(aAlias, aPath, aCheckSpace);
            Finally
              seConfig.AliasList.EndWrite;
            End;
            If (Result = DBIERR_NONE) Then
              Result := SaveConfiguration
            Else
              Client.NotifyExtenders(ffeaDBInsertFail, ffeaNoAction);
          End;
      Finally
        Client.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.seDatabaseAliasListPrim(aList: TList): TffResult;
Var
  Inx: Integer;
  AliasItem: TfsAliasItem;
  TempDescr: PffAliasDescriptor;
Begin
  { Assumption: Thread-safeness enforced at a higher level. }
  Result := DBIERR_NONE;
  For Inx := 0 To pred(seConfig.AliasList.Count) Do
    Begin
      FFGetMem(TempDescr, sizeOf(TffAliasDescriptor));
      AliasItem := seConfig.AliasList[Inx];
      With AliasItem Do
        Begin
          TempDescr^.adAlias := KeyAsStr;
          TempDescr^.adPath := Path;
        End;
      aList.add(TempDescr);
    End;
End;
{--------}

Function TFSServer.DatabaseAliasList(aList: TList;
  aClientID: TffClientID): TffResult;
Var
  Client: TfsSrcClient;
Begin
  Try
    Result := CheckClientIDandGet(aClientID, Client);
    If Result = DBIERR_NONE Then
      Try
        FFSetRetry(Client.Timeout);
        Result := Client.NotifyExtenders(ffeaBeforeDBRead, ffeaNoAction);
        If Result = DBIERR_NONE Then
          Begin
            seConfig.AliasList.BeginRead;
            Try
              Result := seDatabaseAliasListPrim(aList);
            Finally
              seConfig.AliasList.EndRead;
            End;
          End; { if }
      Finally
        Client.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.RecoveryAliasList(aList: TList;
  aClientID: TffClientID): TffResult;
Var
  Client: TfsSrcClient;
Begin
  Try
    Result := seCheckClientIDandGet(aClientID, Client);
    If Result = DBIERR_NONE Then
      Begin
        FFSetRetry(Client.Timeout);
        Result := Client.NotifyExtenders(ffeaBeforeDBRead, ffeaNoAction);
        If Result = DBIERR_NONE Then
          Begin
            seConfig.AliasList.BeginRead;
            Try
              Result := seDatabaseAliasListPrim(aList);
            Finally
              seConfig.AliasList.EndRead;
            End;
          End; { if }
      End; { if }
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.DatabaseChgAliasPath(aAlias: TffName;
  aNewPath: TffPath;
  aCheckSpace: Boolean; {!!.11}
  aClientID: TffClientID)
  : TffResult;
Var
  Client: TfsSrcClient;
Begin
  Try
    Result := CheckClientIDandGet(aClientID, Client);
    If Result = DBIERR_NONE Then
      Try
        FFSetRetry(Client.Timeout);
        {check whether the alias exists}
        Result := Client.NotifyExtenders(ffeaBeforeChgAliasPath, ffeaNoAction);
        If Result = DBIERR_NONE Then
          Begin
            seConfig.AliasList.BeginWrite;
            Try
              If Not seConfig.AliasList.AliasExists(aAlias) Then
                Begin
                  Result := DBIERR_UNKNOWNDB;
                  Exit;
                End;
              {delete the old alias}
              seConfig.AliasList.DeleteAlias(aAlias);

              {add the Alias again and its new path}
              seConfig.AddAlias(aAlias, aNewPath, aCheckSpace); {!!.11}
            Finally
              seConfig.AliasList.EndWrite;
            End;

            If Result = DBIERR_NONE Then
              Result := SaveConfiguration;
          End;
      Finally
        Client.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.DatabaseClose(aDatabaseID: TffDatabaseID): TffResult;
Var
  DB: TfsSrcDatabase;
Begin
  Try
    Result := seCheckDatabaseIDAndGet(aDatabaseID, DB);
    If (Result = DBIERR_NONE) Then
      Begin
        FFSetRetry(DB.Timeout);
        { We can free the database if there are no open cursors
          & if the database is not active.
          Note: We are protected by the TableOpen method's behavior.
                If a table is in the process of being opened
                then DB's state will be ffosActive & we won't free the
                database. }
        If DB.CanClose(True) Then
          Begin
            DB.Free;
            TableList.RemoveUnusedTables;
          End
        Else
          DB.RequestClose;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.seDatabaseDeleteAliasPrim(aAlias: TffName): TffResult;
Begin
  { Assumption: Thread-safeness enforced at a higher level. }
  Result := DBIERR_NONE;

  { Does the alias exist? }
  If Not seConfig.AliasList.AliasExists(aAlias) Then
    { No.  Notify client. }
    Result := DBIERR_UNKNOWNDB
  Else
    { Delete the old alias}
    seConfig.AliasList.DeleteAlias(aAlias);
End;
{--------}

Function TFSServer.DatabaseDeleteAlias(aAlias: TffName;
  aClientID: TffClientID): TffResult;
Var
  Client: TfsSrcClient;
Begin
  Try
    Result := CheckClientIDandGet(aClientID, Client);
    If Result = DBIERR_NONE Then
      Try
        FFSetRetry(Client.Timeout);
        Result := Client.NotifyExtenders(ffeaBeforeDBDelete, ffeaDBDeleteFail);
        If Result = DBIERR_NONE Then
          Begin
            seConfig.AliasList.BeginWrite;
            Try
              Result := seDatabaseDeleteAliasPrim(aAlias);
            Finally
              seConfig.AliasList.EndWrite;
            End;
            If Result = DBIERR_NONE Then
              Result := SaveConfiguration
            Else
              Client.NotifyExtenders(ffeaDBDeleteFail, ffeaNoAction);
          End; { if }
      Finally
        Client.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.DatabaseOpen(aClientID: TffClientID;
  Const aAlias: TffName;
  Const aOpenMode: TffOpenMode;
  Const aShareMode: TffShareMode;
  Const aTimeout: Longint;
  Const aTransIsolation: TfsTransIsolation;
  Const aRecLocking: TfsDataBaseRecLocking;
  Var aDatabaseID: TffDatabaseID)
  : TffResult;
Var
  aDatabase: TfsSrcDatabase;
  aSession: TfsSrcSession;
  Folder: TfsSrcFolder;
  Client: TfsSrcClient;
  DB: TfsSrcDatabase;
  UNCPath: TffPath;
  CheckSpace: Boolean; {!!.11}
Begin
  aDatabase := Nil;
  Folder := Nil;
  Result := CheckClientIDAndGet(aClientID, Client);
  If (Result <> DBIERR_NONE) Then
    Exit;

  Try
    {the client must exist}
    If UpperCase(Client.ClientName) <> UpperCase(fsc_AdminUserID) Then
      If Configuration.GeneralInfo^.giMaxDbOpen > 0 Then
        If Self.DatabaseList.DatabaseCount + 1 > Configuration.GeneralInfo^.giMaxDbOpen Then
          Begin
            Result := DBIERR_DATABASEOPENFAILED;
            Try
              Client.Deactivate;
            Except
            End;
            Exit;
          End;

    Try
      FFSetRetry(Client.Timeout);
      Result := Client.NotifyExtenders(ffeaBeforeDBRead, ffeaNoAction);

      If Result = DBIERR_NONE Then
        Begin
          {get the current session}
          Client.SessionList.BeginRead;
          Try
            aSession := Client.SessionList.CurrentSession;
          Finally
            Client.SessionList.EndRead;
          End;
          {check to see whether the Alias exists}
          seConfig.AliasList.BeginRead;
          Try
            If Not seConfig.AliasList.AliasExists(aAlias) Then
              Begin
                Result := DBIERR_UNKNOWNDB;
                Exit;
              End;
            {get the Alias path}
            UNCPath := seConfig.AliasList.Path[aAlias];
            CheckSpace := seConfig.AliasList.CheckDiskSpace(aAlias); {!!.11}
          Finally
            seConfig.AliasList.EndRead;
          End;
          {check to see whether the directory exists}
          If Not FFDirectoryExists(UNCPath) Then
            Begin
              Result := DBIERR_INVALIDDIR;
              Exit;
            End;
          {get a path id for this path}
          FolderList.BeginWrite;
          Try
            Folder := FolderList.AddFolder(UNCPath, IsReadOnly, seBufMgr);
          Finally
            FolderList.EndWrite;
          End;
          UNCPath := Folder.Path;

          {check to see whether this Alias has already been opened and in
           a non-compatible state (ie we or some other client/session
           wants it opened exclusively)}
          DatabaseList.BeginWrite;
          Try
            DB := DatabaseList.GetDatabaseForFolder(Folder);
            If assigned(DB) Then
              Begin
                If ((DB.ShareMode = smExclusive) Or (aShareMode = smExclusive)) And
                  ((TfsSrcClient(DB.Client).ClientID <> aClientID) Or
                  (DB.Session <> aSession)) Then
                  Begin
                    Result := DBIERR_NEEDEXCLACCESS;
                    Exit;
                  End;
              End;
            {create a new database object, add it to the global list}
            aDatabase := seDatabaseOpenPrim(aSession,
              Folder,
              aAlias,
              aOpenMode,
              aShareMode,
              aTimeout,
              CheckSpace,
              aTransIsolation,
              aRecLocking); {!!.11}
            aDatabaseID := aDatabase.DatabaseID;
          Finally
            DatabaseList.EndWrite;
          End;
        End;
    Finally
      Client.Deactivate;
    End;
  Except
    On E: Exception Do
      Begin
        If (aDatabase <> Nil) Then
          aDatabase.Free
        Else {aDatabase was never created}  If (Folder <> Nil) Then
          Begin
            FolderList.BeginWrite;
            Try
              FolderList.DeleteFolderByID(Folder.FolderID);
            Finally
              FolderList.EndWrite;
            End;
          End;
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.DatabaseOpenNoAlias(aClientID: TffClientID;
  Const aPath: TffPath;
  Const aOpenMode: TffOpenMode;
  Const aShareMode: TffShareMode;
  Const aTimeout: Longint;
  Const aTransIsolation: TfsTransIsolation;
  Const aRecLocking: TfsDataBaseRecLocking;
  Var aDatabaseID: TffDatabaseID)
  : TffResult;
Var
  aDatabase: TfsSrcDatabase;
  anAlias: TffName;
  aSession: TfsSrcSession;
  Folder: TfsSrcFolder;
  Client: TfsSrcClient;
  DatabaseExists: Boolean;
  DB: TfsSrcDatabase;
  UNCPath: TffPath;
  CheckSpace: Boolean; {!!.11}
Begin
  aDatabase := Nil;
  Folder := Nil;
  Try
    { The path cannot be empty. }
    If (aPath = '') Then
      Begin
        Result := DBIERR_INVALIDDIR;
        Exit;
      End;

    { The client must exist. }
    Result := CheckClientIDAndGet(aClientID, Client);
    If (Result <> DBIERR_NONE) Then
      Exit;

    Try
      FFSetRetry(Client.Timeout);
      Result := Client.NotifyExtenders(ffeaBeforeDBRead, ffeaNoAction);

      If Result = DBIERR_NONE Then
        Begin
          {get the current session}
          Client.SessionList.BeginRead;
          Try
            aSession := Client.SessionList.CurrentSession;
          Finally
            Client.SessionList.EndRead;
          End;
          {check to see whether the directory exists}
          If Not FFDirectoryExists(aPath) Then
            Begin
              Result := DBIERR_INVALIDDIR;
              Exit;
            End;
          {get a folder for this path}
          FolderList.BeginWrite;
          Try
            Folder := FolderList.AddFolder(aPath, IsReadOnly, seBufMgr);
          Finally
            FolderList.EndWrite;
          End;
          UNCPath := Folder.Path;
          {check to see whether this path has already been opened and in
           a non-compatible state (ie we or some other client/session
           wants it opened exclusively)}
          anAlias := '';
          CheckSpace := True; {!!.11}
          DatabaseList.BeginWrite;
          Try
            DB := DatabaseList.GetDatabaseForFolder(Folder);
            DatabaseExists := assigned(DB);
            If DatabaseExists Then
              Begin
                CheckSpace := DB.CheckSpace; {!!.11}
                If ((DB.ShareMode = smExclusive) Or (aShareMode = smExclusive)) And
                  ((TfsSrcClient(DB.Client).ClientID <> aClientID) Or
                  (DB.Session <> aSession)) Then
                  Begin
                    Result := DBIERR_NEEDEXCLACCESS;
                    Exit;
                  End;
                anAlias := DB.Alias;
              End;
            { Create a new database object, add it to the global list. }
            aDatabase := seDatabaseOpenPrim(aSession,
              Folder,
              anAlias,
              aOpenMode,
              aShareMode,
              aTimeout,
              CheckSpace,
              aTransIsolation,
              aRecLocking); {!!.11}
            aDatabaseID := aDatabase.DatabaseID;
          Finally
            DatabaseList.EndWrite;
          End;
        End;
    Finally
      Client.Deactivate;
    End;
  Except
    On E: Exception Do
      Begin
        If assigned(aDatabase) Then
          aDatabase.Free
        Else {database was never created}  If (Folder <> Nil) Then
          Begin
            FolderList.BeginWrite;
            Try
              FolderList.DeleteFolderByID(Folder.FolderID);
            Finally
              FolderList.EndWrite;
            End;
          End;
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.DatabaseSetTimeout(Const aDatabaseID: TffDatabaseID;
  Const aTimeout: Longint): TffResult;
Var
  DB: TfsSrcDatabase;
Begin
  Try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If Result = DBIERR_NONE Then
      Try
        DB.Timeout := aTimeout;
      Finally
        DB.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.DatabaseTableExists(aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName;
  Var aExists: Boolean): TffResult;
Var
  DB: TfsSrcDatabase;
  SearchPath: TffPath;
Begin
  Try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If (Result <> DBIERR_NONE) Then
      Exit;

    Try
      FFSetRetry(DB.Timeout);
      Result := DB.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);

      If Result = DBIERR_NONE Then
        Begin
          SearchPath := DB.Folder.Path;
          If (SearchPath[length(SearchPath)] <> '\') Then
            FFShStrAddChar(SearchPath, '\');
          aExists := FFFileExists(SearchPath + FFForceExtension(aTableName, fsc_ExtForData));
        End;

    Finally
      DB.Deactivate;
    End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}

End;
{Begin !!.11}
{--------}

Function TFSServer.seTableExistsPrim(aDB: TfsSrcDatabase;
  Const aTableName: TfsTableName): Boolean;
Var
  SearchPath: TffPath;
Begin
  { The table name must be a valid file name without extension. }
  If Not FFVerifyFileName(aTableName) Then
    FSRaiseException(EfsException, fsStrResServer,
      fserrInvalidTableName, [aTableName]);

  SearchPath := aDB.Folder.Path;
  If (SearchPath[length(SearchPath)] <> '\') Then
    FFShStrAddChar(SearchPath, '\');
  Result := FFFileExists(SearchPath +
    FFForceExtension(aTableName, fsc_ExtForData));
End;
{End !!.11}
{--------}

Function TFSServer.DatabaseTableList(aDatabaseID: TffDatabaseID;
  Const aMask: TffFileNameExt;
  aList: TList): TffResult;
Var
  DB: TfsSrcDatabase;
  FindRes: Integer;
  TableDesc: PffTableDescriptor;
  SearchRec: TffSearchRec;
  SearchMask: TffPath;
Begin
  Try

    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If (Result <> DBIERR_NONE) Then
      Exit;

    Try
      FFSetRetry(DB.Timeout);
      Result := DB.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);

      If Result = DBIERR_NONE Then
        Begin
          SearchMask := DB.Folder.Path;
          If (SearchMask[length(SearchMask)] <> '\') Then
            FFShStrAddChar(SearchMask, '\');
          If (aMask = '') Then
            Begin
              FFShStrConcat(SearchMask, '*.');
              FFShStrConcat(SearchMask, fsc_ExtForData);
            End
          Else
            Begin {BEGIN !!.01}
              FFShStrConcat(SearchMask, aMask);
              {$IFDEF OnlyRetrieveTables}
              FFForceExtension(SearchMask, fsc_ExtForData);
              {$ENDIF}
            End; {END !!.01}
          FindRes := FFFindFirst(SearchMask, [ditFile], diaAnyAttr, SearchRec);
          While (FindRes = 0) Do
            Begin
              FFGetMem(TableDesc, sizeOf(TfsTableDescriptor));
              With SearchRec Do
                Begin
                  TableDesc^.tdTableName := FFExtractFileName(srName);
                  TableDesc^.tdExt := FFExtractExtension(srName);
                  TableDesc^.tdSizeLo := srSize;
                  TableDesc^.tdSizeHi := srSizeHigh;
                  TableDesc^.tdTimeStamp := srTime;
                End;
              aList.Add(TableDesc);
              FindRes := FFFindNext(SearchRec);
            End;
          FFFindClose(SearchRec);
        End;
    Finally
      DB.Deactivate;
    End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.DatabaseTableLockedExclusive(aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName;
  Var aLocked: Boolean): TffResult;
Var
  DB: TfsSrcDatabase;
  Table: TfsSrcBaseTable;
Begin
  aLocked := False;
  Try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If (Result <> DBIERR_NONE) Then
      Exit;

    Try
      FFSetRetry(DB.Timeout);
      Result := DB.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);

      Table := GetTableInstance(DB.Folder, aTableName);

      { Is the table open? }
      If Assigned(Table) Then
        aLocked := Table.Folder.LockMgr.TableLockGranted(Table.TableID) = ffsltExclusive;
    Finally
      DB.Deactivate;
    End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.GetTableInstance(aFolder: TfsSrcFolder;
  Const aTableName: TfsTableName): TfsSrcBaseTable;
Var
  Inx: Integer;
Begin
  { Assumption: Calling routine has locked TableList appropriately. }
  For Inx := 0 To pred(TableList.TableCount) Do
    Begin
      Result := TableList[ftFromIndex, Inx];
      With Result Do
        If (Folder = aFolder) And
          (FFCmpShStrUC(BaseName, aTableName, 255) = 0) Then
          Exit;
    End;
  Result := Nil;
End;
{--------}

Function TFSServer.IndexClear(aCursorID: TffCursorID): TffResult;
{Restructured !!.01}
Var
  Cursor: TfsSrBaseCursor;
  StartedTrans: boolean;
  TransID: TffTransID;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If (Result = DBIERR_NONE) Then
    Try
      StartedTrans := False;
      Try
        FFSetRetry(Cursor.Timeout);
        { Make sure a read-only transaction is active. }
        If Not assigned(Cursor.Database.Transaction) Then
          Begin
            Result := seTransactionStart(Cursor.Database, False,
              fscl_TrImplicit, TransID);
            StartedTrans := (Result = DBIERR_NONE);
          End;
        If Result = DBIERR_NONE Then
          Begin
            Cursor.ClearIndex;
            If StartedTrans Then
              seTransactionCommit(Cursor.Database);
          End;
      Except
        On E: Exception Do
          Begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            If StartedTrans Then
              seTransactionRollback(Cursor.Database);
          End;
      End; {try..except}
    Finally
      Cursor.Deactivate;
    End;
End;
{--------}

Function TFSServer.IsTableNameOpen(aFolder: TfsSrcFolder;
  Const aTableName: TfsTableName): boolean;
Var
  Inx: Integer;
Begin
  Result := True;
  TableList.BeginRead;
  Try
    For Inx := 0 To pred(TableList.TableCount) Do
      With TableList[ftFromIndex, Inx] Do
        If (Folder = aFolder) And
          (FFCmpShStrUC(BaseName, aTableName, 255) = 0) Then
          Exit;
  Finally
    TableList.EndRead;
  End;
  Result := False;
End;
{--------}

Function TFSServer.seCheckSessionIDAndGet(aSessionID: TffSessionID;
  Var aSession: TfsSrcSession): TffResult;
Begin
  Result := DBIERR_FS_UnknownSession;
  Try
    If TObject(aSessionID) Is TfsSrcSession Then
      Begin
        aSession := TfsSrcSession(aSessionID);
        Result := DBIERR_NONE;
      End;
  Except
    { An exception may be raised if the ID is bogus.  Swallow the exception.}
  End;
End;
{--------}

Function TFSServer.CheckSessionIDAndGet(aClientID: TffClientID;
  aSessionID: TffSessionID;
  Var aClient: TfsSrcClient;
  Var aSession: TfsSrcSession): TffResult;
Begin
  If State <> fsesStarted Then
    Begin
      Result := DBIERR_FS_ServerUnavail;
      Exit;
    End;

  Result := CheckClientIDAndGet(aClientID, aClient);
  If (Result = DBIERR_NONE) Then
    Result := seCheckSessionIDAndGet(aSessionID, aSession);
End;
{--------}

Procedure TFSServer.lcSetEventLog(anEventLog: TFSBaseLog);
Begin
  Inherited lcSetEventLog(anEventLog);
  SQLEngine.EventLog := anEventLog;
  seSetLoggingState;
End;
{--------}

Procedure TFSServer.lcSetLogEnabled(Const aEnabled: boolean);
Begin
  Inherited lcSetLogEnabled(aEnabled);
  SQLEngine.EventLogEnabled := aEnabled;
  seSetLoggingState;
End;
{--------}

Function TFSServer.RebuildRegister(aClientID: TffClientID;
  aTotalRecords: Longint): TfsSrcRebuildStatus;
Begin
  Result := seRebuildList.AddRebuildStatus(aClientID, aTotalRecords);
End;
{--------}

Procedure TFSServer.RebuildDeregister(aRebuildID: Longint);
Begin
  seRebuildList.MarkRebuildStatusFinished(aRebuildID);
End;
{--------}

Function TFSServer.RebuildGetStatus(aRebuildID: Longint;
  Const aClientID: TffClientID;
  Var aIsPresent: boolean;
  Var aStatus: TffRebuildStatus): TffResult;
Var
  Client: TfsSrcClient;
Begin
  Result := seCheckClientIDAndGet(aClientID, Client);
  If Result = DBIERR_NONE Then
    Begin
      aIsPresent := seRebuildList.GetRebuildStatus(aRebuildID, aStatus);
      Result := DBIERR_NONE;
    End;
End;
{--------}

Function TFSServer.RecordDelete(aCursorID: TffCursorID; aData: PffByteArray): TffResult;
Var
  Cursor: TfsSrBaseCursor;
  Trans: TfsSrcTransaction;
  TransID: TffTransID;
  StartedTrans: boolean;
Begin
  StartedTrans := False;
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If Result = DBIERR_NONE Then
      Try
        FFSetRetry(Cursor.Timeout);
        If (Result = DBIERR_NONE) Then
          Begin
            Result := Cursor.EnsureWritable(True, True, tluDatabase); {!!.02}
            If (Result = DBIERR_NOACTIVETRAN) Or Cursor.NeedNestedTransaction Then
              Begin {!!.03}
                Result := seTransactionStart(Cursor.Database, False,
                  fscl_TrImplicit, TransID);
                StartedTrans := Result = DBIERR_NONE;
              End;
            Try
              If (Result = DBIERR_NONE) Then
                Begin
                  Result := Cursor.DeleteRecord(aData);
                  If (Result <> DBIERR_NONE) And Not StartedTrans Then
                    Begin
                      Trans := Cursor.Database.Transaction;
                      Trans.IsCorrupt := True;
                    End;
                End;
            Finally
              If StartedTrans Then
                Begin
                  If (Result = DBIERR_NONE) Then
                    Begin
                      Result := seTransactionCommit(Cursor.Database);
                    End
                  Else
                    Begin
                      seTransactionRollback(Cursor.Database);
                    End;
                End;
            End; {try..finally}
          End;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
  End; {try..except}
End;

{--------}

Function TFSServer.RecordDeleteBatch(aCursorID: TffCursorID;
  aBMCount: Longint;
  aBMLen: Longint;
  aData: PffByteArray;
  aErrors: PffLongintArray
  ): TffResult;
{Restructured !!.10}
Var
  Cursor: TfsSrBaseCursor;
  TransID: TffTransID;
  Offset: Longint;
  IRRes: TffResult;
  RecInx: Integer;
  StartedTrans: boolean;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If (Result = DBIERR_NONE) Then
    Try
      StartedTrans := False;
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.EnsureWritable(False, False, tluDatabase); {!!.02}
        If (Result = DBIERR_NOACTIVETRAN) Or
          Cursor.NeedNestedTransaction Then
          Begin {!!.03}
            Result := seTransactionStart(Cursor.Database, False,
              fscl_TrImplicit, TransID);
            StartedTrans := True;
          End;
        If (Result = DBIERR_NONE) Then
          Begin
            Try
              Offset := 0;
              For RecInx := 0 To pred(aBMCount) Do
                Begin
                  IRRes := CursorSetToBookmark(aCursorID, {!!.10}
                    PffByteArray(@aData^[Offset])); {!!.10}
                  If IRRes = DBIERR_NONE Then
                    IRRes := RecordDelete(aCursorID, Nil);
                  aErrors^[RecInx] := IRRes;
                  inc(Offset, aBMLen);
                End;
            Finally
              If StartedTrans Then
                Result := seTransactionCommit(Cursor.Database);
            End; {try..finally}
          End;
      Except
        On E: Exception Do
          Begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            If StartedTrans Then
              seTransactionRollback(Cursor.Database);
          End;
      End; {try..except}
    Finally
      Cursor.Deactivate;
    End;
End;
{--------}

Function TFSServer.RecordExtractKey(aCursorID: TffCursorID;
  aData: PffByteArray;
  aKey: PffByteArray): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeRecRead, ffeaNoAction);
        If Result = DBIERR_NONE Then
          If (Cursor.IndexID = 0) Then
            Result := DBIERR_NOASSOCINDEX
          Else
            Begin
              Result := Cursor.ExtractKey(aData, aKey);
              If Result = DBIERR_NONE Then
                Cursor.NotifyExtenders(ffeaAfterRecRead, ffeaNoAction);
            End;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.RecordGet(aCursorID: TffCursorID;
  aLockType: TffLockType;
  aUserLockType: TfsUserRecLocking;
  aData: PffByteArray;
  Var aFlag: Byte; Var aRefNr: TffInt64; Const aUser: Boolean): TffResult;
Var
  Cursor: TfsSrBaseCursor;
  ServerLockType: TfsSrcLockType;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If Result = DBIERR_NONE Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeRecRead, ffeaNoAction);
        If (Result = DBIERR_NONE) Then
          Begin
            ServerLockType := FFMapLock(aLockType, False);
            Result := Cursor.GetRecord(aData, ServerLockType, aUserLockType, aFlag, arefnr, aUser);
            If Result = DBIERR_NONE Then
              Cursor.NotifyExtenders(ffeaAfterRecRead, ffeaNoAction);
          End;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.RecordGetBatch(aCursorID: TffCursorID;
  aRecCount: Longint;
  aRecLen: Longint;
  Var aRecRead: Longint;
  aData: PffByteArray;
  Var aError: TffResult): TffResult;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  Cursor: TfsSrBaseCursor;
  Offset: Longint;
Begin
  aFlag := 0;
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        If Result = DBIERR_NONE Then
          Begin
            Offset := 0;
            aError := Cursor.GetNextRecord(PffByteArray(@aData^[Offset]), ffsltNone, aFlag, arefnr);
            If (aError = DBIERR_NONE) Then
              aRecRead := 1
            Else
              aRecRead := 0;
            If aError = DBIERR_FS_FilterTimeout Then
              Result := aError;
            While (aError = DBIERR_NONE) And (aRecRead < aRecCount) Do
              Begin
                inc(Offset, aRecLen);
                aError := Cursor.GetNextRecord(PffByteArray(@aData^[Offset]), ffsltNone, aFlag, arefnr);
                If (aError = DBIERR_NONE) Then
                  inc(aRecRead);
              End; {while}
          End; {if}
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.RecordGetForKey(aCursorID: TffCursorID;
  aDirectKey: boolean;
  aFieldCount: Integer;
  aPartialLen: Integer;
  aKeyData: PffByteArray;
  aData: PffByteArray;
  aFirstCall: Boolean): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeRecRead, ffeaNoAction);
        If Result = DBIERR_NONE Then
          Begin
            Result := Cursor.GetRecordForKey(aDirectKey,
              aFieldCount,
              aPartialLen,
              aKeyData,
              aData,
              aFirstCall);
            If Result = DBIERR_NONE Then
              Cursor.NotifyExtenders(ffeaAfterRecRead, ffeaNoAction);
          End;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.RecordGetSetPosition(aValue: Longint; aCursorID: TffCursorID;
  aLockType: TffLockType;
  aData: PffByteArray;
  Var aFlag: Byte;
  Var aRecNo: Longword;
  Var aRefNr: TffInt64;
  aInfoGetSetPosition: TInfoGetSetPosition;
  aSet: Boolean): TffResult;
Var
  Cursor: TfsSrBaseCursor;
  ServerLockType: TfsSrcLockType;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeRecRead, ffeaNoAction);
        If Result = DBIERR_NONE Then
          Begin
            ServerLockType := FFMapLock(ffltNoLock, False);
            Result := Cursor.GetSetPosition(aValue, aData, ServerLockType,
              aFlag, aRecNo, aRefNr, aInfoGetSetPosition, aSet);
            If Result = DBIERR_NONE Then
              Cursor.NotifyExtenders(ffeaAfterRecRead, ffeaNoAction);
          End;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;

Function TFSServer.RecordGetNext(aCursorID: TffCursorID;
  aLockType: TffLockType;
  aData: PffByteArray;
  Var aFlag: Byte;
  Var aRefNr: TffInt64): TffResult;
Var
  Cursor: TfsSrBaseCursor;
  ServerLockType: TfsSrcLockType;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeRecRead, ffeaNoAction);
        If Result = DBIERR_NONE Then
          Begin
            ServerLockType := FFMapLock(aLockType, False);
            Result := Cursor.GetNextRecord(aData, ServerLockType, aFlag, arefnr);
            If Result = DBIERR_NONE Then
              Cursor.NotifyExtenders(ffeaAfterRecRead, ffeaNoAction);
          End;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.RecordGetNextSeq(aCursorID: TffCursorID;
  Var aRefNr: TffInt64;
  aData: PffByteArray): TffResult;
Var
  Cursor: TfsSrBaseCursor;
  aFlag: Byte;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        If Result = DBIERR_NONE Then
          Cursor.Table.GetNextRecordSeq(Cursor.Database.TransactionInfo, aRefNr, aData, False, False, AFlag);
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.RecordGetPrior(aCursorID: TffCursorID;
  aLockType: TffLockType;
  aData: PffByteArray;
  Var aFlag: Byte;
  Var aRefNr: TffInt64): TffResult;
Var
  Cursor: TfsSrBaseCursor;
  ServerLockType: TfsSrcLockType;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeRecRead, ffeaNoAction);
        If Result = DBIERR_NONE Then
          Begin
            ServerLockType := FFMapLock(aLockType, False);
            Result := Cursor.GetPriorRecord(aData, ServerLockType, aFlag, arefnr);
            If Result = DBIERR_NONE Then
              Cursor.NotifyExtenders(ffeaAfterRecRead, ffeaNoAction);
          End;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.RecordInsert(aCursorID: TffCursorID;
  aLockType: TffLockType;
  aData: PffByteArray;
  aUndelete: Boolean;
  Var aRefNr: TffInt64): TffResult;
{Restructured !!.10}
Var
  Cursor: TfsSrBaseCursor;
  ServerLockType: TfsSrcLockType;
  StartedTrans: boolean;
  TransID: TffTransID;
  OldResult: TffResult;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If (Result = DBIERR_NONE) Then
    Try
      StartedTrans := False;
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.EnsureWritable(False, True, tluDatabase); {!!.02}
        { Need to start an implicit transaction? }
        If (Result = DBIERR_NOACTIVETRAN) Or {!!.03}
        Cursor.NeedNestedTransaction Then
          Begin {!!.03}
            Result := seTransactionStart(Cursor.Database, False,
              fscl_TrImplicit, TransID);
            StartedTrans := (Result = DBIERR_NONE);
          End;
        If (Result = DBIERR_NONE) Then
          Begin
            ServerLockType := FFMapLock(aLockType, False);
            If aUndelete Then
              Result := Cursor.UndeleteRecord(aData, ServerLockType, aRefNr)
            Else
              Result := Cursor.InsertRecord(aData, ServerLockType, 0, aRefNr);
          End;

        If StartedTrans Then
          Begin
            If aUndelete Then
              Begin
                If (Result = DBIERR_NONE) Or (Result = DBIERR_EOF) Then
                  Begin
                    OldResult := Result;
                    Result := seTransactionCommit(Cursor.Database);
                    If (Result = DBIERR_NONE) Then
                      Result := OldResult;
                  End
                Else
                  seTransactionRollback(Cursor.Database);
              End
            Else
              Begin
                If (Result = DBIERR_NONE) Then
                  Result := seTransactionCommit(Cursor.Database)
                Else
                  seTransactionRollback(Cursor.Database);
              End;
          End;
      Except
        On E: Exception Do
          Begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            If StartedTrans Then
              seTransactionRollback(Cursor.Database);
          End;
      End; {try..except}
    Finally
      Cursor.Deactivate;
    End;
End;
{--------}

Function TFSServer.RecordInsertBatch(aCursorID: TffCursorID;
  aRecCount: Longint;
  aRecLen: Longint;
  aData: PffByteArray;
  aErrors: PffLongintArray): TffResult;
{Restructured !!.10}
Var
  Cursor: TfsSrBaseCursor;
  TransID: TffTransID;
  Offset: Longint;
  IRRes: TffResult;
  RecInx: Integer;
  StartedTrans: boolean;
  aRefNr: tffInt64;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If (Result = DBIERR_NONE) Then
    Try
      StartedTrans := False;
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.EnsureWritable(False, False, tluDatabase); {!!.02}
        If (Result = DBIERR_NOACTIVETRAN) Or
          Cursor.NeedNestedTransaction Then
          Begin {!!.03}
            Result := seTransactionStart(Cursor.Database, False,
              fscl_TrImplicit, TransID);
            StartedTrans := True;
          End;
        If (Result = DBIERR_NONE) Then
          Begin
            Try
              Offset := 0;
              For RecInx := 0 To pred(aRecCount) Do
                Begin
                  IRRes := RecordInsert(aCursorID, ffltWriteLock,
                    PffByteArray(@aData^[Offset]), False, aRefNr);
                  aErrors^[RecInx] := IRRes;
                  inc(Offset, aRecLen);
                End;
            Finally
              If StartedTrans Then
                Result := seTransactionCommit(Cursor.Database);
            End; {try..finally}
          End;
      Except
        On E: Exception Do
          Begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            If StartedTrans Then
              seTransactionRollback(Cursor.Database);
          End;
      End; {try..except}
    Finally
      Cursor.Deactivate;
    End;
End;
{--------}

Function TFSServer.RecordIsLocked(aCursorID: TffCursorID; aLockType: TffLockType;
  Var aIsLocked: boolean): TffResult;
Var
  Cursor: TfsSrBaseCursor;
  ServerLockType: TfsSrcLockType;
Begin
  Result := DBIERR_NONE;
  aIsLocked := False;
  If (aLockType = ffltNoLock) Then
    Exit;

  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
        If Result = DBIERR_NONE Then
          Begin
            ServerLockType := FFMapLock(aLockType, True);

            aIsLocked := Cursor.IsRecordLocked(ServerLockType);
          End;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.RecordModify(aCursorID: TffCursorID;
  aData: PffByteArray;
  aRelLock: boolean;
  aUserLockType: TfsUserRecLocking;
  aFlag: Byte;
  aSet, Use: Boolean): TffResult;
Var
  Cursor: TfsSrBaseCursor;
  TransID: TffTransID;
  StartedTrans: boolean;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If (Result = DBIERR_NONE) Then
    Try
      StartedTrans := False;
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.EnsureWritable(True, False, aUserLockType);
        { Need to start an implicit transaction? }
        If (Result = DBIERR_NOACTIVETRAN) Or
          Cursor.NeedNestedTransaction Then
          Begin
            Result := seTransactionStart(Cursor.Database, False,
              fscl_TrImplicit, TransID);
            StartedTrans := Result = DBIERR_NONE;
          End;

        If (Result = DBIERR_NONE) Then
          Begin
            Result := Cursor.ModifyRecord(aData, aRelLock, aUserLockType, aFlag, aSet, Use);
          End;

        If StartedTrans Then
          Begin
            If (Result = DBIERR_NONE) Then
              Begin
                Result := seTransactionCommit(Cursor.Database);
                // triger after modify
              End
            Else
              seTransactionRollback(Cursor.Database);
          End; { if }
      Except
        On E: Exception Do
          Begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            If StartedTrans Then
              seTransactionRollback(Cursor.Database);
          End;
      End; {try..except}
    Finally
      Cursor.Deactivate;
    End;
End;
{--------}

Function TFSServer.RecordRelLock(aCursorID: TffCursorID; aAllLocks: boolean): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  { Assumption: Transaction is active. }
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Cursor.RelRecordLock(aAllLocks);
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.seDatabaseOpenPrim(Session: TfsSrcSession;
  Folder: TfsSrcFolder;
  anAlias: TffName;
  aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  aTimeout: Longint;
  aCheckSpace: Boolean;
  aTransIsolation: TfsTransIsolation;
  aRecLocking: TfsDataBaseRecLocking) {!!.11}
: TfsSrcDatabase;
Var
  aMonitor: TFSBaseEngineMonitor;
  anExtender: TFSBaseEngineExtender;
  anIndex: Longint;
  MonitorList: TFSNormalList;
Begin
  Result := TfsSrcDatabase.Create(Self,
    Session,
    Folder,
    anAlias,
    aOpenMode,
    aShareMode,
    aTimeout,
    aCheckSpace,
    aTransIsolation,
    aRecLocking); {!!.11}
  { Assumption: Calling routine has gained write access to the database list. }
  DatabaseList.BeginWrite;
  Try
    DatabaseList.AddDatabase(Result);
  Finally
    DatabaseList.EndWrite;
  End;

  { If there are any monitors interested in databases then see if they
    are interested in this database. }
  MonitorList := GetInterestedMonitors(TfsSrcDatabase);
  If assigned(MonitorList) Then
    Begin
      For anIndex := 0 To pred(MonitorList.Count) Do
        Begin
          aMonitor := TFSBaseEngineMonitor
            (TfsIntListItem(MonitorList[anIndex]).KeyAsInt);
          Try
            anExtender := aMonitor.Interested(Result);
            If assigned(anExtender) Then
              Result.dbAddExtender(anExtender);
          Except
            On E: Exception Do
              seForce('Monitor [%s] exception, seDatabaseOpenPrim: %s', {!!.06 - Start}
                [aMonitor.ClassName, E.message],
                bseGetReadOnly); {!!.06 - End}
          End;
        End;
      MonitorList.Free;
    End;

End;
{--------}

Function TFSServer.SQLAlloc(aClientID: TffClientID;
  aDatabaseID: TffDatabaseID;
  aTimeout: Longint;
  Var aStmtID: TffSqlStmtID): TffResult;
Var
  Client: TfsSrcClient;
  DB: TfsSrcDatabase;
Begin
  Try
    Result := CheckClientIDAndGet(aClientID, Client);
    If Result = DBIERR_NONE Then
      Try
        Result := seCheckDatabaseIDAndGet(aDatabaseID, DB);
        If Result = DBIERR_NONE Then
          Begin
            FFSetRetry(5000);
            Result := DB.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
            If Result = DBIERR_NONE Then
              Begin
                If assigned(seSQLEngine) Then
                  Result := seSQLEngine.Alloc(Self, aClientID, aDatabaseID,
                    aTimeout, aStmtID)
                Else
                  FSRaiseException(EfsServerException, fsStrResServer,
                    fserrNoSQLEngine, [seGetServerName]);
              End;
          End;
      Finally
        Client.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.SQLExec(aStmtID: TffSqlStmtID;
  aOpenMode: TffOpenMode;
  Var aCursorID: TffCursorID;
  aStream: TStream): TffResult;
Begin
  Result := DBIERR_NONE;
  Try
    { Note: Timeout set in SQLAlloc. }
    If assigned(seSQLEngine) Then
      Result := seSQLEngine.Exec(aStmtID, aOpenMode, aCursorID, aStream)
    Else
      FSRaiseException(EfsServerException, fsStrResServer,
        fserrNoSQLEngine, [seGetServerName]);
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.SQLExecDirect(aClientID: TffClientID;
  aDatabaseID: TffDatabaseID;
  aQueryText: PChar;
  aTimeout: Longint;
  aOpenMode: TffOpenMode;
  Var aCursorID: TffCursorID;
  aStream: TStream): TffResult;
Var
  Client: TfsSrcClient;
  DB: TfsSrcDatabase;
Begin
  Try
    Result := CheckClientIDAndGet(aClientID, Client);
    If Result = DBIERR_NONE Then
      Try
        Result := seCheckDatabaseIDAndGet(aDatabaseID, DB);
        If Result = DBIERR_NONE Then
          Begin
            FFSetRetry(aTimeout);
            Result := DB.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
            If Result = DBIERR_NONE Then
              Begin
                If assigned(seSQLEngine) Then
                  Result := seSQLEngine.ExecDirect(Self, aClientID, aDatabaseID,
                    aQueryText, aOpenMode, aTimeout,
                    aCursorID, aStream)
                Else
                  FSRaiseException(EfsServerException, fsStrResServer,
                    fserrNoSQLEngine, [seGetServerName]);
              End;
          End;
      Finally
        Client.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.SQLFree(aStmtID: TffSqlStmtID): TffResult;
Begin
  Result := DBIERR_NONE;
  Try
    FFSetRetry(5000);
    If assigned(seSQLEngine) Then
      Result := seSQLEngine.FreeStmt(aStmtID)
    Else
      FSRaiseException(EfsServerException, fsStrResServer,
        fserrNoSQLEngine, [seGetServerName]);
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.SQLPrepare(aStmtID: TffSqlStmtID;
  aQueryText: PChar;
  aStream: TStream): TffResult;
Begin
  Result := DBIERR_NONE;
  Try
    FFSetRetry(5000);
    If assigned(seSQLEngine) Then
      Result := seSQLEngine.Prepare(aStmtID, aQueryText, aStream)
    Else
      FSRaiseException(EfsServerException, fsStrResServer,
        fserrNoSQLEngine, [seGetServerName]);
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.SQLSetParams(aStmtID: TffSqlStmtID;
  aNumParams: Word;
  aParamDescs: Pointer;
  aDataBuffer: PffByteArray;
  aDataLen: Integer;
  aStream: TStream): TffResult;
Begin
  Result := DBIERR_NONE;
  Try
    FFSetRetry(5000);
    If assigned(seSQLEngine) Then
      Result := seSQLEngine.SetParams(aStmtID, aNumParams, aParamDescs,
        aDataBuffer, aStream)
    Else
      FSRaiseException(EfsServerException, fsStrResServer,
        fserrNoSQLEngine, [seGetServerName]);
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.TableAddIndex(Const aDatabaseID: TffDatabaseID;
  Const aCursorID: TffCursorID;
  Const aTableName: TfsTableName;
  Const aIndexDesc: TffIndexDescriptor): TffResult;
{Restructured !!.10}
Var
  Cursor: TfsSrBaseCursor;
  DB: TfsSrcDatabase;
  StartedTrans: boolean;
  tmpCursorID: TffCursorID;
  tmpTablename: String;
  TransID: TffTransID;
  FI: PffFileInfo;
  FileHeader: PffBlockHeaderFile;
  aRelMethod: TffReleaseMethod;
  Rflags, Passwd: Longword;
Begin
  {choice of two here: if the cursor ID is set, use it. Otherwise
   use the databaseID/tablename}
  If (aCursorID <> 0) Then
    Begin
      Result := CheckCursorIDAndGet(aCursorID, Cursor);
      If (Result = DBIERR_NONE) Then
        Try
          StartedTrans := False;
          Try
            FFSetRetry(Cursor.Timeout);
            Result := Cursor.NotifyExtenders(ffeaBeforeAddInx, ffeaTabAddInxFail);
            If Result = DBIERR_NONE Then
              Begin
                tmpTableName := Cursor.Table.BaseName;
                Result := seTransactionStart(Cursor.Database, False,
                  fscl_TrImplicit, TransID);
                If (Result = DBIERR_NONE) Then
                  Begin
                    StartedTrans := True;
                    Rflags := FFTblReadTableFlags(Cursor.Table.Files[0], Cursor.Database.TransactionInfo);
                    If ((Rflags And fsTableDontRestructure) <> 0) Then
                      Result := DBIERR_NOTSUFFFIELDRIGHTS;

                    Passwd := FFTblReadTablePasswordRest(Cursor.Table.Files[0], Cursor.Database.TransactionInfo);
                    If Passwd > 0 Then
                      If Not Cursor.Database.Session.TablePasswordList.Exists(IntToStr(Passwd)) Then
                        Result := DBIERR_NOTSUFFFIELDRIGHTS;
                    If (Result = DBIERR_NONE) Then
                      Result := Cursor.AddIndexToTable(aIndexDesc);
                    {Begin !!.13}
                    If (Result = DBIERR_NONE) Then
                      Begin
                        {update the file header}
                        TableList.BeginRead;
                        Try
                          FI := TableList.GetTableFromName(aTableName).Files[0];
                        Finally
                          TableList.EndRead;
                        End;
                        FileHeader :=
                          PffBlockHeaderFile(BufferManager.GetBlock(FI, 0, DB.dbTI,
                          True,
                          aRelMethod, fsoNone));
                        inc(FileHeader^.bhfIndexCount);
                        aRelMethod(PffBlock(FileHeader));
                        seTransactionCommit(Cursor.Database)
                      End
                        {End !!.13}
                    Else
                      Begin
                        Cursor.NotifyExtenders(ffeaTabAddInxFail, ffeaNoAction);
                        seTransactionRollback(Cursor.Database)
                      End;
                  End;
              End;
          Except
            On E: Exception Do
              Begin
                Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
                If StartedTrans Then
                  seTransactionRollback(Cursor.Database);
              End;
          End; {try..except}
        Finally
          Cursor.Deactivate;
        End;
    End
  Else {use databaseID/tablename}
    Begin
      Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
      If (Result = DBIERR_NONE) Then
        Try
          StartedTrans := False;
          Try
            FFSetRetry(DB.Timeout);
            Result := TableOpen(aDatabaseID, aTableName,
              False, '', 0, omReadWrite, smExclusive, DB.Timeout,
              tmpCursorID, Nil);
            If (Result = DBIERR_NONE) Then
              Try
                Result := seCheckCursorIDAndGet(tmpCursorID, Cursor);
                If (Result = DBIERR_NONE) Then
                  Begin
                    Result := Cursor.NotifyExtenders(ffeaBeforeAddInx, ffeaTabAddInxFail);
                    If Result = DBIERR_NONE Then
                      Begin
                        Result := seTransactionStart(DB, False, fscl_TrImplicit,
                          TransID);
                        If (Result = DBIERR_NONE) Then
                          Begin
                            StartedTrans := True;
                            Rflags := FFTblReadTableFlags(Cursor.Table.Files[0], db.TransactionInfo);
                            If ((Rflags And fsTableDontRestructure) <> 0) Then
                              Result := DBIERR_NOTSUFFFIELDRIGHTS;

                            Passwd := FFTblReadTablePasswordRest(Cursor.Table.Files[0], db.TransactionInfo);
                            If Passwd > 0 Then
                              If Not db.Session.TablePasswordList.Exists(IntToStr(Passwd)) Then
                                Result := DBIERR_NOTSUFFFIELDRIGHTS;
                            If (Result = DBIERR_NONE) Then
                              Result := Cursor.AddIndexToTable(aIndexDesc);
                            If (Result = DBIERR_NONE) Then
                              Begin
                                {update the file header}
                                TableList.BeginRead;
                                Try
                                  FI := TableList.GetTableFromName(aTableName).Files[0];
                                Finally
                                  TableList.EndRead;
                                End;
                                FileHeader :=
                                  PffBlockHeaderFile(BufferManager.GetBlock(FI, 0, DB.dbTI,
                                  True,
                                  aRelMethod, fsoNone));
                                inc(FileHeader^.bhfIndexCount);
                                aRelMethod(PffBlock(FileHeader));
                                seTransactionCommit(Cursor.Database)
                              End
                            Else
                              Begin
                                Cursor.NotifyExtenders(ffeaTabAddInxFail, ffeaNoAction);
                                seTransactionRollback(Cursor.Database)
                              End;
                          End;
                      End;
                  End;
              Finally
                CursorClose(tmpCursorID);
              End;
          Except
            On E: Exception Do
              Begin
                Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
                If StartedTrans Then
                  seTransactionRollback(DB);
              End;
          End; {try..except}
        Finally
          DB.Deactivate;
        End;
    End;
End;
{--------}

Function TFSServer.TableBuild(aDatabaseID: TffDatabaseID;
  aOverWrite: boolean;
  Const aTableName: TfsTableName;
  aForServer: boolean;
  aDictionary: TFSInfoDict): TffResult;
Var
  DB: TfsSrcDatabase;
Begin
  If IsReadOnly Then
    Begin {!!.01 - Start}
      Result := DBIERR_READONLYDB;
      Exit;
    End; {!!.01 - End}
  Try
    {the database ID must exist}
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If (Result <> DBIERR_NONE) Then
      Exit;

    Try
      FFSetRetry(DB.Timeout);
      Result := DB.NotifyExtenders(ffeaBeforeTabInsert, ffeaTabInsertFail);

      If Result = DBIERR_NONE Then
        Begin
          {the database must be open in readwrite mode}
          If (DB.OpenMode = omReadOnly) Then
            Begin
              Result := DBIERR_READONLYDB;
              Exit;
            End;
          Result := seTableBuildPrim(DB, aOverwrite, aTableName, aForServer,
            aDictionary);
          If Result <> DBIERR_NONE Then
            DB.NotifyExtenders(ffeaTabInsertFail, ffeaNoAction);
        End;
    Finally
      DB.Deactivate;
    End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.seTableBuildPrim(aDB: TfsSrcDatabase;
  aOverwrite: boolean;
  Const aTableName: TfsTableName;
  aForServer: boolean;
  aDict: TFSInfoDict): TffResult;
Var
  Table: TfsSrcBaseTable;
  TableDataFile: TffFileNameExt;
  TransID: TffTransID;
Begin

  { Obtain write access to the table list.  Our purpose is to make sure
    the table is not opened. We have to obtain write access, instead of read
    access, just in case we need to call TableList.RemoveIfUnused. }
  TableList.BeginWrite;
  Try
    { Is the table open? }
    Table := GetTableInstance(aDB.Folder, aTableName);
    If (Table <> Nil) Then
      Begin
        { Yes. See if it can be closed. }
        TableList.RemoveIfUnused(Table);
        If GetTableInstance(aDB.Folder, aTableName) <> Nil Then
          Begin
            Result := DBIERR_TABLEOPEN;
            Exit;
          End;
      End;
    {the table name must be a valid file name without extension}
    If Not FFVerifyFileName(aTableName) Then
      Begin
        Result := DBIERR_INVALIDTABLENAME;
        Exit;
      End;
    {the table's data file connot exist within the database}
    TableDataFile := FFMakeFileNameExt(aTableName, fsc_ExtForData);
    If FFFileExists(FFMakeFullFileName(aDB.Folder.Path, TableDataFile)) Then
      Begin
        If aOverWrite Then
          Begin
            {we want to overwrite this table - we have to delete it first}
            {table exists, is not open - we can delete the table and all files}
            seDeleteTable(aDB, aTableName);
          End
        Else
          Begin
            {table exists, and we're not going to overwrite it}
            Result := DBIERR_TABLEEXISTS;
            Exit;
          End;
      End;

    { Create the table. }
    Table := TfsSrcTable.Create(Self, aTableName, aDB.Folder, seBufMgr,
      omReadWrite);

    Try
      { Start a transaction. Note that if one is already active for this
        database object, this will be a nested transaction. }
      Result := seTransactionStart(aDB, False, fscl_TrImplicit, TransID);
      If Result <> DBIERR_NONE Then
        Exit;

      Try
        { Create files making up the table. }
        Table.BuildFiles(aDB.TransactionInfo, aForServer, aDict, [], Nil);
        { Commit the transaction. }
        seTransactionCommit(aDB);

        { If we are in a nested transaction then the table will not have
          been written out to disk. Make sure the changes are written to
          disk. }
        If aDB.Transaction <> Nil Then
          Table.CommitChanges(aDB.TransactionInfo);
      Except
        On E: Exception Do
          Begin
            seTransactionRollback(aDB);
            Raise;
          End;
      End; {try..except}
    Finally
      { Destroy the table object. This will close all the files. }
      Table.Free;
    End; {try..finally}
  Finally
    TableList.EndWrite;
  End;
End;
{--------}

Function TFSServer.seTableDeletePrim(DB: TfsSrcDatabase;
  Const aTableName: TfsTableName): TffResult;
Var
  Table: TfsSrcBaseTable;
Begin

  Result := DBIERR_NONE;

  { If no tablename specified then exit otherwise a lower level routine
    (FFFindClose) will go into an infinite loop. }
  If aTableName = '' Then
    Begin
      Result := DBIERR_INVALIDTABLENAME;
      Exit;
    End;

  { Obtain write access to the table list.  This is our way of making sure
    nobody opens the table in between our determining the table is NOT open
    and deleting the table. }
  TableList.BeginWrite;
  Try
    { Is the table open? }
    Table := GetTableInstance(DB.Folder, aTableName);
    If (Table <> Nil) Then
      Begin
        { Yes. Can it be closed? }
        TableList.RemoveIfUnused(Table);
        If GetTableInstance(DB.Folder, aTableName) <> Nil Then
          Begin
            { No. Return an error. }
            Result := DBIERR_TABLEOPEN;
            Exit;
          End;
      End;
    seDeleteTable(DB, aTableName)
  Finally
    TableList.EndWrite;
  End;
End;
{--------}

Function TFSServer.TableDelete(aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName): TffResult;
Var
  DB: TfsSrcDatabase;
Begin
  If IsReadOnly Then
    Begin {!!.01 - Start}
      Result := DBIERR_TABLEREADONLY;
      Exit;
    End; {!!.01 - End}
  Try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If Result = DBIERR_NONE Then
      Try
        FFSetRetry(DB.Timeout);
        Result := DB.NotifyExtenders(ffeaBeforeTabDelete, ffeaTabDeleteFail);
        If (Result = DBIERR_NONE) Then
          Begin
            Result := seTableDeletePrim(DB, aTableName);
            If Result <> DBIERR_NONE Then
              DB.NotifyExtenders(ffeaTabDeleteFail, ffeaNoAction);
          End;
      Finally
        DB.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.TableDropIndex(aDatabaseID: TffDatabaseID;
  aCursorID: TffCursorID;
  Const aTableName: TfsTableName;
  Const aIndexName: TffDictItemName;
  aIndexID: Longint): TffResult;
{Restructured !!.10}
Var
  aTable: TfsSrcBaseTable; {!!.02}
  Cursor: TfsSrBaseCursor;
  DB: TfsSrcDatabase;
  StartedTrans: boolean;
  TransID: TffTransID;
  Rflags, Passwd: Longword;
Begin
  { Assumption: Table has been opened for Exclusive use.  This is verified
                in Cursor.DropIndexFromTable. }
  {choice of two here: if the cursor ID is set use that, otherwise
   use the databaseID/tablename}
  If (aCursorID <> 0) Then
    Begin
      Result := CheckCursorIDAndGet(aCursorID, Cursor);
      If (Result = DBIERR_NONE) Then
        Try
          StartedTrans := False;
          Try
            FFSetRetry(Cursor.Timeout);
            Result := Cursor.NotifyExtenders(ffeaBeforeTabDelete, ffeaTabDeleteFail);
            If Result = DBIERR_NONE Then
              Begin
                Result := seTransactionStart(Cursor.Database, False,
                  fscl_TrImplicit, TransID);
                If (Result = DBIERR_NONE) Then
                  Begin
                    StartedTrans := True;
                    Rflags := FFTblReadTableFlags(Cursor.Table.Files[0], Cursor.Database.TransactionInfo);
                    If ((Rflags And fsTableDontRestructure) <> 0) Then
                      Result := DBIERR_NOTSUFFFIELDRIGHTS;

                    Passwd := FFTblReadTablePasswordRest(Cursor.Table.Files[0], Cursor.Database.TransactionInfo);
                    If Passwd > 0 Then
                      If Not Cursor.Database.Session.TablePasswordList.Exists(IntToStr(Passwd)) Then
                        Result := DBIERR_NOTSUFFFIELDRIGHTS;
                    If (Result = DBIERR_NONE) Then
                      Result := Cursor.DropIndexFromTable(aIndexName, aIndexID);
                    If (Result = DBIERR_NONE) Then
                      Begin
                        seTransactionCommit(Cursor.Database);
                      End
                    Else
                      Begin
                        Cursor.NotifyExtenders(ffeaTabDeleteFail, ffeaNoAction);
                        seTransactionRollback(Cursor.Database);
                      End; { if }
                  End; { if }
              End; { if }
          Except
            On E: Exception Do
              Begin
                Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
                If StartedTrans Then
                  seTransactionRollback(Cursor.Database);
              End;
          End; {try..except}
        Finally
          Cursor.Deactivate;
        End;
    End
  Else {use databaseID/tablename}
    Begin
      Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
      If Result = DBIERR_NONE Then
        Try
          StartedTrans := False;
          Try
            FFSetRetry(DB.Timeout);
            Result := TableOpen(aDatabaseID, aTableName,
              False, '', 0, omReadWrite, smExclusive, DB.Timeout,
              aCursorID, Nil);
            If (Result = DBIERR_NONE) Then
              Try
                Result := seCheckCursorIDAndGet(aCursorID, Cursor);
                If (Result = DBIERR_NONE) Then
                  Begin
                    Result := Cursor.NotifyExtenders(ffeaBeforeTabDelete, ffeaTabDeleteFail);
                    If Result = DBIERR_NONE Then
                      Begin
                        Result := seTransactionStart(DB, False, fscl_TrImplicit,
                          TransID);
                        If (Result = DBIERR_NONE) Then
                          Begin
                            StartedTrans := True;
                            {Begin !!.02}
                            Try
                              Rflags := FFTblReadTableFlags(Cursor.Table.Files[0], db.TransactionInfo);
                              If ((Rflags And fsTableDontRestructure) <> 0) Then
                                Result := DBIERR_NOTSUFFFIELDRIGHTS;

                              Passwd := FFTblReadTablePasswordRest(Cursor.Table.Files[0], db.TransactionInfo);
                              If Passwd > 0 Then
                                If Not db.Session.TablePasswordList.Exists(IntToStr(Passwd)) Then
                                  Result := DBIERR_NOTSUFFFIELDRIGHTS;
                              If (Result = DBIERR_NONE) Then
                                Result := Cursor.DropIndexFromTable(aIndexName, aIndexID);
                              If (Result = DBIERR_NONE) Then
                                seTransactionCommit(Cursor.Database)
                              Else
                                Begin
                                  Cursor.NotifyExtenders(ffeaTabDeleteFail, ffeaNoAction);
                                  seTransactionRollback(Cursor.Database)
                                End;
                            Except
                              Cursor.NotifyExtenders(ffeaTabDeleteFail, ffeaNoAction);
                              seTransactionRollback(Cursor.Database);
                              StartedTrans := False;
                              Raise;
                            End;
                            {End !!.02}
                          End;
                      End;
                  End; { if }
              Finally
                aTable := Cursor.Table; {!!.02}
                CursorClose(aCursorID);
                TableList.RemoveIfUnused(aTable); {!!.02}
              End;
          Except
            On E: Exception Do
              Begin
                Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
                If StartedTrans Then
                  seTransactionRollback(DB);
              End;
          End; {try..except}
        Finally
          DB.Deactivate;
        End;
    End;
End;
{--------}

Function TFSServer.TableEmpty(aDatabaseID: TffDatabaseID;
  aCursorID: TffCursorID;
  Const aTableName: TfsTableName): TffResult;
{Restructured !!.10}
Var
  Cursor: TfsSrBaseCursor;
  DB: TfsSrcDatabase;
  Dict: TFSInfoDict;
  Trans: TfsSrcTransaction;
  TransID: TffTransID;
  Rflags: Word;

  Function aTableBuildPrim(aDB: TfsSrcDatabase;
    aOverwrite: boolean;
    Const aTableName: TfsTableName;
    aForServer: boolean;
    aDict: TFSInfoDict): TffResult;
  Var
    Table: TfsSrcBaseTable;
    TableDataFile: TffFileNameExt;
    TransID: TffTransID;
    Mx: Longint;
    Ps, Ps1, ps2, ps3: Longword;
  Begin
    Result := seTransactionStart(aDB, False, fscl_TrImplicit, TransID);
    If Result <> DBIERR_NONE Then
      Exit;

    { Obtain write access to the table list.  Our purpose is to make sure
      the table is not opened. We have to obtain write access, instead of read
      access, just in case we need to call TableList.RemoveIfUnused. }
    TableList.BeginWrite;
    Try
      { Is the table open? }
      Table := GetTableInstance(aDB.Folder, aTableName);
      If (Table = Nil) Then
        Table.OpenFiles(aDB.TransactionInfo, seIsServerTable(aTableName), []);
      Mx := FFTblReadMaxRecords(Table.Files[0], aDB.TransactionInfo);
      Ps := FFTblReadTablePassword(Table.Files[0], aDB.TransactionInfo);
      Ps3 := FFTblReadTablePasswordRest(Table.Files[0], aDB.TransactionInfo);
      Ps1 := FFTblReadTableDBID(Table.Files[0], aDB.TransactionInfo);
      Ps2 := FFTblReadTableFlags(Table.Files[0], aDB.TransactionInfo);

      Rflags := Ps2;
      If ((Rflags And fsTableDontEmptyTable) <> 0) Then
        Begin
          Result := DBIERR_NOTSUFFFIELDRIGHTS;
          If aDB.Transaction <> Nil Then
            Table.CommitChanges(aDB.TransactionInfo);
          Exit;
        End;

      If (Table <> Nil) Then
        Begin
          { Yes. See if it can be closed. }
          TableList.RemoveIfUnused(Table);
          If GetTableInstance(aDB.Folder, aTableName) <> Nil Then
            Begin
              Result := DBIERR_TABLEOPEN;
              Exit;
            End;
        End;
      {the table name must be a valid file name without extension}
      If Not FFVerifyFileName(aTableName) Then
        Begin
          Result := DBIERR_INVALIDTABLENAME;
          Exit;
        End;

      {the table's data file connot exist within the database}
      TableDataFile := FFMakeFileNameExt(aTableName, fsc_ExtForData);
      If FFFileExists(FFMakeFullFileName(aDB.Folder.Path, TableDataFile)) Then
        Begin
          If aOverWrite Then
            Begin
              {we want to overwrite this table - we have to delete it first}
              {table exists, is not open - we can delete the table and all files}
              seDeleteTable(aDB, aTableName);
            End
          Else
            Begin
              {table exists, and we're not going to overwrite it}
              Result := DBIERR_TABLEEXISTS;
              Exit;
            End;
        End;

      { Create the table. }
      Table := TfsSrcTable.Create(Self, aTableName, aDB.Folder, seBufMgr,
        omReadWrite);

      Try

        Try
          { Create files making up the table. }
          Table.BuildFiles(aDB.TransactionInfo, aForServer, aDict, [], Nil);

          FFTblSetTablePassword(Table.Files[0], aDB.TransactionInfo, ps);
          FFTblSetTablePasswordRest(Table.Files[0], aDB.TransactionInfo, ps3);
          FFTblSetMaxRecords(Table.Files[0], aDB.TransactionInfo, mx);
          FFTblSetTableFlags(Table.Files[0], aDB.TransactionInfo, ps2);
          FFTblSetTableDBID(Table.Files[0], aDB.TransactionInfo, ps1);
          { Commit the transaction. }
          seTransactionCommit(aDB);

          { If we are in a nested transaction then the table will not have
            been written out to disk. Make sure the changes are written to
            disk. }
          If aDB.Transaction <> Nil Then
            Table.CommitChanges(aDB.TransactionInfo);
        Except
          On E: Exception Do
            Begin
              seTransactionRollback(aDB);
              Raise;
            End;
        End; {try..except}
      Finally
        { Destroy the table object. This will close all the files. }
        Table.Free;
      End; {try..finally}
    Finally
      TableList.EndWrite;
    End;
  End;
  {--------}

Begin
  If IsReadOnly Then
    Begin {!!.01 - Start}
      Result := DBIERR_TABLEREADONLY;
      Exit;
    End; {!!.01 - End}

  { Choice of two here: if the cursor ID is set use that, otherwise
    use the databaseID/tablename. }
  If (aCursorID <> 0) Then
    Begin
      Result := CheckCursorIDAndGet(aCursorID, Cursor);
      If Result = DBIERR_NONE Then
        Begin
          Rflags := Cursor.Table.TableFlags;
          If ((Rflags And fsTableDontEmptyTable) <> 0) Then
            Begin
              Result := DBIERR_NOTSUFFFIELDRIGHTS;
              Cursor.Deactivate;
              Exit;
            End;
        End;
      If Result = DBIERR_NONE Then
        Try
          Trans := Nil;
          Try
            FFSetRetry(Cursor.Timeout);
            DB := Cursor.Database;

            Result := Cursor.NotifyExtenders(ffeaBeforeTabEmpty, ffeaTabEmptyFail);
            { Verify the cursor is writable & start an implicit transaction if
              necessary. }
            If (Result = DBIERR_NONE) Then
              Begin
                Result := Cursor.EnsureWritable(False, False, tluDatabase); {!!.02}
                If Result = DBIERR_NOACTIVETRAN Then
                  Result := seTransactionStart(Cursor.Database, False,
                    fscl_TrImplicit, TransID);
                Trans := Cursor.Database.Transaction;
              End;
            If (Result = DBIERR_NONE) Then
              Begin
                Result := Cursor.Empty;
                { If this was an implicit transaction then commit/rollback. }
                If (Result = DBIERR_NONE) And Trans.IsImplicit Then
                  seTransactionCommit(DB)
                Else
                  Begin
                    Cursor.NotifyExtenders(ffeaTabEmptyFail, ffeaNoAction);
                    seTransactionRollback(DB);
                  End; { if }
              End; { if }
          Except
            On E: Exception Do
              Begin
                Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
                If assigned(Trans) And Trans.IsImplicit Then
                  seTransactionRollback(Cursor.Database);
              End;
          End; {try..except}
        Finally
          Cursor.Deactivate;
        End;
    End
  Else {use databaseID/tablename}
    Begin
      Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
      If Result = DBIERR_NONE Then
        Try
          Trans := Nil;
          Try
            FFSetRetry(DB.Timeout);
            Result := DB.NotifyExtenders(ffeaBeforeTabEmpty, ffeaTabDeleteFail);
            If Result = DBIERR_NONE Then
              Begin
                Dict := TFSInfoDict.Create(4096);
                Try
                  Result := seGetDictionary(DB, aTableName, Dict);
                  If (Result = DBIERR_NONE) Then
                    Result := aTableBuildPrim(DB, True, aTableName, False, Dict);
                  If Result <> DBIERR_NONE Then
                    DB.NotifyExtenders(ffeaTabEmptyFail, ffeaNoAction);
                Finally
                  Dict.Free;
                End;
              End;
          Except
            On E: Exception Do
              Begin
                Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
                If assigned(Trans) And Trans.IsImplicit Then
                  seTransactionRollback(DB);
              End;
          End; {try..except}
        Finally
          DB.Deactivate;
        End;
    End;
End;

{--------}

Function TFSServer.TableGetAutoInc(aCursorID: TffCursorID;
  Var aValue: Int64; Var aStep: Longint): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
        If (Result = DBIERR_NONE) Then
          Cursor.ReadAutoInc(aValue, aStep);
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;

Function TFSServer.TableGetMaxRecords(aCursorID: TffCursorID;
  Var aValue: Longword): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
        If (Result = DBIERR_NONE) Then
          Cursor.ReadMaxRecords(aValue);
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;

Function TFSServer.TableGetTableFlags(aCursorID: TffCursorID;
  Var aValue: Word): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
        If (Result = DBIERR_NONE) Then
          Cursor.ReadTableFlags(aValue);
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;

Function TFSServer.TableGetTablePassword(aCursorID: TffCursorID;
  Var aValue: Longword): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
        If (Result = DBIERR_NONE) Then
          Cursor.ReadTablePassword(aValue);
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;

Function TFSServer.TableGetTablePasswordRest(aCursorID: TffCursorID;
  Var aValue: Longword): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
        If (Result = DBIERR_NONE) Then
          Cursor.ReadTablePasswordRest(aValue);
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;

Function TFSServer.TableGetTableDBID(aCursorID: TffCursorID;
  Var aValue: Longword): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
        If (Result = DBIERR_NONE) Then
          Cursor.ReadTableDBID(aValue);
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;

{--------}

Function TFSServer.TableGetDictionary(aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName;
  aForServer: boolean;
  aStream: TStream): TffResult;
Var
  DB: TfsSrcDatabase;
  Dict: TFSInfoDict;
Begin
  Try
    {the database ID must exist}
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If (Result <> DBIERR_NONE) Then
      Exit;

    Try
      FFSetRetry(DB.Timeout);
      Result := DB.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);

      If Result = DBIERR_NONE Then
        Begin
          { We must obtain write access on the engine's table list.  Why?
            Because another thread may be looking for the table at the same
            time.  If the table has not been opened, we don't want that thread
            to open the table while we are opening the table. }
          Dict := TFSInfoServerDict.Create(4096);
          TableList.BeginWrite;
          Try
            Result := seGetDictionary(DB, aTableName, Dict);
            If Result = DBIERR_NONE Then
              Dict.WriteToStream(aStream);
          Finally
            TableList.EndWrite;
            Dict.Free;
          End;
        End;
    Finally
      DB.Deactivate;
    End;
  Except
    On E: Exception Do
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
  End; {try..except}
End;
{--------}

Function TFSServer.TableGetRecCount(aCursorID: TffCursorID;
  Var aRecCount: Longword): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
        If Result = DBIERR_NONE Then
          Result := Cursor.GetRecordCount(aRecCount);
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.TableIsLocked(aCursorID: TffCursorID;
  aLockType: TffLockType;
  Var aIsLocked: boolean): TffResult;
Var
  Cursor: TfsSrBaseCursor;
  ServerLockType: TfsSrcLockType;
Begin
  Result := DBIERR_NONE;
  aIsLocked := False;
  If (aLockType = ffltNoLock) Then
    Exit;

  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
        If Result = DBIERR_NONE Then
          Begin
            ServerLockType := FFMapLock(aLockType, True);
            aIsLocked := Cursor.Table.HasLock(Cursor.CursorID, ServerLockType);
          End;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.TableLockAcquire(aCursorID: TffCursorID;
  aLockType: TffLockType): TffResult;
Var
  Cursor: TfsSrBaseCursor;
  ServerLockType: TfsSrcLockType;
Begin
  Result := DBIERR_NONE;
  If (aLockType = ffltNoLock) Then
    Exit;

  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeTableLock, ffeaTableLockFail);
        If Result = DBIERR_NONE Then
          Try
            ServerLockType := FFMapLock(aLockType, True);
            Cursor.Table.AcqClientLock(aCursorID, ServerLockType, False);
            Cursor.NotifyExtenders(ffeaAfterTableLock, ffeaNoAction);
          Except
            Cursor.NotifyExtenders(ffeaTableLockFail, ffeaNoAction);
            Raise;
          End;
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.TableLockRelease(aCursorID: TffCursorID; aAllLocks: Boolean): TffResult;
Var
  Cursor: TfsSrBaseCursor;
Begin
  Try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(Cursor.Timeout);
        Cursor.Table.RelClientLock(aCursorID, aAllLocks);
      Finally
        Cursor.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.TableOpen(Const aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName;
  Const aForServer: Boolean;
  Const aIndexName: TffName;
  aIndexID: Longint;
  Const aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  Const aTimeout: Longint;
  Var aCursorID: TffCursorID;
  aStream: TStream;
  aSysOpen: boolean = True)
  : TffResult;
Var
  Cursor: TfsSrBaseCursor; {!!.06}
  DB: TfsSrcDatabase;
  IndexID: Longint;
  OpenMode: TffOpenMode;
Begin
  Try
    { The database must exist. }
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If (Result <> DBIERR_NONE) Then
      Exit;

    Try
      FFSetRetry(DB.Timeout);
      Result := DB.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);

      { Change the open mode to ReadOnly if the Server is ReadOnly. }
      If Result = DBIERR_NONE Then
        Begin
          If seConfig.GeneralInfo^.giReadOnly Then
            OpenMode := omReadOnly
          Else
            OpenMode := aOpenMode;

          { The database and table open and share modes must 'match'. }
          If (DB.OpenMode = omReadOnly) And (OpenMode <> omReadOnly) Then
            Begin
              Result := DBIERR_READONLYDB;
              Exit;
            End;
          If (DB.ShareMode = smExclusive) Then
            aShareMode := smExclusive;

          { Create a cursor for the table and return it, add it to the
            server's cursor list. }
          Cursor := CursorClass.Create(Self, DB, aTimeout); {!!.06}
          Try
            Cursor.Open(aTableName, aIndexName, aIndexID, OpenMode, aShareMode,
              aForServer, False, [], aSysOpen);

            CursorList.BeginWrite;
            Try
              CursorList.AddCursor(Cursor);
            Finally
              CursorList.EndWrite;
            End;

            { Get the cursor ID. }
            aCursorID := Cursor.CursorID;
            { Write the information out to the stream - caller's responsibility to
              create and destroy the stream - also to rewind it. }
            If (aStream <> Nil) Then
              Begin
                { First, the cursor ID. }
                aStream.Write(aCursorID, sizeof(aCursorID));

                { Next, the data dictionary. }
                Cursor.Dictionary.WriteToStream(aStream);

                { Finally the IndexID for the cursor. }
                IndexID := Cursor.IndexID;
                aStream.Write(IndexID, sizeof(IndexID));
              End;
          Except
            Cursor.Free;
            Raise;
          End;
        End; { if }
    Finally
      DB.Deactivate;
    End;
  Except
    On E: Exception Do
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
  End; {try..except}
End;
{--------}

Function TFSServer.seTableRenamePrim(DB: TfsSrcDatabase;
  Const aOldName, aNewName: TffName): TffResult;

Var
  Dict: TFSInfoDict;
  Table: TfsSrcBaseTable;
Begin
  Dict := TFSInfoDict.Create(4096);
  TableList.BeginWrite;
  Try
    { Is the table open? }
    Table := GetTableInstance(DB.Folder, aOldName);
    If (Table <> Nil) Then
      Begin
        { Yes. Can it be closed? }
        TableList.RemoveIfUnused(Table);
        If GetTableInstance(DB.Folder, aOldName) <> Nil Then
          Begin
            { No. Return an error. }
            Result := DBIERR_TABLEOPEN;
            Exit;
          End;
      End;
    Result := seGetDictionary(DB, aOldName, Dict);
    { Retrieved the dictionary? }
    If Result = DBIERR_NONE Then
      Begin
        { Yes. Delete the files specified by the dictionary. }
        FFTblHlpRename(DB.Folder.Path, aOldName, aNewName, Dict);
        Result := DBIERR_NONE;
      End
  Finally
    TableList.EndWrite;
    Dict.Free;
  End;
End;
{--------}

Function TFSServer.TableRename(aDatabaseID: TffDatabaseID;
  Const aOldName, aNewName: TffName): TffResult;
Var
  DB: TfsSrcDatabase;
Begin
  Try
    { The table name must be a valid file name without extension. }
    If Not FFVerifyFileName(aNewName) Then
      Begin
        Result := DBIERR_INVALIDTABLENAME;
        Exit;
      End;

    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(DB.Timeout);
        Result := DB.NotifyExtenders(ffeaBeforeTabUpdate, ffeaTabUpdateFail);
        If Result = DBIERR_NONE Then
          Begin
            Result := seTableRenamePrim(DB, aOldName, aNewName);
            If Result <> DBIERR_NONE Then
              DB.NotifyExtenders(ffeaTabUpdateFail, ffeaNoAction);
          End;
      Finally
        DB.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.TableSetAutoInc(aCursorID: TffCursorID;
  aValue: Int64; aStep: Longint): TffResult;
{Restructured !!.10}
Var
  Cursor: TfsSrBaseCursor;
  StartedTrans: Boolean;
  TransID: TffTransID;
  passwd: Longword;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If (Result = DBIERR_NONE) Then
    Try
      StartedTrans := False;
      Try
        FFSetRetry(Cursor.Timeout);
        StartedTrans := False;
        Result := Cursor.NotifyExtenders(ffeaBeforeTabSetInc, ffeaTabSetIncFail);
        If Result = DBIERR_NONE Then
          Begin
            Result := Cursor.EnsureWritable(False, False, tluDatabase); {!!.02}
            If (Result = DBIERR_NOACTIVETRAN) Or
              Cursor.NeedNestedTransaction Then
              Begin {!!.03}
                Result := seTransactionStart(Cursor.Database, False,
                  fscl_TrImplicit, TransID);
                StartedTrans := (Result = DBIERR_NONE);
              End;

            If (Result = DBIERR_NONE) Then
              Begin
                Try
                  Passwd := FFTblReadTablePasswordRest(Cursor.Table.Files[0], Cursor.Database.TransactionInfo);
                  If Passwd > 0 Then
                    Begin
                      If Not Cursor.Database.Session.TablePasswordList.Exists(IntToStr(Passwd)) Then
                        Result := DBIERR_NOTSUFFFIELDRIGHTS
                      Else
                        Cursor.SetAutoInc(aValue, aStep);
                    End
                  Else
                    Cursor.SetAutoInc(aValue, aStep);
                Except
                  Cursor.NotifyExtenders(ffeaTabSetIncFail, ffeaNoAction);
                  Raise;
                End;
                If StartedTrans Then
                  Begin
                    If (Result = DBIERR_NONE) Then
                      Result := seTransactionCommit(Cursor.Database)
                    Else
                      seTransactionRollback(Cursor.Database);
                  End;
              End;
          End; { if }
      Except
        On E: Exception Do
          Begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            If StartedTrans Then
              seTransactionRollback(Cursor.Database);
          End;
      End; {try..except}
    Finally
      Cursor.Deactivate;
    End;
End;

Function TFSServer.TableSetMaxRecords(aCursorID: TffCursorID;
  aValue: Longint): TffResult;
{Restructured !!.10}
Var
  Cursor: TfsSrBaseCursor;
  StartedTrans: Boolean;
  TransID: TffTransID;
  passwd: Longword;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If (Result = DBIERR_NONE) Then
    Try
      StartedTrans := False;
      Try
        FFSetRetry(Cursor.Timeout);
        StartedTrans := False;
        Result := Cursor.NotifyExtenders(ffeaBeforeTabSetMaxRec, ffeaTabSetMaxRecFail);
        If Result = DBIERR_NONE Then
          Begin
            Result := Cursor.EnsureWritable(False, False, tluDatabase); {!!.02}
            If (Result = DBIERR_NOACTIVETRAN) Or
              Cursor.NeedNestedTransaction Then
              Begin {!!.03}
                Result := seTransactionStart(Cursor.Database, False,
                  fscl_TrImplicit, TransID);
                StartedTrans := (Result = DBIERR_NONE);
              End;

            If (Result = DBIERR_NONE) Then
              Begin
                Try
                  Passwd := FFTblReadTablePasswordRest(Cursor.Table.Files[0], Cursor.Database.TransactionInfo);
                  If Passwd > 0 Then
                    Begin
                      If Not Cursor.Database.Session.TablePasswordList.Exists(IntToStr(Passwd)) Then
                        Result := DBIERR_NOTSUFFFIELDRIGHTS
                      Else
                        Cursor.SetMaxRecords(aValue);
                    End
                  Else
                    Cursor.SetMaxRecords(aValue);
                Except
                  Cursor.NotifyExtenders(ffeaTabSetMaxRecFail, ffeaNoAction);
                  Raise;
                End;
                If StartedTrans Then
                  Begin
                    If (Result = DBIERR_NONE) Then
                      Result := seTransactionCommit(Cursor.Database)
                    Else
                      seTransactionRollback(Cursor.Database);
                  End;
              End;
          End; { if }
      Except
        On E: Exception Do
          Begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            If StartedTrans Then
              seTransactionRollback(Cursor.Database);
          End;
      End; {try..except}
    Finally
      Cursor.Deactivate;
    End;
End;

Function TFSServer.TableSetTableFlags(aCursorID: TffCursorID;
  aValue: Word): TffResult;
{Restructured !!.10}
Var
  Cursor: TfsSrBaseCursor;
  StartedTrans: Boolean;
  TransID: TffTransID;
  Passwd: Longword;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If (Result = DBIERR_NONE) Then
    Try
      StartedTrans := False;
      Try
        FFSetRetry(Cursor.Timeout);
        StartedTrans := False;
        Result := Cursor.NotifyExtenders(ffeaBeforeTabSetFlags, ffeaTabSetFlagsFail);
        If Result = DBIERR_NONE Then
          Begin
            Result := Cursor.EnsureWritable(False, False, tluDatabase); {!!.02}
            If (Result = DBIERR_NOACTIVETRAN) Or
              Cursor.NeedNestedTransaction Then
              Begin {!!.03}
                Result := seTransactionStart(Cursor.Database, False,
                  fscl_TrImplicit, TransID);
                StartedTrans := (Result = DBIERR_NONE);
              End;

            If (Result = DBIERR_NONE) Then
              Begin
                Try
                  Passwd := FFTblReadTablePasswordRest(Cursor.Table.Files[0], Cursor.Database.TransactionInfo);
                  If Passwd > 0 Then
                    Begin
                      If Not Cursor.Database.Session.TablePasswordList.Exists(IntToStr(Passwd)) Then
                        Result := DBIERR_NOTSUFFFIELDRIGHTS
                      Else
                        Cursor.SetTableFlags(aValue);
                    End
                  Else
                    Cursor.SetTableFlags(aValue);
                Except
                  Cursor.NotifyExtenders(ffeaTabSetFlagsFail, ffeaNoAction);
                  Raise;
                End;
                If StartedTrans Then
                  Begin
                    If (Result = DBIERR_NONE) Then
                      Result := seTransactionCommit(Cursor.Database)
                    Else
                      seTransactionRollback(Cursor.Database);
                  End;
              End;
          End; { if }
      Except
        On E: Exception Do
          Begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            If StartedTrans Then
              seTransactionRollback(Cursor.Database);
          End;
      End; {try..except}
    Finally
      Cursor.Deactivate;
    End;
End;

Function TFSServer.TableSetTablePassword(aCursorID: TffCursorID;
  aValue: Longword): TffResult;
{Restructured !!.10}
Var
  Cursor: TfsSrBaseCursor;
  StartedTrans: Boolean;
  TransID: TffTransID;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If (Result = DBIERR_NONE) Then
    Try
      StartedTrans := False;
      Try
        FFSetRetry(Cursor.Timeout);
        StartedTrans := False;
        Result := Cursor.NotifyExtenders(ffeaBeforeTabSetFlags, ffeaTabSetFlagsFail);
        If Result = DBIERR_NONE Then
          Begin
            Result := Cursor.EnsureWritable(False, False, tluDatabase); {!!.02}
            If (Result = DBIERR_NOACTIVETRAN) Or
              Cursor.NeedNestedTransaction Then
              Begin {!!.03}
                Result := seTransactionStart(Cursor.Database, False,
                  fscl_TrImplicit, TransID);
                StartedTrans := (Result = DBIERR_NONE);
              End;

            If (Result = DBIERR_NONE) Then
              Begin
                Try
                  Cursor.SetTablePassword(aValue);
                Except
                  Cursor.NotifyExtenders(ffeaTabSetFlagsFail, ffeaNoAction);
                  Raise;
                End;
                If StartedTrans Then
                  Begin
                    If (Result = DBIERR_NONE) Then
                      Result := seTransactionCommit(Cursor.Database)
                    Else
                      seTransactionRollback(Cursor.Database);
                  End;
              End;
          End; { if }
      Except
        On E: Exception Do
          Begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            If StartedTrans Then
              seTransactionRollback(Cursor.Database);
          End;
      End; {try..except}
    Finally
      Cursor.Deactivate;
    End;
End;

Function TFSServer.TableSetTablePasswordRest(aCursorID: TffCursorID;
  aValue: Longword): TffResult;
{Restructured !!.10}
Var
  Cursor: TfsSrBaseCursor;
  StartedTrans: Boolean;
  TransID: TffTransID;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If (Result = DBIERR_NONE) Then
    Try
      StartedTrans := False;
      Try
        FFSetRetry(Cursor.Timeout);
        StartedTrans := False;
        Result := Cursor.NotifyExtenders(ffeaBeforeTabSetFlags, ffeaTabSetFlagsFail);
        If Result = DBIERR_NONE Then
          Begin
            Result := Cursor.EnsureWritable(False, False, tluDatabase); {!!.02}
            If (Result = DBIERR_NOACTIVETRAN) Or
              Cursor.NeedNestedTransaction Then
              Begin {!!.03}
                Result := seTransactionStart(Cursor.Database, False,
                  fscl_TrImplicit, TransID);
                StartedTrans := (Result = DBIERR_NONE);
              End;

            If (Result = DBIERR_NONE) Then
              Begin
                Try
                  Cursor.SetTablePasswordRest(aValue);
                Except
                  Cursor.NotifyExtenders(ffeaTabSetFlagsFail, ffeaNoAction);
                  Raise;
                End;
                If StartedTrans Then
                  Result := seTransactionCommit(Cursor.Database);
              End;
          End; { if }
      Except
        On E: Exception Do
          Begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            If StartedTrans Then
              seTransactionRollback(Cursor.Database);
          End;
      End; {try..except}
    Finally
      Cursor.Deactivate;
    End;
End;

Function TFSServer.TableSetTableDBID(aCursorID: TffCursorID;
  aValue: Longword): TffResult;
{Restructured !!.10}
Var
  Cursor: TfsSrBaseCursor;
  StartedTrans: Boolean;
  TransID: TffTransID;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If (Result = DBIERR_NONE) Then
    Try
      StartedTrans := False;
      Try
        FFSetRetry(Cursor.Timeout);
        StartedTrans := False;
        Result := Cursor.NotifyExtenders(ffeaBeforeTabSetFlags, ffeaTabSetFlagsFail);
        If Result = DBIERR_NONE Then
          Begin
            Result := Cursor.EnsureWritable(False, False, tluDatabase); {!!.02}
            If (Result = DBIERR_NOACTIVETRAN) Or
              Cursor.NeedNestedTransaction Then
              Begin {!!.03}
                Result := seTransactionStart(Cursor.Database, False,
                  fscl_TrImplicit, TransID);
                StartedTrans := (Result = DBIERR_NONE);
              End;

            If (Result = DBIERR_NONE) Then
              Begin
                Try
                  Cursor.SetTableDBID(aValue);
                Except
                  Cursor.NotifyExtenders(ffeaTabSetFlagsFail, ffeaNoAction);
                  Raise;
                End;
                If StartedTrans Then
                  Result := seTransactionCommit(Cursor.Database);
              End;
          End; { if }
      Except
        On E: Exception Do
          Begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            If StartedTrans Then
              seTransactionRollback(Cursor.Database);
          End;
      End; {try..except}
    Finally
      Cursor.Deactivate;
    End;
End;

{--------}
{Begin !!.11}

Function TFSServer.TableVersion(aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName;
  Var aVersion: Longint): TffResult;
Var
  DB: TfsSrcDatabase;
  FI: TffFileInfo;
  FileHandle: THandle;
  Table: TfsSrcBaseTable;
  TableDataFile: TffFullFileName;
  PTableDataFile: PAnsiChar;
  Header: TffBlockHeaderFile;
Begin
  PTableDataFile := Nil;
  Try
    {the database ID must exist}
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If (Result <> DBIERR_NONE) Then
      Exit;

    Try
      FFSetRetry(DB.Timeout);
      Result := DB.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
      If Result = DBIERR_NONE Then
        Begin
          { If the table is already open then return the version number from the
            internal file data structure. Otherwise, open the main file for the
            table & retrieve the version number from its header block. }
          seTableList.BeginWrite;
          Try
            { Try & find the open table in the engine's table list. If it exists already
              then reference the existing table. }
            Table := GetTableInstance(DB.Folder, aTableName);

            { Is the table open? }
            If assigned(Table) Then
              { Yes. Return version # from in-memory information. }
              aVersion := Table.Files[0].fiFSVersion
            Else If seTableExistsPrim(DB, aTableName) Then
              Begin
                { Table exists. Open the file directly & retrieve the version number
                  from its header block. }
                TableDataFile := FFMakeFullFileName
                  (DB.Folder.Path,
                  FFMakeFileNameExt(aTableName, fsc_ExtForData));
                FFGetMem(PTableDataFile, Length(TableDataFile) + 1);
                StrPCopy(PTableDataFile, TableDataFile);
                FileHandle := FFOpenFilePrim(PTableDataFile, omReadOnly,
                  smShareRead, False, False);
                Try
                  FI.fiHandle := FileHandle;
                  FI.fiName := FFShStrAlloc(TableDataFile);
                  FFReadFilePrim(@FI, SizeOf(TffBlockHeaderFile), Header);
                  aVersion := Header.bhfFSVersion;
                Finally
                  FFCloseFilePrim(@FI);
                  FFShStrFree(FI.fiName);
                End;
              End
            Else
              { The file does not exist. Raise an error. }
              FSRaiseException(EfsException, fsStrResServer, fserrUnknownTable,
                [aTableName, DB.Alias]);
          Finally
            If PTableDataFile <> Nil Then
              FFFreeMem(PTableDataFile, StrLen(PTableDataFile) + 1);
            seTableList.EndWrite;
          End;
        End; { if }

    Finally
      DB.Deactivate;
    End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{End !!.11}
{--------}

Function TFSServer.seConvertSingleField(aSourceBuf,
  aTargetBuf: PffByteArray;
  aSourceCursorID,
  aTargetCursorID: Longint;
  aSourceFldNr,
  aTargetFldNr: Integer;
  aBLOBBuffer: Pointer;
  aBLOBBufLen: Longint;
  aRangError: boolean): TffResult;
Var
  SourceValue: Pointer;
  TargetValue: Pointer;
  SourceType: TfsFieldType;
  TargetType: TfsFieldType;
  SourceLength: Longint;
  SourceDecimals: Integer;
  TargetLength: Longint;
  TargetFieldBlobLevelComp,
    SourceFieldBlobLevelComp: TDataCompLevel;
  SourceFieldRound, TargetFieldRound: TRound;
  SourceCursor,
    TargetCursor: TfsSrBaseCursor;
  TargetSize,
    TargetDecimals, SourceBlobLen: Integer;
  aString: String;
  p: PChar;
  r: tffresult;
Begin
  Result := DBIERR_NONE;
  Try
    seCheckCursorIDAndGet(aSourceCursorID, SourceCursor);
    seCheckCursorIDAndGet(aTargetCursorID, TargetCursor);

    SourceValue := Nil;
    TargetValue := Nil;

    With SourceCursor.Table.Dictionary Do
      Begin
        SourceType := FieldType[aSourceFldNr];
        SourceLength := FieldLength[aSourceFldNr];
        SourceFieldBlobLevelComp := FieldBlobLevelComp[aSourceFldNr];
        SourceDecimals := FieldDecPl[aSourceFldNr];
        SourceFieldRound := FieldRound[aSourceFldNr];
        If Assigned(aSourceBuf) Then
          Begin

            { If input field is a null, then output is automatically a null
              regardless of datatype. }
            If IsRecordFieldNull(aSourceFldNr, aSourceBuf) Then
              Begin
                TargetCursor.Table.Dictionary.SetRecordField(aTargetFldNr, aTargetBuf, Nil);
                Exit;
              End;

            { also count input field as null if it's a stringtype, the field
              conains the empty string, and output field is a blob. }
            If (TargetCursor.Table.Dictionary.FieldType[aTargetFldNr] In [fstBLOB..ffcLastBlobType]) And
              (((FieldType[aSourceFldNr] In [fstNullString, fstVarNullString]) And
              (Byte(aSourceBuf^[FieldOffset[aSourceFldNr]]) = 0)) Or
              ((FieldType[aSourceFldNr] In [fstShortString]) And
              (Byte(aSourceBuf^[FieldOffset[aSourceFldNr] + 1]) = 0)) Or
              ((FieldType[aSourceFldNr] In [fstWideString, fstVarWideString {, fstUnicode}]) And
              (WideChar(aSourceBuf^[FieldOffset[aSourceFldNr]]) = ''))) Then
              Begin
                TargetCursor.Table.Dictionary.SetRecordField(aTargetFldNr, aTargetBuf, Nil);
                Exit;
              End;

            SourceValue := Addr(aSourceBuf^[FieldOffset[aSourceFldNr]]);

            If SourceType In [fstBLOB..ffcLastBLOBType] Then
              Begin
                If (Not FFVerifyBLOBNr(TffInt64(SourceValue^),
                  SourceCursor.Table.Files[SourceCursor.Table.Dictionary.BLOBFileNumber].fiLog2BlockSize)) Or
                  SourceCursor.IsDeletedBLOB(TffInt64(SourceValue^), r) Then
                  Begin
                    TargetCursor.Table.Dictionary.SetRecordField(aTargetFldNr, aTargetBuf, Nil);
                    Exit;
                  End
                Else
                  Begin
                    SourceBlobLen := SourceCursor.BLOBGetLength(TffInt64(SourceValue^), r);
                    If SourceBlobLen = 0 Then
                      Begin
                        TargetCursor.Table.Dictionary.SetRecordField(aTargetFldNr, aTargetBuf, Nil);
                        Exit;
                      End;
                  End;
              End;

          End;
      End;

    With TargetCursor.Table.Dictionary Do
      Begin
        If Assigned(aTargetBuf) Then
          TargetValue := Addr(aTargetBuf^[FieldOffset[aTargetFldNr]]);

        TargetType := FieldType[aTargetFldNr];
        TargetLength := FieldLength[aTargetFldNr];
        TargetSize := FieldUnits[aTargetFldNr];
        TargetDecimals := FieldDecPl[aTargetFldNr];
        TargetFieldBlobLevelComp := FieldBlobLevelComp[aTargetFldNr];
        TargetFieldRound := FieldRound[aTargetFldNr];
      End;

    Result := FSConvertSingleField(SourceValue, TargetValue,
      SourceType, TargetType, SourceLength, TargetLength,
      TargetSize, SourceDecimals, TargetDecimals,
      SourceFieldRound, TargetFieldRound, aRangError);

    //if (Result <> DBIERR_NONE) and aRangeError then
    If Assigned(aTargetBuf) And (Result = DBIERR_NONE) Then
      Begin

        { Field is not null }
        With TargetCursor.Table.Dictionary Do
          FFClearBit(@aTargetBuf^[LogicalRecordLength], aTargetFldNr);

        { Handle BLOB targets }
        If TargetType In [fstBLOB..ffcLastBLOBType] Then
          Begin
            Result := BLOBCreate(TargetCursor.CursorID, TffInt64(TargetValue^));
            If Result = DBIERR_NONE Then
              If SourceType In [fstBLOB..ffcLastBLOBType] Then
                Begin
                  Result := seBLOBCopy(SourceCursor,
                    TargetCursor,
                    TffInt64(SourceValue^),
                    TffInt64(TargetValue^),
                    aBLOBBuffer,
                    aBLOBBufLen,
                    SourceFieldBlobLevelComp,
                    TargetFieldBlobLevelComp);
                End
              Else If Not (SourceType In [fstBLOB..ffcLastBLOBType]) Then
                Begin
                  aString := FSConvertSingleFieldToString(SourceValue, SourceType, SourceLength, SourceDecimals, SourceFieldRound, TargetLength);
                  If aString <> '' Then
                    Begin
                      p := PChar(aString);
                      Result := TargetCursor.BLOBWrite(TffInt64(TargetValue^),
                        0,
                        TargetLength,
                        P^);
                    End;
                End
              Else
                Begin
                  Result := TargetCursor.BLOBWrite(TffInt64(TargetValue^),
                    0,
                    SourceLength,
                    SourceValue^);
                End;
          End
        Else If (SourceType In [fstBLOB..ffcLastBLOBType]) And (TargetType In [fstShortString..fstWideString]) Then
          Begin
            // source is blob
            // target try convert to other
            // not yet
            Result := fsBlobToString(SourceCursor.CursorID, aSourceFldNr,
              aSourceBuf, TargetValue, TargetType, TargetLength, aRangError);
          End;
      End;
  Except
    {Begin !!.13}
    On E: EOverFlow Do
      Result := DBIERR_INVALIDFLDXFORM;
    {$IFOPT R+}
    On E: ERangeError Do
      Result := DBIERR_INVALIDFLDXFORM;
    {$ENDIF}
    {End !!.13}
    On E: Exception Do
      Begin
        If Result = DBIERR_NONE Then
          Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End;
End;
{--------}
{ Include code for asynchronous requests. }
{$I fssrridx.inc}
{$I fssrpack.inc}
{$I fssrrest.inc}
{$I fssrrcnt.inc}
{--------}

Function TFSServer.TransactionStartSQL(Const aDatabaseID: TffDatabaseID;
  Const notifyExtenders: boolean): TffResult;
Var
  aTransID: TffTransID;
Begin
  Result := seTransactionStart(TfsSrcDatabase(aDatabaseID), False, True, aTransID);
  {Begin !!.06}
  If (Result = DBIERR_NONE) Then
    Begin
      TfsSrcDatabase(aDatabaseID).Transaction.IsReadOnly := True;
      If notifyExtenders Then
        TfsSrcDatabase(aDatabaseID).NotifyExtenders(ffeaAfterStartTrans, ffeaNoAction);
    End;
  {End !!.06}
End;
{End !!.01}
{Begin !!.10}
{--------}

Function TFSServer.TransactionStartWith(Const aDatabaseID: TffDatabaseID;
  Const aFailSafe: Boolean;
  Const aCursorIDs: TfsPointerList): TffResult;
Var
  RetryUntil: DWORD;
  DB: TfsSrcDatabase;
  TransID: TffTransID;
  Limit,
    anIndex: Longint;
  aCursorID: TffCursorID;
  Cursor: TfsSrBaseCursor;
  Lock: TfsPadlock;
  GetCursorResult: TffResult; {!!.13}
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
  If (Result = DBIERR_NONE) Then
    Try
      FFSetRetry(DB.Timeout);
      Result := seTransactionStart(DB, aFailSafe, fscl_TrExplicit,
        TransID);
      If Result = DBIERR_NONE Then
        Try
          Lock := DB.Folder.LockMgr.StartWithLock;
          { Retry this operation until it is successful or we reach the database
            timeout limit. }
          RetryUntil := FFGetRetry;
          Repeat
            If Result <> DBIERR_NONE Then
              Sleep(fsc_StartTranWithDelay);
            Limit := 0;
            Lock.Lock;
            Try
              For anIndex := 0 To pred(aCursorIDs.Count) Do
                Begin
                  aCursorID := TffCursorID(aCursorIDs[anIndex]);
                  Result := CheckCursorIDAndGet(aCursorID, Cursor);
                  If Result = DBIERR_NONE Then
                    Try
                      Result := Cursor.AcqExclContentLock;
                      If Result <> DBIERR_NONE Then
                        Begin
                          Limit := pred(anIndex);
                          Break;
                        End;
                    Finally
                      Cursor.Deactivate;
                    End
                  Else
                    Break;
                End; { for }
              If Result <> DBIERR_NONE Then
                For anIndex := 0 To Limit Do
                  Begin
                    aCursorID := TffCursorID(aCursorIDs[anIndex]);
                    GetCursorResult := CheckCursorIDAndGet(aCursorID, Cursor); {!!.13}
                    If GetCursorResult = DBIERR_NONE Then
                      Begin {!!.13}
                        Cursor.RelContentLock(fsclmWrite);
                      End;
                  End; { for }
            Finally
              Lock.Unlock;
            End;

          Until (Result = DBIERR_NONE) Or
            (RetryUntil <= (GetTickCount - 10));

          If Result = DBIERR_NONE Then
            DB.NotifyExtenders(ffeaAfterStartTrans, ffeaNoAction)
          Else
            Begin
              seTransactionRollback(DB);
              If Result = fserrLockRejected Then
                Result := DBIERR_LOCKED;
            End;

        Except
          On E: Exception Do
            Begin
              Result := ConvertServerExceptionEx(E, FEventLog,
                bseGetReadOnly);
              seTransactionRollback(DB);
            End;
        End;
    Finally
      DB.Deactivate;
    End;
End;
{End !!.10}

{--------}

Function TFSServer.InTransaction(Const aDatabaseID: TffDatabaseID; Var aTransLevel: Longint): TffResult;
Begin
  Result := seInTransaction(aDatabaseID, aTransLevel);
End;

Function TFSServer.seInTransaction(Const aDatabaseID: TffDatabaseID;
  Var aTransLevel: Longint): TffResult;
Var
  DB: TfsSrcDatabase;
Begin
  aTransLevel := 0;
  Try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(DB.Timeout);
        If DB.Transaction = Nil Then
          Result := DBIERR_NOACTIVETRAN
        Else
          aTransLevel := DB.Transaction.TransLevel.Level;
      Finally
        DB.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;

Function TFSServer.TransactionCorrupted(Const aDatabaseID: TffDatabaseID): TffResult;
Begin
  Result := seTransactionCorrupted(aDatabaseID);
End;

Function TFSServer.seTransactionCorrupted(Const aDatabaseID: TffDatabaseID): TffResult;
Var
  DB: TfsSrcDatabase;
Begin
  Try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(DB.Timeout);
        If DB.Transaction = Nil Then
          Result := DBIERR_NOACTIVETRAN
        Else
          DB.Transaction.IsCorrupt := True;
      Finally
        DB.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;

Function TFSServer.seTransactionStart(Const aDB: TfsSrcDatabase;
  Const aFailSafe, aImplicit: boolean;
  Var aTransactionID: TffTransID): TffResult;
Var
  aTrans: TfsSrcTransaction;
Begin
  Try
    Result := aDB.Folder.TransactionMgr.StartTransaction
      (aDB.DatabaseID, aFailSafe, aImplicit,
      False, aDB.Folder.Path, aTrans);
    aDB.Transaction := aTrans;
    aTransactionID := aTrans.TransactionID;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;

Function TFSServer.seTransactionRollback(aDB: TfsSrcDatabase)
  : TffResult;
{Rewritten !!.03}
Var
  aContainer: TfsTransContainer;
  aInx: Longint;
  aTable: TfsSrcTable;
  aTableList: TfsPointerList;
  Nested: Boolean;
Begin
  Result := DBIERR_NONE;
  If aDB.Transaction <> Nil Then
    Begin {!!.05}
      aTableList := TfsPointerList.Create;
      aContainer := TfsTransContainer(aDB.Transaction.TransLockContainer);
      Nested := aDB.Transaction.Nested;
      Try
        { Determine which tables were affected by the transaction. We will rollback
          the changes to their BLOB mgr's in-memory deleted chain. }
        If assigned(aContainer) And (Not Nested) Then
          For aInx := 0 To pred(aContainer.ContentCount) Do
            If aContainer.ContentLockType[aInx] = ffsltExclusive Then
              Begin
                aTable := TfsSrcTable(aContainer.ContentTable[aInx]);
                aTableList.Append(Pointer(aTable));
              End;

        { Tell the transaction manager to rollback. }
        aDB.Folder.TransactionMgr.Rollback(aDB.TransactionID, Nested);

        { Nested transaction? }
        If (Not Nested) Then
          Begin
            { No. For each table involved, rollback the changes to the BLOB resource
              manager's in-memory deleted chain. }
            For aInx := 0 To pred(aTableList.Count) Do
              Begin
                aTable := TfsSrcTable(aTableList.List[aInx]);
                aTable.btRollbackBLOBMgr;
              End;
            If aDB.Transaction <> Nil Then
              Begin
                seBufMgr.bmRemoveCommittedPages(aDB.Transaction);
                seBufMgr.bmRemoveExcessPages;
              End;
            aDB.Transaction := Nil;
            For aInx := Pred(aDB.dbCursorList.CursorCount) Downto 0 Do {!!.13}
              If ((TfsSrBaseCursor(aDB.dbCursorList.solList.Items[aInx]) <> Nil) And
                (TfsSrBaseCursor(aDB.dbCursorList.solList.Items[aInx]).bcCloseWTrans)) Then
                TfsSrBaseCursor(aDB.dbCursorList.solList.Items[aInx]).RemoveIfUnused;
            If Configuration.GeneralInfo^.giCloseInactiveTablesAfterCommitOrRoolback Then
              TableList.RemoveUnusedTAbles;
            FolderList.RemoveUnusedFolders;
          End;
      Finally
        aTableList.Free;
      End;
    End; {!!.05}
End;
{--------}

Function TFSServer.TransactionCommit(Const aDatabaseID: TffDatabaseID; aRemoveFile: Boolean = False): TffResult;
Var
  DB: TfsSrcDatabase;
Begin
  Try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(DB.Timeout);
        If DB.Transaction = Nil Then
          Result := DBIERR_NOACTIVETRAN
        Else If DB.Transaction.IsCorrupt Then
          Begin
            DB.NotifyExtenders(ffeaBeforeRollback, ffeaNoAction);
            seTransactionRollback(DB);
            Result := DBIERR_FS_CorruptTrans;
            DB.NotifyExtenders(ffeaAfterRollback, ffeaNoAction);
          End
        Else
          Begin
            Result := DB.NotifyExtenders(ffeaBeforeCommit, ffeaCommitFail);
            If Result = DBIERR_NONE Then
              Begin
                seTransactionCommit(DB, aRemoveFile);
                DB.NotifyExtenders(ffeaAfterCommit, ffeaNoAction);
              End;
          End;
      Finally
        DB.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.TransactionCommitSubset(Const aDatabaseID: TffDatabaseID): TffResult;
Var
  DB: TfsSrcDatabase;
Begin
  Try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If (Result = DBIERR_NONE) Then
      Try
        FFSetRetry(DB.Timeout);
        Result := seTransactionCommitSubset(DB);
      Finally
        DB.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{Begin !!.01
{--------}

Function TFSServer.TransactionCommitSQL(Const aDatabaseID: TffDatabaseID;
  Const notifyExtenders: Boolean): TffResult;
Var
  aDB: TfsSrcDatabase;
Begin
  aDB := TfsSrcDatabase(aDatabaseID);
  //If aDB <> Nil Then
    //If aDB.Transaction <> Nil Then
  If aDB.Transaction.IsCorrupt Then
    Begin
      If notifyExtenders Then
        aDB.NotifyExtenders(ffeaBeforeRollback, ffeaNoAction);
      seTransactionRollback(aDB);
      Result := DBIERR_FS_CorruptTrans;
      If notifyExtenders Then
        aDB.NotifyExtenders(ffeaAfterRollback, ffeaNoAction);
    End
  Else
    Begin
      If notifyExtenders Then
        aDB.NotifyExtenders(ffeaBeforeCommit, ffeaNoAction);
      Result := seTransactionCommit(aDB);
      If notifyExtenders Then
        aDB.NotifyExtenders(ffeaAfterCommit, ffeaNoAction);
    End;
End;
{End !!.01}
{--------}

Function TFSServer.seTransactionCommit(aDB: TfsSrcDatabase; aRemoveFile: Boolean = False)
  : TffResult;
Var
  aContainer: TfsTransContainer;
  aInx: Longint;
  aTable: TfsSrcTable;
  aTableList: TfsPointerList;
  Nested: Boolean;
  Committed: Boolean;
Begin
  Committed := False;
  { Obtain a commit lock on all tables this transaction has modified.
    We must do this to make sure the readers have finished. }
  aTableList := TfsPointerList.Create;
  aContainer := TfsTransContainer(aDB.Transaction.TransLockContainer);
  Nested := aDB.Transaction.Nested;
  Try
    If assigned(aContainer) And (Not Nested) Then
      For aInx := 0 To pred(aContainer.ContentCount) Do
        Begin
          If aContainer.ContentLockType[aInx] = ffsltExclusive Then
            Begin
              aTable := TfsSrcTable(aContainer.ContentTable[aInx]);
              aTable.BeginCommit;
              aTableList.Append(Pointer(aTable));
            End;
        End;

    Result := aDB.Folder.TransactionMgr.Commit(aDB.TransactionID, Nested);
    Committed := (Result = DBIERR_NONE);

    //If (Not Nested) Then
     // aDB.Transaction := Nil;
  Finally
    If (Not Nested) Then
      Begin
        For aInx := 0 To pred(aTableList.Count) Do
          Begin
            aTable := TfsSrcTable(aTableList.List[aInx]);
            If (Committed) Then
              aTable.btCommitBLOBMgr;
            aTable.EndCommit(aDB.DatabaseID);
          End;
      End;
    aTableList.Free;
  End;
  If (Not Nested) Then
    If aDB.Transaction <> Nil Then
      Begin
        seBufMgr.bmRemoveCommittedPages(aDB.Transaction);
        seBufMgr.bmRemoveExcessPages;
      End;
  If (Not Nested) Then
    aDB.Transaction := Nil;

  If (Not Nested) And Committed Then
    Begin
      For aInx := Pred(aDB.dbCursorList.CursorCount) Downto 0 Do
        Begin
          If ((TfsSrBaseCursor(aDB.dbCursorList.solList.Items[aInx]) <> Nil) And
            (TfsSrBaseCursor(aDB.dbCursorList.solList.Items[aInx]).bcCloseWTrans)) Then
            TfsSrBaseCursor(aDB.dbCursorList.solList.Items[aInx]).RemoveIfUnused;
        End;

      If Configuration.GeneralInfo^.giCloseInactiveTablesAfterCommitOrRoolback Then
        TableList.RemoveUnusedTAbles;
      FolderList.RemoveUnusedFolders;
    End;
End;
{--------}

Function TFSServer.seTransactionCommitSubset(Const aDB: TfsSrcDatabase): TffResult;
{ Rewritten !!.03}
Var
  aContainer: TfsTransContainer;
  aInx: Longint;
  aTable: TfsSrcTable;
  aTableList: TfsPointerList;
  Nested: Boolean;
Begin
  Result := DBIERR_NONE;
  If aDB.Transaction.IsCorrupt Then
    Begin
      aDB.NotifyExtenders(ffeaBeforeRollback, ffeaNoAction);
      seTransactionRollback(aDB);
      Result := DBIERR_FS_CorruptTrans;
      aDB.NotifyExtenders(ffeaAfterRollback, ffeaNoAction);
    End
  Else
    Begin
      aTableList := TfsPointerList.Create;
      aContainer := TfsTransContainer(aDB.Transaction.TransLockContainer);
      Nested := aDB.Transaction.Nested;

      Try
        { Determine which tables were affected by the transaction. We will
          commit the changes to their BLOB mgr's in-memory deleted chain. }
        If assigned(aContainer) And (Not Nested) Then
          For aInx := 0 To pred(aContainer.ContentCount) Do
            If aContainer.ContentLockType[aInx] = ffsltExclusive Then
              Begin
                aTable := TfsSrcTable(aContainer.ContentTable[aInx]);
                aTableList.Append(Pointer(aTable));
              End;

        aDB.NotifyExtenders(ffeaBeforeCommit, ffeaNoAction);
        seBufMgr.CommitTransactionSubset(aDB.Transaction);

        { Nested transaction? }
        If (Not Nested) Then
          Begin
            { No. Release transaction locks. For each table involved, commit the
              changes to the BLOB resource manager's in-memory deleted chain. }
            aDB.Folder.LockMgr.ReleaseTransactionLocks(aDB.Transaction, True);
            For aInx := 0 To pred(aTableList.Count) Do
              Begin
                aTable := TfsSrcTable(aTableList.List[aInx]);
                aTable.btCommitBLOBMgr;
              End;
          End;

        aDB.NotifyExtenders(ffeaAfterCommit, ffeaNoAction);
      Finally
        aTableList.Free;
      End;
    End;
End;
{--------}

Function TFSServer.TransactionRollback(Const aDatabaseID: TffDatabaseID): TffResult;
Var
  DB: TfsSrcDatabase;
Begin
  Try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If (Result = DBIERR_NONE) Then
      Try

        If Not assigned(DB.Transaction) Then
          Begin
            Result := DBIERR_NOACTIVETRAN;
            Exit;
          End;

        FFSetRetry(DB.Timeout);

        DB.NotifyExtenders(ffeaBeforeRollback, ffeaNoAction);
        seTransactionRollback(DB);
        DB.NotifyExtenders(ffeaAfterRollback, ffeaNoAction);
      Finally
        DB.Deactivate;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{Begin !!.01}
{--------}

Function TFSServer.TransactionRollbackSQL(Const aDatabaseID: TffDatabaseID;
  Const notifyExtenders: Boolean): TffResult;
Var
  aDB: TfsSrcDatabase;
Begin
  aDB := TfsSrcDatabase(aDatabaseID);
  If notifyExtenders Then
    aDB.NotifyExtenders(ffeaBeforeRollback, ffeaNoAction);
  Result := seTransactionRollback(aDB);
  If notifyExtenders Then
    aDB.NotifyExtenders(ffeaAfterRollback, ffeaNoAction);
End;
{End !!.01}
{--------}

Function TFSServer.bseGetAutoSaveCfg: Boolean;
Begin
  Result := seConfig.GeneralInfo^.giNoAutoSaveCfg;
End;
{--------}

Function TFSServer.bseGetReadOnly: boolean;
Begin
  Result := seConfig.GeneralInfo^.giReadOnly;
End;
{--------}

Procedure TFSServer.bseSetAutoSaveCfg(aValue: Boolean); {!!.01 - Start}
Begin
  seConfig.GeneralInfo^.giNoAutoSaveCfg := aValue;
End;
{--------}

Procedure TFSServer.bseSetReadOnly(aValue: Boolean);
Begin
  seConfig.GeneralInfo^.giReadOnly := aValue;
End;
{--------}{!!.01 - End}

Function TFSServer.TransactionStart(Const aDatabaseID: TffDatabaseID;
  Const aFailSafe: Boolean): TffResult;
Var
  DB: TfsSrcDatabase;
  TransID: TffTransID;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
  If (Result = DBIERR_NONE) Then
    Try
      FFSetRetry(DB.Timeout);
      Result := seTransactionStart(DB, aFailSafe, fscl_TrExplicit,
        TransID);
      If Result = DBIERR_NONE Then
        DB.NotifyExtenders(ffeaAfterStartTrans, ffeaNoAction);
    Finally
      DB.Deactivate;
    End;
End;
{Begin !!.01}
{--------}

{===Script processing================================================}

Function TFSServer.CalcPriorityIndex(Const PriorityStr: TffShStr): Integer;
Const
  PriorityValues: Array[0..6] Of String[12] = (
    'LOWEST',
    'BELOW NORMAL',
    'NORMAL',
    'ABOVE NORMAL',
    'HIGHEST',
    'BELOWNORMAL',
    'ABOVENORMAL');
Var
  Inx: Integer;
Begin
  For Inx := low(PriorityValues) To high(PriorityValues) Do
    If (PriorityStr = PriorityValues[Inx]) Then
      Begin
        Result := Inx - 2;
        If Result = 3 Then
          Result := -1
        Else If Result = 4 Then
          Result := 1;
        Exit;
      End;
  Result := 0;
End;
{--------}

Function TFSServer.CalcKeyIndex(Const KeyStr: TffShStr): Integer;
Const
  KeyValues: Array[0..21] Of String[13] = (
    'SERVERNAME',
    'MAXRAM',
    'USESINGLEUSER',
    'USEIPXSPX',
    'USETCPIP',
    'USELOGIN',
    'AUTOUPSERVER',
    'AUTOMINIMIZE',
    'IPXSPXLFB',
    'TCPIPLFB',
    'ALLOWENCRYPT',
    'READONLY',
    'LASTMSGINTVAL',
    'ALIVEINTERVAL',
    'ALIVERETRIES',
    'PRIORITY',
    'DELETESCRIPT',
    'TCPINTERFACE',
    'NOAUTOSAVECFG',
    'TEMPSTORESIZE',
    'COLLECTENABLD',
    'COLLECTFREQ');
Var
  Inx: Integer;
Begin
  For Inx := low(KeyValues) To high(KeyValues) Do
    If (KeyStr = KeyValues[Inx]) Then
      Begin
        Result := Inx;
        Exit;
      End;
  Result := -1;
End;
{--------}

Procedure TFSServer.GetServerNames(aList: TStrings;
  aTimeout: Longint);
Begin
  aList.Clear;
  aList.Add('Direct');
End;
{--------}

Function TFSServer.seDatabaseGetAliasPathPrim
  (aAlias: TffName; Var aPath: TffPath): TffResult;
Var
  aList: TList;
  Count: Integer;
  AliasDes: PffAliasDescriptor;
Begin
  { Assumption: Thread-safeness enforced at a higher level. }

  { Retrieve the alias list, and return the path for the matching entry }
  aPath := '';
  aList := TList.Create;
  Try
    Result := seDatabaseAliasListPrim(aList);
    If Result = DBIERR_NONE Then
      For Count := 0 To Pred(aList.Count) Do
        Begin
          AliasDes := PffAliasDescriptor(aList.Items[Count]);
          If FFAnsiCompareText(AliasDes^.adAlias, aAlias) = 0 Then
            Begin {!!.03, !!.10}
              aPath := AliasDes^.adPath;
              Break;
            End;
        End;
  Finally
    aList.Free;
  End;
End;
{--------}

Function TFSServer.DatabaseGetAliasPath(aAlias: TffName;
  Var aPath: TffPath;
  aClientID: TFFClientID)
  : TffResult;
Var
  Client: TfsSrcClient;
Begin
  Try
    Result := CheckClientIDandGet(aClientID, Client);
    If Result = DBIERR_NONE Then
      Begin
        FFSetRetry(Client.Timeout);
        Try
          seConfig.AliasList.BeginRead;
          Try
            Result := Client.NotifyExtenders(ffeaBeforeDBRead, ffeaNoAction);
            If Result = DBIERR_NONE Then
              Result := seDatabaseGetAliasPathPrim(aAlias, aPath);
          Finally
            seConfig.AliasList.EndRead;
          End;
        Finally
          Client.Deactivate;
        End;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.DatabaseGetFreeSpace(Const aDatabaseID: TffDatabaseID;
  Var aFreeSpace: Int64)
  : TffResult;
Var
  DB: TfsSrcDatabase;
Begin
  Try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    If (Result = DBIERR_NONE) Then
      Begin
        Try
          aFreeSpace := FFGetDiskFreeSpace(DB.dbFolder.Path);
        Finally
          DB.Deactivate;
        End;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End;
End;
{--------}

Function TFSServer.DatabaseModifyAlias(Const aClientID: TffClientID;
  Const aAlias: TffName;
  Const aNewName: TffName;
  Const aNewPath: TffPath;
  aCheckSpace: Boolean) {!!.11}
: TffResult;
Var
  Client: TfsSrcClient;
  Name: TffName;
  Path: TffPath;
Begin
  Try
    Result := CheckClientIDandGet(aClientID, Client);
    If Result = DBIERR_NONE Then
      Begin
        FFSetRetry(Client.Timeout);
        Try
          seConfig.AliasList.BeginWrite;
          Try
            Result := Client.NotifyExtenders(ffeaBeforeDBUpdate,
              ffeaDBUpdateFail);
            If Result = DBIERR_NONE Then
              Begin
                Result := Client.NotifyExtenders(ffeaBeforeDBDelete,
                  ffeaDBDeleteFail);
                If Result = DBIERR_NONE Then
                  Begin
                    Name := aAlias;
                    Result := seDatabaseGetAliasPathPrim(aAlias, Path);
                    If Result = DBIERR_NONE Then
                      Begin

                        { Does the alias have a new name? }
                        If aNewName <> '' Then
                          Name := aNewName;

                        { Does the alias have a new path? }
                        If aNewPath <> '' Then
                          Path := aNewPath;

                        Result := seDatabaseDeleteAliasPrim(aAlias);

                        If (Result = DBIERR_NONE) Then
                          Begin
                            Result := Client.NotifyExtenders(ffeaBeforeDBInsert,
                              ffeaDBInsertFail);
                            If Result = DBIERR_NONE Then
                              Begin
                                Result := seDatabaseAddAliasPrim(Name,
                                  Path,
                                  aCheckSpace); {!!.11}
                                If Result <> DBIERR_NONE Then
                                  Client.NotifyExtenders(ffeaDBInsertFail,
                                    ffeaNoAction);
                              End;
                          End
                        Else
                          Client.NotifyExtenders(ffeaDBDeleteFail, ffeaNoAction);
                      End
                    Else { if got existing alias path }
                      Client.NotifyExtenders(ffeaDBDeleteFail, ffeaNoAction);
                  End; { if no clients complained about rights }
              End;
          Finally
            seConfig.AliasList.EndWrite;
            If Result = DBIERR_NONE Then
              Result := SaveConfiguration;
          End;
        Finally
          Client.Deactivate;
        End;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
      End;
  End; {try..except}
End;
{--------}

Function TFSServer.GetServerDateTime(Var aDateTime: TDateTime): TffResult;
Begin
  Result := DBIERR_NONE;
  aDateTime := Now;
End;
{--------}{begin !!.10}

Function TFSServer.GetServerSystemTime(Var aSystemTime: TSystemTime)
  : TffResult;
Begin
  Result := DBIERR_NONE;
  GetSystemTime(aSystemTime);
End;
{--------}

Function TFSServer.GetServerGUID(Var aGUID: TGUID): TffResult;
Begin
  Result := DBIERR_NONE;
  CoCreateGuid(aGuid);
End;
{--------}

Function TFSServer.GetServerID(Var aUniqueID: TGUID): TffResult;
Begin
  Result := DBIERR_NONE;
  aUniqueID := seUniqueID;
End;
{--------}

Function TFSServer.GetServerStatistics(Var aStats: TfsServerStatistics)
  : TffResult;
Begin
  aStats.ssName := Configuration.ServerName;
  aStats.ssVersion := fsVersionNumber;
  aStats.ssState := FSMapStateToString(State);
  aStats.ssClientCount := ClientList.ClientCount;
  aStats.ssSessionCount := SessionList.SessionCount;
  aStats.ssOpenDatabasesCount := DatabaseList.DatabaseCount;
  aStats.ssOpenTablesCount := TableList.TableCount;
  aStats.ssOpenCursorsCount := CursorList.CursorCount;
  aStats.ssRamUsed := BufferManager.RAMUsed;
  aStats.ssMaxRam := BufferManager.MaxRAM;
  aStats.ssUpTimeSecs := (GetTickCount - seStartTime) Div 1000;
  aStats.ssCmdHandlerCount := CmdHandlerCount;
  Result := DBIERR_NONE;
End;
{--------}

Function TFSServer.GetCommandHandlerStatistics(Const aCmdHandlerIdx: Integer;
  Var aStats: TfsCommandHandlerStatistics)
  : TffResult;
Begin
  If (aCmdHandlerIdx < 0) Or
    (aCmdHandlerIdx > Pred(CmdHandlerCount)) Then
    Result := DBIERR_OBJNOTFOUND
  Else
    Begin
      aStats.csTransportCount := CmdHandler[aCmdHandlerIdx].TransportCount;
      Result := DBIERR_NONE;
    End;
End;
{--------}

Function TFSServer.GetTransportStatistics(Const aCmdHandlerIdx: Integer;
  Const aTransportIdx: Integer;
  Var aStats: TfsTransportStatistics)
  : TffResult;
Var
  Trans: TFSBaseTransport;
Begin
  If (aCmdHandlerIdx < 0) Or
    (aCmdHandlerIdx > Pred(CmdHandlerCount)) Then
    Result := DBIERR_OBJNOTFOUND
  Else
    Begin
      If (aTransportIdx < 0) Or
        (aTransportIdx > Pred(CmdHandler[aCmdHandlerIdx].TransportCount)) Then
        Result := DBIERR_OBJNOTFOUND
      Else
        Begin
          Trans := CmdHandler[aCmdHandlerIdx].Transports[aTransportIdx];
          aStats.tsName := Trans.GetName;
          aStats.tsState := FSMapStateToString(Trans.State);
          aStats.tsAddress := Trans.ServerName;
          aStats.tsClientCount := Trans.ConnectionCount;
          aStats.tsMessageCount := Trans.MsgCount;
          aStats.tsMessagesPerSec := Trans.MsgCount / ((GetTickCount - seStartTime) Div 1000);
          Result := DBIERR_NONE;
        End;
    End;
End;
{--------}{end !!.10}

Function TFSServer.ValBoolean(Const BoolStr: TffShStr;
  Var BoolValue: boolean): boolean;
Var
  UpperBoolStr: TffShStr;
Begin
  {only values allowed are 0, 1, YES, NO, TRUE, FALSE}
  UpperBoolStr := FFShStrUpper(BoolStr);
  Result := True;
  BoolValue := False;
  If (UpperBoolStr = '0') Or
    (UpperBoolStr = 'NO') Or
    (UpperBoolStr = 'FALSE') Then
    Exit;
  BoolValue := True;
  If (UpperBoolStr = '1') Or
    (UpperBoolStr = 'YES') Or
    (UpperBoolStr = 'TRUE') Then
    Exit;
  Result := False;
End;
{--------}

Procedure TFSServer.ProcessScriptCommand(Const KeyStr,
  ValueStr: TffShStr;
  Var DeleteScript: Boolean);
Var
  KeyInx: Integer;
  WorkInt: Longint;
  ec: Integer;
  WorkBool: Boolean;
  UpperStr: TffShStr;
Begin
  DeleteScript := False;
  {uppercase the key}
  UpperStr := FFShStrUpper(KeyStr);
  {is it one of the strings we allow?}
  KeyInx := CalcKeyIndex(UpperStr);
  {if it is, process the command}
  If (KeyInx >= 0) Then
    Begin
      Case KeyInx Of
        0: {server name}
          Begin
            Configuration.GeneralInfo^.giServerName := ValueStr;
          End;
        1: {Max RAM}
          Begin
            Val(ValueStr, WorkInt, ec);
            If (ec = 0) And (WorkInt >= 1) Then
              Configuration.GeneralInfo^.giMaxRAM := WorkInt;
          End;
        2: {Use Single User Protocol}
          Begin {!!.01 - Start}
            If ValBoolean(ValueStr, WorkBool) Then
              Configuration.GeneralInfo^.giSingleUser := WorkBool;
          End; {!!.01 - End}
        3: {Use IPX/SPX Protocol}
          Begin {!!.01 - Start}
            If ValBoolean(ValueStr, WorkBool) Then
              Configuration.GeneralInfo^.giIPXSPX := WorkBool;
          End; {!!.01 - End}
        4: {Use TCP/IP Protocol}
          Begin {!!.01 - Start}
            If ValBoolean(ValueStr, WorkBool) Then
              Configuration.GeneralInfo^.giTCPIP := WorkBool;
          End; {!!.01 - End}
        5: {Login security?}
          Begin
            If ValBoolean(ValueStr, WorkBool) Then
              Configuration.GeneralInfo^.giIsSecure := WorkBool;
          End;
        6: {Auto Up?}
          Begin
            If ValBoolean(ValueStr, WorkBool) Then
              Configuration.GeneralInfo^.giAutoUp := WorkBool;
          End;
        7: {Auto Minimize?}
          Begin
            If ValBoolean(ValueStr, WorkBool) Then
              Configuration.GeneralInfo^.giAutoMini := WorkBool;
          End;
        8: {Enable IPX/SPX use broadcasts?}
          Begin
            If ValBoolean(ValueStr, WorkBool) Then
              Configuration.GeneralInfo^.giIPXSPXLFB := WorkBool;
          End;
        9: {Enable TCP/IP use broadcasts?}
          Begin
            If ValBoolean(ValueStr, WorkBool) Then
              Configuration.GeneralInfo^.giTCPIPLFB := WorkBool;
          End;

        10:
          Begin
            If ValBoolean(ValueStr, WorkBool) Then
              Configuration.GeneralInfo^.giEncrypt := WorkBool;
          End;
        11: {ReadOnly?}
          Begin
            If ValBoolean(ValueStr, WorkBool) Then
              Begin
                Configuration.GeneralInfo^.giReadOnly := WorkBool;
                seSetLoggingState;
              End;
          End;
        12: {Last message interval}
          Begin
            Val(ValueStr, WorkInt, ec);
            If (ec = 0) And (WorkInt >= 1000) And (WorkInt <= 86400000) Then
              Configuration.GeneralInfo^.giLastMsgInterval := WorkInt;
          End;
        13: {keep alive interval}
          Begin
            Val(ValueStr, WorkInt, ec);
            If (ec = 0) And (WorkInt >= 1000) And (WorkInt <= 86400000) Then
              Configuration.GeneralInfo^.giKAInterval := WorkInt;
          End;
        14: {keep alive retries}
          Begin
            Val(ValueStr, WorkInt, ec);
            If (ec = 0) And (WorkInt >= 1) And (WorkInt <= 100) Then
              Configuration.GeneralInfo^.giKARetries := WorkInt;
          End;
        15: {Priority}
          Begin
            UpperStr := FFShStrUpper(ValueStr);
            Configuration.GeneralInfo^.giPriority :=
              CalcPriorityIndex(UpperStr);
          End;
        16: {Delete script}
          Begin {!!.01 - Start}
            If ValBoolean(ValueStr, WorkBool) Then
              DeleteScript := WorkBool;
          End; {!!.01 - End}
        17: {TCP/IP Interface}
          Begin
            Val(ValueStr, WorkInt, ec);
            Configuration.GeneralInfo^.giTCPInterface := WorkInt;
          End;
        18: {NoAutoSaveCfg}
          Begin
            If ValBoolean(ValueStr, WorkBool) Then
              Configuration.GeneralInfo^.giNoAutoSaveCfg := WorkBool;
          End;
        19: {giTempStoreSize}
          Begin
            Val(ValueStr, WorkInt, ec);
            {Temp storage must be between 1 meg and 2 gigs.}
            If (ec = 0) And (WorkInt > 0) And (WorkInt < 2049) Then
              Configuration.GeneralInfo^.giTempStoreSize := WorkInt;
          End;
        20: {giCollectEnabled}
          Begin
            If ValBoolean(ValueStr, WorkBool) Then
              Configuration.GeneralInfo^.giCollectEnabled := WorkBool;
          End;
        21: {giCollectFreq}
          Begin
            Val(ValueStr, WorkInt, ec);
            {Garbage collection frequency should be between 30 seconds
             and 60 minutes.}
            If (ec = 0) And (WorkInt > 30000) And (WorkInt < 3600000) Then
              Configuration.GeneralInfo^.giCollectFreq := WorkInt;
          End;
      End; {case}
    End
      {if it isn't it must be an alias definition}
  Else
    Begin
      If FFDirectoryExists(ValueStr) Then
        { Assumption: This routine happens on sever startup therefore we
          do not need to ensure thread-safeness. }
        seDatabaseAddAliasPrim(KeyStr, ValueStr, False); {!!.11}
    End;
End;
{--------}

Procedure TFSServer.ProcessAliasScript;
Var
  CurPath: TffPath;
  ScriptFile: TffFullFileName;
  ScriptItems: TStrings;
  Alias: TffName;
  Path: TffPath;
  i, iPos, iLen: Integer;
  DeleteScript: Boolean;
Begin
  { Get the application's directory. }
  CurPath := FFExtractPath(FFGetExeName);
  { Create the script filename. }
  ScriptFile := FFMakeFullFileName(CurPath, fsc_AliasScript);

  { Does the alias script file (FFAlias.sc$) exist in the directory? }
  If FFFileExists(ScriptFile) Then
    Begin
      { Yes. Process it. }
      ScriptItems := TStringList.Create;
      Try
        ScriptItems.LoadFromFile(ScriptFile);
        { For each item in the file, try to parse it. }
        For i := 0 To pred(ScriptItems.Count) Do
          Begin
            { Only process lines with some length. }
            iLen := Length(ScriptItems[i]);
            If iLen > 2 Then
              Begin
                { Find the semicolon. }
                iPos := Pos(';', ScriptItems[i]);
                { Only process lines with length before and after the semicolon. }
                If (iPos > 1) And (iPos < iLen) Then
                  Begin
                    { Get the alias. }
                    Alias := Copy(ScriptItems[i], 1, pred(iPos));
                    { Get the path. }
                    Path := Copy(ScriptItems[i], succ(iPos), iLen - iPos);
                    { Add the alias. }
                    ProcessScriptCommand(Alias, Path, DeleteScript);
                  End;
              End;
          End;
      Finally
        ScriptItems.Free;
      End;
    End;
End;
{--------}

Procedure TFSServer.ProcessFullScript(Const ScriptFileName: TffFullFileName);
Var
  AfterStr: TffShStr;
  AppliesToSelf: Boolean;
  { If True then script command applies to this server.  Becomes True when
    encounters a section header bearing the same server name.  Becomes False
    when encounters a section header bearing a different server name. }
  DeleteScript: Boolean;
  Inx: Integer;
  Len: Integer;
  Line: TffShStr;
  PosEquals: Integer;
  ScriptItems: TStrings;
  UServerName: TffShStr;
Begin
  AppliesToSelf := True;
  { Default to True since the script may contain leading items that apply
    to all server engines. }
  DeleteScript := False;
  UServerName := Uppercase(Self.Name);
  { Does the script file exist? }
  If FFFileExists(ScriptFileName) Then
    Begin
      { Yes. Process it. }
      ScriptItems := TStringList.Create;
      Try
        ScriptItems.LoadFromFile(ScriptFileName);
        { For each item in the file, try to parse it. }
        For Inx := 0 To pred(ScriptItems.Count) Do
          Begin
            { Only process lines with some length. }
            Line := Trim(ScriptItems[Inx]);
            Len := length(Line);
            If (Len > 2) Then
              Begin
                { Is this a section header? }
                If (Pos('[', Line) = 1) And
                  (Pos(']', Line) = Len) Then
                  Begin
                    { Yes.  Does the section apply to us? }
                    AppliesToSelf := (UpperCase(Copy(Line, 2, Len - 2)) = UServerName);
                  End
                Else If AppliesToSelf Then
                  Begin
                    { Yes. Find the equals sign. }
                    PosEquals := Pos('=', Line);
                    { Only process lines with length before and after the = char. }
                    If (PosEquals > 1) And (PosEquals < Len) Then
                      Begin
                        { Get the before and after strings. }
                        AfterStr := Copy(Line, succ(PosEquals), Len - PosEquals);
                        SetLength(Line, pred(PosEquals));
                        { Process the script command. }
                        ProcessScriptCommand(Line, AfterStr, DeleteScript);
                        If (DeleteScript) Then
                          DeleteFile(ScriptFileName);
                      End;
                  End; { if AppliesToSelf }
              End;
          End;
      Finally
        ScriptItems.Free;
      End;
    End;
End;
{--------}

Procedure XorBuffer(Var pBuf: Pointer; Count: Integer);
Var
  I: Integer;
  p: pBYTE;
  CodeKey: String;
Begin
  CodeKey := 'IRuHsDHs5TuhDmpSZxqlHdgfcwrdG';
  p := pBuf;
  If (CodeKey <> '') Then
    For I := 0 To Count - 1 Do
      Begin
        p^ := Byte(CodeKey[1 + ((I - 1) Mod Length(CodeKey))]) Xor p^;
        inc(p);
      End;
End;

Function fsReadString(Stream: TStream): AnsiString;
Var
  s: AnsiString;
  n: Longint;
Begin
  Result := '';
  If Stream.Position < Stream.Size Then
    Begin
      Stream.Read(n, 4);
      If n > 0 Then
        Begin
          SetLength(s, n);
          Stream.Read(s[1], n);
          Result := s;
        End;
    End;
End;

Procedure fsWriteString(Stream: TStream; s: AnsiString);
Var
  n: Integer;
Begin
  n := Length(s);
  Stream.Write(n, 4);
  If n > 0 Then
    Stream.Write(s[1], n);
End;

Function RWStream(Stream: TStream): Integer;
Var
  pBuf: Pointer;
Begin
  Result := Stream.Size;
  Try
    Try
      GetMem(pBuf, Result);
      Stream.Position := 0;
      Stream.Read(pBuf^, Result);
      XorBuffer(pBuf, Result);
      Stream.Position := 0;
      Stream.Write(pBuf^, Result);
      Stream.Position := 0;
      Result := Stream.Size;
    Except
      Result := -1;
    End;
  Finally
    FreeMem(pBuf, Result);
  End;
End;

Procedure TFSServer.LoadConfiguration(aCfg: String = '');
Var
  BufHash: TffWord32;
  aRights: TffUserRights;
  aSecurityEnabled: boolean;
  aClientID: TffClientID;
  Client: TfsSrcClient;
  aRemainingTime: Longint;
  Result: TffResult;
  aFile: TfileStream;
  aTmpStream: TMemoryStream;
  aConfigFileHeader: TfsConfigFileHeader;
  aRenF: String;
  tcfg: String;
Begin
  If (Not (csLoading In ComponentState)) And
    (Not (csDestroying In ComponentState)) Then
    Try
      aTmpStream := Nil;
      aFile := Nil;

      //load config
      aRemainingTime := FFGetRemainingTime;
      aTmpStream := TMemoryStream.Create;
      aConfigFileHeader.Size := 0;
      { Mark config as loaded.  We must do this in order to avoid recursive
        calls by CreateAdminUser. }
      seConfigLoaded := True;
      Try
        tcfg := Trim(aCfg);
        If (tcfg <> '') Then
          Begin
            If Not fileexists(tcfg) Then
              tcfg := Self.ConfigFile;
          End
        Else
          tcfg := Self.ConfigFile;

        If fileexists(tcfg) Then
          Begin
            aFile := TFileStream.Create(tcfg, fmShareDenyNone Or fmOpenReadWrite);
            Try
              If aFile <> Nil Then
                Begin
                  fillchar(aConfigFileHeader, Sizeof(aConfigFileHeader), 0);
                  aFile.Read(aConfigFileHeader, SizeOf(aConfigFileHeader));

                  If (aConfigFileHeader.Sign = 'FSSQL1.xxx')
                    And (aConfigFileHeader.Size > 0) Then
                    Begin
                      aTmpStream.CopyFrom(aFile, aConfigFileHeader.Size);
                      aTmpStream.Position := 0;
                      If aConfigFileHeader.Encrypt Then
                        RWStream(aTmpStream);
                      aTmpStream.Position := 0;
                    End
                  Else
                    aConfigFileHeader.Size := 0;
                End;
            Finally
              If aFile <> Nil Then
                Begin
                  aFile.free;
                  aFile := Nil;
                End;
            End;
          End;
        { Read the general info. }
        If aConfigFileHeader.Size > 0 Then
          LoadGeneralInfo(aTmpStream, aConfigFileHeader.Version);
        seBufMgr.TempPath := Self.TempPath;
        { Update the buffer manager's Max RAM. }
        seBufMgr.MaxRAM := Configuration.GeneralInfo^.giMaxRAM;
        { Do we need to update the temporary storage size? }
        seBufMgr.TempStoreSize := Configuration.GeneralInfo^.giTempStoreSize;
        seBufMgr.EncryptTempStorage := Configuration.GeneralInfo^.giEncryptTempStorage;
        { Read the aliases. }
        If aConfigFileHeader.Size > 0 Then
          LoadAliasData(aTmpStream, aConfigFileHeader.Version);

        { Read the users. }
        If aConfigFileHeader.Size > 0 Then
          LoadUserData(aTmpStream, aConfigFileHeader.Version);

        If (seConfig.UserList.Count = 0) Then
          CreateAdminUser;

        { Read the keyprocs. }
        //If aConfigFileHeader.Size > 0 Then
        //ReadUserIndex(aTmpStream, aConfigFileHeader.Version);

        { Process alias script and full script (if present). }
        ProcessAliasScript;
        If seScriptFile <> '' Then
          ProcessFullScript(seScriptFile);

        FFSetRetry(aRemainingTime);

      Except
        seConfigLoaded := False;
        If Assigned(aTmpStream) Then
          Begin
            aTmpStream.free;
            aTmpStream := Nil;
          End;
        Raise;
      End;
    Finally
      If Assigned(aTmpStream) Then
        Begin
          aTmpStream.free;
          aTmpStream := Nil;
        End;
    End;
End;
{--------}

Function TFSServer.SaveConfiguration(aCfg: String = ''): TffResult;
Var
  BufHash: TffWord32;
  aRights: TffUserRights;
  aSecurityEnabled: boolean;
  aClientID: TffClientID;
  Client: TfsSrcClient;
  aRemainingTime: Longint;
  aFile: TfileStream;
  aTmpStream: TMemoryStream;
  aConfigFileHeader: TfsConfigFileHeader;
  aRenF, tcfg: String;
Begin
  Result := 0;
  With Configuration.GeneralInfo^ Do
    If giReadOnly Or giNoAutoSaveCfg Then
      Exit;

  aRemainingTime := FFGetRemainingTime;
  aTmpStream := TMemoryStream.Create;
  tcfg := Trim(aCfg);
  If (tcfg = '') Then
    tcfg := Self.ConfigFile;
  Try
    aTmpStream.Position := 0;

    Result := SaveGeneralInfo(aTmpStream);
    Try
      If Result <> 0 Then
        Raise Exception.Create('Error write general information');
      Result := SaveAliasData(aTmpStream);
      If Result <> 0 Then
        Raise Exception.Create('Error write Databases list');
      Result := SaveUserData(aTmpStream);
      If Result <> 0 Then
        Raise Exception.Create('Error write User');
      // create new file config if ok save
      If fileexists(Self.ConfigFile) Then
        Begin
          aRenF := '@#' + Self.ConfigFile;
          RenameFile(Self.ConfigFile, aRenF);
        End;

      aFile := TFileStream.Create(tcfg, fmCreate Or fmOpenReadWrite);

      aConfigFileHeader.Sign := 'FSSQL1.xxx';
      aConfigFileHeader.Version := fsVersionNumber;
      aConfigFileHeader.Encrypt := Configuration.GeneralInfo^.giEncrypt;
      aConfigFileHeader.DateCreate := Now;
      aConfigFileHeader.Size := 0;
      aFile.Size:= 0;
      aFile.Write(aConfigFileHeader, SizeOf(aConfigFileHeader));

      // Simple Encrypt
      Result := 0;
      If aConfigFileHeader.Encrypt Then
        RWStream(aTmpStream);

      aTmpStream.Position := 0;
      aFile.CopyFrom(aTmpStream, aTmpStream.Size);
      // Saveheader
      aConfigFileHeader.DateCreate := Now;
      aConfigFileHeader.Size := aTmpStream.Size;
      aFile.Position := 0;
      aFile.Write(aConfigFileHeader, SizeOf(aConfigFileHeader));

      // save ok
      DeleteFile(aRenF);
      FFSetRetry(aRemainingTime);

      seBufMgr.TempPath := Self.TempPath;
      { Update the buffer manager's Max RAM. }
      seBufMgr.MaxRAM := Configuration.GeneralInfo^.giMaxRAM;
      { Do we need to update the temporary storage size? }
      seBufMgr.TempStoreSize := Configuration.GeneralInfo^.giTempStoreSize;
      seBufMgr.EncryptTempStorage := Configuration.GeneralInfo^.giEncryptTempStorage;
    Except
      On E: Exception Do
        Begin
          Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
        End;
    End;
  Finally
    aTmpStream.free;
    aFile.Free;
  End;
End;

Procedure TFSServer.LoadAliasData(aStream: TStream = Nil; aVersion: Integer = 0);
Var
  Alias, S: String;
  Path, PathBlob: String;
  CheckDisk, Flags: Byte;
  i, j: Integer;
  aRemainingTime: Longint;
Begin
  If aStream = Nil Then Exit;
  aRemainingTime := FFGetRemainingTime;
  S := fsReadString(aStream);
  aStream.Read(j, 4);
  If S = '@ALIAS@' Then
    Begin
      FFSetRetry(aRemainingTime);
      Configuration.AliasList.BeginWrite;
      Configuration.AliasList.Empty;
      Try
        If j > 0 Then
          For i := 0 To Pred(j) Do
            Begin
              Alias := fsReadString(aStream);
              Path := fsReadString(aStream);
              PathBlob := fsReadString(aStream);
              aStream.Read(CheckDisk, 1);
              aStream.Read(Flags, 1);
              Configuration.AddAlias(Alias, Path, boolean(CheckDisk));
              FFSetRetry(aRemainingTime);
            End;
      Finally
        Configuration.AliasList.EndWrite;
      End;
    End;
End;
{--------}

Function TFSServer.SaveAliasData(aStream: TStream = Nil): TffResult;
Var
  i: Integer;
  AliasItem: TfsAliasItem;
  Buffer: String;
  CheckDisk, Flags: Byte;
Begin
  Result := DBIERR_NONE;
  With Configuration.GeneralInfo^ Do
    If giReadOnly Or giNoAutoSaveCfg Then
      Exit;
  Try
    Configuration.AliasList.BeginRead;
    Try
      i := Configuration.AliasList.Count;
      Buffer := '@ALIAS@';
      fsWriteString(aStream, Buffer);
      aStream.Write(i, 4);
      For i := 0 To pred(Configuration.AliasList.Count) Do
        Begin
          AliasItem := Configuration.AliasList[i];
          Buffer := AliasItem.Alias;
          fsWriteString(aStream, Buffer);
          Buffer := AliasItem.Path;
          fsWriteString(aStream, Buffer);
          fsWriteString(aStream, Buffer);
          CheckDisk := Byte(AliasItem.CheckSpace);
          aStream.Write(CheckDisk, 1);
          Flags := 0;
          aStream.Write(Flags, 1);
        End;
    Finally
      Configuration.AliasList.EndRead;
    End;
  Except
    {If an exception occurs, get the error code and fall through to the
     cleanup code below.  The error code will be returned to the calling
     object. }
    On E: Exception Do
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
  End;
End;
{=====================================================================}

{== Read/Write User data from table ==================================}

Procedure TFSServer.LoadUserData(aStream: TStream = Nil; aVersion: Integer = 0);
Var
  BufHash: TffWord32;
  BufRights: TffUserRights;
  BufUserID, BufLast, BufFirst: TffShStr;
  Hash: TffWord32;
  i, j: Integer;
  UserItem: TfsUserItem;
  aRights: TffUserRights;
  aSecurityEnabled: boolean;
  aRemainingTime: Longint;
Begin
  If aStream = Nil Then Exit;
  Configuration.UserList.Empty;
  aRemainingTime := FFGetRemainingTime;
  BufUserID := fsReadString(aStream);
  aStream.Read(j, 4);
  If BufUserID = '@USERS@' Then
    Begin
      FFSetRetry(aRemainingTime);
      If j > 0 Then
        For i := 0 To pred(j) Do
          Begin
            BufUserID := fsReadString(aStream);
            BufLast := fsReadString(aStream);
            BufFirst := fsReadString(aStream);
            aStream.read(BufHash, 4);
            aStream.Read(BufRights, Sizeof(TffUserRights));
            Configuration.AddUser(BufUserID, BufLast, BufFirst, BufHash, BufRights);
            FFSetRetry(aRemainingTime);
          End;
    End;
End;
{--------}

Function TFSServer.SaveUserData(aStream: TStream = Nil): TffResult;
Var
  BufHash: TffWord32;
  BufRights: TffUserRights;
  BufStr: TffShStr;
  Hash: TffWord32;
  i: Integer;
  UserItem: TfsUserItem;
  aRights: TffUserRights;
  aSecurityEnabled: boolean;
Begin
  Result := DBIERR_NONE;
  If aStream = Nil Then Exit;
  With Configuration.GeneralInfo^ Do
    If giReadOnly Or giNoAutoSaveCfg Then
      Exit;

  Try
    {Insert new records.}
    i := Configuration.UserList.Count;
    BufStr := '@USERS@';
    fsWriteString(aStream, BufStr);
    aStream.Write(i, 4);
    For i := 0 To pred(Configuration.UserList.Count) Do
      Begin
        UserItem := Configuration.UserList[i];
        BufStr := UserItem.UserID;
        fsWriteString(aStream, BufStr);
        BufStr := UserItem.LastName;
        fsWriteString(aStream, BufStr);
        BufStr := UserItem.FirstName;
        fsWriteString(aStream, BufStr);
        BufHash := UserItem.PasswordHash;
        aStream.Write(BufHash, 4);
        BufRights := UserItem.Rights;
        aStream.Write(BufRights, Sizeof(TffUserRights));
      End;

  Except
    {If an exception occurs, get the error code and fall through to the
     cleanup code below.  The error code will be returned to the calling
     object. }
    On E: Exception Do
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
  End;
End;
{=====================================================================}

{== Read/write general info from tables ==============================}
Const
  fsc_GeneralClientID = -1;
  {--------}

Procedure TFSServer.LoadGeneralInfo(aStream: TStream = Nil; aVersion: Integer = 0);
Var
  BufStr: String;
Begin
  If aStream = Nil Then Exit;
  Try
    BufStr := fsReadString(aStream);
    If BufStr = '@GENERAL@' Then
      Begin
        With Configuration.GeneralInfo^ Do
          Begin
            giServerName := fsReadString(aStream);
            BufStr := fsReadString(aStream);
            Self.TempPath := BufStr;
            aStream.Read(giMaxRAM, 4);
            aStream.Read(giIsSecure, 2);
            aStream.Read(giAutoUp, 2);
            aStream.Read(giAutoMini, 2);
            aStream.Read(giDebugLog, 2);
            aStream.Read(giSingleUser, 2);
            aStream.Read(giIPXSPX, 4);
            aStream.Read(giIPXSPXLFB, 4);
            aStream.Read(giTCPIP, 4);
            aStream.Read(giTCPIPLFB, 4);
            aStream.Read(giTCPPort, 4);
            aStream.Read(giUDPPortSr, 4);
            aStream.Read(giUDPPortCl, 4);
            aStream.Read(giIPXSocketSr, 4);
            aStream.Read(giIPXSocketCl, 4);
            aStream.Read(giSPXSocket, 4);
            aStream.Read(giEncrypt, 2);
            aStream.Read(giReadOnly, 2);
            aStream.Read(giLastMsgInterval, 4);
            aStream.Read(giKAInterval, 4);
            aStream.Read(giKARetries, 4);
            aStream.Read(giPriority, 2);
            aStream.Read(giTCPInterface, 4);
            aStream.Read(giNoAutoSaveCfg, 2);
            aStream.Read(giTempStoreSize, 4);
            aStream.Read(giCollectEnabled, 2);
            aStream.Read(giCollectFreq, 4);
            aStream.Read(giEnabledTrigers, 2);
            aStream.Read(giEnabledProcedures, 2);
            aStream.Read(giEnabledReferential, 2);
            aStream.Read(giMaxClients, 4);
            aStream.Read(giMaxDuplicateUsers, 4);
            aStream.Read(giSendErrorIfLoginDuplicateUser, 2);
            aStream.Read(giIsSecureServer, 2);
            aStream.Read(giMaxDbOpen, 4);
            aStream.Read(giMaxSessionOpen, 4);
            aStream.Read(giEncryptTempStorage, 2);
            aStream.Read(giClearCachePerCount, 4);
            aStream.Read(giCloseInactiveTablesAfterCommitOrRoolback, 2);
            aStream.Read(giCloseInactiveTables, 2);
            aStream.Read(giClearCacheIfUpdate, 2);
            aStream.Read(giClearCache, 2);
          End; { with }
      End;

  Finally
    { Update the logging state. }
    seSetLoggingState;
  End;
End;
{--------}

Function TFSServer.SaveGeneralInfo(aStream: TStream = Nil)
  : TffResult;
Var
  aRights: TffUserRights;
  aSecurityEnabled: boolean;
  b: Boolean;
  S: String;
Begin
  Result := DBIERR_NONE;
  With Configuration.GeneralInfo^ Do
    If (giReadOnly Or giNoAutoSaveCfg) Then
      Exit;

  Try
    S := '@GENERAL@';
    fsWriteString(aStream, S);
    With Configuration.GeneralInfo^ Do
      Begin
        fsWriteString(aStream, giServerName);
        fsWriteString(aStream, Self.TempPath);
        aStream.Write(giMaxRAM, 4);
        aStream.Write(giIsSecure, 2);
        aStream.Write(giAutoUp, 2);
        aStream.Write(giAutoMini, 2);
        aStream.Write(giDebugLog, 2);
        aStream.Write(giSingleUser, 2);
        aStream.Write(giIPXSPX, 4);
        aStream.Write(giIPXSPXLFB, 4);
        aStream.Write(giTCPIP, 4);
        aStream.Write(giTCPIPLFB, 4);
        aStream.Write(giTCPPort, 4);
        aStream.Write(giUDPPortSr, 4);
        aStream.Write(giUDPPortCl, 4);
        aStream.Write(giIPXSocketSr, 4);
        aStream.Write(giIPXSocketCl, 4);
        aStream.Write(giSPXSocket, 4);
        aStream.Write(giEncrypt, 2);
        aStream.Write(giReadOnly, 2);
        aStream.Write(giLastMsgInterval, 4);
        aStream.Write(giKAInterval, 4);
        aStream.Write(giKARetries, 4);
        aStream.Write(giPriority, 2);
        aStream.Write(giTCPInterface, 4);
        aStream.Write(giNoAutoSaveCfg, 2);
        aStream.Write(giTempStoreSize, 4);
        aStream.Write(giCollectEnabled, 2);
        aStream.Write(giCollectFreq, 4);
        aStream.Write(giEnabledTrigers, 2);
        aStream.Write(giEnabledProcedures, 2);
        aStream.Write(giEnabledReferential, 2);
        aStream.Write(giMaxClients, 4);
        aStream.Write(giMaxDuplicateUsers, 4);
        aStream.Write(giSendErrorIfLoginDuplicateUser, 2);
        aStream.Write(giIsSecureServer, 2);
        aStream.Write(giMaxDbOpen, 4);
        aStream.Write(giMaxSessionOpen, 4);
        aStream.Write(giEncryptTempStorage, 2);
        aStream.Write(giClearCachePerCount, 4);
        aStream.Write(giCloseInactiveTablesAfterCommitOrRoolback, 2);
        aStream.Write(giCloseInactiveTables, 2);
        aStream.Write(giClearCacheIfUpdate, 2);
        aStream.Write(giClearCache, 2);
      End;
  Except
    {If an error occurs at any point, we raise an exception.  The
     exception handling just falls through to the cleanup code below.}
    On E: Exception Do
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
  End;
End;
{=====================================================================}

{== Read/write key proc info from/to tables ==========================}
Const
  fsc_KeyProcClientID = -1;
  {--------}

Procedure TFSServer.LoadUserIndex(aStream: TStream = Nil; aVersion: Integer = 0);
Var
  aFlag: Byte;
  aClientID: TffClientID;
  BufBuild: TffName;
  BufCompare: TffName;
  BufDLL: TffFullFileName;
  BufIndexID: Longint;
  BufPath: TffPath;
  BufTable: TfsTableName;
  Client: TfsSrcClient;
  Cursor: TfsSrBaseCursor; {!!.06}
  DB: TfsSrcDatabase;
  DBIResult: TffResult;
  Dict: TFSInfoDict;
  Folder: TfsSrcFolder;
  Hash: TffWord32;
  IsNull: boolean;
  MyRec: PffByteArray;
  SearchPath: TffPath;
  aRights: TffUserRights;
  aSecurityEnabled: boolean;
  aRefNr: tffInt64;
Begin
  aFlag := 0;
  Client := Nil;
  Folder := Nil;
  Cursor := Nil;
  DB := Nil;
  Try
    {create ourselves a client}
    DBIResult := ClientAdd(aClientID, '', fsc_AdminUserID, 1000, Hash, aRights, aSecurityEnabled);
    If (DBIResult <> DBIERR_NONE) Then
      Exit;

    {open a database (no User) to the server engine directory}
    Client := TfsSrcClient(aClientID);
    Folder := TfsSrcFolder.Create(TempPath, True, seBufMgr);
    DB := seDatabaseOpenPrim(Client.clSessionList.CurrentSession,
      Folder,
      '',
      omReadWrite,
      smExclusive,
      1000,
      False,
      tiSerializable,
      tlOptimisticNoWait); {!!.11}
    If (DBIResult = DBIERR_NONE) Then
      FFSetRetry(DB.Timeout)
    Else
      Exit;

    {read the records}
    Configuration.KeyProcList.Empty;

    { If the table exists then read it. }
    SearchPath := Folder.Path;
    If (SearchPath[length(SearchPath)] <> '\') Then
      FFShStrAddChar(SearchPath, '\');
    If FFFileExists(SearchPath + FFForceExtension(fsc_IndexTableName, fsc_ExtForData)) Then
      Begin
        Cursor := CursorClass.Create(Self, DB, 1000); {!!.06}
        FFSetRetry(Cursor.Timeout); {!!.01}
        Cursor.Open(fsc_IndexTableName, '', 0, omReadOnly, smExclusive,
          True, False, [], True);
        Cursor.CloseTable := True;
        Dict := Cursor.Dictionary;
        FFGetMem(MyRec, Dict.RecordLength);
        Try
          FFSetRetry(Cursor.Timeout);
          Cursor.SetToBegin;
          FFSetRetry(Cursor.Timeout); {!!.01}
          DBIResult := Cursor.GetNextRecord(MyRec, ffsltNone, aFlag, aRefnr);
          While (DBIResult = DBIERR_NONE) Do
            Begin
              Dict.GetRecordField(0, MyRec, IsNull, @BufPath);
              Dict.GetRecordField(1, MyRec, IsNull, @BufTable);
              Dict.GetRecordField(2, MyRec, IsNull, @BufIndexID);
              Dict.GetRecordField(3, MyRec, IsNull, @BufDLL);
              Dict.GetRecordField(4, MyRec, IsNull, @BufBuild);
              Dict.GetRecordField(5, MyRec, IsNull, @BufCompare);
              Configuration.AddKeyProc(BufPath, BufTable, BufIndexID,
                BufDLL, BufBuild, BufCompare);
              FFSetRetry(Cursor.Timeout); {!!.01}
              DBIResult := Cursor.GetNextRecord(MyRec, ffsltNone, aFlag, aRefnr);
            End;
        Finally
          FFFreeMem(MyRec, Dict.RecordLength);
        End; {try..finally}
      End;
  Finally

    { Close the cursor. }
    If assigned(Cursor) Then
      Cursor.Free;

    DB.Free;
    Folder.Free;

    { Remove the client. }
    seClientRemovePrim(Client);

  End;
End;
{--------}

Function TFSServer.SaveUserIndex(aStream: TStream = Nil): TffResult;
Label
  Cleanup,
    InnerCleanup;
Var
  aClientID: TffClientID;
  BufInt: Longint;
  BufStr: TffShStr;
  Dict: TFSInfoDict;
  Folder: TfsSrcFolder;
  Hash: TffWord32;
  i: Integer;
  KeyProcItem: TfsKeyProcItem;
  MyRec: PffByteArray;
  State: Integer;
  TransID: TffTransID;
  Client: TfsSrcClient;
  DB: TfsSrcDatabase;
  Cursor: TfsSrBaseCursor; {!!.06}
  aRights: TffUserRights;
  aSecurityEnabled: boolean;
  aRefNr: tffInt64;
Begin

  Result := DBIERR_NONE;
  With Configuration.GeneralInfo^ Do
    If giReadOnly Or giNoAutoSaveCfg Then
      Exit;

  Client := Nil;
  DB := Nil;
  Dict := Nil;
  Folder := Nil;
  Cursor := Nil;
  State := 0;
  Try

    {create ourselves a client}
    Result := ClientAdd(aClientID, '', fsc_AdminUserID, 1000, Hash, aRights, aSecurityEnabled);
    If (Result <> DBIERR_NONE) Then
      Goto Cleanup;

    State := 100; { client added }

    {open a database (no alias) to the server engine directory}
    Client := TfsSrcClient(aClientID);
    Folder := TfsSrcFolder.Create(TempPath, False, seBufMgr);
    DB := seDatabaseOpenPrim(Client.clSessionList.CurrentSession,
      Folder,
      '',
      omReadWrite,
      smExclusive,
      1000,
      False,
      tiSerializable,
      tlOptimisticNoWait); {!!.11}
    If (Result = DBIERR_NONE) Then
      FFSetRetry(DB.Timeout)
    Else
      Goto Cleanup;

    State := 200; { database opened }

    {Make sure prior instances of the saved and temporary tables are deleted. }
    seTableDeletePrim(DB, fsc_SavedIndexTableName);
    seTableDeletePrim(DB, fsc_TempIndexTableName);

    {Prepare a data dictionary.}
    Dict := TFSInfoServerDict.Create(4096);

    State := 300; { dictionary created }

    {Create the new table as a temporary file. }

    With Dict Do
      Begin
        AddField('Path', '', fstShortString, pred(sizeof(TffPath)), 0, True, Nil, blNone, '', rNone, False, duNormal);
        AddField('Table', '', fstShortString, pred(sizeof(TfsTableName)), 0, True, Nil, blNone, '', rNone, False, duNormal);
        AddField('IndexID', '', fstInt32, 0, 0, True, Nil, blNone, '', rNone, False, duNormal);
        AddField('DLL', '', fstShortString, pred(sizeof(TffFullFileName)), 0, True, Nil, blNone, '', rNone, False, duNormal);
        AddField('BuildKey', '', fstShortString, pred(sizeof(TffName)), 0, True, Nil, blNone, '', rNone, False, duNormal);
        AddField('CompareKey', '', fstShortString, pred(sizeof(TffName)), 0, True, Nil, blNone, '', rNone, False, duNormal);
        AddField('Memo', '', fstBLOBMemo, sizeof(Longint), 0, False, Nil, blNone, '', rNone, False, duNormal);
      End;
    Dict.IsEncrypted := Configuration.GeneralInfo^.giEncrypt;

    Result := seTableBuildPrim(DB, True, fsc_TempIndexTableName, True, Dict);
    If (Result <> DBIERR_NONE) Then
      Goto Cleanup;

    State := 400; { temporary table created }

    { Start a transaction before opening the table. }
    Result := seTransactionStart(DB, False, fscl_TrImplicit, TransID);
    If (Result <> DBIERR_NONE) Then
      Goto Cleanup;

    State := 500; { transaction started for opening table }

    Try
      FFGetMem(MyRec, Dict.RecordLength);

      State := 600;

      Cursor := CursorClass.Create(Self, DB, 1000); {!!.06}
      FFSetRetry(Cursor.Timeout); {!!.01}
      Cursor.Open(fsc_TempIndexTableName, '', 0, omReadWrite, smExclusive,
        True, False, [], True);
      Cursor.CloseTable := True;
      {Insert new records.}
      For i := 0 To pred(Configuration.KeyProcList.Count) Do
        Begin
          Cursor.Dictionary.InitRecord(MyRec);
          KeyProcItem := Configuration.KeyProcList[i];
          BufStr := KeyProcItem.Path;
          Cursor.Dictionary.SetRecordField(0, MyRec, @BufStr);
          BufStr := KeyProcItem.Table;
          Cursor.Dictionary.SetRecordField(1, MyRec, @BufStr);
          BufInt := KeyProcItem.IndexID;
          Cursor.Dictionary.SetRecordField(2, MyRec, @BufInt);
          BufStr := KeyProcItem.DLLName;
          Cursor.Dictionary.SetRecordField(3, MyRec, @BufStr);
          BufStr := KeyProcItem.BuildKeyName;
          Cursor.Dictionary.SetRecordField(4, MyRec, @BufStr);
          BufStr := KeyProcItem.CompareKeyName;
          Cursor.Dictionary.SetRecordField(5, MyRec, @BufStr);
          FFSetRetry(Cursor.Timeout); {!!.01}
          Result := Cursor.InsertRecord(MyRec, ffsltExclusive, 0, arefnr);
          If (Result <> DBIERR_NONE) Then
            Goto InnerCleanup;
        End;

      State := 750;

      { Commit the transaction. }
      FFSetRetry(Cursor.Timeout); {!!.01}
      Result := seTransactionCommit(DB);
      If Result = DBIERR_NONE Then
        State := 800; { transaction committed }

      InnerCleanup:

    Finally
      { Rollback the transaction. }
      If (State >= 500) And (State < 750) Then
        seTransactionRollback(DB);

      If State >= 600 Then
        FFFreeMem(MyRec, Dict.RecordLength);

      {close the cursor}
      If assigned(Cursor) Then
        Cursor.Free;

    End; {try..finally}

    { If the record insertions did not complete then jump to cleanup. }
    If State < 800 Then
      Goto Cleanup;

    { Rename the existing table. }
    Result := seTableRenamePrim(DB, fsc_IndexTableName, fsc_SavedIndexTableName);
    If (Result <> DBIERR_NOSUCHTABLE) And (Result <> DBIERR_NONE) Then
      Goto Cleanup;

    State := 1000; { renamed system table to saved table }

    { Replace the original table with the temporary table. }
    Result := seTableRenamePrim(DB, fsc_TempIndexTableName, fsc_IndexTableName);
    If Result <> DBIERR_NONE Then
      Goto Cleanup;

    State := 1100; { renamed temp table to system table }

    { The new table is now in place.  Get rid of the saved, original
      table.  Ignore errors. }
    If Not IsTableNameOpen(DB.Folder, fsc_SavedIndexTableName) Then
      seDeleteTable(DB, fsc_SavedIndexTableName)
    Else
      Result := DBIERR_TABLEOPEN;

    { The code jumps to this point if an error is detected in a ServerEngine
      method. }
    Cleanup:
  Except
    {If an exception occurs, get the error code and fall through to the
     cleanup code below.  The error code will be returned to the calling
     object. }
    On E: Exception Do
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
  End;

  { Put System table back into its rightful place if a failure occurred
    after it was renamed to the saved table. }
  If (State >= 1000) And (State < 1100) Then
    seTableRenamePrim(DB, fsc_SavedIndexTableName, fsc_IndexTableName);

  { Delete temporary table if it did not replace system table. }
  If (State >= 400) And (State < 1100) Then
    If Not IsTableNameOpen(DB.Folder, fsc_TempIndexTableName) Then
      seDeleteTable(DB, fsc_TempIndexTableName)
    Else
      Result := DBIERR_TABLEOPEN;

  Dict.Free;
  DB.Free;
  Folder.Free;

  {remove the client}
  If State >= 100 Then
    seClientRemovePrim(Client);

End;
{--------}

Procedure TFSServer.CreateAdminUser;
Begin
  Configuration.AddUser(fsc_AdminUserID,
    'Administrator',
    '',
    fsc_AdminPasswd,
    fsc_AdminRights);
  Configuration.AddUser(fsc_QuestUserID,
    'Quest',
    '',
    fsc_QuestPasswd,
    fsc_QuestRights);
End;

// interpreter

Constructor TCursorInterpretator.Create;
Begin
  Inherited Create;
  CursorProcedure := 0;
  InternalFields := TIntVariables.Create;
  InternalCursorList := TIntVariables.Create;

  InternalConsts['rNone'] := rNone;
  InternalConsts['rMathematical'] := rMathematical;
  InternalConsts['rMatAfter1'] := rMatAfter1;
  InternalConsts['rMatAfter2'] := rMatAfter2;
  InternalConsts['rMatAfter3'] := rMatAfter3;
  InternalConsts['rMatAfter4'] := rMatAfter4;
  InternalConsts['rMatAfter5'] := rMatAfter5;
  InternalConsts['rMatAfter6'] := rMatAfter6;
  InternalConsts['rMatAfter7'] := rMatAfter7;
  InternalConsts['rMatAfter8'] := rMatAfter8;
  InternalConsts['rMatAfter9'] := rMatAfter9;

  InternalConsts['fstBoolean'] := fstBoolean;
  InternalConsts['fstSingleChar'] := fstSingleChar;
  InternalConsts['fstSingleWideChar'] := fstSingleWideChar;
  InternalConsts['fstUInt8'] := fstUInt8;
  InternalConsts['fstUInt16'] := fstUInt16;
  InternalConsts['fstUInt32'] := fstUInt32;
  InternalConsts['fstInt8'] := fstInt8;
  InternalConsts['fstInt16'] := fstInt16;
  InternalConsts['fstInt32'] := fstInt32;
  InternalConsts['fstInt64'] := fstInt64;
  InternalConsts['fstAutoInc32'] := fstAutoInc32;
  InternalConsts['fstAutoInc64'] := fstAutoInc64;
  InternalConsts['fstRecVersion'] := fstRecVersion;
  InternalConsts['fstSingle'] := fstSingle;
  InternalConsts['fstDouble'] := fstDouble;
  InternalConsts['fstExtended'] := fstExtended;
  InternalConsts['fstCurrency'] := fstCurrency;
  InternalConsts['fstDate'] := fstDate;
  InternalConsts['fstTime'] := fstTime;
  InternalConsts['fstDateTime'] := fstDateTime;
  InternalConsts['fstBLOB'] := fstBLOB;
  InternalConsts['fstBLOBMemo'] := fstBLOBMemo;
  InternalConsts['fstBLOBGraphic'] := fstBLOBGraphic;
  InternalConsts['fstArrayUInt8'] := fstArrayUInt8;
  InternalConsts['fstShortString'] := fstShortString;
  InternalConsts['fstNullString'] := fstNullString;
  InternalConsts['fstVarNullString'] := fstVarNullString;
  InternalConsts['fstWideString'] := fstWideString;
  InternalConsts['fstVarWideString'] := fstVarWideString;
  InternalConsts['fstArrayInt32'] := fstArrayInt32;
  InternalConsts['fstArrayUInt16'] := fstArrayUInt16;
  InternalConsts['fstArrayDouble'] := fstArrayDouble;
End;

Destructor TCursorInterpretator.Destroy;
Begin
  Clear;
  Inherited Destroy;
End;

Procedure TCursorInterpretator.Clear;
Var
  i: Integer;
  j: Longword;
  tq: TfsSrBaseCursor;
Begin
  Try
    Try
      For i := 0 To Self.InternalCursorList.Count - 1 Do
        Begin
          // free sql alloc
          If Not Self.InternalCursorList.Value3[i] Then
            Begin
              j := Self.InternalCursorList.Value1[i];
              tq := TfsSrBaseCursor(j);
              If tq <> Nil Then
                Begin
                  tq.CloseTable := True;
                  Engine.CursorClose(j);
                End;
            End;
          Engine.sqlfree(Self.InternalCursorList.Value2[i]);
        End;
    Except
    End;
  Finally
    InternalFields.free;
    InternalFields := Nil;
    InternalCursorList.free;
    InternalCursorList := Nil;
  End;
End;

// for parser

Procedure TCursorInterpretator.ErrorString(Const S: String);
Begin
  Raise Exception.CreateFmt('Error: %s', [s]);
End;

Function TCursorInterpretator.CreateTemporaryTableWithoutIndex(CursorName: String): TffCursorId;
Var
  Dictionary: TFSInfoDict;
  Cursor: TfsSrBaseCursor;
  fName,
    Value1,
    Value2,
    Value3,
    Value4: Variant;
Begin
  Result := 0;
  Cursor := Nil;
  Dictionary := TFSInfoDict.Create(fscl_64k);
  Try
    InternalParamPosition2 := 1;
    Try
      While InternalVariables.IndexOf('_@#$-RETURN' + IntToStr(InternalParamPosition2)) > -1 Do
        Begin
          fName := InternalVariables.Variable1['_@#$-RETURN' + IntToStr(InternalParamPosition2)];
          Value1 := InternalVariables.Variable2['_@#$-RETURN' + IntToStr(InternalParamPosition2)];
          Value2 := InternalVariables.Variable3['_@#$-RETURN' + IntToStr(InternalParamPosition2)];
          Value3 := InternalVariables.Variable4['_@#$-RETURN' + IntToStr(InternalParamPosition2)];
          Value4 := InternalVariables.Variable5['_@#$-RETURN' + IntToStr(InternalParamPosition2)];
          Dictionary.AddField(FName, '', TfsFieldType(Value1), Value2, Value3, False, Nil, blNone, '', TRound(Value4), False, duNormal);
          inc(InternalParamPosition2);
        End;
    Except
      InternalParamPosition2 := 1;
    End;

    If InternalParamPosition2 > 1 Then
      Begin
        Try
          Cursor := TfsSimpleSqlResultSet.Create(Self.SrcCursor.Engine, Self.SrcCursor.Database,
            FFGetRemainingTime);
          Cursor.Build('', Dictionary, omReadWrite, smExclusive,
            False, True, [fffaTemporary, fffaBLOBChainSafe], 0);
          Result := Cursor.CursorID;
        Except
          Result := 0;
          If cursor <> Nil Then
            Cursor.free;
        End;
        If Result > 0 Then
          Begin
            Self.InternalCursorList.Variable1[CursorName] := Result;
            Self.InternalCursorList.Variable2[CursorName] := 0;
            Self.InternalCursorList.Variable3[CursorName] := True;
          End;
      End;
    //Cursor.CloseTable := True;
  Finally
    Dictionary.Free;
  End;
End;

Function TCursorInterpretator.iExecSQL(CursorName, Sql: String): TffCursorId;
Var
  atCursorID: TffCursorID;
  Str: TMemoryStream;
  SQLResult: TffResult;
  SqlStmtID: TffSqlStmtID;
  tq: TfsSrBaseCursor;
Begin
  Str := TMemoryStream.Create;
  SqlStmtID := 0;
  SQLResult := Self.Engine.SQLAlloc(ClientID, DatabaseID, Timeout, SqlStmtID);
  Try
    If SQLResult = DBIERR_NONE Then
      Begin
        atCursorID := 0;
        SQLResult := Self.Engine.SQLPrepare(SqlStmtID, pointer(sql), Str);
        If SQLResult = DBIERR_NONE Then
          Begin
            SQLResult := Self.Engine.SQLExec(SqlStmtID, omReadOnly, atCursorID, Str);
            If SQLResult = DBIERR_NONE Then
              Begin
                Result := atCursorID;
                Self.ErrorResult := 0;
                If atCursorID = 0 Then
                  Begin
                    tq := TfsSrBaseCursor(atCursorID);
                    If tq <> Nil Then
                      Begin
                        tq.CloseTable := True;
                        Engine.CursorClose(atCursorID);
                      End;
                    Self.Engine.SQLFree(SqlStmtID);
                  End
                Else
                  Begin
                    If CursorName = '' Then
                      Begin
                        Result := 0;
                        Self.ErrorResult := fserrTransactionFailed;
                        tq := TfsSrBaseCursor(atCursorID);
                        If tq <> Nil Then
                          Begin
                            tq.CloseTable := True;
                            Engine.CursorClose(atCursorID);
                          End;
                        Self.Engine.SQLFree(SqlStmtID);
                      End
                    Else
                      Begin
                        Self.InternalCursorList.Variable1[CursorName] := Result;
                        Self.InternalCursorList.Variable2[CursorName] := SqlStmtID;
                        Self.InternalCursorList.Variable3[CursorName] := False;
                        Self.Engine.SQLFree(SqlStmtID);
                      End;
                  End;
              End
            Else
              Begin
                Result := 0;
                Self.ErrorResult := SQLResult;
                Self.Engine.SQLFree(SqlStmtID);
              End;
          End
        Else
          Begin
            Result := 0;
            Self.ErrorResult := SQLResult;
            Self.Engine.SQLFree(SqlStmtID);
          End;
      End
    Else
      Begin
        Result := 0;
        Self.ErrorResult := SQLResult;
        Self.Engine.SQLFree(SqlStmtID);
      End;
  Finally
    Str.free;
    Self.Engine.SQLFree(SqlStmtID);
  End;
End;

Function TCursorInterpretator.iExecModifySQL(Sql: String): TffResult;
Var
  atCursorID: TffCursorID;
  Str: TMemoryStream;
  SQLResult: TffResult;
  SqlStmtID: TffSqlStmtID;
Begin
  Str := TMemoryStream.Create;
  SqlStmtID := 0;
  SQLResult := Self.Engine.SQLAlloc(ClientID, DatabaseID, Timeout, SqlStmtID);
  Try
    If SQLResult = DBIERR_NONE Then
      Begin
        atCursorID := 0;
        SQLResult := Self.Engine.SQLPrepare(SqlStmtID, pointer(sql), Str);
        If SQLResult = DBIERR_NONE Then
          Begin
            SQLResult := Self.Engine.SQLExec(SqlStmtID, omReadWrite, atCursorID, Str);
            If SQLResult = DBIERR_NONE Then
              Begin
                Result := 0;
                Self.ErrorResult := 0;
                Self.Engine.SQLFree(SqlStmtID);
              End
            Else
              Begin
                Result := SQLResult;
                Self.ErrorResult := SQLResult;
                Self.Engine.SQLFree(SqlStmtID);
              End;
          End
        Else
          Begin
            Result := SQLResult;
            Self.ErrorResult := SQLResult;
            Self.Engine.SQLFree(SqlStmtID);
          End;
      End
    Else
      Begin
        Result := SQLResult;
        Self.ErrorResult := SQLResult;
        Self.Engine.SQLFree(SqlStmtID);
      End;
  Finally
    Str.free;
  End;
End;

Function TCursorInterpretator.iExecSQLClose(CursorName: String): boolean;
Var
  i: Longword;
Begin
  Result := False;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable2[CursorName];
      Result := Self.Engine.sqlfree(i) = DBIERR_NONE;
    End;
End;

Function TCursorInterpretator.iExecSQLSort(CursorName, FieldCsv, OrderCsv: String): Boolean;
Var
  i: Longword;
  cur: TfsSrBaseCursor;
  aFldArray: TffFieldList;
  aOrderByArray: TfsOrderByArray;
  IdxHlprs: TffFieldIHList;
Begin
  Result := False;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
      If cur <> Nil Then
        Begin
          aFldArray[0] := 0;
          aOrderByArray[0] := fsobAscending;

          //FldArray[1] := 0;
          //FldArray[2] := 3;
          //FldArray[3] := 4;
          IdxHlprs[0] := '';
          //IdxHlprs[1] := '';
          //IdxHlprs[2] := '';
          //IdxHlprs[3] := '';
         // cur.Dictionary.AddIndex('REL', '', 0, 1, aFldArray, IdxHlprs, True, True, True);
         // Result := cur.SortRecords(aFldArray, aOrderByArray, 1) = DBIERR_NONE;
          //Result := cur.GetNextRecord(Nil, ffsltNone) = DBIERR_NONE;
        End;
    End;
End;

Function TCursorInterpretator.iExecSQLTableName(CursorName: String): String;
Var
  i: Longword;
  cur: TfsSrBaseCursor;
Begin
  Result := '';
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
      If cur <> Nil Then
        Result := cur.table.BaseName;
    End;
End;

Function TCursorInterpretator.iExecSQLNext(CursorName: String): Boolean;
Var
  aFlag: Byte;
  aRefNr: tffInt64;
  i: Longword;
  cur: TfsSrBaseCursor;
Begin
  aFlag := 0;
  Result := False;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
      If cur <> Nil Then
        Result := cur.GetNextRecord(Nil, ffsltNone, aFlag, aRefnr) = DBIERR_NONE;
    End;
End;

Function TCursorInterpretator.iExecSQLInsert(CursorName: String): Boolean;
Var
  aFlag: Byte;
  aRefNr: tffInt64;
  i, j, len: Longword;
  cur: TfsSrcCursor;
  aData: PffByteArray;
Begin
  aFlag := 0;
  Result := False;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrcCursor(i);
      If cur <> Nil Then
        Begin
          Len := Cur.Dictionary.RecordLength;
          FFGetMem(aData, Len);
          Try
            Cur.Dictionary.InitRecord(aData);
            For j := 0 To Cur.Dictionary.FieldCount - 1 Do
              FFSetBit(PffByteArray(@aData^[Len]), j);
            Result := Cur.InsertRecordNoDefault(aData, ffsltNone, 0, arefnr) = DBIERR_NONE;
            cur.SetToEnd;
            Result := cur.GetPriorRecord(Nil, ffsltNone, aFlag, aRefnr) = DBIERR_NONE;
          Finally
            FFFreeMem(aData, Len);
          End;
        End;
    End;
End;

Function TCursorInterpretator.iExecSQLClearRow(CursorName: String): Boolean;
Var
  aFlag: Byte;
  aRefNr: tffInt64;
  i, j, len: Longword;
  cur: TfsSrcCursor;
  aData: PffByteArray;
Begin
  aFlag := 0;
  Result := False;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrcCursor(i);
      If cur <> Nil Then
        Begin
          Len := Cur.Dictionary.RecordLength;
          FFGetMem(aData, Len);
          Try
            Result := cur.GetRecord(aData, ffsltNone, tluDatabase, aFlag, aRefnr, False) = DBIERR_NONE;
            If Result Then
              For j := 0 To Cur.Dictionary.FieldCount - 1 Do
                cur.Dictionary.SetRecordFieldNull(j, aData, True);
            Result := cur.ModifyRecord(aData, False, tluDatabase, 0, False, False) = DBIERR_NONE;
          Finally
            FFFreeMem(aData, Len);
          End;
        End;
    End;
End;

Function TCursorInterpretator.iExecSQLDelete(CursorName: String): Boolean;
Var
  i: Longword;
  cur: TfsSrBaseCursor;
Begin
  Result := False;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
      If cur <> Nil Then
        Result := cur.DeleteRecord(Nil) = DBIERR_NONE;
    End;
End;

Function TCursorInterpretator.iExecSQLRecordCount(CursorName: String): Longword;
Var
  aFlag: Byte;
  aRefNr: tffInt64;
  i: Longword;
  RecLen: Integer;
  cur: TfsSrBaseCursor;
  aData: PffByteArray;
Begin
  aFlag := 0;
  Result := 0;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
      If cur <> Nil Then
        Begin
          RecLen := cur.Dictionary.RecordLength;
          FFGetMem(aData, RecLen);
          Try
            Result := cur.GetRecord(aData, ffsltNone, tluDatabase, aFlag, aRefnr, False);
            If Result = DBIERR_NONE Then
              cur.GetRecordCount(Result);
            If Result = DBIERR_NONE Then
              Result := cur.GetRecord(aData, ffsltNone, tluDatabase, aFlag, aRefnr, False);
          Finally
            FFFreeMem(aData, RecLen);
          End;
        End;
    End;
End;

Function TCursorInterpretator.iExecSQLUpdate(CursorName, Field: String; Value: Variant): Boolean;
Var
  aFlag: Byte;
  aRefNr: tffInt64;
  i, SQLResult: Longword;
  j, RecLen: Integer;
  cur: TfsSrBaseCursor;
  FieldBuffer, OldData: PffByteArray;
Begin
  aFlag := 0;
  Result := False;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
      If cur <> Nil Then
        Begin
          j := cur.Dictionary.GetFieldFromName(Field);
          If j >= 0 Then
            Begin
              RecLen := cur.Dictionary.RecordLength;
              FFGetMem(OldData, RecLen);
              FFGetMem(FieldBuffer, cur.Dictionary.FieldLength[j]);
              Try
                SQLResult := cur.GetRecord(OldData, ffsltNone, tluDatabase, aFlag, aRefnr, False);
                If SQLResult = DBIERR_NONE Then
                  Begin
                    Try
                      fsCurSetValue(Value, Cur.CursorID, j, OldData, FieldBuffer);
                      Result := cur.ModifyRecord(OldData, False, tluDatabase, 0, False, False) = DBIERR_NONE;
                    Except
                      Result := False;
                    End;
                  End;
              Finally
                FFFreeMem(OldData, RecLen);
                FFFreeMem(FieldBuffer, cur.Dictionary.FieldLength[j]);
              End;
            End;
        End;
    End;
End;

Function TCursorInterpretator.iExecSQLStartTransaction(CursorName: String): Boolean;
Var
  i: Longword;
  cur: TfsSrBaseCursor;
Begin
  Result := False;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
      If cur <> Nil Then
        If cur.Database.TransactionID = 0 Then
          Result := cur.Engine.TransactionStartSQL(cur.DataBase.DatabaseID, False) = DBIERR_NONE;
    End;
End;

Function TCursorInterpretator.iExecSQLCommitTransaction(CursorName: String): Boolean;
Var
  i: Longword;
  cur: TfsSrBaseCursor;
Begin
  Result := False;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
      If cur <> Nil Then
        If cur.Database.TransactionID <> 0 Then
          Result := cur.Engine.TransactionCommitSQL(cur.DataBase.DatabaseID, False) = DBIERR_NONE;
    End;
End;

Function TCursorInterpretator.iExecSQLRollbackTransaction(CursorName: String): Boolean;
Var
  i: Longword;
  cur: TfsSrBaseCursor;
Begin
  Result := False;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
      If cur <> Nil Then
        If cur.Database.TransactionID <> 0 Then
          Result := cur.Engine.TransactionRollbackSQL(cur.DataBase.DatabaseID, False) = DBIERR_NONE;
    End;
End;

Function TCursorInterpretator.iExecSQLInTransaction(CursorName: String): Boolean;
Var
  i: Longword;
  cur: TfsSrBaseCursor;
Begin
  Result := False;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
      If cur <> Nil Then
        Result := cur.Database.TransactionID <> 0;
    End;
End;

Function TCursorInterpretator.iExecSQLPrior(CursorName: String): Boolean;
Var
  aFlag: Byte;
  aRefNr: tffInt64;
  i: Longword;
  cur: TfsSrBaseCursor;
Begin
  aFlag := 0;
  Result := False;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
      If cur <> Nil Then
        Result := cur.GetPriorRecord(Nil, ffsltNone, aFlag, aRefnr) = DBIERR_NONE;
    End;
End;

Function TCursorInterpretator.iExecSQLFirst(CursorName: String): Boolean;
Var
  aFlag: Byte;
  aRefNr: tffInt64;
  i: Longword;
  cur: TfsSrBaseCursor;
Begin
  aFlag := 0;
  Result := False;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
      If cur <> Nil Then
        Begin
          cur.SetToBegin;
          Result := cur.GetNextRecord(Nil, ffsltNone, aFlag, aRefnr) = DBIERR_NONE;
        End;
    End;
End;

Function TCursorInterpretator.iExecSQLEmpty(CursorName: String): Boolean;
Var
  aFlag: Byte;
  aRefNr: tffInt64;
  i: Longword;
  cur: TfsSrBaseCursor;
Begin
  aFlag := 0;
  Result := False;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
      If cur <> Nil Then
        Begin
          cur.SetToBegin;
          Result := cur.GetNextRecord(Nil, ffsltNone, aFlag, aRefnr) <> DBIERR_NONE;
        End;
    End;
End;

Function TCursorInterpretator.iExecSQLLast(CursorName: String): Boolean;
Var
  aFlag: Byte;
  aRefNr: tffInt64;
  i: Longword;
  cur: TfsSrBaseCursor;
Begin
  aFlag := 0;
  Result := False;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
      If cur <> Nil Then
        Begin
          cur.SetToEnd;
          Result := cur.GetPriorRecord(Nil, ffsltNone, aFlag, aRefnr) = DBIERR_NONE;
        End;
    End;
End;

Function TCursorInterpretator.iExecSQLEof(CursorName: String): boolean;
Var
  i: Longword;
  cur: TfsSrBaseCursor;
Begin
  Result := True;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
      If cur <> Nil Then
        Result := cur.Position = cpEOF;
    End;
End;

Function TCursorInterpretator.iExecSQLBof(CursorName: String): Boolean;
Var
  i: Longword;
  cur: TfsSrBaseCursor;
Begin
  Result := True;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
      If cur <> Nil Then
        Result := cur.Position = cpBOF;
    End;
End;

Procedure TCursorInterpretator.iExecSQLSetNewOld(Field: String; Value: Variant; Var iRecord: PffByteArray);
Var
  j: Integer;
  FieldBuffer: PffByteArray;
  cur: TfsSrBaseCursor;
Begin
  cur := SrcCursor;

  If cur <> Nil Then
    Begin
      j := cur.Dictionary.GetFieldFromName(Field);
      If j >= 0 Then
        Begin
          FFGetMem(FieldBuffer, cur.Dictionary.FieldLength[j]);
          Try
            fsCurSetValue(Value, Cur.CursorID, j, iRecord, FieldBuffer);
          Finally
            FFFreeMem(FieldBuffer, cur.Dictionary.FieldLength[j]);
          End;
        End;
    End;
End;

Function TCursorInterpretator.iExecSQLGetNewOld(Field: String; iRecord: PffByteArray; OldRec: Boolean): Variant;
Var
  j: Integer;
  FieldBuffer: PffByteArray;
  IsNull: boolean;
  cur: TfsSrBaseCursor;
  ofs, fll: Integer;
Begin
  Result := null;
  cur := SrcCursor;

  If cur <> Nil Then
    Begin
      j := cur.Dictionary.GetFieldFromName(Field);
      If j >= 0 Then
        Begin
          FFGetMem(FieldBuffer, cur.Dictionary.FieldLength[j]);
          Try
            If Not OldRec Then
              cur.Dictionary.GetRecordField(j, iRecord, IsNull, FieldBuffer)
            Else
              Begin
                ofs := cur.Dictionary.FieldOffset[j];
                fll := cur.Dictionary.FieldLength[j];
                Move(iRecord^[Ofs], FieldBuffer^, fll);
                IsNull := cur.Dictionary.IsRecordFieldNull(j, iRecord);
              End;
            If IsNull Then
              Result := Null
            Else
              Result := fsCurGetValue(Cur.CursorID, j, iRecord, FieldBuffer, False);
          Finally
            FFFreeMem(FieldBuffer, cur.Dictionary.FieldLength[j]);
          End;
        End;
    End;
End;

Function TCursorInterpretator.iExecSQLGetNewOldSql(Field: String; iRecord: PffByteArray; OldRec: Boolean): Variant;
Var
  j: Integer;
  FieldBuffer: PffByteArray;
  IsNull: boolean;
  cur: TfsSrBaseCursor;
  ofs, fll: Integer;
Begin
  Result := null;
  cur := SrcCursor;

  If cur <> Nil Then
    Begin
      j := cur.Dictionary.GetFieldFromName(Field);
      If j >= 0 Then
        Begin
          FFGetMem(FieldBuffer, cur.Dictionary.FieldLength[j]);
          Try
            If Not OldRec Then
              cur.Dictionary.GetRecordField(j, iRecord, IsNull, FieldBuffer)
            Else
              Begin
                ofs := cur.Dictionary.FieldOffset[j];
                fll := cur.Dictionary.FieldLength[j];
                Move(iRecord^[Ofs], FieldBuffer^, fll);
                IsNull := cur.Dictionary.IsRecordFieldNull(j, iRecord);
              End;
            If IsNull Then
              Result := Null
            Else
              Result := fsCurGetValueSql(Cur.CursorID, j, iRecord, FieldBuffer, False, IsNull);
          Finally
            FFFreeMem(FieldBuffer, cur.Dictionary.FieldLength[j]);
          End;
        End;
    End;
End;

Function TCursorInterpretator.iExecSQLFieldExists(CursorName, Field: String): Boolean;
Var
  i: Longword;
  cur: TfsSrBaseCursor;
Begin
  Result := False;
  Cur := Nil;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
    End;

  If cur <> Nil Then
    Result := cur.Dictionary.GetFieldFromName(Field) >= 0;
End;

Function TCursorInterpretator.iExecSQLFieldType(CursorName, Field: String): Integer;
Var
  i: Longword;
  cur: TfsSrBaseCursor;
Begin
  Result := -1;
  Cur := Nil;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
    End;

  If cur <> Nil Then
    Begin
      Result := cur.Dictionary.GetFieldFromName(Field);
      If Result >= 0 Then
        Result := Integer(Cur.Dictionary.FieldType[Result]);

    End;
End;

Function TCursorInterpretator.iExecSQLGetValue(CursorName, Field: String): Variant;
Var
  aFlag: Byte;
  aRefNr: tffInt64;
  j: Integer;
  OldData: PffByteArray;
  RecLen: Integer;
  FieldBuffer: PffByteArray;
  IsNull: boolean;
  SQLResult: TffResult;
  i: Longword;
  cur: TfsSrBaseCursor;
Begin
  aFlag := 0;
  Result := null;
  Cur := Nil;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
    End;

  If cur <> Nil Then
    Begin
      j := cur.Dictionary.GetFieldFromName(Field);
      If j >= 0 Then
        Begin
          RecLen := cur.Dictionary.RecordLength;
          FFGetMem(OldData, RecLen);
          FFGetMem(FieldBuffer, cur.Dictionary.FieldLength[j]);
          Try
            SQLResult := cur.GetRecord(OldData, ffsltNone, tluDatabase, aFlag, aRefnr, False);
            If SQLResult = DBIERR_NONE Then
              Begin
                cur.Dictionary.GetRecordField(j, OldData, IsNull, FieldBuffer);
                If IsNull Then
                  Result := Null
                Else
                  Result := fsCurGetValue(Cur.CursorID, j, OldData, FieldBuffer, True);
              End
            Else
              Result := Null;
          Finally
            FFFreeMem(OldData, RecLen);
            FFFreeMem(FieldBuffer, cur.Dictionary.FieldLength[j]);
          End;
        End;
    End;
End;

Function TCursorInterpretator.iExecSQLGetValueSql(CursorName, Field: String): Variant;
Var
  aFlag: Byte;
  aRefNr: tffInt64;
  j: Integer;
  OldData: PffByteArray;
  RecLen: Integer;
  FieldBuffer: PffByteArray;
  IsNull: boolean;
  SQLResult: TffResult;
  i: Longword;
  cur: TfsSrBaseCursor;
Begin
  aFlag := 0;
  Result := null;
  Cur := Nil;
  If Self.InternalCursorList.IndexOf(CursorName) <> -1 Then
    Begin
      i := Self.InternalCursorList.Variable1[CursorName];
      cur := TfsSrBaseCursor(i);
    End;

  If cur <> Nil Then
    Begin
      j := cur.Dictionary.GetFieldFromName(Field);
      If j >= 0 Then
        Begin
          RecLen := cur.Dictionary.RecordLength;
          FFGetMem(OldData, RecLen);
          FFGetMem(FieldBuffer, cur.Dictionary.FieldLength[j]);
          Try
            SQLResult := cur.GetRecord(OldData, ffsltNone, tluDatabase, aFlag, aRefnr, False);
            If SQLResult = DBIERR_NONE Then
              Begin
                cur.Dictionary.GetRecordField(j, OldData, IsNull, FieldBuffer);
                If IsNull Then
                  Result := Null
                Else
                  Result := fsCurGetValueSql(Cur.CursorID, j, OldData, FieldBuffer, False, IsNull);
              End
            Else
              Result := Null;
          Finally
            FFFreeMem(OldData, RecLen);
            FFFreeMem(FieldBuffer, cur.Dictionary.FieldLength[j]);
          End;
        End;
    End;
End;

// Function iExecSQLGetOldValue(Field: String): Variant;
// Procedure iExecSQLSetNewValue(Field: String; Value: Variant);

Procedure TCursorInterpretator.GetValue(Const Name: String; Var Value: Variant);
Var
  SName, SVar1, SVar2, SVar3: String;

  Function IsField(Sf: String): Integer;
  Begin
    Result := -1;
    If SrcCursor <> Nil Then
      Result := SrcCursor.Dictionary.GetFieldFromName(sf);
  End;

  Function GetUser: String;
  Var
    i: Integer;
  Begin
    Result := '';
    If Engine.ClientList.ClientCount > 0 Then
      Begin
        For i := 0 To Engine.ClientList.ClientCount - 1 Do
          Begin
            If Engine.ClientList.Client[ftFromIndex, i].ClientId = ClientId Then
              Begin
                Result := Engine.ClientList.Client[ftFromIndex, i].ClientName;
                break;
              End;
          End;
      End;
    If Result = '' Then Result := datetimetostr(now);
  End;
Begin
  If Self.ErrorResult <> 0 Then Exit;
  SName := Name;
  SName := Trim(UpperCase(SName));
  Try
    // for stringlist and dynamicarray
    If Pos('.', SName) <> 0 Then
      Begin
        SVar1 := Copy(SName, 1, Pos('.', SName) - 1);
        SVar2 := Copy(SName, Pos('.', SName) + 1, length(SName));
        SVar3 := '';
        If Pos('.', SVar2) <> 0 Then
          Begin
            SVar3 := Copy(SVar2, Pos('.', SVar2) + 1, length(SVar2));
            SVar2 := Copy(SVar2, 1, Pos('.', SVar2) - 1);
          End;
        SVar1 := Trim(SVar1);
        SVar2 := Trim(SVar2);
        SVar3 := Trim(SVar3);
        If SVar1 = '' Then
          If ErrorResult = 0 Then
            Begin
              Self.ErrorResult := 5002;
              Exit;
            End;

        If SVar1 = 'NEW' Then
          Value := iExecSQLGetNewOld(SVar2, Self.NewRecord, False)
        Else If SVar1 = 'OLD' Then
          Value := iExecSQLGetNewOld(SVar2, Self.OldRecord, True)
        Else If SVar1 = 'SQLOLD' Then
          Value := iExecSQLGetNewOldSql(SVar2, Self.OldRecord, True)
        Else If SVar1 = 'SQLNEW' Then
          Value := iExecSQLGetNewOldSql(SVar2, Self.NewRecord, False)
        Else If SVar1 = 'CURSOR' Then
          Begin
            If SVar3 = 'INTRANSACTION' Then
              Begin
                Value := iExecSQLInTransaction(SVar2);
              End
            Else
              Inherited GetValue(Name, Value);
          End
        Else
          Inherited GetValue(Name, Value);
      End
    Else
      Begin
        If SName = 'TABLENAME' Then
          Value := SrcCursor.Table.BaseName
        Else If SName = 'USER' Then
          Value := GetUser
        Else
          Inherited GetValue(Name, Value);
      End;
  Except
    If ErrorResult = 0 Then
      Self.ErrorResult := 50002;
  End;
End;

Procedure TCursorInterpretator.SetValue(Const Name: String; Value: Variant);
Var
  SName, SVar1, SVar2, SVar3: String;
  j: Integer;

  Function IsField(Sf: String): Integer;
  Begin
    Result := -1;
    If SrcCursor <> Nil Then
      Result := SrcCursor.Dictionary.GetFieldFromName(sf);
  End;
Begin
  If Self.ErrorResult <> 0 Then Exit;
  SName := Name;
  SName := Trim(UpperCase(SName));
  Try
    // for stringlist and dynamicarray
    If Pos('.', SName) <> 0 Then
      Begin
        SVar1 := Copy(SName, 1, Pos('.', SName) - 1);
        SVar2 := Copy(SName, Pos('.', SName) + 1, length(SName));
        SVar3 := '';
        If Pos('.', SVar2) <> 0 Then
          Begin
            SVar3 := Copy(SVar2, Pos('.', SVar2) + 1, length(SVar2));
            SVar2 := Copy(SVar2, 1, Pos('.', SVar2) - 1);
          End;
        SVar1 := Trim(SVar1);
        SVar2 := Trim(SVar2);
        SVar3 := Trim(SVar3);
        If SVar1 = '' Then
          If ErrorResult = 0 Then
            Begin
              Self.ErrorResult := 5002;
              Exit;
            End;

        If SVar1 = 'OLD' Then
          Begin
            j := IsField(SVar2);
            If j > -1 Then
              Begin
                If (vartype(Value) = varString) Or (vartype(Value) = varOleStr) Then
                  Begin
                    If (Uppercase(Value) = 'NULL') Or (Value = '') Then
                      Begin
                        iExecSQLSetNewOld(SVar2, Null, Self.OldRecord);
                      End
                    Else
                      Begin
                        iExecSQLSetNewOld(SVar2, Value, Self.OldRecord);
                      End;
                  End
                Else
                  Begin
                    If Value = null Then
                      Begin
                        iExecSQLSetNewOld(SVar2, Null, Self.OldRecord);
                      End
                    Else
                      Begin
                        iExecSQLSetNewOld(SVar2, Value, Self.OldRecord);
                      End;
                  End;
              End
            Else
              Begin
                If ErrorResult = 0 Then
                  Self.ErrorResult := 50002;
                Exit;
              End;
          End
        Else If SVar1 = 'NEW' Then
          Begin
            j := IsField(SVar2);
            If j > -1 Then
              Begin
                If (vartype(Value) = varString) Or (vartype(Value) = varOleStr) Then
                  Begin
                    If (Uppercase(Value) = 'NULL') Or (Value = '') Then
                      Begin
                        iExecSQLSetNewOld(SVar2, Null, Self.NewRecord);
                      End
                    Else
                      Begin
                        iExecSQLSetNewOld(SVar2, Value, Self.NewRecord);
                      End;
                  End
                Else
                  Begin
                    If Value = null Then
                      Begin
                        iExecSQLSetNewOld(SVar2, Null, Self.NewRecord);
                      End
                    Else
                      Begin
                        iExecSQLSetNewOld(SVar2, Value, Self.NewRecord);
                      End;
                  End;
              End
            Else
              Begin
                If ErrorResult = 0 Then
                  Self.ErrorResult := 50002;
                Exit;
              End;
          End
        Else
          Inherited SetValue(Name, Value);
      End
    Else
      Inherited SetValue(Name, Value);
  Except
    If ErrorResult = 0 Then
      Self.ErrorResult := 50002;
  End;
End;

Procedure TCursorInterpretator.GetSetValueFunction(Const Name: String; p1, p2, p3, p4, p5, p6, p7, p8, p9, p10: Variant;
  Var Value: Variant);
Var
  SName, SVar1, SVar2, SVar3: String;
  v1, s: String;
  i: Integer;
  j: Longword;
  tq: TfsSrBaseCursor;
  fName: Variant; // name
  Value1: Variant; // datatype
  Value2: Variant; // size
  Value3: Variant; // decimals
  Value4: Variant; // round

  Function IsField(Sf: String): Integer;
  Begin
    Result := -1;
    If SrcCursor <> Nil Then
      Result := SrcCursor.Dictionary.GetFieldFromName(sf);
  End;

  Function FieldType(Sf: String): Integer;
  Var
    i: Integer;
  Begin
    Result := -1;
    If SrcCursor <> Nil Then
      Begin
        Try
          i := IsField(sf);
          If i >= 0 Then
            Result := Integer(SrcCursor.Dictionary.FieldType[i]);
        Except
        End;
      End;
  End;

  Procedure GetData(Var s: String; MaxVarRecurse: Integer = 10);
  Var
    i, j, k, l, m: Longint;
    s1, s2: String;
  Begin
    i := 1;
    k := 0;
    Repeat
      While (i < Length(s)) And (s[i] <> '[') Do
        Inc(i);
      s1 := ExpandVar(s, i, j);
      If i <> j Then
        Begin
          Delete(s, i, j - i + 1);
          s2 := '';
          s2 := Parser.Calculate(s1);

          Insert(s2, s, i);
          l := pos('[', s);
          m := pos(']', s);
          If k < MaxVarRecurse Then
            Begin
              If (l > 0) And (m > 0) Then
                Begin
                  If l > m Then
                    Inc(i, Length(s2))
                  Else
                    Inc(k);
                End
              Else
                Inc(i, Length(s2));
            End
          Else
            Inc(i, Length(s2));
          j := 0;
        End;
    Until i = j;
  End;

  Procedure RemoveQuotes(Var s: String);
  Begin
    If (s[1] = '"') And (s[Length(s)] = '"') Then
      s := Copy(s, 2, Length(s) - 2);
    If (s[1] = '''') And (s[Length(s)] = '''') Then
      s := Copy(s, 2, Length(s) - 2);
  End;

Begin
  If Self.ErrorResult <> 0 Then Exit;
  SName := Name;
  SName := Trim(UpperCase(SName));
  // for stringlist and dynamicarray
  If Pos('.', SName) <> 0 Then
    Begin
      SVar1 := Copy(SName, 1, Pos('.', SName) - 1);
      SVar2 := Copy(SName, Pos('.', SName) + 1, length(SName));
      SVar3 := '';
      If Pos('.', SVar2) <> 0 Then
        Begin
          SVar3 := Copy(SVar2, Pos('.', SVar2) + 1, length(SVar2));
          SVar2 := Copy(SVar2, 1, Pos('.', SVar2) - 1);
        End;
      SVar1 := Trim(SVar1);
      SVar2 := Trim(SVar2);
      SVar3 := Trim(SVar3);
      If SVar1 = '' Then
        If ErrorResult = 0 Then
          Begin
            Self.ErrorResult := 5002;
            Exit;
          End;

      If SVar1 = 'CURSOR' Then
        Begin
          If SVar3 = 'ISCURSORRESULT' Then
            Begin
              CursorProcedure := 0;
              //Value := 1;
              For i := 0 To Self.InternalCursorList.Count - 1 Do
                Self.InternalCursorList.Value3[i] := False;
              If Self.InternalCursorList.IndexOf(SVar2) >= 0 Then
                Begin
                  //Value := 0;
                  Self.InternalCursorList.Variable3[SVar2] := Parser.Calculate(p1);
                  If Self.InternalCursorList.Variable1[SVar2] Then
                    CursorProcedure := Self.InternalCursorList.Variable1[SVar2];
                End;
            End
          Else If SVar3 = 'SELECT' Then
            Begin // cursorname, sql
              v1 := p1;
              GetData(v1, MaxRecurseVar);
              v1 := Trim(v1);
              RemoveQuotes(v1);
              Value := iExecSQL(SVar2, v1);
            End
          Else If SVar3 = 'CREATE' Then
            Begin // cursorname reszta jest w zmiennej
              v1 := p1;
              GetData(v1, MaxRecurseVar);
              v1 := Trim(v1);
              RemoveQuotes(v1);
              //value := iExecSQLCreate(SVar2);
            End
          Else If (SVar2 = 'MODIFY') Then
            Begin // sql
              v1 := p1;
              GetData(v1, MaxRecurseVar);
              v1 := Trim(v1);
              RemoveQuotes(v1);
              Value := iExecModifySQL(v1);
            End
          Else If SVar3 = 'SQLVALUE' Then
            Begin // cursorname, fieldname
              Value := iExecSQLGetValueSql(SVar2, Parser.Calculate(p1));
            End
          Else If SVar3 = 'VALUE' Then
            Begin // cursorname, fieldname
              Value := iExecSQLGetValue(SVar2, Parser.Calculate(p1));
            End
          Else If SVar3 = 'UPDATE' Then
            Begin
              Value := iExecSQLUpdate(SVar2, Parser.Calculate(p1), Parser.Calculate(p2));
            End // CursorName, Field, Value
          Else If SVar3 = 'CLOSE' Then
            Begin // cursorname
              Value := iExecSQLClose(SVar2);
            End
          Else If SVar3 = 'FIELDEXISTS' Then
            Begin // cursorname
              Value := iExecSQLFieldExists(SVar2, Parser.Calculate(p1));
            End
          Else If SVar3 = 'FIELDTYPE' Then
            Begin // cursorname
              Value := iExecSQLFieldType(SVar2, Parser.Calculate(p1));
            End
          Else If SVar3 = 'NEXT' Then
            Begin // cursorname
              Value := iExecSQLNext(SVar2);
            End
          Else If SVar3 = 'PRIOR' Then
            Begin // cursorname
              Value := iExecSQLPrior(SVar2);
            End
          Else If SVar3 = 'FIRST' Then
            Begin // cursorname
              Value := iExecSQLFirst(SVar2);
            End
          Else If SVar3 = 'LAST' Then
            Begin // cursorname
              Value := iExecSQLLast(SVar2);
            End
          Else If SVar3 = 'EOF' Then
            Begin // cursorname
              Value := iExecSQLEOF(SVar2);
            End
          Else If SVar3 = 'BOF' Then
            Begin // cursorname
              Value := iExecSQLBOF(SVar2);
            End // cursorname
          Else If SVar3 = 'ISEMPTY' Then
            Begin
              Value := iExecSQLEmpty(SVar2);
            End // cursorname
          Else If SVar3 = 'INSERT' Then
            Begin
              Value := iExecSQLInsert(SVar2);
            End // cursorname
          Else If SVar3 = 'DELETE' Then
            Begin
              Value := iExecSQLDelete(SVar2);
            End // cursorname
          Else If SVar3 = 'RECORDCOUNT' Then
            Begin
              Value := iExecSQLRecordCount(SVar2);
            End // cursorname
          Else If SVar3 = 'CLEARROW' Then
            Begin
              Value := iExecSQLClearRow(SVar2);
            End // cursorname
          Else If SVar3 = 'SORT' Then
            Begin
              Value := False; //iExecSQLSort(SVar2, '', '');
            End
          Else If SVar3 = 'STARTTRANSACTION' Then
            Begin
              Value := iExecSQLStartTransaction(SVar2);
            End
          Else If SVar3 = 'ROLLBACKTRANSACTION' Then
            Begin
              Value := iExecSQLRollbackTransaction(SVar2);
            End
          Else If SVar3 = 'COMMITTRANSACTION' Then
            Begin
              Value := iExecSQLCommitTransaction(SVar2);
            End
          Else
            Inherited GetSetValueFunction(Name, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, Value);
        End
      Else If SVar1 = 'CREATERETURNVARIABLE' Then
        Begin
          InternalParamPosition2 := 1;
          While InternalVariables.IndexOf('_@#$-RETURN' + IntToStr(InternalParamPosition2)) > -1 Do
            Begin
              fName := InternalVariables.Variable1['_@#$-RETURN' + IntToStr(InternalParamPosition2)];
              Value1 := InternalVariables.Variable2['_@#$-RETURN' + IntToStr(InternalParamPosition2)];
              Value2 := InternalVariables.Variable3['_@#$-RETURN' + IntToStr(InternalParamPosition2)];
              Value3 := InternalVariables.Variable4['_@#$-RETURN' + IntToStr(InternalParamPosition2)];
              Value4 := InternalVariables.Variable5['_@#$-RETURN' + IntToStr(InternalParamPosition2)];
              inc(InternalParamPosition2);
            End;
          If InternalParamPosition2 > 1 Then
            Begin
              // create dictionary and cursor
              CursorProcedure := 0;
              CursorProcedure := CreateTemporaryTableWithoutIndex(SVar2);
              For i := 0 To Self.InternalCursorList.Count - 1 Do
                Self.InternalCursorList.Value3[i] := False;
              If Self.InternalCursorList.IndexOf(SVar2) >= 0 Then
                Self.InternalCursorList.Variable3[SVar2] := True;
            End;
        End
      Else
        Inherited GetSetValueFunction(Name, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, Value);
    End
  Else
    Begin
      If (SName = 'ERROR') Or (SName = 'ERRORDELETE') Or (SName = 'ERRORUPDATE') Or (SName = 'ERRORINSERT') Then
        Begin
          If (SName = 'ERROR') Then
            Self.ErrorResult := 50011
          Else If (SName = 'ERRORDELETE') Then
            Self.ErrorResult := 50013
          Else If (SName = 'ERRORUPDATE') Then
            Self.ErrorResult := 50014
          Else If (SName = 'ERRORINSERT') Then
            Self.ErrorResult := 50015;

          Try
            For i := 0 To Self.InternalCursorList.Count - 1 Do
              Begin
                // free sql alloc
                j := Self.InternalCursorList.Value1[i];
                tq := TfsSrBaseCursor(j);
                If tq <> Nil Then
                  Begin
                    tq.CloseTable := True;
                    Engine.CursorClose(j);
                  End;
                Engine.sqlfree(Self.InternalCursorList.Value2[i]);
              End;
            Self.InternalCursorList.Clear;
          Except
            Self.ErrorResult := 50012;
          End;
          Exit;
        End;
      Try
        If SName = 'FIELDEXISTS' Then
          Value := (IsField(Parser.Calculate(p1)) >= 0)
        Else If SName = 'FIELDTYPE' Then
          Value := FieldType(Parser.Calculate(p1))
        Else If SName = 'REGTRANSACTIONEVENT' Then
          Begin
            // events transactions VISIBLE ONLY USER TRANSACTION
            If ListEventsTransaction <> Nil Then
              Begin
                s := Parser.Calculate(p1);
                If Not ListEventsTransaction.Exists(s) Then
                  ListEventsTransaction.Insert(s);
              End
            Else
              Self.ErrorResult := 50011;
          End
        Else If SName = 'UNREGTRANSACTIONEVENT' Then
          Begin
            If ListEventsTransaction <> Nil Then
              ListEventsTransaction.Delete(Parser.Calculate(p1))
            Else
              Self.ErrorResult := 50011;
          End
        Else If SName = 'EXISTSTRANSACTIONEVENT' Then
          Begin
            Value := False;
            If ListEventsTransaction <> Nil Then
              Begin
                If ListEventsTransaction.Exists(Parser.Calculate(p1)) Then
                  Value := True;
              End
            Else
              Self.ErrorResult := 50011;
          End
        Else If SName = 'EMPTYTRANSACTIONEVENTS' Then
          Begin
            Value := True;
            If ListEventsTransaction <> Nil Then
              ListEventsTransaction.Empty
            Else
              Self.ErrorResult := 50011;
          End
        Else If SName = 'REGGLOBALTRANSACTIONEVENT' Then
          Begin
            // EVENTS USER TRANSACTION - VISIBLE FOR ANY USER BUT DESTROY IF COMMIT OR ROOLBACK
            If ListEventsGlobalTransaction <> Nil Then
              Begin
                s := Parser.Calculate(p1);
                If Not ListEventsGlobalTransaction.Exists(s) Then
                  Begin
                    ListEventsGlobalTransaction.Insert(s);
                    If Not Self.Engine.ListEventsTransaction.Exists(s) Then
                      Self.Engine.ListEventsTransaction.Insert(s);
                  End;
              End
            Else
              Self.ErrorResult := 50011;
          End
        Else If SName = 'UNREGGLOBALTRANSACTIONEVENT' Then
          Begin
            If ListEventsGlobalTransaction <> Nil Then
              Begin
                s := Parser.Calculate(p1);
                ListEventsGlobalTransaction.Delete(s);
                Self.Engine.ListEventsTransaction.Delete(s);
              End
            Else
              Self.ErrorResult := 50011;
          End
        Else If SName = 'EXISTSGLOBALTRANSACTIONEVENT' Then
          Begin
            Value := False;
            If ListEventsGlobalTransaction <> Nil Then
              Begin
                s := Parser.Calculate(p1);
                If ListEventsGlobalTransaction.Exists(s) And Self.Engine.ListEventsTransaction.Exists(s) Then
                  Value := True;
              End
            Else
              Self.ErrorResult := 50011;
          End
        Else If SName = 'EMPTYGLOBALTRANSACTIONEVENTS' Then
          Begin
            Value := True;
            If ListEventsGlobalTransaction <> Nil Then
              Begin
                For i := 0 To ListEventsGlobalTransaction.Count - 1 Do
                  Self.Engine.ListEventsTransaction.Delete(ListEventsGlobalTransaction.Strings[i]);
                ListEventsGlobalTransaction.Empty;
              End
            Else
              Self.ErrorResult := 50011;
          End
        Else If SName = 'REGSERVEREVENT' Then
          Begin
            // Events server  VISIBLE for al objects server
            If Self.Engine.ListEvents <> Nil Then
              Begin
                s := Parser.Calculate(p1);
                If Not Self.Engine.ListEvents.Exists(s) Then
                  Self.Engine.ListEvents.Insert(s);
              End
            Else
              Self.ErrorResult := 50011;
          End
        Else If SName = 'UNREGSERVEREVENT' Then
          Begin
            If Self.Engine.ListEvents <> Nil Then
              Self.Engine.ListEvents.Delete(Parser.Calculate(p1))
            Else
              Self.ErrorResult := 50011;
          End
        Else If SName = 'EXISTSSERVEREVENT' Then
          Begin
            Value := False;
            If Self.Engine.ListEvents <> Nil Then
              Begin
                If Self.Engine.ListEvents.Exists(Parser.Calculate(p1)) Then
                  Value := True;
              End
            Else
              Self.ErrorResult := 50011;
          End
        Else If SName = 'EMPTYESERVERVENTS' Then
          Begin
            Value := True;
            If Self.Engine.ListEvents <> Nil Then
              Self.Engine.ListEvents.Empty
            Else
              Self.ErrorResult := 50011;
          End
        Else If SName = 'RETURNVARIABLE' Then
          Begin
            fName := Trim(Parser.Calculate(p1));
            If fName = '' Then ErrorResult := 50016; // name
            If ErrorResult <> 0 Then Exit;

            Value1 := Trim(Parser.Calculate(p2));
            Value2 := Parser.Calculate(p3);
            Value3 := Parser.Calculate(p4);
            Value4 := Parser.Calculate(p5);
            InternalVariables.Variable1['_@#$-RETURN' + IntToStr(InternalParamPosition2)] := fName;
            InternalVariables.Variable2['_@#$-RETURN' + IntToStr(InternalParamPosition2)] := Value1;
            InternalVariables.Variable3['_@#$-RETURN' + IntToStr(InternalParamPosition2)] := Value2;
            InternalVariables.Variable4['_@#$-RETURN' + IntToStr(InternalParamPosition2)] := Value3;
            InternalVariables.Variable5['_@#$-RETURN' + IntToStr(InternalParamPosition2)] := Value4;

            inc(InternalParamPosition2);
          End
        Else
          Inherited GetSetValueFunction(Name, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, Value);

      Except
        If ErrorResult = 0 Then
          Self.ErrorResult := 50002;
      End;
    End;
End;
{===Initialization===================================================}

Procedure InitializeUnit;
Begin
  // systemadmin
  // masteradmin
  fsc_AdminPasswd := FSCalcShStrELFHash(XorString('!', 'L@RUDS@ELHO'));
  fsc_AdminUserID := XorString('!', 'RXRUDL@ELHO');
  // systemQuest
  // masterquest
  fsc_QuestPasswd := FSCalcShStrELFHash(XorString('!', 'L@RUDSPTDRU'));
  fsc_QuestUserID := XorString('!', 'RXRUDLpTDRU');
End;
{====================================================================}

Initialization
  InitializeUnit;
End.

