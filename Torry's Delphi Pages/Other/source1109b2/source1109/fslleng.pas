{$I fsdefine.inc}

Unit fslleng;

Interface

Uses
  Windows,
  Classes,
  fshash,
  fsllbase,
  fssrBase,
  fsllcomp,
  fslldict,
  fssrbde,
  fssrlock;

Type
  { This type defines the actions for which an extender may be notified.

    ffeaAfterCreateClient - Called after a client is created.
                            If an extender returns an error code other than
                            DBIERR_NONE, the client will not be added and the
                            error code returned to the client application.  The
                            client application is responsible for catching the
                            resulting exception and interpreting the error code
                            as there may be no client-side resource string
                            associated with the error code.
    - All "after" actions will ignore extender error messages

  }
  TffEngineAction = ({record actions}
    ffeaBeforeRecRead, ffeaAfterRecRead,
    ffeaBeforeRecInsert, ffeaAfterRecInsert, ffeaInsertRecFail,
    ffeaBeforeRecUpdate, ffeaAfterRecUpdate, ffeaUpdateRecFail,
    ffeaBeforeRecDelete, ffeaAfterRecDelete, ffeaDeleteRecFail,
    {table actions}
    ffeaBeforeTabRead,
    ffeaBeforeTabUpdate, ffeaTabUpdateFail,
    ffeaBeforeTabDelete, ffeaTabDeleteFail,

    ffeaBeforeTabEmpty, ffeaTabEmptyFail,
    ffeaBeforeTabSetInc, ffeaTabSetIncFail,
    ffeaBeforeTabSetFlags, ffeaTabSetFlagsFail,
    ffeaBeforeTabSetMaxRec, ffeaTabSetMaxRecFail,

    ffeaBeforeTabInsert, ffeaTabInsertFail,
    ffeaBeforeTabRestruct, ffeaTabRestructFail,
    ffeaBeforeTabPack, ffeaTabPackFail,
    ffeaBeforeAddInx, ffeaTabAddInxFail,
    ffeaBeforeRebuildInx, ffeaTabRebuildInxFail,
    ffeaBeforeTableLock, ffeaAfterTableLock, ffeaTableLockFail,
    {databaseactions}
    ffeaBeforeDBRead,
    ffeaBeforeDBUpdate, ffeaDBUpdateFail,
    ffeaBeforeDBDelete, ffeaDBDeleteFail,
    ffeaBeforeDBInsert, ffeaDBInsertFail,
    ffeaBeforeChgAliasPath, ffeaChgAliasPathFail,
    {transactions actions}
    ffeaAfterStartTrans,
    ffeaBeforeCommit, ffeaAfterCommit, ffeaCommitFail, {!!.06}
    ffeaBeforeRollback, ffeaAfterRollback,
    {cursor actions}
    ffeaBeforeCursorClose,
    {BLOB actions}
    ffeaBeforeBLOBCreate, ffeaAfterBLOBCreate, ffeaBLOBCreateFail,
    ffeaBeforeBLOBRead, ffeaAfterBLOBRead, ffeaBLOBReadFail,
    ffeaBeforeBLOBWrite, ffeaAfterBLOBWrite, ffeaBLOBWriteFail,
    ffeaBeforeBLOBDelete, ffeaAfterBLOBDelete, ffeaBLOBDeleteFail,
    ffeaBeforeBLOBTruncate, ffeaAfterBLOBTruncate, ffeaBLOBTruncateFail,
    ffeaBeforeBLOBGetLength, ffeaAfterBLOBGetLength, ffeaBLOBGetLengthFail,
    ffeaBeforeBLOBFree, ffeaAfterBLOBFree, ffeaBLOBFreeFail,
    ffeaBeforeFileBLOBAdd, ffeaAfterFileBLOBAdd, ffeaFileBLOBAddFail,
    ffeaBeforeBLOBLinkAdd, ffeaAfterBLOBLinkAdd, ffeaBLOBLinkAddFail,
    {client actions}
    ffeaBeforeRemoveClient,
    ffeaAfterCreateClient,

    ffeaBeforeCopyTable, ffeaAfterCopyTable, ffeaCopyTableFail,

    {Future - read write, execute procedures, trigers, view}
    ffeaBeforeViewRead, ffeaAfterViewRead, ffaeExecuteView,
    ffeaBeforeViewInsert, ffeaAfterViewInsert, ffeaInsertViewFail,
    ffeaBeforeViewUpdate, ffeaAfterViewUpdate, ffeaUpdateViewFail,
    ffeaBeforeViewDelete, ffeaAfterViewDelete, ffeaDeleteViewFail,

    ffeaBeforeProcRead, ffeaAfterProcRead, ffaeExecuteProc,
    ffeaBeforeProcInsert, ffeaAfterProcInsert, ffeaInsertProcFail,
    ffeaBeforeProcUpdate, ffeaAfterProcUpdate, ffeaUpdateProcFail,
    ffeaBeforeProcDelete, ffeaAfterProcDelete, ffeaDeleteProcFail,

    ffeaBeforeTrigRead, ffeaAfterTrigRead, ffaeExecuteTrig,
    ffeaBeforeTrigInsert, ffeaAfterTrigInsert, ffeaInsertTrigFail,
    ffeaBeforeTrigUpdate, ffeaAfterTrigUpdate, ffeaUpdateTrigFail,
    ffeaBeforeTrigDelete, ffeaAfterTrigDelete, ffeaDeleteTrigFail,

    ffeaBeforeReferRead, ffeaAfterReferRead,
    ffeaBeforeReferInsert, ffeaAfterReferInsert, ffeaInsertReferFail,
    ffeaBeforeReferUpdate, ffeaAfterReferUpdate, ffeaUpdateReferFail,
    ffeaBeforeReferDelete, ffeaAfterReferDelete, ffeaDeleteReferFail,

    ffeaBeforeGenerRead, ffeaAfterGenerRead, ffaeExecuteGener,
    ffeaBeforeGenerInsert, ffeaAfterGenerInsert, ffeaInsertGenerFail,
    ffeaBeforeGenerUpdate, ffeaAfterGenerUpdate, ffeaUpdateGenerFail,
    ffeaBeforeGenerDelete, ffeaAfterGenerDelete, ffeaDeleteGenerFail,

    ffeaBeforeExceptRead, ffeaAfterExceptRead, ffaeExecuteExcept,
    ffeaBeforeExceptInsert, ffeaAfterExceptInsert, ffeaInsertExceptFail,
    ffeaBeforeExceptUpdate, ffeaAfterExceptUpdate, ffeaUpdateExceptFail,
    ffeaBeforeExceptDelete, ffeaAfterExceptDelete, ffeaDeleteExceptFail,

    ffeaBeforeUserListRead, ffeaAfterUserListRead, ffaeExecuteUserList,
    ffeaBeforeUserListInsert, ffeaAfterUserListInsert, ffeaInsertUserListFail,
    ffeaBeforeUserListUpdate, ffeaAfterUserListUpdate, ffeaUpdateUserListFail,
    ffeaBeforeUserListDelete, ffeaAfterUserListDelete, ffeaDeleteUserListFail,

    ffeaBeforeProtectRow, ffeaAfterProtectRow, ffeaProtectRowFail,
    //ffeaBeforeProtectRowUpdate, ffeaAfterProtectRowUpdate,ffeaProtectRowUpdateFail,

    ffeaBeforeOpenData, // czy ma widzieæ tabele danych, uzytkownikow danych, systemowych
    ffeaBeforeOpenUser,
    ffeaBeforeOpenSystem,
    //
    {misc actions}
    ffeaNoAction {used when no fallback action needs to be taken}
    );

  TffInterestedActions = Set Of TffEngineAction;

  { Used by a monitor to register interest in a specific type of server object.
    For example, TffSrBaseCursor and TffSrDatabase. }
  TffServerObjectClass = Class Of TFSSpecObject;

  TFSBaseEngineMonitor = Class; { forward }
  TFSBaseEngineExtender = Class; { forward }
  TFSInterestStructure = Class; { forward }

  { TFSBaseServerEngine is an abstract, virtual class that specifies the
    minimum interface for a local or remote server engine.  The base engine
    provides support for adding and removing monitors. }
  TFSBaseServerEngine = Class(TfsStateComponent)
  Protected {private}

    FInterests: TFSInterestStructure;
    {-This data structure tracks the interest of various monitors. }

    FMonitors: TFSSpecThreadList;
    {-The monitors registered with the engine.  After a monitor registers
      itself with the engine, it identifies the types of server objects
      in which it is interested. }

  Protected
    {property access methods}
    Function bseGetAutoSaveCfg: Boolean; Virtual; Abstract;
    Function bseGetReadOnly: Boolean; Virtual; Abstract;
    Procedure bseSetAutoSaveCfg(aValue: Boolean); Virtual; Abstract; {!!.01}
    Procedure bseSetReadOnly(aValue: Boolean); Virtual; Abstract; {!!.01}
    Procedure scSetState(Const aState: TfsState); Override;
    Procedure AddInterest(aMonitor: TFSBaseEngineMonitor;
      serverObjectClass: TffServerObjectClass); Virtual;
    {-A monitor uses this method to register interest in a specific type of
      server object. }

{Begin !!.06}
    Function ProcessRequest(aClientID: TffClientID;
      aMsgID: Longint;
      aTimeout: Longint;
      aRequestData: Pointer;
      aRequestDataLen: Longint;
      aRequestDataType: TffNetMsgDataType;
      Var aReply: Pointer;
      Var aReplyLen: Longint;
      aReplyType: TffNetMsgDataType): TffResult; Virtual;
    { Backdoor method for sending a request to a server engine.
      Should only be implemented by remote server engines. }

    Function ProcessRequestNoReply(aClientID: TffClientID;
      aMsgID: Longint;
      aTimeout: Longint;
      aRequestData: Pointer;
      aRequestDataLen: Longint): TffResult; Virtual;
    { Backdoor method for sending a request, no reply expected, to a
      server engine. Should only be implemented by remote server engines. }
{End !!.06}

    Procedure RemoveAllInterest(aMonitor: TFSBaseEngineMonitor); Virtual;
    {-A monitor uses this method to unregister its interest for all classes
      in which it previously expressed interest. }

    Procedure RemoveInterest(aMonitor: TFSBaseEngineMonitor;
      serverObjectClass: TffServerObjectClass); Virtual;
    {-A monitor uses this method to remove interest in a specific type of
      server object. }

  Public
    {creation/destruction}
    Constructor Create(aOwner: TComponent); Override;

    Destructor Destroy; Override;

    Procedure FFAddDependent(ADependent: TFSSpecComp); Override; {!!.11}
    Procedure FFRemoveDependent(ADependent: TFSSpecComp); Override; {!!.11}

    Function GetInterestedMonitors(Const anObjectClass: TffServerObjectClass): TFSNormalList;
    {-Use this method to retrieve a list of engine monitors interested in a
      particular server object class.  If no monitors have registered
      interest then nil is returned.  Otherwise this function returns a
      TFSNormalList containing one or more TffIntListItems.  You can convert
      a TfsIntListItem into a TFSBaseEngineMonitor as follows:

      aMonitor := TFSBaseEngineMonitor(TfsIntListItem(TFSNormalList[index]).KeyAsInt);

      NOTE: The recipient of this functions' result is responsible for
            freeing the TFSNormalList.
    }

    Procedure GetServerNames(aList: TStrings;
      aTimeout: Longint); Virtual; Abstract;
    { Returns a list of the servers available through the server's
      transport. }

{Begin !!.10}
  { Event logging }
    Procedure Log(Const aMsg: String); Virtual; Abstract;
    {-Use this method to log a string to the event log. }

    Procedure LogAll(Const Msgs: Array Of String); Virtual; Abstract;
    {-Use this method to log multiple strings to the event log. }

    Procedure LogFmt(Const aMsg: String; args: Array Of Const); Virtual; Abstract;
    {-Use this method to log a formatted string to the event log. }

  {transaction tracking}
    Function InTransaction(Const aDatabaseID: TffDatabaseID; Var aTransLevel: Longint): TffResult; Virtual; Abstract;
    Function TransactionCorrupted(Const aDatabaseID: TffDatabaseID): TffResult; Virtual; Abstract;
    Function TransactionCommit(Const aDatabaseID: TffDatabaseID; aRemoveFile: Boolean = False): TffResult; Virtual; Abstract;
    Function TransactionRollback(Const aDatabaseID: TffDatabaseID): TffResult; Virtual; Abstract;
    Function TransactionStart(Const aDatabaseID: TffDatabaseID;
      Const aFailSafe: boolean): TffResult; Virtual; Abstract;
    Function TransactionStartWith(Const aDatabaseID: TffDatabaseID;
      Const aFailSafe: Boolean;
      Const aCursorIDs: TfsPointerList): TffResult; Virtual; Abstract;

    {client related stuff}
    Function ClientAdd(Var aClientID: TffClientID;
      Const aClientName: TffNetName;
      Const aUserID: TffName;
      Const timeout: Longint;
      Var aHash: TffWord32;
      Var aRights: TffUserRights;
      Var aSecurityEnabled: boolean): TffResult; Virtual; Abstract;

    {Begin !!.11}
    Function ClientAddEx(Var aClientID: TffClientID;
      Const aClientName: TffNetName;
      Const aUserID: TffName;
      Const timeout: Longint;
      Const aClientVersion: Longint;
      Var aHash: TffWord32;
      Var aRights: TffUserRights;
      Var aSecurityEnabled: boolean): TffResult; Virtual; Abstract;
    { Same as ClientAdd but client version is supplied via the aClientVersion
      parameter. }
{End !!.11}

    Function ClientRemove(aClientID: TffClientID): TffResult; Virtual; Abstract;
    Function ClientSetTimeout(Const aClientID: TffClientID;
      Const aTimeout: Longint): TffResult; Virtual; Abstract;

    {client session related stuff}
    Function SessionAdd(Const aClientID: TffClientID; Const timeout: Longint;
      Var aSessionID: TffSessionID;
      aData: Pointer;
      aDataLength: Longint = 0): TffResult; Virtual; Abstract;
    Function SessionCloseInactiveTables(aClientID: TffClientID): TffResult; Virtual; Abstract; {!!.06}
    Function SessionCount(aClientID: TffClientID; Var aCount: Integer): TffResult; Virtual; Abstract;
    Function SessionGetCurrent(aClientID: TffClientID; Var aSessionID: TffSessionID): TffResult; Virtual; Abstract;
    Function SessionRemove(aClientID: TffClientID; aSessionID: TffSessionID): TffResult; Virtual; Abstract;
    Function SessionSetCurrent(aClientID: TffClientID; aSessionID: TffSessionID): TffResult; Virtual; Abstract;
    Function SessionSetTimeout(Const aClientID: TffClientID;
      Const aSessionID: TffSessionID;
      Const aTimeout: Longint): TffResult; Virtual; Abstract;

    {database related stuff}
    Function DatabaseAddAlias(Const aAlias: TffName;
      Const aPath: TffPath;
      aCheckSpace: Boolean; {!!.11}
      Const aClientID: TffClientID)
      : TffResult; Virtual; Abstract;
    Function DatabaseAliasList(aList: TList;
      aClientID: TffClientID): TffResult; Virtual; Abstract;
    {-Return a list of database aliases.  aList will contain zero or more
      instances of PffAliasDescriptor. }

    Function RecoveryAliasList(aList: TList;
      aClientID: TffClientID): TffResult; Virtual; Abstract;
    {-Return a list of database aliases for use by a journal recovery
      engine. The functionality of this method is identical to
      DatabaseAliasList except that it does not require the server engine
      to be started. }
    Function DatabaseChgAliasPath(aAlias: TffName;
      aNewPath: TffPath;
      aCheckSpace: Boolean; {!!.11}
      aClientID: TffClientID)
      : TffResult; Virtual; Abstract;
    Function DatabaseClose(aDatabaseID: TffDatabaseID): TffResult; Virtual; Abstract;
    Function DatabaseDeleteAlias(aAlias: TffName;
      aClientID: TffClientID): TffResult; Virtual; Abstract;
    Function DatabaseGetAliasPath(aAlias: TffName;
      Var aPath: TffPath;
      aClientID: TffClientID): TffResult; Virtual; Abstract;
    Function DatabaseGetFreeSpace(Const aDatabaseID: TffDatabaseID;
      Var aFreeSpace: Int64): TffResult; Virtual; Abstract;
    Function DatabaseModifyAlias(Const ClientID: TffClientID;
      Const aAlias: TffName;
      Const aNewName: TffName;
      Const aNewPath: TffPath;
      aCheckSpace: Boolean) {!!.11}
    : TffResult; Virtual; Abstract;
    Function DatabaseOpen(aClientID: TffClientID;
      Const aAlias: TffName;
      Const aOpenMode: TffOpenMode;
      Const aShareMode: TffShareMode;
      Const aTimeout: Longint;
      Const aTransIsolation: TfsTransIsolation;
      Const aTransLocking: TfsDataBaseRecLocking;
      Var aDatabaseID: TffDatabaseID): TffResult; Virtual; Abstract;
    Function DatabaseOpenNoAlias(aClientID: TffClientID;
      Const aPath: TffPath;
      Const aOpenMode: TffOpenMode;
      Const aShareMode: TffShareMode;
      Const aTimeout: Longint;
      Const aTransIsolation: TfsTransIsolation;
      Const aTransLocking: TfsDataBaseRecLocking;
      Var aDatabaseID: TffDatabaseID): TffResult; Virtual; Abstract;
    Function DatabaseSetTimeout(Const aDatabaseID: TffDatabaseID;
      Const aTimeout: Longint): TffResult; Virtual; Abstract;
    Function DatabaseTableExists(aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName;
      Var aExists: Boolean): TffResult; Virtual; Abstract;
    Function DatabaseTableList(aDatabaseID: TffDatabaseID;
      Const aMask: TffFileNameExt;
      aList: TList): TffResult; Virtual; Abstract;
    Function DatabaseTableLockedExclusive(aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName;
      Var aLocked: Boolean): TffResult; Virtual; Abstract;
    {-Return a list of the tables for the specified database that fit the
      specified filename mask.  aList will contain zero or more instances
      of PffTableDescriptor. }

  {rebuild status related stuff}
    Function RebuildGetStatus(aRebuildID: Longint;
      Const aClientID: TffClientID;
      Var aIsPresent: boolean;
      Var aStatus: TffRebuildStatus): TffResult; Virtual; Abstract;

    {table related stuff}

    Function TableAddIndex(Const aDatabaseID: TffDatabaseID;
      Const aCursorID: TffCursorID;
      Const aTableName: TfsTableName;
      Const aIndexDesc: TffIndexDescriptor): TffResult; Virtual; Abstract;
    Function TableBuild(aDatabaseID: TffDatabaseID;
      aOverWrite: boolean;
      Const aTableName: TfsTableName;
      aForServer: boolean;
      aDictionary: TFSInfoDict): TffResult; Virtual; Abstract;
    Function TableDelete(aDatabaseID: TffDatabaseID; Const aTableName: TfsTableName): TffResult; Virtual; Abstract;
    Function TableDropIndex(aDatabaseID: TffDatabaseID;
      aCursorID: TffCursorID;
      Const aTableName: TfsTableName;
      Const aIndexName: TffDictItemName;
      aIndexID: Longint): TffResult; Virtual; Abstract;
    Function TableEmpty(aDatabaseID: TffDatabaseID;
      aCursorID: TffCursorID;
      Const aTableName: TfsTableName): TffResult; Virtual; Abstract;
    Function TableGetAutoInc(aCursorID: TffCursorID;
      Var aValue: Int64; Var aStep: Longint): TffResult; Virtual; Abstract;
    Function TableGetMaxRecords(aCursorID: TffCursorID;
      Var aValue: LongWord): TffResult; Virtual; Abstract;
    Function TableGetTableFlags(aCursorID: TffCursorID;
      Var aValue: Word): TffResult; Virtual; Abstract;
    Function TableGetTablePassword(aCursorID: TffCursorID;
      Var aValue: Longword): TffResult; Virtual; Abstract;
    Function TableGetTablePasswordRest(aCursorID: TffCursorID;
      Var aValue: Longword): TffResult; Virtual; Abstract;
    Function TableGetTableDBID(aCursorID: TffCursorID;
      Var aValue: Longword): TffResult; Virtual; Abstract;
    Function TableGetDictionary(aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName;
      aForServer: boolean;
      aStream: TStream): TffResult; Virtual; Abstract;
    Function TableGetRecCount(aCursorID: TffCursorID;
      Var aRecCount: LongWord): TffResult; Virtual; Abstract;
    Function TableGetRecCountAsync(aCursorID: TffCursorID; {!!.10}
      Var aRebuildID: Longint): TffResult; Virtual; Abstract; {!!.10}
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
      aSysOpen: boolean = True): TffResult; Virtual; Abstract;
    Function TablePack(aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName;
      Var aRebuildID: Longint; UndeleteRecords: Boolean; OnlyDeleted: boolean): TffResult; Virtual; Abstract;
    Function TableRebuildIndex(aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName;
      Const aIndexName: TffName;
      aIndexID: Longint;
      Var aRebuildID: Longint): TffResult; Virtual; Abstract;
    Function TableRename(aDatabaseID: TffDatabaseID; Const aOldName, aNewName: TffName): TffResult; Virtual; Abstract;
    Function TableRestructure(aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName;
      aDictionary: TFSInfoDict;
      aFieldMap: TFSSpecStringList;
      Var aRebuildID: Longint;
      aRangeError: boolean): TffResult; Virtual; Abstract;
    Function TableSetAutoInc(aCursorID: TffCursorID;
      aValue: Int64; aStep: Longint): TffResult; Virtual; Abstract;
    Function TableSetMaxRecords(aCursorID: TffCursorID;
      aValue: Longint): TffResult; Virtual; Abstract;
    Function TableSetTableFlags(aCursorID: TffCursorID;
      aValue: Word): TffResult; Virtual; Abstract;
    Function TableSetTablePassword(aCursorID: TffCursorID;
      aValue: Longword): TffResult; Virtual; Abstract;
    Function TableSetTablePasswordRest(aCursorID: TffCursorID;
      aValue: Longword): TffResult; Virtual; Abstract;
    Function TableSetTableDBID(aCursorID: TffCursorID;
      aValue: Longword): TffResult; Virtual; Abstract;
    {Begin !!.11}
    Function TableVersion(aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName;
      Var aVersion: Longint): TffResult; Virtual; Abstract;
    {End !!.11}

          {table locks via cursor}
    Function TableIsLocked(aCursorID: TffCursorID; aLockType: TffLockType;
      Var aIsLocked: boolean): TffResult; Virtual; Abstract;
    Function TableLockAcquire(aCursorID: TffCursorID; aLockType: TffLockType): TffResult; Virtual; Abstract;
    Function TableLockRelease(aCursorID: TffCursorID; aAllLocks: boolean): TffResult; Virtual; Abstract;

    {cursor stuff}
    Function CursorClone(aCursorID: TffCursorID; aOpenMode: TffOpenMode;
      Var aNewCursorID: TffCursorID): TffResult; Virtual; Abstract;
    Function CursorClose(aCursorID: TffCursorID): TffResult; Virtual; Abstract;
    Function CursorCompareBookmarks(aCursorID: TffCursorID;
      aBookmark1,
      aBookmark2: PffByteArray;
      Var aCompResult: Longint): TffResult; Virtual; Abstract;
    {Begin !!.02}
    Function CursorCopyRecords(aSrcCursorID,
      aDestCursorID: TffCursorID;
      aCopyBLOBs: Boolean; CountPerTrans: Longint): TffResult; Virtual; Abstract;
    {End !!.02}
    Function CursorDeleteRecords(aCursorID: TffCursorID; CountPerTrans: Longint): TffResult; Virtual; Abstract; {!!.06}
    Function CursorGetBookmark(aCursorID: TffCursorID; aBookmark: PffByteArray): TffResult; Virtual; Abstract;
    Function CursorGetBookmarkSize(aCursorID: TffCursorID;
      Var aSize: Integer): TffResult; Virtual; Abstract;
    {Begin !!.03}
    Function CursorListBLOBFreeSpace(aCursorID: TffCursorID;
      Const aInMemory: Boolean;
      aStream: TStream): TffResult; Virtual; Abstract;
    {End !!.03}
    Function CursorOverrideFilter(aCursorID: Longint;
      aExpression: pCANExpr;
      aTimeout: TffWord32): TffResult; Virtual; Abstract;
    Function CursorResetRange(aCursorID: TffCursorID): TffResult; Virtual; Abstract;
    Function CursorRestoreFilter(aCursorID: Longint): TffResult; Virtual; Abstract;
    Function CursorSetRange(aCursorID: TffCursorID;
      aDirectKey: boolean;
      aFieldCount1: Integer;
      aPartialLen1: Integer;
      aKeyData1: PffByteArray;
      aKeyIncl1: boolean;
      aFieldCount2: Integer;
      aPartialLen2: Integer;
      aKeyData2: PffByteArray;
      aKeyIncl2: boolean): TffResult; Virtual; Abstract;
    Function CursorSetTimeout(Const aCursorID: TffCursorID;
      Const aTimeout: Longint): TffResult; Virtual; Abstract;
    Function CursorSetToBegin(aCursorID: TffCursorID): TffResult; Virtual; Abstract;
    Function CursorSetToBookmark(aCursorID: TffCursorID; aBookmark: PffByteArray): TffResult; Virtual; Abstract;
    Function CursorSetToCursor(aDestCursorID: TffCursorID; aSrcCursorID: TffCursorID): TffResult; Virtual; Abstract;
    Function CursorSetToEnd(aCursorID: TffCursorID): TffResult; Virtual; Abstract;
    Function CursorSetToKey(aCursorID: TffCursorID;
      aSearchAction: TffSearchKeyAction;
      aDirectKey: boolean;
      aFieldCount: Integer;
      aPartialLen: Integer;
      aKeyData: PffByteArray): TffResult; Virtual; Abstract;
    Function CursorSwitchToIndex(aCursorID: TffCursorID;
      aIndexName: TffDictItemName;
      aIndexID: Integer;
      aPosnOnRec: boolean): TffResult; Virtual; Abstract;
    Function CursorSetFilter(aCursorID: TffCursorID;
      aExpression: pCANExpr;
      aTimeout: TffWord32): TffResult; Virtual; Abstract;

    {record stuff}
    Function RecordDelete(aCursorID: TffCursorID; aData: PffByteArray): TffResult; Virtual; Abstract;
    Function RecordDeleteBatch(aCursorID: TffCursorID;
      aBMCount: Longint;
      aBMLen: Longint;
      aData: PffByteArray;
      aErrors: PffLongintArray): TffResult; Virtual; Abstract;
    Function RecordExtractKey(aCursorID: TffCursorID; aData: PffByteArray; aKey: PffByteArray): TffResult; Virtual; Abstract;
    Function RecordGet(aCursorID: TffCursorID; aLockType: TffLockType; aUserLockType: TfsUserRecLocking; aData: PffByteArray;
    Var aFlag: Byte; Var aRefNr: TffInt64; Const aUser: Boolean): TffResult; Virtual; Abstract;
    Function RecordGetBatch(aCursorID: TffCursorID;
      aRecCount: Longint;
      aRecLen: Longint;
      Var aRecRead: Longint;
      aData: PffByteArray;
      Var aError: TffResult): TffResult; Virtual; Abstract;
    Function RecordGetForKey(aCursorID: TffCursorID;
      aDirectKey: boolean;
      aFieldCount: Integer;
      aPartialLen: Integer;
      aKeyData: PffByteArray;
      aData: PffByteArray;
      aFirstCall: Boolean): TffResult; Virtual; Abstract;
    Function RecordGetSetPosition(aValue: Longint; aCursorID: TffCursorID;
      aLockType: TffLockType;
      aData: PffByteArray;
      Var aFlag: Byte;
      Var aRecNo: LongWord;
      Var aRefNr: TffInt64;
      aInfoGetSetPosition: TInfoGetSetPosition;
      aSet: Boolean): TffResult; Virtual; Abstract;

    Function RecordGetNext(aCursorID: TffCursorID; aLockType: TffLockType; aData: PffByteArray; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult; Virtual; Abstract;
    Function RecordGetPrior(aCursorID: TffCursorID; aLockType: TffLockType; aData: PffByteArray; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult; Virtual; Abstract;
    Function RecordInsert(aCursorID: TffCursorID; aLockType: TffLockType; aData: PffByteArray; aUndelete: Boolean; Var aRefNr: TffInt64): TffResult; Virtual; Abstract;
    Function RecordInsertBatch(aCursorID: TffCursorID;
      aRecCount: Longint;
      aRecLen: Longint;
      aData: PffByteArray;
      aErrors: PffLongintArray): TffResult; Virtual; Abstract;
    Function RecordIsLocked(aCursorID: TffCursorID; aLockType: TffLockType;
      Var aIsLocked: boolean): TffResult; Virtual; Abstract;
    Function RecordModify(aCursorID: TffCursorID; aData: PffByteArray;
    aRelLock: boolean; aUserLockType: TfsUserRecLocking; aFlag: byte; aSet, Use: Boolean): TffResult; Virtual;
      Abstract;
    Function RecordRelLock(aCursorID: TffCursorID; aAllLocks: boolean): TffResult; Virtual; Abstract;

    {BLOB stuff}
    Function BLOBCreate(aCursorID: TffCursorID;
      Var aBlobNr: TffInt64): TffResult; Virtual; Abstract;
    Function BLOBDelete(aCursorID: TffCursorID; aBLOBNr: TffInt64): TffResult; Virtual; Abstract;
    {Begin !!.03}
    Function BLOBListSegments(aCursorID: TffCursorID;
      aBLOBNr: TffInt64;
      aStream: TStream): TffResult; Virtual; Abstract;
    {End !!.03}
    Function BLOBRead(aCursorID: TffCursorID;
      aFieldNo: TffWord32;
      aBLOBNr: TffInt64;
      aOffset: TffWord32; {!!.06}
      aLen: TffWord32; {!!.06}
      Var aBLOB;
      Var aBytesRead: TffWord32) {!!.06}
    : TffResult; Virtual; Abstract;
    Function BLOBFree(aCursorID: TffCursorID; aBLOBNr: TffInt64;
      ReadOnly: boolean): TffResult; Virtual; Abstract;
    Function BLOBGetLength(aCursorID: TffCursorID; aBLOBNr: TffInt64;
      Var aLength: Longint): TffResult; Virtual; Abstract;
    Function BLOBTruncate(aCursorID: TffCursorID; aBLOBNr: TffInt64;
      aBLOBLength: Longint): TffResult; Virtual; Abstract;
    Function BLOBWrite(aCursorID: TffCursorID;
      aFieldNo: TffWord32;
      aBLOBNr: TffInt64;
      aOffset: Longint;
      aLen: Longint;
      Var aBLOB): TffResult; Virtual; Abstract;
    Function FileBLOBAdd(aCursorID: TffCursorID;
      Const aFileName: TffFullFileName;
      Var aBLOBNr: TffInt64): TffResult; Virtual; Abstract;

    {SQL Stuff }
    Function SQLAlloc(aClientID: TffClientID;
      aDatabaseID: TffDatabaseID;
      aTimeout: Longint;
      Var aStmtID: TffSqlStmtID): TffResult; Virtual; Abstract;
    Function SQLExec(aStmtID: TffSqlStmtID;
      aOpenMode: TffOpenMode;
      Var aCursorID: TffCursorID;
      aStream: TStream): TffResult; Virtual; Abstract;
    Function SQLExecDirect(aClientID: TffClientID;
      aDatabaseID: TffDatabaseID;
      aQueryText: PChar;
      aTimeout: Longint;
      aOpenMode: TffOpenMode;
      Var aCursorID: TffCursorID;
      aStream: TStream): TffResult; Virtual; Abstract;
    Function SQLFree(aStmtID: TffSqlStmtID): TffResult; Virtual; Abstract;
    Function SQLPrepare(aStmtID: TffSqlStmtID;
      aQueryText: PChar;
      aStream: TStream): TffResult; Virtual; Abstract;
    Function SQLSetParams(aStmtID: TffSqlStmtID;
      aNumParams: Word;
      aParamDescs: Pointer;
      aDataBuffer: PffByteArray;
      aDataLen: Integer;
      aStream: TStream): TffResult; Virtual; Abstract;

    {misc stuff}
    Function GetServerDateTime(Var aDateTime: TDateTime): TffResult; Virtual; Abstract;
    {Begin !!.10}
    Function GetServerSystemTime(Var aSystemTime: TSystemTime)
      : TffResult; Virtual; Abstract;
    Function GetServerGUID(Var aGUID: TGUID)
      : TffResult; Virtual; Abstract;
    Function GetServerID(Var aUniqueID: TGUID)
      : TffResult; Virtual; Abstract;
    Function GetServerStatistics(Var aStats: TfsServerStatistics)
      : TffResult; Virtual; Abstract;
    Function GetCommandHandlerStatistics(Const aCmdHandlerIdx: Integer;
      Var aStats: TfsCommandHandlerStatistics)
      : TffResult; Virtual; Abstract;
    Function GetTransportStatistics(Const aCmdHandlerIdx: Integer;
      Const aTransportIdx: Integer;
      Var aStats: TfsTransportStatistics)
      : TffResult; Virtual; Abstract;
    {End !!.10}
  Published

    Property IsReadOnly: Boolean
      Read bseGetReadOnly
      Write bseSetReadOnly {!!.01}
    Default False; {!!.01}

    Property NoAutoSaveCfg: Boolean
      Read bseGetAutoSaveCfg
      Write bseSetAutoSaveCfg {!!.01}
    Default False; {!!.01}

  End;

  { This is the base implementation for an engine monitor.  An engine monitor
    attaches directly to a server engine and registers interest in specific
    types of server objects.  When an object of that type is opened in the
    server, the monitor has the opportunity to express interest in the object.
    The monitor can then supply an extender that will be associated with the
    object and will receive notification of events pertaining to the object. }
  TFSBaseEngineMonitor = Class(TfsStateComponent)
  Protected

    FServerEngine: TFSBaseServerEngine;

    Procedure bemSetServerEngine(anEngine: TFSBaseServerEngine); Virtual;
    {-Called when a monitor is associated with a server engine.  If the
      monitor is already associated with a server engine then it calls
      OldEngine.RemoveMonitor.  If the monitor is to be associated with
      a new engine then it calls NewEngine.AddMonitor.
      Subclasses should override this method to register interest in specific
      types of server objects. }

  { State methods }
    Procedure scInitialize; Override;
    Procedure scPrepareForShutdown; Override;
    Procedure scShutdown; Override;
    Procedure scStartup; Override;

  Public

    Destructor Destroy; Override;

    Procedure AddInterest(anObjectClass: TffServerObjectClass);
    {-Use this method to have the monitor notify its parent server engine
      of interest in a server object class. }

    Procedure FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp; {!!.11}
      Const AData: TffWord32); Override; {!!.11}

    Procedure RemoveAllInterest;
    {-Use this method to have the monitor tells its parent engine to remove
      all interests of the monitor. }

    Procedure RemoveInterest(anObjectClass: TffServerObjectClass);
    {-Use this method to have the monitor tells its parent engine to remove
      its interest in the specified object class. }

    Function Interested(aServerObject: TFSSpecObject): TFSBaseEngineExtender; Virtual; Abstract;
    { This function is called from the server when an object (e.g., cursor)
      is first opened. If the monitor is interested in receiving events
      for the object, it must create and return an instance of a class that
      can handle events for the object.  Otherwise it should return nil.
      This method is called only for the type of objects in which the monitor
      previously expressed interested.

      When deriving a class from TFSBaseEngineMonitor, it is up to the
      extender designer to verify the class of ServerObject is one that is
      expected.
    }

  Published

    Property ServerEngine: TFSBaseServerEngine Read FServerEngine
      Write bemSetServerEngine;
    { Associates an engine monitor with an engine. }
  End;

  { This is the base class for engine extenders.  An engine extender is attached
    to a specific type of server object as governed by an engine monitor.  The
    types of notifications received by the extender depend upon the type of
    object being extended.
    An extender is freed when the server object with which it is associated
    is freed. }
  TFSBaseEngineExtender = Class(TFSSpecObject)
  Protected
    FParent: TFSBaseEngineMonitor;
    FActions: TffInterestedActions;
    { Set of actions extender is interested in.}
  Public
    Constructor Create(aOwner: TFSBaseEngineMonitor); Virtual;
    Function Notify(aServerObject: TFSSpecObject;
      aAction: TffEngineAction): TffResult; Virtual; Abstract;
    { This method is called when the extender is to be notified of an
      action affecting the server object with which the extender is
      associated.  If the extender performs its operations, whatever they
      may be, then this function should return DBIERR_NONE.  If a failure
      occurs and the server should discontinue the current operation with this
      server object, this function should return an error code other than
      DBIERR_NONE.

      Some actions may pay attention to the error codes while other actions
      may ignore the error codes.  If an action pays attention to the error
      code then extenders "after" the extender returning the error will not
      be notified of the action.
    }

    Property InterestedActions: TffInterestedActions
      Read FActions;
    { The set of actions in which the extender is interested. }

  End;

  { The following class is used to track a monitor's interest.  It stores
    data in the following manner:

    1. To support retrieval of all monitors interested in a particular
       class of object, it creates a hash table where the hash is based
       on the class' name.  The hash bucket points to a list of monitors.

    2. To support removal of all interest for a monitor, it maintains a
       separate hash table where the hash is based upon the monitor}
  TFSInterestStructure = Class(TFSSpecObject)
  Private
    FHashByInterest: TfsHash;
    { Given a server object class, this hash table returns a list of the
      monitors interested in that object class. }

    FHashByMonitor: TfsHash;
    { Given an engine monitor, this hash table returns a list of the
      object classes in which the monitor has expressed interest.  We use
      this data structure in RemoveAllInterest to speed up our search
      for the monitors in FHashByInterest. }

    FPortal: TfsReadWritePortal;
  Protected
    Procedure DisposeList(Sender: TfsBaseHashTable; aData: pointer);
    {-This method is called when a hash table entry is removed. }

    Procedure RemoveInterestPrim(Const aMonitor: TFSBaseEngineMonitor;
      Const anObjectClass: TffServerObjectClass);
    {-This method removes an interest entry from the FHashByInterest
      hash table. }

  Public

    Constructor Create;

    Destructor Destroy; Override;

    Procedure AddInterest(Const aMonitor: TFSBaseEngineMonitor;
      Const anObjectClass: TffServerObjectClass);
    {-Use this method to add a monitor's interest in a certain class. }

    Function BeginRead: TFSInterestStructure;
    {-Use this method to obtain read access to the data. }

    Function BeginWrite: TFSInterestStructure;
    {-Use this method to obtain write access to the data. }

    Procedure EndRead;
    {-This method must be called after BeginRead once read access is no
      longer needed. }

    Procedure EndWrite;
    {-This method must be called after BeginWrite once write access is no
      longer needed. }

    Function GetInterestedMonitors(Const anObjectClass: TffServerObjectClass): TFSNormalList;
    {-Use this method to retrieve a list of engine monitors interested in a
      particular server object class.  If no monitors have registered
      interest then nil is returned.  Otherwise this function returns a
      TFSNormalList containing one or more TffIntListItems.  You can convert
      a TfsIntListItem into a TFSBaseEngineMonitor as follows:

      aMonitor := TFSBaseEngineMonitor(TfsIntListItem(TFSNormalList[index]).KeyAsInt);

      NOTE: The recipient of this functions' result is responsible for
            freeing the TFSNormalList.
    }

    Procedure RemoveAllInterest(Const aMonitor: TFSBaseEngineMonitor);
    {-Use this method to remove interest in all things for which a monitor
      previously registered interest. }

    Procedure RemoveInterest(Const aMonitor: TFSBaseEngineMonitor;
      Const anObjectClass: TffServerObjectClass);
    {-Use this method to remove a monitor's interest in a certain class. }

  End;

Var
  FFServerEngines: TFSSpecThreadList;

Implementation

{===TFSBaseServerEngine==============================================}

Constructor TFSBaseServerEngine.Create(aOwner: TComponent);
Var
  aListItem: TfsIntListItem;
Begin
  Inherited Create(aOwner);
  { Add our instance to the global server list }
  aListItem := TfsIntListItem.Create(Longint(Self));
  With FFServerEngines.BeginWrite Do
    Try
      Insert(aListItem);
    Finally
      EndWrite;
    End;

  FInterests := TFSInterestStructure.Create;
  FMonitors := TFSSpecThreadList.Create;
End;
{--------}

Destructor TFSBaseServerEngine.Destroy;
Begin
  FFNotifyDependents(ffn_Destroy); {!!.11}
  FMonitors.Free; {!!.11}

  If assigned(FInterests) Then
    Begin
      FInterests.Free;
      FInterests := Nil;
    End;

  { Remove our instance from the global server list }
  With FFServerEngines.BeginWrite Do
    Try
      Delete(Longint(Self));
    Finally
      EndWrite;
    End;

  Inherited Destroy;

End;
{--------}

Procedure TFSBaseServerEngine.scSetState(Const aState: TfsState);
Var
  Idx: Longint;
  NextState: TfsState;
  OldState: TfsState;
  Monitor: TFSBaseEngineMonitor;
Begin

  If aState = scState Then Exit;

  OldState := scState;

  Try
    If Assigned(FMonitors) Then
      With FMonitors.BeginRead Do
        Try
          While scState <> aState Do
            Begin
              { Based upon our current state & the target state, get the next state. }
              NextState := fsStateDiagram[scState, aState];

              { Move all monitors to the specified state. }
              For Idx := Pred(Count) Downto 0 Do
                Begin
                  Monitor := TFSBaseEngineMonitor(TfsIntListItem(Items[Idx]).KeyAsInt);
                  Monitor.State := NextState;
                End;
              { Change our state. }
              scState := NextState;
              { Call the appropriate internal method for this state. }
              Case NextState Of
                fsesInactive, fsesStopped:
                  scShutdown;
                fsesInitializing:
                  scInitialize;
                fsesStarting:
                  scStartup;
                fsesShuttingDown, fsesStopping:
                  scPrepareForShutdown;
              End; { case }
              If assigned(scOnStateChange) Then
                scOnStateChange(Self);
            End; { while }
        Finally
          EndRead;
        End
    Else
      Inherited;
  Except
    scState := OldState;
    Raise;
  End;
End;
{--------}

Procedure TFSBaseServerEngine.AddInterest(aMonitor: TFSBaseEngineMonitor;
  serverObjectClass: TffServerObjectClass);
Begin
  With FInterests.BeginWrite Do
    Try
      AddInterest(aMonitor, serverObjectClass);
    Finally
      EndWrite;
    End;
End;
{Begin !!.11}
{--------}

Procedure TFSBaseServerEngine.FFAddDependent(ADependent: TFSSpecComp);
Var
  aListItem: TfsIntListItem;
Begin
  Inherited;
  If ADependent Is TFSBaseEngineMonitor Then
    Begin
      aListItem := TfsIntListItem.Create(Longint(ADependent));
      With FMonitors.BeginWrite Do
        Try
          FMonitors.Insert(aListItem);
        Finally
          EndWrite;
        End;
    End;
End;
{--------}

Procedure TFSBaseServerEngine.FFRemoveDependent(ADependent: TFSSpecComp);
Begin
  Inherited;
  If ADependent Is TFSBaseEngineMonitor Then
    With FMonitors.BeginWrite Do
      Try
        Delete(Longint(ADependent));
        RemoveAllInterest(TFSBaseEngineMonitor(ADependent));
      Finally
        EndWrite;
      End;
End;
{End !!.11}
{--------}

Function TFSBaseServerEngine.GetInterestedMonitors
  (Const anObjectClass: TffServerObjectClass): TFSNormalList;
Begin
  With FInterests.BeginRead Do
    Try
      Result := FInterests.GetInterestedMonitors(anObjectClass);
    Finally
      EndRead;
    End;
End;
{Begin !!.06}
{--------}

Function TFSBaseServerEngine.ProcessRequest(aClientID: TffClientID;
  aMsgID: Longint;
  aTimeout: Longint;
  aRequestData: Pointer;
  aRequestDataLen: Longint;
  aRequestDataType: TffNetMsgDataType;
  Var aReply: Pointer;
  Var aReplyLen: Longint;
  aReplyType: TffNetMsgDataType): TffResult;
Begin
  { Do nothing. }
  Result := DBIERR_NONE;
End;
{--------}

Function TFSBaseServerEngine.ProcessRequestNoReply(aClientID: TffClientID;
  aMsgID: Longint;
  aTimeout: Longint;
  aRequestData: Pointer;
  aRequestDataLen: Longint): TffResult;
Begin
  { Do nothing. }
  Result := DBIERR_NONE;
End;
{End !!.06}
{--------}

Procedure TFSBaseServerEngine.RemoveAllInterest(aMonitor: TFSBaseEngineMonitor);
Begin
  With FInterests.BeginWrite Do
    Try
      RemoveAllInterest(aMonitor);
    Finally
      EndWrite;
    End;
End;
{--------}

Procedure TFSBaseServerEngine.RemoveInterest(aMonitor: TFSBaseEngineMonitor;
  serverObjectClass: TffServerObjectClass);
Begin
  With FInterests.BeginWrite Do
    Try
      RemoveInterest(aMonitor, serverObjectClass);
    Finally
      EndWrite;
    End;
End;
{====================================================================}

{===TFSBaseEngineMonitor=============================================}

Destructor TFSBaseEngineMonitor.Destroy;
Begin
  If assigned(FServerEngine) Then
    FServerEngine.FFRemoveDependent(Self); {!!.11}

  Inherited Destroy;
End;
{--------}

Procedure TFSBaseEngineMonitor.AddInterest(anObjectClass: TffServerObjectClass);
Begin
  If assigned(FServerEngine) Then
    FServerEngine.AddInterest(Self, anObjectClass);
End;
{--------}

Procedure TFSBaseEngineMonitor.bemSetServerEngine(anEngine: TFSBaseServerEngine);
{Rewritten !!.11}
Begin
  If anEngine <> FServerEngine Then
    Begin
      If assigned(FServerEngine) Then
        FServerEngine.FFRemoveDependent(Self);
      If assigned(anEngine) Then
        anEngine.FFAddDependent(Self);
      FServerEngine := anEngine;
    End;
End;
{Begin !!.11}
{--------}

Procedure TFSBaseEngineMonitor.FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp;
  Const AData: TffWord32);
Begin
  Inherited;
  If (AFrom = FServerEngine) And
    (AOp In [ffn_Destroy, ffn_Remove]) Then
    Begin
      FServerEngine.FFRemoveDependent(Self);
      FServerEngine := Nil;
    End;
End;
{End !!.11}
{--------}

Procedure TFSBaseEngineMonitor.RemoveAllInterest;
Begin
  If assigned(FServerEngine) Then
    FServerEngine.RemoveAllInterest(Self);
End;
{--------}

Procedure TFSBaseEngineMonitor.RemoveInterest(anObjectClass: TffServerObjectClass);
Begin
  If assigned(FServerEngine) Then
    FServerEngine.RemoveInterest(Self, anObjectClass);
End;
{--------}

Procedure TFSBaseEngineMonitor.scInitialize;
Begin
  { Do nothing - avoid abstract error }
End;
{--------}

Procedure TFSBaseEngineMonitor.scPrepareForShutdown;
Begin
  { Do nothing - avoid abstract error }
End;
{--------}

Procedure TFSBaseEngineMonitor.scShutdown;
Begin
  { Do nothing - avoid abstract error }
End;
{--------}

Procedure TFSBaseEngineMonitor.scStartup;
Begin
  { Do nothing - avoid abstract error }
End;
{====================================================================}

{===TFSInterestStructure=============================================}

Constructor TFSInterestStructure.Create;
Begin
  Inherited Create;
  FHashByInterest := TfsHash.Create(0);
  FHashByInterest.OnDisposeData := DisposeList;
  FHashByMonitor := TfsHash.Create(0);
  FHashByMonitor.OnDisposeData := DisposeList;
  FPortal := TfsReadWritePortal.Create;
End;
{--------}

Destructor TFSInterestStructure.Destroy;
Begin
  If assigned(FHashByInterest) Then
    FHashByInterest.Free;

  If assigned(FHashByMonitor) Then
    FHashByMonitor.Free;

  If assigned(FPortal) Then
    FPortal.Free;

  Inherited Destroy;
End;
{--------}

Procedure TFSInterestStructure.AddInterest(Const aMonitor: TFSBaseEngineMonitor;
  Const anObjectClass: TffServerObjectClass);
Var
  MonitorList: TFSNormalList;
  Item: TfsIntListItem;
Begin

  { Has interest already been registered in the class? }
  Item := TfsIntListItem.Create(Longint(aMonitor));
  MonitorList := FHashByInterest.Get(Longint(anObjectClass));
  If assigned(MonitorList) Then
    Begin
      { If so then append the new interest. }
      MonitorList.Insert(Item);
    End
  Else
    Begin
      { Otherwise, create a new entry and add the interest. }
      MonitorList := TFSNormalList.Create;
      MonitorList.Insert(Item);
      FHashByInterest.Add(Longint(anObjectClass), pointer(MonitorList));
    End;

  { Has this monitor registered for any other classes? }
  Item := TfsIntListItem.Create(Longint(anObjectClass));
  MonitorList := FHashByMonitor.Get(Longint(aMonitor));
  If assigned(MonitorList) Then
    Begin
      { If so then add this entry to the hash for monitors. }
      MonitorList.Insert(Item);
    End
  Else
    Begin
      { Otherwise, create a new entry for the monitor. }
      MonitorList := TFSNormalList.Create;
      MonitorList.Insert(Item);
      FHashByMonitor.Add(Longint(aMonitor), pointer(MonitorList));
    End;

End;
{--------}

Function TFSInterestStructure.BeginRead: TFSInterestStructure;
Begin
  FPortal.BeginRead;
  Result := Self;
End;
{--------}

Function TFSInterestStructure.BeginWrite: TFSInterestStructure;
Begin
  FPortal.BeginWrite;
  Result := Self;
End;
{--------}

Procedure TFSInterestStructure.DisposeList(Sender: TfsBaseHashTable; aData: pointer);
Var
  Index: Longint;
  ItemList: TFSNormalList;
Begin
  If assigned(aData) Then
    Begin
      ItemList := TFSNormalList(aData);
      { Free the items in the list. }
      For Index := pred(ItemList.Count) Downto 0 Do
        ItemList[Index].Free;
      ItemList.Free;
    End;
End;
{--------}

Procedure TFSInterestStructure.EndRead;
Begin
  FPortal.EndRead;
End;
{--------}

Procedure TFSInterestStructure.EndWrite;
Begin
  FPortal.EndWrite;
End;
{--------}

Function TFSInterestStructure.GetInterestedMonitors
  (Const anObjectClass: TffServerObjectClass): TFSNormalList;
Var
  anItem: TfsIntListItem;
  Index: Longint;
  MonitorList: TFSNormalList;
Begin

  Result := Nil;

  { Get the list of monitors interested in this object class. }
  MonitorList := FHashByInterest.Get(Longint(anObjectClass));

  { If there are monitors, copy the info over to the result list. }
  If assigned(MonitorList) Then
    Begin
      Result := TFSNormalList.Create;
      For Index := 0 To pred(MonitorList.Count) Do
        Begin
          anItem := TfsIntListItem.Create(TfsIntListItem(MonitorList[Index]).KeyAsInt);
          Result.Insert(anItem);
        End;
    End;

End;
{--------}

Procedure TFSInterestStructure.RemoveAllInterest(Const aMonitor: TFSBaseEngineMonitor);
Var
  Index: Integer;
  ClassList: TFSNormalList;
Begin
  { Do we have any interests registered for this monitor? }
  ClassList := FHashByMonitor.Get(Longint(aMonitor));
  If assigned(ClassList) Then
    Begin
      { For each class in which the monitor registered interest, remove the
        monitor from that class' list in FHashByInterest. }
      For Index := pred(ClassList.Count) Downto 0 Do
        RemoveInterestPrim(aMonitor,
          TffServerObjectClass(TfsIntListItem(ClassList[Index]).KeyAsInt));
      { Now get rid of the entry for this monitor. }
      FHashByMonitor.Remove(Longint(aMonitor));
    End;
End;
{--------}

Procedure TFSInterestStructure.RemoveInterest(Const aMonitor: TFSBaseEngineMonitor;
  Const anObjectClass: TffServerObjectClass);
Var
  ItemList: TFSNormalList;
Begin
  { Remove the monitor's interest for this specific class. }
  RemoveInterestPrim(aMonitor, anObjectClass);

  { Now remove the class from the monitor's list of interests. }
  ItemList := FHashByMonitor.Get(Longint(aMonitor));
  If assigned(ItemList) Then
    ItemList.Delete(Longint(anObjectClass));

  { If our list is empty then get rid of it. }
  If ItemList.Count = 0 Then
    FHashByInterest.Remove(Longint(aMonitor));
End;
{--------}

Procedure TFSInterestStructure.RemoveInterestPrim(Const aMonitor: TFSBaseEngineMonitor;
  Const anObjectClass: TffServerObjectClass);
Var
  MonitorList: TFSNormalList;
Begin
  MonitorList := FHashByInterest.Get(Longint(anObjectClass));
  { If we did find a set of interests for the specified object class,
    scan through it and eliminate registrations for the specified monitor. }
  If assigned(MonitorList) Then
    MonitorList.Delete(aMonitor);

  { If our list is empty then get rid of it. }
  If MonitorList.Count = 0 Then
    FHashByInterest.Remove(Longint(anObjectClass));
End;
{====================================================================}

Constructor TFSBaseEngineExtender.Create(aOwner: TFSBaseEngineMonitor);
Begin
  Inherited Create; {!!.02}
  FParent := aOwner;
  FActions := [];
End;
{====================================================================}

Procedure FinalizeUnit;
Begin
  FFServerEngines.Free;
End;

Procedure InitializeUnit;
Begin
  FFServerEngines := TFSSpecThreadList.Create;
End;

Initialization
  InitializeUnit;

Finalization
  FinalizeUnit;

End.

