{*********************************************************}
{* FlashFiler: Base engine classes                       *}
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

unit fflleng;

interface

uses
  Windows,
  Classes,
  ffhash,
  ffllbase,
  ffllcomp,
  fflldict,
  ffsrbde,
  ffsrlock;

type
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
                     ffeaBeforeRecRead,   ffeaAfterRecRead,
                     ffeaBeforeRecInsert, ffeaAfterRecInsert, ffeaInsertRecFail,
                     ffeaBeforeRecUpdate, ffeaAfterRecUpdate, ffeaUpdateRecFail,
                     ffeaBeforeRecDelete, ffeaAfterRecDelete, ffeaDeleteRecFail,
                     {table actions}
                     ffeaBeforeTabRead,
                     ffeaBeforeTabUpdate,   ffeaTabUpdateFail,
                     ffeaBeforeTabDelete,   ffeaTabDeleteFail,
                     ffeaBeforeTabInsert,   ffeaTabInsertFail,
                     ffeaBeforeTabRestruct, ffeaTabRestructFail,
                     ffeaBeforeTabPack,     ffeaTabPackFail,
                     ffeaBeforeAddInx,      ffeaTabAddInxFail,
                     ffeaBeforeRebuildInx,  ffeaTabRebuildInxFail,
                     ffeaBeforeTableLock,   ffeaAfterTableLock, ffeaTableLockFail,
                     {databaseactions}
                     ffeaBeforeDBRead,
                     ffeaBeforeDBUpdate,     ffeaDBUpdateFail,
                     ffeaBeforeDBDelete,     ffeaDBDeleteFail,
                     ffeaBeforeDBInsert,     ffeaDBInsertFail,
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
                     {misc actions}
                     ffeaNoAction     {used when no fallback action needs to be taken}
                    );

  TffInterestedActions = set of TffEngineAction;

  { Used by a monitor to register interest in a specific type of server object.
    For example, TffSrBaseCursor and TffSrDatabase. }
  TffServerObjectClass = class of TffObject;

  TffBaseEngineMonitor = class;  { forward }
  TffBaseEngineExtender = class; { forward }
  TffInterestStructure = class;  { forward }

  { TffBaseServerEngine is an abstract, virtual class that specifies the
    minimum interface for a local or remote server engine.  The base engine
    provides support for adding and removing monitors. }
  TffBaseServerEngine = class(TffStateComponent)
    protected {private}

      FInterests : TffInterestStructure;
        {-This data structure tracks the interest of various monitors. }

      FMonitors : TffThreadList;
        {-The monitors registered with the engine.  After a monitor registers
          itself with the engine, it identifies the types of server objects
          in which it is interested. }

    protected
      {property access methods}
      function bseGetAutoSaveCfg : Boolean; virtual; abstract;
      function bseGetReadOnly : Boolean; virtual; abstract;
      procedure bseSetAutoSaveCfg(aValue : Boolean); virtual; abstract;{!!.01}
      procedure bseSetReadOnly(aValue : Boolean); virtual; abstract;   {!!.01}
      procedure scSetState(const aState : TffState); override;

      procedure AddInterest(aMonitor : TffBaseEngineMonitor;
                            serverObjectClass : TffServerObjectClass); virtual;
        {-A monitor uses this method to register interest in a specific type of
          server object. }

{Begin !!.06}
      function ProcessRequest(aClientID        : TffClientID;
                              aMsgID           : Longint;
                              aTimeout         : Longint;
                              aRequestData     : Pointer;
                              aRequestDataLen  : Longint;
                              aRequestDataType : TffNetMsgDataType;
                          var aReply           : Pointer;
                          var aReplyLen        : Longint;
                              aReplyType       : TffNetMsgDataType) : TffResult; virtual;
        { Backdoor method for sending a request to a server engine.
          Should only be implemented by remote server engines. }

      function ProcessRequestNoReply(aClientID       : TffClientID;
                                     aMsgID          : Longint;
                                     aTimeout        : Longint;
                                     aRequestData    : Pointer;
                                     aRequestDataLen : Longint ) : TffResult; virtual;
        { Backdoor method for sending a request, no reply expected, to a
          server engine. Should only be implemented by remote server engines. }
{End !!.06}

      procedure RemoveAllInterest(aMonitor : TffBaseEngineMonitor); virtual;
        {-A monitor uses this method to unregister its interest for all classes
          in which it previously expressed interest. }

      procedure RemoveInterest(aMonitor : TffBaseEngineMonitor;
                               serverObjectClass : TffServerObjectClass); virtual;
        {-A monitor uses this method to remove interest in a specific type of
          server object. }

    public
      {creation/destruction}
      constructor Create(aOwner : TComponent); override;

      destructor Destroy; override;

      procedure FFAddDependent(ADependent : TffComponent); override;     {!!.11}
      procedure FFRemoveDependent(ADependent : TffComponent); override;  {!!.11}
      
      function GetInterestedMonitors(const anObjectClass : TffServerObjectClass) : TffList;
      {-Use this method to retrieve a list of engine monitors interested in a
        particular server object class.  If no monitors have registered
        interest then nil is returned.  Otherwise this function returns a
        TffList containing one or more TffIntListItems.  You can convert
        a TffIntListItem into a TffBaseEngineMonitor as follows:

        aMonitor := TffBaseEngineMonitor(TffIntListItem(TffList[index]).KeyAsInt);

        NOTE: The recipient of this functions' result is responsible for
              freeing the TffList.
      }

      procedure GetServerNames(aList    : TStrings;
                               aTimeout : Longint); virtual; abstract;
        { Returns a list of the servers available through the server's
          transport. }

{Begin !!.10}
      { Event logging }
      procedure Log(const aMsg : string); virtual; abstract;
        {-Use this method to log a string to the event log. }

      procedure LogAll(const Msgs : array of string); virtual; abstract;
        {-Use this method to log multiple strings to the event log. }

      procedure LogFmt(const aMsg : string; args : array of const); virtual; abstract;
        {-Use this method to log a formatted string to the event log. }
{End !!.10}

      {transaction tracking}
      function TransactionCommit(const aDatabaseID : TffDatabaseID) : TffResult; virtual; abstract;
      function TransactionRollback(const aDatabaseID : TffDatabaseID) : TffResult; virtual; abstract;
      function TransactionStart(const aDatabaseID : TffDatabaseID;
                                const aFailSafe   : boolean) : TffResult; virtual; abstract;
{Begin !!.10}
      function TransactionStartWith(const aDatabaseID : TffDatabaseID;
                                    const aFailSafe : Boolean;
                                    const aCursorIDs : TffPointerList) : TffResult; virtual; abstract;
{End !!.10}

      {client related stuff}
      function ClientAdd(var aClientID   : TffClientID;
                       const aClientName : TffNetName;
                       const aUserID     : TffName;
                       const timeout     : Longint;
                         var aHash       : TffWord32) : TffResult; virtual; abstract;

{Begin !!.11}
      function ClientAddEx(var aClientID   : TffClientID;
                         const aClientName : TffNetName;
                         const aUserID     : TffName;
                         const timeout     : Longint;
                         const aClientVersion : Longint;
                           var aHash       : TffWord32) : TffResult; virtual; abstract;
        { Same as ClientAdd but client version is supplied via the aClientVersion
          parameter. }
{End !!.11}

      function ClientRemove(aClientID : TffClientID) : TffResult; virtual; abstract;
      function ClientSetTimeout(const aClientID : TffClientID;
                                const aTimeout : Longint) : TffResult; virtual; abstract;

      {client session related stuff}
      function SessionAdd(const aClientID : TffClientID; const timeout : Longint;
                          var aSessionID : TffSessionID) : TffResult; virtual; abstract;
      function SessionCloseInactiveTables(aClientID : TffClientID) : TffResult; virtual; abstract;  {!!.06}
      function SessionCount(aClientID : TffClientID; var aCount : integer) : TffResult; virtual; abstract;
      function SessionGetCurrent(aClientID : TffClientID; var aSessionID : TffSessionID) : TffResult; virtual; abstract;
      function SessionRemove(aClientID : TffClientID; aSessionID : TffSessionID) : TffResult; virtual; abstract;
      function SessionSetCurrent(aClientID : TffClientID; aSessionID : TffSessionID) : TffResult; virtual; abstract;
      function SessionSetTimeout(const aClientID : TffClientID;
                                 const aSessionID : TffSessionID;
                                 const aTimeout : Longint) : TffResult; virtual; abstract;

      {database related stuff}
      function DatabaseAddAlias(const aAlias      : TffName;
                                const aPath       : TffPath;
                                      aCheckSpace : Boolean;            {!!.11}
                                const aClientID   : TffClientID)
                                                  : TffResult; virtual; abstract;
      function DatabaseAliasList(aList     : TList;
                                 aClientID : TffClientID) : TffResult; virtual; abstract;
        {-Return a list of database aliases.  aList will contain zero or more
          instances of PffAliasDescriptor. }

      function RecoveryAliasList(aList     : TList;
                                 aClientID : TffClientID) : TffResult; virtual; abstract;
        {-Return a list of database aliases for use by a journal recovery
          engine. The functionality of this method is identical to
          DatabaseAliasList except that it does not require the server engine
          to be started. }
      function DatabaseChgAliasPath(aAlias      : TffName;
                                    aNewPath    : TffPath;
                                    aCheckSpace : Boolean;              {!!.11}
                                    aClientID   : TffClientID)
                                                : TffResult; virtual; abstract;
      function DatabaseClose(aDatabaseID : TffDatabaseID) : TffResult; virtual; abstract;
      function DatabaseDeleteAlias(aAlias    : TffName;
                                   aClientID : TffClientID) : TffResult; virtual; abstract;
      function DatabaseGetAliasPath(aAlias    : TffName;
                                var aPath     : TffPath;
                                    aClientID : TffClientID) : TffResult; virtual; abstract;
      function DatabaseGetFreeSpace(const aDatabaseID : TffDatabaseID;
                                      var aFreeSpace  : Longint) : TffResult; virtual; abstract;
      function DatabaseModifyAlias(const ClientID    : TffClientID;
                                   const aAlias      : TffName;
                                   const aNewName    : TffName;
                                   const aNewPath    : TffPath;
                                         aCheckSpace : Boolean)         {!!.11}
                                                     : TffResult; virtual; abstract;
      function DatabaseOpen(aClientID  : TffClientID;
                      const aAlias     : TffName;
                      const aOpenMode  : TffOpenMode;
                      const aShareMode : TffShareMode;
                      const aTimeout : Longint;
                        var aDatabaseID  : TffDatabaseID) : TffResult; virtual; abstract;
      function DatabaseOpenNoAlias(aClientID  : TffClientID;
                             const aPath      : TffPath;
                             const aOpenMode  : TffOpenMode;
                             const aShareMode : TffShareMode;
                             const aTimeout   : Longint;
                               var aDatabaseID  : TffDatabaseID) : TffResult; virtual; abstract;
      function DatabaseSetTimeout(const aDatabaseID : TffDatabaseID;
                                  const aTimeout : Longint) : TffResult; virtual; abstract;
      function DatabaseTableExists(aDatabaseID : TffDatabaseID;
                             const aTableName  : TffTableName;
                               var aExists     : Boolean) : TffResult; virtual; abstract;
      function DatabaseTableList(aDatabaseID : TffDatabaseID;
                           const aMask       : TffFileNameExt;
                                 aList       : TList) : TffResult; virtual; abstract;
      function DatabaseTableLockedExclusive(aDatabaseID : TffDatabaseID;
                                      const aTableName  : TffTableName;
                                        var aLocked     : Boolean) : TffResult; virtual; abstract;
        {-Return a list of the tables for the specified database that fit the
          specified filename mask.  aList will contain zero or more instances
          of PffTableDescriptor. }

      {rebuild status related stuff}
      function RebuildGetStatus(aRebuildID : Longint;
                          const aClientID  : TffClientID;
                            var aIsPresent : boolean;
                            var aStatus    : TffRebuildStatus) : TffResult; virtual; abstract;

      {table related stuff}

      function TableAddIndex(const aDatabaseID : TffDatabaseID;
                             const aCursorID   : TffCursorID;
                             const aTableName  : TffTableName;
                             const aIndexDesc  : TffIndexDescriptor) : TffResult; virtual; abstract;
      function TableBuild(aDatabaseID : TffDatabaseID;
                          aOverWrite  : boolean;
                    const aTableName  : TffTableName;
                          aForServer  : boolean;
                          aDictionary : TffDataDictionary) : TffResult; virtual; abstract;
      function TableDelete(aDatabaseID : TffDatabaseID; const aTableName : TffTableName) : TffResult; virtual; abstract;
      function TableDropIndex(aDatabaseID : TffDatabaseID;
                              aCursorID   : TffCursorID;
                        const aTableName  : TffTableName;
                        const aIndexName  : TffDictItemName;
                              aIndexID    : Longint) : TffResult; virtual; abstract;
      function TableEmpty(aDatabaseID : TffDatabaseID;
                          aCursorID   : TffCursorID;
                    const aTableName  : TffTableName) : TffResult; virtual; abstract;
      function TableGetAutoInc(aCursorID   : TffCursorID;
                           var aValue      : TffWord32) : TffResult; virtual; abstract;
      function TableGetDictionary(aDatabaseID : TffDatabaseID;
                            const aTableName  : TffTableName;
                                  aForServer  : boolean;
                                  aStream     : TStream) : TffResult; virtual; abstract;
      function TableGetRecCount(aCursorID : TffCursorID;
                            var aRecCount : Longint) : TffResult; virtual; abstract;
      function TableGetRecCountAsync(aCursorID : TffCursorID;                              {!!.10}
                                 var aRebuildID : Longint) : TffResult; virtual; abstract; {!!.10}
      function TableOpen(const aDatabaseID : TffDatabaseID;
                         const aTableName  : TffTableName;
                         const aForServer  : boolean;
                         const aIndexName  : TffName;
                               aIndexID    : Longint;
                         const aOpenMode   : TffOpenMode;
                               aShareMode  : TffShareMode;
                         const aTimeout    : Longint;
                           var aCursorID   : TffCursorID;
                               aStream     : TStream) : TffResult; virtual; abstract;
      function TablePack(aDatabaseID : TffDatabaseID;
                   const aTableName  : TffTableName;
                     var aRebuildID  : Longint): TffResult; virtual; abstract;
      function TableRebuildIndex(aDatabaseID : TffDatabaseID;
                           const aTableName  : TffTableName;
                           const aIndexName  : TffName;
                                 aIndexID    : Longint;
                             var aRebuildID  : Longint): TffResult; virtual; abstract;
      function TableRename(aDatabaseID : TffDatabaseID; const aOldName, aNewName : TffName) : TffResult; virtual; abstract;
      function TableRestructure(aDatabaseID : TffDatabaseID;
                          const aTableName  : TffTableName;
                                aDictionary : TffDataDictionary;
                                aFieldMap   : TffStringList;
                            var aRebuildID  : Longint): TffResult; virtual; abstract;
      function TableSetAutoInc(aCursorID   : TffCursorID;
                               aValue      : TffWord32) : TffResult; virtual; abstract;
{Begin !!.11}
      function TableVersion(aDatabaseID : TffDatabaseID;
                      const aTableName  : TffTableName;
                        var aVersion : Longint) : TffResult; virtual; abstract;
{End !!.11}

      {table locks via cursor}
      function TableIsLocked(aCursorID : TffCursorID; aLockType : TffLockType;
                         var aIsLocked : boolean) : TffResult; virtual; abstract;
      function TableLockAcquire(aCursorID : TffCursorID; aLockType : TffLockType) : TffResult; virtual; abstract;
      function TableLockRelease(aCursorID : TffCursorID; aAllLocks : boolean) : TffResult; virtual; abstract;

      {cursor stuff}
      function CursorClone(aCursorID : TffCursorID; aOpenMode : TffOpenMode;
                       var aNewCursorID : TffCursorID) : TffResult; virtual; abstract;
      function CursorClose(aCursorID : TffCursorID) : TffResult; virtual; abstract;
      function CursorCompareBookmarks(aCursorID   : TffCursorID;
                                      aBookmark1,
                                      aBookmark2  : PffByteArray;
                                  var aCompResult : Longint) : TffResult; virtual; abstract;
{Begin !!.02}
      function CursorCopyRecords(aSrcCursorID,
                                aDestCursorID : TffCursorID;
                                aCopyBLOBs : Boolean) : TffResult; virtual; abstract;
{End !!.02}
      function CursorDeleteRecords(aCursorID : TffCursorID) : TffResult; virtual; abstract;  {!!.06}
      function CursorGetBookmark(aCursorID : TffCursorID; aBookmark : PffByteArray) : TffResult; virtual; abstract;
      function CursorGetBookmarkSize(aCursorID : TffCursorID;
                                 var aSize     : integer) : TffResult; virtual; abstract;
      {Begin !!.03}
      function CursorListBLOBFreeSpace(aCursorID : TffCursorID;
                                 const aInMemory : Boolean;
                                       aStream : TStream) : TffResult; virtual; abstract;
      {End !!.03}
      function CursorOverrideFilter(aCursorID   : Longint;
                                    aExpression : pCANExpr;
                                    aTimeout    : TffWord32) : TffResult; virtual; abstract;
      function CursorResetRange(aCursorID : TffCursorID) : TffResult; virtual; abstract;
      function CursorRestoreFilter(aCursorID : Longint) : TffResult; virtual; abstract;
      function CursorSetRange(aCursorID : TffCursorID;
                              aDirectKey : boolean;
                              aFieldCount1 : integer;
                              aPartialLen1 : integer;
                              aKeyData1    : PffByteArray;
                              aKeyIncl1    : boolean;
                              aFieldCount2 : integer;
                              aPartialLen2 : integer;
                              aKeyData2    : PffByteArray;
                              aKeyIncl2    : boolean) : TffResult; virtual; abstract;
      function CursorSetTimeout(const aCursorID : TffCursorID;
                                const aTimeout : Longint) : TffResult; virtual; abstract;
      function CursorSetToBegin(aCursorID : TffCursorID) : TffResult; virtual; abstract;
      function CursorSetToBookmark(aCursorID : TffCursorID; aBookmark : PffByteArray) : TffResult; virtual; abstract;
      function CursorSetToCursor(aDestCursorID : TffCursorID; aSrcCursorID : TffCursorID) : TffResult; virtual; abstract;
      function CursorSetToEnd(aCursorID : TffCursorID) : TffResult; virtual; abstract;
      function CursorSetToKey(aCursorID     : TffCursorID;
                              aSearchAction : TffSearchKeyAction;
                              aDirectKey    : boolean;
                              aFieldCount   : integer;
                              aPartialLen   : integer;
                              aKeyData      : PffByteArray) : TffResult; virtual; abstract;
      function CursorSwitchToIndex(aCursorID  : TffCursorID;
                                   aIndexName : TffDictItemName;
                                   aIndexID   : integer;
                                   aPosnOnRec : boolean) : TffResult; virtual; abstract;
      function CursorSetFilter(aCursorID   : TffCursorID;
                               aExpression : pCANExpr;
                               aTimeout    : TffWord32) : TffResult; virtual; abstract;


      {record stuff}
      function RecordDelete(aCursorID : TffCursorID; aData : PffByteArray) : TffResult; virtual; abstract;
      function RecordDeleteBatch(aCursorID : TffCursorID;
                                 aBMCount  : Longint;
                                 aBMLen    : Longint;
                                 aData     : PffByteArray;
                                 aErrors   : PffLongintArray) : TffResult; virtual; abstract;
      function RecordExtractKey(aCursorID : TffCursorID; aData : PffByteArray; aKey : PffByteArray) : TffResult; virtual; abstract;
      function RecordGet(aCursorID : TffCursorID; aLockType : TffLockType; aData : PffByteArray) : TffResult; virtual; abstract;
      function RecordGetBatch(aCursorID : TffCursorID;
                              aRecCount : Longint;
                              aRecLen   : Longint;
                          var aRecRead  : Longint;
                              aData     : PffByteArray;
                          var aError    : TffResult) : TffResult; virtual; abstract;
      function RecordGetForKey(aCursorID   : TffCursorID;
                               aDirectKey  : boolean;
                               aFieldCount : integer;
                               aPartialLen : integer;
                               aKeyData    : PffByteArray;
                               aData       : PffByteArray;
                               aFirstCall  : Boolean) : TffResult; virtual; abstract;
      function RecordGetNext(aCursorID : TffCursorID; aLockType : TffLockType; aData : PffByteArray) : TffResult; virtual; abstract;
      function RecordGetPrior(aCursorID : TffCursorID; aLockType : TffLockType; aData : PffByteArray) : TffResult; virtual; abstract;
      function RecordInsert(aCursorID : TffCursorID; aLockType : TffLockType; aData : PffByteArray) : TffResult; virtual; abstract;
      function RecordInsertBatch(aCursorID : TffCursorID;
                                 aRecCount : Longint;
                                 aRecLen   : Longint;
                                 aData     : PffByteArray;
                                 aErrors   : PffLongintArray) : TffResult; virtual; abstract;
      function RecordIsLocked(aCursorID : TffCursorID; aLockType : TffLockType;
                         var aIsLocked : boolean) : TffResult; virtual; abstract;
      function RecordModify(aCursorID : TffCursorID; aData : PffByteArray; aRelLock : boolean) : TffResult; virtual; abstract;
      function RecordRelLock(aCursorID : TffCursorID; aAllLocks : boolean) : TffResult; virtual; abstract;

      {BLOB stuff}
      function BLOBCreate(aCursorID : TffCursorID;
                      var aBlobNr   : TffInt64) : TffResult; virtual; abstract;
      function BLOBDelete(aCursorID : TffCursorID; aBLOBNr : TffInt64) : TffResult; virtual; abstract;
{Begin !!.03}
      function BLOBListSegments(aCursorID : TffCursorID;
                                aBLOBNr : TffInt64;
                                aStream : TStream) : TffResult; virtual; abstract;
{End !!.03}
      function BLOBRead(aCursorID   : TffCursorID;
                        aBLOBNr     : TffInt64;
                         aOffset    : TffWord32;                       {!!.06}
                         aLen       : TffWord32;                       {!!.06}
                     var aBLOB;
                     var aBytesRead : TffWord32)                       {!!.06}
                                    : TffResult; virtual; abstract;
      function BLOBFree(aCursorID : TffCursorID; aBLOBNr : TffInt64;
                        readOnly : boolean) : TffResult; virtual; abstract;
      function BLOBGetLength(aCursorID : TffCursorID; aBLOBNr : TffInt64;
                         var aLength   : Longint) : TffResult; virtual; abstract;
      function BLOBTruncate(aCursorID : TffCursorID; aBLOBNr : TffInt64;
                             aBLOBLength : Longint) : TffResult; virtual; abstract;
      function BLOBWrite(aCursorID : TffCursorID; aBLOBNr : TffInt64;
                          aOffset : Longint;
                          aLen    : Longint;
                      var aBLOB    ) : TffResult; virtual; abstract;
      function FileBLOBAdd(aCursorID : TffCursorID;
                     const aFileName : TffFullFileName;
                       var aBLOBNr   : TffInt64) : TffResult; virtual; abstract;

      {SQL Stuff }
      function SQLAlloc(aClientID : TffClientID;
                        aDatabaseID : TffDatabaseID;
                        aTimeout : Longint;
                    var aStmtID : TffSqlStmtID) : TffResult; virtual; abstract;
      function SQLExec(aStmtID : TffSqlStmtID;
                       aOpenMode : TffOpenMode;
                   var aCursorID : TffCursorID;
                       aStream : TStream) : TffResult; virtual; abstract;
      function SQLExecDirect(aClientID : TffClientID;
                             aDatabaseID : TffDatabaseID;
                             aQueryText : PChar;
                             aTimeout : Longint;
                             aOpenMode : TffOpenMode;
                         var aCursorID : TffCursorID;
                             aStream : TStream) : TffResult; virtual; abstract;
      function SQLFree(aStmtID : TffSqlStmtID) : TffResult; virtual; abstract;
      function SQLPrepare(aStmtID : TffSqlStmtID;
                          aQueryText : PChar;
                          aStream : TStream) : TffResult; virtual; abstract;
      function SQLSetParams(aStmtID : TffSqlStmtID;
                            aNumParams : word;
                            aParamDescs : Pointer;
                            aDataBuffer : PffByteArray;
                            aDataLen    : integer;
                            aStream   : TStream) : TffResult; virtual; abstract;

      {misc stuff}
      function GetServerDateTime(var aDateTime : TDateTime) : TffResult; virtual; abstract;
{Begin !!.10}
      function GetServerSystemTime(var aSystemTime : TSystemTime)
                                  : TffResult; virtual; abstract;
      function GetServerGUID(var aGUID : TGUID)
                            : TffResult; virtual; abstract;
      function GetServerID(var aUniqueID : TGUID)
                            : TffResult; virtual; abstract;
      function GetServerStatistics(var aStats : TffServerStatistics)
                                  : TffResult; virtual; abstract;
      function GetCommandHandlerStatistics(const aCmdHandlerIdx : Integer;
                                             var aStats : TffCommandHandlerStatistics)
                                          : TffResult; virtual; abstract;
      function GetTransportStatistics(const aCmdHandlerIdx : Integer;
                                      const aTransportIdx  : Integer;
                                        var aStats : TffTransportStatistics)
                                     : TffResult; virtual; abstract;
{End !!.10}
    published

      property IsReadOnly : Boolean
        read bseGetReadOnly
        write bseSetReadOnly                                           {!!.01}
        default False;                                                 {!!.01}

      property NoAutoSaveCfg : Boolean
        read bseGetAutoSaveCfg
        write bseSetAutoSaveCfg                                        {!!.01}
        default False;                                                 {!!.01}
  end;


  { This is the base implementation for an engine monitor.  An engine monitor
    attaches directly to a server engine and registers interest in specific
    types of server objects.  When an object of that type is opened in the
    server, the monitor has the opportunity to express interest in the object.
    The monitor can then supply an extender that will be associated with the
    object and will receive notification of events pertaining to the object. }
  TffBaseEngineMonitor = class(TffStateComponent)
  protected

    FServerEngine : TffBaseServerEngine;

    procedure bemSetServerEngine(anEngine : TffBaseServerEngine); virtual;
      {-Called when a monitor is associated with a server engine.  If the
        monitor is already associated with a server engine then it calls
        OldEngine.RemoveMonitor.  If the monitor is to be associated with
        a new engine then it calls NewEngine.AddMonitor.
        Subclasses should override this method to register interest in specific
        types of server objects. }

    { State methods }
    procedure scInitialize; override;
    procedure scPrepareForShutdown; override;
    procedure scShutdown; override;
    procedure scStartup; override;

  public

    destructor Destroy; override;

    procedure AddInterest(anObjectClass : TffServerObjectClass);
      {-Use this method to have the monitor notify its parent server engine
        of interest in a server object class. }

    procedure FFNotificationEx(const AOp : Byte; AFrom : TffComponent; {!!.11}
                               const AData : TffWord32); override;     {!!.11}
                               
    procedure RemoveAllInterest;
      {-Use this method to have the monitor tells its parent engine to remove
        all interests of the monitor. }

    procedure RemoveInterest(anObjectClass : TffServerObjectClass);
      {-Use this method to have the monitor tells its parent engine to remove
        its interest in the specified object class. }

    function Interested(aServerObject : TffObject) : TffBaseEngineExtender; virtual; abstract;
      { This function is called from the server when an object (e.g., cursor)
        is first opened. If the monitor is interested in receiving events
        for the object, it must create and return an instance of a class that
        can handle events for the object.  Otherwise it should return nil.
        This method is called only for the type of objects in which the monitor
        previously expressed interested.

        When deriving a class from TffBaseEngineMonitor, it is up to the
        extender designer to verify the class of ServerObject is one that is
        expected.
      }

  published

    property ServerEngine : TffBaseServerEngine read FServerEngine
                                                write bemSetServerEngine;
      { Associates an engine monitor with an engine. }
  end;

  { This is the base class for engine extenders.  An engine extender is attached
    to a specific type of server object as governed by an engine monitor.  The
    types of notifications received by the extender depend upon the type of
    object being extended.
    An extender is freed when the server object with which it is associated
    is freed. }
  TffBaseEngineExtender = class(TffObject)
  protected
    FParent  : TffBaseEngineMonitor;
    FActions : TffInterestedActions;
      { Set of actions extender is interested in.}
  public
    constructor Create(aOwner : TffBaseEngineMonitor); virtual;
    function Notify(aServerObject : TffObject;
                    aAction       : TffEngineAction) : TffResult; virtual; abstract;
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

    property InterestedActions : TffInterestedActions
             read FActions;
      { The set of actions in which the extender is interested. }

  end;


  { The following class is used to track a monitor's interest.  It stores
    data in the following manner:

    1. To support retrieval of all monitors interested in a particular
       class of object, it creates a hash table where the hash is based
       on the class' name.  The hash bucket points to a list of monitors.

    2. To support removal of all interest for a monitor, it maintains a
       separate hash table where the hash is based upon the monitor}
  TffInterestStructure = class(TffObject)
  private
    FHashByInterest : TffHash;
      { Given a server object class, this hash table returns a list of the
        monitors interested in that object class. }

    FHashByMonitor : TffHash;
      { Given an engine monitor, this hash table returns a list of the
        object classes in which the monitor has expressed interest.  We use
        this data structure in RemoveAllInterest to speed up our search
        for the monitors in FHashByInterest. }

    FPortal : TffReadWritePortal;
  protected
    procedure DisposeList(Sender : TffBaseHashTable; aData : pointer);
      {-This method is called when a hash table entry is removed. }

    procedure RemoveInterestPrim(const aMonitor : TffBaseEngineMonitor;
                                 const anObjectClass : TffServerObjectClass);
      {-This method removes an interest entry from the FHashByInterest
        hash table. }

  public

    constructor Create;

    destructor Destroy; override;

    procedure AddInterest(const aMonitor : TffBaseEngineMonitor;
                          const anObjectClass : TffServerObjectClass);
      {-Use this method to add a monitor's interest in a certain class. }

    function BeginRead : TffInterestStructure;
      {-Use this method to obtain read access to the data. }

    function BeginWrite : TffInterestStructure;
      {-Use this method to obtain write access to the data. }

    procedure EndRead;
      {-This method must be called after BeginRead once read access is no
        longer needed. }

    procedure EndWrite;
      {-This method must be called after BeginWrite once write access is no
        longer needed. }

    function GetInterestedMonitors(const anObjectClass : TffServerObjectClass) : TffList;
      {-Use this method to retrieve a list of engine monitors interested in a
        particular server object class.  If no monitors have registered
        interest then nil is returned.  Otherwise this function returns a
        TffList containing one or more TffIntListItems.  You can convert
        a TffIntListItem into a TffBaseEngineMonitor as follows:

        aMonitor := TffBaseEngineMonitor(TffIntListItem(TffList[index]).KeyAsInt);

        NOTE: The recipient of this functions' result is responsible for
              freeing the TffList.
      }

    procedure RemoveAllInterest(const aMonitor : TffBaseEngineMonitor);
      {-Use this method to remove interest in all things for which a monitor
        previously registered interest. }

    procedure RemoveInterest(const aMonitor : TffBaseEngineMonitor;
                             const anObjectClass : TffServerObjectClass);
      {-Use this method to remove a monitor's interest in a certain class. }

  end;

var
  FFServerEngines : TffThreadList;

implementation

{===TffBaseServerEngine==============================================}
constructor TffBaseServerEngine.Create(aOwner : TComponent);
var
  aListItem : TffIntListItem;
begin
  inherited Create(aOwner);
  { Add our instance to the global server list }
  aListItem := TffIntListItem.Create(Longint(Self));
  with FFServerEngines.BeginWrite do
    try
      Insert(aListItem);
    finally
      EndWrite;
    end;

  FInterests := TffInterestStructure.Create;
  FMonitors := TffThreadList.Create;
end;
{--------}
destructor TffBaseServerEngine.Destroy;
begin
  FFNotifyDependents(ffn_Destroy);                                     {!!.11}
  FMonitors.Free;                                                      {!!.11}

  if assigned(FInterests) then begin
    FInterests.Free;
    FInterests := nil;
  end;

  { Remove our instance from the global server list }
  with FFServerEngines.BeginWrite do
    try
      Delete(Longint(Self));
    finally
      EndWrite;
    end;

  inherited Destroy;

end;
{--------}
procedure TffBaseServerEngine.scSetState(const aState : TffState);
var
  Idx       : Longint;
  NextState : TffState;
  OldState  : TffState;
  Monitor   : TFFBaseEngineMonitor;
begin

  if aState = scState then exit;

  OldState := scState;

  try
    if Assigned(FMonitors) then
      with FMonitors.BeginRead do
        try
          while scState <> aState do begin
            { Based upon our current state & the target state, get the next state. }
            NextState := ffStateDiagram[scState, aState];

            { Move all monitors to the specified state. }
            for Idx := Pred(Count) downto 0 do begin
              Monitor := TffBaseEngineMonitor(TffIntListItem(Items[Idx]).KeyAsInt);
              Monitor.State := NextState;
            end;
            { Change our state. }
            scState := NextState;
            { Call the appropriate internal method for this state. }
            case NextState of
              ffesInactive, ffesStopped :
                scShutdown;
              ffesInitializing :
                scInitialize;
              ffesStarting :
                scStartup;
              ffesShuttingDown, ffesStopping :
                scPrepareForShutdown;
            end;  { case }
            if assigned(scOnStateChange) then
              scOnStateChange(Self);
          end;  { while }
        finally
          EndRead;
        end
    else
      inherited;
  except
    scState := OldState;
    raise;
  end;
end;
{--------}
procedure TffBaseServerEngine.AddInterest(aMonitor : TffBaseEngineMonitor;
                                          serverObjectClass : TffServerObjectClass);
begin
  with FInterests.BeginWrite do
    try
      AddInterest(aMonitor, serverObjectClass);
    finally
      EndWrite;
    end;
end;
{Begin !!.11}
{--------}
procedure TffBaseServerEngine.FFAddDependent(ADependent : TffComponent);
var
  aListItem : TffIntListItem;
begin
  inherited;
  if ADependent is TffBaseEngineMonitor then begin
    aListItem := TffIntListItem.Create(Longint(ADependent));
    with FMonitors.BeginWrite do
      try
        FMonitors.Insert(aListItem);
      finally
        EndWrite;
      end;
  end;
end;
{--------}
procedure TffBaseServerEngine.FFRemoveDependent(ADependent : TffComponent);
begin
  inherited;
  if ADependent is TffBaseEngineMonitor then
    with FMonitors.BeginWrite do
      try
        Delete(Longint(ADependent));
        RemoveAllInterest(TffBaseEngineMonitor(ADependent));
      finally
        EndWrite;
      end;
end;
{End !!.11}
{--------}
function TffBaseServerEngine.GetInterestedMonitors
           (const anObjectClass : TffServerObjectClass) : TffList;
begin
  with FInterests.BeginRead do
    try
      Result := FInterests.GetInterestedMonitors(anObjectClass);
    finally
      EndRead;
    end;
end;
{Begin !!.06}
{--------}
function TffBaseServerEngine.ProcessRequest(aClientID        : TffClientID;
                                            aMsgID           : Longint;
                                            aTimeout         : Longint;
                                            aRequestData     : Pointer;
                                            aRequestDataLen  : Longint;
                                            aRequestDataType : TffNetMsgDataType;
                                        var aReply           : Pointer;
                                        var aReplyLen        : Longint;
                                            aReplyType       : TffNetMsgDataType) : TffResult;
begin
  { Do nothing. }
  Result := DBIERR_NONE;
end;
{--------}
function TffBaseServerEngine.ProcessRequestNoReply(aClientID       : TffClientID;
                                                   aMsgID          : Longint;
                                                   aTimeout        : Longint;
                                                   aRequestData    : Pointer;
                                                   aRequestDataLen : Longint ) : TffResult;
begin
  { Do nothing. }
  Result := DBIERR_NONE;
end;
{End !!.06}
{--------}
procedure TffBaseServerEngine.RemoveAllInterest(aMonitor : TffBaseEngineMonitor);
begin
  with FInterests.BeginWrite do
    try
      RemoveAllInterest(aMonitor);
    finally
      EndWrite;
    end;
end;
{--------}
procedure TffBaseServerEngine.RemoveInterest(aMonitor : TffBaseEngineMonitor;
                                             serverObjectClass : TffServerObjectClass);
begin
  with FInterests.BeginWrite do
    try
      RemoveInterest(aMonitor, serverObjectClass);
    finally
      EndWrite;
    end;
end;
{====================================================================}

{===TffBaseEngineMonitor=============================================}
destructor TffBaseEngineMonitor.Destroy;
begin
  if assigned(FServerEngine) then
    FServerEngine.FFRemoveDependent(Self);                             {!!.11}

  inherited Destroy;
end;
{--------}
procedure TffBaseEngineMonitor.AddInterest(anObjectClass : TffServerObjectClass);
begin
  if assigned(FServerEngine) then
    FServerEngine.AddInterest(Self, anObjectClass);
end;
{--------}
procedure TffBaseEngineMonitor.bemSetServerEngine(anEngine : TffBaseServerEngine);
{Rewritten !!.11}
begin
  if anEngine <> FServerEngine then begin
    if assigned(FServerEngine) then
      FServerEngine.FFRemoveDependent(Self);
    if assigned(anEngine) then
      anEngine.FFAddDependent(Self);
    FServerEngine := anEngine;
  end;
end;
{Begin !!.11}
{--------}
procedure TffBaseEngineMonitor.FFNotificationEx(const AOp : Byte; AFrom : TffComponent;
                                                const AData : TffWord32);
begin
  inherited;
  if (AFrom = FServerEngine) and
     (AOp in [ffn_Destroy, ffn_Remove]) then begin
    FServerEngine.FFRemoveDependent(Self);
    FServerEngine := nil;
  end;
end;
{End !!.11}
{--------}
procedure TffBaseEngineMonitor.RemoveAllInterest;
begin
  if assigned(FServerEngine) then
    FServerEngine.RemoveAllInterest(Self);
end;
{--------}
procedure TffBaseEngineMonitor.RemoveInterest(anObjectClass : TffServerObjectClass);
begin
  if assigned(FServerEngine) then
    FServerEngine.RemoveInterest(Self, anObjectClass);
end;
{--------}
procedure TffBaseEngineMonitor.scInitialize;
begin
  { Do nothing - avoid abstract error }
end;
{--------}
procedure TffBaseEngineMonitor.scPrepareForShutdown;
begin
  { Do nothing - avoid abstract error }
end;
{--------}
procedure TffBaseEngineMonitor.scShutdown;
begin
  { Do nothing - avoid abstract error }
end;
{--------}
procedure TffBaseEngineMonitor.scStartup;
begin
  { Do nothing - avoid abstract error }
end;
{====================================================================}

{===TffInterestStructure=============================================}
constructor TffInterestStructure.Create;
begin
  inherited Create;
  FHashByInterest := TffHash.Create(0);
  FHashByInterest.OnDisposeData := DisposeList;
  FHashByMonitor := TffHash.Create(0);
  FHashByMonitor.OnDisposeData := DisposeList;
  FPortal := TffReadWritePortal.Create;
end;
{--------}
destructor TffInterestStructure.Destroy;
begin
  if assigned(FHashByInterest) then
    FHashByInterest.Free;

  if assigned(FHashByMonitor) then
    FHashByMonitor.Free;

  if assigned(FPortal) then
    FPortal.Free;

  inherited Destroy;
end;
{--------}
procedure TffInterestStructure.AddInterest(const aMonitor : TffBaseEngineMonitor;
                                           const anObjectClass : TffServerObjectClass);
var
  MonitorList : TffList;
  Item : TffIntListItem;
begin

  { Has interest already been registered in the class? }
  Item := TffIntListItem.Create(Longint(aMonitor));
  MonitorList := FHashByInterest.Get(Longint(anObjectClass));
  if assigned(MonitorList) then begin
    { If so then append the new interest. }
    MonitorList.Insert(Item);
  end else begin
    { Otherwise, create a new entry and add the interest. }
    MonitorList := TffList.Create;
    MonitorList.Insert(Item);
    FHashByInterest.Add(Longint(anObjectClass), pointer(MonitorList));
  end;

  { Has this monitor registered for any other classes? }
  Item := TffIntListItem.Create(Longint(anObjectClass));
  MonitorList := FHashByMonitor.Get(Longint(aMonitor));
  if assigned(MonitorList) then begin
    { If so then add this entry to the hash for monitors. }
    MonitorList.Insert(Item);
  end else begin
    { Otherwise, create a new entry for the monitor. }
    MonitorList := TffList.Create;
    MonitorList.Insert(Item);
    FHashByMonitor.Add(Longint(aMonitor), pointer(MonitorList));
  end;

end;
{--------}
function TffInterestStructure.BeginRead : TffInterestStructure;
begin
  FPortal.BeginRead;
  Result := Self;
end;
{--------}
function TffInterestStructure.BeginWrite : TffInterestStructure;
begin
  FPortal.BeginWrite;
  Result := Self;
end;
{--------}
procedure TffInterestStructure.DisposeList(Sender : TffBaseHashTable; aData : pointer);
var
  Index : Longint;
  ItemList : TffList;
begin
  if assigned(aData) then begin
    ItemList := TffList(aData);
    { Free the items in the list. }
    for Index := pred(ItemList.Count) downto 0 do
      ItemList[Index].Free;
    ItemList.Free;
  end;
end;
{--------}
procedure TffInterestStructure.EndRead;
begin
  FPortal.EndRead;
end;
{--------}
procedure TffInterestStructure.EndWrite;
begin
  FPortal.EndWrite;
end;
{--------}
function TffInterestStructure.GetInterestedMonitors
           (const anObjectClass : TffServerObjectClass) : TffList;
var
  anItem : TffIntListItem;
  Index : Longint;
  MonitorList : TffList;
begin

  Result := nil;

  { Get the list of monitors interested in this object class. }
  MonitorList := FHashByInterest.Get(Longint(anObjectClass));

  { If there are monitors, copy the info over to the result list. }
  if assigned(MonitorList) then begin
    Result := TffList.Create;
    for Index := 0 to pred(MonitorList.Count) do begin
      anItem := TffIntListItem.Create(TffIntListItem(MonitorList[Index]).KeyAsInt);
      Result.Insert(anItem);
    end;
  end;

end;
{--------}
procedure TffInterestStructure.RemoveAllInterest(const aMonitor : TffBaseEngineMonitor);
var
  Index : integer;
  ClassList : TffList;
begin
  { Do we have any interests registered for this monitor? }
  ClassList := FHashByMonitor.Get(Longint(aMonitor));
  if assigned(ClassList) then begin
    { For each class in which the monitor registered interest, remove the
      monitor from that class' list in FHashByInterest. }
    for Index := pred(ClassList.Count) downto 0 do
      RemoveInterestPrim(aMonitor,
                         TffServerObjectClass(TffIntListItem(ClassList[Index]).KeyAsInt));
    { Now get rid of the entry for this monitor. }
    FHashByMonitor.Remove(Longint(aMonitor));
  end;
end;
{--------}
procedure TffInterestStructure.RemoveInterest(const aMonitor : TffBaseEngineMonitor;
                                              const anObjectClass : TffServerObjectClass);
var
  ItemList : TffList;
begin
  { Remove the monitor's interest for this specific class. }
  RemoveInterestPrim(aMonitor, anObjectClass);

  { Now remove the class from the monitor's list of interests. }
  ItemList := FHashByMonitor.Get(Longint(aMonitor));
  if assigned(ItemList) then
    ItemList.Delete(Longint(anObjectClass));

  { If our list is empty then get rid of it. }
  if ItemList.Count = 0 then
    FHashByInterest.Remove(Longint(aMonitor));
end;
{--------}
procedure TffInterestStructure.RemoveInterestPrim(const aMonitor : TffBaseEngineMonitor;
                                                  const anObjectClass : TffServerObjectClass);
var
  MonitorList : TffList;
begin
  MonitorList := FHashByInterest.Get(Longint(anObjectClass));
  { If we did find a set of interests for the specified object class,
    scan through it and eliminate registrations for the specified monitor. }
  if assigned(MonitorList) then
    MonitorList.Delete(aMonitor);

  { If our list is empty then get rid of it. }
  if MonitorList.Count = 0 then
    FHashByInterest.Remove(Longint(anObjectClass));
end;
{====================================================================}

constructor TffBaseEngineExtender.Create(aOwner : TffBaseEngineMonitor);
begin
  inherited Create;                                                    {!!.02}
  FParent := aOwner;
  FActions := [];
end;
{====================================================================}

procedure FinalizeUnit;
begin
  FFServerEngines.Free;
end;

procedure InitializeUnit;
begin
  FFServerEngines := TffThreadList.Create;
end;

initialization
  InitializeUnit;

finalization
  FinalizeUnit;

end.
