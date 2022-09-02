{*********************************************************}
{* FSSQL: Remote Server Engine Classes                   *}
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

Unit fsserverremoteclass;

Interface
Uses
  Windows,
  Dialogs,
  Classes,
  SysUtils,
  fsllbase,
  fssrbase,
  fslldict,
  fsdtmsgq,
  fsllcomm,
  fsllcomp,
  fslleng,
  fsllexcp,
  fsllreq,
  fsnetmsg,
  fssrbde,
  fssrintm,
  fssrlock,
  fsdbbase;

Type
  {forward declarations}
  TFSRemoteServer = Class;
  {The TFSRemoteServer implements the TFsBaseServerEngine abstract
   methods. It's method calls will initiate the process that will format
   a message request to be sent to a remote server via a transport.
   The TFSRemoteServer methods sometimes pass buffers without passing
   the buffer length.  However, the length must be known in order for the
   message to be sent.

   It is also possible for the TFSRemoteServer to be accessed by
   multiple threads.  We want to make sure that messages for one thread don't
   wind up with another thread.

   To handle cases such as these, the TFSRemoteServer needs to track
   information specific to a cursor and client, respectively. To this
   end we have created proxy classes to hold the information.  For
   example, a TFSProxyCursor holds information specific to an open cursor.
   A TFSProxyClient holds information specific to an open client.

   The TFSRemoteServer creates an instance of a proxy class when its
   equivalent server-side object is opened.  Instead of returning the
   server-side object's ID to the object(s) using the remote engine, the
   remote engine returns the pointers to its proxy objects.  This scheme
   allows TFSRemoteServer to derive a server-side ID from its proxy
   object and allows it to maintain information required for its operation.

   In general, all calls to remote server engine wind up calling a method on
   a TffProxy class which in turn formats a request and sends it through
   TFSProxyClient.}

  TFSProxyClientList = Class;
  TFSProxySession = Class;
  TFSProxySessionList = Class;
  TFSProxyDatabase = Class;
  TFSProxyDatabaseList = Class;
  TFSProxyCursor = Class;
  TFSProxyCursorList = Class;
  TFSProxySQLStmt = Class;
  TFSProxySQLStmtList = Class;
  {-End forward declarations}

  {Creating/destroying and ownership issues.
    The TFSProxyClient object will be created/destroyed and owned by it's
     parent, a TFSRemoteServer. The TFSRemoteServer will be
     responsible for keeping a list of the afore mentioned object.

   The TFSProxySession object, and the TFSProxyDatabase object will be
     created/destroyed and owned by it's parent, a TFSProxyClient. The
     TFSProxyClient will be responsible for keeping a list of all instances
     of the afore mentioned objects.

   The TFSProxyCursor object will be created/destroyed and owned by
     it's parent, a TFSProxyDatabase. The TFSProxyDatabase will be responsible
     for keeping a list of all instances of the afore mentioned object.

   The constructor for each of the client classes is resposible for
     contacting the server, and retrieving an ID from the server. The parent
     class will not manipulate the ServerID directly.

   The destructor for each of the client classes is resposible for
     tellint the server to release it's associated object.

   If a proxy class "owns" any other classes then any owned classes must be
     destroyed first.

   In the end there should be no manipulation of ServerID's except in the
     objects constructor. And no way to free a parent class without first
     freeing dependent classes. }

  {TFSProxyClient
    { The proxy client controls interaction between the remote server engine
    and the transport. This class contains a message queue associated with
    a specific client. All requests for data must go through this class'
    ProcessRequest method. Instances where a reply from the server isn't
    necessary can use the ProcessRequestNoReply method. }

  TFSProxyClient = Class(TFSSpecObject)
  Protected
    pcSrClientID: TffClientID;
    {An ID pointing to the associated TFFSrClient class on the server}
    pcRights: TffUserRights;
    pcSecurityEnabled: boolean;
    pcMsgQueue: TffDataMessageQueue;
    {The message queue used to store replies to this client. }

    pcCallbackMethod: TfsReplyCallback;
    {A Method pointer that will be passed to the transport when a
     reply is requested.}

    pcCurrentSession: TFSProxySession;
    {The current session as set by the SessionSetCurrent method}

    pcDatabases: TFSProxyDatabaseList;
    {The databases that are managed by the client}

    pcForceClosed: Boolean;

    pcTransport: TFSBaseTransport;
    {A reference to the RemoteServerEngine's transport. Added here for
     purposes of speed, and readability.}

    pcSessions: TFSProxySessionList;
    {The sessions that are registered with the client. }

    pcTimeout: Longint;
    {The current timeout setting for the TFFBaseConnection Class. The
     TFFBaseConnection class is resposible for updating this object when
     it's published timeout value is changed.}

  Public
    Constructor Create(aTransport: TFSBaseTransport;
      aUserName: TFFName;
      aPasswordHash: Longint;
      aTimeOut: Longint;
      aRights: TffUserRights;
      aSecurityEnabled: boolean);
    Destructor Destroy; Override;

    Function IsReadOnly: Boolean;

    Function ProcessRequest(aMsgID: Longint;
      aTimeout: Longint;
      aRequestData: Pointer;
      aRequestDataLen: Longint;
      aRequestDataType: TffNetMsgDataType;
      Var aReply: Pointer;
      Var aReplyLen: Longint;
      aReplyType: TffNetMsgDataType): TffResult;
    { Use the ProxessRequest method to submit a request that is routed to the
      transport.  This method does the following:

      1. Calls TFSBaseTransport.Request with transportID = 0 and cookie
         equal to Pointer(Self).  At this point, the calling thread is
         blocked until a reply is received from the server or a timeout
         occurs.
      2. When the calling thread returns to this method, the reply has
         been received and placed in the message queue by the
         ProxyClientCallback procedure.
      3. Verify the message is the type that we expected.
      4. Put the message into the MessageQueue and exit.}

    Function ProcessRequestNoReply(aMsgID: Longint;
      aTimeout: Longint;
      aRequestData: Pointer;
      aRequestDataLen: Longint): TffResult;
    { Use the ProxessRequestNoReply method to submit a request that is
      routed to the transport. This method does the following:

      1. Calls TFSBaseTransport.Post with transportID = 0 and reply mode
         to waituntilsent.  At this point, the calling thread is
         blocked until the request has been sent to the server.}
    Function DatabaseClose(aDatabase: TFSProxyDatabase): TffResult;
    Function DatabaseOpen(Const aAlias: TffName;
      aOpenMode: TffOpenMode;
      aShareMode: TffShareMode;
      aTimeout: Longint;
      aTransIsolation: TfsTransIsolation;
      aTransLocking: TfsDataBaseRecLocking;
      Var aDatabaseID: TffDatabaseID): TffResult;
    {Add a database to the pcDatabases list. The client will take
     care of creating}

    Function DatabaseOpenNoAlias(Const aPath: TffPath;
      aOpenMode: TffOpenMode;
      aShareMode: TffShareMode;
      aTimeout: Longint;
      aTransIsolation: TfsTransIsolation;
      aTransLocking: TfsDataBaseRecLocking;
      Var aDatabaseID: TffDatabaseID
      ): TffResult;
    Function GetRebuildStatus(Const aRebuildID: Longint;
      Var aIsPresent: Boolean;
      Var aStatus: TffRebuildStatus): TffResult;
    Function SetTimeout(Const aTimeout: Longint): TffResult;
    Function SessionAdd(Var aSessionID: TffSessionID;
      Const aTimeout: Longint; aData: Pointer; aDataLength: Longint = 0): TffResult;
    {Add a session to the pcSessions list. The client will take
    care of creating the TFSProxySession object, whose ID will
    be returned via aSessionID.}

    Function SessionCloseInactiveTables: TffResult; {!!.06}
    { Close the inactive tables on the server. }

    Function SessionCount: Longint;
    {Retrieve the number of sessions the client is managing.}

    Function SessionGetCurrent: TFSProxySession;
    {Retrieve the current session}

    Function SessionRemove(aSession: TFSProxySession): TffResult;
    {Remove the session from the list. The client will take destroy
     the session, and remove it from the list}

    Function SessionSetCurrent(aSession: TFSProxySession): TffResult;
    {Set the current session}

    Function DatabaseAddAlias(Const aAlias: TffName;
      Const aPath: TffPath;
      aCheckSpace: Boolean) {!!.11}
    : TffResult;
    Function DatabaseAliasList(aList: TList): TffResult;
    Function DatabaseChgAliasPath(Const aAlias: TffName;
      Const aNewPath: TffPath;
      aCheckSpace: Boolean) {!!.11}
    : TffResult;
    Function DatabaseDeleteAlias(Const aAlias: TffName): TffResult;
    Function DatabaseGetAliasPath(Const aAlias: TffName;
      Var aPath: TffPath): TffResult;
    Function DatabaseModifyAlias(Const aAlias: TffName;
      Const aNewName: TffName;
      Const aNewPath: TffPath;
      aCheckSpace: Boolean) {!!.11}
    : TffResult;

    Function GetServerDateTime(Var aDateTime: TDateTime): TffResult;
    {begin !!.10}
    Function GetServerSystemTime(Var aSystemTime: TSystemTime)
      : TffResult;
    Function GetServerGUID(Var aGUID: TGUID): TffResult;
    Function GetServerID(Var aUniqueID: TGUID): TffResult;
    Function GetServerStatistics(Var Stats: TfsServerStatistics)
      : TffResult;
    Function GetCommandHandlerStatistics(Const CmdHandlerIdx: Integer;
      Var Stats: TfsCommandHandlerStatistics)
      : TffResult;
    Function GetTransportStatistics(Const CmdHandlerIdx: Integer;
      Const Transportidx: Integer;
      Var Stats: TfsTransportStatistics)
      : TffResult;
    {end !!.10}

{Begin !!.01}
    Function RemoteRestart: TffResult;
    { Tell the remote server to restart. }

    Function RemoteStart: TffResult;
    { Tell the remote server to startup. }

    Function RemoteStop: TffResult;
    { Tell the remote server to stop. }
{End !!.01}

  {ReadOnly properties for the protected fields}
    Property CurrentSession: TFSProxySession
      Read SessionGetCurrent;
    Property Databases: TFSProxyDatabaseList
      Read pcDatabases;
    Property ForceClosed: Boolean
      Read pcForceClosed
      Write pcForceClosed;
    Property MsgQueue: TFFDataMessageQueue
      Read pcMsgQueue;
    Property Sessions: TFSProxySessionList
      Read pcSessions;
    Property SrClientID: TffClientID
      Read pcSrClientID;
    Property SrRights: TffUserRights Read pcRights;
    // property SrAccessRight: TffAccessRight read  pcAccessRight;
    Property Transport: TFSBaseTransport
      Read pcTransport;
    Property Timeout: Longint
      Read pcTimeout;
  End;

  {List containing a reference for every ProxyClient owned by
   a TFSRemoteServer component.}
  TFSProxyClientList = Class(TFSSpecThreadList);

  {The TFSProxySession is used primarily to keep track of the
   the current Timeout setting, and the Server CursorID.
   Unlike the TFFSession, the ProxySession does not manage a
   set of Databases. TFFProxyDatabases, instead, are managed by
   the ProxyClient class}
  TFSProxySession = Class(TFSSpecObject)
  Protected
    psSrSessionID: TFFSessionID;
    {An ID pointing to the TFFSrSession object on the remote server}

    psClient: TFSProxyClient;
    {A reference to the client who owns this object}

    psTimeout: Longint;
    {Local storage for the current Session timeout setting. The TFFSession
     object is resposible for keeping this value up to date.}

  Public
    Constructor Create(aClient: TFSProxyClient; aTimeout: Longint; aData: Pointer; aDataLength: Longint = 0);

    Destructor Destroy; Override;

    Function SetTimeout(aTimeout: Longint): TffResult;

    {ReadOnly properties for the protected fields}
    Property SrSessionID: TFFSessionID
      Read psSrSessionID;
    Property Client: TFSProxyClient
      Read psClient;
    Property Timeout: Longint
      Read psTimeout;
  End;

  {List containing a reference for every ProxySesion owned by
   a TFSProxyClient object.}
  TFSProxySessionList = Class(TFSSpecThreadList);

  {The TFSProxyDatabase is responsible for basic Table maintenance. It also
   keeps track of the the current Timeout setting, and the Server CursorID.
   TFSProxyDatabase maintains a list of TFSProxyCursor objects.}
  TFSProxyDatabase = Class(TFSSpecObject)
  Protected
    pdSrDatabaseID: TffDatabaseID;
    {An ID pointing to the TffSrDatabase object on the remote server}

    pdClient: TFSProxyClient;
    {A reference to the client who owns this object}

    pdInTrans: Boolean;
    {Have we instantiated a tranaction? }

    pdStmts: TFSProxySQLStmtList;
    {The SQL statements managed by this database}

    pdTables: TFSProxyCursorList;
    {The tables that are managed by the database}

    pdTimeout: Longint;
    pdTransIsolation: TfsTransIsolation;
    pdTransLocking: TfsDataBaseRecLocking;
    {Local storage for the current Database timeout setting. The TFFDatabase
     object is resposible for keeping this value up to date.}
  Public
    Constructor Create(aClient: TFSProxyClient;
      aLocation: String;
      aOpenMode: TffOpenMode;
      aShareMode: TffShareMode;
      aTimeout: Longint;
      aTransIsolation: TfsTransIsolation;
      aTransLocking: TfsDataBaseRecLocking;
      aIsAlias: Boolean);
    Destructor Destroy; Override;
    Function GetDBFreeSpace(Var aFreeSpace: Int64): TffResult;
    Function QueryOpen(aCursorID: TffCursorID;
      aOpenMode: TffOpenMode;
      aShareMode: TffShareMode;
      aTimeout: Longint;
      aStream: TStream;
      Var aFinalCursorID: TffCursorID): TffResult;
    Function SetTimeout(Const aTimeout: Longint): TffResult;
    Function SQLAlloc(Const aTimeout: Longint;
      Var aStmtID: TffSqlStmtID): TffResult;
    Function SQLExecDirect(aQueryText: PChar;
      aOpenMode: TffOpenMode;
      aTimeout: Longint;
      Var aCursorID: TffCursorID;
      aStream: TStream): TffResult;
    Function TableExists(Const aTableName: TfsTableName;
      Var aExists: Boolean): TffResult;
    Function TableList(Const aMask: TffFileNameExt;
      aList: TList): TffResult;
    Function TableLockedExclusive(Const aTableName: TfsTableName;
      Var aLocked: Boolean): TffResult;
    Function TableAddIndex(Const aCursorID: TffCursorID;
      Const aTableName: TfsTableName;
      Const aIndexDesc: TffIndexDescriptor): TffResult;
    Function TableBuild(aOverWrite: Boolean;
      Const aTableName: TfsTableName;
      aForServer: Boolean;
      aDictionary: TFSInfoDict): TffResult;
    Function TableDelete(Const aTableName: TfsTableName): TffResult;
    Function TableDropIndex(aCursorID: TffCursorID;
      Const aTableName: TfsTableName;
      Const aIndexName: TffDictItemName;
      aIndexID: Longint): TffResult;
    Function TableEmpty(aCursorID: TffCursorID;
      Const aTableName: TfsTableName): TffResult;
    Function TableGetDictionary(Const aTableName: TfsTableName;
      aForServer: Boolean;
      aStream: TStream): TffResult;
    Function TableClose(aCursor: TFSProxyCursor): TffResult;
    Function TableOpen(Const aTableName: TfsTableName;
      aForServer: Boolean;
      aIndexName: TffName;
      aIndexID: Longint;
      aOpenMode: TffOpenMode;
      aShareMode: TffShareMode;
      aTimeout: Longint;
      Var aCursorID: TffCursorID;
      aStream: TStream): TffResult;
    Function TablePack(Const aTableName: TfsTableName;
      Var aRebuildID: Longint; UndeleteRecords: boolean; OnlyDeleted: boolean): TffResult;
    Function TableRebuildIndex(Const aTableName: TfsTableName;
      Const aIndexName: TffName;
      aIndexID: Longint;
      Var aRebuildID: Longint): TffResult;
    Function TableRename(Const aOldName: TffName;
      Const aNewName: TffName): TffResult;
    Function TableRestructure(Const aTableName: TfsTableName;
      aDictionary: TFSInfoDict;
      aFieldMap: TFSSpecStringList;
      Var aRebuildID: Longint;
      aRangeError: boolean): TffResult;
    Function TransactionStart(aFailSafe: Boolean): TffResult;
    {Begin !!.10}
    Function TransactionStartWith(Const aFailSafe: Boolean;
      Const aCursorIDs: TfsPointerList): TffResult;
    {End !!.10}
    Function InTransaction(Var aTransLevel: Longint): TffResult;
    Function TransactionCorrupted: TffResult;
    Function TransactionCommit(aRemoveFile: Boolean = False): TffResult;
    Function TransactionRollback: TffResult;

    Property Client: TFSProxyClient
      Read pdClient;
    Property InTrans: Boolean
      Read pdInTrans;
    Property SrDatabaseID: TFFDatabaseID
      Read pdSrDatabaseID;
    Property Tables: TFSProxyCursorList
      Read pdTables;
    Property Timeout: Longint
      Read pdTimeout;
  End;

  TFSProxyDatabaseList = Class(TFSSpecThreadList);

  TFSProxyCursor = Class(TFSSpecObject)
  Protected
    fsExtraRecInfo: pfsExtraRecInfo;
    prSrCursorID: TffCursorID;
    prClient: TFSProxyClient;
    prForServer: Boolean;
    prShareMode: TffShareMode;
    prTableName: TfsTableName;
    prTimeout: Longint;
    prDatabase: TFSProxyDatabase;

    {State Variables}
    prDictionary: TFSInfoDict;
    prIndexID: Longint;
    prIndexName: String;
    prIsSQLCursor: boolean;
    prPhyRecSize: Longint;
  Protected
    Function prGetBookmarkSize: Longint;
  Public
    Constructor Create(aDatabase: TFSProxyDatabase;
      aCursorID: TffCursorID; {used by CursorClone, otherwise set to 0}
      aTableName: String;
      aForServer: Boolean;
      aIndexName: String;
      aIndexID: Longint;
      aOpenMode: TffOpenMode;
      aShareMode: TffShareMode;
      aTimeout: Longint;
      aStream: TStream);

    Constructor CreateSQL(aDatabase: TFSProxyDatabase;
      aCursorID: TffCursorID;
      aOpenMode: TffOpenMode;
      aShareMode: TffShareMode;
      aTimeout: Longint;
      aStream: TStream);
    { This constructor is used to construct a proxy cursor for an executed
      SQL statement. }

    Destructor Destroy; Override;
    Function BlobCreate(Var aBlobNr: TFFInt64): TFFResult;
    Function BLOBDelete(aBlobNr: TFFInt64): TffResult;
    Function BLOBFree(aBlobNr: TffInt64;
      aReadOnly: Boolean): TFFResult;
    Function BLOBGetLength(aBlobNr: TffInt64;
      Var aLength: Longint): TffResult;
    {Begin !!.03}
    Function BLOBListSegments(aBLOBNr: TffInt64;
      aStream: TStream): TffResult;
    {End !!.03}
    Function BLOBRead(aFieldNo: TffWord32;
      aBlobNr: TffInt64;
      aOffset: TffWord32; {!!.06}
      aLen: TffWord32; {!!.06}
      Var aBLOB;
      Var aBytesRead: TffWord32) {!!.06}
    : TffResult;
    Function BLOBTruncate(aBlobNr: TffInt64;
      aBLOBLength: Longint): TffResult;
    Function BLOBWrite(aFieldNo: TffWord32;
      aBlobNr: TffInt64;
      aOffset: Longint;
      aLen: Longint;
      Var aBLOB): TFFResult;
    Function CursorClone(aOpenMode: TFFOpenMode;
      Var aNewCursorID: TFFCursorID): TFFResult;
    Function CompareBookmarks(aBookmark1: PffByteArray;
      aBookmark2: PffByteArray;
      Var aCompResult: Longint): TffResult;
    Function CopyRecords(aSrcCursor: TFSProxyCursor; {!!.02}
      aCopyBLOBs: Boolean; CountPerTrans: Longint): TffResult; {!!.02}
    Function DeleteRecords(CountPerTrans: Longint): TffResult; {1.052}
    Function GetBookmark(aBookmark: PffByteArray): TffResult;
    Function GetBookmarkSize(Var aSize: Longint): TffResult;
    {Begin !!.03}
    Function ListBLOBFreeSpace(Const aInMemory: Boolean;
      aStream: TStream): TffResult;
    {End !!.03}
    Function OverrideFilter(aExpression: pCANExpr;
      aTimeout: TffWord32): TffResult;
    Function ResetRange: TffResult;
    Function RestoreFilter: TffResult;
    Function SetFilter(aExpression: pCANExpr;
      aTimeout: TffWord32): TffResult;
    Function SetRange(aDirectKey: Boolean;
      aFieldCount1: Longint;
      aPartialLen1: Longint;
      aKeyData1: PffByteArray;
      aKeyIncl1: Boolean;
      aFieldCount2: Longint;
      aPartialLen2: Longint;
      aKeyData2: PffByteArray;
      aKeyIncl2: Boolean): TffResult;
    Function SetTimeout(aTimeout: Longint): TffResult;
    Function SetToBegin: TffResult;
    Function SetToBookmark(aBookmark: PffByteArray): TffResult;
    Function SetToCursor(aSourceCursor: TFSProxyCursor): TffResult;
    Function SetToEnd: TffResult;
    Function SetToKey(aSearchAction: TffSearchKeyAction;
      aDirectKey: Boolean;
      aFieldCount: Longint;
      aPartialLen: Longint;
      aKeyData: PffByteArray): TffResult;
    Function SwitchToIndex(aIndexName: TffDictItemName;
      aIndexID: Longint;
      aPosnOnRec: Boolean): TffResult;
    Function FileBLOBAdd(Const aFileName: TffFullFileName;
      Var aBlobNr: TffInt64): TffResult;
    Function RecordDelete(aData: PffByteArray): TffResult;
    Function RecordDeleteBatch(aBMCount: Longint;
      aBMLen: Longint;
      aData: PffByteArray;
      aErrors: PffLongintArray): TffResult;
    Function RecordExtractKey(aData: PffByteArray;
      aKey: PffByteArray): TffResult;
    Function RecordGet(aLockType: TffLockType; aUserLockType: TfsUserRecLocking;
      aData: PffByteArray; Var aFlag: Byte; Var aRefNr: TffInt64;Const aUser: Boolean): TffResult;
    Function RecordGetBatch(aRecCount: Longint;
      aRecLen: Longint;
      Var aRecRead: Longint;
      aData: PffByteArray;
      Var aError: TffResult): TffResult;
    Function RecordGetForKey(aDirectKey: Boolean;
      aFieldCount: Longint;
      aPartialLen: Longint;
      aKeyData: PffByteArray;
      aData: PffByteArray;
      aFirstCall: Boolean): TffResult;
    Function GetSetPosition(aValue: Longint; aData: PffByteArray; aLockType: TffLockType;
      Var aFlag: Byte; Var aRecNo: Longword;
      Var aRefNr: TffInt64;
      aInfoGetSetPosition: TInfoGetSetPosition;
      aSet: Boolean): TffResult;
    Function RecordGetNext(aLockType: TffLockType;
      aData: PffByteArray; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
    Function RecordGetPrior(aLockType: TffLockType;
      aData: PffByteArray; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
    Function RecordInsert(aLockType: TffLockType;
      aData: PffByteArray; aUndelete: Boolean; Var aRefNr: TffInt64): TffResult;
    Function RecordInsertBatch(aRecCount: Longint;
      aRecLen: Longint;
      aData: PffByteArray;
      aErrors: PffLongintArray): TffResult;
    Function RecordIsLocked(aLockType: TffLockType;
      Var aIsLocked: boolean): TffResult;
    Function RecordModify(aData: PffByteArray;
      aRelLock: Boolean; aUserLockType: TfsUserRecLocking; aFlag: Byte; aSet, Use: Boolean): TffResult;
    Function RecordRelLock(aAllLocks: Boolean): TffResult;
    Function TableGetAutoInc(Var aValue: Int64; Var aStep: Longint): TffResult;
    Function TableGetMaxRecords(Var aValue: Longword): TffResult;
    Function TableGetTableFlags(Var aValue: Word): TffResult;
    Function TableGetTablePassword(Var aValue: Longword): TffResult;
    Function TableGetTablePasswordRest(Var aValue: Longword): TffResult;
    Function TableGetTableDBID(Var aValue: Longword): TffResult;
    Function TableGetRecCount(Var aRecCount: Longword): TffResult;
    Function TableGetRecCountAsync(Var aTaskID: Longint): TffResult; {!!.07}
    Function TableIsLocked(aLockType: TffLockType;
      Var aIsLocked: Boolean): TffResult;
    Function TableLockAcquire(aLockType: TffLockType): TffResult;
    Function TableLockRelease(aAllLocks: Boolean): TffResult;
    Function TableSetAutoInc(aValue: Int64; aStep: Longint): TffResult;
    Function TableSetMaxRecords(aValue: Longint): TffResult;
    Function TableSetTableFlags(aValue: Word): TffResult;
    Function TableSetTablePassword(aValue: Longword): TffResult;
    Function TableSetTablePasswordRest(aValue: Longword): TffResult;
    Function TableSetTableDBID(aValue: Longword): TffResult;
    Property Client: TFSProxyClient
      Read prClient;
    Property SrCursorID: TffCursorID
      Read prSrCursorID;
    Property Timeout: Longint
      Read prTimeout;
    Property BookmarkSize: Longint
      Read prGetBookmarkSize;
    Property Database: TFSProxyDatabase
      Read prDatabase;
    Property Dictionary: TFSInfoDict
      Read prDictionary;
    Property IndexID: Longint
      Read prIndexID;
    Property PhysicalRecordSize: Longint
      Read prPhyRecSize;

  End;

  TFSProxyCursorList = Class(TFSSpecThreadList);

  TFSProxySQLStmt = Class(TFSSpecObject)
  Protected {private}

    psClient: TFSProxyClient;
    { The proxy client through which requests are routed. }

    psDatabase: TFSProxyDatabase;
    { The proxy database with which the SQL statement is associated. }

    psSrStmtID: TffSqlStmtID;
    { The actual statement ID. }

    psTimeout: Longint;
    { The SQL statement's timeout (in milliseconds). }

  Public
    {creation/destruction}
    Constructor Create(aDatabase: TFSProxyDatabase; Const aTimeout: Longint);
    Destructor Destroy; Override;

    Function Exec(aOpenMode: TffOpenMode;
      Var aCursorID: TffCursorID;
      aStream: TStream): TffResult;

    Function Prepare(aQueryText: PChar; aStream: TStream): TffResult;

    Function SetParams(aNumParams: Word;
      aParamDescs: Pointer;
      aDataBuffer: PffByteArray;
      aDataLen: Longint;
      aStream: TStream): TffResult;

    Property Database: TFSProxyDatabase Read psDatabase;

    Property SrStmtID: TffSqlStmtID Read psSrStmtID;
    { The statement ID returned by the server engine. }

  End;

  TFSProxySQLStmtList = Class(TFSSpecThreadList);

  TFSRemoteServer = Class(TfsIntermediateServerEngine)
  Private
  Protected {private}
    rsClientList: TFSProxyClientList;
    rsTimeout: TffWord32;
    rsTransport: TFSBaseTransport;
    {Begin !!.06}
    Function ProcessRequest(aClientID: TffClientID;
      aMsgID: Longint;
      aTimeout: Longint;
      aRequestData: Pointer;
      aRequestDataLen: Longint;
      aRequestDataType: TffNetMsgDataType;
      Var aReply: Pointer;
      Var aReplyLen: Longint;
      aReplyType: TffNetMsgDataType): TffResult; Override;
    { Backdoor method for sending a request to a server engine.
      Should only be implemented by remote server engines. }

    Function ProcessRequestNoReply(aClientID: TffClientID;
      aMsgID: Longint;
      aTimeout: Longint;
      aRequestData: Pointer;
      aRequestDataLen: Longint): TffResult; Override;
    { Backdoor method for sending a request, no reply expected, to a
      server engine. Should only be implemented by remote server engines. }
{End !!.06}
    Procedure rsSetTransport(Const Value: TFSBaseTransport);
    //  protected                                                         {!!.01 - Start - Made public}
    //    {validation and checking}
    //    function CheckClientIDAndGet(aClientID : TffClientID;
    //                             var aClient   : TFSProxyClient) : TffResult;
    //    function CheckSessionIDAndGet(aClientID  : TffClientID;
    //                                  aSessionID : TffSessionID;
    //                              var aClient    : TFSProxyClient;
    //                              var aSession   : TFSProxySession) : TffResult;
    //    function CheckDatabaseIDAndGet(aDatabaseID : TffDatabaseID;
    //                               var aDatabase   : TFSProxyDatabase) : TffResult;
    //      {-Find the database specified by aDatabaseID. }
    //
    //    function CheckCursorIDAndGet(aCursorID : TffCursorID;
    //                             var aCursor   : TFSProxyCursor) : TffResult;
    //      {-Find the cursor specified by aCursorID. }
    //
    //    function CheckStmtIDAndGet(aStmtID : TffSqlStmtID;
    //                          var  aStmt   : TFSProxySQLStmt) : TffResult;
    //      {-Find the statement specified by aStmtID. }                  {!!.01 - End}

  Protected
    {State methods}
    Procedure scInitialize; Override;
    Procedure scPrepareForShutdown; Override;
    Procedure scShutdown; Override;
    Procedure scStartup; Override;
    Function bseGetAutoSaveCfg: Boolean; Override;
    Function bseGetReadOnly: Boolean; Override;
    Procedure bseSetAutoSaveCfg(aValue: Boolean); Override; {!!.01}
    Procedure bseSetReadOnly(aValue: Boolean); Override; {!!.01}
  Public
    {Begin !!.07}
          { Event logging }
    Procedure Log(Const aMsg: String); Override;
    {-Use this method to log a string to the event log. }

    Procedure LogAll(Const Msgs: Array Of String); Override;
    {-Use this method to log multiple strings to the event log. }

    Procedure LogFmt(Const aMsg: String; args: Array Of Const); Override;
    {-Use this method to log a formatted string to the event log. }
{End !!.07}

{Begin !!.01 - moved from protected section}
  {validation and checking}
    Function CheckClientIDAndGet(aClientID: TffClientID;
      Var aClient: TFSProxyClient): TffResult;
    Function CheckSessionIDAndGet(aClientID: TffClientID;
      aSessionID: TffSessionID;
      Var aClient: TFSProxyClient;
      Var aSession: TFSProxySession): TffResult;
    Function CheckDatabaseIDAndGet(aDatabaseID: TffDatabaseID;
      Var aDatabase: TFSProxyDatabase): TffResult;
    {-Find the database specified by aDatabaseID. }

    Function CheckCursorIDAndGet(aCursorID: TffCursorID;
      Var aCursor: TFSProxyCursor): TffResult;
    {-Find the cursor specified by aCursorID. }

    Function CheckStmtIDAndGet(aStmtID: TffSqlStmtID;
      Var aStmt: TFSProxySQLStmt): TffResult;
    {-Find the statement specified by aStmtID. }
{End !!.01}

  {creation/destruction}
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp;
      Const AData: TffWord32); Override;

    Function GetDefaultClient: TFSProxyClient;

    Procedure GetServerNames(aList: TStrings;
      aTimeout: Longint); Override;

    Procedure ForceClosing(Const aClientID: TffClientID);

    {Begin !!.01}
    Function RemoteRestart(Const aClientID: TffClientID): TffResult;
    { Tell the remote server to shutdown and startup. }

    Function RemoteStart(Const aClientID: TffClientID): TffResult;
    { Tell the remote server to startup. Only works if the remote server
      is in a stopped state (i.e., transports & cmd handlers still
      listening. }

    Function RemoteStop(Const aClientID: TffClientID): TffResult;
    { Tell the remote server to stop. The server engine shuts down but
      the transport and cmd handlers will still be listening. }
{End !!.01}

  {transaction tracking}
    Function InTransaction(Const aDatabaseID: TffDatabaseID; Var aTransLevel: Longint): TffResult; Override;
    Function TransactionCorrupted(Const aDatabaseID: TffDatabaseID): TffResult; Override;
    Function TransactionCommit(Const aDatabaseID: TffDatabaseID; aRemoveFile: Boolean = False
      ): TffResult; Override;
    Function TransactionRollback(Const aDatabaseID: TffDatabaseID
      ): TffResult; Override;
    Function TransactionStart(Const aDatabaseID: TffDatabaseID;
      Const aFailSafe: Boolean
      ): TffResult; Override;
    {Begin !!.10}
    Function TransactionStartWith(Const aDatabaseID: TffDatabaseID;
      Const aFailSafe: Boolean;
      Const aCursorIDs: TfsPointerList
      ): TffResult; Override;
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
    Function SessionAdd(Const aClientID: TffClientID;
      Const aTimeout: Longint;
      Var aSessionID: TffSessionID;
      aData: Pointer;
      aDataLength: Longint = 0): TffResult; Override;
    Function SessionCloseInactiveTables(aClientID: TffClientID): TffResult; Override; {!!.06}
    Function SessionCount(aClientID: TffClientID;
      Var aCount: Longint): TffResult; Override;
    Function SessionGetCurrent(aClientID: TffClientID;
      Var aSessionID: TffSessionID
      ): TffResult; Override;
    Function SessionRemove(aClientID: TffClientID;
      aSessionID: TffSessionID): TffResult; Override;
    Function SessionSetTimeout(Const aClientID: TffClientID;
      Const aSessionID: TffSessionID;
      Const aTimeout: Longint): TffResult; Override;
    Function SessionSetCurrent(aClientID: TffClientID;
      aSessionID: TffSessionID
      ): TffResult; Override;

    {database related stuff}
    Function DatabaseAddAlias(Const aAlias: TffName;
      Const aPath: TffPath;
      aCheckSpace: Boolean; {!!.11}
      Const aClientID: TffClientID)
      : TffResult; Override;
    Function DatabaseAliasList(aList: TList;
      aClientID: TffClientID)
      : TffResult; Override;
    Function RecoveryAliasList(aList: TList;
      aClientID: TffClientID)
      : TffResult; Override;
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
      Const aTransLocking: TfsDataBaseRecLocking;
      Var aDatabaseID: TffDatabaseID): TffResult; Override;
    Function DatabaseOpenNoAlias(aClientID: TffClientID;
      Const aPath: TffPath;
      Const aOpenMode: TffOpenMode;
      Const aShareMode: TffShareMode;
      Const aTimeout: Longint;
      Const aTransIsolation: TfsTransIsolation;
      Const aTransLocking: TfsDataBaseRecLocking;
      Var aDatabaseID: TffDatabaseID
      ): TffResult; Override;
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
      Var aIsPresent: Boolean;
      Var aStatus: TffRebuildStatus
      ): TffResult; Override;

    {table related stuff}
    Function TableAddIndex(Const aDatabaseID: TffDatabaseID;
      Const aCursorID: TffCursorID;
      Const aTableName: TfsTableName;
      Const aIndexDesc: TffIndexDescriptor
      ): TffResult; Override;
    Function TableBuild(aDatabaseID: TffDatabaseID;
      aOverWrite: Boolean;
      Const aTableName: TfsTableName;
      aForServer: Boolean;
      aDictionary: TFSInfoDict
      ): TffResult; Override;
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
      aForServer: Boolean;
      aStream: TStream): TffResult; Override;
    Function TableGetRecCount(aCursorID: TffCursorID;
      Var aRecCount: Longword): TffResult; Override;
    Function TableGetRecCountAsync(aCursorID: TffCursorID; {!!.07}
      Var aTaskID: Longint): TffResult; Override; {!!.07}
    Function TableOpen(Const aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName;
      Const aForServer: Boolean;
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
      Var aRebuildID: Longint; UndeleteRecords: Boolean; OnlyDeleted: boolean): TffResult; Override;
    Function TableRebuildIndex(aDatabaseID: TffDatabaseID;
      Const aTableName: TfsTableName;
      Const aIndexName: TffName;
      aIndexID: Longint;
      Var aRebuildID: Longint): TffResult; Override;
    Function TableRename(aDatabaseID: TffDatabaseID;
      Const aOldName: TffName;
      Const aNewName: TffName): TffResult; Override;
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
    Function TableIsLocked(aCursorID: TffCursorID;
      aLockType: TffLockType;
      Var aIsLocked: Boolean): TffResult; Override;
    Function TableLockAcquire(aCursorID: TffCursorID;
      aLockType: TffLockType): TffResult; Override;
    Function TableLockRelease(aCursorID: TffCursorID;
      aAllLocks: Boolean): TffResult; Override;

    {cursor stuff}
    Function CursorClone(aCursorID: TffCursorID;
      aOpenMode: TffOpenMode;
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
    Function CursorGetBookmark(aCursorID: TffCursorID;
      aBookmark: PffByteArray): TffResult; Override;

    Function CursorGetBookmarkSize(aCursorID: TffCursorID;
      Var aSize: Longint): TffResult; Override;
    {Begin !!.03}
    Function CursorListBLOBFreeSpace(aCursorID: TffCursorID;
      Const aInMemory: Boolean;
      aStream: TStream): TffResult; Override;
    {End !!.03}
    Function CursorOverrideFilter(aCursorID: Longint;
      aExpression: pCANExpr;
      aTimeout: TffWord32): TffResult; Override;
    Function CursorResetRange(aCursorID: TffCursorID): TffResult; Override;
    Function CursorRestoreFilter(aCursorID: Longint): TffResult; Override;
    Function CursorSetRange(aCursorID: TffCursorID;
      aDirectKey: Boolean;
      aFieldCount1: Longint;
      aPartialLen1: Longint;
      aKeyData1: PffByteArray;
      aKeyIncl1: Boolean;
      aFieldCount2: Longint;
      aPartialLen2: Longint;
      aKeyData2: PffByteArray;
      aKeyIncl2: Boolean): TffResult; Override;
    Function CursorSetTimeout(Const aCursorID: TffCursorID;
      Const aTimeout: Longint): TffResult; Override;
    Function CursorSetToBegin(aCursorID: TffCursorID): TffResult; Override;
    Function CursorSetToBookmark(aCursorID: TffCursorID;
      aBookmark: PffByteArray
      ): TffResult; Override;
    Function CursorSetToCursor(aDestCursorID: TffCursorID;
      aSrcCursorID: TffCursorID
      ): TffResult; Override;
    Function CursorSetToEnd(aCursorID: TffCursorID): TffResult; Override;
    Function CursorSetToKey(aCursorID: TffCursorID;
      aSearchAction: TffSearchKeyAction;
      aDirectKey: Boolean;
      aFieldCount: Longint;
      aPartialLen: Longint;
      aKeyData: PffByteArray
      ): TffResult; Override;
    Function CursorSwitchToIndex(aCursorID: TffCursorID;
      aIndexName: TffDictItemName;
      aIndexID: Longint;
      aPosnOnRec: Boolean): TffResult; Override;
    Function CursorSetFilter(aCursorID: TffCursorID;
      aExpression: pCANExpr;
      aTimeout: TffWord32): TffResult; Override;

    {record stuff}
    Function RecordDelete(aCursorID: TffCursorID;
      aData: PffByteArray): TffResult; Override;
    Function RecordDeleteBatch(aCursorID: TffCursorID;
      aBMCount: Longint;
      aBMLen: Longint;
      aData: PffByteArray;
      aErrors: PffLongintArray): TffResult; Override;
    Function RecordExtractKey(aCursorID: TffCursorID;
      aData: PffByteArray;
      aKey: PffByteArray): TffResult; Override;
    Function RecordGet(aCursorID: TffCursorID;
      aLockType: TffLockType;
      aUserLockType: TfsUserRecLocking;
      aData: PffByteArray; Var aFlag: Byte; Var aRefNr: TffInt64; Const aUser: Boolean): TffResult; Override;
    Function RecordGetBatch(aCursorID: TffCursorID;
      aRecCount: Longint;
      aRecLen: Longint;
      Var aRecRead: Longint;
      aData: PffByteArray;
      Var aError: TffResult): TffResult; Override;
    Function RecordGetForKey(aCursorID: TffCursorID;
      aDirectKey: Boolean;
      aFieldCount: Longint;
      aPartialLen: Longint;
      aKeyData: PffByteArray;
      aData: PffByteArray;
      aFirstCall: Boolean
      ): TffResult; Override;
    Function RecordGetSetPosition(aValue: Longint; aCursorID: TffCursorID;
      aLockType: TffLockType;
      aData: PffByteArray;
      Var aFlag: Byte;
      Var aRecNo: Longword;
      Var aRefNr: TffInt64;
      aInfoGetSetPosition: TInfoGetSetPosition;
      aSet: Boolean): TffResult; Override;

    Function RecordGetNext(aCursorID: TffCursorID;
      aLockType: TffLockType;
      aData: PffByteArray; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult; Override;
    Function RecordGetPrior(aCursorID: TffCursorID;
      aLockType: TffLockType;
      aData: PffByteArray; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult; Override;
    Function RecordInsert(aCursorID: TffCursorID;
      aLockType: TffLockType;
      aData: PffByteArray; aUndelete: Boolean; Var aRefNr: TffInt64): TffResult; Override;
    Function RecordInsertBatch(aCursorID: TffCursorID;
      aRecCount: Longint;
      aRecLen: Longint;
      aData: PffByteArray;
      aErrors: PffLongintArray): TffResult; Override;
    Function RecordIsLocked(aCursorID: TffCursorID;
      aLockType: TffLockType;
      Var aIsLocked: boolean): TffResult; Override;
    Function RecordModify(aCursorID: TffCursorID;
      aData: PffByteArray;
      aRelLock: Boolean;
      aUserLockType: TfsUserRecLocking;
      aFlag: Byte;
      aSet, Use: Boolean): TffResult; Override;
    Function RecordRelLock(aCursorID: TffCursorID;
      aAllLocks: Boolean): TffResult; Override;
    {BLOB stuff}
    Function BLOBCreate(aCursorID: TffCursorID;
      Var aBlobNr: TffInt64): TffResult; Override;
    Function BLOBDelete(aCursorID: TffCursorID;
      aBlobNr: TffInt64): TffResult; Override;
    {Begin !!.03}
    Function BLOBListSegments(aCursorID: TffCursorID;
      aBLOBNr: TffInt64;
      aStream: TStream): TffResult; Override;
    {End !!.03}
    Function BLOBRead(aCursorID: TffCursorID;
      aFieldNo: TffWord32;
      aBlobNr: TffInt64;
      aOffset: TffWord32; {!!.06}
      aLen: TffWord32; {!!.06}
      Var aBLOB;
      Var aBytesRead: TffWord32) {!!.06}
    : TffResult; Override;
    Function BLOBFree(aCursorID: TffCursorID; aBlobNr: TffInt64;
      ReadOnly: Boolean): TffResult; Override;
    Function BLOBGetLength(aCursorID: TffCursorID; aBlobNr: TffInt64;
      Var aLength: Longint): TffResult; Override;
    Function BLOBTruncate(aCursorID: TffCursorID; aBlobNr: TffInt64;
      aBLOBLength: Longint): TffResult; Override;
    Function BLOBWrite(aCursorID: TffCursorID;
      aFieldNo: TffWord32;
      aBlobNr: TffInt64;
      aOffset: Longint;
      aLen: Longint;
      Var aBLOB): TffResult; Override;
    Function FileBLOBAdd(aCursorID: TffCursorID;
      Const aFileName: TffFullFileName;
      Var aBlobNr: TffInt64): TffResult; Override;

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
      aParamDescs: pointer;
      aDataBuffer: PffByteArray;
      aDataLen: Longint;
      aStream: TStream): TffResult; Override;

    {misc stuff}
    Function GetServerDateTime(Var aDateTime: TDateTime
      ): TffResult; Override;
    {begin !!.07}
    Function GetServerSystemTime(Var aSystemTime: TSystemTime): TffResult; Override;
    Function GetServerGUID(Var aGUID: TGUID): TffResult; Override;
    Function GetServerID(Var aUniqueID: TGUID): TffResult; Override;
    Function GetServerStatistics(Var Stats: TfsServerStatistics): TffResult; Override;
    Function GetCommandHandlerStatistics(Const CmdHandlerIdx: Integer;
      Var Stats: TfsCommandHandlerStatistics): TffResult; Override;
    Function GetTransportStatistics(Const CmdHandlerIdx: Integer;
      Const TransportIdx: Integer;
      Var Stats: TfsTransportStatistics): TffResult; Override;
    {end !!.07}

{properties}
    Property ClientList: TFSProxyClientList
      Read rsClientList;

    Property TimeOut: TFFWord32
      Read rsTimeout Write rsTimeout;

  Published
    Property Transport: TFSBaseTransport
      Read rsTransport
      Write rsSetTransport;

  End;

  {Callback method used by the transport to notify us when the request is
   complete.}
Procedure ProxyRequestCallback(aMsgID: Longint;
  aErrorCode: TffResult;
  aReply: Pointer;
  aReplyLen: Longint;
  aReplyCookie: Longint);

Var
  RemoteServerEngines: TFSSpecThreadList;

Implementation

Uses
  Activex,
  fssqlbas;

{--Internal helper routines--}

Function ResultOK(aResult: TffResult): Boolean;
Begin
  Result := aResult = DBIERR_NONE;
End;
{------------------------------------------------------------------------------}

{--Callback routine--}

Procedure ProxyRequestCallback(aMsgID: Longint;
  aErrorCode: TffResult;
  aReply: Pointer;
  aReplyLen: Longint;
  aReplyCookie: Longint);
Var
  Client: TFSProxyClient Absolute aReplyCookie;
Begin
  { hand-off the response from the transport to the ProxyClient }
  Client.pcMsgQueue.Append(aMsgID,
    aReplyCookie,
    0, {RequestID}
    0, {Timeout}
    aErrorCode,
    aReply,
    aReplyLen,
    aReplyLen);
End;
{------------------------------------------------------------------------------}

{-TFSProxyClient---------------------------------------------------------------}

Constructor TFSProxyClient.Create(aTransport: TFSBaseTransport;
  aUserName: TFFName;
  aPasswordHash: Longint;
  aTimeOut: Longint;
  aRights: TffUserRights;
  aSecurityEnabled: boolean);
Begin
  Inherited Create;

  {Initialize internals}
  pcSrClientID := 0;
  //pcAccessRight:=0;
  pcCurrentSession := Nil;
  pcForceClosed := False;
  pcRights := aRights;
  pcSecurityEnabled := aSecurityEnabled;
  pcTransport := aTransport;
  pcTimeout := aTimeOut;

  {Create internal classes}
  pcMsgQueue := TffDataMessageQueue.Create;
  pcSessions := TFSProxySessionList.Create;
  pcDatabases := TFSProxyDatabaseList.Create;

  {Set the CallbackMethod that will be used by the transport to return data}
  pcCallbackMethod := ProxyRequestCallback;

  {Let the ServerEngine know that we are here. Set our SrClientID for later
   reference, as we will need it often.}
  Check(pcTransport.EstablishConnection(aUserName,
    aPasswordHash,
    pcTimeOut,
    pcSrClientID,
    pcRights,
    pcSecurityEnabled));
End;
{----------}

Function TFSProxyClient.DatabaseAddAlias(Const aAlias: TffName;
  Const aPath: TffPath;
  aCheckSpace: Boolean) {!!.11}
: TffResult;
Var
  Request: TfsnmDatabaseAddAliasReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize the request record }
  Request.Alias := aAlias;
  Request.Path := aPath;
  Request.CheckDisk := aCheckSpace; {!!.11}

  Reply := Nil;
  Result := ProcessRequest(fsnmDatabaseAddAlias,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);

  { Calling fsnmDatabaseAddAlias only returns an error code to Result. }
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyClient.DatabaseAliasList(aList: TList): TffResult;
Var
  Stream: TMemoryStream;
  ReplyLen: Longint;
  Count: Longint;
  AliasDes: PffAliasDescriptor;
  DesSize: Longint;
Begin
  Stream := TMemoryStream.Create;
  Try
    { We have no data to send. }
    Result := ProcessRequest(fsnmDatabaseAliasList,
      Timeout,
      Nil,
      0,
      nmdByteArray,
      Pointer(Stream),
      ReplyLen,
      nmdStream);

    If ResultOK(Result) Then
      Begin
        aList.Clear;
        Stream.Position := 0;
        DesSize := SizeOf(TffAliasDescriptor);

        For Count := 1 To (ReplyLen Div DesSize) Do
          Begin
            { Move the alias data from the stream, to a PffAliasDescriptor. Each
              descriptor will be an entry in aList. The caller must free this
              data when it is done using it. }
            FFGetMem(AliasDes, DesSize);
            Stream.Read(AliasDes^, DesSize);
            aList.Add(AliasDes);
          End;
      End;
  Finally
    Stream.Free;
  End;
End;
{----------}

Function TFSProxyClient.DatabaseChgAliasPath(Const aAlias: TffName;
  Const aNewPath: TffPath;
  aCheckSpace: Boolean) {!!.11}
: TffResult;
Var
  Request: TfsnmDatabaseChgAliasPathReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize the request record }
  Request.Alias := aAlias;
  Request.NewPath := aNewPath;
  Request.CheckDisk := aCheckSpace; {!!.11}

  Reply := Nil;
  Result := ProcessRequest(fsnmDatabaseChgAliasPath,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);

  { Calling fsnmDatabaseChgAliasPath only returns an error code to Result. }
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyClient.DatabaseClose(aDatabase: TFSProxyDatabase): TffResult;
Begin
  Result := DBIERR_NONE;
  With pcDatabases.BeginWrite Do
    Try
      Delete(aDatabase); {!!.01}
    Finally
      EndWrite;
    End;
  aDatabase.Free;
  aDatabase := Nil;
End;
{----------}

Function TFSProxyClient.DatabaseDeleteAlias(Const aAlias: TffName): TffResult;
Var
  Request: TfsnmDatabaseDeleteAliasReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize the request record }
  Request.Alias := aAlias;

  Reply := Nil;
  Result := ProcessRequest(fsnmDatabaseDeleteAlias,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);

  { Calling fsnmDatabaseDeleteAlias only returns an error code to Result. }
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyClient.DatabaseGetAliasPath(Const aAlias: TffName;
  Var aPath: TffPath
  ): TffResult;
Var
  Request: TfsnmDatabaseGetAliasPathReq;
  Reply: PfsnmDatabaseGetAliasPathRpy;
  ReplyLen: Longint;
Begin
  { Initialize the request record }
  Request.Alias := aAlias;

  Reply := Nil;
  Result := ProcessRequest(fsnmDatabaseGetAliasPath,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    aPath := Reply^.Path;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyClient.DatabaseModifyAlias(Const aAlias: TffName;
  Const aNewName: TffName;
  Const aNewPath: TffPath;
  aCheckSpace: Boolean) {!!.11}
: TffResult;
Var
  Request: TfsnmDatabaseModifyAliasReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize the request record }
  Request.ClientID := SrClientID;
  Request.Alias := aAlias;
  Request.NewName := aNewName;
  Request.NewPath := aNewPath;
  Request.CheckDisk := aCheckSpace; {!!.11}

  Reply := Nil;
  Result := ProcessRequest(fsnmDatabaseModifyAlias,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyClient.DatabaseOpen(Const aAlias: TffName;
  aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  aTimeout: Longint;
  aTransIsolation: TfsTransIsolation;
  aTransLocking: TfsDataBaseRecLocking;
  Var aDatabaseID: TffDatabaseID)
  : TffResult;
Var
  Database: TFSProxyDatabase;
  ListItem: TfsIntListItem;
Begin
  Database := Nil;
  Result := DBIERR_NONE;

  Try
    Database := TFSProxyDatabase.Create(Self,
      aAlias,
      aOpenMode,
      aShareMode,
      aTimeout,
      aTransIsolation,
      aTransLocking,
      True);
  Except
    On E: Exception Do
      If (E Is EfsException) Or (E Is EfsDatabaseError) Then
        Result := EfsException(E).ErrorCode;
  End;

  If ResultOK(Result) And Assigned(Database) Then
    Begin
      {Add Database to the internal list}
      ListItem := TfsIntListItem.Create(Longint(Database));
      With pcDatabases.BeginWrite Do
        Try
          Insert(ListItem);
        Finally
          EndWrite;
        End;

      aDatabaseID := Longint(Database);
    End;
End;
{----------}

Function TFSProxyClient.DatabaseOpenNoAlias(Const aPath: TffPath;
  aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  aTimeout: Longint;
  aTransIsolation: TfsTransIsolation;
  aTransLocking: TfsDataBaseRecLocking;
  Var aDatabaseID: TffDatabaseID
  ): TffResult;
Var
  Database: TFSProxyDatabase;
  ListItem: TfsIntListItem;
Begin
  Database := Nil;
  Result := DBIERR_NONE;

  Try
    Database := TFSProxyDatabase.Create(Self,
      aPath,
      aOpenMode,
      aShareMode,
      aTimeout,
      aTransIsolation,
      aTransLocking,
      False);
  Except
    On E: Exception Do
      If (E Is EfsException) Or (E Is EfsDatabaseError) Then
        Result := EfsException(E).ErrorCode;
  End;

  If ResultOK(Result) And Assigned(Database) Then
    Begin
      {Add Database to the internal list}
      ListItem := TfsIntListItem.Create(Longint(Database));
      With pcDatabases.BeginWrite Do
        Try
          Insert(ListItem);
        Finally
          EndWrite;
        End;

      aDatabaseID := Longint(Database);
    End;
End;
{----------}

Destructor TFSProxyClient.Destroy;
{Begin !!.03}
//var
//  Idx : Longint;
Begin
  {Destroy managed objects}
  pcMsgQueue.Free;
  pcMsgQueue := Nil;
  pcSessions.Free;
  pcSessions := Nil;
  pcDatabases.Free;
  pcDatabases := Nil;

  {Tell the server that we are disconnecting.}
  If Not ForceClosed Then
    If SrClientID > 0 Then
      pcTransport.TerminateConnection(SrClientID, Timeout);

  {Re-Initialize internals for completeness}
  pcCurrentSession := Nil;
  pcTransport := Nil;
  pcCallbackMethod := Nil;

  Inherited Destroy;
End;
{----------}

Function TFSProxyClient.IsReadOnly: Boolean;
Var
  Reply: PfsnmServerIsReadOnlyRpy;
  ReplyLen: Longint;
  ErrorCode: TffResult;
Begin
  Reply := Nil;
  ErrorCode := ProcessRequest(fsnmServerIsReadOnly,
    Timeout,
    Nil,
    0,
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(ErrorCode) Then
    Result := Reply^.IsReadOnly
  Else
    Result := False;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyClient.GetServerDateTime(Var aDateTime: TDateTime
  ): TffResult;
Var
  Reply: PfsnmGetServerDateTimeRpy;
  ReplyLen: Longint;
Begin
  { Just in case }
  aDateTime := Now;

  { We have no data to send }
  Reply := Nil;
  Result := ProcessRequest(fsnmGetServerDateTime,
    Timeout,
    Nil,
    0,
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    aDateTime := Reply^.ServerNow;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}{begin !!.07}

Function TFSProxyClient.GetServerSystemTime(Var aSystemTime: TSystemTime): TffResult;
Var
  Reply: PfsnmGetServerSystemTimeRpy;
  ReplyLen: Longint;
Begin
  { Just in case }
  GetSystemTime(aSystemTime);

  { We have no data to send }
  Reply := Nil;
  Result := ProcessRequest(fsnmGetServerSystemTime,
    Timeout,
    Nil,
    0,
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    aSystemTime := Reply^.ServerNow;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyClient.GetServerGUID(Var aGUID: TGUID): TffResult;
Var
  Reply: PfsnmGetServerGUIDRpy;
  ReplyLen: Longint;
Begin
  { Just in case }
  CoCreateGuid(aGUID);

  { We have no data to send }
  Reply := Nil;
  Result := ProcessRequest(fsnmGetServerGUID,
    Timeout,
    Nil,
    0,
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    aGUID := Reply^.GUID;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyClient.GetServerID(Var aUniqueID: TGUID): TffResult;
Var
  Reply: PfsnmGetServerIDRpy;
  ReplyLen: Longint;
Begin
  { We have no data to send }
  Reply := Nil;
  Result := ProcessRequest(fsnmGetServerID,
    Timeout,
    Nil,
    0,
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    aUniqueID := Reply^.UniqueID;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyClient.GetServerStatistics(Var Stats: TfsServerStatistics): TffResult;
Var
  Reply: PfsnmServerStatisticsRpy;
  ReplyLen: Longint;
Begin
  { We have no data to send }
  Reply := Nil;
  Result := ProcessRequest(fsnmServerStatistics,
    Timeout,
    Nil,
    0,
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);

  If ResultOK(Result) Then
    Stats := Reply^.Stats;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyClient.GetCommandHandlerStatistics(Const CmdHandlerIdx: Integer;
  Var Stats: TfsCommandHandlerStatistics): TffResult;
Var
  Request: TfsnmCmdHandlerStatisticsReq;
  Reply: PfsnmCmdHandlerStatisticsRpy;
  ReplyLen: Longint;
Begin
  { Initiailize Request }
  Request.CmdHandlerIdx := CmdHandlerIdx;

  Reply := Nil;
  Result := ProcessRequest(fsnmCmdHandlerStatistics,
    pcTimeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);

  If ResultOK(Result) Then
    Stats := Reply^.Stats;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyClient.GetTransportStatistics(Const CmdHandlerIdx: Integer;
  Const Transportidx: Integer;
  Var Stats: TfsTransportStatistics): TffResult;
Var
  Request: TfsnmTransportStatisticsReq;
  Reply: PfsnmTransportStatisticsRpy;
  ReplyLen: Longint;
Begin
  { Initiailize Request }
  Request.CmdHandlerIdx := CmdHandlerIdx;
  Request.TransportIdx := Transportidx;

  Reply := Nil;
  Result := ProcessRequest(fsnmTransportStatistics,
    pcTimeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);

  If ResultOK(Result) Then
    Stats := Reply^.Stats;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}{end !!.07}

Function TFSProxyClient.ProcessRequest(aMsgID: Longint;
  aTimeout: Longint;
  aRequestData: Pointer;
  aRequestDataLen: Longint;
  aRequestDataType: TffNetMsgDataType;
  Var aReply: Pointer;
  Var aReplyLen: Longint;
  aReplyType: TffNetMsgDataType
  ): TffResult;
Var
  ReplyAsStream: TStream Absolute aReply;
  ReplyMsg: PFSDataMessage;
Begin
  If ForceClosed Then
    Begin
      Result := DBIERR_NONE;
      aReply := Nil;
      aReplyLen := 0;
      Exit;
    End;

  Result := DBIERR_NA;
  {A Respose from the server is expected. This call will not return until
   the complete reply has been sent to the transport, and the Client
   callback method has been called.}

      { Use the ProxessRequest method to submit a request that is routed to the
        transport.  This method does the following:

        1. Calls TFSBaseTransport.Request with transportID = 0 and cookie
           equal to Pointer(Self).  At this point, the calling thread is
           blocked until a reply is received from the server or a timeout
           occurs.
        2. When the calling thread returns to this method, the reply has
           been received and placed in the message queue by the
           ProxyClientCallback procedure.
        3. Get the first message off the queue and verify it is what we
           expected.
        4. Put the message into the Reply variables and exit.
       }

  { Is our reply already in the queue (e.g., came back as part
    of a multi-part message?  Assumption: We can get rid of any
    replies that don't match the message we are requesting. }
  ReplyMsg := pcMsgQueue.SoftPop;
  While Assigned(ReplyMsg) And (ReplyMsg^.dmMsg <> aMsgID) Do
    Begin
      FFFreeMem(ReplyMsg^.dmData, ReplyMsg^.dmDataLen);
      FFFreeMem(ReplyMsg, SizeOf(TfsDataMessage));
      ReplyMsg := pcMsgQueue.SoftPop;
    End;

  If Not Assigned(ReplyMsg) Then
    Begin

      pcTransport.Request(0, {For use by future protocols.}
        SrClientID,
        aMsgID,
        aTimeout,
        aRequestData,
        aRequestDataLen,
        pcCallbackMethod,
        Longint(Self));

      {Process the reply from the server. Get the reply message off the queue
       and verify that is what we expected}
      Assert(pcMsgQueue.Count <= 1, 'Too many messages in the queue');
      ReplyMsg := pcMsgQueue.SoftPop;
    End;

  If Assigned(ReplyMsg) Then
    Begin
      If (ReplyMsg^.dmMsg <> aMsgID) Then
        Begin
          Result := DBIERR_NOTSAMESESSION;
          FFFreeMem(ReplyMsg^.dmData, ReplyMsg^.dmDataLen); {!!.03}
          FFFreeMem(ReplyMsg, SizeOf(TfsDataMessage));
          Exit;
        End;

      aReplyLen := ReplyMsg^.dmDataLen;
      If aReplyType = nmdStream Then
        Begin
          Assert(Assigned(ReplyAsStream));
          ReplyAsStream.Position := 0;
          If (aReplyLen > 0) Then
            Begin
              ReplyAsStream.Write(ReplyMsg^.dmData^, aReplyLen);
              FFFreeMem(ReplyMsg^.dmData, aReplyLen);
            End;
        End
      Else
        aReply := ReplyMsg^.dmData;

      Result := ReplyMsg^.dmErrorCode;

      { Free the ReplyMsg, but leave RequestData alone.
        The caller is responsible for releasing data.
        We expect the caller to free the reply data.}
      FFFreeMem(ReplyMsg, SizeOf(TfsDataMessage));

    End;
End;
{----------}

Function TFSProxyClient.ProcessRequestNoReply(aMsgID: Longint;
  aTimeout: Longint;
  aRequestData: Pointer;
  aRequestDataLen: Longint
  ): TffResult;
Begin
  If ForceClosed Then
    Begin
      Result := DBIERR_NONE;
      Exit;
    End;

  {No response from the server is expected, so this call will return as
   soon as the request has been sent from the transport's queue}

  pcTransport.Post(0, {For use by future protocols.}
    SrClientID,
    aMsgID,
    aRequestData,
    aRequestDataLen,
    aTimeout,
    fsrmNoReplyWaitUntilSent);

  Result := DBIERR_NONE;
End;
{Begin !!.01}
{----------}

Function TFSProxyClient.RemoteRestart: TffResult;
Begin
  Result := ProcessRequestNoReply(fsnmServerRestart, Timeout, Nil, 0);
End;
{----------}

Function TFSProxyClient.RemoteStart: TffResult;
Begin
  Result := ProcessRequestNoReply(fsnmServerStartup, Timeout, Nil, 0);
End;
{----------}

Function TFSProxyClient.RemoteStop: TffResult;
Begin
  Result := ProcessRequestNoReply(fsnmServerStop, Timeout, Nil, 0);
End;
{End !!.01}
{----------}

Function TFSProxyClient.SessionAdd(Var aSessionID: TffSessionID;
  Const aTimeout: Longint; aData: Pointer; aDataLength: Longint = 0): TffResult;
Var
  Session: TFSProxySession;
  ListItem: TfsIntListItem;
Begin
  Session := Nil;
  Result := DBIERR_NONE;

  Try
    Session := TFSProxySession.Create(Self, aTimeout, aData, aDataLength);
  Except
    On E: Exception Do
      If (E Is EfsException) Or (E Is EfsDatabaseError) Then
        Result := EfsException(E).ErrorCode;
  End;

  If ResultOK(Result) And Assigned(Session) Then
    Begin
      {Add Session to the internal list}
      ListItem := TfsIntListItem.Create(Longint(Session));
      With pcSessions.BeginWrite Do
        Try
          Insert(ListItem);
        Finally
          EndWrite;
        End;

      aSessionID := Longint(Session);

      {Set the current session if it is nil}
      If Not Assigned(pcCurrentSession) Then
        pcCurrentSession := Session;
    End;
End;
{Begin !!.06}
{----------}

Function TFSProxyClient.SessionCloseInactiveTables: TffResult;
Var
  Request: TfsnmSessionCloseInactiveTblReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initiailize Request }
  Request.SessionID := pcCurrentSession.psSrSessionID;

  Reply := Nil;
  Result := ProcessRequest(fsnmSessionCloseInactTbl,
    pcTimeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{End !!.06}
{----------}

Function TFSProxyClient.SessionCount: Longint;
Begin
  {Retun the number of sessions managed by the ProxyClient}
  With pcSessions.BeginRead Do
    Try
      Result := Count;
    Finally
      EndRead;
    End;
End;
{----------}

Function TFSProxyClient.SessionGetCurrent: TFSProxySession;
Begin
  {Return the current session. This value will be nil if no sessions exist}
  If Assigned(pcCurrentSession) Then
    Result := pcCurrentSession
  Else
    Begin
      If SessionCount > 0 Then
        {Return the first session in the list}
        With pcSessions.BeginRead Do
          Try
            Result := TFSProxySession(Items[0]);
          Finally
            EndRead;
          End
      Else
        {no sessions available}
        Result := Nil;
    End;
End;
{----------}

Function TFSProxyClient.SessionRemove(aSession: TFSProxySession): TffResult;
Begin
  {Remove session from the internal list, and destroy.}
  If Not Assigned(aSession) Then
    Begin
      {aSession parameter is invalid}
      Result := DBIERR_INVALIDHNDL;
      Exit;
    End;

  Result := DBIERR_NONE;
  With pcSessions.BeginWrite Do
    Try
      Delete(aSession); {!!.01}
    Finally
      EndWrite;
    End;

  aSession.Free;
End;
{----------}

Function TFSProxyClient.SessionSetCurrent(aSession: TFSProxySession
  ): TffResult;
Var
  Request: TfsnmSessionSetCurrentReq;
  Reply: PfsnmSessionSetCurrentReq;
  ReplyLen: Longint;
Begin
  {Set the Client's CurrentSession. This function will accept nil as a valid
   option}
  Request.SessionID := aSession.psSrSessionID;
  Reply := Nil;
  Result := ProcessRequest(fsnmSessionSetCurrent,
    pcTimeOut,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);

  //  Result := DBIERR_NONE;
  pcCurrentSession := aSession;
End;
{----------}

Function TFSProxyClient.GetRebuildStatus(Const aRebuildID: Longint;
  Var aIsPresent: Boolean;
  Var aStatus: TffRebuildStatus): TffResult;
Var
  Request: TfsnmGetRebuildStatusReq;
  Reply: PfsnmGetRebuildStatusRpy;
  ReplyLen: Longint;
Begin
  { Initiailize Request }
  Request.RebuildID := aRebuildID;

  Reply := Nil;
  Result := ProcessRequest(fsnmGetRebuildStatus,
    pcTimeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);

  If ResultOK(Result) Then
    Begin
      aIsPresent := Reply^.IsPresent;
      aStatus := Reply^.Status;
    End;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyClient.SetTimeout(Const aTimeout: Longint): TffResult;
Var
  Request: TfsnmClientSetTimeoutReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  Result := DBIERR_NONE;
  If pcTimeout = aTimeout Then Exit;

  pcTimeout := aTimeout;
  { Initialize request }
  Request.Timeout := pcTimeout;

  Reply := Nil;
  Result := ProcessRequest(fsnmClientSetTimeout,
    pcTimeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);

  { Calling fsnmClientSetTimeout only returns an error code to Result. }
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{------------------------------------------------------------------------------}

{-TFSProxySession--------------------------------------------------------------}

Constructor TFSProxySession.Create(aClient: TFSProxyClient;
  aTimeout: Longint; aData: Pointer; aDataLength: Longint = 0);
Var
  Request: PfsnmSessionAddReq;
  Reply: PfsnmSessionAddRpy;
  ReplyLen, ReqLen: Longint;
  Result: TFFResult;
Begin
  Inherited Create;

  {Initalize the object}
  psClient := aClient;
  psSrSessionID := 0;
  psTimeout := aTimeout;

  { Initiailize Request }
  ReqLen := SizeOf(TfsnmSessionAddReq) - SizeOf(TfsVarMsgField) +
    aDataLength + 1;
  //ReqLen := aDataLength + 1;
  Reply := Nil;

  FFGetZeroMem(Request, ReqLen);
  Try
    Request^.IsClientDb[0] := '#';
    Request^.IsClientDb[1] := 'C';
    Request^.IsClientDb[2] := 'L';
    Request^.IsClientDb[3] := 'I';
    Request^.IsClientDb[4] := 'E';
    Request^.IsClientDb[5] := 'N';
    Request^.IsClientDb[6] := 'T';
    Request^.IsClientDb[7] := 'D';
    Request^.IsClientDb[8] := 'B';
    Request^.IsClientDb[9] := '#';
    Request^.Timeout := aTimeout;
    Request^.DataLength := aDataLength;
    If aDataLength > 0 Then
      Move(aData^, Request^.Data, aDataLength);
    // rl:=  SizeOf( Request^ );

    Result := psClient.ProcessRequest(fsnmSessionAdd,
      psTimeout,
      Request,
      ReqLen,
      nmdByteArray,
      Pointer(Reply),
      ReplyLen,
      nmdByteArray);
    {Make sure that result was valid before we continue}
    Check(Result);

    psSrSessionID := Reply^.SessionID;
  Finally
    FFFreeMem(Request, ReqLen);
    FFFreeMem(Reply, ReplyLen);
  End;
End;
{----------}

Destructor TFSProxySession.Destroy;
Var
  Request: TfsnmSessionCloseReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  If SrSessionID > 0 Then
    Begin
      { Initiailize Request }
      Request.SessionID := SrSessionID;

      Reply := Nil;
      Client.ProcessRequest(fsnmSessionClose,
        Timeout,
        @Request,
        SizeOf(Request),
        nmdByteArray,
        Reply,
        ReplyLen,
        nmdByteArray);

      If Assigned(Reply) Then
        FFFreeMem(Reply, ReplyLen);
    End;

  psClient := Nil;

  Inherited Destroy;
End;
{----------}

Function TFSProxySession.SetTimeout(aTimeout: Longint): TffResult;
Var
  Request: TfsnmSessionSetTimeoutReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  Result := DBIERR_NONE;
  If psTimeout = aTimeout Then Exit;

  psTimeout := aTimeout;

  { Initiailize Request }
  Request.SessionID := psSrSessionID;
  Request.Timeout := psTimeout;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmSessionSetTimeout,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{------------------------------------------------------------------------------}

{-TFSProxyDatabase-------------------------------------------------------------}

Constructor TFSProxyDatabase.Create(aClient: TFSProxyClient;
  aLocation: String;
  aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  aTimeout: Longint;
  aTransIsolation: TfsTransIsolation;
  aTransLocking: TfsDataBaseRecLocking;
  aIsAlias: Boolean);
Var
  RequestAlias: TfsnmDatabaseOpenReq;
  RequestPath: TfsnmDatabaseOpenNoAliasReq;
  ReplyAlias: PfsnmDatabaseOpenRpy;
  ReplyPath: PfsnmDatabaseOpenNoAliasRpy;
  ReplyLen: Longint;
  Result: TffResult;
Begin
  Inherited Create;

  pdInTrans := False;
  pdSrDatabaseID := 0;
  pdClient := aClient;
  pdTimeout := aTimeout;
  pdTransIsolation := aTransIsolation;
  pdTransLocking := aTransLocking;

  pdStmts := TFSProxySQLStmtList.Create;
  pdTables := TFSProxyCursorList.Create;

  If aIsAlias Then
    Begin
      { Initiailize Request }
      RequestAlias.Alias := aLocation;
      RequestAlias.OpenMode := aOpenMode;
      RequestAlias.ShareMode := aShareMode;
      RequestAlias.Timeout := aTimeout;
      RequestAlias.TransIsolation := aTransIsolation;
      RequestAlias.TransLocking := aTransLocking;

      ReplyAlias := Nil;
      Result := Client.ProcessRequest(fsnmDatabaseOpen,
        pdTimeout,
        @RequestAlias,
        SizeOf(RequestAlias),
        nmdByteArray,
        Pointer(ReplyAlias),
        ReplyLen,
        nmdByteArray);
      Check(Result);

      pdSrDatabaseID := ReplyAlias^.DatabaseID;

      FFFreeMem(ReplyAlias, ReplyLen);
    End
  Else
    Begin
      { Initiailize Request }
      RequestPath.Path := aLocation;
      RequestPath.OpenMode := aOpenMode;
      RequestPath.ShareMode := aShareMode;
      RequestPath.Timeout := aTimeout;
      RequestPath.TransIsolation := aTransIsolation;
      RequestPath.TransLocking := aTransLocking;

      ReplyPath := Nil;
      Result := Client.ProcessRequest(fsnmDatabaseOpenNoAlias,
        pdTimeout,
        @RequestPath,
        SizeOf(RequestPath),
        nmdByteArray,
        Pointer(ReplyPath),
        ReplyLen,
        nmdByteArray);
      Check(Result);

      pdSrDatabaseID := ReplyPath^.DatabaseID;

      FFFreeMem(ReplyPath, ReplyLen);
    End;
End;
{----------}

Destructor TFSProxyDatabase.Destroy;
Var
  //  Idx      : Longint;                                                {!!.03}
  Request: TfsnmDatabaseCloseReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  {Destroy dependent objects}
  If InTrans Then
    TransactionRollback;

  {Begin !!.03}
  //  with pdTables.BeginWrite do
  //    try
  //      for Idx := 0 to Pred(Count) do
  //        TFSProxyCursor(Items[Idx]).Free;
  //    finally
  //      EndWrite;
  //    end;

  pdTables.Free;
  pdTables := Nil;

  //  with pdStmts.BeginWrite do
  //    try
  //      for Idx := 0 to Pred(Count) do
  //        TFSProxySQLStmt(Items[Idx]).Free;
  //    finally
  //      EndWrite;
  //    end;
  {End !!.03}

  pdStmts.Free;
  pdStmts := Nil;

  {Let the server know that we are leaving}
  If SrDatabaseID > 0 Then
    Begin
      { Initiailize Request }
      Request.DatabaseID := SrDatabaseID;

      Reply := Nil;
      Client.ProcessRequest(fsnmDatabaseClose,
        Timeout,
        @Request,
        SizeOf(Request),
        nmdByteArray,
        Reply,
        ReplyLen,
        nmdByteArray);
      If Assigned(Reply) Then
        FFFreeMem(Reply, ReplyLen);
    End;
  {Reset internals}
  pdSrDatabaseID := 0;
  pdClient := Nil;

  Inherited;
End;
{----------}

Function TFSProxyDatabase.GetDbFreeSpace(Var aFreeSpace: Int64): TffResult;
Var
  Request: TfsnmDatabaseGetFreeSpaceReq;
  Reply: PfsnmDatabaseGetFreeSpaceRpy;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.DatabaseID := pdSrDatabaseID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmDatabaseGetFreeSpace,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    aFreeSpace := Reply^.FreeSpace;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyDatabase.QueryOpen(aCursorID: TffCursorID;
  aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  aTimeout: Longint;
  aStream: TStream;
  Var aFinalCursorID: TffCursorID): TffResult;
Var
  Cursor: TFSProxyCursor;
  ListItem: TfsIntListItem;
Begin
  Cursor := Nil;
  Result := DBIERR_NONE;

  Try
    Cursor := TFSProxyCursor.CreateSQL(Self, aCursorID, aOpenMode, aShareMode,
      aTimeout, aStream);
  Except
    On E: Exception Do
      If (E Is EfsException) Or (E Is EfsDatabaseError) Then
        Result := EfsException(E).ErrorCode;
  End;

  If ResultOK(Result) And Assigned(Cursor) Then
    Begin
      ListItem := TfsIntListItem.Create(Longint(Cursor));
      ListItem.MaintainLinks := False; {!!.02}
      With pdTables.BeginWrite Do
        Try
          Insert(ListItem);
        Finally
          EndWrite;
        End;

      aFinalCursorID := Longint(Cursor);
    End;
End;
{----------}

Function TFSProxyDatabase.SetTimeout(Const aTimeout: Longint): TffResult;
Var
  Request: TfsnmDatabaseSetTimeoutReq;
  Reply: pointer;
  ReplyLen: Longint;
Begin
  Result := DBIERR_NONE;
  If pdTimeout = aTimeout Then Exit;

  pdTimeout := aTimeout;

  { Initialize Request }
  Request.DatabaseID := pdSrDatabaseID;
  Request.Timeout := aTimeout;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmDatabaseSetTimeout,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyDatabase.SQLAlloc(Const aTimeout: Longint;
  Var aStmtID: TffSqlStmtID): TffResult;
Var
  ListItem: TfsIntListItem;
  Statement: TFSProxySQLStmt;
Begin
  Statement := Nil;
  Result := DBIERR_NONE;

  Try
    Statement := TFSProxySQLStmt.Create(Self, aTimeout);
  Except
    On E: Exception Do
      If (E Is EfsException) Or (E Is EfsDatabaseError) Then
        Result := EfsException(E).ErrorCode;
  End;

  If ResultOK(Result) And Assigned(Statement) Then
    Begin
      ListItem := TfsIntListItem.Create(Longint(Statement));
      With pdStmts.BeginWrite Do
        Try
          Insert(ListItem);
        Finally
          EndWrite;
        End;

      aStmtID := Longint(Statement);
    End;

End;
{----------}

Function TFSProxyDatabase.SQLExecDirect(aQueryText: PChar;
  aOpenMode: TffOpenMode;
  aTimeout: Longint;
  Var aCursorID: TffCursorID;
  aStream: TStream): TffResult;
Var
  QueryLen: Longint;
  ReplyLen: Longint;
  Request: PfsnmSQLExecDirectReq;
  ReqLen: Longint;
  SvrCursorID: TffCursorID;
Begin
  Assert(Assigned(aStream));
  QueryLen := StrLen(aQueryText);
  ReqLen := SizeOf(TfsnmSQLExecDirectReq) - sizeOf(TfsVarMsgField) + {!!.05}
  QueryLen + 1; {!!.05}
  FFGetZeroMem(Request, ReqLen);
  Try
    { Prepare the request. }
    Move(aQueryText^, Request^.Query, QueryLen);
    Request^.DatabaseID := pdSrDatabaseID;
    Request^.Timeout := aTimeout;
    Request^.OpenMode := aOpenMode;

    Result := pdClient.ProcessRequest(fsnmSQLExecDirect,
      pdTimeout,
      Request,
      ReqLen,
      nmdByteArray,
      Pointer(aStream),
      ReplyLen,
      nmdStream);

    { Was the execution successful? }
    If Result = DBIERR_NONE Then
      Begin
        { Yes. Get the cursorID from the stream & open a proxy cursor. }
        aStream.Position := 0;
        aStream.Read(SvrCursorID, sizeOf(SvrCursorID));
        If SvrCursorID <> 0 Then {!!.11}
          Result := QueryOpen(SvrCursorID, aOpenMode, smShared, aTimeout,
            aStream, aCursorID);
      End;

    { Assumption: Upper levels are responsible for Stream contents. }

  Finally
    FFFreeMem(Request, ReqLen);
  End;

End;
{----------}

Function TFSProxyDatabase.TableAddIndex(Const aCursorID: TffCursorID;
  Const aTableName: TfsTableName;
  Const aIndexDesc: TffIndexDescriptor
  ): TffResult;
Var
  Request: TfsnmAddIndexReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  If aCursorID > 0 Then
    Request.CursorID := TFSProxyCursor(aCursorID).SrCursorID
  Else
    Request.CursorID := 0;
  Request.TableName := aTableName;
  Request.IndexDesc := aIndexDesc;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmAddIndex,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyDatabase.TableBuild(aOverWrite: Boolean;
  Const aTableName: TfsTableName;
  aForServer: Boolean;
  aDictionary: TFSInfoDict
  ): TffResult;
Var
  Request: TMemoryStream;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request := TMemoryStream.Create;
  Try
    Request.Write(pdSrDatabaseID, SizeOf(pdSRDatabaseID)); {!!.10}
    Request.Write(aOverWrite, SizeOf(aOverWrite));
    Request.Write(aTableName, SizeOf(aTableName));
    aDictionary.WriteToStream(Request);

    Reply := Nil;
    Result := Client.ProcessRequest(fsnmBuildTable,
      Timeout,
      Request.Memory,
      Request.Size,
      nmdStream,
      Reply,
      ReplyLen,
      nmdByteArray);
    If Assigned(Reply) Then
      FFFreeMem(Reply, ReplyLen);
  Finally
    Request.Free;
  End;
End;
{----------}

Function TFSProxyDatabase.TableClose(aCursor: TFSProxyCursor): TffResult;
Begin
  Result := DBIERR_NONE;

  With pdTables.BeginWrite Do
    Try
      Delete(aCursor); {!!.01}
    Finally
      EndWrite;
    End;

  aCursor.Free;
  aCursor := Nil;
End;
{----------}

Function TFSProxyDatabase.TableDelete(Const aTableName: TfsTableName
  ): TffResult;
Var
  Request: TfsnmDeleteTableReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  Request.TableName := aTableName;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmDeleteTable,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyDatabase.TableDropIndex(aCursorID: TffCursorID;
  Const aTableName: TfsTableName;
  Const aIndexName: TffDictItemName;
  aIndexID: Longint): TffResult;
Var
  Request: TfsnmDropIndexReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  If aCursorID > 0 Then
    Request.CursorID := TFSProxyCursor(aCursorID).SrCursorID
  Else
    Request.CursorID := aCursorID;
  Request.TableName := aTableName;
  Request.IndexName := aIndexName;
  Request.IndexNumber := aIndexID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmDropIndex,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyDatabase.TableEmpty(aCursorID: TffCursorID;
  Const aTableName: TfsTableName): TffResult;
Var
  Request: TfsnmEmptyTableReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  If aCursorID > 0 Then
    Request.CursorID := TFSProxyCursor(aCursorID).SrCursorID
  Else
    Request.CursorID := aCursorID;
  Request.TableName := aTableName;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmEmptyTable,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyDatabase.TableGetDictionary(Const aTableName: TfsTableName;
  aForServer: Boolean;
  aStream: TStream
  ): TffResult;
Var
  Request: TfsnmGetTableDictionaryReq;
  ReplyLen: Longint;
Begin
  Assert(Assigned(aStream));
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  Request.TableName := FFExtractFileName(aTableName);

  aStream.Position := 0;
  Result := Client.ProcessRequest(fsnmGetTableDictionary,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(aStream),
    ReplyLen,
    nmdStream);
End;
{----------}

Function TFSProxyDatabase.TableExists(Const aTableName: TfsTableName;
  Var aExists: Boolean): TffResult;
Var
  Request: TfsnmDatabaseTableExistsReq;
  Reply: PfsnmDatabaseTableExistsRpy;
  ReplyLen: Longint;
Begin
  Request.DatabaseID := SrDatabaseID;
  Request.TableName := aTableName;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmDatabaseTableExists,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    aExists := Reply^.Exists;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyDatabase.TableList(Const aMask: TffFileNameExt;
  aList: TList): TffResult;
Var
  Request: TfsnmDatabaseTableListReq;
  ReplyLen: Longint;
  Stream: TStream;
  TableDescr: PffTableDescriptor;
  Count: Longint;
Begin
  Stream := TMemoryStream.Create;
  Try
    { Initialize Request }
    Request.DatabaseID := SrDatabaseID;
    Request.Mask := aMask;

    Result := Client.ProcessRequest(fsnmDatabaseTableList,
      Timeout,
      @Request,
      SizeOf(Request),
      nmdByteArray,
      Pointer(Stream),
      ReplyLen,
      nmdStream);

    If ResultOK(Result) Then
      Begin
        {Build the list}
        Stream.Position := 0;
        aList.Clear;

        For Count := 1 To (Stream.Size Div SizeOf(TfsTableDescriptor)) Do
          Begin
            FFGetMem(TableDescr, SizeOf(TfsTableDescriptor));
            Stream.Read(TableDescr^, SizeOf(TfsTableDescriptor));
            aList.Add(TableDescr);
          End;
      End;
  Finally
    Stream.Free;
  End;
End;

Function TFSProxyDatabase.TableLockedExclusive(Const aTableName: TfsTableName;
  Var aLocked: Boolean
  ): TffResult;
Var
  Request: TfsnmDatabaseTableLockedExclusiveReq;
  Reply: PfsnmDatabaseTableLockedExclusiveRpy;
  ReplyLen: Longint;
Begin
  Request.DatabaseID := SrDatabaseID;
  Request.TableName := aTableName;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmDatabaseTableLockedExclusive,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    aLocked := Reply^.Locked;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyDatabase.TableOpen(Const aTableName: TfsTableName;
  aForServer: Boolean;
  aIndexName: TffName;
  aIndexID: Longint;
  aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  aTimeout: Longint;
  Var aCursorID: TffCursorID;
  aStream: TStream): TffResult;
Var
  Cursor: TFSProxyCursor;
  ListItem: TfsIntListItem;
Begin
  Assert(Assigned(aStream));
  Cursor := Nil;
  Result := DBIERR_NONE;

  Try
    Cursor := TFSProxyCursor.Create(Self,
      0,
      aTableName,
      aForServer,
      aIndexName,
      aIndexID,
      aOpenMode,
      aShareMode,
      aTimeout,
      aStream);
  Except
    On E: Exception Do
      If (E Is EfsException) Or (E Is EfsDatabaseError) Then
        Result := EfsException(E).ErrorCode;
  End;

  If ResultOK(Result) And Assigned(Cursor) Then
    Begin
      ListItem := TfsIntListItem.Create(Longint(Cursor));
      ListItem.MaintainLinks := False; {!!.02}
      With pdTables.BeginWrite Do
        Try
          Insert(ListItem);
        Finally
          EndWrite;
        End;

      aCursorID := Longint(Cursor);
    End;
End;
{----------}

Function TFSProxyDatabase.TablePack(Const aTableName: TfsTableName;
  Var aRebuildID: Longint; UndeleteRecords: boolean; OnlyDeleted: boolean): TffResult;
Var
  Request: TfsnmPackTableReq;
  Reply: PfsnmPackTableRpy;
  ReplyLen: Longint;
Begin
  aRebuildID := -1;
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  Request.TableName := aTableName;
  request.UndeleteRecords := UndeleteRecords;
  request.OnlyDeleted := OnlyDeleted;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmPackTable,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);

  If ResultOK(Result) Then
    aRebuildID := Reply^.RebuildID;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyDatabase.TableRebuildIndex(Const aTableName: TfsTableName;
  Const aIndexName: TffName;
  aIndexID: Longint;
  Var aRebuildID: Longint
  ): TffResult;
Var
  Request: TfsnmReindexTableReq;
  Reply: PfsnmReindexTableRpy;
  ReplyLen: Longint;
Begin
  aRebuildID := -1;
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  Request.TableName := aTableName;
  Request.IndexName := aIndexName;
  Request.IndexNumber := aIndexID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmReindexTable,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    aRebuildID := Reply^.RebuildID;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyDatabase.TableRename(Const aOldName: TffName;
  Const aNewName: TffName): TffResult;
Var
  Request: TfsnmRenameTableReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  Request.OldTableName := aOldName;
  Request.NewTableName := aNewName;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmRenameTable,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyDatabase.TableRestructure(
  Const aTableName: TfsTableName;
  aDictionary: TFSInfoDict;
  aFieldMap: TFSSpecStringList;
  Var aRebuildID: Longint;
  aRangeError: boolean): TffResult;
Var
  I: Longint;
  NullByte: Byte;
  Request: TMemoryStream;
  Reply: PfsnmRestructureTableRpy;
  FieldMapEntry: TffShStr;
  ReplyLen: Longint;
Begin
  NullByte := 0;
  aRebuildID := -1;

  { Initialize Request }
  Request := TMemoryStream.Create;
  Try
    Request.Write(aRangeError, 2);
    Request.Write(SrDatabaseID, SizeOf(Longint));
    Request.Write(aTableName, SizeOf(aTableName));
    aDictionary.WriteToStream(Request);
    If Assigned(aFieldMap) Then
      For I := 0 To aFieldMap.Count - 1 Do
        Begin
          FieldMapEntry := aFieldMap[I];
          Request.Write(FieldMapEntry, Length(FieldMapEntry) + 1);
        End;
    Request.Write(NullByte, SizeOf(NullByte));

    Reply := Nil;
    Result := Client.ProcessRequest(fsnmRestructureTable,
      Timeout,
      Request.Memory,
      Request.Size,
      nmdByteArray,
      Pointer(Reply),
      ReplyLen,
      nmdByteArray);

    If ResultOK(Result) Then
      aRebuildID := Reply^.RebuildID;

    If Assigned(Reply) Then
      FFFreeMem(Reply, ReplyLen);

  Finally
    Request.Free;
  End;
End;
{----------}

Function TFSProxyDatabase.InTransaction(Var aTransLevel: Longint): TffResult;
Var
  Request: TfsnmInTransactionReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  aTransLevel := Request.TransLevel;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmInTransaction,
    pdTimeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;

Function TFSProxyDatabase.TransactionCorrupted: TffResult;
Var
  Request: TfsnmTransactionCorruptedReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmTransactionCorrupted,
    pdTimeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;

Function TFSProxyDatabase.TransactionCommit(aRemoveFile: Boolean = False): TffResult;
Var
  Request: TfsnmEndTransactionReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  Request.ToBeCommitted := True;
  Request.RemoveFiles := aRemoveFile;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmEndTransaction,
    pdTimeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyDatabase.TransactionRollback: TffResult;
Var
  Request: TfsnmEndTransactionReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  Request.ToBeCommitted := False;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmEndTransaction,
    pdTimeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyDatabase.TransactionStart(aFailSafe: Boolean): TffResult;
Var
  Request: TfsnmStartTransactionReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  Request.FailSafe := aFailSafe;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmStartTransaction,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
  Check(Result);
End;
{Start !!.10}
{----------}

Function TFSProxyDatabase.TransactionStartWith(Const aFailSafe: Boolean;
  Const aCursorIDs: TfsPointerList
  ): TffResult;
Var
  Reply: Pointer;
  Inx,
    aCount,
    ReplyLen: Longint;
  Request: TMemoryStream;
  Writer: TWriter;
Begin
  { Initialize Request }
  Request := TMemoryStream.Create;
  Writer := TWriter.Create(Request, 4096);
  Try
    Writer.WriteInteger(pdSrDatabaseID);
    Writer.WriteBoolean(aFailSafe);
    aCount := aCursorIDs.Count;
    Writer.WriteInteger(aCount);
    For Inx := 0 To Pred(aCount) Do
      { Get the cursorID of the proxy cursor. }
      Writer.WriteInteger(TFSProxyCursor(aCursorIDs[Inx]).SrCursorID);
    Writer.FlushBuffer;

    Reply := Nil;
    Result := Client.ProcessRequest(fsnmStartTransactionWith,
      Timeout,
      Request.Memory,
      Request.Size,
      nmdStream,
      Reply,
      ReplyLen,
      nmdByteArray);
  Finally
    Writer.Free;
    Request.Free;
    If Assigned(Reply) Then
      FFFreeMem(Reply, ReplyLen);
  End;
  //  Check(Result);                                                     {Deleted !!.11}
End;
{End !!.10}
{------------------------------------------------------------------------------}

{-TFSProxyCursor---------------------------------------------------------------}

Function TFSProxyCursor.BlobCreate(Var aBlobNr: TFFInt64): TffResult;
Var
  Request: TfsnmCreateBLOBReq;
  Reply: PfsnmCreateBLOBRpy;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmCreateBLOB,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);

  If ResultOK(Result) Then
    aBlobNr := Reply^.BLOBNr;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyCursor.BLOBDelete(aBlobNr: TFFInt64): TffResult;
Var
  Request: TfsnmDeleteBLOBReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.BLOBNr := aBlobNr;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmDeleteBLOB,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyCursor.BLOBFree(aBlobNr: TffInt64;
  aReadOnly: Boolean): TffResult;
Var
  Request: TfsnmFreeBLOBReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.BLOBNr := aBLOBNr;
  Request.ReadOnly := aReadOnly;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmFreeBLOB,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyCursor.BLOBGetLength(aBlobNr: TffInt64;
  Var aLength: Longint): TffResult;
Var
  Request: TfsnmGetBLOBLengthReq;
  Reply: PfsnmGetBLOBLengthRpy;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.BLOBNr := aBLOBNr;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmGetBLOBLength,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    aLength := Reply^.BLOBLength;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{Begin !!.03}
{----------}

Function TFSProxyCursor.BLOBListSegments(aBLOBNr: TffInt64;
  aStream: TStream): TffResult;
Var
  Request: TfsnmListBLOBSegmentsReq;
  ReplyLen: Longint;
Begin
  Request.CursorID := SrCursorID;
  Request.BLOBNr := aBLOBNr;
  Result := Client.ProcessRequest(fsnmListBLOBSegments,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(aStream),
    ReplyLen,
    nmdStream);

  If ResultOK(Result) Then
    aStream.Position := 0;
End;
{End !!.03}
{----------}

Function TFSProxyCursor.BLOBRead(aFieldNo: TffWord32;
  aBlobNr: TffInt64;
  aOffset: TffWord32; {!!.06}
  aLen: TffWord32; {!!.06}
  Var aBLOB;
  Var aBytesRead: TffWord32) {!!.06}
: TffResult;
Var
  Request: TfsnmReadBLOBReq;
  Reply: PfsnmReadBLOBRpy;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.BLOBNr := aBLOBNr;
  Request.Offset := aOffset;
  Request.Len := aLen;
  Request.FieldNo := aFieldNo;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmReadBLOB,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);

  If ResultOK(Result) Then
    Begin
      aBytesRead := Reply^.BytesRead;
      Move(Reply^.BLOB, aBLOB, aBytesRead);
    End;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyCursor.BLOBTruncate(aBlobNr: TffInt64;
  aBLOBLength: Longint): TffResult;
Var
  Request: TfsnmTruncateBLOBReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.BLOBNr := aBLOBNr;
  Request.BLOBLength := aBLOBLength;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmTruncateBLOB,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyCursor.BLOBWrite(aFieldNo: TffWord32;
  aBlobNr: TffInt64;
  aOffset: Longint;
  aLen: Longint;
  Var aBLOB): TffResult;
Var
  Request: PfsnmWriteBLOBReq;
  ReqLen: Longint;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  ReqLen := SizeOf(TfsnmWriteBLOBReq) - 2 + aLen;
  FFGetZeroMem(Request, ReqLen);
  Try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.BLOBNr := aBLOBNr;
    Request^.Offset := aOffSet;
    Request^.Len := aLen;
    Request^.FieldNo := aFieldNo;
    Move(aBLOB, Request^.BLOB, aLen);

    Reply := Nil;
    Result := Client.ProcessRequest(fsnmWriteBLOB,
      Timeout,
      Request,
      ReqLen,
      nmdByteArray,
      Reply,
      ReplyLen,
      nmdByteArray);
    If Assigned(Reply) Then
      FFFreeMem(Reply, ReplyLen);
  Finally
    FFFreeMem(Request, ReqLen);
  End;
End;
{----------}

Function TFSProxyCursor.CompareBookmarks(aBookmark1: PffByteArray;
  aBookmark2: PffByteArray;
  Var aCompResult: Longint): TffResult;
Var
  Request: PfsnmCursorCompareBMsReq;
  ReqLen: Longint;
  Reply: PfsnmCursorCompareBMsRpy;
  pBM2: Pointer;
  ReplyLen: Longint;
Begin
  ReqLen := SizeOf(TfsnmCursorCompareBMsReq) - 4 + (2 * BookmarkSize);
  FFGetZeroMem(Request, ReqLen);
  Try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.BookmarkSize := BookmarkSize;
    Move(aBookMark1^, Request^.Bookmark1, BookmarkSize);
    pBM2 := PffByteArray(PAnsiChar(@Request^.BookMark1) + BookmarkSize);
    Move(aBookMark2^, pBM2^, BookmarkSize);

    Reply := Nil;
    Result := Client.ProcessRequest(fsnmCursorCompareBMs,
      Timeout,
      Request,
      ReqLen,
      nmdByteArray,
      Pointer(Reply),
      ReplyLen,
      nmdByteArray);
    If ResultOK(Result) Then
      aCompResult := Reply^.CompareResult;

    If Assigned(Reply) Then
      FFFreeMem(Reply, ReplyLen);

  Finally
    FFFreeMem(Request, ReqLen);
  End;
End;
{----------}

Constructor TFSProxyCursor.Create(aDatabase: TFSProxyDatabase;
  aCursorID: TffCursorID;
  aTableName: String;
  aForServer: Boolean;
  aIndexName: String;
  aIndexID: Longint;
  aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  aTimeout: Longint;
  aStream: TStream);
Var
  Request: TfsnmOpenTableReq;
  ReplyLen: Longint;
  Result: TffResult;

Begin
  Inherited Create;

  prClient := aDatabase.Client;
  prDatabase := aDatabase;
  prSrCursorID := aCursorID;
  prTableName := aTableName;
  prForServer := aForServer;
  prDictionary := TFSInfoDict.Create(4096);
  prIndexName := aIndexName;
  prIndexID := aIndexID;
  prIsSQLCursor := False;
  prShareMode := aShareMode;
  prPhyRecSize := 0;
  prTimeout := aTimeout;

  If prSrCursorID <> 0 Then Exit; {CursorClone operation, nothing more to do}

  Assert(Assigned(aStream));
  If Not Assigned(fsExtraRecInfo) Then
    Begin
      ffgetmem(fsExtraRecInfo, SizeOf(TfsExtraRecInfo));
      fillchar(fsExtraRecInfo^, SizeOf(TfsExtraRecInfo), 0);
    End;

  { Initialize Request }
  Request.DatabaseID := Database.SrDatabaseID;
  Request.TableName := FFExtractTableName(aTableName);
  Request.IndexName := aIndexName;
  Request.IndexNumber := aIndexID;
  Request.OpenMode := aOpenMode;
  Request.ShareMode := aShareMode;
  Request.Timeout := prTimeout;

  Result := Client.ProcessRequest(fsnmOpenTable,
    prTimeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(aStream),
    ReplyLen,
    nmdStream);

  Check(Result);

  aStream.Position := 0;
  aStream.Read(prSrCursorID, SizeOf(prSrCursorID));

  {save the data dictionary for this table as well}

  Dictionary.ReadFromStream(aStream);
  aStream.Read(prIndexID, SizeOf(prIndexID));
  prIndexName := prDictionary.IndexName[prIndexID];
  prPhyRecSize := prDictionary.RecordLength;
End;
{----------}

Constructor TFSProxyCursor.CreateSQL(aDatabase: TFSProxyDatabase;
  aCursorID: TffCursorID;
  aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  aTimeout: Longint;
  aStream: TStream);
Begin
  Inherited Create;

  Assert(Assigned(aStream));

  prClient := aDatabase.Client;
  prDatabase := aDatabase;
  prTableName := '';
  prForServer := False;
  prDictionary := TFSInfoDict.Create(fscl_64k);
  prIsSQLCursor := True;
  prShareMode := aShareMode;
  prTimeout := aTimeout;

  aStream.Position := 0;
  aStream.Read(prSrCursorID, SizeOf(prSrCursorID));

  { Save the data dictionary for this table. }

  Dictionary.ReadFromStream(aStream);
  //Dictionary.UserName:= aDatabase.Session.Client.UserName;
//  aStream.Read(prIndexID, SizeOf(prIndexID));                        {Deleted !!.10}
  prIndexID := 0; {!!.10}
  prIndexName := prDictionary.IndexName[0]; {!!.10}
  prPhyRecSize := prDictionary.RecordLength;
End;
{----------}

Function TFSProxyCursor.CursorClone(aOpenMode: TFFOpenMode;
  Var aNewCursorID: TFFCursorID): TffResult;
Var
  Request: TfsnmCursorCloneReq;
  Reply: PfsnmCursorCloneRpy;
  ReplyLen: Longint;
  NewCursor: TFSProxyCursor;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.OpenMode := aOpenMode;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmCursorClone,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    Begin
      {Create a new proxy cursor with the appropriate information}
      NewCursor := TFSProxyCursor.Create(prDatabase,
        Reply^.CursorID,
        '' {tableName},
        False, {forserver}
        prIndexName,
        prIndexID,
        aOpenMode,
        smShared, {share mode}
        prTimeout,
        Nil);
      NewCursor.prDictionary.Assign(prDictionary);
      NewCursor.prIndexName := prIndexName;
      NewCursor.prPhyRecSize := NewCursor.prDictionary.RecordLength;
      aNewCursorID := Longint(NewCursor);
    End;
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Destructor TFSProxyCursor.Destroy;
Var
  Request: TfsnmCursorCloseReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  If Assigned(fsExtraRecInfo) Then
    fffreemem(fsExtraRecInfo, SizeOf(TfsExtraRecInfo));
  If SrCursorID > 0 Then
    Begin
      { Initialize Request }
      Request.CursorID := SrCursorID;

      Reply := Nil;
      Client.ProcessRequest(fsnmCursorClose,
        Timeout,
        @Request,
        SizeOf(Request),
        nmdByteArray,
        Reply,
        ReplyLen,
        nmdByteArray);

      If Assigned(Reply) Then
        FFFreeMem(Reply, ReplyLen);
    End;

  prSrCursorID := 0;
  prDictionary.Free;
  prDictionary := Nil;
  prDatabase := Nil;
  prClient := Nil;

  Inherited Destroy;
End;
{----------}

Function TFSProxyCursor.FileBLOBAdd(Const aFileName: TffFullFileName;
  Var aBlobNr: TffInt64): TffResult;
Var
  Request: TfsnmAddFileBLOBReq;
  Reply: PfsnmAddFileBLOBRpy;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.FileName := aFileName;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmAddFileBLOB,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);

  If ResultOK(Result) Then
    aBlobNr := Reply^.BLOBNr;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{Begin !!.02}
{----------}

Function TFSProxyCursor.CopyRecords(aSrcCursor: TFSProxyCursor;
  aCopyBLOBs: Boolean; CountPerTrans: Longint): TffResult;
Var
  Request: TfsnmCursorCopyRecordsReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin

  { Initialize Request }
  Request.SrcCursorID := aSrcCursor.SrCursorID;
  Request.DestCursorID := SrCursorID;
  Request.CopyBLOBs := aCopyBLOBs;
  Request.CountPerTrans := CountPerTrans;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmCursorCopyRecords,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{End !!.02}
{Begin !!.06}
{----------}

Function TFSProxyCursor.DeleteRecords(CountPerTrans: Longint): TffResult;
Var
  Request: TfsnmCursorDeleteRecordsReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin

  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.CountPerTrans := CountPerTrans;
  Reply := Nil;
  Result := Client.ProcessRequest(fsnmCursorDeleteRecords,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{End !!.06}
{----------}

Function TFSProxyCursor.GetBookmark(aBookmark: PffByteArray): TffResult;
Var
  Request: TfsnmCursorGetBookMarkReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.BookMarkSize := BookMarkSize;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmCursorGetBookMark,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);

  If ResultOK(Result) Then
    Move(Reply^, aBookmark^, BookmarkSize); {!!.05}

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyCursor.GetBookmarkSize(Var aSize: Longint): TffResult;
Begin
  Result := DBIERR_NONE;
  If prIsSQLCursor Then
    aSize := fscl_FixedBookmarkSize
  Else
    aSize := fscl_FixedBookmarkSize + Dictionary.IndexKeyLength[IndexID];
End;
{----------}

Function TFSProxyCursor.prGetBookmarkSize: Longint;
Begin
  GetBookmarkSize(Result);
End;
{----------}

Function TFSProxyCursor.RecordDelete(aData: PffByteArray): TffResult;
Var
  Request: TfsnmRecordDeleteReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  If aData = Nil Then
    Request.RecLen := 0
  Else
    Request.RecLen := PhysicalRecordSize;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmRecordDelete,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If ((ResultOK(Result)) And {!!.06}
    (Assigned(aData))) Then {!!.06}
    Move(Reply^, aData^, ReplyLen);

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyCursor.RecordDeleteBatch(aBMCount: Longint;
  aBMLen: Longint;
  aData: PffByteArray;
  aErrors: PffLongintArray
  ): TffResult;
Var
  Request: PfsnmRecordDeleteBatchReq;
  MaxRecs: Longint;
  ReqLen: Longint;
  iErr: Longint;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  MaxRecs := 65500 Div aBMLen;
  If aBMCount > MaxRecs Then
    Begin
      Result := DBIERR_ROWFETCHLIMIT;
      Exit;
    End;
  ReqLen := SizeOf(Request^) - 2 + (aBMLen * aBMCount);
  FFGetZeroMem(Request, ReqLen);
  Try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.BMLen := aBMLen;
    Request^.BMCount := aBMCount;
    Move(aData^, Request^.BMArray, aBMCount * aBMLen);

    Reply := Nil;
    Result := Client.ProcessRequest(fsnmRecordDeleteBatch,
      Timeout,
      Request,
      ReqLen,
      nmdByteArray,
      Reply,
      ReplyLen,
      nmdByteArray);
    If ResultOK(Result) Then
      Begin
        Move(Reply^, aErrors^, ReplyLen);
        For iErr := 0 To Pred(aBMCount) Do
          If aErrors^[iErr] <> DBIERR_NONE Then
            Begin
              Result := aErrors^[iErr];
              Break;
            End;
      End;

    If Assigned(Reply) Then
      FFFreeMem(Reply, ReplyLen);

  Finally
    FFFreeMem(Request, ReqLen);
  End;
End;
{----------}

Function TFSProxyCursor.RecordExtractKey(aData: PffByteArray;
  aKey: PffByteArray): TffResult;
Var
  Request: PfsnmRecordExtractKeyReq;
  ReqLen: Longint;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  ReqLen := SizeOf(TfsnmRecordExtractKeyReq) - 2 + PhysicalRecordSize;
  FFGetZeroMem(Request, ReqLen);
  Try
    { Initialize Request}
    Request^.CursorID := SrCursorID;
    Request^.KeyLen := Dictionary.IndexKeyLength[IndexID];
    If aData = Nil Then
      Request^.ForCurrentRecord := True
    Else
      Begin
        Move(aData^, Request^.Data, PhysicalRecordSize);
        Request^.ForCurrentRecord := False;
      End;

    Reply := Nil;
    Result := Client.ProcessRequest(fsnmRecordExtractKey,
      Timeout,
      Request,
      ReqLen,
      nmdByteArray,
      Reply,
      ReplyLen,
      nmdByteArray);
    If ((ResultOK(Result)) And {!!.06}
      (Assigned(aKey))) Then {!!.06}
      Move(Reply^, aKey^, ReplyLen);

    If Assigned(Reply) Then
      FFFreeMem(Reply, ReplyLen);
  Finally
    FFFreeMem(Request, ReqLen); {!!.06}
  End;
End;
{----------}

Function TFSProxyCursor.RecordGet(aLockType: TffLockType; aUserLockType: TfsUserRecLocking;
  aData: PffByteArray; Var aFlag: Byte; Var aRefNr: TffInt64; Const aUser: Boolean)
  : TffResult;
Var
  Request: TfsnmRecordGetReq;
  Reply: Pointer;
  ReplyLen: TffMemSize;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.LockType := aLockType;
  Request.UserLockType := aUserLockType;
  Request.RecLen := PhysicalRecordSize; {server needs it no matter what}
  Request.BookMarkSize := BookMarkSize;
  Request.aUser:= aUser;
  If (aData = Nil) Then
    ReplyLen := 0
  Else
    ReplyLen := Request.RecLen;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmRecordGet,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If ((Assigned(Reply)) And
    (Assigned(aData))) Then
    Begin
      Move(Reply^, aData^, ReplyLen - SizeOf(TfsExtraRecInfo));
      If Assigned(fsExtraRecInfo) Then
        Begin
          Move(pffByteArray(@Reply^)[ReplyLen - SizeOf(TfsExtraRecInfo)], fsExtraRecInfo^, SizeOf(TfsExtraRecInfo));
          aFlag := fsExtraRecInfo^.eFlagRowInfo;
          aRefNr := fsExtraRecInfo^.eRefNr;
        End;
      FFFreeMem(Reply, ReplyLen);
    End;
End;
{----------}

Function TFSProxyCursor.RecordGetBatch(aRecCount: Longint;
  aRecLen: Longint;
  Var aRecRead: Longint;
  aData: PffByteArray;
  Var aError: TffResult): TffResult;
Var
  Request: TfsnmRecordGetBatchReq;
  Reply: PfsnmRecordGetBatchRpy;
  ReplyLen: Longint;
Begin
  aRecRead := 0;
  ReplyLen := SizeOf(Reply^) - 2 + (aRecLen * aRecCount);
  Request.CursorID := SrCursorID;
  Request.RecLen := aRecLen;
  Request.RecCount := aRecCount;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmRecordGetBatch,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    Begin
      aRecRead := Reply^.RecCount;
      Move(Reply^.RecArray, aData^, aRecRead * aRecLen);
      aError := Reply^.Error;
    End;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyCursor.RecordGetForKey(aDirectKey: Boolean;
  aFieldCount: Longint;
  aPartialLen: Longint;
  aKeyData: PffByteArray;
  aData: PffByteArray;
  aFirstCall: Boolean): TffResult;
Var
  Request: PfsnmRecordGetForKeyReq;
  ReqLen: Longint;
  Reply: Pointer;
  RpyLen: Longint;
  DataLen: Longint;
  DictRecLen: Longint;
Begin
  DictRecLen := PhysicalRecordSize;
  If aDirectKey Then
    DataLen := Dictionary.IndexKeyLength[IndexID]
  Else
    DataLen := DictRecLen;
  ReqLen := SizeOf(TfsnmRecordGetForKeyReq) - 2 + DataLen;
  FFGetZeroMem(Request, ReqLen);
  If (aData = Nil) Then
    RpyLen := 0
  Else
    RpyLen := DictRecLen;
  Try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.BookMarkSize := BookMarkSize;
    Request^.DirectKey := aDirectKey;
    Request^.FieldCount := aFieldCount;
    Request^.PartialLen := aPartialLen;
    Request^.RecLen := DictRecLen;
    Request^.KeyDataLen := DataLen;
    Move(aKeyData^, Request^.KeyData, DataLen);

    Reply := Nil;
    Result := Client.ProcessRequest(fsnmRecordGetForKey,
      Timeout,
      Request,
      ReqLen,
      nmdByteArray,
      Reply,
      RpyLen,
      nmdByteArray);

    If ((Assigned(Reply)) And {!!.06}
      (Assigned(aData))) Then
      Begin {!!.06}
        Move(Reply^, aData^, RpyLen);
        FFFreeMem(Reply, RpyLen);
      End;
  Finally
    FFFreeMem(Request, ReqLen);
  End;
End;
{----------}

Function TFSProxyCursor.GetSetPosition(aValue: Longint; aData: PffByteArray; aLockType: TffLockType;
  Var aFlag: Byte; Var aRecNo: Longword;
  Var aRefNr: TffInt64;
  aInfoGetSetPosition: TInfoGetSetPosition;
  aSet: Boolean): TffResult;
Var
  Request: TfsnmRecordGetSetPositionReq;
  ReplyLen: Longint;
  Reply: Pointer;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.LockType := aLockType;
  Request.Value := aValue;
  Request.InfoGetSetPosition:= aInfoGetSetPosition;
  Request.aSet:= aSet;

  If (aData <> Nil) Then
    Begin
      Request.RecLen := PhysicalRecordSize;
      Request.BookMarkSize := BookMarkSize;
    End
  Else
    Begin
      Request.RecLen := 0;
      Request.BookMarkSize := 0;
    End;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmRecordGetSetPosition,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    Begin
      Move(Reply^, aData^, ReplyLen - SizeOf(TfsExtraRecInfo));
      If Assigned(fsExtraRecInfo) Then
        Begin
          Move(pffByteArray(@Reply^)[ReplyLen - SizeOf(TfsExtraRecInfo)], fsExtraRecInfo^, SizeOf(TfsExtraRecInfo));
          aRefNr := fsExtraRecInfo^.eRefNr;
          aFlag := fsExtraRecInfo^.eFlagRowInfo;
          aRecNo:= fsExtraRecInfo^.eRecNo;
        End;
      FFFreeMem(Reply, ReplyLen);
    End;

End;

Function TFSProxyCursor.RecordGetNext(aLockType: TffLockType;
  aData: PffByteArray; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Var
  Request: TfsnmRecordGetNextReq;
  ReplyLen: Longint;
  Reply: Pointer;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.LockType := aLockType;
  If (aData <> Nil) Then
    Begin
      Request.RecLen := PhysicalRecordSize;
      Request.BookMarkSize := BookMarkSize;
    End
  Else
    Begin
      Request.RecLen := 0;
      Request.BookMarkSize := 0;
    End;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmRecordGetNext,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    Begin
      Move(Reply^, aData^, ReplyLen - SizeOf(TfsExtraRecInfo));
      If Assigned(fsExtraRecInfo) Then
        Begin
          Move(pffByteArray(@Reply^)[ReplyLen - SizeOf(TfsExtraRecInfo)], fsExtraRecInfo^, SizeOf(TfsExtraRecInfo));
          aFlag := fsExtraRecInfo^.eFlagRowInfo;
          aRefNr := fsExtraRecInfo^.eRefNr;
        End;
      FFFreeMem(Reply, ReplyLen);
    End;
End;
{----------}

Function TFSProxyCursor.RecordGetPrior(aLockType: TffLockType;
  aData: PffByteArray; Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Var
  Request: TfsnmRecordGetPrevReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.LockType := aLockType;

  If (aData <> Nil) Then
    Begin
      Request.RecLen := PhysicalRecordSize;
      Request.BookMarkSize := BookMarkSize;
    End
  Else
    Begin
      Request.RecLen := 0;
      Request.BookMarkSize := 0;
    End;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmRecordGetPrev,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);

  If Assigned(Reply) Then
    Begin
      Move(Reply^, aData^, ReplyLen - SizeOf(TfsExtraRecInfo));
      If Assigned(fsExtraRecInfo) Then
        Begin
          Move(pffByteArray(@Reply^)[ReplyLen - SizeOf(TfsExtraRecInfo)], fsExtraRecInfo^, SizeOf(TfsExtraRecInfo));
          aFlag := fsExtraRecInfo^.eFlagRowInfo;
          aRefNr := fsExtraRecInfo^.eRefNr;
        End;
      FFFreeMem(Reply, ReplyLen);
    End;
End;
{----------}

Function TFSProxyCursor.RecordInsert(aLockType: TffLockType;
  aData: PffByteArray; aUndelete: Boolean; Var aRefNr: TffInt64): TffResult;
Var
  Request: PfsnmRecordInsertReq;
  ReqLen: Longint;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  ReqLen := SizeOf(Request^) - 2 + PhysicalRecordSize;
  FFGetZeroMem(Request, ReqLen);
  Try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.LockType := aLockType;
    Request^.RecLen := PhysicalRecordSize;
    Request^.BookMarkSize := BookMarkSize;
    Request^.Undelete := aUndelete;
    Move(aData^, Request^.Data, PhysicalRecordSize);

    Reply := Nil;
    Result := Client.ProcessRequest(fsnmRecordInsert,
      Timeout,
      Request,
      ReqLen,
      nmdByteArray,
      Reply,
      ReplyLen,
      nmdByteArray);
    If Assigned(Reply) Then
      Begin
        If Assigned(fsExtraRecInfo) Then
          Begin
            Move(pffByteArray(@Reply^)[ReplyLen - SizeOf(TfsExtraRecInfo)], fsExtraRecInfo^, SizeOf(TfsExtraRecInfo));
            aRefNr := fsExtraRecInfo^.eRefNr;
          End;

        FFFreeMem(Reply, ReplyLen);
      End;
  Finally
    FFFreeMem(Request, ReqLen);
  End;
End;
{----------}

Function TFSProxyCursor.RecordInsertBatch(aRecCount: Longint;
  aRecLen: Longint;
  aData: PffByteArray;
  aErrors: PffLongintArray
  ): TffResult;
Var
  Request: PfsnmRecordInsertBatchReq;
  MaxRecs: Longint;
  ReqLen: Longint;
  iErr: Longint;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  MaxRecs := 65000 Div aRecLen;
  If aRecCount > MaxRecs Then
    Begin
      Result := DBIERR_ROWFETCHLIMIT;
      Exit;
    End;
  ReqLen := SizeOf(Request^) - 2 + (aRecLen * aRecCount);
  FFGetZeroMem(Request, ReqLen);
  Try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.RecLen := aRecLen;
    Request^.RecCount := aRecCount;
    Move(aData^, Request^.RecArray, aRecCount * aRecLen);

    Reply := Nil;
    Result := Client.ProcessRequest(fsnmRecordInsertBatch,
      Timeout,
      Request,
      ReqLen,
      nmdByteArray,
      Reply,
      ReplyLen,
      nmdByteArray);
    If ResultOK(Result) Then
      Begin
        Move(Reply^, aErrors^, ReplyLen);
        For iErr := 0 To Pred(aRecCount) Do
          If aErrors^[iErr] <> DBIERR_NONE Then
            Begin
              Result := aErrors^[iErr];
              Break;
            End;
      End;

    If Assigned(Reply) Then
      FFFreeMem(Reply, ReplyLen);

  Finally
    FFFreeMem(Request, ReqLen);
  End;
End;
{----------}

Function TFSProxyCursor.RecordIsLocked(aLockType: TffLockType;
  Var aIsLocked: boolean): TffResult;
Var
  Request: TfsnmRecordIsLockedReq;
  Reply: PfsnmRecordIsLockedRpy;
  ReplyLen: Longint;
Begin
  Request.CursorID := SrCursorID;
  Request.LockType := aLockType;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmRecordIsLocked,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    aIsLocked := Reply^.IsLocked;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyCursor.RecordModify(aData: PffByteArray;
  aRelLock: Boolean; aUserLockType: TfsUserRecLocking; aFlag: Byte; aSet, Use: Boolean): TffResult;
Var
  Request: PfsnmRecordModifyReq;
  ReqLen: Longint;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  ReqLen := SizeOf(Request^) - 2 + PhysicalRecordSize;
  FFGetZeroMem(Request, ReqLen);
  Try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.RelLock := aRelLock;
    Request^.aFlag := aFlag;
    Request^.aSet := aSet;
    Request^.aUse := Use;
    Request^.RecLen := PhysicalRecordSize;
    Request^.BookMarkSize := BookMarkSize;
    Request^.UserLockType := aUserLockType;
    Move(aData^, Request^.Data, PhysicalRecordSize);

    Reply := Nil;
    Result := Client.ProcessRequest(fsnmRecordModify,
      Timeout,
      Request,
      ReqLen,
      nmdByteArray,
      Reply,
      ReplyLen,
      nmdByteArray);
    If Assigned(Reply) Then
      FFFreeMem(Reply, ReplyLen);
  Finally
    FFFreeMem(Request, ReqLen);
  End;
End;
{----------}

Function TFSProxyCursor.RecordRelLock(aAllLocks: Boolean): TffResult;
Var
  Request: TfsnmRecordRelLockReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.AllLocks := aAllLocks;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmRecordRelLock,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyCursor.TableGetAutoInc(Var aValue: Int64; Var aStep: Longint): TffResult;
Var
  Request: TfsnmGetTableAutoIncValueReq;
  Reply: PfsnmGetTableAutoIncValueRpy;
  ReplyLen: Longint;
Begin
  Request.CursorID := SrCursorID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmGetTableAutoIncValue,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    Begin
      aValue := Reply^.AutoInc64Value;
      aStep := Reply^.AutoInc64StepValue;
    End;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;

Function TFSProxyCursor.TableGetMaxRecords(Var aValue: Longword): TffResult;
Var
  Request: TfsnmGetTableMaxRecordsValueReq;
  Reply: PfsnmGetTableMaxRecordsValueRpy;
  ReplyLen: Longint;
Begin
  Request.CursorID := SrCursorID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmGetTableMaxRecordsValue,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    Begin
      aValue := Reply^.MaxRecords;
    End;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;

Function TFSProxyCursor.TableGetTableFlags(Var aValue: Word): TffResult;
Var
  Request: TfsnmGetTableTableFlagsValueReq;
  Reply: PfsnmGetTableTableFlagsValueRpy;
  ReplyLen: Longint;
Begin
  Request.CursorID := SrCursorID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmGetTableTableFlagsValue,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    Begin
      aValue := Reply^.TableFlags;
    End;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;

Function TFSProxyCursor.TableGetTablePassword(Var aValue: Longword): TffResult;
Var
  Request: TfsnmGetTableTablePasswordValueReq;
  Reply: PfsnmGetTableTablePasswordValueRpy;
  ReplyLen: Longint;
Begin
  Request.CursorID := SrCursorID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmGetTableTablePasswordValue,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    Begin
      aValue := Reply^.TablePassword;
    End;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;

Function TFSProxyCursor.TableGetTablePasswordRest(Var aValue: Longword): TffResult;
Var
  Request: TfsnmGetTableTablePasswordRestValueReq;
  Reply: PfsnmGetTableTablePasswordRestValueRpy;
  ReplyLen: Longint;
Begin
  Request.CursorID := SrCursorID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmGetTableTablePasswordRestValue,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    Begin
      aValue := Reply^.TablePassword;
    End;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;

Function TFSProxyCursor.TableGetTableDBID(Var aValue: Longword): TffResult;
Var
  Request: TfsnmGetTableTableDBIDValueReq;
  Reply: PfsnmGetTableTableDBIDValueRpy;
  ReplyLen: Longint;
Begin
  Request.CursorID := SrCursorID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmGetTableTableDBIDValue,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    Begin
      aValue := Reply^.TableDBID;
    End;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;

{Begin !!.03}
{----------}

Function TFSProxyCursor.ListBLOBFreeSpace(Const aInMemory: Boolean;
  aStream: TStream): TffResult;
Var
  Request: TfsnmGetBLOBFreeSpaceReq;
  ReplyLen: Longint;
Begin
  Request.CursorID := SrCursorID;
  Request.InMemory := aInMemory;
  Result := Client.ProcessRequest(fsnmListBLOBFreeSpace,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(aStream),
    ReplyLen,
    nmdStream);

  If ResultOK(Result) Then
    aStream.Position := 0;
End;
{End !!.03}
{----------}

Function TFSProxyCursor.OverrideFilter(aExpression: pCANExpr;
  aTimeout: TffWord32): TffResult;
Var
  ReqSize: Longint;
  Request: PfsnmCursorOverrideFilterReq;
  ExprTree: CANExpr;
  Reply: Pointer;
  ReplyLen: Longint;
Begin

  If Not Assigned(aExpression) Then
    Begin
      aExpression := @ExprTree;
      FillChar(ExprTree, SizeOf(ExprTree), 0);
      ExprTree.iVer := CANEXPRVERSION;
      ExprTree.iTotalSize := SizeOf(ExprTree);
    End;

  ReqSize := (SizeOf(Request^) - 2 + aExpression^.iTotalSize);

  FFGetMem(Request, ReqSize);
  Try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.Timeout := aTimeout;

    Move(aExpression^, Request^.ExprTree, aExpression^.iTotalSize);

    Reply := Nil;
    Result := Client.ProcessRequest(fsnmCursorOverrideFilter,
      Timeout,
      Pointer(Request),
      ReqSize,
      nmdByteArray,
      Reply,
      ReplyLen,
      nmdByteArray);
    If Assigned(Reply) Then
      FFFreeMem(Reply, ReplyLen);
  Finally
    FFFreeMem(Request, ReqSize);
  End;
End;
{----------}

Function TFSProxyCursor.ResetRange: TffResult;
Var
  Request: TfsnmCursorResetRangeReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmCursorResetRange,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);

End;
{----------}

Function TFSProxyCursor.RestoreFilter: TffResult;
Var
  Request: TfsnmCursorRestoreFilterReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmCursorRestoreFilter,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);

End;
{----------}

Function TFSProxyCursor.SetFilter(aExpression: pCANExpr;
  aTimeout: TffWord32): TffResult;
Var
  ReqSize: Longint;
  Request: PfsnmCursorSetFilterReq;
  ExprTree: CANExpr;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  If Not Assigned(aExpression) Then
    Begin
      aExpression := @ExprTree;
      FillChar(ExprTree, SizeOf(ExprTree), 0);
      ExprTree.iVer := CANEXPRVERSION;
      ExprTree.iTotalSize := SizeOf(ExprTree);
    End;

  ReqSize := (SizeOf(Request^) - 2 + aExpression^.iTotalSize);

  FFGetMem(Request, ReqSize);
  Try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.Timeout := aTimeout;

    Move(aExpression^, Request^.ExprTree, aExpression^.iTotalSize);

    Reply := Nil;
    Result := Client.ProcessRequest(fsnmCursorSetFilter,
      Timeout,
      Pointer(Request),
      ReqSize,
      nmdByteArray,
      Reply,
      ReplyLen,
      nmdByteArray);
    If Assigned(Reply) Then
      FFFreeMem(Reply, ReplyLen);
  Finally
    FFFreeMem(Request, ReqSize);
  End;
End;
{----------}

Function TFSProxyCursor.SetRange(aDirectKey: Boolean;
  aFieldCount1: Longint;
  aPartialLen1: Longint;
  aKeyData1: PffByteArray;
  aKeyIncl1: Boolean;
  aFieldCount2: Longint;
  aPartialLen2: Longint;
  aKeyData2: PffByteArray;
  aKeyIncl2: Boolean): TffResult;
Var
  Request: PfsnmCursorSetRangeReq;
  ReqLen: Longint;
  KeyLen1: Longint;
  KeyLen2: Longint;
  Reply: Pointer;
  ReplyLen: Longint;
  ReqKeyData2: pointer;
Begin
  {calculate sizes}
  If aKeyData1 = Nil Then
    KeyLen1 := 0
  Else If aDirectKey Then
    KeyLen1 := Dictionary.IndexKeyLength[IndexID]
  Else
    KeyLen1 := PhysicalRecordSize;
  If aKeyData2 = Nil Then
    KeyLen2 := 0
  Else If aDirectKey Then
    KeyLen2 := Dictionary.IndexKeyLength[IndexID]
  Else
    KeyLen2 := PhysicalRecordSize;

  {now, we know how large the Request is}
  ReqLen := SizeOf(Request^) - 4 + KeyLen1 + KeyLen2;

  {allocate and clear it}
  FFGetZeroMem(Request, ReqLen);
  Try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.DirectKey := aDirectKey;
    Request^.FieldCount1 := aFieldCount1;
    Request^.PartialLen1 := aPartialLen1;
    Request^.KeyLen1 := KeyLen1;
    Request^.KeyIncl1 := aKeyIncl1;
    Request^.FieldCount2 := aFieldCount2;
    Request^.PartialLen2 := aPartialLen2;
    Request^.KeyLen2 := KeyLen2;
    Request^.KeyIncl2 := aKeyIncl2;
    Move(aKeyData1^, Request^.KeyData1, KeyLen1);
    ReqKeyData2 := PffByteArray(PAnsiChar(@Request^.KeyData1) + KeyLen1);
    Move(akeyData2^, ReqKeyData2^, KeyLen2);

    Reply := Nil;
    Result := Client.ProcessRequest(fsnmCursorSetRange,
      Timeout,
      Request,
      ReqLen,
      nmdByteArray,
      Reply,
      ReplyLen,
      nmdByteArray);
    If Assigned(Reply) Then
      FFFreeMem(Reply, ReplyLen);
  Finally
    FFFreeMem(Request, ReqLen);
  End;
End;
{----------}

Function TFSProxyCursor.SetTimeout(aTimeout: Longint): TffResult;
Var
  Request: TfsnmCursorSetTimeoutReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  Result := DBIERR_NONE;
  If prTimeout = aTimeout Then Exit;

  prTimeout := aTimeout;

  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.Timeout := prTimeout;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmCursorSetTimeout,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyCursor.SetToBegin: TffResult;
Var
  Request: TfsnmCursorSetToBeginReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmCursorSetToBegin,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyCursor.SetToBookmark(aBookmark: PffByteArray): TffResult;
Var
  Request: PfsnmCursorSetToBookmarkReq;
  ReqLen: Longint;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  ReqLen := SizeOf(Request^) - 2 + BookMarkSize;
  FFGetZeroMem(Request, ReqLen);
  Try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.BookmarkSize := BookMarkSize;
    Move(aBookmark^, Request^.Bookmark, BookMarkSize);

    Reply := Nil;
    Result := Client.ProcessRequest(fsnmCursorSetToBookmark,
      Timeout,
      Request,
      ReqLen,
      nmdByteArray,
      Reply,
      ReplyLen,
      nmdByteArray);
    If Assigned(Reply) Then
      FFFreeMem(Reply, ReplyLen);
  Finally
    FFFreeMem(Request, ReqLen);
  End;
End;
{----------}

Function TFSProxyCursor.SetToCursor(aSourceCursor: TFSProxyCursor
  ): TffResult;
Var
  Request: TfsnmCursorSetToCursorReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.DestCursorID := SrCursorID;
  Request.SrcCursorID := aSourceCursor.SrCursorID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmCursorSetToCursor,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyCursor.SetToEnd: TffResult;
Var
  Request: TfsnmCursorSetToEndReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmCursorSetToEnd,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyCursor.SetToKey(aSearchAction: TffSearchKeyAction;
  aDirectKey: Boolean;
  aFieldCount: Longint;
  aPartialLen: Longint;
  aKeyData: PffByteArray): TffResult;
Var
  Request: PfsnmCursorSetToKeyReq;
  ReqLen: Longint;
  KeyDataLen: Longint;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  If aDirectKey Then
    KeyDataLen := Dictionary.IndexKeyLength[IndexID]
  Else
    KeyDataLen := PhysicalRecordSize;
  ReqLen := SizeOf(TfsnmCursorSetToKeyReq) - 2 + KeyDataLen;
  FFGetZeroMem(Request, ReqLen);
  Try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.Action := aSearchAction;
    Request^.DirectKey := aDirectKey;
    Request^.FieldCount := aFieldCount;
    Request^.PartialLen := aPartialLen;
    Request^.KeyDataLen := KeyDataLen;
    Move(aKeyData^, Request^.KeyData, KeyDataLen);

    Reply := Nil;
    Result := Client.ProcessRequest(fsnmCursorSetToKey,
      Timeout,
      Pointer(Request),
      ReqLen,
      nmdByteArray,
      Reply,
      ReplyLen,
      nmdByteArray);
    If Assigned(Reply) Then
      FFFreeMem(Reply, ReplyLen);
  Finally
    FFFreeMem(Request, ReqLen);
  End;
End;
{----------}

Function TFSProxyCursor.SwitchToIndex(aIndexName: TffDictItemName;
  aIndexID: Longint;
  aPosnOnRec: Boolean): TffResult;
Var
  Request: TfsnmCursorSwitchToIndexReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.IndexName := aIndexName;
  Request.IndexNumber := aIndexID;
  Request.PosnOnRec := aPosnOnRec;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmCursorSwitchToIndex,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);

  If (Request.IndexName <> '') Then
    Begin
      prIndexID := Dictionary.GetIndexFromName(Request.IndexName);
      prIndexName := aIndexName;
    End
  Else
    Begin
      prIndexID := aIndexID;
      prIndexName := Dictionary.IndexName[aIndexID];
    End;
End;
{----------}

Function TFSProxyCursor.TableGetRecCount(Var aRecCount: Longword): TffResult;
Var
  Request: TfsnmGetTableRecCountReq;
  Reply: PfsnmGetTableRecCountRpy;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmGetTableRecCount,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    aRecCount := Reply^.RecCount;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{Begin !!.07}
{----------}

Function TFSProxyCursor.TableGetRecCountAsync(Var aTaskID: Longint): TffResult;
Var
  Request: TfsnmGetTableRecCountAsyncReq;
  Reply: PfsnmGetTableRecCountAsyncRpy;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmGetTableRecCountAsync,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    aTaskID := Reply^.RebuildID;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{End !!.07}
{----------}

Function TFSProxyCursor.TableIsLocked(aLockType: TffLockType;
  Var aIsLocked: Boolean): TffResult;
Var
  Request: TfsnmIsTableLockedReq;
  Reply: PfsnmIsTableLockedRpy;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.LockType := aLockType;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmIsTableLocked,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);
  If ResultOK(Result) Then
    aIsLocked := Reply^.IsLocked;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyCursor.TableLockAcquire(aLockType: TffLockType): TffResult;
Var
  Request: TfsnmAcqTableLockReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialzie Request }
  Request.CursorID := SrCursorID;
  Request.LockType := aLockType;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmAcqTableLock,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyCursor.TableLockRelease(aAllLocks: Boolean): TffResult;
Var
  Request: TfsnmRelTableLockReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.AllLocks := aAllLocks;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmRelTableLock,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;
{----------}

Function TFSProxyCursor.TableSetAutoInc(aValue: Int64; aStep: Longint): TffResult;
Var
  Request: TfsnmSetTableAutoIncValueReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.AutoInc64Value := aValue;
  Request.AutoInc64StepValue := aStep;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmSetTableAutoIncValue,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;

Function TFSProxyCursor.TableSetMaxRecords(aValue: Longint): TffResult;
Var
  Request: TfsnmSetTableMaxRecordsValueReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.MaxRecords := aValue;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmSetTableMaxRecordsValue,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;

Function TFSProxyCursor.TableSetTableFlags(aValue: Word): TffResult;
Var
  Request: TfsnmSetTableTableFlagsValueReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.TableFlags := aValue;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmSetTableTableFlagsValue,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;

Function TFSProxyCursor.TableSetTablePassword(aValue: Longword): TffResult;
Var
  Request: TfsnmSetTableTablePasswordValueReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.TablePassword := aValue;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmSetTableTablePasswordValue,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;

Function TFSProxyCursor.TableSetTablePasswordRest(aValue: Longword): TffResult;
Var
  Request: TfsnmSetTableTablePasswordRestValueReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.TablePassword := aValue;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmSetTableTablePasswordRestValue,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;

Function TFSProxyCursor.TableSetTableDBID(aValue: Longword): TffResult;
Var
  Request: TfsnmSetTableTableDBIDValueReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.TableDBID := aValue;

  Reply := Nil;
  Result := Client.ProcessRequest(fsnmSetTableTableDBIDValue,
    Timeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Reply,
    ReplyLen,
    nmdByteArray);
  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);
End;

{------------------------------------------------------------------------------}

{-TFSProxySQLStmt--------------------------------------------------------------}

Constructor TFSProxySQLStmt.Create(aDatabase: TFSProxyDatabase;
  Const aTimeout: Longint);
Var
  Request: TfsnmSQLAllocReq;
  Reply: PfsnmSQLAllocRpy;
  ReplyLen: Longint;
  Result: TffResult;
Begin
  Inherited Create;

  psClient := aDatabase.Client;
  psDatabase := aDatabase;
  psTimeout := aTimeout;

  { Initialize Request }
  Request.DatabaseID := aDatabase.SrDatabaseID;
  Request.Timeout := aTimeout;

  Reply := Nil;
  Result := psClient.ProcessRequest(fsnmSQLAlloc,
    psTimeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(Reply),
    ReplyLen,
    nmdByteArray);

  Check(Result);

  psSrStmtID := Reply^.StmtID;

  If Assigned(Reply) Then
    FFFreeMem(Reply, ReplyLen);

End;
{----------}

Destructor TFSProxySQLStmt.Destroy;
Var
  Request: TfsnmSQLFreeReq;
  Reply: Pointer;
  ReplyLen: Longint;
Begin

  If psSrStmtID > 0 Then
    Begin
      { Initialize Request }
      Request.StmtID := psSrStmtID;

      Reply := Nil;
      psClient.ProcessRequest(fsnmSQLFree,
        psTimeout,
        @Request,
        SizeOf(Request),
        nmdByteArray,
        Reply,
        ReplyLen,
        nmdByteArray);

      If Assigned(Reply) Then
        FFFreeMem(Reply, ReplyLen);
    End;

  psSrStmtID := 0;
  psDatabase := Nil;

  Inherited Destroy;
End;
{----------}

Function TFSProxySQLStmt.Exec(aOpenMode: TffOpenMode;
  Var aCursorID: TffCursorID;
  aStream: TStream): TffResult;
Var
  Request: TfsnmSQLExecReq;
  ReplyLen: Longint;
  SvrCursorID: TffCursorID;
Begin
  Assert(Assigned(aStream));
  { Initialize Request }
  Request.StmtID := psSrStmtID;
  Request.OpenMode := aOpenMode;

  Result := psClient.ProcessRequest(fsnmSQLExec,
    psTimeout,
    @Request,
    SizeOf(Request),
    nmdByteArray,
    Pointer(aStream),
    ReplyLen,
    nmdStream);

  { Was the execution successful? }
  If Result = DBIERR_NONE Then
    Begin
      { Yes. Get the cursorID from the stream & open a proxy cursor. }
      aStream.Position := 0;
      aStream.Read(SvrCursorID, sizeOf(SvrCursorID));
      aCursorID := SvrCursorID;
      If aCursorID <> 0 Then
        Result := psDatabase.QueryOpen(SvrCursorID, aOpenMode, smShared, psTimeout,
          aStream, aCursorID);
    End;

  { Assumption: If an error occurs then the TfsQuery component is responsible
    for displaying the error message returned from the server. }

End;
{----------}

Function TFSProxySQLStmt.Prepare(aQueryText: PChar;
  aStream: TStream): TffResult;
Var
  QueryLen: Longint;
  ReqLen: Longint;
  Request: PfsnmSQLPrepareReq;
  ReplyLen: Longint;
Begin
  Assert(Assigned(aStream));

  QueryLen := StrLen(aQueryText);
  ReqLen := SizeOf(TfsnmSQLPrepareReq) - SizeOf(TfsVarMsgField) + QueryLen + 1;
  FFGetZeroMem(Request, ReqLen);
  Try
    { Prepare the request. }
    Request.StmtID := psSrStmtID;
    Move(aQueryText^, Request^.Query, QueryLen);

    Result := psClient.ProcessRequest(fsnmSQLPrepare,
      psTimeout,
      Request,
      ReqLen,
      nmdByteArray,
      Pointer(aStream),
      ReplyLen,
      nmdStream);

    { Assumption: Upper levels are responsible for Stream contents. }

  Finally
    FFFreeMem(Request, ReqLen);
  End;

End;
{----------}

Function TFSProxySQLStmt.SetParams(aNumParams: Word;
  aParamDescs: pointer;
  aDataBuffer: PffByteArray;
  aDataLen: Longint;
  aStream: TStream): TffResult;
Var
  ReplyLen: Longint;
  Stream: TMemoryStream;
Begin
  Assert(Assigned(aStream));
  { Output stream is expected to be:
        StmtID     (longint)
        NumParams  (word)
        ParamList  (array of TfsSqlParamInfo)
        BufLen     (longint; size of DataBuffer)
        DataBuffer (data buffer)
  }
  Stream := TMemoryStream.Create;
  Try
    Stream.Write(psSrStmtID, SizeOf(psSrStmtID));
    Stream.Write(aNumParams, SizeOf(aNumParams));
    Stream.Write(aParamDescs^, aNumParams * SizeOf(TfsSqlParamInfo));
    Stream.Write(aDataLen, sizeOf(aDataLen));
    Stream.Write(aDataBuffer^, aDataLen);
    Stream.Position := 0;

    Result := psClient.ProcessRequest(fsnmSQLSetParams,
      psTimeout,
      Stream.Memory,
      Stream.Size,
      nmdStream,
      Pointer(aStream),
      ReplyLen,
      nmdStream);
  Finally
    Stream.Free;
  End;

End;
{------------------------------------------------------------------------------}

{-TFSRemoteServer--------------------------------------------------------}

Function TFSRemoteServer.BLOBCreate(aCursorID: TffCursorID;
  Var aBlobNr: TffInt64): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.BlobCreate(aBlobNr);
End;
{----------}

Function TFSRemoteServer.BLOBDelete(aCursorID: TffCursorID;
  aBlobNr: TffInt64): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.BLOBDelete(aBlobNr);
End;
{----------}

Function TFSRemoteServer.BLOBFree(aCursorID: TffCursorID;
  aBlobNr: TffInt64;
  ReadOnly: Boolean): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.BLOBFree(aBlobNr,
      ReadOnly);
End;
{----------}

Function TFSRemoteServer.BLOBGetLength(aCursorID: TffCursorID;
  aBlobNr: TffInt64;
  Var aLength: Longint): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.BLOBGetLength(aBlobNr,
      aLength);
End;
{Begin !!.03}
{----------}

Function TFSRemoteServer.BLOBListSegments(aCursorID: TffCursorID;
  aBLOBNr: TffInt64;
  aStream: TStream): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.BLOBListSegments(aBLOBNr, aStream);
End;
{End !!.03}
{----------}

Function TFSRemoteServer.BLOBRead(aCursorID: TffCursorID;
  aFieldNo: TffWord32;
  aBlobNr: TffInt64;
  aOffset: TffWord32; {!!.06}
  aLen: TffWord32; {!!.06}
  Var aBLOB;
  Var aBytesRead: TffWord32) {!!.06}
: TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.BLOBRead(aFieldNo,
      aBlobNr,
      aOffset,
      aLen,
      aBLOB,
      aBytesRead);
End;
{----------}

Function TFSRemoteServer.BLOBTruncate(aCursorID: TffCursorID;
  aBlobNr: TffInt64;
  aBLOBLength: Longint): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.BLOBTruncate(aBlobNr,
      aBLOBLength);
End;
{----------}

Function TFSRemoteServer.BLOBWrite(aCursorID: TffCursorID;
  aFieldNo: TffWord32;
  aBlobNr: TffInt64;
  aOffset: Longint;
  aLen: Longint;
  Var aBLOB): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.BLOBWrite(aFieldNo,
      aBlobNr,
      aOffset,
      aLen,
      aBLOB);
End;
{Begin !!.01}
{----------}

Function TFSRemoteServer.RemoteRestart(Const aClientID: TffClientID): TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Result := Client.RemoteRestart;
End;
{----------}

Function TFSRemoteServer.RemoteStart(Const aClientID: TffClientID): TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Result := Client.RemoteStart;
End;
{----------}

Function TFSRemoteServer.RemoteStop(Const aClientID: TffClientID): TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Result := Client.RemoteStop;
End;
{End !!.01}
{----------}

Procedure TFSRemoteServer.scInitialize;
Begin
  { do nothing }
End;
{----------}

Procedure TFSRemoteServer.scPrepareForShutdown;
Begin
  { do nothing }
End;
{----------}

Procedure TFSRemoteServer.scShutdown;
Begin
  { do nothing }
End;
{----------}

Procedure TFSRemoteServer.scStartup;
Begin
  { do nothing }
End;
{----------}

Function TFSRemoteServer.bseGetAutoSaveCfg: Boolean;
Begin
  {This is here to kill warnings. Clients shouldn't care about the
   RSE's NoAutoSaveCfg setting.}
  Result := False;
End;
{----------}

Function TFSRemoteServer.bseGetReadOnly: Boolean;
Var
  Client: TFSProxyClient;
Begin
  Client := GetDefaultClient;
  If Assigned(Client) Then
    Result := Client.IsReadOnly
  Else
    Result := False;
End;
{--------}

Procedure TFSRemoteServer.bseSetAutoSaveCfg(aValue: Boolean); {!!.01 - Start}
Begin
  {do nothing}
End;
{--------}

Procedure TFSRemoteServer.bseSetReadOnly(aValue: Boolean);
Begin
  {do nothing}
End;
{--------}{!!.01 - End}

Procedure TFSRemoteServer.FFNotificationEx(Const AOp: Byte;
  AFrom: TFSSpecComp;
  Const AData: TffWord32);
Var
  CL: TFSProxyClient;
  ClIdx: Longint;
  ClFound: Boolean;
Begin
  Inherited; {!!.11}
  If (AFrom = Transport) Then
    If ((AOp = ffn_Destroy) Or (AOp = ffn_Remove)) Then
      Begin
        FFNotifyDependents(ffn_Deactivate);
        rsTransport := Nil;
      End
    Else If (AOp = ffn_Deactivate) Then
      FFNotifyDependents(ffn_Deactivate)
    Else If (AOp = ffn_ConnectionLost) Then
      Begin
        { If we manage this client, then notify depenents that connection is
          lost. It is up to the baseclient dependents to check the data
          parameter to see if this notification affects them.}
        CL := Nil;
        ClFound := False;
        With ClientList.BeginRead Do
          Try
            For ClIdx := 0 To Pred(ClientList.Count) Do
              Begin
                CL := TFSProxyClient(ClientList[ClIdx].Key^);
                If CL.pcSrClientID = AData Then
                  Begin
                    ClFound := True;
                    Break;
                  End;
              End;
          Finally
            EndRead;
          End;
        If CLFound Then
          Begin
            ForceClosing(Longint(CL));
            ClientRemove(Longint(CL));
            FFNotifyDependentsEx(ffn_ConnectionLost, Longint(CL))
          End;
      End;
End;
{Begin !!.07}
{--------}

Procedure TFSRemoteServer.Log(Const aMsg: String);
Begin
  FEventLog.WriteString(aMsg);
End;
{--------}

Procedure TFSRemoteServer.LogAll(Const Msgs: Array Of String);
Begin
  FEventLog.WriteStrings(Msgs);
End;
{--------}

Procedure TFSRemoteServer.LogFmt(Const aMsg: String; args: Array Of Const);
Begin
  FEventLog.WriteString(format(aMsg, args));
End;
{End !!.07}
{--------}

Function TFSRemoteServer.CheckClientIDAndGet(aClientID: TffClientID;
  Var aClient: TFSProxyClient
  ): TffResult;
Begin
  Result := DBIERR_INVALIDHNDL;

  aClient := Nil;
  Try
    If (TObject(aClientID) Is TFSProxyClient) Then
      Begin
        aClient := TFSProxyClient(aClientID);
        Result := DBIERR_NONE;
      End;
  Except
    { An exception may be raised if the ID is bogus.  Swallow the exception.}
  End;
End;
{----------}

Function TFSRemoteServer.CheckCursorIDAndGet(aCursorID: TffCursorID;
  Var aCursor: TFSProxyCursor
  ): TffResult;
Begin
  Result := DBIERR_INVALIDHNDL;

  aCursor := Nil;
  Try
    If (TObject(aCursorID) Is TFSProxyCursor) Then
      Begin
        aCursor := TFSProxyCursor(aCursorID);
        Result := DBIERR_NONE;
      End;
  Except
    { An exception may be raised if the ID is bogus.  Swallow the exception.}
  End;
End;
{----------}

Function TFSRemoteServer.CheckStmtIDAndGet(aStmtID: TffSqlStmtID;
  Var aStmt: TFSProxySQLStmt): TffResult;
Begin
  Result := DBIERR_INVALIDHNDL;

  aStmt := Nil;
  Try
    If (TObject(aStmtID) Is TFSProxySQLStmt) Then
      Begin
        aStmt := TFSProxySQLStmt(aStmtID);
        Result := DBIERR_NONE;
      End;
  Except
    { An exception may be raised if the ID is bogus.  Swallow the exception.}
  End;
End;
{----------}

Function TFSRemoteServer.CheckDatabaseIDAndGet(
  aDatabaseID: TffDatabaseID;
  Var aDatabase: TFSProxyDatabase
  ): TffResult;
Begin
  Result := DBIERR_INVALIDHNDL;

  aDatabase := Nil;
  Try
    If (TObject(aDatabaseID) Is TFSProxyDatabase) Then
      Begin
        aDatabase := TFSProxyDatabase(aDatabaseID);
        Result := DBIERR_NONE;
      End;
  Except
    { An exception may be raised if the ID is bogus.  Swallow the exception.}
  End;
End;
{----------}

Function TFSRemoteServer.CheckSessionIDAndGet(aClientID: TffClientID;
  aSessionID: TffSessionID;
  Var aClient: TFSProxyClient;
  Var aSession: TFSProxySession
  ): TffResult;
Begin
  aSession := Nil;
  aClient := Nil;

  Result := CheckClientIDAndGet(aClientID, aClient);
  If (Result = DBIERR_NONE) Then
    Begin
      Try
        If (TObject(aSessionID) Is TFSProxySession) Then
          Begin
            aSession := TFSProxySession(aSessionID)
          End;
      Except
        { An exception may be raised if the ID is bogus.  Swallow the exception.}
      End;
    End;
End;
{----------}

Function TFSRemoteServer.ClientAdd(Var aClientID: TffClientID;
  Const aClientName: TffNetName;
  Const aUserID: TffName;
  Const aTimeout: Longint;
  Var aHash: TffWord32;
  Var aRights: TffUserRights;
  Var aSecurityEnabled: boolean
  ): TffResult;
Var
  Client: TFSProxyClient;
  ListItem: TfsIntListItem;

Begin
  Result := DBIERR_NONE;
  Client := Nil;

  {Create client object}
  Try
    Client := TFSProxyClient.Create(rsTransport, aUserID, aHash, aTimeOut, aRights, aSecurityEnabled);
    aRights := Client.pcRights;
    aSecurityEnabled := Client.pcSecurityEnabled;
  Except
    On E: Exception Do
      If (E Is EfsException) Or
        (E Is EfsDatabaseError) Or
        (E Is EfsServerComponentError) Then
        Result := EfsException(E).ErrorCode;
  End;

  If ResultOK(Result) And Assigned(Client) Then
    Begin
      {Add to the internal list}
      ListItem := TfsIntListItem.Create(Longint(Client));
      With rsClientList.BeginWrite Do
        Try
          Insert(ListItem);
        Finally
          EndWrite;
        End;

      {Set the return value}
      aClientID := Longint(Client);
    End;
End;
{Begin !!.11}

Function TFSRemoteServer.ClientAddEx(Var aClientID: TffClientID;
  Const aClientName: TffNetName;
  Const aUserID: TffName;
  Const aTimeout: Longint;
  Const aClientVersion: Longint;
  Var aHash: TffWord32;
  Var aRights: TffUserRights;
  Var aSecurityEnabled: boolean): TffResult;
Begin
  Result := ClientAdd(aClientID, aClientName, aUserID, aTimeout, aHash, aRights, aSecurityEnabled);
End;
{End !!.11}
{----------}

Function TFSRemoteServer.ClientRemove(aClientID: TffClientID
  ): TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    {Remove from the internal list, and free}
    With rsClientList.BeginWrite Do
      Try
        Delete(Client); {!!.01}
        Client.Free;
      Finally
        EndWrite;
      End;
End;
{----------}

Function TFSRemoteServer.ClientSetTimeout(Const aClientID: TffClientID;
  Const aTimeout: Longint
  ): TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Result := Client.SetTimeout(aTimeout);
End;
{----------}

Constructor TFSRemoteServer.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);

  rsClientList := TFSProxyClientList.Create;
  rsTimeout := 0;
  rsTransport := Nil;

  With RemoteServerEngines.BeginWrite Do
    Try
      Insert(TfsIntListItem.Create(Longint(Self)));
    Finally
      EndWrite;
    End;
End;
{----------}

Function TFSRemoteServer.CursorClone(aCursorID: TffCursorID;
  aOpenMode: TffOpenMode;
  Var aNewCursorID: TffCursorID
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.CursorClone(aOpenMode,
      aNewCursorID);
End;
{----------}

Function TFSRemoteServer.CursorClose(aCursorID: TffCursorID): TffResult;
Var
  Cursor: TFSProxyCursor;
  Database: TFSProxyDatabase;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Begin
      Database := Cursor.Database;
      Result := Database.TableClose(Cursor);
    End;
End;
{----------}

Function TFSRemoteServer.CursorCompareBookmarks(
  aCursorID: TffCursorID;
  aBookmark1: PffByteArray;
  aBookmark2: PffByteArray;
  Var aCompResult: Longint
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.CompareBookmarks(aBookmark1,
      aBookmark2,
      aCompResult);
End;
{Begin !!.02}
{----------}

Function TFSRemoteServer.CursorCopyRecords(aSrcCursorID,
  aDestCursorID: TffCursorID;
  aCopyBLOBs: Boolean; CountPerTrans: Longint): TffResult;
Var
  DestCursor, SrcCursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aDestCursorID, DestCursor);
  If ResultOK(Result) Then
    Begin
      Result := CheckCursorIDAndGet(aSrcCursorID, SrcCursor);
      If ResultOK(Result) Then
        Result := DestCursor.CopyRecords(SrcCursor, aCopyBLOBs, CountPerTrans);
    End;
End;
{End !!.02}
{Begin !!.06}
{----------}

Function TFSRemoteServer.CursorDeleteRecords(aCursorID: TffCursorID; CountPerTrans: Longint): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.DeleteRecords(CountPerTrans);
End;
{End !!.06}
{----------}

Function TFSRemoteServer.CursorGetBookmark(aCursorID: TffCursorID;
  aBookmark: PffByteArray
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.GetBookmark(aBookmark);
End;
{Begin !!.03}
{----------}

Function TFSRemoteServer.CursorListBLOBFreeSpace(aCursorID: TffCursorID;
  Const aInMemory: Boolean;
  aStream: TStream): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.ListBLOBFreeSpace(aInMemory, aStream);
End;
{End !!.03}
{----------}

Function TFSRemoteServer.CursorOverrideFilter(aCursorID: Longint;
  aExpression: pCANExpr;
  aTimeout: TffWord32): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.OverrideFilter(aExpression, aTimeout);
End;
{----------}

Function TFSRemoteServer.CursorRestoreFilter(aCursorID: Longint): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.RestoreFilter;
End;
{----------}

Function TFSRemoteServer.CursorGetBookmarkSize(aCursorID: TffCursorID;
  Var aSize: Longint
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.GetBookmarkSize(aSize);
End;
{----------}

Function TFSRemoteServer.CursorResetRange(aCursorID: TffCursorID
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.ResetRange;
End;
{----------}

Function TFSRemoteServer.CursorSetFilter(aCursorID: TffCursorID;
  aExpression: pCANExpr;
  aTimeout: TffWord32
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.SetFilter(aExpression,
      aTimeout);
End;
{----------}

Function TFSRemoteServer.CursorSetRange(aCursorID: TffCursorID;
  aDirectKey: Boolean;
  aFieldCount1: Longint;
  aPartialLen1: Longint;
  aKeyData1: PffByteArray;
  aKeyIncl1: Boolean;
  aFieldCount2: Longint;
  aPartialLen2: Longint;
  aKeyData2: PffByteArray;
  aKeyIncl2: Boolean
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.SetRange(aDirectKey,
      aFieldCount1,
      aPartialLen1,
      aKeyData1,
      aKeyIncl1,
      aFieldCount2,
      aPartialLen2,
      aKeyData2,
      aKeyIncl2);
End;
{----------}

Function TFSRemoteServer.CursorSetTimeout(Const aCursorID: TffCursorID;
  Const aTimeout: Longint
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.SetTimeout(aTimeout);
End;
{----------}

Function TFSRemoteServer.CursorSetToBegin(aCursorID: TffCursorID
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.SetToBegin;
End;
{----------}

Function TFSRemoteServer.CursorSetToBookmark(aCursorID: TffCursorID;
  aBookmark: PffByteArray
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.SetToBookmark(aBookmark);
End;
{----------}

Function TFSRemoteServer.CursorSetToCursor(aDestCursorID: TffCursorID;
  aSrcCursorID: TffCursorID
  ): TffResult;
Var
  DestCursor: TFSProxyCursor;
  SourceCursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aDestCursorID, DestCursor);
  If ResultOK(Result) Then
    Result := CheckCursorIDAndGet(aSrcCursorID, SourceCursor);
  If ResultOK(Result) Then
    Result := DestCursor.SetToCursor(SourceCursor);
End;
{----------}

Function TFSRemoteServer.CursorSetToEnd(aCursorID: TffCursorID
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.SetToEnd;
End;
{----------}

Function TFSRemoteServer.CursorSetToKey(
  aCursorID: TffCursorID;
  aSearchAction: TffSearchKeyAction;
  aDirectKey: Boolean;
  aFieldCount: Longint;
  aPartialLen: Longint;
  aKeyData: PffByteArray
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.SetToKey(aSearchAction,
      aDirectKey,
      aFieldCount,
      aPartialLen,
      aKeyData);
End;
{----------}

Function TFSRemoteServer.CursorSwitchToIndex(aCursorID: TffCursorID;
  aIndexName: TffDictItemName;
  aIndexID: Longint;
  aPosnOnRec: Boolean
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.SwitchToIndex(aIndexName,
      aIndexID,
      aPosnOnRec);
End;
{----------}

Function TFSRemoteServer.DatabaseAddAlias(Const aAlias: TffName;
  Const aPath: TffPath;
  aCheckSpace: Boolean; {!!.11}
  Const aClientID: TffClientID)
  : TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Result := Client.DatabaseAddAlias(aAlias, aPath, aCheckSpace); {!!.11}
End;
{----------}

Function TFSRemoteServer.DatabaseAliasList(aList: TList;
  aClientID: TffClientID)
  : TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Result := Client.DatabaseAliasList(aList);
End;
{----------}

Function TFSRemoteServer.RecoveryAliasList(aList: TList;
  aClientID: TffClientID)
  : TffResult;
Begin
  Assert(False, 'RecoveryAliasList unsupported for TFSRemoteServer.');
  Result := DBIERR_NOTSUPPORTED;
End;
{----------}

Function TFSRemoteServer.DatabaseChgAliasPath(aAlias: TffName;
  aNewPath: TffPath;
  aCheckSpace: Boolean; {!!.11}
  aClientID: TffClientID)
  : TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Result := Client.DatabaseChgAliasPath(aAlias,
      aNewPath,
      aCheckSpace) {!!.11}
End;
{----------}

Function TFSRemoteServer.DatabaseClose(aDatabaseID: TffDatabaseID
  ): TffResult;
Var
  Database: TFSProxyDatabase;
  Client: TFSProxyClient;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Begin
      Client := Database.Client;
      Result := Client.DatabaseClose(Database);
    End;
End;
{----------}

Function TFSRemoteServer.DatabaseDeleteAlias(aAlias: TffName;
  aClientID: TffClientID
  ): TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Result := Client.DatabaseDeleteAlias(aAlias)
End;
{----------}

Function TFSRemoteServer.DatabaseGetAliasPath(aAlias: TffName;
  Var aPath: TffPath;
  aClientID: TffClientID
  ): TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Result := Client.DatabaseGetAliasPath(aAlias, aPath)
End;
{----------}

Function TFSRemoteServer.DatabaseGetFreeSpace(Const aDatabaseID: TffDatabaseID;
  Var aFreeSpace: Int64
  ): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.GetDbFreeSpace(aFreeSpace);
End;
{----------}

Function TFSRemoteServer.DatabaseModifyAlias(Const aClientID: TffClientID;
  Const aAlias: TffName;
  Const aNewName: TffName;
  Const aNewPath: TffPath;
  aCheckSpace: Boolean) {!!.11}
: TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Result := Client.DatabaseModifyAlias(aAlias,
      aNewName,
      aNewPath,
      aCheckSpace) {!!.11}
End;
{----------}

Function TFSRemoteServer.DatabaseOpen(aClientID: TffClientID;
  Const aAlias: TffName;
  Const aOpenMode: TffOpenMode;
  Const aShareMode: TffShareMode;
  Const aTimeout: Longint;
  Const aTransIsolation: TfsTransIsolation;
  Const aTransLocking: TfsDataBaseRecLocking;
  Var aDatabaseID: TffDatabaseID
  ): TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Result := Client.DatabaseOpen(aAlias,
      aOpenMode,
      aShareMode,
      aTimeout,
      aTransIsolation,
      aTransLocking,
      aDatabaseID);
End;
{----------}

Function TFSRemoteServer.DatabaseOpenNoAlias(aClientID: TffClientID;
  Const aPath: TffPath;
  Const aOpenMode: TffOpenMode;
  Const aShareMode: TffShareMode;
  Const aTimeout: Longint;
  Const aTransIsolation: TfsTransIsolation;
  Const aTransLocking: TfsDataBaseRecLocking;
  Var aDatabaseID: TffDatabaseID
  ): TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Result := Client.DatabaseOpenNoAlias(aPath,
      aOpenMode,
      aShareMode,
      aTimeout,
      aTransIsolation,
      aTransLocking,
      aDatabaseID);
End;
{----------}

Function TFSRemoteServer.DatabaseSetTimeout(
  Const aDatabaseID: TffDatabaseID;
  Const aTimeout: Longint
  ): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.SetTimeout(aTimeout);
End;
{----------}

Function TFSRemoteServer.DatabaseTableExists(aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName;
  Var aExists: Boolean
  ): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TableExists(aTableName, aExists);
End;
{----------}

Function TFSRemoteServer.DatabaseTableList(aDatabaseID: TffDatabaseID;
  Const aMask: TffFileNameExt;
  aList: TList
  ): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TableList(aMask,
      aList);
End;
{----------}

Function TFSRemoteServer.DatabaseTableLockedExclusive(
  aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName;
  Var aLocked: Boolean
  ): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TableLockedExclusive(aTableName,
      aLocked);

End;
{----------}

Destructor TFSRemoteServer.Destroy;
//var                                                                  {!!.03}
//  Idx : Longint;                                                     {!!.03}
Begin
  FFNotifyDependents(ffn_Destroy);

  { Make sure we are shutdown. }
  State := fsesInactive;

  {Begin !!.03}
  //  {Free dependent objects}
  //  with rsClientList.BeginWrite do
  //    try
  //      for Idx := 0 to Pred(Count) do
  //        TFSProxyClient(Items[Idx]).Free;
  //    finally
  //      EndWrite;
  //    end;
  {End !!.03}

  With RemoteServerEngines.BeginWrite Do
    Try
      Delete(Longint(Self)); {!!.01}
    Finally
      EndWrite;
    End;

  {Free and nil internal lists}
  rsClientList.Free;
  rsClientList := Nil;

  {Clear the transport}
  Transport := Nil;

  Inherited Destroy;
End;
{----------}

Function TFSRemoteServer.FileBLOBAdd(aCursorID: TffCursorID;
  Const aFileName: TffFullFileName;
  Var aBlobNr: TffInt64): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.FileBLOBAdd(aFileName,
      aBlobNr);
End;
{----------}

Function TFSRemoteServer.GetDefaultClient: TFSProxyClient;
Begin
  Result := Nil;
  With rsClientList.BeginRead Do
    Try
      If Count > 0 Then
        Result := TFSProxyClient(TfsIntListItem(Items[0]).KeyAsInt); {!!.01}
    Finally
      EndRead;
    End;
End;
{----------}

Function TFSRemoteServer.GetServerDateTime(Var aDateTime: TDateTime
  ): TffResult;
Begin
  If (GetDefaultClient <> Nil) Then
    Result := GetDefaultClient.GetServerDateTime(aDateTime)
  Else
    Result := DBIERR_INVALIDHNDL;
End;
{----------}

Function TFSRemoteServer.GetServerSystemTime(Var aSystemTime: TSystemTime): TffResult;
Begin
  If (GetDefaultClient <> Nil) Then
    Result := GetDefaultClient.GetServerSystemTime(aSystemTime)
  Else
    Result := DBIERR_INVALIDHNDL;
End;
{----------}

Function TFSRemoteServer.GetServerGUID(Var aGUID: TGUID): TffResult;
Begin
  If (GetDefaultClient <> Nil) Then
    Result := GetDefaultClient.GetServerGUID(aGUID)
  Else
    Result := DBIERR_INVALIDHNDL;
End;
{----------}

Function TFSRemoteServer.GetServerID(Var aUniqueID: TGUID): TffResult;
Begin
  If (GetDefaultClient <> Nil) Then
    Result := GetDefaultClient.GetServerID(aUniqueID)
  Else
    Result := DBIERR_INVALIDHNDL;
End;
{----------}

Function TFSRemoteServer.GetServerStatistics(Var Stats: TfsServerStatistics): TffResult;
Begin
  ;
  If (GetDefaultClient <> Nil) Then
    Result := GetDefaultClient.GetServerStatistics(Stats)
  Else
    Result := DBIERR_INVALIDHNDL;
End;
{----------}

Function TFSRemoteServer.GetCommandHandlerStatistics(Const CmdHandlerIdx: Integer;
  Var Stats: TfsCommandHandlerStatistics): TffResult;
Begin
  If (GetDefaultClient <> Nil) Then
    Result := GetDefaultClient.GetCommandHandlerStatistics(CmdHandlerIdx,
      Stats)
  Else
    Result := DBIERR_INVALIDHNDL;
End;
{----------}

Function TFSRemoteServer.GetTransportStatistics(Const CmdHandlerIdx: Integer;
  Const TransportIdx: Integer;
  Var Stats: TfsTransportStatistics): TffResult;
Begin
  If (GetDefaultClient <> Nil) Then
    Result := GetDefaultClient.GetTransportStatistics(CmdHandlerIdx,
      TransportIdx,
      Stats)
  Else
    Result := DBIERR_INVALIDHNDL;
End;
{----------}{end !!.07}

Procedure TFSRemoteServer.GetServerNames(aList: TStrings;
  aTimeout: Longint);
Begin
  Transport.GetServerNames(aList, aTimeout);
End;
{----------}

Procedure TFSRemoteServer.ForceClosing(Const aClientID: TffClientID);
Var
  Client: TFSProxyClient;
Begin
  If CheckClientIDAndGet(aClientID, Client) = DBIERR_NONE Then
    Client.ForceClosed := True;
End;
{Begin !!.06}
{--------}

Function TFSRemoteServer.ProcessRequest(aClientID: TffClientID;
  aMsgID: Longint;
  aTimeout: Longint;
  aRequestData: Pointer;
  aRequestDataLen: Longint;
  aRequestDataType: TffNetMsgDataType;
  Var aReply: Pointer;
  Var aReplyLen: Longint;
  aReplyType: TffNetMsgDataType): TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Result := Client.ProcessRequest(aMsgID, aTimeout, aRequestData,
      aRequestDataLen, aRequestDataType,
      aReply, aReplyLen, aReplyType);
End;
{--------}

Function TFSRemoteServer.ProcessRequestNoReply(aClientID: TffClientID;
  aMsgID: Longint;
  aTimeout: Longint;
  aRequestData: Pointer;
  aRequestDataLen: Longint): TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Result := Client.ProcessRequestNoReply(aMsgID, aTimeout, aRequestData,
      aRequestDataLen);
End;
{End !!.06}
{----------}

Function TFSRemoteServer.RebuildGetStatus(aRebuildID: Longint;
  Const aClientID: TffClientID;
  Var aIsPresent: Boolean;
  Var aStatus: TffRebuildStatus
  ): TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Result := Client.GetRebuildStatus(aRebuildID,
      aIsPresent,
      aStatus);
End;
{----------}

Function TFSRemoteServer.RecordDelete(aCursorID: TffCursorID;
  aData: PffByteArray): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.RecordDelete(aData);
End;
{----------}

Function TFSRemoteServer.RecordDeleteBatch(aCursorID: TffCursorID;
  aBMCount: Longint;
  aBMLen: Longint;
  aData: PffByteArray;
  aErrors: PffLongintArray
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.RecordDeleteBatch(aBMCount,
      aBMLen,
      aData,
      aErrors);
End;
{----------}

Function TFSRemoteServer.RecordExtractKey(aCursorID: TffCursorID;
  aData: PffByteArray;
  aKey: PffByteArray
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.RecordExtractKey(aData,
      aKey);
End;
{----------}

Function TFSRemoteServer.RecordGet(aCursorID: TffCursorID;
  aLockType: TffLockType;
  aUserLockType: TfsUserRecLocking;
  aData: PffByteArray; Var aFlag: Byte; Var aRefNr: TffInt64; Const aUser: Boolean): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.RecordGet(aLockType, aUserLockType,
      aData, aFlag, aRefNr, aUser);
End;
{----------}

Function TFSRemoteServer.RecordGetBatch(aCursorID: TffCursorID;
  aRecCount: Longint;
  aRecLen: Longint;
  Var aRecRead: Longint;
  aData: PffByteArray;
  Var aError: TffResult
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.RecordGetBatch(aRecCount,
      aRecLen,
      aRecRead,
      aData,
      aError);
End;
{----------}

Function TFSRemoteServer.RecordGetForKey(aCursorID: TffCursorID;
  aDirectKey: Boolean;
  aFieldCount: Longint;
  aPartialLen: Longint;
  aKeyData: PffByteArray;
  aData: PffByteArray;
  aFirstCall: Boolean
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.RecordGetForKey(aDirectKey,
      aFieldCount,
      aPartialLen,
      aKeyData,
      aData,
      aFirstCall);
End;
{----------}

Function TFSRemoteServer.RecordGetSetPosition(aValue: Longint; aCursorID: TffCursorID;
  aLockType: TffLockType;
  aData: PffByteArray;
  Var aFlag: Byte;
  Var aRecNo: Longword;
  Var aRefNr: TffInt64;
  aInfoGetSetPosition: TInfoGetSetPosition;
  aSet: Boolean): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.GetSetPosition(aValue, aData, aLockType,
      aFlag, aRecNo, aRefNr, aInfoGetSetPosition, aSet);
End;

Function TFSRemoteServer.RecordGetNext(aCursorID: TffCursorID;
  aLockType: TffLockType;
  aData: PffByteArray;
  Var aFlag: Byte;
  Var aRefNr: TffInt64
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.RecordGetNext(aLockType,
      aData, aFlag, aRefNr);
End;
{----------}

Function TFSRemoteServer.RecordGetPrior(aCursorID: TffCursorID;
  aLockType: TffLockType;
  aData: PffByteArray;
  Var aFlag: Byte;
  Var aRefNr: TffInt64
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.RecordGetPrior(aLockType,
      aData, aFlag, aRefNr);
End;
{----------}

Function TFSRemoteServer.RecordInsert(aCursorID: TffCursorID;
  aLockType: TffLockType;
  aData: PffByteArray; aUndelete: Boolean; Var aRefNr: TffInt64): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.RecordInsert(aLockType,
      aData, aUndelete, aRefNr);
End;
{----------}

Function TFSRemoteServer.RecordInsertBatch(aCursorID: TffCursorID;
  aRecCount: Longint;
  aRecLen: Longint;
  aData: PffByteArray;
  aErrors: PffLongintArray
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.RecordInsertBatch(aRecCount,
      aRecLen,
      aData,
      aErrors);
End;
{----------}

Function TFSRemoteServer.RecordIsLocked(aCursorID: TffCursorID;
  aLockType: TffLockType;
  Var aIsLocked: boolean): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.RecordIsLocked(aLockType,
      aIsLocked);
End;
{----------}

Function TFSRemoteServer.RecordModify(aCursorID: TffCursorID;
  aData: PffByteArray;
  aRelLock: Boolean;
  aUserLockType: TfsUserRecLocking;
  aFlag: Byte;
  aSet, Use: Boolean): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.RecordModify(aData, aRelLock, aUserLockType, aFlag, aSet, Use);
End;
{----------}

Function TFSRemoteServer.RecordRelLock(aCursorID: TffCursorID;
  aAllLocks: Boolean): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.RecordRelLock(aAllLocks);
End;
{----------}

Procedure TFSRemoteServer.rsSetTransport(Const Value: TFSBaseTransport);
Begin
  If rsTransport = Value Then
    Exit;

  FFNotifyDependents(ffn_Deactivate);
  If Assigned(rsTransport) Then
    rsTransport.FFRemoveDependent(Self);

  rsTransport := Value;
  If Assigned(rsTransport) Then
    rsTransport.FFAddDependent(Self);
End;
{----------}

Function TFSRemoteServer.SessionAdd(Const aClientID: TffClientID;
  Const aTimeout: Longint;
  Var aSessionID: TffSessionID;
  aData: Pointer;
  aDataLength: Longint = 0): TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Result := Client.SessionAdd(aSessionID, aTimeout, aData, aDataLength);
End;
{----------}

Function TFSRemoteServer.SessionCount(aClientID: TffClientID;
  Var aCount: Longint): TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    aCount := Client.SessionCount;
End;
{----------}

Function TFSRemoteServer.SessionGetCurrent(aClientID: TffClientID;
  Var aSessionID: TffSessionID
  ): TffResult;
Var
  Client: TFSProxyClient;
  Session: TFSProxySession;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Begin
      Session := Client.CurrentSession;
      aSessionID := Longint(Session);
    End;
End;
{Begin !!.06}
{----------}

Function TFSRemoteServer.SessionCloseInactiveTables(aClientID: TffClientID): TffResult;
Var
  Client: TFSProxyClient;
Begin
  Result := CheckClientIDAndGet(aClientID, Client);
  If ResultOK(Result) Then
    Client.SessionCloseInactiveTables;
End;
{End !!.06}
{----------}

Function TFSRemoteServer.SessionRemove(aClientID: TffClientID;
  aSessionID: TffSessionID
  ): TffResult;
Var
  Client: TFSProxyClient;
  Session: TFSProxySession;
Begin
  Result := CheckSessionIDAndGet(aClientID, aSessionID, Client, Session);
  If ResultOK(Result) Then
    Client.SessionRemove(Session);
End;
{----------}

Function TFSRemoteServer.SessionSetCurrent(aClientID: TffClientID;
  aSessionID: TffSessionID
  ): TffResult;
Var
  Client: TFSProxyClient;
  Session: TFSProxySession;
Begin
  Result := CheckSessionIDAndGet(aClientID, aSessionID, Client, Session);
  If ResultOK(Result) Then
    Client.SessionSetCurrent(Session);
End;
{----------}

Function TFSRemoteServer.SessionSetTimeout(
  Const aClientID: TffClientID;
  Const aSessionID: TffSessionID;
  Const aTimeout: Longint
  ): TffResult;
Var
  Client: TFSProxyClient;
  Session: TFSProxySession;
Begin
  Result := CheckSessionIDAndGet(aClientID, aSessionID, Client, Session);
  If ResultOK(Result) Then
    Result := Session.SetTimeout(aTimeout);
End;
{----------}

Function TFSRemoteServer.SQLAlloc(aClientID: TffClientID;
  aDatabaseID: TffDatabaseID;
  aTimeout: Longint;
  Var aStmtID: TffSqlStmtID): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.SQLAlloc(aTimeout, aStmtID);
End;
{----------}

Function TFSRemoteServer.SQLExec(aStmtID: TffSqlStmtID;
  aOpenMode: TffOpenMode;
  Var aCursorID: TffCursorID;
  aStream: TStream): TffResult;
Var
  Statement: TFSProxySQLStmt;
Begin
  Assert(Assigned(aStream));
  Result := CheckStmtIDAndGet(aStmtID, Statement);
  If ResultOK(Result) Then
    Result := Statement.Exec(aOpenMode, aCursorID, aStream);
End;
{----------}

Function TFSRemoteServer.SQLExecDirect(aClientID: TffClientID;
  aDatabaseID: TffDatabaseID;
  aQueryText: PChar;
  aTimeout: Longint;
  aOpenMode: TffOpenMode;
  Var aCursorID: TffCursorID;
  aStream: TStream): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Assert(Assigned(aStream));
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.SQLExecDirect(aQueryText, aOpenMode, aTimeout,
      aCursorID, aStream);
End;
{----------}

Function TFSRemoteServer.SQLFree(aStmtID: TffSqlStmtID): TffResult;
Var
  Statement: TFSProxySQLStmt;
Begin
  { Assumption: The cursor associated with the SQL statement has already been
    closed. }
  Result := CheckStmtIDAndGet(aStmtID, Statement);
  If Result = DBIERR_NONE Then
    Statement.Free;
End;
{----------}

Function TFSRemoteServer.SQLPrepare(aStmtID: TffSqlStmtID;
  aQueryText: PChar;
  aStream: TStream): TffResult;
Var
  Statement: TFSProxySQLStmt;
Begin
  Assert(Assigned(aStream));
  Result := CheckStmtIDAndGet(aStmtID, Statement);
  If Result = DBIERR_NONE Then
    Result := Statement.Prepare(aQueryText, aStream);
End;
{----------}

Function TFSRemoteServer.SQLSetParams(aStmtID: TffSqlStmtID;
  aNumParams: Word;
  aParamDescs: pointer;
  aDataBuffer: PffByteArray;
  aDataLen: Longint;
  aStream: TStream
  ): TffResult;
Var
  Statement: TFSProxySQLStmt;
Begin
  Assert(Assigned(aStream));
  Result := CheckStmtIDAndGet(aStmtID, Statement);
  If Result = DBIERR_NONE Then
    Result := Statement.SetParams(aNumParams, aParamDescs, aDataBuffer, aDataLen, aStream);
End;
{----------}

Function TFSRemoteServer.TableAddIndex(
  Const aDatabaseID: TffDatabaseID;
  Const aCursorID: TffCursorID;
  Const aTableName: TfsTableName;
  Const aIndexDesc: TffIndexDescriptor
  ): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TableAddIndex(aCursorID,
      aTableName,
      aIndexDesc);
End;
{----------}

Function TFSRemoteServer.TableBuild(aDatabaseID: TffDatabaseID;
  aOverWrite: Boolean;
  Const aTableName: TfsTableName;
  aForServer: Boolean;
  aDictionary: TFSInfoDict
  ): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TableBuild(aOverWrite,
      aTableName,
      aForServer,
      aDictionary);
End;
{----------}

Function TFSRemoteServer.TableDelete(aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName
  ): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TableDelete(aTableName);
End;
{----------}

Function TFSRemoteServer.TableDropIndex(aDatabaseID: TffDatabaseID;
  aCursorID: TffCursorID;
  Const aTableName: TfsTableName;
  Const aIndexName: TffDictItemName;
  aIndexID: Longint
  ): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TableDropIndex(aCursorID,
      aTablename,
      aIndexName,
      aIndexID);
End;
{----------}

Function TFSRemoteServer.TableEmpty(aDatabaseID: TffDatabaseID;
  aCursorID: TffCursorID;
  Const aTableName: TfsTableName
  ): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TableEmpty(aCursorID,
      aTableName);
End;
{----------}

Function TFSRemoteServer.TableGetAutoInc(aCursorID: TffCursorID;
  Var aValue: Int64; Var aStep: Longint): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Begin
      Result := Cursor.TableGetAutoInc(aValue, aStep);
    End;
End;

Function TFSRemoteServer.TableGetMaxRecords(aCursorID: TffCursorID;
  Var aValue: Longword): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Begin
      Result := Cursor.TableGetMaxRecords(aValue);
    End;
End;

Function TFSRemoteServer.TableGetTableFlags(aCursorID: TffCursorID;
  Var aValue: Word): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Begin
      Result := Cursor.TableGetTableFlags(aValue);
    End;
End;

Function TFSRemoteServer.TableGetTablePassword(aCursorID: TffCursorID;
  Var aValue: Longword): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Begin
      Result := Cursor.TableGetTablePassword(aValue);
    End;
End;

Function TFSRemoteServer.TableGetTablePasswordRest(aCursorID: TffCursorID;
  Var aValue: Longword): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Begin
      Result := Cursor.TableGetTablePasswordRest(aValue);
    End;
End;

Function TFSRemoteServer.TableGetTableDBID(aCursorID: TffCursorID;
  Var aValue: Longword): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Begin
      Result := Cursor.TableGetTableDBID(aValue);
    End;
End;
{----------}

Function TFSRemoteServer.TableGetDictionary(aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName;
  aForServer: Boolean;
  aStream: TStream
  ): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Assert(Assigned(aStream));
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TableGetDictionary(aTableName,
      aForServer,
      aStream);
End;
{----------}

Function TFSRemoteServer.TableGetRecCount(aCursorID: TffCursorID;
  Var aRecCount: Longword
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.TableGetRecCount(aRecCount);
End;
{Begin !!.07}
{----------}

Function TFSRemoteServer.TableGetRecCountAsync(aCursorID: TffCursorID;
  Var aTaskID: Longint): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.TableGetRecCountAsync(aTaskID);
End;
{End !!.07}
{----------}

Function TFSRemoteServer.TableIsLocked(aCursorID: TffCursorID;
  aLockType: TffLockType;
  Var aIsLocked: Boolean): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.TableIsLocked(aLockType,
      aIsLocked);
End;
{----------}

Function TFSRemoteServer.TableLockAcquire(aCursorID: TffCursorID;
  aLockType: TffLockType
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.TableLockAcquire(aLockType);
End;
{----------}

Function TFSRemoteServer.TableLockRelease(aCursorID: TffCursorID;
  aAllLocks: Boolean
  ): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Result := Cursor.TableLockRelease(aAllLocks);
End;
{----------}

Function TFSRemoteServer.TableOpen(Const aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName;
  Const aForServer: Boolean;
  Const aIndexName: TffName;
  aIndexID: Longint;
  Const aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  Const aTimeout: Longint;
  Var aCursorID: TffCursorID;
  aStream: TStream;
  aSysOpen: boolean = True): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Assert(Assigned(aStream));
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TableOpen(aTableName,
      aForServer,
      aIndexName,
      aIndexID,
      aOpenMode,
      aShareMode,
      aTimeout,
      aCursorID,
      aStream);
End;
{----------}

Function TFSRemoteServer.TablePack(aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName;
  Var aRebuildID: Longint; UndeleteRecords: boolean; OnlyDeleted: boolean): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TablePack(aTableName,
      aRebuildID, UndeleteRecords, OnlyDeleted);
End;
{----------}

Function TFSRemoteServer.TableRebuildIndex(aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName;
  Const aIndexName: TffName;
  aIndexID: Longint;
  Var aRebuildID: Longint
  ): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TableRebuildIndex(aTableName,
      aIndexName,
      aIndexID,
      aRebuildID);
End;
{----------}

Function TFSRemoteServer.TableRename(aDatabaseID: TffDatabaseID;
  Const aOldName: TffName;
  Const aNewName: TffName): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TableRename(aOldName,
      aNewName);
End;
{----------}

Function TFSRemoteServer.TableRestructure(aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName;
  aDictionary: TFSInfoDict;
  aFieldMap: TFSSpecStringList;
  Var aRebuildID: Longint;
  aRangeError: boolean): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TableRestructure(aTableName,
      aDictionary,
      aFieldMap,
      aRebuildID,
      aRangeError);
End;
{----------}

Function TFSRemoteServer.TableSetAutoInc(aCursorID: TffCursorID;
  aValue: Int64; aStep: Longint): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Begin
      Result := Cursor.TableSetAutoInc(aValue, aStep);
    End;
End;

Function TFSRemoteServer.TableSetMaxRecords(aCursorID: TffCursorID;
  aValue: Longint): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Begin
      Result := Cursor.TableSetMaxRecords(aValue);
    End;
End;

Function TFSRemoteServer.TableSetTableFlags(aCursorID: TffCursorID;
  aValue: Word): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Begin
      Result := Cursor.TableSetTableFlags(aValue);
    End;
End;

Function TFSRemoteServer.TableSetTablePassword(aCursorID: TffCursorID;
  aValue: Longword): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Begin
      Result := Cursor.TableSetTablePassword(aValue);
    End;
End;

Function TFSRemoteServer.TableSetTablePasswordRest(aCursorID: TffCursorID;
  aValue: Longword): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Begin
      Result := Cursor.TableSetTablePasswordRest(aValue);
    End;
End;

Function TFSRemoteServer.TableSetTableDBID(aCursorID: TffCursorID;
  aValue: Longword): TffResult;
Var
  Cursor: TFSProxyCursor;
Begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  If ResultOK(Result) Then
    Begin
      Result := Cursor.TableSetTableDBID(aValue);
    End;
End;

{Begin !!.11}
{----------}

Function TFSRemoteServer.TableVersion(aDatabaseID: TffDatabaseID;
  Const aTableName: TfsTableName;
  Var aVersion: Longint): TffResult;
Var
  Database: TFSProxyDatabase;
  Request: TfsnmGetTableVersionReq;
  Reply: PfsnmGetTableVersionRpy;
  ReplyLen: Longint;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Begin
      aVersion := 0;
      { Initialize Request }
      Request.DatabaseID := Database.SrDatabaseID;
      Request.TableName := aTableName;

      Reply := Nil;
      Result := Database.pdClient.ProcessRequest(fsnmGetTableVersion,
        Timeout,
        @Request,
        SizeOf(Request),
        nmdByteArray,
        Pointer(Reply),
        ReplyLen,
        nmdByteArray);

      If ResultOK(Result) Then
        aVersion := Reply^.Version;

      If Assigned(Reply) Then
        FFFreeMem(Reply, ReplyLen);
    End; { if }
End;
{End !!.11}
{----------}

Function TFSRemoteServer.InTransaction(Const aDatabaseID: TffDatabaseID; Var aTransLevel: Longint): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.InTransaction(aTransLevel);
End;

Function TFSRemoteServer.TransactionCorrupted(Const aDatabaseID: TffDatabaseID): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TransactionCorrupted;
End;

Function TFSRemoteServer.TransactionCommit(Const aDatabaseID: TffDatabaseID; aRemoveFile: Boolean = False
  ): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TransactionCommit(aRemoveFile);
End;
{----------}

Function TFSRemoteServer.TransactionRollback(
  Const aDatabaseID: TffDatabaseID
  ): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TransactionRollback;
End;
{----------}

Function TFSRemoteServer.TransactionStart(
  Const aDatabaseID: TffDatabaseID;
  Const aFailSafe: Boolean
  ): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TransactionStart(aFailSafe);
End;
{Begin !!.10}
{----------}

Function TFSRemoteServer.TransactionStartWith(
  Const aDatabaseID: TffDatabaseID;
  Const aFailSafe: Boolean;
  Const aCursorIDs: TfsPointerList
  ): TffResult;
Var
  Database: TFSProxyDatabase;
Begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  If ResultOK(Result) Then
    Result := Database.TransactionStartWith(aFailSafe, aCursorIDs);
End;
{End !!.10}
{----------}

Initialization
  RemoteServerEngines := TFSSpecThreadList.Create;

Finalization
  RemoteServerEngines.Free;
  RemoteServerEngines := Nil;

End.

