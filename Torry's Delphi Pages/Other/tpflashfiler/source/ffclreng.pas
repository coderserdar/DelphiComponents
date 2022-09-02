{*********************************************************}
{* FlashFiler: Remote Server Engine Classes              *}
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

unit ffclreng;

interface
uses
  Windows,
  dialogs,
  Classes,
  SysUtils,
  ffllbase,
  fflldict,
  ffdtmsgq,
  ffllcomm,
  ffllcomp,
  fflleng,
  ffllexcp,
  ffllreq,
  ffnetmsg,
  ffsrbde,
  ffsrintm,
  ffdbbase;

type
  {forward declarations}
  TFFRemoteServerEngine   = class;
    {The TffRemoteServerEngine implements the TFFBaseServerEngine abstract
     methods. It's method calls will initiate the process that will format
     a message request to be sent to a remote server via a transport.
     The TffRemoteServerEngine methods sometimes pass buffers without passing
     the buffer length.  However, the length must be known in order for the
     message to be sent.

     It is also possible for the TffRemoteServerEngine to be accessed by
     multiple threads.  We want to make sure that messages for one thread don't
     wind up with another thread.

     To handle cases such as these, the TffRemoteServerEngine needs to track
     information specific to a cursor and client, respectively. To this
     end we have created proxy classes to hold the information.  For
     example, a TffProxyCursor holds information specific to an open cursor.
     A TffProxyClient holds information specific to an open client.

     The TffRemoteServerEngine creates an instance of a proxy class when its
     equivalent server-side object is opened.  Instead of returning the
     server-side object's ID to the object(s) using the remote engine, the
     remote engine returns the pointers to its proxy objects.  This scheme
     allows TffRemoteServerEngine to derive a server-side ID from its proxy
     object and allows it to maintain information required for its operation.

     In general, all calls to remote server engine wind up calling a method on
     a TffProxy class which in turn formats a request and sends it through
     TffProxyClient.}

  TFFProxyClientList      = class;
  TFFProxySession         = class;
  TFFProxySessionList     = class;
  TFFProxyDatabase        = class;
  TFFProxyDatabaseList    = class;
  TFFProxyCursor          = class;
  TFFProxyCursorList      = class;
  TffProxySQLStmt         = class;
  TffProxySQLStmtList     = class;
  {-End forward declarations}

  {Creating/destroying and ownership issues.
    The TFFProxyClient object will be created/destroyed and owned by it's
     parent, a TFFRemoteServerEngine. The TFFRemoteServerEngine will be
     responsible for keeping a list of the afore mentioned object.

   The TFFProxySession object, and the TFFProxyDatabase object will be
     created/destroyed and owned by it's parent, a TFFProxyClient. The
     TFFProxyClient will be responsible for keeping a list of all instances
     of the afore mentioned objects.

   The TFFProxyCursor object will be created/destroyed and owned by
     it's parent, a TFFProxyDatabase. The TFFProxyDatabase will be responsible
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


  {TFFProxyClient
    { The proxy client controls interaction between the remote server engine
    and the transport. This class contains a message queue associated with
    a specific client. All requests for data must go through this class'
    ProcessRequest method. Instances where a reply from the server isn't
    necessary can use the ProcessRequestNoReply method. }


  TFFProxyClient = class(TffObject)
    protected
      pcSrClientID : TffClientID;
        {An ID pointing to the associated TFFSrClient class on the server}

      pcMsgQueue : TffDataMessageQueue;
        {The message queue used to store replies to this client. }

      pcCallbackMethod : TffReplyCallback;
        {A Method pointer that will be passed to the transport when a
         reply is requested.}

      pcCurrentSession : TffProxySession;
        {The current session as set by the SessionSetCurrent method}

      pcDatabases : TFFProxyDatabaseList;
        {The databases that are managed by the client}

      pcForceClosed : Boolean;

      pcTransport : TffBaseTransport;
        {A reference to the RemoteServerEngine's transport. Added here for
         purposes of speed, and readability.}

      pcSessions : TFFProxySessionList;
        {The sessions that are registered with the client. }

      pcTimeout : Longint;
        {The current timeout setting for the TFFBaseConnection Class. The
         TFFBaseConnection class is resposible for updating this object when
         it's published timeout value is changed.}

    public
      constructor Create(aTransport    : TffBaseTransport;
                         aUserName     : TFFName;
                         aPasswordHash : Longint;
                         aTimeOut      : Longint);
      destructor Destroy; override;

      function IsReadOnly : Boolean;

      function ProcessRequest(aMsgID           : Longint;
                              aTimeout         : Longint;
                              aRequestData     : Pointer;
                              aRequestDataLen  : Longint;
                              aRequestDataType : TffNetMsgDataType;
                          var aReply           : Pointer;
                          var aReplyLen        : Longint;
                              aReplyType       : TffNetMsgDataType) : TffResult;
      { Use the ProxessRequest method to submit a request that is routed to the
        transport.  This method does the following:

        1. Calls TffBaseTransport.Request with transportID = 0 and cookie
           equal to Pointer(Self).  At this point, the calling thread is
           blocked until a reply is received from the server or a timeout
           occurs.
        2. When the calling thread returns to this method, the reply has
           been received and placed in the message queue by the
           ProxyClientCallback procedure.
        3. Verify the message is the type that we expected.
        4. Put the message into the MessageQueue and exit.}


      function ProcessRequestNoReply(aMsgID          : Longint;
                                     aTimeout        : Longint;
                                     aRequestData    : Pointer;
                                     aRequestDataLen : Longint) : TffResult;
      { Use the ProxessRequestNoReply method to submit a request that is
        routed to the transport. This method does the following:

        1. Calls TffBaseTransport.Post with transportID = 0 and reply mode
           to waituntilsent.  At this point, the calling thread is
           blocked until the request has been sent to the server.}
       function DatabaseClose(aDatabase : TffProxyDatabase) : TffResult;
       function DatabaseOpen(const aAlias      : TffName;
                                   aOpenMode   : TffOpenMode;
                                   aShareMode  : TffShareMode;
                                   aTimeout    : Longint;
                               var aDatabaseID : TffDatabaseID) : TffResult;
         {Add a database to the pcDatabases list. The client will take
          care of creating}

       function DatabaseOpenNoAlias(const aPath       : TffPath;
                                          aOpenMode   : TffOpenMode;
                                          aShareMode  : TffShareMode;
                                          aTimeout    : Longint;
                                      var aDatabaseID : TffDatabaseID
                                   ) : TffResult;
      function GetRebuildStatus(const aRebuildID : Longint;
                                  var aIsPresent : Boolean;
                                  var aStatus    : TffRebuildStatus) : TffResult;
      function SetTimeout(const aTimeout : Longint) : TffResult;
      function SessionAdd(var aSessionID : TffSessionID;
                        const aTimeout   : Longint) : TffResult;
        {Add a session to the pcSessions list. The client will take
        care of creating the TFFProxySession object, whose ID will
        be returned via aSessionID.}

      function SessionCloseInactiveTables : TffResult;    {!!.06}
        { Close the inactive tables on the server. }

      function SessionCount : Longint;
        {Retrieve the number of sessions the client is managing.}

      function SessionGetCurrent : TffProxySession;
        {Retrieve the current session}

      function SessionRemove(aSession : TFFProxySession) : TffResult;
        {Remove the session from the list. The client will take destroy
         the session, and remove it from the list}

      function SessionSetCurrent(aSession : TFFProxySession) : TffResult;
        {Set the current session}

      function DatabaseAddAlias(const aAlias      : TffName;
                                const aPath       : TffPath;
                                      aCheckSpace : Boolean)           {!!.11}
                                                  : TffResult;
      function DatabaseAliasList(aList : TList) : TffResult;
      function DatabaseChgAliasPath(const aAlias      : TffName;
                                    const aNewPath    : TffPath;
                                          aCheckSpace : Boolean)       {!!.11}
                                                      : TffResult;
      function DatabaseDeleteAlias(const aAlias : TffName) : TffResult;
      function DatabaseGetAliasPath(const aAlias    : TffName;
                                      var aPath     : TffPath) : TffResult;
      function DatabaseModifyAlias(const aAlias      : TffName;
                                   const aNewName    : TffName;
                                   const aNewPath    : TffPath;
                                         aCheckSpace : Boolean)        {!!.11}
                                                     : TffResult;

      function GetServerDateTime(var aDateTime : TDateTime) : TffResult;
                                                                       {begin !!.10}
      function GetServerSystemTime(var aSystemTime : TSystemTime)
                                                   : TffResult;
      function GetServerGUID(var aGUID : TGUID) : TffResult;
      function GetServerID(var aUniqueID : TGUID) : TffResult;
      function GetServerStatistics(var Stats : TffServerStatistics)
                                             : TffResult;
      function GetCommandHandlerStatistics(const CmdHandlerIdx : Integer;
                                             var Stats         : TffCommandHandlerStatistics)
                                                               : TffResult;
      function GetTransportStatistics(const CmdHandlerIdx : Integer;
                                      const Transportidx  : Integer;
                                        var Stats         : TffTransportStatistics)
                                                          : TffResult;
                                                                       {end !!.10}


{Begin !!.01}
      function RemoteRestart : TffResult;
        { Tell the remote server to restart. }

      function RemoteStart : TffResult;
        { Tell the remote server to startup. }

      function RemoteStop : TffResult;
        { Tell the remote server to stop. }
{End !!.01}

      {ReadOnly properties for the protected fields}
      property CurrentSession : TFFProxySession
         read SessionGetCurrent;
      property Databases : TFFProxyDatabaseList
         read pcDatabases;
      property ForceClosed : Boolean
         read pcForceClosed
         write pcForceClosed;
      property MsgQueue : TFFDataMessageQueue
         read pcMsgQueue;
      property Sessions : TFFProxySessionList
         read pcSessions;
      property SrClientID : TffClientID
         read  pcSrClientID;
      property Transport : TFFBaseTransport
         read pcTransport;
      property Timeout : Longint
         read pcTimeout;
  end;

  {List containing a reference for every ProxyClient owned by
   a TFFRemoteServerEngine component.}
  TFFProxyClientList = class(TffThreadList);


  {The TFFProxySession is used primarily to keep track of the
   the current Timeout setting, and the Server CursorID.
   Unlike the TFFSession, the ProxySession does not manage a
   set of Databases. TFFProxyDatabases, instead, are managed by
   the ProxyClient class}
  TFFProxySession = class(TFFObject)
    protected
      psSrSessionID : TFFSessionID;
        {An ID pointing to the TFFSrSession object on the remote server}

      psClient      : TFFProxyClient;
        {A reference to the client who owns this object}

      psTimeout     : Longint;
        {Local storage for the current Session timeout setting. The TFFSession
         object is resposible for keeping this value up to date.}

    public
      constructor Create(aClient : TFFProxyClient; aTimeout : Longint);

      destructor Destroy; override;

      function SetTimeout(aTimeout : Longint) : TffResult;

      {ReadOnly properties for the protected fields}
      property SrSessionID : TFFSessionID
         read psSrSessionID;
      property Client : TFFProxyClient
         read psClient;
      property Timeout : LongInt
         read psTimeout;
  end;

  {List containing a reference for every ProxySesion owned by
   a TFFProxyClient object.}
  TFFProxySessionList = class(TffThreadList);


  {The TFFProxyDatabase is responsible for basic Table maintenance. It also
   keeps track of the the current Timeout setting, and the Server CursorID.
   TFFProxyDatabase maintains a list of TFFProxyCursor objects.}
  TFFProxyDatabase = class(TffObject)
    protected
      pdSrDatabaseID : TffDatabaseID;
        {An ID pointing to the TffSrDatabase object on the remote server}

      pdClient       : TFFProxyClient;
        {A reference to the client who owns this object}

      pdInTrans      : Boolean;
        {Have we instantiated a tranaction? }

      pdStmts        : TffProxySQLStmtList;
        {The SQL statements managed by this database}

      pdTables       : TFFProxyCursorList;
        {The tables that are managed by the database}

      pdTimeout      : Longint;
        {Local storage for the current Database timeout setting. The TFFDatabase
         object is resposible for keeping this value up to date.}
    public
      constructor Create(aClient    : TFFProxyClient;
                         aLocation  : string;
                         aOpenMode  : TffOpenMode;
                         aShareMode : TffShareMode;
                         aTimeout   : Longint;
                         aIsAlias   : Boolean);
      destructor Destroy; override;
      function GetDBFreeSpace(var aFreeSpace : Longint) : TffResult;
      function QueryOpen(aCursorID : TffCursorID;
                         aOpenMode : TffOpenMode;
                         aShareMode : TffShareMode;
                         aTimeout  : longInt;
                         aStream   : TStream;
                     var aFinalCursorID : TffCursorID) : TffResult;
      function SetTimeout(const aTimeout : Longint) : TffResult;
      function SQLAlloc(const aTimeout : longInt;
                          var aStmtID : TffSqlStmtID) : TffResult;
      function SQLExecDirect(aQueryText  : PChar;
                             aOpenMode   : TffOpenMode;
                             aTimeout    : longInt;
                         var aCursorID   : TffCursorID;
                             aStream     : TStream) : TffResult;
      function TableExists(const aTableName  : TffTableName;
                             var aExists     : Boolean) : TffResult;
      function TableList(const aMask : TffFileNameExt;
                               aList : TList) : TffResult;
      function TableLockedExclusive(const aTableName  : TffTableName;
                                      var aLocked     : Boolean) : TffResult;
      function TableAddIndex(const aCursorID   : TffCursorID;
                             const aTableName  : TffTableName;
                             const aIndexDesc  : TffIndexDescriptor) : TffResult;
      function TableBuild(aOverWrite  : Boolean;
                    const aTableName  : TffTableName;
                          aForServer  : Boolean;
                          aDictionary : TffDataDictionary) : TffResult;
      function TableDelete(const aTableName  : TffTableName) : TffResult;
      function TableDropIndex(aCursorID   : TffCursorID;
                        const aTableName  : TffTableName;
                        const aIndexName  : TffDictItemName;
                              aIndexID    : longint) : TffResult;
      function TableEmpty(aCursorID  : TffCursorID;
                    const aTableName : TffTableName) : TffResult;
      function TableGetDictionary(const aTableName  : TffTableName;
                                        aForServer  : Boolean;
                                        aStream     : TStream) : TffResult;
      function TableClose(aCursor : TFFProxyCursor) : TffResult;
      function TableOpen(const aTableName : TffTableName;
                               aForServer : Boolean;
                               aIndexName : TffName;
                               aIndexID   : Longint;
                               aOpenMode  : TffOpenMode;
                               aShareMode : TffShareMode;
                               aTimeout   : Longint;
                           var aCursorID  : TffCursorID;
                               aStream    : TStream) : TffResult;
      function TablePack(const aTableName  : TffTableName;
                           var aRebuildID  : Longint) : TffResult;
      function TableRebuildIndex(const aTableName : TffTableName;
                                 const aIndexName : TffName;
                                       aIndexID   : Longint;
                                   var aRebuildID : Longint) : TffResult;
      function TableRename(const aOldName : TffName;
                           const aNewName : TffName) : TffResult;
      function TableRestructure(const aTableName  : TffTableName;
                                      aDictionary : TffDataDictionary;
                                      aFieldMap   : TffStringList;
                                  var aRebuildID  : Longint) : TffResult;
      function TransactionStart(aFailSafe      : Boolean) : TffResult;
{Begin !!.10}
      function TransactionStartWith(const aFailSafe : Boolean;
                                    const aCursorIDs : TffPointerList) : TffResult;
{End !!.10}
      function TransactionCommit : TffResult;
      function TransactionRollback : TffResult;

      property Client : TFFProxyClient
         read pdClient;
      property InTrans : Boolean
         read pdInTrans;
      property SrDatabaseID : TFFDatabaseID
         read pdSrDatabaseID;
      property Tables : TffProxyCursorList
         read pdTables;
      property Timeout : Longint
         read pdTimeout;
  end;

  TFFProxyDatabaseList = class(TffThreadList);

  TFFProxyCursor = class(TffObject)
    protected
      prSrCursorID : TffCursorID;
      prClient     : TFFProxyClient;
      prForServer  : Boolean;
      prShareMode  : TffShareMode;
      prTableName  : TFFTableName;
      prTimeout    : Longint;
      prDatabase   : TFFProxyDatabase;

      {State Variables}
      prDictionary   : TffDataDictionary;
      prIndexID      : Longint;
      prIndexName    : string;
      prIsSQLCursor  : boolean;
      prPhyRecSize   : Longint;
    protected
      function prGetBookmarkSize : Longint;
    public
      constructor Create(aDatabase  : TFFProxyDatabase;
                         aCursorID  : TffCursorID;  {used by CursorClone, otherwise set to 0}
                         aTableName : string;
                         aForServer : Boolean;
                         aIndexName : string;
                         aIndexID   : Longint;
                         aOpenMode  : TffOpenMode;
                         aShareMode : TffShareMode;
                         aTimeout   : LongInt;
                         aStream    : TStream);

      constructor CreateSQL(aDatabase  : TffProxyDatabase;
                            aCursorID  : TffCursorID;
                            aOpenMode  : TffOpenMode;
                            aShareMode : TffShareMode;
                            aTimeout   : longInt;
                            aStream    : TStream);
        { This constructor is used to construct a proxy cursor for an executed
          SQL statement. }

      destructor Destroy; override;
      function BlobCreate(var aBlobNr : TFFInt64) : TFFResult;
      function BLOBDelete(aBlobNr : TFFInt64) : TffResult;
      function BLOBFree(aBlobNr   : TffInt64;
                        aReadOnly : Boolean) : TFFResult;
      function BLOBGetLength(aBlobNr : TffInt64;
                         var aLength : Longint) : TffResult;
{Begin !!.03}
      function BLOBListSegments(aBLOBNr : TffInt64;
                                aStream : TStream) : TffResult;
{End !!.03}
      function BLOBRead(aBlobNr    : TffInt64;
                        aOffset    : TffWord32;                        {!!.06}
                        aLen       : TffWord32;                        {!!.06}
                    var aBLOB;
                    var aBytesRead : TffWord32)                        {!!.06}
                                   : TffResult;
      function BLOBTruncate(aBlobNr     : TffInt64;
                            aBLOBLength : Longint) : TffResult;
      function BLOBWrite(aBlobNr : TffInt64;
                         aOffset : Longint;
                         aLen    : Longint;
                     var aBLOB) : TFFResult;
      function CursorClone(aOpenMode : TFFOpenMode;
                       var aNewCursorID : TFFCursorID) : TFFResult;
      function CompareBookmarks(aBookmark1  : PffByteArray;
                                aBookmark2  : PffByteArray;
                            var aCompResult : Longint) : TffResult;
      function CopyRecords(aSrcCursor : TffProxyCursor;                {!!.02}
                           aCopyBLOBs : Boolean) : TffResult;          {!!.02}
      function DeleteRecords : TffResult;                              {!!.06}
      function GetBookmark(aBookmark : PffByteArray) : TffResult;
      function GetBookmarkSize(var aSize : Longint) : TffResult;
{Begin !!.03}
      function ListBLOBFreeSpace(const aInMemory : Boolean;
                                       aStream : TStream) : TffResult;
{End !!.03}
      function OverrideFilter(aExpression : pCANExpr;
                              aTimeout    : TffWord32) : TffResult;
      function ResetRange : TffResult;
      function RestoreFilter : TffResult;
      function SetFilter(aExpression : pCANExpr;
                         aTimeout    : TffWord32) : TffResult;
      function SetRange(aDirectKey   : Boolean;
                        aFieldCount1 : Longint;
                        aPartialLen1 : Longint;
                        aKeyData1    : PffByteArray;
                        aKeyIncl1    : Boolean;
                        aFieldCount2 : Longint;
                        aPartialLen2 : Longint;
                        aKeyData2    : PffByteArray;
                        aKeyIncl2    : Boolean) : TffResult;
      function SetTimeout(aTimeout : Longint) : TffResult;
      function SetToBegin : TffResult;
      function SetToBookmark(aBookmark : PffByteArray) : TffResult;
      function SetToCursor(aSourceCursor : TFFProxyCursor) : TffResult;
      function SetToEnd : TffResult;
      function SetToKey(aSearchAction : TffSearchKeyAction;
                        aDirectKey    : Boolean;
                        aFieldCount   : Longint;
                        aPartialLen   : Longint;
                        aKeyData      : PffByteArray) : TffResult;
      function SwitchToIndex(aIndexName : TffDictItemName;
                             aIndexID   : Longint;
                             aPosnOnRec : Boolean) : TffResult;
      function FileBLOBAdd(const aFileName : TffFullFileName;
                             var aBlobNr   : TffInt64) : TffResult;
      function RecordDelete(aData : PffByteArray) : TffResult;
      function RecordDeleteBatch(aBMCount  : Longint;
                                 aBMLen    : Longint;
                                 aData     : PffByteArray;
                                 aErrors   : PffLongintArray) : TffResult;
      function RecordExtractKey(aData : PffByteArray;
                                aKey  : PffByteArray) : TffResult;
      function RecordGet(aLockType : TffLockType;
                         aData     : PffByteArray) : TffResult;
      function RecordGetBatch(aRecCount : Longint;
                              aRecLen   : Longint;
                          var aRecRead  : Longint;
                              aData     : PffByteArray;
                          var aError    : TffResult) : TffResult;
      function RecordGetForKey(aDirectKey  : Boolean;
                               aFieldCount : Longint;
                               aPartialLen : Longint;
                               aKeyData    : PffByteArray;
                               aData       : PffByteArray;
                               aFirstCall  : Boolean) : TffResult;
      function RecordGetNext(aLockType : TffLockType;
                             aData     : PffByteArray) : TffResult;
      function RecordGetPrior(aLockType : TffLockType;
                              aData     : PffByteArray) : TffResult;
      function RecordInsert(aLockType : TffLockType;
                            aData     : PffByteArray) : TffResult;
      function RecordInsertBatch(aRecCount : Longint;
                                 aRecLen   : Longint;
                                 aData     : PffByteArray;
                                 aErrors   : PffLongintArray) : TffResult;
      function RecordIsLocked(aLockType : TffLockType;
                          var aIsLocked : boolean) : TffResult;
      function RecordModify(aData    : PffByteArray;
                            aRelLock : Boolean) : TffResult;
      function RecordRelLock(aAllLocks : Boolean) : TffResult;
      function TableGetAutoInc(var aValue : TffWord32) : TffResult;
      function TableGetRecCount(var aRecCount : Longint) : TffResult;
      function TableGetRecCountAsync(var aTaskID : Longint) : TffResult; {!!.07}
      function TableIsLocked(aLockType : TffLockType;
                         var aIsLocked : Boolean) : TffResult;
      function TableLockAcquire(aLockType : TffLockType) : TffResult;
      function TableLockRelease(aAllLocks : Boolean) : TffResult;
      function TableSetAutoInc(aValue : TffWord32) : TffResult;

      property Client : TFFProxyClient
         read prClient;
      property SrCursorID : TffCursorID
         read prSrCursorID;
     property Timeout : Longint
        read prTimeout;
     property BookmarkSize : longint
        read prGetBookmarkSize;
     property Database : TFFProxyDatabase
        read prDatabase;
     property Dictionary : TffDataDictionary
        read prDictionary;
     property IndexID : Longint
        read prIndexID;
     property PhysicalRecordSize : Longint
        read prPhyRecSize;

  end;

  TFFProxyCursorList = class(TffThreadList);

  TffProxySQLStmt = class(TffObject)
  protected {private}

    psClient   : TffProxyClient;
      { The proxy client through which requests are routed. }

    psDatabase : TffProxyDatabase;
      { The proxy database with which the SQL statement is associated. }

    psSrStmtID : TffSqlStmtID;
      { The actual statement ID. }

    psTimeout  : longInt;
      { The SQL statement's timeout (in milliseconds). }

  public
    {creation/destruction}
    constructor Create(aDatabase : TffProxyDatabase; const aTimeout : longInt);
    destructor Destroy; override;

    function Exec(aOpenMode : TffOpenMode;
              var aCursorID : TffCursorID;
                  aStream   : TStream) : TffResult;

    function Prepare(aQueryText: PChar; aStream : TStream) : TffResult;

    function SetParams(aNumParams : Word;
                      aParamDescs : Pointer;
                      aDataBuffer : PffByteArray;
                      aDataLen    : Longint;
                      aStream     : TStream) : TffResult;

    property Database : TffProxyDatabase read psDatabase;

    property SrStmtID : TffSqlStmtID read psSrStmtID;
      { The statement ID returned by the server engine. }

  end;

  TffProxySQLStmtList = class(TffThreadList);

  TFFRemoteServerEngine = class(TffIntermediateServerEngine)
  private
    protected {private}
      rsClientList      : TFFProxyClientList;
      rsTimeout         : TffWord32;
      rsTransport       : TffBaseTransport;
{Begin !!.06}
      function ProcessRequest(aClientID        : TffClientID;
                              aMsgID           : Longint;
                              aTimeout         : Longint;
                              aRequestData     : Pointer;
                              aRequestDataLen  : Longint;
                              aRequestDataType : TffNetMsgDataType;
                          var aReply           : Pointer;
                          var aReplyLen        : Longint;
                              aReplyType       : TffNetMsgDataType) : TffResult; override;
        { Backdoor method for sending a request to a server engine.
          Should only be implemented by remote server engines. }

      function ProcessRequestNoReply(aClientID       : TffClientID;
                                     aMsgID          : Longint;
                                     aTimeout        : Longint;
                                     aRequestData    : Pointer;
                                     aRequestDataLen : Longint ) : TffResult; override;
        { Backdoor method for sending a request, no reply expected, to a
          server engine. Should only be implemented by remote server engines. }
{End !!.06}
      procedure rsSetTransport(const Value : TFFBaseTransport);
//  protected                                                         {!!.01 - Start - Made public}
//    {validation and checking}
//    function CheckClientIDAndGet(aClientID : TffClientID;
//                             var aClient   : TffProxyClient) : TffResult;
//    function CheckSessionIDAndGet(aClientID  : TffClientID;
//                                  aSessionID : TffSessionID;
//                              var aClient    : TffProxyClient;
//                              var aSession   : TffProxySession) : TffResult;
//    function CheckDatabaseIDAndGet(aDatabaseID : TffDatabaseID;
//                               var aDatabase   : TffProxyDatabase) : TffResult;
//      {-Find the database specified by aDatabaseID. }
//
//    function CheckCursorIDAndGet(aCursorID : TffCursorID;
//                             var aCursor   : TffProxyCursor) : TffResult;
//      {-Find the cursor specified by aCursorID. }
//
//    function CheckStmtIDAndGet(aStmtID : TffSqlStmtID;
//                          var  aStmt   : TffProxySQLStmt) : TffResult;
//      {-Find the statement specified by aStmtID. }                  {!!.01 - End}

    protected
      {State methods}
      procedure scInitialize; override;
      procedure scPrepareForShutdown; override;
      procedure scShutdown; override;
      procedure scStartup; override;
      function bseGetAutoSaveCfg : Boolean; override;
      function bseGetReadOnly : Boolean; override;
      procedure bseSetAutoSaveCfg(aValue : Boolean); override;         {!!.01}
      procedure bseSetReadOnly(aValue : Boolean); override;            {!!.01}
    public
{Begin !!.07}
      { Event logging }
      procedure Log(const aMsg : string); override;
        {-Use this method to log a string to the event log. }

      procedure LogAll(const Msgs : array of string); override;
        {-Use this method to log multiple strings to the event log. }

      procedure LogFmt(const aMsg : string; args : array of const); override;
        {-Use this method to log a formatted string to the event log. }
{End !!.07}

{Begin !!.01 - moved from protected section}
      {validation and checking}
      function CheckClientIDAndGet(aClientID : TffClientID;
                               var aClient   : TffProxyClient) : TffResult;
      function CheckSessionIDAndGet(aClientID  : TffClientID;
                                    aSessionID : TffSessionID;
                                var aClient    : TffProxyClient;
                                var aSession   : TffProxySession) : TffResult;
      function CheckDatabaseIDAndGet(aDatabaseID : TffDatabaseID;
                                 var aDatabase   : TffProxyDatabase) : TffResult;
        {-Find the database specified by aDatabaseID. }

      function CheckCursorIDAndGet(aCursorID : TffCursorID;
                               var aCursor   : TffProxyCursor) : TffResult;
        {-Find the cursor specified by aCursorID. }

      function CheckStmtIDAndGet(aStmtID : TffSqlStmtID;
                            var  aStmt   : TffProxySQLStmt) : TffResult;
        {-Find the statement specified by aStmtID. }
{End !!.01}

      {creation/destruction}
      constructor Create(aOwner : TComponent); override;
      destructor Destroy; override;
      procedure FFNotificationEx(const AOp : Byte; AFrom : TffComponent;
                                 const AData : TffWord32); override;

      function GetDefaultClient : TFFProxyClient;

      procedure GetServerNames(aList    : TStrings;
                               aTimeout : Longint); override;

      procedure ForceClosing(const aClientID : TffClientID);

{Begin !!.01}
      function RemoteRestart(const aClientID : TffClientID) : TffResult;
        { Tell the remote server to shutdown and startup. }

      function RemoteStart(const aClientID : TffClientID) : TffResult;
        { Tell the remote server to startup. Only works if the remote server
          is in a stopped state (i.e., transports & cmd handlers still
          listening. }

      function RemoteStop(const aClientID : TffClientID) : TffResult;
        { Tell the remote server to stop. The server engine shuts down but
          the transport and cmd handlers will still be listening. }
{End !!.01}

      {transaction tracking}
      function TransactionCommit(const aDatabaseID : TffDatabaseID
                                 ) : TffResult; override;
      function TransactionRollback(const aDatabaseID : TffDatabaseID
                                   ) : TffResult; override;
      function TransactionStart(const aDatabaseID    : TffDatabaseID;
                                const aFailSafe      : Boolean
                                ) : TffResult; override;
{Begin !!.10}
      function TransactionStartWith(const aDatabaseID : TffDatabaseID;
                                    const aFailSafe : Boolean;
                                    const aCursorIDs : TffPointerList
                                    ) : TffResult; override;
{End !!.10}

      {client related stuff}
      function ClientAdd(var aClientID   : TffClientID;
                       const aClientName : TffNetName;
                       const aUserID     : TffName;
                       const aTimeout    : Longint;
                         var aHash       : TffWord32) : TffResult; override;
{Begin !!.11}
      function ClientAddEx(var aClientID   : TffClientID;
                         const aClientName : TffNetName;
                         const aUserID     : TffName;
                         const aTimeout     : Longint;
                         const aClientVersion : Longint;
                           var aHash       : TffWord32) : TffResult; override;
        { Same as ClientAdd but client version is supplied via the aClientVersion
          parameter. }
{End !!.11}
      function ClientRemove(aClientID : TffClientID) : TffResult; override;
      function ClientSetTimeout(const aClientID : TffClientID;
                                const aTimeout : longInt) : TffResult; override;


      {client session related stuff}
      function SessionAdd(const aClientID  : TffClientID;
                          const aTimeout   : Longint;
                            var aSessionID : TffSessionID) : TffResult; override;
      function SessionCloseInactiveTables(aClientID  : TffClientID) : TffResult; override; {!!.06}
      function SessionCount(aClientID : TffClientID;
                        var aCount    : Longint) : TffResult; override;
      function SessionGetCurrent(aClientID  : TffClientID;
                             var aSessionID : TffSessionID
                                 ) : TffResult; override;
      function SessionRemove(aClientID  : TffClientID;
                             aSessionID : TffSessionID) : TffResult; override;
      function SessionSetTimeout(const aClientID : TffClientID;
                                 const aSessionID : TffSessionID;
                                 const aTimeout   : Longint) : TffResult; override;
      function SessionSetCurrent(aClientID  : TffClientID;
                                 aSessionID : TffSessionID
                                 ) : TffResult; override;

      {database related stuff}
      function DatabaseAddAlias(const aAlias      : TffName;
                                const aPath       : TffPath;
                                      aCheckSpace : Boolean;           {!!.11}
                                const aClientID   : TffClientID)
                                                  : TffResult; override;
      function DatabaseAliasList(aList     : TList;
                                 aClientID : TffClientID)
                                           : TffResult; override;
      function RecoveryAliasList(aList     : TList;
                                 aClientID : TffClientID)
                                           : TffResult; override;
      function DatabaseChgAliasPath(aAlias      : TffName;
                                    aNewPath    : TffPath;
                                    aCheckSpace : Boolean;             {!!.11}
                                    aClientID   : TffClientID)
                                                : TffResult; override;
      function DatabaseClose(aDatabaseID : TffDatabaseID) : TffResult; override;
      function DatabaseDeleteAlias(aAlias    : TffName;
                                   aClientID : TffClientID) : TffResult; override;
      function DatabaseGetAliasPath(aAlias    : TffName;
                                var aPath     : TffPath;
                                    aClientID : TffClientID) : TffResult; override;
      function DatabaseGetFreeSpace(const aDatabaseID : TffDatabaseID;
                                      var aFreeSpace  : Longint) : TffResult; override;
      function DatabaseModifyAlias(const aClientID   : TffClientID;
                                   const aAlias      : TffName;
                                   const aNewName    : TffName;
                                   const aNewPath    : TffPath;
                                         aCheckSpace : Boolean)        {!!.11}
                                                     : TffResult; override;
      function DatabaseOpen(aClientID   : TffClientID;
                      const aAlias      : TffName;
                      const aOpenMode   : TffOpenMode;
                      const aShareMode  : TffShareMode;
                      const aTimeout    : Longint;
                        var aDatabaseID : TffDatabaseID) : TffResult; override;
      function DatabaseOpenNoAlias(aClientID   : TffClientID;
                             const aPath       : TffPath;
                             const aOpenMode   : TffOpenMode;
                             const aShareMode  : TffShareMode;
                             const aTimeout    : Longint;
                               var aDatabaseID : TffDatabaseID
                                   ) : TffResult; override;
      function DatabaseSetTimeout(const aDatabaseID : TffDatabaseID;
                                  const aTimeout    : Longint) : TffResult; override;
      function DatabaseTableExists(aDatabaseID : TffDatabaseID;
                             const aTableName  : TffTableName;
                               var aExists     : Boolean) : TffResult; override;
      function DatabaseTableList(aDatabaseID : TffDatabaseID;
                           const aMask       : TffFileNameExt;
                                 aList       : TList) : TffResult; override;
      function DatabaseTableLockedExclusive(aDatabaseID : TffDatabaseID;
                                      const aTableName  : TffTableName;
                                        var aLocked     : Boolean) : TffResult; override;

      {rebuild status related stuff}
      function RebuildGetStatus(aRebuildID : longint;
                          const aClientID : TffClientID;
                            var aIsPresent : Boolean;
                            var aStatus    : TffRebuildStatus
                                ) : TffResult; override;

      {table related stuff}
      function TableAddIndex(const aDatabaseID : TffDatabaseID;
                             const aCursorID   : TffCursorID;
                             const aTableName  : TffTableName;
                             const aIndexDesc  : TffIndexDescriptor
                             ) : TffResult; override;
      function TableBuild(aDatabaseID : TffDatabaseID;
                          aOverWrite  : Boolean;
                    const aTableName  : TffTableName;
                          aForServer  : Boolean;
                          aDictionary : TffDataDictionary
                          ) : TffResult; override;
      function TableDelete(aDatabaseID : TffDatabaseID;
                     const aTableName  : TffTableName) : TffResult; override;
      function TableDropIndex(aDatabaseID : TffDatabaseID;
                              aCursorID   : TffCursorID;
                        const aTableName  : TffTableName;
                        const aIndexName  : TffDictItemName;
                              aIndexID    : longint) : TffResult; override;
      function TableEmpty(aDatabaseID : TffDatabaseID;
                          aCursorID   : TffCursorID;
                    const aTableName  : TffTableName) : TffResult; override;
      function TableGetAutoInc(aCursorID   : TffCursorID;
                           var aValue      : TffWord32) : TffResult; override;
      function TableGetDictionary(aDatabaseID : TffDatabaseID;
                            const aTableName  : TffTableName;
                                  aForServer  : Boolean;
                                  aStream     : TStream) : TffResult; override;
      function TableGetRecCount(aCursorID : TffCursorID;
                            var aRecCount : longint) : TffResult; override;
      function TableGetRecCountAsync(aCursorID : TffCursorID;                  {!!.07}
                                 var aTaskID : Longint) : TffResult; override; {!!.07}
      function TableOpen(const aDatabaseID : TffDatabaseID;
                         const aTableName  : TffTableName;
                         const aForServer  : Boolean;
                         const aIndexName  : TffName;
                               aIndexID    : longint;
                         const aOpenMode   : TffOpenMode;
                               aShareMode  : TffShareMode;
                         const aTimeout    : Longint;
                           var aCursorID   : TffCursorID;
                               aStream     : TStream) : TffResult; override;
      function TablePack(aDatabaseID : TffDatabaseID;
                   const aTableName  : TffTableName;
                     var aRebuildID  : longint) : TffResult; override;
      function TableRebuildIndex(aDatabaseID : TffDatabaseID;
                           const aTableName  : TffTableName;
                           const aIndexName  : TffName;
                                 aIndexID    : longint;
                             var aRebuildID  : longint) : TffResult; override;
      function TableRename(aDatabaseID : TffDatabaseID;
                     const aOldName    : TffName;
                     const aNewName    : TffName) : TffResult; override;
      function TableRestructure(aDatabaseID : TffDatabaseID;
                          const aTableName  : TffTableName;
                                aDictionary : TffDataDictionary;
                                aFieldMap   : TffStringList;
                            var aRebuildID  : longint) : TffResult; override;
      function TableSetAutoInc(aCursorID : TffCursorID;
                               aValue    : TffWord32) : TffResult; override;
{Begin !!.11}
      function TableVersion(aDatabaseID : TffDatabaseID;
                      const aTableName  : TffTableName;
                        var aVersion : Longint) : TffResult; override;
{End !!.11}

      {table locks via cursor}
      function TableIsLocked(aCursorID : TffCursorID;
                             aLockType : TffLockType;
                         var aIsLocked : Boolean) : TffResult; override;
      function TableLockAcquire(aCursorID : TffCursorID;
                                aLockType : TffLockType) : TffResult; override;
      function TableLockRelease(aCursorID : TffCursorID;
                                aAllLocks : Boolean) : TffResult; override;

      {cursor stuff}
      function CursorClone(aCursorID    : TffCursorID;
                           aOpenMode    : TffOpenMode;
                       var aNewCursorID : TffCursorID) : TffResult; override;
      function CursorClose(aCursorID : TffCursorID) : TffResult; override;
      function CursorCompareBookmarks(aCursorID : TffCursorID;
                                      aBookmark1,
                                      aBookmark2  : PffByteArray;
                                  var aCompResult : longint) : TffResult; override;
{Begin !!.02}
      function CursorCopyRecords(aSrcCursorID,
                                aDestCursorID : TffCursorID;
                                aCopyBLOBs : Boolean) : TffResult; override;
{End !!.02}
      function CursorDeleteRecords(aCursorID : TffCursorID) : TffResult; override;  {!!.06}
      function CursorGetBookmark(aCursorID : TffCursorID;
                                 aBookmark : PffByteArray) : TffResult; override;

      function CursorGetBookmarkSize(aCursorID : TffCursorID;
                                 var aSize     : Longint) : TffResult; override;
{Begin !!.03}
      function CursorListBLOBFreeSpace(aCursorID : TffCursorID;
                                 const aInMemory : Boolean;
                                       aStream : TStream) : TffResult; override;
{End !!.03}
      function CursorOverrideFilter(aCursorID   : Longint;
                                    aExpression : pCANExpr;
                                    aTimeout    : TffWord32) : TffResult; override;
      function CursorResetRange(aCursorID : TffCursorID) : TffResult; override;
      function CursorRestoreFilter(aCursorID : longInt) : TffResult; override;
      function CursorSetRange(aCursorID    : TffCursorID;
                              aDirectKey   : Boolean;
                              aFieldCount1 : Longint;
                              aPartialLen1 : Longint;
                              aKeyData1    : PffByteArray;
                              aKeyIncl1    : Boolean;
                              aFieldCount2 : Longint;
                              aPartialLen2 : Longint;
                              aKeyData2    : PffByteArray;
                              aKeyIncl2    : Boolean) : TffResult; override;
      function CursorSetTimeout(const aCursorID : TffCursorID;
                               const aTimeout  : Longint) : TffResult; override;
      function CursorSetToBegin(aCursorID : TffCursorID) : TffResult; override;
      function CursorSetToBookmark(aCursorID : TffCursorID;
                                   aBookmark : PffByteArray
                                   ) : TffResult; override;
      function CursorSetToCursor(aDestCursorID : TffCursorID;
                                 aSrcCursorID : TffCursorID
                                 ) : TffResult; override;
      function CursorSetToEnd(aCursorID : TffCursorID) : TffResult; override;
      function CursorSetToKey(aCursorID     : TffCursorID;
                              aSearchAction : TffSearchKeyAction;
                              aDirectKey    : Boolean;
                              aFieldCount   : Longint;
                              aPartialLen   : Longint;
                              aKeyData      : PffByteArray
                              ) : TffResult; override;
      function CursorSwitchToIndex(aCursorID  : TffCursorID;
                                   aIndexName : TffDictItemName;
                                   aIndexID   : Longint;
                                   aPosnOnRec : Boolean) : TffResult; override;
      function CursorSetFilter(aCursorID   : TffCursorID;
                               aExpression : pCANExpr;
                               aTimeout    : TffWord32) : TffResult; override;

      {record stuff}
      function RecordDelete(aCursorID : TffCursorID;
                            aData     : PffByteArray) : TffResult; override;
      function RecordDeleteBatch(aCursorID : TffCursorID;
                                 aBMCount  : Longint;
                                 aBMLen    : Longint;
                                 aData     : PffByteArray;
                                 aErrors   : PffLongintArray) : TffResult; override;
      function RecordExtractKey(aCursorID : TffCursorID;
                                aData     : PffByteArray;
                                aKey      : PffByteArray) : TffResult; override;
      function RecordGet(aCursorID : TffCursorID;
                         aLockType : TffLockType;
                         aData     : PffByteArray) : TffResult; override;
      function RecordGetBatch(aCursorID : TffCursorID;
                              aRecCount : longint;
                              aRecLen   : longint;
                          var aRecRead  : longint;
                              aData     : PffByteArray;
                          var aError    : TffResult) : TffResult; override;
      function RecordGetForKey(aCursorID   : TffCursorID;
                               aDirectKey  : Boolean;
                               aFieldCount : Longint;
                               aPartialLen : Longint;
                               aKeyData    : PffByteArray;
                               aData       : PffByteArray;
                               aFirstCall  : Boolean
                               ) : TffResult; override;
      function RecordGetNext(aCursorID : TffCursorID;
                             aLockType : TffLockType;
                             aData     : PffByteArray) : TffResult; override;
      function RecordGetPrior(aCursorID : TffCursorID;
                              aLockType : TffLockType;
                              aData : PffByteArray) : TffResult; override;
      function RecordInsert(aCursorID : TffCursorID;
                            aLockType : TffLockType;
                            aData     : PffByteArray) : TffResult; override;
      function RecordInsertBatch(aCursorID : TffCursorID;
                                 aRecCount : longint;
                                 aRecLen   : longint;
                                 aData     : PffByteArray;
                                 aErrors   : PffLongintArray) : TffResult; override;
      function RecordIsLocked(aCursorID : TffCursorID;
                              aLockType : TffLockType;
                          var aIsLocked : boolean) : TffResult; override;
      function RecordModify(aCursorID : TffCursorID;
                            aData     : PffByteArray;
                            aRelLock  : Boolean) : TffResult; override;
      function RecordRelLock(aCursorID : TffCursorID;
                             aAllLocks : Boolean) : TffResult; override;

      {BLOB stuff}
      function BLOBCreate(aCursorID : TffCursorID;
                      var aBlobNr   : TffInt64) : TffResult; override;
      function BLOBDelete(aCursorID : TffCursorID;
                          aBlobNr   : TffInt64) : TffResult; override;
{Begin !!.03}
      function BLOBListSegments(aCursorID : TffCursorID;
                                aBLOBNr : TffInt64;
                                aStream : TStream) : TffResult; override;
{End !!.03}
      function BLOBRead(aCursorID  : TffCursorID;
                        aBlobNr    : TffInt64;
                        aOffset    : TffWord32;                        {!!.06}
                        aLen       : TffWord32;                        {!!.06}
                    var aBLOB;
                    var aBytesRead : TffWord32)                        {!!.06}
                                   : TffResult; override;
      function BLOBFree(aCursorID : TffCursorID; aBlobNr : TffInt64;
                        readOnly  : Boolean) : TffResult; override;
      function BLOBGetLength(aCursorID : TffCursorID; aBlobNr : TffInt64;
                         var aLength   : longint) : TffResult; override;
      function BLOBTruncate(aCursorID    : TffCursorID; aBlobNr : TffInt64;
                             aBLOBLength : longint) : TffResult; override;
      function BLOBWrite(aCursorID : TffCursorID; aBlobNr : TffInt64;
                          aOffset  : longint;
                          aLen     : longint;
                      var aBLOB) : TffResult; override;
      function FileBLOBAdd(aCursorID : TffCursorID;
                     const aFileName : TffFullFileName;
                       var aBlobNr   : TffInt64) : TffResult; override;

      {query stuff}
      function SQLAlloc(aClientID   : TffClientID;
                        aDatabaseID : TffDatabaseID;
                        aTimeout    : longInt;
                    var aStmtID     : TffSqlStmtID) : TffResult; override;
      function SQLExec(aStmtID   : TffSqlStmtID;
                       aOpenMode : TffOpenMode;
                   var aCursorID : TffCursorID;
                       aStream   : TStream) : TffResult; override;
      function SQLExecDirect(aClientID   : TffClientID;
                             aDatabaseID : TffDatabaseID;
                             aQueryText  : PChar;
                             aTimeout    : longInt;
                             aOpenMode   : TffOpenMode;
                         var aCursorID   : TffCursorID;
                             aStream     : TStream) : TffResult; override;
      function SQLFree(aStmtID : TffSqlStmtID) : TffResult; override;
      function SQLPrepare(aStmtID    : TffSqlStmtID;
                          aQueryText : PChar;
                          aStream    : TStream) : TffResult; override;
      function SQLSetParams(aStmtID     : TffSqlStmtID;
                            aNumParams  : word;
                            aParamDescs : pointer;
                            aDataBuffer : PffByteArray;
                            aDataLen    : Longint;
                            aStream     : TStream) : TffResult; override;

      {misc stuff}
      function GetServerDateTime(var aDateTime : TDateTime
                                 ) : TffResult; override;
                                                               {begin !!.07}
      function GetServerSystemTime(var aSystemTime : TSystemTime) : TffResult; override;
      function GetServerGUID(var aGUID : TGUID) : TffResult; override;
      function GetServerID(var aUniqueID : TGUID) : TffResult; override;
      function GetServerStatistics(var Stats : TffServerStatistics) : TffResult; override;
      function GetCommandHandlerStatistics(const CmdHandlerIdx : Integer;
                                             var Stats : TffCommandHandlerStatistics) : TffResult; override;
      function GetTransportStatistics(const CmdHandlerIdx : Integer;
                                      const TransportIdx : Integer;
                                        var Stats : TffTransportStatistics) : TffResult; override;
                                                                 {end !!.07}


      {properties}
      property ClientList : TFFProxyClientList
         read rsClientList;

      property TimeOut : TFFWord32
         read rsTimeout write rsTimeout;

    published
      property Transport : TFFBaseTransport
         read rsTransport
         write rsSetTransport;

  end;

  {Callback method used by the transport to notify us when the request is
   complete.}
  procedure ProxyRequestCallback(aMsgID        : Longint;
                                 aErrorCode    : TffResult;
                                 aReply        : Pointer;
                                 aReplyLen     : Longint;
                                 aReplyCookie  : Longint);

var
  RemoteServerEngines : TFFThreadList;

implementation

uses
  ActiveX,
  ffsqlbas;

{--Internal helper routines--}
function ResultOK(aResult : TffResult) : Boolean;
begin
  Result := aResult = DBIERR_NONE;
end;
{------------------------------------------------------------------------------}


{--Callback routine--}
procedure ProxyRequestCallback(aMsgID        : Longint;
                               aErrorCode    : TffResult;
                               aReply        : Pointer;
                               aReplyLen     : Longint;
                               aReplyCookie  : Longint);
var
  Client : TFFProxyClient absolute aReplyCookie;
begin
  { hand-off the response from the transport to the ProxyClient }
  Client.pcMsgQueue.Append(aMsgID,
                           aReplyCookie,
                           0, {RequestID}
                           0, {Timeout}
                           aErrorCode,
                           aReply,
                           aReplyLen,
                           aReplyLen);
end;
{------------------------------------------------------------------------------}



{-TffProxyClient---------------------------------------------------------------}
constructor TFFProxyClient.Create(aTransport     : TffBaseTransport;
                                  aUserName      : TFFName;
                                  aPasswordHash  : Longint;
                                  aTimeOut       : Longint);
begin
  inherited Create;

  {Initialize internals}
  pcSrClientID := 0;
  pcCurrentSession := nil;
  pcForceClosed := False;

  pcTransport := aTransport;
  pcTimeout   := aTimeOut;

  {Create internal classes}
  pcMsgQueue  := TffDataMessageQueue.Create;
  pcSessions  := TFFProxySessionList.Create;
  pcDatabases := TFFProxyDatabaseList.Create;

  {Set the CallbackMethod that will be used by the transport to return data}
  pcCallbackMethod := ProxyRequestCallback;

  {Let the ServerEngine know that we are here. Set our SrClientID for later
   reference, as we will need it often.}
  Check(pcTransport.EstablishConnection(aUserName,
                                        aPasswordHash,
                                        pcTimeOut,
                                        pcSrClientID));
end;
{----------}
function TFFProxyClient.DatabaseAddAlias(const aAlias      : TffName;
                                         const aPath       : TffPath;
                                               aCheckSpace : Boolean)  {!!.11}
                                                           : TffResult;
var
  Request  : TffnmDatabaseAddAliasReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize the request record }
  Request.Alias := aAlias;
  Request.Path  := aPath;
  Request.CheckDisk := aCheckSpace;                                    {!!.11}

  Reply := nil;
  Result := ProcessRequest(ffnmDatabaseAddAlias,
                           Timeout,
                           @Request,
                           SizeOf(Request),
                           nmdByteArray,
                           Reply,
                           ReplyLen,
                           nmdByteArray);

  { Calling ffnmDatabaseAddAlias only returns an error code to Result. }
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyClient.DatabaseAliasList(aList: TList) : TffResult;
var
  Stream   : TMemoryStream;
  ReplyLen : Longint;
  Count    : Longint;
  AliasDes : PffAliasDescriptor;
  DesSize  : Longint;
begin
  Stream := TMemoryStream.Create;
  try
    { We have no data to send. }
    Result := ProcessRequest(ffnmDatabaseAliasList,
                             Timeout,
                             nil,
                             0,
                             nmdByteArray,
                             Pointer(Stream),
                             ReplyLen,
                             nmdStream);

    if ResultOK(Result) then begin
      aList.Clear;
      Stream.Position := 0;
      DesSize := SizeOf(TffAliasDescriptor);

      for Count := 1 to (ReplyLen div DesSize) do begin
        { Move the alias data from the stream, to a PffAliasDescriptor. Each
          descriptor will be an entry in aList. The caller must free this
          data when it is done using it. }
        FFGetMem(AliasDes, DesSize);
        Stream.Read(AliasDes^, DesSize);
        aList.Add(AliasDes);
      end;
    end;
  finally
    Stream.Free;
  end;
end;
{----------}
function TFFProxyClient.DatabaseChgAliasPath(const aAlias      : TffName;
                                             const aNewPath    : TffPath;
                                                   aCheckSpace : Boolean) {!!.11}
                                                               : TffResult;
var
  Request  : TffnmDatabaseChgAliasPathReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize the request record }
  Request.Alias := aAlias;
  Request.NewPath := aNewPath;
  Request.CheckDisk := aCheckSpace;                                    {!!.11}

  Reply := nil;
  Result := ProcessRequest(ffnmDatabaseChgAliasPath,
                           Timeout,
                           @Request,
                           SizeOf(Request),
                           nmdByteArray,
                           Reply,
                           ReplyLen,
                           nmdByteArray);

  { Calling ffnmDatabaseChgAliasPath only returns an error code to Result. }
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyClient.DatabaseClose(aDatabase : TffProxyDatabase) : TffResult;
begin
  Result := DBIERR_NONE;
  with pcDatabases.BeginWrite do
    try
      Delete(aDatabase);                                               {!!.01}
    finally
      EndWrite;
    end;
  aDatabase.Free;
  aDatabase := nil;
end;
{----------}
function TFFProxyClient.DatabaseDeleteAlias(const aAlias : TffName) : TffResult;
var
  Request  : TffnmDatabaseDeleteAliasReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize the request record }
  Request.Alias := aAlias;

  Reply := nil;
  Result := ProcessRequest(ffnmDatabaseDeleteAlias,
                           Timeout,
                           @Request,
                           SizeOf(Request),
                           nmdByteArray,
                           Reply,
                           ReplyLen,
                           nmdByteArray);

  { Calling ffnmDatabaseDeleteAlias only returns an error code to Result. }
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyClient.DatabaseGetAliasPath(const aAlias    : TffName;
                                               var aPath     : TffPath
                                            ) : TffResult;
var
  Request : TffnmDatabaseGetAliasPathReq;
  Reply   : PffnmDatabaseGetAliasPathRpy;
  ReplyLen : Longint;
begin
  { Initialize the request record }
  Request.Alias := aAlias;

  Reply := nil;
  Result := ProcessRequest(ffnmDatabaseGetAliasPath,
                           Timeout,
                           @Request,
                           SizeOf(Request),
                           nmdByteArray,
                           Pointer(Reply),
                           ReplyLen,
                           nmdByteArray);
  if ResultOK(Result) then
    aPath := Reply^.Path;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TffProxyClient.DatabaseModifyAlias(const aAlias      : TffName;
                                            const aNewName    : TffName;
                                            const aNewPath    : TffPath;
                                                  aCheckSpace : Boolean) {!!.11}
                                                              : TffResult;
var
  Request  : TffnmDatabaseModifyAliasReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize the request record }
  Request.ClientID := SrClientID;
  Request.Alias := aAlias;
  Request.NewName := aNewName;
  Request.NewPath := aNewPath;
  Request.CheckDisk := aCheckSpace;                                    {!!.11}

  Reply := nil;
  Result := ProcessRequest(ffnmDatabaseModifyAlias,
                           Timeout,
                           @Request,
                           SizeOf(Request),
                           nmdByteArray,
                           Reply,
                           ReplyLen,
                           nmdByteArray);

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyClient.DatabaseOpen(const aAlias      : TffName;
                                           aOpenMode   : TffOpenMode;
                                           aShareMode  : TffShareMode;
                                           aTimeout    : Longint;
                                       var aDatabaseID : TffDatabaseID)
                                                       : TffResult;
var
  Database : TFFProxyDatabase;
  ListItem : TffIntListItem;
begin
  Database := nil;
  Result := DBIERR_NONE;

  try
    Database := TFFProxyDatabase.Create(Self,
                                        aAlias,
                                        aOpenMode,
                                        aShareMode,
                                        aTimeout,
                                        True);
  except
    on E:Exception do
      if (E is EffException) or (E is EffDatabaseError) then
        Result := EffException(E).ErrorCode;
  end;

  if ResultOK(Result) and Assigned(Database) then begin
    {Add Database to the internal list}
    ListItem := TffIntListItem.Create(Longint(Database));
    with pcDatabases.BeginWrite do
      try
        Insert(ListItem);
      finally
        EndWrite;
      end;

    aDatabaseID := Longint(Database);
  end;
end;
{----------}
function TFFProxyClient.DatabaseOpenNoAlias(const aPath       : TffPath;
                                                  aOpenMode   : TffOpenMode;
                                                  aShareMode  : TffShareMode;
                                                  aTimeout    : Longint;
                                              var aDatabaseID : TffDatabaseID
                                           ) : TffResult;
var
  Database : TFFProxyDatabase;
  ListItem : TffIntListItem;
begin
  Database := nil;
  Result := DBIERR_NONE;

  try
    Database := TFFProxyDatabase.Create(Self,
                                        aPath,
                                        aOpenMode,
                                        aShareMode,
                                        aTimeout,
                                        False);
  except
    on E:Exception do
      if (E is EffException) or (E is EffDatabaseError) then
        Result := EffException(E).ErrorCode;
  end;

  if ResultOK(Result) and Assigned(Database) then begin
    {Add Database to the internal list}
    ListItem := TffIntListItem.Create(Longint(Database));
    with pcDatabases.BeginWrite do
      try
        Insert(ListItem);
      finally
        EndWrite;
      end;

    aDatabaseID := Longint(Database);
  end;
end;
{----------}
destructor TFFProxyClient.Destroy;
{Begin !!.03}
//var
//  Idx : Longint;
begin
  {Destroy managed objects}
  pcMsgQueue.Free;
  pcMsgQueue := nil;
  pcSessions.Free;
  pcSessions := nil;
  pcDatabases.Free;
  pcDatabases := nil;
//  with pcDatabases.BeginWrite do
//    try
//      for Idx := 0 to Pred(Count) do
//        TFFProxyDatabase(Items[Idx]).Free;
//    finally
//      EndWrite;
//    end;

//  with pcSessions.BeginWrite do
//    try
//      for Idx := 0 to Pred(Count) do
//        TFFProxySession(Items[Idx]).Free;
//    finally
//      EndWrite;
//    end;

  {Tell the server that we are disconnecting.}
  if not ForceClosed then
    if SrClientID > 0 then
      pcTransport.TerminateConnection(SrClientID, Timeout);

//  {Destroy internal classes}
//  pcMsgQueue.Free;
//  pcMsgQueue := nil;
//  pcSessions.Free;
//  pcSessions := nil;
//  pcDatabases.Free;
//  pcDatabases := nil;
{End !!.03}

  {Re-Initialize internals for completeness}
  pcCurrentSession := nil;
  pcTransport      := nil;
  pcCallbackMethod := nil;

  inherited Destroy;
end;
{----------}
function TffProxyClient.IsReadOnly : Boolean;
var
  Reply     : PffnmServerIsReadOnlyRpy;
  ReplyLen  : Longint;
  ErrorCode : TffResult;
begin
  Reply := nil;
  ErrorCode := ProcessRequest(ffnmServerIsReadOnly,
                              Timeout,
                              nil,
                              0,
                              nmdByteArray,
                              Pointer(Reply),
                              ReplyLen,
                              nmdByteArray);
  if ResultOK(ErrorCode) then
    Result := Reply^.IsReadOnly
  else
    Result := False;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyClient.GetServerDateTime(var aDateTime : TDateTime
                                         ) : TffResult;
var
  Reply    : PffnmGetServerDateTimeRpy;
  ReplyLen : Longint;
begin
  { Just in case }
  aDateTime := Now;

  { We have no data to send }
  Reply := nil;
  Result := ProcessRequest(ffnmGetServerDateTime,
                           Timeout,
                           nil,
                           0,
                           nmdByteArray,
                           Pointer(Reply),
                           ReplyLen,
                           nmdByteArray);
  if ResultOK(Result) then
    aDateTime := Reply^.ServerNow;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}                                                   {begin !!.07}
function TFFProxyClient.GetServerSystemTime(var aSystemTime : TSystemTime) : TffResult;
var
  Reply    : PffnmGetServerSystemTimeRpy;
  ReplyLen : Longint;
begin
  { Just in case }
  GetSystemTime(aSystemTime);

  { We have no data to send }
  Reply := nil;
  Result := ProcessRequest(ffnmGetServerSystemTime,
                           Timeout,
                           nil,
                           0,
                           nmdByteArray,
                           Pointer(Reply),
                           ReplyLen,
                           nmdByteArray);
  if ResultOK(Result) then
    aSystemTime := Reply^.ServerNow;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyClient.GetServerGUID(var aGUID : TGUID) : TffResult;
var
  Reply    : PffnmGetServerGUIDRpy;
  ReplyLen : Longint;
begin
  { Just in case }
  CoCreateGuid(aGUID);

  { We have no data to send }
  Reply := nil;
  Result := ProcessRequest(ffnmGetServerGUID,
                           Timeout,
                           nil,
                           0,
                           nmdByteArray,
                           Pointer(Reply),
                           ReplyLen,
                           nmdByteArray);
  if ResultOK(Result) then
    aGUID := Reply^.GUID;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyClient.GetServerID(var aUniqueID : TGUID) : TffResult;
var
  Reply    : PffnmGetServerIDRpy;
  ReplyLen : Longint;
begin
  { We have no data to send }
  Reply := nil;
  Result := ProcessRequest(ffnmGetServerID,
                           Timeout,
                           nil,
                           0,
                           nmdByteArray,
                           Pointer(Reply),
                           ReplyLen,
                           nmdByteArray);
  if ResultOK(Result) then
    aUniqueID := Reply^.UniqueID;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyClient.GetServerStatistics(var Stats : TffServerStatistics) : TffResult;
var
  Reply    : PffnmServerStatisticsRpy;
  ReplyLen : Longint;
begin
  { We have no data to send }
  Reply := nil;
  Result := ProcessRequest(ffnmServerStatistics,
                           Timeout,
                           nil,
                           0,
                           nmdByteArray,
                           Pointer(Reply),
                           ReplyLen,
                           nmdByteArray);

  if ResultOK(Result) then
    Stats := Reply^.Stats;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyClient.GetCommandHandlerStatistics(const CmdHandlerIdx : Integer;
                                                      var Stats : TffCommandHandlerStatistics) : TffResult;
var
  Request  : TffnmCmdHandlerStatisticsReq;
  Reply    : PffnmCmdHandlerStatisticsRpy;
  ReplyLen : Longint;
begin
  { Initiailize Request }
  Request.CmdHandlerIdx := CmdHandlerIdx;

  Reply := nil;
  Result := ProcessRequest(ffnmCmdHandlerStatistics,
                           pcTimeout,
                           @Request,
                           SizeOf(Request),
                           nmdByteArray,
                           Pointer(Reply),
                           ReplyLen,
                           nmdByteArray);

  if ResultOK(Result) then
    Stats := Reply^.Stats;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyClient.GetTransportStatistics(const CmdHandlerIdx : Integer;
                                               const Transportidx : Integer;
                                                 var Stats : TffTransportStatistics) : TffResult;
var
  Request  : TffnmTransportStatisticsReq;
  Reply    : PffnmTransportStatisticsRpy;
  ReplyLen : Longint;
begin
  { Initiailize Request }
  Request.CmdHandlerIdx := CmdHandlerIdx;
  Request.TransportIdx := Transportidx;

  Reply := nil;
  Result := ProcessRequest(ffnmTransportStatistics,
                           pcTimeout,
                           @Request,
                           SizeOf(Request),
                           nmdByteArray,
                           Pointer(Reply),
                           ReplyLen,
                           nmdByteArray);

  if ResultOK(Result) then
    Stats := Reply^.Stats;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}                                                     {end !!.07}
function TFFProxyClient.ProcessRequest(aMsgID           : longInt;
                                       aTimeout         : longInt;
                                       aRequestData     : Pointer;
                                       aRequestDataLen  : longInt;
                                       aRequestDataType : TffNetMsgDataType;
                                   var aReply           : Pointer;
                                   var aReplyLen        : longInt;
                                       aReplyType       : TffNetMsgDataType
                                      ) : TffResult;
var
  ReplyAsStream : TStream absolute aReply;
  ReplyMsg      : PffDataMessage;
begin
  if ForceClosed then begin
    Result := DBIERR_NONE;
    aReply := nil;
    aReplyLen := 0;
    Exit;
  end;

  Result := DBIERR_NA;
  {A Respose from the server is expected. This call will not return until
   the complete reply has been sent to the transport, and the Client
   callback method has been called.}

      { Use the ProxessRequest method to submit a request that is routed to the
        transport.  This method does the following:

        1. Calls TffBaseTransport.Request with transportID = 0 and cookie
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
  while Assigned(ReplyMsg) and (ReplyMsg^.dmMsg <> aMsgID) do begin
    FFFreeMem(ReplyMsg^.dmData, ReplyMsg^.dmDataLen);
    FFFreeMem(ReplyMsg, SizeOf(TFFDataMessage));
    ReplyMsg := pcMsgQueue.SoftPop;
  end;

  if not Assigned(ReplyMsg) then begin

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
  end;

  if Assigned(ReplyMsg) then begin
    if (ReplyMsg^.dmMsg <> aMsgID) then begin
      Result := DBIERR_NOTSAMESESSION;
      FFFreeMem(ReplyMsg^.dmData, ReplyMsg^.dmDataLen);                {!!.03}
      FFFreeMem(ReplyMsg, SizeOf(TFFDataMessage));
      Exit;
    end;

    aReplyLen := ReplyMsg^.dmDataLen;
    if aReplyType = nmdStream then begin
      Assert(Assigned(ReplyAsStream));
      ReplyAsStream.Position := 0;
      if (aReplyLen > 0) then begin
        ReplyAsStream.Write(ReplyMsg^.dmData^, aReplyLen);
        FFFreeMem(ReplyMsg^.dmData, aReplyLen);
      end;
    end else
      aReply := ReplyMsg^.dmData;

    Result := ReplyMsg^.dmErrorCode;

    { Free the ReplyMsg, but leave RequestData alone.
      The caller is responsible for releasing data.
      We expect the caller to free the reply data.}
    FFFreeMem(ReplyMsg, SizeOf(TFFDataMessage));

  end;
end;
{----------}
function TFFProxyClient.ProcessRequestNoReply(aMsgID          : Longint;
                                              aTimeout        : Longint;
                                              aRequestData    : Pointer;
                                              aRequestDataLen : Longint
                                             ) : TffResult;
begin
  if ForceClosed then begin
    Result := DBIERR_NONE;
    Exit;
  end;

  {No response from the server is expected, so this call will return as
   soon as the request has been sent from the transport's queue}

  pcTransport.Post(0, {For use by future protocols.}
                   SrClientID,
                   aMsgID,
                   aRequestData,
                   aRequestDataLen,
                   aTimeout,
                   ffrmNoReplyWaitUntilSent);

  Result := DBIERR_NONE;
end;
{Begin !!.01}
{----------}
function TffProxyClient.RemoteRestart : TffResult;
begin
  Result := ProcessRequestNoReply(ffnmServerRestart, Timeout, nil, 0);
end;
{----------}
function TffProxyClient.RemoteStart : TffResult;
begin
  Result := ProcessRequestNoReply(ffnmServerStartup, Timeout, nil, 0);
end;
{----------}
function TffProxyClient.RemoteStop : TffResult;
begin
  Result := ProcessRequestNoReply(ffnmServerStop, Timeout, nil, 0);
end;
{End !!.01}
{----------}
function TFFProxyClient.SessionAdd(var aSessionID : TffSessionID;
                                 const aTimeout   : Longint) : TffResult;
var
  Session  : TFFProxySession;
  ListItem : TffIntListItem;
begin
  Session := nil;
  Result := DBIERR_NONE;

  try
    Session := TFFProxySession.Create(Self, aTimeout);
  except
    on E:Exception do
      if (E is EffException) or (E is EffDatabaseError) then
        Result := EffException(E).ErrorCode;
  end;

  if ResultOK(Result) and Assigned(Session) then begin
    {Add Session to the internal list}
    ListItem := TffIntListItem.Create(Longint(Session));
    with pcSessions.BeginWrite do
      try
        Insert(ListItem);
      finally
        EndWrite;
      end;

    aSessionID := Longint(Session);

    {Set the current session if it is nil}
    if not Assigned(pcCurrentSession) then
      pcCurrentSession := Session;
  end;
end;
{Begin !!.06}
{----------}
function TFFProxyClient.SessionCloseInactiveTables : TffResult;
var
  Request  : TffnmSessionCloseInactiveTblReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initiailize Request }
  Request.SessionID := pcCurrentSession.psSrSessionID;

  Reply := nil;
  Result := ProcessRequest(ffnmSessionCloseInactTbl,
                           pcTimeout,
                           @Request,
                           SizeOf(Request),
                           nmdByteArray,
                           Reply,
                           ReplyLen,
                           nmdByteArray);
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{End !!.06}
{----------}
function TFFProxyClient.SessionCount : Longint;
begin
  {Retun the number of sessions managed by the ProxyClient}
  with pcSessions.BeginRead do
    try
      Result := Count;
    finally
      EndRead;
    end;
end;
{----------}
function TFFProxyClient.SessionGetCurrent : TffProxySession;
begin
  {Return the current session. This value will be nil if no sessions exist}
  if Assigned(pcCurrentSession) then
    Result := pcCurrentSession
  else begin
    if SessionCount > 0 then
      {Return the first session in the list}
      with pcSessions.BeginRead do
        try
          Result := TFFProxySession(Items[0]);
        finally
          EndRead;
        end
    else
      {no sessions available}
      Result := nil;
  end;
end;
{----------}
function TFFProxyClient.SessionRemove(aSession : TFFProxySession) : TffResult;
begin
  {Remove session from the internal list, and destroy.}
  if not Assigned(aSession) then begin
    {aSession parameter is invalid}
    Result := DBIERR_INVALIDHNDL;
    Exit;
  end;

  Result := DBIERR_NONE;
  with pcSessions.BeginWrite do
    try
      Delete(aSession);                                                {!!.01}
    finally
      EndWrite;
    end;

  aSession.Free;
end;
{----------}
function TFFProxyClient.SessionSetCurrent(aSession : TFFProxySession
                                         ) : TffResult;
var
  Request : TffnmSessionSetCurrentReq;
  Reply   : PffnmSessionSetCurrentReq;
  ReplyLen : Longint;
begin
  {Set the Client's CurrentSession. This function will accept nil as a valid
   option}
  Request.SessionID := aSession.psSrSessionID;
  Reply := nil;
  Result := ProcessRequest(ffnmSessionSetCurrent,
                           pcTimeOut,
                           @Request,
                           SizeOf(Request),
                           nmdByteArray,
                           Pointer(Reply),
                           ReplyLen,
                           nmdByteArray);

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);

//  Result := DBIERR_NONE;
  pcCurrentSession := aSession;
end;
{----------}
function TffProxyClient.GetRebuildStatus(const aRebuildID : Longint;
                                           var aIsPresent : Boolean;
                                           var aStatus    : TffRebuildStatus) : TffResult;
var
  Request  : TffnmGetRebuildStatusReq;
  Reply    : PffnmGetRebuildStatusRpy;
  ReplyLen : Longint;
begin
  { Initiailize Request }
  Request.RebuildID := aRebuildID;

  Reply := nil;
  Result := ProcessRequest(ffnmGetRebuildStatus,
                           pcTimeout,
                           @Request,
                           SizeOf(Request),
                           nmdByteArray,
                           Pointer(Reply),
                           ReplyLen,
                           nmdByteArray);

  if ResultOK(Result) then begin
    aIsPresent := Reply^.IsPresent;
    aStatus := Reply^.Status;
  end;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyClient.SetTimeout(const aTimeout : Longint) : TffResult;
var
  Request  : TffnmClientSetTimeoutReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  Result := DBIERR_NONE;
  if pcTimeout = aTimeout then Exit;

  pcTimeout := aTimeout;
  { Initialize request }
  Request.Timeout := pcTimeout;

  Reply := nil;
  Result := ProcessRequest(ffnmClientSetTimeout,
                           pcTimeout,
                           @Request,
                           SizeOf(Request),
                           nmdByteArray,
                           Reply,
                           ReplyLen,
                           nmdByteArray);

  { Calling ffnmClientSetTimeout only returns an error code to Result. }
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{------------------------------------------------------------------------------}


{-TFFProxySession--------------------------------------------------------------}
constructor TFFProxySession.Create(aClient  : TFFProxyClient;
                                   aTimeout : Longint);
var
  Request  : TffnmSessionAddReq;
  Reply    : PffnmSessionAddRpy;
  ReplyLen : Longint;
  Result   : TFFResult;
begin
  inherited Create;

  {Initalize the object}
  psClient := aClient;
  psSrSessionID := 0;
  psTimeout := aTimeout;

  { Initiailize Request }
  Request.Timeout := aTimeout;

  {Create a session object, and add it to the list}
  Reply := nil;
  Result := psClient.ProcessRequest(ffnmSessionAdd,
                                    psTimeout,
                                    @Request,
                                    SizeOf(Request),
                                    nmdByteArray,
                                    Pointer(Reply),
                                    ReplyLen,
                                    nmdByteArray);

  {Make sure that result was valid before we continue}
  Check(Result);

  psSrSessionID := Reply^.SessionID;

  FFFreeMem(Reply, ReplyLen);
end;
{----------}
destructor TFFProxySession.Destroy;
var
  Request  : TffnmSessionCloseReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  if SrSessionID > 0 then begin
    { Initiailize Request }
    Request.SessionID := SrSessionID;

    Reply := nil;
    Client.ProcessRequest(ffnmSessionClose,
                          Timeout,
                          @Request,
                          SizeOf(Request),
                          nmdByteArray,
                          Reply,
                          ReplyLen,
                          nmdByteArray);

    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);
  end;

  psClient := nil;

  inherited Destroy;
end;
{----------}
function TFFProxySession.SetTimeout(aTimeout : Longint) : TffResult;
var
  Request  : TffnmSessionSetTimeoutReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  Result := DBIERR_NONE;
  if psTimeout = aTimeout then Exit;

  psTimeout := aTimeout;

  { Initiailize Request }
  Request.SessionID := psSrSessionID;
  Request.Timeout := psTimeout;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmSessionSetTimeout,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{------------------------------------------------------------------------------}



{-TFFProxyDatabase-------------------------------------------------------------}
constructor TFFProxyDatabase.Create(aClient    : TFFProxyClient;
                                    aLocation  : string;
                                    aOpenMode  : TffOpenMode;
                                    aShareMode : TffShareMode;
                                    aTimeout   : Longint;
                                    aIsAlias   : Boolean);
var
  RequestAlias : TffnmDatabaseOpenReq;
  RequestPath  : TffnmDatabaseOpenNoAliasReq;
  ReplyAlias   : PffnmDatabaseOpenRpy;
  ReplyPath    : PffnmDatabaseOpenNoAliasRpy;
  ReplyLen     : Longint;
  Result       : TffResult;
begin
  inherited Create;

  pdInTrans := False;
  pdSrDatabaseID := 0;
  pdClient := aClient;
  pdTimeout := aTimeout;

  pdStmts := TffProxySQLStmtList.Create;
  pdTables := TFFProxyCursorList.Create;

  if aIsAlias then begin
    { Initiailize Request }
    RequestAlias.Alias := aLocation;
    RequestAlias.OpenMode := aOpenMode;
    RequestAlias.ShareMode := aShareMode;
    RequestAlias.Timeout := aTimeout;

    ReplyAlias := nil;
    Result := Client.ProcessRequest(ffnmDatabaseOpen,
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
  end else begin
    { Initiailize Request }
    RequestPath.Path := aLocation;
    RequestPath.OpenMode := aOpenMode;
    RequestPath.ShareMode := aShareMode;
    RequestPath.Timeout := aTimeout;

    ReplyPath := nil;
    Result := Client.ProcessRequest(ffnmDatabaseOpenNoAlias,
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
  end;
end;
{----------}
destructor TFFProxyDatabase.Destroy;
var
//  Idx      : Longint;                                                {!!.03}
  Request  : TffnmDatabaseCloseReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  {Destroy dependent objects}
  if InTrans then
    TransactionRollback;

{Begin !!.03}
//  with pdTables.BeginWrite do
//    try
//      for Idx := 0 to Pred(Count) do
//        TFFProxyCursor(Items[Idx]).Free;
//    finally
//      EndWrite;
//    end;

  pdTables.Free;
  pdTables := nil;

//  with pdStmts.BeginWrite do
//    try
//      for Idx := 0 to Pred(Count) do
//        TffProxySQLStmt(Items[Idx]).Free;
//    finally
//      EndWrite;
//    end;
{End !!.03}

  pdStmts.Free;
  pdStmts := nil;

  {Let the server know that we are leaving}
  if SrDatabaseID > 0 then begin
    { Initiailize Request }
    Request.DatabaseID := SrDatabaseID;

    Reply := nil;
    Client.ProcessRequest(ffnmDatabaseClose,
                          Timeout,
                          @Request,
                          SizeOf(Request),
                          nmdByteArray,
                          Reply,
                          ReplyLen,
                          nmdByteArray);
    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);
  end;
  {Reset internals}
  pdSrDatabaseID := 0;
  pdClient := nil;

  inherited;
end;
{----------}
function TffProxyDatabase.GetDbFreeSpace(var aFreeSpace : Longint) : TffResult;
var
  Request  : TffnmDatabaseGetFreeSpaceReq;
  Reply    : PffnmDatabaseGetFreeSpaceRpy;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.DatabaseID := pdSrDatabaseID;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmDatabaseGetFreeSpace,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);
  if ResultOK(Result) then
    aFreeSpace := Reply^.FreeSpace;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TffProxyDatabase.QueryOpen(aCursorID : TffCursorID;
                                    aOpenMode : TffOpenMode;
                                    aShareMode : TffShareMode;
                                    aTimeout  : longInt;
                                    aStream   : TStream;
                                var aFinalCursorID : TffCursorID) : TffResult;
var
  Cursor   : TFFProxyCursor;
  ListItem : TffIntListItem;
begin
  Cursor := nil;
  Result := DBIERR_NONE;

  try
    Cursor := TFFProxyCursor.CreateSQL(Self, aCursorID, aOpenMode, aShareMode,
                                       aTimeout, aStream);
  except
    on E:Exception do
      if (E is EffException) or (E is EffDatabaseError) then
        Result := EffException(E).ErrorCode;
  end;

  if ResultOK(Result) and Assigned(Cursor) then begin
    ListItem := TffIntListItem.Create(Longint(Cursor));
    ListItem.MaintainLinks := False;                                   {!!.02}
    with pdTables.BeginWrite do
      try
        Insert(ListItem);
      finally
        EndWrite;
      end;

    aFinalCursorID := Longint(Cursor);
  end;
end;
{----------}
function TFFProxyDatabase.SetTimeout(const aTimeout : Longint) : TffResult;
var
  Request  : TffnmDatabaseSetTimeoutReq;
  Reply    : pointer;
  ReplyLen : Longint;
begin
  Result := DBIERR_NONE;
  if pdTimeout = aTimeout then Exit;

  pdTimeout := aTimeout;

  { Initialize Request }
  Request.DatabaseID := pdSrDatabaseID;
  Request.Timeout := aTimeout;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmDatabaseSetTimeout,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TffProxyDatabase.SQLAlloc(const aTimeout : longInt;
                                     var aStmtID : TffSqlStmtID) : TffResult;
var
  ListItem : TffIntListItem;
  Statement : TffProxySQLStmt;
begin
  Statement := nil;
  Result := DBIERR_NONE;

  try
    Statement := TffProxySQLStmt.Create(Self, aTimeout);
  except
    on E:Exception do
      if (E is EffException) or (E is EffDatabaseError) then
        Result := EffException(E).ErrorCode;
  end;

  if ResultOK(Result) and Assigned(Statement) then begin
    ListItem := TffIntListItem.Create(Longint(Statement));
    with pdStmts.BeginWrite do
      try
        Insert(ListItem);
      finally
        EndWrite;
      end;

    aStmtID := Longint(Statement);
  end;

end;
{----------}
function TffProxyDatabase.SQLExecDirect(aQueryText  : PChar;
                                        aOpenMode   : TffOpenMode;
                                        aTimeout    : longInt;
                                    var aCursorID   : TffCursorID;
                                        aStream     : TStream) : TffResult;
var
  QueryLen : Longint;
  ReplyLen : Longint;
  Request : PffnmSQLExecDirectReq;
  ReqLen  : Longint;
  SvrCursorID : TffCursorID;
begin
  Assert(Assigned(aStream));
  QueryLen := StrLen(aQueryText);
  ReqLen := SizeOf(TffnmSQLExecDirectReq) - sizeOf(TffVarMsgField) +   {!!.05}
            QueryLen + 1;                                              {!!.05}
  FFGetZeroMem(Request, ReqLen);
  try
    { Prepare the request. }
    Move(aQueryText^, Request^.Query, QueryLen);
    Request^.DatabaseID := pdSrDatabaseID;
    Request^.Timeout    := aTimeout;
    Request^.OpenMode   := aOpenMode;

    Result := pdClient.ProcessRequest(ffnmSQLExecDirect,
                                      pdTimeout,
                                      Request,
                                      ReqLen,
                                      nmdByteArray,
                                      Pointer(aStream),
                                      ReplyLen,
                                      nmdStream);

    { Was the execution successful? }
    if Result = DBIERR_NONE then begin
      { Yes. Get the cursorID from the stream & open a proxy cursor. }
      aStream.Position := 0;
      aStream.Read(SvrCursorID, sizeOf(SvrCursorID));
      if SvrCursorID <> 0 then                                           {!!.11}
        Result := QueryOpen(SvrCursorID, aOpenMode, smShared, aTimeout,
                            aStream, aCursorID);
    end;

    { Assumption: Upper levels are responsible for Stream contents. }

  finally
    FFFreeMem(Request, ReqLen);
  end;

end;
{----------}
function TFFProxyDatabase.TableAddIndex(const aCursorID  : TffCursorID;
                                        const aTableName : TffTableName;
                                        const aIndexDesc : TffIndexDescriptor
                                       ) : TffResult;
var
  Request  : TffnmAddIndexReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  if aCursorID > 0 then
    Request.CursorID := TFFProxyCursor(aCursorID).SrCursorID
  else
    Request.CursorID := 0; 
  Request.TableName := aTableName;
  Request.IndexDesc := aIndexDesc;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmAddIndex,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyDatabase.TableBuild(aOverWrite  : Boolean;
                               const aTableName  : TffTableName;
                                     aForServer  : Boolean;
                                     aDictionary : TffDataDictionary
                                    ) : TffResult;
var
  Request  : TMemoryStream;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request := TMemoryStream.Create;
  try
    Request.Write(pdSrDatabaseID, SizeOf(pdSRDatabaseID));             {!!.10}
    Request.Write(aOverWrite, SizeOf(aOverWrite));
    Request.Write(aTableName, SizeOf(aTableName));
    aDictionary.WriteToStream(Request);

    Reply := nil;
    Result := Client.ProcessRequest(ffnmBuildTable,
                                    Timeout,
                                    Request.Memory,
                                    Request.Size,
                                    nmdStream,
                                    Reply,
                                    ReplyLen,
                                    nmdByteArray);
    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);
  finally
    Request.Free;
  end;
end;
{----------}
function TFFProxyDatabase.TableClose(aCursor : TFFProxyCursor) : TffResult;
begin
  Result := DBIERR_NONE;

  with pdTables.BeginWrite do
    try
      Delete(aCursor);                                                 {!!.01}
    finally
      EndWrite;
    end;

  aCursor.Free;
  aCursor := nil;
end;
{----------}
function TFFProxyDatabase.TableDelete(const aTableName : TffTableName
                                     ) : TffResult;
var
  Request  : TffnmDeleteTableReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  Request.TableName := aTableName;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmDeleteTable,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TffProxyDatabase.TableDropIndex(aCursorID   : TffCursorID;
                                   const aTableName  : TffTableName;
                                   const aIndexName  : TffDictItemName;
                                         aIndexID    : longint) : TffResult;
var
  Request  : TffnmDropIndexReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  if aCursorID > 0 then
    Request.CursorID := TFFProxyCursor(aCursorID).SrCursorID
  else
    Request.CursorID := aCursorID;
  Request.TableName := aTableName;
  Request.IndexName := aIndexName;
  Request.IndexNumber := aIndexID;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmDropIndex,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TffProxyDatabase.TableEmpty(aCursorID  : TffCursorID;
                               const aTableName : TffTableName) : TffResult;
var
  Request  : TffnmEmptyTableReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  if aCursorID > 0 then
    Request.CursorID := TFFProxyCursor(aCursorID).SrCursorID
  else
    Request.CursorID := aCursorID;
  Request.TableName := aTableName;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmEmptyTable,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyDatabase.TableGetDictionary(const aTableName : TffTableName;
                                                   aForServer : Boolean;
                                                   aStream    : TStream
                                            ) : TffResult;
var
  Request  : TffnmGetTableDictionaryReq;
  ReplyLen : Longint;
begin
  Assert(Assigned(aStream));
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  Request.TableName := FFExtractFileName(aTableName);

  aStream.Position := 0;
  Result := Client.ProcessRequest(ffnmGetTableDictionary,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(aStream),
                                  ReplyLen,
                                  nmdStream);
end;
{----------}
function TffProxyDatabase.TableExists(const aTableName  : TffTableName;
                                        var aExists     : Boolean) : TffResult;
var
  Request  : TffnmDatabaseTableExistsReq;
  Reply    : PffnmDatabaseTableExistsRpy;
  ReplyLen : Longint;
begin
  Request.DatabaseID := SrDatabaseID;
  Request.TableName := aTableName;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmDatabaseTableExists,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);
  if ResultOK(Result) then
    aExists := Reply^.Exists;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyDatabase.TableList(const aMask : TffFileNameExt;
                                          aList : TList) : TffResult;
var
  Request    : TffnmDatabaseTableListReq;
  ReplyLen   : Longint;
  Stream     : TStream;
  TableDescr : PffTableDescriptor;
  Count      : Longint;
begin
  Stream := TMemoryStream.Create;
  try
    { Initialize Request }
    Request.DatabaseID := SrDatabaseID;
    Request.Mask := aMask;

    Result := Client.ProcessRequest(ffnmDatabaseTableList,
                                    Timeout,
                                    @Request,
                                    SizeOf(Request),
                                    nmdByteArray,
                                    Pointer(Stream),
                                    ReplyLen,
                                    nmdStream);

    if ResultOK(Result) then begin
      {Build the list}
      Stream.Position := 0;
      aList.Clear;

      for Count := 1 to (Stream.Size div SizeOf(TffTableDescriptor)) do begin
        FFGetMem(TableDescr, SizeOf(TFFTableDescriptor));
        Stream.Read(TableDescr^, SizeOf(TffTableDescriptor));
        aList.Add(TableDescr);
      end;
    end;
  finally
    Stream.Free;
  end;
end;
function TffProxyDatabase.TableLockedExclusive(const aTableName : TffTableName;
                                                 var aLocked    : Boolean
                                              ) : TffResult;
var
  Request  : TffnmDatabaseTableLockedExclusiveReq;
  Reply    : PffnmDatabaseTableLockedExclusiveRpy;
  ReplyLen : Longint;
begin
  Request.DatabaseID := SrDatabaseID;
  Request.TableName := aTableName;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmDatabaseTableLockedExclusive,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);
  if ResultOK(Result) then
    aLocked := Reply^.Locked;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyDatabase.TableOpen(const aTableName : TffTableName;
                                          aForServer : Boolean;
                                          aIndexName : TffName;
                                          aIndexID   : Longint;
                                          aOpenMode  : TffOpenMode;
                                          aShareMode : TffShareMode;
                                          aTimeout   : Longint;
                                      var aCursorID  : TffCursorID;
                                          aStream    : TStream) : TffResult;
var
  Cursor   : TFFProxyCursor;
  ListItem : TffIntListItem;
begin
  Assert(Assigned(aStream));
  Cursor := nil;
  Result := DBIERR_NONE;

  try
    Cursor := TFFProxyCursor.Create(Self,
                                    0,
                                    aTableName,
                                    aForServer,
                                    aIndexName,
                                    aIndexID,
                                    aOpenMode,
                                    aShareMode,
                                    aTimeout,
                                    aStream);
  except
    on E:Exception do
      if (E is EffException) or (E is EffDatabaseError) then
        Result := EffException(E).ErrorCode;
  end;

  if ResultOK(Result) and Assigned(Cursor) then begin
    ListItem := TffIntListItem.Create(Longint(Cursor));
    ListItem.MaintainLinks := False;                                   {!!.02}
    with pdTables.BeginWrite do
      try
        Insert(ListItem);
      finally
        EndWrite;
      end;

    aCursorID := Longint(Cursor);
  end;
end;
{----------}
function TFFProxyDatabase.TablePack(const aTableName : TffTableName;
                                      var aRebuildID : Longint) : TffResult;
var
  Request  : TffnmPackTableReq;
  Reply    : PffnmPackTableRpy;
  ReplyLen : Longint;
begin
  aRebuildID := -1;
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  Request.TableName  := aTableName;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmPackTable,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);

  if ResultOK(Result) then
    aRebuildID := Reply^.RebuildID;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyDatabase.TableRebuildIndex(const aTableName : TffTableName;
                                            const aIndexName : TffName;
                                                  aIndexID   : Longint;
                                              var aRebuildID : Longint
                                           ) : TffResult;
var
  Request  : TffnmReindexTableReq;
  Reply    : PffnmReindexTableRpy;
  ReplyLen : Longint;
begin
  aRebuildID := -1;
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  Request.TableName  := aTableName;
  Request.IndexName  := aIndexName;
  Request.IndexNumber := aIndexID;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmReindexTable,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);
  if ResultOK(Result) then
    aRebuildID := Reply^.RebuildID;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyDatabase.TableRename(const aOldName : TffName;
                                      const aNewName : TffName) : TffResult;
var
  Request  : TffnmRenameTableReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.DatabaseID   := SrDatabaseID;
  Request.OldTableName := aOldName;
  Request.NewTableName := aNewName;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmRenameTable,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyDatabase.TableRestructure(
                                     const aTableName  : TffTableName;
                                           aDictionary : TffDataDictionary;
                                           aFieldMap   : TffStringList;
                                       var aRebuildID  : Longint
                                           ) : TffResult;
var
  I             : Longint;
  NullByte      : Byte;
  Request       : TMemoryStream;
  Reply         : PffnmRestructureTableRpy;
  FieldMapEntry : TffShStr;
  ReplyLen      : Longint;
begin
  NullByte := 0;
  aRebuildID := -1;

  { Initialize Request }
  Request := TMemoryStream.Create;
  try
    Request.Write(SrDatabaseID, SizeOf(LongInt));
    Request.Write(aTableName, SizeOf(aTableName));
    aDictionary.WriteToStream(Request);
    if Assigned(aFieldMap) then
      for I := 0 to aFieldMap.Count - 1 do begin
        FieldMapEntry := aFieldMap[I];
        Request.Write(FieldMapEntry, Length(FieldMapEntry) + 1);
      end;
    Request.Write(NullByte, SizeOf(NullByte));

    Reply := nil;
    Result := Client.ProcessRequest(ffnmRestructureTable,
                                    Timeout,
                                    Request.Memory,
                                    Request.Size,
                                    nmdByteArray,
                                    Pointer(Reply),
                                    ReplyLen,
                                    nmdByteArray);

    if ResultOK(Result) then
      aRebuildID := Reply^.RebuildID;

    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);

  finally
    Request.Free;
  end;
end;
{----------}
function TFFProxyDatabase.TransactionCommit : TffResult;
var
  Request  : TffnmEndTransactionReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  Request.ToBeCommitted := True;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmEndTransaction,
                                  pdTimeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyDatabase.TransactionRollback : TffResult;
var
  Request  : TffnmEndTransactionReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  Request.ToBeCommitted := False;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmEndTransaction,
                                  pdTimeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);
  FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyDatabase.TransactionStart(aFailSafe : Boolean) : TffResult;
var
  Request   : TffnmStartTransactionReq;
  Reply     : Pointer;
  ReplyLen  : Longint;
begin
  { Initialize Request }
  Request.DatabaseID := SrDatabaseID;
  Request.FailSafe   := aFailSafe;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmStartTransaction,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
  Check(Result);
end;
{Start !!.10}
{----------}
function TFFProxyDatabase.TransactionStartWith(const aFailSafe : Boolean;
                                               const aCursorIDs : TffPointerList
                                              ) : TffResult;
var
  Reply     : Pointer;
  Inx,
  aCount,
  ReplyLen  : Longint;
  Request   : TMemoryStream;
  Writer : TWriter;
begin
  { Initialize Request }
  Request := TMemoryStream.Create;
  Writer := TWriter.Create(Request, 4096);
  try
    Writer.WriteInteger(pdSrDatabaseID);
    Writer.WriteBoolean(aFailSafe);
    aCount := aCursorIDs.Count;
    Writer.WriteInteger(aCount);
    for Inx := 0 to Pred(aCount) do
      { Get the cursorID of the proxy cursor. }
      Writer.WriteInteger(TffProxyCursor(aCursorIDs[Inx]).SrCursorID);
    Writer.FlushBuffer;

    Reply := nil;
    Result := Client.ProcessRequest(ffnmStartTransactionWith,
                                    Timeout,
                                    Request.Memory,
                                    Request.Size,
                                    nmdStream,
                                    Reply,
                                    ReplyLen,
                                    nmdByteArray);
  finally
    Writer.Free;
    Request.Free;
    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);
  end;
//  Check(Result);                                                     {Deleted !!.11}
end;
{End !!.10}
{------------------------------------------------------------------------------}



{-TFFProxyCursor---------------------------------------------------------------}
function TFFProxyCursor.BlobCreate(var aBlobNr : TFFInt64) : TffResult;
var
  Request  : TffnmCreateBLOBReq;
  Reply    : PffnmCreateBLOBRpy;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmCreateBLOB,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);

  if ResultOK(Result) then
    aBlobNr := Reply^.BLOBNr;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyCursor.BLOBDelete(aBlobNr : TFFInt64) : TffResult;
var
  Request  : TffnmDeleteBLOBReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.BLOBNr := aBlobNr;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmDeleteBLOB,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyCursor.BLOBFree(aBlobNr   : TffInt64;
                                 aReadOnly : Boolean) : TffResult;
var
  Request  : TffnmFreeBLOBReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.BLOBNr := aBLOBNr;
  Request.ReadOnly := aReadOnly;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmFreeBLOB,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyCursor.BLOBGetLength(aBlobNr : TffInt64;
                                  var aLength : Longint) : TffResult;
var
  Request  : TffnmGetBLOBLengthReq;
  Reply    : PffnmGetBLOBLengthRpy;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.BLOBNr := aBLOBNr;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmGetBLOBLength,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);
  if ResultOK(Result) then
    aLength := Reply^.BLOBLength;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{Begin !!.03}
{----------}
function TffProxyCursor.BLOBListSegments(aBLOBNr : TffInt64;
                                         aStream : TStream) : TffResult;
var
  Request : TffnmListBLOBSegmentsReq;
  ReplyLen : Longint;
begin
  Request.CursorID := SrCursorID;
  Request.BLOBNr := aBLOBNr;
  Result := Client.ProcessRequest(ffnmListBLOBSegments,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(aStream),
                                  ReplyLen,
                                  nmdStream);

  if ResultOK(Result) then
    aStream.Position := 0;
end;
{End !!.03}
{----------}
function TFFProxyCursor.BLOBRead(aBlobNr    : TffInt64;
                                 aOffset    : TffWord32;               {!!.06}
                                 aLen       : TffWord32;               {!!.06}
                             var aBLOB;
                             var aBytesRead : TffWord32)               {!!.06}
                                            : TffResult;
var
  Request  : TffnmReadBLOBReq;
  Reply    : PffnmReadBLOBRpy;
  ReplyLen : longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.BLOBNr   := aBLOBNr;
  Request.Offset   := aOffset;
  Request.Len      := aLen;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmReadBLOB,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);

  if ResultOK(Result) then begin
    aBytesRead := Reply^.BytesRead;
    Move(Reply^.BLOB, aBLOB, aBytesRead);
  end;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyCursor.BLOBTruncate(aBlobNr     : TffInt64;
                                     aBLOBLength : Longint) : TffResult;
var
  Request  : TffnmTruncateBLOBReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID   := SrCursorID;
  Request.BLOBNr     := aBLOBNr;
  Request.BLOBLength := aBLOBLength;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmTruncateBLOB,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyCursor.BLOBWrite(aBlobNr : TffInt64;
                                  aOffset : Longint;
                                  aLen    : Longint;
                              var aBLOB) : TffResult;
var
  Request  : PffnmWriteBLOBReq;
  ReqLen   : longint;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  ReqLen := SizeOf(TffnmWriteBLOBReq) - 2 + aLen;
  FFGetZeroMem(Request, ReqLen);
  try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.BLOBNr := aBLOBNr;
    Request^.Offset := aOffSet;
    Request^.Len := aLen;
    Move(aBLOB, Request^.BLOB, aLen);

    Reply := nil;
    Result := Client.ProcessRequest(ffnmWriteBLOB,
                                    Timeout,
                                    Request,
                                    ReqLen,
                                    nmdByteArray,
                                    Reply,
                                    ReplyLen,
                                    nmdByteArray);
    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);
  finally
    FFFreeMem(Request, ReqLen);
  end;
end;
{----------}
function TFFProxyCursor.CompareBookmarks(aBookmark1  : PffByteArray;
                                         aBookmark2  : PffByteArray;
                                     var aCompResult : Longint) : TffResult;
var
  Request  : PffnmCursorCompareBMsReq;
  ReqLen   : Longint;
  Reply    : PffnmCursorCompareBMsRpy;
  pBM2     : Pointer;
  ReplyLen : Longint;
begin
  ReqLen := SizeOf(TffnmCursorCompareBMsReq) - 4 + (2 * BookmarkSize);
  FFGetZeroMem(Request, ReqLen);
  try
    { Initialize Request }
    Request^.CursorID     := SrCursorID;
    Request^.BookmarkSize := BookmarkSize;
    Move(aBookMark1^, Request^.Bookmark1, BookmarkSize);
    pBM2 := PffByteArray(PAnsiChar(@Request^.BookMark1) + BookmarkSize);
    Move(aBookMark2^, pBM2^, BookmarkSize);
    
    Reply := nil;
    Result  := Client.ProcessRequest(ffnmCursorCompareBMs,
                                     Timeout,
                                     Request,
                                     ReqLen,
                                     nmdByteArray,
                                     Pointer(Reply),
                                     ReplyLen,
                                     nmdByteArray);
    if ResultOK(Result) then
      aCompResult := Reply^.CompareResult;

    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);

  finally
    FFFreeMem(Request, ReqLen);
  end;
end;
{----------}
constructor TFFProxyCursor.Create(aDatabase  : TFFProxyDatabase;
                                  aCursorID  : TffCursorID;
                                  aTableName : string;
                                  aForServer : Boolean;
                                  aIndexName : string;
                                  aIndexID   : Longint;
                                  aOpenMode  : TffOpenMode;
                                  aShareMode : TffShareMode;
                                  aTimeout   : Longint;
                                  aStream    : TStream);
var
  Request  : TffnmOpenTableReq;
  ReplyLen : Longint;
  Result   : TffResult;

begin
  inherited Create;

  prClient      := aDatabase.Client;
  prDatabase    := aDatabase;
  prSrCursorID  := aCursorID;
  prTableName   := aTableName;
  prForServer   := aForServer;
  prDictionary  := TffDataDictionary.Create(4096);
  prIndexName   := aIndexName;
  prIndexID     := aIndexID;
  prIsSQLCursor := false;
  prShareMode   := aShareMode;
  prPhyRecSize  := 0;
  prTimeout     := aTimeout;

  if prSrCursorID <> 0 then Exit; {CursorClone operation, nothing more to do}

  Assert(Assigned(aStream));

  { Initialize Request }
  Request.DatabaseID  := Database.SrDatabaseID;
  Request.TableName   := FFExtractTableName(aTableName);
  Request.IndexName   := aIndexName;
  Request.IndexNumber := aIndexID;
  Request.OpenMode    := aOpenMode;
  Request.ShareMode   := aShareMode;
  Request.Timeout     := prTimeout;

  Result := Client.ProcessRequest(ffnmOpenTable,
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
end;
{----------}
constructor TffProxyCursor.CreateSQL(aDatabase  : TffProxyDatabase;
                                     aCursorID  : TffCursorID;
                                     aOpenMode  : TffOpenMode;
                                     aShareMode : TffShareMode;
                                     aTimeout   : longInt;
                                     aStream    : TStream);
begin
  inherited Create;

  Assert(Assigned(aStream));
  
  prClient      := aDatabase.Client;
  prDatabase    := aDatabase;
  prTableName   := '';
  prForServer   := false;
  prDictionary  := TffDataDictionary.Create(ffcl_64k);
  prIsSQLCursor := True;
  prShareMode   := aShareMode;
  prTimeout     := aTimeout;

  aStream.Position := 0;
  aStream.Read(prSrCursorID, SizeOf(prSrCursorID));

  { Save the data dictionary for this table. }

  Dictionary.ReadFromStream(aStream);
//  aStream.Read(prIndexID, SizeOf(prIndexID));                        {Deleted !!.10}
  prIndexID := 0;                                                      {!!.10}
  prIndexName := prDictionary.IndexName[0];                            {!!.10}
  prPhyRecSize := prDictionary.RecordLength;
end;
{----------}
function TFFProxyCursor.CursorClone(aOpenMode    : TFFOpenMode;
                                var aNewCursorID : TFFCursorID) : TffResult;
var
  Request  : TffnmCursorCloneReq;
  Reply    : PffnmCursorCloneRpy;
  ReplyLen : Longint;
  NewCursor : TffProxyCursor;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.OpenMode := aOpenMode;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmCursorClone,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);
  if ResultOK(Result) then begin
    {Create a new proxy cursor with the appropriate information}
    NewCursor := TffProxyCursor.Create(prDatabase,
                                       Reply^.CursorID,
                                       ''{tableName},
                                       False, {forserver}
                                       prIndexName,
                                       prIndexID,
                                       aOpenMode,
                                       smShared, {share mode}
                                       prTimeout,
                                       nil);
    NewCursor.prDictionary.Assign(prDictionary);
    NewCursor.prIndexName := prIndexName;
    NewCursor.prPhyRecSize := NewCursor.prDictionary.RecordLength;
    aNewCursorID := Longint(NewCursor);
  end;
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
destructor TFFProxyCursor.Destroy;
var
  Request  : TffnmCursorCloseReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  if SrCursorID > 0 then begin
    { Initialize Request }
    Request.CursorID := SrCursorID;

    Reply := nil;
    Client.ProcessRequest(ffnmCursorClose,
                          Timeout,
                          @Request,
                          SizeOf(Request),
                          nmdByteArray,
                          Reply,
                          ReplyLen,
                          nmdByteArray);

    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);
  end;

  prSrCursorID := 0;
  prDictionary.Free;
  prDictionary := nil;
  prDatabase := nil;
  prClient := nil;

  inherited Destroy;
end;
{----------}
function TFFProxyCursor.FileBLOBAdd(const aFileName : TffFullFileName;
                                      var aBlobNr   : TffInt64) : TffResult;
var
  Request  : TffnmAddFileBLOBReq;
  Reply    : PffnmAddFileBLOBRpy;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.FileName := aFileName;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmAddFileBLOB,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);

  if ResultOK(Result) then
    aBlobNr := Reply^.BLOBNr;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{Begin !!.02}
{----------}
function TffProxyCursor.CopyRecords(aSrcCursor : TffProxyCursor;
                                    aCopyBLOBs : Boolean) : TffResult;
var
  Request  : TffnmCursorCopyRecordsReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin

  { Initialize Request }
  Request.SrcCursorID  := aSrcCursor.SrCursorID;
  Request.DestCursorID := SrCursorID;
  Request.CopyBLOBs    := aCopyBLOBs;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmCursorCopyRecords,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{End !!.02}
{Begin !!.06}
{----------}
function TffProxyCursor.DeleteRecords : TffResult;
var
  Request  : TffnmCursorDeleteRecordsReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin

  { Initialize Request }
  Request.CursorID  := SrCursorID;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmCursorDeleteRecords,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{End !!.06}
{----------}
function TFFProxyCursor.GetBookmark(aBookmark : PffByteArray) : TffResult;
var
  Request  : TffnmCursorGetBookMarkReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID     := SrCursorID;
  Request.BookMarkSize := BookMarkSize;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmCursorGetBookMark,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);

  if ResultOK(Result) then
    Move(Reply^, aBookmark^, BookmarkSize);                            {!!.05}

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyCursor.GetBookmarkSize(var aSize : Longint) : TffResult;
begin
  Result := DBIERR_NONE;
  if prIsSQLCursor then
    aSize := ffcl_FixedBookmarkSize
  else
    aSize := ffcl_FixedBookmarkSize + Dictionary.IndexKeyLength[IndexID];
end;
{----------}
function TFFProxyCursor.prGetBookmarkSize : Longint;
begin
  GetBookmarkSize(Result);
end;
{----------}
function TFFProxyCursor.RecordDelete(aData : PffByteArray) : TffResult;
var
  Request  : TffnmRecordDeleteReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  if aData = nil then
    Request.RecLen := 0
  else
    Request.RecLen := PhysicalRecordSize;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmRecordDelete,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);
  if ((ResultOK(Result)) and                                           {!!.06}
      (Assigned(aData))) then                                          {!!.06}
    Move(Reply^, aData^, ReplyLen);

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TffProxyCursor.RecordDeleteBatch(aBMCount  : Longint;
                                          aBMLen    : Longint;
                                          aData     : PffByteArray;
                                          aErrors   : PffLongintArray
                                         ) : TffResult;
var
  Request  : PffnmRecordDeleteBatchReq;
  MaxRecs  : LongInt;
  ReqLen   : LongInt;
  iErr     : Longint;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  MaxRecs := 65500 div aBMLen;
  if aBMCount > MaxRecs then begin
    Result := DBIERR_ROWFETCHLIMIT;
    Exit;
  end;
  ReqLen := SizeOf(Request^) - 2 +  (aBMLen * aBMCount);
  FFGetZeroMem(Request, ReqLen);
  try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.BMLen := aBMLen;
    Request^.BMCount := aBMCount;
    Move(aData^, Request^.BMArray, aBMCount * aBMLen);

    Reply := nil;
    Result := Client.ProcessRequest(ffnmRecordDeleteBatch,
                                    Timeout,
                                    Request,
                                    ReqLen,
                                    nmdByteArray,
                                    Reply,
                                    ReplyLen,
                                    nmdByteArray);
    if ResultOK(Result) then begin
      Move(Reply^, aErrors^, ReplyLen);
      for iErr := 0 to Pred(aBMCount) do
        if aErrors^[iErr] <> DBIERR_NONE then begin
          Result := aErrors^[iErr];
          Break;
        end;
    end;

    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);

  finally
    FFFreeMem(Request, ReqLen);
  end;
end;
{----------}
function TFFProxyCursor.RecordExtractKey(aData : PffByteArray;
                                         aKey  : PffByteArray) : TffResult;
var
  Request  : PffnmRecordExtractKeyReq;
  ReqLen   : Longint;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  ReqLen := SizeOf(TffnmRecordExtractKeyReq) - 2 + PhysicalRecordSize;
  FFGetZeroMem(Request, ReqLen);
  try
    { Initialize Request}
    Request^.CursorID := SrCursorID;
    Request^.KeyLen := Dictionary.IndexKeyLength[IndexID];
    if aData = nil then
      Request^.ForCurrentRecord := True
    else begin
      Move(aData^, Request^.Data, PhysicalRecordSize);
      Request^.ForCurrentRecord := False;
    end;

    Reply := nil;
    Result := Client.ProcessRequest(ffnmRecordExtractKey,
                                    Timeout,
                                    Request,
                                    ReqLen,
                                    nmdByteArray,
                                    Reply,
                                    ReplyLen,
                                    nmdByteArray);
    if ((ResultOK(Result)) and                                         {!!.06}
        (Assigned(aKey))) then                                         {!!.06}
      Move(Reply^, aKey^, ReplyLen);

    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);
  finally
    FFFreeMem(Request, ReqLen);                                        {!!.06}
  end;
end;
{----------}
function TFFProxyCursor.RecordGet(aLockType : TffLockType;
                                  aData     : PffByteArray)
                                            : TffResult;
var
  Request : TffnmRecordGetReq;
  Reply   : Pointer;
  RpyLen  : TffMemSize;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.LockType := aLockType;
  Request.RecLen   := PhysicalRecordSize; {server needs it no matter what}
  Request.BookMarkSize := BookMarkSize;
  if (aData = nil) then
    RpyLen := 0
  else
    RpyLen := Request.RecLen;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmRecordGet,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  RpyLen,
                                  nmdByteArray);
  if ((Assigned(Reply)) and                                            {!!.06}
      (Assigned(aData))) then begin                                    {!!.06}
    Move(Reply^, aData^, RpyLen);
    FFFreeMem(Reply, RpyLen);
  end;
end;
{----------}
function TFFProxyCursor.RecordGetBatch(aRecCount : Longint;
                                       aRecLen   : Longint;
                                   var aRecRead  : Longint;
                                       aData     : PffByteArray;
                                   var aError    : TffResult) : TffResult;
var
  Request  : TffnmRecordGetBatchReq;
  Reply    : PffnmRecordGetBatchRpy;
  ReplyLen : LongInt;
begin
  aRecRead := 0;
  ReplyLen := SizeOf(Reply^) - 2 + (aRecLen * aRecCount);
  Request.CursorID := SrCursorID;
  Request.RecLen   := aRecLen;
  Request.RecCount := aRecCount;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmRecordGetBatch,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);
  if ResultOK(Result) then begin
    aRecRead := Reply^.RecCount;
    Move(Reply^.RecArray, aData^, aRecRead * aRecLen);
    aError := Reply^.Error;
  end;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyCursor.RecordGetForKey(aDirectKey  : Boolean;
                                        aFieldCount : Longint;
                                        aPartialLen : Longint;
                                        aKeyData    : PffByteArray;
                                        aData       : PffByteArray;
                                        aFirstCall  : Boolean) : TffResult;
var
  Request    : PffnmRecordGetForKeyReq;
  ReqLen     : longint;
  Reply      : Pointer;
  RpyLen     : longint;
  DataLen    : Longint;
  DictRecLen : Longint;
begin
  DictRecLen := PhysicalRecordSize;
  if aDirectKey then
    DataLen := Dictionary.IndexKeyLength[IndexID]
  else
    DataLen := DictRecLen;
  ReqLen := SizeOf(TffnmRecordGetForKeyReq) - 2 + DataLen;
  FFGetZeroMem(Request, ReqLen);
  if (aData = nil) then
    RpyLen := 0
  else
    RpyLen := DictRecLen;
  try
    { Initialize Request }
    Request^.CursorID     := SrCursorID;
    Request^.BookMarkSize := BookMarkSize;
    Request^.DirectKey    := aDirectKey;
    Request^.FieldCount   := aFieldCount;
    Request^.PartialLen   := aPartialLen;
    Request^.RecLen       := DictRecLen;
    Request^.KeyDataLen   := DataLen;
    Move(aKeyData^, Request^.KeyData, DataLen);

    Reply := nil;
    Result := Client.ProcessRequest(ffnmRecordGetForKey,
                                    Timeout,
                                    Request,
                                    ReqLen,
                                    nmdByteArray,
                                    Reply,
                                    RpyLen,
                                    nmdByteArray);

    if ((Assigned(Reply)) and                                          {!!.06}
        (Assigned(aData))) then begin                                  {!!.06}
      Move(Reply^, aData^, RpyLen);
      FFFreeMem(Reply, RpyLen);
    end;
  finally
    FFFreeMem(Request, ReqLen);
  end;
end;
{----------}
function TFFProxyCursor.RecordGetNext(aLockType : TffLockType;
                                      aData     : PffByteArray) : TffResult;
var
  Request  : TffnmRecordGetNextReq;
  ReplyLen : Longint;
  Reply    : Pointer;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.LockType := aLockType;
  if (aData <> nil) then begin
    Request.RecLen := PhysicalRecordSize;
    Request.BookMarkSize := BookMarkSize;
  end else begin
    Request.RecLen := 0;
    Request.BookMarkSize := 0;
  end;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmRecordGetNext,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);
  if Assigned(Reply) then begin
    Move(Reply^, aData^, ReplyLen);
    FFFreeMem(Reply, ReplyLen);
  end;
end;
{----------}
function TFFProxyCursor.RecordGetPrior(aLockType : TffLockType;
                                       aData     : PffByteArray) : TffResult;
var
  Request  : TffnmRecordGetPrevReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.LockType := aLockType;
  if (aData <> nil) then begin
    Request.RecLen := PhysicalRecordSize;
    Request.BookMarkSize := BookMarkSize;
  end
  else begin
    Request.RecLen := 0;
    Request.BookMarkSize := 0;
  end;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmRecordGetPrev,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);

  if Assigned(Reply) then begin
    Move(Reply^, aData^, ReplyLen);
    FFFreeMem(Reply, ReplyLen);
  end;
end;
{----------}
function TFFProxyCursor.RecordInsert(aLockType : TffLockType;
                                     aData     : PffByteArray) : TffResult;
var
  Request  : PffnmRecordInsertReq;
  ReqLen   : Longint;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  ReqLen := SizeOf(Request^) - 2 + PhysicalRecordSize;
  FFGetZeroMem(Request, ReqLen);
  try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.LockType := aLockType;
    Request^.RecLen := PhysicalRecordSize;
    Request^.BookMarkSize := BookMarkSize;
    Move(aData^, Request^.Data, PhysicalRecordSize);

    Reply := nil;
    Result := Client.ProcessRequest(ffnmRecordInsert,
                                    Timeout,
                                    Request,
                                    ReqLen,
                                    nmdByteArray,
                                    Reply,
                                    ReplyLen,
                                    nmdByteArray);
    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);
  finally
    FFFreeMem(Request, ReqLen);
  end;
end;
{----------}
function TFFProxyCursor.RecordInsertBatch(aRecCount : Longint;
                                          aRecLen   : Longint;
                                          aData     : PffByteArray;
                                          aErrors   : PffLongintArray
                                         ) : TffResult;
var
  Request  : PffnmRecordInsertBatchReq;
  MaxRecs  : LongInt;
  ReqLen   : LongInt;
  iErr     : Longint;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  MaxRecs := 65500 div aRecLen;
  if aRecCount > MaxRecs then begin
    Result := DBIERR_ROWFETCHLIMIT;
    Exit;
  end;
  ReqLen := SizeOf(Request^) - 2 +  (aRecLen * aRecCount);
  FFGetZeroMem(Request, ReqLen);
  try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.RecLen := aRecLen;
    Request^.RecCount := aRecCount;
    Move(aData^, Request^.RecArray, aRecCount * aRecLen);

    Reply := nil;
    Result := Client.ProcessRequest(ffnmRecordInsertBatch,
                                    Timeout,
                                    Request,
                                    ReqLen,
                                    nmdByteArray,
                                    Reply,
                                    ReplyLen,
                                    nmdByteArray);
    if ResultOK(Result) then begin
      Move(Reply^, aErrors^, ReplyLen);
      for iErr := 0 to Pred(aRecCount) do
        if aErrors^[iErr] <> DBIERR_NONE then begin
          Result := aErrors^[iErr];
          Break;
        end;
    end;

    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);

  finally
    FFFreeMem(Request, ReqLen);
  end;
end;
{----------}
function TffProxyCursor.RecordIsLocked(aLockType : TffLockType;
                                   var aIsLocked : boolean) : TffResult;
var
  Request  : TffnmRecordIsLockedReq;
  Reply    : PffnmRecordIsLockedRpy;
  ReplyLen : Longint;
begin
  Request.CursorID := SrCursorID;
  Request.LockType := aLockType;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmRecordIsLocked,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);
  if ResultOK(Result) then
    aIsLocked := Reply^.IsLocked;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyCursor.RecordModify(aData    : PffByteArray;
                                     aRelLock : Boolean) : TffResult;
var
  Request  : PffnmRecordModifyReq;
  ReqLen   : Longint;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  ReqLen := SizeOf(Request^) - 2 + PhysicalRecordSize;
  FFGetZeroMem(Request, ReqLen);
  try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.RelLock  := aRelLock;
    Request^.RecLen   := PhysicalRecordSize;
    Request^.BookMarkSize := BookMarkSize;
    Move(aData^, Request^.Data, PhysicalRecordSize);

    Reply := nil;
    Result := Client.ProcessRequest(ffnmRecordModify,
                                    Timeout,
                                    Request,
                                    ReqLen,
                                    nmdByteArray,
                                    Reply,
                                    ReplyLen,
                                    nmdByteArray);
    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);
  finally
    FFFreeMem(Request, ReqLen);
  end;
end;
{----------}
function TFFProxyCursor.RecordRelLock(aAllLocks : Boolean) : TffResult;
var
  Request  : TffnmRecordRelLockReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.AllLocks := aAllLocks;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmRecordRelLock,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TffProxyCursor.TableGetAutoInc(var aValue : TffWord32) : TffResult;
var
  Request  : TffnmGetTableAutoIncValueReq;
  Reply    : PffnmGetTableAutoIncValueRpy;
  ReplyLen : Longint;
begin
  Request.CursorID := SrCursorID;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmGetTableAutoIncValue,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);
  if ResultOK(Result) then
    aValue := Reply^.AutoIncValue;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{Begin !!.03}
{----------}
function TffProxyCursor.ListBLOBFreeSpace(const aInMemory : Boolean;
                                                aStream : TStream) : TffResult;
var
  Request : TffnmGetBLOBFreeSpaceReq;
  ReplyLen : Longint;
begin
  Request.CursorID := SrCursorID;
  Request.InMemory := aInMemory;
  Result := Client.ProcessRequest(ffnmListBLOBFreeSpace,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(aStream),
                                  ReplyLen,
                                  nmdStream);

  if ResultOK(Result) then
    aStream.Position := 0;
end;
{End !!.03}
{----------}
function TffProxyCursor.OverrideFilter(aExpression : pCANExpr;
                                       aTimeout    : TffWord32) : TffResult;
var
  ReqSize   : Longint;
  Request   : PffnmCursorOverrideFilterReq;
  ExprTree  : CANExpr;
  Reply     : Pointer;
  ReplyLen  : Longint;
begin

  if not Assigned(aExpression) then begin
    aExpression := @ExprTree;
    FillChar(ExprTree, SizeOf(ExprTree), 0);
    ExprTree.iVer := CANEXPRVERSION;
    ExprTree.iTotalSize := SizeOf(ExprTree);
  end;

  ReqSize := (SizeOf(Request^) - 2 + aExpression^.iTotalSize);

  FFGetMem(Request, ReqSize);
  try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.Timeout  := aTimeout;

    Move(aExpression^, Request^.ExprTree, aExpression^.iTotalSize);

    Reply := nil;
    Result := Client.ProcessRequest(ffnmCursorOverrideFilter,
                                    Timeout,
                                    Pointer(Request),
                                    ReqSize,
                                    nmdByteArray,
                                    Reply,
                                    ReplyLen,
                                    nmdByteArray);
    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);
  finally
    FFFreeMem(Request, ReqSize);
  end;
end;
{----------}
function TFFProxyCursor.ResetRange : TffResult;
var
  Request  : TffnmCursorResetRangeReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmCursorResetRange,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);

end;
{----------}
function TffProxyCursor.RestoreFilter : TffResult;
var
  Request  : TffnmCursorRestoreFilterReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmCursorRestoreFilter,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);

end;
{----------}
function TFFProxyCursor.SetFilter(aExpression : pCANExpr;
                                  aTimeout    : TffWord32) : TffResult;
var
  ReqSize   : Longint;
  Request   : PffnmCursorSetFilterReq;
  ExprTree  : CANExpr;
  Reply     : Pointer;
  ReplyLen  : Longint;
begin
  if not Assigned(aExpression) then begin
    aExpression := @ExprTree;
    FillChar(ExprTree, SizeOf(ExprTree), 0);
    ExprTree.iVer := CANEXPRVERSION;
    ExprTree.iTotalSize := SizeOf(ExprTree);
  end;

  ReqSize := (SizeOf(Request^) - 2 + aExpression^.iTotalSize);

  FFGetMem(Request, ReqSize);
  try
    { Initialize Request }
    Request^.CursorID := SrCursorID;
    Request^.Timeout  := aTimeout;

    Move(aExpression^, Request^.ExprTree, aExpression^.iTotalSize);

    Reply := nil;
    Result := Client.ProcessRequest(ffnmCursorSetFilter,
                                    Timeout,
                                    Pointer(Request),
                                    ReqSize,
                                    nmdByteArray,
                                    Reply,
                                    ReplyLen,
                                    nmdByteArray);
    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);
  finally
    FFFreeMem(Request, ReqSize);
  end;
end;
{----------}
function TFFProxyCursor.SetRange(aDirectKey   : Boolean;
                                 aFieldCount1 : Longint;
                                 aPartialLen1 : Longint;
                                 aKeyData1    : PffByteArray;
                                 aKeyIncl1    : Boolean;
                                 aFieldCount2 : Longint;
                                 aPartialLen2 : Longint;
                                 aKeyData2    : PffByteArray;
                                 aKeyIncl2    : Boolean) : TffResult;
var
  Request  : PffnmCursorSetRangeReq;
  ReqLen   : Longint;
  KeyLen1  : Longint;
  KeyLen2  : Longint;
  Reply    : Pointer;
  ReplyLen : Longint;
  ReqKeyData2 : pointer;
begin
  {calculate sizes}
  if aKeyData1 = nil then
    KeyLen1 := 0
  else if aDirectKey then
    KeyLen1 := Dictionary.IndexKeyLength[ IndexID ]
  else
    KeyLen1 := PhysicalRecordSize;
  if aKeyData2 = nil then
    KeyLen2 := 0
  else if aDirectKey then
    KeyLen2 := Dictionary.IndexKeyLength[ IndexID ]
  else
    KeyLen2 := PhysicalRecordSize;

  {now, we know how large the Request is}
  ReqLen := SizeOf(Request^) - 4 + KeyLen1 + KeyLen2;

  {allocate and clear it}
  FFGetZeroMem(Request, ReqLen);
  try
    { Initialize Request }
    Request^.CursorID    := SrCursorID;
    Request^.DirectKey   := aDirectKey;
    Request^.FieldCount1 := aFieldCount1;
    Request^.PartialLen1 := aPartialLen1;
    Request^.KeyLen1     := KeyLen1;
    Request^.KeyIncl1    := aKeyIncl1;
    Request^.FieldCount2 := aFieldCount2;
    Request^.PartialLen2 := aPartialLen2;
    Request^.KeyLen2     := KeyLen2;
    Request^.KeyIncl2    := aKeyIncl2;
    Move(aKeyData1^, Request^.KeyData1, KeyLen1);
    ReqKeyData2 := PffByteArray(PAnsiChar(@Request^.KeyData1) + KeyLen1);
    Move(akeyData2^, ReqKeyData2^, KeyLen2);

    Reply := nil;
    Result := Client.ProcessRequest(ffnmCursorSetRange,
                                    Timeout,
                                    Request,
                                    ReqLen,
                                    nmdByteArray,
                                    Reply,
                                    ReplyLen,
                                    nmdByteArray);
    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);
  finally
    FFFreeMem(Request, ReqLen);
  end;
end;
{----------}
function TFFProxyCursor.SetTimeout(aTimeout : Longint) : TffResult;
var
  Request  : TffnmCursorSetTimeoutReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  Result := DBIERR_NONE;
  if prTimeout = aTimeout then Exit;

  prTimeout := aTimeout;

  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.Timeout := prTimeout;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmCursorSetTimeout,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyCursor.SetToBegin : TffResult;
var
  Request  : TffnmCursorSetToBeginReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmCursorSetToBegin,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyCursor.SetToBookmark(aBookmark : PffByteArray) : TffResult;
var
  Request  : PffnmCursorSetToBookmarkReq;
  ReqLen   : Longint;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  ReqLen := SizeOf(Request^) - 2 + BookMarkSize;
  FFGetZeroMem(Request, ReqLen);
  try
    { Initialize Request }
    Request^.CursorID     := SrCursorID;
    Request^.BookmarkSize := BookMarkSize;
    Move(aBookmark^, Request^.Bookmark, BookMarkSize);

    Reply := nil;
    Result := Client.ProcessRequest(ffnmCursorSetToBookmark,
                                    Timeout,
                                    Request,
                                    ReqLen,
                                    nmdByteArray,
                                    Reply,
                                    ReplyLen,
                                    nmdByteArray);
    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);
  finally
    FFFreeMem(Request, ReqLen);
  end;
end;
{----------}
function TFFProxyCursor.SetToCursor(aSourceCursor : TFFProxyCursor
                                   ) : TffResult;
var
  Request  : TffnmCursorSetToCursorReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.DestCursorID := SrCursorID;
  Request.SrcCursorID  := aSourceCursor.SrCursorID;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmCursorSetToCursor,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyCursor.SetToEnd : TffResult;
var
  Request  : TffnmCursorSetToEndReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmCursorSetToEnd,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyCursor.SetToKey(aSearchAction : TffSearchKeyAction;
                                 aDirectKey    : Boolean;
                                 aFieldCount   : Longint;
                                 aPartialLen   : Longint;
                                 aKeyData      : PffByteArray) : TffResult;
var
  Request    : PffnmCursorSetToKeyReq;
  ReqLen     : Longint;
  KeyDataLen : Longint;
  Reply      : Pointer;
  ReplyLen   : Longint;
begin
  if aDirectKey then
    KeyDataLen := Dictionary.IndexKeyLength[IndexID]
  else
    KeyDataLen := PhysicalRecordSize;
  ReqLen := SizeOf(TffnmCursorSetToKeyReq) - 2 + KeyDataLen;
  FFGetZeroMem(Request, ReqLen);
  try
    { Initialize Request }
    Request^.CursorID   := SrCursorID;
    Request^.Action     := aSearchAction;
    Request^.DirectKey  := aDirectKey;
    Request^.FieldCount := aFieldCount;
    Request^.PartialLen := aPartialLen;
    Request^.KeyDataLen := KeyDataLen;
    Move(aKeyData^, Request^.KeyData, KeyDataLen);

    Reply := nil;
    Result := Client.ProcessRequest(ffnmCursorSetToKey,
                                    Timeout,
                                    Pointer(Request),
                                    ReqLen,
                                    nmdByteArray,
                                    Reply,
                                    ReplyLen,
                                    nmdByteArray);
    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);
  finally
    FFFreeMem(Request, ReqLen);
  end;
end;
{----------}
function TFFProxyCursor.SwitchToIndex(aIndexName : TffDictItemName;
                                      aIndexID   : Longint;
                                      aPosnOnRec : Boolean) : TffResult;
var
  Request  : TffnmCursorSwitchToIndexReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID    := SrCursorID;
  Request.IndexName   := aIndexName;
  Request.IndexNumber := aIndexID;
  Request.PosnOnRec   := aPosnOnRec;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmCursorSwitchToIndex,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);

  if (Request.IndexName <> '') then begin
    prIndexID   := Dictionary.GetIndexFromName(Request.IndexName);
    prIndexName := aIndexName;
  end else begin
    prIndexID   := aIndexID;
    prIndexName := Dictionary.IndexName[aIndexID];
  end;
end;
{----------}
function TFFProxyCursor.TableGetRecCount(var aRecCount : Longint) : TffResult;
var
  Request  : TffnmGetTableRecCountReq;
  Reply    : PffnmGetTableRecCountRpy;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmGetTableRecCount,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);
  if ResultOK(Result) then
    aRecCount := Reply^.RecCount;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{Begin !!.07}
{----------}
function TFFProxyCursor.TableGetRecCountAsync(var aTaskID : Longint) : TffResult;
var
  Request  : TffnmGetTableRecCountAsyncReq;
  Reply    : PffnmGetTableRecCountAsyncRpy;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmGetTableRecCountAsync,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);
  if ResultOK(Result) then
    aTaskID := Reply^.RebuildID;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{End !!.07}
{----------}
function TFFProxyCursor.TableIsLocked(aLockType : TffLockType;
                                  var aIsLocked : Boolean) : TffResult;
var
  Request  : TffnmIsTableLockedReq;
  Reply    : PffnmIsTableLockedRpy;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.LockType := aLockType;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmIsTableLocked,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Pointer(Reply),
                                  ReplyLen,
                                  nmdByteArray);
  if ResultOK(Result) then
    aIsLocked := Reply^.IsLocked;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyCursor.TableLockAcquire(aLockType : TffLockType) : TffResult;
var
  Request  : TffnmAcqTableLockReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialzie Request }
  Request.CursorID := SrCursorID;
  Request.LockType := aLockType;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmAcqTableLock,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyCursor.TableLockRelease(aAllLocks : Boolean) : TffResult;
var
  Request  : TffnmRelTableLockReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.AllLocks := aAllLocks;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmRelTableLock,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{----------}
function TFFProxyCursor.TableSetAutoInc(aValue : TffWord32) : TffResult;
var
  Request  : TffnmSetTableAutoIncValueReq;
  Reply    : Pointer;
  ReplyLen : Longint;
begin
  { Initialize Request }
  Request.CursorID := SrCursorID;
  Request.AutoIncValue := aValue;

  Reply := nil;
  Result := Client.ProcessRequest(ffnmSetTableAutoIncValue,
                                  Timeout,
                                  @Request,
                                  SizeOf(Request),
                                  nmdByteArray,
                                  Reply,
                                  ReplyLen,
                                  nmdByteArray);
  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);
end;
{------------------------------------------------------------------------------}

{-TffProxySQLStmt--------------------------------------------------------------}
constructor TffProxySQLStmt.Create(aDatabase : TffProxyDatabase;
                             const aTimeout  : longInt);
var
  Request  : TffnmSQLAllocReq;
  Reply    : PffnmSQLAllocRpy;
  ReplyLen : Longint;
  Result   : TffResult;
begin
  inherited Create;

  psClient   := aDatabase.Client;
  psDatabase := aDatabase;
  psTimeout  := aTimeout;

  { Initialize Request }
  Request.DatabaseID := aDatabase.SrDatabaseID;
  Request.Timeout    := aTimeout;

  Reply := nil;
  Result := psClient.ProcessRequest(ffnmSQLAlloc,
                                    psTimeout,
                                    @Request,
                                    SizeOf(Request),
                                    nmdByteArray,
                                    Pointer(Reply),
                                    ReplyLen,
                                    nmdByteArray);

  Check(Result);

  psSrStmtID := Reply^.StmtID;

  if Assigned(Reply) then
    FFFreeMem(Reply, ReplyLen);

end;
{----------}
destructor TffProxySQLStmt.Destroy;
var
  Request : TffnmSQLFreeReq;
  Reply : Pointer;
  ReplyLen : Longint;
begin

  if psSrStmtID > 0 then begin
    { Initialize Request }
    Request.StmtID := psSrStmtID;

    Reply := nil;
    psClient.ProcessRequest(ffnmSQLFree,
                            psTimeout,
                            @Request,
                            SizeOf(Request),
                            nmdByteArray,
                            Reply,
                            ReplyLen,
                            nmdByteArray);

    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);
  end;

  psSrStmtID := 0;
  psDatabase := nil;

  inherited Destroy;
end;
{----------}
function TffProxySQLStmt.Exec(aOpenMode : TffOpenMode;
                          var aCursorID : TffCursorID;
                              aStream   : TStream) : TffResult;
var
  Request  : TffnmSQLExecReq;
  ReplyLen : Longint;
  SvrCursorID : TffCursorID;
begin
  Assert(Assigned(aStream));
  { Initialize Request }
  Request.StmtID := psSrStmtID;
  Request.OpenMode := aOpenMode;

  Result := psClient.ProcessRequest(ffnmSQLExec,
                                    psTimeout,
                                    @Request,
                                    SizeOf(Request),
                                    nmdByteArray,
                                    Pointer(aStream),
                                    ReplyLen,
                                    nmdStream);

  { Was the execution successful? }
  if Result = DBIERR_NONE then begin
    { Yes. Get the cursorID from the stream & open a proxy cursor. }
    aStream.Position := 0;
    aStream.Read(SvrCursorID, sizeOf(SvrCursorID));
    aCursorID := SvrCursorID;
    if aCursorID <> 0 then
      Result := psDatabase.QueryOpen(SvrCursorID, aOpenMode, smShared, psTimeout,
                                     aStream, aCursorID);
  end;

  { Assumption: If an error occurs then the TffQuery component is responsible
    for displaying the error message returned from the server. }

end;
{----------}
function TffProxySQLStmt.Prepare(aQueryText: PChar;
                                 aStream : TStream) : TffResult;
var
  QueryLen : Longint;
  ReqLen : Longint;
  Request : PffnmSQLPrepareReq;
  ReplyLen : Longint;
begin
  Assert(Assigned(aStream));
  
  QueryLen := StrLen(aQueryText);
  ReqLen := SizeOf(TffnmSQLPrepareReq) - SizeOf(TffVarMsgField) + QueryLen + 1;
  FFGetZeroMem(Request, ReqLen);
  try
    { Prepare the request. }
    Request.StmtID := psSrStmtID;
    Move(aQueryText^, Request^.Query, QueryLen);

    Result := psClient.ProcessRequest(ffnmSQLPrepare,
                                      psTimeout,
                                      Request,
                                      ReqLen,
                                      nmdByteArray,
                                      Pointer(aStream),
                                      ReplyLen,
                                      nmdStream);

    { Assumption: Upper levels are responsible for Stream contents. }

  finally
    FFFreeMem(Request, ReqLen);
  end;

end;
{----------}
function TffProxySQLStmt.SetParams(aNumParams  : word;
                                   aParamDescs : pointer;
                                   aDataBuffer : PffByteArray;
                                   aDataLen    : Longint;
                                   aStream     : TStream) : TffResult;
var
  ReplyLen : Longint;
  Stream : TMemoryStream;
begin
  Assert(Assigned(aStream));
{ Output stream is expected to be:
      StmtID     (longint)
      NumParams  (word)
      ParamList  (array of TffSqlParamInfo)
      BufLen     (longint; size of DataBuffer)
      DataBuffer (data buffer)
}
  Stream := TMemoryStream.Create;
  try
    Stream.Write(psSrStmtID, SizeOf(psSrStmtID));
    Stream.Write(aNumParams, SizeOf(aNumParams));
    Stream.Write(aParamDescs^, aNumParams * SizeOf(TffSqlParamInfo));
    Stream.Write(aDataLen, sizeOf(aDataLen));
    Stream.Write(aDataBuffer^, aDataLen);
    Stream.Position := 0;

    Result := psClient.ProcessRequest(ffnmSQLSetParams,
                                      psTimeout,
                                      Stream.Memory,
                                      Stream.Size,
                                      nmdStream,
                                      Pointer(aStream),
                                      ReplyLen,
                                      nmdStream);
  finally
    Stream.Free;
  end;

end;
{------------------------------------------------------------------------------}

{-TFFRemoteServerEngine--------------------------------------------------------}
function TFFRemoteServerEngine.BLOBCreate(aCursorID : TffCursorID;
                                      var aBlobNr   : TffInt64) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.BlobCreate(aBlobNr);
end;
{----------}
function TFFRemoteServerEngine.BLOBDelete(aCursorID : TffCursorID;
                                           aBlobNr  : TffInt64) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.BLOBDelete(aBlobNr);
end;
{----------}
function TFFRemoteServerEngine.BLOBFree(aCursorID : TffCursorID;
                                         aBlobNr  : TffInt64;
                                         readOnly : Boolean) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.BLOBFree(aBlobNr,
                              ReadOnly);
end;
{----------}
function TFFRemoteServerEngine.BLOBGetLength(aCursorID : TffCursorID;
                                             aBlobNr   : TffInt64;
                                         var aLength   : Longint) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.BLOBGetLength(aBlobNr,
                                   aLength);
end;
{Begin !!.03}
{----------}
function TffRemoteServerEngine.BLOBListSegments(aCursorID : TffCursorID;
                                                aBLOBNr : TffInt64;
                                                aStream : TStream) : TffResult;
var
  Cursor : TffProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.BLOBListSegments(aBLOBNr, aStream);
end;
{End !!.03}
{----------}
function TFFRemoteServerEngine.BLOBRead(aCursorID  : TffCursorID;
                                        aBlobNr    : TffInt64;
                                        aOffset    : TffWord32;        {!!.06}
                                        aLen       : TffWord32;        {!!.06}
                                    var aBLOB;
                                    var aBytesRead : TffWord32)        {!!.06}
                                                   : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.BLOBRead(aBlobNr,
                              aOffset,
                              aLen,
                              aBLOB,
                              aBytesRead);
end;
{----------}
function TFFRemoteServerEngine.BLOBTruncate(aCursorID   : TffCursorID;
                                            aBlobNr     : TffInt64;
                                            aBLOBLength : Longint) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.BLOBTruncate(aBlobNr,
                                  aBLOBLength);
end;
{----------}
function TFFRemoteServerEngine.BLOBWrite(aCursorID : TffCursorID;
                                         aBlobNr   : TffInt64;
                                         aOffset   : Longint;
                                         aLen      : Longint;
                                     var aBLOB) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.BLOBWrite(aBlobNr,
                               aOffset,
                               aLen,
                               aBLOB);
end;
{Begin !!.01}
{----------}
function TffRemoteServerEngine.RemoteRestart(const aClientID : TffClientID) : TffResult;
var
  Client : TFFProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    Result := Client.RemoteRestart;
end;
{----------}
function TffRemoteServerEngine.RemoteStart(const aClientID : TffClientID) : TffResult;
var
  Client : TFFProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    Result := Client.RemoteStart;
end;
{----------}
function TffRemoteServerEngine.RemoteStop(const aClientID : TffClientID) : TffResult;
var
  Client : TFFProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    Result := Client.RemoteStop;
end;
{End !!.01}
{----------}
procedure TFFRemoteServerEngine.scInitialize;
begin
  { do nothing }
end;
{----------}
procedure TffRemoteServerEngine.scPrepareForShutdown;
begin
  { do nothing }
end;
{----------}
procedure TffRemoteServerEngine.scShutdown;
begin
  { do nothing }
end;
{----------}
procedure TffRemoteServerEngine.scStartup;
begin
  { do nothing }
end;
{----------}
function TffRemoteServerEngine.bseGetAutoSaveCfg : Boolean;
begin
  {This is here to kill warnings. Clients shouldn't care about the
   RSE's NoAutoSaveCfg setting.}
  Result := False;
end;
{----------}
function TFFRemoteServerEngine.bseGetReadOnly : Boolean;
var
  Client : TffProxyClient;
begin
  Client := GetDefaultClient;
  if Assigned(Client) then
    Result := Client.IsReadOnly
  else
    Result := False;
end;
{--------}
procedure TFFRemoteServerEngine.bseSetAutoSaveCfg(aValue : Boolean);   {!!.01 - Start}
begin
  {do nothing}
end;
{--------}
procedure TFFRemoteServerEngine.bseSetReadOnly(aValue : Boolean);
begin
  {do nothing}
end;
{--------}                                                             {!!.01 - End}
procedure TFFRemoteServerEngine.FFNotificationEx(const AOp   : Byte;
                                                       AFrom : TffComponent;
                                                 const AData : TffWord32);
var
  CL : TFFProxyClient;
  ClIdx : Longint;
  ClFound : Boolean;
begin
  inherited;                                                           {!!.11}
  if (AFrom = Transport) then
    if ((AOp = ffn_Destroy) or (AOp = ffn_Remove))  then begin
      FFNotifyDependents(ffn_Deactivate);
      rsTransport := nil;
    end else if (AOp = ffn_Deactivate) then
      FFNotifyDependents(ffn_Deactivate)
    else if (AOp = ffn_ConnectionLost) then begin
      { If we manage this client, then notify depenents that connection is
        lost. It is up to the baseclient dependents to check the data
        parameter to see if this notification affects them.}
      CL := nil;
      ClFound := False;
      with ClientList.BeginRead do
        try
          for ClIdx := 0 to Pred(ClientList.Count) do begin
            CL := TFFProxyClient(ClientList[ClIdx].Key^);
            if CL.pcSrClientID = AData then begin
              ClFound := True;
              Break;
            end;
          end;
        finally
          EndRead;
        end;
      if CLFound then begin
        ForceClosing(Longint(CL));
        ClientRemove(Longint(CL));
        FFNotifyDependentsEx(ffn_ConnectionLost, Longint(CL))
      end;
    end;
end;
{Begin !!.07}
{--------}
procedure TffRemoteServerEngine.Log(const aMsg : string);
begin
  FEventLog.WriteString(aMsg);
end;
{--------}
procedure TffRemoteServerEngine.LogAll(const Msgs : array of string);
begin
  FEventLog.WriteStrings(Msgs);
end;
{--------}
procedure TffRemoteServerEngine.LogFmt(const aMsg : string; args : array of const);
begin
  FEventLog.WriteString(format(aMsg, args));
end;
{End !!.07}
{--------}
function TFFRemoteServerEngine.CheckClientIDAndGet(aClientID : TffClientID;
                                               var aClient   : TffProxyClient
                                                  ) : TffResult;
begin
  Result := DBIERR_INVALIDHNDL;

  aClient := nil;
  try
    if (TObject(aClientID) is TFFProxyClient) then begin
      aClient := TffProxyClient(aClientID);
      Result := DBIERR_NONE;
    end;
  except
   { An exception may be raised if the ID is bogus.  Swallow the exception.}
  end;
end;
{----------}
function TFFRemoteServerEngine.CheckCursorIDAndGet(aCursorID : TffCursorID;
                                               var aCursor   : TffProxyCursor
                                                  ) : TffResult;
begin
  Result := DBIERR_INVALIDHNDL;

  aCursor := nil;
  try
    if (TObject(aCursorID) is TFFProxyCursor) then begin
      aCursor := TffProxyCursor(aCursorID);
      Result := DBIERR_NONE;
    end;
  except
    { An exception may be raised if the ID is bogus.  Swallow the exception.}
  end;
end;
{----------}
function TffRemoteServerEngine.CheckStmtIDAndGet(aStmtID : TffSqlStmtID;
                                             var aStmt : TffProxySQLStmt) : TffResult;
begin
  Result := DBIERR_INVALIDHNDL;

  aStmt := nil;
  try
    if (TObject(aStmtID) is TffProxySQLStmt) then begin
      aStmt := TffProxySQLStmt(aStmtID);
      Result := DBIERR_NONE;
    end;
  except
    { An exception may be raised if the ID is bogus.  Swallow the exception.}
  end;
end;
{----------}
function TFFRemoteServerEngine.CheckDatabaseIDAndGet(
                                                 aDatabaseID : TffDatabaseID;
                                             var aDatabase : TffProxyDatabase
                                                    ) : TffResult;
begin
  Result := DBIERR_INVALIDHNDL;

  aDatabase := nil;
  try
    if (TObject(aDatabaseID) is TFFProxyDatabase) then begin
      aDatabase := TffProxyDatabase(aDatabaseID);
      Result := DBIERR_NONE;
    end;
  except
    { An exception may be raised if the ID is bogus.  Swallow the exception.}
  end;
end;
{----------}
function TFFRemoteServerEngine.CheckSessionIDAndGet(aClientID  : TffClientID;
                                                    aSessionID : TffSessionID;
                                                var aClient    : TffProxyClient;
                                                var aSession   : TffProxySession
                                                   ) : TffResult;
begin
  aSession := nil;
  aClient := nil;

  Result := CheckClientIDAndGet(aClientID, aClient);
  if (Result = DBIERR_NONE) then begin
    try
      if (TObject(aSessionID) is TFFProxySession) then begin
        aSession := TffProxySession(aSessionID)
      end;
    except
      { An exception may be raised if the ID is bogus.  Swallow the exception.}
    end;
  end;
end;
{----------}
function TFFRemoteServerEngine.ClientAdd(var aClientID   : TffClientID;
                                       const aClientName : TffNetName;
                                       const aUserID     : TffName;
                                       const aTimeout    : Longint;
                                         var aHash       : TffWord32
                                        ) : TffResult;
var
  Client   : TFFProxyClient;
  ListItem : TffIntListItem;

begin
  Result := DBIERR_NONE;
  Client := nil;

  {Create client object}
  try
    Client := TFFProxyClient.Create(rsTransport, aUserID, aHash, aTimeOut);
  except
    on E:Exception do
      if (E is EffException) or
         (E is EffDatabaseError) or
         (E is EffServerComponentError) then
        Result := EffException(E).ErrorCode;
  end;

  if ResultOK(Result) and Assigned(Client) then begin
    {Add to the internal list}
    ListItem := TffIntListItem.Create(Longint(Client));
    with rsClientList.BeginWrite do
      try
        Insert(ListItem);
      finally
        EndWrite;
      end;

    {Set the return value}
    aClientID := Longint(Client);
  end;
end;
{Begin !!.11}
function TffRemoteServerEngine.ClientAddEx(var aClientID   : TffClientID;
                                         const aClientName : TffNetName;
                                         const aUserID     : TffName;
                                         const aTimeout     : Longint;
                                         const aClientVersion : Longint;
                                           var aHash       : TffWord32) : TffResult;
begin
  Result := ClientAdd(aClientID, aClientName, aUserID, aTimeout, aHash);
end;
{End !!.11}
{----------}
function TFFRemoteServerEngine.ClientRemove(aClientID : TffClientID
                                            ) : TffResult;
var
  Client : TFFProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    {Remove from the internal list, and free}
    with rsClientList.BeginWrite do
      try
        Delete(Client);                                                {!!.01}
        Client.Free;
      finally
        EndWrite;
      end;
end;
{----------}
function TFFRemoteServerEngine.ClientSetTimeout(const aClientID : TffClientID;
                                                const aTimeout  : Longint
                                               ) : TffResult;
var
  Client : TFFProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    Result := Client.SetTimeout(aTimeout);
end;
{----------}
constructor TFFRemoteServerEngine.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);

  rsClientList  := TFFProxyClientList.Create;
  rsTimeout     := 0;
  rsTransport   := nil;

  with RemoteServerEngines.BeginWrite do
    try
      Insert(TffIntListItem.Create(Longint(Self)));
    finally
      EndWrite;
    end;
end;
{----------}
function TFFRemoteServerEngine.CursorClone(aCursorID    : TffCursorID;
                                           aOpenMode    : TffOpenMode;
                                       var aNewCursorID : TffCursorID
                                           ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.CursorClone(aOpenMode,
                                 aNewCursorID);
end;
{----------}
function TFFRemoteServerEngine.CursorClose(aCursorID : TffCursorID) : TffResult;
var
  Cursor : TFFProxyCursor;
  Database : TFFProxyDatabase;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then begin
    Database := Cursor.Database;
    Result := Database.TableClose(Cursor);
  end;
end;
{----------}
function TFFRemoteServerEngine.CursorCompareBookmarks(
                                                    aCursorID   : TffCursorID;
                                                    aBookmark1  : PffByteArray;
                                                    aBookmark2  : PffByteArray;
                                                var aCompResult : Longint
                                                     ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.CompareBookmarks(aBookmark1,
                                      aBookmark2,
                                      aCompResult);
end;
{Begin !!.02}
{----------}
function TffRemoteServerEngine.CursorCopyRecords(aSrcCursorID,
                                                 aDestCursorID : TffCursorID;
                                                 aCopyBLOBs : Boolean) : TffResult;
var
  DestCursor, SrcCursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aDestCursorID, DestCursor);
  if ResultOK(Result) then begin
    Result := CheckCursorIDAndGet(aSrcCursorID, SrcCursor);
    if ResultOK(Result) then
      Result := DestCursor.CopyRecords(SrcCursor, aCopyBLOBs);
  end;
end;
{End !!.02}
{Begin !!.06}
{----------}
function TffRemoteServerEngine.CursorDeleteRecords(aCursorID : TffCursorID) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.DeleteRecords;
end;
{End !!.06}
{----------}
function TFFRemoteServerEngine.CursorGetBookmark(aCursorID : TffCursorID;
                                                 aBookmark : PffByteArray
                                                ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.GetBookmark(aBookmark);
end;
{Begin !!.03}
{----------}
function TffRemoteServerEngine.CursorListBLOBFreeSpace(aCursorID : TffCursorID;
                                                 const aInMemory : Boolean;
                                                       aStream : TStream) : TffResult;
var
  Cursor : TffProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.ListBLOBFreeSpace(aInMemory, aStream);
end;
{End !!.03}
{----------}
function TffRemoteServerEngine.CursorOverrideFilter(aCursorID   : longint;
                                                    aExpression : pCANExpr;
                                                    aTimeout    : TffWord32) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.OverrideFilter(aExpression, aTimeout);
end;
{----------}
function TffRemoteServerEngine.CursorRestoreFilter(aCursorID : longInt) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.RestoreFilter;
end;
{----------}
function TFFRemoteServerEngine.CursorGetBookmarkSize(aCursorID : TffCursorID;
                                                 var aSize     : Longint
                                                    ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.GetBookmarkSize(aSize);
end;
{----------}
function TFFRemoteServerEngine.CursorResetRange(aCursorID : TffCursorID
                                                ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.ResetRange;
end;
{----------}
function TFFRemoteServerEngine.CursorSetFilter(aCursorID   : TffCursorID;
                                               aExpression : pCANExpr;
                                               aTimeout    : TffWord32
                                              ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.SetFilter(aExpression,
                               aTimeout);
end;
{----------}
function TFFRemoteServerEngine.CursorSetRange(aCursorID    : TffCursorID;
                                              aDirectKey   : Boolean;
                                              aFieldCount1 : Longint;
                                              aPartialLen1 : Longint;
                                              aKeyData1    : PffByteArray;
                                              aKeyIncl1    : Boolean;
                                              aFieldCount2 : Longint;
                                              aPartialLen2 : Longint;
                                              aKeyData2    : PffByteArray;
                                              aKeyIncl2    : Boolean
                                             ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.SetRange(aDirectKey,
                              aFieldCount1,
                              aPartialLen1,
                              aKeyData1,
                              aKeyIncl1,
                              aFieldCount2,
                              aPartialLen2,
                              aKeyData2,
                              aKeyIncl2);
end;
{----------}
function TFFRemoteServerEngine.CursorSetTimeout(const aCursorID : TffCursorID;
                                                const aTimeout  : Longint
                                               ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.SetTimeout(aTimeout);
end;
{----------}
function TFFRemoteServerEngine.CursorSetToBegin(aCursorID : TffCursorID
                                               ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.SetToBegin;
end;
{----------}
function TFFRemoteServerEngine.CursorSetToBookmark(aCursorID : TffCursorID;
                                                   aBookmark : PffByteArray
                                                  ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.SetToBookmark(aBookmark);
end;
{----------}
function TFFRemoteServerEngine.CursorSetToCursor(aDestCursorID : TffCursorID;
                                                 aSrcCursorID  : TffCursorID
                                                ) : TffResult;
var
  DestCursor   : TFFProxyCursor;
  SourceCursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aDestCursorID, DestCursor);
  if ResultOK(Result) then
    Result := CheckCursorIDAndGet(aSrcCursorID, SourceCursor);
    if ResultOK(Result) then
      Result := DestCursor.SetToCursor(SourceCursor);
end;
{----------}
function TFFRemoteServerEngine.CursorSetToEnd(aCursorID : TffCursorID
                                             ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.SetToEnd;
end;
{----------}
function TFFRemoteServerEngine.CursorSetToKey(
                                            aCursorID     : TffCursorID;
                                            aSearchAction : TffSearchKeyAction;
                                            aDirectKey    : Boolean;
                                            aFieldCount   : Longint;
                                            aPartialLen   : Longint;
                                            aKeyData      : PffByteArray
                                             ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.SetToKey(aSearchAction,
                              aDirectKey,
                              aFieldCount,
                              aPartialLen,
                              aKeyData);
end;
{----------}
function TFFRemoteServerEngine.CursorSwitchToIndex(aCursorID  : TffCursorID;
                                                   aIndexName : TffDictItemName;
                                                   aIndexID   : Longint;
                                                   aPosnOnRec : Boolean
                                                  ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.SwitchToIndex(aIndexName,
                                   aIndexID,
                                   aPosnOnRec);
end;
{----------}
function TFFRemoteServerEngine.DatabaseAddAlias(const aAlias      : TffName;
                                                const aPath       : TffPath;
                                                      aCheckSpace : Boolean; {!!.11}
                                                const aClientID   : TffClientID)
                                                                  : TffResult;
var
  Client : TffProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    Result := Client.DatabaseAddAlias(aAlias, aPath, aCheckSpace);     {!!.11}
end;
{----------}
function TFFRemoteServerEngine.DatabaseAliasList(aList     : TList;
                                                 aClientID : TffClientID)
                                                           : TffResult;
var
  Client : TffProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    Result := Client.DatabaseAliasList(aList);
end;
{----------}
function TFFRemoteServerEngine.RecoveryAliasList(aList     : TList;
                                                 aClientID : TffClientID)
                                                           : TffResult;
begin
  Assert(False, 'RecoveryAliasList unsupported for TffRemoteServerEngine.');
  Result := DBIERR_NOTSUPPORTED;
end;
{----------}
function TFFRemoteServerEngine.DatabaseChgAliasPath(aAlias      : TffName;
                                                    aNewPath    : TffPath;
                                                    aCheckSpace : Boolean; {!!.11}
                                                    aClientID   : TffClientID)
                                                                : TffResult;
var
  Client : TffProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    Result := Client.DatabaseChgAliasPath(aAlias,
                                          aNewPath,
                                          aCheckSpace)                 {!!.11}
end;
{----------}
function TFFRemoteServerEngine.DatabaseClose(aDatabaseID : TffDatabaseID
                                             ) : TffResult;
var
  Database : TFFProxyDatabase;
  Client   : TFFProxyClient;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then begin
    Client := Database.Client;
    Result := Client.DatabaseClose(Database);
  end;  
end;
{----------}
function TFFRemoteServerEngine.DatabaseDeleteAlias(aAlias    : TffName;
                                                   aClientID : TffClientID
                                                  ) : TffResult;
var
  Client : TFFProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    Result := Client.DatabaseDeleteAlias(aAlias)
end;
{----------}
function TFFRemoteServerEngine.DatabaseGetAliasPath(aAlias    : TffName;
                                                var aPath     : TffPath;
                                                    aClientID : TffClientID
                                                    ) : TffResult;
var
  Client : TFFProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    Result := Client.DatabaseGetAliasPath(aAlias, aPath)
end;
{----------}
function TFFRemoteServerEngine.DatabaseGetFreeSpace(const aDatabaseID : TffDatabaseID;
                                                      var aFreeSpace  : Longint
                                                   ) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.GetDbFreeSpace(aFreeSpace);
end;
{----------}
function TffRemoteServerEngine.DatabaseModifyAlias(const aClientID   : TffClientID;
                                                   const aAlias      : TffName;
                                                   const aNewName    : TffName;
                                                   const aNewPath    : TffPath;
                                                         aCheckSpace : Boolean) {!!.11}
                                                                     : TffResult;
var
  Client : TFFProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    Result := Client.DatabaseModifyAlias(aAlias,
                                         aNewName,
                                         aNewPath,
                                         aCheckSpace)                  {!!.11}
end;
{----------}
function TFFRemoteServerEngine.DatabaseOpen(aClientID   : TffClientID;
                                      const aAlias      : TffName;
                                      const aOpenMode   : TffOpenMode;
                                      const aShareMode  : TffShareMode;
                                      const aTimeout    : Longint;
                                        var aDatabaseID : TffDatabaseID
                                           ) : TffResult;
var
  Client : TFFProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    Result := Client.DatabaseOpen(aAlias,
                                  aOpenMode,
                                  aShareMode,
                                  aTimeout,
                                  aDatabaseID);
end;
{----------}
function TFFRemoteServerEngine.DatabaseOpenNoAlias(aClientID   : TffClientID;
                                             const aPath       : TffPath;
                                             const aOpenMode   : TffOpenMode;
                                             const aShareMode  : TffShareMode;
                                             const aTimeout    : Longint;
                                               var aDatabaseID : TffDatabaseID
                                                  ) : TffResult;
var
  Client : TFFProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    Result := Client.DatabaseOpenNoAlias(aPath,
                                         aOpenMode,
                                         aShareMode,
                                         aTimeout,
                                         aDatabaseID);
end;
{----------}
function TFFRemoteServerEngine.DatabaseSetTimeout(
                                            const aDatabaseID : TffDatabaseID;
                                            const aTimeout    : Longint
                                                  ) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.SetTimeout(aTimeout);
end;
{----------}
function TffRemoteServerEngine.DatabaseTableExists(aDatabaseID : TffDatabaseID;
                                             const aTableName  : TffTableName;
                                               var aExists     : Boolean
                                                  ) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TableExists(aTableName, aExists);
end;
{----------}
function TFFRemoteServerEngine.DatabaseTableList(aDatabaseID : TffDatabaseID;
                                           const aMask       : TffFileNameExt;
                                                 aList       : TList
                                                ) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TableList(aMask,
                                 aList);
end;
{----------}
function TffRemoteServerEngine.DatabaseTableLockedExclusive(
                                                    aDatabaseID : TffDatabaseID;
                                              const aTableName  : TffTableName;
                                                var aLocked     : Boolean
                                                           ) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TableLockedExclusive(aTableName,
                                            aLocked);

end;
{----------}
destructor TFFRemoteServerEngine.Destroy;
//var                                                                  {!!.03}
//  Idx : Longint;                                                     {!!.03}
begin
  FFNotifyDependents(ffn_Destroy);

  { Make sure we are shutdown. }
  State := ffesInactive;

{Begin !!.03}
//  {Free dependent objects}
//  with rsClientList.BeginWrite do
//    try
//      for Idx := 0 to Pred(Count) do
//        TFFProxyClient(Items[Idx]).Free;
//    finally
//      EndWrite;
//    end;
{End !!.03}

  with RemoteServerEngines.BeginWrite do
    try
      Delete(Longint(Self));                                           {!!.01}
    finally
      EndWrite;
    end;

  {Free and nil internal lists}
  rsClientList.Free;
  rsClientList := nil;

  {Clear the transport}
  Transport := nil;

  inherited Destroy;
end;
{----------}
function TFFRemoteServerEngine.FileBLOBAdd(aCursorID : TffCursorID;
                                     const aFileName : TffFullFileName;
                                       var aBlobNr   : TffInt64) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.FileBLOBAdd(aFileName,
                                 aBlobNr);
end;
{----------}
function TFFRemoteServerEngine.GetDefaultClient: TFFProxyClient;
begin
  Result := nil;
  with rsClientList.BeginRead do
    try
      if Count > 0 then
        Result := TFFProxyClient(TffIntListItem(Items[0]).KeyAsInt);   {!!.01}
    finally
      EndRead;
    end;
end;
{----------}
function TFFRemoteServerEngine.GetServerDateTime(var aDateTime : TDateTime
                                                ) : TffResult;
begin
  if (GetDefaultClient <> nil) then
    Result := GetDefaultClient.GetServerDateTime(aDateTime)
  else
    Result := DBIERR_INVALIDHNDL;
end;
{----------}
function TFFRemoteServerEngine.GetServerSystemTime(var aSystemTime : TSystemTime) : TffResult;
begin
  if (GetDefaultClient <> nil) then
    Result := GetDefaultClient.GetServerSystemTime(aSystemTime)
  else
    Result := DBIERR_INVALIDHNDL;
end;
{----------}
function TFFRemoteServerEngine.GetServerGUID(var aGUID : TGUID) : TffResult;
begin
  if (GetDefaultClient <> nil) then
    Result := GetDefaultClient.GetServerGUID(aGUID)
  else
    Result := DBIERR_INVALIDHNDL;
end;
{----------}
function TFFRemoteServerEngine.GetServerID(var aUniqueID : TGUID) : TffResult;
begin
  if (GetDefaultClient <> nil) then
    Result := GetDefaultClient.GetServerID(aUniqueID)
  else
    Result := DBIERR_INVALIDHNDL;
end;
{----------}
function TFFRemoteServerEngine.GetServerStatistics(var Stats : TffServerStatistics) : TffResult;
begin;
  if (GetDefaultClient <> nil) then
    Result := GetDefaultClient.GetServerStatistics(Stats)
  else
    Result := DBIERR_INVALIDHNDL;
end;
{----------}
function TFFRemoteServerEngine.GetCommandHandlerStatistics(const CmdHandlerIdx : Integer;
                                                             var Stats : TffCommandHandlerStatistics) : TffResult;
begin
  if (GetDefaultClient <> nil) then
    Result := GetDefaultClient.GetCommandHandlerStatistics(CmdHandlerIdx,
                                                           Stats)
  else
    Result := DBIERR_INVALIDHNDL;
end;
{----------}
function TFFRemoteServerEngine.GetTransportStatistics(const CmdHandlerIdx : Integer;
                                                      const TransportIdx : Integer;
                                                        var Stats : TffTransportStatistics) : TffResult;
begin
  if (GetDefaultClient <> nil) then
    Result := GetDefaultClient.GetTransportStatistics(CmdHandlerIdx,
                                                      TransportIdx,
                                                      Stats)
  else
    Result := DBIERR_INVALIDHNDL;
end;
{----------}                                                     {end !!.07}
procedure TFFRemoteServerEngine.GetServerNames(aList: TStrings;
                                               aTimeout : Longint);
begin
  Transport.GetServerNames(aList, aTimeout);
end;
{----------}
procedure TFFRemoteServerEngine.ForceClosing(const aClientID : TffClientID);
var
  Client : TFFProxyClient;
begin
  if CheckClientIDAndGet(aClientID, Client) = DBIERR_NONE then
    Client.ForceClosed := True;
end;
{Begin !!.06}
{--------}
function TffRemoteServerEngine.ProcessRequest(aClientID        : TffClientID;
                                              aMsgID           : Longint;
                                              aTimeout         : Longint;
                                              aRequestData     : Pointer;
                                              aRequestDataLen  : Longint;
                                              aRequestDataType : TffNetMsgDataType;
                                          var aReply           : Pointer;
                                          var aReplyLen        : Longint;
                                              aReplyType       : TffNetMsgDataType) : TffResult;
var
  Client : TFFProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    Result := Client.ProcessRequest(aMsgID, aTimeout, aRequestData,
                                    aRequestDataLen, aRequestDataType,
                                    aReply, aReplyLen, aReplyType);
end;
{--------}
function TffRemoteServerEngine.ProcessRequestNoReply(aClientID       : TffClientID;
                                                     aMsgID          : Longint;
                                                     aTimeout        : Longint;
                                                     aRequestData    : Pointer;
                                                     aRequestDataLen : Longint ) : TffResult;
var
  Client : TFFProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    Result := Client.ProcessRequestNoReply(aMsgID, aTimeout, aRequestData,
                                           aRequestDataLen);
end;
{End !!.06}
{----------}
function TFFRemoteServerEngine.RebuildGetStatus(aRebuildID : Longint;
                                          const aClientID  : TffClientID;
                                            var aIsPresent : Boolean;
                                            var aStatus    : TffRebuildStatus
                                               ) : TffResult;
var
  Client : TFFProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    Result := Client.GetRebuildStatus(aRebuildID,
                                      aIsPresent,
                                      aStatus);
end;
{----------}
function TFFRemoteServerEngine.RecordDelete(aCursorID : TffCursorID;
                                            aData     : PffByteArray
                                           ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.RecordDelete(aData);
end;
{----------}
function TffRemoteServerEngine.RecordDeleteBatch(aCursorID : TffCursorID;
                                                 aBMCount  : Longint;
                                                 aBMLen    : Longint;
                                                 aData     : PffByteArray;
                                                 aErrors   : PffLongintArray
                                                ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.RecordDeleteBatch(aBMCount,
                                       aBMLen,
                                       aData,
                                       aErrors);
end;
{----------}
function TFFRemoteServerEngine.RecordExtractKey(aCursorID : TffCursorID;
                                                aData     : PffByteArray;
                                                aKey      : PffByteArray
                                               ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.RecordExtractKey(aData,
                                      aKey);
end;
{----------}
function TFFRemoteServerEngine.RecordGet(aCursorID : TffCursorID;
                                         aLockType : TffLockType;
                                         aData     : PffByteArray) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.RecordGet(aLockType,
                               aData);
end;
{----------}
function TFFRemoteServerEngine.RecordGetBatch(aCursorID : TffCursorID;
                                              aRecCount : Longint;
                                              aRecLen   : Longint;
                                          var aRecRead  : Longint;
                                              aData     : PffByteArray;
                                          var aError    : TffResult
                                             ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.RecordGetBatch(aRecCount,
                                    aRecLen,
                                    aRecRead,
                                    aData,
                                    aError);
end;
{----------}
function TFFRemoteServerEngine.RecordGetForKey(aCursorID   : TffCursorID;
                                               aDirectKey  : Boolean;
                                               aFieldCount : Longint;
                                               aPartialLen : Longint;
                                               aKeyData    : PffByteArray;
                                               aData       : PffByteArray;
                                               aFirstCall  : Boolean
                                              ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.RecordGetForKey(aDirectKey,
                                     aFieldCount,
                                     aPartialLen,
                                     aKeyData,
                                     aData,
                                     aFirstCall);
end;
{----------}
function TFFRemoteServerEngine.RecordGetNext(aCursorID : TffCursorID;
                                             aLockType : TffLockType;
                                             aData     : PffByteArray
                                            ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.RecordGetNext(aLockType,
                                   aData);
end;
{----------}
function TFFRemoteServerEngine.RecordGetPrior(aCursorID : TffCursorID;
                                              aLockType : TffLockType;
                                              aData     : PffByteArray
                                             ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.RecordGetPrior(aLockType,
                                    aData);
end;
{----------}
function TFFRemoteServerEngine.RecordInsert(aCursorID : TffCursorID;
                                            aLockType : TffLockType;
                                            aData     : PffByteArray
                                           ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.RecordInsert(aLockType,
                                  aData);
end;
{----------}
function TFFRemoteServerEngine.RecordInsertBatch(aCursorID : TffCursorID;
                                                 aRecCount : Longint;
                                                 aRecLen   : Longint;
                                                 aData     : PffByteArray;
                                                 aErrors   : PffLongintArray
                                                ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.RecordInsertBatch(aRecCount,
                                       aRecLen,
                                       aData,
                                       aErrors);
end;
{----------}
function TffRemoteServerEngine.RecordIsLocked(aCursorID : TffCursorID;
                                              aLockType : TffLockType;
                                          var aIsLocked : boolean) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.RecordIsLocked(aLockType,
                                    aIsLocked);
end;
{----------}
function TFFRemoteServerEngine.RecordModify(aCursorID : TffCursorID;
                                            aData     : PffByteArray;
                                            aRelLock  : Boolean) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.RecordModify(aData,
                                  aRelLock);
end;
{----------}
function TFFRemoteServerEngine.RecordRelLock(aCursorID : TffCursorID;
                                             aAllLocks : Boolean) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.RecordRelLock(aAllLocks);
end;
{----------}
procedure TFFRemoteServerEngine.rsSetTransport(const Value : TFFBaseTransport);
begin
  if rsTransport = Value then
    Exit;

  FFNotifyDependents(ffn_Deactivate);
  if Assigned(rsTransport) then
    rsTransport.FFRemoveDependent(Self);

  rsTransport := Value;
  if Assigned(rsTransport) then
    rsTransport.FFAddDependent(Self);
end;
{----------}
function TFFRemoteServerEngine.SessionAdd(const aClientID  : TffClientID;
                                          const aTimeout   : Longint;
                                            var aSessionID : TffSessionID
                                         ) : TffResult;
var
  Client : TFFProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    Result := Client.SessionAdd(aSessionID, aTimeout);
end;
{----------}
function TFFRemoteServerEngine.SessionCount(aClientID : TffClientID;
                                        var aCount    : Longint) : TffResult;
var
  Client : TFFProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    aCount := Client.SessionCount;
end;
{----------}
function TFFRemoteServerEngine.SessionGetCurrent(aClientID  : TffClientID;
                                             var aSessionID : TffSessionID
                                                ) : TffResult;
var
  Client  : TFFProxyClient;
  Session : TFFProxySession;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then begin
    Session := Client.CurrentSession;
    aSessionID := Longint(Session);
  end;
end;
{Begin !!.06}
{----------}
function TFFRemoteServerEngine.SessionCloseInactiveTables(aClientID  : TffClientID) : TffResult;
var
  Client  : TFFProxyClient;
begin
  Result := CheckClientIDAndGet(aClientID, Client);
  if ResultOK(Result) then
    Client.SessionCloseInactiveTables;
end;
{End !!.06}
{----------}
function TFFRemoteServerEngine.SessionRemove(aClientID  : TffClientID;
                                             aSessionID : TffSessionID
                                            ) : TffResult;
var
  Client  : TFFProxyClient;
  Session : TFFProxySession;
begin
  Result := CheckSessionIDAndGet(aClientID, aSessionID, Client, Session);
  if ResultOK(Result) then
    Client.SessionRemove(Session);
end;
{----------}
function TFFRemoteServerEngine.SessionSetCurrent(aClientID  : TffClientID;
                                                 aSessionID : TffSessionID
                                                ) : TffResult;
var
  Client  : TFFProxyClient;
  Session : TFFProxySession;
begin
  Result := CheckSessionIDAndGet(aClientID, aSessionID, Client, Session);
  if ResultOK(Result) then
    Client.SessionSetCurrent(Session);
end;
{----------}
function TFFRemoteServerEngine.SessionSetTimeout(
                                           const aClientID  : TffClientID;
                                           const aSessionID : TffSessionID;
                                           const aTimeout   : Longint
                                                ) : TffResult;
var
  Client  : TFFProxyClient;
  Session : TFFProxySession;
begin
  Result := CheckSessionIDAndGet(aClientID, aSessionID, Client, Session);
  if ResultOK(Result) then
    Result := Session.SetTimeout(aTimeout);
end;
{----------}
function TFFRemoteServerEngine.SQLAlloc(aClientID   : TffClientID;
                                        aDatabaseID : TffDatabaseID;
                                        aTimeout    : longInt;
                                    var aStmtID     : TffSqlStmtID) : TffResult;
var
  Database : TffProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.SQLAlloc(aTimeout, aStmtID);
end;
{----------}
function TFFRemoteServerEngine.SQLExec(aStmtID   : TffSqlStmtID;
                                       aOpenMode : TffOpenMode;
                                   var aCursorID : TffCursorID;
                                       aStream   : TStream) : TffResult;
var
  Statement : TffProxySQLStmt;
begin
  Assert(Assigned(aStream));
  Result := CheckStmtIDAndGet(aStmtID, Statement);
  if ResultOK(Result) then
    Result := Statement.Exec(aOpenMode, aCursorID, aStream);
end;
{----------}
function TFFRemoteServerEngine.SQLExecDirect(aClientID   : TffClientID;
                                             aDatabaseID : TffDatabaseID;
                                             aQueryText  : PChar;
                                             aTimeout    : longInt;
                                             aOpenMode   : TffOpenMode;
                                         var aCursorID   : TffCursorID;
                                             aStream     : TStream) : TffResult;
var
  Database : TffProxyDatabase;
begin
  Assert(Assigned(aStream));
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.SQLExecDirect(aQueryText, aOpenMode, aTimeout,
                                     aCursorID, aStream);
end;
{----------}
function TFFRemoteServerEngine.SQLFree(aStmtID : TffSqlStmtID) : TffResult;
var
  Statement : TffProxySQLStmt;
begin
  { Assumption: The cursor associated with the SQL statement has already been
    closed. }
  Result := CheckStmtIDAndGet(aStmtID, Statement);
  if Result = DBIERR_NONE then
    Statement.Free;
end;
{----------}
function TFFRemoteServerEngine.SQLPrepare(aStmtID    : TffSqlStmtID;
                                          aQueryText : PChar;
                                          aStream    : TStream) : TffResult;
var
  Statement : TffProxySQLStmt;
begin
  Assert(Assigned(aStream));
  Result := CheckStmtIDAndGet(aStmtID, Statement);
  if Result = DBIERR_NONE then
    Result := Statement.Prepare(aQueryText, aStream);
end;
{----------}
function TFFRemoteServerEngine.SQLSetParams(aStmtID     : TffSqlStmtID;
                                            aNumParams  : word;
                                            aParamDescs : pointer;
                                            aDataBuffer : PffByteArray;
                                            aDataLen    : Longint;
                                            aStream     : TStream
                                           ) : TffResult;
var
  Statement : TffProxySQLStmt;
begin
  Assert(Assigned(aStream));
  Result := CheckStmtIDAndGet(aStmtID, Statement);
  if Result = DBIERR_NONE then
    Result := Statement.SetParams(aNumParams, aParamDescs, aDataBuffer, aDataLen, aStream);
end;
{----------}
function TFFRemoteServerEngine.TableAddIndex(
                                       const aDatabaseID : TffDatabaseID;
                                       const aCursorID : TffCursorID;
                                       const aTableName : TffTableName;
                                       const aIndexDesc: TffIndexDescriptor
                                            ) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TableAddIndex(aCursorID,
                                     aTableName,
                                     aIndexDesc);
end;
{----------}
function TFFRemoteServerEngine.TableBuild(aDatabaseID : TffDatabaseID;
                                          aOverWrite  : Boolean;
                                    const aTableName  : TffTableName;
                                          aForServer  : Boolean;
                                          aDictionary : TffDataDictionary
                                         ) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TableBuild(aOverWrite,
                                  aTableName,
                                  aForServer,
                                  aDictionary);
end;
{----------}
function TFFRemoteServerEngine.TableDelete(aDatabaseID : TffDatabaseID;
                                     const aTableName  : TffTableName
                                          ) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TableDelete(aTableName);
end;
{----------}
function TFFRemoteServerEngine.TableDropIndex(aDatabaseID : TffDatabaseID;
                                              aCursorID   : TffCursorID;
                                        const aTableName  : TffTableName;
                                        const aIndexName  : TffDictItemName;
                                              aIndexID    : Longint
                                             ) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TableDropIndex(aCursorID,
                                      aTablename,
                                      aIndexName,
                                      aIndexID);
end;
{----------}
function TFFRemoteServerEngine.TableEmpty(aDatabaseID : TffDatabaseID;
                                          aCursorID   : TffCursorID;
                                    const aTableName  : TffTableName
                                         ) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TableEmpty(aCursorID,
                                  aTableName);
end;
{----------}
function TffRemoteServerEngine.TableGetAutoInc(aCursorID   : TffCursorID;
                                           var aValue      : TffWord32) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.TableGetAutoInc(aValue);
end;
{----------}
function TFFRemoteServerEngine.TableGetDictionary(aDatabaseID : TffDatabaseID;
                                            const aTableName  : TffTableName;
                                                  aForServer  : Boolean;
                                                  aStream     : TStream
                                                 ) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Assert(Assigned(aStream));
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TableGetDictionary(aTableName,
                                          aForServer,
                                          aStream);
end;
{----------}
function TFFRemoteServerEngine.TableGetRecCount(aCursorID : TffCursorID;
                                            var aRecCount : Longint
                                               ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.TableGetRecCount(aRecCount);
end;
{Begin !!.07}
{----------}
function TFFRemoteServerEngine.TableGetRecCountAsync(aCursorID : TffCursorID;
                                                 var aTaskID : Longint) : TffResult;
var
  Cursor : TffProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.TableGetRecCountAsync(aTaskID);
end;
{End !!.07}
{----------}
function TFFRemoteServerEngine.TableIsLocked(aCursorID : TffCursorID;
                                             aLockType : TffLockType;
                                         var aIsLocked : Boolean) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.TableIsLocked(aLockType,
                                   aIsLocked);
end;
{----------}
function TFFRemoteServerEngine.TableLockAcquire(aCursorID : TffCursorID;
                                                aLockType : TffLockType
                                               ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.TableLockAcquire(aLockType);
end;
{----------}
function TFFRemoteServerEngine.TableLockRelease(aCursorID : TffCursorID;
                                                aAllLocks : Boolean
                                               ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.TableLockRelease(aAllLocks);
end;
{----------}
function TFFRemoteServerEngine.TableOpen(const aDatabaseID : TffDatabaseID;
                                         const aTableName  : TffTableName;
                                         const aForServer  : Boolean;
                                         const aIndexName  : TffName;
                                               aIndexID    : Longint;
                                         const aOpenMode   : TffOpenMode;
                                               aShareMode  : TffShareMode;
                                         const aTimeout    : Longint;
                                           var aCursorID   : TffCursorID;
                                               aStream     : TStream) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Assert(Assigned(aStream));
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TableOpen(aTableName,
                                 aForServer,
                                 aIndexName,
                                 aIndexID,
                                 aOpenMode,
                                 aShareMode,
                                 aTimeout,
                                 aCursorID,
                                 aStream);
end;
{----------}
function TFFRemoteServerEngine.TablePack(aDatabaseID : TffDatabaseID;
                                   const aTableName  : TffTableName;
                                     var aRebuildID  : Longint) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TablePack(aTableName,
                                 aRebuildID);
end;
{----------}
function TFFRemoteServerEngine.TableRebuildIndex(aDatabaseID : TffDatabaseID;
                                           const aTableName  : TffTableName;
                                           const aIndexName  : TffName;
                                                 aIndexID    : Longint;
                                             var aRebuildID  : Longint
                                                ) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TableRebuildIndex(aTableName,
                                         aIndexName,
                                         aIndexID,
                                         aRebuildID);
end;
{----------}
function TFFRemoteServerEngine.TableRename(aDatabaseID : TffDatabaseID;
                                     const aOldName : TffName;
                                     const aNewName : TffName) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TableRename(aOldName,
                                   aNewName);
end;
{----------}
function TFFRemoteServerEngine.TableRestructure(aDatabaseID : TffDatabaseID;
                                          const aTableName  : TffTableName;
                                                aDictionary : TffDataDictionary;
                                                aFieldMap   : TffStringList;
                                            var aRebuildID  : Longint
                                               ) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TableRestructure(aTableName,
                                        aDictionary,
                                        aFieldMap,
                                        aRebuildID);
end;
{----------}
function TFFRemoteServerEngine.TableSetAutoInc(aCursorID : TffCursorID;
                                               aValue    : TffWord32
                                              ) : TffResult;
var
  Cursor : TFFProxyCursor;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if ResultOK(Result) then
    Result := Cursor.TableSetAutoInc(aValue);
end;
{Begin !!.11}
{----------}
function TFFRemoteServerEngine.TableVersion(aDatabaseID : TffDatabaseID;
                                      const aTableName  : TffTableName;
                                        var aVersion : Longint) : TffResult;
var
  Database : TFFProxyDatabase;
  Request  : TffnmGetTableVersionReq;
  Reply    : PffnmGetTableVersionRpy;
  ReplyLen : Longint;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then begin
    aVersion := 0;
    { Initialize Request }
    Request.DatabaseID := Database.SrDatabaseID;
    Request.TableName  := aTableName;

    Reply := nil;
    Result := Database.pdClient.ProcessRequest(ffnmGetTableVersion,
                                               Timeout,
                                               @Request,
                                               SizeOf(Request),
                                               nmdByteArray,
                                               Pointer(Reply),
                                               ReplyLen,
                                               nmdByteArray);

    if ResultOK(Result) then
      aVersion := Reply^.Version;

    if Assigned(Reply) then
      FFFreeMem(Reply, ReplyLen);
  end;  { if }
end;
{End !!.11}
{----------}
function TFFRemoteServerEngine.TransactionCommit(
                                           const aDatabaseID : TffDatabaseID
                                                ) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TransactionCommit;
end;
{----------}
function TFFRemoteServerEngine.TransactionRollback(
                                             const aDatabaseID : TffDatabaseID
                                                  ) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TransactionRollback;
end;
{----------}
function TFFRemoteServerEngine.TransactionStart(
                                          const aDatabaseID    : TffDatabaseID;
                                          const aFailSafe      : Boolean
                                               ) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TransactionStart(aFailSafe);
end;
{Begin !!.10}
{----------}
function TFFRemoteServerEngine.TransactionStartWith(
                                          const aDatabaseID    : TffDatabaseID;
                                          const aFailSafe      : Boolean;
                                          const aCursorIDs     : TffPointerList
                                               ) : TffResult;
var
  Database : TFFProxyDatabase;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, Database);
  if ResultOK(Result) then
    Result := Database.TransactionStartWith(aFailSafe, aCursorIDs);
end;
{End !!.10}
{----------}

initialization
  RemoteServerEngines := TffThreadList.Create;

finalization
  RemoteServerEngines.Free;
  RemoteServerEngines := nil;

end.
