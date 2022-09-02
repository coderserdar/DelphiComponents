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

{ Uncomment the following define in order to have the automatic transports
  log all activity to a file named AUTOTRANS.LOG. }
{.$DEFINE AutoLog}

{ Comment out the following define to disable raising of "Bookmarks do not
  match table" exceptions for invalid bookmarks in TFSDataSet.CompareBookmarks.
  Disabling this behavior is appropriate for certain data-aware controls
  such as the InfoPower DBTreeView and the VCL DBGrid. }
{$DEFINE RaiseBookmarksExcept}

Unit fsdb;

Interface

Uses
  {$IFDEF DCC6OrLater}
  Variants,
  {$ENDIF}
  Windows,
  Classes,
  {$IFNDEF DCC4OrLater}
  DBTables,
  {$ENDIF}
  ComCtrls,
  Controls,
  SysUtils,
  Db,
  fssrbde,
  fsclbde,
  fsllcomp,
  fslleng,
  fsclbase,
  fslogdlg,
  fsllbase,
  fssrbase,
  fsllcomm,
  fsclcfg,
  fsllprot,
  fslldict,
  fscltbrg,
  fsdbbase,
  fsCommon,
  fssrvdlg,
  fsstdate,
  fsllcoll,
  fshash,
  fsnetmsg,
  fsserverremoteclass,
  fslllgcy,
  Messages,
  fsllthrd,
  fsfunInterp,
  fsexfield,
  fszlib,
  fssqlbas,
  fssrcmd,
  fssrsec,
  contnrs,
  fslllog;

Const
  DefaultTimeOut = 10 * 1000; { 10 Seconds } {!!.01}
  dsFSMaxStringSize = 32767;
  dsFSMaxArraySize = 65531;
  dsFSMaxWordArraySize = 32767;
  dsFSMaxIntArraySize = 16383;
  dsFSMaxDoubleArraySize = 8191;

Type
  TfsConnectionLostEvent = Procedure(aSource: TObject) Of Object;
  {-an event triggered once when the conneciton to the server is lost, and
    onceafter code to retry, or clear associated components is complete. By
    default aRetry is set to False. If this is set to true then the client
    will try to reestablish the connection, and associated components. }

  TfsLoginEvent = Procedure(aSource: TObject;
    Var aUserName: TffName;
    Var aPassword: TffName;
    Var aResult: Boolean) Of Object;
  {-an event to get a user name and password for login purposes}

  TfsChooseServerEvent = Procedure(aSource: TObject;
    aServerNames: TStrings;
    Var aServerName: TffNetAddress;
    Var aResult: Boolean) Of Object;
  {-an event to choose server name to attach to}

  TfsFindServersEvent = Procedure(aSource: TObject;
    aStarting: Boolean) Of Object;
  {-an event to enable a 'waiting...' dialog or splash screen to be
    shown whilst finding server names}

Type
  TfsKeyEditType = ({Types of key to edit and store..}
    ketNormal, {..normal search key}
    ketRangeStart, {..range start key}
    ketRangeEnd, {..range end key}
    ketCurRangeStart, {..current range start key}
    ketCurRangeEnd, {..current range end key}
    ketSaved); {..saved key (for rollback)}

Type
  TfsCursorProperties = Packed Record { Virtual Table properties }
    TableName: String; { Table name}
    FileNameSize: Word; { Full file name size }
    FieldsCount: Word; { No of fields in Table }
    RecordSize: Word; { Record size (logical record) }
    RecordBufferSize: Word; { Record size (physical record) }
    KeySize: Word; { Key size }
    IndexCount: Word; { Number of indexes }
    ValChecks: Word; { Number of val checks }
    BookMarkSize: Word; { Bookmark size }
    BookMarkStable: Boolean; { Stable book marks }
    OpenMode: TffOpenMode; { ReadOnly / RW }
    ShareMode: TffShareMode; { Excl / Share }
    Indexed: Boolean; { Index is in use }
    XltMode: FFXLTMode; { Translate Mode }
    TblRights: Word; { Table  rights }
    Filters: Word; { Number of filters }
  End;

Type
  PfsNodeValue = ^TfsNodeValue;
  TfsNodeValue = Packed Record
    nvDType: Word;
    nvDSType: Word;
    nvSize: Word;
    nvValue: Pointer;
    nvIsNull: Boolean;
    nvIsConst: Boolean;
  End;

  PfsFilterNode = ^TfsFilterNode;
  TfsFilterNode = Packed Record
    Case Integer Of
      1: (fnHdr: CANHdr);
      2: (fnUnary: CANUnary);
      3: (fnBinary: CANBinary);
      4: (fnField: CANField);
      5: (fnConst: CANConst);
      7: (fnContinue: CANContinue);
      8: (fnCompare: CANCompare);
  End;

  TFSFilterListItem = Class(TffCollectionItem)
  Protected {private}
    fliActive: Boolean;
    fliCanAbort: Boolean;
    fliExpression: pCANExpr;
    fliExprSize: Word;
    fliFilterFunc: pfGENFilter;
    fliClientData: Longint;
    fliOwner: TObject;
    fliPriority: Integer;

  Protected
    Function fliGetLiteralPtr(aoffset: Word): Pointer;
    Function fliGetNodePtr(aoffset: Word): PfsFilterNode;

    Function fliEvaluateBinaryNode(aNode: PfsFilterNode;
      aRecBuf: Pointer;
      aNoCase: Boolean;
      aPartial: Word): Boolean;
    Function fliEvaluateConstNode(aNode: PfsFilterNode;
      aValue: PfsNodeValue;
      aRecBuf: Pointer): Boolean;
    Function fliEvaluateFieldNode(aNode: PfsFilterNode;
      aValue: PfsNodeValue;
      aRecBuf: Pointer): Boolean;
    Function fliEvaluateLogicalNode(aNode: PfsFilterNode;
      aRecBuf: Pointer): Boolean;
    Function fliEvaluateNode(aNode: PfsFilterNode;
      aValue: PfsNodeValue;
      aRecBuf: Pointer): Boolean;
    Function fliEvaluateUnaryNode(aNode: PfsFilterNode;
      aRecBuf: Pointer): Boolean;

    Function fliCompareValues(Var aCompareResult: Integer;
      Var aFirst: TfsNodeValue;
      Var aSecond: TfsNodeValue;
      aIgnoreCase: Boolean;
      aPartLen: Integer): Boolean;

  Public
    Constructor Create(aContainer: TffCollection;
      aOwner: TObject;
      aClientData: Longint;
      aPriority: Integer;
      aCanAbort: Boolean;
      aExprTree: pCANExpr;
      aFiltFunc: pfGENFilter);
    Destructor Destroy; Override;

    Function MatchesRecord(aRecBuf: Pointer): Boolean;
    Procedure GetFilterInfo(Index: Word; Var FilterInfo: FilterInfo);

    Property Active: Boolean
      Read fliActive
      Write fliActive;
  End;

Type
  TFSClient = Class;
  TFSClientList = Class;
  TFSSession = Class;
  TFSSessionList = Class;
  TFSBaseTable = Class;
  TFSBaseDatabase = Class;
  TFSDatabase = Class;
  TFSDatabaseList = Class;
  TFSTableProxy = Class;
  TFSTableProxyList = Class;
  TFSDataSet = Class;
  TFSTable = Class;

  TfsStream = Class;
  TfsMemoryStream = Class;
  TfsMemoryBlobStream = Class;
  TfsDirectBlobStream = Class;

  TFSClient = Class(TfsDBListItem)
  Protected {private}
    bcAutoClientName: Boolean;
    bcBeepOnLoginError: Boolean; {!!.06}
    bcOwnServerEngine: Boolean;
    bcClientID: TffClientID;
    bcOnConnectionLost: TfsConnectionLostEvent;
    bcPasswordRetries: Integer;
    bcServerEngine: TFSBaseServerEngine;
    bcTimeOut: Longint;
    bcUserName: TffNetName;
    bcPassword: String; {!!.06}
    bcRights: TffUserRights;
    {bcPassword is only used to store the last password at design-time.
     It is not used at run-time.}
    Function dbliCreateOwnedList: TfsDBList; Override;
    Procedure dbliClosePrim; Override;
    Procedure dbliDBItemAdded(aItem: TfsDBListItem); Override;
    Procedure dbliDBItemDeleted(aItem: TfsDBListItem); Override;
    Procedure dbliMustBeClosedError; Override;
    Procedure dbliMustBeOpenError; Override;

    Function bcGetServerEngine: TFSBaseServerEngine;
    Function bcGetUserName: String;
    Function bcGetPassword: String;
    Procedure bcSetAutoClientName(Const Value: Boolean);
    Procedure bcSetClientName(Const aName: String);
    Procedure bcSetUserName(Const Value: String);
    Procedure bcSetPassword(Const Value: String);
    Procedure bcSetServerEngine(Value: TFSBaseServerEngine);
    Procedure bcSetTimeout(Const Value: Longint);
    Function bcGetSession(aInx: Integer): TFSSession;
    Function bcGetSessionCount: Integer;

    Procedure OpenConnection(aSession: TFSSession); Virtual;

    Procedure bcDoConnectionLost; Dynamic;
    Function bcReinstateDependents: Boolean;
    Procedure bcClearDependents;

    Function ProcessRequest(aMsgID: Longint;
      aTimeout: Longint;
      aRequestData: Pointer;
      aRequestDataLen: Longint;
      aRequestDataType: TffNetMsgDataType;
      Var aReply: Pointer;
      Var aReplyLen: Longint;
      aReplyType: TffNetMsgDataType): TffResult; Virtual;
    { Backdoor method for sending a request to a server engine.
      Should only be implemented by remote server engines. }

    Function ProcessRequestNoReply(aMsgID: Longint;
      aTimeout: Longint;
      aRequestData: Pointer;
      aRequestDataLen: Longint): TffResult; Virtual;
    { Backdoor method for sending a request, no reply expected, to a
      server engine. Should only be implemented by remote server engines. }

  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure IDEConnectionLost(aSource: TObject);
    Procedure FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp;
      Const AData: TffWord32); Override;

    Procedure GetServerNames(aServerNames: TStrings); Virtual; {!!.01}
    Function IsConnected: Boolean; Virtual;

    Property ClientID: TffClientID
      Read bcClientID;
    Property OwnServerEngine: Boolean
      Read bcOwnServerEngine
      Stored False;
    Property SessionCount: Integer
      Read bcGetSessionCount
      Stored False;
    Property Sessions[aInx: Integer]: TFSSession
    Read bcGetSession;
    Property Rights: TffUserRights Read bcRights Write bcRights;

  Published
    Property Active;
    Property AutoClientName: Boolean
      Read bcAutoClientName
      Write bcSetAutoClientName
      Default False;
    Property BeepOnLoginError: Boolean {!!.06}
    Read bcBeepOnLoginError
      Write bcBeepOnLoginError
      Default True;
    Property ClientName: String
      Read dbliDBName
      Write bcSetClientName;

    Property OnConnectionLost: TfsConnectionLostEvent
      Read bcOnConnectionLost
      Write bcOnConnectionLost;
    Property PasswordRetries: Integer
      Read bcPasswordRetries
      Write bcPasswordRetries
      Default 3;
    Property ServerEngine: TFSBaseServerEngine
      Read bcGetServerEngine
      Write bcSetServerEngine;
    Property TimeOut: Longint
      Read bcTimeOut
      Write bcSetTimeOut
      Default DefaultTimeout;
    { Timeout specified in milliseconds }
    Property UserName: String
      Read bcGetUserName
      Write bcSetUserName;
    Property Password: String
      Read bcGetPassword
      Write bcSetPassword;
  End;

  TFSClientList = Class(TfsDBStandaloneList)
  Protected {private}
    Function clGetItem(aInx: Integer): TFSClient;
  Public
    Property Clients[aInx: Integer]: TFSClient
    Read clGetItem; Default;
  End;

  TFSSession = Class(TfsDBListItem)
  Protected {private}
    //scQuickConnect: TQuickConnect;
    scAutoSessionName: Boolean;
    scSessionID: TffSessionID;

    scOnStartup: TNotifyEvent;
    scChooseServer: TfsChooseServerEvent;
    scFindServers: TfsFindServersEvent;
    scLogin: TfsLoginEvent;
    scServerEngine: TFSBaseServerEngine;
    scTimeout: Longint;
    scTablePassword: TStrings;
  Protected
    Function scGetClient: TFSClient;
    Function scGetDatabase(aInx: Integer): TFSBaseDatabase;
    Function scGetDatabaseCount: Integer;
    Function scGetServerEngine: TFSBaseServerEngine;
    Procedure scRefreshTimeout; {!!.11}
    Procedure scSetAutoSessionName(Const Value: Boolean);
    Procedure scSetSessionName(Const aName: String);
    Procedure scSetTimeout(Const Value: Longint);
    Procedure setTablePassword(aValue: TStrings);
    Function dbliCreateOwnedList: TfsDBList; Override;
    Procedure dbliClosePrim; Override;
    Function dbliFindDBOwner(Const aName: String)
      : TfsDBListItem; Override;
    Procedure dbliMustBeClosedError; Override;
    Procedure dbliMustBeOpenError; Override;
    Procedure dbliOpenPrim; Override;
    Procedure DoStartup; Virtual;
    Procedure ChooseServer(Var aServerName: TffNetAddress);
    Procedure FindServers(aStarting: Boolean);
    Procedure DoLogin(Var aUserName: TffName;
      Var aPassword: TffName;
      Var aResult: Boolean);

    Function ProcessRequest(aMsgID: Longint;
      aTimeout: Longint;
      aRequestData: Pointer;
      aRequestDataLen: Longint;
      aRequestDataType: TffNetMsgDataType;
      Var aReply: Pointer;
      Var aReplyLen: Longint;
      aReplyType: TffNetMsgDataType)
      : TffResult; Virtual;
    { Backdoor method for sending a request to a server engine.
      Should only be implemented by remote server engines. }

    Function ProcessRequestNoReply(aMsgID: Longint;
      aTimeout: Longint;
      aRequestData: Pointer;
      aRequestDataLen: Longint)
      : TffResult; Virtual;
    { Backdoor method for sending a request, no reply expected, to a
      server engine. Should only be implemented by remote server engines. }

  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure AddAlias(Const aName: String;
      Const aPath: String;
      aCheckSpace: Boolean {!!.11}
      {$IFDEF DCC4OrLater} {!!.11}
      = False {!!.11}
      {$ENDIF}); {!!.11}
    Function AddAliasEx(Const aName: String;
      Const aPath: String;
      aCheckSpace: Boolean {!!.11}
      {$IFDEF DCC4OrLater} {!!.11}
      = False {!!.11}
      {$ENDIF}) {!!.11}
    : TffResult;
    Procedure CloseDatabase(aDatabase: TFSBaseDatabase);
    Procedure CloseInactiveTables; {!!.06}
    Procedure DeleteAlias(Const aName: String);
    Function DeleteAliasEx(Const aName: String): TffResult;
    Function FindDatabase(Const aName: String): TFSBaseDatabase;
    Procedure GetAliasNames(aList: TStrings);
    Function GetAliasNamesEx(aList: TStrings;
      Const aEmptyList: Boolean)
      : TffResult;
    Procedure GetAliasPath(Const aName: String;
      Var aPath: String);
    Procedure GetDatabaseNames(aList: TStrings);
    Function GetServerDateTime(Var aServerNow: TDateTime): TffResult;
    {begin !!.10}
    Function GetServerSystemTime(Var aServerNow: TSystemTime): TffResult;
    Function GetServerGUID(Var aGUID: TGUID): TffResult;
    Function GetServerID(Var aUniqueID: TGUID): TffResult;
    Function GetServerStatistics(Var aStats: TfsServerStatistics)
      : TffResult;
    Function GetCommandHandlerStatistics(Const aCmdHandlerIdx: Integer;
      Var aStats: TfsCommandHandlerStatistics)
      : TffResult;
    Function GetTransportStatistics(Const aCmdHandlerIdx: Integer;
      Const aTransportIdx: Integer;
      Var aStats: TfsTransportStatistics)
      : TffResult;
    {End !!.10}
    Procedure GetTableNames(Const aDatabaseName: String;
      Const aPattern: String;
      aExtensions: Boolean;
      aSystemTables: Boolean;
      aList: TStrings);
    Function GetTaskStatus(Const aTaskID: Longint;
      Var aCompleted: Boolean;
      Var aStatus: TffRebuildStatus): TffResult;
    Function GetTimeout: Longint;
    Function IsAlias(Const aName: String): Boolean;
    Function ModifyAlias(Const aName: String;
      Const aNewName: String;
      Const aNewPath: String;
      aCheckSpace: Boolean {!!.11}
      {$IFDEF DCC4OrLater} {!!.11}
      = False {!!.11}
      {$ENDIF}) {!!.11}
    : TffResult;
    Function OpenDatabase(Const aName: String): TFSBaseDatabase;
    Procedure SetLoginRetries(Const aRetries: Integer);
    Procedure SetLoginParameters(Const aName: TffName; aPassword: TffName);

    Property Client: TFSClient
      Read scGetClient;

    Property DatabaseCount: Integer
      Read scGetDatabaseCount;
    { TODO:: This functionality assumes that all dependents are databases.
      This is not the case when a plugin engine attaches itself to the
      session in order to re-use the connection. }

    Property Databases[aInx: Integer]: TFSBaseDatabase
    Read scGetDatabase;
    { TODO:: This functionality assumes that all dependents are databases.
      This is not the case when a plugin engine attaches itself to the
      session in order to re-use the connection. }

    Property ServerEngine: TFSBaseServerEngine
      Read scGetServerEngine;

    Property SessionID: TffSessionID
      Read scSessionID;

  Published
    Property Active;

    Property AutoSessionName: Boolean
      Read scAutoSessionName
      Write scSetAutoSessionName
      Default False;

    Property ClientName: String
      Read dbligetDBOwnerName
      Write dbliSetDBOwnerName;

    Property SessionName: String
      Read dbliDBName
      Write scSetSessionName;
    Property Passwords: TStrings
      Read scTablePassword
      Write setTablePassword;

    Property OnStartup: TNotifyEvent
      Read scOnStartup
      Write scOnStartup;

    Property OnChooseServer: TfsChooseServerEvent
      Read scChooseServer
      Write scChooseServer;

    Property OnFindServers: TfsFindServersEvent
      Read scFindServers
      Write scFindServers;

    Property OnLogin: TfsLoginEvent
      Read scLogin
      Write scLogin;

    Property TimeOut: Longint
      Read scTimeout
      Write scSetTimeout
      Default -1;
    { Timeout specified in milliseconds }
  End;

  TFSSessionList = Class(TfsDBList)
  Protected {private}
    slCurrSess: TFSSession;
  Protected
    Function slGetCurrSess: TFSSession;
    Function slGetItem(aInx: Integer): TFSSession;
    Procedure slSetCurrSess(CS: TFSSession);
  Public
    Property CurrentSession: TFSSession
      Read slGetCurrSess
      Write slSetCurrSess;

    Property Sessions[aInx: Integer]: TFSSession
    Read slGetItem; Default;
  End;

  TfsServerFilterTimeoutEvent = Procedure(Sender: TFSDataSet;
    Var Cancel: Boolean) Of Object;
  TffFilterEvaluationType = (fseLocal, fseServer);
  { If ffeLocal then filter statement is evaluated local to client.
    If fseserver then filter statement is evaluated on server. }

  TFSFieldDescItem = Class(TffCollectionItem)
  Protected {private}
    fdiPhyDesc: pFLDDesc;
    fdiLogDesc: pFLDDesc;
    fdiFieldNum: Integer;

  Public
    Constructor Create(aContainer: TffCollection; Const FD: FLDDesc);
    Destructor Destroy; Override;

    Property LogDesc: pFLDDesc
      Read fdiLogDesc;

    Property PhyDesc: pFLDDesc
      Read fdiPhyDesc;

    Property FieldNumber: Integer
      Read fdiFieldNum;
  End;

  TfsTableState = (TblClosed, TblOpened);

  TBlobDataArray = Array Of TBlobData;
  TfsBlobMode = (bmDirect, bmAuto, bmInMemory, bmCache);

  TFSDataSet = Class(TDataSet)
  Private
    fCurrentRecord, fLastCurrentRecord,
      fRecordCount: Longword;
    fSupportRecNo, fFlipOrder, fInRange: boolean;
  Protected {private}
    dsBookmarkOfs: Integer; {offset to bookmark in TDataSet record Buffer}
    dsBlobOpenMode: TffOpenMode;
    dsCalcFldOfs: Integer; {offset to calcfields in TDataSet record Buffer}
    dsClosing: Boolean;
    dsCurRecBuf: Pointer;
    dsCursorID: TffCursorID;
    dsDictionary: TFSInfoDict;
    dsExclusive: Boolean;
    dsExprFilter: hDBIFilter;
    dsFieldDescs: TffCollection;
    dsFilterActive: Boolean;
    dsFilterEval: TffFilterEvaluationType;
    dsFilterResync: Boolean;
    dsFilters: TffCollection;
    dsFilterTimeout: TffWord32;
    dsFuncFilter: hDBIFilter;
    dsOldValuesBuffer: PChar;
    dsOpenMode: TffOpenMode;
    dsPhyRecSize: Integer; {FlashFiler physical record size}
    dsProxy: TFSTableProxy;
    dsReadOnly: Boolean;
    dsRecBufSize: Integer; {TDataSet record Buffer size}
    dsRecInfoOfs: Integer; {offset to rec info in TDataSet record Buffer}
    dsRecordToFilter: Pointer;
    dsServerEngine: TFSBaseServerEngine;
    dsShareMode: TffShareMode;
    dsTableState: TfsTableState;
    dsTimeout: Longint;
    dsCheckTimeout: Longint;
    dsDeleteTimeout: Longint;

    dsRecLockedBeforeEdit: Boolean;
    dsRecLockedType: TfsUserRecLocking;
    //user decides how and when record has to be blocked

    btKeyBufSize: Integer; {key Buffer length}
    btKeyInfoOfs: Integer; {offset to key info in key Buffer}
    btKeyLength: Integer; {key length for current index}
    btKeyBuffer: Pointer; {current key Buffer being edited}
    btKeyBuffers: Pointer; {all Buffers for editing keys}
    btFieldsInIndex: Array[0..pred(fscl_MaxIndexFlds)] Of Integer;
    {fields in key for current index}
    btIndexByName: Boolean;
    {True if index specified by name, False, by fields}
    btIndexDefs: TIndexDefs; {index definitions}
    btIndexFieldCount: Integer;
    {count of fields in key for current index}
    btIndexFieldStr: String;
    {list of field names in index, sep by semicolons}
    btIndexName: String; {index name}
    btIndexID: Word;
    ActiveBuf: PChar;
    { If you need a timeout value, use the Timeout property.  Do not
      directly access this property as it may be set to -1.  The Timeout
      property takes this into account. }
    dsXltMode: FFXltMode;
    dsOnServerFilterTimeout: TfsServerFilterTimeoutEvent;
    // FSQLDISPLAY: TStrings;
    FConLostDestroyFields: Boolean;

    fBlobMode: TfsBlobMode;
    { 0 means do not limit "chunk" sizes, any other value determines }
    { the maximum number of bytes read/written to the server at once}
    fBlobChunkSize: Integer; //1024 * 512; //1024 * 1024;
    FListBlobCache: TObjectList;
    btIgnoreDataEvents: Boolean;
    fBlobAutoStartTransaction, fBlobModifiedError: boolean;

    Function CreateNormalBlobStream(aField: TField; aMode: TBlobStreamMode; aChunkSize: Longint): TfsStream;
    Function CreateCacheBlobStream(aField: TField; aMode: TBlobStreamMode; aChunkSize: Longint): TfsStream;
    Procedure WriteCacheBlob;
    Procedure RefreshCacheBlob;
  Protected
    fBlobStartedTransaction: boolean;

    Procedure btRetrieveIndexName(Const aNameOrFields: String;
      aIndexByName: Boolean;
      Var aIndexName: String);

    {---Property access methods---}
   // Procedure quSetSQLDISPLAY(aValue: TStrings);
    Function dsGetDatabase: TFSBaseDatabase;
    Function dsGetDatabaseName: String;
    Function dsGetServerEngine: TFSBaseServerEngine; Virtual;
    Function dsGetSession: TFSSession;
    Function dsGetSessionName: String;
    Function dsGetTableName: String;
    Function dsGetVersion: String;
    Procedure dsRefreshTimeout; {!!.11}
    Procedure SetBlobChunkSize(Const Value: Integer);
    Procedure SetBlobMode(Const Value: TfsBlobMode);
    Procedure dsSetDatabaseName(Const aValue: String);
    Procedure dsSetExclusive(Const aValue: Boolean);
    Procedure dsSetReadOnly(Const aValue: Boolean);
    Procedure dsSetSessionName(Const aValue: String);
    Procedure dsSetTableLock(LockType: TffLockType; Lock: Boolean);
    Procedure dsSetTableName(Const aValue: String); Virtual;
    Function dsGetTimeout: Longint;
    Procedure dsSetTimeout(Const Value: Longint);
    Procedure dsSetCheckTimeout(Const Value: Longint);
    Procedure dsSetDeleteTimeout(Const Value: Longint);
    Procedure dsSetVersion(Const aValue: String);

    {---Filtering---}
    Function dsActivateFilter(hFilter: hDBIFilter): TffResult;
    Procedure dsAddExprFilter(Const aText: String;
      Const aOpts: TFilterOptions);
    Function dsAddFilter(iClientData: Longint;
      iPriority: Word;
      bCanAbort: Bool;
      pCANExpr: pCANExpr;
      pffilter: pfGENFilter;
      Var hFilter: hDBIFilter): TffResult;
    Procedure dsAddFuncFilter(aFilterFunc: pfGENFilter);
    Function dsCancelServerFilter: Boolean; Virtual;
    Procedure dsClearServerSideFilter;
    Function dsCreateLookupFilter(aFields: TList;
      Const aValues: Variant;
      aOptions: TLocateOptions): HDBIFilter;
    Function dsDeactivateFilter(hFilter: hDBIFilter): TffResult;
    Procedure dsActivateFilters; Virtual; {!!.03}
    Procedure dsDeactivateFilters; Virtual; {!!.03}
    Function dsDropFilter(hFilter: hDBIFilter): TffResult;
    Procedure dsDropFilters;
    Function dsMatchesFilter(pRecBuff: Pointer): Boolean;
    Function dsOnFilterRecordCallback({ulClientData = Self}
      pRecBuf: Pointer;
      iPhyRecNum: Longint
      ): Smallint Stdcall;

    Procedure dsSetFilterEval(Const aMode: TffFilterEvaluationType);
    Procedure dsSetFilterTextAndOptions(Const aText: String;
      Const aOpts: TFilterOptions;
      Const aMode: TffFilterEvaluationType;
      Const atimeOut: TffWord32);
    Procedure dsSetServerSideFilter(Const aText: String;
      Const aOpts: TFilterOptions;
      aTimeout: TffWord32);
    Procedure dsSetFilterTimeout(Const numMS: TffWord32);
    Procedure dsUpdateFilterStatus;

    Function btGetIndexDesc(iIndexSeqNo: Word;
      Var idxDesc: IDXDesc): TffResult;
    Function btGetIndexDescs(Desc: pIDXDesc): TffResult;
    Function GetCursorProps(Var aProps: TfsCursorProperties): TffResult; Virtual;

    Function dsGetNextRecord(eLock: TffLockType;
      pRecBuff: Pointer;
      RecProps: pRECProps;
      Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
    Function dsGetNextRecordPrim(aCursorID: TffCursorID;
      eLock: TffLockType;
      pRecBuff: Pointer;
      RecProps: pRECProps;
      Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;

    Function dsGetPhyRecSize: Integer;

    Function dsGetPriorRecord(eLock: TffLockType;
      pRecBuff: Pointer;
      RecProps: pRECProps;
      Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
    Function dsGetPriorRecordPrim(eLock: TffLockType;
      pRecBuff: Pointer;
      RecProps: pRECProps;
      Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
    Function dsGetRecord(eLock: TffLockType;
      pRecBuff: Pointer;
      RecProps: pRECProps;
      Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;

    Function dsGetRecordCountPrim(Var iRecCount: Longword): TffResult;
    Function dsGetRecordPrim(eLock: TffLockType;
      pRecBuff: Pointer;
      RecProps: pRECProps;
      Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
    Procedure dsGetRecordInfo(aReadProps: Boolean); Virtual;
    Function dsModifyRecord(aBuffer: Pointer; aRelLock: Boolean): TffResult;

    {---Field management---}
    Procedure dsAddFieldDesc(aFieldDesc: PffFieldDescriptor;
      aFieldNo: Integer);
    Function dsGetFieldDescItem(iField: Integer;
      Var FDI: TFSFieldDescItem): Boolean;
    Function dsGetFieldNumber(FieldName: PChar): Integer;
    Procedure dsReadFieldDescs;
    Function dsTranslateCmp(Var aFirst: TfsNodeValue;
      Var aSecond: TfsNodeValue;
      aIgnoreCase: Boolean;
      aPartLen: Integer): Integer;
    Function dsTranslateGet(FDI: TFSFieldDescItem;
      pRecBuff: Pointer;
      pDest: Pointer;
      Var bBlank: Boolean): TffResult;
    Function dsTranslatePut(FDI: TFSFieldDescItem;
      pRecBuff: Pointer;
      pSrc: Pointer): TffResult;

    //FieldClass
    Function GetFieldClass(FieldType: TFieldType): TFieldClass; Override;
    Procedure DataConvert(Field: TField;
      Source,
      Dest: Pointer;
      ToNative: Boolean); Reintroduce; Virtual;

    {---Handle stuff---}
    Function dsCreateHandle: TffCursorID;
    Procedure DestroyHandle(aHandle: TffCursorID); Virtual;
    Function GetCursorHandle(aIndexName: String): TffCursorID; Virtual;

    {---Stuff required for descendent dataset's. Empty stubs it this class}
    Procedure dsGetIndexInfo; Virtual;
    Procedure dsAllocKeyBuffers; Virtual;
    Procedure dsCheckMasterRange; Virtual;

    {---Modes---}
    Procedure dsEnsureDatabaseOpen(aValue: Boolean);

    {---Blob stuff---}
    Function dsGetBlobHandle(pRecBuf: Pointer;
      aField: TField;
      Var aIsNull: Boolean;
      Var aBLOBNr: TffInt64): TffResult;
    Procedure dsSetBLOBHandle(pRecBuf: Pointer;
      aField: TField;
      aBLOBNr: TffInt64);
    Function dsForceBLOB(pRecBuf: Pointer;
      aField: TField;
      Var aBLOBNr: TffInt64): TffResult;
    Function dsGetBlobSize(aField: TField): Longint;
    Procedure CloseBlob(aField: TField); Override;
    Function dsFreeBlob(pRecBuf: Pointer; aField: TField): TffResult;
    Function dsTruncateBlob(pRecBuf: pointer; aField: TField; iLen: Longint): TffResult;

    {$IFDEF ResizePersistFields}
    Procedure ReSizePersistentFields;
    {$ENDIF}

    {---TDataSet method overrides---}
    {Opening, initializing and closing}
    Procedure CloseCursor; Override;
    Procedure InitFieldDefs; Override;
    Procedure InternalClose; Override;
    Procedure InternalOpen; Override;
    Procedure InternalInitFieldDefs; Override;
    Function IsCursorOpen: Boolean; Override;
    Procedure OpenCursor(aInfoQuery: Boolean); Override;
    {information}
    Procedure DataEvent(aEvent: TDataEvent; aInfo: Longint); Override;
    Procedure DoAfterScroll; Override;
    Procedure DoBeforeScroll; Override;
    Procedure DoBeforeInsert; Override;
    Procedure DoAfterDelete; Override;
    {Bookmark management and use}
    Procedure GetBookmarkData(aBuffer: PChar; aData: Pointer); Override;
    Function GetBookmarkFlag(aBuffer: PChar): TBookmarkFlag; Override;
    Procedure InternalGotoBookmark(aBookmark: TBookmark); Override;
    Procedure SetBookmarkData(aBuffer: PChar; aData: Pointer); Override;
    Procedure SetBookmarkFlag(aBuffer: PChar;
      aValue: TBookmarkFlag); Override;

    {Record Buffer allocation and disposal}
    Function AllocRecordBuffer: PChar; Override;
    Procedure FreeRecordBuffer(Var aBuffer: PChar); Override;
    Function GetRecordSize: Word; Override;
    Procedure btInitKeyBuffer(aBuf: Pointer);
    Procedure btFreeKeyBuffers;

    {Field access and update}
    Procedure ClearCalcFields(aBuffer: PChar); Override;

    Procedure InternalInitRecord(aBuffer: PChar); Override;
    Procedure SetFieldData(Field: TField; Buffer: Pointer;
      NativeFormat: Boolean); Override;
    Procedure SetFieldData(aField: TField; aBuffer: Pointer); Override;

    {Record access and update}
    Function FindRecord(aRestart, aGoForward: Boolean): Boolean; Override;
    Function GetRecNo: Integer; Override;
    Procedure SetRecNo(Value: Integer); Override;

    Procedure SetFlipOrder(Const Value: Boolean);
    Procedure SetSupportRecNo(Const Value: Boolean);

    Function GetRecord(aBuffer: PChar;
      aGetMode: TGetMode;
      aDoCheck: Boolean): TGetResult; Override;
    Procedure InternalAddRecord(aBuffer: Pointer;
      aAppend: Boolean); Override;
    Procedure InternalCancel; Override;
    Procedure InternalDelete; Override;
    Procedure InternalEdit; Override;
    Procedure InternalInsert; Override;
    Procedure InternalFirst; Override;
    Procedure InternalLast; Override;
    Procedure InternalPost; Override;
    Procedure InternalRefresh; Override;
    Procedure DoBeforeEdit; Override;
    Procedure DoBeforePost; Override;
    Procedure DoAfterOpen; Override;
    Procedure DoAfterInsert; Override;
    Procedure DoAfterPost; Override;
    Procedure InternalSetToRecord(aBuffer: PChar); Override;

    {information}
    Function GetCanModify: Boolean; Override;
    Function GetRecordCount: Integer; Override;
    Procedure InternalHandleException; Override;
    Procedure SetName(Const NewName: TComponentName); Override;

    {filtering}
    Procedure SetFiltered(Value: Boolean); Override;
    Procedure SetFilterOptions(Value: TFilterOptions); Override;
    Procedure SetFilterText(Const Value: String); Override;
    Procedure SetOnFilterRecord(Const Value: TFilterRecordEvent); Override;

    Procedure dsCloseViaProxy; Virtual;

    Property Exclusive: Boolean
      Read dsExclusive
      Write dsSetExclusive
      Default False;

    Property FieldDescs: TffCollection
      Read dsFieldDescs;

    Property FilterActive: Boolean
      Read dsFilterActive;

    Property Filters: TffCollection
      Read dsFilters;
    Property KeySize: Integer
      Read btKeyLength;

    Property OpenMode: TffOpenMode
      Read dsOpenMode;

    Property PhysicalRecordSize: Integer
      Read dsGetPhyRecSize;

    Property RecLockedBeforeEdit: Boolean
      Read dsRecLockedBeforeEdit
      Write dsRecLockedBeforeEdit;
    Property RecLockedType: TfsUserRecLocking
      Read dsRecLockedType
      Write dsRecLockedType;

    Property ReadOnly: Boolean
      Read dsReadOnly
      Write dsSetReadOnly
      Default False;

    Property ShareMode: TffShareMode
      Read dsShareMode;

    Property TableState: TfsTableState
      Read dsTableState
      Write dsTableState;

    Property XltMode: FFXltMode
      Read dsXltMode;

    Property TableName: String
      Read dsGetTableName
      Write dsSetTableName;
    // Property SQLDISPLAY: TStrings
     //  Read FSQLDISPLAY
    //   Write quSetSQLDISPLAY;
    Property BlobChunkSize: Integer
      Read fBlobChunkSize Write SetBlobChunkSize;
    Property BlobMode: TfsBlobMode
      Read FBlobMode Write SetBlobMode;
    Property BlobAutoStartTransaction: boolean
      Read fBlobAutoStartTransaction
      Write fBlobAutoStartTransaction;
    Property BlobModifiedError: boolean
      Read fBlobModifiedError
      Write fBlobModifiedError;
    Property SupportRecNo: boolean
      Read fSupportRecNo
      Write SetSupportRecNo;
    Property FlipOrder: boolean
      Read fFlipOrder
      Write SetFlipOrder;
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure Cancel; Override;
    Function GetFlagRecord: Byte;
    Function GetRefNr: TffInt64;

    Property IndexID: Word Read btIndexID;
    {---Record and key Buffer management---}
    Function GetActiveRecBuf(Var aRecBuf: PChar; NewBuffer: PChar = Nil): Boolean; Virtual;
    //
    Function FieldRound(Name: String): TRound;
    //Function AddFileBlob(Const aField: Word;
    //  Const aFileName: TffFullFileName): TffResult;
    Function BookmarkValid(aBookmark: TBookmark): Boolean; Override;
    Function CompareBookmarks(Bookmark1,
      Bookmark2: TBookmark): Integer; Override;
    Procedure CopyRecords(aSrcTable: TFSDataSet; aCopyBLOBs: Boolean; CountPerTrans: Longint); {1.052}
    Function CreateBlobStream(aField: TField; aMode: TBlobStreamMode): TStream; Override;
    Function FsCreateBlobStream(aField: TField; aMode: TBlobStreamMode): TfsStream;

    Procedure DeleteTable;
    Procedure EmptyTable;
    Function GetCurrentRecord(aBuffer: PChar): Boolean; Override;
    Function GetFieldData(Field: TField; Buffer: Pointer;
      NativeFormat: Boolean): Boolean; Override;
    Function GetFieldData(aField: TField;
      aBuffer: Pointer): Boolean; Override;
    Function GetRecordBatch(
      RequestCount: Longint;
      Var ReturnCount: Longint;
      pRecBuff: Pointer): TffResult;
    Function GetRecordBatchEx(
      RequestCount: Longint;
      Var ReturnCount: Longint;
      pRecBuff: Pointer;
      Var Error: TffResult): TffResult;
    Procedure GotoCurrent(aDataSet: TFSDataSet);
    Procedure fsGetRecord;
    Function InsertRecordBatch(
      Count: Longint;
      pRecBuff: Pointer;
      Errors: PffLongintArray): TffResult;
    Procedure Loaded; Override;

    Function OverrideFilterEx(aExprTree: fsSrBDE.pCANExpr;
      Const aTimeout: TffWord32): TffResult;
    Function PackTable(Var aTaskID: Longint; UndeleteRecords: boolean; OnlyDeleted: boolean): TffResult;
    Procedure RecordCountAsync(Var TaskID: Longint); {!!.07}
    Procedure RenameTable(Const aNewTableName: String);
    Function RestoreFilterEx: TffResult;
    Function RestructureTable(aDictionary: TFSInfoDict;
      aFieldMap: TStrings;
      Var aTaskID: Longint;
      aRangeError: boolean): TffResult;
    Function SetFilterEx(aExprTree: fsSrBDE.pCANExpr;
      Const aTimeout: TffWord32): TffResult;
    Function GetTableAutoIncValue(Var aValue: Int64; Var aStep: Longint): TffResult;
    Function SetTableAutoIncValue(Const aValue: Int64; Const aStep: Longint): TffResult;
    Function ReadLastAutoInc: Int64;
    Function GetTableMaxRecordsValue(Var aValue: Longword): TffResult;
    Function SetTableMaxRecordsValue(Const aValue: Longword): TffResult;
    Function GetTableFlagsValue(Var aValue: Word): TffResult;
    Function SetTableFlagsValue(Const aValue: Word): TffResult;

    Function GetTablePassword(Var aValue: Longword): TffResult;
    Function SetTablePassword(aOldValue, aNewValue: String): TffResult;
    Function GetTablePasswordRest(Var aValue: Longword): TffResult;
    Function SetTablePasswordRest(aOldValue, aNewValue: String): TffResult;

    Function GetTableDBIDValue(Var aValue: Longword): TffResult;
    //Function SetTableDBIDValue(Const aValue: Longword): TffResult;

    Function Exists: Boolean;

    Procedure LockTable(LockType: TffLockType);
    Procedure UnlockTable(LockType: TffLockType);
    Procedure UnlockTableAll;
    Function TableIsLocked(LockType: TffLockType): boolean;

    Procedure LockRecord;
    Procedure UnLockRecord;
    Procedure UnLockRecordAll;
    Function RecordIsLocked: boolean;

    Procedure UpdateFormatFields;

    Function IsSequenced: Boolean; Override;

    Property InRange: Boolean Read fInRange Write fInRange;

    Property Session: TFSSession
      Read dsGetSession;

    Property CursorID: TffCursorID
      Read dsCursorID;

    Property Database: TFSBaseDatabase
      Read dsGetDatabase;

    Property Dictionary: TFSInfoDict
      Read dsDictionary
      Write dsDictionary;

    Property ServerEngine: TFSBaseServerEngine
      Read dsGetServerEngine;

    Property DataBaseName: String
      Read dsGetDatabaseName
      Write dsSetDatabaseName;

    Property FilterEval: TffFilterEvaluationType
      Read dsFilterEval
      Write dsSetFilterEval;
    { This property determines where the filter is evaluated.  For best
      performance, evaluate the filter on the server by setting this
      property to fseserver.  Otherwise, setting this property to
      ffeLocal causes the filter to be evaluated on the client. }

    Property FilterResync: Boolean
      Read dsFilterResync
      Write dsFilterResync
      Default True;
    { When this property is set to True, changing the Filter or the
      FilterEval properties causes the server to refresh the dataset.
      Set this property to False when you don't want the server to
      refresh the dataset.  For example, if you have created a cache
      table that inherits from TFSTable and the cache table must set to
      the beginning of the dataset anyway, set this property to False
      so that the server does not filter the dataset twice. }

    Property FilterTimeout: TffWord32
      Read dsFilterTimeOut
      Write dsSetFilterTimeOut
      Default 500;
    { When retrieving a filtered dataset from the server, the
      number of milliseconds in which the server has to
      respond.  If the server does not respond within the
      specified milliseconds, the OnServerFilterTimeout event
      is raised. }

    Property ListBlobCache: TObjectList
      Read FListBlobCache
      Write FListBlobCache;
    Property OnServerFilterTimeout: TfsServerFilterTimeoutEvent
      Read dsOnServerFilterTimeout
      Write dsOnServerFilterTimeout;

    Property SessionName: String
      Read dsGetSessionName
      Write dsSetSessionName;

    Property Timeout: Longint
      Read dsTimeout
      Write dsSetTimeout
      Default -1;
    Property CheckTimeout: Longint Read dsCheckTimeout Write dsSetCheckTimeout;
    Property DeleteTimeout: Longint Read dsDeleteTimeout Write dsSetDeleteTimeout;
    { Timeout specified in milliseconds }

    Property Version: String
      Read dsGetVersion
      Write dsSetVersion
      Stored False;

    { The following properties will be published by descendent classes,
      they are included here to reduce duplicity of documentation }
    Property BeforeOpen;
    Property AfterOpen;
    Property BeforeClose;
    Property AfterClose;
    Property BeforeInsert;
    Property AfterInsert;
    Property BeforeEdit;
    Property AfterEdit;
    Property BeforePost;
    Property AfterPost;
    Property BeforeCancel;
    Property AfterCancel;
    Property BeforeDelete;
    Property AfterDelete;
    Property BeforeScroll;
    Property AfterScroll;
    {$IFDEF DCC5OrLater}
    Property BeforeRefresh;
    Property AfterRefresh;
    {$ENDIF}
    Property OnCalcFields;
    Property OnDeleteError;
    Property OnEditError;
    Property OnFilterRecord;
    Property OnNewRecord;
    Property OnPostError;
  End;

  TFSBaseTable = Class(TFSDataSet)
  Protected {private}
    btLookupCursorID: TffCursorID; {lookup cursor}
    btLookupIndexID: Integer; {lookup index ID}
    btLookupIndexName: String; {lookup index name}
    btLookupKeyFields: String; {key fields for lookup cursor}
    btLookupNoCase: Boolean; {case insens. lookup cursor}
    btMasterLink: TMasterDataLink; {link to the master table}
    btNoCaseIndex: Boolean; {True=case insensitive index}
    btRangeStack: TfsTableRangeStack;
  Protected
    {---Property access methods---}
    Procedure btSetIndexName(Const aValue: String);
    Function btGetIndexName: String;

    Function btGetFFVersion: Integer;
    Function btGetIndexField(aInx: Integer): TField;
    Function btGetIndexFieldNames: String;
    Function btGetKeyExclusive: Boolean;
    Function btGetKeyFieldCount: Integer;
    Function btGetMasterFields: String;
    Function btGetMasterSource: TDataSource;
    Procedure btSetKeyExclusive(Const aValue: Boolean);
    Procedure btSetKeyFieldCount(Const aValue: Integer);
    Procedure btSetIndexField(aInx: Integer; Const aValue: TField);
    Procedure btSetIndexFieldNames(Const aValue: String);
    Procedure btSetMasterFields(Const aValue: String);
    Procedure btSetMasterSource(Const aValue: TDataSource);
    Procedure dsSetTableName(Const aValue: String); Override;
    Procedure btSetIndexDefs(Value: TIndexDefs); {!!.06}
    Function btIndexDefsStored: Boolean; {!!.06}

    {---Record and key Buffer management---}
    Procedure btEndKeyBufferEdit(aCommit: Boolean);

    Function btGetRecordForKey(aCursorID: TffCursorID;
      bDirectKey: Boolean;
      iFields: Word;
      iLen: Word;
      pKey: Pointer;
      pRecBuff: Pointer
      ): TffResult;

    Procedure btSetKeyBuffer(aInx: TfsKeyEditType; aMustClear: Boolean);
    Procedure btSetKeyFields(aInx: TfsKeyEditType;
      Const aValues: Array Of Const);

    {---Record access---}
    Function btLocateRecord(Const aKeyFields: String;
      Const aKeyValues: Variant;
      aOptions: TLocateOptions;
      aSyncCursor: Boolean): Boolean;
    Function GetCursorProps(Var aProps: TfsCursorProperties): TffResult; Override;

    {---Field management---}
    Function btDoFldsMapToCurIdx(aFields: TList;
      aNoCase: Boolean): Boolean;

    {---Index and key management---}
    Procedure btDecodeIndexDesc(Const aIndexDesc: IDXDesc;
      Var aName, aFields: String;
      Var aOptions: TIndexOptions);
    Procedure btDestroyLookupCursor;
    Procedure dsGetIndexInfo; Override;

    Function btGetLookupCursor(Const aKeyFields: String;
      aNoCase: Boolean): TffCursorID;
    Function btResetRange(aCursorID: TffCursorID;
      SwallowSeqAccessError: Boolean): Boolean; Virtual;
    Procedure btResetRangePrim(aCursorID: TffCursorID;
      SwallowSeqAccessError: Boolean);
    Procedure btSetIndexTo(Const aParam: String; aIndexByName: Boolean);
    Function btSetRange: Boolean;
    Function btSetRangePrim(aCursorID: TffCursorID;
      bKeyItself: Boolean;
      iFields1: Word;
      iLen1: Word;
      pKey1: Pointer;
      bKey1Incl: Boolean;
      iFields2: Word;
      iLen2: Word;
      pKey2: Pointer;
      bKey2Incl: Boolean): TffResult;
    Procedure btSwitchToIndex(Const aIndexName: String);
    Function btSwitchToIndexEx(aCursorID: TffCursorID;
      Const aIndexName: String;
      Const aIndexID: Integer;
      Const aCurrRec: Boolean): TffResult;

    {---Modes---}
    Procedure btCheckKeyEditMode;

    {---Master/detail stuff---}
    Procedure dsCheckMasterRange; Override;
    Procedure btMasterChanged(Sender: TObject);
    Procedure btMasterDisabled(Sender: TObject);
    Procedure btSetLinkRange(aMasterFields: TList);

    {---Handle stuff---}
    Procedure btChangeHandleIndex;
    Procedure DestroyHandle(aHandle: TffCursorID); Override;
    Function GetCursorHandle(aIndexName: String): TffCursorID; Override;

    {---TDataSet method overrides---}
    {Opening, initializing and closing}
    Procedure InternalClose; Override;
    Procedure InternalOpen; Override;
    {information}
    Procedure DataEvent(aEvent: TDataEvent; aInfo: Longint); Override;

    Function GetIsIndexField(Field: TField): Boolean; Override;

    {Record access and update}
    Procedure DoOnNewRecord; Override;

    {field access and update}
    Procedure SetFieldData(aField: TField; aBuffer: Pointer); Override;

    {filtering}
    Procedure SetFiltered(Value: Boolean); Override;
    Procedure dsActivateFilters; Override; {!!.03}
    Procedure dsDeactivateFilters; Override; {!!.03}

    {indexes - such that they exist at TDataSet level}
    Procedure UpdateIndexDefs; Override;

    {$IFDEF ProvidesDatasource}
    Function GetDataSource: TDataSource; Override;
    {$ENDIF}

    Property IndexDefs: TIndexDefs
      Read btIndexDefs
      Write btSetIndexDefs {!!.06}
    Stored btIndexDefsStored; {!!.06}

    Property IndexFields[aIndex: Integer]: TField
    Read btGetIndexField
      Write btSetIndexField;

    Property IndexFieldCount: Integer
      Read btIndexFieldCount;
    Property IndexName: String
      Read btGetIndexName
      Write btSetIndexName;
    Property KeyExclusive: Boolean
      Read btGetKeyExclusive
      Write btSetKeyExclusive;

    Property KeyFieldCount: Integer
      Read btGetKeyFieldCount
      Write btSetKeyFieldCount;

    Property IndexFieldNames: String
      Read btGetIndexFieldNames
      Write btSetIndexFieldNames;

    Property MasterFields: String
      Read btGetMasterFields
      Write btSetMasterFields;

    Property MasterSource: TDataSource
      Read btGetMasterSource
      Write btSetMasterSource;

    {Begin !!.11}
    Property TableVersion: Integer
      Read btGetFFVersion;
    {End !!.11}
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;

    // for modify flag record frProtectDeleteRecord (16), frProtectUpdateRecord (32)
    Procedure SetFlagRecord(aFlag: Byte; aSet: Boolean);

    Function Undelete(aRefresh: boolean): Boolean;
    Procedure AddIndex(Const aName, aFields: String;
      aOptions: TIndexOptions);
    Function AddIndexEx(Const aIndexDesc: TffIndexDescriptor;
      Var aTaskID: Longint): TffResult;
    Procedure ApplyRange;
    Procedure Cancel; Override;
    Procedure CancelRange;
    //    procedure CopyRecords(aSrcTable : TFSTable; aCopyBLOBs : Boolean);
    Procedure CreateTable;
    Procedure CreateTableEx(Const aBlockSize: Integer);
    Procedure DeleteIndex(Const aIndexName: String);
    Procedure DeleteRecords(CountPerTrans: Longint);
    Procedure EditKey;
    Procedure EditRangeEnd;
    Procedure EditRangeStart;
    Function FindKey(Const aKeyValues: Array Of Const): Boolean;
    Procedure FindNearest(Const aKeyValues: Array Of Const);
    Procedure GetIndexNames(aList: TStrings);
    Function GotoKey: Boolean;
    Procedure GotoNearest;
    Function Locate(Const aKeyFields: String;
      Const aKeyValues: Variant;
      aOptions: TLocateOptions): Boolean; Override;
    Function Lookup(Const aKeyFields: String;
      Const aKeyValues: Variant;
      Const aResultFields: String): Variant; Override;
    Procedure Post; Override;
    Function ReIndexTable(Const aIndexNum: Integer;
      Var aTaskID: Longint): TffResult;
    Procedure SetKey;
    Procedure SetRange(Const aStartValues, aEndValues: Array Of Const);
    Procedure SetRangeEnd;
    Procedure SetRangeStart;
  End;

  TFSBaseDatabase = Class(TfsDBListItem)
  Protected {private}
    bdAutoDBName: Boolean;
    bdDatabaseID: TffDatabaseID;
    bdExclusive: Boolean;
    bdFailSafe: Boolean;
    bdReadOnly: Boolean;
    bdServerEngine: TFSBaseServerEngine;
    bdTransIsolation: TfsTransIsolation; // = (tiRepeatableRead);//, tiSerializable);
    bdRecLocking: TfsDataBaseRecLocking;
    bdTimeout: Longint;
  Protected
    Function bdGetDataSet(aInx: Integer): TFSDataSet;
    Function bdGetDataSetCount: Integer;
    Function bdGetDatabaseID: TffDatabaseID;
    Function bdGetSession: TFSSession;
    Function bdGetServerEngine: TFSBaseServerEngine;
    Procedure bdRefreshTimeout; {!!.11}
    Procedure bdSetAutoDBName(Const Value: Boolean);
    Procedure bdSetDatabaseName(Const aName: String);
    Procedure bdSetExclusive(aValue: Boolean);
    Procedure bdSetReadOnly(aValue: Boolean);
    Procedure bdSetTimeout(Const Value: Longint);

    Function dbliCreateOwnedList: TfsDBList; Override;
    Function dbliFindDBOwner(Const aName: String): TfsDBListItem; Override;
    Procedure bdInformTablesAboutDestruction;
    Procedure dbliMustBeClosedError; Override;
    Procedure dbliMustBeOpenError; Override;
    Procedure dbliOpenPrim; Override;

    Property AutoDatabaseName: Boolean
      Read bdAutoDBName
      Write bdSetAutoDBName
      Default False;

    Property DatabaseID: TffDatabaseID
      Read bdGetDatabaseID;

    Property DataSetCount: Integer
      Read bdGetDataSetCount;

    Property DataSets[aInx: Integer]: TFSDataSet
    Read bdGetDataSet;

    Property ServerEngine: TFSBaseServerEngine
      Read bdGetServerEngine;

    Property Session: TFSSession
      Read bdGetSession;
    Property Connected;

    Property DataBaseName: String
      Read dbliDBName
      Write bdSetDatabaseName;

    Property Exclusive: Boolean
      Read bdExclusive
      Write bdSetExclusive
      Default False;

    Property ReadOnly: Boolean
      Read bdReadOnly
      Write bdSetReadOnly
      Default False;

    Property SessionName: String
      Read dbliGetDBOwnerName
      Write dbliSetDBOwnerName;

    Property Timeout: Longint
      Read bdTimeout
      Write bdSetTimeout
      Default -1;
    { Timeout specified in milliseconds }
    Property TransIsolation: TfsTransIsolation Read bdTransIsolation Write bdTransIsolation;

  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;

    Function GetFreeDiskSpace(Var aFreeSpace: Int64): TffResult;
    Function GetTimeout: Longint;
    Procedure CloseDataSets;
    Function IsSQLBased: Boolean;
    Function PackTable(Const aTableName: TfsTableName;
      Var aTaskID: Longint; UndeleteRecords: Boolean; OnlyDeleted: boolean): TffResult;
    Procedure Commit;
    Function ReIndexTable(Const aTableName: TfsTableName;
      Const aIndexNum: Integer;
      Var aTaskID: Longint): TffResult;
    Procedure Rollback;
    Function GetInTransaction: boolean;
    Procedure StartTransaction;
    Function StartTransactionWith(Const aTables: Array Of TFSBaseTable): TffResult; {!!.10}
    { Start a transaction, but only if an exclusive lock is obtained
      for the specified tables. }
    Function TryStartTransaction: Boolean;
    Procedure TransactionCorrupted;
    Function TableExists(Const aTableName: TfsTableName): Boolean;

    {---Miscellaneous---}
    Function GetFFDataDictionary({ return a FlashFiler DD}
      Const TableName: TfsTableName;
      Stream: TStream
      ): TffResult;

    Property FailSafe: Boolean
      Read bdFailSafe
      Write bdFailSafe
      Default False;

    Property InTransaction: Boolean
      Read GetInTransaction;
    Property RecLocking: TfsDataBaseRecLocking
      Read bdRecLocking
      Write bdRecLocking;

  End;

  TFSDatabase = Class(TFSBaseDatabase)
  Protected {private}
    dcAliasName: String;
  Protected
    Procedure dcSetAliasName(Const aName: String);

    Procedure dbliClosePrim; Override;
    Procedure dbliOpenPrim; Override;
  Public
    Function CreateTable(Const aOverWrite: Boolean;
      Const aTableName: TfsTableName;
      aDictionary: TFSInfoDict): TffResult;

    Procedure GetTableNames(aList: TStrings);

    Function RestructureTable(Const aTableName: TfsTableName;
      aDictionary: TFSInfoDict;
      aFieldMap: TStrings;
      Var aTaskID: Longint;
      aRangeError: boolean): TffResult;

    Property DatabaseID;
    Property DataSetCount;
    Property DataSets;
    Property ServerEngine;
    Property Session;
    Property Temporary;
  Published
    Property AliasName: String
      Read dcAliasName
      Write dcSetAliasName;

    Property AutoDatabaseName;
    Property Connected;
    Property DataBaseName;
    Property Exclusive;
    Property FailSafe;
    Property ReadOnly;
    Property SessionName;
    Property Timeout;
    //Property TransIsolation;
    Property RecLocking;
  End;

  TFSDatabaseList = Class(TfsDBList)
  Protected {private}
    Function dlGetItem(aInx: Integer): TFSBaseDatabase;
  Public
    Property Databases[aInx: Integer]: TFSBaseDatabase
    Read dlGetItem; Default;
  End;

  TFSTableProxy = Class(TfsDBListItem)
  Protected {private}
    tpClosing: Boolean;
    tpCursorID: TffCursorID;
    tpDBGone: Boolean;
    tpffTable: TFSDataSet;
    tpServerEngine: TFSBaseServerEngine;
    tpSession: TFSSession;
    tpSessionName: String;

  Protected
    Function tpGetCursorID: TffCursorID;
    Function tpGetDatabase: TFSBaseDatabase;
    Function tpGetSession: TFSSession;
    Function tpGetSessionName: String;
    Function tpGetServerEngine: TFSBaseServerEngine;
    Procedure tpSetSessionName(aValue: String);

    Procedure dbliClosePrim; Override;
    Function dbliFindDBOwner(Const aName: String): TfsDBListItem; Override;
    Procedure dbliLoaded; Override;
    Procedure dbliMustBeClosedError; Override;
    Procedure dbliMustBeOpenError; Override;
    Procedure dbliOpenPrim; Override;
    Procedure dbliDBOwnerChanged; Override;

    Procedure tpDatabaseIsDestroyed;
    Procedure tpResolveSession;

    Property fsTable: TFSDataSet
      Read tpffTable
      Write tpffTable;
  Public
    Constructor Create(aOwner: TComponent); Override;

    Property CursorID: TffCursorID
      Read tpGetCursorID;

    Property Database: TFSBaseDatabase
      Read tpGetDatabase;

    Property Session: TFSSession
      Read tpGetSession;

    Property Active;

    Property DataBaseName: String
      Read dbliGetDBOwnerName
      Write dbliSetDBOwnerName;

    Property SessionName: String
      Read tpGetSessionName
      Write tpSetSessionName;

    Property ServerEngine: TFSBaseServerEngine
      Read tpGetServerEngine;

    Property TableName: String
      Read dbliDBName
      Write dbliSetDBName;
  End;

  TFSTableProxyList = Class(TfsDBList)
  Protected {private}
    Procedure dblFreeItem(aItem: TfsDBListItem); Override;
    Function tlGetItem(aInx: Integer): TFSTableProxy;
  Public
    Property Tables[aInx: Integer]: TFSTableProxy
    Read tlGetItem; Default;
  End;

  TFSTable = Class(TFSBaseTable)
  Public
    Property CursorID;
    Property Database;
    Property Dictionary;
    Property TableVersion; {!!.11}
    {$IFDEF Delphi3} {!!.01}
    Property IndexDefs;
    {$ENDIF} {!!.01}
    Property IndexFields;
    Property IndexFieldCount;
    Property KeyExclusive;
    Property KeyFieldCount;
    Property KeySize;
  Published
    Property Active;
    Property AutoCalcFields;
    Property BlobAutoStartTransaction;
    Property BlobModifiedError;
    Property BlobMode;
    Property CheckTimeout;
    Property DeleteTimeout;
    Property DataBaseName;
    Property Exclusive;
    Property RecLockedBeforeEdit;
    Property RecLockedType;
    {Begin !!.01}
    {$IFDEF CBuilder3}
    Property FieldDefs;
    {$ENDIF}
    {$IFDEF Dcc4orLater}
    Property FieldDefs;
    {$ENDIF}
    {End !!.01}
    Property Filter;
    Property Filtered;
    Property FilterEval;
    Property FilterOptions;
    Property FilterResync;
    Property FilterTimeout;
    Property FlipOrder;
    {Begin !!.01}
    {$IFDEF CBuilder3}
    Property IndexDefs;
    {$ENDIF}
    {$IFDEF Dcc4orLater}
    Property IndexDefs;
    {$ENDIF}
    {End !!.01}
    Property IndexFieldNames;
    Property IndexName;
    Property MasterFields;
    Property MasterSource;
    Property BlobChunkSize;
    Property ReadOnly;
    Property SessionName;
    Property SupportRecNo;
    Property TableName;
    Property Timeout;
    Property Version;

    Property BeforeOpen;
    Property AfterOpen;
    Property BeforeClose;
    Property AfterClose;
    Property BeforeInsert;
    Property AfterInsert;
    Property BeforeEdit;
    Property AfterEdit;
    Property BeforePost;
    Property AfterPost;
    Property BeforeCancel;
    Property AfterCancel;
    Property BeforeDelete;
    Property AfterDelete;
    Property BeforeScroll;
    Property AfterScroll;
    {$IFDEF DCC5OrLater}
    Property BeforeRefresh;
    Property AfterRefresh;
    {$ENDIF}
    Property OnCalcFields;
    Property OnDeleteError;
    Property OnEditError;
    Property OnFilterRecord;
    Property OnNewRecord;
    Property OnPostError;
    Property OnServerFilterTimeout;
  End;

  TfsStream = Class(TStream)
  Private
    bsChunkSize: Longint;
    Function GetSize: Longint; Reintroduce; Virtual;

  Public
    Constructor Create(aChunkSize: Longint);
    Destructor Destroy; Override;
    Function CopyFrom(Source: TStream; Count: Longint): Longint; Virtual;
    Procedure SaveToStream(Stream: TStream); Virtual;
    Procedure SaveToFile(Const FileName: String); Virtual;
    Procedure LoadFromStream(Stream: TStream); Virtual;
    Procedure LoadFromFile(Const FileName: String); Virtual;
    Procedure Truncate; Virtual;
    Procedure Clear; Virtual;
    Property ChunkSize: Longint Read bsChunkSize Write bsChunkSize;
  End;

  TfsMemoryStream = Class(TfsStream)
  Private
    //bsChunkSize: Longint;
  Private
    FMemory: Pointer;
    FSize, FPosition: Longint;
    FCapacity: Longint;
    Procedure SetCapacity(NewCapacity: Longint);
  Protected
    Procedure SetPointer(Ptr: Pointer; Size: Longint);
    Function Realloc(Var NewCapacity: Longint): Pointer; Virtual;
    Property Capacity: Longint Read FCapacity Write SetCapacity;
  Public
    Constructor Create(aChunkSize: Longint);
    Destructor Destroy; Override;
    Procedure Truncate; Override;
    Function Read(Var Buffer; Count: Longint): Longint; Override;
    Function Seek(Offset: Longint; Origin: Word): Longint; Override;
    Procedure SaveToStream(Stream: TStream); Override;
    Procedure SaveToFile(Const FileName: String); Override;
    Procedure Clear; Override;
    Procedure LoadFromStream(Stream: TStream); Reintroduce;
    Procedure LoadFromFile(Const FileName: String); Reintroduce;
    Procedure SetSize(NewSize: Longint); Override;
    Function Write(Const Buffer; Count: Longint): Longint; Override;
    Property Memory: Pointer Read FMemory;
    //Property ChunkSize: Longint Read bsChunkSize Write bsChunkSize;
  End;

  TfsDirectBlobStream = Class(TfsStream)
  Private
    bsRecBuf: PChar;
    bsTable: TfsDataSet;
    bsField: TBlobField;
    bsBLOBNr: TffInt64;
    bsFieldNo: Integer;
    bsMode: TBlobStreamMode;
    bsModified: Boolean;
    bsOpened: Boolean;
    bsPosition: Longint;
    bsCancel: Boolean;
    Function GetSize: Longint; Override;
  Protected
    Function bsGetBlobHandleSize: Longint;
  Public
    Constructor Create(aField: TBlobField; aMode: TBlobStreamMode; aChunkSize: Longint);
    Destructor Destroy; Override;

    Function Read(Var aBuffer; aCount: Longint): Longint; Override;
    Function Write(Const aBuffer; aCount: Longint): Longint; Override;
    Function Seek(aoffset: Longint; aOrigin: Word): Longint; Override;
    Procedure Truncate; Override;

    Property CancelTransfer: Boolean Write bsCancel;
  End;

  TfsMemoryBlobStream = Class(TfsMemoryStream)
  Private
    bsRecBuf: PChar;
    bsTable: TFSDataSet;
    bsField: TBlobField;
    bsFieldNo: Integer;
    bsBLOBNr: TffInt64;
    bsMode: TBlobStreamMode;
    bsModified: Boolean;
    bsOpened: Boolean;
    bsPosition: Longint;
    bsCancel: Boolean;
  Protected
    Function bsGetBlobHandleSize: Longint;
  Public
    Constructor Create(aField: TBlobField; aMode: TBlobStreamMode; aChunkSize: Longint);
    Destructor Destroy; Override;
    Procedure Clear; Override;
    Procedure SetSize(NewSize: Longint); Override;
    Function Write(Const Buffer; Count: Longint): Longint; Override;
    Procedure Truncate; Override;

    Function WriteToBlob(Const aBuffer; aCount: Longint): Longint;
    Function ReadBlobToStream(bsFieldNo: Integer): Longint;

    Property CancelTransfer: Boolean Write bsCancel;
  End;

  TfsListBlobCacheObject = Class;
  TfsCacheBlobStream = Class(TfsStream)
  Protected
    fModified: boolean;
    fListBlobCacheObject: TfsListBlobCacheObject;
  Public
    Constructor Create(aChunkSize: Longint);
    Destructor Destroy; Override;

    Function Read(Var Buffer; Count: Longint): Longint; Override;
    Function Write(Const Buffer; Count: Longint): Longint; Override;
    Function Seek(Offset: Longint; Origin: Word): Longint; Override;
    Procedure Clear; Override;
    Procedure Truncate; Override;
  End;

  TfsListBlobCacheObject = Class
  Private
    fDirty: boolean;
    fBlobCount: Integer;
    Field: TField;
    BlobData: TfsMemoryStream;
  Public
    Constructor Create(aChunkSize: Longint);
    Destructor Destroy; Override;
  End;

  TFSQuery = Class; { forward declaration }

  {$IFDEF DCC4OrLater}
  TFSQueryDataLink = Class(TDetailDataLink)
    {$ELSE}
  TFSQueryDataLink = Class(TDataLink)
    {$ENDIF}
  Protected {private}
    FQuery: TFSQuery;
  Protected
    Procedure ActiveChanged; Override;
    Procedure RecordChanged(Field: TField); Override;
    {$IFDEF DCC4OrLater}
    Function GetDetailDataSet: TDataSet; Override;
    {$ENDIF}
    Procedure CheckBrowseMode; Override;
  Public
    Constructor Create(aQuery: TFSQuery);
  End;

  TFSQuery = Class(TFSDataSet)
  Protected {private}
    FCanModify: Boolean; {!!.10}
    FDataLink: TDataLink;
    FExecuted: boolean;
    { Set to True if statement has been executed. }
    FParamCheck: boolean;
    FParams: TfsParams;
    FPrepared: boolean;
    FRequestLive: boolean;
    FRowsAffected: Integer; {!!.10}
    FRecordsRead: Integer; {!!.10}
    FSQL: TStrings;
    FSQLORDER: TStrings;
    FStmtID: TffSqlStmtID;
    FText: String;
    OnlineCursor: Boolean;
    Procedure InternalOpen; Override;

    {$IFDEF DCC4OrLater}
    Procedure DefineProperties(Filer: TFiler); Override;
    {$ENDIF}
    Procedure DestroyHandle(aHandle: TffCursorID); Override;
    Procedure dsCloseViaProxy; Override;
    Function dsGetServerEngine: TFSBaseServerEngine; Override;
    Function GetCanModify: Boolean; Override;
    Function GetCursorHandle(aIndexName: String): TffCursorID; Override;
    Function GetCursorProps(Var aProps: TfsCursorProperties): TffResult; Override;
    Procedure InternalClose; Override;
    Procedure quBuildParams(Var ParamsList: PfsSqlParamInfoList;
      Var ParamsData: PffByteArray;
      Var ParamsDataLen: Integer);
    {-Constructs the parameter data sent to the server. }
    Procedure quDisconnect;
    Procedure quExecSQLStmt(Const aOpenMode: TffOpenMode;
      Var aCursorID: TffCursorID);
    Procedure quFreeStmt;
    Function quGetDataSource: TDataSource;
    Function quGetParamCount: Word;
    Function quGetRowsAffected: Integer; {!!.10}
    {Begin !!.01}
    Function quLocateRecord(Const aKeyFields: String;
      Const aKeyValues: Variant;
      aOptions: TLocateOptions;
      aSyncCursor: Boolean): Boolean;
    {End !!.01}
    Function quParseSQL(aStmt: String; createParams: boolean;
      aParams: TfsParams): String;
    Procedure quPreparePrim(prepare: boolean);
    {$IFDEF DCC4OrLater}
    Procedure quReadParams(Reader: TReader);
    {$ENDIF}
    Procedure quRefreshParams;
    Procedure quSetDataSource(aSrc: TDataSource);
    Procedure quSetParams(aParamList: TfsParams);
    Procedure quSetParamsFromCursor;
    Procedure quSetPrepared(aFlag: boolean);
    Procedure quSetRequestLive(aFlag: boolean);
    Procedure quSetSQL(aValue: TStrings);
    Procedure quSetSQLORDER(aValue: TStrings);

    Procedure quSQLChanged(Sender: TObject);
    Procedure quSQLORDERChanged(Sender: TObject);
    {-Called when the SQL property changes.  Allows us to update the
      Params property. }
    {$IFDEF DCC4OrLater}
    Procedure quWriteParams(Writer: TWriter);
    {$ENDIF}

    Property DataLink: TDataLink
      Read FDataLink;

  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure ExecSQL; {!!.10}
    {Begin !!.01}
    Function Locate(Const aKeyFields: String;
      Const aKeyValues: Variant;
      aOptions: TLocateOptions): Boolean; Override;
    {End !!.01}
    Function Lookup(Const aKeyFields: String;
      Const aKeyValues: Variant;
      Const aResultFields: String): Variant; Override;

    Function ParamByName(Const aName: String): TfsParam;
    Procedure Prepare;
    Procedure Unprepare;
    Property Prepared: boolean
      Read FPrepared
      Write quSetPrepared;
    Property RowsAffected: Integer {!!.10}
    Read quGetRowsAffected;
    Property RecordsRead: Integer Read FRecordsRead; {!!.10}
    Property Text: String
      Read FText;
    Property StmtHandle: TffSqlStmtID
      Read FStmtID;
  Published
    Property Active;
    Property AutoCalcFields;
    Property BlobAutoStartTransaction;
    Property BlobChunkSize;
    Property BlobModifiedError;
    Property BlobMode;
    Property DataBaseName;
    Property DataSource: TDataSource
      Read quGetDataSource
      Write quSetDataSource;
    //Property UserLockType;
    Property Filter;
    Property Filtered;
    Property FilterEval;
    Property FilterOptions;
    Property FilterResync;
    Property FilterTimeout;
    Property FlipOrder;
    Property ParamCheck: boolean
      Read FParamCheck
      Write FParamCheck
      Default True;
    Property ParamCount: Word
      Read quGetParamCount;
    Property Params: TfsParams
      Read FParams
      Write quSetParams
      Stored False;
    Property RequestLive: boolean
      Read FRequestLive
      Write quSetRequestLive
      Default False;
    Property SessionName;
    Property SQL: TStrings
      Read FSQL
      Write quSetSQL;
    Property SQLOrder: TStrings
      Read FSQLORDER
      Write quSetSQLORDER;
    //Property SQLDisplay;
    Property SupportRecNo;
    Property Timeout;
    Property Version;

    { Events }
    Property BeforeOpen;
    Property AfterOpen;
    Property BeforeClose;
    Property AfterClose;
    Property BeforeInsert;
    Property AfterInsert;
    Property BeforeEdit;
    Property AfterEdit;
    Property BeforePost;
    Property AfterPost;
    Property BeforeCancel;
    Property AfterCancel;
    Property BeforeDelete;
    Property AfterDelete;
    Property BeforeScroll;
    Property AfterScroll;
    {$IFDEF DCC5OrLater}
    Property BeforeRefresh;
    Property AfterRefresh;
    {$ENDIF}
    Property OnCalcFields;
    Property OnDeleteError;
    Property OnEditError;
    Property OnFilterRecord;
    Property OnNewRecord;
    Property OnPostError;
    Property OnServerFilterTimeout;
  End;

  {---Helper routines---}

Function FindFSClientName(Const aName: String): TFSClient;
{ Find a client by name}

Function FindFSSessionName(Const aName: String): TFSSession;
{ Find a session object by name }

Function FindFSDatabaseName(aSession: TFSSession;
  Const aName: String;
  Const aCreate: Boolean): TFSBaseDatabase;
{ Find a database object by name}

Procedure GetFSClientNames(aList: TStrings);
{ Populate a list with the names of all TFSClient instances}

Procedure GetFSSessionNames(aList: TStrings);
{ Populate a list with the names of all TFSSession instances}

Procedure GetFSDatabaseNames(aSession: TFSSession; aList: TStrings);

{ Populate a list with all TFSBaseDatabase instances }

{---Global variables---}
Var
  fsClients: TFSClientList;

Implementation

{Notes: A record Buffer is in the following format
           - physical record Buffer
               (offset 0, length RecordSize)
           - calculated fields Buffer
               (offset dsCalcFldOfs, length CalcFieldSize)
           - bookmark data
               (offset dsBookmarkOfs, length BookmarkSize)
           - TfsDataSetRecInfo data
               (offset dsRecInfoOfs, length sizeof(TfsDataSetRecInfo))
        A key Buffer is in the following format
           - physical record Buffer
               (offset 0, length RecordSize)
           - TfsKeyRecInfo data
               (offset btKeyInfoOfs, length sizeof(TfsKeyRecInfo))
        TDataSet maintains an array of record Buffers.
        TFSTable maintains an array of key Buffers, one for each of
          the TfsKeyEditType enum values}

Uses
  Forms,
  TypInfo,
  fsconst,
  fsllexcp,
  fsclconv,
  fsclintf,
  {$IFDEF AutoLog} {!!.01}
  fslllog, {!!.01}
  {$ENDIF} {!!.01}
  Dialogs,
  fsutil,
  dbconsts;

Const

  {$IFDEF IsVerDataset1} // d5
  fsFldTypeMap: TfsFieldMap = (
    fldUNKNOWN, fldShortString, fldINT16, fldINT32, fldWord16, fldBOOLEAN,
    fldDouble, fldCURRENCY, fldBCD, fldDATE, fldTIME, fldDateTime, fldByteArray,
    fldUNKNOWN, fldAUTOINC32, fldBLOB, fldBLOBMEMO, fldBLOBGRAPHIC, fldBLOB, fldBLOB,
    fldBLOB, fldBLOB, fldUNKNOWN, fldShortString, fldUnicode, fldINT64, fldUNKNOWN,
    fldUNKNOWN, fldUNKNOWN, fldUNKNOWN, fldBLOB, fldBLOB, fldUNKNOWN,
    fldUNKNOWN, fldUNKNOWN, fldShortString);
  {$ENDIF}
  {$IFDEF IsVerDataset2} // d6-2005
  fsFldTypeMap: TfsFieldMap = (
    fldUNKNOWN, fldShortString, fldINT16, fldINT32, fldWord16, fldBOOLEAN,
    fldDouble, fldCURRENCY, fldBCD, fldDATE, fldTIME, fldDateTime, fldByteArray,
    fldUNKNOWN, fldAUTOINC32, fldBLOB, fldBLOBMEMO, fldBLOBGRAPHIC, fldBLOB, fldBLOB,
    fldBLOB, fldBLOB, fldUNKNOWN, fldShortString, fldUnicode, fldINT64, fldUNKNOWN,
    fldUNKNOWN, fldUNKNOWN, fldUNKNOWN, fldBLOB, fldBLOB, fldUNKNOWN,
    fldUNKNOWN, fldUNKNOWN, fldShortString, fldDateTime, fldBCD);
  {$ENDIF}
  {$IFDEF IsVerDataset3} // d2006
  fsFldTypeMap: TfsFieldMap = (
    fldUNKNOWN, fldShortString, fldINT16, fldINT32, fldWord16, fldBOOLEAN,
    fldDouble, fldCURRENCY, fldBCD, fldDATE, fldTIME, fldDateTime, fldByteArray,
    fldUNKNOWN, fldAUTOINC32, fldBLOB, fldBLOBMEMO, fldBLOBGRAPHIC, fldBLOB, fldBLOB,
    fldBLOB, fldBLOB, fldUNKNOWN, fldShortString, fldUnicode, fldINT64, fldUNKNOWN,
    fldUNKNOWN, fldUNKNOWN, fldUNKNOWN, fldBLOB, fldBLOB, fldUNKNOWN,
    fldUNKNOWN, fldUNKNOWN, fldShortString, fldDateTime, fldBCD,
    fldShortString, fldBLOBMEMO, fldDateTime, fldUNKNOWN);
  {$ENDIF}

  fsDataTypeMap: Array[0..MAXLOGFLDTYPES] Of TFieldType = (
    ftUnknown,
    ftBoolean,
    ftString,
    ftString, // ftWideString
    ftSmallint,
    ftWord,
    ftInteger,
    ftSmallint,
    ftSmallint,
    ftInteger,
    ftLargeInt,
    ftAutoInc,
    ftLargeInt,
    ftFloat,
    ftFloat,
    ftFloat,
    ftCurrency,
    ftDate,
    ftTime,
    ftDateTime,
    ftBlob,
    ftMemo,
    ftGraphic,
    ftBlob,
    ftBlob,
    ftBlob,
    ftUnknown,
    ftUnknown,
    ftUnknown,
    ftUnknown,
    ftUnknown,
    ftUnknown,
    ftUnknown,
    ftUnknown,
    ftUnknown,
    ftUnknown,
    ftUnknown,
    ftUnknown,
    ftUnknown,
    ftUnknown,
    ftUnknown,
    ftUnknown,
    ftBCD,
    ftBytes,
    ftBytes,
    ftBytes,
    ftLargeInt, // RecVersion
    ftBytes, //fldByteArray,
    ftString,
    ftString,
    ftString,
    ftString,
    ftString, //ftWideString
    ftWideString, // ftWideString RealUnicode
    ftString,
    ftMemo,
    ftDateTime,
    ftUnknown);

Const
  fscClientName = 'ClientName';
  fscDatabaseName = 'DatabaseName';
  fscSessionName = 'SessionName';
  ffcTableName = 'TableName';
  {$IFDEF AutoLog}
  fscAutoLogfile = 'AutoTrans.lg';
  {$ENDIF}

Type
  PfsFLDDescArray = ^TfsFLDDescArray;
  TfsFLDDescArray = Array[0..($FFE0 Div sizeof(FLDDesc))] Of FLDDesc;

  PfsIDXDescArray = ^TfsIDXDescArray;
  TfsIDXDescArray = Array[0..($FFE0 Div sizeof(IDXDesc))] Of IDXDesc;

  PfsVCHKDescArray = ^TfsVCHKDescArray;
  TfsVCHKDescArray = Array[0..($FF00 Div sizeof(VCHKDesc))] Of VCHKDesc;

Type
  PfsDataSetRecInfo = ^TfsDataSetRecInfo;
  TfsDataSetRecInfo = Packed Record
    riBookmarkFlag: TBookmarkFlag;
    riRecNo: Longint;
    riRefNr: TffInt64;
    riRecFlag: Byte;
  End;

  PfsKeyRecInfo = ^TfsKeyRecInfo;
  TfsKeyRecInfo = Packed Record
    kriFieldCount: Integer; {for the KeyFieldCount property}
    kriExclusive: Boolean; {for the KeyExclusive property}
    kriModified: Boolean; {data in Buffer has been modified}
  End;

  PKeyBuffers = ^TKeyBuffers;
  TKeyBuffers = Array[TfsKeyEditType] Of Pointer;

  {== Database object search routines ==================================}

Function IsFFAliasName(aSession: TFSSession;
  aName: String)
  : Boolean;
Var
  i: Integer;
  AliasList: TStringList;
Begin
  If (aSession = Nil) Or (aName = '') Then
    Begin
      Result := False;
      Exit;
    End;
  Result := True;
  AliasList := TStringList.Create;
  Try
    aSession.GetAliasNamesEx(AliasList, False);
    For i := 0 To pred(AliasList.Count) Do
      If (FFAnsiCompareText(AliasList[i], aName) = 0) Then {!!.10}
        Exit;
  Finally
    AliasList.Free;
  End; {try..finally}
  Result := False;
End;
{--------}

Function IsFFDatabaseName(aSession: TFSSession;
  aName: String)
  : Boolean;
Var
  DB: TfsDBListItem;
Begin
  If (aSession = Nil) Or (aName = '') Then
    Result := False
  Else
    Result := aSession.OwnedDBItems.FindItem(aName, DB);
End;

Function FindFSClientName(Const aName: String): TFSClient;
Begin
  Assert(Assigned(fsClients));
  If aName = '' Then
    Result := Nil
  Else If Not fsClients.FindItem(aName, TfsDBListItem(Result)) Then
    Result := Nil;
End;
{--------}

Function FindFSSessionName(Const aName: String): TFSSession;
Var
  CEInx: Integer;
Begin
  Assert(Assigned(fsClients));
  If aName = '' Then
    Result := Nil
  Else
    Begin
      fsClients.BeginRead; {!!.02}
      Try {!!.02}
        For CEInx := 0 To pred(fsClients.Count) Do
          Begin
            If (fsClients[CEInx]).
              OwnedDBItems.
              FindItem(aName, TfsDBListItem(Result)) Then
              Exit;
          End;
      Finally {!!.02}
        fsClients.EndRead; {!!.02}
      End; {!!.02}
      Result := Nil;
    End;
End;
{--------}

Function FindFSDatabaseName(aSession: TFSSession;
  Const aName: String;
  Const aCreate: Boolean): TFSBaseDatabase;
Var
  i: Integer;
  AliasList: TStringList;
Begin
  If (aName = '') Or (aSession = Nil) Then
    Begin
      Result := Nil;
      Exit;
    End;
  { if the database is found, set result and exit}
  If aSession.OwnedDBItems.FindItem(aName, TfsDBListItem(Result)) Then
    Exit;
  If aCreate Then
    Begin
      AliasList := TStringList.Create;
      Try
        aSession.GetAliasNamesEx(AliasList, False);
        { if the alias is valid, create the database and exit }
        For i := 0 To pred(AliasList.Count) Do
          Begin
            If (FFAnsiCompareText(AliasList[i], aName) = 0) Then
              Begin {!!.07}
                Result := TFSDatabase.Create(Nil);
                Result.dbliSwitchOwnerTo(aSession); {!!.01}
                Result.DataBaseName := aName;
                Result.Temporary := True;
                Exit;
              End
            Else
              Begin
                // fssql 1.046
                // path
                Result := TFSDatabase.Create(Nil);
                Result.dbliSwitchOwnerTo(aSession); {!!.01}
                Result.DataBaseName := aName;
                Result.Temporary := True;
                Exit;
              End;
          End;
      Finally
        AliasList.Free;
      End;
    End;
  { the database was not found, or the alias did not exist }
  Result := Nil;
End;
{--------}

Procedure GetFSDatabaseNames(aSession: TFSSession; aList: TStrings);
Begin
  Assert(Assigned(aList));
  Assert(Assigned(aSession));
  aList.BeginUpdate;
  Try
    aList.Clear;
    aSession.OwnedDBItems.GetItemNames(aList);
    aSession.GetAliasNamesEx(aList, False);
  Finally
    aList.EndUpdate;
  End;
End;

{===Database object name lists=======================================}

Procedure GetFSClientNames(aList: TStrings);
Begin
  Assert(Assigned(fsClients));
  Assert(Assigned(aList));
  aList.BeginUpdate;
  Try
    aList.Clear;
    fsClients.GetItemNames(aList);
  Finally
    aList.EndUpdate;
  End;
End;
{--------}

Procedure GetFSSessionNames(aList: TStrings);
Var
  Inx: Integer;
Begin
  Assert(Assigned(fsClients));
  Assert(Assigned(aList));
  fsClients.BeginRead; {!!.02}
  Try {!!.02}
    For Inx := 0 To Pred(fsClients.Count) Do
      fsClients[Inx].OwnedDBItems.GetItemNames(aList);
  Finally {!!.02}
    fsClients.EndRead; {!!.02}
  End; {!!.02}
End;
{====================================================================}

{===TFSFilterListItem==================================================}

Constructor TFSFilterListItem.Create(aContainer: TffCollection;
  aOwner: TObject;
  aClientData: Longint;
  aPriority: Integer;
  aCanAbort: Boolean;
  aExprTree: pCANExpr;
  aFiltFunc: pfGENFilter);
Begin
  Inherited Create(Nil, aContainer);

  fliOwner := aOwner;
  fliClientData := aClientData;
  fliPriority := aPriority;
  fliCanAbort := aCanAbort;
  If Assigned(aExprTree) Then
    Begin
      fliExprSize := pCANExpr(aExprTree)^.iTotalSize;
      If (fliExprSize > 0) Then
        Begin
          FFGetMem(fliExpression, fliExprSize);
          Move(aExprTree^, fliExpression^, fliExprSize);
        End;
    End;
  fliFilterFunc := aFiltFunc;
  fliActive := False;
End;
{--------}

Destructor TFSFilterListItem.Destroy;
Begin
  If (fliExprSize > 0) And Assigned(fliExpression) Then
    FFFreeMem(fliExpression, fliExprSize);

  Inherited Destroy;
End;
{--------}

Function TFSFilterListItem.fliGetLiteralPtr(aoffset: Word): Pointer;
Var
  i: Word;
Begin
  i := fliExpression^.iLiteralStart + aoffset;
  Result := @PByteArray(fliExpression)^[i];
End;
{--------}

Function TFSFilterListItem.fliGetNodePtr(aoffset: Word): PfsFilterNode;
Var
  i: Word;
Begin
  i := fliExpression^.iNodeStart + aoffset;
  Result := PfsFilterNode(@PByteArray(fliExpression)^[i]);
End;
{--------}

Procedure TFSFilterListItem.GetFilterInfo(Index: Word;
  Var FilterInfo: FilterInfo);
Begin
  {Initialize}
  FillChar(FilterInfo, sizeof(FilterInfo), 0);

  {Set info}
  FilterInfo.iFilterId := Index;
  FilterInfo.hFilter := @Self;
  FilterInfo.iClientData := fliClientData;
  FilterInfo.iPriority := fliPriority;
  FilterInfo.bCanAbort := fliCanAbort;
  FilterInfo.pffilter := fliFilterFunc;
  FilterInfo.pCanExpr := fliExpression;
  FilterInfo.bActive := fliActive;
End;
{--------}

Function TFSFilterListItem.MatchesRecord(aRecBuf: Pointer): Boolean;
Var
  FiltFuncResult: Integer;
  Root: PfsFilterNode;
Begin
  {inactive filters match all records, ie, no filtering takes place}
  If Not Active Then
    Result := True
      {otherwise, with active filters we must do some work}
  Else
    Begin
      {call the filter function first}
      If Assigned(fliFilterFunc) Then
        Begin
          FiltFuncResult := fliFilterFunc(fliClientData, aRecBuf, 0);
          If fliCanAbort And (FiltFuncResult = FSClBDE.ABORT) Then
            Begin
              Result := False;
              Exit;
            End;
          Result := FiltFuncResult <> 0;
        End
      Else {there is no filter function, ergo it matches}
        Result := True;

      {if the record matches so far, run it through the filter tree}
      If Result And Assigned(fliExpression) Then
        Begin
          Root := fliGetNodePtr(0);
          Result := fliEvaluateNode(Root, Nil, aRecBuf);
        End;
    End;
End;
{--------}

Function TFSFilterListItem.fliEvaluateNode(aNode: PfsFilterNode;
  aValue: PfsNodeValue;
  aRecBuf: Pointer): Boolean;
Begin
  If (aValue <> Nil) Then
    FillChar(aValue^, sizeof(aValue^), 0);
  Case aNode^.fnHdr.NodeClass Of
    FSSrBDE.nodeUNARY:
      Result := fliEvaluateUnaryNode(aNode, aRecBuf);
    FSSrBDE.nodeBINARY:
      If (aNode^.fnHdr.CANOp In [canAND, canOR]) Then
        Result := fliEvaluateLogicalNode(aNode, aRecBuf)
      Else
        Result := fliEvaluateBinaryNode(aNode, aRecBuf, False, 0);
    FSSrBDE.nodeCOMPARE:
      Result := fliEvaluateBinaryNode(aNode, aRecBuf,
        aNode^.fnCompare.bCaseInsensitive,
        aNode^.fnCompare.iPartialLen);
    FSSrBDE.nodeFIELD:
      Result := fliEvaluateFieldNode(aNode, aValue, aRecBuf);
    FSSrBDE.nodeCONST:
      Result := fliEvaluateConstNode(aNode, aValue, aRecBuf);
    FSSrBDE.nodeCONTINUE:
      Result := aNode^.fnContinue.iContOperand <> 0;
    Else
      {all other node classes cause the node match to fail}
      Result := False;
  End; {case}
End;
{--------}

Function TFSFilterListItem.fliEvaluateUnaryNode(aNode: PfsFilterNode;
  aRecBuf: Pointer): Boolean;
Var
  OperandNode: PfsFilterNode;
  NodeValue: TfsNodeValue;
Begin
  OperandNode := fliGetNodePtr(aNode^.fnUnary.iOperand1);
  If fliEvaluateNode(OperandNode, @NodeValue, aRecBuf) Then
    Case aNode^.fnHdr.CANOp Of
      canISBLANK:
        Result := NodeValue.nvIsNull;
      canNOTBLANK:
        Result := Not NodeValue.nvIsNull;
      Else
        Result := False;
    End {case}
  Else { the node didn't match }
    Result := aNode^.fnHdr.CANOp = canNOT;
End;
{--------}

Function TFSFilterListItem.fliEvaluateLogicalNode(aNode: PfsFilterNode;
  aRecBuf: Pointer): Boolean;
Var
  LeftNode: PfsFilterNode;
  RightNode: PfsFilterNode;
Begin
  LeftNode := fliGetNodePtr(aNode^.fnBINARY.iOperand1);
  RightNode := fliGetNodePtr(aNode^.fnBINARY.iOperand2);
  Case aNode^.fnHdr.CANOp Of
    canAND: Result := fliEvaluateNode(LeftNode, Nil, aRecBuf) And
      fliEvaluateNode(RightNode, Nil, aRecBuf);
    canOR: Result := fliEvaluateNode(LeftNode, Nil, aRecBuf) Or
      fliEvaluateNode(RightNode, Nil, aRecBuf);
    Else
      {anything else fails}
      Result := False;
  End; {case}
End;
{--------}

Function TFSFilterListItem.fliEvaluateBinaryNode(aNode: PfsFilterNode;
  aRecBuf: Pointer;
  aNoCase: Boolean;
  aPartial: Word): Boolean;
Var
  LeftNode: PfsFilterNode;
  RightNode: PfsFilterNode;
  LeftValue: TfsNodeValue;
  RightValue: TfsNodeValue;
  CompareResult: Integer;
Begin
  Result := False;
  If (aNode^.fnHdr.NodeClass = FSSrBDE.nodeCOMPARE) Then
    Begin
      LeftNode := fliGetNodePtr(aNode^.fnCompare.iOperand1);
      RightNode := fliGetNodePtr(aNode^.fnCompare.iOperand2);
    End
  Else
    Begin
      LeftNode := fliGetNodePtr(aNode^.fnBINARY.iOperand1);
      RightNode := fliGetNodePtr(aNode^.fnBINARY.iOperand2);
    End;
  If Not fliEvaluateNode(LeftNode, @LeftValue, aRecBuf) Then
    Exit;
  If Not fliEvaluateNode(RightNode, @RightValue, aRecBuf) Then
    Exit;
  If Not fliCompareValues(CompareResult, LeftValue, RightValue,
    aNoCase, aPartial) Then
    Exit;
  Case aNode^.fnHdr.CANOp Of
    canEQ: Result := CompareResult = 0;
    //canIN : Result := CompareResult = 0;
    canLike: Result := CompareResult = 0; {!!.11}
    canNE: Result := CompareResult <> 0;
    canGT: Result := CompareResult > 0;
    canLT: Result := CompareResult < 0;
    canGE: Result := CompareResult >= 0;
    canLE: Result := CompareResult <= 0;
    Else
      {anything else fails}
      Result := False;
  End; {case}
End;
{--------}

Function TFSFilterListItem.fliEvaluateConstNode(aNode: PfsFilterNode;
  aValue: PfsNodeValue;
  aRecBuf: Pointer): Boolean;
Begin
  aValue^.nvDType := aNode^.fnConst.iDType;
  //aValue^.nvDSType := aNode^.fnConst.iDSType;
  aValue^.nvSize := aNode^.fnConst.iSize;
  aValue^.nvValue := fliGetLiteralPtr(aNode^.fnConst.ioffset);
  aValue^.nvIsNull := False;
  aValue^.nvIsConst := True;
  Result := True;
End;
{--------}

Function TFSFilterListItem.fliEvaluateFieldNode(aNode: PfsFilterNode;
  aValue: PfsNodeValue;
  aRecBuf: Pointer): Boolean;
Var
  FieldDesc: TFSFieldDescItem;
  RecBufAsBytes: PByteArray Absolute aRecBuf;
  FilterFldName: PChar;
Begin
  TFSDataSet(fliOwner).dsGetFieldDescItem(aNode^.fnFIELD.iFieldNum, FieldDesc);

  {get round InfoPower filter bug}
  {the bug is this: the iFieldNum field of the node is supposed to be
   the field number of the field we are interested in (field 1 being
   the first field in the record, 2 the second field); InfoPower's
   filter parsing code sets it to a field count instead, starting at 1
   and incrementing for every field encountered in the filter string.
   We'll patch the filter binary block the first time through since
   GetFieldNumber is relatively slow.}
  FilterFldName := fliGetLiteralPtr(aNode^.fnFIELD.iNameoffset);
  If (FFAnsiStrIComp(FilterFldName, FieldDesc.PhyDesc^.szName) <> 0) Then
    Begin {!!.06, !!.07}
      {patch the filter block, so we don't keep on doing this}
      aNode^.fnFIELD.iFieldNum :=
        TFSDataSet(fliOwner).dsGetFieldNumber(FilterFldName);
      TFSDataSet(fliOwner).dsGetFieldDescItem(aNode^.fnFIELD.iFieldNum, FieldDesc);
    End;

  aValue^.nvDType := FieldDesc.PhyDesc^.iFldType;
  aValue^.nvDSType := FieldDesc.PhyDesc^.iSubType;
  aValue^.nvSize := FieldDesc.PhyDesc^.iLen;
  aValue^.nvValue := @RecBufAsBytes^[FieldDesc.PhyDesc^.ioffset];
  aValue^.nvIsConst := False;
  TFSDataSet(fliOwner).dsTranslateGet(FieldDesc, aRecBuf, Nil, aValue^.nvIsNull);

  Result := True;
End;
{--------}

Function TFSFilterListItem.fliCompareValues(Var aCompareResult: Integer;
  Var aFirst: TfsNodeValue;
  Var aSecond: TfsNodeValue;
  aIgnoreCase: Boolean;
  aPartLen: Integer): Boolean;
Begin
  Result := True;
  {Deal with nulls first, we don't have to ask the table to do it
   since null < any value, except null}
  If aFirst.nvIsNull Then
    If aSecond.nvIsNull Then
      Begin
        aCompareResult := 0;
        Exit;
      End
    Else
      Begin
        aCompareResult := -1;
        Exit;
      End
  Else {aFirst is not null}  If aSecond.nvIsNull Then
    Begin
      aCompareResult := 1;
      Exit;
    End;
  {Otherwise let the table deal with it since some translation may be
   required}
  aCompareResult := TFSDataSet(fliOwner).dsTranslateCmp(aFirst,
    aSecond,
    aIgnoreCase,
    aPartLen);
End;

{===TFSClient===================================================}

Constructor TFSClient.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);

  dbliReqPropName := fscClientName;
  bcAutoClientName := False;
  bcBeepOnLoginError := True; {!!.06}
  bcOwnServerEngine := False;
  bcServerEngine := Nil;
  bcClientID := 0;
  bcPasswordRetries := fsclLoginRetries;
  bcUserName := fsclUserName;
  bcRights := [];
  bcTimeOut := DefaultTimeOut;
  dbliNeedsNoOwner := True;
  {add ourselves to the global comms engine list}
  fsClients.AddItem(Self);
  dbliLoadPriority := 1;

  bcOnConnectionLost := IDEConnectionLost;
End;
{--------}

Destructor TFSClient.Destroy;
Begin
  FFNotifyDependents(ffn_Destroy);

  Close;

  If bcOwnServerEngine Then
    Begin
      If ServerEngine Is TFSRemoteServer Then
        TFSRemoteServer(ServerEngine).Transport.Free;
      ServerEngine.Free;
      ServerEngine := Nil;
      bcOwnServerEngine := False; {!!.06}
    End;

  If Assigned(ServerEngine) Then
    ServerEngine.FFRemoveDependent(Self);

  {remove ourselves from the global comms engine list}
  If Assigned(fsClients) Then
    fsClients.DeleteItem(Self);

  Inherited Destroy;
End;
{--------}

Procedure TFSClient.IDEConnectionLost(aSource: TObject);
Resourcestring
  cMsg = 'The connection to the server has been lost!';
Begin
  MessageDlg(cMsg, mtError, [mbOk], 1);
End;
{Begin !!.06}
{--------}
Type
  TFSServerCracker = Class(TFSBaseServerEngine);
  {--------}

Function TFSClient.ProcessRequest(aMsgID: Longint;
  aTimeout: Longint;
  aRequestData: Pointer;
  aRequestDataLen: Longint;
  aRequestDataType: TffNetMsgDataType;
  Var aReply: Pointer;
  Var aReplyLen: Longint;
  aReplyType: TffNetMsgDataType): TffResult;
Begin
  Result := TFSServerCracker(bcServerEngine).ProcessRequest(bcClientID,
    aMsgID,
    aTimeout,
    aRequestData,
    aRequestDataLen,
    aRequestDataType,
    aReply,
    aReplyLen,
    aReplyType);
End;
{--------}

Function TFSClient.ProcessRequestNoReply(aMsgID: Longint;
  aTimeout: Longint;
  aRequestData: Pointer;
  aRequestDataLen: Longint): TffResult;
Begin
  Result := TFSServerCracker(bcServerEngine).ProcessRequestNoReply(bcClientID,
    aMsgID,
    aTimeout,
    aRequestData,
    aRequestDataLen);
End;
{End !!.06}
{====================================================================}

Function TFSClient.bcGetSession(aInx: Integer): TFSSession;
Begin
  Result := TFSSession(OwnedDBItems[aInx])
End;
{--------}

Function TFSClient.bcGetSessionCount: Integer;
Begin
  Result := OwnedDBItems.Count;
End;
{--------}

Procedure TFSClient.bcDoConnectionLost;
Begin
  If Assigned(bcOnConnectionLost) Then
    Begin
      bcOnConnectionLost(Self);
    End
  Else
    Begin
      If csDesigning In ComponentState Then
        IDEConnectionLost(Self);
    End;

  dbliActive := False;
  bcClientID := 0;

  TFSRemoteServer(ServerEngine).Transport.Shutdown;
  bcClearDependents;
End;
{--------}

Function TFSClient.bcReinstateDependents: Boolean;
Var
  SessIdx: Integer;
  Sess: TFSSession;

  DBIdx: Integer;
  OwnedCmp: TFSSpecComp; {!!.12}
  DB: TFSBaseDatabase;

  DSIdx: Integer;
  DS: TFSDataSet;

  WasActive: Boolean;
  WasPrepared: Boolean;

Begin
  Result := False;
  Try
    For SessIdx := 0 To Pred(SessionCount) Do
      Begin
        Sess := Sessions[SessIdx];
        WasActive := Sess.dbliActive;
        Sess.dbliActive := False;
        Sess.scSessionID := 0;
        Sess.scServerEngine := Nil;
        If WasActive Then
          Sess.Open;

        For DBIdx := 0 To Pred(Sess.OwnedDBItems.Count) Do
          Begin {!!.12}
            OwnedCmp := Sess.OwnedDBItems[DBIdx]; {!!.12}
            If OwnedCmp Is TFSBasePluginEngine Then
              Begin {!!.12}
                TFSBasePluginEngine(OwnedCmp).Shutdown; {!!.12}
                TFSBasePluginEngine(OwnedCmp).Startup; {!!.12}
              End {!!.12}
            Else If OwnedCmp Is TFSBaseDatabase Then
              Begin {!!.12}
                DB := Sess.Databases[DBIdx];
                WasActive := DB.dbliActive;
                DB.dbliActive := False;
                DB.bdDatabaseID := 0;
                DB.bdServerEngine := Nil;
                If WasActive Then
                  DB.Open;

                For DSIdx := 0 To Pred(DB.DataSetCount) Do
                  Begin
                    DS := DB.DataSets[DSIdx];
                    WasActive := DS.dsProxy.dbliActive;
                    WasPrepared := False;
                    DS.dsProxy.dbliActive := False;
                    DS.dsProxy.tpServerEngine := Nil;
                    DS.TableState := TblClosed;
                    DS.dsCursorID := 0;
                    DS.Close;
                    If DS Is TFSBaseTable Then
                      With TFSBaseTable(DS) Do
                        Begin
                          btLookupCursorID := 0;
                          btLookupKeyFields := '';
                          btLookupNoCase := False;
                          btRangeStack.Clear;
                        End
                    Else If DS Is TFSQuery Then
                      With TFSQuery(DS) Do
                        Begin
                          WasPrepared := FPrepared;
                          FPrepared := False;
                          FStmtID := 0;
                        End;
                    If (DS Is TFSQuery) And
                      (WasPrepared) Then
                      TFSQuery(DS).Prepare;
                    If WasActive Then
                      DS.Open;
                  End; { for }
              End; { if }
          End; { if }
      End;
    Result := True;
  Except
  End;

End;
{--------}

Procedure TFSClient.bcClearDependents;
Var
  SessIdx: Integer;
  Sess: TFSSession;

  DBIdx: Integer;
  OwnedCmp: TFSSpecComp; {!!.12}
  DB: TFSBaseDatabase;

  DSIdx: Integer;
  DS: TFSDataSet;
Begin
  For SessIdx := 0 To Pred(SessionCount) Do
    Begin
      Sess := Sessions[SessIdx];
      Sess.dbliActive := False;
      Sess.scSessionID := 0;
      Sess.scServerEngine := Nil;

      For DBIdx := 0 To Pred(Sess.OwnedDBItems.Count) Do
        Begin {!!.12}
          OwnedCmp := Sess.OwnedDBItems[DBIdx]; {!!.12}
          If OwnedCmp Is TFSBasePluginEngine Then {!!.12}
            TFSBasePluginEngine(OwnedCmp).Shutdown {!!.12}
          Else If OwnedCmp Is TFSBaseDatabase Then
            Begin {!!.12}
              DB := Sess.Databases[DBIdx];
              DB.dbliActive := False;
              DB.bdDatabaseID := 0;
              DB.bdServerEngine := Nil;

              For DSIdx := 0 To Pred(DB.DataSetCount) Do
                Begin
                  DS := DB.DataSets[DSIdx];
                  If DS Is TFSBaseTable Then {!!.06}
                    DS.btIgnoreDataEvents := True; {!!.06}
                  DS.dsProxy.dbliActive := False;
                  DS.dsProxy.tpServerEngine := Nil;
                  DS.TableState := TblClosed;
                  DS.dsCursorID := 0;
                  DS.Close;
                  If DS Is TFSBaseTable Then
                    With TFSBaseTable(DS) Do
                      Begin
                        btLookupCursorID := 0;
                        btLookupKeyFields := '';
                        btLookupNoCase := False;
                        btRangeStack.Clear;
                      End
                  Else If DS Is TFSQuery Then
                    With TFSQuery(DS) Do
                      Begin
                        FStmtID := 0;
                      End;
                End; { for }
            End; { if } {!!.12}
        End;
    End;
End;
{--------}

Procedure TFSClient.FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp;
  Const AData: TffWord32);
Begin
  If (AFrom = bcServerEngine) Then
    If ((AOp = ffn_Destroy) Or (AOp = ffn_Remove)) Then
      Begin
        FFNotifyDependents(ffn_Deactivate);
        Close;
        bcServerEngine := Nil;
      End
    Else If (AOp = ffn_Deactivate) Then
      Begin
        FFNotifyDependents(ffn_Deactivate);
        Close;
      End
    Else If (AOp = ffn_ConnectionLost) Then
      Begin
        If (Active) And (bcClientID = AData) Then
          Begin
            bcDoConnectionLost;
          End;
      End;
End;
{--------}

Function TFSClient.bcGetServerEngine: TFSBaseServerEngine;
Begin
  Result := bcServerEngine;
End;
{--------}

Procedure TFSClient.bcSetAutoClientName(Const Value: Boolean);
Begin
  If Value = bcAutoClientName Then
    Exit;

  If Value Then
    Begin
      CheckInactive(False);
      ClientName := 'FSClient_' + IntToStr(Longint(Self));
    End;

  bcAutoClientName := Value;
End;
{--------}

Procedure TFSClient.bcSetClientName(Const aName: String);
{Rewritten !!.11}
Var
  CL: TFSClient;
  Counter: Integer;
  TmpName: String;
Begin
  If DBName = aName Then
    Exit;

  CheckInactive(False);
  TmpName := aName;
  CL := FindFSClientName(TmpName);
  If (CL <> Nil) Then
    If bcAutoClientName Then
      Begin
        { Generate a unique name. }
        Counter := 0;
        Repeat
          TmpName := aName + IntToStr(Counter);
          inc(Counter);
        Until FindFSClientName(TmpName) = Nil;
      End
    Else If Not (AnsiUpperCase(TmpName) = AnsiUpperCase(DBName)) Then
      Raise EfsDatabaseError.Create(
        Format(fsStrResDataSet[fsdse_CLNameExists], [TmpName]));
  DBName := TmpName;
End;
{--------}

Procedure TFSClient.bcSetUserName(Const Value: String);
Begin
  CheckInactive(False);
  bcUserName := Value;
End;

Procedure TFSClient.bcSetPassword(Const Value: String);
Begin
  CheckInactive(False);
  bcPassword := Value;
End;
{--------}

Function TFSClient.bcGetUserName: String;
Begin
  Result := bcUserName;
End;

Function TFSClient.bcGetPassword: String;
Begin
  Result := bcPassword;
End;
{--------}

Procedure TFSClient.bcSetServerEngine(Value: TFSBaseServerEngine);
Begin
  If bcServerEngine = Value Then
    Exit;

  CheckInactive(False);

  {Begin !!.02}
  If Assigned(bcServerEngine) Then
    Begin
      bcServerEngine.FFRemoveDependent(Self);
      If bcOwnServerEngine Then
        Begin
          If ServerEngine Is TFSRemoteServer Then
            TFSRemoteServer(ServerEngine).Transport.Free;
          bcServerEngine.Free;
          bcOwnServerEngine := False; {!!.06}
        End;
    End;
  {End !!.02}

  bcServerEngine := Value;
  If Assigned(bcServerEngine) Then
    bcServerEngine.FFAddDependent(Self);
End;
{--------}

Procedure TFSClient.bcSetTimeout(Const Value: Longint);
Var
  Idx: Integer; {!!.11}
Begin
  If bcTimeout = Value Then
    Exit;

  bcTimeout := Value;
  If bcClientID <> 0 Then
    If Assigned(ServerEngine) Then
      Begin
        Check(ServerEngine.ClientSetTimeout(bcClientID, Value));
        { Inform children of timeout change }
        For Idx := 0 To Pred(OwnedDBItems.Count) Do
          TFSSession(OwnedDBItems[Idx]).scRefreshTimeout;
      End;
End;
{--------}

Procedure TFSClient.dbliClosePrim;
Begin
  Inherited dbliClosePrim;

  If bcClientID <> 0 Then
    If Assigned(ServerEngine) Then
      Begin
        Check(ServerEngine.ClientRemove(bcClientID));
        If bcOwnServerEngine And (ServerEngine Is TFSRemoteServer) Then
          TFSRemoteServer(ServerEngine).Transport.State := fsesInactive;
      End;
  bcClientID := 0;
End;
{--------}

Function TFSClient.dbliCreateOwnedList: TfsDBList;
Begin
  Result := TfsDBList(TFSSessionList.Create(Self));
End;
{--------}

Procedure TFSClient.dbliDBItemAdded(aItem: TfsDBListItem);
Var
  Sess: TFSSession Absolute aItem;
Begin
  Assert(Assigned(aItem));
End;
{--------}

Procedure TFSClient.dbliDBItemDeleted(aItem: TfsDBListItem);
Var
  Sess: TFSSession Absolute aItem;
Begin
  Assert(Assigned(aItem));
End;
{--------}

Procedure TFSClient.dbliMustBeClosedError;
Begin
  RaiseFSErrorObj(Self, fsdse_CLMustBeClosed);
End;
{--------}

Procedure TFSClient.dbliMustBeOpenError;
Begin
  RaiseFSErrorObj(Self, fsdse_CLMustBeOpen);
End;
{--------}

Procedure TFSClient.GetServerNames(aServerNames: TStrings);
Var
  Prot: TfsCommsProtocolClass;
  ProtName: TffShStr;
  RSE: TFSRemoteServer; { for convenient access}
  LTrans: TFSBaseTransport; { for convenient access}
Begin
  Assert(Assigned(aServerNames));
  CheckActive;
  If IsConnected Then
    Begin {Begin !!.01}
      Assert(Assigned(ServerEngine));
      ServerEngine.GetServerNames(aServerNames, bcTimeout);
    End
  Else
    Begin
      If Assigned(ServerEngine) Then
        ServerEngine.GetServerNames(aServerNames, bcTimeout)
      Else
        Begin
          {Get the protocol from the registry}
          FFClientConfigReadProtocol(Prot, ProtName);

          { We must create our own remote server engine, transport, etc. }
          RSE := TFSRemoteServer.Create(Self);
          Try
            RSE.TimeOut := Timeout;
            LTrans := TFSParamConnect.Create(RSE);
            Try
              LTrans.Mode := fstmSEnd;
              TFSParamConnect(LTrans).Protocol := FsGetProtocolType(ProtName);
              LTrans.ServerName := FFClientConfigReadServerName;
              RSE.Transport := LTrans;

              { Get the list }
              RSE.GetServerNames(aServerNames, bcTimeout);

            Finally
              LTrans.Free;
            End;
          Finally
            RSE.Free;
          End;
        End;
    End;
End;
{--------}

Function TFSClient.IsConnected: Boolean;
Begin
  Result := ClientID <> 0;
End;
{--------}

Procedure TFSClient.OpenConnection(aSession: TFSSession);
Var
  aUserName: TffName;
  aPassword: TffName;
  aPWHash: TffWord32;
  aServerPWHash: TffWord32;
  aRights: TffUserRights;
  aClickedOK: Boolean;
  aProt: TfsCommsProtocolClass;
  aProtName: TffShStr;
  aRSE: TFSRemoteServer; { for convenient access}
  aLTrans: TFSBaseTransport; { for convenient access}
  aServerName: TffNetAddress;
  aStatus: TffResult;
  aRetryCount: Integer;
  aSecurityEnabled: boolean;

Begin
  Assert(Assigned(aSession));
  aRights := [];
  aSecurityEnabled := False;
  { Each time a session is made active, this method will be called. Since
    we may serve multiple sessions, we must check to see if we are already
    connected to a server }
  If IsConnected Then
    Exit;

  If (bcServerEngine = Nil) Then
    Begin
      {Get the protocol from the registry}
      FFClientConfigReadProtocol(aProt, aProtName);

      { We must create our own remote server engine, transport, etc. }
      aRSE := TFSRemoteServer.Create(Self);
      bcOwnServerEngine := True;
      aRSE.TimeOut := Timeout;
      aLTrans := TFSParamConnect.Create(aRSE);
      {Begin !!.01}
      {$IFDEF AutoLog}
      aLTrans.EventLog := TffEventLog.Create(aLTrans);
      aLTrans.EventLog.Enabled := True;
      aLTrans.EventLog.FileName := fscAutoLogfile;
      aLTrans.EventLogEnabled := True;
      aLTrans.EventLogOptions := [fftpLogErrors, fftpLogRequests, fftpLogReplies];
      {$ENDIF}
      aLTrans.Mode := fstmSEnd;
      TFSParamConnect(aLTrans).Protocol := FsGetProtocolType(aProtName);
      aLTrans.ServerName := FFClientConfigReadServerName;
      {$IFDEF AutoLog}
      aLTrans.EventLog.WriteStringFmt('Automatic transport serverName: %s',
        [aLTrans.ServerName]);
      {$ENDIF}
      {End !!.01}
      aRSE.Transport := aLTrans;
      bcServerEngine := aRSE;
      bcServerEngine.FFAddDependent(Self); {!!.01}
    End;

  If Assigned(bcServerEngine) Then
    Begin
      { Let the server engine know we are here. }
      If ServerEngine Is TFSRemoteServer Then
        Begin
          aLTrans := TFSRemoteServer(ServerEngine).Transport;
          If Assigned(aLTrans) Then
            Begin
              If aLTrans.State = fsesInactive Then
                Begin {!!.05}
                  aLTrans.Enabled := True;
                  { Select the appropriate server if necessary }
                  If (aLTrans Is TFSParamConnect) Then {!!.13}
                    If TFSParamConnect(aLTrans).Protocol = ptRegistry Then {!!.13}
                      aLTrans.ServerName := FFClientConfigReadServerName; {!!.13}
                  If aLTrans.ServerName = '' Then
                    Begin
                      aSession.ChooseServer(aServerName);
                      If aServerName = '' Then
                        Check(DBIERR_SERVERNOTFOUND);
                      aLTrans.ServerName := aServerName;
                    End;
                  aLTrans.State := fsesStarted;
                End;
            End
          Else
            Begin {!!.05}
              Check(fsdse_RSENeedsTransport) {!!.05}
            End; {!!.05}
        End;
      If ServerEngine.State In [fsesInactive, fsesStopped] Then
        ServerEngine.State := fsesStarted;
      aRetryCount := 0;
      If bcUserName <> '' Then
        aUserName := bcUserName
      Else
        aUserName := fsclUserName;
      If bcPassword <> '' Then
        aPassword := bcPassword
      Else
        aPassword := fsclPassword;

      If aPassword = '' Then
        aPWHash := 4294967295
      Else
        aPWHash := FSCalcShStrELFHash(aPassword);
      aServerPWHash := aPWHash;
      aStatus := ServerEngine.ClientAdd(bcClientID, aUserName, aUserName, bcTimeOut, aServerPWHash, aRights, aSecurityEnabled);
      bcRights := aRights;
      { Make sure the password was correct }
      If aStatus = DBIERR_NONE Then
        Begin
          If aSecurityEnabled Then
            If aPWHash <> aServerPWHash Then
              aStatus := DBIERR_INVALIDUSRPASS;
        End;
      While (aRetryCount < bcPasswordRetries) And
        (aStatus = DBIERR_INVALIDUSRPASS) Do
        Begin
          If bcBeepOnLoginError Then
            MessageBeep(0);

          aSession.DoLogin(aUserName, aPassword, aClickedOK);
          If Not aClickedOK Then
            Break
          Else
            Begin
              inc(aRetryCount);
              aPWHash := FSCalcShStrELFHash(aPassword);
              aServerPWHash := aPWHash; {!!.06}
              aStatus := ServerEngine.ClientAdd(bcClientID, aUserName, aUserName, bcTimeout, aPWHash, aRights, aSecurityEnabled);

              { Make sure the password was correct }
              If aStatus = DBIERR_NONE Then
                Begin
                  If aSecurityEnabled Then
                    If aPWHash <> aServerPWHash Then
                      aStatus := DBIERR_INVALIDUSRPASS;
                End;
              If aStatus = fserrReplyTimeout Then
                aStatus := DBIERR_INVALIDUSRPASS;
            End;
        End;
      Check(aStatus);
      { store login in the client component}
      bcUserName := aUserName; {!!.06}
      bcRights := aRights;
      bcPassword := aPassword; {!!.06}
    End
  Else
    Begin
      { There is no ServerEngine, so raise an exception }
      Check(DBIERR_FS_OpenNoMem)
    End;
End;
{--------}{!!BEGIN .01}

Function TFSClientList.clGetItem(aInx: Integer): TFSClient;
Begin
  Result := TFSClient(dblGetItem(aInx));
End;

{===TFSSession=======================================================}

Constructor TFSSession.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  scTablePassword := TStringList.Create;
  dbliReqPropName := fscSessionName;
  scAutoSessionName := False;
  scSessionID := 0;
  scTimeout := -1;
  scServerEngine := Nil;

  dbliLoadPriority := 2;
End;
{--------}

Destructor TFSSession.Destroy;
Begin
  dbliFreeTemporaryDependents; {!!.01}
  FFNotifyDependents(ffn_Destroy);

  Close; {!!.01}
  scTablePassword.free;
  Inherited Destroy;
End;
{--------}

Procedure TFSSession.AddAlias(Const aName: String;
  Const aPath: String;
  aCheckSpace: Boolean); {!!.11}
Begin
  Check(AddAliasEx(aName, aPath, aCheckSpace)); {!!.11}
End;
{--------}

Function TFSSession.AddAliasEx(Const aName: String;
  Const aPath: String;
  aCheckSpace: Boolean) {!!.11}
: TffResult;
Begin
  Assert(aName <> '');
  Assert(aPath <> '');
  CheckActive;
  Result := ServerEngine.DatabaseAddAlias(aName,
    aPath,
    aCheckSpace, {!!.11}
    Client.ClientID);
End;
{--------}

Procedure TFSSession.CloseDatabase(aDatabase: TFSBaseDatabase);
Begin
  If (aDatabase <> Nil) Then
    Begin
      aDatabase.Active := False; {decrement open reference count}
      If (Not aDatabase.Active) And aDatabase.Temporary Then
        aDatabase.Free;
    End;
End;
{Begin !!.06}
{--------}

Procedure TFSSession.CloseInactiveTables;
Begin
  CheckActive;
  Check(ServerEngine.SessionCloseInactiveTables(Client.ClientID)); {!!.06}
End;
{End !!.06}
{--------}

Procedure TFSSession.dbliClosePrim;
Begin
  Inherited dbliClosePrim;

  If scSessionID <> 0 Then
    If Assigned(ServerEngine) Then
      Check(ServerEngine.SessionRemove(Client.ClientID, SessionID));
  scSessionID := 0;
  scServerEngine := Nil;
End;
{--------}

Function TFSSession.dbliCreateOwnedList: TfsDBList;
Begin
  Result := TfsDBList(TFSDatabaseList.Create(Self));
End;
{--------}

Function TFSSession.dbliFindDBOwner(Const aName: String): TfsDBListItem;
Begin
  Result := FindFSClientName(aName);
End;
{--------}

Procedure TFSSession.dbliMustBeClosedError;
Begin
  RaiseFSErrorObj(Self, fsdse_SessMustBeClosed);
End;
{--------}

Procedure TFSSession.dbliMustBeOpenError;
Begin
  RaiseFSErrorObj(Self, fsdse_SessMustBeOpen);
End;
{--------}

Procedure TFSSession.dbliOpenPrim;
Var
  Stream: TMemoryStream;
  StL: TStringList;
  i, j: Integer;
  S1: AnsiString;
  cs: Integer;
Begin
  Stream := Nil;

  scServerEngine := Client.ServerEngine;
  DoStartup;
  Assert(Assigned(ServerEngine), 'ServerEngine has not been Assigned');
  {The TfffServerEngine creates a default session for every client. If there
   is not a session already in the client list, then we must create another one.}
  If Client.SessionCount = 0 Then
    Check(ServerEngine.SessionGetCurrent(Client.ClientID, scSessionID))
  Else
    Begin
      Try
        Stream := TMemoryStream.Create;
        StL := TStringList.Create;
        If Self.scTablePassword.Count > 0 Then
          Begin
            For i := 0 To Self.scTablePassword.Count - 1 Do
              Begin
                S1 := Trim(Self.scTablePassword[i]);
                If S1 <> '' Then
                  StL.Add(S1);
              End;
          End;

        j := StL.Count;
        If StL.Count > 0 Then
          Begin
            Stream.Write(j, 4);
            For i := 0 To StL.Count - 1 Do
              Begin
                j := FSCalcShStrELFHash(StL[i]);
                Stream.Write(j, 4);
              End;
            Stream.Position := 0;
          End;
        cs := Stream.Size;
        If Stream.Memory = Nil Then cs := 0;
        Check(ServerEngine.SessionAdd(Client.bcClientID, GetTimeOut, scSessionID, Stream.Memory, cs));
      Finally
        FreeAndNil(Stream);
        FreeAndNil(StL);
      End;
    End;
End;
{--------}

Procedure TFSSession.DeleteAlias(Const aName: String);
Begin
  Check(DeleteAliasEx(aName));
End;
{--------}

Function TFSSession.DeleteAliasEx(Const aName: String): TffResult;
Begin
  Assert(aName <> '');
  CheckActive;
  Result := ServerEngine.DatabaseDeleteAlias(aName,
    Client.ClientID);
End;
{--------}

Function TFSSession.FindDatabase(Const aName: String): TFSBaseDatabase;
Begin
  Result := FindFSDatabaseName(Self, aName, False);
End;
{--------}

Procedure TFSSession.GetAliasNames(aList: TStrings);
Begin
  GetAliasNamesEx(aList, True);
End;
{--------}

Function TFSSession.GetAliasNamesEx(aList: TStrings;
  Const aEmptyList: Boolean): TffResult;
Var
  WasActive: Boolean;
  CEWasActive: Boolean;
  TmpList: TList;
  I: Integer;
  PItem: PffAliasDescriptor;
Begin
  Assert(Assigned(aList));
  If aEmptyList Then
    aList.Clear;
  CEWasActive := Client.Active;
  WasActive := Active;
  If Not WasActive Then
    Active := True;
  Try
    TmpList := TList.Create;
    Try
      aList.BeginUpdate;
      Try
        Result := ServerEngine.DatabaseAliasList(TmpList, Client.ClientID);
        If Result = DBIERR_NONE Then
          For I := 0 To Pred(TmpList.Count) Do
            Begin
              PItem := PffAliasDescriptor(TmpList.Items[i]);
              If (aList.IndexOf(PItem^.adAlias) = -1) Then {New !!.01}
                aList.Add(PItem^.adAlias);
            End;
      Finally
        aList.EndUpdate;
      End;
    Finally
      For I := Pred(TmpList.Count) Downto 0 Do
        Begin
          PItem := PffAliasDescriptor(TmpList.Items[i]);
          FFFreeMem(PItem, SizeOf(PItem^));
        End;
      TmpList.Free;
    End;
  Finally
    If Not WasActive Then
      Active := False;
    If Not CEWasActive Then
      Client.Active := False;
  End; {try..finally}
End;
{--------}

Procedure TFSSession.GetAliasPath(Const aName: String;
  Var aPath: String);
{rewritten !!.11}
Var
  ffPath: TffPath;
  WasActive: Boolean;
  CEWasActive: Boolean;
Begin
  Assert(aName <> '');
  If Not IsAlias(aName) Then
    aPath := ''
  Else
    Begin
      WasActive := Active;
      CEWasActive := Client.Active;
      Try
        If Not WasActive Then
          Open;
        Check(ServerEngine.DatabaseGetAliasPath(AName,
          ffPath,
          Client.ClientID));
        aPath := ffPath;
      Finally
        If Not WasActive Then
          Close;
        If Not CEWasActive Then
          Client.Close;
      End;
    End;
End;
{--------}

Procedure TFSSession.setTablePassword(aValue: TStrings);
Begin
  scTablePassword.Assign(aValue);
End;

Procedure TFSSession.GetDatabaseNames(aList: TStrings);
Begin
  GetFSDatabaseNames(Self, aList);
End;
{--------}

Function TFSSession.GetServerDateTime(Var aServerNow: TDateTime): TffResult;
Begin
  Result := ServerEngine.GetServerDateTime(aServerNow);

  If Result <> DBIERR_NONE Then
    {Just is case something bad happened to aServerNow, we will reset it
     to the local machines date time}
    aServerNow := Now;
End;
{--------}{begin !!.07}

Function TFSSession.GetServerSystemTime(Var aServerNow: TSystemTime): TffResult;
Begin
  Result := ServerEngine.GetServerSystemTime(aServerNow);
End;
{--------}

Function TFSSession.GetServerGUID(Var aGUID: TGUID): TffResult;
Begin
  Result := ServerEngine.GetServerGUID(aGUID);
End;
{--------}

Function TFSSession.GetServerID(Var aUniqueID: TGUID): TffResult;
Begin
  Result := ServerEngine.GetServerID(aUniqueID);
End;
{--------}

Function TFSSession.GetServerStatistics(Var aStats: TfsServerStatistics): TffResult;
Begin
  Result := ServerEngine.GetServerStatistics(aStats);
End;
{--------}

Function TFSSession.GetCommandHandlerStatistics(Const aCmdHandlerIdx: Integer;
  Var aStats: TfsCommandHandlerStatistics): TffResult;
Begin
  Result := ServerEngine.GetCommandHandlerStatistics(aCmdHandlerIdx, aStats);
End;
{--------}

Function TFSSession.GetTransportStatistics(Const aCmdHandlerIdx: Integer;
  Const aTransportIdx: Integer;
  Var aStats: TfsTransportStatistics): TffResult;
Begin
  Result := ServerEngine.GetTransportStatistics(aCmdHandlerIdx, aTransportIdx, aStats);
End;
{--------}{end !!.07}

Procedure TFSSession.GetTableNames(Const aDatabaseName: String;
  Const aPattern: String;
  aExtensions: Boolean;
  aSystemTables: Boolean;
  aList: TStrings);
Var
  DB: TFSBaseDatabase;
  TmpList: TList;
  I: Integer;
  PItem: PffTableDescriptor;
  WasActive: Boolean; {!!.01}
  s: String;
Begin
  Assert(Assigned(aList));
  aList.BeginUpdate;
  Try
    aList.Clear;
    If (aDatabaseName <> '') Then
      Begin
        DB := FindFSDatabaseName(Self, aDatabaseName, True); {!!.01}
        If Assigned(DB) Then
          Begin {!!.01}
            WasActive := DB.Active; {!!.01}
            DB.Active := True; {!!.01}
            Try
              TmpList := TList.Create;
              Try
                Check(ServerEngine.DatabaseTableList(DB.DatabaseID,
                  PChar(aPattern),
                  TmpList));
                For I := 0 To Pred(TmpList.Count) Do
                  Begin
                    PItem := PffTableDescriptor(TmpList.Items[I]);
                    If aExtensions Then
                      s := PItem^.tdTableName + '.' + PItem^.tdExt
                    Else
                      s := PItem^.tdTableName;
                    If Not aSystemTables Then
                      Begin
                        If pos('SYS$', UpperCase(s)) <= 0 Then
                          aList.Add(s);
                      End
                    Else
                      aList.Add(s);
                  End;
              Finally
                For I := Pred(TmpList.Count) Downto 0 Do
                  Begin
                    PItem := PffTableDescriptor(TmpList.Items[I]);
                    FFFreeMem(PItem, SizeOf(PItem^));
                  End;
                TmpList.Free;
              End;
            Finally
              If Not WasActive Then {!!.01}
                CloseDatabase(DB);
            End; {try..finally}
          End;
      End;
  Finally
    aList.EndUpdate;
  End; {try..finally}
End;
{--------}

Function TFSSession.GetTaskStatus(
  Const aTaskID: Longint;
  Var aCompleted: Boolean;
  Var aStatus: TffRebuildStatus): TffResult;
Var
  IsPresent: Boolean;
Begin
  Result := DBIERR_NONE;

  If (aTaskID = -1) Then
    Begin
      {TaskID of -1 means no task was created, so pretend it has been
       completed - there's no need to call the server on this one}
      aCompleted := True;
      FillChar(aStatus, SizeOf(aStatus), 0);
      aStatus.rsFinished := True;
      Exit;
    End;

  Result := ServerEngine.RebuildGetStatus(aTaskID,
    Client.ClientID,
    IsPresent,
    aStatus);
  If IsPresent Then
    Begin
      aCompleted := aStatus.rsFinished;
    End
  Else
    Result := DBIERR_OBJNOTFOUND;
End;
{--------}

Function TFSSession.IsAlias(Const aName: String): Boolean;
Begin
  Result := IsFFAliasName(Self, aName);
End;
{--------}

Function TFSSession.ModifyAlias(Const aName: String;
  Const aNewName: String;
  Const aNewPath: String;
  aCheckSpace: Boolean) {!!.11}
: TffResult;
Begin
  Assert(aName <> '');
  Assert((aNewName <> '') Or (ANewPath <> ''));
  CheckActive;
  Result := ServerEngine.DatabaseModifyAlias(Client.ClientID,
    aName,
    aNewName,
    aNewPath,
    aCheckSpace); {!!.11}
End;

{--------}

Function TFSSession.OpenDatabase(Const aName: String)
  : TFSBaseDatabase;
Begin
  Result := FindFSDatabaseName(Self, aName, True);
  If Assigned(Result) Then
    Result.Active := True;
End;
{Begin !!.06}
{--------}

Function TFSSession.ProcessRequest(aMsgID: Longint;
  aTimeout: Longint;
  aRequestData: Pointer;
  aRequestDataLen: Longint;
  aRequestDataType: TffNetMsgDataType;
  Var aReply: Pointer;
  Var aReplyLen: Longint;
  aReplyType: TffNetMsgDataType): TffResult;
Begin
  Result := scGetClient.ProcessRequest(aMsgID,
    aTimeout,
    aRequestData,
    aRequestDataLen,
    aRequestDataType,
    aReply,
    aReplyLen,
    aReplyType);
End;
{--------}

Function TFSSession.ProcessRequestNoReply(aMsgID: Longint;
  aTimeout: Longint;
  aRequestData: Pointer;
  aRequestDataLen: Longint): TffResult;
Begin
  Result := scGetClient.ProcessRequestNoReply(aMsgID,
    aTimeout,
    aRequestData,
    aRequestDataLen);
End;
{End !!.06}
{--------}

Procedure TFSSession.SetLoginParameters(Const aName: TffName; aPassword: TffName);
Begin
  If Assigned(Client) Then
    Begin
      Client.UserName := aName;
      Client.Password := aPassword;
    End
  Else
    Begin
      fsclUserName := aName;
      fsclPassword := aPassword;
    End;
End;
{--------}

Procedure TFSSession.SetLoginRetries(Const aRetries: Integer);
Begin
  If Assigned(Client) Then
    Client.PasswordRetries := aRetries
  Else
    fsclLoginRetries := aRetries;
End;
{--------}

Function TFSSession.scGetClient: TFSClient;
Begin
  Result := TFSClient(DBOwner);
End;
{--------}

Function TFSSession.scGetDatabase(aInx: Integer): TFSBaseDatabase;
Begin
  Result := TFSBaseDatabase(OwnedDBItems[aInx]);
End;
{--------}

Function TFSSession.scGetDatabaseCount: Integer;
Begin
  Result := OwnedDBItems.Count;
End;
{--------}

Function TFSSession.scGetServerEngine: TFSBaseServerEngine;
Begin
  If Assigned(scServerEngine) And Active Then
    Result := scServerEngine
  Else
    Result := Client.ServerEngine;
End;
{--------}

Procedure TFSSession.scRefreshTimeout; {new !!.11}
Var
  Idx: Integer;
Begin
  If Active Then
    Begin
      Check(ServerEngine.SessionSetTimeout(Client.bcClientID, scSessionID, GetTimeout));
      For Idx := 0 To Pred(OwnedDBItems.Count) Do
        TFSBaseDatabase(OwnedDBItems[Idx]).bdRefreshTimeout;
    End;
End;
{--------}

Procedure TFSSession.scSetAutoSessionName(Const Value: Boolean);
Begin
  If Value <> scAutoSessionName Then
    Begin
      If Value Then
        Begin
          CheckInactive(False);
          SessionName := 'FSSession_' + IntToStr(Longint(Self));
        End;
      scAutoSessionName := Value;
    End;
End;
{--------}

Procedure TFSSession.scSetSessionName(Const aName: String);
{Rewritten !!.11}
Var
  S: TFSSession;
  Counter: Integer;
  TmpName: String;
Begin
  If DBName = aName Then
    Exit;

  TmpName := aName;
  S := FindFSSessionName(TmpName);
  If (S <> Nil) Then
    If scAutoSessionName Then
      Begin
        { Generate a unique name. }
        Counter := 0;
        Repeat
          TmpName := aName + IntToStr(Counter);
          inc(Counter);
        Until FindFSSessionName(TmpName) = Nil;
      End
    Else If Not (AnsiUpperCase(TmpName) = AnsiUpperCase(DBName)) Then
      RaiseFSErrorObjFmt(Self, fsdse_SessNameExists, [TmpName]);
  DBName := TmpName;
End;
{--------}

Function TFSSession.GetTimeout: Longint;
Begin
  If (scTimeOut = -1) And assigned(Client) Then
    Result := Client.Timeout
  Else
    Result := scTimeout;
End;
{--------}

Procedure TFSSession.scSetTimeout(Const Value: Longint);
Begin
  If scTimeout = Value Then
    Exit;
  scTimeout := Value;
  scRefreshTimeout;
End;
{--------}

Procedure TFSSession.DoStartup;
Begin
  { Fire the OnStartup event if necessary }
  If Assigned(scOnStartup) Then
    scOnStartup(Self);

  { ask the client to open the connection to the server }
  Client.OpenConnection(Self);
End;
{--------}

Procedure TFSSession.ChooseServer(Var aServerName: TffNetAddress);
Var
  Names: TStringList;
  //  OurServerName : TffNetAddress;                                     {!!.01}
  ChoseOne: boolean;
Begin
  aServerName := '';
  Names := TStringList.Create;
  Try
    Names.Sorted := True;
    FindServers(True);
    Try
      Client.GetServerNames(Names);
    Finally
      FindServers(False);
    End;
    If (Names.Count = 1) Then
      aServerName := Names[0]
    Else If (Names.Count > 1) Then
      Begin
        If Assigned(scChooseServer) Then
          scChooseServer(Self, Names, aServerName, ChoseOne)
        Else
          With TFsPickServerDlg.Create(Nil) Do
            Try
              CBNames.Items.Assign(Names);
              CBNames.ItemIndex := 0;
              ShowModal;
              If (ModalResult = mrOk) Then
                Begin
                  aServerName := CBNames.Text;
                  ChoseOne := True;
                End;
            Finally
              Free;
            End;
        If Not ChoseOne Then {!!.01}
          //        aServerName := OurServerName                                 {!!.01}
          //      else                                                           {!!.01}
          aServerName := Names[0];
      End;
  Finally
    Names.Free;
  End;
End;
{--------}

Procedure TFSSession.FindServers(aStarting: Boolean);
Begin
  If Assigned(scFindServers) Then
    scFindServers(Self, aStarting);
End;
{--------}

Procedure TFSSession.DoLogin(Var aUserName: TffName;
  Var aPassword: TffName;
  Var aResult: Boolean);
Var
  FFLoginDialog: TFsLoginDialog;
Begin
  If Assigned(scLogin) Then
    scLogin(Self, aUserName, aPassword, aResult)
  Else
    Begin
      FFLoginDialog := TFsLoginDialog.Create(Nil);
      Try
        With FFLoginDialog Do
          Begin
            UserName := aUserName;
            Password := aPassword;
            ShowModal;
            aResult := ModalResult = mrOK;
            If aResult Then
              Begin
                aUserName := UserName;
                aPassword := Password;
              End;
          End;
      Finally
        FFLoginDialog.Free;
      End;
    End;
End;
{====================================================================}

{===TFSSessionList===================================================}

Function TFSSessionList.slGetCurrSess: TFSSession;
Begin
  Result := slCurrSess;
End;
{--------}

Function TFSSessionList.slGetItem(aInx: Integer): TFSSession;
Begin
  Result := TFSSession(dblGetItem(aInx));
End;
{--------}

Procedure TFSSessionList.slSetCurrSess(CS: TFSSession);
Begin
  slCurrSess := CS;
End;
{====================================================================}

{===TFSDatabase======================================================}

Constructor TFSBaseDatabase.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);

  dbliReqPropName := fscDatabaseName;
  bdAutoDBName := False;
  bdDatabaseID := 0;
  bdTimeout := -1;
  bdServerEngine := Nil;

  dbliLoadPriority := 3;
  bdTransIsolation := tiRepeatableRead;
  bdRecLocking := tlOptimisticNoWait;
End;
{--------}

Destructor TFSBaseDatabase.Destroy;
Begin
  FFNotifyDependents(ffn_Destroy);

  Close; {!!.01}

  bdInformTablesAboutDestruction;

  Inherited Destroy;
End;
{--------}

Function TFSBaseDatabase.GetFreeDiskSpace(Var aFreeSpace: Int64): TffResult;
Begin
  CheckActive;
  Result := ServerEngine.DatabaseGetFreeSpace(DatabaseID, aFreeSpace);
End;
{--------}

Function TFSBaseDatabase.GetTimeout: Longint;
Begin
  If (bdTimeout = -1) And assigned(Session) Then
    Result := Session.GetTimeout
  Else
    Result := bdTimeout;
End;
{--------}

Procedure TFSBaseDatabase.CloseDataSets;
Begin
  Inherited dbliClosePrim;
End;
{--------}

Function TFSDatabase.CreateTable(
  Const aOverWrite: Boolean;
  Const aTableName: TfsTableName;
  aDictionary: TFSInfoDict): TffResult;
Begin
  Assert(aTableName <> '');
  Assert(Assigned(aDictionary));
  Result := ServerEngine.TableBuild(DatabaseID,
    aOverWrite,
    aTableName,
    False,
    aDictionary);
End;
{--------}

Function TFSBaseDatabase.ReIndexTable(Const aTableName: TfsTableName;
  Const aIndexNum: Integer;
  Var aTaskID: Longint): TffResult;
Begin
  Assert(aTableName <> '');
  aTaskID := -1;

  Result := ServerEngine.TableRebuildIndex(DatabaseID,
    aTableName,
    '',
    aIndexNum,
    aTaskID);
  If Result <> DBIERR_NONE Then
    aTaskID := -1;
End;
{--------}

Function TFSDatabase.RestructureTable(
  Const aTableName: TfsTableName;
  aDictionary: TFSInfoDict;
  aFieldMap: TStrings;
  Var aTaskID: Longint;
  aRangeError: boolean): TffResult;
Var
  I: Integer;
  FieldMapEntry: TffShStr;
  TmpTableName: TfsTableName;
  TmpFieldMap: TFSSpecStringList;
Begin
  Assert(aTableName <> '');
  Assert(Assigned(aDictionary));
  aTaskID := -1;
  TmpTableName := ffExtractFileName(aTableName);

  TmpFieldMap := TFSSpecStringList.Create;
  Try
    If Assigned(aFieldMap) Then
      For I := 0 To aFieldMap.Count - 1 Do
        Begin
          FieldMapEntry := aFieldMap[I];
          TmpFieldMap.Insert(FieldMapEntry);
        End;

    Result := ServerEngine.TableRestructure(DatabaseID,
      TmpTableName,
      aDictionary,
      TmpFieldMap,
      aTaskID,
      aRangeError);
  Finally
    TmpFieldMap.Free;
  End;

  If Result <> DBIERR_NONE Then
    aTaskID := -1;
End;
{--------}

Procedure TFSDatabase.dbliClosePrim;
Begin
  Inherited dbliClosePrim;

  If (bdDatabaseID > 0) Then
    If Assigned(ServerEngine) Then
      Check(ServerEngine.DatabaseClose(bdDatabaseID));
  bdDatabaseID := 0;
  bdServerEngine := Nil;
End;
{--------}

Function TFSBaseDatabase.dbliCreateOwnedList: TfsDBList;
Begin
  Result := TfsDBList(TFSTableProxyList.Create(Self));
End;
{--------}

Function TFSBaseDatabase.dbliFindDBOwner(Const aName: String): TfsDBListItem;
Begin
  Result := FindFSSessionName(aName);
End;
{--------}

Procedure TFSBaseDatabase.dbliMustBeClosedError;
Begin
  RaiseFSErrorObj(Self, fsdse_DBMustBeClosed);
End;
{--------}

Procedure TFSBaseDatabase.dbliMustBeOpenError;
Begin
  RaiseFSErrorObj(Self, fsdse_DBMustBeOpen);
End;
{--------}

Procedure TFSBaseDatabase.dbliOpenPrim;
Begin
  Inherited dbliOpenPrim;

  bdServerEngine := Session.ServerEngine;
End;
{--------}

Procedure TFSDatabase.dbliOpenPrim;
Var
  Alias: String;
Begin
  If (AliasName <> '') Then
    Alias := AliasName
  Else
    Alias := DataBaseName;

  Check(ServerEngine.SessionSetCurrent(Session.Client.ClientID,
    Session.SessionID));

  If Not fsIsPath(Alias) Then
    Begin
      Check(ServerEngine.DatabaseOpen(Session.Client.ClientID,
        Alias,
        TffOpenMode(Not ReadOnly),
        TffShareMode(Not Exclusive),
        GetTimeOut,
        TransIsolation,
        RecLocking,
        bdDatabaseID));
    End
  Else
    Begin
      { Alias is a specified as a path }
      Check(ServerEngine.DatabaseOpenNoAlias(Session.Client.ClientID,
        Alias,
        TffOpenMode(Not ReadOnly),
        TFFShareMode(Not Exclusive),
        GetTimeOut,
        TransIsolation,
        RecLocking,
        bdDatabaseID));
    End;
End;
{--------}

Procedure TFSBaseDatabase.bdSetAutoDBName(Const Value: Boolean);
Begin
  If Value = bdAutoDBName Then
    Exit;

  If Value Then
    Begin
      CheckInactive(False);
      DataBaseName := 'FSDB_' + IntToStr(Longint(Self));
    End;

  bdAutoDBName := Value;
End;
{--------}

Function TFSBaseDatabase.bdGetDataSetCount: Integer;
Begin
  Result := OwnedDBItems.Count;
End;
{--------}

Function TFSBaseDatabase.bdGetDataSet(aInx: Integer): TFSDataSet;
Begin
  Result := TFSTableProxy(OwnedDBItems[aInx]).fsTable;
End;
{--------}

Function TFSBaseDatabase.bdGetDatabaseID: TffDatabaseID;
Begin
  If Not Active Then
    Active := True;
  Result := bdDatabaseID;
End;
{--------}

Function TFSBaseDatabase.bdGetSession: TFSSession;
Begin
  Result := TFSSession(DBOwner);
  If (Result = Nil) Then
    RaiseFSErrorObjFmt(Self, fsdse_DBNoOwningSess, [DataBaseName]);
End;
{--------}

Procedure TFSBaseDatabase.bdInformTablesAboutDestruction;
Var
  Inx: Integer;
Begin
  For Inx := Pred(DataSetCount) Downto 0 Do
    TFSTableProxyList(OwnedDBItems)[Inx].tpDatabaseIsDestroyed;
End;
{--------}

Procedure TFSDatabase.dcSetAliasName(Const aName: String);
Begin
  CheckInactive(False);
  dcAliasName := aName;
End;
{--------}

Procedure TFSBaseDatabase.bdSetDatabaseName(Const aName: String);
{Rewritten !!.11}
Var
  Counter: Integer;
  TmpName: String;
Begin
  If DBName = aName Then
    Exit;

  TmpName := aName;
  If Not (csReading In ComponentState) Then
    Begin
      If (Owner <> Nil) And IsffAliasName(Session, TmpName) Then
        RaiseFSErrorObjFmt(Self, fsdse_MatchesAlias, [TmpName]);
      If IsffDatabaseName(Session, TmpName) Then
        If bdAutoDBName Then
          Begin
            { Generate a unique name. }
            Counter := 0;
            Repeat
              TmpName := aName + IntToStr(Counter);
              inc(Counter);
            Until Not IsFFDatabaseName(Session, TmpName);
          End
        Else If Not (AnsiUpperCase(TmpName) = AnsiUpperCase(DBName)) Then
          RaiseFSErrorObjFmt(Self, fsdse_DBNameExists, [TmpName]);
    End;
  dbliSetDBName(TmpName);
End;
{--------}

Procedure TFSBaseDatabase.bdSetExclusive(aValue: Boolean);
Var
  Inx: Integer;
Begin
  CheckInactive(False);
  bdExclusive := aValue;
  If aValue Then
    For Inx := pred(DataSetCount) Downto 0 Do
      TFSTableProxyList(OwnedDBItems)[Inx].fsTable.Exclusive := True;
End;
{--------}

Procedure TFSBaseDatabase.bdSetReadOnly(aValue: Boolean);
Var
  Inx: Integer;
Begin
  CheckInactive(False);
  bdReadOnly := aValue;
  If aValue Then
    For Inx := pred(DataSetCount) Downto 0 Do
      TFSTableProxyList(OwnedDBItems)[Inx].fsTable.ReadOnly := True;
End;
{--------}

Procedure TFSBaseDatabase.bdSetTimeout(Const Value: Longint);
Begin
  If bdTimeout = Value Then
    Exit;
  bdTimeout := Value;
  bdRefreshTimeout;
End;
{--------}

Procedure TFSDatabase.GetTableNames(aList: TStrings);
Var
  CEWasActive: Boolean;
  SSWasActive: Boolean;
  WasActive: Boolean;
  TmpList: TList;
  I: Integer;
  PItem: PffTableDescriptor;

Begin
  Assert(Assigned(aList));

  CEWasActive := Session.Client.Active;
  SSWasActive := Session.Active;
  WasActive := Active;
  If Not WasActive Then
    Active := True;
  Try
    aList.BeginUpdate;
    Try
      TmpList := TList.Create;
      Try
        Check(ServerEngine.DatabaseTableList(DatabaseID,
          '',
          TmpList));
        For I := 0 To Pred(TmpList.Count) Do
          Begin
            PItem := PffTableDescriptor(TmpList.Items[I]);
            aList.Add(PItem^.tdTableName);
          End;
      Finally
        For I := Pred(TmpList.Count) Downto 0 Do
          Begin
            PItem := PffTableDescriptor(TmpList.Items[I]);
            FFFreeMem(PItem, SizeOf(PItem^));
          End;
        TmpList.Free;
      End;
    Finally
      aList.EndUpdate;
    End; {try..finally}
  Finally
    If Not WasActive Then
      Active := False;
    If Not SSWasActive Then
      Session.Active := False;
    If Not CEWasActive Then
      Session.Client.Active := False;
  End; {try..finally}
End;
{--------}

Function TFSBaseDatabase.PackTable(Const aTableName: TfsTableName;
  Var aTaskID: Longint; UndeleteRecords: Boolean; OnlyDeleted: boolean): TffResult;
Begin
  Assert(aTableName <> '');
  aTaskID := -1;

  Result := ServerEngine.TablePack(DatabaseID,
    aTableName,
    aTaskID,
    UndeleteRecords,
    OnlyDeleted);
  If Result <> DBIERR_NONE Then
    aTaskID := -1;
End;
{--------}

Function TFSBaseDatabase.IsSQLBased: Boolean;
Begin
  Result := False;
End;
{--------}

Procedure TFSBaseDatabase.Commit;
Begin
  CheckActive;
  Check(ServerEngine.TransactionCommit(DatabaseID, True));
End;
{--------}

Procedure TFSBaseDatabase.Rollback;
Begin
  CheckActive;
  Check(ServerEngine.TransactionRollback(DatabaseID));
End;
{--------}

Function TFSBaseDatabase.GetInTransaction: boolean;
Var
  TrLevel: Longint;
  err: TffResult;

Begin
  CheckActive;
  TrLevel := -1;
  err := ServerEngine.InTransaction(bdDatabaseID, TrLevel);
  If err = DBIERR_NOACTIVETRAN Then
    Result := False
  Else
    Begin
      Check(Err);
      Result := TrLevel >= 0;
    End;
End;

Procedure TFSBaseDatabase.StartTransaction;
Begin
  CheckActive;
  If InTransaction Then
    Check(DBIERR_ACTIVETRAN);
  Check(ServerEngine.TransactionStart(bdDatabaseID, bdFailSafe));
End;
{--------}

Function TFSBaseDatabase.StartTransactionWith(Const aTables: Array Of TFSBaseTable): TffResult;
Var
  CursorIDList: TfsPointerList;
  Inx: Integer;
Begin
  CheckActive;
  If InTransaction Then
    Check(DBIERR_ACTIVETRAN);
  CursorIDList := TfsPointerList.Create;
  Try
    For Inx := Low(aTables) To High(aTables) Do
      Begin
        If Not aTables[Inx].Active Then
          RaiseFSErrorObjFmt(Self, fsdse_StartTranTblActive,
            [aTables[Inx].TableName]);
        CursorIDList.Append(Pointer(aTables[Inx].CursorID));
      End; { for }

    Result := ServerEngine.TransactionStartWith(bdDatabaseID,
      bdFailSafe,
      CursorIDList);
  Finally
    CursorIDList.Free;
  End;
End;
{End !!.10}
{--------}

Function TFSBaseDatabase.TryStartTransaction;
Begin
  Result := Not InTransaction;
  If Result Then
    StartTransaction;
End;
{--------}

Procedure TFSBaseDatabase.TransactionCorrupted;
Begin
  CheckActive;
  Check(ServerEngine.TransactionCorrupted(bdDatabaseID));
End;
{--------}

Function TFSBaseDatabase.TableExists(Const aTableName: TfsTableName): Boolean;
{rewritten !!.11}
Var
  SSWasActive: Boolean;
  CEWasActive: Boolean;
  WasActive: Boolean;
Begin
  Assert(aTableName <> '');
  SSWasActive := Session.Active;
  CEWasActive := Session.Client.Active;
  WasActive := Active;
  Try
    If Not WasActive Then
      Open;
    Check(ServerEngine.DatabaseTableExists(DatabaseID,
      aTableName,
      Result));
  Finally
    If Not WasActive Then
      Close;
    If Not SSWasActive Then
      Session.Close;
    If Not CEWasActive Then
      Session.Client.Close;
  End;
End;
{--------}

Function TFSBaseDatabase.GetFFDataDictionary(Const TableName: TfsTableName;
  Stream: TStream): TffResult;
Begin
  Assert(TableName <> '');
  Assert(Assigned(Stream));
  Result := ServerEngine.TableGetDictionary(DatabaseID,
    FFExtractFileName(TableName),
    False,
    Stream);
End;
{====================================================================}

{====================================================================}

Function TFSDatabaseList.dlGetItem(aInx: Integer): TFSBaseDatabase;
Begin
  Result := TFSBaseDatabase(dblGetItem(aInx));
End;
{====================================================================}

{===TFSTableProxyList================================================}

Procedure TFSTableProxyList.dblFreeItem(aItem: TfsDBListItem);
Var
  Inx: Integer;
  TableProxy: TFSTableProxy;
Begin
  Inx := IndexOfItem(aItem);
  If (Inx <> -1) Then
    Begin
      TableProxy := Tables[Inx];
      TableProxy.fsTable.Free;
      TableProxy.fsTable := Nil;
    End;
End;
{--------}

Function TFSTableProxyList.tlGetItem(aInx: Integer): TFSTableProxy;
Begin
  Result := TFSTableProxy(dblGetItem(aInx));
End;
{====================================================================}

{===TFSTableProxy====================================================}

Constructor TFSTableProxy.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);

  dbliReqPropName := ffcTableName;
  tpServerEngine := Nil;
  dbliLoadPriority := 4;
End;
{--------}

Procedure TFSTableProxy.dbliClosePrim;
Begin
  If Not tpClosing Then
    Begin
      tpClosing := True;
      {close the real table}
      If (fsTable <> Nil) Then
        fsTable.dsCloseViaProxy;
      {let our ancestor do its stuff}

      tpServerEngine := Nil;
      Inherited dbliClosePrim;

      tpClosing := False;
    End;
End;
{--------}

Function TFSTableProxy.dbliFindDBOwner(Const aName: String): TfsDBListItem;
Var
  i: Integer;
  DB: TFSDatabase;
Begin
  If (tpSession = Nil) Then
    Result := Nil
  Else
    Begin
      Try
        Result := FindFSDatabaseName(tpSession, aName, (Not FixingFromStream)); {!!.05}

        {if not found just look on the same form}
        If (Result = Nil) And
          (aName <> '') And
          (fsTable <> Nil) And
          (fsTable.Owner <> Nil) Then
          Begin
            For i := 0 To pred(fsTable.Owner.ComponentCount) Do
              If fsTable.Owner.Components[i] Is TFSDatabase Then
                Begin
                  DB := TFSDatabase(fsTable.Owner.Components[i]);
                  If (DB.SessionName = SessionName) And
                    (DB.DataBaseName = aName) Then
                    Begin
                      Result := DB;
                      Exit;
                    End;
                End;
          End;

      Except
        Result := Nil;
      End;
    End;
End;
{--------}

Procedure TFSTableProxy.dbliLoaded;
Var
  StreamName: String;
Begin
  Try
    If (tpSessionName <> '') Then
      Begin
        StreamName := tpSessionName;
        tpSessionName := '';
        SessionName := StreamName;
      End;
  Except
    If (csDesigning In ComponentState) Then
      Application.HandleException(Self)
    Else
      Raise;
  End; {try..except}
  If (Session <> Nil) And Session.LoadActiveFailed Then
    dbliMakeActive := False;

  Inherited dbliLoaded;
End;
{--------}

Procedure TFSTableProxy.dbliMustBeClosedError;
Begin
  RaiseFSErrorObj(Self, fsdse_TblMustBeClosed);
End;
{--------}

Procedure TFSTableProxy.dbliMustBeOpenError;
Begin
  RaiseFSErrorObj(Self, fsdse_TblMustBeOpen);
End;
{--------}

Procedure TFSTableProxy.dbliOpenPrim;
Begin
  tpServerEngine := Session.ServerEngine;
End;
{--------}

Procedure TFSTableProxy.dbliDBOwnerChanged;
Begin
  Inherited;

  SessionName := Database.SessionName;
End;
{--------}

Procedure TFSTableProxy.tpDatabaseIsDestroyed;
Begin
  tpDBGone := True;
End;
{--------}

Function TFSTableProxy.tpGetCursorID: TffCursorID;
Begin
  If Not Active Then
    Active := True;
  Result := tpCursorID;
End;
{--------}

Function TFSTableProxy.tpGetDatabase: TFSBaseDatabase;
Begin
  Result := TFSBaseDatabase(DBOwner);
End;
{--------}

Function TFSTableProxy.tpGetSession: TFSSession;
Begin
  If (tpSession = Nil) Then
    tpResolveSession;
  Result := tpSession;
End;
{--------}

Function TFSTableProxy.tpGetSessionName: String;
Begin
  If (tpSession <> Nil) Then
    tpSessionName := tpSession.SessionName;
  Result := tpSessionName;
End;
{--------}

Procedure TFSTableProxy.tpResolveSession;
Begin
  tpSession := FindFSSessionName(tpSessionName);
End;
{--------}

Procedure TFSTableProxy.tpSetSessionName(aValue: String);
Begin
  CheckInactive(True);
  If (csReading In ComponentState) Or LoadingFromStream Then
    Begin
      tpSessionName := aValue;
      tpSession := Nil;
    End
  Else If (FFAnsiCompareText(aValue, SessionName) <> 0) Then
    Begin {!!.07}
      tpSession := FindFSSessionName(aValue);
      If (tpSession <> Nil) Then
        tpSessionName := tpSession.SessionName
      Else
        tpSessionName := aValue;
      If (Not FixingFromStream) Then
        Begin
          {if we're changing session, we should invalidate our database}
          { Our owner may have had it's session changed, so we first need
            to see if our database is in this new session }
          If Assigned(dbliDbOwner) Then
            If Database.dbliDBOwner = tpSession Then
              {our database's session changed too, leave the internal database field alNone }
            Else
              //dbliDBOwner := nil;                                        {!!.12}
              dbliSetDBOwner(Nil); {!!.12}
        End;
    End;
End;
{====================================================================}

{===TFSFieldDescItem=================================================}

Constructor TFSFieldDescItem.Create(aContainer: TffCollection;
  Const FD: FLDDesc);
Begin
  Inherited Create(Nil, aContainer);

  FFGetMem(fdiPhyDesc, sizeof(FLDDesc));
  Move(FD, fdiPhyDesc^, sizeof(FLDDesc));
  FFGetMem(fdiLogDesc, sizeof(FLDDesc));
  GetBDELogicalFieldDescriptor(fdiPhyDesc^, fdiLogDesc^);
  fdiFieldNum := succ(Identifier);
End;
{--------}

Destructor TFSFieldDescItem.Destroy;
Begin
  If (fdiPhyDesc <> Nil) Then
    FFFreeMem(fdiPhyDesc, sizeof(FLDDesc));
  If (fdiLogDesc <> Nil) Then
    FFFreeMem(fdiLogDesc, sizeof(FLDDesc));

  Inherited Destroy;
End;
{====================================================================}

{===TFSTable=========================================================}
{--------}

Procedure FieldsClear(DataSet: TFSDataSet);
Var
  F: TField;
Begin
  While DataSet.Fields.Count > 0 Do
    Begin
      F := DataSet.Fields[0];
      If F <> Nil Then
        Begin
          F.DataSet := Nil;
          F.Free;
        End;
    End;
  DataSet.Fields.Clear;
  DataSet.DataEvent(deFieldListChange, 0);
End;

Destructor TFSDataSet.Destroy;
Begin
  FListBlobCache.Free;
  dsDictionary.Free;
  dsDictionary := Nil;
  dsFilters.Free;
  dsFilters := Nil;
  dsFieldDescs.Free;
  dsFieldDescs := Nil;

  {destroy our proxy}
  dsProxy.Free;
  dsProxy := Nil;
  // FSQLDISPLAY.free;
  Inherited Destroy;
End;
{--------}

Constructor TFSDataSet.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  FListBlobCache := TObjectList.Create;
  fSupportRecNo := False;
  fFlipOrder := False;
  fInRange := False;
  FBlobMode := bmAuto;
  fBlobAutoStartTransaction := False;
  fBlobChunkSize := 1024 * 64;
  fBlobStartedTransaction := False;
  fBlobModifiedError := False;
  dsCursorID := 0;
  dsTimeout := -1;
  dsCheckTimeout := 0;
  dsDeleteTimeout := 0;
  dsXltMode := xltFIELD;
  dsCurRecBuf := Nil;
  dsFilterTimeOut := 3000;
  dsFilterEval := fseServer;
  dsFilterResync := True;
  dsServerEngine := Nil;
  dsRecLockedBeforeEdit := False;
  dsRecLockedType := tluDataBase;

  dsFieldDescs := TffCollection.Create;
  dsFilters := TffCollection.Create;
  // FSQLDISPLAY := TStringList.Create;
   {create our proxy}
  dsProxy := TFSTableProxy.Create(Self);
  dsProxy.fsTable := Self;

  dsDictionary := TFSInfoDict.Create(4096);
  FConLostDestroyFields := True;
End;
{--------}

Constructor TFSBaseTable.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);

  btLookupCursorID := 0;
  btIgnoreDataEvents := False;

  {create the index definitions}
  btIndexDefs := TIndexDefs.Create(Self);
  {set up a master table link, if needed}
  btMasterLink := TMasterDataLink.Create(Self);
  btMasterLink.OnMasterChange := btMasterChanged;
  btMasterLink.OnMasterDisable := btMasterDisabled;
  btRangeStack := TfsTableRangeStack.Create;
End;
{--------}

Destructor TFSBaseTable.Destroy;
Begin
  Close;

  btRangeStack.Free;
  btRangeStack := Nil;
  btMasterLink.Free;
  btMasterLink := Nil;
  btIndexDefs.Free;
  btIndexDefs := Nil;

  Inherited Destroy;
End;

{Function TFSDataSet.AddFileBlob(Const aField: Word;
  Const aFileName: TffFullFileName): TffResult;
Var
  IsNull: Boolean;
  BLOBNr: TffInt64;
  aData: Pointer;
Begin
  Assert(aFileName <> '');
  aData := ActiveBuffer;
  If Not (Dictionary.FieldType[Pred(aField)] In
    [fstBLOB..ffcLastBLOBType]) then
    Begin
      Result := DBIERR_NOTABLOB;
      Exit;
    End ;

  Result := DBIERR_NONE;
  Dictionary.GetRecordField(Pred(aField),
    aData,
    IsNull,
    @BLOBNr);
  If Not IsNull then
    Begin
      Result := dsTruncateBlob(ActiveBuffer, aField, 0);
      If Result = DBIERR_NONE then
        Result := dsFreeBlob(ActiveBuffer, aField);
    End ;

  If Result <> DBIERR_NONE then
    Exit;

  Result := ServerEngine.FileBLOBAdd(CursorID,
    aFileName,
    BLOBNr);
  If Result = DBIERR_NONE then
    Dictionary.SetRecordField(Pred(aField),
      aData,
      @BLOBNr);
End ;}

{--------}

Procedure TFSBaseTable.AddIndex(Const aName, aFields: String;
  aOptions: TIndexOptions);
Var
  {$IFDEF DCC2006Win32OrLater}
  wFields: WideString;
  {$ENDIF}
  IndexDesc: TffIndexDescriptor;
  EFNPOS: Integer;
  Fld: String;
  FldsInKey: Integer;
  FldList: TffFieldList;
  FldCase, FldSize, FldAscDesc, FldFlags, FldNullTop: TffFieldList;
  TaskID: Longint;
  Done: Boolean;
  TaskStatus: TffRebuildStatus;
  Stream: TMemoryStream;
  WasActive: Boolean;
  Bookmark: TBookmark;
  RangeSaved: Boolean;
  Request: PfsnmCursorSetRangeReq;
  SetRangeReqLen: Integer;
Begin
  WasActive := Active;
  {ensure the field definitions are updated}
  FieldDefs.Update;

  {encode the index descriptor}
  IndexDesc.idNumber := 0;
  IndexDesc.idName := aName;
  IndexDesc.idDesc := '';
  IndexDesc.idFile := 0;
  IndexDesc.idKeyLen := 0;
  FillChar(IndexDesc.idFieldIHlprs, SizeOf(IndexDesc.idFieldIHlprs), 0);
  IndexDesc.idDups := Not (ixUnique In aOptions);

  EFNPOS := 0;
  FldsInKey := 0;

  While (EFNPos <= Length(aFields)) And
    (FldsInKey < DBIMAXFLDSINKEY) Do
    Begin
      {$IFDEF DCC2006Win32OrLater}
      wFields := aFields;
      Fld := ExtractFieldName(wFields, EFNPos);
      {$ELSE}
      Fld := ExtractFieldName(aFields, EFNPos);
      {$ENDIF}

      If (Fld <> '') And (Fld[length(Fld)] = ';') Then
        System.Delete(Fld, length(Fld), 1);
      FldList[FldsInKey] := Pred(FieldDefs.Find(Fld).FieldNo);
      FldCase[FldsInKey] := Byte(ixCaseInsensitive In aOptions);
      FldAscDesc[FldsInKey] := Byte(Not (ixDescending In aOptions));
      FldSize[FldsInKey] := 0;
      FldFlags[FldsInKey] := 0;
      FldNullTop[FldsInKey] := 1;
      Inc(FldsInKey);
    End;

  IndexDesc.idCount := FldsInKey;
  IndexDesc.idFields := FldList;
  IndexDesc.idFieldsCase := FldCase;
  IndexDesc.idFieldsSize := FldSize;
  IndexDesc.idFieldsAscDesc := FldAscDesc;
  IndexDesc.idFieldsFlags := FldFlags;
  IndexDesc.idFieldsNullTop := FldNullTop;
  {if the table is open, make sure it's in browse mode and then add
   the index}

  If WasActive Then
    Begin
      { We need to restore the position of the cursor when we are done. }
      Bookmark := GetBookmark;
      { If a range is active then push it onto the range stack.
        We will restore the range when we are done. }
      RangeSaved := False;
      If btRangeStack.SavedRequest Then
        Begin
          btRangeStack.PushSavedRequest;
          RangeSaved := True;
        End;

      { The table must be closed before an index can be added. }
      CheckBrowseMode;
      CursorPosChanged;
      Check(ServerEngine.CursorClose(CursorID));
      Try
        Check(ServerEngine.TableAddIndex(Database.DatabaseID,
          0,
          TableName,
          IndexDesc));
        Check(ServerEngine.TableRebuildIndex(Database.DatabaseID,
          TableName,
          IndexDesc.idName,
          IndexDesc.idNumber,
          TaskID));

        { OK, now wait until the re-index is complete ... }
        Done := False;
        While Not Done Do
          Begin
            Sleep(250);
            Check(Session.GetTaskStatus(TaskID, Done, TaskStatus));
          End;
      Finally
        { Re-open the table. }
        dsCursorID := GetCursorHandle(IndexName);
        { Do we need to restore a prior range? }
        If rangeSaved Then
          Begin
            btRangeStack.popSavedRequest(PffByteArray(Request), SetRangeReqLen);
            { Send the request.  Assume that if it fails we should
              continue operation anyway. }

            ServerEngine.CursorSetRange(Request^.CursorID,
              Request^.DirectKey,
              Request^.FieldCount1,
              Request^.PartialLen1,
              PffByteArray(@Request^.KeyData1),
              Request^.KeyIncl1,
              Request^.FieldCount2,
              Request^.PartialLen2,
              PffByteArray(@Request^.KeyData2),
              Request^.KeyIncl2);

          End;
        {reset the record position}
        If (Bookmark <> Nil) Then
          Begin
            Check(ServerEngine.CursorSetToBookmark(CursorID,
              Bookmark));
            FreeBookmark(Bookmark);
          End;
      End;

    End
  Else
    Begin
      {otherwise use our database to add the index}
      dsEnsureDatabaseOpen(True);
      Try
        Check(ServerEngine.TableAddIndex(Database.DatabaseID,
          CursorID,
          TableName,
          IndexDesc));
        Check(ServerEngine.TableRebuildIndex(Database.DatabaseID,
          TableName,
          IndexDesc.idName,
          IndexDesc.idNumber,
          TaskID));

        { OK, now wait until the re-index is complete ... }
        Done := False;
        While Not Done Do
          Begin
            Sleep(250);
            Check(Session.GetTaskStatus(TaskID, Done, TaskStatus));
          End;

      Finally
        dsEnsureDatabaseOpen(False);
      End;

      { re-fetch data dictionary }
      Stream := TMemoryStream.Create;
      Try
        If Database.GetFFDataDictionary(TableName, Stream) = DBIERR_NONE Then
          Begin
            Stream.Position := 0;
            Dictionary.ReadFromStream(Stream);
          End;
      Finally
        Stream.Free;
      End;

    End;

  { Make sure the index definitions are updated when required. }
  btIndexDefs.Updated := False;
End;
{--------}

Function TFSBaseTable.AddIndexEx(Const aIndexDesc: TffIndexDescriptor;
  Var aTaskID: Longint): TffResult;
Begin
  CheckInactive;
  Result := ServerEngine.TableAddIndex(Database.DatabaseID,
    CursorID,
    TableName,
    aIndexDesc);
  If Result = DBIERR_NONE Then
    Result := ServerEngine.TableRebuildIndex(Database.DatabaseID,
      TableName,
      aIndexDesc.idName,
      aIndexDesc.idNumber,
      aTaskID);
  If Result <> DBIERR_NONE Then
    aTaskID := -1;
End;
{--------}

Function TFSDataSet.AllocRecordBuffer: PChar;
Begin
  FFGetZeroMem(Result, dsRecBufSize);
  Assert(Assigned(Result), 'Rec Buf not Assigned');
End;
{--------}

Procedure TFSBaseTable.ApplyRange;
Begin
  CheckBrowseMode;
  If btSetRange Then
    Begin
      First;
      fInRange := True;
    End;
End;
{--------}

Function TFSDataSet.BookmarkValid(aBookmark: TBookmark): Boolean;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
Begin
  aFlag := 0;
  If (dsCursorID = 0) Or Not Assigned(aBookmark) Then
    Result := False
  Else
    Begin
      CursorPosChanged;
      Result := ServerEngine.CursorSetToBookmark(CursorID,
        aBookmark) = DBIERR_NONE;
      If Result Then
        Result := dsGetRecord(ffltNoLock, Nil, Nil, aFlag, aRefNr) = DBIERR_NONE;
    End;
End;
{--------}

Procedure TFSBaseTable.Cancel;
Begin
  Inherited Cancel;

  If (State = dsSetKey) Then
    btEndKeyBufferEdit(False);
End;
{--------}

Procedure TFSBaseTable.CancelRange;
Begin
  CheckBrowseMode;
  UpdateCursorPos;
  If btResetRange(CursorID, False) Then
    Begin
      Resync([]);
      fInRange := False;
      First;
    End;
End;
{--------}

Procedure TFSDataSet.ClearCalcFields(aBuffer: PChar);
Begin
  FillChar(aBuffer[dsCalcFldOfs], CalcFieldsSize, 0);
End;
{--------}

Procedure TFSDataSet.CloseCursor;
Begin
  {Begin !!.05}
  Try
    {call our ancestor (who'll call InternalClose)}
    Inherited CloseCursor;

    {if we have a handle destroy it}
    If (dsCursorID > 0) Then
      Try
        DestroyHandle(dsCursorID);
      Finally
        dsCursorID := 0;
      End;
  Finally
    {close our table proxy}
    If (dsProxy <> Nil) Then
      Begin
        dsClosing := True;
        dsProxy.Close;
        dsClosing := False;
      End;
  End;
  {End !!.05}
End;
{--------}

Function TFSDataSet.CompareBookmarks(Bookmark1,
  Bookmark2: TBookmark): Integer;
{Begin !!.02}
{$IFNDEF RaiseBookmarksExcept}
Var
  aResult: TffResult;
  {$ENDIF}
  {End !!.02}
Begin
  If (BookMark1 = Nil) Or (Bookmark2 = Nil) Then
    Begin
      If (Bookmark1 = Nil) Then
        If (Bookmark2 = Nil) Then
          Result := 0
        Else
          Result := 1
      Else
        Result := -1;
      Exit;
    End;

  CheckActive;
  {Begin !!.02}
  {$IFDEF RaiseBookmarksExcept}
  Check(ServerEngine.CursorCompareBookmarks(CursorID,
    Bookmark1,
    Bookmark2,
    Result));
  {$ELSE}
  aResult := ServerEngine.CursorCompareBookmarks(CursorID,
    Bookmark1,
    Bookmark2,
    Result);
  If aResult <> DBIERR_NONE Then
    Result := aResult;
  {$ENDIF}
  {End !!.02}
End;
{--------}

{Begin !!.02}
{--------}

Procedure TFSDataSet.CopyRecords(aSrcTable: TFSDataSet; aCopyBLOBs: Boolean; CountPerTrans: Longint); {!!.06}
Var
  WasOpen: Boolean;
Begin
  CheckBrowseMode;
  { Make sure the source table is open. }
  WasOpen := aSrcTable.Active;
  If Not WasOpen Then
    aSrcTable.Open;
  Try
    Check(ServerEngine.CursorCopyRecords(aSrcTable.CursorID, CursorID, aCopyBLOBs, CountPerTrans));
  Finally
    If Not WasOpen Then
      aSrcTable.Close;
  End;
End;
{--------}

Procedure TFSBaseTable.CreateTable; {!!.05}
Begin {!!.05}
  Assert(Assigned(Dictionary)); {!!.10}
  CreateTableEx(Dictionary.BlockSize); {!!.10}
End; {!!.05}
{--------}

Procedure TFSBaseTable.CreateTableEx(Const aBlockSize: Integer); {!!.05}
Var
  Dict: TFSInfoDict;
  EFNPOS: Integer;
  Fld: String;
  FldList: TffFieldList;
  FieldsAscDesc: TffFieldList;
  FieldsCase, FieldsSize, FieldsFlags, FieldsNullTop: TffFieldList;
  FldIHList: TffFieldIHList;
  FldType: TfsFieldType;
  FldsInKey: Integer;
  i: Integer;
  FldPhysSize: Longint;
  SeqAccessName: TffShStr;
Begin
  {the table can't be open}
  dsProxy.CheckInactive(True);
  {make sure we have defined all fields within our object}
  If (FieldDefs.Count = 0) Then
    For i := 0 To pred(FieldCount) Do
      If (Fields[i].FieldKind = fkData) Then
        FieldDefs.Add(Fields[i].FieldName,
          Fields[i].DataType,
          Fields[i].Size,
          Fields[i].Required);
  {now fill in the descriptor fields}
  dsEnsureDatabaseOpen(True);
  Try
    Dict := TFSInfoDict.Create(aBlockSize); {!!.05}
    Try
      For i := 0 To pred(FieldDefs.Count) Do
        With FieldDefs[i] Do
          Begin
            MapVCLTypeToFF(DataType, Size, FldType, FldPhysSize);
            If FldType <> fstInterval Then
              Begin
                Dict.AddField(Name, '', FldType, FldPhysSize, Precision, Required, Nil, blNone, '', rNone, False, duNormal)
              End
            Else
              RaiseFSErrorObjFmt(Self, fsdse_InvalidFieldType,
                [GetEnumName(TypeInfo(TFieldType), ord(DataType)),
                Name]);
          End;

      SeqAccessName := uppercase(fsStrResGeneral[fsscSeqAccessIndexName]);
      For i := 0 To pred(IndexDefs.Count) Do
        With IndexDefs[i] Do
          If (UpperCase(Name) <> SeqAccessName) Then
            Begin
              { Get Field List }
              EFNPOS := 0;
              FldsInKey := 0;
              While (EFNPos <= Length(Fields)) And
                (FldsInKey < DBIMAXFLDSINKEY) Do
                Begin
                  Fld := ExtractFieldName(Fields, EFNPos);
                  If (Fld <> '') And
                    (Fld[length(Fld)] = ';') Then
                    System.Delete(Fld, length(Fld), 1);
                  FldList[FldsInKey] := pred(FieldDefs.Find(Fld).FieldNo);
                  FieldsAscDesc[FldsInKey] := Byte(Not (ixDescending In Options));
                  FieldsCase[FldsInKey] := Byte(ixCaseInsensitive In Options);
                  FldIHLIst[FldsInKey] := '';
                  FieldsSize[FldsInKey] := 0;
                  FieldsFlags[FldsInKey] := 0;
                  FieldsNullTop[FldsInKey] := 1;

                  Inc(FldsInKey);
                End;
              Dict.AddIndex(Name,
                '',
                0,
                FldsInKey,
                FldList,
                FieldsAscDesc,
                FieldsCase,
                FieldsSize,
                FieldsFlags,
                FieldsNullTop,
                FldIHList,
                Not (ixUnique In Options));
            End;

      TFSDatabase(Database).CreateTable(True, TableName, Dict);
    Finally
      Dict.Free;
    End;
  Finally
    dsEnsureDatabaseOpen(False);
  End;
End;
{--------}

Procedure TFSDataSet.DoBeforeScroll;
Begin
  If BlobMode = bmCache Then
    Self.FListBlobCache.Clear;
  Inherited DoBeforeScroll;
End;

Procedure TFSDataSet.DoBeforeInsert;
Begin
  Inherited DoBeforeInsert;
End;

Procedure TFSDataSet.DoAfterDelete;
Begin
  If Self.SupportRecNo Then
    If fCurrentrecord <> fLastCurrentrecord Then
      Begin
        fCurrentrecord := fLastCurrentrecord;
        Resync([]);
      End;
  Inherited DoAfterDelete;
End;

Procedure TFSDataSet.DoAfterScroll;
Begin
  Inherited DoAfterScroll;
End;

Procedure TFSDataset.DataEvent(aEvent: TDataEvent; aInfo: Longint);
Begin
  If btIgnoreDataEvents Then
    Exit;

  Inherited DataEvent(aEvent, aInfo);

  If aEvent = deUpdateState Then
    If State = dsEdit Then
      Begin
        FreeRecordBuffer(dsOldValuesBuffer);
        dsOldValuesBuffer := AllocRecordBuffer;
        Move(ActiveBuffer^, dsOldValuesBuffer^, dsRecBufSize);
      End
    Else
      Begin
        FreeRecordBuffer(dsOldValuesBuffer);
        dsOldValuesBuffer := Nil;
      End;
End;

Procedure TFSBaseTable.DataEvent(aEvent: TDataEvent; aInfo: Longint);
Begin
  If (aEvent = dePropertyChange) Then
    IndexDefs.Updated := False;
  Inherited;
End;
{--------}

Procedure TFSBaseTable.DeleteIndex(Const aIndexName: String);
Var
  VerifiedName: String;
Begin
  btRetrieveIndexName(aIndexName, True, VerifiedName);
  If Active Then
    Begin
      CheckBrowseMode;
      Check(ServerEngine.TableDropIndex(Database.DatabaseID,
        CursorID,
        TableName,
        VerifiedName,
        0));
    End
  Else
    Begin
      dsEnsureDatabaseOpen(True);
      Try
        Check(ServerEngine.TableDropIndex(Database.DatabaseID,
          0,
          TableName,
          VerifiedName,
          0));
      Finally
        dsEnsureDatabaseOpen(False);
      End;
    End;
  btIndexDefs.Updated := False;
End;

Procedure TFSBaseTable.DeleteRecords(CountPerTrans: Longint);
Var
  tempTO: Integer;

  Procedure DelRec;
  Begin
    If State In [dsInsert, dsSetKey] Then
      Cancel
    Else
      Begin
        DataEvent(deCheckBrowseMode, 0);
        DoBeforeDelete;
        DoBeforeScroll;
        Check(ServerEngine.CursorDeleteRecords(CursorID, CountPerTrans));
        FreeFieldBuffers;
        SetState(dsBrowse);
        Resync([]);
        DoAfterDelete;
        DoAfterScroll;
        FRecordCount := 0;
        FCurrentRecord := 0;
        fLastCurrentRecord := 0;
      End;
  End;
Begin
  CheckActive;
  If ((dsDeleteTimeout > 0) Or (dsDeleteTimeout = -1)) Then
    Begin
      tempTO := Timeout;
      Timeout := dsDeleteTimeout;
      Try
        {delete the records}
        DelRec
      Finally
        Timeout := tempTO;
      End;
    End
  Else
    DelRec;
End;
{End !!.06}
{--------}

Procedure TFSDataSet.DeleteTable;
Begin
  dsProxy.CheckInactive(True);
  dsEnsureDatabaseOpen(True);
  Try
    Check(ServerEngine.TableDelete(Database.DatabaseID,
      TableName));
  Finally
    dsEnsureDatabaseOpen(False);
  End;
End;
{--------}

Procedure TFSBaseTable.DoOnNewRecord;
Var
  I: Integer;
  aLinkField: TField;
Begin
  If btMasterLink.Active And (btMasterLink.Fields.Count > 0) Then
    For I := 0 To btMasterLink.Fields.Count - 1 Do
      Begin
        If IndexFieldCount > I Then
          aLinkField := IndexFields[I]
        Else
          aLinkField := FindField(TField(btMasterLink.Fields[I]).FieldName);
        If aLinkField <> Nil Then
          aLinkField.Assign(TField(btMasterLink.Fields[I]));
      End;
  Inherited DoOnNewRecord;
End;
{--------}

Procedure TFSBaseTable.EditKey;
Begin
  btSetKeyBuffer(ketNormal, False);
End;
{--------}

Procedure TFSBaseTable.EditRangeEnd;
Begin
  btSetKeyBuffer(ketRangeEnd, False);
End;
{--------}

Procedure TFSBaseTable.EditRangeStart;
Begin
  btSetKeyBuffer(ketRangeStart, False);
End;
{--------}

Procedure TFSDataSet.EmptyTable;

Begin
  If Active Then
    Begin
      CheckBrowseMode;
      Active := False;
      Check(ServerEngine.TableEmpty(Database.DatabaseID,
        0,
        TableName));
      Active := True;
      FRecordCount := 0;
      FCurrentRecord := 0;
      fLastCurrentRecord := 0;
    End
  Else
    Begin
      dsEnsureDatabaseOpen(True);
      Try
        Check(ServerEngine.TableEmpty(Database.DatabaseID,
          0,
          TableName));
        FRecordCount := 0;
        FCurrentRecord := 0;
        fLastCurrentRecord := 0;
      Finally
        dsEnsureDatabaseOpen(False);
      End;
    End;
End;
{--------}

Function TFSBaseTable.FindKey(Const aKeyValues: Array Of Const): Boolean;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  aRecNo: Longword;
Begin
  CheckBrowseMode;
  btSetKeyFields(ketNormal, aKeyValues);
  Result := GotoKey;
  If Result Then
    If Self.SupportRecNo Then
      Begin
        resync([]);
        If Self.FlipOrder Then
          ServerEngine.RecordGetSetPosition(1, Self.cursorid, ffltNoLock, Nil, aFlag, aRecNo, aRefNr, imPosition, False)
        Else
          ServerEngine.RecordGetSetPosition(-1, Self.cursorid, ffltNoLock, Nil, aFlag, aRecNo, aRefNr, imPosition, False);
        fCurrentRecord := aRecNo + 1;
        resync([]);
      End
    Else
End;
{--------}

Procedure TFSBaseTable.FindNearest(Const aKeyValues: Array Of Const);
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  aRecNo: Longword;
Begin
  CheckBrowseMode;
  btSetKeyFields(ketNormal, aKeyValues);
  GotoNearest;
  If Self.SupportRecNo Then
    Begin
      resync([]);
      If Self.FlipOrder Then
        ServerEngine.RecordGetSetPosition(1, Self.cursorid, ffltNoLock, Nil, aFlag, aRecNo, aRefNr, imPosition, False)
      Else
        ServerEngine.RecordGetSetPosition(-1, Self.cursorid, ffltNoLock, Nil, aFlag, aRecNo, aRefNr, imPosition, False);
      fCurrentRecord := aRecNo + 1;
      resync([]);
    End;
End;
{--------}

Function TFSDataSet.FindRecord(aRestart, aGoForward: Boolean): Boolean;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  aRecNo: Longword;
Begin
  {Note: this method is called by FindFirst/Last/Next/Prior; for each
   possibility the parameters are    TT    / TF / FT / ff  }
  CheckBrowseMode;
  DoBeforeScroll;
  SetFound(False);
  UpdateCursorPos;
  CursorPosChanged;
  If Not Filtered Then
    dsActivateFilters;
  Try
    If aGoForward Then
      Begin
        If aRestart Then
          InternalFirst;
        Result := (dsGetNextRecord(ffltNoLock, Nil, Nil, aFlag, aRefNr) = DBIERR_NONE);
      End
    Else
      Begin
        If aRestart Then
          Check(ServerEngine.CursorSetToEnd(CursorID));
        Result := (dsGetPriorRecord(ffltNoLock, Nil, Nil, aFlag, aRefNr) = DBIERR_NONE); {!!.01}
      End;
  Finally
    If Not Filtered Then
      dsDeactivateFilters;
  End;
  If Result Then
    Begin
      If Self.SupportRecNo Then
        Begin
          resync([]);
          If Self.FlipOrder Then
            ServerEngine.RecordGetSetPosition(1, Self.cursorid, ffltNoLock, Nil, aFlag, aRecNo, aRefNr, imPosition, False)
          Else
            ServerEngine.RecordGetSetPosition(-1, Self.cursorid, ffltNoLock, Nil, aFlag, aRecNo, aRefNr, imPosition, False);
          fCurrentRecord := aRecNo + 1;
          resync([]);
        End
      Else
        Resync([rmExact, rmCenter]);
      SetFound(True);
      DoAfterScroll;
    End;
  Result := Found;
End;
{--------}

Procedure TFSDataSet.FreeRecordBuffer(Var aBuffer: PChar);
Begin
  If Assigned(aBuffer) Then
    Begin
      FFFreeMem(aBuffer, dsRecBufSize);
      aBuffer := Nil;
    End;

End;
{--------}

Procedure TFSDataSet.GetBookmarkData(aBuffer: PChar; aData: Pointer);
Begin
  Move(aBuffer[dsBookmarkOfs], aData^, BookmarkSize);
End;
{--------}

Function TFSDataSet.GetBookmarkFlag(aBuffer: PChar): TBookmarkFlag;
Begin
  Result := PfsDataSetRecInfo(aBuffer + dsRecInfoOfs)^.riBookmarkFlag
End;
{--------}

Function TFSDataSet.GetCanModify: Boolean;
Begin
  {the TFSTable can be modified if it is open, and in readwrite mode}
  Result := Active And (Not ReadOnly);
End;
{--------}

Function TFSDataSet.GetCurrentRecord(aBuffer: PChar): Boolean;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
Begin
  aFlag := 0;
  If (Not IsEmpty) And (GetBookmarkFlag(ActiveBuffer) = bfCurrent) Then
    Begin
      UpdateCursorPos;
      Result := dsGetRecord(ffltNoLock, aBuffer, Nil, aFlag, aRefNr) = DBIERR_NONE;
    End
  Else
    Result := False;
  If Not SupportRecNo Then Exit;
  If Result Then
    fCurrentRecord := PfsDataSetRecInfo(aBuffer + dsRecInfoOfs)^.riRecno;
End;
{--------}
{$IFDEF ProvidesDatasource}

Function TFSBaseTable.GetDataSource: TDataSource;
Begin
  Result := MasterSource;
End;
{$ENDIF}
{--------}

Procedure TFSDataSet.DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean);

{ DateTime Conversions }

  Function NativeToDateTime(DataType: TFieldType; Data: TDateTimeRec): TDateTime;
  Var
    TimeStamp: TTimeStamp;
  Begin
    Case DataType Of
      ftDate:
        Begin
          TimeStamp.Time := 0;
          TimeStamp.Date := Data.Date;
        End;
      ftTime:
        Begin
          TimeStamp.Time := Data.Time;
          TimeStamp.Date := DateDelta;
        End;
      Else
        Try
          TimeStamp := MSecsToTimeStamp(Data.DateTime);
        Except
          TimeStamp.Time := 0;
          TimeStamp.Date := 0;
        End;
    End;
    Result := TimeStampToDateTime(TimeStamp);
  End;

  Function DateTimeToNative(DataType: TFieldType; Data: TDateTime): TDateTimeRec;
  Var
    TimeStamp: TTimeStamp;
  Begin
    TimeStamp := DateTimeToTimeStamp(Data);
    Case DataType Of
      ftDate: Result.Date := TimeStamp.Date;
      ftTime: Result.Time := TimeStamp.Time;
      Else
        Result.DateTime := TimeStampToMSecs(TimeStamp);
    End;
  End;

  { Byte Field Conversions }

  Procedure BufferToByteArray(Data: Pointer; DataSize: Integer; Var VarArray: OleVariant);
  Var
    PVarData: Pointer;
  Begin
    VarArray := VarArrayCreate([0, DataSize - 1], varByte);
    PVarData := VarArrayLock(VarArray);
    Try
      Move(Data^, PVarData^, DataSize);
    Finally
      VarArrayUnlock(VarArray);
    End;
  End;

  Procedure ByteArrayToBuffer(Const Data: OleVariant; Buffer: Pointer; Var DataSize: Word);
  Var
    PVarData: Pointer;
  Begin
    DataSize := VarArrayHighBound(Data, 1) + 1;
    PVarData := VarArrayLock(Data);
    Try
      Move(PVarData^, Buffer^, DataSize);
    Finally
      VarArrayUnlock(Data);
    End;
  End;

Var
  DataSize: Word;
  W: WideString;

Begin
  Case Field.DataType Of
    ftWideString:
      Begin
        If ToNative Then
          Begin
            W := pWideChar(Source^);
            If W <> '' Then
              Move(Pointer(pChar(pWideChar(Source^)) - 1)^, Dest^, Field.Size);
          End
        Else
          Begin
            pWideString(Dest)^ := pWideChar(pChar(Source) + 1);
          End;
      End;
    ftDate, ftTime, ftDateTime:
      If ToNative Then
        TDateTimeRec(Dest^) := DateTimeToNative(Field.DataType, TDateTime(Source^))
      Else
        TDateTime(Dest^) := NativeToDateTime(Field.DataType, TDateTimeRec(Source^));
    {ftBCD:
      If ToNative then
        CurrToBCD(Currency(Source^), TBcd(Dest^), 32, Field.Size)
      Else If Not BCDToCurr(TBcd(Source^), Currency(Dest^)) then
        Raise EOverFlow.CreateFmt(SFieldOutOfRange, [Field.DisplayName]);}
    ftCurrency:
      If ToNative Then
        Currency(Dest^) := Int64(Source^) / 10000.0
      Else
        Int64(Dest^) := Round(Currency(Source^) * 10000.0);
    ftBytes:
      If ToNative Then
        ByteArrayToBuffer(POleVariant(Source)^, Dest, DataSize)
      Else
        BufferToByteArray(Source, Field.DataSize, POleVariant(Dest)^);
    ftVarBytes:
      If ToNative Then
        ByteArrayToBuffer(POleVariant(Source)^, PChar(Dest) + 2, PWord(Dest)^)
      Else
        BufferToByteArray(PChar(Source) + 2, PWord(Source)^, POleVariant(Dest)^);
  End;
End;

Function TFSDataSet.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
Var
  NativeBuf: Array[0..dsFSMaxStringSize] Of Char;
Begin
  If NativeFormat Then
    Result := GetFieldData(Field, Buffer)
  Else
    Begin
      Result := GetFieldData(Field, @NativeBuf);
      If Result Then
        DataConvert(Field, @NativeBuf, Buffer, False);
    End;
End;

Procedure TFSDataSet.SetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean);
Var
  NativeBuf: Array[0..dsFSMaxStringSize] Of Char;
Begin
  If NativeFormat Then
    SetFieldData(Field, Buffer)
  Else
    Begin
      If Buffer <> Nil Then
        DataConvert(Field, Buffer, @NativeBuf, True);
      SetFieldData(Field, @NativeBuf);
    End;
End;

Function TFSDataSet.GetFieldData(aField: TField; aBuffer: Pointer): Boolean;
Var
  IsBlank: Boolean;
  RecBuf: PChar;
  FDI: TFSFieldDescItem;
  Status: TffResult;
Begin
  Result := False;
  If Not GetActiveRecBuf(RecBuf) Then
    Exit;
  If aField.FieldNo > 0 Then
    Begin
      If dsCursorID <> 0 Then
        Begin
          If (RecBuf = Nil) Then
            Status := DBIERR_INVALIDPARAM
          Else
            Begin
              {If (aField.DataType In [ftBCD]) And (aBuffer <> Nil) then
                Begin
                  If dsGetFieldDescItem(aField.FieldNo, FDI) then
                    Status := dsTranslateGet(FDI, RecBuf, @lTempCurr, IsBlank)
                  Else
                    Status := DBIERR_OUTOFRANGE;
                End
              Else
                Begin}
              If dsGetFieldDescItem(aField.FieldNo, FDI) Then
                Status := dsTranslateGet(FDI, RecBuf, aBuffer, IsBlank)
              Else
                Status := DBIERR_OUTOFRANGE;
              //End ;
            End;
          Check(Status);
        End;

      Result := Not IsBlank;
      {If Result then
        If (aField.DataType In [ftBCD]) And (aBuffer <> Nil) then
          CurrToBCD(lTempCurr, TBCD(aBuffer^), 32, aField.Size); }

    End
  Else {FieldNo <= 0}
    Begin
      If State In [dsBrowse, dsEdit, dsInsert, dsCalcFields] Then
        Begin
          Inc(RecBuf, dsCalcFldOfs + aField.offset);
          Result := Boolean(RecBuf[0]);
          If Result And (aBuffer <> Nil) Then
            Move(RecBuf[1], aBuffer^, aField.DataSize);
        End;
    End;
End;
{--------}

Procedure TFSBaseTable.GetIndexNames(aList: TStrings);
Var
  i: Integer;
Begin
  UpdateIndexDefs;
  aList.BeginUpdate;
  Try
    aList.Clear;
    For i := 0 To pred(btIndexDefs.Count) Do
      If (btIndexDefs[i].Name <> '') Then
        aList.Add(btIndexDefs[i].Name);
  Finally
    aList.EndUpdate;
  End;
End;
{--------}

Function TFSBaseTable.GetIsIndexField(Field: TField): Boolean;
Var
  i: Integer;
Begin
  Result := True;
  For i := 0 To pred(IndexFieldCount) Do
    If (Field.FieldNo = btFieldsInIndex[i]) Then
      Exit;
  Result := False;
End;
{--------}

Function TFSDataSet.GetRecNo: Integer;
Var
  BufPtr: pchar;

Begin
  Result := 0;
  CheckActive;
  If state = dsinsert Then Exit;
  If Not SupportRecNo Then Exit;
  If bof And eof Then Exit;
  If Not GetActiveRecBuf(BufPtr) Then
    Exit;
  UpdateCursorPos;
  Result := FcurrentRecord; //PfsDataSetRecInfo(BufPtr + dsRecInfoOfs)^.riRecno;
End;

Procedure TFSDataSet.SetRecNo(Value: Integer);
Var
  i, r, v: Integer;
  aRecNo: Longword;
  aFlag: Byte;
  RefNr: TffInt64;

Begin
  aRecno := 0;
  Refnr.iLow := 0;
  RefNr.iHigh := 0;
  If Not SupportRecNo Then Exit;
  CheckActive;
  If state In [dsinsert, dsedit] Then Exit;
  i := RecNo;
  If Value = i Then Exit;
  DoBeforeScroll;
  Try
    If Value <= 1 Then
      First
    Else
      Begin
        r := RecordCount;
        If Value >= r Then
          Last
        Else If (Value >= 1) And (Value <= r) Then
          Begin
            If InRange Or ((Self.FilterEval = fseLocal) And Self.Filtered) Then
              Begin
                Self.MoveBy(Value - i);
                FcurrentRecord := Value;
              End
            Else
              Begin
                v := Value - i;
                If Self.FlipOrder Then v := -v;
                Self.fsGetRecord;
                ServerEngine.RecordGetSetPosition(V, Self.cursorid, ffltNoLock, Nil, aFlag, aRecNo, RefNr, imMoveBy, True);
                FcurrentRecord := Value;
              End;
            fLastCurrentRecord := fCurrentRecord;
          End;
      End;
  Finally
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  End;
End;

Procedure TFSDataSet.SetFlipOrder(Const Value: Boolean);
Begin
  If fFlipOrder <> Value Then
    Begin
      fFlipOrder := Value;
      fCurrentRecord := 0;
      fLastCurrentRecord := 0;
      If Active Then
        First;
    End;
End;

Procedure TFSDataSet.SetSupportRecNo(Const Value: Boolean);
Begin
  If fSupportRecNo <> Value Then
    Begin
      fSupportRecNo := Value;
      fCurrentRecord := 0;
      fLastCurrentRecord := 0;
      If Active Then
        First;
    End;
End;
{--------}

Function TFSDataSet.GetFlagRecord: Byte;
Var
  BufPtr: PChar;
Begin
  Result := 0;
  CheckActive;
  If Not GetActiveRecBuf(BufPtr) Then
    Exit;
  Result := PfsDataSetRecInfo(BufPtr + dsRecInfoOfs)^.riRecFlag;
End;

Function TFSDataSet.GetRefNr: TffInt64;
Var
  BufPtr: PChar;
Begin
  Result.iLow := 0;
  Result.iHigh := 0;
  CheckActive;
  If Not GetActiveRecBuf(BufPtr) Then
    Exit;
  Result := PfsDataSetRecInfo(BufPtr + dsRecInfoOfs)^.riRefNr;
End;

Function TFSDataSet.GetRecord(aBuffer: PChar;
  aGetMode: TGetMode;
  aDoCheck: Boolean): TGetResult;
Var
  Status: TffResult;
  Buff: Pointer;
  aFlag: Byte;
  aRefNr: TffInt64;
Begin
  aFlag := 0;
  {read the current, next or prior record; no locks placed}
  Case aGetMode Of
    gmCurrent:
      Begin
        Status := dsGetRecord(ffltNoLock, aBuffer, Nil, aFlag, aRefNr);
        //fCurrentRecord:= aRecNo;
      End;
    gmNext:
      Begin
        If fFlipOrder Then
          Begin
            Status := dsGetPriorRecord(ffltNoLock, Pointer(aBuffer), Nil, aFlag, aRefNr);
          End
        Else
          Begin
            Status := dsGetNextRecord(ffltNoLock, Pointer(aBuffer), Nil, aFlag, aRefNr);
          End;
        If status = 0 Then
          inc(fCurrentRecord);
      End;
    gmPrior:
      Begin
        If fFlipOrder Then
          Begin
            Status := dsGetNextRecord(ffltNoLock, Pointer(aBuffer), Nil, aFlag, aRefNr);
          End
        Else
          Begin
            Status := dsGetPriorRecord(ffltNoLock, Pointer(aBuffer), Nil, aFlag, aRefNr);
          End;
        If status = 0 Then
          dec(fCurrentRecord);
      End;
    Else
      Status := DBIERR_NONE;
  End;
  {check the status}
  {..for success, set the record info fields, and get the bookmark}
  {..for EOF and BOF, set the bookmark status}
  {..for anything else, return an error}
  Case Status Of
    DBIERR_NONE:
      Begin
        With PfsDataSetRecInfo(aBuffer + dsRecInfoOfs)^ Do
          Begin
            riBookmarkFlag := bfCurrent;
            //if agetmode in [gmPrior, gmNext] then
            riRecNo := fCurrentRecord;
            riRefNr := aRefNr;
            If aFlag = 0 Then aFlag := 1;
            riRecFlag := aFlag;
          End;

        fRecordCount := fCurrentRecord;
        Buff := aBuffer + dsBookmarkOfs;
        GetCalcFields(aBuffer);

        Check(ServerEngine.CursorGetBookmark(CursorID, Buff));
        Result := grOK;
      End;
    DBIERR_BOF:
      Begin
        Result := grBOF;
        With PfsDataSetRecInfo(aBuffer + dsRecInfoOfs)^ Do
          Begin
            riBookmarkFlag := bfBOF;
          End;
      End;
    DBIERR_EOF:
      Begin
        Result := grEOF;
        With PfsDataSetRecInfo(aBuffer + dsRecInfoOfs)^ Do
          Begin
            riBookmarkFlag := bfEOF;
          End;
      End;
    Else
      Begin
        Result := grError;
      End;
      If aDoCheck Then
        Check(Status);
  End;
End;
{--------}

Function TFSDataSet.GetRecordBatch(RequestCount: Longint;
  Var ReturnCount: Longint;
  pRecBuff: Pointer): TffResult;
Var
  aError: TffResult;
Begin
  CheckActive;
  ReturnCount := 0;
  Result := ServerEngine.RecordGetBatch(CursorID,
    RequestCount,
    PhysicalRecordSize,
    ReturnCount,
    pRecBuff,
    aError);
End;
{------}

Function TFSDataSet.GetRecordBatchEx(RequestCount: Longint;
  Var ReturnCount: Longint;
  pRecBuff: Pointer;
  Var Error: TffResult): TffResult;
Begin
  CheckActive;
  ReturnCount := 0;
  Result := ServerEngine.RecordGetBatch(CursorID,
    RequestCount,
    PhysicalRecordSize,
    ReturnCount,
    pRecBuff,
    Error);
End;
{------}

Function TFSDataSet.GetRecordCount: Integer;
Var
  rc: Longword;
Begin
  Result := 0;
  fRecordCount := Result;
  CheckActive;
  Check(dsGetRecordCountPrim(rc));
  Result := rc;
  fRecordCount := Result;
End;
{--------}

Function TFSDataSet.GetRecordSize: Word;
Begin
  Result := dsPhyRecSize;
End;
{--------}

Function TFSDataSet.dsGetTimeout: Longint;
Begin
  If (dsTimeout = -1) And assigned(Database) Then
    Result := Database.GetTimeout
  Else
    Result := dsTimeout;
End;
{--------}

Procedure TFSDataSet.GotoCurrent(aDataSet: TFSDataSet);
Begin
  If (FFAnsiCompareText(DataBaseName, aDataSet.DataBaseName) <> 0) Or {!!.07}
  (FFAnsiCompareText(TableName, aDataSet.TableName) <> 0) Then {!!.07}
    RaiseFSErrorObj(Self, fsdse_NotSameTbl);
  CheckBrowseMode;
  aDataSet.CheckBrowseMode;
  aDataSet.UpdateCursorPos;
  Check(ServerEngine.CursorSetToCursor(CursorID,
    aDataSet.CursorID));
  DoBeforeScroll;
  Resync([rmExact, rmCenter]);
  DoAfterScroll;
End;

Procedure TFSDataSet.fsGetRecord;
Var
  pRecBuff: PChar;
  aFlag: Byte;
  aRefNr: TffInt64;

Begin
  GetActiveRecBuf(pRecBuff);
  dsGetRecord(ffltNoLock,
    Pointer(pRecBuff),
    Nil,
    aFlag,
    aRefNr);
End;
{--------}

Function TFSBaseTable.GotoKey: Boolean;
Var
  fsKeyRecInfo: PfsKeyRecInfo;
  KeyRecBuffer: PChar;
Begin
  CheckBrowseMode;
  DoBeforeScroll;
  CursorPosChanged;
  KeyRecBuffer := PKeyBuffers(btKeyBuffers)^[ketNormal];
  fsKeyRecInfo := PfsKeyRecInfo(KeyRecBuffer + btKeyInfoOfs);
  ffGetMem(dsCurRecBuf, dsPhyRecSize);
  Try
    Result := btGetRecordForKey(CursorID, False,
      fsKeyRecInfo^.kriFieldCount,
      0,
      KeyRecBuffer,
      dsCurRecBuf) = DBIERR_NONE;
    If Result Then
      Begin
        Resync([rmExact, rmCenter]);
        DoAfterScroll;
      End;
  Finally
    FFFreeMem(dsCurRecBuf, dsPhyRecSize);
    dsCurRecBuf := Nil;
  End;
End;
{--------}

Procedure TFSBaseTable.GotoNearest;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  SearchCond: TffSearchKeyAction;
  fsKeyRecInfo: PfsKeyRecInfo;
  KeyRecBuffer: PChar;
  Status: TffResult;
Begin
  aFlag := 0;
  CheckBrowseMode;
  CursorPosChanged;
  KeyRecBuffer := PKeyBuffers(btKeyBuffers)^[ketNormal];
  fsKeyRecInfo := PfsKeyRecInfo(KeyRecBuffer + btKeyInfoOfs);
  If fsKeyRecInfo^.kriExclusive Then
    SearchCond := skaGreater
  Else
    SearchCond := skaGreaterEqual;
  Status := ServerEngine.CursorSetToKey(CursorID,
    SearchCond,
    False,
    fsKeyRecInfo^.kriFieldCount,
    0,
    Pointer(KeyRecBuffer));
  If Status = DBIERR_FS_FilterTimeout Then
    If Not dsCancelServerFilter Then
      Status := dsGetNextRecordPrim(CursorID, ffltNOLOCK, Nil, Nil, aFlag, aRefNr);
  Check(Status);
  Resync([rmCenter]);
End;
{--------}

Procedure TFSDataSet.InitFieldDefs;
Var
  SaveHandle: TffCursorID;
Begin
  dsEnsureDatabaseOpen(True);
  Try
    If (TableName = '') Then
      RaiseFSErrorObj(Self, fsdse_UnnamedTblNoFlds);
    SaveHandle := cursorID;
    If (SaveHandle = 0) Then
      OpenCursor(True);
    Try
      InternalInitFieldDefs;
    Finally
      If (SaveHandle = 0) Then
        Begin
          CloseCursor;
        End;
    End;
  Finally
    dsEnsureDatabaseOpen(False);
  End; {try..finally}
End;
{--------}

Function TFSDataSet.InsertRecordBatch(Count: Longint;
  pRecBuff: Pointer;
  Errors: PffLongintArray): TffResult;
Var
  iErr: Integer;
Begin
  If Not Assigned(pRecBuff) Or Not Assigned(Errors) Then
    Begin
      Result := DBIERR_INVALIDHNDL;
      Exit;
    End;
  CheckBrowseMode;
  Result := ServerEngine.RecordInsertBatch(CursorID,
    Count,
    PhysicalRecordSize,
    pRecBuff,
    Errors);
  If Result = DBIERR_NONE Then
    Begin
      For iErr := 0 To pred(Count) Do
        If Errors^[iErr] <> DBIERR_NONE Then
          Begin
            Result := Errors^[iErr];
            Break;
          End;
    End;
End;
{------}

Procedure TFSDataSet.InternalAddRecord(aBuffer: Pointer; aAppend: Boolean);
Var
  TransactionStarted, UserTransactionStarted: Boolean;
  aRefNr: TffInt64;
Begin
  TransactionStarted := False;

  If BlobAutoStartTransaction Then
    Begin
      Try
        If fBlobStartedTransaction Then
          UserTransactionStarted := False
        Else
          UserTransactionStarted := Database.InTransaction;
        TransactionStarted := UserTransactionStarted;
        UserTransactionStarted := UserTransactionStarted And (Not fBlobStartedTransaction);
        If Not UserTransactionStarted Then
          If Not TransactionStarted Then
            Begin
              Database.StartTransaction;
              TransactionStarted := True;
            End;

        If aAppend Then
          Check(ServerEngine.CursorSetToEnd(CursorID));
        Check(ServerEngine.RecordInsert(CursorID,
          ffltWriteLock,
          aBuffer,
          False,
          aRefNr));
        If BlobMode = bmCache Then
          Begin
            WriteCacheBlob;
            fListBlobCache.Clear;
          End;

        If TransactionStarted Then
          If Not UserTransactionStarted Then
            If Database.InTransaction Then
              Begin
                Database.Commit;
                fBlobStartedTransaction := False;
              End;
        Inc(fRecordCount);
      Except
        If TransactionStarted Then
          Begin
            fBlobStartedTransaction := False;
            If BlobMode = bmCache Then
              fListBlobCache.Clear;
            Database.Rollback;
          End;
        Raise;
      End;
    End
  Else
    Begin
      If aAppend Then
        Check(ServerEngine.CursorSetToEnd(CursorID));
      Check(ServerEngine.RecordInsert(CursorID,
        ffltWriteLock,
        aBuffer, False, aRefNr));
      If BlobMode = bmCache Then
        Begin
          WriteCacheBlob;
          fListBlobCache.Clear;
        End;
      Inc(fRecordCount);
    End;
End;
{--------}

Procedure TFSDataSet.InternalRefresh;
Begin
  Inherited;
  If BlobMode = bmCache Then
    Begin
      Resync([]);
      UpdateCursorPos;
      If BlobMode = bmCache Then
        RefreshCacheBlob;
    End;
End;

Procedure TFSDataSet.Cancel;
Begin
  If BlobMode = bmCache Then
    fListBlobCache.Clear;

  If BlobAutoStartTransaction Then
    Begin
      If Database.InTransaction And
        fBlobStartedTransaction Then
        Begin
          fBlobStartedTransaction := False;
          Database.Rollback;
          Resync([]);
          UpdateCursorPos;
          If BlobMode = bmCache Then
            RefreshCacheBlob;
        End;
    End;
  Inherited Cancel;
End;

Procedure TFSDataSet.InternalCancel;
Begin
  If (State = dsEdit) Then
    Check(ServerEngine.RecordRelLock(CursorID, False));

  If Self.SupportRecNo Then
    If (State = dsInsert) Then
      fCurrentRecord := fLastCurrentRecord;
  //If (Dictionary.EngineDeleteType = edtUndeleteFull) then
  //  dec(fCurrentrecord);
  Inherited;
End;
{--------}

Procedure TFSDataSet.InternalClose;
Begin
  fListBlobCache.Clear;
  Try
    {deactivate filters}
    If Filtered Then
      dsDeactivateFilters;
  Finally
    {drop filters}
    dsDropFilters;
    {clear up the fields}
    BindFields(False);
    If DefaultFields Then
      DestroyFields;
    dsServerEngine := Nil;
    btFreeKeyBuffers;
    btKeyLength := 0;
  End;
End;
{--------}

Procedure TFSBaseTable.InternalClose;
Begin
  Try
    Try
      Check(ServerEngine.RecordRelLock(CursorID, False));
    Except
    End;
    Inherited InternalClose;
    {reset important variables}
    btIndexFieldCount := 0;
    btNoCaseIndex := False;
  Except
  End;
End;
{--------}

Procedure TFSDataSet.InternalDelete;
Var
  tempTO: Integer;
  Result: TffResult;
  r: Longword;
Begin
  If ((dsDeleteTimeout > 0) Or (dsDeleteTimeout = -1)) Then
    Begin
      tempTO := Timeout;
      Timeout := dsDeleteTimeout;
      Try
        {delete the record}
        Result := ServerEngine.RecordDelete(CursorID, Nil);
        {apart from success, we allow not found type errors; check others}
        If (Result <> DBIERR_NONE) And
          (ErrCat(Result) <> ERRCAT_NOTFOUND) Then
          Check(Result);

      Finally
        Timeout := tempTO;
      End;
    End
  Else
    Begin
      {delete the record}
      Result := ServerEngine.RecordDelete(CursorID, Nil);
      {apart from success, we allow not found type errors; check others}
      If (Result <> DBIERR_NONE) And
        (ErrCat(Result) <> ERRCAT_NOTFOUND) Then
        Check(Result);
    End;
  If SupportRecNo Then
    Begin
      r := RecordCount;
      fLastCurrentrecord := fCurrentrecord;
      If (fCurrentrecord < r + 1) And (fCurrentrecord >= 1) Then
        Begin
          dec(fCurrentrecord);
        End
      Else
        dec(fLastCurrentrecord);
      dec(fRecordCount);
    End;
  {
        resync([]);
        If Self.FlipOrder then
          ServerEngine.RecordGetSetPosition(1, Self.cursorid, ffltNoLock, Nil, aFlag, aRecNo, aRefNr, imPosition, False)
        Else
          ServerEngine.RecordGetSetPosition(-1, Self.cursorid, ffltNoLock, Nil, aFlag, aRecNo, aRefNr, imPosition, False);
        fCurrentRecord := aRecNo + 1;
        resync([]);
  }
End;
{--------}

Function TFSDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
Begin
  Case FieldType Of
    ftInteger: Result := TfsIntegerField;
    ftFloat: Result := TFSExtendedField;
    ftBlob: Result := TFSBlobField;
    ftGraphic: Result := TFSGraphicField;
    ftMemo: Result := TFSMemoField;
    ftFmtMemo: Result := TFSFmtMemoField;
    ftCurrency: Result := TFSCurrencyField;
    ftBCD: Result := TFSBcdField;
    ftBytes: Result := TfsArrayField;
    Else
      Result := DefaultFieldClasses[FieldType];
  End;
End;

Procedure TFSDataSet.DoBeforeEdit;
Begin
  Inherited dobeforeedit;
End;

Procedure TFSDataSet.DoAfterOpen;
Begin
  Inherited DoAfterOpen;
End;

Procedure TFSDataSet.DoAfterInsert;
Begin
  Inherited DoAfterInsert;
End;

Procedure TFSDataSet.DoBeforePost;
Begin
  Inherited dobeforepost;
End;

Procedure TfsDataSet.InternalInsert;
Begin
  Inherited InternalInsert;
End;

Procedure TfsDataSet.DoAfterPost;
Var
  aFlag: Byte;
  aRecNo: Longword;
  aRefNr: TffInt64;
  I: Integer;

Begin
  If Self.SupportRecNo And (Not InRange) Then
    If (Dictionary.EngineDeleteType <> edtUndeleteFull) Then
      Begin
        resync([]);
        If Self.FlipOrder Then
          i := 1
        Else
          i := -1;
        ServerEngine.RecordGetSetPosition(i, Self.cursorid, ffltNoLock, Nil, aFlag, aRecNo, aRefNr, imPosition, False);
        fCurrentRecord := aRecNo + 1;
        resync([]);
      End
    Else
      Begin
        If Self.FlipOrder Then
          Begin
            fCurrentRecord := 1;
            resync([]);
          End;
      End;
  Inherited DoAfterPost;
End;

Procedure TFSDataSet.InternalEdit;
Var
  r: Longint;
  tempTO: Integer;
  aFlag: Byte;
  aRefNr: TffInt64;
Begin
  aFlag := 0;
  If (dsRecLockedBeforeEdit) Then
    Begin
      If ((dsCheckTimeout > 0) Or (dsCheckTimeout = -1)) Then
        Begin
          tempTO := Timeout;
          Timeout := dsCheckTimeout;
          Try
            r := ServerEngine.RecordGet(CursorID, ffltWriteLock, dsRecLockedType, Pointer(ActiveBuffer), aFlag, aRefNr, False);
            If r <> 0 Then
              Begin
                Check(R);
                Check(ServerEngine.RecordRelLock(CursorID, False));
                SysUtils.Abort;
                Cancel;
              End;
          Finally
            Timeout := tempTO;
          End;
        End
      Else
        Begin
          r := ServerEngine.RecordGet(CursorID, ffltWriteLock, dsRecLockedType, Pointer(ActiveBuffer), aFlag, aRefnr, False);
          If r <> 0 Then
            Begin
              Check(R);
              Check(ServerEngine.RecordRelLock(CursorID, False));
              SysUtils.Abort;
              Cancel;
            End;
        End;
    End;
End;

{--------}

Procedure TFSDataSet.InternalGotoBookmark(aBookmark: TBookmark);
{Var
  BufPtr: PChar; }
Begin
  If Not Assigned(aBookmark) Then
    Check(DBIERR_INVALIDHNDL);
  Check(ServerEngine.CursorSetToBookmark(CursorID, aBookmark));
  {  If Not SupportRecNo then Exit;
  If Not GetActiveRecBuf(BufPtr) then
    Exit;
  fCurrentRecord := PfsDataSetRecInfo(BufPtr + dsRecInfoOfs)^.riRecno;
  }
End;
{--------}

Procedure TFSDataSet.InternalHandleException;
Begin
  Application.HandleException(Self);
End;
{--------}

Procedure TFSDataSet.InternalInitFieldDefs;
Var
  ffFldDesc: PffFieldDescriptor;
  i: Integer;
Begin
  FieldDefs.Clear;
  With Dictionary Do
    For i := 0 To pred(FieldCount) Do
      Begin
        ffFldDesc := FieldDescriptor[i];
        dsAddFieldDesc(ffFldDesc, succ(i));
      End;
End;
{--------}

Procedure TFSDataSet.InternalInitRecord(aBuffer: PChar);
Begin
  Dictionary.InitRecord(Pointer(aBuffer));
  Dictionary.FUserName := Self.Session.Client.UserName;
  Dictionary.SetDefaultFieldUpdateValues(PffByteArray(aBuffer), Nil);
  If BlobMode = bmCache Then
    fListBlobCache.Clear;
  With PfsDataSetRecInfo(aBuffer + dsRecInfoOfs)^ Do
    Begin
      If Self.SupportRecNo Then
        Begin
          fLastCurrentRecord := fCurrentRecord;
          If (Dictionary.EngineDeleteType = edtUndeleteFull) Then
            Begin
              fCurrentrecord := fRecordCount + 1;
              riRecNo := fCurrentrecord;
            End
          Else If (Eof Or (fCurrentrecord = Longword(RecordCount * 1))) Then
            Begin
              inc(fCurrentrecord);
              riRecNo := fCurrentrecord;
            End
          Else
            Begin
              inc(fCurrentrecord);
              riRecNo := fCurrentrecord;
            End;
        End
      Else
        riRecNo := 0;
      riBookmarkFlag := bfInserted;
      riRefNr.iLow := 0;
      riRefNr.iHigh := 0;
      riRecFlag := 1;
    End;
End;
{--------}

Procedure TFSDataSet.InternalFirst;
Begin
  fCurrentRecord := 0;
  fLastCurrentRecord := 0;
  If fFlipOrder Then
    Check(ServerEngine.CursorSetToEnd(CursorID))
  Else
    Check(ServerEngine.CursorSetToBegin(CursorID));
End;
{--------}

Procedure TFSDataSet.InternalLast;
Begin
  If fFlipOrder Then
    Check(ServerEngine.CursorSetToBegin(CursorID))
  Else
    Check(ServerEngine.CursorSetToEnd(CursorID));
  If Not SupportRecNo Then Exit;
  fCurrentRecord := RecordCount + 1;
  fLastCurrentRecord := fCurrentRecord;
End;
{$IFDEF ResizePersistFields}
{--------}

Procedure TFSDataSet.ReSizePersistentFields;
Var
  I, FieldIndex: Integer;
  FieldDef: TFieldDef;
Begin
  For I := 0 To Fields.Count - 1 Do
    With Fields[I] Do
      Begin
        If FieldKind = fkData Then
          Begin
            FieldIndex := FieldDefList.IndexOf(FullName);
            If FieldIndex <> -1 Then
              Begin
                FieldDef := FieldDefList[FieldIndex];
                If (DataType = ftString) And (Size <> FieldDef.Size) Then
                  Size := FieldDef.Size;
              End;
          End;
      End;
End;
{$ENDIF}
{--------}

Procedure TFSDataSet.InternalOpen;
Var
  CursorProps: TfsCursorProperties;
Begin
  fCurrentRecord := 0;
  fLastCurrentRecord := 0;
  fInRange := False;
  If BlobMode = bmCache Then
    fListBlobCache.Clear;
  dsServerEngine := Session.ServerEngine;
  {Note: by the time this method gets called, the FlashFiler table has
         been physically opened and tcHandle is valid.}
  GetCursorProps(CursorProps);
  dsPhyRecSize := CursorProps.RecordBufferSize;
  BookmarkSize := CursorProps.BookmarkSize;
  InternalInitFieldDefs;
  dsGetIndexInfo;
  If DefaultFields Then
    CreateFields;
  {$IFDEF ResizePersistFields}
  ReSizePersistentFields;
  {$ENDIF}

  BindFields(True);

  dsGetRecordInfo(True);
  dsAllocKeyBuffers;
  InternalFirst;
  dsCheckMasterRange;
  If (FilterEval = fseLocal) And (Filter <> '') Then
    dsAddExprFilter(Filter, FilterOptions);
  If Assigned(OnFilterRecord) Then
    dsAddFuncFilter(@TFSBaseTable.dsOnFilterRecordCallback);
  If Filtered Then
    dsActivateFilters;
  UpdateFormatFields;

  SetBufListSize(0);
  dsGetRecordInfo(True);
  {get new record Buffers}
  SetBufListSize(BufferCount + 1);
  fBlobStartedTransaction := False;
End;
{--------}

Procedure TFSDataSet.InternalPost;
Var
  R, tempTO: Longint;
  TransactionStarted: Boolean;
  UserTransactionStarted: Boolean;
  aRefNr: TffInt64;

Begin
  R := 0;
  TransactionStarted := False;

  {$IFDEF DCC6OrLater}
  Inherited InternalPost;
  {$ENDIF}

  If BlobAutoStartTransaction Then
    Begin
      Try
        If fBlobStartedTransaction Then
          UserTransactionStarted := False
        Else
          UserTransactionStarted := Database.InTransaction;
        TransactionStarted := UserTransactionStarted;
        UserTransactionStarted := UserTransactionStarted And (Not fBlobStartedTransaction);
        If Not UserTransactionStarted Then
          If Not fBlobStartedTransaction Then
            Begin
              Database.StartTransaction;
              TransactionStarted := True;
            End
          Else
            TransactionStarted := True;
        If (BlobMode = bmCache) And (State In [dsEdit, dsInsert]) Then
          Begin
            WriteCacheBlob;
            fListBlobCache.Clear;
          End;

        If (State = dsEdit) Then
          Begin
            If dsRecLockedBeforeEdit = False Then
              Begin
                If ((dsCheckTimeout > 0) Or (dsCheckTimeout = -1)) Then
                  Begin
                    tempTO := Timeout;
                    Timeout := dsCheckTimeout;
                    Try
                      R := dsModifyRecord(Pointer(ActiveBuffer), True);
                    Finally
                      Timeout := tempTO;
                    End;
                  End
                Else
                  R := dsModifyRecord(Pointer(ActiveBuffer), True);
              End
            Else
              R := dsModifyRecord(Pointer(ActiveBuffer), True);
          End
        Else If (State = dsInsert) Then
          Begin
            R := ServerEngine.RecordInsert(CursorID, ffltWriteLock, Pointer(ActiveBuffer), False, aRefNr);
            If r = 0 Then
              Inc(fRecordCount);
          End;
        If r <> 0 Then
          Check(r);

        If TransactionStarted Then
          If Not UserTransactionStarted Then
            //If Database.InTransaction then
            Begin
              fBlobStartedTransaction := False;
              Database.Commit;
            End;
      Except
        If TransactionStarted Then
          Begin
            fBlobStartedTransaction := False;
            If BlobMode = bmCache Then
              fListBlobCache.Clear;
            If Database.InTransaction Then
              Database.Rollback;
          End;
        Raise;
      End;
    End
  Else
    Begin
      If (BlobMode = bmCache) And (State In [dsEdit, dsInsert]) Then
        Begin
          WriteCacheBlob;
          fListBlobCache.Clear;
        End;
      If (State = dsEdit) Then
        Begin
          If dsRecLockedBeforeEdit = False Then
            Begin
              If ((dsCheckTimeout > 0) Or (dsCheckTimeout = -1)) Then
                Begin
                  tempTO := Timeout;
                  Timeout := dsCheckTimeout;
                  Try
                    R := dsModifyRecord(Pointer(ActiveBuffer), True);
                  Finally
                    Timeout := tempTO;
                  End;
                End
              Else
                R := dsModifyRecord(Pointer(ActiveBuffer), True);
            End
          Else
            R := dsModifyRecord(Pointer(ActiveBuffer), True);
        End
      Else If (State = dsInsert) Then
        Begin
          R := ServerEngine.RecordInsert(CursorID, ffltWriteLock, Pointer(ActiveBuffer), False, aRefNr);
          If r = 0 Then Inc(fRecordCount);
        End;
      If r <> 0 Then
        Check(r);
    End;
  If (Self.SupportRecNo) And (State = dsInsert) Then
    Begin
      If (Dictionary.EngineDeleteType = edtUndeleteFull) Then
        fCurrentrecord := RecordCount
      Else
        Begin
          Begin
            //ServerEngine.RecordGetSetPosition(0, Self.cursorid, ffltNoLock, Nil, aFlag, aRecNo, aRefNr, imPosition, false);
            //fCurrentRecord := aRecNo + 1;
          End;
        End;
    End;
End;
{--------}

Procedure TFSDataSet.InternalSetToRecord(aBuffer: PChar);
Begin
  If PfsDataSetRecInfo(aBuffer + dsRecInfoOfs)^.riBookmarkFlag In [bfCurrent, bfInserted] Then
    InternalGotoBookmark(aBuffer + dsBookmarkOfs);
  If Not SupportRecNo Then Exit;

  fCurrentRecord := PfsDataSetRecInfo(aBuffer + dsRecInfoOfs)^.riRecno;

  CursorPosChanged;
End;
{--------}

Function TFSDataSet.IsCursorOpen: Boolean;
Begin
  Result := (CursorID > 0);
End;
{--------}

Function TFSDataSet.IsSequenced: Boolean;
Begin
  Result := Inherited IsSequenced And SupportRecNo;
End;
{--------}

Procedure TFSDataSet.Loaded;
Begin
  dsProxy.Loaded;

  Inherited Loaded;
End;
{--------}

Function TFSBaseTable.Locate(Const aKeyFields: String;
  Const aKeyValues: Variant;
  aOptions: TLocateOptions): Boolean;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  aRecNo: Longword;
Begin
  DoBeforeScroll;
  Result := btLocateRecord(aKeyFields, aKeyValues, aOptions, True);
  If Result Then
    Begin
      If Self.SupportRecNo Then
        Begin
          resync([]);
          If Self.FlipOrder Then
            ServerEngine.RecordGetSetPosition(1, Self.cursorid, ffltNoLock, Nil, aFlag, aRecNo, aRefNr, imPosition, False)
          Else
            ServerEngine.RecordGetSetPosition(-1, Self.cursorid, ffltNoLock, Nil, aFlag, aRecNo, aRefNr, imPosition, False);
          fCurrentRecord := aRecNo + 1;
          resync([]);
        End
      Else
        Resync([rmExact, rmCenter]);
      DoAfterScroll;
    End;
End;
{--------}

Function TFSBaseTable.Lookup(Const aKeyFields: String;
  Const aKeyValues: Variant;
  Const aResultFields: String): Variant;
Begin
  Result := Null;
  If btLocateRecord(aKeyFields, aKeyValues, [], False) Then
    Begin
      SetTempState(dsCalcFields);
      Try
        CalculateFields(TempBuffer);
        Result := FieldValues[aResultFields];
      Finally
        RestoreState(dsBrowse);
      End; {try..finally}
    End;
End;
{--------}

Function TFSDataSet.PackTable(Var aTaskID: Longint; UndeleteRecords: boolean; OnlyDeleted: boolean): TffResult;
Begin
  Result := Database.PackTable(TableName, aTaskID, UndeleteRecords, OnlyDeleted);
End;
{--------}

Procedure TFSDataSet.OpenCursor(aInfoQuery: Boolean);
Begin
  {make sure our database is open first}
  dsEnsureDatabaseOpen(True);
  {open our proxy table}
  dsProxy.Open;
  {create the cursor handle}
  dsCursorID := dsCreateHandle;
  If (CursorID = 0) Then
    RaiseFSErrorObj(Self, fsdse_CantGetTblHandle);
  {call our ancestor (who'll call InternalOpen, where the rest of the
   open process happens)}

  Inherited OpenCursor(aInfoQuery);
End;
{--------}

Function TFSBaseTable.Undelete(aRefresh: boolean): boolean;
Var
  R: Longint;
  aRefNr: TffInt64;
Begin
  Result := False;
  CheckActive;
  If state In [dsinsert, dsedit] Then Exit;
  R := ServerEngine.RecordInsert(CursorID, ffltWriteLock, pointer(activebuffer), True, aRefNr);
  If (r = DBIERR_NONE) Or (r = DBIERR_EOF) Then
    Begin
      Result := (r = DBIERR_NONE);
      If Result And aRefresh Then Refresh;
    End
  Else
    Check(r);
End;

Procedure TFSBaseTable.InternalOpen;
Begin
  btChangeHandleIndex;
  btIgnoreDataEvents := False;
  Inherited InternalOpen;
  UpdateFormatFields;
End;
{--------}

Function TFSDataSet.OverrideFilterEx(aExprTree: FSSrBDE.pCANExpr;
  Const aTimeout: TffWord32): TffResult;
Var
  ExprTree: CANExpr;
Begin
  If Not Assigned(aExprTree) Then
    Begin
      aExprTree := @ExprTree;
      FillChar(ExprTree, SizeOf(ExprTree), 0);
      ExprTree.iVer := CANEXPRVERSION;
      ExprTree.iTotalSize := SizeOf(ExprTree);
    End;

  Result := ServerEngine.CursorOverrideFilter(CursorID,
    aExprTree,
    aTimeout);
End;
{--------}

Procedure TFSBaseTable.Post;
Begin
  Inherited Post;

  If (State = dsSetKey) Then
    Begin {!!.03}
      btEndKeyBufferEdit(True);
      Resync([]); {!!.03}
    End; {!!.03}
End;
{--------}

Function TFSBaseTable.ReIndexTable(Const aIndexNum: Integer;
  Var aTaskID: Longint): TffResult;
Begin
  Result := Database.ReIndexTable(TableName, aIndexNum, aTaskID);
End;
{--------}

Procedure TFSDataSet.RenameTable(Const aNewTableName: String);
Begin
  dsProxy.CheckInactive(True);
  dsEnsureDatabaseOpen(True);
  Try
    Check(ServerEngine.TableRename(Database.DatabaseID,
      TableName,
      aNewTableName));
  Finally
    dsEnsureDatabaseOpen(False);
  End;
  TableName := aNewTableName;
End;
{Begin !!.07}
{--------}

Procedure TFSDataSet.RecordCountAsync(Var TaskID: Longint);
Begin
  CheckActive;
  Check(ServerEngine.TableGetRecCountAsync(CursorID, TaskID));
End;
{End !!.07}
{--------}

Function TFSDataSet.RestoreFilterEx: TffResult;
Begin
  Result := ServerEngine.CursorRestoreFilter(CursorID);
End;
{--------}

Function TFSDataSet.RestructureTable(aDictionary: TFSInfoDict;
  aFieldMap: TStrings;
  Var aTaskID: Longint;
  aRangeError: boolean): TffResult;
Begin
  CheckInactive;
  Result := TFSDatabase(Database).RestructureTable(TableName,
    aDictionary,
    aFieldMap,
    aTaskID,
    aRangeError);
End;
{--------}

Function TFSDataSet.SetFilterEx(aExprTree: FSSrBDE.pCANExpr;
  Const aTimeout: TffWord32): TffResult;
Var
  ExprTree: CANExpr;
Begin
  If Not Assigned(aExprTree) Then
    Begin
      aExprTree := @ExprTree;
      FillChar(ExprTree, SizeOf(ExprTree), 0);
      ExprTree.iVer := CANEXPRVERSION;
      ExprTree.iTotalSize := SizeOf(ExprTree);
    End;

  Result := ServerEngine.CursorSetFilter(CursorID,
    aExprTree,
    aTimeout);
End;
{--------}

Procedure TFSDataSet.SetBookmarkData(aBuffer: PChar; aData: Pointer);
Begin
  Move(aData^, aBuffer[dsBookmarkOfs], BookmarkSize);
End;
{--------}

Procedure TFSDataSet.SetBookmarkFlag(aBuffer: PChar; aValue: TBookmarkFlag);
Begin
  PfsDataSetRecInfo(aBuffer + dsRecInfoOfs).riBookmarkFlag := aValue;
End;
{--------}

Procedure TFSDataSet.SetFieldData(aField: TField; aBuffer: Pointer);
Var
  RecBuf: PChar;
  FDI: TFSFieldDescItem;
  Status: TffResult;
Begin
  With aField Do
    Begin
      If Not (State In dsWriteModes) Then
        RaiseFSErrorObj(Self, fsdse_TblNotEditing);
      If Not GetActiveRecBuf(RecBuf) Then
        RaiseFSErrorObj(Self, fsdse_TblCantGetBuf);
      If (FieldNo > 0) Then
        Begin
          If (State = dsCalcFields) Then
            RaiseFSErrorObj(Self, fsdse_TblCalcFlds);
          If ReadOnly And
            (Not (State In [dsSetKey, dsFilter])) Then
            RaiseFSErrorObj(Self, fsdse_TblReadOnlyEdit);
          Validate(aBuffer);
          If (FieldKind <> fkInternalCalc) Then
            Begin
              If (RecBuf = Nil) Then
                Status := DBIERR_INVALIDPARAM
              Else
                Begin
                  {If (aField.DataType In [ftBCD]) And (aBuffer <> Nil) then
                     Begin
                       BCDToCurr(TBCD(aBuffer^), lTempCurr);
                       aBuffer := @lTempCurr;
                     End ;}
                  If dsGetFieldDescItem(FieldNo, FDI) Then
                    Status := dsTranslatePut(FDI, RecBuf, aBuffer)
                  Else
                    Status := DBIERR_OUTOFRANGE;
                End;
              Check(Status);
            End;
        End
      Else {FieldNo = 0; ie fkCalculated, fkLookup}
        Begin
          inc(RecBuf, dsCalcFldOfs + offset);
          Boolean(RecBuf[0]) := LongBool(aBuffer);
          If Boolean(RecBuf[0]) Then
            Move(aBuffer^, RecBuf[1], DataSize);
        End;
      If Not (State In [dsCalcFields, dsFilter, dsNewValue]) Then
        SetModified(True); //DataEvent(deFieldChange, Longint(aField));
    End;
End;
{--------}

Procedure TFSBaseTable.SetFieldData(aField: TField; aBuffer: Pointer);
Begin
  With aField Do
    Begin
      If (State = dsSetKey) And
        ((FieldNo < 0) Or
        (IndexFieldCount > 0) And (Not IsIndexField)) Then
        RaiseFSErrorObj(Self, fsdse_TblFldNotInIndex);
    End;
  Inherited SetFieldData(aField, aBuffer);
End;
{--------}

Procedure TFSDataSet.SetFiltered(Value: Boolean);
Begin
  If Not Active Then
    Inherited SetFiltered(Value)
  Else
    Begin
      CheckBrowseMode;
      If (Filtered <> Value) Then
        Begin
          If (Not Value) Or dsFilterResync Then
            InternalFirst;
          If Value Then
            dsActivateFilters
          Else
            dsDeactivateFilters;
          Inherited SetFiltered(Value);
          If (Not Value) Or dsFilterResync Then
            First;
        End;
    End;
End;
{--------}

Procedure TFSBaseTable.SetFiltered(Value: Boolean);
Begin
  If Not Active Then
    Inherited SetFiltered(Value)
  Else
    Begin
      CheckBrowseMode;
      If (Filtered <> Value) Then
        Begin
          btDestroyLookupCursor;
          Inherited SetFiltered(Value);
        End;
    End;
End;
{Begin !!.03}
{--------}

Procedure TFSBaseTable.dsActivateFilters;
Begin
  Inherited;
  btDestroyLookupCursor;
End;
{--------}

Procedure TFSBaseTable.dsDeactivateFilters;
Begin
  Inherited;
  btDestroyLookupCursor;
End;
{End !!.03}
{--------}

Procedure TFSDataSet.SetFilterOptions(Value: TFilterOptions);
Begin
  dsSetFilterTextAndOptions(Filter, Value, dsFilterEval,
    dsFilterTimeOut);
End;
{--------}

Procedure TFSDataSet.SetFilterText(Const Value: String);
Begin
  dsSetFilterTextAndOptions(Value, FilterOptions, dsFilterEval,
    dsFilterTimeOut);
  { If the new filter string is blank, we may need to reset the Filtered flag }
  If (Value = '') And Filtered Then
    Filtered := False;
End;
{--------}

Procedure TFSBaseTable.SetKey;
Begin
  btSetKeyBuffer(ketNormal, True);
End;
{--------}

Procedure TFSDataSet.SetName(Const NewName: TComponentName);
Begin
  Inherited SetName(NewName);

  dsProxy.Name := NewName + '_Proxy';
End;
{--------}

Procedure TFSDataSet.SetOnFilterRecord(Const Value: TFilterRecordEvent);
Begin
  {if there is no change there's nothing to do}
  If (@Value = @OnFilterRecord) Then
    Exit;
  {if the table is active...}
  If Active Then
    Begin
      CheckBrowseMode;
      {firstly drop the current function filter}
      If (dsFuncFilter <> Nil) Then
        Begin
          Check(dsDropFilter(dsFuncFilter));
          dsFuncFilter := Nil;
        End;
      {if the filter function is not nil...}
      If Assigned(Value) Then
        Begin
          {add the new function}
          dsAddFuncFilter(@TFSBaseTable.dsOnFilterRecordCallback);
          {activate it}
          If Filtered Then
            Check(dsActivateFilter(dsFuncFilter));
        End;

      {call our ancestor}
      Inherited SetOnFilterRecord(Value);

      {if the table is being filtered, go to the start}
      If Filtered Then
        First;
    End
  Else {table is not active}
    Begin
      {call our ancestor}
      Inherited SetOnFilterRecord(Value);
    End;
End;
{--------}

Procedure TFSBaseTable.SetRange(Const aStartValues, aEndValues: Array Of Const);
Begin
  CheckBrowseMode;
  btSetKeyFields(ketRangeStart, aStartValues);
  btSetKeyFields(ketRangeEnd, aEndValues);
  ApplyRange;
End;
{--------}

Procedure TFSBaseTable.SetRangeEnd;
Begin
  btSetKeyBuffer(ketRangeEnd, True);
End;
{--------}

Procedure TFSBaseTable.SetRangeStart;
Begin
  btSetKeyBuffer(ketRangeStart, True);
End;
{--------}

Function TFSDataSet.SetTableAutoIncValue(Const aValue: Int64; Const aStep: Longint): TffResult;
Begin
  Result := ServerEngine.TableSetAutoInc(CursorID,
    aValue, aStep);
End;

Function TFSDataSet.GetTableAutoIncValue(Var aValue: Int64; Var aStep: Longint): TffResult;
Begin
  Result := ServerEngine.TableGetAutoInc(CursorID,
    aValue, aStep);
End;

Function TFSDataSet.ReadLastAutoInc: Int64;
Var
  aValue: Int64;
  aStep: Longint;
  r: TffResult;

Begin
  r := ServerEngine.TableGetAutoInc(CursorID,
    aValue,
    aStep);
  Check(r);
  Result := aValue;
End;

Function TFSDataSet.SetTableMaxRecordsValue(Const aValue: Longword): TffResult;
Begin
  Result := ServerEngine.TableSetMaxRecords(CursorID, aValue);
End;

Function TFSDataSet.GetTableMaxRecordsValue(Var aValue: Longword): TffResult;
Begin
  Result := ServerEngine.TableGetMaxRecords(CursorID, aValue);
End;

Function TFSDataSet.SetTableFlagsValue(Const aValue: Word): TffResult;
Begin
  Result := ServerEngine.TableSetTableFlags(CursorID, aValue);
End;

Function TFSDataSet.GetTableFlagsValue(Var aValue: Word): TffResult;
Begin
  Result := ServerEngine.TableGetTableFlags(CursorID, aValue);
End;

Function TFSDataSet.SetTablePassword(aOldValue, aNewValue: String): TffResult;
Var
  S, S1: String;
  Inew, Iold, IOldTable: Longword;
Begin
  S := Trim(aNewValue);
  S1 := Trim(aOldValue);
  Inew := FSCalcShStrELFHash(S);
  Iold := FSCalcShStrELFHash(S1);

  Result := getTablePassword(IOldTable);
  Check(Result);

  If (IOldTable = 0) Or (Iold = IOldTable) Then
    Result := ServerEngine.TableSetTablePassword(CursorID, Inew)
  Else
    Check(50100);
End;

Function TFSDataSet.GetTablePassword(Var aValue: Longword): TffResult;
Begin
  Result := ServerEngine.TableGetTablePassword(CursorID, aValue);
End;

Function TFSDataSet.SetTablePasswordRest(aOldValue, aNewValue: String): TffResult;
Var
  S, S1: String;
  Inew, Iold, IOldTable: Longword;
Begin
  S := Trim(aNewValue);
  S1 := Trim(aOldValue);
  Inew := FSCalcShStrELFHash(S);
  Iold := FSCalcShStrELFHash(S1);

  Result := getTablePasswordRest(IOldTable);
  Check(Result);

  If (IOldTable = 0) Or (Iold = IOldTable) Then
    Result := ServerEngine.TableSetTablePasswordRest(CursorID, Inew)
  Else
    Check(50100);
End;

Function TFSDataSet.GetTablePasswordRest(Var aValue: Longword): TffResult;
Begin
  Result := ServerEngine.TableGetTablePasswordRest(CursorID, aValue);
End;

{Function TFSDataSet.SetTableDBIDValue(Const aValue: Longword): TffResult;
Begin
  Result := ServerEngine.TableSetTableDBID(CursorID, aValue);
End ;}

Function TFSDataSet.GetTableDBIDValue(Var aValue: Longword): TffResult;
Begin
  Result := ServerEngine.TableGetTableDBID(CursorID, aValue);
End;

{--------}

Function TFSDataSet.Exists: Boolean;
Begin
  Result := Active;
  If Result Or (TableName = '') Then
    Exit;

  dsEnsureDatabaseOpen(True); {!!.11}
  Result := Database.TableExists(TableName);
End;
{--------}

Procedure TFSDataSet.dsActivateFilters;
Begin
  {activate the server side filter}
  If (dsFilterEval = fseServer) Then
    dsSetServerSideFilter(Filter, FilterOptions, dsFilterTimeOut);

  {activate the expression filter}
  If (dsExprFilter <> Nil) Then
    Begin
      Check(dsActivateFilter(dsExprFilter));
    End;

  {activate the function filter}
  If (dsFuncFilter <> Nil) Then
    Begin
      Check(dsActivateFilter(dsFuncFilter));
    End;
End;
{--------}

Procedure TFSDataSet.dsAddExprFilter(Const aText: String;
  Const aOpts: TFilterOptions);
Var
  Parser: TfsExprParser;
Begin
  Parser := TfsExprParser.Create(Self, aText, aOpts, [poExtSyntax], '', Nil, fsFldTypeMap);
  Try
    Check(dsAddFilter(0, 0, False,
      PCANExpr(Parser.FilterData),
      Nil, dsExprFilter));
  Finally
    Parser.Free;
  End;
End;
{--------}

Procedure TFSDataSet.dsAddFieldDesc(aFieldDesc: PffFieldDescriptor;
  aFieldNo: Integer);
Var
  BDEType: Word;
  BDESize: Longint;
  VCLType: TFieldType;
  {$IFDEF CBuilder3}
  FieldDef: TFieldDef;
  {$ENDIF}
Begin
  With aFieldDesc^ Do
    Begin
      MapffTypeToBDE(fdType, fdLength, BDEType, BDESize);
      If BDEType In [fldIntArray, fldWordArray, fldDoubleArray, fldByteArray] Then
        VCLType := ftBytes
      Else
        VCLType := fsDataTypeMap[BDEType];

      If (VCLType <> ftUnknown) Then
        Begin
          If (VCLType <> ftString) And
            (VCLType <> ftBytes) And
            (VCLType <> ftBCD) And
            (VCLType <> ftWideString) Then
            BDESize := 0;

          {$IFDEF CBuilder3}
          FieldDef := TFieldDef.Create(FieldDefs);
          FieldDef.Name := fdName;
          FieldDef.DataType := VCLType;
          FieldDef.Size := BDESize;
          FieldDef.Required := fdRequired;
          FieldDef.FieldNo := aFieldNo;
          {$ELSE}
          TFieldDef.Create(FieldDefs,
            fdName,
            VCLType,
            BDESize,
            fdRequired,
            aFieldNo);
          {$ENDIF}
        End;
    End;
End;
{--------}

Procedure TFSDataSet.dsAddFuncFilter(aFilterFunc: pfGENFilter);
Begin
  Check(dsAddFilter(Integer(Self), 0, False, Nil, aFilterFunc, dsFuncFilter));
End;
{--------}

Function TFSDataSet.dsCancelServerFilter: Boolean;
Begin
  Result := False;
  If Assigned(dsOnServerFilterTimeout) Then
    dsOnServerFilterTimeout(Self, Result);
End;
{------}

Procedure TFSDataSet.dsAllocKeyBuffers;
Var
  i: TfsKeyEditType;
Begin
  FFGetMem(btKeyBuffers, sizeof(Pointer) * succ(ord(High(TfsKeyEditType))));
  For i := Low(TfsKeyEditType) To High(TfsKeyEditType) Do
    Begin
      FFGetMem(PKeyBuffers(btKeyBuffers)^[i], btKeyBufSize);
      btInitKeyBuffer(PKeyBuffers(btKeyBuffers)^[i]);
    End;
End;
{--------}

Procedure TFSDataSet.btFreeKeyBuffers;
Var
  i: TfsKeyEditType;
Begin
  If (btKeyBuffers <> Nil) Then
    Begin
      For i := Low(TfsKeyEditType) To High(TfsKeyEditType) Do
        Begin
          If (PKeyBuffers(btKeyBuffers)^[i] <> Nil) Then
            FFFreeMem(PKeyBuffers(btKeyBuffers)^[i], btKeyBufSize);
        End;
      FFFreeMem(btKeyBuffers, sizeof(Pointer) * succ(ord(High(TfsKeyEditType))));
      btKeyBuffers := Nil;
    End;
  btKeyBuffer := Nil;
End;
{--------}

Procedure TFSBaseTable.btChangeHandleIndex;
Var
  IdxName: String;
Begin
  btIndexDefs.Updated := False;
  If btIndexByName Then
    btRetrieveIndexName(btIndexName, True, IdxName)
  Else
    btRetrieveIndexName(btIndexFieldStr, False, IdxName);
  If (IdxName <> '') Then
    Begin
      Try
        btSwitchToIndexEx(CursorID, IdxName, btIndexID, False);
      Except
        Check(ServerEngine.CursorClose(CursorID));
        TableState := TblClosed;
        dsCursorID := 0;
        btRangeStack.Clear;
        Raise;
      End;
    End;
End;
{--------}

Procedure TFSBaseTable.btCheckKeyEditMode;
Begin
  If (State <> dsSetKey) Then
    RaiseFSErrorObj(Self, fsdse_TblChkKeyNoEdit)
End;
{--------}

Procedure TFSBaseTable.dsCheckMasterRange;
Begin
  If btMasterLink.Active And (btMasterLink.Fields.Count > 0) Then
    Begin
      btSetLinkRange(btMasterLink.Fields);
      btSetRange;
    End;
End;
{--------}

Procedure TFSDataSet.dsClearServerSideFilter;
Begin
  SetFilterEx(Nil, 0);
End;
{--------}

Procedure TFSDataSet.dsCloseViaProxy;
Begin
  If Not dsClosing Then
    Close;
End;
{--------}

Function TFSDataSet.dsCreateHandle: TffCursorID;
Begin
  If (TableName = '') Then
    RaiseFSErrorObj(Self, fsdse_TblNoName);
  Result := GetCursorHandle('');
End;
{--------}

Function TFSDataSet.dsCreateLookupFilter(aFields: TList;
  Const aValues: Variant;
  aOptions: TLocateOptions): HDBIFilter;
Var
  i: Integer;
  Filter: TfsFilterExpr;
  Tree: PExprNode;
  Node: PExprNode;
  FilterOptions: TFilterOptions;
Begin
  {calculate the filter options}
  If (loCaseInsensitive In aOptions) Then
    FilterOptions := [foNoPartialCompare, foCaseInsensitive]
  Else
    FilterOptions := [foNoPartialCompare];
  {create the filter expression tree}
  Filter := TfsFilterExpr.Create(Self, FilterOptions, [poExtSyntax], '', Nil, fsFldTypeMap);

  Try
    {add the nodes}
    {if there's just one field value, do it separately}
    If (aFields.Count = 1) Then
      Begin
        Node := Filter.NewCompareNode(TField(aFields[0]), coEQ, aValues);
        Tree := Node;
      End
        {if there are more than one, create a properly linked tree}
    Else
      Begin
        Node := Filter.NewCompareNode(TField(aFields[0]), coEQ, aValues[0]);
        Tree := Node;
        For i := 1 To pred(aFields.Count) Do
          Begin
            Node := Filter.NewCompareNode(TField(aFields[i]), coEQ, aValues[i]);
            Tree := Filter.NewNode(enOperator, coAND, UnAssigned, Tree, Node);
          End;
      End;
    {if we have a partial match make sure the final node agrees}
    If (loPartialKey In aOptions) Then
      Node^.FPartial := True;

    {add the filter}
    If FilterEval = fseServer Then
      Check(OverrideFilterEx(FSSrBDE.pCANExpr(Filter.GetFilterData(Tree)),
        FilterTimeOut))
    Else
      Begin
        Check(dsAddFilter(0, 0, False,
          PCANExpr(Filter.GetFilterData(Tree)),
          Nil, Result));
        dsActivateFilter(Result);
      End;

  Finally
    Filter.Free;
  End; {try..finally}
End;
{--------}

Procedure TFSDataSet.dsDeactivateFilters;
Begin
  {deactivate the server side filter}
  If (dsFilterEval = fseServer) Then
    dsClearServerSideFilter;

  {deactivate the expression filter}
  If (dsExprFilter <> Nil) Then
    Begin
      Check(dsDeactivateFilter(dsExprFilter));
    End;
  {deactivate the function filter}
  If (dsFuncFilter <> Nil) Then
    Begin
      Check(dsDeactivateFilter(dsFuncFilter));
    End;
End;
{--------}

Procedure TFSBaseTable.btDecodeIndexDesc(Const aIndexDesc: IDXDesc;
  Var aName, aFields: String;
  Var aOptions: TIndexOptions);
Var
  IndexOptions: TIndexOptions;
  i: Integer;
Begin
  With aIndexDesc Do
    Begin
      {get name}
      aName := szName;
      {get index options - use local variable for speed}
      IndexOptions := [];
      If bPrimary Then
        Include(IndexOptions, ixPrimary);
      If bUnique Then
        Include(IndexOptions, ixUnique);
      If bDescending Then
        Include(IndexOptions, ixDescending);
      If bCaseInsensitive Then
        Include(IndexOptions, ixCaseInsensitive);
      If bExpIdx Or (iFldsInKey = 0) Then
        Include(IndexOptions, ixExpression);
      aOptions := IndexOptions;
      {get index fields}
      If (iFldsInKey = 0) Then
        aFields := ''
      Else {more than one field in index key}
        Begin
          aFields := FieldDefs[pred(aiKeyFld[0])].Name;
          For i := 1 To pred(iFldsInKey) Do
            aFields := aFields + ';' +
              FieldDefs[pred(aiKeyFld[i])].Name;
        End;
    End;
End;
{--------}

Procedure TFSDataSet.DestroyHandle(aHandle: TffCursorID);
Begin
  {release record lock, ignore errors}
  Check(ServerEngine.RecordRelLock(CursorID,
    False));
  {close the cursor handle, ignore errors}
  Check(ServerEngine.CursorClose(CursorID));
  TableState := TblClosed;
  dsCursorID := 0;
End;
{--------}

Procedure TFSBaseTable.DestroyHandle(aHandle: TffCursorID);
Begin
  {destroy the lookup cursor (if there is one)}
  btDestroyLookupCursor;

  Inherited DestroyHandle(aHandle);

  btRangeStack.Clear;
End;
{--------}

Procedure TFSBaseTable.btDestroyLookupCursor;
Begin
  If (btLookupCursorID > 0) Then
    Begin
      Check(ServerEngine.CursorClose(btLookupCursorID));
      btLookupCursorID := 0;
      btLookupKeyFields := '';
      btLookupNoCase := False;
    End;
End;
{--------}

Function TFSBaseTable.btDoFldsMapToCurIdx(aFields: TList;
  aNoCase: Boolean): Boolean;
Var
  i: Integer;
Begin
  {returns whether the field list matches the current index fields}
  {assume not}
  Result := False;

  {if the case sensitivity doesn't match, exit}
  If (aNoCase <> btNoCaseIndex) Then
    Exit;
  {if the field count is larger than the index's, exit}
  If (aFields.Count > btIndexFieldCount) Then
    Exit;
  {check that all fields match}
  For i := 0 To pred(aFields.Count) Do
    If (TField(aFields[i]).FieldNo <> btFieldsInIndex[i]) Then
      Exit;
  {if we got this far, the field list is the same as the index's}
  Result := True;
End;
{--------}

Function TFSDataSet.dsGetFieldDescItem(iField: Integer;
  Var FDI: TFSFieldDescItem): Boolean;
Begin
  If (FieldDescs.Count = 0) Then
    dsReadFieldDescs;
  If (0 < iField) And (iField <= FieldDescs.Count) Then
    Begin
      Result := True;
      FDI := TFSFieldDescItem(FieldDescs[pred(iField)]);
    End
  Else {iField is out of range}
    Begin
      Result := False;
      FDI := Nil;
    End;
End;
{--------}

Function TFSDataSet.dsGetFieldNumber(FieldName: PChar): Integer;
Var
  i: Integer;
  FDI: TFSFieldDescItem;
Begin
  Result := 0;
  If (FieldDescs.Count <> 0) Then
    Begin
      For i := 0 To pred(FieldDescs.Count) Do
        Begin
          FDI := TFSFieldDescItem(FieldDescs.Items[i]);
          If (FFAnsiStrIComp(FieldName, FDI.PhyDesc^.szName) = 0) Then
            Begin {!!.06, !!.07}
              Result := FDI.FieldNumber;
              Exit;
            End;
        End;
    End;
End;
{--------}

Procedure TFSDataSet.dsReadFieldDescs;
Var
  ffFieldDesc: PffFieldDescriptor;
  BDEPhyDesc: FLDDesc;
  i: Integer;
  offset: Integer;
Begin
  {destroy any existing field desc items}
  For i := Pred(FieldDescs.Count) Downto 0 Do
    TFSFieldDescItem(FieldDescs.Items[i]).Free;

  {create a bunch of field desc items}
  For i := 0 To pred(Dictionary.FieldCount) Do
    Begin
      ffFieldDesc := Dictionary.FieldDescriptor[i];
      GetBDEFieldDescriptor(ffFieldDesc^, BDEPhyDesc);
      {note: the line below adds the new item automatically to the
             collection}
      TFSFieldDescItem.Create(FieldDescs, BDEPhyDesc);
    End;
  {Now patch up the offsets for the logical field descs}
  offset := 0;
  For i := 0 To pred(Dictionary.FieldCount) Do
    Begin
      With TFSFieldDescItem(FieldDescs[i]).LogDesc^ Do
        Begin
          ioffset := offset;
          inc(offset, iLen);
        End;
    End;
End;
{--------}

Function TFSDataSet.dsTranslateCmp(Var aFirst: TfsNodeValue;
  Var aSecond: TfsNodeValue;
  aIgnoreCase: Boolean;
  aPartLen: Integer): Integer;
{------}

  Function ConvertIntValue(Var aNode: TfsNodeValue; Var C: Int64): boolean;
  Begin
    Result := True;
    With aNode Do
      Begin
        If nvIsConst Then
          Begin
            Case nvDType Of
              fldByte: C := Int64(nvValue^);
              fldWord16: C := Int64(nvValue^);
              fldWord32: C := Int64(nvValue^);
              fldInt8: C := Int64(nvValue^);
              fldInt16: C := Int64(nvValue^);
              fldInt32, fldAutoInc32: C := Int64(nvValue^);
              fldInt64, fldAutoInc64, fldRecVersion: C := Int64(nvValue^);
              Else
                Result := False;
            End; {case}
          End
        Else
          Begin
            Case TfsFieldType(nvDType) Of
              fstUInt8: C := Byte(nvValue^);
              fstUInt16: C := Word(nvValue^);
              fstUInt32: C := Longword(nvValue^);
              fstInt8: C := Shortint(nvValue^);
              fstInt16: C := Smallint(nvValue^);
              fstInt32, fstAutoInc32: C := Longint(nvValue^);
              fstInt64, fstAutoInc64, fstRecVersion: C := Int64(nvValue^);
              Else
                Result := False;
            End; {case}
          End;
      End;
  End;
  {------}

  Function ConvertDateTimeValue(Var aNode: TfsNodeValue; Var DT: TDateTime): boolean;
  Begin
    Result := True;
    With aNode Do
      Begin
        If nvIsConst Then
          Begin
            Case nvDType Of
              fldDATE: DT := DbiDate(nvValue^);
              fldTIME: DT := DbiTime(nvValue^) / 86400000.0;
              fldDateTime: DT := TimeStamp(nvValue^) / 86400000.0;
              Else
                Result := False;
            End; {case}
          End
        Else
          Begin
            Case TfsFieldType(nvDType) Of
              fstDate: DT := StDateToDateTime(TStDate(nvValue^))
                + 693594;
              fstTime: DT := StTimeToDateTime(TStTime(nvValue^));
              fstDateTime:
                Begin
                  DT := TDateTime(nvValue^);
                  fsSetMillisecond(dt, 0);
                End;
              Else
                Result := False;
            End; {case}
          End;
      End;
  End;
  {------}

  Function ConvertFloatValue(Var aNode: TfsNodeValue; Var F: Extended): boolean;
  Begin
    Result := True;
    With aNode Do
      Begin
        If nvIsConst Then
          Begin
            Case nvDType Of
              fldSingle: F := Extended(nvValue^);
              fldDouble: F := Extended(nvValue^);
              fldExtended: F := Extended(nvValue^);
              Else
                Result := False;
            End; {case}
          End
        Else
          Begin
            Case TfsFieldType(nvDType) Of
              fstSingle: F := Single(nvValue^);
              fstDouble: F := Double(nvValue^);
              fstExtended: F := Extended(nvValue^);
              Else
                Result := False;
            End; {case}
          End;
      End;
  End;
  {------}

  Function ConvertBcdValue(Var aNode: TfsNodeValue; Var F: Currency): boolean;
  Begin
    Result := True;
    With aNode Do
      Begin
        If nvIsConst Then
          Begin
            Case nvDType Of
              fldBcd,
                fldCurrency: F := Currency(nvValue^);
              Else
                Result := False;
            End; {case}
          End
        Else
          Begin
            Case TfsFieldType(nvDType) Of
              fstBcd, fstCurrency:
                Begin
                  F := Currency(nvValue^);
                End;
              Else
                Result := False;
            End; {case}
          End;
      End;
  End;
  {------}

  Function ConvertBooleanValue(Var aNode: TfsNodeValue; Var B: boolean): boolean;
  Begin
    Result := True;
    With aNode Do
      Begin
        If nvIsConst Then
          Begin
            Case nvDType Of
              fldBOOLean: B := WordBool(nvValue^);
              Else
                Result := False;
            End; {case}
          End
        Else
          Begin
            Case TfsFieldType(nvDType) Of
              fstBoolean: B := boolean(nvValue^);
              Else
                Result := False;
            End; {case}
          End;
      End;
  End;
  {------}

  Function ConvertStringValue(Var aNode: TfsNodeValue; Var P: PChar): boolean;
  Var
    StrZ: TffStringZ;
    WorkString: String;
    Len: Integer;

    Function ArrayAsString: String;

      Function ByteArrayToString(ByteArray: Pointer; ArrayLength: Integer): String;
      Var
        idx: Integer;
        BArr: PffByteArray Absolute ByteArray;
      Begin
        Result := '';
        Result := IntToStr(BArr[0]);
        For idx := 1 To ArrayLength - 1 Do
          Result := Result + ',' + IntToStr(BArr[idx]);
      End;

      Function WordArrayToString(WordArray: Pointer; ArrayLength: Integer): String;
      Var
        idx: Integer;
        BArr: PffWordArray Absolute WordArray;
      Begin
        Result := '';
        Result := IntToStr(BArr[0]);
        For idx := 1 To ArrayLength - 1 Do
          Result := Result + ',' + IntToStr(BArr[idx]);
      End;

      Function IntegerArrayToString(IntArray: Pointer; ArrayLength: Integer): String;
      Var
        idx: Integer;
        BArr: PffIntArray Absolute intArray;
      Begin
        Result := '';
        Result := IntToStr(BArr[0]);
        For idx := 1 To ArrayLength - 1 Do
          Result := Result + ',' + IntToStr(BArr[idx]);
      End;

      Function DoubleArrayToString(DoubleArray: Pointer; ArrayLength: Integer; Decimal: Byte): String;
      Var
        idx: Integer;
        BArr: PffDoubleArray Absolute DoubleArray;
        S: String;
        D: Extended;
        c: Char;
        r: TRound;
      Begin
        Result := '';
        D := BArr[0];
        c := DecimalSeparator;
        DecimalSeparator := '.';
        r := rMathematical;
        Try
          If decimal > 0 Then
            Begin
              d := RoundExtended(d, decimal, r);
              S := fsFloatToStrF(D, ffFixed, 20, decimal);
            End
          Else
            S := fsFloatToStr(D);
          Result := S;
          For idx := 1 To ArrayLength - 1 Do
            Begin
              D := BArr[idx];
              If decimal > 0 Then
                Begin
                  d := RoundExtended(d, decimal, r);
                  S := fsFloatToStrF(D, ffFixed, 20, decimal);
                End
              Else
                S := fsFloatToStr(D);
              Result := Result + ',' + S;
            End;
        Finally
          DecimalSeparator := c;
        End;
      End;

    Begin
      If TfsFieldType(aNode.nvDType) = fstArrayUInt8 Then
        Result := ByteArrayToString(aNode.nvValue, anode.nvSize)
      Else If TfsFieldType(aNode.nvDType) = fstArrayUInt16 Then
        Result := WordArrayToString(aNode.nvValue, anode.nvSize Div 2)
      Else If TfsFieldType(aNode.nvDType) = fstArrayInt32 Then
        Result := IntegerArrayToString(aNode.nvValue, anode.nvSize Div 4)
      Else If TfsFieldType(aNode.nvDType) = fstArrayDouble Then
        Result := DoubleArrayToString(aNode.nvValue, anode.nvSize Div 8, 0)
      Else
        Result := '';
    End;
  Begin
    Result := True;
    With aNode Do
      Begin
        If nvIsConst Then
          Begin
            Case nvDType Of
              fldSingleChar, fldShortString,
                fldVarNullString, fldNullString,
                fldByteArray, fldWordArray..fldDoubleArray: P := nvValue;
              Else
                Result := False;
            End; {case}
          End
        Else
          Begin
            Case TfsFieldType(nvDType) Of
              fstSingleChar:
                Begin
                  P := StrAlloc(2);
                  P[0] := char(nvValue^);
                  P[1] := #0;
                End;
              fstShortString:
                Begin
                  P := StrNew(StrPCopy(StrZ, TffShStr(nvValue^)));
                End;
              fstNullString,
                fstVarNullString:
                Begin
                  P := StrNew(nvValue);
                End;
              fstSingleWideChar:
                Begin
                  P := StrAlloc(2);
                  len := lstrlenw(PWideChar(nvValue^));
                  WorkString := WideCharLenToString(PWideChar(nvValue^), len);
                  P[0] := WorkString[1];
                  P[1] := #0;
                End;
              fstWideString, fstVarWideString:
                Begin
                  len := lstrlenw(PWideChar(nvValue));
                  P := StrAlloc(len);
                  WorkString := WideCharLenToString(PWideChar(nvValue), len);
                  StrPLCopy(P, WorkString, len);
                End;
              fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble:
                Begin
                  P := StrNew(StrPCopy(StrZ, ArrayAsString));
                End;
              {fstUnicode:
                Begin

                End ; }
              Else
                Result := False;
            End; {case}
          End;
      End;
  End;

  {------}
Var
  Bool1, Bool2: boolean;
  Comp1, Comp2: Int64;
  PChar1, PChar2: PAnsiChar;
  DT1, DT2: TDateTime;
  Ext1, Ext2: Extended;
  Cur1, Cur2: Currency;
Begin
  {Note: there are two types of things to compare: constants and
         fields. In neither case will this routine be called with null
         values - the caller takes care of this}
  {Note: this routine doesn't have to worry about comparing dissimilar
         types (eg dates and strings); this is illegal and will have
         been already excluded by the filter parser; similarly with
         fields that can't be compared (eg, BLOBs)}
  {Note: constant values are stored as logical types, field values as
         physical types}

  {Deal with integer types first}
  If ConvertIntValue(aFirst, Comp1) Then
    Begin
      ConvertIntValue(aSecond, Comp2);
      If (Comp1 < Comp2) Then
        Result := -1
      Else If (Comp1 = Comp2) Then
        Result := 0
      Else
        Result := 1;
      Exit;
    End;

  {Deal with floating point types next}
  If ConvertFloatValue(aFirst, Ext1) Then
    Begin
      ConvertFloatValue(aSecond, Ext2);
      If (Ext1 < Ext2) Then
        Result := -1
      Else If (Ext1 = Ext2) Then
        Result := 0
      Else
        Result := 1;
      Exit;
    End;

  {Deal with floating point types next}
  If ConvertBcdValue(aFirst, Cur1) Then
    Begin
      ConvertBcdValue(aSecond, Cur2);
      If (Cur1 < Cur2) Then
        Result := -1
      Else If (Cur1 = Cur2) Then
        Result := 0
      Else
        Result := 1;
      Exit;
    End;

  {Deal with date/time types next}
  If ConvertDateTimeValue(aFirst, DT1) Then
    Begin
      ConvertDateTimeValue(aSecond, DT2);
      If (DT1 < DT2) Then
        Result := -1
      Else If (DT1 = DT2) Then
        Result := 0
      Else
        Result := 1;
      Exit;
    End;

  {Deal with boolean types next; false < true}
  If ConvertBooleanValue(aFirst, Bool1) Then
    Begin
      ConvertBooleanValue(aSecond, Bool2);
      If Bool1 Then
        If Bool2 Then
          Result := 0
        Else
          Result := 1
      Else {Bool1 is false}  If Bool2 Then
        Result := -1
      Else
        Result := 0;
      Exit;
    End;

  {Deal with strings next}
  If ConvertStringValue(aFirst, PChar1) Then
    Begin
      ConvertStringValue(aSecond, PChar2);
      If aIgnoreCase Then
        If (aPartLen = 0) Then
          Result := FFAnsiStrIComp(PChar1, PChar2) {!!.06} {!!.07}
        Else
          Result := FFAnsiStrLIComp(PChar1, PChar2, aPartLen) {!!.06} {!!.07}
      Else If (aPartLen = 0) Then
        Result := AnsiStrComp(PChar1, PChar2) {!!.06}
      Else
        Result := AnsiStrLComp(PChar1, PChar2, aPartLen); {!!.06}
      If Not aFirst.nvIsConst Then
        StrDispose(PChar1);
      If Not aSecond.nvIsConst Then
        StrDispose(PChar2);
      Exit;
    End;

  {otherwise just compare the bytes}
  Result := FFCmpBytes(Pointer(aFirst.nvValue),
    Pointer(aSecond.nvValue),
    FFMinI(aFirst.nvSize, aSecond.nvSize));
End;
{------}

Function TFSDataSet.dsTranslateGet(FDI: TFSFieldDescItem;
  pRecBuff: Pointer;
  pDest: Pointer;
  Var bBlank: Boolean): TffResult;
Begin
  Result := DBIERR_NONE;
  If (pRecBuff = Nil) Then
    Result := DBIERR_INVALIDPARAM
  Else {pRecBuff is non-nil}
    Begin
      bBlank := Dictionary.IsRecordFieldNull(pred(FDI.FieldNumber), pRecBuff);
      If (pDest = Nil) Then
        Result := DBIERR_NONE
      Else {there is somewhere to xlat data into, if needed}
        Begin
          If bBlank Then
            Begin
              Result := DBIERR_NONE;
              If (XltMode = xltField) Then
                FillChar(pDest^, FDI.LogDesc^.iLen, 0)
              Else {no translation}
                FillChar(pDest^, FDI.PhyDesc^.iLen, 0)
            End
          Else {field is not blank}
            Begin
              If (XltMode <> xltField) {no translation} Then
                Begin
                  With FDI.PhyDesc^ Do
                    Move(PffByteArray(pRecBuff)^[ioffset], pDest^, iLen);
                End
              Else {field must be translated}
                Begin
                  With FDI.PhyDesc^ Do
                    Begin
                      inc(PAnsiChar(pRecBuff), ioffset);
                      If MapffDataToBDE(TfsFieldType(iFldType),
                        iLen,
                        pRecBuff,
                        pDest) Then
                        Result := DBIERR_NONE
                      Else
                        Result := DBIERR_INVALIDXLATION;
                    End;
                End;
            End;
        End;
    End;
End;
{--------}

Function TFSDataSet.dsTranslatePut(FDI: TFSFieldDescItem;
  pRecBuff: Pointer;
  pSrc: Pointer): TffResult;
Begin
  If (pRecBuff = Nil) Then
    Result := DBIERR_INVALIDPARAM
  Else {pRecBuff is non-nil}
    Begin
      If (pSrc = Nil) {this means set field to null} Then
        Begin
          Dictionary.SetRecordFieldNull(pred(FDI.FieldNumber), pRecBuff, True);
          Result := DBIERR_NONE;
        End
      Else {pSrc is non-nil}
        Begin
          Dictionary.SetRecordFieldNull(pred(FDI.FieldNumber), pRecBuff, False);
          If (XltMode <> xltField) {no translation} Then
            Begin
              With FDI.PhyDesc^ Do
                Move(pSrc^, PffByteArray(pRecBuff)^[ioffset], iLen);
              Result := DBIERR_NONE;
            End
          Else {field must be translated}
            Begin
              With FDI.PhyDesc^ Do
                Begin
                  inc(PAnsiChar(pRecBuff), ioffset);
                  If MapDataToFS(TfsFieldType(iFldType), iLen, pSrc, pRecBuff) Then
                    Result := DBIERR_NONE
                  Else
                    Result := DBIERR_INVALIDXLATION;
                End;
            End;
        End;
    End;
End;
{--------}

Procedure TFSDataSet.dsDropFilters;
Begin
  {drop the expression filter}
  If (dsExprFilter <> Nil) Then
    Begin
      Check(dsDropFilter(dsExprFilter));
      dsExprFilter := Nil;
    End;
  {drop the function filter}
  If (dsFuncFilter <> Nil) Then
    Begin
      Check(dsDropFilter(dsFuncFilter));
      dsFuncFilter := Nil;
    End;
End;
{--------}

Function TFSDataSet.dsMatchesFilter(pRecBuff: Pointer): Boolean;
Var
  i: Integer;
  Filt: TFSFilterListItem;
Begin
  Result := False;
  If (pRecBuff = Nil) Then
    Exit;
  If dsFilterActive Then
    Begin
      For i := 0 To pred(dsFilters.Count) Do
        Begin
          Filt := TFSFilterListItem(dsFilters.Items[i]);
          If (Filt <> Nil) Then
            If Not Filt.MatchesRecord(pRecBuff) Then
              Exit;
        End;
    End;
  Result := True;
End;
{--------}

Procedure TFSBaseTable.btEndKeyBufferEdit(aCommit: Boolean);
Begin
  DataEvent(deCheckBrowseMode, 0);
  If aCommit Then
    PfsKeyRecInfo(PChar(btKeyBuffer) + btKeyInfoOfs)^.kriModified := Modified
  Else {rollback}
    Move(PKeyBuffers(btKeyBuffers)^[ketSaved]^, btKeyBuffer^, btKeyBufSize);
  SetState(dsBrowse);
  DataEvent(deDataSetChange, 0);
End;
{--------}

Procedure TFSDataSet.dsEnsureDatabaseOpen(aValue: Boolean);
{Note: this routine exists in order that the table object can ensure
       that it's database parent is open before something happens
       that requires it open. For example, you can get an index list
       for a table before opening it - to do this requires that the
       database is opened automatically first. }
Var
  DB: TFSDatabase;
Begin
  If (dsProxy.Session = Nil) Then
    dsProxy.tpResolveSession;
  DB := TFSDatabase(Database);
  If (DB = Nil) Then
    RaiseFSErrorObj(Self, fsdse_TblBadDBName);
  If aValue Then
    DB.Active := True;
End;
{--------}

Function TFSDataSet.GetCursorProps(Var aProps: TfsCursorProperties): TffResult;
Var
  i: Integer;
Begin
  FillChar(aProps, SizeOf(TfsCursorProperties), 0);
  aProps.TableName := TableName;
  aProps.FileNameSize := fscl_Path + 1 + fscl_FileName + 1 + fscl_Extension;
  aProps.FieldsCount := Dictionary.FieldCount;
  { Record size (logical record) }
  If (XltMode = xltField) Then
    With TFSFieldDescItem(FieldDescs[pred(FieldDescs.Count)]).LogDesc^ Do
      aProps.RecordSize := ioffset + iLen
  Else
    aProps.RecordSize := PhysicalRecordSize;
  { Record size (physical record) }
  aProps.RecordBufferSize := PhysicalRecordSize;
  aprops.ValChecks := 0;
  With Dictionary Do
    Begin
      For i := 0 To pred(FieldCount) Do
        If FieldRequired[i] Or (FieldVCheck[i] <> Nil) Then
          inc(aProps.ValChecks);
    End;
  aProps.BookMarkSize := Dictionary.BookmarkSize[0];
  aProps.BookMarkStable := True;
  aProps.OpenMode := OpenMode;
  aProps.ShareMode := ShareMode;
  aProps.Indexed := True;
  aProps.xltMode := XltMode;
  aProps.TblRights := prvUNKNOWN;
  aProps.Filters := Filters.Count;
  Result := DBIERR_NONE;
End;
{--------}

Function TFSBaseTable.GetCursorProps(Var aProps: TfsCursorProperties): TffResult;
Begin
  Result := Inherited GetCursorProps(aProps);
  aProps.KeySize := Dictionary.IndexKeyLength[IndexID];
  aProps.IndexCount := Dictionary.IndexCount;
  aProps.BookMarkSize := Dictionary.BookmarkSize[IndexID];
End;
{--------}

Function TFSDataSet.dsGetNextRecord(eLock: TffLockType;
  pRecBuff: Pointer;
  RecProps: pRECProps;
  Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Var
  FoundNext: Boolean;
  CreatedBuffer: Boolean;
Begin
  If (pRecBuff <> Nil) Then
    CreatedBuffer := False
  Else
    Begin
      FFGetMem(pRecBuff, PhysicalRecordSize);
      CreatedBuffer := True;
    End;
  FoundNext := False;
  Result := dsGetNextRecordPrim(CursorID, ffltNOLOCK, pRecBuff, RecProps, aFlag, aRefNr);
  While (Result = DBIERR_NONE) And (Not FoundNext) Do
    Begin
      If dsMatchesFilter(pRecBuff) Then
        Begin
          FoundNext := True;
          If (eLock <> ffltNOLOCK) Then
            Result := dsGetRecordPrim(eLock, Nil, Nil, aFlag, aRefNr);
        End
      Else
        Result := dsGetNextRecordPrim(CursorID, ffltNOLOCK, pRecBuff, RecProps, aFlag, aRefNr);
    End;
  If CreatedBuffer Then
    FFFreeMem(pRecBuff, PhysicalRecordSize);
End;
{--------}

Function TFSDataSet.dsGetNextRecordPrim(aCursorID: TffCursorID;
  eLock: TffLockType;
  pRecBuff: Pointer;
  RecProps: pRECProps;
  Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Begin
  Repeat
    Result := ServerEngine.RecordGetNext(aCursorID,
      eLock,
      pRecBuff,
      aFlag,
      aRefNr);
    If Result = DBIERR_FS_FilterTimeout Then
      Begin
        If dsCancelServerFilter Then
          break;
      End
    Else
      break;
  Until False;
  If (RecProps <> Nil) Then
    FillChar(RecProps^, sizeof(RECProps), 0);
End;
{------}

Function TFSDataSet.GetActiveRecBuf(Var aRecBuf: PChar; NewBuffer: PChar = Nil): Boolean;
Begin
  Result := True;
  Case State Of
    dsBrowse:
      If IsEmpty Then
        Begin
          aRecBuf := Nil;
          Result := False;
        End
      Else If NewBuffer <> Nil Then
        aRecBuf := NewBuffer
      Else
        aRecBuf := ActiveBuffer;
    dsEdit,
      dsInsert:
      aRecBuf := ActiveBuffer;
    dsCalcFields:
      aRecBuf := CalcBuffer;
    dsFilter:
      aRecBuf := dsRecordToFilter;
    dsSetKey:
      aRecBuf := PChar(btKeyBuffer);
    dsOldValue:
      Begin
        aRecBuf := dsOldValuesBuffer;
        Result := Assigned(aRecBuf);
      End;
    Else
      aRecBuf := Nil;
      Result := False;
  End;
End;
{--------}

Function TFSDataSet.GetCursorHandle(aIndexName: String): TffCursorID;
Var
  RetCode: TffResult;
  Stream: TStream;
  OpenCursorID: Longint;
  OpenIndexID: Longint;
Begin
  {try to open the table}
  Stream := TMemoryStream.Create;
  Try
    RetCode := ServerEngine.TableOpen(Database.DatabaseID,
      TableName,
      False,
      '',
      0,
      TffOpenMode(Not ReadOnly),
      TffShareMode(Not Exclusive),
      dsGetTimeOut,
      Result,
      Stream);
    If RetCode = DBIERR_NONE Then
      Begin
        Stream.Position := 0;
        Stream.Read(OpenCursorID, SizeOf(OpenCursorID));
        {save the data dictionary for this table as well}
        Dictionary.ReadFromStream(Stream);
        Stream.Read(OpenIndexID, SizeOf(OpenIndexID));
        btIndexID := OpenIndexID;
        btIndexName := Dictionary.IndexName[OpenIndexID];
        dsReadFieldDescs;
      End
    Else
      Result := 0;
  Finally
    Stream.Free;
  End;

  {if we failed, but the error was 'table is readonly', try to open
   the table in that mode; switch the internal ReadOnly flag}
  If (RetCode = DBIERR_TABLEREADONLY) Then
    Begin
      If dsReadOnly Then
        RaiseFSErrorObj(Self, fsdse_TblBadReadOnly);
      dsReadOnly := True;
      Result := GetCursorHandle(aIndexName);
      RetCode := DBIERR_NONE;
    End;
  {finally check the return code}
  Check(RetCode);
End;
{--------}

Function TFSBaseTable.GetCursorHandle(aIndexName: String): TffCursorID;
Var
  RetCode: TffResult;
  Stream: TStream;
  OpenCursorID: Longint;
  OpenIndexID: Longint;
Begin
  {try to open the table}
  Stream := TMemoryStream.Create;
  Try
    RetCode := ServerEngine.TableOpen(Database.DatabaseID,
      TableName,
      False,
      IndexName,
      0,
      TffOpenMode(Not ReadOnly),
      TffShareMode(Not Exclusive),
      dsGetTimeOut,
      Result,
      Stream);
    If RetCode = DBIERR_NONE Then
      Begin
        Stream.Position := 0;
        Stream.Read(OpenCursorID, SizeOf(OpenCursorID));
        {save the data dictionary for this table as well}
        Dictionary.ReadFromStream(Stream);
        Stream.Read(OpenIndexID, SizeOf(OpenIndexID));
        btIndexID := OpenIndexID;
        btIndexName := Dictionary.IndexName[OpenIndexID];
        dsReadFieldDescs;
      End
    Else
      Result := 0;
  Finally
    Stream.Free;
  End;
  {if we failed, but the error was 'table is readonly', try to open
   the table in that mode; switch the internal ReadOnly flag}
  If (RetCode = DBIERR_TABLEREADONLY) Then
    Begin
      If dsReadOnly Then
        RaiseFSErrorObj(Self, fsdse_TblBadReadOnly);
      dsReadOnly := True;
      Result := GetCursorHandle(aIndexName);
      RetCode := DBIERR_NONE;
    End;
  {finally check the return code}
  Check(RetCode);
End;
{--------}

Function TFSDataSet.dsGetDatabase: TFSBaseDatabase;
Begin
  Result := dsProxy.Database;
End;
{--------}

Function TFSDataSet.dsGetDatabaseName: String;
Begin
  Result := dsProxy.DataBaseName;
End;
{Begin !!.11}
{--------}

Function TFSBaseTable.btGetFFVersion: Integer;
Var
  Version: Longint;
Begin
  Check(ServerEngine.TableVersion(Database.DatabaseID,
    dsGetTableName, Version));
  Result := Version;
End;

{End !!.11}
{--------}

Function TFSBaseTable.btGetIndexField(aInx: Integer): TField;
Var
  FieldNo: Integer;
Begin
  If (aInx < 0) Or (aInx >= IndexFieldCount) Then
    RaiseFSErrorObj(Self, fsdse_TblIdxFldRange);
  FieldNo := btFieldsInIndex[aInx];
  Result := FieldByNumber(FieldNo);
  If (Result = Nil) Then
    RaiseFSErrorObj(Self, fsdse_TblIdxFldMissing);
End;
{--------}

Function TFSBaseTable.btGetIndexFieldNames: String;
Begin
  If btIndexByName Then
    Result := ''
  Else
    Result := btIndexFieldStr;
End;
{--------}

Procedure TFSDataSet.dsGetIndexInfo;
Var
  IndexDesc: IDXDesc;
Begin
  If (btGetIndexDesc(0, IndexDesc) = DBIERR_NONE) Then
    Begin
      btKeyLength := IndexDesc.iKeyLen;
      btKeyInfoOfs := dsPhyRecSize;
      btKeyBufSize := btKeyInfoOfs + sizeof(TfsKeyRecInfo);
    End;
End;
{--------}

Procedure TFSDataSet.dsCheckMasterRange;
Begin
  { do nothing }
End;

Function TFSDataSet.btGetIndexDesc(iIndexSeqNo: Word;
  Var idxDesc: IDXDesc): TffResult;
Begin
  FillChar(idxDesc, sizeof(idxDesc), 0);

  {note: BDE index sequence numbers are 1-based, 0 means 'current
         index'}
  If (iIndexSeqNo = 0) Then
    iIndexSeqNo := IndexID
  Else
    dec(iIndexSeqNo);

  {check to be sure it is a valid index id}
  If iIndexSeqNo >= Dictionary.IndexCount Then
    Result := DBIERR_NOSUCHINDEX
  Else
    Begin
      GetBDEIndexDescriptor(Dictionary.IndexDescriptor[iIndexSeqNo]^, idxDesc);
      Result := DBIERR_NONE;
    End;
End;
{--------}

Function TFSDataSet.btGetIndexDescs(Desc: pIDXDesc): TffResult;
Var
  IDA: PfsIDXDescArray Absolute Desc;
  Props: TfsCursorProperties;
  i: Word;
Begin
  Result := GetCursorProps(Props);
  If (Result = DBIERR_NONE) Then
    Begin
      For i := 1 To Props.IndexCount Do
        Begin
          Result := btGetIndexDesc(i, IDA^[pred(i)]);
          If Not (Result = DBIERR_NONE) Then
            Begin
              Exit;
            End;
        End;
    End;
End;
{--------}
{--------}

Procedure TFSBaseTable.dsGetIndexInfo;
Var
  i: Integer;
  IndexDesc: IDXDesc;
Begin
  If (btGetIndexDesc(0, IndexDesc) = DBIERR_NONE) Then
    Begin
      btNoCaseIndex := IndexDesc.bCaseInsensitive;
      btIndexFieldCount := IndexDesc.iFldsInKey;
      FillChar(btFieldsInIndex, sizeof(btFieldsInIndex), 0);
      For i := 0 To pred(IndexDesc.iFldsInKey) Do
        btFieldsInIndex[i] := IndexDesc.aiKeyFld[i];
      btKeyLength := IndexDesc.iKeyLen;
      btKeyInfoOfs := dsPhyRecSize;
      btKeyBufSize := btKeyInfoOfs + sizeof(TfsKeyRecInfo);
    End;
End;
{--------}

Function TFSBaseTable.btGetIndexName: String;
Begin
  If btIndexByName Then
    Result := btIndexName
  Else
    Result := '';
End;
{--------}

Function TFSBaseTable.btGetKeyExclusive: Boolean;
Begin
  btCheckKeyEditMode;
  Result := PfsKeyRecInfo(PChar(btKeyBuffer) + btKeyInfoOfs)^.kriExclusive;
End;
{--------}

Function TFSBaseTable.btGetKeyFieldCount: Integer;
Begin
  btCheckKeyEditMode;
  Result := PfsKeyRecInfo(PChar(btKeyBuffer) + btKeyInfoOfs)^.kriFieldCount;
End;
{--------}

Function TFSBaseTable.btGetLookupCursor(Const aKeyFields: String;
  aNoCase: Boolean): TffCursorID;
Var
  KeyIndex: TIndexDef;
  RangeStart: PChar;
  RangeEnd: PChar;
  RangeStartInfo: PfsKeyRecInfo;
  RangeEndInfo: PfsKeyRecInfo;
  TmpInt: Integer;
  TmpStr: String;
Begin
  {create a new cursor only if something has changed}
  If (aKeyFields <> btLookupKeyFields) Or
    (aNoCase <> btLookupNoCase) Then
    Begin
      {destroy the old cursor}
      btDestroyLookupCursor;

      (*Note: Case sensitivity should not matter when just interested in integer
            key fields *)
      { If a range is active then do not create a cursor.  We will handle it
        via a lookup filter. }
      RangeStart := PKeyBuffers(btKeyBuffers)^[ketCurRangeStart];
      RangeStartInfo := PfsKeyRecInfo(RangeStart + btKeyInfoOfs);
      RangeEnd := PKeyBuffers(btKeyBuffers)^[ketCurRangeEnd];
      RangeEndInfo := PfsKeyRecInfo(RangeEnd + btKeyInfoOfs);
      If (Not RangeStartInfo^.kriModified) And
        (Not RangeEndInfo^.kriModified) Then
        Begin
          {get the index definition for the field names}
          KeyIndex := IndexDefs.GetIndexForFields(aKeyFields, aNoCase);
          {if there was one...}
          If (KeyIndex <> Nil) Then
            Begin
              {clone our handle and switch indexes}
              Check(ServerEngine.CursorClone(CursorID,
                omReadOnly,
                btLookupCursorID));
              TmpInt := 0;
              TmpStr := KeyIndex.Name;
              Check(btSwitchToIndexEx(btLookupCursorID, TmpStr, TmpInt, False));
              {save the parameters for next time}{!!.01}
              btLookupKeyFields := aKeyFields; {!!.01}
              btLookupNoCase := aNoCase; {!!.01}
            End;
        End;
    End;
  Result := btLookupCursorID;
End;
{--------}

Function TFSBaseTable.btGetMasterFields: String;
Begin
  Result := btMasterLink.FieldNames;
End;
{--------}

Function TFSBaseTable.btGetMasterSource: TDataSource;
Begin
  Result := btMasterLink.DataSource;
End;
{--------}

Procedure TFSDataSet.dsGetRecordInfo(aReadProps: Boolean);
Var
  CursorProps: TfsCursorProperties;
Begin
  If aReadProps Then
    Begin
      Check(GetCursorProps(CursorProps));
      BookmarkSize := CursorProps.BookmarkSize;
      dsPhyRecSize := CursorProps.RecordBufferSize;
    End;
  dsCalcFldOfs := dsPhyRecSize;
  dsBookmarkOfs := dsCalcFldOfs + CalcFieldsSize;
  dsRecInfoOfs := dsBookmarkOfs + BookmarkSize;
  dsRecBufSize := dsRecInfoOfs + SizeOf(TfsDataSetRecInfo);
End;
{--------}

Function TFSDataSet.dsGetSession: TFSSession;
Begin
  Result := dsProxy.Session;
End;
{--------}

Function TFSDataSet.dsGetSessionName: String;
Begin
  Result := dsProxy.SessionName;
End;
{--------}

Function TFSDataSet.dsGetTableName: String;
Begin
  Result := dsProxy.TableName;
End;
{--------}

Function TFSDataSet.dsGetVersion: String;
Begin
  Result := dsProxy.Version;
End;
{--------}

Procedure TFSDataSet.dsRefreshTimeout; {new !!.11}
Begin
  If Active Then
    Check(ServerEngine.CursorSetTimeout(CursorID, dsGetTimeout));
End;
{--------}

Procedure TFSDataSet.btInitKeyBuffer(aBuf: Pointer);
Begin
  FillChar(PfsKeyRecInfo(PChar(aBuf) + btKeyInfoOfs)^, sizeof(TfsKeyRecInfo), 0);
  Dictionary.InitRecord(aBuf);
  Dictionary.FUserName := Self.Session.Client.bcUserName;
  If aBuf <> Nil Then
    Dictionary.SetDefaultFieldUpdateValues(PffByteArray(aBuf), Nil);
End;
{--------}

Function TFSDataSet.dsModifyRecord(aBuffer: Pointer; aRelLock: Boolean): TffResult;
Begin
  Result := ServerEngine.RecordModify(CursorID,
    aBuffer, aRelLock, dsRecLockedType, 0, False, False);
End;

Procedure TFSBaseTable.SetFlagRecord(aFlag: Byte; aSet: Boolean);
Var
  r: TffResult;
  ABuf: Pchar;
Begin
  R := 0;
  If Self.tableversion < 1059 Then
    Raise Exception.Create('Only table version is >= 1.059');
  //                       16                 32
  If Not (aFlag In [frProtectDeleteRecord, frProtectUpdateRecord, frMarkAsBadRecord]) Then
    Raise Exception.Create('Only flag frProtectDeleteRecord, frProtectUpdateRecord, frMarkAsBadRecord ');
  checkactive;
  If isempty Then Raise Exception.Create('Table is empty');
  CheckBrowseMode;
  //
  If Self.GetActiveRecBuf(ABuf) Then
    Begin
      If Self.GetCurrentRecord(ABuf) Then
        R := ServerEngine.RecordModify(CursorID, Pointer(ABuf), True, tluDatabase, aFlag, aSet, True);
    End;
  If r <> 0 Then
    check(r);
End;
{--------}

Function TFSBaseTable.btLocateRecord(Const aKeyFields: String;
  Const aKeyValues: Variant;
  aOptions: TLocateOptions;
  aSyncCursor: Boolean): Boolean;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  i, FieldCount, PartialLength: Integer;
  OurBuffer: PChar;
  OurFields: TList;
  LookupCursor: TffCursorID;
  FilterHandle: HDBIFilter;
  Status: TffResult;
  NoCase: Boolean;
Begin
  aFlag := 0;
  {make sure we're in browse mode}
  CheckBrowseMode;
  CursorPosChanged;
  {get a temporary record Buffer}
  OurBuffer := TempBuffer;
  {create list of fields}
  OurFields := TList.Create;
  Try
    {get the actual fields in the parameter aKeyFields}
    GetFieldList(OurFields, aKeyFields);
    {see whether we can use an index to rapidly lookup the record}
    NoCase := loCaseInsensitive In aOptions;
    If btDoFldsMapToCurIdx(OurFields, NoCase) Then
      LookupCursor := CursorID
    Else
      LookupCursor := btGetLookupCursor(aKeyFields, NoCase);
    {if we have no lookup cursor, locate the record via a filter}
    If (LookupCursor = 0) Then
      Begin
        InternalFirst;
        FilterHandle := dsCreateLookupFilter(OurFields, aKeyValues, aOptions);
        Status := dsGetNextRecord(ffltNoLock, OurBuffer, Nil, aFlag, aRefNr);
        If FilterEval = fseServer Then
          RestoreFilterEx
        Else
          dsDropFilter(FilterHandle);
      End
        {otherwise if we do have a lookup cursor, use it}
    Else
      Begin
        {temporarily move into the filter state - this fools the field
         setting logic to fill the filter Buffer (ie, the temp Buffer)}
        SetTempState(dsFilter);
        dsRecordToFilter := OurBuffer;
        Try
          {initialize the Buffer we're using}
          Dictionary.InitRecord(PffByteArray(OurBuffer));
          Dictionary.FUserName := Self.Session.Client.UserName;
          Dictionary.SetDefaultFieldUpdateValues(PffByteArray(OurBuffer), Nil);
          {set up the field values in the Buffer}
          FieldCount := OurFields.Count;
          If FieldCount = 1 Then
            TField(OurFields[0]).Value := aKeyValues
          Else
            Begin
              For i := 0 To pred(FieldCount) Do
                TField(OurFields[i]).Value := aKeyValues[i];
            End;
          {calculate any partial length - only counts if the last field
           is a string field}
          PartialLength := 0;
          If (loPartialKey In aOptions) And
            (TField(OurFields.Last).DataType = ftString) Then
            Begin
              dec(FieldCount);
              PartialLength := length(TField(OurFields.Last).AsString);
            End;
          {get the record for the given key in the Buffer}
          Status := btGetRecordForKey(LookupCursor, False,
            FieldCount,
            PartialLength,
            OurBuffer,
            OurBuffer);
        Finally
          {reset the state to browse mode}
          RestoreState(dsBrowse);
        End; {try..finally}
        {if we have to sync up, then do so}
        If (Status = DBIERR_NONE) And
          aSyncCursor And
          (LookupCursor <> CursorID) Then
          Status := ServerEngine.CursorSetToCursor(CursorID,
            btLookupCursorID);
      End;
  Finally
    OurFields.Free;
  End; {try..finally}

  { check the result, raise an error if a timeout occurred }{begin !!.11}
  Case Status Of
    DBIERR_FS_FilterTimeout,
      DBIERR_FS_ReplyTimeout,
      DBIERR_FS_Timeout,
      DBIERR_FS_GeneralTimeout:
      Begin
        Result := False; //needed to avoid compiler warning
        Check(Status);
      End;
    Else
      Result := (Status = DBIERR_NONE);
  End; {end !!.11}
End;
{--------}

Procedure TFSBaseTable.btMasterChanged(Sender: TObject);
Begin
  CheckBrowseMode;
  btSetLinkRange(btMasterLink.Fields);
  ApplyRange;
End;
{--------}

Procedure TFSBaseTable.btMasterDisabled(Sender: TObject);
Begin
  CancelRange;
End;
{--------}

Function TFSDataSet.dsOnFilterRecordCallback(
  pRecBuf: Pointer;
  iPhyRecNum: Longint): Smallint;
Var
  Accept: Boolean;
  SaveState: TDataSetState;
Begin
  SaveState := SetTempState(dsFilter);
  Try
    Accept := True;
    Result := Ord(Accept);
    dsRecordToFilter := pRecBuf;
    Try
      If Assigned(OnFilterRecord) Then
        OnFilterRecord(Self, Accept);
      Result := Ord(Accept);
    Except
      Raise;
    End;
    dsRecordToFilter := Nil;
  Finally
    RestoreState(SaveState);
  End;
End;
{--------}

Function TFSBaseTable.btResetRange(aCursorID: TffCursorID;
  SwallowSeqAccessError: Boolean): Boolean;
Var
  RangeStart: PChar;
  RangeEnd: PChar;
  RangeStartInfo: PfsKeyRecInfo;
  RangeEndInfo: PfsKeyRecInfo;
Begin
  RangeStart := PKeyBuffers(btKeyBuffers)^[ketCurRangeStart];
  RangeStartInfo := PfsKeyRecInfo(RangeStart + btKeyInfoOfs);
  RangeEnd := PKeyBuffers(btKeyBuffers)^[ketCurRangeEnd];
  RangeEndInfo := PfsKeyRecInfo(RangeEnd + btKeyInfoOfs);
  If (Not RangeStartInfo^.kriModified) And
    (Not RangeEndInfo^.kriModified) Then
    Result := False
  Else
    Begin
      btResetRangePrim(aCursorID, SwallowSeqAccessError);
      btInitKeyBuffer(RangeStart);
      btInitKeyBuffer(RangeEnd);
      btDestroyLookupCursor;
      Result := True;
    End;
End;
{--------}

Procedure TFSBaseTable.btResetRangePrim(aCursorID: TffCursorID;
  SwallowSeqAccessError: Boolean);
Var
  Status: TffResult;
Begin
  Status := ServerEngine.CursorResetRange(aCursorID);
  If (Status <> DBIERR_NONE) Then
    Begin
      If (Status <> DBIERR_NOASSOCINDEX) Or
        (Not SwallowSeqAccessError) Then
        Check(Status);
    End
  Else
    Begin
      btRangeStack.ClearSaved;
    End;
End;
{--------}

Procedure TFSDataset.btRetrieveIndexName(Const aNameOrFields: String;
  aIndexByName: Boolean;
  Var aIndexName: String);
Var
  Inx: Integer;
Begin
  If (aNameOrFields <> '') Then
    Begin
      UpdateIndexDefs;
      If aIndexByName Then
        Begin
          Inx := btIndexDefs.IndexOf(aNameOrFields);
          If (Inx = -1) Then
            Check(DBIERR_NOSUCHINDEX);
          aIndexName := aNameOrFields;
        End
      Else
        Begin
          aIndexName := btIndexDefs.FindIndexForFields(aNameOrFields).Name;
        End;
    End;
End;
{--------}

Procedure TFSDataSet.dsSetDatabaseName(Const aValue: String);
Begin
  If (csReading In ComponentState) Then
    dsProxy.LoadingFromStream := True;
  dsProxy.DataBaseName := aValue;
  If Active Then
    DataEvent(dePropertyChange, 0);
End;
{--------}

Procedure TFSDataSet.dsSetExclusive(Const aValue: Boolean);
Begin
  dsProxy.CheckInactive(True);

  If (csLoading In ComponentState) Then
    Begin
      dsExclusive := aValue;
      Exit;
    End;

  If (dsProxy.Database <> Nil) And dsProxy.Database.Exclusive Then
    dsExclusive := True
  Else
    dsExclusive := aValue;
End;
{--------}

Procedure TFSDataSet.dsSetFilterEval(Const aMode: TffFilterEvaluationType);

Begin
  dsSetFilterTextAndOptions(Filter, FilterOptions, aMode,
    dsFilterTimeOut);
End;
{--------}

Procedure TFSDataSet.dsSetFilterTextAndOptions(Const aText: String;
  Const aOpts: TFilterOptions;
  Const aMode: TffFilterEvaluationType;
  Const atimeOut: TffWord32);
Begin
  {if there is no change there's nothing to do}
  If (Filter = aText) And (FilterOptions = aOpts) And
    (dsFilterEval = aMode) And (dsFilterTimeOut = atimeOut) Then
    Exit;

  {if the table is active...}
  If Active Then
    Begin
      CheckBrowseMode;

      { Determine whether or not we have to clear an existing filter. }
      Case dsFilterEval Of
        fseLocal:
          {firstly drop the current expression filter}
          If (dsExprFilter <> Nil) Then
            Begin
              Check(dsDropFilter(dsExprFilter));
              dsExprFilter := Nil;
            End;
        fseServer:
          If aMode = fseLocal Then
            Begin
              dsClearServerSideFilter;
            End;
      End; { case }

      dsFilterEval := aMode;
      dsFilterTimeOut := atimeOut;

      {call our ancestor}
      Inherited SetFilterText(aText);

      { If a filter is being set then create the new filter based upon where
        it is to be evaluated. }
      If (aText <> '') Then
        Begin
          If aMode = fseLocal Then
            Begin
              {add the new expression & activate it}
              dsAddExprFilter(aText, aOpts);
              If Filtered Then
                dsActivateFilter(dsExprFilter);
            End
          Else If Filtered Then
            dsActivateFilters;
        End; { If have filter text }

      {call our ancestor}
      Inherited SetFilterOptions(aOpts);

      {if the table is being filtered, go to the start}
      If Filtered Then
        First;
    End
  Else {table is not active}
    Begin

      {call our ancestor}
      Inherited SetFilterText(aText);
      Inherited SetFilterOptions(aOpts);

      dsFilterEval := aMode;
      dsFilterTimeOut := atimeOut;
    End;
End;
{--------}

Function TFSDataSet.dsAddFilter(iClientData: Longint;
  iPriority: Word;
  bCanAbort: Bool;
  pCANExpr: pCANExpr;
  pffilter: pfGENFilter;
  Var hFilter: hDBIFilter): TffResult;
Var
  Filter: TFSFilterListItem;
Begin
  Filter := TFSFilterListItem.Create(dsFilters, Self,
    iClientData, iPriority, bCanAbort,
    pCANExpr, pffilter);
  hFilter := hDBIFilter(Filter);
  dsUpdateFilterStatus;
  Result := DBIERR_NONE;
End;
{--------}

Function TFSDataSet.dsActivateFilter(hFilter: hDBIFilter): TffResult;
Var
  i: Integer;
  Filter: TFSFilterListItem;
Begin
  Result := DBIERR_NONE;
  If (hFilter = Nil) Then
    Begin
      For i := 0 To Pred(dsFilters.Count) Do
        Begin
          Filter := TFSFilterListItem(dsFilters.Items[i]);
          If (Filter <> Nil) Then
            Begin
              Filter.Active := True;
              dsFilterActive := True;
            End;
        End;
    End
  Else {hFilter is an actual handle}
    Begin
      Filter := TFSFilterListItem(hFilter);
      If (dsFilters.IndexOf(Filter) <> -1) Then
        Begin
          Filter.Active := True;
          dsFilterActive := True;
        End
      Else
        Result := DBIERR_NOSUCHFILTER;
    End;
End;
{--------}

Function TFSDataSet.dsDeactivateFilter(hFilter: hDBIFilter): TffResult;
Var
  i: Integer;
  Filter: TFSFilterListItem;
Begin
  Result := DBIERR_NONE;
  If (hFilter = Nil) Then
    Begin
      For i := 0 To Pred(dsFilters.Count) Do
        Begin
          Filter := TFSFilterListItem(dsFilters.Items[i]);
          If (Filter <> Nil) Then
            Filter.Active := False;
        End;
      dsFilterActive := False;
    End
  Else
    Begin
      Filter := TFSFilterListItem(hFilter);
      If (dsFilters.IndexOf(Filter) <> -1) Then
        Begin
          If Filter.Active Then
            Begin
              Filter.Active := False;
              dsUpdateFilterStatus;
            End
          Else {filter wasn't active}
            Result := DBIERR_NA;
        End
      Else {filter not found}
        Result := DBIERR_NOSUCHFILTER;
    End;
End;
{--------}

Procedure TFSDataSet.dsSetFilterTimeout(Const numMS: TffWord32);
Begin
  dsSetFilterTextAndOptions(Filter, FilterOptions, dsFilterEval,
    numMS);
End;

{--------}

Procedure TFSBaseTable.btSetIndexField(aInx: Integer; Const aValue: TField);
Begin
  btGetIndexField(aInx).Assign(aValue);
End;
{--------}

Procedure TFSBaseTable.btSetIndexFieldNames(Const aValue: String);
Begin
  btSetIndexTo(aValue, aValue = '');
End;
{--------}

Procedure TFSBaseTable.btSetIndexName(Const aValue: String);
Begin
  btSetIndexTo(aValue, True);
End;
{--------}

Procedure TFSBaseTable.btSetIndexTo(Const aParam: String; aIndexByName: Boolean);
Var
  IndexName: String;
Begin
  If (aIndexByName <> btIndexByName) Or
    (aIndexByName And (aParam <> btIndexName)) Or
    ((Not aIndexByName) And (aParam <> btIndexFieldStr)) Then
    Begin
      If Active Then
        Begin
          CheckBrowseMode;
          btRetrieveIndexName(aParam, aIndexByName, IndexName);
          btSwitchToIndex(IndexName);
          dsCheckMasterRange;
        End;
      If aIndexByName Then
        btIndexName := aParam
      Else {indexing by list of field names}
        Begin
          btIndexName := IndexName;
          btIndexFieldStr := aParam;
        End;
      btIndexByName := aIndexByName;
      If Active Then
        first;
    End;
End;
{--------}

Procedure TFSBaseTable.btSetKeyBuffer(aInx: TfsKeyEditType; aMustClear: Boolean);
Begin
  {if the current index is not composite, raise error}
  CheckBrowseMode;
  btKeyBuffer := PKeyBuffers(btKeyBuffers)^[aInx];
  Move(btKeyBuffer^, PKeyBuffers(btKeyBuffers)^[ketSaved]^, btKeyBufSize);
  If aMustClear Then
    btInitKeyBuffer(btKeyBuffer);
  SetState(dsSetKey);
  SetModified(PfsKeyRecInfo(PChar(btKeyBuffer) + btKeyInfoOfs)^.kriModified);
  DataEvent(deDataSetChange, 0);
End;
{--------}

Procedure TFSBaseTable.btSetKeyFields(aInx: TfsKeyEditType;
  Const aValues: Array Of Const);
Var
  OldState: TDataSetState;
  i: Integer;
Begin
  { if the current index is not composite, raise error}{!!.10}
  If Dictionary.IndexType[btIndexID] > itExpression Then {!!.10}
    Raise EfsDatabaseError.Create(fsStrResDataSet[fsdse_TblIdxFldMissing]); {!!.10}
  OldState := SetTempState(dsSetKey);
  Try
    btKeyBuffer := PKeyBuffers(btKeyBuffers)^[aInx];
    btInitKeyBuffer(btKeyBuffer);
    For i := 0 To High(aValues) Do
      btGetIndexField(i).AssignValue(aValues[i]);
    With PfsKeyRecInfo(PChar(btKeyBuffer) + btKeyInfoOfs)^ Do
      Begin
        kriFieldCount := High(aValues) + 1;
        kriExclusive := False;
        kriModified := Modified;
      End;
  Finally
    RestoreState(OldState);
  End; {try..finally}
End;
{--------}

Function TFSDataSet.dsGetPhyRecSize: Integer;
Begin
  Result := Dictionary.RecordLength;
End;
{--------}

Function TFSDataSet.dsGetPriorRecord(eLock: TffLockType;
  pRecBuff: Pointer;
  RecProps: pRECProps;
  Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Var
  FoundPrior: Boolean;
  CreatedBuffer: Boolean;
Begin
  If (pRecBuff <> Nil) Then
    CreatedBuffer := False
  Else
    Begin
      FFGetMem(pRecBuff, PhysicalRecordSize);
      CreatedBuffer := True;
    End;
  FoundPrior := False;
  Result := dsGetPriorRecordPrim(ffltNOLOCK, pRecBuff, RecProps, aFlag, aRefNr);
  While (Result = DBIERR_NONE) And (Not FoundPrior) Do
    Begin
      If dsMatchesFilter(pRecBuff) Then
        Begin
          FoundPrior := True;
          If (eLock <> ffltNOLOCK) Then
            Result := dsGetRecordPrim(eLock, Nil, Nil, aFlag, aRefNr);
        End
      Else
        Result := dsGetPriorRecordPrim(ffltNOLOCK, pRecBuff, RecProps, aFlag, aRefNr);
    End;
  If CreatedBuffer Then
    FFFreeMem(pRecBuff, PhysicalRecordSize);
End;
{--------}

Function TFSDataSet.dsGetPriorRecordPrim(eLock: TffLockType;
  pRecBuff: Pointer;
  RecProps: pRECProps;
  Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Begin
  Repeat
    Result := ServerEngine.RecordGetPrior(CursorID,
      eLock,
      pRecBuff,
      aFlag,
      aRefNr);
    If Result = DBIERR_FS_FilterTimeout Then
      Begin
        If dsCancelServerFilter Then
          break;
      End
    Else
      break;
  Until False;
  If (RecProps <> Nil) Then
    FillChar(RecProps^, sizeof(RECProps), 0);
End;
{------}

Function TFSDataSet.dsGetRecord(eLock: TffLockType;
  pRecBuff: Pointer;
  RecProps: pRECProps;
  Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Var
  CreatedBuffer: Boolean;
Begin
  If (pRecBuff <> Nil) Then
    CreatedBuffer := False
  Else
    Begin
      FFGetMem(pRecBuff, PhysicalRecordSize);
      CreatedBuffer := True;
    End;
  Result := dsGetRecordPrim(eLock, pRecBuff, RecProps, aFlag, aRefNr);
  If (Result = DBIERR_NONE) Then
    Begin
      If (Not dsMatchesFilter(pRecBuff)) Then
        Begin
          If (eLock <> ffltNOLOCK) Then
            Check(ServerEngine.RecordRelLock(CursorID,
              False));
          Result := DBIERR_RECNOTFOUND;
        End;
    End;
  If CreatedBuffer Then
    FFFreeMem(pRecBuff, PhysicalRecordSize);
End;
{--------}

Function TFSDataSet.dsGetRecordCountPrim(Var iRecCount: Longword): TffResult;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  BM: pointer;
  Buff: pointer;
  Marked: Boolean;

Begin
  aFlag := 0;
  If Not FilterActive Then
    Begin
      { Query the server engine for the exact record count}
      Result := ServerEngine.TableGetRecCount(CursorID,
        iRecCount);
    End
  Else
    Begin
      { We will manually count the records at the client.      }
      {This can take some time, and consume copious amounts of }
      {bandwitdth. It is recommended that a record count       }
      {only be requested when absolutely necessary when        }
      {filters are active!                                     }
      iRecCount := 0;
      FFGetMem(Buff, PhysicalRecordSize);
      Try
        DisableControls;
        Try
          { Retrieve a bookmark so we can reset the cursor when we are done}
          BM := GetBookMark;
          Try
            Marked := Assigned(BM);
            Try
              InternalFirst;
              Result := dsGetNextRecord(ffltNOLOCK, Buff, Nil, aFlag, aRefNr);
              While (Result = DBIERR_NONE) Do
                Begin
                  Inc(iRecCount);
                  Result := dsGetNextRecord(ffltNOLOCK, Buff, Nil, aFlag, aRefNr);
                End;
            Finally
              { if an error occured, we need to make sure the cursor is set}
              {properly!}
              If Marked Then
                InternalGotoBookmark(BM);
            End;
          Finally
            FreeBookmark(BM);
          End;
        Finally
          EnableControls;
        End;
      Finally
        FFFreeMem(Buff, PhysicalRecordSize);
      End;
    End;

  { If an unexpected error occurs set RecordCount to 0}{!!.01 - Start}
  If (Result <> DBIERR_NONE) Then
    Begin
      If (Result = DBIERR_EOF) Then
        Result := DBIERR_NONE
      Else
        iRecCount := 0;
    End; {!!.01 - End}
End;
{------}

Function TFSDataSet.dsGetRecordPrim(eLock: TffLockType;
  pRecBuff: Pointer;
  RecProps: pRECProps;
  Var aFlag: Byte; Var aRefNr: TffInt64): TffResult;
Begin
  Result := ServerEngine.RecordGet(CursorID,
    eLock,
    tluDatabase,
    pRecBuff,
    aFlag,
    aRefNr,
    False);
  If (RecProps <> Nil) Then
    FillChar(RecProps^, sizeof(RECProps), 0);
End;
{------}

Function TFSBaseTable.btGetRecordForKey(aCursorID: TffCursorID;
  bDirectKey: Boolean;
  iFields: Word;
  iLen: Word;
  pKey: Pointer;
  pRecBuff: Pointer
  ): TffResult;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  FoundNext: Boolean;
  Bookmark: Pointer;
  CreatedBuffer: Boolean;
  FuncResult: TffResult;
  RangeSaved: Boolean;
  Request: PfsnmCursorSetRangeReq;
  SetRangeReqLen: Integer;
  FirstCall: Boolean;
Begin
  aFlag := 0;
  If (aCursorID = CursorID) Then
    Begin {Begin !!.03}
      If (Not bDirectKey) And (btIndexID = 0) Then
        Begin
          Result := DBIERR_INVALIDINDEXTYPE;
          Exit;
        End;
    End
  Else
    Begin
      If (Not bDirectKey) And (btLookupIndexID = 0) Then
        Begin
          Result := DBIERR_INVALIDINDEXTYPE;
          Exit;
        End;
    End; {END !!.03}

  If FilterActive Then
    Begin

      RangeSaved := False;

      { If a range is active then push it onto the range stack.
        We will restore the range when we are done. }
      If btRangeStack.SavedRequest Then
        Begin
          btRangeStack.PushSavedRequest;
          RangeSaved := True;
        End;

      Bookmark := Nil;
      FuncResult := DBIERR_NONE;
      {set the range for this key}
      Result := btSetRangePrim(aCursorID,
        bDirectKey,
        iFields,
        iLen,
        pKey,
        True,
        iFields,
        iLen,
        pKey,
        True);
      If (Result = DBIERR_NONE) Then
        Begin
          {create a record Buffer if one wasn't passed in}
          CreatedBuffer := False;
          If (pRecBuff = Nil) Then
            Begin
              CreatedBuffer := True;
              FFGetMem(pRecBuff, PhysicalRecordSize);
            End;
          {search for valid record in range}
          FoundNext := False;
          Result := dsGetNextRecordPrim(aCursorID, ffltNoLock, pRecBuff, Nil, aFlag, aRefNr);
          While (Result = DBIERR_NONE) And (Not FoundNext) Do
            Begin
              If dsMatchesFilter(pRecBuff) Then
                Begin
                  FoundNext := True;
                End
              Else
                Result := dsGetNextRecordPrim(aCursorID, ffltNoLock, pRecBuff, Nil, aFlag, aRefNr);
            End;
          {if we succeeded in finding a record in range, get its bookmark}
          {because the reset range in a moment will lose the record}
          {position}
          If Not (Result = DBIERR_NONE) Then
            FuncResult := DBIERR_RECNOTFOUND
          Else
            Begin
              //      if BookmarkAvailable then begin                               {!!.06}
              GetMem(Bookmark, BookmarkSize); {!!.03}
              Check(ServerEngine.CursorGetBookmark(aCursorID, Bookmark)); {!!.03}
              //      End ;                                                          {!!.06}
            End;
          {reset the range}
          btResetRangePrim(aCursorID, True);

          { Do we need to restore a prior range? }
          If rangeSaved Then
            Begin
              btRangeStack.popSavedRequest(PffByteArray(Request), SetRangeReqLen);
              { Send the request.  Assume that if it fails we should
                continue operation anyway. }

              Result := ServerEngine.CursorSetRange(Request^.CursorID,
                Request^.DirectKey,
                Request^.FieldCount1,
                Request^.PartialLen1,
                PffByteArray(@Request^.KeyData1),
                Request^.KeyIncl1,
                Request^.FieldCount2,
                Request^.PartialLen2,
                {Begin !!.06}
                PffByteArray(PAnsiChar(@Request^.KeyData1) +
                Request^.KeyLen1),
                {End !!.06}
                Request^.KeyIncl2);

            End;
          {reset the record position}
          If (Bookmark <> Nil) And
            BookmarkValid(Bookmark) Then
            Begin {!!.06}
              Check(ServerEngine.CursorSetToBookmark(aCursorID,
                Bookmark));
              FreeBookmark(Bookmark);
            End;
          If CreatedBuffer Then
            FFFreeMem(pRecBuff, PhysicalRecordSize);
        End;
      If (Result = DBIERR_NONE) Then
        Result := FuncResult;
    End
  Else
    Begin
      FirstCall := True;
      Repeat
        Result := ServerEngine.RecordGetForKey(aCursorID,
          bDirectKey,
          iFields,
          iLen,
          pKey,
          pRecBuff,
          FirstCall);
        If Result = DBIERR_FS_FILTERTimeout Then
          Begin
            If dsCancelServerFilter Then
              Break
            Else
              FirstCall := False;
          End
        Else
          Break;
      Until False;
    End;
End;
{------}

Procedure TFSBaseTable.btSetKeyExclusive(Const aValue: Boolean);
Begin
  btCheckKeyEditMode;
  PfsKeyRecInfo(PChar(btKeyBuffer) + btKeyInfoOfs)^.kriExclusive := aValue;
End;
{--------}

Procedure TFSBaseTable.btSetKeyFieldCount(Const aValue: Integer);
Begin
  btCheckKeyEditMode;
  PfsKeyRecInfo(PChar(btKeyBuffer) + btKeyInfoOfs)^.kriFieldCount := aValue;
End;
{--------}

Procedure TFSBaseTable.btSetLinkRange(aMasterFields: TList);
Var
  i: Integer;
  SaveState: TDataSetState;
  RangeStart: PChar;
  RangeStartInfo: PfsKeyRecInfo;
Begin
  {temporarily change the DataSet state so we can modify the key
   range when we modify field values}
  SaveState := SetTempState(dsSetKey);
  Try
    {set up the Buffer to modify the the start of the range, and then
     set it to the current record in the master}
    RangeStart := PKeyBuffers(btKeyBuffers)^[ketRangeStart];
    btKeyBuffer := RangeStart;
    RangeStartInfo := PfsKeyRecInfo(RangeStart + btKeyInfoOfs);
    btInitKeyBuffer(RangeStart);
    RangeStartInfo^.kriModified := True;
    For i := 0 To Pred(aMasterFields.Count) Do
      btGetIndexField(i).Assign(TField(aMasterFields[i]));
    RangeStartInfo^.kriFieldCount := aMasterFields.Count;
  Finally
    RestoreState(SaveState);
  End;
  {make the range end equal to the range start}
  Move(PKeyBuffers(btKeyBuffers)^[ketRangeStart]^,
    PKeyBuffers(btKeyBuffers)^[ketRangeEnd]^,
    btKeyBufSize);
End;
{--------}

Procedure TFSBaseTable.btSetMasterFields(Const aValue: String);
Begin
  btMasterLink.FieldNames := aValue;
End;
{--------}

Procedure TFSBaseTable.btSetMasterSource(Const aValue: TDataSource);
Begin
  If IsLinkedTo(aValue) Then
    RaiseFSErrorObjFmt(Self, fsdse_TblCircDataLink, [aValue.Name]);
  btMasterLink.DataSource := aValue;
End;
{--------}

Procedure TFSBaseTable.dsSetTableName(Const aValue: String);
Begin
  Inherited dsSetTableName(aValue);

  IndexDefs.Updated := False;
End;
{--------}

Procedure TFSBaseTable.btSetIndexDefs(Value: TIndexDefs); {!!.06}
Begin
  IndexDefs.Assign(Value);
End;
{--------}

Function TFSBaseTable.btIndexDefsStored: Boolean; {!!.06}
Begin
  Result := IndexDefs.Count > 0;
End;
{--------}

Function TFSBaseTable.btSetRange: Boolean;
Var
  RangeStart: PChar;
  RangeEnd: PChar;
  StartKeyOrRec: PChar;
  EndKeyOrRec: PChar;
  RangeStartInfo: PfsKeyRecInfo;
  RangeEndInfo: PfsKeyRecInfo;
Begin
  { Assume we don't set the range. }
  Result := False;

  { If range is the same, exit now. }
  If (BuffersEqual(PKeyBuffers(btKeyBuffers)^[ketRangeStart],
    PKeyBuffers(btKeyBuffers)^[ketCurRangeStart],
    btKeyBufSize) And
    BuffersEqual(PKeyBuffers(btKeyBuffers)^[ketRangeEnd],
    PKeyBuffers(btKeyBuffers)^[ketCurRangeEnd],
    btKeyBufSize)) Then
    Exit;

  { Determine what to use for the setrange call. }
  RangeStart := PKeyBuffers(btKeyBuffers)^[ketRangeStart];
  RangeStartInfo := PfsKeyRecInfo(RangeStart + btKeyInfoOfs);
  If RangeStartInfo^.kriModified Then {ie, some key fields are set}
    StartKeyOrRec := RangeStart
  Else
    StartKeyOrRec := Nil;

  RangeEnd := PKeyBuffers(btKeyBuffers)^[ketRangeEnd];
  RangeEndInfo := PfsKeyRecInfo(RangeEnd + btKeyInfoOfs);
  If RangeEndInfo^.kriModified Then {ie, some key fields are set}
    EndKeyOrRec := RangeEnd
  Else
    EndKeyOrRec := Nil;
  {set the range}
  Check(btSetRangePrim(CursorID, False,
    RangeStartInfo^.kriFieldCount,
    0,
    StartKeyOrRec,
    Not RangeStartInfo^.kriExclusive,
    RangeEndInfo^.kriFieldCount,
    0,
    EndKeyOrRec,
    Not RangeEndInfo^.kriExclusive));
  {save the new current range}
  Move(RangeStart^,
    PKeyBuffers(btKeyBuffers)^[ketCurRangeStart]^,
    btKeyBufSize);
  Move(RangeEnd^,
    PKeyBuffers(btKeyBuffers)^[ketCurRangeEnd]^,
    btKeyBufSize);
  btDestroyLookupCursor;
  {we succeeded}
  Result := True;
End;
{--------}

Function TFSBaseTable.btSetRangePrim(aCursorID: TffCursorID;
  bKeyItself: Boolean;
  iFields1: Word;
  iLen1: Word;
  pKey1: Pointer;
  bKey1Incl: Boolean;
  iFields2: Word;
  iLen2: Word;
  pKey2: Pointer;
  bKey2Incl: Boolean): TffResult;
Var
  Request: PfsnmCursorSetRangeReq;
  ReqLen: Integer;
  KeyLen1, KeyLen2: Integer;
  pKeyData2: pointer;
Begin
  Result := DBIERR_NOMEMORY;
  {calculate sizes}
  If pKey1 = Nil Then
    KeyLen1 := 0
  Else If bKeyItself Then
    KeyLen1 := Dictionary.IndexKeyLength[IndexID]
  Else
    KeyLen1 := PhysicalRecordSize;
  If pKey2 = Nil Then
    KeyLen2 := 0
  Else If bKeyItself Then
    KeyLen2 := Dictionary.IndexKeyLength[IndexID]
  Else
    KeyLen2 := PhysicalRecordSize;
  {now, we know how large the Request is}
  ReqLen := sizeof(TfsnmCursorSetRangeReq) - 4 + KeyLen1 + KeyLen2;
  {allocate and clear it}
  ffGetZeroMem(Request, ReqLen);
  Try
    {fill the request}
    Request^.CursorID := aCursorID;
    Request^.DirectKey := bKeyItself;
    Request^.FieldCount1 := iFields1;
    Request^.PartialLen1 := iLen1;
    Request^.KeyLen1 := KeyLen1;
    Request^.KeyIncl1 := bKey1Incl;
    Request^.FieldCount2 := iFields2;
    Request^.PartialLen2 := iLen2;
    Request^.KeyLen2 := KeyLen2;
    Request^.KeyIncl2 := bKey2Incl;
    Move(pKey1^, Request^.KeyData1, KeyLen1);
    pKeyData2 := PffByteArray(PAnsiChar(@Request^.KeyData1) + KeyLen1);
    Move(pKey2^, pKeyData2^, KeyLen2);

    Result := ServerEngine.CursorSetRange(aCursorID, bKeyItself,
      iFields1, iLen1, pKey1, bKey1Incl,
      iFields2, iLen2, pKey2, bKey2Incl);
  Finally
    If (Result = DBIERR_NONE) Then
      btRangeStack.SaveLastRequest(PffByteArray(Request), ReqLen)
    Else
      FFFreeMem(Request, ReqLen);
  End;
End;
{------}

Function TFSDataSet.FieldRound(Name: String): TRound;
Var
  i: Integer;
Begin
  Result := rNone;
  For i := 0 To Dictionary.FieldCount - 1 Do
    Begin
      If Trim(AnsiUpperCase(Dictionary.FieldDescriptor[i].fdName)) =
        Trim(AnsiUpperCase(Name)) Then
        Begin
          Result := Dictionary.FieldDescriptor[i].fdRound;
          Exit;
        End;
    End;
End;

Procedure TFSDataSet.dsSetReadOnly(Const aValue: Boolean);
Begin
  dsProxy.CheckInactive(False); {!!.06}

  If (csLoading In ComponentState) Then
    Begin
      dsReadOnly := aValue; {!!.01}
      Exit;
    End;

  If (dsProxy.Database <> Nil) And dsProxy.Database.ReadOnly Then
    dsReadOnly := True
  Else
    dsReadOnly := aValue;
End;
{--------}

Procedure TFSDataSet.dsSetServerSideFilter(Const aText: String;
  Const aOpts: TFilterOptions;
  aTimeout: TffWord32);

Var
  Parser: TfsExprParser;
Begin
  If (aText <> '') Then
    Begin
      Parser := TfsExprParser.Create(Self, aText, aOpts, [poExtSyntax], '', Nil,
        fsFldTypeMap);
      Try
        Check(SetFilterEx(FSSrBDE.pCANExpr(Parser.FilterData), aTimeout));
      Finally
        Parser.Free;
      End;
    End
  Else
    dsClearServerSideFilter;
End;
{--------}

Procedure TFSDataSet.dsUpdateFilterStatus;
Var
  Filt: TFSFilterListItem;
  i: Integer;
Begin
  For i := 0 To Pred(dsFilters.Count) Do
    Begin
      Filt := TFSFilterListItem(dsFilters.Items[i]);
      If (Filt <> Nil) And (Filt.Active) Then
        Begin
          dsFilterActive := True;
          Exit;
        End;
    End;
  dsFilterActive := False;
End;
{--------}

Function TFSDataSet.dsDropFilter(hFilter: hDBIFilter): TffResult;
Var
  Inx: Integer;
  Filter: TFSFilterListItem;
Begin
  If (hFilter = Nil) Then
    Begin
      dsFilters.FreeAll;
      Result := DBIERR_NONE;
    End
  Else
    Begin
      Filter := TFSFilterListItem(hFilter);
      Inx := dsFilters.IndexOf(Filter);
      If (Inx = -1) Then
        Result := DBIERR_NOSUCHFILTER
      Else
        Begin
          Filter.Free;
          dsUpdateFilterStatus;
          Result := DBIERR_NONE;
        End;
    End;
End;
{--------}

Procedure TFSDataSet.dsSetSessionName(Const aValue: String);
Begin
  If (csReading In ComponentState) Then
    dsProxy.LoadingFromStream := True;
  dsProxy.SessionName := aValue;
  If Active Then
    DataEvent(dePropertyChange, 0);
End;
{--------}

Procedure TFSDataSet.SetBlobMode(Const Value: TfsBlobMode);
Begin
  fBlobMode := Value;
End;

Procedure TFSDataSet.SetBlobChunkSize(Const Value: Integer);
Begin
  fBlobChunkSize := Value;
  If fBlobChunkSize < 0 Then
    fBlobChunkSize := 0;
End;

Procedure TFSDataSet.dsSetTableLock(LockType: TffLockType; Lock: Boolean);

Begin
  CheckActive;
  If Lock Then
    Check(ServerEngine.TableLockAcquire(CursorID,
      LockType))
  Else
    Check(ServerEngine.TableLockRelease(CursorID,
      False));
End;
{--------}

Procedure TFSDataSet.dsSetTableName(Const aValue: String);
Begin
  If (csReading In ComponentState) Then
    dsProxy.LoadingFromStream := True;
  dsProxy.TableName := ffExtractTableName(aValue);
  If Active Then
    DataEvent(dePropertyChange, 0);
End;
{--------}

Procedure TFSDataSet.dsSetTimeout(Const Value: Longint);
Begin
  If dsTimeout = Value Then
    Exit;
  dsTimeout := Value;
  If Active Then
    Check(ServerEngine.CursorSetTimeout(CursorID, dsGetTimeout));
End;

Procedure TFSDataSet.dsSetCheckTimeout(Const Value: Longint);
Begin
  If dsCheckTimeout = Value Then
    Exit;
  If Value < -1 Then Exit;
  dsCheckTimeout := Value;
End;

Procedure TFSDataSet.dsSetDeleteTimeout(Const Value: Longint);
Begin
  If dsDeleteTimeout = Value Then
    Exit;
  If Value < -1 Then Exit;
  dsDeleteTimeout := Value;
End;
{--------}

Procedure TFSDataSet.dsSetVersion(Const aValue: String);
Begin
  {do nothing}
End;
{--------}

Procedure TFSBaseTable.btSwitchToIndex(Const aIndexName: String);
Var
  Status: TffResult;
  aIndexID: Integer;
Begin
  btResetRange(CursorID, True);
  UpdateCursorPos;
  {switch to the new index by name, try and keep on the current record}
  aIndexID := 0;
  Status := btSwitchToIndexEx(CursorID,
    aIndexName,
    aIndexID,
    True);
  {if the new index existed, but there was no current record, try
   again without keeping the current record current}
  If (Status = DBIERR_NOCURRREC) Or (Status = DBIERR_FS_RecDeleted) Then {!!.11}
    Status := btSwitchToIndexEx(CursorID,
      aIndexName,
      aIndexID,
      False);
  {check we did OK}
  Check(Status);

  btKeyLength := 0;
  btNoCaseIndex := False;
  btIndexFieldCount := 0;
  {destroy our record Buffers - the bookmark stuff has changed}
  SetBufListSize(0);
  dsGetRecordInfo(True);
  Try
    {get new record Buffers}
    SetBufListSize(BufferCount + 1);
  Except
    {if we're out of memory - or worse - bail out}
    SetState(dsInactive);
    CloseCursor;
    Raise;
  End;
  {get the new index information}
  dsGetIndexInfo;
End;
{--------}

Function TFSBaseTable.btSwitchToIndexEx(aCursorID: TffCursorID;
  Const aIndexName: String;
  Const aIndexID: Integer;
  Const aCurrRec: Boolean): TffResult;
Var
  Stream: TStream;
  TempDict: TFSInfoDict;
Begin
  Result := ServerEngine.CursorSwitchToIndex(aCursorID,
    aIndexName,
    aIndexID,
    aCurrRec);
  If (aCursorID = CursorID) And (Result = DBIERR_NONE) Then
    Begin {!!.03}
      If (aIndexName <> '') Then
        Begin
          btIndexID := Dictionary.GetIndexFromName(aIndexName);
          btIndexName := aIndexName;
          btRangeStack.Clear;
        End
      Else
        Begin
          btIndexName := Dictionary.IndexName[aIndexID];
          btIndexID := aIndexID;
        End;
    End
  Else
    Begin
      { fetch data dictionary }
      TempDict := TFSInfoDict.Create(4096);
      Try
        Stream := TMemoryStream.Create;
        Try
          If Database.GetFFDataDictionary(TableName, Stream) = DBIERR_NONE Then
            Begin
              Stream.Position := 0;
              TempDict.ReadFromStream(Stream);
            End;
        Finally
          Stream.Free;
        End;
        If (aCursorID = btLookupCursorID) And (Result = DBIERR_NONE) Then
          Begin
            If (aIndexName <> '') Then
              Begin
                btLookupIndexID := TempDict.GetIndexFromName(aIndexName);
                btLookupIndexName := aIndexName;
              End
            Else
              Begin
                btIndexID := aIndexID;
                btIndexName := TempDict.IndexName[aIndexID];
              End;
          End;
      Finally
        TempDict.Free;
      End;
    End;
End;
{--------}

Procedure TFSBaseTable.UpdateIndexDefs;
Var
  i: Integer;
  SaveHandle: TffCursorID;
  IndexCount: Integer;
  IndexArray: PfsIDXDescArray;
  Options: TIndexOptions;
  Name: String;
  FieldsStr: String;
  CursorProps: TfsCursorProperties;
Begin
  {if the indexes are not up to date, go get info on them...}
  If Not IndexDefs.Updated Then
    Begin
      dsEnsureDatabaseOpen(True);
      Try
        SaveHandle := CursorID;
        If (SaveHandle = 0) Then
          dsCursorID := GetCursorHandle('');
        FieldDefs.Update;
        Try
          GetCursorProps(CursorProps);
          IndexCount := CursorProps.IndexCount;
          FFGetMem(IndexArray, IndexCount * sizeof(IDXDesc));
          Try
            IndexDefs.Clear;
            btGetIndexDescs(PIDXDesc(IndexArray));
            For i := 0 To Pred(IndexCount) Do
              Begin
                btDecodeIndexDesc(IndexArray^[i], Name, FieldsStr, Options);
                IndexDefs.Add(Name, FieldsStr, Options);
              End;
            IndexDefs.Updated := True;
          Finally
            FFFreeMem(IndexArray, IndexCount * sizeof(IDXDesc));
          End; {try..finally}
        Finally
          If (SaveHandle = 0) Then
            Begin
              DestroyHandle(CursorID);
              dsCursorID := 0;
            End;
        End; {try..finally}
      Finally
        dsEnsureDatabaseOpen(False);
      End; {try..finally}
    End;
End;
{--------}

Procedure TFSDataSet.LockTable(LockType: TffLockType);
Var
  tempTO: Integer;

Begin
  CheckActive;
  If ((dsCheckTimeout > 0) Or (dsCheckTimeout = -1)) Then
    Begin
      tempTO := Timeout;
      Timeout := dsCheckTimeout;
      Try
        dsSetTableLock(LockType, True);
      Finally
        Timeout := tempTO;
      End;
    End
  Else
    dsSetTableLock(LockType, True);
End;
{--------}

Procedure TFSDataSet.UnlockTable(LockType: TffLockType);

Begin
  CheckActive;
  dsSetTableLock(LockType, False);
End;
{--------}

Procedure TFSDataSet.UnlockTableAll;
Begin
  CheckActive;
  Check(ServerEngine.TableLockRelease(CursorID, True));
End;

Function TFSDataSet.TableIsLocked(LockType: TffLockType): boolean;
Begin
  CheckActive;
  Result := False;
  Check(ServerEngine.TableIsLocked(CursorID, LockType, Result));
End;

Procedure TFSDataSet.LockRecord;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  buff: pchar;
  tempTO: Integer;
Begin
  CheckActive;
  If Self.GetActiveRecBuf(buff) Then
    If Self.GetCurrentRecord(Buff) Then
      Begin
        If ((dsCheckTimeout > 0) Or (dsCheckTimeout = -1)) Then
          Begin
            tempTO := Timeout;
            Timeout := dsCheckTimeout;
            Try
              Check(ServerEngine.RecordGet(CursorID, ffltWriteLock, Self.RecLockedType, pointer(buff), aFlag, aRefNr, True));
            Finally
              Timeout := tempTO;
            End;
          End
        Else
          Check(ServerEngine.RecordGet(CursorID, ffltWriteLock, Self.RecLockedType, pointer(buff), aFlag, aRefNr, True));
      End;
End;

Procedure TFSDataSet.UnLockRecord;
Var
  buff: pchar;
Begin
  CheckActive;
  If Self.GetActiveRecBuf(buff) Then
    If Self.GetCurrentRecord(Buff) Then
      Check(ServerEngine.RecordRelLock(CursorID, False));
End;

Procedure TFSDataSet.UnLockRecordAll;
Var
  buff: pchar;
  bm: pointer;
  Marked: boolean;
Begin
  CheckActive;
  If Not isempty Then
    If Self.GetActiveRecBuf(buff) Then
      If Self.GetCurrentRecord(Buff) Then
        Begin
          DisableControls;
          Try
            BM := GetBookMark;
            Try
              Marked := Assigned(BM);
              Try
                Check(ServerEngine.RecordRelLock(CursorID, True));
              Finally
                If Marked Then
                  InternalGotoBookmark(BM);
              End;
            Finally
              FreeBookmark(BM);
            End;
          Finally
            EnableControls;
          End;
        End;
End;

Function TFSDataSet.RecordIsLocked: boolean;
Var
  buff: pchar;
Begin
  Result := False;
  CheckActive;
  If Self.GetActiveRecBuf(buff) Then
    If Self.GetCurrentRecord(Buff) Then
      Check(ServerEngine.RecordIsLocked(CursorID, ffltWriteLock, Result));
End;

Function TFSDataSet.dsFreeBlob(
  pRecBuf: Pointer;
  aField: TField
  ): TffResult;
Var
  BLOBNr: TffInt64;
  IsNull: Boolean;
Begin
  Result := dsGetBlobHandle(pRecBuf, aField, IsNull, BLOBNr);
  If (Result = DBIERR_NONE) And (Not IsNull) Then
    Begin
      Result := ServerEngine.BLOBFree(CursorID, BLOBNr, dsBlobOpenMode = omREADONLY);
      If (Result = DBIERR_BLOBMODIFIED) Then
        Begin
          {DBIERR_BLOBMODIFIED is a special fs 'error' when received here:
           it means that the BLOB was empty and so the BLOB number has
           been deleted at the server; the client must set the BLOB field
           to null}
          Dictionary.SetRecordField(pred(aField.FieldNo), pRecBuf, Nil);
          dsModifyRecord(pRecBuf, False);
          If Not BlobModifiedError Then
            Result := 0;
        End;
    End;
End;
{--------}

Procedure TFSDataSet.CloseBlob(aField: TField);
Begin
  check(dsFreeBlob(ActiveBuffer, aField));
End;

Function TfsDataSet.dsGetBlobSize(aField: TField): Longint;
Var
  Status: TffResult;
  IsNull: Boolean;
  BLOBNr: TffInt64;
  bsRecBuf: PChar;
Begin
  Result := 0;
  If Not GetActiveRecBuf(bsRecBuf) Then Exit;
  Status := dsGetBlobHandle(bsRecBuf, aField, IsNull, BLOBNr);
  If (Status = DBIERR_NONE) And (Not IsNull) Then
    Status := ServerEngine.BLOBGetLength(CursorID, BLOBNr, Result);
  Check(Status);
End;

Function TFSDataSet.dsGetBlobHandle(pRecBuf: Pointer;
  aField: TField;
  Var aIsNull: Boolean;
  Var aBLOBNr: TffInt64): TffResult;
Var
  TempI64: TffInt64;
Begin
  TempI64.iLow := 0;
  TempI64.iHigh := 0;
  Result := DBIERR_NONE;
  Dictionary.GetRecordField(Pred(aField.FieldNo), pRecBuf, aIsNull, @aBLOBNr);
  If (Not aIsNull) And (ffCmpI64(aBLOBNr, TempI64) = 0) Then
    aIsNull := True;
End;

Procedure TfsDataset.dsSetBLOBHandle(pRecBuf: Pointer;
  aField: TField;
  aBLOBNr: TffInt64);
Var
  TempI64: TffInt64;
Begin
  TempI64.iLow := 0;
  TempI64.iHigh := 0;

  If (ffCmpI64(aBLOBNr, TempI64) <> 0) Then
    dsDictionary.SetRecordField(Pred(aField.FieldNo), pRecBuf, @aBlobNr)
  Else
    dsDictionary.SetRecordField(Pred(aField.FieldNo), pRecBuf, Nil);
End;
{------}

Function TFSDataSet.dsForceBLOB(pRecBuf: Pointer;
  aField: TField;
  Var aBLOBNr: TffInt64): TffResult;
Var
  IsNull: Boolean;
  TempI64: TffInt64;
Begin
  TempI64.iLow := 0;
  TempI64.iHigh := 0;

  If (getflags(GetFlagRecord, frProtectUpdateRecord)) And (State In [dsedit]) Then
    Begin
      Result := DBIERR_NOTSUFFFIELDRIGHTS;
      check(Result);
      Exit;
    End;

  Dictionary.GetRecordField(Pred(aField.FieldNo), pRecBuf, IsNull, @aBLOBNr);
  If IsNull Or (ffCmpI64(aBLOBNr, TempI64) = 0) Then
    Begin
      Result := ServerEngine.BLOBCreate(CursorID, aBLOBNr);
      If (Result = DBIERR_NONE) Then
        Dictionary.SetRecordField(Pred(aField.FieldNo), pRecBuf, @aBLOBNr);
    End
  Else
    Result := DBIERR_NONE;
End;
{--------}

Function TFSDataSet.dsTruncateBlob(pRecBuf: Pointer;
  aField: TField;
  iLen: Longint): TffResult;
Var
  BLOBNr: TffInt64;
  IsNull: boolean;
Begin
  If (getflags(GetFlagRecord, frProtectUpdateRecord)) And (State In [dsedit]) Then
    Begin
      Result := DBIERR_NOTSUFFFIELDRIGHTS;
      Exit;
    End;

  Result := dsGetBlobHandle(pRecBuf, aField, IsNull, BLOBNr);
  If (Result = DBIERR_NONE) Then
    Begin
      If IsNull Then
        Begin
          If (iLen <> 0) Then
            Result := DBIERR_INVALIDBLOBoffset
          Else
            Result := DBIERR_NONE;
        End
      Else
        Begin
          {BLOB field was not null}
          {tell the server the new length}
          Result := ServerEngine.BLOBTruncate(CursorID,
            BLOBNr,
            iLen);
        End;
    End;
End;

Function TfsDataSet.FsCreateBlobStream(aField: TField; aMode: TBlobStreamMode): TfsStream;
Var
  bsRecBuf: PChar;
Begin
  Assert(Assigned(aField));
  GetActiveRecBuf(bsRecBuf);
  If BlobMode In [bmInMemory, bmDirect, bmAuto] Then
    Result := CreateNormalBlobStream(aField, aMode, BlobChunkSize)
  Else
    Result := CreateCacheBlobStream(aField, aMode, BlobChunkSize);
End;

Function TfsDataSet.CreateBlobStream(aField: TField; aMode: TBlobStreamMode): TStream;
Var
  bsRecBuf: PChar;
Begin
  Assert(Assigned(aField));
  GetActiveRecBuf(bsRecBuf);
  If BlobMode In [bmInMemory, bmDirect, bmAuto] Then
    Result := CreateNormalBlobStream(aField, aMode, BlobChunkSize)
  Else
    Result := CreateCacheBlobStream(aField, aMode, BlobChunkSize);
End;

// TfsStream ---------------------------------------------

Constructor TfsStream.Create(aChunkSize: Longint);
Begin
  Inherited Create;
  bsChunkSize := aChunkSize;
End;

Destructor TfsStream.Destroy;
Begin
  Inherited Destroy;
End;

Function TfsStream.GetSize: Longint;
Var
  Pos: Longint;
Begin
  Try
    Pos := Seek(0, 1);
    Result := Seek(0, 2);
    Seek(Pos, 0);
  Except
    Result := 0;
  End;
End;

Function TfsStream.CopyFrom(Source: TStream; Count: Longint): Longint;
Var
  BufSize, N, MBChunk: Integer;
  Buffer: PChar;
Begin
  If Count = 0 Then
    Begin
      Source.Position := 0;
      Count := Source.Size;
    End;
  Result := Count;
  MBChunk := bsChunkSize;
  If MBChunk <= 0 Then MBChunk := Count;
  If Count > MBChunk Then
    BufSize := MBChunk
  Else
    BufSize := Count;
  GetMem(Buffer, BufSize);
  Try
    While Count <> 0 Do
      Begin
        If Count > BufSize Then
          N := BufSize
        Else
          N := Count;
        Source.ReadBuffer(Buffer^, N);
        WriteBuffer(Buffer^, N);
        Dec(Count, N);
      End;
  Finally
    FreeMem(Buffer, BufSize);
  End;
End;

Procedure TfsStream.LoadFromStream(Stream: TStream);
Var
  Count: Longint;
Begin
  Stream.Position := 0;
  Count := Stream.Size;
  SetSize(Count);
  If Count <> 0 Then CopyFrom(Stream, 0);
End;

Procedure TfsStream.LoadFromFile(Const FileName: String);
Var
  Stream: TStream;
Begin
  Stream := TFileStream.Create(FileName, fmOpenRead Or fmShareDenyWrite);
  Try
    LoadFromStream(Stream);
  Finally
    Stream.Free;
  End;
End;

Procedure TfsStream.SaveToStream(Stream: TStream);
Begin
  If Size <> 0 Then Stream.CopyFrom(Self, 0);
End;

Procedure TfsStream.SaveToFile(Const FileName: String);
Var
  Stream: TStream;
Begin
  Stream := TFileStream.Create(FileName, fmCreate);
  Try
    SaveToStream(Stream);
  Finally
    Stream.Free;
  End;
End;

Procedure TfsStream.Truncate;
Begin
  SetSize(Position);
End;

Procedure TfsStream.Clear;
Begin
  SetSize(0);
End;

Constructor TfsMemoryStream.Create(aChunkSize: Longint);
Begin
  Inherited Create(aChunkSize);
  bsChunkSize := aChunkSize;
End;

Destructor TfsMemoryStream.Destroy;
Begin
  Clear;
  Inherited Destroy;
End;

Procedure TfsMemoryStream.Truncate;
Begin
  SetSize(Position);
End;

Procedure TfsMemoryStream.SetPointer(Ptr: Pointer; Size: Longint);
Begin
  FMemory := Ptr;
  FSize := Size;
End;

Function TfsMemoryStream.Read(Var Buffer; Count: Longint): Longint;
Begin
  If (FPosition >= 0) And (Count >= 0) Then
    Begin
      Result := FSize - FPosition;
      If Result > 0 Then
        Begin
          If Result > Count Then Result := Count;
          Move(Pointer(Longint(FMemory) + FPosition)^, Buffer, Result);
          Inc(FPosition, Result);
          Exit;
        End;
    End;
  Result := 0;
End;

Function TfsMemoryStream.Seek(Offset: Longint; Origin: Word): Longint;
Begin
  Case Origin Of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: Inc(FPosition, Offset);
    soFromEnd: FPosition := FSize + Offset;
  End;
  Result := FPosition;
End;

Procedure TfsMemoryStream.SaveToStream(Stream: TStream);
Begin
  If FSize <> 0 Then Stream.WriteBuffer(FMemory^, FSize);
End;

Procedure TfsMemoryStream.SaveToFile(Const FileName: String);
Var
  Stream: TStream;
Begin
  Stream := TFileStream.Create(FileName, fmCreate);
  Try
    SaveToStream(Stream);
  Finally
    Stream.Free;
  End;
End;

Const
  MemoryDelta = $2000;

Procedure TfsMemoryStream.Clear;
Begin
  SetCapacity(0);
  FSize := 0;
  FPosition := 0;
End;

Procedure TfsMemoryStream.LoadFromStream(Stream: TStream);
Var
  Count: Longint;
Begin
  Stream.Position := 0;
  Count := Stream.Size;
  SetSize(Count);
  If Count <> 0 Then Stream.ReadBuffer(FMemory^, Count);
End;

Procedure TfsMemoryStream.LoadFromFile(Const FileName: String);
Var
  Stream: TStream;
Begin
  Stream := TFileStream.Create(FileName, fmOpenRead Or fmShareDenyWrite);
  Try
    LoadFromStream(Stream);
  Finally
    Stream.Free;
  End;
End;

Procedure TfsMemoryStream.SetCapacity(NewCapacity: Longint);
Begin
  SetPointer(Realloc(NewCapacity), FSize);
  FCapacity := NewCapacity;
End;

Procedure TfsMemoryStream.SetSize(NewSize: Longint);
Var
  OldPosition: Longint;
Begin
  OldPosition := FPosition;
  SetCapacity(NewSize);
  FSize := NewSize;
  If OldPosition > NewSize Then Seek(0, soFromEnd);
End;

Function TfsMemoryStream.Realloc(Var NewCapacity: Longint): Pointer;
Begin
  If NewCapacity > 0 Then
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) And Not (MemoryDelta - 1);
  Result := Memory;
  If NewCapacity <> FCapacity Then
    Begin
      If NewCapacity = 0 Then
        Begin
          GlobalFreePtr(Memory);
          Result := Nil;
        End
      Else
        Begin
          If Capacity = 0 Then
            {$IFDEF DCC2005Win32OrLater}
            {$WARN SYMBOL_PLATFORM OFF}
            {$ENDIF}
            Result := GlobalAllocPtr(HeapAllocFlags, NewCapacity)
          Else
            Result := GlobalReallocPtr(Memory, NewCapacity, HeapAllocFlags);
          {$IFDEF DCC2005Win32OrLater}
          {$WARN SYMBOL_PLATFORM ON}
          {$ENDIF}
          If Result = Nil Then Raise EStreamError.Create('Memory Stream Error');
        End;
    End;
End;

Function TfsMemoryStream.Write(Const Buffer; Count: Longint): Longint;
Var
  Pos: Longint;
Begin
  If (FPosition >= 0) And (Count >= 0) Then
    Begin
      Pos := FPosition + Count;
      If Pos > 0 Then
        Begin
          If Pos > FSize Then
            Begin
              If Pos > FCapacity Then
                SetCapacity(Pos);
              FSize := Pos;
            End;
          System.Move(Buffer, Pointer(Longint(FMemory) + FPosition)^, Count);
          FPosition := Pos;
          Result := Count;
          Exit;
        End;
    End;
  Result := 0;
End;

// End TfsStream ---------------------------------------------

Procedure TfsDataSet.WriteCacheBlob;
Var
  i: Integer;
  aListBlobCacheObject: TfsListBlobCacheObject;
  aStream: TfsStream;
Begin
  For i := 0 To fListBlobCache.Count - 1 Do
    Begin
      aListBlobCacheObject := pointer(fListBlobCache[i]);
      If Not aListBlobCacheObject.fDirty Then continue;
      aStream := CreateNormalBlobStream(aListBlobCacheObject.Field, bmWrite, BlobChunkSize);
      Try
        aListBlobCacheObject.BlobData.Position := 0;
        aStream.CopyFrom(aListBlobCacheObject.BlobData, aListBlobCacheObject.BlobData.Size);
      Finally
        aStream.free;
      End;
    End;
End;

Procedure TfsDataSet.RefreshCacheBlob;
Var
  i: Integer;
  aListBlobCacheObject: TfsListBlobCacheObject;
  aStream: TfsStream;
Begin

  For i := 0 To fListBlobCache.Count - 1 Do
    Begin
      aListBlobCacheObject := pointer(fListBlobCache[i]);
      aStream := CreateNormalBlobStream(aListBlobCacheObject.Field, bmRead, BlobChunkSize);
      Try
        If aStream <> Nil Then
          //If aStream.Size > 0 then
          Begin
            aListBlobCacheObject.BlobData.Clear;
            aListBlobCacheObject.BlobData.Position := 0;
            If aStream.Size > 0 Then
              aListBlobCacheObject.BlobData.CopyFrom(aStream, aStream.Size);
          End;
        aListBlobCacheObject.BlobData.Position := 0;
      Finally
        aStream.free;
      End;
    End;
End;

Function TfsDataSet.CreateCacheBlobStream(aField: TField;
  aMode: TBlobStreamMode; aChunkSize: Longint): TfsStream;
Var
  aStream: TfsStream;
  l: Integer;
  aListBlobCacheObject: TfsListBlobCacheObject;
Begin
  aListBlobCacheObject := Nil;
  For l := 0 To fListBlobCache.Count - 1 Do
    Begin
      aListBlobCacheObject := pointer(fListBlobCache[l]);
      If aListBlobCacheObject.Field = aField Then break;
      aListBlobCacheObject := Nil;
    End;
  If aListBlobCacheObject = Nil Then
    Begin
      aListBlobCacheObject := TfsListBlobCacheObject.Create(aChunkSize);
      aListBlobCacheObject.Field := aField;
      If aMode <> bmWrite Then
        Begin
          aStream := CreateNormalBlobStream(aField, aMode, aChunkSize);
          Try
            aListBlobCacheObject.BlobData.CopyFrom(aStream, aStream.Size);
          Finally
            aStream.Free;
          End;
        End;
      fListBlobCache.Add(aListBlobCacheObject);
    End;
  If amode = bmWrite Then aListBlobCacheObject.BlobData.Clear;
  inc(aListBlobCacheObject.fBlobCount);
  If aMode <> bmReadWrite Then
    aListBlobCacheObject.BlobData.position := 0;
  If aMode = bmWrite Then aListBlobCacheObject.fDirty := True;
  Result := TfsCacheBlobStream.Create(aChunkSize);
  TfsCacheBlobStream(Result).fListBlobCacheObject := aListBlobCacheObject;
End;

Function TfsDataSet.CreateNormalBlobStream(aField: TField;
  aMode: TBlobStreamMode;
  aChunkSize: Longint): TfsStream;
Var
  bsRecBuf: PChar;

Begin
  Assert(Assigned(aField));
  GetActiveRecBuf(bsRecBuf);
  //If afield.DataSet = Nil then Exit;
  //If afield.FieldName = '' then Exit;
  If (BlobMode = bmDirect) Then
    Result := TfsDirectBlobStream.Create(aField As TBlobField, aMode, aChunkSize)
  Else If (Dictionary.FieldBlobLevelComp[afield.FieldNo - 1] <> blNone) Or (BlobMode In [bmInMemory]) Then
    // if blob is compressed or inmemory
    Result := TfsMemoryBlobStream.Create(aField As TBlobField, aMode, aChunkSize)
  Else // if blob is direct or cachedirect
    Result := TfsDirectBlobStream.Create(aField As TBlobField, aMode, aChunkSize);
  Result.Position := 0;
End;

Constructor TfsDirectBlobStream.Create(aField: TBlobField; aMode: TBlobStreamMode; aChunkSize: Longint);
Var
  OpenMode: TffOpenMode;
  IsNull: boolean;
Begin
  OpenMode := omReadWrite;

  Inherited Create(aChunkSize);

  bsMode := aMode;
  bsField := aField;
  bsTable := bsField.DataSet As TfsDataSet;
  bsFieldNo := bsField.FieldNo;
  bsChunkSize := aChunkSize;
  If Not bsTable.GetActiveRecBuf(bsRecBuf) Then
    Exit;

  bsTable.dsGetBlobHandle(bsRecBuf, bsField, IsNull, bsBlobNr);
  If Not bsField.Modified Then
    Begin
      If (aMode = bmRead) Then
      Else {BLOB stream mode is not readonly}
        Begin
          If aField.ReadOnly Then
            RaiseFSErrorObj(aField, fsdse_BLOBAccessNoMatch);
          If Not (bsTable.State In [dsEdit, dsInsert]) Then
            RaiseFSErrorObj(aField, fsdse_BLOBTblNoEdit);
          OpenMode := omReadWrite;
        End;
      bsTable.dsBlobOpenMode := OpenMode;
    End;
  bsOpened := True;
  If (aMode = bmWrite) Then
    Begin
      If Not bsTable.GetActiveRecBuf(bsRecBuf)
        Or (bsTable.dsGetBlobHandle(bsRecBuf, bsField,
        IsNull, bsBlobNr) <> DBIERR_NONE) Or IsNull Then
        Begin
          bsBlobNr.iLow := 0;
          bsBlobNr.iHigh := 0;
        End;
      Truncate;
    End;
End;
{--------}

Destructor TfsDirectBlobStream.Destroy;
Begin
  If bsOpened Then
    Begin
      If bsModified Then
        bsField.Modified := True;
      If Not bsField.Modified Then
        bsTable.dsFreeBlob(bsRecBuf, bsField);
    End;
  If bsModified And bsField.Modified Then
    Begin
      Try
        bsTable.DataEvent(deFieldChange, Longint(bsField));
      Except
        Raise;
      End;
    End;
  Inherited Destroy;
End;

Function TfsDirectBlobStream.GetSize: Longint;
Begin
  Result := Inherited GetSize;
End;

Function TfsDirectBlobStream.bsGetBlobHandleSize: Longint;
Var
  Status: TffResult;
  IsNull: Boolean;
  BLOBNr: TffInt64;
Begin
  Result := 0;
  If bsOpened Then
    Begin
      Status := bsTable.dsGetBlobHandle(bsRecBuf, bsField, IsNull, BLOBNr);
      If (Status = DBIERR_NONE) And (Not IsNull) Then
        Status := bsTable.ServerEngine.BLOBGetLength(bsTable.CursorID, BLOBNr, Result);
      Check(Status);
    End;
End;

Function TfsDirectBlobStream.Read(Var aBuffer; aCount: Longint): Longint;
Var
  Status: TffResult;
  T, N: Integer;
  IsNull: Boolean;
  BLOBNr: TffInt64;
  Dest: Pointer;
  BytesRead: TffWord32;
Begin
  Result := 0;
  If bsOpened Then
    Begin
      T := 0;
      bsCancel := False;
      Status := bsTable.dsGetBlobHandle(bsRecBuf, bsField, ISNull, BLOBNr);

      If (Status = DBIERR_NONE) And (Not IsNull) Then
        Begin
          While aCount > 0 Do
            Begin
              If bsChunkSize = 0 Then
                N := aCount
              Else If aCount > bsChunkSize Then
                N := bsChunkSize
              Else
                N := aCount;
              Dest := @PChar(@aBuffer)[T];
              Status := bsTable.ServerEngine.BLOBRead(bsTable.CursorID,
                bsFieldNo,
                BLOBNr,
                bsPosition,
                N,
                Dest^,
                BytesRead);
              Result := BytesRead;
              Case Status Of
                DBIERR_NONE,
                  DBIERR_ENDOFBLOB:
                  Begin
                    inc(bsPosition, Result);
                  End;
                DBIERR_INVALIDBLOBoffset:
                  Result := 0;
                Else
                  RaisefsErrorCode(Status);
              End; {case}
              If bsCancel Then RaisefsErrorCode(DBIERR_ENDOFBLOB);
              dec(aCount, Result);
              Inc(T, Result);

              { If fewer bytes were returned than requested then
                we have reached the end of the BLOB. }
              If Result < N Then
                break;
            End;
        End;
      Result := T;
    End;
End;
{--------}

Function TfsDirectBlobStream.Write(Const aBuffer; aCount: Longint): Longint;
Var
  T, N: Integer;
  BLOBNr: TffInt64;
  Status: TffResult;
  Src: Pointer;
  UserTransactionStarted: boolean;
Begin
  Result := 0;
  If bsOpened And (aCount > 0) Then
    Begin
      If bsTable.BlobAutoStartTransaction Then
        Begin
          Try
            UserTransactionStarted := bsTable.Database.InTransaction;
            If Not UserTransactionStarted Then
              Begin
                bsTable.Database.StartTransaction;
                bsTable.fBlobStartedTransaction := True;
              End;

            T := 0;
            Status := bsTable.dsForceBLOB(bsRecBuf, bsField, BLOBNr);
            bsCancel := False;
            If (Status = DBIERR_NONE) Then
              While (aCount > 0) And (Status = DBIERR_NONE) Do
                Begin
                  If bsChunkSize = 0 Then
                    N := aCount
                  Else If aCount > bsChunkSize Then
                    N := bsChunkSize
                  Else
                    N := aCount;
                  Src := @PChar(@aBuffer)[T];
                  Status := bsTable.ServerEngine.BLOBWrite(bsTable.CursorID,
                    bsFieldNo,
                    BLOBNr,
                    bsPosition,
                    N,
                    Src^);
                  Check(Status);
                  inc(bsPosition, N);
                  inc(T, N);
                  Dec(aCount, N);
                  If bsCancel Then
                    RaisefsErrorCode(DBIERR_ENDOFBLOB);
                End;
            Result := T;
            bsModified := True;
          Except
            Try
              If bsTable.GetActiveRecBuf(bsRecBuf) Then
                bsTable.dsSetBLOBHandle(bsRecBuf, bsField, bsBlobNr);
            Except
            End;
            If bsTable.fBlobStartedTransaction Then
              If bsTable.Database.InTransaction Then
                Begin
                  bsTable.fBlobStartedTransaction := False;
                  bsTable.Database.Rollback;
                End;
            Raise;
          End;
        End
      Else
        Begin
          T := 0;
          Status := bsTable.dsForceBLOB(bsRecBuf, bsField, BLOBNr);
          bsCancel := False;
          Try
            If (Status = DBIERR_NONE) Then
              While (aCount > 0) And (Status = DBIERR_NONE) Do
                Begin
                  If bsChunkSize = 0 Then
                    N := aCount
                  Else If aCount > bsChunkSize Then
                    N := bsChunkSize
                  Else
                    N := aCount;
                  Src := @PChar(@aBuffer)[T];
                  Status := bsTable.ServerEngine.BLOBWrite(bsTable.CursorID,
                    bsFieldNo,
                    BLOBNr,
                    bsPosition,
                    N,
                    Src^);
                  Check(Status);
                  inc(bsPosition, N);
                  inc(T, N);
                  Dec(aCount, N);
                  If bsCancel Then
                    RaisefsErrorCode(DBIERR_ENDOFBLOB);
                End;
            Result := T;
            bsModified := True;
          Except
            Try
              If bsTable.GetActiveRecBuf(bsRecBuf) Then
                bsTable.dsSetBLOBHandle(bsRecBuf, bsField, bsBlobNr);
            Except
            End;
            Raise;
          End;
        End;
    End;
End;

Function TfsDirectBlobStream.Seek(aoffset: Longint; aOrigin: Word): Longint;
Begin
  Case aOrigin Of
    soFromBeginning: bsPosition := aoffset;
    soFromCurrent: inc(bsPosition, aoffset);
    soFromEnd: bsPosition := bsGetBlobHandleSize + aoffset;
  End;
  Result := bsPosition;
End;

Procedure TfsDirectBlobStream.Truncate;
Var
  UserTransactionStarted: boolean;
  IsNull: Boolean;
  aResult: TffResult;
Begin
  If bsOpened Then
    Begin
      If Not bsTable.GetActiveRecBuf(bsRecBuf)
        Or (bsTable.dsGetBlobHandle(bsRecBuf, bsField,
        IsNull, bsBlobNr) <> DBIERR_NONE) Or IsNull Then
        Begin
          bsBlobNr.iLow := 0;
          bsBlobNr.iHigh := 0;
        End;
      If bsTable.BlobAutoStartTransaction Then
        Begin
          Try
            UserTransactionStarted := bsTable.Database.InTransaction;
            If Not UserTransactionStarted Then
              Begin
                bsTable.Database.StartTransaction;
                bsTable.fBlobStartedTransaction := True;
              End;

            aResult := bsTable.dsTruncateBlob(bsRecBuf, bsField, bsPosition);
            Check(aResult);
            bsModified := True;
          Except
            Try
              If bsTable.GetActiveRecBuf(bsRecBuf) Then
                bsTable.dsSetBLOBHandle(bsRecBuf, bsField, bsBlobNr);
            Except
            End;
            If bsTable.fBlobStartedTransaction Then
              If bsTable.Database.InTransaction Then
                Begin
                  bsTable.fBlobStartedTransaction := False;
                  bsTable.Database.Rollback;
                End;
            Raise;
          End;
        End
      Else
        Begin
          Try
            aResult := bsTable.dsTruncateBlob(bsRecBuf, bsField, bsPosition);
            Check(aResult);
            bsModified := True;
          Except
            Try
              If bsTable.GetActiveRecBuf(bsRecBuf) Then
                bsTable.dsSetBLOBHandle(bsRecBuf, bsField, bsBlobNr);
            Except
              Raise;
            End;
            Raise;
          End;
        End;
    End;
  Inherited;
End;

Constructor TfsMemoryBlobStream.Create(aField: TBlobField; aMode: TBlobStreamMode; aChunkSize: Longint);
Var
  OpenMode: TffOpenMode;
  IsNull: boolean;
Begin
  Inherited Create(aChunkSize);
  bsModified := False;
  bsMode := aMode;
  bsField := aField;
  bsTable := bsField.DataSet As TFSDataSet;
  bsFieldNo := bsField.FieldNo;
  bsChunkSize := aChunkSize;
  If Not bsTable.GetActiveRecBuf(bsRecBuf) Then
    Exit;

  bsTable.dsGetBlobHandle(bsRecBuf, bsField, IsNull, bsBlobNr);
  If (aMode = bmRead) Then
    OpenMode := omReadOnly
  Else {BLOB stream mode is not readonly}
    Begin
      If aField.ReadOnly Then
        RaiseFSErrorObj(aField, fsdse_BLOBAccessNoMatch);
      If Not (bsTable.State In [dsEdit, dsInsert, dsNewValue]) Then
        RaiseFSErrorObj(aField, fsdse_BLOBTblNoEdit);
      OpenMode := omReadWrite;
    End;
  bsTable.dsBlobOpenMode := OpenMode;
  bsOpened := True;
  If (aMode = bmWrite) Then
    Truncate
  Else If (aMode = bmRead) Then
    Begin
      Try
        ReadBlobToStream(aField.FieldNo);
      Finally
        // free because data is transfered to memory stream
        bsTable.dsFreeBlob(bsRecBuf, aField);
      End;
    End;
End;

Destructor TfsMemoryBlobStream.Destroy;
Var
  UserTransactionStarted: Boolean;
  IsNull, ReallyWrite: Boolean;

  Procedure BCompress;
  Var
    aOutput: TfsMemoryStream;
  Begin
    Case bstable.Dictionary.FieldBlobLevelComp[bsFieldNo - 1] Of
      blNone:
        Begin
          WriteToBlob(TfsMemoryStream(Self).memory^, Self.size);
        End;
      blFastest:
        Begin
          aOutput := TfsMemoryStream.Create(ChunkSize);
          Try
            aOutput.Position := 0;
            Position := 0;
            If Self.Size > 0 Then
              fsZCompress(zcFastest, Self, aOutput);
            aOutput.Position := 0;
            WriteToBlob(aOutput.memory^, aOutput.size);
          Finally
            aOutput.free;
          End;
        End;
      blDefault:
        Begin
          aOutput := TfsMemoryStream.Create(ChunkSize);
          Try
            aOutput.Position := 0;
            Position := 0;
            If Self.Size > 0 Then
              fsZCompress(zcDefault, Self, aOutput);
            aOutput.Position := 0;
            WriteToBlob(aOutput.memory^, aOutput.size);
          Finally
            aOutput.free;
          End;
        End;
      blMax:
        Begin
          aOutput := TfsMemoryStream.Create(ChunkSize);
          Try
            aOutput.Position := 0;
            Position := 0;
            If Self.Size > 0 Then
              fsZCompress(zcMax, Self, aOutput);
            aOutput.Position := 0;
            WriteToBlob(aOutput.memory^, aOutput.size);
          Finally
            aOutput.free;
          End;
        End;
    End;
  End;
Begin
  ReallyWrite := False;
  If bsOpened Then
    Begin
      If bsModified And (bsMode = bmwrite) And (bsTable.State In [dsEdit, dsInsert]) Then
        Begin
          If bsModified Then
            bsField.Modified := True;
          If Not bsTable.GetActiveRecBuf(bsRecBuf)
            Or (bsTable.dsGetBlobHandle(bsRecBuf, bsField,
            IsNull, bsBlobNr) <> DBIERR_NONE) Or IsNull Then
            Begin
              bsBlobNr.iLow := 0;
              bsBlobNr.iHigh := 0;
            End;

          If bsTable.BlobAutoStartTransaction Then
            Begin
              Try
                UserTransactionStarted := bsTable.Database.InTransaction;
                If Not UserTransactionStarted Then
                  Begin
                    bsTable.Database.StartTransaction;
                    bsTable.fBlobStartedTransaction := True;
                  End;

                If Self.Size > 0 Then
                  BCompress
                Else
                  Check(bsTable.dsTruncateBlob(bsRecBuf, bsField, 0));
                If bsTable.BlobMode In [bmCache] Then
                  bsTable.dsFreeBlob(bsRecBuf, bsField);
                ReallyWrite := True;
              Except
                Try
                  If bsTable.GetActiveRecBuf(bsRecBuf) Then
                    bsTable.dsSetBLOBHandle(bsRecBuf, bsField, bsBlobNr);
                Except
                End;
                If bsTable.Database.InTransaction Then
                  Begin
                    bsTable.fBlobStartedTransaction := False;
                    bsTable.Database.Rollback;
                  End;
                Raise;
              End;
            End
          Else
            Begin
              Try
                If Self.Size > 0 Then
                  BCompress
                Else
                  Check(bsTable.dsTruncateBlob(bsRecBuf, bsField, 0));
                bsTable.dsFreeBlob(bsRecBuf, bsField);
                ReallyWrite := True;
              Except
                Try
                  If bsTable.GetActiveRecBuf(bsRecBuf) Then
                    bsTable.dsSetBLOBHandle(bsRecBuf, bsField, bsBlobNr);
                Except
                  Raise;
                End;
              End;
            End;
        End;
      If Not bsField.Modified Then
        If bsTable.BlobMode In [bmInMemory, bmDirect, bmAuto] Then
          bsTable.dsFreeBlob(bsRecBuf, bsField);
    End;
  If bsModified And bsField.Modified And (bsMode = bmwrite) And ReallyWrite
    And (bsTable.State In [dsEdit, dsInsert]) Then
    Begin
      Try
        bsTable.DataEvent(deFieldChange, Longint(bsField));
      Except
        Raise;
      End;
    End;
  Inherited Destroy;
End;

Function TfsMemoryBlobStream.bsGetBlobHandleSize: Longint;
Var
  Status: TffResult;
  IsNull: Boolean;
  BLOBNr: TffInt64;
Begin
  Result := 0;
  If bsOpened Then
    Begin
      Status := bsTable.dsGetBlobHandle(bsRecBuf, bsField, IsNull, BLOBNr);
      If (Status = DBIERR_NONE) And (Not IsNull) Then
        Status := bsTable.ServerEngine.BLOBGetLength(bsTable.CursorID, BLOBNr, Result);
      Check(Status);
    End;
End;

Procedure TfsMemoryBlobStream.SetSize(NewSize: Integer);
Begin
  Inherited SetSize(NewSize);
  bsModified := True;
End;

Function TfsMemoryBlobStream.Write(Const Buffer; Count: Integer): Longint;
Begin
  Result := Inherited Write(Buffer, Count);
  bsModified := True;
End;

Function TfsMemoryBlobStream.WriteToBlob(Const aBuffer; aCount: Longint): Longint;
Var
  T, N: Integer;
  BLOBNr: TffInt64;
  Status: TffResult;
  Src: Pointer;
Begin
  Result := 0;
  If bsOpened And (aCount > 0) Then
    Begin
      T := 0;
      Check(bsTable.dsTruncateBlob(bsRecBuf, bsField, 0));
      Status := bsTable.dsForceBLOB(bsRecBuf, bsField, BLOBNr);
      Position := 0;
      bsposition := 0;
      bsCancel := False;
      If (Status = DBIERR_NONE) Then
        While (aCount > 0) And (Status = DBIERR_NONE) Do
          Begin
            If bsChunkSize = 0 Then
              N := aCount
            Else If aCount > bsChunkSize Then
              N := bsChunkSize
            Else
              N := aCount;

            Src := @PChar(@abuffer)[T];
            Status := bsTable.ServerEngine.BLOBWrite(bsTable.CursorID, bsFieldNo,
              BLOBNr,
              bsPosition,
              N,
              Src^);

            Check(Status);
            inc(bsPosition, N);
            inc(T, N);
            Dec(aCount, N);
            If bsCancel Then
              RaiseFSErrorCode(DBIERR_ENDOFBLOB);
          End;
      Result := T;
      bsModified := True;
    End;
End;

Function TfsMemoryBlobStream.ReadBlobToStream(bsFieldNo: Integer): Longint;
Var
  Status: TffResult;
  N: Integer;
  IsNull: Boolean;
  BLOBNr: TffInt64;
  BytesRead: TffWord32;
  abuffer: pointer;
  aCount, bsPosition: Longint;
  bsRecBuf: PChar;
  TmpStream: TfsMemoryStream;
Begin
  TmpStream := Nil;

  Result := 0;
  If Not bsTable.GetActiveRecBuf(bsRecBuf) Then
    Exit;
  ACount := bsGetBlobHandleSize;
  If ACount = 0 Then Exit;
  Result := ACount;
  If bsChunkSize <= 0 Then
    N := aCount
  Else If aCount > bsChunkSize Then
    N := bsChunkSize
  Else
    N := aCount;

  GetMem(abuffer, n + 1);
  If bsTable.Dictionary.FieldBlobLevelComp[bsfield.FieldNo - 1] <> blNone Then
    Begin
      TmpStream := TfsMemoryStream.Create(ChunkSize);
      TmpStream.position := 0;
    End;
  Position := 0;
  bsPosition := 0;
  Try
    Status := bsTable.dsGetBlobHandle(bsRecBuf, bsField, ISNull, BLOBNr);
    If (Status = DBIERR_NONE) And (Not IsNull) Then
      While aCount > 0 Do
        Begin
          If bsChunkSize <= 0 Then
            N := aCount
          Else If aCount > bsChunkSize Then
            N := bsChunkSize
          Else
            N := aCount;

          Status := bsTable.ServerEngine.BLOBRead(bsTable.CursorID, bsFieldNo,
            BLOBNr,
            bsPosition,
            N,
            abuffer^,
            BytesRead);
          Result := BytesRead;
          If BytesRead > 0 Then
            If bsTable.Dictionary.FieldBlobLevelComp[bsfield.FieldNo - 1] <> blNone Then
              TmpStream.writebuffer(abuffer^, BytesRead)
            Else
              writebuffer(abuffer^, BytesRead);
          Case Status Of
            DBIERR_NONE,
              DBIERR_ENDOFBLOB:
              inc(bsPosition, Result);
            DBIERR_INVALIDBLOBoffset:
              Result := 0;
            Else
              RaiseFSErrorCode(Status);
          End;
          dec(aCount, Result);
          If bsCancel Then
            RaiseFSErrorCode(DBIERR_ENDOFBLOB);
          If Result < N Then
            break;
        End;
    If bsTable.Dictionary.FieldBlobLevelComp[bsfield.FieldNo - 1] <> blNone Then
      If TmpStream.Size > 0 Then
        Begin
          TmpStream.position := 0;
          Position := 0;
          fsZDecompress(TmpStream, Self);
        End;
    Position := 0;
  Finally
    FreeMem(abuffer);
    If bsTable.Dictionary.FieldBlobLevelComp[bsfield.FieldNo - 1] <> blNone Then
      TmpStream.free;
    bsModified := False;
  End;
End;

{--------}

Procedure TfsMemoryBlobStream.Truncate;
Begin
  SetSize(Position);
  bsModified := True;
End;

Procedure TfsMemoryBlobStream.Clear;
Begin
  bsModified := True;
  Inherited Clear;
End;

{ TfsCacheBlobStream }

Constructor TfsCacheBlobStream.Create(aChunkSize: Longint);
Begin
  Inherited Create(aChunkSize);
End;

Destructor TfsCacheBlobStream.Destroy;
Begin
  dec(fListBlobCacheObject.fBlobCount);
  If fModified Then fListBlobCacheObject.fDirty := True;
  If fModified Then
    TBlobField(fListBlobCacheObject.Field).Modified := True;
  If fModified Then
    Begin
      Try
        With TfsDataSet(fListBlobCacheObject.Field.Dataset) Do
          Begin
            DataEvent(deFieldChange, Longint(fListBlobCacheObject.Field));
          End;
      Except
        Raise;
      End;
    End;
  Inherited;
End;

Function TfsCacheBlobStream.Read(Var Buffer; Count: Integer): Longint;
Begin
  Result := fListBlobCacheObject.BlobData.Read(Buffer, Count);
End;

Function TfsCacheBlobStream.Seek(Offset: Integer; Origin: Word): Longint;
Begin
  Result := fListBlobCacheObject.BlobData.Seek(Offset, Origin);
End;

Function TfsCacheBlobStream.Write(Const Buffer; Count: Integer): Longint;
Begin
  fModified := True;
  Result := fListBlobCacheObject.BlobData.Write(Buffer, Count);
End;

Procedure TfsCacheBlobStream.Clear;
Begin
  fModified := True;
  fListBlobCacheObject.BlobData.Clear;
End;

Procedure TfsCacheBlobStream.Truncate;
Begin
  fModified := True;
  fListBlobCacheObject.BlobData.Truncate;
End;

{ TfsListBlobCacheObject }

Constructor TfsListBlobCacheObject.Create(aChunkSize: Longint);
Begin
  Inherited Create;
  BlobData := TfsMemoryStream.Create(aChunkSize);
End;

Destructor TfsListBlobCacheObject.Destroy;
Begin
  If fBlobCount <> 0 Then
    Raise Exception.Create('ListBlobCache Count not 0');
  BlobData.Free;
  Inherited;
End;

Function TFSDataSet.dsGetServerEngine: TFSBaseServerEngine;
Begin
  If Assigned(dsServerEngine) And Active Then
    Result := dsServerEngine
  Else
    Result := Session.ServerEngine;
End;
{--------}

Function TFSBaseDatabase.bdGetServerEngine: TFSBaseServerEngine;
Begin
  If Assigned(bdServerEngine) And Active Then
    Result := bdServerEngine
  Else
    Result := Session.ServerEngine;
End;
{--------}

Procedure TFSBaseDatabase.bdRefreshTimeout; {new !!.11}
Var
  Idx: Integer;
Begin
  If Active Then
    Begin
      Check(ServerEngine.DatabaseSetTimeout(bdDatabaseID, GetTimeout));
      For Idx := 0 To Pred(OwnedDBItems.Count) Do
        TFSTableProxyList(OwnedDBItems)[Idx].fsTable.dsRefreshTimeout;
    End;
End;
{--------}

Function TFSTableProxy.tpGetServerEngine: TFSBaseServerEngine;
Begin
  If Assigned(tpServerEngine) And Active Then
    Result := tpServerEngine
  Else
    Result := Session.ServerEngine;
End;
{====================================================================}

{===TFSQueryDataLink=================================================}

Constructor TFSQueryDataLink.Create(aQuery: TFSQuery);
Begin
  Inherited Create;
  FQuery := aQuery;
End;

Procedure TFSQueryDataLink.ActiveChanged;
Begin
  If FQuery.Active Then
    Begin
      If FQuery.BlobMode = bmCache Then
        FQuery.ListBlobCache.Clear;
      FQuery.quRefreshParams;
    End;
End;

{$IFDEF DCC4OrLater}

Function TFSQueryDataLink.GetDetailDataSet: TDataSet;
Begin
  Result := FQuery;
End;
{$ENDIF}

Procedure TFSQueryDataLink.RecordChanged(Field: TField);
Begin
  If (Field = Nil) And FQuery.Active Then
    Begin
      If FQuery.BlobMode = bmCache Then
        FQuery.ListBlobCache.Clear;
      FQuery.quRefreshParams;
    End;
End;

Procedure TFSQueryDataLink.CheckBrowseMode;
Begin
  If FQuery.Active Then
    FQuery.CheckBrowseMode;
End;
{=====================================================================}

{== TFSQuery =========================================================}

Constructor TFSQuery.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  { We must give dsProxy a unique name. }
  dsProxy.DBName := IntToStr(GetCurrentThreadID) + IntToStr(GetTickCount);
  FDataLink := TFSQueryDataLink.Create(Self);
  FExecuted := True;
  FParamCheck := True;
  {$IFDEF DCC4OrLater}
  FParams := TfsParams.Create(Self);
  {$ELSE}
  FParams := TfsParams.Create;
  {$ENDIF}
  FPrepared := False;
  FSQL := TStringList.Create;
  FSQLORDER := TStringList.Create;
  TStringList(FSQL).OnChange := quSQLChanged;
  TStringList(FSQLOrder).OnChange := quSQLOrderChanged;
  FStmtID := 0;
  FRowsAffected := -1;
  FCanModify := False;
  //btIndexID:=0;
End;
{--------}

Destructor TFSQuery.Destroy;
Begin
  quDisconnect;
  FDataLink.Free;
  FParams.Free;
  FSQL.Free;
  FSQLORDER.Free;
  Inherited Destroy;
End;
{--------}{begin !!.10}

Procedure TFSQuery.ExecSQL;
Var
  Dummy: TffCursorID;
Begin
  CheckInactive;
  quExecSQLStmt(omReadOnly, Dummy);
End;
{--------}

Procedure TFSQuery.quExecSQLStmt(Const aOpenMode: TffOpenMode;
  Var aCursorID: TffCursorID);
Var
  Msg: String;
  MsgLen: Integer;
  OpenCursorID: Longint;
  ParamsData: PffByteArray;
  ParamsDataLen: Integer;
  ParamsList: PfsSqlParamInfoList;
  SQLResult: TffResult;
  Stream: TStream;
  OpenCanModify: Boolean; {!!.10}
  OpenRowsAffected: Integer; {!!.10}

Begin
  Msg := '';
  MsgLen := 0;
  FRowsAffected := -1; {!!.10}
  FRecordsRead := 0; {!!.10}
  btIndexID := 0;

  { Do we have a SQL statement? }
  If FSQL.Count > 0 Then
    Begin
      { Yes.  Prepare the statement. }
      ParamsData := Nil;
      ParamsDataLen := 0;
      ParamsList := Nil;
      { Allocate & prepare the SQL statement. }

      quPreparePrim(True);

      { Are we linked to a datasource? }
      If assigned(FDataLink.DataSource) Then
        quSetParamsFromCursor;

      { Do we have parameters? }
      If FParams.Count > 0 Then
        Begin
          { Yes.  Send them to the server. }
          quBuildParams(ParamsList, ParamsData, ParamsDataLen);
          Stream := TMemoryStream.Create;
          Try
            SQLResult := ServerEngine.SQLSetParams(FStmtID, FParams.Count,
              pointer(ParamsList),
              ParamsData, ParamsDataLen,
              Stream);
            { Was the set parameters successful? }
            If SQLResult <> DBIERR_NONE Then
              Begin
                { No.  Raise an error. }
                Stream.Position := 0;
                Stream.Read(MsgLen, sizeOf(MsgLen));
                If MsgLen > 0 Then
                  Begin
                    SetLength(Msg, MsgLen);
                    Stream.Read(Msg[1], MsgLen);
                    RaiseFSErrorObjFmt(Self, fsdse_QuerySetParamsFail, [#13#10, Msg]);
                  End
                Else
                  Check(SQLResult);
              End;
          Finally
            Stream.Free;
          End;
        End;

      { Execute the query. }
      Stream := TMemoryStream.Create;
      Try
        SQLResult := ServerEngine.SQLExec(FStmtID, aOpenMode, aCursorID, Stream);
        { Was the execution successful? }
        If SQLResult <> DBIERR_NONE Then
          Begin
            { No.  Raise an error. }
            If Stream.Size > 0 Then
              Begin
                Stream.Position := 0;
                Stream.Read(MsgLen, sizeOf(MsgLen));
              End;
            If MsgLen > 0 Then
              Begin
                SetLength(Msg, MsgLen);
                Stream.Read(Msg[1], MsgLen);
                RaiseFSErrorObjFmt(Self, fsdse_QueryExecFail, [#13#10, Msg]);
              End
            Else
              Check(SQLResult);
          End;

        { Load the data dictionary, if necessary. }
        Stream.Position := 0;
        Stream.Read(OpenCursorID, SizeOf(OpenCursorID));
        aCursorID := OpenCursorID;

        If aCursorID <> 0 Then
          Begin {begin !!.10}
            Dictionary.ReadFromStream(Stream);
            Stream.Read(OpenCanModify, SizeOf(OpenCanModify));
            Stream.Read(FRecordsRead, SizeOf(FRecordsRead));
            Stream.Read(btIndexID, SizeOf(btIndexID));
          End
        Else
          Begin
            {get rows affected}
            Stream.Read(OpenRowsAffected, SizeOf(OpenRowsAffected));
            FRowsAffected := OpenRowsAffected;
            Stream.Read(FRecordsRead, SizeOf(FRecordsRead));
          End; {end !!.10}

      Finally
        Stream.Free;
        If assigned(ParamsData) Then
          FFFreemem(ParamsData, ParamsDataLen);
        If assigned(ParamsList) Then
          FFFreemem(ParamsList, SizeOf(TfsSqlParamInfo) * FParams.Count);
      End;
    End
  Else
    RaiseFSErrorObj(Self, fsdse_EmptySQLStatement);
End;
{--------}{end !!.10}
{$IFDEF DCC4OrLater}

Procedure TFSQuery.DefineProperties(Filer: TFiler);

  Function HasData: boolean;
  Begin
    { We have data to write if our parameters are different than our ancestor
      class or, if we have no ancestor class, we have 1 or more parameters. }
    If assigned(Filer.Ancestor) Then
      Result := Not FParams.IsEqual(TFSQuery(Filer.Ancestor).FParams)
    Else
      Result := (FParams.Count > 0);
  End;

Begin
  Inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', quReadParams, quWriteParams, HasData);
End;
{$ENDIF}
{--------}

Procedure TFSQuery.DestroyHandle(aHandle: TffCursorID);
Begin
  { Release any existing record locks. }
  Check(ServerEngine.RecordRelLock(dsCursorID, False));

  { Close the cursor handle, ignore errors. }
  Check(ServerEngine.CursorClose(dsCursorID));
  dsCursorID := 0;
End;
{--------}

Procedure TFSQuery.dsCloseViaProxy;
Begin
  Inherited dsCloseViaProxy;

  Unprepare;
End;
{--------}

Function TFSQuery.dsGetServerEngine: TFSBaseServerEngine;
Begin
  If Assigned(dsServerEngine) Then
    Result := dsServerEngine
  Else
    Result := Session.ServerEngine;
End;
{--------}

Function TFSQuery.GetCanModify: Boolean;
Begin
  Result := FCanModify; {!!.10}
End;
{--------}

Function TFSQuery.GetCursorHandle(aIndexName: String): TffCursorID;
Var
  Msg: String;
  an: TffCursorID;
  MsgLen: Integer;
  OpenCursorID: Longint;
  OpenMode: TffOpenMode; {!!.10}
  ParamsData: PffByteArray;
  ParamsDataLen: Integer;
  ParamsList: PfsSqlParamInfoList;
  SQLResult: TffResult;
  Stream: TStream;
  OpenRowsAffected: Integer; {!!.11}
Begin
  Result := 0;
  FExecuted := False;
  Msg := '';
  MsgLen := 0;
  btIndexID := 0;
  { Do we have a SQL statement? }
  If FSQL.Count > 0 Then
    Begin
      { Yes.  Prepare the statement. }
      ParamsData := Nil;
      ParamsDataLen := 0;
      ParamsList := Nil;
      { Allocate & prepare the SQL statement. }
      quPreparePrim(True);

      { Are we linked to a datasource? }
      If assigned(FDataLink.DataSource) Then
        quSetParamsFromCursor;

      { Do we have parameters? }
      If FParams.Count > 0 Then
        Begin
          { Yes.  Send them to the server. }
          quBuildParams(ParamsList, ParamsData, ParamsDataLen);
          Stream := TMemoryStream.Create;
          Try
            SQLResult := ServerEngine.SQLSetParams(FStmtID, FParams.Count,
              pointer(ParamsList),
              ParamsData, ParamsDataLen,
              Stream);
            { Was the set parameters successful? }
            If SQLResult <> DBIERR_NONE Then
              Begin
                { No.  Raise an error. }
                Stream.Position := 0;
                Stream.Read(MsgLen, sizeOf(MsgLen));
                If MsgLen > 0 Then
                  Begin
                    SetLength(Msg, MsgLen);
                    Stream.Read(Msg[1], MsgLen);
                    RaiseFSErrorObjFmt(Self, fsdse_QuerySetParamsFail, [#13#10, Msg]);
                  End
                Else
                  Check(SQLResult);
              End;
          Finally
            Stream.Free;
          End;
        End;

      { Execute the query. }
      If FRequestLive Then
        OpenMode := omReadWrite
      Else
        OpenMode := omReadOnly;
      Stream := TMemoryStream.Create;
      Try
        SQLResult := ServerEngine.SQLExec(FStmtID, OpenMode, dsCursorID, Stream);
        { Was the execution successful? }
        If SQLResult <> DBIERR_NONE Then
          Begin
            { No.  Raise an error. }
            If Stream.Size > 0 Then
              Begin
                Stream.Position := 0;
                Stream.Read(MsgLen, sizeOf(MsgLen));
              End;
            If MsgLen > 0 Then
              Begin
                SetLength(Msg, MsgLen);
                Stream.Read(Msg[1], MsgLen);
                RaiseFSErrorObjFmt(Self, fsdse_QueryExecFail, [#13#10, Msg]);
              End
            Else
              Check(SQLResult);
          End;

        { Load the data dictionary. }
        FCanModify := False;
        Stream.Position := 0;
        Stream.Read(OpenCursorID, SizeOf(OpenCursorID));
        If dsCursorID <> 0 Then
          Begin
            Dictionary.ReadFromStream(Stream);
            Stream.Read(OnlineCursor, SizeOf(OnlineCursor));
            Stream.Read(FRecordsRead, SizeOf(FRecordsRead));
            Stream.Read(btIndexID, SizeOf(btIndexID));
            If OnlineCursor Then
              Begin
                ServerEngine.CursorClone(dsCursorID, TffOpenMode(omReadWrite), an);
                dsCursorID := an;
              End;
            If RequestLive Then
              FCanModify := OnlineCursor;
          End
        Else
          Begin
            Stream.Read(OpenRowsAffected, SizeOf(OpenRowsAffected));
            FRowsAffected := OpenRowsAffected;
            Stream.Read(FRecordsRead, SizeOf(FRecordsRead));
          End;

        // btIndexID := OpenIndexID;
        btIndexName := Dictionary.IndexName[btIndexID];
        dsReadFieldDescs;
        Result := dsCursorID;
        FExecuted := True;
      Finally
        Stream.Free;
        If assigned(ParamsData) Then
          FFFreemem(ParamsData, ParamsDataLen);
        If assigned(ParamsList) Then
          FFFreemem(ParamsList, SizeOf(TfsSqlParamInfo) * FParams.Count);
      End;
    End
  Else
    RaiseFSErrorObj(Self, fsdse_EmptySQLStatement);

End;

Procedure TFSQuery.InternalOpen;
Begin
  Inherited InternalOpen;
  If btIndexID > 0 Then
    Check(ServerEngine.CursorSwitchToIndex(CursorID,
      btIndexName,
      IndexID,
      False));
End;
{--------}

Function TFSQuery.GetCursorProps(Var aProps: TfsCursorProperties): TffResult;
Begin
  Result := Inherited GetCursorProps(aProps);
  aProps.KeySize := Dictionary.IndexKeyLength[IndexID];
  aProps.IndexCount := Dictionary.IndexCount;
  aProps.BookMarkSize := Dictionary.BookmarkSize[IndexID];
End;
{--------}

Procedure TFSQuery.InternalClose;
Begin
  Self.ListBlobCache.Clear;
  fInRange := False;
  FExecuted := False;
  {deactivate filters}
  If Filtered Then
    dsDeactivateFilters;
  {drop filters}
  dsDropFilters;
  {clear up the fields}
  BindFields(False);
  If DefaultFields Then
    //FieldsClear(Self);

    DestroyFields;
  dsServerEngine := Nil; {!!.11}
End;
{Begin !!.01}
{--------}

Function TFSQuery.Locate(Const aKeyFields: String;
  Const aKeyValues: Variant;
  aOptions: TLocateOptions): Boolean;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  aRecNo: Longword;
Begin
  DoBeforeScroll;
  Result := quLocateRecord(aKeyFields, aKeyValues, aOptions, True);
  If Result Then
    Begin
      If Self.SupportRecNo Then
        Begin
          resync([]);
          If Self.FlipOrder Then
            ServerEngine.RecordGetSetPosition(1, Self.cursorid, ffltNoLock, Nil, aFlag, aRecNo, aRefNr, imPosition, False)
          Else
            ServerEngine.RecordGetSetPosition(-1, Self.cursorid, ffltNoLock, Nil, aFlag, aRecNo, aRefNr, imPosition, False);
          fCurrentRecord := aRecNo + 1;
          resync([]);
        End
      Else
        Resync([rmExact, rmCenter]);
      DoAfterScroll;
    End;
End;
{End !!.01}
{--------}

Function TFSQuery.Lookup(Const aKeyFields: String;
  Const aKeyValues: Variant;
  Const aResultFields: String): Variant;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  OurBuffer: PChar;
  OurFields: TList;
  FilterHandle: HDBIFilter;
Begin
  aFlag := 0;
  Result := Null;

  {make sure we're in browse mode}
  CheckBrowseMode;
  CursorPosChanged;
  {get a temporary record Buffer}
  OurBuffer := TempBuffer;
  {create list of fields}
  OurFields := TList.Create;
  Try
    {get the actual fields in the parameter aKeyFields}
    GetFieldList(OurFields, aKeyFields);
    InternalFirst;
    FilterHandle := dsCreateLookupFilter(OurFields, aKeyValues, []);
    If dsGetNextRecord(ffltNoLock, OurBuffer, Nil, aFlag, aRefNr) = 0 Then
      Begin
        If FilterEval = fseServer Then
          RestoreFilterEx
        Else
          dsDropFilter(FilterHandle);
        SetTempState(dsCalcFields);
        Try
          CalculateFields(TempBuffer);
          Result := FieldValues[aResultFields];
        Finally
          RestoreState(dsBrowse);
        End; {try..finally}
      End;
  Finally
    OurFields.Free;
  End; {try..finally}
End;
{--------}

Function TFSQuery.ParamByName(Const aName: String): TfsParam;
Begin
  Result := FParams.ParamByName(aName);
End;
{--------}

Procedure TFSQuery.Prepare;
Begin
  quPreparePrim(True);
End;
{--------}

Procedure TFSQuery.quBuildParams(Var ParamsList: PfsSqlParamInfoList;
  Var ParamsData: PffByteArray;
  Var ParamsDataLen: Integer);
Var
  aParam: TfsParam;
  aSrcBuffer: pointer;
  aTgtBuffer: pointer;
  Index: Integer;
  Offset: Integer;
  PSqlParamInfo: PfsSqlParamInfo;
Begin
  { Get memory for the params list. }
  FFGetMem(ParamsList, sizeOf(TfsSqlParamInfo) * FParams.Count);

  Offset := 0;
  ParamsDataLen := 0;

  { Fill in the parameter list. }
  For Index := 0 To Pred(FParams.Count) Do
    Begin
      aParam := FParams.Items[Index];
      PSqlParamInfo := @ParamsList^[Index];
      With PSqlParamInfo^ Do
        Begin
          piNum := Succ(Index);
          { parameter number, base 1 }
          piName := aParam.Name;
          { parameter name }
          MapVCLTypeToFF(aParam.DataType, aParam.GetDataSize, piType, piLength);
          { If this is a BLOB then we must obtain the actual size of the data. }
          If piType In [fstBLOB..fstBLOBGraphic] Then
            Begin
              // if pi
              piLength := aParam.GetDataSize;
            End;
          { data type & length }
          piOffset := Offset;
          { offset in data buffer }
          inc(Offset, piLength);
          inc(ParamsDataLen, piLength);
          // support null
          piIsNull := aParam.IsNull;
        End;
    End;

  { Allocate memory for the parameter data buffer. }
  FFGetMem(ParamsData, ParamsDataLen);

  { Fill the parameter data buffer. }
  For Index := 0 To Pred(FParams.Count) Do
    Begin
      aParam := FParams.Items[Index];
      PSqlParamInfo := @ParamsList^[Index];
      { Convert the data into FF format and store it in the buffer. }
      With PSqlParamInfo^ Do
        Begin
          {Begin !!.13}
          aTgtBuffer := @ParamsData^[piOffset];
          If piType In [fstBLOB..fstBLOBGraphic] Then
            Begin
              If piLength > 0 Then
                aParam.GetData(aTgtBuffer);
            End
          Else
            Begin
              FFGetmem(aSrcBuffer, aParam.GetDataSize);
              Try
                aParam.GetData(aSrcBuffer);
                MapDataToFS(piType, aParam.GetDataSize, aSrcBuffer, aTgtBuffer);
              Finally
                FFFreemem(aSrcBuffer, aParam.GetDataSize);
              End;
            End; { if..else }
          {End !!.13}
        End; { with }
    End; { for }

End;
{--------}

Procedure TFSQuery.quDisconnect;
Begin
  Close;
  Unprepare;
End;
{--------}

Procedure TFSQuery.quFreeStmt;
Var
  Result: TffResult;
Begin
  If FStmtID > 0 Then
    Begin
      Result := ServerEngine.SQLFree(FStmtID);
      FStmtID := 0;
      If Not (csDestroying In ComponentState) Then
        Check(Result);
    End;
End;
{--------}

Function TFSQuery.quGetDataSource: TDataSource;
Begin
  Result := FDataLink.DataSource;
End;
{Begin !!.01}
{--------}

Function TFSQuery.quLocateRecord(Const aKeyFields: String;
  Const aKeyValues: Variant;
  aOptions: TLocateOptions;
  aSyncCursor: Boolean): Boolean;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  OurBuffer: PChar;
  OurFields: TList;
  FilterHandle: HDBIFilter;
  Status: TffResult;
Begin
  { Make sure we're in browse mode. }
  CheckBrowseMode;
  CursorPosChanged;
  { Get a temporary record buffer. }
  OurBuffer := TempBuffer;
  { Create list of fields. }
  OurFields := TList.Create;
  Try
    { Get the actual fields in the parameter aKeyFields. }
    GetFieldList(OurFields, aKeyFields);

    { Locate the record via a filter. }
    InternalFirst;
    FilterHandle := dsCreateLookupFilter(OurFields, aKeyValues, aOptions);
    Status := dsGetNextRecord(ffltNoLock, OurBuffer, Nil, aFlag, aRefNr);
    If FilterEval = fseServer Then
      RestoreFilterEx
    Else
      dsDropFilter(FilterHandle);
  Finally
    OurFields.Free;
  End; {try..finally}
  Result := (Status = DBIERR_NONE);
End;
{End !!.01}
{--------}

Function TFSQuery.quGetParamCount: Word;
Begin
  Result := FParams.Count;
End;
{--------}{begin !!.10}

Function TFSQuery.quGetRowsAffected: Integer;
Begin
  Result := FRowsAffected;
End;
{--------}{end !!.10}

Function TFSQuery.quParseSQL(aStmt: String; createParams: boolean;
  aParams: TfsParams): String;
Const
  MaxNest = 5;
  ParamNameTerminators = [#9, #10, #13, ' ', ',', ';', ')', '=', {!!.11}
  '>', '<']; {!!.11}
  StringDelims = ['''', '"', '`'];
  { Things that delimit a string. }
Var
  CurPos, EndPos, NameEndPos, NameStartPos, StartPos: Integer;
  DelimStackTop: Integer;
  DelimStack: Array[1..MaxNest] Of char;
  aLen: Integer;
Begin
  { Parameter format:
     :<contiguous text>
     :"<text>" (i.e., for multiword param names)

    Excluded:
     double colons
     a colon occuring within double or single quotes
  }

  If aStmt = '' Then
    Exit;

  Result := aStmt;

  CurPos := 1;
  DelimStackTop := 0;

  Repeat

    { Skip past the leading bytes of multi-byte character set. }
    While Result[CurPos] In LeadBytes Do
      inc(CurPos);

    { Is this the start of a literal? }
    If Result[CurPos] In StringDelims Then
      Begin
        { Yes.  Skip to the end of the literal.  Note that we can have nested
          delimiters. }
        inc(DelimStackTop);
        DelimStack[DelimStackTop] := Result[CurPos];

        Repeat

          inc(CurPos);
          aLen := Length(Result);

          While (CurPos < aLen) And
            (Not (Result[CurPos] In StringDelims)) Do
            Begin
              { Skip past leading bytes of MBCS. }
              While Result[CurPos] In LeadBytes Do
                inc(CurPos);
              { Skip this char. }
              inc(CurPos);
            End;

          If CurPos > aLen Then
            break;

          { Does this delimiter match the beginning delimiter? }
          If Result[CurPos] = DelimStack[DelimStackTop] Then
            { Yes. Decrement the stack.  We will leave this loop once
              the stack is empty (e.g., DelimStackTop = 0). }
            dec(DelimStackTop)
          Else If DelimStackTop < MaxNest Then
            Begin
              { No.  We have encountered nested delimiters.  Add the delimiter
                to the stack. }
              inc(DelimStackTop);
              DelimStack[DelimStackTop] := Result[CurPos];
            End;

        Until DelimStackTop = 0;

        { Move to the character after the final string delimiter. }
        inc(CurPos);

      End
    Else If (Result[CurPos] = ':') Then
      Begin
        { Is this a double colon? }
        If (Result[CurPos + 1] = ':') Then
          inc(CurPos, 2)
        Else
          Begin
            { No.  We have found a single colon.  Grab the name.  Note that the
              name may be in single quotes. }
            StartPos := CurPos;
            inc(CurPos);
            { Is the colon followed by a double quote?  In other words, is the
              param name delimited by double quotes? }
            If Result[CurPos] = '"' Then
              Begin
                inc(CurPos);
                NameStartPos := CurPos;
                Repeat
                  inc(CurPos);
                Until Result[CurPos] = '"';
                EndPos := CurPos;
                NameEndPos := CurPos - 1;
              End
            Else
              Begin
                NameStartPos := CurPos;
                Repeat
                  inc(CurPos);
                Until Result[CurPos] In ParamNameTerminators;
                EndPos := CurPos - 1;
                NameEndPos := EndPos;
              End;

            { Create a TfsParam if necessary.  Replace the name with a '?'. }
            If createParams And assigned(aParams) Then
              aParams.CreateParam(ftUnknown,
                Copy(Result, NameStartPos,
                (NameEndPos - NameStartPos) + 1), ptUnknown);

            Result[StartPos] := '?';
            System.Delete(Result, StartPos + 1, EndPos - StartPos);
            CurPos := StartPos + 1;

          End;
      End
    Else
      { Not the start of a literal or a colon.  Move to next character. }
      inc(CurPos);

  Until (CurPos > Length(Result)) Or (Result[CurPos] = #0);

End;
{--------}

Procedure TFSQuery.quPreparePrim(prepare: boolean);
Var
  SQLResult: TffResult;
  Msg: String;
  MsgLen: Integer;
  Stream: TMemoryStream;
Begin
  { Requirement: Query must be closed. }
  If dsCursorID > 0 Then
    RaiseFSErrorObj(Self, fsdse_QueryMustBeClosed);

  If (FPrepared <> prepare) Then
    Begin

      FExecuted := False;

      { Are we preparing? }
      If prepare Then
        Begin
          { Yes. Requirement: Must have a database. }{!!.03}
          dsEnsureDatabaseOpen(True); {!!.03}
          FRowsAffected := -1; {!!.10}
          FCanModify := False; {!!.10}
          FRecordsRead := 0; {!!.10}

          { If we have a SQL statement then allocate & prepare a SQL
            statement on the engine. }
          If (length(FText) > 0) Then
            Begin
              Check(ServerEngine.SQLAlloc(dsProxy.Database.Session.Client.ClientID,
                dsProxy.Database.DatabaseID, dsGetTimeout,
                FStmtID));
              Stream := TMemoryStream.Create;
              Try
                Try
                  SQLResult := ServerEngine.SQLPrepare(FStmtID, pointer(FText),
                    Stream);
                  If SQLResult <> DBIERR_NONE Then
                    Begin
                      Stream.Position := 0;
                      Stream.Read(MsgLen, sizeOf(MsgLen));
                      If MsgLen > 0 Then
                        Begin
                          SetLength(Msg, MsgLen);
                          Stream.Read(Msg[1], MsgLen);
                          RaiseFSErrorObjFmt(Self, fsdse_QueryPrepareFail, [#13#10, Msg]);
                        End
                      Else
                        Check(SQLResult);
                    End;
                Except
                  quFreeStmt;
                  Raise;
                End;
              Finally
                Stream.Free;
              End;
            End
          Else
            { No SQL statement.  Raise an exception. }
            RaiseFSErrorObj(Self, fsdse_EmptySQLStatement);
        End
      Else
        { No.  Free the statement. }
        quFreeStmt;
      FPrepared := prepare;
    End;
End;
{$IFDEF DCC4OrLater}
{--------}

Procedure TFSQuery.quReadParams(Reader: TReader);
Begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
End;
{$ENDIF}
{--------}

Procedure TFSQuery.quRefreshParams;
Var
  DataSet: TDataSet;
Begin
  If Not Active Then
    Exit;

  DisableControls;
  Try
    If assigned(FDataLink.DataSource) Then
      Begin
        DataSet := FDataLink.DataSource.DataSet;
        If assigned(DataSet) Then
          If DataSet.Active And (DataSet.State <> dsSetKey) Then
            Begin
              Close;
              Open;
              EnableControls;
              First;
            End;
      End;
  Finally
    EnableControls;
  End;
End;
{--------}

Procedure TFSQuery.quSetDataSource(aSrc: TDataSource);
Begin
  { If we have a circular link then raise an exception. }
  If IsLinkedTo(aSrc) Then
    RaiseFSErrorObjFmt(Self, fsdse_TblCircDataLink, [aSrc.Name]);
  FDataLink.DataSource := aSrc;
End;
{--------}

Procedure TFSQuery.quSetParams(aParamList: TfsParams);
Begin
  FParams.AssignValues(aParamList);
End;
{--------}

Procedure TFSQuery.quSetParamsFromCursor;
Var
  I: Integer;
  DataSet: TDataSet;
Begin
  If assigned(FDataLink.DataSource) Then
    Begin
      DataSet := FDataLink.DataSource.DataSet;
      If assigned(DataSet) Then
        Begin
          DataSet.FieldDefs.Update;
          For I := 0 To Pred(FParams.Count) Do
            With FParams[I] Do
              { Has this parameter been bound? }
              If Not Bound Then
                Begin
                  { No. Get a value from the dataset. }
                  AssignField(DataSet.FieldByName(Name));
                  Bound := False;
                End;
        End;
    End;
End;
{--------}

Procedure TFSQuery.quSetPrepared(aFlag: boolean);
Begin
  If aFlag Then
    Prepare
  Else
    Unprepare;
End;
{--------}

Procedure TFSQuery.quSetRequestLive(aFlag: boolean);
Begin
  If FRequestLive <> aFlag Then
    Begin
      FRequestLive := aFlag;
      dsReadOnly := (Not aFlag);
    End;
End;
{--------}

Procedure TFSQuery.quSetSQL(aValue: TStrings);
Begin
  If FSQL.Text <> aValue.Text Then
    Begin
      quDisconnect;
      FSQL.BeginUpdate;
      Try
        FSQL.Assign(aValue);
      Finally
        FSQL.EndUpdate;
      End;
    End;
End;

Procedure TFSQuery.quSetSQLORDER(aValue: TStrings);
Begin
  quDisconnect;
  FSQLORDER.BeginUpdate;
  Try
    FSQLORDER.Assign(aValue);
  Finally
    FSQLORDER.EndUpdate;
  End;
End;

{Procedure TFSDataset.quSetSQLDISPLAY(aValue: TStrings);
Begin
  FSQLDISPLAY.BeginUpdate;
  Try
    FSQLDISPLAY.Assign(aValue);
  Finally
    FSQLDISPLAY.EndUpdate;
  End ;
End ;}

{--------}

Procedure TFSQuery.quSQLORDERChanged(Sender: TObject);
Begin
  quSQLChanged(Sender);
End;

Procedure TFSQuery.quSQLChanged(Sender: TObject);
Var
  aList: TfsParams;
Begin
  {Begin !!.02}
  {$IFNDEF DCC4OrLater}
  aList := Nil;
  {$ENDIF}
  {End !!.02}
    { Is the component loading? }
  If Not (csReading In ComponentState) Then
    Begin
      { No.  Disconnect from the server. }
      quDisconnect;
      { Are we supposed to regenerate the parameters or are we in the IDE? }
      If FParamCheck Or (csDesigning In ComponentState) Then
        Begin
          { Yes.  Rebuild the parameters. }
          {$IFDEF DCC4OrLater}
          aList := TfsParams.Create(Self);
          {$ELSE}
          aList := TfsParams.Create;
          {$ENDIF}
          Try
            FText := quParseSQL(FSQL.Text + ' ' + #13 + FSQLORDER.Text, True, aList);
            aList.AssignValues(FParams);
            FParams.Clear;
            FParams.Assign(aList);
          Finally
            aList.Free;
          End;
        End
      Else
        FText := FSQL.Text + ' ' + #13 + SQLORDER.Text;
      DataEvent(dePropertyChange, 0);
    End
  Else
    { Yes.  Parse the text, replacing parameters with question marks. }
{Begin !!.02}
    {$IFDEF DCC4OrLater}
    FText := quParseSQL(FSQL.Text + ' ' + #13 + SQLORDER.Text, False, Nil);
  {$ELSE}
    Begin {!!.03}
      aList := TfsParams.Create;
      Try
        FText := quParseSQL(FSQL.Text + ' ' + #13 + SQLORDER.Text, True, aList);
        aList.AssignValues(FParams);
        FParams.Clear;
        FParams.Assign(aList);
      Finally
        aList.Free;
      End;
    End; {!!.03}
  {$ENDIF}
End;
{$IFDEF DCC4OrLater}
{--------}

Procedure TFSQuery.quWriteParams(Writer: TWriter);
Begin
  Writer.WriteCollection(FParams);
End;
{$ENDIF}
{--------}

Procedure TFSQuery.Unprepare;
Begin
  quPreparePrim(False);
End;
{====================================================================}

Procedure TFSDataSet.UpdateFormatFields;
Var
  i, r: Integer;

  Function NoF(Name: String): Integer;
  Var
    j: Integer;
  Begin
    Result := -1;
    For j := 0 To Self.FieldCount - 1 Do
      Begin
        If AnsiUpperCase(Name) = AnsiUpperCase(Fields[j].FieldName) Then
          Begin
            Result := j;
            System.Break;
          End;
      End;

  End;

Begin
  If (Self.FieldDefs.Count > 0) Then
    Begin
      For i := 0 To Dictionary.FieldCount - 1 Do
        Begin
          r := nof(Dictionary.FieldDescriptor[i].fdName);
          If r > -1 Then
            Begin
              If Dictionary.FieldDescriptor[i].fdDesc <> '' Then
                Fields[r].DisplayLabel := Dictionary.FieldDescriptor[i].fdDesc;
              {Else
                Begin
                  If (FSQLDISPLAY.Count > 0) And (FSQLDISPLAY.Count - 1 >= i) then
                    Fields[r].DisplayLabel := FSQLDISPLAY[i];
                End ;}
              Fields[r].ReadOnly := Dictionary.FieldDescriptor[i].fdReadOnly;
              Fields[r].Visible := Dictionary.FieldDescriptor[i].fdVisible;
              If Fields[r] Is TfsArrayField Then
                Begin
                  TfsArrayField(Fields[r]).fsDatatype := Dictionary.FieldDescriptor[i].fdType;
                  TfsArrayField(Fields[r]).Decimal := Dictionary.FieldDescriptor[i].fdDecPl;
                  TfsArrayField(Fields[r]).VRound := Dictionary.FieldDescriptor[i].fdRound;
                End;

              If Fields[r] Is TFSNumericField Then
                With TFSNumericField(Fields[r]) Do
                  Begin
                    If Fields[r].DataType In [ftInteger] Then
                      Begin
                        TfsIntegerField(Fields[r]).fsDatatype := Dictionary.FieldDescriptor[i].fdType;
                      End
                    Else If Fields[r].DataType In [ftFloat] Then
                      Begin
                        TFSExtendedField(Fields[r]).fsDatatype := Dictionary.FieldDescriptor[i].fdType;
                        TFSExtendedField(Fields[r]).VRound := Dictionary.FieldDescriptor[i].fdRound;
                        If Dictionary.FieldDescriptor[i].fdDecPl > 0 Then
                          TFSExtendedField(Fields[r]).size := Dictionary.FieldDescriptor[i].fdDecPl;
                        TFSExtendedField(Fields[r]).precision := Dictionary.FieldDescriptor[i].fdUnits;
                      End
                    Else If Fields[r].DataType In [ftBCD] Then
                      Begin
                        TFSBcdField(Fields[r]).fsDatatype := Dictionary.FieldDescriptor[i].fdType;
                        TFSBcdField(Fields[r]).VRound := Dictionary.FieldDescriptor[i].fdRound;
                        TFSBcdField(Fields[r]).size := Dictionary.FieldDescriptor[i].fdDecPl;
                        TFSBcdField(Fields[r]).precision := Dictionary.FieldDescriptor[i].fdUnits;
                      End
                    Else If Fields[r].DataType In [ftCurrency] Then
                      Begin
                        TFSCurrencyField(Fields[r]).fsDatatype := Dictionary.FieldDescriptor[i].fdType;
                        TFSCurrencyField(Fields[r]).VRound := Dictionary.FieldDescriptor[i].fdRound;
                        If Dictionary.FieldDescriptor[i].fdDecPl > 0 Then
                          TFSCurrencyField(Fields[r]).size := Dictionary.FieldDescriptor[i].fdDecPl;
                        TFSCurrencyField(Fields[r]).precision := Dictionary.FieldDescriptor[i].fdUnits + 2;
                      End;
                  End;
            End;
        End;
    End;
End;

{===Initialization routine===========================================}

Procedure InitializeUnit;
Begin
  {create the fsClients list}
  fsClients := TFSClientList.Create;
End;
{====================================================================}

{===Finalization routine=============================================}

Procedure FinalizeUnit;
Begin
  FreeAndnil(fsClients);
End;

{====================================================================}

Initialization
  InitializeUnit;
  {--------}
Finalization
  FinalizeUnit;
  {--------}
End.

