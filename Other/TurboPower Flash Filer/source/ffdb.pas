{*********************************************************}
{* FlashFiler: Data Access Components for Delphi 3+      *}
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

{ Uncomment the following define in order to have the automatic transports
  log all activity to a file named FFAUTOTRANS.LOG. }
{.$DEFINE AutoLog}

{ Comment out the following define to disable raising of "Bookmarks do not
  match table" exceptions for invalid bookmarks in TffDataSet.CompareBookmarks.
  Disabling this behavior is appropriate for certain data-aware controls
  such as the InfoPower DBTreeView and the VCL DBGrid. }
{$DEFINE RaiseBookmarksExcept}

unit ffdb;

interface

uses
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
  DB,
  {$IFDEF UsesBDE}
  bde,
  {$ENDIF}
  ffsrbde,
  ffclbde,
  ffllcomp,
  fflleng,
  ffclbase,
  fflogdlg,
  ffllbase,
  ffllcomm,
  ffclcfg,
  ffllprot,
  fflldict,
  ffcltbrg,
  ffdbbase,
  DBCommon,
  ffsrvdlg,
  ffstdate,
  ffllcoll,
  ffhash,
  ffnetmsg,
  ffclreng,
  fflllgcy,
  Messages,
  ffllthrd,
{Begin !!.02}
  ffsqlbas
  {$IFDEF SingleEXE}
  , ffsreng
  {$ENDIF}
  ;
{End !!.02}

const
  DefaultTimeOut = 10 * 1000; { 10 Seconds }                             {!!.01}
  AutoObjName = '[automatic]';

type
  TffConnectionLostEvent = procedure (aSource   : TObject;
                                      aStarting : Boolean;
                                  var aRetry    : Boolean) of object;
    {-an event triggered once when the conneciton to the server is lost, and
      onceafter code to retry, or clear associated components is complete. By
      default aRetry is set to False. If this is set to true then the client
      will try to reestablish the connection, and associated components. }

  TffLoginEvent = procedure (aSource   : TObject;
                         var aUserName : TffName;
                         var aPassword : TffName;
                         var aResult   : Boolean) of object;
    {-an event to get a user name and password for login purposes}

  TffChooseServerEvent = procedure (aSource      : TObject;
                                    aServerNames : TStrings;
                                var aServerName  : TffNetAddress;
                                var aResult      : Boolean) of object;
    {-an event to choose server name to attach to}

  TffFindServersEvent = procedure (aSource   : TObject;
                                   aStarting : Boolean) of object;
    {-an event to enable a 'waiting...' dialog or splash screen to be
      shown whilst finding server names}

type
  TffKeyEditType = (                 {Types of key to edit and store..}
                    ketNormal,       {..normal search key}
                    ketRangeStart,   {..range start key}
                    ketRangeEnd,     {..range end key}
                    ketCurRangeStart,{..current range start key}
                    ketCurRangeEnd,  {..current range end key}
                    ketSaved);       {..saved key (for rollback)}

type
  TffCursorProps = packed record         { Virtual Table properties }
    TableName        : string;           { Table name}
    FileNameSize     : Word;             { Full file name size }
    FieldsCount      : Word;             { No of fields in Table }
    RecordSize       : Word;             { Record size (logical record) }
    RecordBufferSize : Word;             { Record size (physical record) }
    KeySize          : Word;             { Key size }
    IndexCount       : Word;             { Number of indexes }
    ValChecks        : Word;             { Number of val checks }
    BookMarkSize     : Word;             { Bookmark size }
    BookMarkStable   : Boolean;          { Stable book marks }
    OpenMode         : TffOpenMode;      { ReadOnly / RW }
    ShareMode        : TffShareMode;     { Excl / Share }
    Indexed          : Boolean;          { Index is in use }
    XltMode          : FFXLTMode;        { Translate Mode }
    TblRights        : Word;             { Table  rights }
    Filters          : Word;             { Number of filters }
  end;

type
  PffNodeValue = ^TffNodeValue;
  TffNodeValue = packed record
    nvType    : Word;
    nvSize    : Word;
    nvValue   : Pointer;
    nvIsNull  : Boolean;
    nvIsConst : Boolean;
  end;

  PffFilterNode = ^TffFilterNode;
  TffFilterNode = packed record
    Case Integer of
      1:(fnHdr      : CANHdr);
      2:(fnUnary    : CANUnary);
      3:(fnBinary   : CANBinary);
      4:(fnField    : CANField);
      5:(fnConst    : CANConst);
      7:(fnContinue : CANContinue);
      8:(fnCompare  : CANCompare);
  end;

  TffFilterListItem = class(TffCollectionItem)
    protected {private}
      fliActive      : Boolean;
      fliCanAbort    : Boolean;
      fliExpression  : pCANExpr;
      fliExprSize    : Word;
      fliFilterFunc  : pfGENFilter;
      fliClientData  : Longint;
      fliOwner       : TObject;
      fliPriority    : Integer;

    protected
      function fliGetLiteralPtr(aoffset : Word) : Pointer;
      function fliGetNodePtr(aoffset : Word) : PffFilterNode;

      function fliEvaluateBinaryNode(aNode   : PffFilterNode;
                                     aRecBuf : Pointer;
                                     aNoCase : Boolean;
                                     aPartial: Word) : Boolean;
      function fliEvaluateConstNode(aNode   : PffFilterNode;
                                    aValue  : PffNodeValue;
                                    aRecBuf : Pointer) : Boolean;
      function fliEvaluateFieldNode(aNode   : PffFilterNode;
                                    aValue  : PffNodeValue;
                                    aRecBuf : Pointer) : Boolean;
      function fliEvaluateLogicalNode(aNode   : PffFilterNode;
                                      aRecBuf : Pointer) : Boolean;
      function fliEvaluateNode(aNode   : PffFilterNode;
                               aValue  : PffNodeValue;
                               aRecBuf : Pointer) : Boolean;
      function fliEvaluateUnaryNode(aNode   : PffFilterNode;
                                    aRecBuf : Pointer) : Boolean;

      function fliCompareValues(var aCompareResult : Integer;
                                var aFirst      : TffNodeValue;
                                var aSecond     : TffNodeValue;
                                    aIgnoreCase : Boolean;
                                    aPartLen    : Integer) : Boolean;

    public
      constructor Create(aContainer : TffCollection;
                         aOwner     : TObject;
                         aClientData: Longint;
                         aPriority  : Integer;
                         aCanAbort  : Boolean;
                         aExprTree  : pCANExpr;
                         aFiltFunc  : pfGENFilter);
      destructor Destroy; override;

      function MatchesRecord(aRecBuf : Pointer) : Boolean;
      procedure GetFilterInfo(Index : Word; var FilterInfo : FilterInfo);

      property Active : Boolean
         read fliActive
         write fliActive;
  end;

type
  TffBaseClient     = class;
  TffClient         = class;
  TffCommsEngine    = class;
  TffClientList     = class;
  TffSession        = class;
  TffSessionList    = class;
  TffBaseTable      = class;  
  TffBaseDatabase   = class;
  TffDatabase       = class;
  TffDatabaseList   = class;
  TffTableProxy     = class;
  TffTableProxyList = class;
  TffDataSet        = class;
  TffTable          = class;

  TffBaseClient = class(TffDBListItem)
    protected {private}
      bcAutoClientName   : Boolean;
      bcBeepOnLoginError : Boolean;                                   {!!.06}
      bcOwnServerEngine  : Boolean;
      bcClientID         : TffClientID;
      bcIsDefault        : Boolean;
      bcOnConnectionLost : TffConnectionLostEvent;
      bcPasswordRetries  : Integer;
      bcServerEngine     : TffBaseServerEngine;
      bcTimeOut          : Longint;
      bcUserName         : TffNetName;
      bcPassword         : string;                                    {!!.06}
        {bcPassword is only used to store the last password at design-time.
         It is not used at run-time.}
      function dbliCreateOwnedList : TffDBList; override;
      procedure dbliClosePrim; override;
      procedure dbliDBItemAdded(aItem : TffDBListItem); override;
      procedure dbliDBItemDeleted(aItem : TffDBListItem); override;
      procedure dbliMustBeClosedError; override;
      procedure dbliMustBeOpenError; override;

      function bcGetServerEngine : TffBaseServerEngine;
      function bcGetUserName : string;                                 {!!.10}
      procedure bcSetAutoClientName(const Value : Boolean);
      procedure bcSetClientName(const aName : string);
      procedure bcSetIsDefault(const Value : Boolean);
      procedure bcSetUserName(const Value : string);
      procedure bcSetServerEngine(Value : TffBaseServerEngine);
      procedure bcSetTimeout(const Value : Longint);
      function bcGetSession(aInx : Integer) : TffSession;
      function bcGetSessionCount : Integer;

      function bcGetDefaultSession : TffSession;
      procedure bcMakeSessionDefault(aSession : TffSession;
                                     aValue   : Boolean);
      procedure OpenConnection(aSession : TffSession); virtual; abstract;

      procedure bcDoConnectionLost; dynamic;
      function bcReinstateDependents : Boolean;
      procedure bcClearDependents;

      function ProcessRequest(aMsgID           : Longint;
                              aTimeout         : Longint;
                              aRequestData     : Pointer;
                              aRequestDataLen  : Longint;
                              aRequestDataType : TffNetMsgDataType;
                          var aReply           : Pointer;
                          var aReplyLen        : Longint;
                              aReplyType       : TffNetMsgDataType) : TffResult; virtual;
        { Backdoor method for sending a request to a server engine.
          Should only be implemented by remote server engines. }


      function ProcessRequestNoReply(aMsgID          : Longint;
                                     aTimeout        : Longint;
                                     aRequestData    : Pointer;
                                     aRequestDataLen : Longint) : TffResult; virtual;
        { Backdoor method for sending a request, no reply expected, to a
          server engine. Should only be implemented by remote server engines. }

    public
      constructor Create(aOwner : TComponent); override;
      destructor Destroy; override;
      procedure IDEConnectionLost(aSource   : TObject;
                                  aStarting : Boolean;
                              var aRetry    : Boolean);
      procedure FFNotificationEx(const AOp : Byte; AFrom : TffComponent;
                                 const AData : TffWord32); override;

      procedure GetServerNames(aServerNames : TStrings); virtual;        {!!.01}
      function IsConnected : Boolean; virtual;

      property AutoClientName : Boolean
        read bcAutoClientName
        write bcSetAutoClientName
        default False;

      property BeepOnLoginError : Boolean                                {!!.06}
        read bcBeepOnLoginError
        write bcBeepOnLoginError
        default True;

      property ClientID : TffClientID
         read bcClientID;

      property ClientName : string
         read dbliDBName
         write bcSetClientName;

      property CommsEngineName : string
         read dbliDBName
         write bcSetClientName;

      property IsDefault : Boolean
         read bcIsDefault
         write bcSetIsDefault
         default False;

      property OnConnectionLost : TffConnectionLostEvent
         read bcOnConnectionLost
         write bcOnConnectionLost;

      property OwnServerEngine : Boolean
         read bcOwnServerEngine
         stored False;

      property PasswordRetries : Integer
         read bcPasswordRetries
         write bcPasswordRetries
         default 3;

      property ServerEngine : TffBaseServerEngine
         read bcGetServerEngine
         write bcSetServerEngine;

      property SessionCount : Integer
         read bcGetSessionCount
         stored False;

      property Sessions[aInx : Integer] : TffSession
         read bcGetSession;

      property TimeOut : Longint
         read bcTimeOut
         write bcSetTimeOut
         default DefaultTimeout;
         { Timeout specified in milliseconds }

      property UserName : string                                       {!!.10}
         read bcGetUserName
         write bcSetUserName;

  end;

  TffClient = class(TffBaseClient)
    public
      procedure OpenConnection (aSession : TffSession); override;
      property ClientID;
      property SessionCount;
      property Sessions;
    published
      property Active;
      property AutoClientName;
      property BeepOnLoginError;                                         {!!.06}
      property ClientName;
      property IsDefault;
      property OnConnectionLost;
      property PasswordRetries;
      property ServerEngine;
      property TimeOut;
      property UserName;
  end;

  TffCommsEngine = class(TffBaseClient)
    protected {private}
      FServerName    : TffNetName;
      ceProtocol     : TffProtocolType;
      ceRegProt      : TffCommsProtocolClass;
      ceRegProtRead  : Boolean;
      ceServerName   : TffNetAddress;

    protected
      procedure ceSetProtocol(const Value : TffProtocolType);
      procedure ceSetServerName(const Value : string);                 {!!.10}
      function ceGetServerName : string;                               {!!.10}
      procedure ceReadRegistryProtocol;
    public
      constructor Create(aOwner : TComponent); override;

      procedure GetServerNames(aServerNames : TStrings); override;      {!! .01}
      procedure OpenConnection (aSession : TffSession); override;
      function ProtocolClass : TffCommsProtocolClass; dynamic;

      property ClientID;
      property SessionCount;
      property Sessions;

    published
      property Active;
      property AutoClientName;
      property BeepOnLoginError;                                         {!!.06}
      property CommsEngineName;
      property IsDefault;
      property OnConnectionLost;
      property PasswordRetries;
      property ServerEngine;
      property TimeOut;
      property UserName;

      property Protocol : TffProtocolType
         read ceProtocol
         write ceSetProtocol
         default ptSingleUser;

      property ServerName : string                                     {!!.10}
         read ceGetServerName
         write ceSetServerName;
  end;

  TffClientList = class(TffDBStandaloneList)
    protected {private}
      function clGetItem(aInx : Integer) : TffBaseClient;
    public
      property Clients[aInx : Integer] : TffBaseClient
        read clGetItem; default;
      property CommsEngines[aInx : Integer] : TffBaseClient
         read clGetItem;
  end;


  TffSession = class(TffDBListItem)
    protected {private}
      scAutoSessionName : Boolean;
      scSessionID       : TffSessionID;
      scIsDefault       : Boolean;

      scOnStartup    : TNotifyEvent;
      scChooseServer : TffChooseServerEvent;
      scFindServers  : TffFindServersEvent;
      scLogin        : TffLoginEvent;
      scServerEngine : TffBaseServerEngine;
      scTimeout      : Longint;
    protected
      function scGetClient : TffBaseClient;
      function scGetDatabase(aInx : Integer) : TffBaseDatabase;
      function scGetDatabaseCount : Integer;
      function scGetIsDefault : Boolean;
      function scGetServerEngine : TffBaseServerEngine;
      procedure scRefreshTimeout;                                      {!!.11}
      procedure scSetAutoSessionName(const Value : Boolean);
      procedure scSetIsDefault(const Value : Boolean);
      procedure scSetSessionName(const aName : string);
      procedure scSetTimeout(const Value : Longint);

      function dbliCreateOwnedList : TffDBList; override;
      procedure dbliClosePrim; override;
      function dbliFindDBOwner(const aName : string)
                                           : TffDBListItem; override;
      procedure dbliMustBeClosedError; override;
      procedure dbliMustBeOpenError; override;
      procedure dbliOpenPrim; override;
      procedure DoStartup; virtual;
      procedure ChooseServer(var aServerName : TffNetAddress);
      procedure FindServers(aStarting : Boolean);
      procedure DoLogin(var aUserName : TffName;
                        var aPassword : TffName;
                        var aResult   : Boolean);

      function ProcessRequest(aMsgID           : Longint;
                              aTimeout         : Longint;
                              aRequestData     : Pointer;
                              aRequestDataLen  : Longint;
                              aRequestDataType : TffNetMsgDataType;
                          var aReply           : Pointer;
                          var aReplyLen        : Longint;
                              aReplyType       : TffNetMsgDataType)
                                               : TffResult; virtual;
        { Backdoor method for sending a request to a server engine.
          Should only be implemented by remote server engines. }


      function ProcessRequestNoReply(aMsgID          : Longint;
                                     aTimeout        : Longint;
                                     aRequestData    : Pointer;
                                     aRequestDataLen : Longint)
                                                     : TffResult; virtual;
        { Backdoor method for sending a request, no reply expected, to a
          server engine. Should only be implemented by remote server engines. }

    public
      constructor Create(aOwner : TComponent); override;
      destructor Destroy; override;

      procedure AddAlias(const aName       : string;
                         const aPath       : string;
                               aCheckSpace : Boolean                   {!!.11}
                               {$IFDEF DCC4OrLater}                    {!!.11}
                               = False                                 {!!.11}
                               {$ENDIF});                              {!!.11}
      function AddAliasEx(const aName       : string;
                          const aPath       : string;
                                aCheckSpace : Boolean                  {!!.11}
                                {$IFDEF DCC4OrLater}                   {!!.11}
                                = False                                {!!.11}
                                {$ENDIF})                              {!!.11}
                                            : TffResult;
      procedure CloseDatabase(aDatabase : TffBaseDatabase);
      procedure CloseInactiveTables;                                   {!!.06}
      procedure DeleteAlias(const aName : string);
      function DeleteAliasEx(const aName : string) : TffResult;
      function FindDatabase(const aName : string) : TffBaseDatabase;
      procedure GetAliasNames(aList : TStrings);
      function GetAliasNamesEx(aList      : TStrings;
                         const aEmptyList : Boolean)
                                          : TffResult;
      procedure GetAliasPath(const aName : string;
                               var aPath : string);
      procedure GetDatabaseNames(aList : TStrings);
      function GetServerDateTime(var aServerNow : TDateTime) : TffResult;
                                                                       {begin !!.10}
      function GetServerSystemTime(var aServerNow : TSystemTime) : TffResult;
      function GetServerGUID(var aGUID : TGUID) : TffResult;
      function GetServerID(var aUniqueID : TGUID) : TffResult;
      function GetServerStatistics(var aStats : TffServerStatistics)
                                              : TffResult;
      function GetCommandHandlerStatistics(const aCmdHandlerIdx : Integer;
                                             var aStats         : TffCommandHandlerStatistics)
                                                                : TffResult;
      function GetTransportStatistics(const aCmdHandlerIdx : Integer;
                                      const aTransportIdx  : Integer;
                                        var aStats         : TffTransportStatistics)
                                                           : TffResult;
                                                                       {End !!.10}
      procedure GetTableNames(const aDatabaseName : string;
                              const aPattern      : string;
                                    aExtensions   : Boolean;
                                    aSystemTables : Boolean;
                                    aList         : TStrings);
      function GetTaskStatus(const aTaskID    : Longint;
                               var aCompleted : Boolean;
                               var aStatus    : TffRebuildStatus) : TffResult;
      function GetTimeout : Longint;
      function IsAlias(const aName : string) : Boolean;
      function ModifyAlias(const aName    : string;
                           const aNewName : string;
                           const aNewPath : string;
                                 aCheckSpace : Boolean                 {!!.11}
                                 {$IFDEF DCC4OrLater}                  {!!.11}
                                 = False                               {!!.11}
                                 {$ENDIF})                             {!!.11}
                                          : TffResult;
      function OpenDatabase(const aName : string) : TffBaseDatabase;
      procedure SetLoginRetries(const aRetries : Integer);
      procedure SetLoginParameters(const aName : TffName; aPassword : TffName);

      property Client : TffBaseClient
         read scGetClient;

      property CommsEngine : TffBaseClient
         read scGetClient;

      property DatabaseCount : Integer
         read scGetDatabaseCount;
        { TODO:: This functionality assumes that all dependents are databases.
          This is not the case when a plugin engine attaches itself to the
          session in order to re-use the connection. }

      property Databases[aInx : Integer] : TffBaseDatabase
         read scGetDatabase;
        { TODO:: This functionality assumes that all dependents are databases.
          This is not the case when a plugin engine attaches itself to the
          session in order to re-use the connection. }

      property ServerEngine : TffBaseServerEngine
         read scGetServerEngine;

      property SessionID : TffSessionID
         read scSessionID;

    published
      property Active;

      property AutoSessionName : Boolean
         read scAutoSessionName
         write scSetAutoSessionName
         default False;

      property ClientName : string
         read dbligetDBOwnerName
         write dbliSetDBOwnerName;

      property CommsEngineName : string
         read dbliGetDBOwnerName
         write dbliSetDBOwnerName
         stored False;
         {Since the ClientName, and CommsEngine name
          are mirrod, we only need to store the ClientName.}

      property IsDefault : Boolean
         read scGetIsDefault
         write scSetIsDefault
         default False;

      property SessionName : string
         read dbliDBName
         write scSetSessionName;

      property OnStartup : TNotifyEvent
         read scOnStartup
         write scOnStartup;

      property OnChooseServer : TffChooseServerEvent
         read scChooseServer
         write scChooseServer;

      property OnFindServers : TffFindServersEvent
         read scFindServers
         write scFindServers;

      property OnLogin : TffLoginEvent
         read scLogin
         write scLogin;

      property TimeOut : Longint
         read scTimeout
         write scSetTimeout
         default -1;
         { Timeout specified in milliseconds }
  end;

  TffSessionList = class(TffDBList)
    protected {private}
      slCurrSess : TffSession;
    protected
      function slGetCurrSess : TffSession;
      function slGetItem(aInx : Integer) : TffSession; 
      procedure slSetCurrSess(CS : TffSession); 
    public
      property CurrentSession : TffSession
         read slGetCurrSess
         write slSetCurrSess;

       property Sessions[aInx : Integer] : TffSession
         read slGetItem; default;
  end;


  TffServerFilterTimeoutEvent = procedure(Sender : TffDataSet;
                                      var Cancel : Boolean) of object;
  TffFilterEvaluationType = (ffeLocal, ffeServer);
    { If ffeLocal then filter statement is evaluated local to client.
      If ffeServer then filter statement is evaluated on server. }


  TffFieldDescItem = class(TffCollectionItem)
    protected {private}
      fdiPhyDesc : pFLDDesc;
      fdiLogDesc : pFLDDesc;
      fdiFieldNum: Integer;

    public
      constructor Create(aContainer : TffCollection; const FD : FLDDesc);
      destructor Destroy; override;

      property LogDesc : pFLDDesc
         read fdiLogDesc;

      property PhyDesc : pFLDDesc
         read fdiPhyDesc;

      property FieldNumber : Integer
         read fdiFieldNum;
  end;

  TTableState =(TblClosed, TblOpened);

  TffDataSet = class(TDataSet)
    protected {private}
      dsBookmarkOfs     : Integer;{offset to bookmark in TDataSet record Buffer}
      dsBlobOpenMode    : TffOpenMode;
      dsCalcFldOfs      : Integer;{offset to calcfields in TDataSet record Buffer}
      dsClosing         : Boolean;
      dsCurRecBuf       : Pointer;
      dsCursorID        : TffCursorID;
      dsDictionary      : TffDataDictionary;
      dsExclusive       : Boolean;
      dsExprFilter      : hDBIFilter;
      dsFieldDescs      : TffCollection;
      dsFilterActive    : Boolean;
      dsFilterEval      : TffFilterEvaluationType;
      dsFilterResync    : Boolean;
      dsFilters         : TffCollection;
      dsFilterTimeout   : TffWord32;
      dsFuncFilter      : hDBIFilter;
      dsOldValuesBuffer : PChar;
      dsOpenMode        : TffOpenMode;
      dsPhyRecSize      : Integer;      {FlashFiler physical record size}
      dsProxy           : TffTableProxy;
      dsReadOnly        : Boolean;
      dsRecBufSize      : Integer;      {TDataSet record Buffer size}
      dsRecInfoOfs      : Integer;      {offset to rec info in TDataSet record Buffer}
      dsRecordToFilter  : Pointer;
      dsServerEngine    : TffBaseServerEngine;
      dsShareMode       : TffShareMode;
      dsTableState      : TTableState;
      dsTimeout         : Longint;
        { If you need a timeout value, use the Timeout property.  Do not
          directly access this property as it may be set to -1.  The Timeout
          property takes this into account. }
      dsXltMode         : FFXltMode;
      dsOnServerFilterTimeout : TffServerFilterTimeoutEvent;
    protected
      {---Property access methods---}
      function dsGetDatabase : TffBaseDatabase;
      function dsGetDatabaseName : string;
      function dsGetServerEngine : TffBaseServerEngine; virtual;
      function dsGetSession : TffSession;
      function dsGetSessionName : string;
      function dsGetTableName : string;
      function dsGetVersion : string;
      procedure dsRefreshTimeout;                                        {!!.11}
      procedure dsSetDatabaseName(const aValue : string);
      procedure dsSetExclusive(const aValue : Boolean);
      procedure dsSetReadOnly(const aValue : Boolean);
      procedure dsSetSessionName(const aValue : string);
      procedure dsSetTableLock(LockType: TffLockType; Lock: Boolean);
      procedure dsSetTableName(const aValue : string); virtual;
      function  dsGetTimeout : Longint;
      procedure dsSetTimeout(const Value : Longint);
      procedure dsSetVersion(const aValue : string);

      {---Filtering---}
      function dsActivateFilter(hFilter : hDBIFilter) : TffResult;
      procedure dsAddExprFilter(const aText : string;
                                const aOpts : TFilterOptions);
      function dsAddFilter(iClientData : Longint;
                         iPriority   : Word;
                         bCanAbort   : Bool;
                         pCANExpr    : pCANExpr;
                         pffilter    : pfGENFilter;
                     var hFilter     : hDBIFilter) : TffResult;
      procedure dsAddFuncFilter(aFilterFunc : pfGENFilter);
      function dsCancelServerFilter: Boolean; virtual;
      procedure dsClearServerSideFilter;
      function dsCreateLookupFilter(aFields  : TList;
                              const aValues  : Variant;
                                    aOptions : TLocateOptions): HDBIFilter;
      function dsDeactivateFilter(hFilter : hDBIFilter) : TffResult;
      procedure dsActivateFilters; virtual;                            {!!.03}
      procedure dsDeactivateFilters; virtual;                          {!!.03}
      function dsDropFilter(hFilter : hDBIFilter) : TffResult;
      procedure dsDropFilters;
      function dsMatchesFilter(pRecBuff : Pointer) : Boolean;
      function dsOnFilterRecordCallback({ulClientData = Self}
                                         pRecBuf      : Pointer;
                                         iPhyRecNum   : Longint
                                        ): SmallInt stdcall;

      procedure dsSetFilterEval(const aMode : TffFilterEvaluationType);
      procedure dsSetFilterTextAndOptions(const aText : string;
                                          const aOpts : TFilterOptions;
                                          const aMode : TffFilterEvaluationType;
                                          const atimeOut : TffWord32);
      procedure dsSetServerSideFilter(const aText : string;
                                      const aOpts : TFilterOptions;
                                            aTimeout : TffWord32);
      procedure dsSetFilterTimeout(const numMS : TffWord32);
      procedure dsUpdateFilterStatus;

      {---Record and key Buffer management---}
      function GetActiveRecBuf(var aRecBuf : PChar): Boolean; virtual;
      function GetCursorProps(var aProps : TffCursorProps) : TffResult; virtual;
      function dsGetNextRecord(eLock     : TffLockType;
                               pRecBuff  : Pointer;
                               RecProps  : pRECProps) : TffResult;
      function dsGetNextRecordPrim(aCursorID : TffCursorID;
                                   eLock     : TffLockType;
                                   pRecBuff  : Pointer;
                                   RecProps  : pRECProps) : TffResult;
      function dsGetPhyRecSize : Integer;
      function dsGetPriorRecord(eLock    : TffLockType;
                                pRecBuff : Pointer;
                                RecProps : pRECProps) : TffResult;
      function dsGetPriorRecordPrim(eLock    : TffLockType;
                                    pRecBuff : Pointer;
                                    RecProps : pRECProps) : TffResult;
      function dsGetRecord(eLock    : TffLockType;
                           pRecBuff : Pointer;
                           RecProps : pRECProps) : TffResult;
      function dsGetRecordCountPrim(var iRecCount : Longint) : TffResult;
      function dsGetRecordPrim(eLock    : TffLockType;
                               pRecBuff : Pointer;
                               RecProps : pRECProps) : TffResult;
      procedure dsGetRecordInfo(aReadProps : Boolean); virtual;
      function dsModifyRecord(aBuffer : Pointer; aRelLock : Boolean) : TffResult;

      {---Field management---}
      procedure dsAddFieldDesc(aFieldDesc : PffFieldDescriptor;
                               aFieldNo : Integer);
      function dsGetFieldDescItem(iField : Integer;
                              var FDI    : TffFieldDescItem) : Boolean;
      function dsGetFieldNumber(FieldName : PChar) : Integer;
      procedure dsReadFieldDescs;
      function dsTranslateCmp(var aFirst      : TffNodeValue;
                              var aSecond     : TffNodeValue;
                                  aIgnoreCase : Boolean;
                                  aPartLen    : Integer) : Integer;
      function dsTranslateGet(FDI      : TffFieldDescItem;
                              pRecBuff : Pointer;
                              pDest    : Pointer;
                          var bBlank   : Boolean) : TffResult;
      function dsTranslatePut(FDI      : TffFieldDescItem;
                              pRecBuff : Pointer;
                              pSrc     : Pointer) : TffResult;

      {---Handle stuff---}
      function dsCreateHandle : TffCursorID;
      procedure DestroyHandle(aHandle : TffCursorID); virtual;
      function GetCursorHandle(aIndexName : string) : TffCursorID; virtual;

      {---Stuff required for descendent dataset's. Empty stubs it this class}
      procedure dsGetIndexInfo; virtual;
      procedure dsAllocKeyBuffers; virtual;
      procedure dsCheckMasterRange; virtual;

      {---Modes---}
      procedure dsEnsureDatabaseOpen(aValue : Boolean);

      {---Blob stuff---}
      function dsCheckBLOBHandle(pRecBuf : Pointer;
                                 iField  : Integer;
                             var aIsNull : Boolean;
                             var aBLOBNr : TffInt64) : TffResult;
      function dsEnsureBLOBHandle(pRecBuf : Pointer;
                                  iField  : Integer;
                              var aBLOBNr : TffInt64) : TffResult;

  {$IFDEF ResizePersistFields}
      procedure ReSizePersistentFields;
  {$ENDIF}

      {---TDataSet method overrides---}
      {Opening, initializing and closing}
      procedure CloseCursor; override;
      procedure InitFieldDefs; override;
      procedure InternalClose; override;
      procedure InternalOpen; override;
      procedure InternalInitFieldDefs; override;
      function IsCursorOpen : Boolean; override;
      procedure OpenCursor(aInfoQuery : Boolean); override;

      {Bookmark management and use}
      procedure GetBookmarkData(aBuffer : PChar; aData : Pointer); override;
      function GetBookmarkFlag(aBuffer : PChar): TBookmarkFlag; override;
      procedure InternalGotoBookmark(aBookmark : TBookmark); override;
      procedure SetBookmarkData(aBuffer : PChar; aData : Pointer); override;
      procedure SetBookmarkFlag(aBuffer : PChar;
                                aValue  : TBookmarkFlag); override;

      {Record Buffer allocation and disposal}
      function AllocRecordBuffer : PChar; override;
      procedure FreeRecordBuffer(var aBuffer : PChar); override;
      function GetRecordSize : Word; override;

      {Field access and update}
      procedure ClearCalcFields(aBuffer : PChar); override;
      procedure CloseBlob(aField : TField); override;
      procedure InternalInitRecord(aBuffer : PChar); override;
      procedure SetFieldData(aField : TField; aBuffer : Pointer); override;
      function FreeBlob(                                { Free the blob }
                        pRecBuf : Pointer;              { Record Buffer }
                        iField  : Word                  { Field number of blob(1..n) }
                       ) : TffResult;

      {Record access and update}
      function FindRecord(aRestart, aGoForward : Boolean) : Boolean; override;
      function GetRecNo: Integer; override;
      function GetRecord(aBuffer  : PChar;
                         aGetMode : TGetMode;
                         aDoCheck : Boolean): TGetResult; override;
      procedure InternalAddRecord(aBuffer : Pointer;
                                  aAppend : Boolean); override;
      procedure InternalCancel; override;
      procedure InternalDelete; override;
      procedure InternalEdit; override;
      procedure InternalFirst; override;
      procedure InternalLast; override;
      procedure InternalPost; override;
      procedure InternalSetToRecord(aBuffer : PChar); override;

      {information}
      function GetCanModify : Boolean; override;
      function GetRecordCount : Integer; override;
      procedure InternalHandleException; override;
      procedure SetName(const NewName : TComponentName); override;

      {filtering}
      procedure SetFiltered(Value : Boolean); override;
      procedure SetFilterOptions(Value : TFilterOptions); override;
      procedure SetFilterText(const Value : string); override;
      procedure SetOnFilterRecord(const Value : TFilterRecordEvent); override;

      procedure dsCloseViaProxy; virtual;

      property Exclusive : Boolean
         read dsExclusive
         write dsSetExclusive
         default False;

      property FieldDescs : TffCollection
         read dsFieldDescs;

      property FilterActive : Boolean
         read dsFilterActive;

      property Filters : TffCollection
        read dsFilters;

      property OpenMode : TffOpenMode
         read dsOpenMode;

      property PhysicalRecordSize : Integer
         read dsGetPhyRecSize;

      property ReadOnly : Boolean
         read dsReadOnly
         write dsSetReadOnly
         default False;

      property ShareMode : TffShareMode
         read dsShareMode;

      property TableState : TTableState
         read dsTableState
         write dsTableState;

      property XltMode : FFXltMode
        read dsXltMode;

      property TableName : string
         read dsGetTableName
         write dsSetTableName;

    public
      constructor Create(aOwner : TComponent); override;
      destructor Destroy; override;

      function AddFileBlob(const aField    : Word;
                             const aFileName : TffFullFileName) : TffResult;
      function BookmarkValid(aBookmark : TBookmark) : Boolean; override;
      function CompareBookmarks(Bookmark1,
                                Bookmark2 : TBookmark) : Integer; override;
      procedure CopyRecords(aSrcTable : TffDataset; aCopyBLOBs : Boolean);  {!!.06}
      function CreateBlobStream(aField : TField;
                                aMode  : TBlobStreamMode) : TStream; override;
      procedure DeleteTable;
      procedure EmptyTable;
      function GetCurrentRecord(aBuffer : PChar) : Boolean; override;
      function GetFieldData(aField  : TField;
                            aBuffer : Pointer): Boolean; override;
      function GetRecordBatch(
                              RequestCount : Longint;
                          var ReturnCount  : Longint;
                              pRecBuff     : Pointer) : TffResult;
      function GetRecordBatchEx(
                              RequestCount : Longint;
                          var ReturnCount  : Longint;
                              pRecBuff     : Pointer;
                          var Error        : TffResult) : TffResult;
      procedure GotoCurrent(aDataSet : TffDataSet);
      function InsertRecordBatch(
                                 Count    : Longint;
                                 pRecBuff : Pointer;
                                 Errors   : PffLongintArray) : TffResult;
      procedure Loaded; override;
      procedure LockTable(LockType: TffLockType);
      function OverrideFilterEx(aExprTree : ffSrBDE.pCANExpr;
                          const aTimeout  : TffWord32) : TffResult;
      function PackTable(var aTaskID    : LongInt) : TffResult;
      procedure RecordCountAsync(var TaskID : Longint);                {!!.07}
      procedure RenameTable(const aNewTableName: string);
      function RestoreFilterEx : TffResult;
      function RestructureTable(aDictionary : TffDataDictionary;
                                  aFieldMap   : TStrings;
                              var aTaskID     : LongInt) : TffResult;
      function SetFilterEx(aExprTree : ffSrBDE.pCANExpr;
                     const aTimeout  : TffWord32) : TffResult;
      function SetTableAutoIncValue(const aValue: TffWord32) : TffResult;
      function Exists : Boolean;
      function TruncateBlob(pRecBuf : pointer;
                            iField  : Word;
                            iLen    : Longint) : TffResult;
      procedure UnlockTable(LockType: TffLockType);
      procedure UnlockTableAll;

      function IsSequenced : Boolean; override;

      property Session : TffSession
         read dsGetSession;

      property CursorID : TffCursorID
         read dsCursorID;

      property Database : TffBaseDatabase
         read dsGetDatabase;

      property Dictionary : TffDataDictionary
         read dsDictionary
         write dsDictionary;

      property ServerEngine : TffBaseServerEngine
        read dsGetServerEngine;

      property DatabaseName : string
         read dsGetDatabaseName
         write dsSetDatabaseName;

      property FilterEval : TffFilterEvaluationType
         read dsFilterEval
         write dsSetFilterEval
         default ffeServer;
         { This property determines where the filter is evaluated.  For best
           performance, evaluate the filter on the server by setting this
           property to ffeServer.  Otherwise, setting this property to
           ffeLocal causes the filter to be evaluated on the client. }

      property FilterResync : Boolean
         read dsFilterResync
         write dsFilterResync
         default True;
         { When this property is set to True, changing the Filter or the
           FilterEval properties causes the server to refresh the dataset.
           Set this property to False when you don't want the server to
           refresh the dataset.  For example, if you have created a cache
           table that inherits from TffTable and the cache table must set to
           the beginning of the dataset anyway, set this property to False
           so that the server does not filter the dataset twice. }

      property FilterTimeout : TffWord32
         read dsFilterTimeOut
         write dsSetFilterTimeOut
         default 500;
        { When retrieving a filtered dataset from the server, the
          number of milliseconds in which the server has to
          respond.  If the server does not respond within the
          specified milliseconds, the OnServerFilterTimeout event
          is raised. }

      property OnServerFilterTimeout: TffServerFilterTimeoutEvent
        read dsOnServerFilterTimeout
        write dsOnServerFilterTimeout;

      property SessionName : string
         read dsGetSessionName
         write dsSetSessionName;

      property Timeout : Longint
         read dsTimeout                                               {!!.06}
         write dsSetTimeout
         default -1;                                                  {!!.01}
         { Timeout specified in milliseconds }

      property Version : string
         read dsGetVersion
         write dsSetVersion
         stored False;

      { The following properties will be published by descendent classes,
        they are included here to reduce duplicity of documentation }
      property BeforeOpen;
      property AfterOpen;
      property BeforeClose;
      property AfterClose;
      property BeforeInsert;
      property AfterInsert;
      property BeforeEdit;
      property AfterEdit;
      property BeforePost;
      property AfterPost;
      property BeforeCancel;
      property AfterCancel;
      property BeforeDelete;
      property AfterDelete;
      property BeforeScroll;
      property AfterScroll;
      {$IFDEF DCC5OrLater}
      property BeforeRefresh;
      property AfterRefresh;
      {$ENDIF}
      property OnCalcFields;
      property OnDeleteError;
      property OnEditError;
      property OnFilterRecord;
      property OnNewRecord;
      property OnPostError;
  end;

  
  TffBaseTable = class(TffDataSet)
    protected {private}
      btFieldsInIndex   : array [0..pred(ffcl_MaxIndexFlds)] of Integer;
                              {fields in key for current index}
      btIndexByName     : Boolean;
        {True if index specified by name, False, by fields}
      btIndexDefs       : TIndexDefs;   {index definitions}
      btIndexFieldCount : Integer;
        {count of fields in key for current index}
      btIndexFieldStr   : string;
        {list of field names in index, sep by semicolons}
      btIndexID         : Word;         {index ID}
      btIndexName       : string;       {index name}
      btKeyBuffer       : Pointer;      {current key Buffer being edited}
      btKeyBuffers      : Pointer;      {all Buffers for editing keys}
      btKeyBufSize      : Integer;      {key Buffer length}
      btKeyInfoOfs      : Integer;      {offset to key info in key Buffer}
      btKeyLength       : Integer;      {key length for current index}
      btLookupCursorID  : TffCursorID;  {lookup cursor}
      btLookupIndexID   : Integer;      {lookup index ID}
      btLookupIndexName : string;       {lookup index name}
      btLookupKeyFields : string;       {key fields for lookup cursor}
      btLookupNoCase    : Boolean;      {case insens. lookup cursor}
      btMasterLink      : TMasterDataLink;     {link to the master table}
      btNoCaseIndex     : Boolean;      {True=case insensitive index}
      btRangeStack      : TffTableRangeStack;
      btIgnoreDataEvents: Boolean;                                     {!!.06}
    protected
      {---Property access methods---}
      function btGetFFVersion : string;                                {!!.11}
      function btGetIndexField(aInx : Integer): TField;
      function btGetIndexFieldNames : string;
      function btGetIndexName : string;
      function btGetKeyExclusive : Boolean;
      function btGetKeyFieldCount : Integer;
      function btGetMasterFields : string;
      function btGetMasterSource : TDataSource;
      procedure btSetKeyExclusive(const aValue : Boolean);
      procedure btSetKeyFieldCount(const aValue : Integer);
      procedure btSetIndexField(aInx : Integer; const aValue : TField);
      procedure btSetIndexFieldNames(const aValue : string);
      procedure btSetIndexName(const aValue : string);
      procedure btSetMasterFields(const aValue : string);
      procedure btSetMasterSource(const aValue : TDataSource);
      procedure dsSetTableName(const aValue : string); override;
      procedure btSetIndexDefs(Value : TIndexDefs);                   {!!.06}
      function btIndexDefsStored : Boolean;                           {!!.06}


      {---Record and key Buffer management---}
      procedure dsAllocKeyBuffers; override;
      procedure btEndKeyBufferEdit(aCommit : Boolean);
      procedure btFreeKeyBuffers;
      function GetActiveRecBuf(var aRecBuf : PChar): Boolean; override;
      function btGetRecordForKey(aCursorID  : TffCursorID;
                                 bDirectKey : Boolean;
                                 iFields    : Word;
                                 iLen       : Word;
                                 pKey       : Pointer;
                                 pRecBuff   : Pointer
                                ) : TffResult;
      procedure btInitKeyBuffer(aBuf : Pointer);
      procedure btSetKeyBuffer(aInx : TffKeyEditType; aMustClear : Boolean);
      procedure btSetKeyFields(aInx : TffKeyEditType;
                         const aValues : array of const);


      {---Record access---}
      function btLocateRecord(const aKeyFields : string;
                              const aKeyValues : Variant;
                                    aOptions   : TLocateOptions;
                                    aSyncCursor: Boolean): Boolean;
      function GetCursorProps(var aProps : TffCursorProps) : TffResult; override;

      {---Field management---}
      function btDoFldsMapToCurIdx(aFields : TList;
                                   aNoCase : Boolean) : Boolean;

      {---Index and key management---}
      procedure btDecodeIndexDesc(const aIndexDesc : IDXDesc;
                                    var aName, aFields : string;
                                    var aOptions   : TIndexOptions);
      procedure btDestroyLookupCursor;
      procedure dsGetIndexInfo; override;
      function btGetIndexDesc(iIndexSeqNo : Word;
                          var idxDesc     : IDXDesc) : TffResult;
      function btGetIndexDescs(Desc : pIDXDesc) : TffResult;
      function btGetLookupCursor(const aKeyFields : string;
                                       aNoCase    : Boolean): TffCursorID;
      function btResetRange(aCursorID : TffCursorID;
                            SwallowSeqAccessError : Boolean) : Boolean; virtual;
      procedure btResetRangePrim(aCursorID : TffCursorID;
                                SwallowSeqAccessError : Boolean);
      procedure btRetrieveIndexName(const aNameOrFields : string;
                                          aIndexByName  : Boolean;
                                      var aIndexName    : string);
      procedure btSetIndexTo(const aParam : string; aIndexByName : Boolean);
      function btSetRange : Boolean;
      function btSetRangePrim(aCursorID  : TffCursorID;
                              bKeyItself : Boolean;
                              iFields1   : Word;
                              iLen1      : Word;
                              pKey1      : Pointer;
                              bKey1Incl  : Boolean;
                              iFields2   : Word;
                              iLen2      : Word;
                              pKey2      : Pointer;
                              bKey2Incl  : Boolean) : TffResult;
      procedure btSwitchToIndex(const aIndexName : string);
      function btSwitchToIndexEx(aCursorID  : TffCursorID;
                            const aIndexName : string;
                            const aIndexID   : Integer;
                            const aCurrRec   : Boolean) : TffResult;

      {---Modes---}
      procedure btCheckKeyEditMode;

      {---Master/detail stuff---}
      procedure dsCheckMasterRange; override;
      procedure btMasterChanged(Sender : TObject);
      procedure btMasterDisabled(Sender : TObject);
      procedure btSetLinkRange(aMasterFields : TList);

      {---Handle stuff---}
      procedure btChangeHandleIndex;
      procedure DestroyHandle(aHandle : TffCursorID); override;
      function GetCursorHandle(aIndexName : string) : TffCursorID; override;

      {---TDataSet method overrides---}
      {Opening, initializing and closing}
      procedure InternalClose; override;
      procedure InternalOpen; override;

      function GetIsIndexField(Field : TField): Boolean; override;

      {Record access and update}
      procedure DoOnNewRecord; override;

      {field access and update}
      procedure SetFieldData(aField : TField; aBuffer : Pointer); override;

      {filtering}
      procedure SetFiltered(Value : Boolean); override;
      procedure dsActivateFilters; override;                           {!!.03}
      procedure dsDeactivateFilters; override;                         {!!.03}

      {information}
      procedure DataEvent(aEvent: TDataEvent; aInfo: Longint); override;

      {indexes - such that they exist at TDataSet level}
      procedure UpdateIndexDefs; override;

      {$IFDEF ProvidesDatasource}
      function GetDataSource: TDataSource; override;
      {$ENDIF}

      property IndexDefs : TIndexDefs
         read btIndexDefs
         write btSetIndexDefs                                         {!!.06}
         stored btIndexDefsStored;                                    {!!.06}

      property IndexFields[aIndex: Integer]: TField
         read btGetIndexField
         write btSetIndexField;

      property IndexFieldCount : Integer
         read btIndexFieldCount;

      property IndexID : Word
         read btIndexID;

      property KeyExclusive : Boolean
         read btGetKeyExclusive
         write btSetKeyExclusive;

      property KeyFieldCount : Integer
         read btGetKeyFieldCount
         write btSetKeyFieldCount;

      property KeySize : Integer
         read btKeyLength;

      property IndexFieldNames : string
         read btGetIndexFieldNames
         write btSetIndexFieldNames;

      property IndexName : string
         read btGetIndexName
         write btSetIndexName;

      property MasterFields : string
         read btGetMasterFields
         write btSetMasterFields;

      property MasterSource : TDataSource
         read btGetMasterSource
         write btSetMasterSource;

{Begin !!.11}
      property FFVersion : string
         read btGetFFVersion;
        { Returns a formatted string (e.g., "2.1300") identifying the version
          of FlashFiler with which the table was created. }
{End !!.11}
    public
      constructor Create(aOwner : TComponent); override;
      destructor Destroy; override;

      procedure AddIndex(const aName, aFields : string;
                               aOptions       : TIndexOptions);
      function AddIndexEx(const aIndexDesc : TffIndexDescriptor;
                            var aTaskID    : LongInt) : TffResult;
      procedure ApplyRange;
      procedure Cancel; override;
      procedure CancelRange;
//    procedure CopyRecords(aSrcTable : TffTable; aCopyBLOBs : Boolean);  {!!.06}
      procedure CreateTable;
      procedure CreateTableEx(const aBlockSize : Integer);            {!!.05}
      procedure DeleteIndex(const aIndexName : string);
      procedure DeleteRecords;                                         {!!.06}
      procedure EditKey;
      procedure EditRangeEnd;
      procedure EditRangeStart;
      function FindKey(const aKeyValues : array of const) : Boolean;
      procedure FindNearest(const aKeyValues : array of const);
      procedure GetIndexNames(aList : TStrings);
      function GotoKey : Boolean;
      procedure GotoNearest;
      function Locate(const aKeyFields : string;
                      const aKeyValues : Variant;
                            aOptions   : TLocateOptions) : Boolean; override;
      function Lookup(const aKeyFields    : string;
                      const aKeyValues    : Variant;
                      const aResultFields : string) : Variant; override;
      procedure Post; override;
      function ReIndexTable(const aIndexNum  : Integer;
                             var aTaskID    : Longint) : TffResult;
      procedure SetKey;
      procedure SetRange(const aStartValues, aEndValues : array of const);
      procedure SetRangeEnd;
      procedure SetRangeStart;
  end;

  TffBaseDatabase = class(TffDBListItem)
    protected {private}
      bdAutoDBName : Boolean;
      bdInTransaction : Boolean;
      bdDatabaseID    : TffDatabaseID;
      bdTransactionCorrupted : Boolean;
      bdExclusive     : Boolean;
      bdFailSafe      : Boolean;
      bdReadOnly      : Boolean;
      bdServerEngine  : TffBaseServerEngine;
//      bdTemporary     : Boolean;                                     {Deleted !!.01}
      bdTimeout       : Longint;
    protected
      function bdGetDataSet(aInx : Integer) : TffDataSet;
      function bdGetDataSetCount : Integer;
      function bdGetDatabaseID : TffDatabaseID;
      function bdGetSession : TffSession;
      function bdGetServerEngine : TffBaseServerEngine;
      procedure bdRefreshTimeout;                                        {!!.11}
      procedure bdSetAutoDBName(const Value : Boolean);
      procedure bdSetDatabaseName(const aName : string);
      procedure bdSetExclusive(aValue : Boolean);
      procedure bdSetReadOnly(aValue : Boolean);
      procedure bdSetTimeout(const Value : Longint);

      function dbliCreateOwnedList : TffDBList; override;
      function dbliFindDBOwner(const aName : string) : TffDBListItem; override;
      procedure bdInformTablesAboutDestruction;
      procedure dbliMustBeClosedError; override;
      procedure dbliMustBeOpenError; override;
      procedure dbliOpenPrim; override;

      property AutoDatabaseName : Boolean
        read bdAutoDBName
        write bdSetAutoDBName
        default False;

      property DatabaseID : TffDatabaseID
         read bdGetDatabaseID;

      property DataSetCount : Integer
         read bdGetDataSetCount;

      property DataSets[aInx : Integer] : TffDataSet
         read bdGetDataSet;

      property ServerEngine : TffBaseServerEngine
        read bdGetServerEngine;

      property Session : TffSession
         read bdGetSession;

{Begin !!.01}
//      property Temporary : Boolean
//         read bdTemporary
//         write bdTemporary;
{End !!.01}

      property Connected;

      property DatabaseName : string
         read dbliDBName
         write bdSetDatabaseName;

      property Exclusive : Boolean
         read bdExclusive
         write bdSetExclusive
         default False;

      property ReadOnly : Boolean
         read bdReadOnly
         write bdSetReadOnly
         default False;

      property SessionName : string
         read dbliGetDBOwnerName
         write dbliSetDBOwnerName;

      property Timeout : Longint
         read bdTimeout
         write bdSetTimeout
         default -1;
         { Timeout specified in milliseconds }
   public
      constructor Create(aOwner : TComponent); override;
      destructor Destroy; override;

      function GetFreeDiskSpace (var aFreeSpace : Longint) : TffResult;
      function GetTimeout : Longint;
      procedure CloseDataSets;
      function IsSQLBased : Boolean;
      function PackTable(const aTableName : TffTableName;
                           var aTaskID    : LongInt) : TffResult;
      procedure Commit;
      function ReIndexTable(const aTableName : TffTableName;
                              const aIndexNum  : Integer;
                                var aTaskID    : Longint) : TffResult;
      procedure Rollback;
      procedure StartTransaction;
      function StartTransactionWith(const aTables: array of TffBaseTable) : TffResult; {!!.10}
        { Start a transaction, but only if an exclusive lock is obtained
          for the specified tables. }
      function TryStartTransaction : Boolean;
      procedure TransactionCorrupted;
      function TableExists(const aTableName : TffTableName) : Boolean;

      {---Miscellaneous---}
      function GetFFDataDictionary(                 { return a FlashFiler DD}
                             const TableName   : TffTableName;
                                   Stream      : TStream
                                  ) : TffResult;

      property FailSafe : Boolean
         read bdFailSafe
         write bdFailSafe
         default False;

      property InTransaction : Boolean
         read bdInTransaction;
  end;

  TffDatabase = class(TffBaseDatabase)
    protected {private}
      dcAliasName     : string;
    protected
      procedure dcSetAliasName(const aName : string);

      procedure dbliClosePrim; override;
      procedure dbliOpenPrim; override;
    public
      function CreateTable(const aOverWrite  : Boolean;
                           const aTableName  : TffTableName;
                                 aDictionary : TffDataDictionary) : TffResult;

      procedure GetTableNames(aList : TStrings);

      function RestructureTable(const aTableName  : TffTableName;
                                      aDictionary : TffDataDictionary;
                                      aFieldMap   : TStrings;
                                  var aTaskID     : LongInt) : TffResult;

      property DatabaseID;
      property DataSetCount;
      property DataSets;
      property ServerEngine;
      property Session;
      property Temporary;
    published
      property AliasName : string
         read dcAliasName
         write dcSetAliasName;

      property AutoDatabaseName;
      property Connected;
      property DatabaseName;
      property Exclusive;
      property FailSafe;
      property ReadOnly;
      property SessionName;
      property Timeout;
  end;

  TffDatabaseList = class(TffDBList)
    protected {private}
      function dlGetItem(aInx : Integer) : TffBaseDatabase;
    public
      property Databases[aInx : Integer] : TffBaseDatabase
         read dlGetItem; default;
  end;

  TffTableProxy = class(TffDBListItem)
    protected {private}
      tpClosing     : Boolean;
      tpCursorID    : TffCursorID;
      tpDBGone      : Boolean;
      tpffTable     : TffDataSet;
      tpServerEngine: TffBaseServerEngine;
      tpSession     : TffSession;
      tpSessionName : string;

    protected
      function tpGetCursorID : TffCursorID;
      function tpGetDatabase : TffBaseDatabase;
      function tpGetSession : TffSession;
      function tpGetSessionName : string;
      function tpGetServerEngine : TffBaseServerEngine;
      procedure tpSetSessionName(aValue : string);

      procedure dbliClosePrim; override;
      function dbliFindDBOwner(const aName : string) : TffDBListItem; override;
      procedure dbliLoaded; override;
      procedure dbliMustBeClosedError; override;
      procedure dbliMustBeOpenError; override;
      procedure dbliOpenPrim; override;
      procedure dbliDBOwnerChanged; override;

      procedure tpDatabaseIsDestroyed;
      procedure tpResolveSession;

      property ffTable : TffDataSet
         read tpffTable
         write tpffTable;
    public
      constructor Create(aOwner : TComponent); override;

      property CursorID : TffCursorID
         read tpGetCursorID;

      property Database : TffBaseDatabase
         read tpGetDatabase;

      property Session : TffSession
         read tpGetSession;

      property Active;

      property DatabaseName : string
         read dbliGetDBOwnerName
         write dbliSetDBOwnerName;

      property SessionName : string
         read tpGetSessionName
         write tpSetSessionName;

      property ServerEngine : TffBaseServerEngine
         read tpGetServerEngine;

      property TableName : string
         read dbliDBName
         write dbliSetDBName;
  end;

  TffTableProxyList = class(TffDBList)
    protected {private}
      procedure dblFreeItem(aItem : TffDBListItem); override;
      function tlGetItem(aInx : Integer) : TffTableProxy;
    public
      property Tables[aInx : Integer] : TffTableProxy
         read tlGetItem; default;
  end;


  TffTable = class(TffBaseTable)
    public
      property CursorID;
      property Database;
      property Dictionary;
      property FFVersion;                                              {!!.11}
      {$IFDEF Delphi3}                                                 {!!.01}
      property IndexDefs;
      {$ENDIF}                                                         {!!.01}
      property IndexFields;
      property IndexFieldCount;
      property KeyExclusive;
      property KeyFieldCount;
      property KeySize;
    published
      property Active;
      property AutoCalcFields;
      property DatabaseName;
      property Exclusive;
{Begin !!.01}
      {$IFDEF CBuilder3}
      property FieldDefs;
      {$ENDIF}
      {$IFDEF Dcc4orLater}
      property FieldDefs;
      {$ENDIF}
{End !!.01}
      property Filter;
      property Filtered;
      property FilterEval;
      property FilterOptions;
      property FilterResync;
      property FilterTimeout;
{Begin !!.01}
      {$IFDEF CBuilder3}
      property IndexDefs;
      {$ENDIF}
      {$IFDEF Dcc4orLater}
      property IndexDefs;
      {$ENDIF}
{End !!.01}
      property IndexFieldNames;
      property IndexName;
      property MasterFields;
      property MasterSource;
      property ReadOnly;
      property SessionName;
      property TableName;
      property Timeout;
      property Version;

      property BeforeOpen;
      property AfterOpen;
      property BeforeClose;
      property AfterClose;
      property BeforeInsert;
      property AfterInsert;
      property BeforeEdit;
      property AfterEdit;
      property BeforePost;
      property AfterPost;
      property BeforeCancel;
      property AfterCancel;
      property BeforeDelete;
      property AfterDelete;
      property BeforeScroll;
      property AfterScroll;
      {$IFDEF DCC5OrLater}
      property BeforeRefresh;
      property AfterRefresh;
      {$ENDIF}
      property OnCalcFields;
      property OnDeleteError;
      property OnEditError;
      property OnFilterRecord;
      property OnNewRecord;
      property OnPostError;
      property OnServerFilterTimeout;
  end;

  TffBlobStream = class(TStream)
    private
      bsRecBuf    : PChar;
      bsTable     : TffDataSet;
      bsField     : TBlobField;
      bsFieldNo   : Integer;
      bsMode      : TBlobStreamMode;
      bsModified  : Boolean;
      bsOpened    : Boolean;
      bsPosition  : Longint;
      bsChunkSize : Longint;
      bsCancel    : Boolean;

    protected
      function bsGetBlobSize : Longint;

    public
      constructor Create(aField : TBlobField; aMode : TBlobStreamMode);
      destructor Destroy; override;

      function Read(var aBuffer;
                        aCount : Longint)
                               : Longint; override;
      function Write(const aBuffer; aCount: Longint) : Longint; override;
      function Seek(aoffset : Longint; aOrigin : Word) : Longint; override;
      procedure Truncate;

      property CurrPosition : Longint
         read bsPosition;

      property CurrSize : Longint
         read bsGetBlobSize;

      property ChunkSize : Longint
         read bsChunkSize
         write bsChunkSize;

      property CancelTransfer : Boolean
         write bsCancel;
  end;

  TffQuery = class;  { forward declaration }

  {$IFDEF DCC4OrLater}
  TffQueryDataLink = class(TDetailDataLink)
  {$ELSE}
  TffQueryDataLink = class(TDataLink)
  {$ENDIF}
  protected {private}
    FQuery: TffQuery;
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    {$IFDEF DCC4OrLater}
    function GetDetailDataSet: TDataSet; override;
    {$ENDIF}
    procedure CheckBrowseMode; override;
  public
    constructor Create(aQuery: TffQuery);
  end;


  TffQuery = class(TffDataSet)
    protected {private}
      FCanModify    : Boolean;                                        {!!.10}
      FDataLink     : TDataLink;
      FExecuted     : boolean;
        { Set to True if statement has been executed. }
      FParamCheck   : boolean;
      FParams       : TParams;
      FPrepared     : boolean;
      FRequestLive  : boolean;
      FRowsAffected : Integer;                                        {!!.10}
      FRecordsRead  : Integer;                                        {!!.10}
      FSQL          : TStrings;
      FStmtID       : TffSqlStmtID;
      FText         : string;

      {$IFDEF DCC4OrLater}
      procedure DefineProperties(Filer : TFiler); override;
      {$ENDIF}
      procedure DestroyHandle(aHandle : TffCursorID); override;
      procedure dsCloseViaProxy; override;
      function dsGetServerEngine : TffBaseServerEngine; override;
      function GetCanModify : Boolean; override;
      function GetCursorHandle(aIndexName : string) : TffCursorID; override;
      function GetCursorProps(var aProps : TffCursorProps) : TffResult; override;
      procedure InternalClose; override;
      procedure quBuildParams(var ParamsList : PffSqlParamInfoList;
                              var ParamsData : PffByteArray;
                              var ParamsDataLen : integer);
        {-Constructs the parameter data sent to the server. }
      procedure quDisconnect;
      procedure quExecSQLStmt(const aOpenMode : TffOpenMode;
                                var aCursorID : TffCursorID);
      procedure quFreeStmt;
      function quGetDataSource : TDataSource;
      function quGetParamCount : Word;
      function quGetRowsAffected : Integer;                           {!!.10}
{Begin !!.01}
      function quLocateRecord(const aKeyFields : string;
                              const aKeyValues : Variant;
                                    aOptions   : TLocateOptions;
                                    aSyncCursor: Boolean): Boolean;
{End !!.01}
      function quParseSQL(aStmt : string; createParams : boolean;
                          aParams : TParams) : string;
      procedure quPreparePrim(prepare : boolean);
      {$IFDEF DCC4OrLater}
      procedure quReadParams(Reader : TReader);
      {$ENDIF}
      procedure quRefreshParams;
      procedure quSetDataSource(aSrc : TDataSource);
      procedure quSetParams(aParamList : TParams);
      procedure quSetParamsFromCursor;
      procedure quSetPrepared(aFlag : boolean);
      procedure quSetRequestLive(aFlag : boolean);
      procedure quSetSQL(aValue : TStrings);
      procedure quSQLChanged(Sender : TObject);
        {-Called when the SQL property changes.  Allows us to update the
          Params property. }
      {$IFDEF DCC4OrLater}
        procedure quWriteParams(Writer : TWriter);
      {$ENDIF}

      property DataLink : TDataLink
         read FDataLink;

    public
      constructor Create(aOwner : TComponent); override;
      destructor Destroy; override;
      procedure ExecSQL;                                              {!!.10}
{Begin !!.01}
      function Locate(const aKeyFields : string;
                      const aKeyValues : Variant;
                            aOptions   : TLocateOptions) : Boolean; override;
{End !!.01}
      function Lookup(const aKeyFields    : string;
                      const aKeyValues    : Variant;
                      const aResultFields : string) : Variant; override;

      function ParamByName(const aName : string) : TParam;
      procedure Prepare;
      procedure Unprepare;

      property Prepared : boolean
         read FPrepared
         write quSetPrepared;
      property RowsAffected : Integer                                 {!!.10}
         read quGetRowsAffected;
      property RecordsRead: Integer read FRecordsRead;                {!!.10}
      property Text : string
         read FText;

    published
      property Active;
      property AutoCalcFields;
      property DatabaseName;
      property DataSource : TDataSource
         read quGetDataSource
         write quSetDataSource;
      property Filter;
      property Filtered;
      property FilterEval;
      property FilterOptions;
      property FilterResync;
      property FilterTimeout;
      property ParamCheck : boolean
         read FParamCheck
         write FParamCheck
         default True;
      property ParamCount : Word
         read quGetParamCount;
      property Params : TParams
         read FParams
         write quSetParams
         stored False;
      property RequestLive : boolean
         read FRequestLive
         write quSetRequestLive
         default False;
      property SessionName;
      property SQL : TStrings
         read FSQL
         write quSetSQL;
      property StmtHandle : TffSqlStmtID
         read FStmtID;
      property Timeout;
      property Version;

    { Events }
      property BeforeOpen;
      property AfterOpen;
      property BeforeClose;
      property AfterClose;
      property BeforeInsert;
      property AfterInsert;
      property BeforeEdit;
      property AfterEdit;
      property BeforePost;
      property AfterPost;
      property BeforeCancel;
      property AfterCancel;
      property BeforeDelete;
      property AfterDelete;
      property BeforeScroll;
      property AfterScroll;
      {$IFDEF DCC5OrLater}
      property BeforeRefresh;
      property AfterRefresh;
      {$ENDIF}
      property OnCalcFields;
      property OnDeleteError;
      property OnEditError;
      property OnFilterRecord;
      property OnNewRecord;
      property OnPostError;
      property OnServerFilterTimeout;
  end;


{---Helper routines---}
function FindAutoFFClient : TffBaseClient;
{ Find the automatically created client component}

function FindDefaultFFClient : TffBaseClient;
{ Find the default Client component }

function FindDefaultFFSession : TffSession;
{ Find the default session }

function FindFFClientName(const aName : string) : TffBaseClient;
{ Find a client by name}

function FindFFSessionName(const aName : string) : TffSession;
{ Find a session object by name }

function FindFFDatabaseName(aSession : TffSession;
                      const aName    : string;
                      const aCreate  : Boolean) : TffBaseDatabase;
{ Find a database object by name}

function GetDefaultFFClient : TffBaseClient;
{ Return the default client. If one doesn't exist, raise
  an exception}

function GetDefaultFFSession : TffSession;
{ Return the default session. If one does not exist, raise
  an exception}

procedure GetFFClientNames(aList : TStrings);
{ Populate a list with the names of all TffBaseClient instances}

procedure GetFFSessionNames(aList : TStrings);
{ Populate a list with the names of all TffSession instances}

procedure GetFFDatabaseNames(aSession : TffSession; aList : TStrings);

{ Populate a list with all TffBaseDatabase instances }

function Session : TffSession;
{ Return the default session component}

function FFSession : TffSession;
{ Return the default session component. Included to ease confusion
  when writing applications that use both the BDE and FlashFiler}

const
  { 0 means do not limit "chunk" sizes, any other value determines }
  { the maximum number of bytes read/written to the server at once}
  ffMaxBlobChunk : Integer = 64000;

{---Global variables---}
var
  Clients : TffClientList;

implementation

{Notes: A record Buffer is in the following format
           - physical record Buffer
               (offset 0, length RecordSize)
           - calculated fields Buffer
               (offset dsCalcFldOfs, length CalcFieldSize)
           - bookmark data
               (offset dsBookmarkOfs, length BookmarkSize)
           - TDataSetRecInfo data
               (offset dsRecInfoOfs, length sizeof(TDataSetRecInfo))
        A key Buffer is in the following format
           - physical record Buffer
               (offset 0, length RecordSize)
           - TKeyRecInfo data
               (offset btKeyInfoOfs, length sizeof(TKeyRecInfo))
        TDataSet maintains an array of record Buffers.
        TffTable maintains an array of key Buffers, one for each of
          the TffKeyEditType enum values}

uses
  Forms,
  TypInfo,
  {$IFDEF HasNonComVariant}
  Variant,
  {$ENDIF}
  ffconst,
  ffllexcp,
  ffclconv,
  ffclintf,
{$IFDEF AutoLog}                                                       {!!.01}
  fflllog,                                                             {!!.01}
{$ENDIF}                                                               {!!.01}
  Dialogs,
  ffutil;

{$UNDEF DeclareMissingIdentifiers}
{$IFDEF DCC5OrLater}                                                   {!!.11}
{$DEFINE DeclareMissingIdentifiers}
{$ENDIF}

{$IFDEF DeclareMissingIdentifiers}
{Note: In Delphi 3, 4 and C++Builder 3, 4, the following constants
       were defined in DBCOMMON.PAS and were available to third-party
       database engine developers. In Delphi 5, they were moved to
       DBTABLES.PAS which, because of the initialization section
       cannot be used as a unit in ffDB. Hence these definitions are
       copied here from Delphi 5's DBTABLES.PAS. A bug report has been
       filed with Borland.}
const

  {$IFNDEF DCC6OrLater}
  FldTypeMap: TFieldMap = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16, fldBOOL,
    fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, fldBYTES,
    fldVARBYTES, fldINT32, fldBLOB, fldBLOB, fldBLOB, fldBLOB, fldBLOB,
    fldBLOB, fldBLOB, fldCURSOR, fldZSTRING, fldZSTRING, fldINT64, fldADT,
    fldArray, fldREF, fldTABLE, fldBLOB, fldBLOB, fldUNKNOWN, fldUNKNOWN,
    fldUNKNOWN, fldZSTRING);

  DataTypeMap: array[0..MAXLOGFLDTYPES - 1] of TFieldType = (
    ftUnknown, ftString, ftDate, ftBlob, ftBoolean, ftSmallint,
    ftInteger, ftFloat, ftBCD, ftBytes, ftTime, ftDateTime,
    ftWord, ftInteger, ftUnknown, ftVarBytes, ftUnknown, ftUnknown,
    ftLargeInt, ftLargeInt, ftADT, ftArray, ftReference, ftDataSet);

  {$ELSE}
  FldTypeMap: TFieldMap = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16, fldBOOL,
    fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, fldBYTES,
    fldVARBYTES, fldINT32, fldBLOB, fldBLOB, fldBLOB, fldBLOB, fldBLOB,
    fldBLOB, fldBLOB, fldCURSOR, fldZSTRING, fldZSTRING, fldINT64, fldADT,
    fldArray, fldREF, fldTABLE, fldBLOB, fldBLOB, fldUNKNOWN, fldUNKNOWN,
    fldUNKNOWN, fldZSTRING, fldTIMESTAMP, fldBCD);

  DataTypeMap: array[0..MAXLOGFLDTYPES - 1] of TFieldType = (
    ftUnknown, ftString, ftDate, ftBlob, ftBoolean, ftSmallint,
    ftInteger, ftFloat, ftBCD, ftBytes, ftTime, ftDateTime,
    ftWord, ftInteger, ftUnknown, ftVarBytes, ftUnknown, ftUnknown,
    ftLargeInt, ftLargeInt, ftADT, ftArray, ftReference, ftDataSet,
    ftTimeStamp, ftFMTBCD);

  {$ENDIF}

 BlobTypeMap: array[fldstMEMO..fldstBFILE] of TFieldType = (
    ftMemo, ftBlob, ftFmtMemo, ftParadoxOle, ftGraphic, ftDBaseOle,
    ftTypedBinary, ftBlob, ftBlob, ftBlob, ftBlob, ftOraClob, ftOraBlob,
    ftBlob, ftBlob);
{$ENDIF}

const
  ffcClientName = 'ClientName';
  ffcDatabaseName = 'DatabaseName';
  ffcSessionName = 'SessionName';
  ffcTableName = 'TableName';
  {$IFDEF AutoLog}
  ffcAutoLogfile = 'FFAutoTrans.log';
  {$ENDIF}

type
  PffFLDDescArray = ^TffFLDDescArray;
  TffFLDDescArray = array [0..($ffE0 div sizeof(FLDDesc))] of FLDDesc;

  PffIDXDescArray = ^TffIDXDescArray;
  TffIDXDescArray = array [0..($ffE0 div sizeof(IDXDesc))] of IDXDesc;

  PffVCHKDescArray = ^TffVCHKDescArray;
  TffVCHKDescArray = array [0..($ff00 div sizeof(VCHKDesc))] of VCHKDesc;


type
  PDataSetRecInfo = ^TDataSetRecInfo;
  TDataSetRecInfo = packed record
    riBookmarkFlag : TBookmarkFlag;
    riRecNo : TffWord32;
  end;

  PKeyRecInfo = ^TKeyRecInfo;
  TKeyRecInfo = packed record
    kriFieldCount : Integer;   {for the KeyFieldCount property}
    kriExclusive  : Boolean;   {for the KeyExclusive property}
    kriModified   : Boolean;   {data in Buffer has been modified}
  end;

  PKeyBuffers = ^TKeyBuffers;
  TKeyBuffers = array [TffKeyEditType] of Pointer;

{$IFDEF SingleEXE}
var
  ServerEngine : TffServerEngine;
{$ENDIF}

{== Database object search routines ==================================}
function IsFFAliasName(aSession : TffSession;
                       aName    : string)
                                : Boolean;
var
  i         : Integer;
  AliasList : TStringList;
begin
  if (aSession = nil) or (aName = '') then begin
    Result := False;
    Exit;
  end;
  Result := True;
  AliasList := TStringList.Create;
  try
    aSession.GetAliasNamesEx(AliasList, False);
    for i := 0 to pred(AliasList.Count) do
      if (FFAnsiCompareText(AliasList[i], aName) = 0) then             {!!.10}
        Exit;
  finally
    AliasList.Free;
  end;{try..finally}
  Result := False;
end;
{--------}
function IsFFDatabaseName(aSession : TffSession;
                          aName    : string)
                                   : Boolean;
var
  DB : TffDbListItem;
begin
  if (aSession = nil) or (aName = '') then
    Result := False
  else
    Result := aSession.OwnedDBItems.FindItem(aName, DB);
end;
{--------}
function FindAutoffClient : TffBaseClient;
begin
  Result := FindFFClientName(AutoObjName);
end;
{--------}
function FindDefaultFFClient : TffBaseClient;
var
  Inx : Integer;
begin
  Assert(Assigned(Clients));
  Clients.BeginRead;                                                   {!!.02}
  try                                                                  {!!.02}
    for Inx := 0 to Pred(Clients.Count) do begin
      Result := TffBaseClient(Clients[Inx]);
      if Result.IsDefault then
        Exit;
    end;
  finally                                                              {!!.02}
    Clients.EndRead;                                                   {!!.02}
  end;                                                                 {!!.02}
  Result := nil;
end;
{--------}
function FindDefaultFFSession : TffSession;
var
  CL : TffBaseClient;
begin
  CL := FindDefaultFFClient;
  if Assigned(CL) then
    Result := CL.bcGetDefaultSession
  else
    Result := nil;
end;
{--------}
function FindFFClientName(const aName : string) : TffBaseClient;
begin
  Assert(Assigned(Clients));
  if aName = '' then
    Result := nil
  else
    if not Clients.FindItem(aName, TffDBListItem(Result)) then
      Result := nil;
end;
{--------}
function FindFFSessionName(const aName : string) : TffSession;
var
  CEInx : Integer;
begin
  Assert(Assigned(Clients));
  if aName = '' then
    Result := nil
  else begin
    Clients.BeginRead;                                                 {!!.02}
    try                                                                {!!.02}
      for CEInx := 0 to pred(Clients.Count) do begin
        if (Clients[CEInx]).
              OwnedDBItems.
                FindItem(aName, TffDBListItem(Result)) then
          Exit;
      end;
    finally                                                            {!!.02}
      Clients.EndRead;                                                 {!!.02}
    end;                                                               {!!.02}
    Result := nil;
  end;
end;
{--------}
function FindFFDatabaseName(aSession : TffSession;
                      const aName    : string;
                      const aCreate  : Boolean) : TffBaseDatabase;
var
  i         : Integer;
  AliasList : TStringList;
begin
  if (aName = '') or (aSession = nil) then begin
    Result := nil;
    Exit;
  end;
  { if the database is found, set result and exit}
  if aSession.OwnedDBItems.FindItem(aName, TffDBListItem(Result)) then
    Exit;
  if aCreate then begin
    AliasList := TStringList.Create;
    try
      aSession.GetAliasNamesEx(AliasList, False);
      { if the alias is valid, create the database and exit }
      for i := 0 to pred(AliasList.Count) do
        if (FFAnsiCompareText(AliasList[i], aName) = 0) then begin    {!!.07}
          Result := TffDatabase.Create(nil);
          Result.dbliSwitchOwnerTo(aSession);                          {!!.01}
//          Result.SessionName := aSession.SessionName;                {Deleted !!.01}
          Result.DatabaseName := aName;
          Result.Temporary := True;
          Exit;
        end;
    finally
      AliasList.Free;
    end;
  end;
  { the database was not found, or the alias did not exist }
  Result := nil;
end;
{--------}
function GetDefaultFFClient : TffBaseClient;
begin
  Result := FindDefaultFFClient;
  if (Result = nil) then
    raise EffDatabaseError.Create(ffStrResDataSet[ffdse_NoDefaultCL]);
end;
{--------}
function GetDefaultFFSession : TffSession;
begin
  Result := GetDefaultFFClient.bcGetDefaultSession;
  if (Result = nil) then
    raise EffDatabaseError.Create(ffStrResDataSet[ffdse_NoSessions]);
end;
{--------}
procedure GetFFDatabaseNames(aSession : TffSession; aList : TStrings);
begin
  Assert(Assigned(aList));
  Assert(Assigned(aSession));
  aList.BeginUpdate;
  try
    aList.Clear;
    aSession.OwnedDBItems.GetItemNames(aList);
    aSession.GetAliasNamesEx(aList, False);
  finally
    aList.EndUpdate;
  end;
end;
{--------}
function FFSession : TffSession;
begin
  Result := GetDefaultffSession;
end;
{--------}
function Session : TffSession;
begin
  Result := FFSession;
end;

{====================================================================}


{===Database object name lists=======================================}
procedure GetFFClientNames(aList : TStrings);
begin
  Assert(Assigned(Clients));
  Assert(Assigned(aList));
  aList.BeginUpdate;
  try
    aList.Clear;
    Clients.GetItemNames(aList);
  finally
    aList.EndUpdate;
  end;
end;
{--------}
procedure GetFFSessionNames(aList : TStrings);
var
  Inx : Integer;
begin
  Assert(Assigned(Clients));
  Assert(Assigned(aList));
  Clients.BeginRead;                                                   {!!.02}
  try                                                                  {!!.02}
    for Inx := 0 to Pred(Clients.Count) do
      Clients[Inx].OwnedDBItems.GetItemNames(aList);
  finally                                                              {!!.02}
    Clients.EndRead;                                                   {!!.02}
  end;                                                                 {!!.02}
end;
{====================================================================}

{===TffFilterListItem==================================================}
constructor TffFilterListItem.Create(aContainer : TffCollection;
                                     aOwner     : TObject;
                                     aClientData: Longint;
                                     aPriority  : Integer;
                                     aCanAbort  : Boolean;
                                     aExprTree  : pCANExpr;
                                     aFiltFunc  : pfGENFilter);
begin
  inherited Create(nil, aContainer);

  fliOwner := aOwner;
  fliClientData := aClientData;
  fliPriority := aPriority;
  fliCanAbort := aCanAbort;
  if Assigned(aExprTree) then begin
    fliExprSize := pCANExpr(aExprTree)^.iTotalSize;
    if (fliExprSize > 0) then begin
      FFGetMem(fliExpression, fliExprSize);
      Move(aExprTree^, fliExpression^, fliExprSize);
    end;
  end;
  fliFilterFunc := aFiltFunc;
  fliActive := False;
end;
{--------}
destructor TffFilterListItem.Destroy;
begin
  if (fliExprSize > 0) and Assigned(fliExpression) then
     FFFreeMem(fliExpression, fliExprSize);

  inherited Destroy;
end;
{--------}
function TffFilterListItem.fliGetLiteralPtr(aoffset : Word) : Pointer;
var
  i : Word;
begin
  i := fliExpression^.iLiteralStart + aoffset;
  Result := @PByteArray(fliExpression)^[i];
end;
{--------}
function TffFilterListItem.fliGetNodePtr(aoffset : Word) : PffFilterNode;
var
  i : Word;
begin
  i := fliExpression^.iNodeStart + aoffset;
  Result := PffFilterNode(@PByteArray(fliExpression)^[i]);
end;
{--------}
procedure TffFilterListItem.GetFilterInfo(Index      : Word;
                                      var FilterInfo : FilterInfo);
begin
  {Initialize}
  FillChar(FilterInfo, sizeof(FilterInfo), 0);

  {Set info}
  FilterInfo.iFilterId   := Index;
  FilterInfo.hFilter     := @Self;
  FilterInfo.iClientData := fliClientData;
  FilterInfo.iPriority   := fliPriority;
  FilterInfo.bCanAbort   := fliCanAbort;
  FilterInfo.pffilter    := fliFilterFunc;
  FilterInfo.pCanExpr    := fliExpression;
  FilterInfo.bActive     := fliActive;
end;
{--------}
function TffFilterListItem.MatchesRecord(aRecBuf : Pointer) : Boolean;
var
  FiltFuncResult : Integer;
  Root           : PffFilterNode;
begin
  {inactive filters match all records, ie, no filtering takes place}
  if not Active then
    Result := True
  {otherwise, with active filters we must do some work}
  else begin
    {call the filter function first}
    if Assigned(fliFilterFunc) then begin
      FiltFuncResult := fliFilterFunc(fliClientData, aRecBuf, 0);
      if fliCanAbort and (FiltFuncResult = FFClBDE.ABORT) then begin
        Result := False;
        Exit;
      end;
      Result := FiltFuncResult <> 0;
    end
    else {there is no filter function, ergo it matches}
      Result := True;

    {if the record matches so far, run it through the filter tree}
    if Result and Assigned(fliExpression) then begin
      Root := fliGetNodePtr(0);
      Result := fliEvaluateNode(Root, nil, aRecBuf);
    end;
  end;
end;
{--------}
function TffFilterListItem.fliEvaluateNode(aNode   : PffFilterNode;
                                           aValue  : PffNodeValue;
                                           aRecBuf : Pointer) : Boolean;
begin
  if (aValue <> nil) then
    FillChar(aValue^, sizeof(aValue^), 0);
  case aNode^.fnHdr.NodeClass of
    FFSrBDE.nodeUNARY:
      Result := fliEvaluateUnaryNode(aNode, aRecBuf);
    FFSrBDE.nodeBINARY:
      if (aNode^.fnHdr.CANOp in [canAND, canOR]) then
        Result := fliEvaluateLogicalNode(aNode, aRecBuf)
      else
        Result := fliEvaluateBinaryNode(aNode, aRecBuf, False, 0);
    FFSrBDE.nodeCOMPARE:
      Result := fliEvaluateBinaryNode(aNode, aRecBuf,
                   aNode^.fnCompare.bCaseInsensitive,
                   aNode^.fnCompare.iPartialLen);
    FFSrBDE.nodeFIELD:
      Result := fliEvaluateFieldNode(aNode, aValue, aRecBuf);
    FFSrBDE.nodeCONST:
      Result := fliEvaluateConstNode(aNode, aValue, aRecBuf);
    FFSrBDE.nodeCONTINUE:
      Result := aNode^.fnContinue.iContOperand <> 0;
  else
    {all other node classes cause the node match to fail}
    Result := False;
  end;{case}
end;
{--------}
function TffFilterListItem.fliEvaluateUnaryNode(aNode   : PffFilterNode;
                                                aRecBuf : Pointer) : Boolean;
var
  OperandNode : PffFilterNode;
  NodeValue   : TffNodeValue;
begin
  OperandNode := fliGetNodePtr(aNode^.fnUnary.iOperand1);
  if fliEvaluateNode(OperandNode, @NodeValue, aRecBuf) then
    case aNode^.fnHdr.CANOp of
      canISBLANK:
        Result := NodeValue.nvIsNull;
      canNOTBLANK:
        Result := not NodeValue.nvIsNull;
    else
      Result := False;
    end {case}
  else { the node didn't match }
    Result := aNode^.fnHdr.CANOp = canNOT;
end;
{--------}
function TffFilterListItem.fliEvaluateLogicalNode(aNode   : PffFilterNode;
                                                  aRecBuf : Pointer) : Boolean;
var
  LeftNode   : PffFilterNode;
  RightNode  : PffFilterNode;
begin
  LeftNode := fliGetNodePtr(aNode^.fnBINARY.iOperand1);
  RightNode := fliGetNodePtr(aNode^.fnBINARY.iOperand2);
  case aNode^.fnHdr.CANOp of
    canAND : Result := fliEvaluateNode(LeftNode, nil, aRecBuf) and
                       fliEvaluateNode(RightNode, nil, aRecBuf);
    canOR  : Result := fliEvaluateNode(LeftNode, nil, aRecBuf) or
                       fliEvaluateNode(RightNode, nil, aRecBuf);
  else
    {anything else fails}
    Result := False;
  end;{case}
end;
{--------}
function TffFilterListItem.fliEvaluateBinaryNode(aNode   : PffFilterNode;
                                                 aRecBuf : Pointer;
                                                 aNoCase : Boolean;
                                                 aPartial: Word) : Boolean;
var
  LeftNode   : PffFilterNode;
  RightNode  : PffFilterNode;
  LeftValue  : TffNodeValue;
  RightValue : TffNodeValue;
  CompareResult : Integer;
begin
  Result := False;
  if (aNode^.fnHdr.NodeClass = FFSrBDE.nodeCOMPARE) then begin
    LeftNode := fliGetNodePtr(aNode^.fnCompare.iOperand1);
    RightNode := fliGetNodePtr(aNode^.fnCompare.iOperand2);
  end else begin
    LeftNode := fliGetNodePtr(aNode^.fnBINARY.iOperand1);
    RightNode := fliGetNodePtr(aNode^.fnBINARY.iOperand2);
  end;
  if not fliEvaluateNode(LeftNode, @LeftValue, aRecBuf) then
    Exit;
  if not fliEvaluateNode(RightNode, @RightValue, aRecBuf) then
    Exit;
  if not fliCompareValues(CompareResult, LeftValue, RightValue,
                                         aNoCase, aPartial) then
    Exit;
  case aNode^.fnHdr.CANOp of
    canEQ : Result := CompareResult = 0;
    canNE : Result := CompareResult <> 0;
    canGT : Result := CompareResult > 0;
    canLT : Result := CompareResult < 0;
    canGE : Result := CompareResult >= 0;
    canLE : Result := CompareResult <= 0;
  else
    {anything else fails}
    Result := False;
  end;{case}
end;
{--------}
function TffFilterListItem.fliEvaluateConstNode(aNode   : PffFilterNode;
                                                aValue  : PffNodeValue;
                                                aRecBuf : Pointer) : Boolean;
begin
  aValue^.nvType    := aNode^.fnConst.iType;
  aValue^.nvSize    := aNode^.fnConst.iSize;
  aValue^.nvValue   := fliGetLiteralPtr(aNode^.fnConst.ioffset);
  aValue^.nvIsNull  := False;
  aValue^.nvIsConst := True;
  Result := True;
end;
{--------}
function TffFilterListItem.fliEvaluateFieldNode(aNode   : PffFilterNode;
                                                aValue  : PffNodeValue;
                                                aRecBuf : Pointer) : Boolean;
var
  FieldDesc : TffFieldDescItem;
  RecBufAsBytes : PByteArray absolute aRecBuf;
  FilterFldName : PChar;
begin
  TffDataSet(fliOwner).dsGetFieldDescItem(aNode^.fnFIELD.iFieldNum, FieldDesc);

  {get round InfoPower filter bug}
  {the bug is this: the iFieldNum field of the node is supposed to be
   the field number of the field we are interested in (field 1 being
   the first field in the record, 2 the second field); InfoPower's
   filter parsing code sets it to a field count instead, starting at 1
   and incrementing for every field encountered in the filter string.
   We'll patch the filter binary block the first time through since
   GetFieldNumber is relatively slow.}
  FilterFldName := fliGetLiteralPtr(aNode^.fnFIELD.iNameoffset);
  if (FFAnsiStrIComp(FilterFldName, FieldDesc.PhyDesc^.szName) <> 0) then begin {!!.06, !!.07}
    {patch the filter block, so we don't keep on doing this}
    aNode^.fnFIELD.iFieldNum :=
      TffDataSet(fliOwner).dsGetFieldNumber(FilterFldName);
    TffDataSet(fliOwner).dsGetFieldDescItem(aNode^.fnFIELD.iFieldNum, FieldDesc);
  end;

  aValue^.nvType    := FieldDesc.PhyDesc^.iFldType;
  aValue^.nvSize    := FieldDesc.PhyDesc^.iLen;
  aValue^.nvValue   := @RecBufAsBytes^[FieldDesc.PhyDesc^.ioffset];
  aValue^.nvIsConst := False;
  TffDataSet(fliOwner).dsTranslateGet(FieldDesc, aRecBuf, nil, aValue^.nvIsNull);

  Result := True;
end;
{--------}
function TffFilterListItem.fliCompareValues(var aCompareResult : Integer;
                                            var aFirst      : TffNodeValue;
                                            var aSecond     : TffNodeValue;
                                                aIgnoreCase : Boolean;
                                                aPartLen    : Integer): Boolean;
begin
  Result := True;
  {Deal with nulls first, we don't have to ask the table to do it
   since null < any value, except null}
  if aFirst.nvIsNull then
    if aSecond.nvIsNull then begin
      aCompareResult := 0;
      Exit;
    end else begin
      aCompareResult := -1;
      Exit;
    end
  else {aFirst is not null} if aSecond.nvIsNull then begin
    aCompareResult := 1;
    Exit;
  end;
  {Otherwise let the table deal with it since some translation may be
   required}
  aCompareResult := TffDataSet(fliOwner).dsTranslateCmp(aFirst,
                                                        aSecond,
                                                        aIgnoreCase,
                                                        aPartLen);
end;


{===TffBaseClient===================================================}
constructor TffBaseClient.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);

  dbliReqPropName := ffcClientName;
  bcAutoClientName := False;
  bcBeepOnLoginError := True;                                            {!!.06}
  bcOwnServerEngine := False;
  bcServerEngine := nil;
  bcClientID := 0;
  bcPasswordRetries := ffclLoginRetries;
  bcUserName := ffclUserName;
  bcTimeOut := DefaultTimeOut;
  dbliNeedsNoOwner := True;
  {add ourselves to the global comms engine list}
  Clients.AddItem(Self);
  dbliLoadPriority := 1;

  bcOnConnectionLost := IDEConnectionLost;
end;
{--------}
destructor TffBaseClient.Destroy;
begin
  FFNotifyDependents(ffn_Destroy);

  Close;

  if bcOwnServerEngine then begin
    if ServerEngine is TffRemoteServerEngine then
      TffRemoteServerEngine(ServerEngine).Transport.Free;
    ServerEngine.Free;
    ServerEngine := nil;
    bcOwnServerEngine := False;                                        {!!.06}
  end;

  if Assigned(ServerEngine) then
    ServerEngine.FFRemoveDependent(Self);

  {make sure we're no longer the default}
  if IsDefault then
    IsDefault := False;

  {remove ourselves from the global comms engine list}
  if Assigned(Clients) then
    Clients.DeleteItem(Self);

  inherited Destroy;
end;
{--------}
procedure TffBaseClient.IDEConnectionLost(aSource   : TObject;
                            aStarting : Boolean;
                        var aRetry    : Boolean);
resourcestring
  cMsg = 'The connection to the server has been lost. Reconnect?';
begin
  if aStarting then begin
    aRetry := MessageDlg(cMsg, mtError, [mbYes, mbNo], 0) = mrYes
  end else
    if aRetry and (aSource is TffBaseClient) then
      if TffBaseClient(aSource).ClientID <= 0 then begin
        MessageDlg('Reconnect was unsuccessful', mtInformation, [mbOK], 0);
      end;
end;
{Begin !!.06}
{--------}
type
  TffServerCracker = class(TffBaseServerEngine);
{--------}
function TffBaseClient.ProcessRequest(aMsgID           : Longint;
                                      aTimeout         : Longint;
                                      aRequestData     : Pointer;
                                      aRequestDataLen  : Longint;
                                      aRequestDataType : TffNetMsgDataType;
                                  var aReply           : Pointer;
                                  var aReplyLen        : Longint;
                                      aReplyType       : TffNetMsgDataType) : TffResult;
begin
  Result := TffServerCracker(bcServerEngine).ProcessRequest(bcClientID,
                                                            aMsgID,
                                                            aTimeout,
                                                            aRequestData,
                                                            aRequestDataLen,
                                                            aRequestDataType,
                                                            aReply,
                                                            aReplyLen,
                                                            aReplyType);
end;
{--------}
function TffBaseClient.ProcessRequestNoReply(aMsgID          : Longint;
                                             aTimeout        : Longint;
                                             aRequestData    : Pointer;
                                             aRequestDataLen : Longint ) : TffResult;
begin
  Result := TffServerCracker(bcServerEngine).ProcessRequestNoReply(bcClientID,
                                                                   aMsgID,
                                                                   aTimeout,
                                                                   aRequestData,
                                                                   aRequestDataLen);
end;
{End !!.06}
{====================================================================}


{===TffCommsEngine===================================================}
constructor TffCommsEngine.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);

  Protocol := ptSingleUser;
end;
{--------}
function TffBaseClient.bcGetDefaultSession : TffSession;
var
  Inx : Integer;
begin
  for Inx := 0 to pred(OwnedDBItems.Count) do begin
    Result := TffSession(OwnedDBItems[Inx]);
    if Result.IsDefault then
      Exit;
  end;
  if (OwnedDBItems.Count = 0) then
    Result := nil
  else begin
    Result := TffSession(OwnedDBItems[0]);
    Result.scIsDefault := True;
  end;
end;
{--------}
function TffBaseClient.bcGetSession(aInx : Integer) : TffSession;
begin
  Result := TffSession(OwnedDBItems[aInx])
end;
{--------}
function TffBaseClient.bcGetSessionCount : Integer;
begin
  Result := OwnedDBItems.Count;
end;
{--------}
procedure TffBaseClient.bcMakeSessionDefault(aSession : TffSession;
                                             aValue   : Boolean);
var
  Inx  : Integer;
  Sess : TffSession;
  NeedDefault : Boolean;

begin
  Assert(Assigned(aSession));
  if aValue then begin
    for Inx := 0 to pred(OwnedDBItems.Count) do
      TffSession(OwnedDBItems[Inx]).scIsDefault := False;
    aSession.scIsDefault := True
  end else begin
    NeedDefault := aSession.scIsDefault;
    aSession.scIsDefault := False;
    if NeedDefault then begin
      for Inx := 0 to pred(OwnedDBItems.Count) do begin
        Sess := TffSession(OwnedDBItems[Inx]);
        if (aSession <> Sess) then begin
          Sess.scIsDefault := True;
          Exit;
        end;
      end;
      if (OwnedDBItems.Count > 0) then
        TffSession(OwnedDBItems[0]).scIsDefault := True;
    end;
  end;
end;
{--------}
procedure TffBaseClient.bcDoConnectionLost;
var
  Retry : Boolean;
  RetrySuccess : Boolean;
begin
  Retry := False;
  if Assigned(bcOnConnectionLost) then begin
    bcOnConnectionLost(Self, True, Retry);
  end else begin
    if csDesigning in ComponentState then begin
      IDEConnectionLost(Self, True, Retry);
    end else
  end;

  RetrySuccess := False;
  if Retry and dbliActive then begin
    try
      Open;
      RetrySuccess := True;
    except
      { Any exception will cause us to assume the retry was unsuccessful}
    end;
  end;


  { Clear the client's internals manually }
  dbliActive := False;
  bcClientID := 0;

  if RetrySuccess then
    { If retry for client was successful, reinstate all dependents }
    RetrySuccess := bcReinstateDependents;

  if not RetrySuccess then begin
    { If retry was not successful clear all dependents components }
    TffRemoteServerEngine(ServerEngine).Transport.Shutdown;               {!!.06}
    bcClearDependents;
  end;

  if Assigned(bcOnConnectionLost) then
    bcOnConnectionLost(Self, False, Retry)
  else
    if csDesigning in ComponentState then
      IDEConnectionLost(Self, True, Retry);
end;
{--------}
function TffBaseClient.bcReinstateDependents : Boolean;
var
  SessIdx : Integer;
  Sess    : TffSession;

  DBIdx   : Integer;
  OwnedCmp : TffComponent;                                             {!!.12}
  DB      : TffBaseDatabase;

  DSIdx   : Integer;
  DS      : TffDataSet;

  WasActive : Boolean;
  WasPrepared : Boolean;
begin
  Result := False;
  try
    for SessIdx := 0 to Pred(SessionCount) do begin
      Sess := Sessions[SessIdx];
      WasActive := Sess.dbliActive;
      Sess.dbliActive := False;
      Sess.scSessionID := 0;
      Sess.scServerEngine := nil;
      if WasActive then
        Sess.Open;

      for DBIdx := 0 to Pred(Sess.OwnedDBItems.Count) do begin         {!!.12}
        OwnedCmp := Sess.OwnedDBItems[DBIdx];                          {!!.12}
        if OwnedCmp is TffBasePluginEngine then begin                  {!!.12}
          TffBasePluginEngine(OwnedCmp).Shutdown;                      {!!.12}
          TffBasePluginEngine(OwnedCmp).Startup;                       {!!.12}
        end                                                            {!!.12}
        else if OwnedCmp is TffBaseDatabase then begin                 {!!.12}
          DB := Sess.Databases[DBIdx];
          WasActive := DB.dbliActive;
          DB.dbliActive := False;
          DB.bdDatabaseID := 0;
          DB.bdServerEngine := nil;
          if WasActive then
            DB.Open;

          for DSIdx := 0 to Pred(DB.DataSetCount) do begin
            DS := DB.DataSets[DSIdx];
            WasActive := DS.dsProxy.dbliActive;
            WasPrepared := False;
            DS.dsProxy.dbliActive := False;
            DS.dsProxy.tpServerEngine := nil;
            DS.TableState := TblClosed;
            DS.dsCursorID := 0;
            DS.Close;
            if DS is TffBaseTable then
              with TffBaseTable(DS) do begin
                btLookupCursorID := 0;
                btLookupKeyFields := '';
                btLookupNoCase := False;
                btRangeStack.Clear;
              end
            else if DS is TffQuery then
              with TffQuery(DS) do begin
                WasPrepared := FPrepared;
                FPrepared := False;
                FStmtID := 0;
              end;
{Begin !!.13}
            if (DS is TffQuery) and
               (WasPrepared) then
              TffQuery(DS).Prepare;
            if WasActive then
              DS.Open;
{End !!.13}
          end;  { for }
        end;  { if }
      end;  { if }                                                     {!!.12}
    end;
    Result := True;
  except
    { if any exceptions occur, we assume that the connection cannot be reinstated }
  end;
end;
{--------}
procedure TffBaseClient.bcClearDependents;
var
  SessIdx : Integer;
  Sess    : TffSession;

  DBIdx   : Integer;
  OwnedCmp : TffComponent;                                             {!!.12}
  DB      : TffBaseDatabase;

  DSIdx   : Integer;
  DS      : TffDataSet;
begin
  for SessIdx := 0 to Pred(SessionCount) do begin
    Sess := Sessions[SessIdx];
    Sess.dbliActive := False;
    Sess.scSessionID := 0;
    Sess.scServerEngine := nil;

    for DBIdx := 0 to Pred(Sess.OwnedDBItems.Count) do begin           {!!.12}
      OwnedCmp := Sess.OwnedDBItems[DBIdx];                            {!!.12}
      if OwnedCmp is TffBasePluginEngine then                          {!!.12}
        TffBasePluginEngine(OwnedCmp).Shutdown                         {!!.12}
      else if OwnedCmp is TffBaseDatabase then begin                   {!!.12}
        DB := Sess.Databases[DBIdx];
        DB.dbliActive := False;
        DB.bdDatabaseID := 0;
        DB.bdServerEngine := nil;

        for DSIdx := 0 to Pred(DB.DataSetCount) do begin
          DS := DB.DataSets[DSIdx];
          if DS is TffBaseTable then                                   {!!.06}
            TffBaseTable(DS).btIgnoreDataEvents := True;               {!!.06}
          DS.dsProxy.dbliActive := False;
          DS.dsProxy.tpServerEngine := nil;
          DS.TableState := TblClosed;
          DS.dsCursorID := 0;
          DS.Close;
          if DS is TffBaseTable then
            with TffBaseTable(DS) do begin
              btLookupCursorID := 0;
              btLookupKeyFields := '';
              btLookupNoCase := False;
              btRangeStack.Clear;
            end
          else if DS is TffQuery then
            with TffQuery(DS) do begin
              FStmtID := 0;
            end;
        end;  { for }
      end;  { if }                                                     {!!.12}
    end;
  end;
end;
{--------}
procedure TffBaseClient.FFNotificationEx(const AOp : Byte; AFrom : TffComponent;
                                         const AData : TffWord32);
begin
  if (AFrom = bcServerEngine) then
    if ((AOp = ffn_Destroy) or (AOp = ffn_Remove) ) then begin
      FFNotifyDependents(ffn_Deactivate);
      Close;
      bcServerEngine := nil;
    end else if (AOp = ffn_Deactivate) then begin
      FFNotifyDependents(ffn_Deactivate);
      Close;
    end else if (AOp = ffn_ConnectionLost) then begin
      if (Active) and (bcClientID = AData) then begin
        bcDoConnectionLost;
      end;
    end;
end;
{--------}
procedure TffCommsEngine.ceReadRegistryProtocol;
var
  ProtName : TffShStr;
begin
  if not ceRegProtRead then begin
    ffClientConfigReadProtocol(ceRegProt, ProtName);
    ceRegProtRead := True;
  end;
end;
{--------}
function TffBaseClient.bcGetServerEngine : TffBaseServerEngine;
begin
  Result := bcServerEngine;
end;
{--------}
procedure TffBaseClient.bcSetAutoClientName(const Value : Boolean);
begin
  if Value = bcAutoClientName then
    Exit;

  if Value then begin
    CheckInactive(False);
    ClientName := 'FFClient_' + IntToStr(Longint(Self));
  end;
  
  bcAutoClientName := Value;
end;
{--------}
procedure TffBaseClient.bcSetClientName(const aName : string);
{Rewritten !!.11}
var
  CL : TffBaseClient;
  Counter : Integer;
  TmpName : string;
begin
  if DBName = aName then
    Exit;

  CheckInactive(False);
  TmpName := aName;
  CL := FindFFClientName(TmpName);
  if (CL <> nil) then
    if bcAutoClientName then begin
      { Generate a unique name. }
      Counter := 0;
      repeat
        TmpName := aName + IntToStr(Counter);
        inc(Counter);
      until FindFFClientName(TmpName) = nil;
    end
    else
      { Allow case changes to existing name }
      if not (AnsiUpperCase(TmpName) = AnsiUpperCase(DBName)) then
        raise EffDatabaseError.Create(
          Format(ffStrResDataSet[ffdse_CLNameExists], [TmpName]));
  DBName := TmpName;
end;
{--------}
procedure TffBaseClient.bcSetIsDefault(const Value : Boolean);
var
  CurDefCL   : TffBaseClient;
  CurDefSess : TffSession;
begin
  if (Value = bcIsDefault) then
    Exit;
    
  if Value then begin {making it the default}
    {find the current default engine, and make sure it's no longer
     the default}
    CurDefCL := FindDefaultFFClient;
    if Assigned(CurDefCL) then
      CurDefCL.bcIsDefault := False;
    {we're now the default}
    bcIsDefault := True;
    {make sure we have a default session}
    if (OwnedDBItems.Count > 0) then begin
      CurDefSess := bcGetDefaultSession;
      if (CurDefSess = nil) then
        bcMakeSessionDefault(TffSession(OwnedDBItems[0]), True);
    end;
  end else {it's no longer the default} begin
    {we're no longer the default}
    bcIsDefault := False;
    {make the automatically created engine the default}
    CurDefCL := FindAutoFFClient;
    if Assigned(CurDefCL) then
      CurDefCL.IsDefault := True;
  end;
end;
{--------}
procedure TffCommsEngine.ceSetProtocol(const Value : TffProtocolType);
begin
  CheckInactive(csDesigning in ComponentState);
  ceProtocol := Value;
end;
{--------}
function TffCommsEngine.ceGetServerName : string;                      {!!.10}
begin
  Result := ceServerName;
end;
{--------}
procedure TffCommsEngine.ceSetServerName(const Value : string);        {!!.10}
begin
  CheckInactive(False);
  ceServerName := Value;
end;
{--------}
procedure TffBaseClient.bcSetUserName(const Value : string);
begin
  CheckInactive(False);
  bcUserName := Value;
end;
{--------}
function TffBaseClient.bcGetUserName : string;
begin
  Result := bcUserName;
end;
{--------}
procedure TffBaseClient.bcSetServerEngine(Value : TffBaseServerEngine);
begin
  if bcServerEngine = Value then
    Exit;

  CheckInactive(False);

{Begin !!.02}
  if Assigned(bcServerEngine) then begin
    bcServerEngine.FFRemoveDependent(Self);
    if bcOwnServerEngine then begin
      if ServerEngine is TffRemoteServerEngine then
        TffRemoteServerEngine(ServerEngine).Transport.Free;
      bcServerEngine.Free;
      bcOwnServerEngine := False;                                      {!!.06}
    end;
  end;
{End !!.02}

  bcServerEngine := Value;
  if Assigned(bcServerEngine) then
    bcServerEngine.FFAddDependent(Self);
end;
{--------}
procedure TffBaseClient.bcSetTimeout(const Value : Longint);
var
  Idx : Integer;                                                         {!!.11}
begin
  if bcTimeout = Value then
    Exit;

  bcTimeout := Value;
  if bcClientID <> 0 then
    if Assigned(ServerEngine) then begin
      Check(ServerEngine.ClientSetTimeout(bcClientID, Value));
      { Inform children of timeout change }
      for Idx := 0 to Pred(OwnedDBItems.Count) do
        TffSession(OwnedDBItems[Idx]).scRefreshTimeout;
    end;
end;
{--------}
procedure TffBaseClient.dbliClosePrim;
begin
  inherited dbliClosePrim;

  if bcClientID <> 0 then
    if Assigned(ServerEngine) then begin
      Check(ServerEngine.ClientRemove(bcClientID));
      if bcOwnServerEngine and (ServerEngine is TffRemoteServerEngine) then
        TffRemoteServerEngine(ServerEngine).Transport.State := ffesInactive;
    end;
   bcClientID := 0;
end;
{--------}
function TffBaseClient.dbliCreateOwnedList : TffDBList;
begin
  Result := TffDBList(TffSessionList.Create(Self));
end;
{--------}
procedure TffBaseClient.dbliDBItemAdded(aItem : TffDBListItem);
var
  Sess : TffSession absolute aItem;
begin
  Assert(Assigned(aItem));
  if (OwnedDBItems.Count = 1) then
    Sess.scIsDefault := True;
end;
{--------}
procedure TffBaseClient.dbliDBItemDeleted(aItem : TffDBListItem);
var
  Sess : TffSession absolute aItem;
begin
  Assert(Assigned(aItem));
  if Sess.scIsDefault then
    bcMakeSessionDefault(Sess, False);
end;
{--------}
procedure TffBaseClient.dbliMustBeClosedError;
begin
  RaiseFFErrorObj(Self, ffdse_CLMustBeClosed);
end;
{--------}
procedure TffBaseClient.dbliMustBeOpenError;
begin
  RaiseFFErrorObj(Self, ffdse_CLMustBeOpen);
end;
{--------}
procedure TffBaseClient.GetServerNames(aServerNames : TStrings);
{$IFNDEF SingleEXE}                                                    {Moved !!.02}
var                                                                    {Begin !!.01}
  Prot     : TffCommsProtocolClass;
  ProtName : TffShStr;
  RSE      : TffRemoteServerEngine;  { for convenient access}
  LTrans   : TffBaseTransport;       { for convenient access}          {Moved !!.02}
{$ENDIF}
                                                                       {End !!.01}
begin
  Assert(Assigned(aServerNames));
  CheckActive;
  if IsConnected then begin                                            {Begin !!.01}
    Assert(Assigned(ServerEngine));
    ServerEngine.GetServerNames(aServerNames, bcTimeout);
  end else begin
    if Assigned(ServerEngine) then
      ServerEngine.GetServerNames(aServerNames, bcTimeout)
    else begin
      { Since no ServerEngine is available we must create one here to
        retrieve the server names. }
      {$IFDEF SingleEXE}
      aServerNames.Add('Local server');
      {$ELSE}

      {Get the protocol from the registry}
      FFClientConfigReadProtocol(Prot, ProtName);

      { We must create our own remote server engine, transport, etc. }
      RSE := TffRemoteServerEngine.Create(Self);
      try
        RSE.TimeOut := Timeout;
        LTrans := TffLegacyTransport.Create(RSE);
        try
          LTrans.Mode := fftmSend;
          TffLegacyTransport(LTrans).Protocol := FFGetProtocolType(ProtName);
          LTrans.ServerName := FFClientConfigReadServerName;
          RSE.Transport := LTrans;

          { Get the list }
          RSE.GetServerNames(aServerNames, bcTimeout);

        finally
          LTrans.Free;
        end;
      finally
        RSE.Free;
      end;
      {$ENDIF}
    end;
  end;                                                                 {End !!.01}
end;
{--------}
function TffCommsEngine.ProtocolClass : TffCommsProtocolClass;
begin
  if (Protocol <> ptRegistry) then
    case Protocol of
      ptSingleUser : Result := TffSingleUserProtocol;
      ptTCPIP      : Result := TffTCPIPProtocol;
      ptIPXSPX     : Result := TffIPXSPXProtocol;
    else
      Result := TffSingleUserProtocol;
    end
  else begin
    ceReadRegistryProtocol;
    Result := ceRegProt;
  end;
end;
{--------}
function TffBaseClient.IsConnected : Boolean;
begin
  Result := ClientID <> 0;
end;
{--------}
procedure TffClient.OpenConnection(aSession : TffSession);
var
  aUserName    : TffName;
  aPassword    : TffName;
  aPWHash      : TffWord32;
  aServerPWHash: TffWord32;
  aClickedOK   : Boolean;
  {$IFNDEF SingleEXE}
  aProt        : TffCommsProtocolClass;
  aProtName    : TffShStr;
  aRSE         : TffRemoteServerEngine;  { for convenient access}
  {$ENDIF}
  aLTrans      : TffBaseTransport;       { for convenient access}
  aServerName  : TffNetAddress;
  aStatus      : TffResult;
  aRetryCount  : Integer;
begin
  Assert(Assigned(aSession));

  { Each time a session is made active, this method will be called. Since
    we may serve multiple sessions, we must check to see if we are already
    connected to a server }
  if IsConnected then
    Exit;

  if (bcServerEngine = nil) then begin
    {$IFDEF SingleEXE}
      if (FFDB.ServerEngine = nil) then
        FFDB.ServerEngine := TffServerEngine.Create(nil);
      bcServerEngine := FFDB.ServerEngine;
      bcServerEngine.FFAddDependent(Self);                             {!!.01}
    {$ELSE}
      {Get the protocol from the registry}
      FFClientConfigReadProtocol(aProt, aProtName);

      { We must create our own remote server engine, transport, etc. }
      aRSE := TffRemoteServerEngine.Create(Self);
      bcOwnServerEngine := True;
      aRSE.TimeOut := Timeout;
      aLTrans := TffLegacyTransport.Create(aRSE);
{Begin !!.01}
      {$IFDEF AutoLog}
      aLTrans.EventLog := TffEventLog.Create(aLTrans);
      aLTrans.EventLog.Enabled := True;
      aLTrans.EventLog.FileName := ffcAutoLogFile;
      aLTrans.EventLogEnabled := True;
      aLTrans.EventLogOptions := [fftpLogErrors, fftpLogRequests, fftpLogReplies];
      {$ENDIF}
      aLTrans.Mode := fftmSend;
      TffLegacyTransport(aLTrans).Protocol := FFGetProtocolType(aProtName);
      aLTrans.ServerName := FFClientConfigReadServerName;
      {$IFDEF AutoLog}
      aLTrans.EventLog.WriteStringFmt('Automatic transport serverName: %s',
                                     [aLTrans.ServerName]);
      {$ENDIF}
{End !!.01}
      aRSE.Transport := aLTrans;
      bcServerEngine := aRSE;
      bcServerEngine.FFAddDependent(Self);                             {!!.01}
    {$ENDIF}
    end;

  if Assigned(bcServerEngine) then begin
    { Let the server engine know we are here. }
    if ServerEngine is TffRemoteServerEngine then begin
      aLTrans := TffRemoteServerEngine(ServerEngine).Transport;
      if Assigned(aLTrans) then begin
        if aLTrans.State = ffesInactive then begin                     {!!.05}
          aLTrans.Enabled := True;
          { Select the appropriate server if necessary }
          if (aLTrans is TffLegacyTransport) then                      {!!.13}
            if TffLegacyTransport(aLTrans).Protocol = ptRegistry then  {!!.13}
              aLTrans.ServerName := FFClientConfigReadServerName;      {!!.13}
          if aLTrans.ServerName = '' then begin
            aSession.ChooseServer(aServerName);
            if aServerName = '' then
              Check(DBIERR_SERVERNOTFOUND);
            aLTrans.ServerName := aServerName;
          end;
          aLTrans.State := ffesStarted;
        end;
      end else begin                                                  {!!.05}
        Check(ffdse_RSENeedsTransport)                                {!!.05}
      end;                                                            {!!.05}
    end;
    if ServerEngine.State in [ffesInactive, ffesStopped] then
      ServerEngine.State := ffesStarted;
    aRetryCount := 0;
    if bcUserName <> '' then
      aUserName := bcUserName
    else
      aUserName := ffclUserName;
    aPassword := ffclPassword;
    if (csDesigning in ComponentState) and (bcPassword <> '') then
      aPassword := bcPassword;                                        {!!.06}
    aPWHash := FFCalcShStrELFHash(aPassword);
    aServerPWHash := aPWHash;                                         {!!.06}
    aStatus := ServerEngine.ClientAdd(bcClientID, aUserName, aUserName, bcTimeOut, aServerPWHash);
    { Make sure the password was correct }
    if aStatus = DBIERR_NONE then                                     {!!.06}
      if aPWHash <> aServerPWHash then                                {!!.06}
        aStatus := DBIERR_INVALIDUSRPASS;                             {!!.06}
    while (aRetryCount < bcPasswordRetries) and
          (aStatus = DBIERR_INVALIDUSRPASS) do begin
      if bcBeepOnLoginError then                                      {!!.06}
        MessageBeep(0);

      aSession.DoLogin(aUserName, aPassword, aClickedOK);
      if not aClickedOK then
        Break
      else begin
        inc(aRetryCount);
        aPWHash := FFCalcShStrELFHash(aPassword);
        aServerPWHash := aPWHash;                                     {!!.06}
        aStatus := ServerEngine.ClientAdd(bcClientID, aUserName, aUserName, bcTimeout, aPWHash);

       { Make sure the password was correct }
        if aStatus = DBIERR_NONE then                                 {!!.06}
          if aPWHash <> aServerPWHash then                            {!!.06}
            aStatus := DBIERR_INVALIDUSRPASS;                         {!!.06}
        if aStatus = fferrReplyTimeout then                           {!!.06}
          aStatus := DBIERR_INVALIDUSRPASS;                           {!!.06}
      end;
    end;
    Check(aStatus);
    { store login in the client component}

    bcUserName := aUserName;                                          {!!.06}
    if csDesigning in ComponentState  then
      bcPassword := aPassword;                                        {!!.06}
  end else begin
    { There is no ServerEngine, so raise an exception }
    Check(DBIERR_FF_OpenNoMem)
  end;
end;
{--------}                                                         {!!BEGIN .01}
procedure TffCommsEngine.GetServerNames(aServerNames : TStrings);
{$IFNDEF SingleEXE}                                                    {Moved !!.02}
var
  Prot     : TffCommsProtocolClass;
  ProtName : TffShStr;
  RSE      : TffRemoteServerEngine;  { for convenient access}
  LTrans   : TffBaseTransport;       { for convenient access}          {Moved !!.02}
{$ENDIF}
begin
  Assert(Assigned(aServerNames));
  CheckActive;
  if IsConnected then begin
    Assert(Assigned(ServerEngine));
    ServerEngine.GetServerNames(aServerNames, bcTimeout);
  end else begin
    if Assigned(ServerEngine) then
      ServerEngine.GetServerNames(aServerNames, bcTimeout)
    else begin
      { Since no ServerEngine is available we must create one here to
        retrieve the server names. }
      {$IFDEF SingleEXE}
      aServerNames.Add('Local server');
      {$ELSE}

      LTrans := nil;
      RSE := TffRemoteServerEngine.Create(nil);
      try
        LTrans := TffLegacyTransport.Create(nil);
        RSE.TimeOut := Timeout;
        LTrans.Mode := fftmSend;
        RSE.Transport := LTrans;
        if (Protocol = ptRegistry) then begin
          {Get the protocol from the registry}
          FFClientConfigReadProtocol(Prot, ProtName);
          TffLegacyTransport(LTrans).Protocol := FFGetProtocolType(ProtName);
          LTrans.ServerName := FFClientConfigReadServerName;
        end else begin
          TffLegacyTransport(LTrans).Protocol := Protocol;
          LTrans.ServerName := ServerName;
        end;
        { Get the list }
        RSE.GetServerNames(aServerNames, bcTimeout);
      finally
        LTrans.Free;
        RSE.Free;
      end;
      {$ENDIF}
    end;
  end;
end;                                                                 {!!END .01}
{--------}
procedure TffCommsEngine.OpenConnection(aSession : TffSession);
var
  aUserName   : TffName;
  aPassword   : TffName;
  aPWHash     : TffWord32;
  aServerPWHash : TFfWord32;
  aClickedOK  : Boolean;
  {$IFNDEF SingleEXE}
  aProt       : TffCommsProtocolClass;
  aProtName   : TffShStr;
  aRSE        : TffRemoteServerEngine;  { for convenient access}
  {$ENDIF}
  aLTrans     : TffBaseTransport;       { for convenient access}
  aServerName : TffNetAddress;
  aRetryCount : Integer;
  aStatus     : TffResult;
begin
  Assert(Assigned(aSession));

  if IsConnected then
    Exit;

  {$IFDEF SingleEXE}
    if (FFDB.ServerEngine = nil) then
      FFDB.ServerEngine := TffServerEngine.Create(nil);
    bcServerEngine := FFDB.ServerEngine;
    bcServerEngine.FFAddDependent(Self);                               {!!.01}
  {$ELSE}

  if (Protocol = ptRegistry) then begin
    {Get the protocol from the registry}
    FFClientConfigReadProtocol(aProt, aProtName);

    { We must create our own remote server engine, transport, etc. }
    aRSE := TffRemoteServerEngine.Create(Self);
    bcOwnServerEngine := True;
    aRSE.TimeOut := Timeout;
    aLTrans := TffLegacyTransport.Create(aRSE);
{Begin !!.01}
    {$IFDEF AutoLog}
    aLTrans.EventLog := TffEventLog.Create(aLTrans);
    aLTrans.EventLog.Enabled := True;
    aLTrans.EventLog.FileName := ffcAutoLogFile;
    aLTrans.EventLogEnabled := True;
    aLTrans.EventLogOptions := [fftpLogErrors, fftpLogRequests, fftpLogReplies];
    {$ENDIF}
    aLTrans.Mode := fftmSend;
    TffLegacyTransport(aLTrans).Protocol := FFGetProtocolType(aProtName);
    aLTrans.ServerName := FFClientConfigReadServerName;
    {$IFDEF AutoLog}
    aLTrans.EventLog.WriteStringFmt('Automatic CommsEngine serverName: %s',
                                    [aLTrans.ServerName]);
    {$ENDIF}
{End !!.01}
    aRSE.Transport := aLTrans;
    bcServerEngine := aRSE;
    bcServerEngine.FFAddDependent(Self);                               {!!.01}
  end else if not Assigned(ServerEngine) then begin
    { The server engine property is not Assigned, so we need to create one }
    { We must create our own remote server engine, transport, etc. }
    aRSE := TffRemoteServerEngine.Create(Self);
    bcOwnServerEngine := True;
    aRSE.TimeOut := Timeout;
    aLTrans := TffLegacyTransport.Create(aRSE);
{Begin !!.01}
    {$IFDEF AutoLog}
    aLTrans.EventLog := TffEventLog.Create(aLTrans);
    aLTrans.EventLog.Enabled := True;
    aLTrans.EventLog.FileName := ffcAutoLogFile;
    aLTrans.EventLogEnabled := True;
    aLTrans.EventLogOptions := [fftpLogErrors, fftpLogRequests, fftpLogReplies];
    {$ENDIF}
    aLTrans.Mode := fftmSend;
    TffLegacyTransport(aLTrans).Protocol := Protocol;
    aLTrans.ServerName := ServerName;
    {$IFDEF AutoLog}
    aLTrans.EventLog.WriteStringFmt('Automatic CommsEngine serverName: %s',
                                    [aLTrans.ServerName]);
    {$ENDIF}
{End !!.01}
    aRSE.Transport := aLTrans;
    bcServerEngine := aRSE;
    bcServerEngine.FFAddDependent(Self);                               {!!.01}
  end;
  {$ENDIF}
  if Assigned(ServerEngine) then begin
    { Let the server engine know we are here. }
    if ServerEngine is TffRemoteServerEngine then begin
      aLTrans := TffRemoteServerEngine(ServerEngine).Transport;
      if Assigned(aLTrans) then begin                                  {!!.05}
        aLTrans.Enabled := True;
        { Select the appropriate server if necessary }
        if (aLTrans is TffLegacyTransport) then                        {!!.13}
          if TffLegacyTransport(aLTrans).Protocol = ptRegistry then    {!!.13}
            aLTrans.ServerName := FFClientConfigReadServerName;        {!!.13}
        if aLTrans.ServerName = '' then begin
          aSession.ChooseServer(aServerName);
          if aServerName = '' then
            Check(DBIERR_SERVERNOTFOUND);
          aLTrans.ServerName := aServerName;
        end;
        aLTrans.State := ffesStarted;
      end else begin                                                  {!!.05}
        Check(ffdse_RSENeedsTransport);                               {!!.05}
      end;                                                            {!!.05}
    end;
    ServerEngine.State := ffesStarted;

    aRetryCount := 0;
    if bcUserName <> '' then
      aUserName := bcUserName
    else
      aUserName := ffclUserName;
    aPassword := ffclPassword;
    if (csDesigning in ComponentState) and (bcPassword <> '') then
      aPassword := bcPassword;                                        {!!.06}
    aPWHash := FFCalcShStrELFHash(aPassword);
    aServerPWHash := aPWHash;
    aStatus := ServerEngine.ClientAdd(bcClientID, aUserName, aUserName,
                                     bcTimeOut, aPWHash);
    { Make sure the password was correct }
    if aStatus = DBIERR_NONE then                                     {!!.06}
      if aPWHash <> aServerPWHash then                                {!!.06}
        aStatus := DBIERR_INVALIDUSRPASS;                             {!!.06}
    while (aRetryCount < bcPasswordRetries) and
          (aStatus = DBIERR_INVALIDUSRPASS) do begin
      if aRetryCount > 0 then
        if bcBeepOnLoginError then                                    {!!.06}
          MessageBeep(0);

      aSession.DoLogin(aUserName, aPassword, aClickedOK);
      if not aClickedOK then
        Break
      else begin
        inc(aRetryCount);
        aPWHash := FFCalcShStrELFHash(aPassword);
        aServerPWHash := aPWHash;                                     {!!.06}
        aStatus := ServerEngine.ClientAdd(bcClientID, aUserName, aUserName,
                                         bcTimeout, aPWHash);

       { Make sure the password was correct }
        if aStatus = DBIERR_NONE then                                 {!!.06}
          if aPWHash <> aServerPWHash then                            {!!.06}
            aStatus := DBIERR_INVALIDUSRPASS;                         {!!.06}
        if aStatus = fferrReplyTimeout then                           {!!.06}
          aStatus := DBIERR_INVALIDUSRPASS;                           {!!.06}
      end;
    end;  { while }
    Check(aStatus);
    { store user name in the client component}
    bcUserName := aUserName;                                          {!!.06}
    if csDesigning in ComponentState  then
      bcPassword := aPassword;                                        {!!.06}
  end else begin
    { There is no ServerEngine, so raise an exception }
    Check(DBIERR_FF_OpenNoMem)
  end;
end;
{====================================================================}


{===TffCommsEngineList===============================================}
function TffClientList.clGetItem(aInx : Integer) : TffBaseClient;
begin
  Result := TffBaseClient(dblGetItem(aInx));
end;
{====================================================================}


{===TffSession=======================================================}
constructor TffSession.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);

  dbliReqPropName := ffcSessionName;
  scAutoSessionName := False;
  scSessionID := 0;
  scTimeout := -1;
  scServerEngine := nil;

  {attach ourselves to the default comms engine}
  ClientName := GetDefaultffClient.ClientName;
  dbliLoadPriority := 2;

end;
{--------}
destructor TffSession.Destroy;
begin
  dbliFreeTemporaryDependents;                                         {!!.01}
  FFNotifyDependents(ffn_Destroy);

  Close;                                                               {!!.01}

  {make sure we're no longer the default session}
  if IsDefault then
    IsDefault := False;
  {if we're still the default, make sure our comms engine is no longer
   the default}
  if IsDefault and (Client <> nil) then begin
    if Client.IsDefault then
      Client.IsDefault := False;
    if IsDefault then
      IsDefault := False;
  end;

  inherited Destroy;
end;
{--------}
procedure TffSession.AddAlias(const aName       : string;
                              const aPath       : string;
                                    aCheckSpace : Boolean);            {!!.11}
begin
  Check(AddAliasEx(aName, aPath, aCheckSpace));                        {!!.11}
end;
{--------}
function TffSession.AddAliasEx(const aName       : string;
                               const aPath       : string;
                                     aCheckSpace : Boolean)            {!!.11}
                                                 : TffResult;
begin
  Assert(aName <> '');
  Assert(aPath <> '');
  CheckActive;
  Result := ServerEngine.DatabaseAddAlias(aName,
                                          aPath,
                                          aCheckSpace,                 {!!.11}
                                          Client.ClientID);
end;
{--------}
procedure TffSession.CloseDatabase(aDatabase : TffBaseDatabase);
begin
  if (aDatabase <> nil) then begin
    aDatabase.Active := False; {decrement open reference count}
    if (not aDatabase.Active) and aDatabase.Temporary then
      aDatabase.Free;
  end;
end;
{Begin !!.06}
{--------}
procedure TffSession.CloseInactiveTables;
begin
  CheckActive;
  Check(ServerEngine.SessionCloseInactiveTables(Client.ClientID));     {!!.06}
end;
{End !!.06}
{--------}
procedure TffSession.dbliClosePrim;
begin
  inherited dbliClosePrim;

  if scSessionID <> 0 then
    if Assigned(ServerEngine) then
      Check(ServerEngine.SessionRemove(Client.ClientID, SessionID));
  scSessionID := 0;
  scServerEngine := nil;
end;
{--------}
function TffSession.dbliCreateOwnedList : TffDBList;
begin
  Result := TffDBList(TffDatabaseList.Create(Self));
end;
{--------}
function TffSession.dbliFindDBOwner(const aName : string) : TffDBListItem;
begin
  if (aName = '') then
    Result := FindDefaultFFClient
  else
    Result := FindFFClientName(aName);
end;
{--------}
procedure TffSession.dbliMustBeClosedError;
begin
  RaiseFFErrorObj(Self, ffdse_SessMustBeClosed);
end;
{--------}
procedure TffSession.dbliMustBeOpenError;
begin
  RaiseFFErrorObj(Self, ffdse_SessMustBeOpen);
end;
{--------}
procedure TffSession.dbliOpenPrim;
begin
  scServerEngine := Client.ServerEngine;
  DoStartup;
  Assert(Assigned(ServerEngine), 'ServerEngine has not been Assigned');
  {The TfffServerEngine creates a default session for every client. If there
   is not a session already in the client list, then we must create another one.}
  if Client.SessionCount = 0 then
    Check(ServerEngine.SessionGetCurrent(Client.ClientID, scSessionID))
  else
    Check(ServerEngine.SessionAdd(Client.bcClientID, GetTimeOut,
                                 scSessionID));
end;
{--------}
procedure TffSession.DeleteAlias(const aName : string);
begin
  Check(DeleteAliasEx(aName));
end;
{--------}
function TffSession.DeleteAliasEx(const aName : string) : TffResult;
begin
  Assert(aName <> '');
  CheckActive;
  Result := ServerEngine.DatabaseDeleteAlias(aName,
                                             Client.ClientID);
end;
{--------}
function TffSession.FindDatabase(const aName : string) : TffBaseDatabase;
begin
  Result := FindFFDatabaseName(Self, aName, False);
end;
{--------}
procedure TffSession.GetAliasNames(aList : TStrings);
begin
  GetAliasNamesEx(aList, True);
end;
{--------}
function TffSession.GetAliasNamesEx(aList : TStrings;
                              const aEmptyList : Boolean) : TffResult;
var
  WasActive   : Boolean;
  CEWasActive : Boolean;
  TmpList     : TList;
  I           : Integer;
  PItem       : PffAliasDescriptor;
begin
  Assert(Assigned(aList));
  if aEmptyList then
    aList.Clear;
  CEWasActive := Client.Active;
  WasActive := Active;
  if not WasActive then
    Active := True;
  try
    TmpList := TList.Create;
    try
      aList.BeginUpdate;
      try
        Result := ServerEngine.DatabaseAliasList(TmpList, Client.ClientID);
        if Result = DBIERR_NONE then
          for I := 0 to Pred(TmpList.Count) do begin
            PItem := PffAliasDescriptor(TmpList.Items[i]);
            if (aList.IndexOf(PItem^.adAlias) = -1) then             {New !!.01}
              aList.Add(PItem^.adAlias);
          end;
      finally
        aList.EndUpdate;
      end;
    finally
      for I := Pred(TmpList.Count) downto 0 do begin
        PItem := PffAliasDescriptor(TmpList.Items[i]);
        FFFreeMem(PItem, SizeOf(PItem^));
      end;
      TmpList.Free;
    end;
  finally
    if not WasActive then
      Active := False;
    if not CEWasActive then
      Client.Active := False;
  end;{try..finally}
end;
{--------}
procedure TffSession.GetAliasPath(const aName : string;
                                    var aPath : string);
                                                               {rewritten !!.11}
var
  ffPath : TffPath;
  WasActive : Boolean;
  CEWasActive : Boolean;
begin
  Assert(aName <> '');
  if not IsAlias(aName) then
    aPath := ''
  else begin
    WasActive := Active;
    CEWasActive := Client.Active;
    try
      if not WasActive then
        Open;
      Check(ServerEngine.DatabaseGetAliasPath(AName,
                                            ffPath,
                                            Client.ClientID));
      aPath := ffPath;
    finally
      if not WasActive then
        Close;
      if not CEWasActive then
        Client.Close;
    end;
  end;
end;
{--------}
procedure TffSession.GetDatabaseNames(aList : TStrings);
begin
  GetFFDatabaseNames(Self, aList);
end;
{--------}
function TffSession.GetServerDateTime(var aServerNow : TDateTime) : TffResult;
begin
  Result := ServerEngine.GetServerDateTime(aServerNow);

  if Result <> DBIERR_NONE then
    {Just is case something bad happened to aServerNow, we will reset it
     to the local machines date time}
    aServerNow := Now;
end;
{--------}                                                     {begin !!.07}
function TffSession.GetServerSystemTime(var aServerNow : TSystemTime) : TffResult;
begin
  Result := ServerEngine.GetServerSystemTime(aServerNow);
end;
{--------}
function TffSession.GetServerGUID(var aGUID : TGUID) : TffResult;
begin
  Result := ServerEngine.GetServerGUID(aGUID);
end;
{--------}
function TffSession.GetServerID(var aUniqueID : TGUID) : TffResult;
begin
  Result := ServerEngine.GetServerID(aUniqueID);
end;
{--------}
function TffSession.GetServerStatistics(var aStats : TffServerStatistics) : TffResult;
begin
  Result := ServerEngine.GetServerStatistics(aStats);
end;
{--------}
function TffSession.GetCommandHandlerStatistics(const aCmdHandlerIdx : Integer;
                                                  var aStats : TffCommandHandlerStatistics) : TffResult;
begin
  Result := ServerEngine.GetCommandHandlerStatistics(aCmdHandlerIdx, aStats);
end;
{--------}
function TffSession.GetTransportStatistics(const aCmdHandlerIdx : Integer;
                                           const aTransportIdx  : Integer;
                                             var aStats : TffTransportStatistics) : TffResult;
begin
  Result := ServerEngine.GetTransportStatistics(aCmdHandlerIdx, aTransportIdx, aStats);
end;
{--------}                                                       {end !!.07}
procedure TffSession.GetTableNames(const aDatabaseName : string;
                                   const aPattern      : string;
                                         aExtensions   : Boolean;
                                         aSystemTables : Boolean;
                                         aList         : TStrings);
var
  DB        : TffBaseDatabase;
  TmpList   : TList;
  I         : Integer;
  PItem     : PffTableDescriptor;
  WasActive : Boolean;                                                 {!!.01}
begin
  Assert(Assigned(aList));
  aList.BeginUpdate;
  try
    aList.Clear;
    if (aDatabaseName <> '') then begin
      DB := FindFFDatabaseName(Self, aDatabaseName, True);             {!!.01}
      if Assigned(DB) then begin                                        {!!.01}
        WasActive := DB.Active;                                         {!!.01}
        DB.Active := True;                                              {!!.01}
        try
          TmpList := TList.Create;
          try
            Check(ServerEngine.DatabaseTableList(DB.DatabaseID,
                                                 PChar(aPattern),
                                                 TmpList));
            for I := 0 to Pred(TmpList.Count) do begin
              PItem := PffTableDescriptor(TmpList.Items[I]);
              if aExtensions then
                aList.Add(PItem^.tdTableName + '.' + PItem^.tdExt)
              else
                aList.Add(PItem^.tdTableName);
            end;
          finally
            for I := Pred(TmpList.Count) downto 0 do begin
              PItem := PffTableDescriptor(TmpList.Items[I]);
              FFFreeMem(PItem, SizeOf(PItem^));
            end;
            TmpList.Free;
          end;
        finally
          if not WasActive then                                        {!!.01}
            CloseDatabase(DB);
        end;{try..finally}
      end;
    end;
  finally
    aList.EndUpdate;
  end;{try..finally}
end;
{--------}
function TffSession.GetTaskStatus(
                            const aTaskID    : Longint;
                              var aCompleted : Boolean;
                              var aStatus    : TffRebuildStatus) : TffResult;
var
  IsPresent : Boolean;
begin
  Result := DBIERR_NONE;

  if (aTaskID = -1) then begin
    {TaskID of -1 means no task was created, so pretend it has been
     completed - there's no need to call the server on this one}
    aCompleted := True;
    FillChar(aStatus, SizeOf(aStatus), 0);
    aStatus.rsFinished := True;
    Exit;
  end;

  Result := ServerEngine.RebuildGetStatus(aTaskID,
                                          Client.ClientID,
                                          IsPresent,
                                          aStatus);
  if IsPresent then begin
    aCompleted := aStatus.rsFinished;
  end else
    Result := DBIERR_OBJNOTFOUND;
end;
{--------}
function TffSession.IsAlias(const aName : string) : Boolean;
begin
  Result := IsFFAliasName(Self, aName);
end;
{--------}
function TffSession.ModifyAlias(const aName       : string;
                                const aNewName    : string;
                                const aNewPath    : string;
                                      aCheckSpace : Boolean)           {!!.11}
                                                  : TffResult;
begin
  Assert(aName <> '');
  Assert((aNewName <> '') or (ANewPath <> ''));
  CheckActive;
  Result := ServerEngine.DatabaseModifyAlias(Client.ClientID,
                                             aName,
                                             aNewName,
                                             aNewPath,
                                             aCheckSpace);             {!!.11}
end;

{--------}
function TffSession.OpenDatabase(const aName : string)
                                             : TffBaseDatabase;
begin
  Result := FindFFDatabaseName(Self, aName, True);
  if Assigned(Result) then
    Result.Active := True; 
end;
{Begin !!.06}
{--------}
function TffSession.ProcessRequest(aMsgID           : Longint;
                                   aTimeout         : Longint;
                                   aRequestData     : Pointer;
                                   aRequestDataLen  : Longint;
                                   aRequestDataType : TffNetMsgDataType;
                               var aReply           : Pointer;
                               var aReplyLen        : Longint;
                                   aReplyType       : TffNetMsgDataType) : TffResult;
begin
  Result := scGetClient.ProcessRequest(aMsgID,
                                       aTimeout,
                                       aRequestData,
                                       aRequestDataLen,
                                       aRequestDataType,
                                       aReply,
                                       aReplyLen,
                                       aReplyType);
end;
{--------}
function TffSession.ProcessRequestNoReply(aMsgID          : Longint;
                                          aTimeout        : Longint;
                                          aRequestData    : Pointer;
                                          aRequestDataLen : Longint ) : TffResult;
begin
  Result := scGetClient.ProcessRequestNoReply(aMsgID,
                                              aTimeout,
                                              aRequestData,
                                              aRequestDataLen);
end;
{End !!.06}
{--------}
procedure TffSession.SetLoginParameters(const aName : TffName; aPassword : TffName);
begin
  if Assigned(Client) then
    Client.UserName := aName
  else
    ffclUsername := aName;
  ffclPassword := aPassword;
end;
{--------}
procedure TffSession.SetLoginRetries(const aRetries : Integer);
begin
  if Assigned(Client) then
    Client.PasswordRetries := aRetries
  else
    ffclLoginRetries := aRetries;
end;
{--------}
function TffSession.scGetClient : TffBaseClient;
begin
  Result := TffBaseClient(DBOwner);
end;
{--------}
function TffSession.scGetDatabase(aInx : Integer) : TffBaseDatabase;
begin
  Result := TffBaseDatabase(OwnedDBItems[aInx]);
end;
{--------}
function TffSession.scGetDatabaseCount : Integer;
begin
  Result := OwnedDBItems.Count;
end;
{--------}
function TffSession.scGetIsDefault : Boolean;
begin
  if (DBOwner = nil) then
    Result := False
  else
    Result := TffBaseClient(DBOwner).IsDefault and scIsDefault;
end;
{--------}
function TffSession.scGetServerEngine : TffBaseServerEngine;
begin
  if Assigned(scServerEngine) and Active then
    Result := scServerEngine
  else
    Result := Client.ServerEngine;
end;
{--------}
procedure TffSession.scRefreshTimeout;                               {new !!.11}
var
  Idx : Integer;
begin
  if Active then begin
    Check(ServerEngine.SessionSetTimeout(Client.bcClientID, scSessionID, GetTimeout));
    for Idx :=0 to Pred(OwnedDBItems.Count) do
      TffBaseDatabase(OwnedDBItems[Idx]).bdRefreshTimeout;
  end;
end;
{--------}
procedure TffSession.scSetAutoSessionName(const Value : Boolean);
begin
  if Value <> scAutoSessionName then begin
    if Value then begin
      CheckInactive(False);
      SessionName := 'FFSession_' + IntToStr(Longint(Self));
    end;
    scAutoSessionName := Value;
  end;
end;
{--------}
procedure TffSession.scSetIsDefault(const Value : Boolean);
begin
  if (Value <> scIsDefault) then begin
    if (DBOwner = nil) then
      scIsDefault := False
    else
      TffBaseClient(DBOwner).bcMakeSessionDefault(Self, Value);
  end;
end;
{--------}
procedure TffSession.scSetSessionName(const aName : string);
{Rewritten !!.11}
var
  S : TffSession;
  Counter : Integer;
  TmpName : string;
begin
  if DBName = aName then Exit;

  TmpName := aName;
  S := FindFFSessionName(TmpName);
  if (S <> nil) then
    if scAutoSessionName then begin
      { Generate a unique name. }
      Counter := 0;
      repeat
        TmpName := aName + IntToStr(Counter);
        inc(Counter);
      until FindFFSessionName(TmpName) = nil;
    end
    else
      { Allow case changes to existing name }
      if not (AnsiUpperCase(TmpName) = AnsiUpperCase(DBName)) then
        RaiseFFErrorObjFmt(Self, ffdse_SessNameExists, [TmpName]);
  DBName := TmpName;
end;
{--------}
function TffSession.GetTimeout : Longint;
begin
  if (scTimeOut = -1) and assigned(Client) then
   Result := Client.Timeout
  else
    Result := scTimeout;
end;
{--------}
procedure TffSession.scSetTimeout(const Value : Longint);
begin
  if scTimeout = Value then Exit;
  scTimeout := Value;

(* removed !!.11
   if Active then
    Check(ServerEngine.SessionSetTimeout(Client.bcClientID, scSessionID, GetTimeout)); {!!.06}*)
  scRefreshTimeout;
end;
{--------}
procedure TffSession.DoStartup;
begin
  { Fire the OnStartup event if necessary }
  if Assigned(scOnStartup) then
    scOnStartup(Self);

  { ask the client to open the connection to the server }
  Client.OpenConnection(Self);
end;
{--------}
procedure TffSession.ChooseServer(var aServerName  : TffNetAddress);
var
  Names         : TStringList;
//  OurServerName : TffNetAddress;                                     {!!.01}
  ChoseOne      : boolean;
begin
  aServerName := '';
  Names := TStringList.Create;
  try
    Names.Sorted := true;
    FindServers(true);
    try
      Client.GetServerNames(Names);
    finally
      FindServers(false);
    end;
    if (Names.Count = 1) then
      aServerName := Names[0]
    else if (Names.Count > 1) then begin
      if Assigned(scChooseServer) then
        scChooseServer(Self, Names, aServerName, ChoseOne)
      else
        with TFFPickServerDlg.Create(nil) do
          try
            CBNames.Items.Assign(Names);
            CBNames.ItemIndex := 0;
            ShowModal;
            if (ModalResult = mrOk) then begin
              aServerName := CBNames.Text;
              ChoseOne := true;
            end;
          finally
            Free;
          end;
      if not ChoseOne then                                             {!!.01}
//        aServerName := OurServerName                                 {!!.01}
//      else                                                           {!!.01}
        aServerName := Names[0];
    end;
  finally
    Names.Free;
  end;
end;
{--------}
procedure TffSession.FindServers(aStarting : Boolean);
begin
  if Assigned(scFindServers) then
    scFindServers(Self, aStarting);
end;
{--------}
procedure TffSession.DoLogin(var aUserName : TffName;
                             var aPassword : TffName;
                             var aResult   : Boolean);
var
  FFLoginDialog : TFFLoginDialog;
begin
  if Assigned(scLogin) then
    scLogin(Self, aUserName, aPassword, aResult)
  else begin
    FFLoginDialog := TFFLoginDialog.Create(nil);
    try
      with FFLoginDialog do begin
        UserName := aUserName;
        Password := aPassword;
        ShowModal;
        aResult := ModalResult = mrOK;
        if aResult then begin
          aUserName := UserName;
          aPassword := Password;
        end;
      end;
    finally
      FFLoginDialog.Free;
    end;
  end;
end;
{====================================================================}


{===TffSessionList===================================================}
function TffSessionList.slGetCurrSess : TffSession;
begin
  Result := slCurrSess;
end;
{--------}
function TffSessionList.slGetItem(aInx : Integer) : TffSession;
begin
  Result := TffSession(dblGetItem(aInx));
end;
{--------}
procedure TffSessionList.slSetCurrSess(CS : TffSession);
begin
  slCurrSess := CS;
end;
{====================================================================}


{===TffDatabase======================================================}
constructor TffBaseDatabase.Create(aOwner : TComponent);
var
  DefSess : TffSession;
begin
  inherited Create(aOwner);

  dbliReqPropName := ffcDatabaseName;
  bdAutoDBName := False;
  bdDatabaseID := 0;
  bdInTransaction := False;
  bdTimeout := -1;
  bdServerEngine := nil;

  dbliLoadPriority := 3;
  {attach ourselves to the default session}
  DefSess := FindDefaultFFSession;
  if DefSess <> nil then
    SessionName := DefSess.SessionName;
end;
{--------}
destructor TffBaseDatabase.Destroy;
begin
  FFNotifyDependents(ffn_Destroy);

  Close;                                                               {!!.01}

  bdInformTablesAboutDestruction;

  inherited Destroy;
end;
{--------}
function TffBaseDatabase.GetFreeDiskSpace(var aFreeSpace : Longint) : TffResult;
begin
  CheckActive;
  Result := ServerEngine.DatabaseGetFreeSpace(DatabaseID, aFreeSpace);
end;
{--------}
function TffBaseDatabase.GetTimeout : Longint;
begin
  if (bdTimeout = -1) and assigned(Session) then
    Result := Session.GetTimeout
  else
    Result := bdTimeout;
end;
{--------}
procedure TffBaseDatabase.CloseDataSets;
begin
  inherited dbliClosePrim;
end;
{--------}
function TffDatabase.CreateTable(
                           const aOverWrite  : Boolean;
                           const aTableName  : TffTableName;
                                 aDictionary : TffDataDictionary) : TffResult;
begin
  Assert(aTableName <> '');
  Assert(Assigned(aDictionary));
  Result := ServerEngine.TableBuild(DatabaseID,
                                    aOverWrite,
                                    aTableName,
                                    False,
                                    aDictionary);
end;
{--------}
procedure TffBaseDatabase.Commit;
begin
  if bdTransactionCorrupted then
    Check(DBIERR_FF_CorruptTrans);

  CheckActive;
  Check(ServerEngine.TransactionCommit(DatabaseID));

  bdInTransaction := False;
  bdTransactionCorrupted := False;
end;
{--------}
function TffBaseDatabase.ReIndexTable(const aTableName : TffTableName;
                                        const aIndexNum  : Integer;
                                          var aTaskID    : Longint) : TffResult;
begin
  Assert(aTableName <> '');
  aTaskID := -1;

  Result := ServerEngine.TableRebuildIndex(DatabaseID,
                                           aTableName,
                                           '',
                                           aIndexNum,
                                           aTaskID);
  if Result <> DBIERR_NONE then
    aTaskID := -1;
end;
{--------}
function TffDatabase.RestructureTable(
                                const aTableName  : TffTableName;
                                      aDictionary : TffDataDictionary;
                                      aFieldMap   : TStrings;
                                  var aTaskID     : LongInt) : TffResult;
var
  I             : Integer;
  FieldMapEntry : TffShStr;
  TmpTableName  : TffTableName;
  TmpFieldMap   : TffStringList;
begin
  Assert(aTableName <> '');
  Assert(Assigned(aDictionary));
  aTaskID := -1;
  TmpTableName := ffExtractFileName(aTableName);

  TmpFieldMap := TffStringList.Create;
  try
    if Assigned(aFieldMap) then
      for I := 0 to aFieldMap.Count - 1 do begin
        FieldMapEntry := aFieldMap[I];
        TmpFieldMap.Insert(FieldMapEntry);
      end;

    Result := ServerEngine.TableRestructure(DatabaseID,
                                            TmpTableName,
                                            aDictionary,
                                            TmpFieldMap,
                                            aTaskID);
  finally
    TmpFieldMap.Free;
  end;

  if Result <> DBIERR_NONE then
    aTaskID := -1;
end;
{--------}
procedure TffDatabase.dbliClosePrim;
begin
  inherited dbliClosePrim;

  if (bdDatabaseID > 0) then
    if Assigned(ServerEngine) then
      Check(ServerEngine.DatabaseClose(bdDatabaseID));
  bdDatabaseID := 0;
  bdServerEngine := nil;
end;
{--------}
function TffBaseDatabase.dbliCreateOwnedList : TffDBList;
begin
  Result := TffDBList(TffTableProxyList.Create(Self));
end;
{--------}
function TffBaseDatabase.dbliFindDBOwner(const aName : string) : TffDBListItem;
begin
  if (aName = '') then
    Result := FindDefaultFFSession
  else
    Result := FindFFSessionName(aName);
end;
{--------}
procedure TffBaseDatabase.dbliMustBeClosedError;
begin
  RaiseFFErrorObj(Self, ffdse_DBMustBeClosed);
end;
{--------}
procedure TffBaseDatabase.dbliMustBeOpenError;
begin
  RaiseFFErrorObj(Self, ffdse_DBMustBeOpen);
end;
{--------}
procedure TffBaseDatabase.dbliOpenPrim;
begin
  inherited dbliOpenPrim;

  bdServerEngine := Session.ServerEngine;
end;
{--------}
procedure TffDatabase.dbliOpenPrim;
var
  Alias : string;
begin
  if (AliasName <> '') then
    Alias := AliasName
  else
    Alias := DatabaseName;

  Check(ServerEngine.SessionSetCurrent(Session.Client.ClientID,
                                       Session.SessionID));

  if not IsPath(Alias) then begin
    Check(ServerEngine.DatabaseOpen(Session.Client.ClientID,
                                    Alias,
                                    TffOpenMode(not ReadOnly),
                                    TffShareMode(not Exclusive),
                                    GetTimeOut,
                                    bdDatabaseID));
  end else begin
    { Alias is a specified as a path }
    Check(ServerEngine.DatabaseOpenNoAlias(Session.Client.ClientID,
                                           Alias,
                                           TffOpenMode(not ReadOnly),
                                           TFFShareMode(not Exclusive),
                                           GetTimeOut,
                                           bdDatabaseID));
  end;
end;
{--------}
procedure TffBaseDatabase.bdSetAutoDBName(const Value : Boolean);
begin
  if Value = bdAutoDBName then
    Exit;

  if Value then begin
    CheckInactive(False);
    DatabaseName := 'FFDB_' + IntToStr(Longint(Self));
  end;
  
  bdAutoDBName := Value;
end;
{--------}
function TffBaseDatabase.bdGetDataSetCount : Integer;
begin
  Result := OwnedDBItems.Count;
end;
{--------}
function TffBaseDatabase.bdGetDataSet(aInx : Integer) : TffDataSet;
begin
  Result := TffTableProxy(OwnedDBItems[aInx]).ffTable;
end;
{--------}
function TffBaseDatabase.bdGetDatabaseID : TffDatabaseID;
begin
  if not Active then
    Active := True;
  Result := bdDatabaseID;
end;
{--------}
function TffBaseDatabase.bdGetSession : TffSession;
begin
  Result := TffSession(DBOwner);
  if (Result = nil) then
    RaiseFFErrorObjFmt(Self, ffdse_DBNoOwningSess, [DatabaseName]);
end;
{--------}
procedure TffBaseDatabase.bdInformTablesAboutDestruction;
var
  Inx : Integer;
begin
  for Inx := Pred(DataSetCount) downto 0 do
    TffTableProxyList(OwnedDBItems)[Inx].tpDatabaseIsDestroyed;
end;
{--------}
procedure TffDatabase.dcSetAliasName(const aName : string);
begin
  CheckInactive(False);
  dcAliasName := aName;
end;
{--------}
procedure TffBaseDatabase.bdSetDatabaseName(const aName : string);
{Rewritten !!.11}
var
  Counter : Integer;
  TmpName : string;
begin
  if DBName = aName then Exit;

  TmpName := aName;
  if not (csReading in ComponentState) then begin
    if (Owner <> nil) and IsffAliasName(Session, TmpName) then
      RaiseFFErrorObjFmt(Self, ffdse_MatchesAlias, [TmpName]);
    if IsffDatabaseName(Session, TmpName) then
      if bdAutoDBName then begin
        { Generate a unique name. }
        Counter := 0;
        repeat
          TmpName := aName + IntToStr(Counter);
          inc(Counter);
        until not IsFFDatabaseName(Session, TmpName);
      end
      else
        { Allow case changes to existing name }
        if not (AnsiUpperCase(TmpName) = AnsiUpperCase(DBName)) then
          RaiseFFErrorObjFmt(Self, ffdse_DBNameExists, [TmpName]);
  end;
  dbliSetDBName(TmpName);
end;
{--------}
procedure TffBaseDatabase.bdSetExclusive(aValue : Boolean);
var
  Inx : Integer;
begin
  CheckInactive(False);
  bdExclusive := aValue;
  if aValue then
    for Inx := pred(DataSetCount) downto 0 do
      TffTableProxyList(OwnedDBItems)[Inx].ffTable.Exclusive := True;
end;
{--------}
procedure TffBaseDatabase.bdSetReadOnly(aValue : Boolean);
var
  Inx : Integer;
begin
  CheckInactive(False);
  bdReadOnly := aValue;
  if aValue then
    for Inx := pred(DataSetCount) downto 0 do
      TffTableProxyList(OwnedDBItems)[Inx].ffTable.ReadOnly := True;
end;
{--------}
procedure TffBaseDatabase.bdSetTimeout(const Value : Longint);
begin
  if bdTimeout = Value then Exit;
  bdTimeout := Value;

(* removed !!.11
  if Active then begin
    Check(ServerEngine.DatabaseSetTimeout(bdDatabaseID, GetTimeout)); {!!.06}
  end; *)
  bdRefreshTimeout;
end;
{--------}
procedure TffDatabase.GetTableNames(aList : TStrings);
var
  CEWasActive : Boolean;
  SSWasActive : Boolean;
  WasActive   : Boolean;
  TmpList     : TList;
  I           : Integer;
  PItem       : PffTableDescriptor;

begin
  Assert(Assigned(aList));

  CEWasActive := Session.Client.Active;
  SSWasActive := Session.Active;
  WasActive := Active;
  if not WasActive then
    Active := True;
  try
    aList.BeginUpdate;
    try
      TmpList := TList.Create;
      try
        Check(ServerEngine.DatabaseTableList(DatabaseID,
                                             '',
                                             TmpList));
        for I := 0 to Pred (TmpList.Count) do begin
          PItem := PffTableDescriptor(TmpList.Items[I]);
          aList.Add(PItem^.tdTableName);
        end;
      finally
        for I := Pred(TmpList.Count) downto 0 do begin
          PItem := PffTableDescriptor(TmpList.Items[I]);
          FFFreeMem(PItem, SizeOf(PItem^));
        end;
        TmpList.Free;
      end;
    finally
      aList.EndUpdate;
    end;{try..finally}
  finally
    if not WasActive then
      Active := False;
    if not SSWasActive then
      Session.Active := False;
    if not CEWasActive then
      Session.Client.Active := False;
  end;{try..finally}
end;
{--------}
function TffBaseDatabase.PackTable(const aTableName : TffTableName;
                                     var aTaskID    : LongInt) : TffResult;
begin
  Assert(aTableName <> '');
  aTaskID := -1;

  Result := ServerEngine.TablePack(DatabaseID,
                                   aTableName,
                                   aTaskID);
  if Result <> DBIERR_NONE then
    aTaskID := -1;
end;
{--------}
function TffBaseDatabase.IsSQLBased : Boolean;
begin
  Result := False;
end;
{--------}
procedure TffBaseDatabase.Rollback;
begin
  CheckActive;
  Check(ServerEngine.TransactionRollback(DatabaseID));

  bdInTransaction := False;
  bdTransactionCorrupted := False;
end;
{--------}
procedure TffBaseDatabase.StartTransaction;
begin
  CheckActive;
  if bdInTransaction then
    Check(DBIERR_ACTIVETRAN);

  Check(ServerEngine.TransactionStart(bdDatabaseID,
                                      bdFailSafe));
  bdInTransaction := True;
  bdTransactionCorrupted := False;
end;
{Begin !!.10}
{--------}
function TffBaseDatabase.StartTransactionWith(const aTables: array of TffBaseTable) : TffResult;
var
  CursorIDList : TffPointerList;
  Inx : Integer;
begin
  CheckActive;
  if bdInTransaction then
    Check(DBIERR_ACTIVETRAN);

  CursorIDList := TffPointerList.Create;
  try
    for Inx := Low(aTables) to High(aTables) do begin
      if not aTables[Inx].Active then
        RaiseFFErrorObjFmt(Self, ffdse_StartTranTblActive,
                           [aTables[Inx].TableName]);
      CursorIDList.Append(Pointer(aTables[Inx].CursorID));
    end;  { for }

    Result := ServerEngine.TransactionStartWith(bdDatabaseID,
                                                bdFailSafe,
                                                CursorIDList);
    if Result = DBIERR_NONE then begin
      bdInTransaction := True;
      bdTransactionCorrupted := False;
    end;

  finally
    CursorIDList.Free;
  end;
end;
{End !!.10}
{--------}
function TffBaseDatabase.TryStartTransaction;
begin
  Result := not InTransaction;
    if Result then
      StartTransaction;
end;
{--------}
procedure TffBaseDatabase.TransactionCorrupted;
begin
  bdTransactionCorrupted := True;
end;
{--------}
function TffBaseDatabase.TableExists(const aTableName : TffTableName) : Boolean;
                                                               {rewritten !!.11}
var
  SSWasActive : Boolean;
  CEWasActive : Boolean;
  WasActive : Boolean;
begin
  Assert(aTableName <> '');
  SSWasActive := Session.Active;
  CEWasActive := Session.Client.Active;
  WasActive := Active;
  try
    if not WasActive then
      Open;
    Check(ServerEngine.DatabaseTableExists(DatabaseID,
                                           aTableName,
                                           Result));
  finally
    if not WasActive then
      Close;
    if not SSWasActive then
      Session.Close;
    if not CEWasActive then
      Session.Client.Close;
  end;
end;
{--------}
function TffBaseDatabase.GetFFDataDictionary(const TableName : TffTableName;
                                             Stream : TStream) : TffResult;
begin
  Assert(TableName <> '');
  Assert(Assigned(Stream));
  Result := ServerEngine.TableGetDictionary(DatabaseID,
                                            FFExtractFileName(TableName),
                                            False,
                                            Stream);
end;
{====================================================================}


{====================================================================}
function TffDatabaseList.dlGetItem(aInx : Integer) : TffBaseDatabase;
begin
  Result := TffBaseDatabase(dblGetItem(aInx));
end;
{====================================================================}


{===TffTableProxyList================================================}
procedure TffTableProxyList.dblFreeItem(aItem : TffDBListItem);
var
  Inx        : Integer;
  TableProxy : TffTableProxy;
begin
  Inx := IndexOfItem(aItem);
  if (Inx <> -1) then begin
    TableProxy := Tables[Inx];
    TableProxy.ffTable.Free;
    TableProxy.ffTable := nil;
  end;
end;
{--------}
function TffTableProxyList.tlGetItem(aInx : Integer) : TffTableProxy;
begin
  Result := TffTableProxy(dblGetItem(aInx));
end;
{====================================================================}


{===TffTableProxy====================================================}
constructor TffTableProxy.Create(aOwner : TComponent);
var
  DefSess : TffSession;
begin
  inherited Create(aOwner);

  dbliReqPropName := ffcTableName;
  tpServerEngine := nil;
  dbliLoadPriority := 4;
  {make us have the default session as our session}
  DefSess := FindDefaulTffSession;
  if (DefSess <> nil) then
    SessionName := DefSess.SessionName;
end;
{--------}
procedure TffTableProxy.dbliClosePrim;
begin
  if not tpClosing then begin
    tpClosing := True;
    {close the real table}
    if (ffTable <> nil) then
      ffTable.dsCloseViaProxy;
    {let our ancestor do its stuff}

    tpServerEngine := nil;
    inherited dbliClosePrim;

    tpClosing := False;
  end;
end;
{--------}
function TffTableProxy.dbliFindDBOwner(const aName : string) : TffDBListItem;
var
  i  : Integer;
  DB : TffDatabase;
begin
  if (tpSession = nil) then
    Result := nil
  else begin
    try
      Result := FindffDatabaseName(tpSession, aName, (not FixingFromStream)); {!!.05}

      {if not found just look on the same form}
      if (Result = nil) and
         (aName <>'') and
         (ffTable <> nil) and
         (ffTable.Owner <> nil) then begin
        for i := 0 to pred(ffTable.Owner.ComponentCount) do
          if ffTable.Owner.Components[i] is TffDatabase then begin
            DB := TffDatabase(ffTable.Owner.Components[i]);
            if (DB.SessionName = SessionName) and
               (DB.DatabaseName = aName) then begin
              Result := DB;
              Exit;
            end;
          end;
      end;

    except
      Result := nil;
    end;
  end;
end;
{--------}
procedure TffTableProxy.dbliLoaded;
var
  StreamName : string;
begin
  try
    if (tpSessionName <> '') then begin
      StreamName := tpSessionName;
      tpSessionName := '';
      SessionName := StreamName;
    end;
  except
    if (csDesigning in ComponentState) then
      Application.HandleException(Self)
    else
      raise;
  end;{try..except}
  if (Session <> nil) and Session.LoadActiveFailed then
    dbliMakeActive := False;

  inherited dbliLoaded;
end;
{--------}
procedure TffTableProxy.dbliMustBeClosedError;
begin
  RaiseFFErrorObj(Self, ffdse_TblMustBeClosed);
end;
{--------}
procedure TffTableProxy.dbliMustBeOpenError;
begin
  RaiseFFErrorObj(Self, ffdse_TblMustBeOpen);
end;
{--------}
procedure TffTableProxy.dbliOpenPrim;
begin
  tpServerEngine := Session.ServerEngine;
end;
{--------}
procedure TffTableProxy.dbliDBOwnerChanged;
begin
  inherited;

  SessionName := Database.SessionName;
end;
{--------}
procedure TffTableProxy.tpDatabaseIsDestroyed;
begin
  tpDBGone := True;
end;
{--------}
function TffTableProxy.tpGetCursorID : TffCursorID;
begin
  if not Active then
    Active := True;
  Result := tpCursorID;
end;
{--------}
function TffTableProxy.tpGetDatabase : TffBaseDatabase;
begin
  Result := TffBaseDatabase(DBOwner);
end;
{--------}
function TffTableProxy.tpGetSession : TffSession;
begin
  if (tpSession = nil) then
    tpResolveSession;
  Result := tpSession;
end;
{--------}
function TffTableProxy.tpGetSessionName : string;
begin
  if (tpSession <> nil) then
    tpSessionName := tpSession.SessionName;
  Result := tpSessionName;
end;
{--------}
procedure TffTableProxy.tpResolveSession;
begin
  tpSession := FindffSessionName(tpSessionName);
end;
{--------}
procedure TffTableProxy.tpSetSessionName(aValue : string);
begin
  CheckInactive(True);
  if (csReading in ComponentState) or LoadingFromStream then begin
    tpSessionName := aValue;
    tpSession := nil;
  end
  else
    if (FFAnsiCompareText(aValue, SessionName) <> 0) then begin       {!!.07}
      tpSession := FindffSessionName(aValue);
      if (tpSession <> nil) then
        tpSessionName := tpSession.SessionName
      else
        tpSessionName := aValue;
      if (not FixingFromStream) then begin
        {if we're changing session, we should invalidate our database}
        { Our owner may have had it's session changed, so we first need
          to see if our database is in this new session }
        if Assigned(dbliDbOwner) then
          if Database.dbliDBOwner = tpSession then
          {our database's session changed too, leave the internal database field alNone }
        else
          //dbliDBOwner := nil;                                        {!!.12}
          dbliSetDBOwner(nil);                                         {!!.12}
      end;
    end;
end;
{====================================================================}


{===TffFieldDescItem=================================================}
constructor TffFieldDescItem.Create(aContainer : TffCollection;
                              const FD         : FLDDesc);
begin
  inherited Create(nil, aContainer);

  FFGetMem(fdiPhyDesc, sizeof(FLDDesc));
  Move(FD, fdiPhyDesc^, sizeof(FLDDesc));
  FFGetMem(fdiLogDesc, sizeof(FLDDesc));
  GetBDELogicalFieldDescriptor(fdiPhyDesc^, fdiLogDesc^);
  fdiFieldNum := succ(Identifier);
end;
{--------}
destructor TffFieldDescItem.Destroy;
begin
  if (fdiPhyDesc <> nil) then
     FFFreeMem(fdiPhyDesc, sizeof(FLDDesc));
  if (fdiLogDesc <> nil) then
     FFFreeMem(fdiLogDesc, sizeof(FLDDesc));

  inherited Destroy;
end;
{====================================================================}


{===TffTable=========================================================}
{--------}
destructor TffDataSet.Destroy;
begin
  dsDictionary.Free;
  dsDictionary := nil;
  dsFilters.Free;
  dsFilters := nil;
  dsFieldDescs.Free;
  dsFieldDescs := nil;

  {destroy our proxy}
  dsProxy.Free;
  dsProxy := nil;

  inherited Destroy;
end;
{--------}
constructor TffDataSet.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  dsCursorID := 0;
  dsTimeout := -1;
  dsXltMode := xltFIELD;
  dsCurRecBuf := nil;
  dsFilterTimeOut := 500;
  dsFilterEval := ffeServer;
  dsFilterResync := True;
  dsServerEngine := nil;

  dsFieldDescs := TffCollection.Create;
  dsFilters := TffCollection.Create;

  {create our proxy}
  dsProxy := TffTableProxy.Create(Self);
  dsProxy.ffTable := Self;

  dsDictionary := TffDataDictionary.Create(4096);

end;
{--------}
constructor TffBaseTable.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);

  btLookupCursorID := 0;
  btIgnoreDataEvents := False;                                        {!!.06}

  {create the index definitions}
  btIndexDefs := TIndexDefs.Create(Self);
  {set up a master table link, if needed}
  btMasterLink := TMasterDataLink.Create(Self);
  btMasterLink.OnMasterChange := btMasterChanged;
  btMasterLink.OnMasterDisable := btMasterDisabled;
  btRangeStack := TffTableRangeStack.Create;
end;
{--------}
destructor TffBaseTable.Destroy;
begin
  Close;

  btRangeStack.Free;
  btRangeStack := nil;
  btMasterLink.Free;
  btMasterLink := nil;
  btIndexDefs.Free;
  btIndexDefs := nil;

  inherited Destroy;
end;
{--------}
function TffDataSet.AddFileBlob(const aField    : Word;
                                const aFileName : TffFullFileName) : TffResult;
var
  IsNull : Boolean;
  BLOBNr : TffInt64;
  aData  : Pointer;
begin
  Assert(aFileName <> '');
  aData := ActiveBuffer;
  if not (Dictionary.FieldType[Pred(aField)] in
           [fftBLOB..ffcLastBLOBType]) then begin
    Result := DBIERR_NOTABLOB;
    Exit;
  end;

  Result := DBIERR_NONE;
  {if the BLOB exists, we need to delete it}
  Dictionary.GetRecordField(Pred(aField),
                            aData,
                            IsNull,
                            @BLOBNr);
  if not IsNull then begin
    {truncate it to 0}
    Result := TruncateBLOB(ActiveBuffer, aField, 0);
    {and now Free it}
    if Result = DBIERR_NONE then
      Result := FreeBLOB(ActiveBuffer, aField);
  end;

  if Result <> DBIERR_NONE then
    Exit;

  {now, there's no BLOB there - Add the fileBLOB}
  Result := ServerEngine.FileBLOBAdd(CursorID,
                                     aFileName,
                                     BLOBNr);
  if Result = DBIERR_NONE then
    Dictionary.SetRecordField(Pred(aField),
                              aData,
                              @BLOBNr);
end;

{--------}
procedure TffBaseTable.AddIndex(const aName, aFields : string;
                                      aOptions       : TIndexOptions);
var
  IndexDesc  : TffIndexDescriptor;
  EFNPOS     : Integer;
  Fld        : string;
  FldsInKey  : Integer;
  FldList    : TffFieldList;
  TaskID     : Longint;
  Done       : Boolean;
  TaskStatus : TffRebuildStatus;
  Stream     : TMemoryStream;
  WasActive  : Boolean;
  Bookmark   : TBookmark;
  RangeSaved : Boolean;
  Request    : PffnmCursorSetRangeReq;
  SetRangeReqLen : Integer;
begin
  WasActive := Active;
  {ensure the field definitions are updated}
  FieldDefs.Update;

  {encode the index descriptor}
  IndexDesc.idNumber := 0;
  IndexDesc.idName   := aName;
  IndexDesc.idDesc   := '';
  IndexDesc.idFile   := 0;
  IndexDesc.idKeyLen := 0;
  FillChar(IndexDesc.idFieldIHlprs, SizeOf(IndexDesc.idFieldIHlprs), 0);
  IndexDesc.idDups   := not (ixUnique in aOptions);
  IndexDesc.idAscend := not (ixDescending in aOptions);
  IndexDesc.idNoCase := ixCaseInsensitive in aOptions;
  EFNPOS := 0;
  FldsInKey := 0;

  while (EFNPos <= Length(aFields)) and
   (FldsInKey < DBIMAXFLDSINKEY) do begin
   Fld:= ExtractFieldName(aFields, EFNPos);
   if (Fld <> '') and (Fld[length(Fld)] = ';') then
     System.Delete(Fld, length(Fld), 1);
   FldList[FldsInKey] := Pred(FieldDefs.Find(Fld).FieldNo);
   Inc(FldsInKey);
  end;

  IndexDesc.idCount  := FldsInKey;
  IndexDesc.idFields := FldList;

  {if the table is open, make sure it's in browse mode and then add
   the index}

  if WasActive then begin
    { We need to restore the position of the cursor when we are done. }
    Bookmark := GetBookmark;
    { If a range is active then push it onto the range stack.
      We will restore the range when we are done. }
    RangeSaved := False;
    if btRangeStack.SavedRequest then begin
      btRangeStack.PushSavedRequest;
      RangeSaved := True;
    end;

    { The table must be closed before an index can be added. }
    CheckBrowseMode;
    CursorPosChanged;
    Check(ServerEngine.CursorClose(CursorID));
    try
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
      while not Done do begin
        Sleep(250);
        Check(Session.GetTaskStatus(TaskID, Done, TaskStatus));
      end;
    finally
      { Re-open the table. }
      dsCursorID := GetCursorHandle(IndexName);
      { Do we need to restore a prior range? }
      if rangeSaved then begin
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

      end;
      {reset the record position}
      if (Bookmark <> nil) then begin
        Check(ServerEngine.CursorSetToBookmark(CursorID,
                                               Bookmark));
        FreeBookmark(Bookmark);
      end;
    end;

  end else begin
    {otherwise use our database to add the index}
    dsEnsureDatabaseOpen(True);
    try
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
      while not Done do begin
        Sleep(250);
        Check(Session.GetTaskStatus(TaskID, Done, TaskStatus));
      end;

    finally
      dsEnsureDatabaseOpen(False);
    end;

    { re-fetch data dictionary }
    Stream := TMemoryStream.Create;
    try
      if Database.GetFFDataDictionary(TableName, Stream) = DBIERR_NONE then begin
        Stream.Position:= 0;
        Dictionary.ReadFromStream(Stream);
      end;
    finally
      Stream.Free;
    end;

  end;

  { Make sure the index definitions are updated when required. }
  btIndexDefs.Updated := False;
end;
{--------}
function TffBaseTable.AddIndexEx(const aIndexDesc : TffIndexDescriptor;
                                   var aTaskID    : LongInt) : TffResult;
begin
  CheckInactive;
  Result := ServerEngine.TableAddIndex(Database.DatabaseID,
                                       CursorID,
                                       TableName,
                                       aIndexDesc);
  if Result = DBIERR_NONE then
    Result := ServerEngine.TableRebuildIndex(Database.DatabaseID,
                                             TableName,
                                             aIndexDesc.idName,
                                             aIndexDesc.idNumber,
                                             aTaskID);
  if Result <> DBIERR_NONE then
    aTaskID := -1;
end;
{--------}
function TffDataSet.AllocRecordBuffer : PChar;
begin
  FFGetZeroMem(Result, dsRecBufSize);
  Assert(Assigned(Result), 'Rec Buf not Assigned');
end;
{--------}
procedure TffBaseTable.ApplyRange;
begin
  CheckBrowseMode;
  if btSetRange then
    First;
end;
{--------}
function TffDataSet.BookmarkValid(aBookmark : TBookmark) : Boolean;
begin
  if (dsCursorID = 0) or not Assigned(aBookmark) then
    Result := False
  else begin
    CursorPosChanged;
    Result := ServerEngine.CursorSetToBookmark(CursorID,
                                               aBookmark) = DBIERR_NONE;
    if Result then
      Result := dsGetRecord(ffltNoLock, nil, nil) = DBIERR_NONE;
  end;
end;
{--------}
procedure TffBaseTable.Cancel;
begin
  inherited Cancel;

  if (State = dsSetKey) then
    btEndKeyBufferEdit(False);
end;
{--------}
procedure TffBaseTable.CancelRange;
begin
  CheckBrowseMode;
  UpdateCursorPos;
  if btResetRange(CursorID, False) then
    Resync([]);
end;
{--------}
procedure TffDataSet.ClearCalcFields(aBuffer : PChar);
begin
  FillChar(aBuffer[dsCalcFldOfs], CalcFieldsSize, 0);
end;
{--------}
procedure TffDataSet.CloseBlob(aField : TField);
begin
  FreeBlob(ActiveBuffer, aField.FieldNo);
end;
{--------}
procedure TffDataSet.CloseCursor;
begin
{Begin !!.05}
  try
    {call our ancestor (who'll call InternalClose)}
    inherited CloseCursor;

    {if we have a handle destroy it}
    if (dsCursorID > 0) then
      try
        DestroyHandle(dsCursorID);
      finally
        dsCursorID := 0;
      end;
  finally
    {close our table proxy}
    if (dsProxy <> nil) then begin
      dsClosing := True;
      dsProxy.Close;
      dsClosing := False;
    end;
  end;
{End !!.05}
end;
{--------}
function TffDataSet.CompareBookmarks(Bookmark1,
                                     Bookmark2 : TBookmark) : Integer;
{Begin !!.02}
{$IFNDEF RaiseBookmarksExcept}
var
  aResult : TffResult;
{$ENDIF}
{End !!.02}
begin
  if (BookMark1 = nil) or (Bookmark2 = nil) then begin
    if (Bookmark1 = nil) then
      if (Bookmark2 = nil) then
        Result := 0
      else
        Result := 1
    else
      Result := -1;
    Exit;
  end;

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
  if aResult <> DBIERR_NONE then
    Result := aResult;
{$ENDIF}
{End !!.02}
end;
{--------}
function TffDataSet.CreateBlobStream(aField : TField;
                                     aMode  : TBlobStreamMode) : TStream;
begin
  Assert(Assigned(aField));
  Result := TffBlobStream.Create(aField as TBlobField, aMode);
end;
{Begin !!.02}
{--------}
procedure TffDataset.CopyRecords(aSrcTable : TffDataset; aCopyBLOBs : Boolean); {!!.06}
var
  WasOpen : Boolean;
begin
  CheckBrowseMode;
  { Make sure the source table is open. }
  WasOpen := aSrcTable.Active;
  if not WasOpen then
    aSrcTable.Open;
  try
    Check(ServerEngine.CursorCopyRecords(aSrcTable.CursorID, CursorID, aCopyBLOBs));
  finally
    if not WasOpen then
      aSrcTable.Close;
  end;
end;
{--------}
procedure TffBaseTable.CreateTable;                                   {!!.05}
begin                                                                 {!!.05}
  Assert(Assigned(Dictionary));                                       {!!.10}
  CreateTableEx(Dictionary.BlockSize);                                {!!.10}
end;                                                                  {!!.05}
{--------}
procedure TffBaseTable.CreateTableEx(const aBlockSize : Integer);     {!!.05}
var
  Dict          : TffDataDictionary;
  EFNPOS        : Integer;
  Fld           : string;
  FldList       : TffFieldList;
  FldIHList     : TffFieldIHList;
  FldType       : TffFieldType;
  FldsInKey     : Integer;
  i             : integer;
  FldPhysSize   : word;
  SeqAccessName : TffShStr;
begin
  {the table can't be open}
  dsProxy.CheckInactive(true);
  {make sure we have defined all fields within our object}
  if (FieldDefs.Count = 0) then
    for i := 0 to pred(FieldCount) do
      if (Fields[i].FieldKind = fkData) then
        FieldDefs.Add(Fields[i].FieldName,
                      Fields[i].DataType,
                      Fields[i].Size,
                      Fields[i].Required);
  {now fill in the descriptor fields}
  dsEnsureDatabaseOpen(true);
  try
    Dict := TffDataDictionary.Create(aBlockSize);                     {!!.05}
    try
      for i := 0 to pred(FieldDefs.Count) do
        with FieldDefs[i] do begin
          MapVCLTypeToFF(DataType, Size, FldType, FldPhysSize);
          if FldType <> fftReserved20 then begin
            Dict.AddField(Name, '', FldType, FldPhysSize, Precision, Required, nil)
          end else
            RaiseFFErrorObjFmt(Self, ffdse_InvalidFieldType,
                               [GetEnumName(TypeInfo(TFieldType), ord(DataType)),
                                Name]);
        end;

      SeqAccessName := uppercase(ffStrResGeneral[ffscSeqAccessIndexName]);
      for i := 0 to pred(IndexDefs.Count) do
        with IndexDefs[i] do
          if (UpperCase(Name) <> SeqAccessName) then begin
            { Get Field List }
            EFNPOS := 0;
            FldsInKey := 0;
            while (EFNPos <= Length(Fields)) and
              (FldsInKey < DBIMAXFLDSINKEY) do begin
               Fld:= ExtractFieldName(Fields, EFNPos);
               if (Fld<>'') and
                  (Fld[length(Fld)]=';') then
                  System.delete(Fld, length(Fld), 1);
                FldList[FldsInKey] := pred(FieldDefs.Find(Fld).FieldNo);
                FldIHLIst[FldsInKey] := '';
              Inc(FldsInKey);
            end;
            Dict.AddIndex(Name,
                          '',
                          0,
                          FldsInKey,
                          FldList,
                          FldIHList,
                          not (ixUnique in Options),
                          not (ixDescending in Options),
                          ixCaseInsensitive in Options);
          end;

      TffDatabase(Database).CreateTable(True, TableName, Dict);
    finally
      Dict.Free;
    end;
  finally
    dsEnsureDatabaseOpen(false);
  end;
end;
{--------}
procedure TffBaseTable.DataEvent(aEvent: TDataEvent; aInfo: Longint);
begin
  if btIgnoreDataEvents then                                          {!!.06}
    Exit;                                                             {!!.06}
  if (aEvent = dePropertyChange) then
    IndexDefs.Updated := False;

  inherited DataEvent(aEvent, aInfo);

  if aEvent = deUpdateState then
    if State = dsEdit then begin
      FreeRecordBuffer(dsOldValuesBuffer);
      dsOldValuesBuffer := AllocRecordBuffer;
      Move(ActiveBuffer^, dsOldValuesBuffer^, dsRecBufSize);
    end else begin
      FreeRecordBuffer(dsOldValuesBuffer);
      dsOldValuesBuffer := nil;
    end;
end;
{--------}
procedure TffBaseTable.DeleteIndex(const aIndexName : string);
var
  VerifiedName : string;
begin
  btRetrieveIndexName(aIndexName, True, VerifiedName);
  if Active then begin
    CheckBrowseMode;
    Check(ServerEngine.TableDropIndex(Database.DatabaseID,
                                      CursorID,
                                      TableName,
                                      VerifiedName,
                                      0));
  end else begin
    dsEnsureDatabaseOpen(True);
    try
    Check(ServerEngine.TableDropIndex(Database.DatabaseID,
                                      0,
                                      TableName,
                                      VerifiedName,
                                      0));
    finally
      dsEnsureDatabaseOpen(False);
    end;
  end;
  btIndexDefs.Updated := False;
end;
{Begin !!.06}
{--------}
procedure TffBaseTable.DeleteRecords;
begin
  CheckActive;
  if State in [dsInsert, dsSetKey] then Cancel else
  begin
    DataEvent(deCheckBrowseMode, 0);
    DoBeforeDelete;
    DoBeforeScroll;
    Check(ServerEngine.CursorDeleteRecords(CursorID));
    FreeFieldBuffers;
    SetState(dsBrowse);
    Resync([]);
    DoAfterDelete;
    DoAfterScroll;
  end;
end;
{End !!.06}
{--------}
procedure TffDataSet.DeleteTable;
begin
  dsProxy.CheckInactive(True);
  dsEnsureDatabaseOpen(True);
  try
    Check(ServerEngine.TableDelete(Database.DatabaseID,
                                   TableName));
  finally
    dsEnsureDatabaseOpen(False);
  end;
end;
{--------}
procedure TffBaseTable.DoOnNewRecord;
var
  i : Integer;
begin
  if btMasterLink.Active and (btMasterLink.Fields.Count > 0) then
    for i := 0 to pred(btMasterLink.Fields.Count) do
      IndexFields[i] := TField(btMasterLink.Fields[i]);

  inherited DoOnNewRecord;
end;
{--------}
procedure TffBaseTable.EditKey;
begin
  btSetKeyBuffer(ketNormal, False);
end;
{--------}
procedure TffBaseTable.EditRangeEnd;
begin
  btSetKeyBuffer(ketRangeEnd, False);
end;
{--------}
procedure TffBaseTable.EditRangeStart;
begin
  btSetKeyBuffer(ketRangeStart, False);
end;
{--------}
procedure TffDataSet.EmptyTable;

begin
  if Active then begin
    CheckBrowseMode;
    Active := False;
    Check(ServerEngine.TableEmpty(Database.DatabaseID,
                                  0,
                                  TableName));
    Active := True;
  end else begin
    dsEnsureDatabaseOpen(True);
    try
      Check(ServerEngine.TableEmpty(Database.DatabaseID,
                                    0,
                                    TableName));
    finally
      dsEnsureDatabaseOpen(False);
    end;
  end;
end;
{--------}
function TffBaseTable.FindKey(const aKeyValues: array of const): Boolean;
begin
  CheckBrowseMode;
  btSetKeyFields(ketNormal, aKeyValues);
  Result := GotoKey;
end;
{--------}
procedure TffBaseTable.FindNearest(const aKeyValues : array of const);
begin
  CheckBrowseMode;
  btSetKeyFields(ketNormal, aKeyValues);
  GotoNearest;
end;
{--------}
function TffDataSet.FreeBlob(                                { Free the blob }
                             pRecBuf : Pointer;              { Record Buffer }
                             iField  : Word                  { Field number of blob(1..n) }
                            ) : TffResult;
var
  BLOBNr : TffInt64;
  IsNull : Boolean;
begin
  Result := dsCheckBLOBHandle(pRecBuf, iField, IsNull, BLOBNr);
  if (Result = DBIERR_NONE) and (not IsNull) then begin
    Result := ServerEngine.BLOBFree(CursorID,
                                    BLOBNr,
                                    dsBlobOpenMode = omREADONLY);
    if (Result = DBIERR_BLOBMODIFIED) then begin
      {DBIERR_BLOBMODIFIED is a special ff 'error' when received here:
       it means that the BLOB was empty and so the BLOB number has
       been deleted at the server; the client must set the BLOB field
       to null}
      Dictionary.SetRecordField(pred(iField), pRecBuf, nil);
      dsModifyRecord(pRecBuf, False);
    end;
  end;
end;
{--------}
function TffDataSet.FindRecord(aRestart, aGoForward : Boolean) : Boolean;
begin
  {Note: this method is called by FindFirst/Last/Next/Prior; for each
   possibility the parameters are    TT    / TF / FT / ff  }
  CheckBrowseMode;
  DoBeforeScroll;
  SetFound(False);
  UpdateCursorPos;
  CursorPosChanged;
  if not Filtered then
    dsActivateFilters;
  try
    if aGoForward then begin
      if aRestart then
        InternalFirst;
      Result := (dsGetNextRecord(ffltNoLock, nil, nil) = DBIERR_NONE);
    end else begin
      if aRestart then
        Check(ServerEngine.CursorSetToEnd(CursorID));
      Result := (dsGetPriorRecord(ffltNoLock, nil, nil) = DBIERR_NONE);{!!.01}
    end;
  finally
    if not Filtered then
      dsDeactivateFilters;
  end;
  if Result then begin
    Resync([rmExact, rmCenter]);
    SetFound(True);
    DoAfterScroll;
  end;
  Result := Found;
end;
{--------}
procedure TffDataSet.FreeRecordBuffer(var aBuffer : PChar);
begin
  if Assigned(aBuffer) then begin
    FFFreeMem(aBuffer, dsRecBufSize);
    aBuffer := nil;
  end;
end;
{--------}
procedure TffDataSet.GetBookmarkData(aBuffer : PChar; aData : Pointer);
begin
  Move(aBuffer[dsBookmarkOfs], aData^, BookmarkSize);
end;
{--------}
function TffDataSet.GetBookmarkFlag(aBuffer : PChar): TBookmarkFlag;
begin
  Result := PDataSetRecInfo(aBuffer + dsRecInfoOfs)^.riBookmarkFlag
end;
{--------}
function TffDataSet.GetCanModify : Boolean;
begin
  {the TffTable can be modified if it is open, and in readwrite mode}
  Result := Active and (not ReadOnly);
end;
{--------}
function TffDataSet.GetCurrentRecord(aBuffer : PChar) : Boolean;
begin
  if (not IsEmpty) and (GetBookmarkFlag(ActiveBuffer) = bfCurrent) then begin
    UpdateCursorPos;
    Result := dsGetRecord(ffltNoLock, aBuffer, nil) = DBIERR_NONE;
  end else
    Result := False;
end;
{--------}
{$IFDEF ProvidesDatasource}
function TffBaseTable.GetDataSource: TDataSource;
begin
  Result := MasterSource;
end;
{$ENDIF}
{--------}
function TffDataSet.GetFieldData(aField : TField; aBuffer : Pointer): Boolean;
var
  IsBlank : Boolean;
  RecBuf  : PChar;
  FDI : TffFieldDescItem;
  Status : TffResult;
begin
  Result := False;
  if not GetActiveRecBuf(RecBuf) then
    Exit;
  if aField.FieldNo > 0 then begin
    if dsCursorID <> 0 then begin
      if (RecBuf = nil) then
        Status := DBIERR_INVALIDPARAM
      else begin
        if dsGetFieldDescItem(aField.FieldNo, FDI) then
          Status := dsTranslateGet(FDI, RecBuf, aBuffer, IsBlank)
        else
          Status := DBIERR_OUTOFRANGE;
      end;
      Check(Status);
    end;
    Result := not IsBlank;
  end
  else {FieldNo <= 0} begin
    if State in [dsBrowse, dsEdit, dsInsert, dsCalcFields] then begin
      Inc(RecBuf, dsCalcFldOfs + aField.offset);
      Result := Boolean(RecBuf[0]);
      if Result and (aBuffer <> nil) then
        Move(RecBuf[1], aBuffer^, aField.DataSize);
    end;
  end;
end;
{--------}
procedure TffBaseTable.GetIndexNames(aList : TStrings);
var
  i : Integer;
begin
  UpdateIndexDefs;
  aList.BeginUpdate;
  try
    aList.Clear;
    for i := 0 to pred(btIndexDefs.Count) do
      if (btIndexDefs[i].Name <> '') then
        aList.Add(btIndexDefs[i].Name);
  finally
    aList.EndUpdate;
  end;
end;
{--------}
function TffBaseTable.GetIsIndexField(Field : TField): Boolean;
var
  i : Integer;
begin
  Result := True;
  for i := 0 to pred(IndexFieldCount) do
    if (Field.FieldNo = btFieldsInIndex[i]) then
      Exit;
  Result := False;
end;
{--------}
function TffDataSet.GetRecNo: Integer;
begin
  Result := -1;
end;
{--------}
function TffDataSet.GetRecord(aBuffer  : PChar;
                              aGetMode : TGetMode;
                              aDoCheck : Boolean): TGetResult;
var
  Status : TffResult;
  Buff : Pointer;
begin
  {read the current, next or prior record; no locks placed}
  case aGetMode of
    gmCurrent :
      (*if Assigned(dsCurRecBuf) then begin                   {removed !!.03}
         Move(dsCurRecBuf^,aBuffer^,dsPhyRecSize);
         Status := DBIERR_NONE;
      end else*)
         Status := dsGetRecord(ffltNoLock, aBuffer, nil);
    gmNext :
      begin
        Status := dsGetNextRecord(ffltNoLock, Pointer(aBuffer), nil);
      end;
    gmPrior :
      begin
        Status := dsGetPriorRecord(ffltNoLock, Pointer(aBuffer), nil);
      end;
  else
    Status := DBIERR_NONE;
  end;
  {check the status}
  {..for success, set the record info fields, and get the bookmark}
  {..for EOF and BOF, set the bookmark status}
  {..for anything else, return an error}
  case Status of
    DBIERR_NONE :
      begin
        with PDataSetRecInfo(aBuffer + dsRecInfoOfs)^ do begin
          riBookmarkFlag := bfCurrent;
          riRecNo := 0;
        end;
        Buff := aBuffer + dsBookmarkOfs;
        Check(ServerEngine.CursorGetBookmark(CursorID,
                                             Buff));
        GetCalcFields(aBuffer);
        Result := grOK;
      end;
    DBIERR_BOF :
      Result := grBOF;
    DBIERR_EOF :
      Result := grEOF;
  else
    Result := grError;
    if aDoCheck then
      Check(Status);
  end;
end;
{--------}
function TffDataSet.GetRecordBatch(RequestCount : Longint;
                               var ReturnCount  : Longint;
                                   pRecBuff     : Pointer): TffResult;
var
  aError  : TffResult;
begin
  CheckActive;
  ReturnCount := 0;
  Result := ServerEngine.RecordGetBatch(CursorID,
                                        RequestCount,
                                        PhysicalRecordSize,
                                        ReturnCount,
                                        pRecBuff,
                                        aError);
end;
{------}
function TffDataSet.GetRecordBatchEx(RequestCount : Longint;
                                 var ReturnCount  : Longint;
                                     pRecBuff     : Pointer;
                                 var Error        : TffResult): TffResult;
begin
  CheckActive;
  ReturnCount := 0;
  Result := ServerEngine.RecordGetBatch(CursorID,
                                        RequestCount,
                                        PhysicalRecordSize,
                                        ReturnCount,
                                        pRecBuff,
                                        Error);
end;
{------}
function TffDataSet.GetRecordCount : Integer;
begin
  CheckActive;
  Check(dsGetRecordCountPrim(Result));
end;
{--------}
function TffDataSet.GetRecordSize : Word;
begin
  Result := dsPhyRecSize;
end;
{--------}
function TffDataset.dsGetTimeout : Longint;
begin
  if (dsTimeout = -1) and assigned(Database) then
    Result := Database.GetTimeout
  else
    Result := dsTimeout;
end;
{--------}
procedure TffDataSet.GotoCurrent(aDataSet : TffDataSet);
begin
  if (FFAnsiCompareText(DatabaseName, aDataSet.DatabaseName) <> 0) or {!!.07}
     (FFAnsiCompareText(TableName, aDataSet.TableName) <> 0) then     {!!.07}
    RaiseFFErrorObj(Self, ffdse_NotSameTbl);
  CheckBrowseMode;
  aDataSet.CheckBrowseMode;
  aDataSet.UpdateCursorPos;
  Check(ServerEngine.CursorSetToCursor(CursorID,
                                       aDataSet.CursorID));
  DoBeforeScroll;
  Resync([rmExact, rmCenter]);
  DoAfterScroll;
end;
{--------}
function TffBaseTable.GotoKey : Boolean;
var
  KeyRecInfo   : PKeyRecInfo;
  KeyRecBuffer : PChar;
begin
  CheckBrowseMode;
  DoBeforeScroll;
  CursorPosChanged;
  KeyRecBuffer := PKeyBuffers(btKeyBuffers)^[ketNormal];
  KeyRecInfo := PKeyRecInfo(KeyRecBuffer + btKeyInfoOfs);
  ffGetMem(dsCurRecBuf,dsPhyRecSize);
  try
    Result  := btGetRecordForKey(CursorID, False,
                                 KeyRecInfo^.kriFieldCount,
                                 0,
                                 KeyRecBuffer,
                                 dsCurRecBuf) = DBIERR_NONE;
    if Result then begin
      Resync([rmExact, rmCenter]);
      DoAfterScroll;
    end;
  finally
     FFFreeMem(dsCurRecBuf,dsPhyRecSize);
     dsCurRecBuf := nil;
  end;
end;
{--------}
procedure TffBaseTable.GotoNearest;
var
  SearchCond   : TffSearchKeyAction;
  KeyRecInfo   : PKeyRecInfo;
  KeyRecBuffer : PChar;
  Status       : TffResult;
begin
  CheckBrowseMode;
  CursorPosChanged;
  KeyRecBuffer := PKeyBuffers(btKeyBuffers)^[ketNormal];
  KeyRecInfo := PKeyRecInfo(KeyRecBuffer + btKeyInfoOfs);
  if KeyRecInfo^.kriExclusive then
    SearchCond := skaGreater
  else
    SearchCond := skaGreaterEqual;
  Status := ServerEngine.CursorSetToKey(CursorID,
                                        SearchCond,
                                        False,
                                        KeyRecInfo^.kriFieldCount,
                                        0,
                                        Pointer(KeyRecBuffer));
  if Status = DBIERR_ff_FilterTimeout then
    if not dsCancelServerFilter then
      Status := dsGetNextRecordPrim(CursorID, ffltNOLOCK, nil, nil);
  Check(Status);
  Resync([rmCenter]);
end;
{--------}
procedure TffDataSet.InitFieldDefs;
var
  SaveHandle : TffCursorID;
begin
  dsEnsureDatabaseOpen(True);
  try
    if (TableName = '') then
      RaiseFFErrorObj(Self, ffdse_UnnamedTblNoFlds);
    SaveHandle := cursorID;
    if (SaveHandle = 0) then
{Begin !!.03}
      OpenCursor(True);
//      dsCursorID := GetCursorHandle('');
    try
      InternalInitFieldDefs;
    finally
      if (SaveHandle = 0) then begin
        CloseCursor;
//        DestroyHandle(dsCursorID);
//        dsCursorID := 0;
{End !!.03}
      end;
    end;
  finally
    dsEnsureDatabaseOpen(False);
  end;{try..finally}
end;
{--------}
function TffDataSet.InsertRecordBatch(Count    : Longint;
                                      pRecBuff : Pointer;
                                      Errors   : PffLongintArray) : TffResult;
var
  iErr : Integer;
begin
  if not Assigned(pRecBuff) or not Assigned(Errors) then begin
    Result := DBIERR_INVALIDHNDL;
    Exit;
  end;
  CheckBrowseMode;
  Result := ServerEngine.RecordInsertBatch(CursorID,
                                           Count,
                                           PhysicalRecordSize,
                                           pRecBuff,
                                           Errors);
  if Result = DBIERR_NONE then begin
    for iErr := 0 to pred(Count) do
      if Errors^[iErr] <> DBIERR_NONE then begin
        Result := Errors^[iErr];
        Break;
      end;
    end;
end;
{------}
procedure TffDataSet.InternalAddRecord(aBuffer : Pointer; aAppend : Boolean);
begin
  if aAppend then
    Check(ServerEngine.CursorSetToEnd(CursorID));
  Check(ServerEngine.RecordInsert(CursorID,
                                  ffltWriteLock,
                                  aBuffer));
end;
{--------}
procedure TffDataSet.InternalCancel;
begin
  if (State = dsEdit) or (State = dsInsert) then
    Check(ServerEngine.RecordRelLock(CursorID,
                                     False));
end;
{--------}
procedure TffDataSet.InternalClose;
begin
{Begin !!.05}
  try
    {deactivate filters}
    if Filtered then
      dsDeactivateFilters;
  finally
    {drop filters}
    dsDropFilters;
    {clear up the fields}
    BindFields(False);
    if DefaultFields then
      DestroyFields;
    dsServerEngine := nil;
  end;
{End !!.05}
end;
{--------}
procedure TffBaseTable.InternalClose;
begin
  inherited InternalClose;
  {free our key Buffers}
  btFreeKeyBuffers;

  {reset important variables}
  btIndexFieldCount := 0;
  btKeyLength := 0;
  btNoCaseIndex := False;
end;
{--------}
procedure TffDataSet.InternalDelete;
var
  Result : TffResult;
begin
  {delete the record}
  Result := ServerEngine.RecordDelete(CursorID,
                                      nil);
  {apart from success, we allow not found type errors; check others}
   if (Result <> DBIERR_NONE) and
     (ErrCat(Result) <> ERRCAT_NOTFOUND) then
    Check(Result);
end;
{--------}
procedure TffDataSet.InternalEdit;
begin
  {get the record, placing a lock for the duration of the edit}
  Check(ServerEngine.RecordGet(CursorID,
                               ffltWriteLock,
                               Pointer(ActiveBuffer)));
end;
{--------}
procedure TffDataSet.InternalFirst;
begin
  Check(ServerEngine.CursorSetToBegin(CursorID));
end;
{--------}
procedure TffDataSet.InternalGotoBookmark(aBookmark : TBookmark);
begin
  if not Assigned(aBookmark) then
    Check(DBIERR_INVALIDHNDL);

  Check(ServerEngine.CursorSetToBookmark(CursorID,
                                         aBookmark));
end;
{--------}
procedure TffDataSet.InternalHandleException;
begin
  Application.HandleException(Self);
end;
{--------}
procedure TffDataSet.InternalInitFieldDefs;
var
  ffFldDesc : PffFieldDescriptor;
  i         : Integer;
begin
  FieldDefs.Clear;
  with Dictionary do
    for i := 0 to pred(FieldCount) do begin
      ffFldDesc := FieldDescriptor[i];
      dsAddFieldDesc(ffFldDesc, succ(i));
    end;
end;
{--------}
procedure TffDataSet.InternalInitRecord(aBuffer : PChar);
begin
  Dictionary.InitRecord(Pointer(aBuffer));
  Dictionary.SetDefaultFieldValues(Pointer(aBuffer));
  with PDataSetRecInfo(aBuffer + dsRecInfoOfs)^ do begin
    riRecNo := 0;
  end;
end;
{--------}
procedure TffDataSet.InternalLast;
begin
  Check(ServerEngine.CursorSetToEnd(CursorID));
end;
{$IFDEF ResizePersistFields}
{--------}
procedure TffDataSet.ReSizePersistentFields;
var
  I, FieldIndex: Integer;
  FieldDef: TFieldDef;
begin
  for I := 0 to Fields.Count - 1 do
    with Fields[I] do begin
      if FieldKind = fkData then begin
        FieldIndex := FieldDefList.IndexOf(FullName);
        if FieldIndex <> -1 then begin
          FieldDef := FieldDefList[FieldIndex];
          if (DataType = ftString) and (Size <> FieldDef.Size) then
            Size := FieldDef.Size;
        end;
     end;
  end;
end;
{$ENDIF}
{--------}
procedure TffDataset.InternalOpen;
var
  CursorProps : TffCursorProps;
begin
  dsServerEngine := Session.ServerEngine;
  {Note: by the time this method gets called, the FlashFiler table has
         been physically opened and tcHandle is valid.}
  GetCursorProps(CursorProps);
  dsPhyRecSize := CursorProps.RecordBufferSize;
  BookmarkSize := CursorProps.BookmarkSize;
  InternalInitFieldDefs;
  dsGetIndexInfo;
  if DefaultFields then
    CreateFields;
{$IFDEF ResizePersistFields}
  ReSizePersistentFields;
{$ENDIF}

  BindFields(True);
  dsGetRecordInfo(False);
  dsAllocKeyBuffers;
  InternalFirst;
  dsCheckMasterRange;
  if (FilterEval = ffeLocal) and (Filter <> '') then
    dsAddExprFilter(Filter, FilterOptions);
  if Assigned(OnFilterRecord) then
    dsAddFuncFilter(@TffBaseTable.dsOnFilterRecordCallback);
  if Filtered then
    dsActivateFilters;
end;
{--------}
procedure TffDataSet.InternalPost;
begin
  {$IFDEF DCC6OrLater}                                                {!!.05}
  inherited InternalPost;                                             {!!.05}
  {$ENDIF}                                                            {!!.05}

  {if we're editing a record, modify the record & remove lock}
  if (State = dsEdit) then
    Check(dsModifyRecord(Pointer(ActiveBuffer), True))
  {if we're inserting a record, do it & don't place lock}
  else if (State = dsInsert) then
    Check(ServerEngine.RecordInsert(CursorID,
                                    ffltWriteLock,
                                    Pointer(ActiveBuffer)));
end;
{--------}
procedure TffDataSet.InternalSetToRecord(aBuffer: PChar);
begin
  InternalGotoBookmark(aBuffer + dsBookmarkOfs);
end;
{--------}
function TffDataSet.IsCursorOpen : Boolean;
begin
  Result := (CursorID > 0);
end;
{--------}
function TffDataSet.IsSequenced : Boolean;
begin
  Result := False;
end;
{--------}
procedure TffDataSet.Loaded;
begin
  dsProxy.Loaded;

  inherited Loaded;
end;
{--------}
function TffBaseTable.Locate(const aKeyFields : string;
                         const aKeyValues : Variant;
                               aOptions   : TLocateOptions) : Boolean;
begin
  DoBeforeScroll;
  Result := btLocateRecord(aKeyFields, aKeyValues, aOptions, True);
  if Result then begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;
{--------}
procedure TffDataSet.LockTable(LockType: TffLockType);

begin
  dsSetTableLock(LockType, True);
end;
{--------}
function TffBaseTable.Lookup(const aKeyFields    : string;
                         const aKeyValues    : Variant;
                         const aResultFields : string) : Variant;
begin
  Result := Null;
  if btLocateRecord(aKeyFields, aKeyValues, [], False) then begin
    SetTempState(dsCalcFields);
    try
      CalculateFields(TempBuffer);
      Result := FieldValues[aResultFields];
    finally
      RestoreState(dsBrowse);
    end;{try..finally}
  end;
end;
{--------}
function TffDataSet.PackTable(var aTaskID : LongInt) : TffResult;
begin
  Result := Database.PackTable(TableName, aTaskID);
end;
{--------}
procedure TffDataSet.OpenCursor(aInfoQuery : Boolean);
begin
  {make sure our database is open first}
  dsEnsureDatabaseOpen(True);
  {open our proxy table}
  dsProxy.Open;
  {create the cursor handle}
  dsCursorID := dsCreateHandle;
  if (CursorID = 0) then
    RaiseFFErrorObj(Self, ffdse_CantGetTblHandle);
  {call our ancestor (who'll call InternalOpen, where the rest of the
   open process happens)}

  inherited OpenCursor(aInfoQuery);
end;
{--------}
procedure TffBaseTable.InternalOpen;
begin
  btChangeHandleIndex;
  btIgnoreDataEvents := False;                                        {!!.06}

  inherited InternalOpen;
end;
{--------}
function TffDataSet.OverrideFilterEx(aExprTree : ffSrBDE.pCANExpr;
                               const aTimeout  : TffWord32) : TffResult;
var
  ExprTree : CANExpr;
begin
  if not Assigned(aExprTree) then begin
    aExprTree := @ExprTree;
    FillChar(ExprTree, SizeOf(ExprTree), 0);
    ExprTree.iVer := CANEXPRVERSION;
    ExprTree.iTotalSize := SizeOf(ExprTree);
  end;

  Result := ServerEngine.CursorOverrideFilter(CursorID,
                                              aExprTree,
                                              aTimeout);
end;
{--------}
procedure TffBaseTable.Post;
begin
  inherited Post;

  if (State = dsSetKey) then begin                                     {!!.03}
    btEndKeyBufferEdit(True);
    Resync([]);                                                        {!!.03}
  end;                                                                 {!!.03}
end;
{--------}
function TffBaseTable.ReIndexTable(const aIndexNum  : Integer;
                                   var aTaskID    : Longint) : TffResult;
begin
  Result := Database.ReIndexTable(TableName, aIndexNum, aTaskID);
end;
{--------}
procedure TffDataSet.RenameTable(const aNewTableName : string);
begin
  dsProxy.CheckInactive(True);
  dsEnsureDatabaseOpen(True);
  try
    Check(ServerEngine.TableRename(Database.DatabaseID,
                                   TableName,
                                   aNewTableName));
  finally
    dsEnsureDatabaseOpen(False);
  end;
  TableName := aNewTableName;
end;
{Begin !!.07}
{--------}
procedure TffDataSet.RecordCountAsync(var TaskID : Longint);
begin
  CheckActive;
  Check(ServerEngine.TableGetRecCountAsync(CursorID, TaskID));
end;
{End !!.07}
{--------}
function TffDataSet.RestoreFilterEx : TffResult;
begin
  Result := ServerEngine.CursorRestoreFilter(CursorID);
end;
{--------}
function TffDataSet.RestructureTable(aDictionary : TffDataDictionary;
                                     aFieldMap   : TStrings;
                                 var aTaskID     : LongInt) : TffResult;
begin
  CheckInactive;
  Result := TffDatabase(Database).RestructureTable(TableName,
                                                   aDictionary,
                                                   aFieldMap,
                                                   aTaskID);
end;
{--------}
function TffDataSet.SetFilterEx(aExprTree : ffSrBDE.pCANExpr;
                          const aTimeout  : TffWord32) : TffResult;
var
  ExprTree : CANExpr;
begin
  if not Assigned(aExprTree) then begin
    aExprTree := @ExprTree;
    FillChar(ExprTree, SizeOf(ExprTree), 0);
    ExprTree.iVer := CANEXPRVERSION;
    ExprTree.iTotalSize := SizeOf(ExprTree);
  end;

  Result := ServerEngine.CursorSetFilter(CursorID,
                                         aExprTree,
                                         aTimeout);
end;
{--------}
procedure TffDataSet.SetBookmarkData(aBuffer : PChar; aData : Pointer);
begin
  Move(aData^, aBuffer[dsBookmarkOfs], BookmarkSize);
end;
{--------}
procedure TffDataSet.SetBookmarkFlag(aBuffer : PChar; aValue : TBookmarkFlag);
begin
  PDataSetRecInfo(aBuffer + dsRecInfoOfs).riBookmarkFlag := aValue;
end;
{--------}
procedure TffDataSet.SetFieldData(aField : TField; aBuffer : Pointer);
var
  RecBuf : PChar;
  FDI    : TffFieldDescItem;
  Status : TffResult;
begin
  with aField do begin
    if not (State in dsWriteModes) then
      RaiseFFErrorObj(Self, ffdse_TblNotEditing);
    if not GetActiveRecBuf(RecBuf) then
      RaiseFFErrorObj(Self, ffdse_TblCantGetBuf);
    if (FieldNo > 0) then begin
      if (State = dsCalcFields) then
        RaiseFFErrorObj(Self, ffdse_TblCalcFlds);
      if ReadOnly and
         (not (State in [dsSetKey, dsFilter])) then
        RaiseFFErrorObj(Self, ffdse_TblReadOnlyEdit);
      Validate(aBuffer);
      if (FieldKind <> fkInternalCalc) then begin
        if (RecBuf = nil) then
          Status := DBIERR_INVALIDPARAM
        else begin
          if dsGetFieldDescItem(FieldNo, FDI) then
            Status := dsTranslatePut(FDI, RecBuf, aBuffer)
          else
            Status := DBIERR_OUTOFRANGE;
        end;
        Check(Status);
      end;
    end
    else {FieldNo = 0; ie fkCalculated, fkLookup} begin
      inc(RecBuf, dsCalcFldOfs + offset);
      Boolean(RecBuf[0]) := LongBool(aBuffer);
      if Boolean(RecBuf[0]) then
        Move(aBuffer^, RecBuf[1], DataSize);
    end;
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, Longint(aField));
  end;
end;
{--------}
procedure TffBaseTable.SetFieldData(aField : TField; aBuffer : Pointer);
begin
  with aField do begin
    if (State = dsSetKey) and
       ((FieldNo < 0) or
        (IndexFieldCount > 0) and (not IsIndexField)) then
      RaiseFFErrorObj(Self, ffdse_TblFldNotInIndex);
  end;
  inherited SetFieldData(aField, aBuffer);
end;
{--------}
procedure TffDataSet.SetFiltered(Value : Boolean);
begin
  if not Active then
    inherited SetFiltered(Value)
  else begin
    CheckBrowseMode;
    if (Filtered <> Value) then begin
      if (not Value) or dsFilterResync then
        InternalFirst;
      if Value then
        dsActivateFilters
      else
        dsDeactivateFilters;
      inherited SetFiltered(Value);
      if (not Value) or dsFilterResync then
        First;
    end;
  end;
end;
{--------}
procedure TffBaseTable.SetFiltered(Value : Boolean);
begin
  if not Active then
    inherited SetFiltered(Value)
  else begin
    CheckBrowseMode;
    if (Filtered <> Value) then begin
      btDestroyLookupCursor;
      inherited SetFiltered(Value);
    end;
  end;
end;
{Begin !!.03}
{--------}
procedure TffBaseTable.dsActivateFilters;
begin
  inherited;
  btDestroyLookupCursor;
end;
{--------}
procedure TffBaseTable.dsDeactivateFilters;
begin
  inherited;
  btDestroyLookupCursor;
end;                                                                
{End !!.03}
{--------}
procedure TffDataSet.SetFilterOptions(Value : TFilterOptions);
begin
  dsSetFilterTextAndOptions(Filter, Value, dsFilterEval,
                            dsFilterTimeOut);
end;
{--------}
procedure TffDataSet.SetFilterText(const Value : string);
begin
  dsSetFilterTextAndOptions(Value, FilterOptions, dsFilterEval,
                            dsFilterTimeOut);
  { If the new filter string is blank, we may need to reset the Filtered flag }                          
  if (Value = '') and Filtered then
    Filtered := False;
end;
{--------}
procedure TffBaseTable.SetKey;
begin
  btSetKeyBuffer(ketNormal, True);
end;
{--------}
procedure TffDataSet.SetName(const NewName : TComponentName);
begin
  inherited SetName(NewName);

  dsProxy.Name := NewName + '_Proxy';
end;
{--------}
procedure TffDataSet.SetOnFilterRecord(const Value : TFilterRecordEvent);
begin
  {if there is no change there's nothing to do}
  if (@Value = @OnFilterRecord) then
    Exit;
  {if the table is active...}
  if Active then begin
    CheckBrowseMode;
    {firstly drop the current function filter}
    if (dsFuncFilter <> nil) then begin
      Check(dsDropFilter(dsFuncFilter));
      dsFuncFilter := nil;
    end;
    {if the filter function is not nil...}
    if Assigned(Value) then begin
      {add the new function}
      dsAddFuncFilter(@TffBaseTable.dsOnFilterRecordCallback);
      {activate it}
      if Filtered then
        Check(dsActivateFilter(dsFuncFilter));
    end;

    {call our ancestor}
    inherited SetOnFilterRecord(Value);

    {if the table is being filtered, go to the start}
    if Filtered then
      First;
  end
  else {table is not active} begin
    {call our ancestor}
    inherited SetOnFilterRecord(Value);
  end;
end;
{--------}
procedure TffBaseTable.SetRange(const aStartValues, aEndValues: array of const);
begin
  CheckBrowseMode;
  btSetKeyFields(ketRangeStart, aStartValues);
  btSetKeyFields(ketRangeEnd, aEndValues);
  ApplyRange;
end;
{--------}
procedure TffBaseTable.SetRangeEnd;
begin
  btSetKeyBuffer(ketRangeEnd, True);
end;
{--------}
procedure TffBaseTable.SetRangeStart;
begin
  btSetKeyBuffer(ketRangeStart, True);
end;
{--------}
function TffDataSet.SetTableAutoIncValue(const aValue: TffWord32) : TffResult;
begin
  Result := ServerEngine.TableSetAutoInc(CursorID,
                                         aValue);
end;
{--------}
function TffDataset.Exists : Boolean;
begin
  Result := Active;
  if Result or (TableName = '') then Exit;

  dsEnsureDatabaseOpen(True);                                          {!!.11}
  Result := Database.TableExists(TableName);
end;
{--------}
procedure TffDataSet.dsActivateFilters;
begin
  {activate the server side filter}
  if (dsFilterEval = ffeServer) then
    dsSetServerSideFilter(Filter, FilterOptions, dsFilterTimeOut);

  {activate the expression filter}
  if (dsExprFilter <> nil) then begin
    Check(dsActivateFilter(dsExprFilter));
  end;

  {activate the function filter}
  if (dsFuncFilter <> nil) then begin
    Check(dsActivateFilter(dsFuncFilter));
  end;
end;
{--------}
procedure TffDataSet.dsAddExprFilter(const aText : string;
                                   const aOpts : TFilterOptions);
var
  Parser : TExprParser;
begin
  {$IFDEF ExprParserType1}
  Parser := TExprParser.Create(Self, aText, aOpts);
  {$ENDIF}
  {$IFDEF ExprParserType2}
  Parser := TExprParser.Create(Self, aText, aOpts, [], '', nil);
  {$ENDIF}
  {$IFDEF ExprParserType3}
  Parser := TExprParser.Create(Self, aText, aOpts, [], '', nil, FldTypeMap);
  {$ENDIF}
  try
    Check(dsAddFilter(0, 0, False,
                      PCANExpr(Parser.FilterData),
                      nil, dsExprFilter));
  finally
    Parser.Free;
  end;
end;
{--------}
procedure TffDataSet.dsAddFieldDesc(aFieldDesc : PffFieldDescriptor;
                                    aFieldNo   : Integer);
var
  BDEType    : Word;
  BDESubType : Word;
  BDESize    : Word;
  VCLType    : TFieldType;
  {$IFDEF CBuilder3}
  FieldDef   : TFieldDef;
  {$ENDIF}
begin
  with aFieldDesc^ do begin
    {convert the ff type to the nearest BDE logical one}
    MapffTypeToBDE(fdType, fdLength, BDEType, BDESubType, BDESize);
    {convert the BDE logical type to a VCL type}
    VCLType := DataTypeMap[BDEType];
    {qualify the VCL type, if required}
    case VCLType of
      ftInteger :
        if (BDESubType = fldstAUTOINC) then
          VCLType := ftAutoInc;
      ftFloat :
        if (BDESubType = fldstMONEY) then
          VCLType := ftCurrency;
      ftBLOB :
        VCLType := BlobTypeMap[BDESubType];
    end;
    {create the new field definition}
    if (VCLType <> ftUnknown) then begin
      if (VCLType <> ftString) and
         (VCLType <> ftBytes) and
         (VCLType <> ftBCD) then
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
    end;
  end;
end;
{--------}
procedure TffDataSet.dsAddFuncFilter(aFilterFunc : pfGENFilter);
begin
  Check(dsAddFilter(Integer(Self), 0, False, nil, aFilterFunc, dsFuncFilter));
end;
{--------}
function TffDataSet.dsCancelServerFilter: Boolean;
begin
  Result := False;
  if Assigned(dsOnServerFilterTimeout) then
    dsOnServerFilterTimeout(Self, Result);
end;
{------}
procedure TffBaseTable.dsAllocKeyBuffers;
var
  i : TffKeyEditType;
begin
  FFGetMem(btKeyBuffers, sizeof(Pointer) * succ(ord(High(TffKeyEditType))));
  for i := Low(TffKeyEditType) to High(TffKeyEditType) do begin
    FFGetMem(PKeyBuffers(btKeyBuffers)^[i], btKeyBufSize);
    btInitKeyBuffer(PKeyBuffers(btKeyBuffers)^[i]);
  end;
end;
{--------}
procedure TffBaseTable.btFreeKeyBuffers;
var
  i : TffKeyEditType;
begin
  if (btKeyBuffers <> nil) then begin
    for i := Low(TffKeyEditType) to High(TffKeyEditType) do begin
      if (PKeyBuffers(btKeyBuffers)^[i] <> nil) then
         FFFreeMem(PKeyBuffers(btKeyBuffers)^[i], btKeyBufSize);
    end;
     FFFreeMem(btKeyBuffers, sizeof(Pointer) * succ(ord(High(TffKeyEditType))));
    btKeyBuffers := nil;
  end;
  btKeyBuffer := nil;
end;
{--------}
procedure TffBaseTable.btChangeHandleIndex;
var
  IdxName  : string;
begin
  IndexDefs.Updated := False;
  if btIndexByName then
    btRetrieveIndexName(btIndexName, True, IdxName)
  else
    btRetrieveIndexName(btIndexFieldStr, False, IdxName);
  if (IdxName <> '') then begin
    try
      btSwitchToIndexEx(CursorID, IdxName, btIndexID, False);
    except
      Check(ServerEngine.CursorClose(CursorID));
      TableState := TblClosed;
      dsCursorID := 0;
      btRangeStack.Clear;
      raise;
    end;
  end;
end;
{--------}
procedure TffBaseTable.btCheckKeyEditMode;
begin
  if (State <> dsSetKey) then
    RaiseFFErrorObj(Self, ffdse_TblChkKeyNoEdit)
end;
{--------}
procedure TffBaseTable.dsCheckMasterRange;
begin
  if btMasterLink.Active and (btMasterLink.Fields.Count > 0) then begin
    btSetLinkRange(btMasterLink.Fields);
    btSetRange;
  end;
end;
{--------}
procedure TffDataSet.dsClearServerSideFilter;
begin
  SetFilterEx(nil, 0);
end;
{--------}
procedure TffDataSet.dsCloseViaProxy;
begin
  if not dsClosing then
    Close;
end;
{--------}
function TffDataSet.dsCreateHandle : TffCursorID;
begin
  if (TableName = '') then
    RaiseFFErrorObj(Self, ffdse_TblNoName);
  Result := GetCursorHandle('');
end;
{--------}
function TffDataSet.dsCreateLookupFilter(aFields  : TList;
                                   const aValues  : Variant;
                                         aOptions : TLocateOptions): HDBIFilter;
var
  i     : Integer;
  Filter: TFilterExpr;
  Tree  : PExprNode;
  Node  : PExprNode;
  FilterOptions: TFilterOptions;
begin
  {calculate the filter options}
  if (loCaseInsensitive in aOptions) then
    FilterOptions := [foNoPartialCompare, foCaseInsensitive]
  else
    FilterOptions := [foNoPartialCompare];
  {create the filter expression tree}

  {$IFDEF ExprParserType1}
  Filter := TFilterExpr.Create(Self, FilterOptions);
  {$ENDIF}
  {$IFDEF ExprParserType2}
  Filter := TFilterExpr.Create(Self, FilterOptions, [], '', nil);
  {$ENDIF}
  {$IFDEF ExprParserType3}
  Filter := TFilterExpr.Create(Self, FilterOptions, [], '', nil, FldTypeMap);
  {$ENDIF}

  try
    {add the nodes}
    {if there's just one field value, do it separately}
    if (aFields.Count = 1) then begin
      {$IFDEF ExprParserType3}
      Node := Filter.NewCompareNode(TField(aFields[0]), coEQ, aValues);
      {$ELSE}
      {$IFDEF UsesBDE}
      Node := Filter.NewCompareNode(TField(aFields[0]), BDE.canEQ, aValues);
      {$ELSE}
      Node := Filter.NewCompareNode(TField(aFields[0]), canEQ, aValues);
      {$ENDIF}
      {$ENDIF}
      Tree := Node;
    end
    {if there are more than one, create a properly linked tree}
    else begin
      {$IFDEF ExprParserType3}
      Node := Filter.NewCompareNode(TField(aFields[0]), coEQ, aValues[0]);
      {$ELSE}
      {$IFDEF UsesBDE}
      Node := Filter.NewCompareNode(TField(aFields[0]), BDE.canEQ, aValues[0]);
      {$ELSE}
      Node := Filter.NewCompareNode(TField(aFields[0]), canEQ, aValues[0]);
      {$ENDIF}
      {$ENDIF}
      Tree := Node;
      for i := 1 to pred(aFields.Count) do begin
        {$IFDEF ExprParserType3}
        Node := Filter.NewCompareNode(TField(aFields[i]), coEQ, aValues[i]);
        Tree := Filter.NewNode(enOperator, coAND, UnAssigned, Tree, Node);
        {$ELSE}
        {$IFDEF UsesBDE}
        Node := Filter.NewCompareNode(TField(aFields[i]), BDE.canEQ, aValues[i]);
        Tree := Filter.NewNode(enOperator, BDE.CanAND, UnAssigned, Tree, Node);
        {$ELSE}
        Node := Filter.NewCompareNode(TField(aFields[i]), canEQ, aValues[i]);
        Tree := Filter.NewNode(enOperator, canAND, UnAssigned, Tree, Node);
        {$ENDIF}
        {$ENDIF}
      end;
    end;
    {if we have a partial match make sure the final node agrees}
    if (loPartialKey in aOptions) then
      Node^.FPartial := True;

    {add the filter}
    if FilterEval = ffeServer then
      Check(OverrideFilterEx(ffSrBDE.pCANExpr(Filter.GetFilterData(Tree)),
                             FilterTimeOut))
    else begin
      Check(dsAddFilter(0, 0, false,
                        PCANExpr(Filter.GetFilterData(Tree)),
                        nil, Result));
      dsActivateFilter(Result);
    end;

  finally
    Filter.Free;
  end;{try..finally}
end;
{--------}
procedure TffDataset.dsDeactivateFilters;
begin
  {deactivate the server side filter}
  if (dsFilterEval = ffeServer) then
    dsClearServerSideFilter;

  {deactivate the expression filter}
  if (dsExprFilter <> nil) then begin
    Check(dsDeactivateFilter(dsExprFilter));
  end;
  {deactivate the function filter}
  if (dsFuncFilter <> nil) then begin
    Check(dsDeactivateFilter(dsFuncFilter));
  end;
end;
{--------}
procedure TffBaseTable.btDecodeIndexDesc(const aIndexDesc : IDXDesc;
                                       var aName, aFields : string;
                                       var aOptions   : TIndexOptions);
var
  IndexOptions : TIndexOptions;
  i            : Integer;
begin
  with aIndexDesc do begin
    {get name}
    aName := szName;
    {get index options - use local variable for speed}
    IndexOptions := [];
    if bPrimary then
      Include(IndexOptions, ixPrimary);
    if bUnique then
      Include(IndexOptions, ixUnique);
    if bDescending then
      Include(IndexOptions, ixDescending);
    if bCaseInsensitive then
      Include(IndexOptions, ixCaseInsensitive);
    if bExpIdx or (iFldsInKey = 0) then
      Include(IndexOptions, ixExpression);
    aOptions := IndexOptions;
    {get index fields}
    if (iFldsInKey = 0) then
      aFields := ''
    else {more than one field in index key} begin
      aFields := FieldDefs[pred(aiKeyFld[0])].Name;
      for i := 1 to pred(iFldsInKey) do
        aFields := aFields + ';' +
                   FieldDefs[pred(aiKeyFld[i])].Name;
    end;
  end;
end;
{--------}
procedure TffDataSet.DestroyHandle(aHandle : TffCursorID);
begin
  {release record lock, ignore errors}
  Check(ServerEngine.RecordRelLock(CursorID,
                                   False));
  {close the cursor handle, ignore errors}
  Check(ServerEngine.CursorClose(CursorID));
  TableState := TblClosed;
  dsCursorID := 0;
end;
{--------}
procedure TffBaseTable.DestroyHandle(aHandle : TffCursorID);
begin
  {destroy the lookup cursor (if there is one)}
  btDestroyLookupCursor;

  inherited DestroyHandle(aHandle);

  btRangeStack.Clear;
end;
{--------}
procedure TffBaseTable.btDestroyLookupCursor;
begin
  if (btLookupCursorID > 0) then begin
    Check(ServerEngine.CursorClose(btLookupCursorID));
    btLookupCursorID := 0;
    btLookupKeyFields := '';
    btLookupNoCase := False;
  end;
end;
{--------}
function TffBaseTable.btDoFldsMapToCurIdx(aFields : TList;
                                      aNoCase : Boolean) : Boolean;
var
  i : Integer;
begin
  {returns whether the field list matches the current index fields}
  {assume not}
  Result := False;

  {if the case sensitivity doesn't match, exit}
  if (aNoCase <> btNoCaseIndex) then
    Exit;
  {if the field count is larger than the index's, exit}
  if (aFields.Count > btIndexFieldCount) then
    Exit;
  {check that all fields match}
  for i := 0 to pred(aFields.Count) do
    if (TField(aFields[i]).FieldNo <> btFieldsInIndex[i]) then
      Exit;
  {if we got this far, the field list is the same as the index's}
  Result := True;
end;
{--------}
function TffDataSet.dsGetFieldDescItem(iField : Integer;
                                   var FDI    : TffFieldDescItem) : Boolean;
begin
  if (FieldDescs.Count = 0) then
    dsReadFieldDescs;
  if (0 < iField) and (iField <= FieldDescs.Count) then begin
    Result := True;
    FDI := TffFieldDescItem(FieldDescs[pred(iField)]);
  end
  else {iField is out of range} begin
    Result := False;
    FDI := nil;
  end;
end;
{--------}
function TffDataSet.dsGetFieldNumber(FieldName : PChar) : Integer;
var
  i   : Integer;
  FDI : TffFieldDescItem;
begin
  Result := 0;
  if (FieldDescs.Count <> 0) then begin
    for i := 0 to pred(FieldDescs.Count) do begin
      FDI := TffFieldDescItem(FieldDescs.Items[i]);
      if (FFAnsiStrIComp(FieldName, FDI.PhyDesc^.szName) = 0) then begin {!!.06, !!.07}
        Result := FDI.FieldNumber;
        Exit;
      end;
    end;
  end;
end;
{--------}
procedure TffDataSet.dsReadFieldDescs;
var
  ffFieldDesc : PffFieldDescriptor;
  BDEPhyDesc  : FLDDesc;
  i           : Integer;
  offset      : Integer;
begin
  {destroy any existing field desc items}
  for i := Pred(FieldDescs.Count) downto 0 do
     TffFieldDescItem(FieldDescs.Items[i]).Free;

  {create a bunch of field desc items}
  for i := 0 to pred(Dictionary.FieldCount) do begin
    ffFieldDesc := Dictionary.FieldDescriptor[i];
    GetBDEFieldDescriptor(ffFieldDesc^, BDEPhyDesc);
    {note: the line below adds the new item automatically to the
           collection}
    TffFieldDescItem.Create(FieldDescs, BDEPhyDesc);
  end;
  {Now patch up the offsets for the logical field descs}
  offset := 0;
  for i := 0 to pred(Dictionary.FieldCount) do begin
    with TffFieldDescItem(FieldDescs[i]).LogDesc^ do begin
      ioffset := offset;
      inc(offset, iLen);
    end;
  end;
end;
{--------}
function TffDataSet.dsTranslateCmp(var aFirst      : TffNodeValue;
                                   var aSecond     : TffNodeValue;
                                       aIgnoreCase : Boolean;
                                       aPartLen    : Integer) : Integer;
  {------}
  function ConvertIntValue(var aNode : TffNodeValue; var C : comp) : Boolean;
  begin
    Result := True;
    with aNode do begin
      if nvIsConst then begin
        case nvType of
          fldINT16  : C := smallint(nvValue^);
          fldINT32  : C := Longint(nvValue^);
          fldUINT16 : C := Word(nvValue^);
          fldUINT32 : begin
                        C := Longint(nvValue^);
                        if (C < 0) then
                          C := C + $80000000;
                      end;
        else
          Result := False;
        end;{case}
      end
      else begin
        case TffFieldType(nvType) of
          fftByte   : C := byte(nvValue^);
          fftWord16 : C := Word(nvValue^);
          fftWord32 : begin
                        C := Longint(nvValue^);
                        if (C < 0) then
                          C := C + $80000000;
                      end;
          fftInt8   : C := shortint(nvValue^);
          fftInt16  : C := smallint(nvValue^);
          fftInt32  : C := Longint(nvValue^);
          fftAutoInc: begin
                        C := Longint(nvValue^);
                        if (C < 0) then
                          C := C + $80000000;
                      end;
          fftComp   : C := comp(nvValue^);
        else
          Result := False;
        end;{case}
      end;
    end;
  end;
  {------}
  function ConvertDateTimeValue(var aNode : TffNodeValue;
                                var DT : TDateTime) : Boolean;
  begin
    Result := True;
    with aNode do begin
      if nvIsConst then begin
        case nvType of
          fldDATE      : DT := DbiDate(nvValue^);
          fldTIME      : DT := FFClBDE.Time(nvValue^) / 86400000.0;
          fldTIMESTAMP : DT := TimeStamp(nvValue^) / 86400000.0;
        else
          Result := False;
        end;{case}
      end
      else begin
        case TffFieldType(nvType) of
          fftStDate   : DT := StDateToDateTime(TStDate(nvValue^))
                              + 693594;
          fftStTime   : DT := StTimeToDateTime(TStTime(nvValue^));
          fftDateTime : DT := TDateTime(nvValue^);
        else
          Result := False;
        end;{case}
      end;
    end;
  end;
  {------}
  function ConvertFloatValue(var aNode : TffNodeValue;
                             var F : extended) : Boolean;
  begin
    Result := True;
    with aNode do begin
      if nvIsConst then begin
        case nvType of
          fldFLOAT     : F := double(nvValue^);
          fldFLOATIEEE : F := extended(nvValue^);
        else
          Result := False;
        end;{case}
      end
      else begin
        case TffFieldType(nvType) of
          fftSingle   : F := single(nvValue^);
          fftDouble   : F := double(nvValue^);
          fftExtended : F := extended(nvValue^);
          fftCurrency : F := currency(nvValue^);
        else
          Result := False;
        end;{case}
      end;
    end;
  end;
  {------}
  function ConvertBooleanValue(var aNode : TffNodeValue;
                               var B : Boolean) : Boolean;
  begin
    Result := True;
    with aNode do begin
      if nvIsConst then begin
        case nvType of
          fldBOOL : B := WordBool(nvValue^);
        else
          Result := False;
        end;{case}
      end
      else begin
        case TffFieldType(nvType) of
          fftBoolean : B := Boolean(nvValue^);
        else
          Result := False;
        end;{case}
      end;
    end;
  end;
  {------}
  function ConvertStringValue(var aNode : TffNodeValue;
                              var P : PChar) : Boolean;
  var
    StrZ : TffStringZ;
  begin
    Result := True;
    with aNode do begin
      if nvIsConst then begin
        case nvType of
          fldZSTRING : P := nvValue;
        else
          Result := False;
        end;{case}
      end
      else begin
        case TffFieldType(nvType) of
          fftChar :
            begin
              P := StrAlloc(2);
              P[0] := char(nvValue^);
              P[1] := #0;
            end;
          fftShortString,
          fftShortAnsiStr :
            begin
              P := StrNew(StrPCopy(StrZ, ShortString(nvValue^)));
            end;
          fftNullString,
          fftNullAnsiStr :
            begin
              P := StrNew(nvValue);
            end;
        else
          Result := False;
        end;{case}
      end;
    end;
  end;
  {------}
var
  Bool1, Bool2   : Boolean;
  Comp1, Comp2   : comp;
  PChar1, PChar2 : PAnsiChar;
  DT1, DT2       : TDateTime;
  Ext1, Ext2     : extended;
begin
  {Note: there are two types of things to compare: constants and
         fields. In neither case will this routine be called with null
         values - the caller takes care of this}
  {Note: this routine doesn't have to worry about comparing dissimilar
         types (eg dates and strings); this is illegal and will have
         been already excluded by the filter parser; similarly with
         fields that can't be compared (eg, BLOBs)}
  {Note: constant values are stored as logical types, field values as
         physical types}

  {Deal with Integer types first}
  if ConvertIntValue(aFirst, Comp1) then begin
    ConvertIntValue(aSecond, Comp2);
    if (Comp1 < Comp2) then      Result := -1
    else if (Comp1 = Comp2) then Result := 0
    else                         Result := 1;
    Exit;
  end;

  {Deal with floating point types next}
  if ConvertFloatValue(aFirst, Ext1) then begin
    ConvertFloatValue(aSecond, Ext2);
    if (Ext1 < Ext2) then      Result := -1
    else if (Ext1 = Ext2) then Result := 0
    else                       Result := 1;
    Exit;
  end;

  {Deal with date/time types next}
  if ConvertDateTimeValue(aFirst, DT1) then begin
    ConvertDateTimeValue(aSecond, DT2);
    if (DT1 < DT2) then      Result := -1
    else if (DT1 = DT2) then Result := 0
    else                     Result := 1;
    Exit;
  end;

  {Deal with Boolean types next; False < True}
  if ConvertBooleanValue(aFirst, Bool1) then begin
    ConvertBooleanValue(aSecond, Bool2);
    if Bool1 then
      if Bool2 then Result := 0
      else          Result := 1
    else {Bool1 is False}
      if Bool2 then Result := -1
      else          Result := 0;
    Exit;
  end;

  {Deal with strings next}
  if ConvertStringValue(aFirst, PChar1) then begin
    ConvertStringValue(aSecond, PChar2);
    if aIgnoreCase then
      if (aPartLen = 0) then
        Result := FFAnsiStrIComp(PChar1, PChar2)                      {!!.06}{!!.07}
      else
        Result := FFAnsiStrLIComp(PChar1, PChar2, aPartLen)           {!!.06}{!!.07}
    else
      if (aPartLen = 0) then
        Result := AnsiStrComp(PChar1, PChar2)                         {!!.06}
      else
        Result := AnsiStrLComp(PChar1, PChar2, aPartLen);             {!!.06}
    if not aFirst.nvIsConst then
      StrDispose(PChar1);
    if not aSecond.nvIsConst then
      StrDispose(PChar2);
    Exit;
  end;

  {otherwise just compare the bytes}
  Result := ffCmpBytes(PffByteArray(aFirst.nvValue),
                       PffByteArray(aSecond.nvValue),
                       ffMinI(aFirst.nvSize, aSecond.nvSize));
end;
{------}
function TffDataSet.dsTranslateGet(FDI      : TffFieldDescItem;
                                   pRecBuff : Pointer;
                                   pDest    : Pointer;
                               var bBlank   : Boolean) : TffResult;
begin
  Result := DBIERR_NONE;
  if (pRecBuff = nil) then
    Result := DBIERR_INVALIDPARAM
  else {pRecBuff is non-nil} begin
    bBlank := Dictionary.IsRecordFieldNull(pred(FDI.FieldNumber), pRecBuff);
    if (pDest = nil) then
      Result := DBIERR_NONE
    else {there is somewhere to xlat data into, if needed} begin
      if bBlank then begin
        Result := DBIERR_NONE;
        if (XltMode = xltField) then
          FillChar(pDest^, FDI.LogDesc^.iLen, 0)
        else {no translation}
          FillChar(pDest^, FDI.PhyDesc^.iLen, 0)
      end
      else {field is not blank} begin
        if (XltMode <> xltField) {no translation} then begin
          with FDI.PhyDesc^ do
            Move(PffByteArray(pRecBuff)^[ioffset], pDest^, iLen);
        end
        else {field must be translated} begin
          with FDI.PhyDesc^ do begin
            inc(PAnsiChar(pRecBuff), ioffset);
            if MapffDataToBDE(TffFieldType(iFldType),
                              iLen,
                              pRecBuff,
                              pDest) then
              Result := DBIERR_NONE
            else
              Result := DBIERR_INVALIDXLATION;
          end;
        end;
      end;
    end;
  end;
end;
{--------}
function TffDataSet.dsTranslatePut(FDI      : TffFieldDescItem;
                                   pRecBuff : Pointer;
                                   pSrc     : Pointer) : TffResult;
begin
  if (pRecBuff = nil) then
    Result := DBIERR_INVALIDPARAM
  else {pRecBuff is non-nil} begin
    if (pSrc = nil) {this means set field to null} then begin
      Dictionary.SetRecordFieldNull(pred(FDI.FieldNumber), pRecBuff, True);
      Result := DBIERR_NONE;
    end
    else {pSrc is non-nil} begin
      Dictionary.SetRecordFieldNull(pred(FDI.FieldNumber), pRecBuff, False);
      if (XltMode <> xltField) {no translation} then begin
        with FDI.PhyDesc^ do
          Move(pSrc^, PffByteArray(pRecBuff)^[ioffset], iLen);
        Result := DBIERR_NONE;
      end
      else {field must be translated} begin
        with FDI.PhyDesc^ do begin
          inc(PAnsiChar(pRecBuff), ioffset);
          if MapBDEDataToff(TffFieldType(iFldType), iLen, pSrc, pRecBuff) then
            Result := DBIERR_NONE
          else
            Result := DBIERR_INVALIDXLATION;
        end;
      end;
    end;
  end;
end;
{--------}
procedure TffDataSet.dsDropFilters;
begin
  {drop the expression filter}
  if (dsExprFilter <> nil) then begin
    Check(dsDropFilter(dsExprFilter));
    dsExprFilter := nil;
  end;
  {drop the function filter}
  if (dsFuncFilter <> nil) then begin
    Check(dsDropFilter(dsFuncFilter));
    dsFuncFilter := nil;
  end;
end;
{--------}
function TffDataSet.dsMatchesFilter(pRecBuff : Pointer) : Boolean;
var
  i    : Integer;
  Filt : TffFilterListItem;
begin
  Result := False;
  if (pRecBuff = nil) then
    Exit;
  if dsFilterActive then begin
    for i := 0 to pred(dsFilters.Count) do begin
      Filt := TffFilterListItem(dsFilters.Items[i]);
      if (Filt <> nil) then
        if not Filt.MatchesRecord(pRecBuff) then
          Exit;
    end;
  end;
  Result := True;
end;
{--------}
procedure TffBaseTable.btEndKeyBufferEdit(aCommit : Boolean);
begin
  DataEvent(deCheckBrowseMode, 0);
  if aCommit then
    PKeyRecInfo(PChar(btKeyBuffer) + btKeyInfoOfs)^.kriModified := Modified
  else {rollback}
    Move(PKeyBuffers(btKeyBuffers)^[ketSaved]^, btKeyBuffer^, btKeyBufSize);
  SetState(dsBrowse);
  DataEvent(deDataSetChange, 0);
end;
{--------}
procedure TffDataSet.dsEnsureDatabaseOpen(aValue : Boolean);
  {Note: this routine exists in order that the table object can ensure
         that it's database parent is open before something happens
         that requires it open. For example, you can get an index list
         for a table before opening it - to do this requires that the
         database is opened automatically first. }
var
  DB : TffDatabase;
begin
  if (dsProxy.Session = nil) then
    dsProxy.tpResolveSession;
  DB := TffDatabase(Database);
  if (DB = nil) then
    RaiseFFErrorObj(Self, ffdse_TblBadDBName);
  if aValue then
    DB.Active := True;
end;
{--------}
function TffDataSet.GetCursorProps(var aProps : TffCursorProps) : TffResult;
var
  i : Integer;
begin
  FillChar(aProps, SizeOf(TffCursorProps), 0);
  aProps.TableName := TableName;
  aProps.FileNameSize :=ffcl_Path + 1 + ffcl_FileName + 1 + ffcl_Extension;
  aProps.FieldsCount := Dictionary.FieldCount;
  { Record size (logical record) }
  if (XltMode = xltField) then
    with TffFieldDescItem(FieldDescs[pred(FieldDescs.Count)]).LogDesc^ do
      aProps.RecordSize := ioffset + iLen
    else
      aProps.RecordSize := PhysicalRecordSize;
  { Record size (physical record) }
  aProps.RecordBufferSize := PhysicalRecordSize;
  aprops.ValChecks := 0;
  with Dictionary do begin
    for i := 0 to pred(FieldCount) do
      if FieldRequired[i] or (FieldVCheck[i] <> nil) then
        inc(aProps.ValChecks);
  end;
  aProps.BookMarkSize := Dictionary.BookmarkSize[0];
  aProps.BookMarkStable := True;
  aProps.OpenMode := OpenMode;
  aProps.ShareMode := ShareMode;
  aProps.Indexed := True;
  aProps.xltMode := XltMode;
  aProps.TblRights := prvUNKNOWN;
  aProps.Filters := Filters.Count;
  Result := DBIERR_NONE;
end;
{--------}
function TffBaseTable.GetCursorProps(var aProps : TffCursorProps) : TffResult;
begin
  Result := inherited GetCursorProps(aProps);
  aProps.KeySize := Dictionary.IndexKeyLength[IndexID];
  aProps.IndexCount := Dictionary.IndexCount;
  aProps.BookMarkSize := Dictionary.BookmarkSize[IndexID];
end;
{--------}

function TffDataSet.dsGetNextRecord(eLock     : TffLockType;
                                    pRecBuff  : Pointer;
                                    RecProps  : pRECProps) : TffResult;
var
  FoundNext : Boolean;
  CreatedBuffer : Boolean;
begin
  if (pRecBuff <> nil) then
    CreatedBuffer := False
  else begin
    FFGetMem(pRecBuff, PhysicalRecordSize);
    CreatedBuffer := True;
  end;
  FoundNext := False;
  Result := dsGetNextRecordPrim(CursorID, ffltNOLOCK, pRecBuff, RecProps);
  while (Result = DBIERR_NONE) and (not FoundNext) do begin
    if dsMatchesFilter(pRecBuff) then begin
      FoundNext := True;
      if (eLock <> ffltNOLOCK) then
        Result := dsGetRecordPrim(eLock, nil, nil);
    end
    else
      Result := dsGetNextRecordPrim(CursorID, ffltNOLOCK, pRecBuff, RecProps);
  end;
  if CreatedBuffer then
     FFFreeMem(pRecBuff, PhysicalRecordSize);
end;
{--------}
function TffDataSet.dsGetNextRecordPrim(aCursorID : TffCursorID;
                                        eLock     : TffLockType;
                                        pRecBuff  : Pointer;
                                        RecProps  : pRECProps) : TffResult;
begin
  repeat
    Result := ServerEngine.RecordGetNext(aCursorID,
                                         eLock,
                                         pRecBuff);
    if Result = DBIERR_ff_FilterTimeout then begin
      if dsCancelServerFilter then
        break;
    end else
      break;
  until False;
  if (RecProps <> nil) then
    FillChar(RecProps^, sizeof(RECProps), 0);
end;
{------}
function TffDataSet.GetActiveRecBuf(var aRecBuf : PChar): Boolean;
begin
  Result := True;
  case State of
    dsBrowse :
      if IsEmpty then begin
        aRecBuf := nil;
        Result := False;
      end
      else
        aRecBuf := ActiveBuffer;
    dsEdit,
    dsInsert :
      aRecBuf := ActiveBuffer;
    dsCalcFields :
      aRecBuf := CalcBuffer;
    dsFilter :
      aRecBuf := dsRecordToFilter;
    dsOldValue :
      begin
        aRecBuf := dsOldValuesBuffer;
        Result := Assigned(aRecBuf);
      end;
  else
    aRecBuf := nil;
    Result := False;
  end;
end;
{--------}
function TffBaseTable.GetActiveRecBuf(var aRecBuf : PChar): Boolean;
begin
  Result := True;
  case State of
    dsSetKey :
      aRecBuf := PChar(btKeyBuffer);
  else
    Result := inherited GetActiveRecBuf(aRecBuf);
  end;
end;
{--------}
function TffDataSet.GetCursorHandle(aIndexName : string) : TffCursorID;
var
  RetCode : TffResult;
  Stream  : TStream;
  OpenCursorID : Longint;
  OpenIndexID  : Longint;
begin
  {try to open the table}
  Stream := TMemoryStream.Create;
  try
    RetCode := ServerEngine.TableOpen(Database.DatabaseID,
                                      TableName,
                                      False,
                                      '', { IndexName}
                                      0,
                                      TffOpenMode(not ReadOnly),
                                      TffShareMode(not Exclusive),
                                      dsGetTimeOut,
                                      Result,
                                      Stream);
    if RetCode = DBIERR_NONE then begin
      Stream.Position := 0;
      Stream.Read(OpenCursorID, SizeOf(OpenCursorID));
      {save the data dictionary for this table as well}
      Dictionary.ReadFromStream(Stream);
      Stream.Read(OpenIndexID, SizeOf(OpenIndexID));
      dsReadFieldDescs;
     end else
       Result := 0;
  finally
    Stream.Free;
  end;

  {if we failed, but the error was 'table is readonly', try to open
   the table in that mode; switch the internal ReadOnly flag}
  if (RetCode = DBIERR_TABLEREADONLY) then begin
    if dsReadOnly then
      RaiseFFErrorObj(Self, ffdse_TblBadReadOnly);
    dsReadOnly := True;
    Result := GetCursorHandle(aIndexName);
    RetCode := DBIERR_NONE;
  end;
  {finally check the return code}
  Check(RetCode);
end;
{--------}
function TffBaseTable.GetCursorHandle(aIndexName : string) : TffCursorID;
var
  RetCode : TffResult;
  Stream  : TStream;
  OpenCursorID : Longint;
  OpenIndexID  : Longint;
begin
  {try to open the table}
  Stream := TMemoryStream.Create;
  try
    RetCode := ServerEngine.TableOpen(Database.DatabaseID,
                                      TableName,
                                      False,
                                      IndexName,
                                      0,
                                      TffOpenMode(not ReadOnly),
                                      TffShareMode(not Exclusive),
                                      dsGetTimeOut,
                                      Result,
                                      Stream);
    if RetCode = DBIERR_NONE then begin
      Stream.Position := 0;
      Stream.Read(OpenCursorID, SizeOf(OpenCursorID));
      {save the data dictionary for this table as well}
      Dictionary.ReadFromStream(Stream);
      Stream.Read(OpenIndexID, SizeOf(OpenIndexID));
      btIndexID := OpenIndexID;
      btIndexName := Dictionary.IndexName[OpenIndexID];
      dsReadFieldDescs;
     end else
       Result := 0;
  finally
    Stream.Free;
  end;
  {if we failed, but the error was 'table is readonly', try to open
   the table in that mode; switch the internal ReadOnly flag}
  if (RetCode = DBIERR_TABLEREADONLY) then begin
    if dsReadOnly then
      RaiseFFErrorObj(Self, ffdse_TblBadReadOnly);
    dsReadOnly := True;
    Result := GetCursorHandle(aIndexName);
    RetCode := DBIERR_NONE;
  end;
  {finally check the return code}
  Check(RetCode);
end;
{--------}
function TffDataSet.dsGetDatabase : TffBaseDatabase;
begin
  Result := dsProxy.Database;
end;
{--------}
function TffDataSet.dsGetDatabaseName : string;
begin
  Result := dsProxy.DatabaseName;
end;
{Begin !!.11}
{--------}
function TffBaseTable.btGetFFVersion : string;
var
  Version : Longint;
begin
  Check(ServerEngine.TableVersion(Database.DatabaseID,
                                  dsGetTableName, Version));
  Result := Format('%5.4f', [Version / 10000.0]);
end;
{End !!.11}
{--------}
function TffBaseTable.btGetIndexField(aInx : Integer) : TField;
var
  FieldNo : Integer;
begin
  if (aInx < 0) or (aInx >= IndexFieldCount) then
    RaiseFFErrorObj(Self, ffdse_TblIdxFldRange);
  FieldNo := btFieldsInIndex[aInx];
  Result := FieldByNumber(FieldNo);
  if (Result = nil) then
    RaiseFFErrorObj(Self, ffdse_TblIdxFldMissing);
end;
{--------}
function TffBaseTable.btGetIndexFieldNames : string;
begin
  if btIndexByName then
    Result := ''
  else
    Result := btIndexFieldStr;
end;
{--------}
procedure TffDataset.dsGetIndexInfo;
begin
  { do nothing }
end;
{--------}
procedure TffDataset.dsAllocKeyBuffers;
begin
  { do nothing }
end;
{--------}
procedure TffDataset.dsCheckMasterRange;
begin
  { do nothing }
end;
{--------}
procedure TffBaseTable.dsGetIndexInfo;
var
  i         : Integer;
  IndexDesc : IDXDesc;
begin
  if (btGetIndexDesc(0, IndexDesc) = DBIERR_NONE) then begin
    btNoCaseIndex := IndexDesc.bCaseInsensitive;
    btIndexFieldCount := IndexDesc.iFldsInKey;
    FillChar(btFieldsInIndex, sizeof(btFieldsInIndex), 0);
    for i := 0 to pred(IndexDesc.iFldsInKey) do
      btFieldsInIndex[i] := IndexDesc.aiKeyFld[i];
    btKeyLength := IndexDesc.iKeyLen;
    btKeyInfoOfs := dsPhyRecSize;
    btKeyBufSize := btKeyInfoOfs + sizeof(TKeyRecInfo);
  end;
end;
{--------}
function TffBaseTable.btGetIndexDesc(iIndexSeqNo : Word;
                             var idxDesc     : IDXDesc) : TffResult;
begin
  FillChar(idxDesc, sizeof(idxDesc), 0);

  {note: BDE index sequence numbers are 1-based, 0 means 'current
         index'}
  if (iIndexSeqNo = 0) then
    iIndexSeqNo := IndexID
  else
    dec(iIndexSeqNo);

  {check to be sure it is a valid index id}
  if iIndexSeqNo >= Dictionary.IndexCount then
    Result := DBIERR_NOSUCHINDEX
  else begin
    GetBDEIndexDescriptor(Dictionary.IndexDescriptor[iIndexSeqNo]^, idxDesc);
    Result := DBIERR_NONE;
  end;
end;
{--------}
function TffBaseTable.btGetIndexDescs(Desc : pIDXDesc) : TffResult;
var
  IDA   : PffIDXDescArray absolute Desc;
  Props : TffCursorProps;
  i     : Word;
begin
  Result := GetCursorProps(Props);
  if (Result = DBIERR_NONE) then begin
    for i := 1 to Props.IndexCount do begin
      Result := btGetIndexDesc(i, IDA^[pred(i)]);
      if not (Result = DBIERR_NONE) then begin
        Exit;
      end;
    end;
  end;
end;
{--------}
function TffBaseTable.btGetIndexName : string;
begin
  if btIndexByName then
    Result := btIndexName
  else
    Result := '';
end;
{--------}
function TffBaseTable.btGetKeyExclusive : Boolean;
begin
  btCheckKeyEditMode;
  Result := PKeyRecInfo(PChar(btKeyBuffer) + btKeyInfoOfs)^.kriExclusive;
end;
{--------}
function TffBaseTable.btGetKeyFieldCount : Integer;
begin
  btCheckKeyEditMode;
  Result := PKeyRecInfo(PChar(btKeyBuffer) + btKeyInfoOfs)^.kriFieldCount;
end;
{--------}
function TffBaseTable.btGetLookupCursor(const aKeyFields : string;
                                          aNoCase    : Boolean) : TffCursorID;
var
  KeyIndex  : TIndexDef;
  RangeStart     : PChar;
  RangeEnd       : PChar;
  RangeStartInfo : PKeyRecInfo;
  RangeEndInfo   : PKeyRecInfo;
  TmpInt    : Integer;
  TmpStr    : string;
begin
  {create a new cursor only if something has changed}
  if (aKeyFields <> btLookupKeyFields) or
     (aNoCase <> btLookupNoCase) then begin
    {destroy the old cursor}
    btDestroyLookupCursor;


    (*Note: Case sensitivity should not matter when just interested in integer
          key fields *)
    { If a range is active then do not create a cursor.  We will handle it
      via a lookup filter. }
    RangeStart := PKeyBuffers(btKeyBuffers)^[ketCurRangeStart];
    RangeStartInfo := PKeyRecInfo(RangeStart + btKeyInfoOfs);
    RangeEnd := PKeyBuffers(btKeyBuffers)^[ketCurRangeEnd];
    RangeEndInfo := PKeyRecInfo(RangeEnd + btKeyInfoOfs);
    if (not RangeStartInfo^.kriModified) and
       (not RangeEndInfo^.kriModified) then begin
      {get the index definition for the field names}
      KeyIndex := IndexDefs.GetIndexForFields(aKeyFields, aNoCase);
      {if there was one...}
      if (KeyIndex <> nil) then begin
        {clone our handle and switch indexes}
        Check(ServerEngine.CursorClone(CursorID,
                                       omReadOnly,
                                       btLookupCursorID));
        TmpInt := 0;
        TmpStr := KeyIndex.Name;
        Check(btSwitchToIndexEx(btLookupCursorID, TmpStr, TmpInt, False));
        {save the parameters for next time}                            {!!.01}
        btLookupKeyFields := aKeyFields;                               {!!.01}
        btLookupNoCase := aNoCase;                                     {!!.01}
      end;
{Begin !!.01}
      {save the parameters for next time}
//      btLookupKeyFields := aKeyFields;
//      btLookupNoCase := aNoCase;
{End !!.01}
    end;
  end;
  Result := btLookupCursorID;
end;
{--------}
function TffBaseTable.btGetMasterFields : string;
begin
  Result := btMasterLink.FieldNames;
end;
{--------}
function TffBaseTable.btGetMasterSource : TDataSource;
begin
  Result := btMasterLink.DataSource;
end;
{--------}
procedure TffDataSet.dsGetRecordInfo(aReadProps : Boolean);
var
  CursorProps : TffCursorProps;
begin
  if aReadProps then begin
    Check(GetCursorProps(CursorProps));
    BookmarkSize := CursorProps.BookmarkSize;
    dsPhyRecSize := CursorProps.RecordBufferSize;
  end;
  dsCalcFldOfs := dsPhyRecSize;
  dsBookmarkOfs := dsCalcFldOfs + CalcFieldsSize;
  dsRecInfoOfs := dsBookmarkOfs + BookmarkSize;
  dsRecBufSize := dsRecInfoOfs + SizeOf(TDataSetRecInfo);
end;
{--------}
function TffDataSet.dsGetSession : TffSession;
begin
  Result := dsProxy.Session;
end;
{--------}
function TffDataSet.dsGetSessionName : string;
begin
  Result := dsProxy.SessionName;
end;
{--------}
function TffDataSet.dsGetTableName : string;
begin
  Result := dsProxy.TableName;
end;
{--------}
function TffDataSet.dsGetVersion : string;
begin
  Result := dsProxy.Version;
end;
{--------}
procedure TffDataSet.dsRefreshTimeout;                               {new !!.11}
begin
  if Active then
    Check(ServerEngine.CursorSetTimeout(CursorID, dsGetTimeout));
end;
{--------}
procedure TffBaseTable.btInitKeyBuffer(aBuf : Pointer);
begin
  FillChar(PKeyRecInfo(PChar(aBuf) + btKeyInfoOfs)^, sizeof(TKeyRecInfo), 0);
  Dictionary.InitRecord(aBuf);
  Dictionary.SetDefaultFieldValues(aBuf);
end;
{--------}
function TffDataSet.dsModifyRecord(aBuffer : Pointer; aRelLock : Boolean) : TffResult;
begin
  Result := ServerEngine.RecordModify(CursorID,
                                      aBuffer,
                                      aRelLock);
end;
{--------}
function TffBaseTable.btLocateRecord(const aKeyFields : string;
                                 const aKeyValues : Variant;
                                       aOptions   : TLocateOptions;
                                       aSyncCursor: Boolean): Boolean;
var
  i, FieldCount, PartialLength : Integer;
  OurBuffer    : PChar;
  OurFields    : TList;
  LookupCursor : TffCursorID;
  FilterHandle : HDBIFilter;
  Status       : TffResult;
  NoCase       : Boolean;
begin
  {make sure we're in browse mode}
  CheckBrowseMode;
  CursorPosChanged;
  {get a temporary record Buffer}
  OurBuffer := TempBuffer;
  {create list of fields}
  OurFields := TList.Create;
  try
    {get the actual fields in the parameter aKeyFields}
    GetFieldList(OurFields, aKeyFields);
    {see whether we can use an index to rapidly lookup the record}
    NoCase := loCaseInsensitive in aOptions;
    if btDoFldsMapToCurIdx(OurFields, NoCase) then
      LookupCursor := CursorID
    else
      LookupCursor := btGetLookupCursor(aKeyFields, NoCase);
    {if we have no lookup cursor, locate the record via a filter}
    if (LookupCursor = 0) then begin
      InternalFirst;
      FilterHandle := dsCreateLookupFilter(OurFields, aKeyValues, aOptions);
      Status := dsGetNextRecord(ffltNoLock, OurBuffer, nil);
      if FilterEval = ffeServer then
        RestoreFilterEx
      else
        dsDropFilter(FilterHandle);
    end
    {otherwise if we do have a lookup cursor, use it}
    else begin
      {temporarily move into the filter state - this fools the field
       setting logic to fill the filter Buffer (ie, the temp Buffer)}
      SetTempState(dsFilter);
      dsRecordToFilter := OurBuffer;
      try
        {initialize the Buffer we're using}
        Dictionary.InitRecord(PffByteArray(OurBuffer));
        Dictionary.SetDefaultFieldValues(PffByteArray(OurBuffer));
        {set up the field values in the Buffer}
        FieldCount := OurFields.Count;
        if FieldCount = 1 then
          TField(OurFields[0]).Value := aKeyValues
        else begin
          for i := 0 to pred(FieldCount) do
            TField(OurFields[i]).Value := aKeyValues[i];
        end;
        {calculate any partial length - only counts if the last field
         is a string field}
        PartialLength := 0;
        if (loPartialKey in aOptions) and
           (TField(OurFields.Last).DataType = ftString) then begin
          dec(FieldCount);
          PartialLength := length(TField(OurFields.Last).AsString);
        end;
        {get the record for the given key in the Buffer}
        Status := btGetRecordForKey(LookupCursor, False,
                                    FieldCount,
                                    PartialLength,
                                    OurBuffer,
                                    OurBuffer);
      finally
        {reset the state to browse mode}
        RestoreState(dsBrowse);
      end;{try..finally}
      {if we have to sync up, then do so}
      if (Status = DBIERR_NONE) and
         aSyncCursor and
         (LookupCursor <> CursorID) then
        Status := ServerEngine.CursorSetToCursor(CursorID,
                                                 btLookupCursorID);
    end;
  finally
    OurFields.Free;
  end;{try..finally}

  { check the result, raise an error if a timeout occurred }     {begin !!.11}
  case Status of
    DBIERR_FF_FilterTimeout,
    DBIERR_FF_ReplyTimeout,
    DBIERR_FF_Timeout,
    DBIERR_FF_GeneralTimeout :
      begin
        Result := False;  //needed to avoid compiler warning
        Check(Status);
      end;
  else
    Result := (Status = DBIERR_NONE);
  end;                                                             {end !!.11}
end;
{--------}
procedure TffBaseTable.btMasterChanged(Sender : TObject);
begin
  CheckBrowseMode;
  btSetLinkRange(btMasterLink.Fields);
  ApplyRange;
end;
{--------}
procedure TffBaseTable.btMasterDisabled(Sender : TObject);
begin
  CancelRange;
end;
{--------}
function TffDataSet.dsOnFilterRecordCallback({ulClientData = Self}
                                            pRecBuf      : Pointer;
                                            iPhyRecNum   : Longint): SmallInt;
var
  Accept    : Boolean;
  SaveState : TDataSetState;
begin
  SaveState := SetTempState(dsFilter);
  try
    Accept := True;
    Result := Ord(Accept);
    dsRecordToFilter := pRecBuf;
    try
      if Assigned(OnFilterRecord) then
        OnFilterRecord(Self, Accept);
      Result := Ord(Accept);
    except
      raise;
    end;
    dsRecordToFilter := nil;
  finally
    RestoreState(SaveState);
  end;
end;
{--------}
function TffBaseTable.btResetRange(aCursorID : TffCursorID;
                                   SwallowSeqAccessError : Boolean) : Boolean;
var
  RangeStart     : PChar;
  RangeEnd       : PChar;
  RangeStartInfo : PKeyRecInfo;
  RangeEndInfo   : PKeyRecInfo;
begin
  RangeStart := PKeyBuffers(btKeyBuffers)^[ketCurRangeStart];
  RangeStartInfo := PKeyRecInfo(RangeStart + btKeyInfoOfs);
  RangeEnd := PKeyBuffers(btKeyBuffers)^[ketCurRangeEnd];
  RangeEndInfo := PKeyRecInfo(RangeEnd + btKeyInfoOfs);
  if (not RangeStartInfo^.kriModified) and
     (not RangeEndInfo^.kriModified) then
    Result := False
  else begin
    btResetRangePrim(aCursorID, SwallowSeqAccessError);
    btInitKeyBuffer(RangeStart);
    btInitKeyBuffer(RangeEnd);
    btDestroyLookupCursor;
    Result := True;
  end;
end;
{--------}
procedure TffBaseTable.btResetRangePrim(aCursorID : TffCursorID;
                                       SwallowSeqAccessError : Boolean);
var
  Status         : TffResult;
begin
  Status := ServerEngine.CursorResetRange(aCursorID);
  if (Status <> DBIERR_NONE) then begin
    if (Status <> DBIERR_NOASSOCINDEX) or
       (not SwallowSeqAccessError) then
      Check(Status);
  end else begin
    btRangeStack.ClearSaved;
  end;
end;
{--------}
procedure TffBaseTable.btRetrieveIndexName(const aNameOrFields : string;
                                             aIndexByName  : Boolean;
                                         var aIndexName    : string);
var
  Inx : Integer;
begin
  if (aNameOrFields <> '') then begin
    UpdateIndexDefs;
    if aIndexByName then begin
      Inx := IndexDefs.IndexOf(aNameOrFields);
      if (Inx = -1) then
        Check(DBIERR_NOSUCHINDEX);
      aIndexName := aNameOrFields;
    end
    else begin
      aIndexName := IndexDefs.FindIndexForFields(aNameOrFields).Name;
    end;
  end;
end;
{--------}
procedure TffDataSet.dsSetDatabaseName(const aValue : string);
begin
  if (csReading in ComponentState) then
    dsProxy.LoadingFromStream := True;
  dsProxy.DatabaseName := aValue;
  if Active then
    DataEvent(dePropertyChange, 0);
end;
{--------}
procedure TffDataSet.dsSetExclusive(const aValue : Boolean);
begin
  dsProxy.CheckInactive(True);

  if (csLoading in ComponentState) then begin
    dsExclusive := aValue;
    Exit;
  end;

  if (dsProxy.Database <> nil) and dsProxy.Database.Exclusive then
    dsExclusive := True
  else
    dsExclusive := aValue;
end;
{--------}
procedure TffDataSet.dsSetFilterEval(const aMode : TffFilterEvaluationType);

begin
  dsSetFilterTextAndOptions(Filter, FilterOptions, aMode,
                            dsFilterTimeOut);
end;
{--------}
procedure TffDataSet.dsSetFilterTextAndOptions(const aText : string;
                                               const aOpts : TFilterOptions;
                                               const aMode : TffFilterEvaluationType;
                                               const atimeOut : TffWord32);
begin
  {if there is no change there's nothing to do}
  if (Filter = aText) and (FilterOptions = aOpts) and
     (dsFilterEval = aMode) and (dsFilterTimeOut = atimeOut) then
    Exit;

  {if the table is active...}
  if Active then begin
    CheckBrowseMode;

    { Determine whether or not we have to clear an existing filter. }
    case dsFilterEval of
      ffeLocal :
        {firstly drop the current expression filter}
        if (dsExprFilter <> nil) then begin
          Check(dsDropFilter(dsExprFilter));
          dsExprFilter := nil;
        end;
      ffeServer :
        if aMode = ffeLocal then begin
          dsClearServerSideFilter;
        end;
    end;  { case }

    dsFilterEval := aMode;
    dsFilterTimeOut := atimeOut;

    {call our ancestor}
    inherited SetFilterText(aText);

    { If a filter is being set then create the new filter based upon where
      it is to be evaluated. }
    if (aText <> '') then begin
      if aMode = ffeLocal then begin
        {add the new expression & activate it}
        dsAddExprFilter(aText, aOpts);
        if Filtered then
          dsActivateFilter(dsExprFilter);
      end
      else if Filtered then
        dsActivateFilters;
    end;  { If have filter text }

    {call our ancestor}
    inherited SetFilterOptions(aOpts);

    {if the table is being filtered, go to the start}
    if Filtered then
      First;
  end
  else {table is not active} begin

    {call our ancestor}
    inherited SetFilterText(aText);
    inherited SetFilterOptions(aOpts);

    dsFilterEval := aMode;
    dsFilterTimeOut := atimeOut;
  end;
end;
{--------}
function TffDataSet.dsAddFilter(iClientData : Longint;
                              iPriority   : Word;
                              bCanAbort   : Bool;
                              pCANExpr    : pCANExpr;
                              pffilter    : pfGENFilter;
                          var hFilter     : hDBIFilter) : TffResult;
var
  Filter : TffFilterListItem;
begin
  Filter := TffFilterListItem.Create(dsFilters, Self,
                                     iClientData, iPriority, bCanAbort,
                                     pCANExpr, pffilter);
  hFilter := hDBIFilter(Filter);
  dsUpdateFilterStatus;
  Result := DBIERR_NONE;
end;
{--------}
function TffDataSet.dsActivateFilter(hFilter : hDBIFilter) : TffResult;
var
  i : Integer;
  Filter : TffFilterListItem;
begin
  Result := DBIERR_NONE;
  if (hFilter = nil) then begin
    for i := 0 to Pred(dsFilters.Count) do begin
      Filter := TffFilterListItem(dsFilters.Items[i]);
      if (Filter <> nil) then begin
        Filter.Active := True;
        dsFilterActive := True;
      end;
    end;
  end
  else {hFilter is an actual handle} begin
    Filter := TffFilterListItem(hFilter);
    if (dsFilters.IndexOf(Filter) <> -1) then begin
      Filter.Active := True;
      dsFilterActive := True;
    end
    else
      Result := DBIERR_NOSUCHFILTER;
  end;
end;
{--------}
function TffDataSet.dsDeactivateFilter(hFilter : hDBIFilter) : TffResult;
var
  i      : Integer;
  Filter : TffFilterListItem;
begin
  Result := DBIERR_NONE;
  if (hFilter = nil) then begin
    for i := 0 to Pred(dsFilters.Count) do begin
      Filter := TffFilterListItem(dsFilters.Items[i]);
      if (Filter <> nil) then
        Filter.Active := False;
    end;
    dsFilterActive := False;
  end
  else begin
    Filter := TffFilterListItem(hFilter);
    if (dsFilters.IndexOf(Filter) <> -1) then begin
      if Filter.Active then begin
        Filter.Active := False;
        dsUpdateFilterStatus;
      end
      else {filter wasn't active}
        Result := DBIERR_NA;
    end
    else {filter not found}
      Result := DBIERR_NOSUCHFILTER;
  end;
end;
{--------}
procedure TffDataSet.dsSetFilterTimeout(const numMS : TffWord32);
begin
  dsSetFilterTextAndOptions(Filter, FilterOptions, dsFilterEval,
                            numMS);
end;

{--------}
procedure TffBaseTable.btSetIndexField(aInx : Integer; const aValue : TField);
begin
  btGetIndexField(aInx).Assign(aValue);
end;
{--------}
procedure TffBaseTable.btSetIndexFieldNames(const aValue : string);
begin
  btSetIndexTo(aValue, aValue = '');
end;
{--------}
procedure TffBaseTable.btSetIndexName(const aValue : string);
begin
  btSetIndexTo(aValue, True);
end;
{--------}
procedure TffBaseTable.btSetIndexTo(const aParam : string; aIndexByName : Boolean);
var
  IndexName : string;
begin
  if (aIndexByName <> btIndexByName) or
     (aIndexByName and (aParam <> btIndexName)) or
     ((not aIndexByName) and (aParam <> btIndexFieldStr)) then begin
    if Active then begin
      CheckBrowseMode;
      btRetrieveIndexName(aParam, aIndexByName, IndexName);
      btSwitchToIndex(IndexName);
      dsCheckMasterRange;
    end;
    if aIndexByName then
      btIndexName := aParam
    else {indexing by list of field names} begin
      btIndexName := IndexName;
      btIndexFieldStr := aParam;
    end;
    btIndexByName := aIndexByName;
    if Active then
      Resync([]);
  end;
end;
{--------}
procedure TffBaseTable.btSetKeyBuffer(aInx : TffKeyEditType; aMustClear : Boolean);
begin
  {if the current index is not composite, raise error}
  CheckBrowseMode;
  btKeyBuffer := PKeyBuffers(btKeyBuffers)^[aInx];
  Move(btKeyBuffer^, PKeyBuffers(btKeyBuffers)^[ketSaved]^, btKeyBufSize);
  if aMustClear then
    btInitKeyBuffer(btKeyBuffer);
  SetState(dsSetKey);
  SetModified(PKeyRecInfo(PChar(btKeyBuffer) + btKeyInfoOfs)^.kriModified);
  DataEvent(deDataSetChange, 0);
end;
{--------}
procedure TffBaseTable.btSetKeyFields(aInx : TffKeyEditType;
                                const aValues : array of const);
var
  OldState : TDataSetState;
  i        : Integer;
begin
  { if the current index is not composite, raise error}                {!!.10}
  if Dictionary.IndexType[btIndexID] = itUserDefined then              {!!.10}
    raise EffDatabaseError.Create(ffStrResDataSet[ffdse_TblIdxFldMissing]); {!!.10}
  OldState := SetTempState(dsSetKey);
  try
    btKeyBuffer := PKeyBuffers(btKeyBuffers)^[aInx];
    btInitKeyBuffer(btKeyBuffer);
    for i := 0 to High(aValues) do
      btGetIndexField(i).AssignValue(aValues[i]);
    with PKeyRecInfo(PChar(btKeyBuffer) + btKeyInfoOfs)^ do begin
      kriFieldCount := High(aValues) + 1;
      kriExclusive := False;
      kriModified := Modified;
    end;
  finally
    RestoreState(OldState);
  end;{try..finally}
end;
{--------}
function TffDataSet.dsGetPhyRecSize : Integer;
begin
  Result := Dictionary.RecordLength;
end;
{--------}
function TffDataSet.dsGetPriorRecord(eLock    : TffLockType;
                                     pRecBuff : Pointer;
                                     RecProps : pRECProps) : TffResult;
var
  FoundPrior : Boolean;
  CreatedBuffer : Boolean;
begin
  if (pRecBuff <> nil) then
    CreatedBuffer := False
  else begin
    FFGetMem(pRecBuff, PhysicalRecordSize);
    CreatedBuffer := True;
  end;
  FoundPrior := False;
  Result := dsGetPriorRecordPrim(ffltNOLOCK, pRecBuff, RecProps);
  while (Result = DBIERR_NONE) and (not FoundPrior) do begin
    if dsMatchesFilter(pRecBuff) then begin
      FoundPrior := True;
      if (eLock <> ffltNOLOCK) then
        Result := dsGetRecordPrim(eLock, nil, nil);
    end
    else
      Result := dsGetPriorRecordPrim(ffltNOLOCK, pRecBuff, RecProps);
  end;
  if CreatedBuffer then
     FFFreeMem(pRecBuff, PhysicalRecordSize);
end;
{--------}
function TffDataSet.dsGetPriorRecordPrim(eLock    : TffLockType;
                                         pRecBuff : Pointer;
                                         RecProps : pRECProps) : TffResult;
begin
  repeat
    Result := ServerEngine.RecordGetPrior(CursorID,
                                          eLock,
                                          pRecBuff);
    if Result = DBIERR_ff_FilterTimeout then begin
      if dsCancelServerFilter then
        break;
    end else
      break;
  until False;
  if (RecProps <> nil) then
    FillChar(RecProps^, sizeof(RECProps), 0);
end;
{------}
function TffDataSet.dsGetRecord(eLock    : TffLockType;
                                 pRecBuff : Pointer;
                                 RecProps : pRECProps) : TffResult;
var
  CreatedBuffer : Boolean;
begin
  if (pRecBuff <> nil) then
    CreatedBuffer := False
  else begin
    FFGetMem(pRecBuff, PhysicalRecordSize);
    CreatedBuffer := True;
  end;
  Result := dsGetRecordPrim(eLock, pRecBuff, RecProps);
  if (Result = DBIERR_NONE) then begin
    if (not dsMatchesFilter(pRecBuff)) then begin
      if (eLock <> ffltNOLOCK) then
        Check(ServerEngine.RecordRelLock(CursorID,
                                         False));
      Result := DBIERR_RECNOTFOUND;
    end;
  end;
  if CreatedBuffer then
     FFFreeMem(pRecBuff, PhysicalRecordSize);
end;
{--------}
function TffDataSet.dsGetRecordCountPrim(var iRecCount : Longint) : TffResult;
var
  BM     : pointer;
  Buff   : pointer;
  Marked : Boolean;

begin
  if not FilterActive then begin
    { Query the server engine for the exact record count}
    Result := ServerEngine.TableGetRecCount(CursorID,
                                            iRecCount);
  end else begin
    { We will manually count the records at the client.      }
    {This can take some time, and consume copious amounts of }
    {bandwitdth. It is recommended that a record count       }
    {only be requested when absolutely necessary when        }
    {filters are active!                                     }
    iRecCount := 0;
    FFGetMem(Buff, PhysicalRecordSize);
    try
      DisableControls;
      try
        { Retrieve a bookmark so we can reset the cursor when we are done}
        BM := GetBookMark;
        try
          Marked := Assigned(BM);
          try
            InternalFirst;
            Result := dsGetNextRecord(ffltNOLOCK, Buff, nil);
            while (Result = DBIERR_NONE) do begin
              Inc(iRecCount);
              Result := dsGetNextRecord(ffltNOLOCK, Buff, nil);
            end;
          finally
            { if an error occured, we need to make sure the cursor is set}
            {properly!}
            if Marked then
              InternalGotoBookmark(BM);
          end;
        finally
          FreeBookmark(BM);
        end;
      finally
        EnableControls;
      end;
    finally
       FFFreeMem(Buff, PhysicalRecordSize);
    end;
  end;

  { If an unexpected error occurs set RecordCount to 0}                {!!.01 - Start}
  if (Result <> DBIERR_NONE) then begin
    if (Result = DBIERR_EOF) then
      Result := DBIERR_NONE
    else
      iRecCount := 0;
  end;                                                                 {!!.01 - End}
end;
{------}
function TffDataSet.dsGetRecordPrim(eLock    : TffLockType;
                                  pRecBuff : Pointer;
                                  RecProps : pRECProps) : TffResult;
begin
  Result := ServerEngine.RecordGet(CursorID,
                                   eLock,
                                   pRecBuff);
  if (RecProps <> nil) then
    FillChar(RecProps^, sizeof(RECProps), 0);
end;
{------}
function TffBaseTable.btGetRecordForKey(aCursorID  : TffCursorID;
                                        bDirectKey : Boolean;
                                        iFields    : Word;
                                        iLen       : Word;
                                        pKey       : Pointer;
                                        pRecBuff   : Pointer
                                       ) : TffResult;
var
  FoundNext : Boolean;
  Bookmark  : Pointer;
  CreatedBuffer : Boolean;
  FuncResult : TffResult;
  RangeSaved : Boolean;
  Request : PffnmCursorSetRangeReq;
  SetRangeReqLen : Integer;
  FirstCall : Boolean;
begin
  if (aCursorID = CursorID) then begin                                {Begin !!.03}
    if (not bDirectKey) and (btIndexID = 0) then begin
     Result := DBIERR_INVALIDINDEXTYPE;
      Exit;
    end;
  end else begin
    if (not bDirectKey) and (btLookupIndexID = 0) then begin
     Result := DBIERR_INVALIDINDEXTYPE;
      Exit;
    end;
  end;                                                                {END !!.03}

  if FilterActive then begin

    RangeSaved := False;

    { If a range is active then push it onto the range stack.
      We will restore the range when we are done. }
    if btRangeStack.SavedRequest then begin
      btRangeStack.PushSavedRequest;
      RangeSaved := True;
    end;

    Bookmark := nil;
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
    if (Result = DBIERR_NONE) then begin
      {create a record Buffer if one wasn't passed in}
      CreatedBuffer := False;
      if (pRecBuff = nil) then begin
        CreatedBuffer := True;
        FFGetMem(pRecBuff, PhysicalRecordSize);
      end;
      {search for valid record in range}
      FoundNext := False;
      Result := dsGetNextRecordPrim(aCursorID, ffltNoLock, pRecBuff, nil);
      while (Result = DBIERR_NONE) and (not FoundNext) do begin
        if dsMatchesFilter(pRecBuff) then begin
           FoundNext := True;
        end else
          Result := dsGetNextRecordPrim(aCursorID, ffltNoLock, pRecBuff, nil);
      end;
      {if we succeeded in finding a record in range, get its bookmark}
      {because the reset range in a moment will lose the record}
      {position}
      if not (Result = DBIERR_NONE) then
        FuncResult := DBIERR_RECNOTFOUND
      else begin
//      if BookmarkAvailable then begin                               {!!.06}
          GetMem(Bookmark, BookmarkSize);                             {!!.03}
          Check(ServerEngine.CursorGetBookmark(aCursorID, Bookmark)); {!!.03}
//      end;                                                          {!!.06}
      end;
      {reset the range}
      btResetRangePrim(aCursorID, True);

      { Do we need to restore a prior range? }
      if rangeSaved then begin
        btRangeStack.popSavedRequest(PffByteArray(Request), SetRangeReqLen);
        { Send the request.  Assume that if it fails we should
          continue operation anyway. }

        Result :=ServerEngine.CursorSetRange(Request^.CursorID,
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

      end;
      {reset the record position}
      if (Bookmark <> nil) and
        BookmarkValid(Bookmark) then begin                            {!!.06}
        Check(ServerEngine.CursorSetToBookmark(aCursorID,
                                               Bookmark));
        FreeBookmark(Bookmark);
      end;
      if CreatedBuffer then
         FFFreeMem(pRecBuff, PhysicalRecordSize);
    end;
    if (Result = DBIERR_NONE) then
      Result := FuncResult;
  end else begin
    FirstCall := True;
    repeat
      Result := ServerEngine.RecordGetForKey(aCursorID,
                                             bDirectKey,
                                             iFields,
                                             iLen,
                                             pKey,
                                             pRecBuff,
                                             FirstCall);
      if Result = DBIERR_FF_FILTERTimeout then begin
        if dsCancelServerFilter then
          Break
        else
          FirstCall := False;
      end else
        Break;
    until False;
  end;
end;
{------}
procedure TffBaseTable.btSetKeyExclusive(const aValue : Boolean);
begin
  btCheckKeyEditMode;
  PKeyRecInfo(PChar(btKeyBuffer) + btKeyInfoOfs)^.kriExclusive := aValue;
end;
{--------}
procedure TffBaseTable.btSetKeyFieldCount(const aValue : Integer);
begin
  btCheckKeyEditMode;
  PKeyRecInfo(PChar(btKeyBuffer) + btKeyInfoOfs)^.kriFieldCount := aValue;
end;
{--------}
procedure TffBaseTable.btSetLinkRange(aMasterFields : TList);
var
  i              : Integer;
  SaveState      : TDataSetState;
  RangeStart     : PChar;
  RangeStartInfo : PKeyRecInfo;
begin
  {temporarily change the DataSet state so we can modify the key
   range when we modify field values}
  SaveState := SetTempState(dsSetKey);
  try
    {set up the Buffer to modify the the start of the range, and then
     set it to the current record in the master}
    RangeStart := PKeyBuffers(btKeyBuffers)^[ketRangeStart];
    btKeyBuffer := RangeStart;
    RangeStartInfo := PKeyRecInfo(RangeStart + btKeyInfoOfs);
    btInitKeyBuffer(RangeStart);
    RangeStartInfo^.kriModified := True;
    for i := 0 to Pred(aMasterFields.Count) do
      btGetIndexField(i).Assign(TField(aMasterFields[i]));
    RangeStartInfo^.kriFieldCount := aMasterFields.Count;
  finally
    RestoreState(SaveState);
  end;
  {make the range end equal to the range start}
  Move(PKeyBuffers(btKeyBuffers)^[ketRangeStart]^,
       PKeyBuffers(btKeyBuffers)^[ketRangeEnd]^,
       btKeyBufSize);
end;
{--------}
procedure TffBaseTable.btSetMasterFields(const aValue : string);
begin
  btMasterLink.FieldNames := aValue;
end;
{--------}
procedure TffBaseTable.btSetMasterSource(const aValue : TDataSource);
begin
  if IsLinkedTo(aValue) then
    RaiseFFErrorObjFmt(Self, ffdse_TblCircDataLink, [aValue.Name]);
  btMasterLink.DataSource := aValue;
end;
{--------}
procedure TffBaseTable.dsSetTableName(const aValue : string);
begin
  inherited dsSetTableName(aValue);

  IndexDefs.Updated := False;
end;
{--------}
procedure TffBaseTable.btSetIndexDefs(Value : TIndexDefs);            {!!.06}
begin
  IndexDefs.Assign(Value);
end;
{--------}
function TffBaseTable.btIndexDefsStored : Boolean;                    {!!.06}
begin
  Result := IndexDefs.Count > 0;
end;
{--------}
function TffBaseTable.btSetRange : Boolean;
var
  RangeStart     : PChar;
  RangeEnd       : PChar;
  StartKeyOrRec  : PChar;
  EndKeyOrRec    : PChar;
  RangeStartInfo : PKeyRecInfo;
  RangeEndInfo   : PKeyRecInfo;
begin
  { Assume we don't set the range. }
  Result := False;

  { If range is the same, exit now. }
  if (BuffersEqual(PKeyBuffers(btKeyBuffers)^[ketRangeStart],
                   PKeyBuffers(btKeyBuffers)^[ketCurRangeStart],
                   btKeyBufSize) and
      BuffersEqual(PKeyBuffers(btKeyBuffers)^[ketRangeEnd],
                   PKeyBuffers(btKeyBuffers)^[ketCurRangeEnd],
                   btKeyBufSize)) then
    Exit;

  { Determine what to use for the setrange call. }
  RangeStart := PKeyBuffers(btKeyBuffers)^[ketRangeStart];
  RangeStartInfo := PKeyRecInfo(RangeStart + btKeyInfoOfs);
  if RangeStartInfo^.kriModified then {ie, some key fields are set}
    StartKeyOrRec := RangeStart
  else
    StartKeyOrRec := nil;

  RangeEnd := PKeyBuffers(btKeyBuffers)^[ketRangeEnd];
  RangeEndInfo := PKeyRecInfo(RangeEnd + btKeyInfoOfs);
  if RangeEndInfo^.kriModified then {ie, some key fields are set}
    EndKeyOrRec := RangeEnd
  else
    EndKeyOrRec := nil;
  {set the range}
  Check(btSetRangePrim(CursorID, False,
                       RangeStartInfo^.kriFieldCount,
                       0,
                       StartKeyOrRec,
                       not RangeStartInfo^.kriExclusive,
                       RangeEndInfo^.kriFieldCount,
                       0,
                       EndKeyOrRec,
                       not RangeEndInfo^.kriExclusive));
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
end;
{--------}
function TffBaseTable.btSetRangePrim(aCursorID  : TffCursorID;
                                     bKeyItself : Boolean;
                                     iFields1   : Word;
                                     iLen1      : Word;
                                     pKey1      : Pointer;
                                     bKey1Incl  : Boolean;
                                     iFields2   : Word;
                                     iLen2      : Word;
                                     pKey2      : Pointer;
                                     bKey2Incl  : Boolean) : TffResult;
var
  Request : PffnmCursorSetRangeReq;
  ReqLen : Integer;
  KeyLen1, KeyLen2 : Integer;
  pKeyData2 : pointer;
begin
  Result := DBIERR_NOMEMORY;
  {calculate sizes}
  if pKey1 = nil then
    KeyLen1 := 0
  else if bKeyItself then
    KeyLen1 := Dictionary.IndexKeyLength[ IndexID ]
  else
    KeyLen1 := PhysicalRecordSize;
  if pKey2 = nil then
    KeyLen2 := 0
  else if bKeyItself then
    KeyLen2 := Dictionary.IndexKeyLength[ IndexID ]
  else
    KeyLen2 := PhysicalRecordSize;
  {now, we know how large the Request is}
  ReqLen := sizeof(TffnmCursorSetRangeReq) - 4 + KeyLen1 + KeyLen2;
  {allocate and clear it}
  ffGetZeroMem(Request, ReqLen);
  try
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
  finally
    if (Result = DBIERR_NONE) then
      btRangeStack.SaveLastRequest(PffByteArray(Request), ReqLen)
    else
      FFFreeMem(Request, ReqLen);
  end;
end;
{------}
function TffDataSet.dsCheckBLOBHandle(pRecBuf : Pointer;
                                      iField  : Integer;
                                  var aIsNull : Boolean;
                                  var aBLOBNr : TffInt64) : TffResult;
var
  TempI64 : TffInt64;
begin
  TempI64.iLow := 0;
  TempI64.iHigh := 0;
  Dictionary.GetRecordField(Pred(iField), pRecBuf, aIsNull, @aBLOBNr);
  if (not aIsNull) and (ffCmpI64(aBLOBNr, TempI64) = 0) then
    Result := DBIERR_INVALIDBLOBHANDLE
  else
    Result := DBIERR_NONE;
end;
{------}
function TffDataSet.dsEnsureBlobHandle(pRecBuf : Pointer;
                                       iField  : Integer;
                                   var aBLOBNr : TffInt64) : TffResult;
var
  IsNull  : Boolean;
  TempI64 : TffInt64;
begin
  TempI64.iLow := 0;
  TempI64.iHigh := 0;
  Dictionary.GetRecordField(Pred(iField), pRecBuf, IsNull, @aBLOBNr);
  if IsNull then begin
    Result := ServerEngine.BLOBCreate(CursorID,
                                      aBLOBNr);
    if (Result = DBIERR_NONE) then begin
      Dictionary.SetRecordField(Pred(iField), pRecBuf, @aBLOBNr);
    end;
  end
  else if (ffCmpI64(aBLOBNr, TempI64) = 0) then
    Result := DBIERR_INVALIDBLOBHANDLE
  else
    Result := DBIERR_NONE;
end;
{--------}
function TffDataSet.TruncateBlob(pRecBuf : Pointer;
                                 iField  : Word;
                                 iLen    : Longint) : TffResult;
var
  BLOBNr  : TffInt64;
  IsNull  : boolean;
begin
  Result := dsCheckBLOBHandle(pRecBuf, iField, IsNull, BLOBNr);
  if (Result = DBIERR_NONE) then begin
    if IsNull then begin
      if (iLen <> 0) then
        Result := DBIERR_INVALIDBLOBoffset
      else
        Result := DBIERR_NONE;
    end else begin
      {BLOB field was not null}
      {tell the server the new length}
      Result := ServerEngine.BLOBTruncate(CursorID,
                                          BLOBNr,
                                          iLen);
    end;
  end;
end;
{------}
procedure TffDataSet.dsSetReadOnly(const aValue : Boolean);
begin
  dsProxy.CheckInactive(False);                                       {!!.06}

  if (csLoading in ComponentState) then begin
    dsReadOnly := aValue;                                             {!!.01}
    Exit;
  end;

  if (dsProxy.Database <> nil) and dsProxy.Database.ReadOnly then
    dsReadOnly := True
  else
    dsReadOnly := aValue;
end;
{--------}
procedure TffDataSet.dsSetServerSideFilter(const aText : string;
                                           const aOpts : TFilterOptions;
                                                 aTimeout : TffWord32);

var
  Parser : TExprParser;
begin
  if (aText <> '') then begin
    {$IFDEF ExprParserType1}
    Parser := TExprParser.Create(Self, aText, aOpts);
    {$ENDIF}
    {$IFDEF ExprParserType2}
    Parser := TExprParser.Create(Self, aText, aOpts, [], '', nil);
    {$ENDIF}
    {$IFDEF ExprParserType3}
    Parser := TExprParser.Create(Self, aText, aOpts, [], '', nil,
                                 FldTypeMap);
    {$ENDIF}
    try
      Check(SetFilterEx(ffSrBDE.pCANExpr(Parser.FilterData), aTimeout));
    finally
      Parser.Free;
    end;
  end
  else
    dsClearServerSideFilter;
end;
{--------}
procedure TffDataSet.dsUpdateFilterStatus;
var
  Filt : TffFilterListItem;
  i    : Integer;
begin
  for i := 0 to Pred(dsFilters.Count) do begin
    Filt := TffFilterListItem(dsFilters.Items[i]);
    if (Filt <> nil) and (Filt.Active) then begin
      dsFilterActive := True;
      Exit;
    end;
  end;
  dsFilterActive := False;
end;
{--------}
function TffDataSEt.dsDropFilter(hFilter : hDBIFilter) : TffResult;
var
  Inx : Integer;
  Filter : TffFilterListItem;
begin
  if (hFilter = nil) then begin
    dsFilters.FreeAll;
    Result := DBIERR_NONE;
  end
  else begin
    Filter := TffFilterListItem(hFilter);
    Inx := dsFilters.IndexOf(Filter);
    if (Inx = -1) then
      Result := DBIERR_NOSUCHFILTER
    else begin
      Filter.Free;
      dsUpdateFilterStatus;
      Result := DBIERR_NONE;
    end;
  end;
end;
{--------}
procedure TffDataSet.dsSetSessionName(const aValue : string);
begin
  if (csReading in ComponentState) then
    dsProxy.LoadingFromStream := True;
  dsProxy.SessionName := aValue;
  if Active then
    DataEvent(dePropertyChange, 0);
end;
{--------}
procedure TffDataSEt.dsSetTableLock(LockType: TffLockType; Lock: Boolean);

begin
  CheckActive;
  if Lock then
    Check(ServerEngine.TableLockAcquire(CursorID,
                                        LockType))
  else
    Check(ServerEngine.TableLockRelease(CursorID,
                                        False));
end;
{--------}
procedure TffDataSet.dsSetTableName(const aValue : string);
begin
  if (csReading in ComponentState) then
    dsProxy.LoadingFromStream := True;
  dsProxy.TableName := ffExtractTableName(aValue);
  if Active then
    DataEvent(dePropertyChange, 0);
end;
{--------}
procedure TffDataset.dsSetTimeout(const Value : Longint);
begin
  if dsTimeout = Value then Exit;
  dsTimeout := Value;
  if Active then
    Check(ServerEngine.CursorSetTimeout(CursorID, dsGetTimeout));
end;
{--------}
procedure TffDataSet.dsSetVersion(const aValue : string);
begin
  {do nothing}
end;
{--------}
procedure TffBaseTable.btSwitchToIndex(const aIndexName : string);
var
  Status : TffResult;
  aIndexID : Integer;
begin
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
  if (Status = DBIERR_NOCURRREC) or (Status = DBIERR_FF_RecDeleted) then {!!.11}
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
  try
    {get new record Buffers}
    SetBufListSize(BufferCount + 1);
  except
    {if we're out of memory - or worse - bail out}
    SetState(dsInactive);
    CloseCursor;
    raise;
  end;
  {get the new index information}
  dsGetIndexInfo;
end;
{--------}
function TffBaseTable.btSwitchToIndexEx(aCursorID  : TffCursorID;
                                  const aIndexName : string;
                                  const aIndexID   : Integer;
                                  const aCurrRec   : Boolean) : TffResult;
var
  Stream : TStream;
  TempDict : TffDataDictionary;
begin
  Result := ServerEngine.CursorSwitchToIndex(aCursorID,
                                             aIndexName,
                                             aIndexID,
                                             aCurrRec);
  if (aCursorID = CursorID) and (Result = DBIERR_NONE) then begin      {!!.03}
    if (aIndexName <> '') then begin
      btIndexID := Dictionary.GetIndexFromName(aIndexName);
      btIndexName := aIndexName;
      btRangeStack.Clear;
    end else begin
      btIndexName := Dictionary.IndexName[aIndexID];
      btIndexID := aIndexID;
    end;
  end else begin
    { fetch data dictionary }
    TempDict := TffDataDictionary.Create(4096);
    try
      Stream := TMemoryStream.Create;
      try
        if Database.GetFFDataDictionary(TableName, Stream) = DBIERR_NONE then begin
          Stream.Position:= 0;
          TempDict.ReadFromStream(Stream);
        end;
      finally
        Stream.Free;
      end;
      if (aCursorID = btLookupCursorID) and (Result = DBIERR_NONE) then begin
        if (aIndexName <> '') then begin
          btLookupIndexID := TempDict.GetIndexFromName(aIndexName);
          btLookupIndexName := aIndexName;
        end else begin
          btIndexID := aIndexID;
          btIndexName := TempDict.IndexName[aIndexID];
        end;
      end;
    finally
      TempDict.Free;
    end;
  end;
end;
{--------}
procedure TffBaseTable.UpdateIndexDefs;
var
  i           : Integer;
  SaveHandle  : TffCursorID;
  IndexCount  : Integer;
  IndexArray  : PffIDXDescArray;
  Options     : TIndexOptions;
  Name        : string;
  FieldsStr   : string;
  CursorProps : TffCursorProps;
begin
  {if the indexes are not up to date, go get info on them...}
  if not IndexDefs.Updated then begin
    dsEnsureDatabaseOpen(True);
    try
      SaveHandle := CursorID;
      if (SaveHandle = 0) then
        dsCursorID := GetCursorHandle('');
      FieldDefs.Update;
      try
        GetCursorProps(CursorProps);
        IndexCount := CursorProps.IndexCount;
        FFGetMem(IndexArray, IndexCount * sizeof(IDXDesc));
        try
          IndexDefs.Clear;
          btGetIndexDescs(PIDXDesc(IndexArray));
          for i := 0 to Pred(IndexCount) do begin
            btDecodeIndexDesc(IndexArray^[i], Name, FieldsStr, Options);
            IndexDefs.Add(Name, FieldsStr, Options);
          end;
          IndexDefs.Updated := True;
        finally
           FFFreeMem(IndexArray, IndexCount * sizeof(IDXDesc));
        end;{try..finally}
      finally
        if (SaveHandle = 0) then begin
          DestroyHandle(CursorID);
          dsCursorID := 0;
        end;
      end;{try..finally}
    finally
      dsEnsureDatabaseOpen(False);
    end;{try..finally}
  end;
end;
{--------}
procedure TffDataSet.UnlockTable(LockType: TffLockType);

begin
  dsSetTableLock(LockType, False);
end;
{--------}
procedure TffDataSet.UnlockTableAll;

begin
  CheckActive;
  Check(ServerEngine.TableLockRelease(CursorID,
                                      True));
end;
{====================================================================}


{===TffBlobStream====================================================}
constructor TffBlobStream.Create(aField : TBlobField; aMode : TBlobStreamMode);
var
  OpenMode : TffOpenMode;
begin
  inherited Create;

  bsMode := aMode;
  bsField := aField;
  bsTable := bsField.DataSet as TffDataSet;
  bsFieldNo := bsField.FieldNo;
  bsChunkSize := ffMaxBlobChunk;
  if not bsTable.GetActiveRecBuf(bsRecBuf) then
    Exit;
  if (bsTable.State = dsFilter) then
    RaiseFFErrorObj(aField, ffdse_BLOBFltNoFldAccess);
  if not bsField.Modified then begin
    if (aMode = bmRead) then
      OpenMode := omReadOnly
    else {BLOB stream mode is not readonly} begin
      if aField.ReadOnly then
        RaiseFFErrorObj(aField, ffdse_BLOBAccessNoMatch);
      if not (bsTable.State in [dsEdit, dsInsert]) then
        RaiseFFErrorObj(aField, ffdse_BLOBTblNoEdit);
      OpenMode := omReadWrite;
    end;
    bsTable.dsBlobOpenMode := OpenMode;
  end;
  bsOpened := True;
  if (aMode = bmWrite) then
    Truncate;
end;
{--------}
destructor TffBlobStream.Destroy;
begin
  if bsOpened then begin
    if bsModified then
      bsField.Modified := True;
    if not bsField.Modified then
      bsTable.FreeBlob(bsRecBuf, bsFieldNo);
  end;
  if bsModified then begin
    try
      bsTable.DataEvent(deFieldChange, Longint(bsField));
    except
      raise;
    end;
  end;

  inherited Destroy;
end;
{--------}
function TffBlobStream.bsGetBlobSize : Longint;
var
  Status : TffResult;
  IsNull : Boolean;
  BLOBNr : TffInt64;
begin
  Result := 0;
  if bsOpened then begin
    Status := bsTable.dsCheckBLOBHandle(bsRecBuf,
                                        bsFieldNo,
                                        IsNull,
                                        BLOBNr);
    if (Status = DBIERR_NONE) and (not IsNull) then begin
      Status := bsTable.ServerEngine.BLOBGetLength(bsTable.CursorID,
                                                   BLOBNr,
                                                   Result);
    end;
    Check(Status);
  end;
end;
{--------}
function TffBlobStream.Read(var aBuffer; aCount : Longint) : Longint;
var
  Status    : TffResult;
  T,N       : Integer;
  IsNull    : Boolean;
  BLOBNr    : TffInt64;
  Dest      : Pointer;
  BytesRead : TffWord32;                                               {!!.06}
begin
  Result := 0;
  if bsOpened then begin
    T := 0;
    bsCancel := False;
    while aCount > 0 do begin
      if bsChunkSize = 0 then
         N := aCount
      else if aCount > bsChunkSize then
         N := bsChunkSize
      else
         N := aCount;
      Result := 0;
      Status := bsTable.dsCheckBLOBHandle(bsRecBuf, bsFieldNo, ISNull, BLOBNr);
      if (Status = DBIERR_NONE) and (not IsNull) then begin
        Dest := @PChar(@aBuffer)[T];
        Status := bsTable.ServerEngine.BLOBRead(bsTable.CursorID,
                                                BLOBNr,
                                                bsPosition,
                                                N,
                                                Dest^,
                                                BytesRead);            {!!.06}
        Result := BytesRead;                                           {!!.06}
      end;
      case Status of
        DBIERR_NONE,
        DBIERR_ENDOFBLOB:
          inc(bsPosition, Result);
        DBIERR_INVALIDBLOBoffset:
          Result := 0;
      else
        RaiseffErrorCode(Status);
      end;{case}
      if bsCancel then RaiseffErrorCode(DBIERR_ENDOFBLOB);
      dec(aCount,Result);
      Inc(T,Result);

      { If fewer bytes were returned than requested then
        we have reached the end of the BLOB. }
      if Result < N then
        break;

    end;
    Result := T;
  end;
end;
{--------}
function TffBlobStream.Write(const aBuffer; aCount : Longint) : Longint;
var
  T,N    : Integer;
  BLOBNr : TffInt64;
  Status : TffResult;
  Src    : Pointer;
begin
  Result := 0;
  if bsOpened then begin
    T := 0;
    bsCancel := False;
    while aCount > 0 do begin
      if bsChunkSize = 0 then
         N := aCount
      else if aCount > bsChunkSize then
         N := bsChunkSize
      else
         N := aCount;

      Status := bsTable.dsEnsureBLOBHandle(bsRecBuf, bsFieldNo, BLOBNr);
      if (Status = DBIERR_NONE) then begin
        Src := @PChar(@aBuffer)[T];
        Status := bsTable.ServerEngine.BLOBWrite(bsTable.CursorID,
                                                 BLOBNr,
                                                 bsPosition,
                                                 N,
                                                 Src^);
      end;
      Check(Status);
      inc(bsPosition, N);
      inc(T,N);
      Dec(aCount,N);
      if bsCancel then RaiseffErrorCode(DBIERR_ENDOFBLOB)
    end;
    Result := T;
    bsModified := True;
  end;
end;
{--------}
function TffBlobStream.Seek(aoffset : Longint; aOrigin : Word) : Longint;
begin
  case aOrigin of
    soFromBeginning : bsPosition := aoffset;
    soFromCurrent   : inc(bsPosition, aoffset);
    soFromEnd       : bsPosition := bsGetBlobSize + aoffset;
  end;
  Result := bsPosition;
end;
{--------}
procedure TffBlobStream.Truncate;
begin
  if bsOpened then begin
    Check(bsTable.TruncateBlob(bsRecBuf, bsFieldNo, bsPosition));
    bsModified := true;
  end;

end;
{====================================================================}

function TffDataSet.dsGetServerEngine: TffBaseServerEngine;
begin
  if Assigned(dsServerEngine) and Active then
    Result := dsServerEngine
  else
    Result := Session.ServerEngine;
end;
{--------}
function TffBaseDatabase.bdGetServerEngine: TffBaseServerEngine;
begin
  if Assigned(bdServerEngine) and Active then
    Result := bdServerEngine
  else
    Result := Session.ServerEngine;
end;
{--------}
procedure TffBaseDatabase.bdRefreshTimeout;                          {new !!.11}
var
  Idx : Integer;
begin
  if Active then begin
    Check(ServerEngine.DatabaseSetTimeout(bdDatabaseID, GetTimeout));
    for Idx := 0 to Pred(OwnedDBItems.Count) do
      TffTableProxyList(OwnedDBItems)[Idx].ffTable.dsRefreshTimeout;
  end;
end;
{--------}
function TffTableProxy.tpGetServerEngine: TffBaseServerEngine;
begin
  if Assigned(tpServerEngine) and Active then
    Result := tpServerEngine
  else
    Result := Session.ServerEngine;
end;
{====================================================================}

{===TffQueryDataLink=================================================}
constructor TffQueryDataLink.Create(aQuery: TffQuery);
begin
  inherited Create;
  FQuery := aQuery;
end;

procedure TffQueryDataLink.ActiveChanged;
begin
  if FQuery.Active then FQuery.quRefreshParams;
end;

{$IFDEF DCC4OrLater}
function TffQueryDataLink.GetDetailDataSet: TDataSet;
begin
  Result := FQuery;
end;
{$ENDIF}

procedure TffQueryDataLink.RecordChanged(Field: TField);
begin
  if (Field = nil) and FQuery.Active then FQuery.quRefreshParams;
end;

procedure TffQueryDataLink.CheckBrowseMode;
begin
  if FQuery.Active then FQuery.CheckBrowseMode;
end;
{=====================================================================}

{== TffQuery =========================================================}
constructor TffQuery.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  { We must give dsProxy a unique name. }
  dsProxy.DBName := intToStr(GetCurrentThreadID) + intToStr(GetTickCount);
  FDataLink := TffQueryDataLink.Create(Self);
  FExecuted := True;
  FParamCheck := True;
  {$IFDEF DCC4OrLater}
  FParams := TParams.Create(Self);
  {$ELSE}
  FParams := TParams.Create;
  {$ENDIF}
  FPrepared := False;
  FSQL := TStringList.Create;
  TStringList(FSQL).OnChange := quSQLChanged;
  FStmtID := 0;
  FRowsAffected := -1;                                                 {!!.10}
  FCanModify := False;                                                 {!!.10}
end;
{--------}
destructor TffQuery.Destroy;
begin
  quDisconnect;
  FDataLink.Free;
  FParams.Free;
  FSQL.Free;
  inherited Destroy;
end;
{--------}                           {begin !!.10}
procedure TffQuery.ExecSQL;
var
  Dummy : TffCursorID;
begin
  CheckInactive;

  quExecSQLStmt(omReadOnly, Dummy);
end;
{--------}
procedure TffQuery.quExecSQLStmt(const aOpenMode : TffOpenMode;
                                   var aCursorID : TffCursorID);
var
  Msg : string;
  MsgLen : integer;
  OpenCursorID : Longint;
  ParamsData : PffByteArray;
  ParamsDataLen : integer;
  ParamsList : PffSqlParamInfoList;
  SQLResult : TffResult;
  Stream  : TStream;
  OpenCanModify : Boolean;                                            {!!.10}
  OpenRowsAffected : Integer;                                         {!!.10}

begin
  Msg := '';
  MsgLen := 0;
  FRowsAffected := -1;                                                {!!.10}
  FRecordsRead := 0;                                                  {!!.10}

  { Do we have a SQL statement? }
  if FSQL.Count > 0 then begin
    { Yes.  Prepare the statement. }
    ParamsData := nil;
    ParamsDataLen := 0;
    ParamsList := nil;
    { Allocate & prepare the SQL statement. }
    quPreparePrim(True);

    { Are we linked to a datasource? }
    if assigned(FDataLink.DataSource) then
      quSetParamsFromCursor;

    { Do we have parameters? }
    if FParams.Count > 0 then begin
      { Yes.  Send them to the server. }
      quBuildParams(ParamsList, ParamsData, ParamsDataLen);
      Stream := TMemoryStream.Create;
      try
        SQLResult := ServerEngine.SQLSetParams(FStmtID, FParams.Count,
                                               pointer(ParamsList),
                                               ParamsData, ParamsDataLen,
                                               Stream);
        { Was the set parameters successful? }
        if SQLResult <> DBIERR_NONE then begin
          { No.  Raise an error. }
          Stream.Position := 0;
          Stream.Read(MsgLen, sizeOf(MsgLen));
          if MsgLen > 0 then begin
            SetLength(Msg, MsgLen);
            Stream.Read(Msg[1], MsgLen);
            RaiseFFErrorObjFmt(Self, ffdse_QuerySetParamsFail, [#13#10, Msg]);
          end
          else
            Check(SQLResult);
        end;
      finally
        Stream.Free;
      end;
    end;

    { Execute the query. }
    Stream := TMemoryStream.Create;
    try
      SQLResult := ServerEngine.SQLExec(FStmtID, aOpenMode, aCursorID, Stream);
      { Was the execution successful? }
      if SQLResult <> DBIERR_NONE then begin
        { No.  Raise an error. }
        if Stream.Size > 0 then begin
          Stream.Position := 0;
          Stream.Read(MsgLen, sizeOf(MsgLen));
        end;
        if MsgLen > 0 then begin
          SetLength(Msg, MsgLen);
          Stream.Read(Msg[1], MsgLen);
          RaiseFFErrorObjFmt(Self, ffdse_QueryExecFail, [#13#10, Msg]);
        end
        else
          Check(SQLResult);
      end;

      { Load the data dictionary, if necessary. }
      Stream.Position := 0;
      Stream.Read(OpenCursorID, SizeOf(OpenCursorID));
      aCursorID := OpenCursorID;

      if aCursorID <> 0 then begin                              {begin !!.10}
        Dictionary.ReadFromStream(Stream);
        Stream.Read(OpenCanModify, SizeOf(OpenCanModify));
        Stream.Read(FRecordsRead, SizeOf(FRecordsRead));
      end else begin
        {get rows affected}
        Stream.Read(OpenRowsAffected, SizeOf(OpenRowsAffected));
        FRowsAffected := OpenRowsAffected;
        Stream.Read(FRecordsRead, SizeOf(FRecordsRead));
      end;                                                        {end !!.10}

    finally
      Stream.Free;
      if assigned(ParamsData) then
        FFFreemem(ParamsData, ParamsDataLen);
      if assigned(ParamsList) then
        FFFreemem(ParamsList, SizeOf(TffSQLParamInfo) * FParams.Count);
    end;
  end else
    RaiseFFErrorObj(Self, ffdse_EmptySQLStatement);
end;
{--------}                                                     {end !!.10}
{$IFDEF DCC4OrLater}
procedure TffQuery.DefineProperties(Filer : TFiler);

  function HasData : boolean;
  begin
    { We have data to write if our parameters are different than our ancestor
      class or, if we have no ancestor class, we have 1 or more parameters. }
    if assigned(Filer.Ancestor) then
      Result := not FParams.IsEqual(TffQuery(Filer.Ancestor).FParams)
    else
      Result := (FParams.Count > 0);
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', quReadParams, quWriteParams, HasData);
end;
{$ENDIF}
{--------}
procedure TffQuery.DestroyHandle(aHandle : TffCursorID);
begin
  { Release any existing record locks. }
  Check(ServerEngine.RecordRelLock(dsCursorID, False));

  { Close the cursor handle, ignore errors. }
  Check(ServerEngine.CursorClose(dsCursorID));
  dsCursorID := 0;
end;
{--------}
procedure TffQuery.dsCloseViaProxy;
begin
  inherited dsCloseViaProxy;

  Unprepare;
end;
{--------}
function TffQuery.dsGetServerEngine: TffBaseServerEngine;
begin
  if Assigned(dsServerEngine) then
    Result := dsServerEngine
  else
    Result := Session.ServerEngine;
end;
{--------}
function TffQuery.GetCanModify : Boolean;
begin
  Result := FCanModify;                                               {!!.10}
end;
{--------}
function TffQuery.GetCursorHandle(aIndexName : string) : TffCursorID;
var
  Msg : string;
  MsgLen : integer;
  OpenCursorID : Longint;
  OpenMode : TffOpenMode;                                             {!!.10}
  OpenCanModify : Boolean;                                            {!!.10}
  ParamsData : PffByteArray;
  ParamsDataLen : integer;
  ParamsList : PffSqlParamInfoList;
  SQLResult : TffResult;
  Stream  : TStream;
  OpenRowsAffected : Integer;                                         {!!.11}
begin
  Result := 0;
  FExecuted := False;
  Msg := '';
  MsgLen := 0;

  { Do we have a SQL statement? }
  if FSQL.Count > 0 then begin
    { Yes.  Prepare the statement. }
    ParamsData := nil;
    ParamsDataLen := 0;
    ParamsList := nil;
    { Allocate & prepare the SQL statement. }
    quPreparePrim(True);

    { Are we linked to a datasource? }
    if assigned(FDataLink.DataSource) then
      quSetParamsFromCursor;

    { Do we have parameters? }
    if FParams.Count > 0 then begin
      { Yes.  Send them to the server. }
      quBuildParams(ParamsList, ParamsData, ParamsDataLen);
      Stream := TMemoryStream.Create;
      try
        SQLResult := ServerEngine.SQLSetParams(FStmtID, FParams.Count,
                                               pointer(ParamsList),
                                               ParamsData, ParamsDataLen,
                                               Stream);
        { Was the set parameters successful? }
        if SQLResult <> DBIERR_NONE then begin
          { No.  Raise an error. }
          Stream.Position := 0;
          Stream.Read(MsgLen, sizeOf(MsgLen));
          if MsgLen > 0 then begin
            SetLength(Msg, MsgLen);
            Stream.Read(Msg[1], MsgLen);
            RaiseFFErrorObjFmt(Self, ffdse_QuerySetParamsFail, [#13#10, Msg]);
          end
          else
            Check(SQLResult);
        end;
      finally
        Stream.Free;
      end;
    end;

    { Execute the query. }
    if FRequestLive then
      OpenMode := omReadWrite
    else
      OpenMode := omReadOnly;
    Stream := TMemoryStream.Create;
    try
      SQLResult := ServerEngine.SQLExec(FStmtID, OpenMode, dsCursorID, Stream);
      { Was the execution successful? }
      if SQLResult <> DBIERR_NONE then begin
        { No.  Raise an error. }
        if Stream.Size > 0 then begin
          Stream.Position := 0;
          Stream.Read(MsgLen, sizeOf(MsgLen));
        end;
        if MsgLen > 0 then begin
          SetLength(Msg, MsgLen);
          Stream.Read(Msg[1], MsgLen);
          RaiseFFErrorObjFmt(Self, ffdse_QueryExecFail, [#13#10, Msg]);
        end
        else
          Check(SQLResult);
      end;

      { Load the data dictionary. }
{Begin !!.11}
      FCanModify := False;
      Stream.Position := 0;
      Stream.Read(OpenCursorID, SizeOf(OpenCursorID));
      if dsCursorID <> 0 then begin
        Dictionary.ReadFromStream(Stream);
        Stream.Read(OpenCanModify, SizeOf(OpenCanModify));
        Stream.Read(FRecordsRead, SizeOf(FRecordsRead));
        if RequestLive then
          FCanModify := OpenCanModify;
      end
      else begin
        Stream.Read(OpenRowsAffected, SizeOf(OpenRowsAffected));
        FRowsAffected := OpenRowsAffected;
        Stream.Read(FRecordsRead, SizeOf(FRecordsRead));
      end;
{End !!.11}
      dsReadFieldDescs;
      Result := dsCursorID;
      FExecuted := True;
    finally
      Stream.Free;
      if assigned(ParamsData) then
        FFFreemem(ParamsData, ParamsDataLen);
      if assigned(ParamsList) then
        FFFreemem(ParamsList, SizeOf(TffSQLParamInfo) * FParams.Count);
    end;
  end
  else
    RaiseFFErrorObj(Self, ffdse_EmptySQLStatement);

end;
{--------}
function TffQuery.GetCursorProps(var aProps : TffCursorProps) : TffResult;
begin
  Result := inherited GetCursorProps(aProps);
  aProps.KeySize := 0;
  aProps.IndexCount := 0;
  {aProps.BookMarkSize := ffcl_FixedBookmarkSize;}                     {!!.10}
end;
{--------}
procedure TffQuery.InternalClose;
begin
  FExecuted := False;
  {deactivate filters}
  if Filtered then
    dsDeactivateFilters;
  {drop filters}
  dsDropFilters;
  {clear up the fields}
  BindFields(False);
  if DefaultFields then
    DestroyFields;
  dsServerEngine := nil;                                               {!!.11}
end;
{Begin !!.01}
{--------}
function TffQuery.Locate(const aKeyFields : string;
                         const aKeyValues : Variant;
                               aOptions   : TLocateOptions) : Boolean;
begin
  DoBeforeScroll;
  Result := quLocateRecord(aKeyFields, aKeyValues, aOptions, True);
  if Result then begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;
{End !!.01}
{--------}
function TffQuery.Lookup(const aKeyFields    : string;
                         const aKeyValues    : Variant;
                         const aResultFields : string) : Variant;
var
  OurBuffer    : PChar;
  OurFields    : TList;
  FilterHandle : HDBIFilter;
begin
  Result := Null;

  {make sure we're in browse mode}
  CheckBrowseMode;
  CursorPosChanged;
  {get a temporary record Buffer}
  OurBuffer := TempBuffer;
  {create list of fields}
  OurFields := TList.Create;
  try
    {get the actual fields in the parameter aKeyFields}
    GetFieldList(OurFields, aKeyFields);
    InternalFirst;
    FilterHandle := dsCreateLookupFilter(OurFields, aKeyValues, []);
    if dsGetNextRecord(ffltNoLock, OurBuffer, nil) = 0 then begin
      if FilterEval = ffeServer then
        RestoreFilterEx
      else
        dsDropFilter(FilterHandle);
      SetTempState(dsCalcFields);
      try
        CalculateFields(TempBuffer);
        Result := FieldValues[aResultFields];
      finally
        RestoreState(dsBrowse);
      end;{try..finally}
    end;
  finally
    OurFields.Free;
  end;{try..finally}
end;
{--------}
function TffQuery.ParamByName(const aName : string) : TParam;
begin
  Result := FParams.ParamByName(aName);
end;
{--------}
procedure TffQuery.Prepare;
begin
  quPreparePrim(True);
end;
{--------}
procedure TffQuery.quBuildParams(var ParamsList : PffSqlParamInfoList;
                                 var ParamsData : PffByteArray;
                                 var ParamsDataLen : integer);
var
  aParam : TParam;
  aSrcBuffer : pointer;
  aTgtBuffer : pointer;
  Index : integer;
  Offset : integer;
  PSqlParamInfo : PffSqlParamInfo;
begin
  { Get memory for the params list. }
  FFGetMem(ParamsList, sizeOf(TffSqlParamInfo) * FParams.Count);

  Offset := 0;
  ParamsDataLen := 0;

  { Fill in the parameter list. }
  for Index := 0 to Pred(FParams.Count) do begin
    aParam := FParams.Items[Index];
    PSqlParamInfo := @ParamsList^[Index];
    with PSqlParamInfo^ do begin
      piNum := Succ(Index);
        { parameter number, base 1 }
      piName := aParam.Name;
        { parameter name }
      MapVCLTypeToFF(aParam.DataType, aParam.GetDataSize, piType, piLength);
{Begin !!.13}
      { If this is a BLOB then we must obtain the actual size of the data. }
      if piType in [fftBLOB..fftBLOBTypedBin] then
        piLength := aParam.GetDataSize;
{End !!.13}

        { data type & length }
      piOffset := Offset;
        { offset in data buffer }

      inc(Offset, piLength);
      inc(ParamsDataLen, piLength);

    end;
  end;

  { Allocate memory for the parameter data buffer. }
  FFGetMem(ParamsData, ParamsDataLen);

  { Fill the parameter data buffer. }
  for Index := 0 to Pred(FParams.Count) do begin
    aParam := FParams.Items[Index];
    PSqlParamInfo := @ParamsList^[Index];
    { Convert the data into FF format and store it in the buffer. }
    with PSqlParamInfo^ do begin
{Begin !!.13}
        aTgtBuffer := @ParamsData^[piOffset];
        if piType in [fftBLOB..fftBLOBTypedBin] then begin
          if piLength > 0 then
            aParam.GetData(aTgtBuffer);
        end
        else begin
          FFGetmem(aSrcBuffer, aParam.GetDataSize);
          try
            aParam.GetData(aSrcBuffer);
            MapBDEDataToFF(piType, aParam.GetDataSize, aSrcBuffer, aTgtBuffer);
          finally
            FFFreemem(aSrcBuffer, aParam.GetDataSize);
          end;
        end;  { if..else }
{End !!.13}
    end;  { with }
  end;  { for }

end;
{--------}
procedure TffQuery.quDisconnect;
begin
  Close;
  Unprepare;
end;
{--------}
procedure TffQuery.quFreeStmt;
var
  Result : TffResult;
begin
  if FStmtID > 0 then begin
    Result := ServerEngine.SQLFree(FStmtID);
    FStmtID := 0;
    if not (csDestroying in ComponentState) then
      Check(Result);
  end;
end;
{--------}
function TffQuery.quGetDataSource : TDataSource;
begin
  Result := FDataLink.DataSource;
end;
{Begin !!.01}
{--------}
function TffQuery.quLocateRecord(const aKeyFields : string;
                                 const aKeyValues : Variant;
                                       aOptions   : TLocateOptions;
                                       aSyncCursor: Boolean): Boolean;
var
  OurBuffer    : PChar;
  OurFields    : TList;
  FilterHandle : HDBIFilter;
  Status       : TffResult;
begin
  { Make sure we're in browse mode. }
  CheckBrowseMode;
  CursorPosChanged;
  { Get a temporary record buffer. }
  OurBuffer := TempBuffer;
  { Create list of fields. }
  OurFields := TList.Create;
  try
    { Get the actual fields in the parameter aKeyFields. }
    GetFieldList(OurFields, aKeyFields);

    { Locate the record via a filter. }
    InternalFirst;
    FilterHandle := dsCreateLookupFilter(OurFields, aKeyValues, aOptions);
    Status := dsGetNextRecord(ffltNoLock, OurBuffer, nil);
    if FilterEval = ffeServer then
      RestoreFilterEx
    else
      dsDropFilter(FilterHandle);
  finally
    OurFields.Free;
  end;{try..finally}
  Result := (Status = DBIERR_NONE);
end;
{End !!.01}
{--------}
function TffQuery.quGetParamCount : Word;
begin
  Result := FParams.Count;
end;
{--------}                                                      {begin !!.10}
function TffQuery.quGetRowsAffected : Integer;
begin
  Result := FRowsAffected;
end;
{--------}                                                        {end !!.10}
function TffQuery.quParseSQL(aStmt : string; createParams : boolean;
                             aParams : TParams) : string;
const
  MaxNest = 5;
  ParamNameTerminators = [#9, #10, #13, ' ', ',', ';', ')', '=',       {!!.11}
                          '>', '<'];                                   {!!.11}
  StringDelims = ['''', '"', '`'];
    { Things that delimit a string. }
var
  CurPos, EndPos, NameEndPos, NameStartPos, StartPos : integer;
  DelimStackTop : integer;
  DelimStack : array[1..MaxNest] of char;
  aLen : integer;
begin
  { Parameter format:
     :<contiguous text>
     :"<text>" (i.e., for multiword param names)

    Excluded:
     double colons
     a colon occuring within double or single quotes
  }

  if aStmt = '' then
    Exit;

  Result := aStmt;

  CurPos := 1;
  DelimStackTop := 0;

  repeat

    { Skip past the leading bytes of multi-byte character set. }
    while Result[CurPos] in LeadBytes do inc(CurPos);

    { Is this the start of a literal? }
    if Result[CurPos] in StringDelims then begin
      { Yes.  Skip to the end of the literal.  Note that we can have nested
        delimiters. }
      inc(DelimStackTop);
      DelimStack[DelimStackTop] := Result[CurPos];

      repeat

        inc(CurPos);
        aLen := Length(Result);

        while (CurPos < aLen) and
              (not (Result[CurPos] in StringDelims)) do begin
          { Skip past leading bytes of MBCS. }
          while Result[CurPos] in LeadBytes do inc(CurPos);
          { Skip this char. }
          inc(CurPos);
        end;

        if CurPos > aLen then
          break;

        { Does this delimiter match the beginning delimiter? }
        if Result[CurPos] = DelimStack[DelimStackTop] then
          { Yes. Decrement the stack.  We will leave this loop once
            the stack is empty (e.g., DelimStackTop = 0). }
          dec(DelimStackTop)
        else if DelimStackTop < MaxNest then begin
          { No.  We have encountered nested delimiters.  Add the delimiter
            to the stack. }
          inc(DelimStackTop);
          DelimStack[DelimStackTop] := Result[CurPos];
        end;

      until DelimStackTop = 0;

      { Move to the character after the final string delimiter. }
      inc(CurPos);

    end
    else if (Result[CurPos] = ':') then begin
      { Is this a double colon? }
      if (Result[CurPos + 1] = ':') then
        inc(CurPos, 2)
      else begin
        { No.  We have found a single colon.  Grab the name.  Note that the
          name may be in single quotes. }
        StartPos := CurPos;
        inc(CurPos);
        { Is the colon followed by a double quote?  In other words, is the
          param name delimited by double quotes? }
        if Result[CurPos] = '"' then begin
          inc(CurPos);
          NameStartPos := CurPos;
          repeat
            inc(CurPos);
          until Result[CurPos] = '"';
          EndPos := CurPos;
          NameEndPos := CurPos - 1;
        end
        else begin
          NameStartPos := CurPos;
          repeat
            inc(CurPos);
          until Result[CurPos] in ParamNameTerminators;
          EndPos := CurPos - 1;
          NameEndPos := EndPos;
        end;


        { Create a TParam if necessary.  Replace the name with a '?'. }
        if createParams and assigned(aParams) then
          aParams.CreateParam(ftUnknown,
                              Copy(Result, NameStartPos,
                                   (NameEndPos - NameStartPos) + 1), ptUnknown);

        Result[StartPos] := '?';
        System.Delete(Result, StartPos + 1, EndPos - StartPos);
        CurPos := StartPos + 1;

      end;
    end else
      { Not the start of a literal or a colon.  Move to next character. }
      inc(CurPos);

  until (CurPos > Length(Result)) or (Result[CurPos] = #0);

end;
{--------}
procedure TffQuery.quPreparePrim(prepare : boolean);
var
  SQLResult : TffResult;
  Msg : string;
  MsgLen : integer;
  Stream : TMemoryStream;
begin
  { Requirement: Query must be closed. }
  if dsCursorID > 0 then
    RaiseFFErrorObj(Self, ffdse_QueryMustBeClosed);

  if (FPrepared <> prepare) then begin

    FExecuted := False;

//    { Requirement: Must have a database. }                           {Moved !!.03}
//    dsEnsureDatabaseOpen(True);                                      {Moved !!.03}

    { Are we preparing? }
    if prepare then begin
      { Yes. Requirement: Must have a database. }                      {!!.03}
      dsEnsureDatabaseOpen(True);                                      {!!.03}
      FRowsAffected := -1;                                             {!!.10}
      FCanModify := False;                                             {!!.10}
      FRecordsRead := 0;                                               {!!.10}

      { If we have a SQL statement then allocate & prepare a SQL
        statement on the engine. }
      if (length(FText) > 0) then begin
        Check(ServerEngine.SQLAlloc(dsProxy.Database.Session.Client.ClientID,
                                    dsProxy.Database.DatabaseID, dsGetTimeout,
                                    FStmtID));
        Stream := TMemoryStream.Create;
        try
          try
            SQLResult := ServerEngine.SQLPrepare(FStmtID, pointer(FText),
                                                 Stream);
            if SQLResult <> DBIERR_NONE then begin
              Stream.Position := 0;
              Stream.Read(MsgLen, sizeOf(MsgLen));
              if MsgLen > 0 then begin
                SetLength(Msg, MsgLen);
                Stream.Read(Msg[1], MsgLen);
                RaiseFFErrorObjFmt(Self, ffdse_QueryPrepareFail, [#13#10, Msg]);
              end
              else
                Check(SQLResult);
            end;
          except
            quFreeStmt;
            raise;
          end;
        finally
          Stream.Free;
        end;
      end
      else
        { No SQL statement.  Raise an exception. }
        RaiseFFErrorObj(Self, ffdse_EmptySQLStatement);
    end
    else
      { No.  Free the statement. }
      quFreeStmt;
    FPrepared := prepare;
  end;
end;
{$IFDEF DCC4OrLater}
{--------}
procedure TffQuery.quReadParams(Reader : TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;
{$ENDIF}
{--------}
procedure TffQuery.quRefreshParams;
var
  DataSet: TDataSet;
begin
  DisableControls;
  try
    if assigned(FDataLink.DataSource) then begin
      DataSet := FDataLink.DataSource.DataSet;
      if assigned(DataSet) then
        if DataSet.Active and (DataSet.State <> dsSetKey) then begin
          Close;
          Open;
        end;
    end;
  finally
    EnableControls;
  end;
end;
{--------}
procedure TffQuery.quSetDataSource(aSrc : TDataSource);
begin
  { If we have a circular link then raise an exception. }
  if IsLinkedTo(aSrc) then
    RaiseFFErrorObjFmt(Self, ffdse_TblCircDataLink, [aSrc.Name]);
  FDataLink.DataSource := aSrc;
end;
{--------}
procedure TffQuery.quSetParams(aParamList : TParams);
begin
  FParams.AssignValues(aParamList);
end;
{--------}
procedure TffQuery.quSetParamsFromCursor;
var
  I: Integer;
  DataSet: TDataSet;
begin
  if assigned(FDataLink.DataSource) then begin
    DataSet := FDataLink.DataSource.DataSet;
    if assigned(DataSet) then begin
      DataSet.FieldDefs.Update;
      for I := 0 to Pred(FParams.Count) do
        with FParams[I] do
          { Has this parameter been bound? }
          if not Bound then begin
            { No. Get a value from the dataset. }
            AssignField(DataSet.FieldByName(Name));
            Bound := False;
          end;
    end;
  end;
end;
{--------}
procedure TffQuery.quSetPrepared(aFlag : boolean);
begin
  if aFlag then
    Prepare
  else
    Unprepare;
end;
{--------}
procedure TffQuery.quSetRequestLive(aFlag : boolean);
begin
  if aFlag then Exit;                                               {!!.11}
(*  if FRequestLive <> aFlag then begin                             {!!.11}
    FRequestLive := aFlag;
    dsReadOnly := (not aFlag);
  end;*)
end;
{--------}
procedure TffQuery.quSetSQL(aValue : TStrings);
begin
  if FSQL.Text <> aValue.Text then begin
    quDisconnect;
    FSQL.BeginUpdate;
    try
      FSQL.Assign(aValue);
    finally
      FSQL.EndUpdate;
    end;
  end;
end;
{--------}
procedure TffQuery.quSQLChanged(Sender : TObject);
var
  aList : TParams;
begin
{Begin !!.02}
  {$IFNDEF DCC4OrLater}
  aList := nil;
  {$ENDIF}
{End !!.02}
  { Is the component loading? }
  if not (csReading in ComponentState) then begin
    { No.  Disconnect from the server. }
    quDisconnect;
    { Are we supposed to regenerate the parameters or are we in the IDE? }
    if FParamCheck or (csDesigning in ComponentState) then begin
      { Yes.  Rebuild the parameters. }
      {$IFDEF DCC4OrLater}
      aList := TParams.Create(Self);
      {$ELSE}
      aList := TParams.Create;
      {$ENDIF}
      try
        FText := quParseSQL(FSQL.Text, True, aList);
        aList.AssignValues(FParams);
        FParams.Clear;
        FParams.Assign(aList);
      finally
        aList.Free;
      end;
    end else
      FText := FSQL.Text;
    DataEvent(dePropertyChange, 0);
  end
  else
    { Yes.  Parse the text, replacing parameters with question marks. }
{Begin !!.02}
    {$IFDEF DCC4OrLater}
    FText := quParseSQL(FSQL.Text, False, nil);
    {$ELSE}
    begin                                                              {!!.03}
      aList := TParams.Create;
      try
        FText := quParseSQL(FSQL.Text, True, aList);
        aList.AssignValues(FParams);
        FParams.Clear;
        FParams.Assign(aList);
      finally
        aList.Free;
      end;
    end;                                                               {!!.03}
    {$ENDIF}
end;
{$IFDEF DCC4OrLater}
{--------}
procedure TffQuery.quWriteParams(Writer : TWriter);
begin
  Writer.WriteCollection(FParams);
end;
{$ENDIF}
{--------}
procedure TffQuery.Unprepare;
begin
  quPreparePrim(False);
end;
{====================================================================}

{===Initialization routine===========================================}
procedure InitializeUnit;
var
  Sess : TffSession;
  CL   : TffClient;
begin
  {create the Clients list}
  Clients := TffClientList.Create;

  {create the default comms engine}
  CL := TffClient.Create(nil);
  CL.ClientName := AutoObjName;
  CL.IsDefault := True;

  {create the default session in the default comms engine}
  Sess := TffSession.Create(nil);
  Sess.SessionName := AutoObjName;
  Sess.IsDefault := True;
end;
{====================================================================}


{===Finalization routine=============================================}
procedure FinalizeUnit;
var
  Sess : TffSession;
  CL   : TffBaseClient;
begin
  Sess := FindDefaultffSession;
  CL := FindDefaultFFClient;
  Sess.Free;
  CL.Free;
  Clients.Free;
  Clients := nil;
  {$IFDEF SingleExe}
  if Assigned(ServerEngine) then begin
    ServerEngine.Free;
    ServerEngine := nil;
  end;
  {$ENDIF}
end;
{====================================================================}


initialization
  InitializeUnit;
{--------}
finalization
  FinalizeUnit;
{--------}
end.


