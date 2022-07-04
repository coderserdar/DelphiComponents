{
  Kylix / Delphi open source DbExpress driver for ODBC
  Version 3.206 2008-12-18

  Copyright (c) 2001-2009 Edward Benson

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2.1
  of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
}
unit DbxOpenOdbc;

{$include DbxOpenOdbc_options.inc}

interface

uses
//  Variants, // for implemented TBlobChunkCollection.ReadBlobToVariant
  DSIntf,
  DbxOpenOdbcInterface,
  OdbcApi,
  {$IFDEF _D11UP_}
  DbxOpenOdbcDbx3Types,
  {$ELSE}
  DBXpress,
  {$ENDIF}
  FmtBcd,
  {$IFDEF LINUX}
    {.$IFDEF _K1UP_}
    Types,
    {.$ENDIF IFDEF _K3UP_}
  {$ENDIF IFDEF LINUX}
  {$IFDEF _RegExprParser_}
    DbxObjectParser,
  {$ENDIF}
  {.$IFDEF _TRACE_CALLS_}
    DbxOpenOdbcTrace,
  {.$ENDIF _TRACE_CALLS_}
  {.$IFDEF _DBXCB_}
  DbxOpenOdbcCallback,
  {.$ENDIF}
  Classes,
  SysUtils;

const
  DbxOpenOdbcVersion = '3.206 (2008-12-18): Beta';

{.$INCLUDE DbxOpenOdbc_history.inc}

{ getSQLDriverODBC is the starting point for everything else... }

// priority ansi odbc api
function getSQLDriverODBC(sVendorLib, sResourceFile: PAnsiChar; out Obj): SQLResult; stdcall;
// priority unicode odbc api
function getSQLDriverODBCAW(sVendorLib, sResourceFile: PAnsiChar; out Obj): SQLResult; stdcall;

//exports getSQLDriverODBC;

{ Connection Extended Options }

type
  EDbxErrorCustom = class(Exception);
  EDbxOdbcWarning = class(EDbxErrorCustom);
  EDbxError = class(EDbxErrorCustom);       // The 4 Exceptions below descendents of this
  EDbxOdbcError = class(EDbxError);         // Odbc returned error result code
  EDbxNotSupported = class(EDbxError);      // Feature not yet implemented
  EDbxInvalidCall = class(EDbxError);       // Invalid function call or function parameter
  EDbxInternalError = class(EDbxError);     // Other error
  EDbxInvalidParam = class(EDbxError);      // Corresponds DBXERR_INVALIDPARAM

  { TSQLMonitor }

  pSQLTRACEDesc30 = ^SQLTRACEDesc30;
  SQLTRACEDesc30 = packed record             { trace callback info }
    pszTrace        : array [0..1023] of WideChar;
    eTraceCat       : TRACECat;
    ClientData      : Integer;
    uTotalMsgLen    : Word;
  end;

  pSQLTRACEDesc25 = ^SQLTRACEDesc25;
  SQLTRACEDesc25 = packed record             { trace callback info }
    pszTrace        : array [0..1023] of AnsiChar;
    eTraceCat       : TRACECat;
    ClientData      : Integer;
    uTotalMsgLen    : Word;
  end;

  {$IFDEF _D10UP_}
//   pSQLTRACEDesc = pSQLTRACEDesc25;
//   SQLTRACEDesc = SQLTRACEDesc25;
  {$ELSE}
//   pSQLTRACEDesc = pSQLTRACEDesc30;
//   SQLTRACEDesc = SQLTRACEDesc30;

   ISQLConnection25 = ISQLConnection;
   ISQLCommand25 = ISQLCommand;
   ISQLCursor25 = ISQLCursor;
   ISQLMetaData25 = ISQLMetaData;
  {$ENDIF}

  {$IFDEF _DBX30_}
  THostPlatform = set of (hpWindows, hpLinux, hpCLR, hp32Bit, hp64bit);
  {$ENDIF}
  // Restrictions of updating for connection options depending on the current
  // status of connection.
  TConnectionOptionRestriction = (
    cor_connection_off,  // can be changed only before connection
    //Are not used:
    {
    cor_connection_on,   // can be changed only after connection
    cor_SqlHStmtMax0,    // can be changed when not allocated any SqlHStmt or before connection
    }
    cor_ActiveCursors0,  // can be changed when there is no open Cursors
    cor_driver_off       // cannot be changed to value other from in driver option (can changed only
                         // when driver option == osOff).
  );
  TConnectionOptionsRestriction = set of TConnectionOptionRestriction;
  TConnectionOptionsRestrictions = array [TConnectionOption] of TConnectionOptionsRestriction;

const

  { Default Connection Extended Options }
  cConnectionOptionsDefault: TConnectionOptions = (
    // Connection features:
    osOn,       // - coSafeMode
    {$IFDEF _InternalCloneConnection_}
    osOn,       // - coInternalCloneConnection
    {$ELSE}
    osOff,      // - coInternalCloneConnection
   {$ENDIF}
    osDefault,  // - coBlobChunkSize,
    osDefault,  // - coNetwrkPacketSize,
    osOff,      // - coReadOnly
    osDefault,  // - coCatalogPrefix
    osDefault,  // - coConTimeout
    osDefault,  // - coNumericSeparator
    osDefault,  // - coConnectionString
    // Metada features:
    osOn,       // - coSupportsMetadata
    osOn,       // - coSupportsCatalog
    osOff,      // - coSupportsSchemaFilter
    // BindField features:
    osOn,       // - coMapInt64ToBcd
    osOff,      // - coMapSmallBcdToNative
    osOff,      // - coIgnoreUnknownFieldType
    osOff,      // - coMapCharAsBDE
    osOn,       // - coEnableBCD
    osOff,      // - coMaxBCD
    osDefault,  // - coEnableUnicode
    osOn,       // - coSupportsAutoInc
    osOn,       // - coFldReadOnly
    // Field & Params features:
    osOff,      // - coTrimChar
    osOff,      // - coEmptyStrParam
    osOff,      // - coNullStrParam
    osDefault,  // - coParamDateByOdbcLevel3
    osOff,      // - coBCD2Exp
    // Rows Fetch features:
    osOff,      // - coMixedFetch
    osOn,       // - coBlobFragmentation
    osOff,      // - coBlobNotTerminationChar
    // ISQLCommand features:
    osDefault,  // - coNetTimeout
    osDefault,  // - coLockMode
    osOff,      // - coSPSN
    osOff,      // - coTLSTO
    osOn,       // - coOBPBPL
    osOn        // - coCFC
    );

  cConnectionOptionsRestrictions: TConnectionOptionsRestrictions = (
  // Are processed in procedure "IsRestrictedConnectionOption()".
    // Connection features:
    [ // there are no limitations
     ],                            // - coSafeMode
    [
      cor_connection_off],         // - coInternalCloneConnection (It is read in "SqlExpr.pas"
                                   //     right after establishments of connection).
    [ // there are no limitations
     ],                            // - coBlobChunkSize
    [ // there are no limitations
     ],                            // - coNetwrkPacketSize
    [
      cor_connection_off],         // - coReadOnly
    [
      cor_connection_off],         // - coCatalogPrefix
    [
      cor_connection_off],         // - coConTimeout
    [ // there are no limitations
      ],                           // - coNumericSeparator
    [
      cor_connection_off],         // - coConnectionString
    // Metada features:
    [ // there are no limitations
     ],                            // - coSupportsMetadata
    [ // there are no limitations
     ],                            // - coSupportsCatalog
    [ // there are no limitations
     ],                            // - coSupportsSchemaFilter
    // BindField features:
    [ // there are no limitations
     ],                            // - coMapInt64ToBcd
    [ // there are no limitations
     ],                            // - coMapSmallBcdToNative
    [ // there are no limitations
     ],                            // - coIgnoreUnknownFieldType
    [ // there are no limitations
     ],                            // - coMapCharAsBDE
    [ // there are no limitations
     ],                            // - coEnableBCD
    [ // there are no limitations
     ],                            // - coMaxBCD
    [
      cor_ActiveCursors0],         // - coEnableUnicode
    [ // there are no limitations
     ],                            // - coSupportsAutoInc
    [ // there are no limitations
     ],                            // - coFldReadOnly
    // Field & Params features:
    [ // there are no limitations
     ],                            // - coTrimChar
    [ // there are no limitations
     ],                            // - coEmptyStrParam
    [ // there are no limitations
     ],                            // - coNullStrParam
    [ // there are no limitations
     ],                            // - coParamDateByOdbcLevel3
    [ // there are no limitations
     ],                            // - coBCD2Exp
    // Rows Fetch features:
    [
      cor_driver_off],             // - coMixedFetch
    [ // there are no limitations
     ],                            // - coBlobFragmentation
    [ // there are no limitations
     ],                            // - coBlobNotTerminationChar
    // ISQLCommand features:
    [ // there are no limitations
     ],                            // - coNetTimeout
    [ // there are no limitations
     ],                            // - coLockMode
    // others
    [ // there are no limitations
     ],                            // - coSPSN
    [ // there are no limitations
     ],                            // - coTLSTO
    [ // there are no limitations
     ],                            // - coOBPBPL
    [ // there are no limitations
     ]                             // - coCFC
  );

  // Connection
  // SQL_ATTR_LOGIN_TIMEOUT
  cConnectionTimeoutDefault = -1; // SQL_LOGIN_TIMEOUT_DEFAULT = ULong(15); (-1) - Ignored.
  // Blob
  cBlobChunkSizeMin = 256;
  cBlobChunkSizeMax = 1024 * 1000;
  cBlobChunkSizeDefault = 40960;
  // Network
  cNetwrkPacketSizeDefault = 8192; //4096;
  cNetwrkPacketSizeMin = 512;
  // Query:
  // SQL_ATTR_CONNECTION_TIMEOUT
  coNetTimeoutDefault = -1; // (-1) - Ignored.
  // SQL_ATTR_QUERY_TIMEOUT
  cLockModeDefault = {$ifndef _debug_emulate_stmt_per_con_}
                     -1; // // (-1) == SQL_QUERY_TIMEOUT_DEFAULT = ULong(0)
                     {$else}
                     3;
                     {$endif}

{$IFDEF _DBX30_}
var
  gHostPlatform: THostPlatform = [
    //{$IFDEF LINUX}hpLinux{$ELSE}hpWindows{$ENDIF}
    //{$IFDEF CLR},hpCLR{$ENDIF}
    //hp32Bit, hp64bit
  ];
{$ENDIF}

// Array of transformations of types (date, time, date time) depending on the odbc version:
type
  TBindMapDateTimeOdbcIndexes = ( biDate, biTime, biDateTime );
  TBindMapDateTimeOdbc = array [TBindMapDateTimeOdbcIndexes] of SqlUInteger;
  PBindMapDateTimeOdbc = ^TBindMapDateTimeOdbc;

const
  //In ODBC 2.x, the C date, time, and timestamp data types are SQL_C_DATE, SQL_C_TIME, and SQL_C_TIMESTAMP.
  cBindMapDateTimeOdbc2: TBindMapDateTimeOdbc = ( SQL_C_DATE, SQL_C_TIME, SQL_C_TIMESTAMP);
  cBindMapDateTimeOdbc3: TBindMapDateTimeOdbc = ( SQL_C_TYPE_DATE, SQL_C_TYPE_TIME, SQL_C_TYPE_TIMESTAMP);

type

  //Internal Clone Connection managments:
  {begin:}
    // Connection + Statement cache, for databases that support
    // quantity of statements per connection (eg MS SqlServer):
    TDbxConStmtList = TList;
    // The list of connections(PDbxConStmt) is sorted on priorities of connections.
    PDbxHStmtNode = ^TDbxHStmtNode;
    TDbxHStmtNode = packed record
      HStmt: SqlHStmt;
      fPrevDbxHStmtNode: PDbxHStmtNode;
      fNextDbxHStmtNode: PDbxHStmtNode;
    end;
    TArrayOfDbxHStmtNode = array of TDbxHStmtNode;
    TDbxConStmt = packed record
      fHCon: SqlHDbc;
      fActiveDbxHStmtNodes: PDbxHStmtNode; // allocated statements list
      fNullDbxHStmtNodes: PDbxHStmtNode; // no allocated statememnts list
      fSqlHStmtAllocated: Integer; // Quantity allocated SqlHStmt.
      fActiveCursors: Integer; // Quantity of open cursors.
      fInTransaction: Integer;
      fAutoCommitMode: SqlUInteger;
      fRowsAffected: Integer; // Total quantity of changes during the current transaction or
                              // quantity of changes after execute of  last query.
      fOutOfDateCon: Boolean;
      fDeadConnection: Boolean; // SQL_ATTR_CONNECTION_DEAD
      // Memory buffer for DbxHStmtNodes:
      fBucketDbxHStmtNodes: TArrayOfDbxHStmtNode;
    end;
    PDbxConStmt = ^TDbxConStmt;
    TDbxConStmtInfo = packed record
      fDbxConStmt: PDbxConStmt;
      fDbxHStmtNode: PDbxHStmtNode;
    end;
    PDbxConStmtInfo = ^TDbxConStmtInfo;
  {end.}

  TSqlConnectionOdbc = class;
  TSqlCommandOdbc = class;
  TSqlCursorOdbc = class;

  TSqlDriverOdbc = class(TInterfacedObject, ISQLDriver)
  protected
    fOdbcApi: TOdbcApiProxy;
    fIsUnicodeOdbcApi: Boolean;
    //fDbxTraceCallbackEven: TSQLCallBackEvent; // not use in delphi
    //fDbxTraceClientData: Integer; // not use in delphi
    fDrvBlobSizeLimitK: Integer;
    fOdbcErrorLines: TStringList;
    fhEnv: SqlHEnv;
    fNativeErrorCode: SqlInteger;
    fSqlStateChars: TSqlState; // 5 Chars long + null terminator
    fDbxDrvRestrict: Longword;
    fIgnoreErrors: Boolean;
    fDBXVersion: Integer; // 30 == DBX 3.0 Up else DBX 2.5 Down
    fClientVersion: Integer; // Auto detection of the client supporting fldWIDESTRING, fldstWIDEMEMO. Values is like fDBXVersion.

    procedure AllocHCon(out HCon: SqlHDbc);
    procedure AllocHEnv;
    procedure FreeHCon(var HCon: SqlHDbc; DbxConStmt: PDbxConStmt; bIgnoreError: Boolean = False);
    procedure FreeHEnv;
    procedure RetrieveOdbcErrorInfo(
      CheckCode: SqlReturn;
      HandleType: Smallint;
      Handle: SqlHandle;
      DbxConStmt: PDbxConStmt;
      Connection: TSqlConnectionOdbc;
      Command: TSqlCommandOdbc;
      Cursor: TSqlCursorOdbc = nil;
      bClearErrorCount: Integer = 0;
      maxErrorCount: Integer = 0;
      eTraceCat: TRACECat = cTDBXTraceFlags_none);
    procedure OdbcCheck(
      CheckCode: SqlReturn;
      const OdbcFunctionName: AnsiString;
      HandleType: Smallint;
      Handle: SqlHandle;
      DbxConStmt: PDbxConStmt;
      Connection: TSqlConnectionOdbc = nil;
      Command: TSqlCommandOdbc = nil;
      Cursor: TSqlCursorOdbc = nil;
      maxErrorCount: Integer = 0;
      eTraceCat: TRACECat = cTDBXTraceFlags_none);
  protected
    { begin ISQLDriver methods }
    function getSQLConnection(
      out pConn: ISQLConnection
      ): SQLResult; stdcall;
    function SetOption(
      eDOption: TSQLDriverOption;
      PropValue: Longint
      ): SQLResult; stdcall;
    function GetOption(
      eDOption: TSQLDriverOption;
      PropValue: Pointer;
      MaxLength: Smallint;
      out Length: Smallint
      ): SQLResult; stdcall;
    { end ISQLDriver methods }
  public
    constructor Create(AOdbcApi: TOdbcApiProxy; bIsUnicodeOdbcApi: Boolean);
    destructor Destroy; override;
    procedure Drivers(DriverList: TStrings);
  end;

  { TSqlConnectionOdbc implements ISQLConnection }

  TSqlConnectionOdbc = class(TInterfacedObject, ISQLConnection25, ISqlConnectionOdbc)
  protected
    fConnectionErrorLines: TStringList;
    fOwnerDbxDriver: TSqlDriverOdbc;
    fDbxTraceCallbackEven: TSQLCallBackEvent;
    fDbxTraceClientData: Integer;
    fConnected: Boolean;
    fSafeMode: Boolean; // ignored many errors (silent mode).
    fConnectionClosed: Boolean; // supports of killed connection
    fConnBlobSizeLimitK: Integer;
    // Private fields below are specific to ODBC
    fhCon: SqlHDbc;
    fStatementPerConnection: SqlUSmallint;
    //Internal Clone Connection managments:
    {begin:}
      fDbxConStmtList: TDbxConStmtList;
      fDbxConStmtActive: Integer; // Quantity of active connections in cache.
      fCon0SqlHStmt: Integer; // count of active connection with not allocated SqlHStmt.
      fCurrDbxConStmt: PDbxConStmt; // Current/Last active connection. It is established after the
      // first connection and after performance of query changing the transactions data in a
      // mode (fRowsAffected).
    {end.}
    fWantQuotedTableName: Boolean;
    fOdbcConnectString: AnsiString;
    fOdbcConnectStringHidePassword: AnsiString;
    fConnConnectionString: AnsiString;
    fOdbcReturnedConnectString: AnsiString;
    fOdbcMaxColumnNameLen: SqlUSmallint;
    fOdbcMaxCatalogNameLen: SqlUSmallint;
    fOdbcMaxSchemaNameLen: SqlUSmallint;
    fOdbcMaxTableNameLen: SqlUSmallint;
    fOdbcMaxIdentifierLen: SqlUSmallint;
    fDbmsName: AnsiString;
    fDbmsType: TDbmsType;
    fDbmsVersionString: AnsiString;
    fDbmsVersionMajor: Integer;
    fDbmsVersionMinor: Integer;
    fDbmsVersionRelease: Integer;
    fDbmsVersionBuild: Integer;
    fOdbcDriverName: AnsiString;
    fOdbcDriverType: TOdbcDriverType;
    fOdbcDriverVersionString: AnsiString;
    fOdbcDriverVersionMajor: Integer;
    fOdbcDriverVersionMinor: Integer;
    fOdbcDriverVersionRelease: Integer;
    fOdbcDriverVersionBuild: Integer;
    fOdbcDriverLevel: Integer; // 2 or 3
    fInTransaction: Integer;
    fSupportsCatalog: Boolean;
    fSupportsSQLSTATISTICS: Boolean;
    fSupportsSQLPRIMARYKEYS: Boolean;
    fSupportsSchemaDML: Boolean;
    fSupportsSchemaProc: Boolean;
    fSupportsCatalogDML: Boolean;
    fSupportsCatalogProc: Boolean;
    fGetDataAnyColumn: Boolean;
    fCurrentCatalog: AnsiString;
    fQuoteChar: AnsiChar;
    fAutoCommitMode: SqlUInteger;
    fSupportsTransaction: Boolean;
    fSupportsNestedTransactions: Boolean;
    fSupportsTransactionMetadata: Boolean;
    fCurrentSchema: AnsiString; // This is no ODBC API call to get this!
    // Defined by option: fSupportsSchemaFilter
    fConnectionOptions: TConnectionOptions;
    fConnectionOptionsDrv: TConnectionOptions; // Driver Default Options
    fBlobChunkSize: Integer;
    fNetwrkPacketSize: Longint;
    {.$IFDEF _K3UP_}
    fQualifiedName: AnsiString;
    {.$ENDIF}
    {Ability to retrieve Error info}
    fNativeErrorCode: SqlInteger;
    fSqlStateChars: TSqlState; // 5 Chars long + null terminator
    {Bypass SetCatalog call}
    fDbxCatalog: AnsiString;
    fOdbcCatalogPrefix: AnsiString;
    fDbmsVersion: AnsiString;
    fLastStoredProc: TSqlCommandOdbc;
    {$IFDEF _RegExprParser_}
    fObjectNameParser: TObjectNameParser;
    {$ENDIF}
    fOdbcIsolationLevel: SqlUInteger;
    fSupportsBlockRead: Boolean;
    fSqlHStmtAllocated: Integer; // Quantity allocated SqlHStmt.
    fCursorPreserved: Boolean; // Characterizes an opportunity to continue work with the cursor
                               // after change of transaction.
    fActiveCursors: Integer; // Quantity of open cursors.
    fRowsAffected: Integer; // Total quantity of changes during the current transaction or quantity
                            // of changes after execute of  last query.
    fBindMapDateTimeOdbc: PBindMapDateTimeOdbc; // The reference to then table of values of bindings
                                                // for types: date, time, datetime (depends on the
                                                // version odbc).
    fConnectionTimeout: Integer; // ->: SQL_ATTR_LOGIN_TIMEOUT
    fNetworkTimeout: Integer; // ->: SQL_ATTR_CONNECTION_TIMEOUT
    fLockMode: Integer;  // ->: SQL_ATTR_QUERY_TIMEOUT
    fDecimalSeparator: AnsiChar;
    fPrepareSQL: Boolean;
    function FindFreeConnection(out DbxConStmtInfo: TDbxConStmtInfo;
      MaxStatementsPerConnection: Integer;
      bMetadataRead: Boolean = False;
      bOnlyPreservedCursors: Boolean = False): Boolean;
    procedure AllocHStmt(out HStmt: SqlHStmt;
      aDbxConStmtInfo: PDbxConStmtInfo = nil;
      bMetadataRead: Boolean = False);
    procedure CheckTransactionSupport;
    procedure SynchronizeInTransaction(var DbxConStmt: TDbxConStmt);
    procedure CloneOdbcConnection(out DbxConStmtInfo: TDbxConStmtInfo;
      bSynchronizeTransaction: Boolean = True);
    procedure FreeHStmt(out HStmt: SqlHStmt; aDbxConStmtInfo: PDbxConStmtInfo = nil);
    function GetMetaDataOption(
      eDOption: TSQLMetaDataOption;
      PropValue: Pointer;
      MaxLength: Smallint;
      out Length: Smallint
      ): SQLResult;
    function GetCurrentDbxConStmt: PDbxConStmt; overload;
    function GetCurrentDbxConStmt(out HStmt: SqlHStmt): PDbxConStmt; overload; {$IFDEF _INLINE_} inline; {$ENDIF}
    function GetCurrentConnectionHandle: SqlHDbc; {$IFDEF _INLINE_} inline; {$ENDIF}
    procedure OdbcCheck(OdbcCode: SqlReturn; const OdbcFunctionName: AnsiString;
      DbxConStmt: PDbxConStmt; TraceCat: TRACECat = cTDBXTraceFlags_none); {$IFDEF _INLINE_} inline; {$ENDIF}
    function RetrieveDriverName: SQLResult;
    function RetrieveDbmsOptions: SQLResult;
    function GetCatalog(aHConStmt: SqlHDbc = SQL_NULL_HANDLE): AnsiString;
    procedure GetCurrentCatalog(aHConStmt: SqlHDbc = SQL_NULL_HANDLE); {$IFDEF _INLINE_} inline; {$ENDIF}
    procedure TransactionCheck(const DbxConStmtInfo: TDbxConStmtInfo);
    procedure ClearConnectionOptions;
    procedure SetCurrentDbxConStmt(aDbxConStmt: PDbxConStmt); {$IFDEF _INLINE_} inline; {$ENDIF}
    function SetLoginTimeout(hCon: SqlHDbc; TimeoutSeconds: Integer): Boolean;
    function SetNetworkTimeout(hCon: SqlHDbc; TimeoutSeconds: Integer): Boolean;
    //function GetNetworkTimeout(hCon: SqlHDbc): Integer;
    function GetDefaultConnectionOptions(): PConnectionOptions;
    // Quotation methods:
    function DecodeObjectFullName({$IFNDEF _D11UP_}const {$ENDIF}ObjectFullName: AnsiString;
      var sCatalogName, sSchemaName, sObjectName: AnsiString; bStoredProcSpace: Boolean = False): Pointer;
    function EncodeObjectFullName(CatalogName, SchemaName, ObjectName: AnsiString;
      pTemplateInfo: Pointer = nil): AnsiString;
    function GetQuotedObjectName(const ObjectName: AnsiString; bStoredProcSpace: Boolean = False;
      bWithoutQuoting: Boolean = False): AnsiString;
    function GetUnquotedName(const Name: AnsiString): AnsiString;
    function GetUnquotedNameLen(const Name: AnsiString): Integer;
    function ObjectIsStoredProc(const Name: AnsiString): Boolean;
    // Quotation methods.
    {$IFDEF _DBXCB_}
    procedure DbxCallBackSendMsg(TraceCat: TRACECat; const Msg: AnsiString);
    procedure DbxCallBackSendMsgFmt(TraceCat: TRACECat; const FmtMsg: AnsiString; const FmtArgs: array of const);
    {$ENDIF}
  protected
    { begin ISQLConnection methods }
    function connect(
      ServerName: PAnsiChar;
      UserName: PAnsiChar;
      Password: PAnsiChar
      ): SQLResult; stdcall;
    function disconnect: SQLResult; stdcall;
    function getSQLCommand(
      out pComm: ISQLCommand25
      ): SQLResult; stdcall;
    function getSQLMetaData(
      out pMetaData: ISQLMetaData25
      ): SQLResult; stdcall;
    function SetOption(
      eConnectOption: TSQLConnectionOption;
      lValue: Longint
      ): SQLResult; stdcall;
    function GetOption(
      eDOption: TSQLConnectionOption
      ; PropValue: Pointer;
      MaxLength: Smallint;
      out Length: Smallint
      ): SQLResult; stdcall;
    function beginTransaction(
      TranID: Longword
      ): SQLResult; stdcall;
    function commit(
      TranID: Longword
      ): SQLResult; stdcall;
    function rollback(
      TranID: Longword
      ): SQLResult; stdcall;
    function getErrorMessage(
      Error: PAnsiChar
      ): SQLResult; stdcall;
    function getErrorMessageLen(
      out ErrorLen: Smallint
      ): SQLResult; stdcall;
    { end ISQLConnection methods }
    { begin ISQLConnectionOdbc methods }
    function GetDbmsName: AnsiString;
    function GetDbmsType: TDbmsType;
    function GetDbmsVersionString: AnsiString;
    function GetDbmsVersionMajor: Integer;
    function GetDbmsVersionMinor: Integer;
    function GetDbmsVersionRelease: Integer;
    function GetDbmsVersionBuild: Integer;
    function GetLastOdbcSqlState: PAnsiChar;
    function GetOdbcConnectString: AnsiString;
    procedure GetOdbcConnectStrings(ConnectStringList: TStrings);
    function GetOdbcDriverName: AnsiString;
    function GetOdbcDriverType: TOdbcDriverType;
    function GetOdbcDriverVersionString: AnsiString;
    function GetOdbcDriverVersionMajor: Integer;
    function GetOdbcDriverVersionMinor: Integer;
    function GetOdbcDriverVersionRelease: Integer;
    function GetOdbcDriverVersionBuild: Integer;
    function GetCursorPreserved: Boolean;
    function GetIsSystemODBCManager: Boolean;
    function GetOdbcDriverLevel: Integer;
    function GetSupportsSqlPrimaryKeys: Boolean;
    function GetStatementsPerConnection: Integer;
    function GetEnvironmentHandle: Pointer;
    function GetConnectionHandle: Pointer;
    function GetOdbcApiIntf: IUnknown;
    function GetDecimalSeparator: AnsiChar;
    { end ISQLConnectionOdbc methods }
  public
    constructor Create(OwnerDbxDriver: TSqlDriverOdbc); {$IFDEF _DBX30_} virtual; {$ENDIF}
    destructor Destroy; override;

    { begin additional public methods/props }
    property DbmsName: AnsiString read fDbmsName;
    property DbmsType: TDbmsType read fDbmsType;
    property DbmsVersionMajor: Integer read fDbmsVersionMajor;
    property DbmsVersionString: AnsiString read fDbmsVersion;
    property LastOdbcSqlState: PAnsiChar read GetLastOdbcSqlState;
    property OdbcConnectString: AnsiString read fOdbcConnectString;
    property OdbcDriverName: AnsiString read fOdbcDriverName;
    property OdbcDriverType: TOdbcDriverType read fOdbcDriverType;
    property OdbcDriverVersionMajor: Integer read fOdbcDriverVersionMajor;
    property OdbcDriverVersionString: AnsiString read fOdbcDriverVersionString;
    { end additional public methods/props }
  end;

  { TSqlCommandOdbc implements ISQLCommand }

  TStmtCommandStatus = set of (scsStmtBinded, scsStmtExecuted, scsIsCursor);

  TSqlCommandOdbc = class(TInterfacedObject, ISQLCommand25, ISqlCommandOdbc)
  protected
    fOwnerDbxConnection: TSqlConnectionOdbc;
    fOwnerDbxDriver: TSqlDriverOdbc;
    fCommandBlobSizeLimitK: Integer;
    fCommandRowSetSize: Integer; // New for Delphi 6.02. Map into ODBC option: SQL_ATTR_ROW_ARRAY_SIZE
    fSupportsBlockRead: Boolean; // It is used in vapour with fCommandRowSetSize. (Default = True).
    // ansi odbc api:
    fSql: AnsiString; // fSQL is saved in prepare / executeImmediate
    fSqlPrepared: AnsiString;
    // unicode odbc api:
    fSqlW: WideString; // fSQL is saved in prepare / executeImmediate
    fSqlPreparedW: WideString;
    // Private fields below are specific to ODBC
    fHStmt: SqlHStmt;
    fOdbcParamList: TList;
    fOdbcRowsAffected: Longword;
    fTrimChar: Boolean;
    fExecutedOk: Boolean;
    fPreparedOnly: Boolean;
    fSupportsMixedFetch: Boolean; // flag to using SQL_ATTR_CURSOR_TYPE as SQL_CURSOR_STATIC. Need
                                  // for "ARRAY FETCH" jointly witch SqlSetPos & SqlGetData.
    fStoredProc: Integer;
    fStoredProcPackName: AnsiString;
    fStoredProcWithResult: Boolean;
    fCatalogName: AnsiString;
    fSchemaName: AnsiString;
    fIsMoreResults: Integer; // Multiple cursors flag. Values :
      // (-1) - unknown;
      // (+0) - not more results;
      // (+1) - is more results; 2 - is more results and is cached SQLMoreREsults result into fStmt
    fStmtStatus: TStmtCommandStatus;
    //Internal Clone Connection managments:
    {begin:}
    fDbxConStmtInfo: TDbxConStmtInfo;// handle fStatementPerConnection and Transaction
    {end.}
    procedure AddError(eError: Exception); //{$IFDEF _INLINE_} inline; {$ENDIF}
    procedure OdbcCheck(OdbcCode: SqlReturn; const OdbcFunctionName: AnsiString; eTraceCat: TRACECat = cTDBXTraceFlags_none); {$IFDEF _INLINE_} inline; {$ENDIF}
    function BuildStoredProcSQL: AnsiString;
    procedure CloseStmt(bClearParams: Boolean = True; bFreeStmt: Boolean = True);
    procedure ClearParams;
    function GetConnectionHandle: SqlHDbc;
    procedure DoAllocateStmt;
    procedure DoUnprepareStmt;
    function DoSQLMoreResults: OdbcApi.SqlReturn;
    procedure DoAllocateParams(ParamCount: Word);
    procedure DoExpandParams(ParamCount: Word);
    //
    // DoPrepare, DoExecute, DoExecuteImmediate: ( bUseUnicodeOdbc == True only when SQL is PWideChar ).
    //
    function DoPrepare(SQL: PAnsiChar; ParamCount: Word; UpdateParams, bPrepareSQL: Boolean; bUseUnicodeOdbc: Boolean): SQLResult;
    function DoExecute(var Cursor: ISQLCursor25; bUseUnicodeOdbc: Boolean): SQLResult;
    function DoExecuteImmediate(SQL: PAnsiChar; var Cursor: ISQLCursor25; bUseUnicodeOdbc: Boolean): SQLResult;
  protected
    { begin ISQLCommand methods }
    function SetOption(
      eSqlCommandOption: TSQLCommandOption;
      ulValue: Integer
      ): SQLResult; stdcall;
    function GetOption(
      eSqlCommandOption: TSQLCommandOption;
      // Borland changed GetOption function prototype between Delphi V6 and V7
      // Kylix 3 uses Delphi 6 prototype
      {$IFDEF _D7UP_}
      PropValue: Pointer;
      {$ELSE}
      var pValue: Integer;
      {$ENDIF}
      MaxLength: Smallint;
      out Length: Smallint
      ): SQLResult; stdcall;
    function setParameter(
      ulParameter: Word;
      ulChildPos: Word;
      eParamType: TSTMTParamType;
      uLogType: Word;
      uSubType: Word;
      iPrecision: Integer;
      iScale: Integer;
      Length: Longword;
      pBuffer: Pointer;
      bIsNull: Integer
      ): SQLResult; stdcall;
    function getParameter(
      ParameterNumber: Word;
      ulChildPos: Word;
      Value: Pointer;
      Length: Integer;
      var IsBlank: Integer
      ): SQLResult; stdcall;
    function prepare(
      SQL: PAnsiChar;
      ParamCount: Word
      ): SQLResult; stdcall;
    function execute(
      var Cursor: ISQLCursor25
      ): SQLResult; stdcall;
    function ExecuteImmediate(
      SQL: PAnsiChar;
      var Cursor: ISQLCursor25
      ): SQLResult; stdcall;
    function getNextCursor(
      var Cursor: ISQLCursor25
      ): SQLResult; stdcall;
    function getRowsAffected(
      var Rows: Longword
      ): SQLResult; stdcall;
    function close: SQLResult; stdcall;
    function getErrorMessage(
      Error: PAnsiChar
      ): SQLResult; stdcall;
    function getErrorMessageLen(
      out ErrorLen: Smallint
      ): SQLResult; stdcall;
    { end ISQLCommand methods }
    { begin ISQLCommandOdbc methods }
    procedure Cancel;
    function SetLockTimeout(TimeoutSeconds: Integer): Boolean;
    function GetLockTimeout: Integer;
    { end ISQLCommandOdbc methods }
  public
    constructor Create(OwnerDbxConnection: TSqlConnectionOdbc);
    destructor Destroy; override;
    property hOdbcStmt: SqlHStmt read fHStmt;
    property ConnectionHandle: SqlHDbc read GetConnectionHandle;
  end;

  { TSQLMetaDataOdbc implements ISQLMetaData }

  TSQLMetaDataOdbc = class(TInterfacedObject, ISQLMetaData25)
  protected
    fSupportWideString: Boolean;
    fOwnerDbxConnection: TSqlConnectionOdbc;
    fMetaDataErrorLines: TStringList;
    fMetaSchemaName: AnsiString;
    fMetaCatalogName: AnsiString;
    fMetaPackName: AnsiString;

    function DoGetTables(
      TableName: PAnsiChar;
      TableType: Longword;
      out Cursor: Pointer;
      bUnicode: Boolean
      ): SQLResult; stdcall;

  protected
    { begin ISQLMetaData methods }
    function SetOption(
      eDOption: TSQLMetaDataOption;
      PropValue: Longint
      ): SQLResult; stdcall;
    function GetOption(
      eDOption: TSQLMetaDataOption;
      PropValue: Pointer;
      MaxLength: Smallint;
      out Length: Smallint
      ): SQLResult; stdcall;
    function getObjectList(
      eObjType: TSQLObjectType;
      out Cursor: ISQLCursor25
      ): SQLResult; stdcall;
    function getTables(
      TableName: PAnsiChar;
      TableType: Longword;
      out Cursor: ISQLCursor25
      ): SQLResult; stdcall;
    function getProcedures(
      ProcedureName: PAnsiChar;
      ProcType: Longword;
      out Cursor: ISQLCursor25
      ): SQLResult; stdcall;
    function getColumns(
      TableName: PAnsiChar;
      ColumnName: PAnsiChar;
      ColType: Longword;
      out Cursor: ISQLCursor25
      ): SQLResult; stdcall;
    function getProcedureParams(
      ProcName: PAnsiChar;
      ParamName: PAnsiChar;
      out Cursor: ISQLCursor25
      ): SQLResult; stdcall;
    function getIndices(
      TableName: PAnsiChar;
      IndexType: Longword;
      out Cursor: ISQLCursor25
      ): SQLResult; stdcall;
    function getErrorMessage(
      Error: PAnsiChar
      ): SQLResult; stdcall;
    function getErrorMessageLen(
      out ErrorLen:
      Smallint
      ): SQLResult; stdcall;
    { end ISQLMetaData methods }
  public
    constructor Create(ASupportWideString: Boolean; OwnerDbxConnection: TSqlConnectionOdbc);
    destructor Destroy; override;
  end;

  { TSqlCursorOdbc implements ISQLCursor }

  TOdbcBindCol = class;

  TSqlCursorOdbc = class(TInterfacedObject, ISQLCursor25)
  protected
    fOwnerCommand: TSqlCommandOdbc;
    fOwnerDbxConnection: TSqlConnectionOdbc;
    fOwnerDbxDriver: TSqlDriverOdbc;
    fRowNo: Double;
    // Private fields below are specific to ODBC
    fHStmt: SqlHStmt;
    fOdbcNumCols: SqlSmallint;
    fOdbcBindList: TList;
    fCursorFetchRowCount: Integer; // It is necessary for usage SQL_ATTR_ROW_ARRAY_SIZE (ARAY FETCH).
    fOdbcBindBuffer: Pointer; // The common buffer for data receiving.
    fOdbcBindBufferRowSize: Integer;
    fOdbcRowsStatus: array of SqlSmallint;
    fOdbcBindBufferPos: Integer;
    fOdbcRowsFetched: SqlInteger;
    fOdbcLateBoundsFound: Boolean; // flag to using SQLSetPos() when is used "ARAY FETCH"
    fOdbcColumnsFetchConsecutively: Boolean; // == fConnectionOptions[coCFC] <> osOff
    procedure BindResultSet;
    procedure OdbcCheck(OdbcCode: SqlReturn;
      const OdbcFunctionName: AnsiString;
      maxErrorCount: Integer = 0); {$IFDEF _INLINE_} inline; {$ENDIF}
    procedure DoFetchLongData(aOdbcBindCol: TOdbcBindCol;
      bAllowFragmentation: Boolean;
      FirstChunkSize: Integer);
    procedure FetchLongData(ColNo: SqlUSmallint;
      bAllowFragmentation: Boolean = False;
      FirstChunkSize: Integer = 0); {$IFDEF _INLINE_} inline; {$ENDIF}
    procedure DoFetchLateBoundData(aOdbcBindCol: TOdbcBindCol); {$IFDEF _INLINE_} inline; {$ENDIF}
    procedure FetchLateBoundData(ColNo: SqlUSmallint); {$IFDEF _INLINE_} inline; {$ENDIF}
    procedure CheckFetchCacheColumns(ColNoLimit: SqlUSmallint);
    procedure AddError(eError: Exception); {$IFDEF _INLINE_} inline; {$ENDIF}
    procedure ClearCursor(bFreeStmt: Boolean = False);
  protected
    { begin ISQLCusror methods }
    function SetOption(
      eOption: TSQLCursorOption;
      PropValue: Longint
      ): SQLResult; stdcall;
    function GetOption(
      eOption: TSQLCursorOption;
      PropValue: Pointer;
      MaxLength: Smallint;
      out Length: Smallint
      ): SQLResult; stdcall;
    function getErrorMessage(
      Error: PAnsiChar
      ): SQLResult; stdcall;
    function getErrorMessageLen(
      out ErrorLen: Smallint
      ): SQLResult; stdcall;
    function getColumnCount(
      var pColumns: Word
      ): SQLResult; stdcall;
    function getColumnNameLength(
      ColumnNumber: Word;
      var pLen: Word): SQLResult; stdcall;
    function getColumnName(
      ColumnNumber: Word;
      pColumnName: PAnsiChar
      ): SQLResult; stdcall;
    function getColumnType(
      ColumnNumber: Word;
      var puType: Word;
      var puSubType: Word
      ): SQLResult; stdcall;
    function getColumnLength(
      ColumnNumber: Word;
      var pLength: Longword
      ): SQLResult; stdcall;
    function getColumnPrecision(
      ColumnNumber: Word;
      var piPrecision: Smallint
      ): SQLResult; stdcall;
    function getColumnScale(
      ColumnNumber: Word;
      var piScale: Smallint
      ): SQLResult; stdcall;
    function isNullable(
      ColumnNumber: Word;
      var Nullable: LongBool
      ): SQLResult; stdcall;
    function isAutoIncrement(
      ColumnNumber: Word;
      var AutoIncr: LongBool
      ): SQLResult; stdcall;
    function isReadOnly(
      ColumnNumber: Word;
      var ReadOnly: LongBool
      ): SQLResult; stdcall;
    function isSearchable(
      ColumnNumber: Word;
      var Searchable: LongBool
      ): SQLResult; stdcall;
    function isBlobSizeExact(
      ColumnNumber: Word;
      var IsExact: LongBool
      ): SQLResult; stdcall;
    function next: SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getShort(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getLong(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getDouble(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getBcd(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getTimeStamp(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getTime(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getDate(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getBytes(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getBlobSize(
      ColumnNumber: Word;
      var Length: Longword;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getBlob(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool;
      Length: Longword): SQLResult; stdcall;
    { end ISQLCusror methods }
  public
    constructor Create(OwnerCommand: TSqlCommandOdbc);
    destructor Destroy; override;
  end;

  TSqlCursorMetaDataColumns = class; // forward declaration
  TSqlCursorMetaDataTables = class; // forward declaration
  TMetaIndexColumn = class; // forward declaration

  { TMetaTable - represents 1 row returned by ISQLMetaData.GetTables }

  TMetaTable = class(TObject)
  protected
    fCat: PAnsiChar;
    fSchema: PAnsiChar;
    fTableName: PAnsiChar;
    fWCat: PWideChar;
    fWSchema: PWideChar;
    fWTableName: PWideChar;
    fQualifiedTableName: PAnsiChar;
    fTableType: Integer;

    fPrimaryKeyColumn1: TMetaIndexColumn;
    fIndexColumnList: TList;
  public
    constructor Create(
      SqlConnectionOdbc: TSqlConnectionOdbc;
      Cat: PAnsiChar;
      Schema: PAnsiChar;
      TableName: PAnsiChar;
      TableType: Integer);
    constructor CreateW(
      SqlConnectionOdbc: TSqlConnectionOdbc;
      Cat: PWideChar;
      Schema: PWideChar;
      TableName: PWideChar;
      TableType: Integer);
    destructor Destroy; override;
  end;

  { TMetaColumn - represents 1 row returned by ISQLMetaData.GetColumns }

  TMetaColumn = class(TObject)
  protected
    fMetaTable: TMetaTable;
    fColumnName: PAnsiChar;
    fWColumnName: PWideChar;
    fOrdinalPosition: Smallint;
    fLength: Integer;
    fTypeName: PAnsiChar;
    fWTypeName: PWideChar;
    fPrecision: Integer;
    fDecimalScale: Smallint;
    fDbxType: Smallint;
    fDbxSubType: Smallint;
    fDbxNullable: Smallint;
    fDbxColumnType: Smallint;
  public
    constructor Create(
      ColumnName: PAnsiChar;
      OrdinalPosition: Smallint;
      TypeName: PAnsiChar);
    constructor CreateW(
      ColumnName: PWideChar;
      OrdinalPosition: Smallint;
      TypeName: PWideChar);
    destructor Destroy; override;
  end;

  { TMetaIndexColumn - represents 1 row returned by ISQLMetaData.GetIndices }

  TMetaIndexColumn = class(TObject)
  protected
    fMetaTable: TMetaTable;
    fCatName: PAnsiChar;
    fWCatName: PWideChar;
    fSchemaName: PAnsiChar;
    fWSchemaName: PWideChar;
    fTableName: PAnsiChar;
    fWTableName: PWideChar;
    fIndexName: PAnsiChar;
    fWIndexName: PWideChar;
    fIndexColumnName: PAnsiChar;
    fWIndexColumnName: PWideChar;
    fColumnPosition: Smallint;
    fIndexType: Smallint;
    fSortOrder: AnsiChar;
    fWSortOrder: WideChar;
    fFilter: PAnsiChar;
    fWFilter: PWideChar;
  public
    constructor Create(
      MetaTable: TMetaTable;
      CatName, SchemaName, TableName, IndexName: PAnsiChar;
      IndexColumnName: PAnsiChar);
    constructor CreateW(
      MetaTable: TMetaTable;
      CatName, SchemaName, TableName, IndexName: PWideChar;
      IndexColumnName: PAnsiChar);
    destructor Destroy; override;
  end;

  { TMetaProcedure - represents 1 row returned by ISQLMetaData.GetProcedures }

  TMetaProcedure = class(TObject)
  protected
    fCat: PAnsiChar;
    fWCat: PWideChar;
    fSchema: PAnsiChar;
    fWSchema: PWideChar;
    fProcName: PAnsiChar;
    fWProcName: PWideChar;
    fProcType: Integer;
  public
    constructor Create(
      Cat: PAnsiChar;
      Schema: PAnsiChar;
      ProcName: PAnsiChar;
      ProcType: Integer);
    constructor CreateW(
      Cat: PWideChar;
      Schema: PWideChar;
      ProcName: PWideChar;
      ProcType: Integer);
    destructor Destroy; override;
  end;

  { TMetaProcedureParam - represents 1 row returned by ISQLMetaData.GetProcedureParams }

  TMetaProcedureParam = class(TObject)
  protected
    fMetaProcedure: TMetaProcedure;
    fParamName: PAnsiChar;
    fWParamName: PWideChar;
    fDataTypeName: PAnsiChar;
    fWDataTypeName: PWideChar;
    fParamType: TSTMTParamType;
    fDataType: Smallint;
    fDataSubtype: Smallint;
    fPrecision: Integer;
    fScale: Smallint;
    fLength: Integer;
    fNullable: Smallint;
    fPosition: Smallint;
  public
    constructor Create(ParamName: PAnsiChar);
    constructor CreateW(ParamName: PWideChar);
    destructor Destroy; override;
  end;

  { TColumnNames / TColumnTypes used by TSqlCursorMetaData}

  TColumnNames = array[0..MaxListSize] of AnsiString;
  TColumnTypes = array[0..MaxListSize] of Word;
  PColumnNames = ^TColumnNames;
  PColumnTypes = ^TColumnTypes;

  { TSqlCursorMetaData - parent for all the MetaData cursor classes}

  TSqlCursorMetaData = class(TInterfacedObject, ISQLCursor25)
  protected
    fSqlCursorErrorMsg: TStringList;

    fSupportWideString: Boolean;
    fOwnerMetaData: TSqlMetaDataOdbc;
    fSqlConnectionOdbc: TSqlConnectionOdbc;
    fSqlDriverOdbc: TSqlDriverOdbc;

    fHStmt: SqlHStmt;

    fRowNo: Integer;
    fColumnCount: Integer;
    fColumnNames: PColumnNames;
    fColumnTypes: PColumnTypes;
    fMetaCatalogName: PAnsiChar;
    fMetaSchemaName: PAnsiChar;
    fMetaTableName: PAnsiChar;

    procedure OdbcCheck(OdbcCode: SqlReturn; const OdbcFunctionName: AnsiString); {$IFDEF _INLINE_} inline; {$ENDIF}
    procedure ParseTableNameBase(TableName: PAnsiChar);
    procedure ParseTableName(CatalogName, SchemaName, TableName: PAnsiChar);
    function DescribeAllocBindString(ColumnNo: SqlUSmallint; var BindString: PAnsiChar;
      var BindInd: SqlInteger; bIgnoreError: Boolean = False): Boolean;
    function DescribeAllocBindWString(ColumnNo: SqlUSmallint; var BindString: PWideChar;
      var BindInd: SqlInteger; bIgnoreError: Boolean = False): Boolean;
    function BindInteger(ColumnNo: SqlUSmallint; var BindInteger: Integer;
      BindInd: PSqlInteger; bIgnoreError: Boolean = False): Boolean;
    function BindSmallint(ColumnNo: SqlUSmallint; var BindSmallint: Smallint;
      PBindInd: PSqlInteger; bIgnoreError: Boolean = False): Boolean;
    procedure ClearMetaData; {$IFDEF _INLINE_} inline; {$ENDIF}
  protected
    { begin ISQLCusror methods }
    function SetOption(
      eOption: TSQLCursorOption;
      PropValue: Longint
      ): SQLResult; stdcall;
    function GetOption(
      eOption: TSQLCursorOption;
      PropValue: Pointer;
      MaxLength: Smallint;
      out Length: Smallint
      ): SQLResult; stdcall;
    function getErrorMessage(
      Error: PAnsiChar
      ): SQLResult; stdcall;
    function getErrorMessageLen(
      out ErrorLen: Smallint
      ): SQLResult; stdcall;
    function getColumnCount(
      var pColumns: Word
      ): SQLResult; stdcall;
    function getColumnNameLength(
      ColumnNumber: Word;
      var pLen: Word): SQLResult; stdcall;
    function getColumnName(
      ColumnNumber: Word;
      pColumnName: PAnsiChar
      ): SQLResult; stdcall;
    function getColumnType(
      ColumnNumber: Word;
      var puType: Word;
      var puSubType: Word
      ): SQLResult; stdcall;
    function getColumnLength(
      ColumnNumber: Word;
      var pLength: Longword
      ): SQLResult; stdcall;
    function getColumnPrecision(
      ColumnNumber: Word;
      var piPrecision: Smallint
      ): SQLResult; stdcall;
    function getColumnScale(
      ColumnNumber: Word;
      var piScale: Smallint
      ): SQLResult; stdcall;
    function isNullable(
      ColumnNumber: Word;
      var Nullable: LongBool
      ): SQLResult; stdcall;
    function isAutoIncrement(
      ColumnNumber: Word;
      var AutoIncr: LongBool
      ): SQLResult; stdcall;
    function isReadOnly(
      ColumnNumber: Word;
      var ReadOnly: LongBool
      ): SQLResult; stdcall;
    function isSearchable(
      ColumnNumber: Word;
      var Searchable: LongBool
      ): SQLResult; stdcall;
    function isBlobSizeExact(
      ColumnNumber: Word;
      var IsExact: LongBool
      ): SQLResult; stdcall;
    function next: SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getShort(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getLong(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getDouble(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getBcd(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getTimeStamp(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getTime(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getDate(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getBytes(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getBlobSize(
      ColumnNumber: Word;
      var Length: Longword;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getBlob(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool;
      Length: Longword): SQLResult; stdcall;
    { end ISQLCusror methods }
  public
    constructor Create(ASupportWideString: Boolean; OwnerSqlMetaData: TSqlMetaDataOdbc);
    destructor Destroy; override;
  end;

  { TSqlCursorMetaDataTables - implements cursor returned by ISQLMetaData.GetTables }

  TSqlCursorMetaDataTables = class(TSQLCursorMetaData, ISQLCursor25)
  protected
    fTableList: TList;
    fMetaTableCurrent: TMetaTable;
    fCatLenMax: Integer;
    fSchemaLenMax: Integer;
    fQualifiedTableLenMax: Integer;

    procedure FetchTables(SearchTableName: PAnsiChar;
      SearchTableType: Longword; bUnicode: Boolean);
    procedure Clear;
    // Delphi 2006 Up (DBX3 Up).
    function DoGetWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
  protected
    function getColumnLength(
      ColumnNumber: Word;
      var pLength: Longword
      ): SQLResult; stdcall;
    function getColumnPrecision(
      ColumnNumber: Word;
      var piPrecision: Smallint
      ): SQLResult; stdcall;
    function getLong(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function next: SQLResult; stdcall;
  public
    constructor Create(ASupportWideString: Boolean; OwnerMetaData: TSQLMetaDataOdbc);
    destructor Destroy; override;
  end;

  { TSqlCursorMetaDataColumns - implements cursor returned by ISQLMetaData.GetColumns }

  TSqlCursorMetaDataColumns = class(TSQLCursorMetaData, ISQLCursor25)
  protected
    fTableList: TList;
    fColumnList: TList;

    fMetaTableCurrent: TMetaTable;
    fMetaColumnCurrent: TMetaColumn;

    fCatLenMax: Integer;
    fSchemaLenMax: Integer;
    fTableLenMax: Integer;
    fColumnLenMax: Integer;
    fTypeNameLenMax: Integer;

    procedure FetchColumns(SearchCatalogName, SearchSchemaName,
      SearchTableName, SearchColumnName: PAnsiChar; SearchColType: Longword);
    procedure Clear;
    // Delphi 2006 Up (DBX3 Up).
    function DoGetWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
  protected
    function getColumnLength(
      ColumnNumber: Word;
      var pLength: Longword
      ): SQLResult; stdcall;
    function getColumnPrecision(
      ColumnNumber: Word;
      var piPrecision: Smallint
      ): SQLResult; stdcall;
    function getLong(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getShort(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function next: SQLResult; stdcall;
  public
    constructor Create(ASupportWideString: Boolean; OwnerMetaData: TSQLMetaDataOdbc);
    destructor Destroy; override;
  end;

  { TSqlCursorMetaDataIndexes - implements cursor returned by ISQLMetaData.GetIndices }

  TSqlCursorMetaDataIndexes = class(TSQLCursorMetaData, ISQLCursor25)
  protected
    fIndexList: TList;
    fTableList: TList;
    fCurrentIndexColumn: TMetaIndexColumn;

    fCatLenMax: Integer;
    fSchemaLenMax: Integer;
    fTableLenMax: Integer;
    fIndexNameLenMax: Integer;
    fIndexColumnNameLenMax: Integer;
    fPkCatalogLenMax: Integer;
    fPkSchemaLenMax: Integer;
    fPkTableLenMax: Integer;
    fPkNameLenMax: Integer;
    fFilterLenMax: Integer;

    procedure FetchIndexes(SearchCatalogName, SearchSchemaName,
      SearchTableName: PAnsiChar; SearchIndexType: Longword);
    // Delphi 2006 Up (DBX3 Up).
    function DoGetWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
  protected
    function getColumnLength(
      ColumnNumber: Word;
      var pLength: Longword
      ): SQLResult; stdcall;
    function getColumnPrecision(
      ColumnNumber: Word;
      var piPrecision: Smallint
      ): SQLResult; stdcall;
    function getLong(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getShort(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function next: SQLResult; stdcall;
  public
    constructor Create(ASupportWideString: Boolean; OwnerMetaData: TSQLMetaDataOdbc);
    destructor Destroy; override;
  end;

  { TSqlCursorMetaDataProcedures - implements cursor returned by ISQLMetaData.GetProcedures }

  TSqlCursorMetaDataProcedures = class(TSQLCursorMetaData, ISQLCursor25)
  protected
    fProcList: TList;
    fCatLenMax: Integer;
    fSchemaLenMax: Integer;
    fProcLenMax: Integer;
    fMetaProcedureCurrent: TMetaProcedure;

    procedure FetchProcedures(ProcedureName: PAnsiChar; ProcType: Longword);
    // Delphi 2006 Up (DBX3 Up).
    function DoGetWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
  protected
    function getColumnLength(
      ColumnNumber: Word;
      var pLength: Longword
      ): SQLResult; stdcall;
    function getColumnPrecision(
      ColumnNumber: Word;
      var piPrecision: Smallint
      ): SQLResult; stdcall;
    function getLong(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getShort(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function next: SQLResult; stdcall;
  public
    constructor Create(ASupportWideString: Boolean; OwnerMetaData: TSQLMetaDataOdbc);
    destructor Destroy; override;
  end;

  { TSqlCursorMetaDataProcedureParams - implements cursor returned by ISQLMetaData.GetProcedureParams }

  TSqlCursorMetaDataProcedureParams = class(TSQLCursorMetaData, ISQLCursor25)
  protected
    fProcList: TList;
    fProcColumnList: TList;

    fCatLenMax: Integer;
    fSchemaLenMax: Integer;
    fProcNameLenMax: Integer;
    fParamNameLenMax: Integer;
    fDataTypeNameLenMax: Integer;

    fMetaProcedureParamCurrent: TMetaProcedureParam;

    procedure FetchProcedureParams(SearchCatalogName, SearchSchemaName,
      SearchProcedureName, SearchParamName: PAnsiChar);
    // Delphi 2006 Up (DBX3 Up).
    function DoGetWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
  protected
    function getColumnLength(
      ColumnNumber: Word;
      var pLength: Longword
      ): SQLResult; stdcall;
    function getColumnPrecision(
      ColumnNumber: Word;
      var piPrecision: Smallint
      ): SQLResult; stdcall;
    function getLong(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getShort(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function next: SQLResult; stdcall;
  public
    constructor Create(ASupportWideString: Boolean; OwnerMetaData: TSQLMetaDataOdbc);
    destructor Destroy; override;
  end;

  SqlByte = Byte; // The description of type for abstraction of the reference
  PSqlByte = ^SqlByte;
  PSqlBigInt = ^SqlBigInt;
  PSqlDouble = ^SqlDouble; // not founfd in OdbcApi.pas
  TArrayOfBytes = array[0..255] of Byte;
  PArrayOfBytes = ^TArrayOfBytes;
  TArrayOfWords = array[0..128] of Word;
  PArrayOfWords = ^TArrayOfWords;

  TOdbcHostVarAddress = record // Variants of references to the buffer of the data of a column
    case SmallInt of
      0:               (Ptr: SqlPointer);
      SQL_C_CHAR:      (ptrAnsiChar: PAnsiChar);
      SQL_C_WCHAR:     (ptrWideChar: PWideChar);
      SQL_C_LONG:      (ptrSqlInteger: PSqlInteger);
      SQL_C_SHORT:     (ptrSqlSmallint: PSqlSmallint);
      SQL_C_DOUBLE:    (ptrSqlDouble: PSqlDouble);
      SQL_C_DATE:      (ptrSqlDateStruct: PSqlDateStruct);
      SQL_C_TIME:      (ptrSqlTimeStruct: PSqlTimeStruct);
      SQL_C_TIMESTAMP: (ptrOdbcTimestamp: POdbcTimestamp);
      SQL_C_BIT:       (ptrSqlByte: PSqlByte);
      SQL_C_SBIGINT:   (ptrSqlBigInt: PSqlBigInt);
      SQL_C_BINARY:    (ptrBytesArray: PArrayOfBytes); // - for debug view
      SQL_C_BINARY+1:  (ptrWordsArray: PArrayOfWords); // - for debug view
  end;

  PBlobChunkCollectionItem = ^TBlobChunkCollectionItem;
  TBlobChunkCollectionItem = record
    Data: PAnsiChar;
    Size: Longint;
    NextFragment: PBlobChunkCollectionItem;
  end;

  TBlobChunkCollection = class(TInterfacedObject, IBlobChunkCollection)
  protected
    fFragments: PBlobChunkCollectionItem;
    fFragmentLast: PBlobChunkCollectionItem;
    fSize: Int64;
    {$IFDEF _DEBUG_}
    fCount: Integer;
    {$ENDIF}
  protected
    { IBlobChunkCollection }
    function GetSize: Int64;
    procedure Read(var Buffer: Pointer);
    function ReadBlobToVariant(out Data: Variant): int64;
    function ReadBlobToStream(Stream: ISequentialStream): Int64;
    procedure Clear;
  public
    destructor Destroy; override;
    procedure AddFragment(Data: Pointer; DataSize: LongInt);

    //{$IFDEF _DEBUG_}
    //property Count: Integer read fCount;
    //{$ENDIF}
  end;

  TOdbcBindCol = class(TObject)
  public
    fOdbcColNo: Integer;
    fColName: PAnsiChar;
    fColNameW: WideString;
    fColNameSize: SqlSmallint;
    fSqlType: SqlSmallint;
    fColSize: SqlUInteger;
    fColScale: SqlSmallint;
    fNullable: SqlSmallint;
    fColValueSizePtr: PSqlInteger; // value allocated in Buffer
    fColValueSizeLoc: SqlInteger;
    fDbxType: Word;
    fDbxSubType: Word;
    fOdbcHostVarType: SqlSmallint;
    fOdbcHostVarSize: SqlUInteger;
    fOdbcHostVarAddress: TOdbcHostVarAddress; // pointer to value
    fOdbcHostVarChunkSize: SqlUInteger;
    fOdbcLateBound: Boolean;
    fIsFetched: Boolean;
    fIsBuffer: Boolean; // Flag indicating the local buffer for BLOB. fOdbcHostVarAddress should be allocated and released.
    fBlobChunkCollection: TBlobChunkCollection; // BLOB fragmented collection
    fReadOnly: SqlSmallint;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TOdbcBindParamRec = packed record
    case Smallint of
      SQL_C_CHAR: (OdbcParamValueString: array[0..255] of AnsiChar);
      SQL_C_WCHAR: (OdbcParamValueWideString: array[0..128] of WideChar);
      SQL_C_LONG: (OdbcParamValueInteger: SqlInteger);
      SQL_C_SHORT: (OdbcParamValueShort: SqlSmallint);
      SQL_C_DOUBLE: (OdbcParamValueDouble: SqlDouble);
      SQL_C_DATE: (OdbcParamValueDate: TSqlDateStruct);
      SQL_C_TIME: (OdbcParamValueTime: TSqlTimeStruct);
      SQL_C_TIMESTAMP: (OdbcParamValueTimeStamp: TOdbcTimestamp);
      SQL_C_BIT: (OdbcParamValueBit: SqlByte);
      SQL_C_SBIGINT: (OdbcParamValueBigInt: SqlBigInt);
  end;

  TOdbcBindParam = class(TObject)
  protected
    fDbxType, fDbxSubType: Word;
    fOdbcParamNumber: SqlUSmallint;
    fOdbcInputOutputType: SqlUInteger;
    fOdbcParamCType: SqlSmallint;
    fOdbcParamSqlType: SqlSmallint;
    fOdbcParamCbColDef: SqlUInteger;
    fOdbcParamIbScale: SqlSmallint;
    fOdbcParamLenOrInd: SqlInteger;
    fBuffer: Pointer;
    fValue: TOdbcBindParamRec;
    // Quick ReBind (for Refresh Query).
    fBindData: Pointer;
    fBindOutputBufferLength: Integer;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function GetOptionValue(var ConnectString: AnsiString; OptionName: AnsiString;
  HideOption: Boolean = False; TrimResult: Boolean = True; bOneChar: Boolean = False;
  const HideTemplate: AnsiString = #0): AnsiString;

{$IFNDEF _D10UP_}
type
  DBINAME32 = DBINAME;
  //DBINAME32   = packed array [0..31] of AnsiChar; { holds a name }
  DBINAME128  = packed array [0..128] of WideChar; { holds a name }
{$ENDIF}

{$IFNDEF _D11UP_}
const
  MAX_VERSION_STRING_LENGTH = 128;
{$ENDIF}

const
  dsMaxStringSize = 8192; { Maximum string field size } //  from: "db.pas"
  cDecimalSeparatorDefault: AnsiChar = '.';

  cOdbcReturnedConnectStringMax = 2048;
  cMaxBcdCharDigits = {FmtBcd.}MaxFMTBcdDigits * 2;

  // StatementPerConnection > 0:
  {begin:}
    cStatementPerConnectionBlockCount = {$IFNDEF _debug_emulate_stmt_per_con_}
                                          512;
                                        {$ELSE}
                                          2;
                                        {$ENDIF}
    // The following constants of value can appear critical if in transaction collects much of cached
    // connections and such situations take place to repeat.
    cMaxCacheConnectionCount = // Max cache "NOT SQL_NULL_HANDLE" connection handles.
                             {$IFNDEF _debug_emulate_stmt_per_con_}
                               16; // Should be more than 0.
                             {$ELSE}
                               2;
                             {$ENDIF}
    cMaxCacheNullConnectionCount = // Max cache "SQL_NULL_HANDLE" connection pointers.
                             {$IFNDEF _debug_emulate_stmt_per_con_}
                               16;  // Should be more or equally 0.
                             {$ELSE}
                               2;
                             {$ENDIF}
     {$IFDEF _debug_emulate_stmt_per_con_}
     cStmtPerConnEmulate = 2;
     {$ENDIF}
  {end.}

  cOdbcMaxColumnNameLenDefault = 128;
  cOdbcMaxTableNameLenDefault = 128;
  cOdbcMaxCatalogNameLenDefault = 1024;
  cOdbcMaxSchemaNameLenDefault = 1024;
  cOdbcMaxIdentifierLenDefault = 128;

  {$IFNDEF _D9UP_}
  DBXERR_NONE = DBXpress.SQL_SUCCESS;
  DBX_MAXSTATICERRORS = DBXpress.MaxReservedStaticErrors;
  {$ENDIF}
  DBX_SQL_NULL_DATA = 100; // == DBXpress.SQL_NULL_DATA;
  DBX_FIRSTSTATICERRORS = DBX_MAXSTATICERRORS + 1;
  SQL_SUCCESS = OdbcApi.SQL_SUCCESS;

procedure CheckMaxLines(List: TStringList);

implementation

uses
{$IFDEF MSWINDOWS}
  Windows, // The only reason this is needed is to supply Window handle for SqlDriverConect
{$ENDIF}
{$IFDEF _ASA_MESSAGE_CALLBACK_}
  DbxOpenOdbcASA,
{$ENDIF}
{$IFDEF _DBX30_}
  DbxOpenOdbc3,
{$ENDIF}
  DB,
{$IFNDEF _D11UP_}
  SqlExpr,
{$ENDIF}
{$IFNDEF _D12UP_}
  StrUtils,
{$ENDIF}
{$IFDEF _D12UP_}
  AnsiStrings,
{$ENDIF}
  DbxOpenOdbcFuncs,
  SqlTimst, DateUtils;

{ Public function getSQLDriverODBC is the starting point for everything else... }

function getSQLDriverODBC;//(sVendorLib, sResourceFile: PAnsiChar; out Obj): SQLResult; stdcall;
var
  OdbcApiProxy: TOdbcApiProxy;
begin
  Pointer(Obj) := nil;
  OdbcApiProxy := LoadOdbcDriverManager(sVendorLib, {UnicodePriority:}False);
  if OdbcApiProxy = nil then
    raise EDbxError.Create('Unable to load specified Odbc Driver manager DLL: ''' +
      sVendorLib + '''');
  try
    ISQLDriver(Obj) := TSqlDriverOdbc.Create(OdbcApiProxy, {IsUnicodeOdbcApi:}False);
    Result := DBXERR_NONE;
  except
    on e:Exception{EDbxError} do
    begin
      {$IFDEF _TRACE_CALLS_}
      LogExceptProc('getSQLDriverODBC', e);
      {$ENDIF}
      raise;
    end;
  end;
end;

function getSQLDriverODBCAW;//(sVendorLib, sResourceFile: PAnsiChar; out Obj): SQLResult; stdcall;
var
  OdbcApiProxy: TOdbcApiProxy;
begin
  Pointer(Obj) := nil;
  OdbcApiProxy := LoadOdbcDriverManager(sVendorLib, {UnicodePriority:}True);
  if OdbcApiProxy = nil then
    raise EDbxError.Create('Unable to load specified Odbc Driver manager DLL: ''' +
      sVendorLib + '''');
  ISQLDriver(Obj) := TSqlDriverOdbc.Create(OdbcApiProxy, {IsUnicodeOdbcApi:}True);
  Result := DBXERR_NONE;
end;

function IsRestrictedConnectionOptionValue(Option: TConnectionOption;
  OptionValue: TOptionSwitches;
  const OptionDriverDefault: PConnectionOptions;
  SqlConnectionOdbc: TSqlConnectionOdbc): Boolean;
begin
// Restrictions of updating for connection options depending on the current status of connection.
{begin:} //(*
   Result := True; {access is forbidden}
   if Assigned(SqlConnectionOdbc)
     and (cConnectionOptionsRestrictions[Option] <> []) then
   begin
     if (SqlConnectionOdbc.fConnected) then
     begin

       // cor_connection_off // can be changed only before connection
       if (cor_connection_off in cConnectionOptionsRestrictions[Option])
       then
         exit;

       { Are not used:
       //cor_SqlHStmtMax0,    // can be changed when not allocated any SqlHStmt
       if (cor_SqlHStmtMax0 in cConnectionOptionsRestrictions[Option])
         and (SqlConnectionOdbc.fSqlHStmtAllocated > 0)
       then
         exit;
       }

       //cor_ActiveCursors0,  // can be changed when there is no open Cursors
       if (cor_ActiveCursors0 in cConnectionOptionsRestrictions[Option])
         and (SqlConnectionOdbc.fConnected) and (SqlConnectionOdbc.fActiveCursors > 0)
       then
         exit;


       //cor_driver_off        // cannot be changed to value other from in driver option
       if (cor_driver_off in cConnectionOptionsRestrictions[Option])
         and Assigned(OptionDriverDefault)
         and (OptionDriverDefault[Option] <> osOn) // can changed only when driver option == osOff
         and (OptionDriverDefault[Option] <> OptionValue)
       then
         exit;
       // not used and probably do not make sense:

     end
     else
     begin

       { Are not used:
       //cor_connection_on   // can be changed only after connection
       if (cor_connection_on in cConnectionOptionsRestrictions[Option])
       then
         exit;
       }

     end;
   end;
{end.} //*)
   Result := False; {access is allowed}
end;

function SetConnectionOption(
  var ConnectionOptions: TConnectionOptions;
  const OptionDriverDefault: PConnectionOptions;
  Option: TConnectionOption;
  const Value: AnsiString;
  SqlConnectionOdbc: TSqlConnectionOdbc): Boolean;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  vInt: Integer;
  eConnectOption: TSQLConnectionOption;
  xeConnectOption: TXSQLConnectionOption absolute eConnectOption;
  pConnectionOptionsDefault: PConnectionOptions;
begin
  Result := False;

  if (Value = '') or IsRestrictedConnectionOptionValue(Option, ConnectionOptions[Option],
    OptionDriverDefault, SqlConnectionOdbc)
  then
    exit;

  {$IFDEF _DBX30_}
  if SqlConnectionOdbc <> nil then
    pConnectionOptionsDefault := SqlConnectionOdbc.GetDefaultConnectionOptions()
  else
  {$ENDIF}
    pConnectionOptionsDefault := @cConnectionOptionsDefault;

  case cConnectionOptionsTypes[Option] of
    cot_Bool:
      begin
        case UpCase( (Value + cNullAnsiChar)[1] ) of
          cOptCharFalse:
            begin
              ConnectionOptions[Option] := osOff;
              Result := True;
            end;
          cOptCharTrue:
            begin
              if (OptionDriverDefault<>nil) and (OptionDriverDefault^[Option] <> osOff) then
              begin
                ConnectionOptions[Option] := osOn;
                Result := True;
              end
              else
                Result := False;
            end;
          cOptCharDefault:
            begin
              if ConnectionOptions[Option] = osDefault then
              begin
                // set when call .connect()
                // Result := False; { == Default Value}
                if (OptionDriverDefault<>nil) and (OptionDriverDefault^[Option] <> osDefault) then
                  ConnectionOptions[Option] := OptionDriverDefault^[Option]
                else
                  ConnectionOptions[Option] := pConnectionOptionsDefault[Option];
              end
              else
              begin
                // set after or before call .connect()
                if Assigned(OptionDriverDefault) then
                  ConnectionOptions[Option] := OptionDriverDefault^[Option]
                else
                  ConnectionOptions[Option] := pConnectionOptionsDefault[Option];
              end;
            end;
        end;//of: case Value[1]
        if Result and (SqlConnectionOdbc <> nil) then
        begin
          case Option of
            coSafeMode:
              begin
                SqlConnectionOdbc.fSafeMode := ConnectionOptions[coSafeMode] <> osOff;
              end;
          end;//of: case Option
        end;
      end;
    cot_String:
      begin
        if SqlConnectionOdbc = nil then
          exit;
        case Option of
          coCatalogPrefix:
            begin
              SqlConnectionOdbc.fOdbcCatalogPrefix := Value;
              Result := True;
            end;
        end;
      end;
    cot_Int:
      begin
        if SqlConnectionOdbc = nil then
          exit;
        if Value <> cOptCharDefault then
        begin
          vInt := StrToIntDef(string(Value), High(Integer));
          if vInt = High(Integer) then
            Exit;
        end
        else
        begin
          case Option of
            coLockMode:
              vInt := cLockModeDefault;
            else
              Exit;
          end;
        end;
        case Option of
          coLockMode:
            begin
              if vInt < 0 then
                vInt := 0
              else
              if vInt = 0 then
                vInt := 1;
              SqlConnectionOdbc.fLockMode := vInt;
              Result := True;
            end;
        end;
      end;
    cot_UInt:
      begin
        if SqlConnectionOdbc = nil then
          exit;

        if Value <> cOptCharDefault then
        begin
          vInt := StrToIntDef(string(Value), -1);
          if vInt < 0 then
            exit;
        end
        else
        begin
          vInt := -1;
        end;

        case Option of
          coConTimeout:
            begin
              if vInt >= 0 then
                SqlConnectionOdbc.fConnectionTimeout := vInt
              else
                SqlConnectionOdbc.fConnectionTimeout := cConnectionTimeoutDefault;
              Result := True;
            end;
          coBlobChunkSize:
            begin
              if vInt < 0 then
                vInt := cBlobChunkSizeDefault
              else if vInt < cBlobChunkSizeMin then
                vInt := cBlobChunkSizeMin
              else
              if vInt > cBlobChunkSizeMax then
                vInt := cBlobChunkSizeMax;
              SqlConnectionOdbc.fBlobChunkSize := vInt;
              Result := True;
            end;
          coNetwrkPacketSize:
            begin
              if vInt< 0 then
                vInt := cNetwrkPacketSizeDefault
              else if (vInt < cNetwrkPacketSizeMin) then
                vInt := cNetwrkPacketSizeMin;
              with SqlConnectionOdbc.fOwnerDbxDriver.fOdbcApi do
              begin
                OdbcRetcode := SQLSetConnectAttr(SqlConnectionOdbc.fhCon, SQL_ATTR_PACKET_SIZE,
                  Pointer(vInt), 0);
                if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
                  // clear last error:
                  SqlConnectionOdbc.fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode,
                    SQL_HANDLE_DBC, SqlConnectionOdbc.fhCon, nil, SqlConnectionOdbc, nil, nil, 1)
                else
                begin
                  SqlConnectionOdbc.fNetwrkPacketSize := vInt;
                  Result := True;
                end;
              end;
            end;
          coNetTimeout:
            begin
              if vInt < 0 then
                vInt := coNetTimeoutDefault;
              SqlConnectionOdbc.fNetworkTimeout := vInt;
              Result := True;
            end;
        end;
      end;
    cot_Char:
      begin
        if (SqlConnectionOdbc = nil) or (Value = '') then
          exit;
        case Option of
          coNumericSeparator:
            begin
              xeConnectOption := xeConnDecimalSeparator;
              Result := SqlConnectionOdbc.SetOption(eConnectOption,
                LongInt(Byte(Value[1]))) = DBXERR_NONE;
            end;
        end;
      end;
  end;//of: case cConnectionOptionsTypes[Option]
end;

function ExtractCatalog(sOdbcCatalogName: AnsiString; const sCatalogPrefix: AnsiString): AnsiString;
var
  iPos: integer;
begin
  iPos := AnsiPos(AnsiString('='), sOdbcCatalogName);
  if iPos <= 0  then
  begin
    Result := sOdbcCatalogName;
    if Result = '?' then
      Result := '';
  end
  else
  begin
    Result := GetOptionValue(sOdbcCatalogName, sCatalogPrefix);
    if Result = cNullAnsiChar then
      Result := '';
  end;
end;

{ Private utility functions... }

function min(v1, v2: Integer): Integer;  {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  if v1 <= v2 then
    Result := v1
  else
    Result := v2;
end;

procedure Str2BCD(pIn: PAnsiChar; ASize: LongWord; var ABcd: TBcd; ADot: AnsiChar);
var
  lNeg, lDec: Boolean;
  DecimalPos: LongWord;
  pTmp: PAnsiChar;
begin
  {$IFDEF _TRACE_CALLS_}try{$ENDIF _TRACE_CALLS_}
  FillChar(ABcd.Fraction, SizeOf(ABcd.Fraction), 0);
  if (pIn^ = '0') and ((ASize = 1) or ((pIn + 1)^ = cNullAnsiChar)) or
     (pIn^ = cNullAnsiChar) then begin
    ABcd.Precision := 8;
    ABcd.SignSpecialPlaces := 2;
    Exit;
  end;

  {
  if ADot in ['0'..'9', '+', '-', 'E', 'e'] then
    ADot := cDecimalSeparatorDefault;
  }
  DecimalPos := 0;
  lDec := False;
  while DecimalPos < ASize do begin
    {+}
    //if pIn[DecimalPos] = ADot then begin
    if not (pIn[DecimalPos] in ['0'..'9', '-', '+', ' ']) then begin
    {+.}
      ADot := pIn[DecimalPos];
      lDec := True;
      Break;
    end;
    Inc(DecimalPos);
  end;
  if DecimalPos = ASize then
    DecimalPos := 0;

  { Strip leading whitespace }
  while (pIn^ <= ' ') or (pIn^ = '0') do begin
    Inc(pIn);
    Dec(ASize);
    if DecimalPos > 0 then
      Dec(DecimalPos);
  end;

  { Strip trailing whitespace }
  pTmp := pIn + ASize - 1;
  while pTmp^ <= ' ' do begin
    pTmp^ := cNullAnsiChar;
    Dec(pTmp);
  end;

  { Is the number negative? }
  if pIn^ = '-' then begin
    lNeg := True;
    if DecimalPos > 0 then
      Dec(DecimalPos);
    Inc(pIn);
    Dec(ASize);
  end
  else begin
    lNeg := False;
    if pIn^ = '+' then begin
      if DecimalPos > 0 then
        Dec(DecimalPos);
      Inc(pIn);
      Dec(ASize);
    end;
  end;

  { Clear structure }
  if pIn^ = '0' then begin
    Inc(pIn);  // '0.' scenario
    Dec(ASize);
    if DecimalPos > 0 then
      Dec(DecimalPos);
  end;

  if ASize > 66 then begin
    ABcd.Precision := 8;
    ABcd.SignSpecialPlaces := 2;
    Exit;
  end;

  asm
      // From bytes to nibbles, both left aligned
      PUSH    ESI
      PUSH    EDI
      PUSH    EBX
      MOV     ESI, pIn       // move pIn to ESI
      MOV     EDI, ABcd      // move pTo to EDI
      ADD     EDI, OFFSET TBcd.Fraction
      MOV     ECX, ASize     // store count in ECX
      MOV     DL,0           // Flag: when to store
      CLD
@@1:  LODSB                  // moves [ESI] into al
      CMP     AL, ADot
      JE      @@4
      SUB     AL, '0'
      CMP     DL, 0
      JNE     @@2
      SHL     AL, 4
      MOV     AH, AL
      JMP     @@3
@@2:  OR      AL, AH         // takes AH and ors in AL
      STOSB                  // always moves AL into [EDI]
@@3:  NOT     dl             // flip all bits
@@4:  DEC     ECX            // LOOP @@1, decrements cx and checks if it's 0
      JNE     @@1
      CMP     DL, 0          // are any bytes left unstored?
      JE      @@5
      MOV     AL, AH         // if so, move to al
      STOSB                  // and store to [EDI]
@@5:  POP     EBX
      POP     EDI
      POP     ESI
  end;

  if lDec then begin
    ABcd.Precision := Byte(ASize - 1);
    if lNeg then
      ABcd.SignSpecialPlaces := (1 shl 7) + Byte(ASize - DecimalPos - 1)
    else
      ABcd.SignSpecialPlaces := (0 shl 7) + Byte(ASize - DecimalPos - 1);
  end
  else begin
    ABcd.Precision := Byte(ASize);
    if lNeg then
      ABcd.SignSpecialPlaces := (1 shl 7)
    else
      ABcd.SignSpecialPlaces := (0 shl 7);
  end;
  {$IFDEF _TRACE_CALLS_}
  except
    on e:Exception do
    begin
      LogExceptProc('Str2BCD', e);
      raise;
    end;
  end;
  {$ENDIF}
end;

function BCD2Str(pOut: PAnsiChar; const ABcd: TBcd; ADot: AnsiChar = '.';
  bExpFormat: Boolean = False): integer; {$IFDEF _INLINE_} inline; {$ENDIF}
// returned result length
var
  iLoop, iNumDigits, iWasDigitCount, iExp: Integer;
  cVal: Byte;
  iSignSpecialPlaces : Byte;
begin
  {$IFDEF _TRACE_CALLS_}try{$ENDIF _TRACE_CALLS_}
  Result := integer(pOut);
  //Debug: PAnsiChar(Result)
  {
  if ADot in ['0'..'9', '+', '-', 'E', 'e'] then
    ADot := cDecimalSeparatorDefault;
  }
  iSignSpecialPlaces := ABcd.SignSpecialPlaces;
  if bExpFormat then
  begin
    iExp := iSignSpecialPlaces and 63;
    bExpFormat := iExp > 0;
    if bExpFormat then
      iSignSpecialPlaces := iSignSpecialPlaces and not 63;
  end
  else
    iExp := 0;

  // First, is the number negative?
  if (iSignSpecialPlaces and (1 shl 7)) <> 0 then
  begin
    pOut^ := '-';
    Inc(pOut);
  end;

  // Now, loop through the whole part of the bcd number
  // use lower 6 bits of iSignSpecialPlaces.
  iWasDigitCount := 0;
  iNumDigits := ABcd.Precision - (iSignSpecialPlaces and 63);
  for iLoop := 0 to iNumDigits - 1 do
  begin
    if (iLoop mod 2) <> 0 then
      // lower 4 bits only
      cVal := (ABcd.Fraction[(iLoop - 1 ) div 2] and 15) + Ord('0')
    else
      // upper 4 bits only
      cVal := (ABcd.Fraction[iLoop div 2] shr 4) + Ord('0');

    // This little test is used to strip leading '0' chars:

    //if not bZero then
    if (cVal <> Ord('0')) or (iWasDigitCount > 0) then
    begin
      pOut^ := AnsiChar(Chr(cVal));
      Inc(pOut);
      inc(iWasDigitCount);
    end;
  end;

  // If no data is stored yet, add a leading '0'.
  if iWasDigitCount = 0 then begin
    pOut^ := '0';
    Inc(pOut);
    iExp := 0;
  end;

  if not bExpFormat then
  begin

    pOut^ := ADot;
    Inc(pOut);

    iWasDigitCount := 0;
    for iLoop := iNumDigits to ABcd.Precision - 1 do begin
      if iLoop mod 2 <> 0 then
        // lower 4 bits only
        pOut^ := AnsiChar(Chr((ABcd.Fraction[(iLoop - 1) div  2] and 15) + Ord('0')))
      else
        // upper 4 bits only
        pOut^ := AnsiChar(Chr((ABcd.Fraction[iLoop div 2] shr 4) + Ord('0')));
      Inc(pOut);
      Inc(iWasDigitCount);
    end;

    // If trailing char is decimal point, add a '0'.

    if (pOut - 1)^ = ADot then
    begin
      Dec(pOut);
    end
    else
    begin
      // Right trim '0' chars
      while (iWasDigitCount > 0) and ((pOut - 1)^ = '0') do
      begin
        Dec(pOut);
        Dec(iWasDigitCount);
      end;
    end;

  end
  else // if bExpFormat then
  begin

    // Right trim '0' chars
    while ((pOut - 1)^ = '0') and (iWasDigitCount > 1) do
    begin
      Dec(iWasDigitCount);
      Dec(pOut);
      Dec(iExp);
    end;

    if iExp > 0 then
    begin
      // '-E'
      pOut^ := '-';
      Inc(pOut);
      pOut^ := 'E';
      Inc(pOut);

      StrCopy(pOut, PAnsiChar(AnsiString(IntToStr(iExp))));
      if iExp < 10 then
        Inc(pOut)
      else
        Inc(pOut, 2);
    end;
  end;

  pOut^ := cNullAnsiChar;

  // ASize:
  Result := Integer(pOut) - Result; { with #0 }

  {$IFDEF _TRACE_CALLS_}
  except
    on e:Exception do
    begin
      LogExceptProc('BCD2Str', e);
      raise;
    end;
  end;
  {$ENDIF}
end;

{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
(* // OLD:

{+2.08}

// QC : 6169:
//
//  BcdToStr can have results with a different separator.
//  BcdToStr uses for a separator '.', and used by it FractionToStr
//  uses global DecimalSeparator.

//resourcestring
//  SBcdOverflow = 'BCD overflow';

function FractionToStr(const pIn: PAnsiChar; count: SmallInt;
         DecPosition: ShortInt; Negative: Boolean;
         StartWithDecimal: Boolean): AnsiString;
var
  NibblesIn, BytesIn, DigitsOut: Integer;
  P, POut: PAnsiChar;
  Dot: AnsiChar;

  procedure AddOneChar(Value: AnsiChar);
  begin
    P[0] := Value;
    Inc(P);
    Inc(DigitsOut);
  end;
  procedure AddDigit(Value: AnsiChar);
  begin
    if ((DecPosition > 0) and (NibblesIn  = DecPosition)) or
       ((NibblesIn = 0) and StartWithDecimal) then
    begin
      if DigitsOut = 0 then AddOneChar('0');
      AddOneChar(Dot);
    end;
    if (Value > cNullAnsiChar) or (DigitsOut > 0) then
      AddOneChar(Char(Integer(Value)+48));
    Inc(NibblesIn);
  end;

begin
  POut := AllocMem(Count + 3);  // count + negative/decimal/zero
  try
    Dot := '.';//DecimalSeparator; //*** FIXED QC: 6169
    P := POut;
    DigitsOut := 0;
    BytesIn := 0;
    NibblesIn := 0;
    while NibblesIn < Count do
    begin
      AddDigit(Char(Integer(pIn[BytesIn]) SHR 4));
      if NibblesIn < Count then
        AddDigit(Char(Integer(pIn[BytesIn]) AND 15));
      Inc(BytesIn);
    end;
    while (DecPosition > 0) and (NibblesIn  > DecPosition) and (DigitsOut > 1) do
    begin
      if POut[DigitsOut-1] = '0' then
      begin
        Dec(DigitsOut);
        POut[DigitsOut] := cNullAnsiChar;
      end else
        break;
    end;
    if POut[DigitsOut-1] = Dot then
      Dec(DigitsOut);
    POut[DigitsOut] := cNullAnsiChar;
    SetString(Result, POut, DigitsOut);
  finally
    FreeMem(POut, Count + 2);
  end;
  if Result = '' then Result := '0'
  else if Negative then Result := '-' + Result;
end;

procedure OverflowError(const Message: AnsiString);
begin
  raise EBcdOverflowException.Create(Message);
end;

function BcdToStr(const Bcd: TBcd): AnsiString;
var
  NumDigits: Integer;
  pStart: PAnsiChar;
  DecPos: SmallInt;
  Negative: Boolean;
begin
  if (Bcd.Precision = 0) or (Bcd.Precision > MaxFMTBcdFractionSize) then
    OverFlowError(SBcdOverFlow)
  else
  begin
    Negative := Bcd.SignSpecialPlaces and (1 shl 7) <> 0;
    NumDigits := Bcd.Precision;
    pStart := PAnsiChar(@Bcd.Fraction);   // move to fractions
    // use lower 6 bits of iSignSpecialPlaces.
    if (Bcd.SignSpecialPlaces and 63) > 0 then
    begin
      DecPos := ShortInt(NumDigits - (Bcd.SignSpecialPlaces and 63));
    end else
      DecPos := NumDigits + 1;     // out of range
    Result := FractionToStr(pStart, NumDigits, DecPos, Negative,
           (NumDigits = Bcd.SignSpecialPlaces and 63));
    if Result[1] in ['0', '-'] then
      if (Result = '-0') or (Result = '0.0') or (Result = '-0.0') then Result := '0';
  end;
end;

{/+2.08}

function StrCopy(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar;  {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  if Source<>nil then
    Result := SysUtils.StrCopy(Dest, Source)
  else
    Result := Dest;
end;

// *)
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}

procedure MaxSet(var x: Integer; n: Integer); {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  if x < n then
    x := n;
end;

function StrCompNil(const Str1, Str2: PAnsiChar): Integer; {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  if (Str1 = nil) then
  begin
    if (str2 = nil) then
      Result := 0
    else
      Result := -1
  end
  else
  if (str2 = nil) then
     Result := 1
  else
    Result := strComp(str1, str2);
end;

function StrLenNil(const AStr: PAnsiChar): Integer; {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  if AStr = nil then
    Result := 0
  else
    Result := StrLen(AStr);
end;

procedure FreeMemAndNil(var MemPtr); {$IFDEF _INLINE_} inline; {$ENDIF}
var
  ptr: Pointer;
begin
  if Pointer(MemPtr) <> nil then
  begin
    ptr := Pointer(MemPtr);
    {$IFDEF _TRACE_CALLS_}
    try
    //  LogEnterProc('FreeMemAndNil(' + '$'+IntToHex(DWORD(ptr), 9) + ')' );
    //try
    {$ENDIF _TRACE_CALLS_}
      {$IFDEF _DEBUG_}
      //OutputDebugString(PAnsiChar('$'+IntToHex(DWORD(ptr), 9)));
      {$ENDIF}
      Pointer(MemPtr) := nil;
      FreeMem(ptr);
    {$IFDEF _TRACE_CALLS_}
    except
      on e:Exception do
      begin
        LogExceptProc(AnsiString('FreeMemAndNil(' + '$'+IntToHex(DWORD(ptr), 9) + ')'), e);
        raise;
      end;
    end;
    //finally LogExitProc('FreeMemAndNil'); end;
    {$ENDIF}
  end;
end;

{$IFDEF _DEBUG_}
{
procedure GetMem(var Mem; Size: Cardinal);
var
  pMem: Pointer absolute Mem;
begin
  System.GetMem(pMem, Size);
  FillChar(pMem^, Size, AnsiChar('#'));
end;
//}
{$ENDIF}

{Parse options in string:}
function GetOptionValue(var ConnectString: AnsiString; OptionName: AnsiString;
  HideOption: Boolean = False; TrimResult: Boolean = True;
  bOneChar: Boolean = False; const HideTemplate: AnsiString = #0): AnsiString;
var
  pLeft, pRight: Integer;
  sLeft, sVal: AnsiString;
  sUprConnectString: AnsiString;
  cLeft, lRight, cLeftVal, cTmp, cRight: PAnsiChar;
  bIsValue: Boolean;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('GetOptionValue',['OptionName =', OptionName, 'Options =', ConnectString]); {$ENDIF _TRACE_CALLS_}
  Result := #0; // when result is #0 then value not found, but if result='' then value is empty
  if (OptionName = '') or (Length(ConnectString) <= Length(OptionName)) then
    exit;
  OptionName := UpperCase(OptionName);
  sUprConnectString := UpperCase(ConnectString);
  // seek to option name
  cLeft := StrPos( PAnsiChar(sUprConnectString), PAnsiChar(OptionName) );
  if cLeft=nil then
    exit;
  lRight := PAnsiChar(@sUprConnectString[Length(sUprConnectString)]);// pointed to last symbol
  cTmp := #0;
  bIsValue := False;
  repeat
    begin
      // check right limitation
      cLeftVal := cLeft;
      inc(cLeftVal, Length(OptionName));// seek to last symbol in OptionName
      if DWORD(cLeftVal) > DWORD(lRight) then
        break;
      // seek to symbol '='
      while cLeftVal^<>#0 do
      begin
        if cLeftVal^='=' then
        begin
          inc(cLeftVal,2);
          bIsValue := True;
          break;
        end
        else
        if cLeftVal ^ in [#9, #10, #13, #32] then
          inc(cLeftVal)
        else
          break;
      end;
      if bIsValue then
      begin
        // search left delimiter ';' or start pos sUprConnectString:
        cTmp := cLeft-1;
        while cTmp >= PAnsiChar(sUprConnectString) do
        begin
          if cTmp^=';' then
            break
          else
          if cTmp^ in [#9, #10, #13, #32] then
            dec(cTmp)
          else
          begin
            bIsValue := False;
            break;
          end;
        end;
        if bIsValue then
          break;
      end;
      // seek to next same option name
      cLeft := StrPos( cLeftVal, PAnsiChar(OptionName) );
    end //of: repeat:
  until (cLeft=nil);
  if bIsValue then
  begin
    pLeft := DWORD(cLeft)-DWORD(PAnsiChar(sUprConnectString));
    sLeft := Copy(ConnectString, 1, pLeft - 1);
    pLeft := DWORD(cLeftVal)-DWORD(PAnsiChar(sUprConnectString));
    cRight := StrPos( cLeftVal, '=' );
    if cRight = nil then
    begin
      pRight := Length(sUprConnectString) + 1;
      cTmp := lRight;
    end
    else
    begin
      dec(cRight);
      while (cRight > cTmp) and (cRight^<>';') do
        dec(cRight);
      cTmp := cRight;
      pRight := DWORD(cRight) - DWORD(PAnsiChar(sUprConnectString)) + 1;
    end;
    sVal := Copy(ConnectString, pLeft, pRight - pLeft{ + 1});
    if HideOption then
    begin
      if HideTemplate = #0 then
      begin
        // remove options value and name
        if cTmp^=';' then
          inc(pRight);
        pLeft := Length(ConnectString) - pRight + 1;
        if (pLeft>0)and(sLeft<>'')and(sLeft[Length(sLeft)]<>';') then
          sLeft := sLeft + ';';
        ConnectString := sLeft + Copy(ConnectString, pRight, pLeft);
      end
      else // hide only value
      begin
        // replace value to template
        ConnectString :=
          Copy(ConnectString, 1, pLeft-1) +
          HideTemplate +
          Copy(ConnectString, pRight, Length(ConnectString) - pRight + 1);
      end;
    end;
    if TrimResult then
      Result := Trim(sVal)
    else
      Result := sVal;
    if bOneChar then
      Result := (Result+' ')[1];
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('GetOptionValue', e);  raise; end; end;
    finally LogExitProc('GetOptionValue', 'Result = "' + Trim(Result) + '"'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure OdbcDataTypeToDbxType(aSqlType: Smallint;
  var DbxType: Smallint; var DbxSubType: Smallint;
  SqlConnectionOdbc: TSqlConnectionOdbc; AEnableUnicode: Boolean);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('OdbcDataTypeToDbxType', ['aSqlType =',aSqlType]); {$ENDIF _TRACE_CALLS_}
  DbxType := fldUNKNOWN;
  DbxSubType := 0;
  case aSqlType of
    SQL_INTEGER:
      DbxType := fldINT32;
    SQL_BIGINT:
      begin
        DbxType := fldBCD;
        // DbExpress does NOT currently full/correctly support INT64. See db.pas: "function TParam.GetDataSize: Integer;".
        if (SqlConnectionOdbc <> nil) then
        begin
          {$IFDEF _DBX30_}
          if (SqlConnectionOdbc.fOwnerDbxDriver.fDBXVersion >= 30) and ((SqlConnectionOdbc.fConnectionOptions[coMapInt64ToBcd] = osOff)) then
          begin
            // QC: 58681: Delphi not correctly supported Int64 (TLargeintField). See db.pas: "function TParam.GetDataSize: Integer;".
            // The Field it is impossible will change since it is impossible calculate parameter data size.
            {$IFDEF _INT64_BUGS_FIXED_}
            DbxType := fldINT64;
            {$ELSE}
            DbxType := fldFLOAT;
            {$ENDIF}
          end
          else
          {$ENDIF}
          if (SqlConnectionOdbc.fConnectionOptions[coMapInt64ToBcd] = osOff) or
            (SqlConnectionOdbc.fConnectionOptions[coEnableBCD] = osOff) then
          begin
            // Default code:
            //DbxType := fldINT32;
            DbxType := fldFLOAT;
          end;
        end
        else
        begin
          // Remapping to BCD
          DbxType := fldBCD;
        end;
      end;
    SQL_SMALLINT, SQL_TINYINT:
      DbxType := fldINT16;
    SQL_BIT:
      DbxType := fldBOOL;
    SQL_NUMERIC, SQL_DECIMAL:
      begin
        //DbxType := fldBCD;
        //https://sourceforge.net/forum/forum.php?thread_id=1338393&forum_id=119358
        if (SqlConnectionOdbc<>nil)
          and (SqlConnectionOdbc.fConnectionOptions[coEnableBCD] = osOff) then
        begin
          DbxType := fldFLOAT;
        end
        else
        begin
          DbxType := fldBCD;
        end;
      end;
    SQL_DOUBLE, SQL_FLOAT, SQL_REAL:
      DbxType := fldFLOAT;
    SQL_CHAR, SQL_GUID:
      begin
        DbxType := fldZSTRING;
        DbxSubType := fldstFIXED;
      end;
    SQL_VARCHAR:
      DbxType := fldZSTRING;
    SQL_WCHAR, SQL_WVARCHAR:
      begin
        DbxType := fldZSTRING;
        if AEnableUnicode then
          DbxSubType := fldstWIDEMEMO;
        if aSqlType = SQL_WCHAR then
          DbxSubType := fldstFIXED;
      end;
    SQL_BINARY:
      DbxType := fldBYTES;
    SQL_VARBINARY:
      DbxType := fldVARBYTES;
    SQL_TYPE_DATE:
      DbxType := fldDATE;
    SQL_TYPE_TIME, SQL_TIME{=SQL_INTERVAL}: // SQL_TIME has been obtained from Pervasive.SQL
      DbxType := fldTIME;
    SQL_TYPE_TIMESTAMP, SQL_DATETIME{=SQL_DATE}, SQL_TIMESTAMP:
      DbxType := fldDATETIME;
    SQL_LONGVARCHAR, SQL_WLONGVARCHAR:
      begin
        DbxType := fldBLOB;
        DbxSubType := fldstMEMO;
      end;
    SQL_LONGVARBINARY:
      begin
        DbxType := fldBLOB;
        DbxSubType := fldstBINARY;
      end;
    SQL_INTERVAL_YEAR..SQL_INTERVAL_MINUTE_TO_SECOND:
      begin
        DbxType := fldZSTRING;
        DbxSubType := fldstFIXED;
      end;
  else
    begin
      {+2.08}
      if (SqlConnectionOdbc <> nil) then
      begin
        //-403
        if (SqlConnectionOdbc.fOdbcDriverType = eOdbcDriverTypeInformix) then
          case aSqlType of
            SQL_INFX_UDT_BLOB,
            SQL_INFX_UDT_CLOB:
              begin
                DbxType := fldBLOB;
                if (aSqlType = SQL_INFX_UDT_BLOB) then
                  DbxSubType := fldstHBINARY
                else
                  DbxSubType := fldstHMEMO;
              end;
            (*
            SQL_INFX_UDT_FIXED:
            SQL_INFX_UDT_VARYING:
            SQL_INFX_UDT_LVARCHAR:
            SQL_INFX_RC_ROWL:
            SQL_INFX_RC_COLLECTION:
            SQL_INFX_RC_LIST:
            SQL_INFX_RC_SET:
            SQL_INFX_RC_MULTISET:
            *)
          end//of: case aSqlType
        else if (SqlConnectionOdbc.fOdbcDriverType = eOdbcDriverTypeOracle) then
          case aSqlType of
            SQL_ORA_CURSOR:
              DbxType := fldCursor;
          end//of: case aSqlType
        else if (SqlConnectionOdbc.fOdbcDriverType in [eOdbcDriverTypeMsSqlServer, eOdbcDriverTypeMsSqlServer2005Up]) then
        begin
          case aSqlType of
            SQL_MSSQL_VARIANT: // sql_variant: SELECT value FROM "dbo"."sysproperties"
              if AEnableUnicode then
                DbxType := fldWIDESTRING
              else
                DbxType := fldZSTRING;
            SQL_MSSQL_XML: // xml field type
              if AEnableUnicode then
              begin
                DbxType := fldBLOB;
                DbxSubType := fldstWIDEMEMO;
              end
              else
              begin
                DbxType := fldBLOB;
                DbxSubType := fldstMEMO;
              end;
          end;
        end;
      end;
      {/+2.08}
    end;
  end; //of: case
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('OdbcDataTypeToDbxType', e); raise; end; end;
    finally LogExitProc('OdbcDataTypeToDbxType', ['DbxType =', DbxType, 'DbxSubType =', DbxSubType]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function FormatParameter(Parameter: TOdbcBindParam; Connection: TSqlConnectionOdbc): AnsiString;
var
  WS: WideString;
  vHandled: Boolean;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('FormatParameter'); {$ENDIF _TRACE_CALLS_}
  Result := AnsiString(IntToStr(Parameter.fOdbcParamNumber));
  Result := Result + AnsiString(' ');
  case Parameter.fOdbcInputOutputType of
    SQL_PARAM_TYPE_UNKNOWN:
      Result := Result + AnsiString('TYPE_UNKNOWN');
    SQL_PARAM_INPUT:
      Result := Result + AnsiString('INPUT');
    SQL_PARAM_INPUT_OUTPUT:
      Result := Result + AnsiString('INPUT_OUTPUT');
    SQL_RESULT_COL:
      Result := Result + AnsiString('RESULT_COL');
    SQL_PARAM_OUTPUT:
      Result := Result + AnsiString('OUTPUT');
    SQL_RETURN_VALUE:
      Result := Result + AnsiString('RETURN_VALUE');
    else
      Result := Result + AnsiString('TYPE_UNSUPPORTED (' + IntToStr(Parameter.fOdbcInputOutputType) + ')');
  end;

  if not (Parameter.fOdbcInputOutputType in [SQL_PARAM_INPUT, SQL_PARAM_INPUT_OUTPUT]) then
    exit;

  Result := Result + AnsiString(' ');

  vHandled := True;
  case Parameter.fOdbcParamSqlType of
    SQL_UNKNOWN_TYPE:
      begin
        Result := Result + AnsiString('[VALUE NOT ASSIGNED]');
        exit;
      end;
    SQL_CHAR:
      Result := Result + AnsiString('CHAR');
    SQL_WCHAR:
      Result := Result + AnsiString('WCHAR');
    SQL_VARCHAR:
      Result := Result + AnsiString('VARCHAR');
    SQL_WVARCHAR:
      Result := Result + AnsiString('WVARCHAR');
    SQL_GUID:
      begin
        if Assigned(Connection) and (Connection.fOdbcDriverType in [eOdbcDriverTypeMsSqlServer, eOdbcDriverTypeMsSqlServer2005Up]) then
          Result := Result + AnsiString('GUID')
        else
          vHandled := False;
      end;
    SQL_INTEGER:
      Result := Result + AnsiString('INTEGER');
    SQL_SMALLINT:
      Result := Result + AnsiString('SMALLINT');
    SQL_TINYINT:
      Result := Result + AnsiString('TINYINT');
    SQL_BIGINT:
      Result := Result + AnsiString('BIGINT');
    SQL_DOUBLE:
      Result := Result + AnsiString('DOUBLE');
    SQL_FLOAT:
      Result := Result + AnsiString('FLOAT');
    SQL_REAL:
      Result := Result + AnsiString('REAL');
    SQL_DATETIME: // == SQL_DATE
      Result := Result + AnsiString('DATETIME');
    SQL_TYPE_DATE:
      Result := Result + AnsiString('DATE');
    SQL_TIME, SQL_TYPE_TIME:
      Result := Result + AnsiString('TIME');
    SQL_TIMESTAMP, SQL_TYPE_TIMESTAMP:
      Result := Result + AnsiString('TIMESTAMP');
    SQL_DECIMAL:
      Result := Result + AnsiString(Format('DECIMAL(%d,%d)', [
        Parameter.fOdbcParamCbColDef,
          Parameter.fOdbcParamIbScale]));
    SQL_NUMERIC:
      Result := Result + AnsiString('NUMERIC');
    SQL_BIT:
      Result := Result + AnsiString('BOOLEAN');
    SQL_LONGVARBINARY:
      Result := Result + AnsiString('LONGVARBINARY');
    SQL_LONGVARCHAR:
      Result := Result + AnsiString('SQL_LONGVARCHAR');
    SQL_WLONGVARCHAR:
      Result := Result + AnsiString('SQL_WLONGVARCHAR');
    SQL_BINARY:
      Result := Result + AnsiString('BINARY');
    SQL_VARBINARY:
      Result := Result + AnsiString('VARBINARY');
    SQL_MSSQL_VARIANT:
      begin
        if Assigned(Connection) and (Connection.fOdbcDriverType in [eOdbcDriverTypeMsSqlServer, eOdbcDriverTypeMsSqlServer2005Up]) then
          Result := Result + AnsiString('VARIANT')
        else
          vHandled := False;
      end;
    SQL_MSSQL_XML:
      begin
        if Assigned(Connection) and (Connection.fOdbcDriverType in [eOdbcDriverTypeMsSqlServer, eOdbcDriverTypeMsSqlServer2005Up]) then
          Result := Result + AnsiString('XML')
        else
          vHandled := False;
      end;
  end; // of: case Parameter.fOdbcParamSqlType

  if not vHandled then
  begin
    Result := Result + AnsiString('[unknown data type ' + IntToStr(Parameter.fOdbcParamSqlType) + ']');
    exit;
  end;

  Result := Result + AnsiString(': ');

  if Parameter.fOdbcParamLenOrInd = OdbcApi.SQL_NULL_DATA then
  begin
    Result := Result + AnsiString('[NULL]');
    exit;
  end;

  case Parameter.fOdbcParamSqlType of
    SQL_CHAR, SQL_VARCHAR, SQL_LONGVARCHAR:
      if Parameter.fBuffer = nil then
        Result := Result + AnsiString('''') + Parameter.fValue.OdbcParamValueString + AnsiString('''')
      else
        Result := Result + AnsiString('[Long string]'); // debug: PAnsiChar(Parameter.fBuffer)
    SQL_WCHAR, SQL_WVARCHAR, SQL_WLONGVARCHAR:
      if Parameter.fBuffer = nil then begin
        WS := Parameter.fValue.OdbcParamValueWideString;
        Result := Result + AnsiString('''' + WS + '''');
      end
      else
        Result := Result + AnsiString('[Long unicode string]'); // debug: PWideChar(Parameter.fBuffer)
    SQL_GUID:
      begin
        if Assigned(Connection) and (Connection.fOdbcDriverType in [eOdbcDriverTypeMsSqlServer, eOdbcDriverTypeMsSqlServer2005Up]) then
        begin
          if Parameter.fBuffer = nil then
            Result := Result + AnsiString('''') + Parameter.fValue.OdbcParamValueString + AnsiString('''')
          else
            Result := Result + AnsiString('[Long string]'); // debug: PAnsiChar(Parameter.fBuffer)
        end;
      end;
    SQL_INTEGER:
      Result := Result + AnsiString(IntToStr(Parameter.fValue.OdbcParamValueInteger));
    SQL_SMALLINT, SQL_TINYINT:
      Result := Result + AnsiString(IntToStr(Parameter.fValue.OdbcParamValueShort));
    SQL_BIGINT:
      Result := Result + AnsiString(IntToStr(Parameter.fValue.OdbcParamValueBigInt));
    SQL_DOUBLE, SQL_FLOAT, SQL_REAL:
      Result := Result + AnsiString(FloatToStr(Parameter.fValue.OdbcParamValueDouble));
    SQL_TYPE_DATE: // == SQL_DATE:
      Result := Result + AnsiString(Format('%.4d-%.2d-%.2d', [
        Parameter.fValue.OdbcParamValueDate.Year,
          Parameter.fValue.OdbcParamValueDate.Month,
          Parameter.fValue.OdbcParamValueDate.Day]));
    SQL_TIME, SQL_TYPE_TIME:
      Result := Result + AnsiString(Format('%.2d:%.2d:%.2d', [
        Parameter.fValue.OdbcParamValueTime.Hour,
          Parameter.fValue.OdbcParamValueTime.Minute,
          Parameter.fValue.OdbcParamValueTime.Second]));
    SQL_TIMESTAMP, SQL_DATETIME, SQL_TYPE_TIMESTAMP:
      Result := Result + AnsiString(Format('%.4d-%.2d-%.2d %.2d:%.2d:%.2d.%.9d', [
        Parameter.fValue.OdbcParamValueTimeStamp.Year,
          Parameter.fValue.OdbcParamValueTimeStamp.Month,
          Parameter.fValue.OdbcParamValueTimeStamp.Day,
          Parameter.fValue.OdbcParamValueTimeStamp.Hour,
          Parameter.fValue.OdbcParamValueTimeStamp.Minute,
          Parameter.fValue.OdbcParamValueTimeStamp.Second,
          Parameter.fValue.OdbcParamValueTimeStamp.Fraction]));
    SQL_DECIMAL, SQL_NUMERIC:
      Result := Result + AnsiString(Parameter.fValue.OdbcParamValueString);
    SQL_BIT:
      Result := Result + AnsiString(IntToStr(Parameter.fValue.OdbcParamValueBit));
    SQL_LONGVARBINARY:
      Result := Result + AnsiString('[long data]');
    SQL_BINARY,
    SQL_VARBINARY:
      Result := Result + AnsiString('[binary data]');
    SQL_MSSQL_VARIANT:
      begin
        if Assigned(Connection) and (Connection.fOdbcDriverType in [eOdbcDriverTypeMsSqlServer, eOdbcDriverTypeMsSqlServer2005Up]) then
        begin
          Result := Result + AnsiString('[variant data]');
        end;
      end;
    SQL_MSSQL_XML:
      begin
        if Assigned(Connection) and (Connection.fOdbcDriverType in [eOdbcDriverTypeMsSqlServer, eOdbcDriverTypeMsSqlServer2005Up]) then
        begin
          Result := Result + AnsiString('[xml data]');
        end;
      end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('FormatParameter', e);  raise; end; end;
    finally LogExitProc('FormatParameter'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function FormatParameters(SqlCommandOdbc: TSqlCommandOdbc): AnsiString;
var
  i: Integer;
  Parameter: TOdbcBindParam;
begin
  Result := '';
  if (SqlCommandOdbc = nil) or (SqlCommandOdbc.fOdbcParamList = nil) or (SqlCommandOdbc.fOdbcParamList.Count = 0) then
    Exit;
  Result := AnsiString('  Parameters:');
  for i := 0 to SqlCommandOdbc.fOdbcParamList.Count - 1 do
  begin
    Parameter := TOdbcBindParam(SqlCommandOdbc.fOdbcParamList[i]);
    Result := Result + AnsiString(#13'    ') + FormatParameter(Parameter, SqlCommandOdbc.fOwnerDbxConnection);
  end;
end;

procedure CheckMaxLines(List: TStringList);
const
  MaxCount: Integer = 50;
var
  i: Integer;
begin
  if List.Count >= MaxCount then
  begin
    for i := 0 to 14 do
    begin
      List.Delete(0);
    end;
  end;
end;

function NewDbxConStmt: PDbxConStmt; {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  New(Result);
  FillChar(Result^, SizeOf(TDbxConStmt), 0);
  SetLength(Result^.fBucketDbxHStmtNodes, 0);
end;

procedure DisposeDbxConStmt(var DbxConStmt: PDbxConStmt); {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  if DbxConStmt <> nil then
  begin
    SetLength(DbxConStmt^.fBucketDbxHStmtNodes, 0);
    Dispose(DbxConStmt);
    DbxConStmt := nil;
  end;
end;

procedure AllocateDbxHStmtNodes(DbxConStmtInfo: PDbxConStmtInfo; iOffsetCount: Integer);
var
  DbxHStmtNode, DbxHStmtNodePrev: PDbxHStmtNode;
  i, iOffset: Integer;
  ArrayOfDbxHStmtNode: TArrayOfDbxHStmtNode;
begin
  if Assigned(DbxConStmtInfo) and Assigned(DbxConStmtInfo.fDbxConStmt) then
  begin
    if iOffsetCount > 0 then
    begin  // *** ADD:
      iOffset := Length(DbxConStmtInfo.fDbxConStmt.fBucketDbxHStmtNodes);
      if iOffset > 0 then
        dec(iOffset);
      // allocation or reallocation (iOffset > 0):
      SetLength(DbxConStmtInfo.fDbxConStmt.fBucketDbxHStmtNodes, iOffset + iOffsetCount);
      dec(iOffsetCount);
      DbxHStmtNodePrev := DbxConStmtInfo.fDbxConStmt.fNullDbxHStmtNodes;
      DbxConStmtInfo.fDbxConStmt.fNullDbxHStmtNodes := nil;
      for i := 0 to iOffsetCount do
      begin
        DbxHStmtNode := @DbxConStmtInfo.fDbxConStmt.fBucketDbxHStmtNodes[iOffset + i];
        DbxHStmtNode.HStmt := SQL_NULL_HANDLE;
        DbxHStmtNode.fPrevDbxHStmtNode := DbxHStmtNodePrev;
        DbxHStmtNodePrev := DbxHStmtNode;
        if i < iOffsetCount then
          DbxHStmtNode.fNextDbxHStmtNode := @DbxConStmtInfo.fDbxConStmt.fBucketDbxHStmtNodes[iOffset + 1]
        else
          DbxHStmtNode.fNextDbxHStmtNode := nil;
      end;
      DbxConStmtInfo.fDbxConStmt.fNullDbxHStmtNodes :=
        @DbxConStmtInfo.fDbxConStmt.fBucketDbxHStmtNodes[iOffset];
    end
    else   // *** REMOVE (PACK):
    begin
      // pack SqlSTMTs List (array):
      // can be disposed only "DbxConStmtInfo.fDbxConStmt.fNullDbxHStmtNodes":
      DbxHStmtNodePrev := DbxConStmtInfo.fDbxConStmt.fNullDbxHStmtNodes;
      if DbxHStmtNodePrev = nil then
        exit;
      iOffset := Length(DbxConStmtInfo.fDbxConStmt.fBucketDbxHStmtNodes);
      if iOffset <= cStatementPerConnectionBlockCount then
        exit;
      iOffsetCount := -iOffsetCount;
      if iOffsetCount > iOffset then
        iOffsetCount := iOffset;
      iOffset := iOffset - iOffsetCount; // == new array size
      if DbxConStmtInfo.fDbxConStmt.fActiveDbxHStmtNodes = nil then
      begin
        // all nodes is null nodes.
        DbxConStmtInfo.fDbxConStmt.fNullDbxHStmtNodes := nil;
        SetLength(DbxConStmtInfo.fDbxConStmt.fBucketDbxHStmtNodes, 0);
        AllocateDbxHStmtNodes(DbxConStmtInfo, iOffset);
        exit;
      end;
      i := 0;
      //remove null nodes from "fNullDbxHStmtNodes":
      DbxHStmtNode := nil;
      while i < iOffsetCount do
      begin
        DbxHStmtNode := DbxHStmtNodePrev.fNextDbxHStmtNode;
        if DbxHStmtNode = nil then
          break;
        DbxHStmtNodePrev := DbxHStmtNode;
        inc(i);
      end;
      DbxConStmtInfo.fDbxConStmt.fNullDbxHStmtNodes := DbxHStmtNode;
      if DbxHStmtNode <> nil then
        DbxHStmtNode.fPrevDbxHStmtNode := nil;
      DbxHStmtNodePrev.fNextDbxHStmtNode := nil;
      // Copying of the staying nodes into ArrayOfDbxHStmtNode.
      SetLength(ArrayOfDbxHStmtNode, iOffset);
      i := 0;
      // copying "DbxConStmtInfo.fDbxConStmt.fActiveDbxHStmtNodes":
      DbxHStmtNode := DbxConStmtInfo.fDbxConStmt.fActiveDbxHStmtNodes;
      while DbxHStmtNode <> nil do
      begin
        ArrayOfDbxHStmtNode[i] := DbxHStmtNode^;
        inc(i);
        DbxHStmtNode := DbxHStmtNode.fNextDbxHStmtNode;
      end;
      // copying "DbxConStmtInfo.fDbxConStmt.fNullDbxHStmtNodes":
      DbxHStmtNode := DbxConStmtInfo.fDbxConStmt.fNullDbxHStmtNodes;
      while DbxHStmtNode <> nil do
      begin
        ArrayOfDbxHStmtNode[i] := DbxHStmtNode^;
        inc(i);
        DbxHStmtNode := DbxHStmtNode.fNextDbxHStmtNode;
      end;
      // replace array "DbxConStmtInfo.fDbxConStmt.fBucketDbxHStmtNodes" to "ArrayOfDbxHStmtNode":
      SetLength(DbxConStmtInfo.fDbxConStmt.fBucketDbxHStmtNodes, 0);
      DbxConStmtInfo.fDbxConStmt.fBucketDbxHStmtNodes := ArrayOfDbxHStmtNode;
    end;
  end;
end;

// Is intended for minimization of efforts at formation of exceptions in methods:
//  .GetOption()
//  .GetMetaDataOption()
//
{begin:}
type
  TGetOptionExceptionInfo = (
      eiNone,
    // TSQLConnectionOption
      eiConnAutoCommit, eiConnBlockingMode, eiConnBlobSize, eiConnRoleName,
      eiConnWaitOnLocks, eiConnCommitRetain, eiConnTxnIsoLevel,
      eiConnNativeHandle, eiConnServerVersion, eiConnCallBack, eiConnHostName,
      eiConnDatabaseName, eiConnCallBackInfo, eiConnObjectMode,
      eiConnMaxActiveComm, eiConnServerCharSet, eiConnSqlDialect,
      eiConnRollbackRetain, eiConnObjectQuoteChar, eiConnConnectionName,
      eiConnOSAuthentication, eiConnSupportsTransaction, eiConnMultipleTransaction,
      eiConnServerPort, eiConnOnLine, eiConnTrimChar, eiConnQualifiedName,
      eiConnCatalogName, eiConnSchemaName, eiConnObjectName, eiConnQuotedObjectName,
      eiConnCustomInfo, eiConnTimeout,
      eiConnConnectionString, eiVendorProperty,
    // TSQLCommandOption
      eiCommRowsetSize, eiCommBlobSize, eiCommBlockRead, eiCommBlockWrite,
      eiCommParamCount, eiCommNativeHandle, eiCommCursorName, eiCommStoredProc,
      eiCommSQLDialect, eiCommTransactionID, eiCommPackageName, eiCommTrimChar,
      eiCommQualifiedName, eiCommCatalogName, eiCommSchemaName, eiCommObjectName,
      eiCommQuotedObjectName,
    // TSQLMetaDataOption
      eiMetaCatalogName, eiMetaSchemaName, eiMetaDatabaseName,
      eiMetaDatabaseVersion, eiMetaTransactionIsoLevel, eiMetaSupportsTransaction,
      eiMetaMaxObjectNameLength, eiMetaMaxColumnsInTable, eiMetaMaxColumnsInSelect,
      eiMetaMaxRowSize, eiMetaMaxSQLLength, eiMetaObjectQuoteChar,
      eiMetaSQLEscapeChar, eiMetaProcSupportsCursor, eiMetaProcSupportsCursors,
      eiMetaSupportsTransactions, eiMetaPackageName
  );
const
  cGetOptionExceptionInfos: array[TGetOptionExceptionInfo] of AnsiString = ( { Do not localize }
      '',
    // TSQLConnectionOption
      'ConnAutoCommit', 'ConnBlockingMode', 'ConnBlobSize', 'ConnRoleName',
      'ConnWaitOnLocks', 'ConnCommitRetain', 'ConnTxnIsoLevel',
      'ConnNativeHandle', 'ConnServerVersion', 'ConnCallBack', 'ConnHostName',
      'ConnDatabaseName', 'ConnCallBackInfo', 'ConnObjectMode',
      'ConnMaxActiveComm', 'ConnServerCharSet', 'ConnSqlDialect',
      'ConnRollbackRetain', 'ConnObjectQuoteChar', 'ConnConnectionName',
      'ConnOSAuthentication', 'ConnSupportsTransaction', 'ConnMultipleTransaction',
      'ConnServerPort', 'ConnOnLine', 'ConnTrimChar', 'ConnQualifiedName',
      'ConnCatalogName', 'ConnSchemaName', 'ConnObjectName', 'ConnQuotedObjectName',
      'ConnCustomInfo', 'ConnTimeout',
      'ConnConnectionString', 'ConnVendorProperty',
    // TSQLCommandOption
      'CommRowsetSize', 'CommBlobSize', 'CommBlockRead', 'CommBlockWrite',
      'CommParamCount', 'CommNativeHandle', 'CommCursorName', 'CommStoredProc',
      'CommSQLDialect', 'CommTransactionID', 'CommPackageName', 'CommTrimChar',
      'CommQualifiedName', 'CommCatalogName', 'CommSchemaName', 'CommObjectName',
      'CommQuotedObjectName',
    // TSQLMetaDataOption
      'MetaCatalogName', 'MetaSchemaName', 'MetaDatabaseName',
      'MetaDatabaseVersion', 'MetaTransactionIsoLevel', 'MetaSupportsTransaction',
      'MetaMaxObjectNameLength', 'MetaMaxColumnsInTable', 'MetaMaxColumnsInSelect',
      'MetaMaxRowSize', 'MetaMaxSQLLength', 'MetaObjectQuoteChar',
      'MetaSQLEscapeChar', 'MetaProcSupportsCursor', 'MetaProcSupportsCursors',
      'MetaSupportsTransactions', 'MetaPackageName'
  );

function GetStringOptions(
  CallerObj: TObject;
  const sValue: AnsiString;
  var OutPropValue: PAnsiChar;
  MaxLength: Smallint;
  out OutLength: Smallint;
  ExceptionInfo: TGetOptionExceptionInfo = eiNone;
  bAllowTrimResult: Boolean = False
): Boolean;

  function MakeStringExceptionFromInfo: AnsiString;
  begin
    Result := '';
    if ExceptionInfo <> eiNone then
    begin
      if CallerObj <> nil then
      begin
        if CallerObj is TSqlConnectionOdbc then
        begin
          if not ( (ExceptionInfo >= eiMetaCatalogName) and (ExceptionInfo <= eiMetaPackageName) ) then
            Result := 'TSqlConnectionOdbc.GetOption'
          else
            Result := 'TSqlConnectionOdbc.GetMetaDataOption';
        end
        else
        if CallerObj is TSQLCommandOdbc then
          Result := 'TSQLCommandOdbc.GetOption'
        else
        if CallerObj is TSQLMetaDataOdbc then
          Result := 'TSQLMetaDataOdbc.GetOption'
      end;
      if Result = '' then
      begin
        case ExceptionInfo of
         eiConnAutoCommit..eiVendorProperty:
           Result := 'TSqlConnectionOdbc.GetOption';
          eiCommRowsetSize..eiCommQuotedObjectName:
            Result := 'TSQLCommandOdbc.GetOption';
        eiMetaCatalogName..eiMetaPackageName:
          Result := 'TSQLMetaDataOdbc.GetOption';
        else
          begin
            Result := '';
            exit;
          end;
        end;//of: case GetOptionExceptionInfo
      end;

      Result :=
        Result + AnsiString('.(e') + cGetOptionExceptionInfos[ExceptionInfo] + AnsiString('). ') +
        AnsiString('Supplied MaxLength too small for value. ' +
        'MaxLength=' + IntToStr(MaxLength) +
        ', ') + cGetOptionExceptionInfos[ExceptionInfo] + AnsiString('=') + sValue;
    end;
  end;

begin
  if MaxLength < 0 then
  begin
    if ExceptionInfo <> eiNone then
      raise EDbxInvalidParam.Create( string(MakeStringExceptionFromInfo()) );
    Result := False;
    exit;
  end;
  Result := True;

  OutLength := System.Length(sValue);
  if (OutPropValue = nil) or (MaxLength = 0) then
  begin
    // get result length only
    if (OutPropValue = nil) then
    begin
      if (OutLength > 0) then
        inc(OutLength);
    end
    else
    begin
      OutLength := 0; // buffer is small
      if MaxLength > 0 then
      begin
        OutPropValue^ := cNullAnsiChar;
        if MaxLength > 1 then
          OutPropValue[1] := cNullAnsiChar;
      end;
    end;
    exit;
  end;
  // trim result:
  if (OutLength > MaxLength) and bAllowTrimResult then
    OutLength := MaxLength;
  // save result
  OutPropValue^ := cNullAnsiChar;
  if OutLength < MaxLength then
  begin
    if OutLength > 0 then
      StrCopy( OutPropValue, PAnsiChar(sValue));
  end
  else
  begin
    inc(OutLength);

    if ExceptionInfo <> eiNone then
      raise EDbxInvalidParam.Create(
        string(MakeStringExceptionFromInfo({ExceptionInfo, sValue, MaxLength)})));

    Result := False;
  end;
end;
{end.}

{ TSqlDriverOdbc }

constructor TSqlDriverOdbc.Create;//(AOdbcApi: TOdbcApiProxy; bIsUnicodeOdbcApi: Boolean);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlDriverOdbc.Create', ['bIsUnicodeOdbcApi =', bIsUnicodeOdbcApi]); {$ENDIF _TRACE_CALLS_}
  inherited Create;
  fOdbcApi := AOdbcApi;
  fIsUnicodeOdbcApi := bIsUnicodeOdbcApi;
  if fIsUnicodeOdbcApi then with fOdbcApi do
  begin
    fIsUnicodeOdbcApi := Assigned(SQLPrepareW) and Assigned(SQLExecDirectW);
  end;
  fOdbcErrorLines := TStringList.Create;
  fSqlStateChars := '00000' + cNullAnsiChar;
  fDrvBlobSizeLimitK := -1;
  AllocHEnv;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlDriverOdbc.Create', e);  raise; end; end;
    finally LogExitProc('TSqlDriverOdbc.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TSqlDriverOdbc.Destroy;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc(AnsiString(ClassName)+'.Destroy'); {$ENDIF _TRACE_CALLS_}
  FreeHEnv;
  FreeAndNil(fOdbcErrorLines);
  UnLoadOdbcDriverManager(fODBCApi);
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc(AnsiString(ClassName)+'.Destroy', e);  raise; end; end;
    finally LogExitProc(AnsiString(ClassName)+'.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlDriverOdbc.OdbcCheck;//(
//  CheckCode: SqlReturn;
//  const OdbcFunctionName: AnsiString;
//  HandleType: Smallint;
//  Handle: SqlHandle;
//  DbxConStmt: PDbxConStmt;
//  Connection: TSqlConnectionOdbc = nil;
//  Command: TSqlCommandOdbc = nil;
//  Cursor: TSqlCursorOdbc = nil;
//  maxErrorCount: Integer = 0;
//  eTraceCat: TRACECat = cTDBXTraceFlags_none);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlDriverOdbc.OdbcCheck', ['CheckCode =', CheckCode, 'OdbcFunctionName =', OdbcFunctionName, 'HandleType =', HandleType]); {$ENDIF _TRACE_CALLS_}
  case CheckCode of
    OdbcApi.SQL_SUCCESS:
      exit;
    OdbcApi.SQL_SUCCESS_WITH_INFO:
      begin
        try
          fOdbcErrorLines.Clear;
          fOdbcErrorLines.Add(string('SQL_SUCCESS_WITH_INFO returned from ODBC function ' +
            OdbcFunctionName));
          RetrieveOdbcErrorInfo(CheckCode, HandleType, Handle, DbxConStmt, Connection, Command,
            Cursor, 0, maxErrorCount, eTraceCat);
          raise EDbxODBCWarning.Create(fOdbcErrorLines.Text);
        except
          on e:EDbxOdbcWarning do
          begin
            {$IFDEF _TRACE_CALLS_}
            LogExceptProc('TSqlDriverOdbc.OdbcCheck', e);
            {$ENDIF _TRACE_CALLS_}
            fOdbcErrorLines.Clear; // Clear the error - warning only
          end;
        end
      end;
    OdbcApi.SQL_NO_DATA:
      begin
        fOdbcErrorLines.Clear;
        fOdbcErrorLines.Add(string('Unexpected end of data returned from ODBC function: ' +
          OdbcFunctionName));
        raise EDbxODBCError.Create(fOdbcErrorLines.Text);
      end;
  else
    begin
      fOdbcErrorLines.Clear;
      fOdbcErrorLines.Add(string('Error returned from ODBC function ' + OdbcFunctionName));
      RetrieveOdbcErrorInfo(CheckCode, HandleType, Handle, DbxConStmt, Connection, Command, Cursor, 0, maxErrorCount, eTraceCat);
      raise EDbxOdbcError.Create(fOdbcErrorLines.Text);
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlDriverOdbc.OdbcCheck', e);  raise; end; end;
    finally LogExitProc('TSqlDriverOdbc.OdbcCheck'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlDriverOdbc.RetrieveOdbcErrorInfo;//(
//  CheckCode: SqlReturn;
//  HandleType: Smallint;
//  Handle: SqlHandle;
//  DbxConStmt: PDbxConStmt;
//  Connection: TSqlConnectionOdbc;
//  Command: TSqlCommandOdbc;
//  Cursor: TSqlCursorOdbc = nil;
//  bClearErrorCount: Integer = 0;
//  maxErrorCount: Integer = 0;
//  eTraceCat: TRACECat = cTDBXTraceFlags_none);
  // ---
var
  CheckCodeText: AnsiString;
  GetDiagRetCode: SqlReturn;
  GetDiagRecNumber: Smallint;
  SqlStateChars: TSqlState; // 5 chars long + null terminator
  SqlState: PSqlState;
  NativeError: SqlInteger;
  pMessageText: PAnsiChar;
  BufferLengthRet: SqlSmallint;
  i, iL, iR, iD: Integer;
  fNewErrorLines: TList;
  vPString: PAnsiString;
  vDbxConnection: TSqlConnectionOdbc;
  AttrVal: SqlUInteger;
  // ---
  function GetConnection: TSqlConnectionOdbc;
  begin
    if vDbxConnection = nil then
    begin
      if Assigned(Cursor) then
        vDbxConnection := Cursor.fOwnerDbxConnection
      else
      if Assigned(Command) then
        vDbxConnection := Command.fOwnerDbxConnection
      else
      if Connection <> nil then
        vDbxConnection := Connection;
    end;
    Result := vDbxConnection;
  end;
  // ---
  function IsConnection: Boolean;
  begin
    Result := GetConnection <> nil;
  end;
  // ---
  procedure ClearNewErrors;
  var
    i: integer;
    S: AnsiString;
  begin
    if (fNewErrorLines = nil) then
      exit;
    if fNewErrorLines.Count > 0 then
    begin
      i := fNewErrorLines.Count - 1;
        S := PAnsiString(fNewErrorLines[i])^;
      for i:=0 to i - 1 do
      begin
        S := S + PAnsiString(fNewErrorLines[i])^;
        Dispose( PAnsiString(fNewErrorLines[i]) );
      end;
      fOdbcErrorLines.Add(string(S));
    end;
    fNewErrorLines.Free;
  end;
  // ---
var
  EnvironmentHandle: SqlHEnv;
  ConnectionHandle: SqlHDbc;
  StatementHandle: SqlHStmt;
  bSQLGetDiagRec2: Boolean;
  iSQLGetDiagRec2: Integer;
  // ---
  function SQLGetDiagRecLevel2(HandleType: SqlSmallint;
    Handle: SqlHandle; RecNumber: SqlSmallint; SqlState: PAnsiChar;
    var NativeError: SqlInteger; MessageText: PAnsiChar;
    BufferLength: SqlSmallint; var TextLength: SqlSmallint): SqlReturn;

    procedure LDoMakeResult(e: Exception);
    begin
      if (iSQLGetDiagRec2 = 0) and Assigned(e) then // EasySoft ODBC does not give function SQLError, SQLGetDiagRec.
      begin
        inc(iSQLGetDiagRec2);
        StrLCopy(MessageText, PAnsiChar(AnsiString(e.Message)), BufferLength);
        Result := OdbcApi.SQL_SUCCESS;
      end
      else
        Result := OdbcApi.SQL_NO_DATA;
    end;

  begin
    with fOdbcApi do
    try
      {$IFDEF DynamicOdbcImport}
      if Assigned(SQLErrorA) or Assigned(SQLErrorW) then
      {$ELSE}
      if Assigned(@SQLError) then
      {$ENDIF}
      Result := SQLError(
        EnvironmentHandle,
        ConnectionHandle,
        StatementHandle,
        SqlChar(SqlState^),
        NativeError,
        SqlChar(MessageText^),
        BufferLength,
        TextLength
      )
      else
        LDoMakeResult(nil);
    except
      on e:Exception do
      begin
        LDoMakeResult(e);
      end;
    end;
  end;
  // ---
begin
  {$IFDEF _TRACE_CALLS_} try LogEnterProc('TSqlDriverOdbc.RetrieveOdbcErrorInfo', ['CheckCode =', CheckCode, 'HandleType =', HandleType, 'Handle =', Handle]); {$ENDIF _TRACE_CALLS_}
  pMessageText := nil;
  fNewErrorLines := nil;
  vDbxConnection := nil;
  vPString := nil;
  iL:=0;
  iR := 0;
  iSQLGetDiagRec2 := 0;
  if bClearErrorCount < 0 then
    bClearErrorCount := MaxInt;
  if maxErrorCount <= 0 then
    maxErrorCount := MaxInt;

  with fOdbcApi do
  try

  {$IFDEF DynamicOdbcImport}
  if not ( Assigned(fOdbcApi.SQLGetDiagRecA) or Assigned(fOdbcApi.SQLGetDiagRecW) ) then
  {$ELSE}
  if not Assigned(@SQLGetDiagRec) then
  {$ENDIF}
  begin
    bSQLGetDiagRec2 := True;
  end
  else
    bSQLGetDiagRec2 := False;

  // prepare handles
  if (Handle = SQL_NULL_HANDLE) or bSQLGetDiagRec2 then
  begin
    EnvironmentHandle := SQL_NULL_HANDLE;
    ConnectionHandle := SQL_NULL_HANDLE;
    StatementHandle := SQL_NULL_HANDLE;
    case HandleType of
      SQL_HANDLE_ENV:
        EnvironmentHandle := Handle;
      SQL_HANDLE_DBC:
        ConnectionHandle := Handle;
      SQL_HANDLE_STMT:
        StatementHandle := Handle;
    end;
    if EnvironmentHandle = SQL_NULL_HANDLE then
    begin
      if Assigned(Connection) then
        EnvironmentHandle := Connection.fOwnerDbxDriver.fhEnv
      else
      if Assigned(Command) then
        EnvironmentHandle := Command.fOwnerDbxDriver.fhEnv
      else
      if Assigned(Cursor) then
        EnvironmentHandle := Cursor.fOwnerDbxDriver.fhEnv
    end;
    if ConnectionHandle = SQL_NULL_HANDLE then
    begin
      if DbxConStmt <> nil then
        ConnectionHandle := DbxConStmt.fHCon;
      if ConnectionHandle = SQL_NULL_HANDLE then
      begin
        if Assigned(Cursor) then
        begin
          if (Cursor.fOwnerDbxConnection.fStatementPerConnection > 0) then
            ConnectionHandle := Cursor.fOwnerCommand.fDbxConStmtInfo.fDbxConStmt.fHCon
          else
            ConnectionHandle := Cursor.fOwnerDbxConnection.fhCon;
        end
        else
        if Assigned(Command) then
        begin
          if (Command.fOwnerDbxConnection.fStatementPerConnection > 0) then
            ConnectionHandle := Command.fDbxConStmtInfo.fDbxConStmt.fHCon
          else
            ConnectionHandle := Command.fOwnerDbxConnection.fhCon;
        end
        else
        if Assigned(Connection) then
          ConnectionHandle := Connection.fhCon
      end;
    end;
    if StatementHandle = SQL_NULL_HANDLE then
    begin
      if Assigned(Cursor) then
      begin
        if (Cursor.fOwnerDbxConnection.fStatementPerConnection > 0) then
          ConnectionHandle := Cursor.fOwnerCommand.fDbxConStmtInfo.fDbxHStmtNode.HStmt
        else
          ConnectionHandle := Cursor.fOwnerCommand.fHStmt;
      end
      else
      if Assigned(Command) then
      begin
        if (Command.fOwnerDbxConnection.fStatementPerConnection > 0) then
          ConnectionHandle := Command.fDbxConStmtInfo.fDbxHStmtNode.HStmt
        else
          ConnectionHandle := Command.fHStmt
      end;
    end;

    if Handle = SQL_NULL_HANDLE then
    begin
      case HandleType of
        SQL_HANDLE_ENV:
          Handle := EnvironmentHandle;
        SQL_HANDLE_DBC:
          Handle := ConnectionHandle;
        SQL_HANDLE_STMT:
          Handle := StatementHandle;
      end;
    end;
  end;

  fNativeErrorCode := 0;
  FillChar(fSqlStateChars[0], SizeOf(fSqlStateChars)-1, AnsiChar('0'));
  fSqlStateChars[SizeOf(fSqlStateChars)-1] := cNullAnsiChar;

  case CheckCode of
    OdbcApi.SQL_SUCCESS:
      CheckCodeText := 'SQL_SUCCESS';
    SQL_SUCCESS_WITH_INFO:
      CheckCodeText := 'SQL_SUCCESS_WITH_INFO';
    SQL_NO_DATA:
      CheckCodeText := 'SQL_NO_DATA';
    SQL_ERROR:
      CheckCodeText := 'SQL_ERROR';
    SQL_INVALID_HANDLE:
      CheckCodeText := 'SQL_INVALID_HANDLE';
    SQL_STILL_EXECUTING:
      CheckCodeText := 'SQL_STILL_EXECUTING';
    SQL_NEED_DATA:
      CheckCodeText := 'SQL_NEED_DATA';
    else
      CheckCodeText := 'Unknown Error code';
  end;

  if bClearErrorCount = 0 then
    fOdbcErrorLines.Add('ODBC Return Code: ' + IntToStr(CheckCode) + ': ' + string(CheckCodeText))
  else
  if bClearErrorCount > 0 then
  begin
    fNewErrorLines := TList.Create();
    New(vPString);
    fNewErrorLines.Add(vPString);
    vPString^ := AnsiString('ODBC Return Code: ' + IntToStr(CheckCode) + ': ') + CheckCodeText;
  end;

  pMessageText := AllocMem(SQL_MAX_MESSAGE_LENGTH + 2);
  pMessageText[0] := cNullAnsiChar;
  pMessageText[1] := cNullAnsiChar;

  SqlState := @SqlStateChars;
  GetDiagRecNumber := 1;

  if not Assigned(Handle) then
    raise EDbxError.Create('dbxoodbc driver error: unknown Handle for "TSqlDriverOdbc.RetrieveOdbcErrorInfo"');

  if not bSQLGetDiagRec2 then
      GetDiagRetCode := SQLGetDiagRec(HandleType, Handle, GetDiagRecNumber,
        SqlState, NativeError, pMessageText, SQL_MAX_MESSAGE_LENGTH, BufferLengthRet)
  else
    GetDiagRetCode := SQLGetDiagRecLevel2(HandleType, Handle, GetDiagRecNumber,
      SqlState, NativeError, pMessageText, SQL_MAX_MESSAGE_LENGTH, BufferLengthRet);

  if GetDiagRetCode = OdbcApi.SQL_SUCCESS then
  begin
    fSqlStateChars := SqlStateChars;
    if Connection <> nil then
    begin
      Connection.fSqlStateChars := SqlStateChars;
      Connection.fNativeErrorCode := NativeError;
    end;
    if Assigned(Cursor) and (StrCompNil(SqlState, '24000') = 0)
      and (Cursor.fOwnerDbxConnection.fDbmsType = eDbmsTypeMsSqlServer) then
    begin
      if bClearErrorCount = 0 then
        fOdbcErrorLines.Add('Check up that value of property "TClientDataSet.PacketResords" was equaled "-1".' +
                    #13#10 +'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^')
      else
      if bClearErrorCount > 0 then
        vPString^ := vPString^ + #13#10 + 'Check up that value of property "TClientDataSet.PacketResords" was equaled "-1".' +
                                 #13#10 + '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^';
    end
    else
    if (StrCompNil(SqlState, '08S01') = 0) then
    begin
      if IsConnection then
        vDbxConnection.fConnectionClosed := True;
      if (DbxConStmt <> nil) then
        DbxConStmt.fDeadConnection := True;
    end
    else
    if (StrCompNil(SqlState, '01004') = 0) then
    begin
      // SQLGetInfo (SQL_MAX_COLUMN_NAME_LEN) returns establishes value less than it is necessary for SQLDescribeCol (...)
      if IsConnection and
        (vDbxConnection.fOdbcMaxColumnNameLen < cOdbcMaxColumnNameLenDefault)
      then
        vDbxConnection.fOdbcMaxColumnNameLen := cOdbcMaxColumnNameLenDefault
    end
    else
    if ConnectionHandle <> SQL_NULL_HANDLE then
    begin
      AttrVal := SQL_CD_FALSE; // Connection is open/available
      GetDiagRetCode := SQLGetConnectAttr(ConnectionHandle, SQL_ATTR_CONNECTION_DEAD, @AttrVal, 0, nil);
      if GetDiagRetCode = SQL_SUCCESS then
      begin
        if AttrVal = SQL_CD_TRUE then // Connection is closed/dead
        begin
          if IsConnection then
            vDbxConnection.fConnectionClosed := True;
          if (DbxConStmt <> nil) then
            DbxConStmt.fDeadConnection := True;
        end;
      end
      else
        GetDiagRetCode := SQL_SUCCESS;
    end;
  end
  else  // The most significant SqlState is always the FIRST record:
  begin
    if bClearErrorCount = 0 then
      fOdbcErrorLines.Add('No ODBC diagnostic info available')
    else
    if bClearErrorCount > 0 then
      vPString^ := vPString^ + #13#10 + 'No ODBC diagnostic info available';
  end;

  if bClearErrorCount > 0 then
    iL := fNewErrorLines.Count;
  i:=0;
  while (GetDiagRetCode = OdbcApi.SQL_SUCCESS)
  and (i < 100) // added: limitation for errors quantity on screen
  do
  begin
    inc(i);
    if i <= maxErrorCount then
    begin
      if bClearErrorCount = 0 then
      begin
        fOdbcErrorLines.Add('');
        fOdbcErrorLines.Add(string('ODBC SqlState:        ' + StrPas(SqlState)));
      end
      else
      if bClearErrorCount > 0 then
      begin
        New(vPString);
        fNewErrorLines.Add(vPString);
        vPString^ := #13#10#13#10 + 'ODBC SqlState:        ' + StrPas(SqlState);
      end;

      if (NativeError <> 0) then
      begin
        if bClearErrorCount = 0 then
          fOdbcErrorLines.Add('Native Error Code:    ' + IntToStr(NativeError))
        else
        if bClearErrorCount > 0 then
          vPString^ := vPString^ + AnsiString('Native Error Code:    ' + IntToStr(NativeError));
        if (fNativeErrorCode <> 0) then
          fNativeErrorCode := NativeError;
      end;

      if BufferLengthRet > SQL_MAX_MESSAGE_LENGTH then
        BufferLengthRet := SQL_MAX_MESSAGE_LENGTH;
      pMessageText[BufferLengthRet] := cNullAnsiChar;

      CheckCodeText := StrPas(pMessageText);
      if CheckCodeText <> '' then
      begin
        {$IFDEF _DBXCB_}
        if eTraceCat <> cTDBXTraceFlags_none then
        begin
          if Assigned(Connection) and Assigned(Connection.fDbxTraceCallbackEven) then
            Connection.DbxCallBackSendMsg(cTDBXTraceFlags_Error, 'error (' + AnsiString(GetTraceFlagName(eTraceCat)) + '): '+ CheckCodeText);
          eTraceCat := cTDBXTraceFlags_none;
        end;
        {$ENDIF}

        if bClearErrorCount = 0 then
          fOdbcErrorLines.Add(string(CheckCodeText))
        else
        if bClearErrorCount > 0 then
          vPString^ := vPString^ + #13#10 + CheckCodeText;
      end;
    end;
    Inc(GetDiagRecNumber);
    if not bSQLGetDiagRec2 then
      GetDiagRetCode := SQLGetDiagRec(HandleType, Handle, GetDiagRecNumber,
        SqlState, NativeError, pMessageText, SQL_MAX_MESSAGE_LENGTH, BufferLengthRet)
    else
      GetDiagRetCode := SQLGetDiagRecLevel2(HandleType, Handle, GetDiagRecNumber,
        SqlState, NativeError, pMessageText, SQL_MAX_MESSAGE_LENGTH, BufferLengthRet)
  end;//of: while (GetDiagRetCode = 0)

  if bClearErrorCount > 0 then
    iR := fNewErrorLines.Count;

  FreeMemAndNil(pMessageText);

  if bClearErrorCount > 0 then
  begin
    New(vPString);
    fNewErrorLines.Add(vPString);
    vPString^ := '';
  end;
  if (Connection <> nil) and (Connection.fDbmsName <> '') then
  begin
    if i > 0 then
    begin
      if bClearErrorCount = 0 then
        fOdbcErrorLines.Add('')
      else
      if bClearErrorCount > 0 then
        vPString^ := #13#10;
    end;
    if bClearErrorCount = 0 then
      fOdbcErrorLines.Add('DBMS: "' + string(Connection.fDbmsName)+
        '", version: ' + string(Connection.fDbmsVersionString) +
        ', ODBC Driver: "' + string(Connection.fOdbcDriverName) +
        '", version: ' + string(Connection.fOdbcDriverVersionString) )
    else
      vPString^ := vPString^ + AnsiString(#13#10 +
                          'DBMS: "' + string(Connection.fDbmsName)+
        '", version: ' + string(Connection.fDbmsVersionString) +
        ', ODBC Driver: "' + string(Connection.fOdbcDriverName) +
        '", version: ' + string(Connection.fOdbcDriverVersionString));
  end;

  if (Command <> nil) then
  begin
    if bClearErrorCount = 0 then
    begin
      fOdbcErrorLines.Add('');
      fOdbcErrorLines.Add('SQL:');
    end
    else
    if bClearErrorCount > 0 then
      vPString^ := vPString^ + AnsiString(#13#10'SQL:');

    if Command.fSqlPrepared <> '' then
    begin
      if bClearErrorCount = 0 then
        fOdbcErrorLines.Add(string(Command.fSqlPrepared));
      if bClearErrorCount > 0 then
        vPString^ := vPString^ + AnsiString(#13#10) + Command.fSqlPrepared;
    end
    else
    begin
      if bClearErrorCount = 0 then
        fOdbcErrorLines.Add(string(Command.fSql))
      else
      if bClearErrorCount > 0 then
        vPString^ := vPString^ + AnsiString(#13#10) + Command.fSql;
    end;
    if (fOdbcErrorLines.Count > 0) and (Command.fOdbcParamList <> nil) and (Command.fOdbcParamList.Count > 0) then
    begin
      if bClearErrorCount = 0 then
      begin
        fOdbcErrorLines.Add('');
        fOdbcErrorLines.Add('Parameters:');
      end
      else
      if bClearErrorCount > 0 then
        vPString^ := vPString^ + AnsiString(#13#10#13#10'Parameters:');
      for i := 0 to Command.fOdbcParamList.Count - 1 do
      begin
        if bClearErrorCount = 0 then
          fOdbcErrorLines.Add(string(FormatParameter(Command.fOdbcParamList[i], GetConnection() )))
        else
        if bClearErrorCount > 0 then
          vPString^ := vPString^ + AnsiString(#13#10) + FormatParameter(Command.fOdbcParamList[i], GetConnection() );
      end;
    end;
    if bClearErrorCount = 0 then
    begin
      fOdbcErrorLines.Add('');
      fOdbcErrorLines.Add('Connection string:');
      fOdbcErrorLines.Add(string(Connection.fOdbcConnectStringHidePassword));
    end
    else
    begin
      New(vPString);
      fNewErrorLines.Add(vPString);
      vPString^ := AnsiString(#13#10#13#10'Connection string:'#13#10) + Connection.fOdbcConnectStringHidePassword;
    end;
  end;

  if bClearErrorCount > 0 then
  begin
    iD := fNewErrorLines.Count - (iR - iL);
    if bClearErrorCount < iR - iL then
      iR := iL + bClearErrorCount + 1;
    for i:= iL to iR - 1 do
    begin
      Dispose( PString(fNewErrorLines[iL]) ); //recomended debug breakpoint: PString(fNewErrorLines[iL])^
      fNewErrorLines.Delete(iL);
    end;
    if fNewErrorLines.Count = iD then
      while fNewErrorLines.Count > 0 do
      begin
        Dispose( PString(fNewErrorLines[0]) );
        fNewErrorLines.Delete(0);
      end;
    ClearNewErrors;
  end;

  except on e:exception do
    begin
      if bClearErrorCount > 0 then
        ClearNewErrors;
      fOdbcErrorLines.Add('Error RetrieveOdbcErrorInfo: ' + e.Message);
      FreeMem(pMessageText);
      fNewErrorLines.Free;
      {$IFDEF _TRACE_CALLS_}
      LogExceptProc('TSqlDriverOdbc.RetrieveOdbcErrorInfo', e);
      {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    //except on e:exception do begin LogExceptProc('TSqlDriverOdbc.RetrieveOdbcErrorInfo', e);  raise; end; end;
    finally LogExitProc('TSqlDriverOdbc.RetrieveOdbcErrorInfo'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlDriverOdbc.AllocHCon;//(out HCon: SqlHDbc);
var
  OdbcRetcode: OdbcApi.SqlReturn;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlDriverOdbc.AllocHCon'); {$ENDIF _TRACE_CALLS_}
  with fOdbcApi do
  begin

  OdbcRetcode := SQLAllocHandle(SQL_HANDLE_DBC, fhEnv, HCon);
  if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    OdbcCheck(OdbcRetcode, 'SQLAllocHandle(SQL_HANDLE_DBC)', SQL_HANDLE_ENV, fhEnv, nil);

  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlDriverOdbc.AllocHCon', e);  raise; end; end;
    finally LogExitProc('TSqlDriverOdbc.AllocHCon'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlDriverOdbc.FreeHCon;//(var HCon: SqlHDbc;
//  DbxConStmt: PDbxConStmt; bIgnoreError: Boolean = False);
var
  OdbcRetcode: OdbcApi.SqlReturn;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlDriverOdbc.FreeHCon'); {$ENDIF _TRACE_CALLS_}
  if HCon = SQL_NULL_HANDLE then
    exit;
  with fOdbcApi do
  begin

  OdbcRetcode := SQLFreeHandle(SQL_HANDLE_DBC, HCon);
  if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
  begin
    if  bIgnoreError then
      fIgnoreErrors := bIgnoreError
    else
      OdbcCheck(OdbcRetcode, 'SQLFreeHandle(SQL_HANDLE_DBC)', SQL_HANDLE_DBC, HCon, DbxConStmt);
  end;
  HCon := SQL_NULL_HANDLE;

  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlDriverOdbc.FreeHCon', e);  raise; end; end;
    finally LogExitProc('TSqlDriverOdbc.FreeHCon'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlDriverOdbc.AllocHEnv;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  iOdbcVersion: ULong;
  sOdbcVersion: AnsiString;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlDriverOdbc.AllocHEnv'); {$ENDIF _TRACE_CALLS_}
  with fOdbcApi do
  begin

  OdbcRetcode := SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, fhEnv);
  if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    OdbcCheck(OdbcRetcode, 'SQLAllocHandle(SQL_HANDLE_ENV)', SQL_HANDLE_ENV, fhEnv, nil);

  fIgnoreErrors := False;

  if not
    Assigned({$IFDEF DynamicOdbcImport}fOdbcApi.SQLSetEnvAttrA{$ELSE}@SQLSetEnvAttr{$ENDIF})
  then
    exit;

  // This specifies ODBC version 3 (called before SQLConnect)
  if (OdbcDriverLevel > 0) and (OdbcDriverLevel <3) then
  begin
    iOdbcVersion := SQL_OV_ODBC2;
    sOdbcVersion := 'SQL_OV_ODBC2';
  end
  else
  begin
    iOdbcVersion := SQL_OV_ODBC3;
    sOdbcVersion := 'SQL_OV_ODBC3';
  end;

  OdbcRetcode := SQLSetEnvAttr(fhEnv, SQL_ATTR_ODBC_VERSION, Pointer(iOdbcVersion), 0);
  if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
    OdbcCheck(OdbcRetcode, 'SQLSetEnvAttr(SQL_ATTR_ODBC_VERSION, '+sOdbcVersion+')', SQL_HANDLE_ENV,
      fhEnv, nil);

  end;//of: with fOdbcApi

  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlDriverOdbc.AllocHEnv', e);  raise; end; end;
    finally LogExitProc('TSqlDriverOdbc.AllocHEnv'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlDriverOdbc.FreeHEnv;
var
  OdbcRetcode: OdbcApi.SqlReturn;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlDriverOdbc.FreeHEnv'); {$ENDIF _TRACE_CALLS_}
  with fOdbcApi do
  begin

  OdbcRetcode := SQLFreeHandle(SQL_HANDLE_ENV, fhEnv);
  if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
  try
    OdbcCheck(OdbcRetcode, 'SQLFreeHandle(SQL_HANDLE_ENV)', SQL_HANDLE_ENV, fhEnv, nil);
  except
    //if not fIgnoreErrors then
    //  raise;
  end;
  fhEnv := SQL_NULL_HANDLE;

  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlDriverOdbc.FreeHEnv', e);  raise; end; end;
    finally LogExitProc('TSqlDriverOdbc.FreeHEnv'); end;
  {$ENDIF _TRACE_CALLS_}
end;

{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
{
type
  TISQLConnectionRef = class of TISQLConnection;
  TDBXDrvVersion = record
    DrvVersion: AnsiString;
    ProdVersion: AnsiString;
    SQLConnection: TISQLConnectionRef;
  end;

const
  ProductVersionStr = '3.0';
  MaxDBXDrvTableEntry = 1;
  DBXDrvMap : array[1..MaxDBXDrvTableEntry] of TDBXDrvVersion = (
  (
   DrvVersion: DBXDRIVERVERSION30;
   ProdVersion: DBXPRODUCTVERSION30;
   SQLConnection:TISQLConnection30
  ));
{}
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}

{$IFNDEF _D10UP_}
Type
  // debug: TISQLConnectionRef = class of TISQLConnection;
  TDBXDrvVersion = record
    DrvVersion: AnsiString;
    ProdVersion: AnsiString;
    SQLConnection: pointer;// TISQLConnectionRef;
  end;
{$ENDIF}

{$IFDEF _DBX30_}
const
  cDbxDriverVersionString: AnsiString  = '3.0';
  cDbxProductVersionString: AnsiString = '3.0';
{$ENDIF}

function TSqlDriverOdbc.GetOption;//(eDOption: TSQLDriverOption;
//  PropValue: Pointer; MaxLength: Smallint; out Length: Smallint): SQLResult;
var
  xeDOption: TXSQLDriverOption absolute eDOption;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlDriverOdbc.GetOption', ['eDOption =', cSQLDriverOption[xeDOption]]); {$ENDIF _TRACE_CALLS_}
  Length := 0;
  if PropValue = nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  try
    case xeDOption of
      xeDrvBlobSize:
        if MaxLength >= SizeOf(Longint) then
          Longint(PropValue) := fDrvBlobSizeLimitK
        else
          Result := DBXERR_INVALIDPARAM;
      xeDrvCallBack: // not use in delphi
        if MaxLength >= SizeOf(TSQLCallbackEvent) then
          TSQLCallbackEvent(PropValue) := nil //fDbxTraceCallbackEven
        else
          Result := DBXERR_INVALIDPARAM;
      xeDrvCallBackInfo: // not use in delphi
        if MaxLength >= SizeOf(Longint) then
          Longint(PropValue) := 0 // fDbxTraceClientData
        else
          Result := DBXERR_INVALIDPARAM;
      xeDrvRestrict:
        if MaxLength >= SizeOf(Longword) then
          Longword(PropValue) := fDbxDrvRestrict
        else
          Result := DBXERR_INVALIDPARAM;
      // Delphi 2006
      xeDrvVersion:
        begin
          if fClientVersion < 30 then
            fClientVersion := 30;
          {$IFDEF _DBX30_}
          if fDBXVersion >= 30 then
          begin
            StrLCopy(PAnsiChar(PropValue), PAnsiChar(cDbxDriverVersionString), MaxLength);
            Length := System.Length(cDbxDriverVersionString);
          end;
          {$ENDIF}
        end;
      xeDrvProductVersion:
        begin
          {$IFDEF _DBX30_}
          if fDBXVersion >= 30 then
          begin
            StrLCopy(PAnsiChar(PropValue), PAnsiChar(cDbxProductVersionString), MaxLength);
            Length := System.Length(cDbxProductVersionString);
          end;
          {$ENDIF}
        end;
    //else
    //  raise EDbxInvalidCall.Create('Invalid option passed to TSqlDriverOdbc.GetOption');
    end;
  except
    on E: Exception do
    begin
      Result := DBX_FIRSTSTATICERRORS;
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlDriverOdbc.GetOption', e);  raise; end; end;
    finally LogExitProc('TSqlDriverOdbc.GetOption', ['Result =', Result, 'Length =', Length]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlDriverOdbc.getSQLConnection;//(out pConn: ISQLConnection): SQLResult;
var
  pConn25: ISQLConnection25 absolute pConn;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlDriverOdbc.getSQLConnection'); {$ENDIF _TRACE_CALLS_}
  try
    Pointer(pConn25) := nil;
    pConn25 := TSqlConnectionOdbc.Create(Self);
  except
    on E: Exception do
    begin
      Result := DBX_FIRSTSTATICERRORS;
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlDriverOdbc.getSQLConnection', e);  raise; end; end;
    finally LogExitProc('TSqlDriverOdbc.getSQLConnection', ['Result =', Result, 'pConn =', Pointer(pConn)]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlDriverOdbc.SetOption;//(eDOption: TSQLDriverOption;
//  PropValue: Longint): SQLResult;
var
  xeDOption: TXSQLDriverOption absolute eDOption;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlDriverOdbc.SetOption', ['eDOption =',cSQLDriverOption[xeDOption], 'PropValue =', Pointer(PropValue)]); {$ENDIF _TRACE_CALLS_}
  try
    case xeDOption of
      xeDrvBlobSize:
        fDrvBlobSizeLimitK := PropValue;
      xeDrvCallBack: // not use in delphi
        ; // fDbxTraceCallbackEven := TSQLCallbackEvent(PropValue);
      xeDrvCallBackInfo: // not use in delphi
        ; //fDbxTraceClientData := PropValue;
      xeDrvRestrict:
        fDbxDrvRestrict := Longword(PropValue);
      // Delphi 2006
      xeDrvVersion:
        begin
          if fClientVersion < 30 then
            fClientVersion := 30;
          {$IFDEF _TRACE_CALLS_}
            LogInfoProc(['eDrvVersion =', PAnsiChar(PropValue)]);
          {$ENDIF _TRACE_CALLS_}
          {$IFDEF _DBX30_}
          if (fDBXVersion < 30) or (PropValue = 0)
            or ( PAnsiChar(PropValue) <> cDbxDriverVersionString)
          then
            Result := DBXERR_DRIVERINCOMPATIBLE;
          {$ENDIF}
        end;
      xeDrvProductVersion:
        begin
          {$IFDEF _TRACE_CALLS_}
            LogInfoProc(['eDrvProductVersion =', PAnsiChar(PropValue)]);
          {$ENDIF _TRACE_CALLS_}
          {$IFDEF _DBX30_}
          if (fDBXVersion < 30) or (PropValue = 0)
            or ( PAnsiChar(PropValue) <> cDbxProductVersionString)
          then
            Result := DBXERR_DRIVERINCOMPATIBLE
          else if fClientVersion < 30 then
            fClientVersion := 30;
          {$ENDIF}
        end;
    //else
    //  raise EDbxInvalidCall.Create('Invalid option passed to TSqlDriverOdbc.SetOption');
    end;
  except
    on E: Exception do
    begin
      Result := DBX_FIRSTSTATICERRORS;
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlDriverOdbc.SetOption', e);  raise; end; end;
    finally LogExitProc('TSqlDriverOdbc.SetOption', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlDriverOdbc.Drivers;//(DriverList: TStrings);
const
  DriverDescLengthMax = 255;
  DriverAttributesLengthMax = 4000;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  sDriverDescBuffer: PAnsiChar;
  sDriverAttributesBuffer: PAnsiChar;
  aDriverDescLength: SqlSmallint;
  aDriverAttributesLength: SqlSmallint;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlDriverOdbc.Drivers'); {$ENDIF _TRACE_CALLS_}
  with fOdbcApi do
  begin

  sDriverDescBuffer := nil;
  sDriverAttributesBuffer := nil;
  DriverList.BeginUpdate;
  try
    GetMem(sDriverDescBuffer, DriverDescLengthMax);
    GetMem(sDriverAttributesBuffer, DriverAttributesLengthMax);
    DriverList.Clear;
    sDriverDescBuffer[0] := cNullAnsiChar;
    sDriverAttributesBuffer[0] := cNullAnsiChar;
    OdbcRetcode := SQLDrivers(fhEnv, SQL_FETCH_FIRST,
      sDriverDescBuffer, DriverDescLengthMax, aDriverDescLength,
      sDriverAttributesBuffer, DriverAttributesLengthMax, aDriverAttributesLength);
    if (OdbcRetcode <> OdbcApi.SQL_NO_DATA) and (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
      OdbcCheck(OdbcRetcode, 'SQLDrivers(SQL_FETCH_FIRST)', SQL_HANDLE_ENV, fhEnv, nil);
    while OdbcRetcode = 0 do
    begin
      DriverList.Add(string(StrPas(sDriverDescBuffer)));
      sDriverDescBuffer[0] := cNullAnsiChar;
      sDriverAttributesBuffer[0] := cNullAnsiChar;
      OdbcRetcode := SQLDrivers(fhEnv, SQL_FETCH_NEXT,
        sDriverDescBuffer, DriverDescLengthMax, aDriverDescLength,
        sDriverAttributesBuffer, DriverAttributesLengthMax, aDriverAttributesLength);
      if (OdbcRetcode <> OdbcApi.SQL_NO_DATA) and (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
        OdbcCheck(OdbcRetcode, 'SQLDrivers(SQL_FETCH_NEXT)', SQL_HANDLE_ENV, fhEnv, nil);
    end;
  finally
    DriverList.EndUpdate;
    FreeMem(sDriverAttributesBuffer);
    FreeMem(sDriverDescBuffer);
  end;

  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlDriverOdbc.Drivers', e);  raise; end; end;
    finally LogExitProc('TSqlDriverOdbc.Drivers'); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSqlConnectionOdbc }

constructor TSqlConnectionOdbc.Create;//(OwnerDbxDriver: TSqlDriverOdbc);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc.Create'); {$ENDIF _TRACE_CALLS_}
  inherited Create;
  fhCon := SQL_NULL_HANDLE;
  fNativeErrorCode := 0;
  fSqlStateChars := '00000' + cNullAnsiChar;
  fConnected := False;
  fSafeMode := True;
  fConnectionErrorLines := TStringList.Create;
  fOwnerDbxDriver := OwnerDbxDriver;
  fOwnerDbxDriver.AllocHCon(fhCon);
  // set default connection fields:
  ClearConnectionOptions;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.Create', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TSqlConnectionOdbc.Destroy;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc(AnsiString(ClassName) + '.Destroy'); {$ENDIF _TRACE_CALLS_}
  disconnect;
  if (fhCon <> SQL_NULL_HANDLE) then
    fOwnerDbxDriver.FreeHCon(fhCon, nil, fSafeMode or fConnectionClosed);
{$IFDEF _RegExprParser_}
  FreeAndNil(fObjectNameParser);
{$ENDIF}
  FreeAndNil(fConnectionErrorLines);
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc(AnsiString(ClassName) + '.Destroy', e);  raise; end; end;
    finally LogExitProc(AnsiString(ClassName) + '.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlConnectionOdbc.SynchronizeInTransaction;//(var DbxConStmt: TDbxConStmt);
var
  OdbcRetcode: OdbcApi.SqlReturn;
  AttrValMain, AttrVal: SqlUInteger;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc.SynchronizeTransaction'); {$ENDIF _TRACE_CALLS_}
  with fOwnerDbxDriver.fOdbcApi do
  begin

    if (DbxConStmt.fInTransaction <= 0) or (DbxConStmt.fDeadConnection) then
      exit;

    if fSupportsTransaction then
    begin
        // Read Main Connection Transaction Isolation Level
      AttrValMain := fOdbcIsolationLevel;
        // Read New Connection Transaction Isolation Level
      OdbcRetCode := SQLGetConnectAttr(DbxConStmt.fHCon, SQL_ATTR_TXN_ISOLATION, @AttrVal, 0, nil);
      if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
        fOwnerDbxDriver.OdbcCheck(OdbcRetCode,
          'SynchronizeTransaction - SQLGetConnectAttr(SQL_ATTR_TXN_ISOLATION)',
          SQL_HANDLE_DBC, DbxConStmt.fHCon, @DbxConStmt);
        // Synchronize Transaction Isolation Level:
      if AttrVal <> AttrValMain then
      begin
        OdbcRetCode := SQLSetConnectAttr(DbxConStmt.fHCon, SQL_ATTR_TXN_ISOLATION,
          SqlPointer(AttrValMain), 0);
        if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
          fOwnerDbxDriver.OdbcCheck(OdbcRetCode,
            'SynchronizeTransaction - SQLSetConnectAttr(SQL_ATTR_TXN_ISOLATION)',
            SQL_HANDLE_DBC, DbxConStmt.fHCon, @DbxConStmt);
      end;
      // Synchronize fAutoCommitMode:
      OdbcRetCode := SQLGetConnectAttr(DbxConStmt.fHCon, SQL_ATTR_AUTOCOMMIT, @AttrVal, 0, nil);
      if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
        fOwnerDbxDriver.OdbcCheck(OdbcRetCode,
          'SynchronizeTransaction - SQLGetConnectAttr(SQL_ATTR_AUTOCOMMIT)',
          SQL_HANDLE_DBC, DbxConStmt.fHCon, @DbxConStmt);
      if AttrVal <> fAutoCommitMode then
      begin
        OdbcRetCode := SQLSetConnectAttr(DbxConStmt.fHCon, SQL_ATTR_AUTOCOMMIT,
          SqlPointer(fAutoCommitMode), 0);
        if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
          fOwnerDbxDriver.OdbcCheck(OdbcRetCode,
            'SynchronizeTransaction - SQLSetConnectAttr(SQL_ATTR_AUTOCOMMIT)',
            SQL_HANDLE_DBC, DbxConStmt.fHCon, @DbxConStmt);
      end;
    end;
    inc(DbxConStmt.fInTransaction);
    DbxConStmt.fAutoCommitMode := fAutoCommitMode;
    DbxConStmt.fOutOfDateCon := False;

  end;//of: with fOdbcApi
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.SynchronizeTransaction', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.SynchronizeTransaction'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlConnectionOdbc.CloneOdbcConnection;//(out DbxConStmtInfo: TDbxConStmtInfo;
//  bSynchronizeTransaction: Boolean = True);
var
  OdbcRetcode: OdbcApi.SqlReturn;
  cbConnStrOut: SqlSmallint;
  aTempOdbcReturnedConnectString: AnsiString;
  OLDDbxConStmt: PDbxConStmt;
  aOLDHCon: SqlHDbc;
  bNewDbxConStmt: Boolean;
  iAddCount: Integer;
  bIsConnected: Boolean;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc.CloneOdbcConnection'); {$ENDIF _TRACE_CALLS_}
  {$IFDEF _DBXCB_}
  if Assigned(fDbxTraceCallbackEven) then
    DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'connect (clone): "%s"', [fOdbcConnectStringHidePassword]);
  {$ENDIF}
  with fOwnerDbxDriver.fOdbcApi do
  begin

  OLDDbxConStmt := GetCurrentDbxConStmt();
  if fCurrDbxConStmt = nil then
    OLDDbxConStmt := nil;

  if OLDDbxConStmt = nil then
    aOLDHCon := Self.fhCon
  else
    aOLDHCon := OLDDbxConStmt.fHCon;

  bIsConnected := False;
  bNewDbxConStmt := DbxConStmtInfo.fDbxConStmt = nil;
  try

  if bNewDbxConStmt then
  begin
    DbxConStmtInfo.fDbxConStmt := NewDbxConStmt();
    bNewDbxConStmt := True;
    fDbxConStmtList.Add(DbxConStmtInfo.fDbxConStmt);
    if fStatementPerConnection <= cStatementPerConnectionBlockCount then
      iAddCount := fStatementPerConnection
    else
      iAddCount := cStatementPerConnectionBlockCount;
    AllocateDbxHStmtNodes(@DbxConStmtInfo, iAddCount{0});
  end;
  DbxConStmtInfo.fDbxConStmt.fHCon := SQL_NULL_HANDLE;
  with DbxConStmtInfo do
  begin
    fOwnerDbxDriver.AllocHCon(fDbxConStmt.fHCon);
    //fCurrDbxConStmt := DbxConStmtInfo.fDbxConStmt;
    SetLength(aTempOdbcReturnedConnectString, cOdbcReturnedConnectStringMax);
    aTempOdbcReturnedConnectString[1] := cNullAnsiChar;
    // Synchronize SQL_ATTR_LOGIN_TIMEOUT:
    if (fConnectionTimeout >= 0) {and (fConnectionTimeout <> cConnectionTimeoutDefault)} then
      SetLoginTimeout(fDbxConStmt.fhCon, fConnectionTimeout);
    // Synchronize SQL_ATTR_CONNECTION_TIMEOUT:
    if fNetworkTimeout >= 0 then
      SetNetworkTimeout(fDbxConStmt.fhCon, fNetworkTimeout);
    // Synchronize ReadOnly:
    if fConnectionOptions[coReadOnly] = osOn then
    begin
      OdbcRetcode := SQLSetConnectAttr(fDbxConStmt.fHCon, SQL_ATTR_ACCESS_MODE,
        SqlPointer(SQL_MODE_READ_ONLY), 0);
      // clear last error:
      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fDbxConStmt.fHCon,
          @fDbxConStmt, Self, nil, nil, 1);
    end;
    OdbcRetcode := SQLDriverConnect(
      fDbxConStmt.fHCon, 0,
      PAnsiChar(fOdbcReturnedConnectString), SQL_NTS,
      PAnsiChar(aTempOdbcReturnedConnectString), cOdbcReturnedConnectStringMax, cbConnStrOut,
      SQL_DRIVER_NOPROMPT);
    if (OdbcRetcode <> OdbcApi.SQL_SUCCESS)and(OdbcRetcode <> OdbcApi.SQL_SUCCESS_WITH_INFO) then
      fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'CloneOdbcConnection - SQLDriverConnect (NoPrompt)',
        SQL_HANDLE_DBC, fDbxConStmt.fHCon, @fDbxConStmt);

     bIsConnected := True;

    //Synchronize ConPacketSize:
    //if (fNetwrkPacketSize > cNetwrkPacketSizeDefault) then
    OdbcRetcode := SQLSetConnectAttr(DbxConStmtInfo.fDbxConStmt.fHCon, SQL_ATTR_PACKET_SIZE,
        SqlPointer(fNetwrkPacketSize), 0);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      // clear last error:
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode,
        SQL_HANDLE_DBC, DbxConStmtInfo.fDbxConStmt.fhCon, @DbxConStmtInfo.fDbxConStmt, Self, nil, nil, 1);
    // Synchronize Current Catalog:
    if fSupportsCatalog then
    begin
      GetCurrentCatalog(aOLDHCon);
      // catalog name <> current catalog
      if fSupportsCatalog and (fCurrentCatalog <> '') then
      begin
        OdbcRetcode := SQLSetConnectAttr(fDbxConStmt.fHCon, SQL_ATTR_CURRENT_CATALOG,
          PAnsiChar(fCurrentCatalog), SQL_NTS);
        if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLSetConnectAttr(SQL_ATTR_CURRENT_CATALOG)',
          SQL_HANDLE_DBC, fDbxConStmt.fHCon, @DbxConStmtInfo.fDbxConStmt);
      end;
    end;
    // Synchronize Transaction Isolation Level: (added 2.06 - Vadim Lopushansky)
    if bSynchronizeTransaction then
      SynchronizeInTransaction(DbxConStmtInfo.fDbxConStmt^);
    inc(fDbxConStmtActive);
    inc(Self.fCon0SqlHStmt);
  end;//of: with DbxConStmtInfo
  except
    with DbxConStmtInfo do
    if fDbxConStmt.fHCon <> SQL_NULL_HANDLE then
    begin
      if bIsConnected then
        SQLDisconnect(fDbxConStmt.fHCon);
      fOwnerDbxDriver.FreeHCon(fDbxConStmt.fHCon, @fDbxConStmt, True);
      fDbxConStmt.fHCon := SQL_NULL_HANDLE;
      fDbxConStmt.fDeadConnection := False;
      if bNewDbxConStmt then
      begin
        fDbxConStmtList.Remove(fDbxConStmt);
        DisposeDbxConStmt(fDbxConStmt);
        DbxConStmtInfo.fDbxConStmt := nil;
      end;
      DbxConStmtInfo.fDbxHStmtNode := nil;
    end;
    fCurrDbxConStmt := OLDDbxConStmt;
    raise;
  end;

  end;//of: with fOdbcApi
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.CloneOdbcConnection', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.CloneOdbcConnection'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlConnectionOdbc.SetCurrentDbxConStmt;//(aDbxConStmt: PDbxConStmt);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc.SetCurrentDbxConStmt', ['DbxConStmt =', aDbxConStmt]); {$ENDIF _TRACE_CALLS_}

  if (fDbxConStmtList <> nil) and (
    (aDbxConStmt = nil) or ( fDbxConStmtList.IndexOf(aDbxConStmt) >= 0 ) )
  then
    fCurrDbxConStmt := aDbxConStmt;

  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.SetCurrentDbxConStmt', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.SetCurrentDbxConStmt'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc.SetLoginTimeout;//(hCon: SqlHDbc; TimeoutSeconds: Integer): Boolean;
{ need call only before connect }
var
  Value, StmtValue: SQLUINTEGER;
  OdbcRetcode: OdbcApi.SqlReturn;
begin
  {$IFDEF _TRACE_CALLS_} Result := False; try try LogEnterProc('TSqlConnectionOdbc.SetLoginTimeout', ['TimeoutSeconds =', TimeoutSeconds]); {$ENDIF _TRACE_CALLS_}

  if TimeoutSeconds < 0 then
  begin
    Result := True;
    Exit;
  end;

  with fOwnerDbxDriver.fOdbcApi do
  begin

  Result := False;
  Value := TimeoutSeconds;
  StmtValue := Value;

  OdbcRetCode := SQLGetConnectAttr(hCon, SQL_ATTR_LOGIN_TIMEOUT, @StmtValue, 0, nil);
  if (OdbcRetcode = OdbcApi.SQL_SUCCESS) then
  begin
    if (StmtValue <> Value) then
      OdbcRetcode := SQLSetConnectAttr(hCon, SQL_ATTR_LOGIN_TIMEOUT, SqlPointer(Value), 0);
    Result := OdbcRetcode = OdbcApi.SQL_SUCCESS;
  end;
  if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, hCon, nil, Self,
      nil, nil, {clear last error count=}1);
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.SetLoginTimeout', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.SetLoginTimeout', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc.SetNetworkTimeout;//(hCon: SqlHDbc; TimeoutSeconds: Integer): Boolean;
var
  Value, StmtValue: SQLUINTEGER;
  OdbcRetcode: OdbcApi.SqlReturn;
begin
  {$IFDEF _TRACE_CALLS_} Result := False; try try LogEnterProc('TSqlConnectionOdbc.SetNetworkTimeout', ['hCon =', hCon, 'TimeoutSeconds =', TimeoutSeconds]); {$ENDIF _TRACE_CALLS_}

  if TimeoutSeconds < 0 then
  begin
    Result := True;
    Exit;
  end;

  with fOwnerDbxDriver.fOdbcApi do
  begin

  Result := False;
  Value := TimeoutSeconds;
  StmtValue := Value;

  OdbcRetCode := SQLGetConnectAttr(hCon, SQL_ATTR_CONNECTION_TIMEOUT, @StmtValue, 0, nil);
  if (OdbcRetcode = OdbcApi.SQL_SUCCESS) then
  begin
    if (StmtValue <> Value) then
      OdbcRetcode := SQLSetConnectAttr(hCon, SQL_ATTR_CONNECTION_TIMEOUT, SqlPointer(Value), 0);
    Result := OdbcRetcode = OdbcApi.SQL_SUCCESS;
  end;
  if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, hCon, nil, Self,
        nil, nil, {clear last error count=}1);
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.SetNetworkTimeout', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.SetNetworkTimeout', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
(*
function TSqlConnectionOdbc.GetNetworkTimeout(hCon: SqlHDbc): Integer;
var
  StmtValue: SQLUINTEGER;
  OdbcRetcode: OdbcApi.SqlReturn;
begin
  {$IFDEF _TRACE_CALLS_} Result := -1; try try LogEnterProc('TSqlConnectionOdbc.GetNetworkTimeout', ['hCon =', hCon]); {$ENDIF _TRACE_CALLS_}

  with fOwnerDbxDriver.fOdbcApi do
  begin

  OdbcRetCode := SQLGetConnectAttr(hCon, SQL_ATTR_CONNECTION_TIMEOUT, @StmtValue, 0, nil);
  if OdbcRetcode = OdbcApi.SQL_SUCCESS then
    Result := StmtValue
  else
  begin
    fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, hCon, nil, Self,
      nil, nil, {clear last error count=}1);
    Result := -1;
  end;

  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.GetNetworkTimeout', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.GetNetworkTimeout', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;
//*)
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}

function TSqlConnectionOdbc.GetDefaultConnectionOptions(): PConnectionOptions;
begin
  {$IFDEF _DBX30_}
  if fOwnerDbxDriver.fDBXVersion >= 30 then
    Result := @cConnectionOptionsDefault3
  else
  {$ENDIF}
    Result := @cConnectionOptionsDefault;
end;

//
// FastCharPos:
//
function AnsiCharPosEx(const aSource: AnsiString; const C: AnsiChar; StartPos: Integer = 1): Integer;
var
  L                           : Integer;
begin
  Result := 0;
  if StartPos <= 0 then
    Exit;
  L := Length(aSource);
  if L = 0 then exit;
  if StartPos > L then exit;
  Dec(StartPos);
  asm
      PUSH EDI                 //Preserve this register

      mov  EDI, aSource        //Point EDI at aSource
      add  EDI, StartPos
      mov  ECX, L              //Make a note of how many chars to search through
      sub  ECX, StartPos
      mov  AL,  C              //and which char we want
    @Loop:
      cmp  Al, [EDI]           //compare it against the SourceString
      jz   @Found
      inc  EDI
      dec  ECX
      jnz  @Loop
      jmp  @NotFound
    @Found:
      sub  EDI, aSource        //EDI has been incremented, so EDI-OrigAdress = Char pos !
      inc  EDI
      mov  Result,   EDI
    @NotFound:

      POP  EDI
  end;
end;

{$IFNDEF _D7UP_}
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
 * The Original Code is Fastcode
 *
 * The Initial Developer of the Original Code is Fastcode
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Charalabos Michael <chmichael@creationpower.com>
 * John O'Harrow <john@elmcrest.demon.co.uk>
 * Aleksandr Sharahov
 *
 * BV Version: 1.64
 * ***** END LICENSE BLOCK ***** *)
function PosEx(const SubStr, S: AnsiString; Offset: Integer = 1): Integer;
var
  len, lenSub: integer;
  ch: AnsiChar;
  p, pSub, pStart, pStop: PAnsiChar;
label
  L_Loop0, L_Loop4,
  L_TestT, L_Test0, L_Test1, L_Test2, L_Test3, L_Test4,
  L_AfterTestT, L_AfterTest0,
  L_Ret, L_Exit;
begin;
  pSub:=pointer(SubStr);
  p:=pointer(S);

  if (p=nil) or (pSub=nil) or (Offset<1) then
  begin
    Result:=0;
    goto L_Exit;
  end;

  lenSub:=pinteger(pSub-4)^-1;
  len:=pinteger(p-4)^;
  if (len<lenSub+Offset) or (lenSub<0) then
  begin
    Result:=0;
    goto L_Exit;
  end;

  pStop:=p+len;
  p:=p+lenSub;
  pSub:=pSub+lenSub;
  pStart:=p;
  p:=p+Offset+3;

  ch:=pSub[0];
  lenSub:=-lenSub;
  if p<pStop then
    goto L_Loop4;
  p:=p-4;
  goto L_Loop0;

L_Loop4:
  if ch=p[-4] then
    goto L_Test4;
  if ch=p[-3] then
    goto L_Test3;
  if ch=p[-2] then
    goto L_Test2;
  if ch=p[-1] then
    goto L_Test1;
L_Loop0:
  if ch=p[0] then
    goto L_Test0;
L_AfterTest0:
  if ch=p[1] then
    goto L_TestT;
L_AfterTestT:
  p:=p+6;
  if p<pStop then
    goto L_Loop4;
  p:=p-4;
  if p<pStop then
    goto L_Loop0;
  Result:=0;
  goto L_Exit;

L_Test3: p:=p-2;
L_Test1: p:=p-2;
L_TestT: len:=lenSub;
  if lenSub<>0 then
  repeat
    if (psub[len]<>p[len+1]) or (psub[len+1]<>p[len+2]) then
      goto L_AfterTestT;
    len:=len+2;
  until len>=0;
  p:=p+2;
  if p<=pStop then
    goto L_Ret;
  Result:=0;
  goto L_Exit;

L_Test4: p:=p-2;
L_Test2: p:=p-2;
L_Test0: len:=lenSub;
  if lenSub<>0 then
  repeat
    if (psub[len]<>p[len])
    or (psub[len+1]<>p[len+1]) then
      goto L_AfterTest0;
    len:=len+2;
  until len>=0;
  inc(p);
L_Ret:
  Result:=p-pStart;
L_Exit:
end;
{$ENDIF}

function TSqlConnectionOdbc.GetUnquotedNameLen(const Name: AnsiString): Integer;
var
  iPos, iLen: Integer;
begin
  Result := Length(Name);
  if (fQuoteChar <> cNullAnsiChar) and (Result > 0)then
  begin
    iPos := 1;
    iLen := Result;
    if (Name[Result] = fQuoteChar) then
    begin
      Dec(Result);
      Dec(iLen);
    end;
    if (Result > 0) and (Name[1] = fQuoteChar) then
    begin
      Dec(Result);
      Inc(iPos);
    end;
    case fDbmsType of
      eDbmsTypeOracle:
        // remove quotation for: "package'."function"
        if iLen <> Length(Name) then
        begin
          iPos := {$IFDEF _D12UP_}AnsiStrings.{$ENDIF}PosEx(fQuoteChar, Name, iPos);
          while (iPos> 0) and (iPos < iLen) do
          begin
            iPos := {$IFDEF _D12UP_}AnsiStrings.{$ENDIF}PosEx(fQuoteChar, Name, iPos);
          end;
        end;
    end;
  end;
end;

function TSqlConnectionOdbc.GetUnquotedName(const Name: AnsiString): AnsiString;
var
  iLen: Integer;
begin
  Result := Name;
  if (fQuoteChar <> cNullAnsiChar) then
  begin
    iLen := Length(Name);
    if (iLen > 0) then
    begin
      if (Name[iLen] = fQuoteChar) then
      begin
        SetLength(Result, iLen-1);
        Dec(iLen);
      end;
      if (iLen > 0) and (Name[1] = fQuoteChar) then
        Delete(Result, 1, 1);
      //
      case fDbmsType of
        eDbmsTypeOracle:
          // remove quotation fo: "package'."function"
          if iLen <> Length(Name) then
          begin
            Result := {$IFDEF _D12UP_}AnsiStrings.{$ENDIF}
              StringReplace(Result, fQuoteChar, Ansistring(''), [rfReplaceAll]);
          end;
      end;
    end;
  end;
end;

function TSqlConnectionOdbc.ObjectIsStoredProc(const Name: AnsiString): Boolean;
var
  MetaData: ISQLMetaData25;
  Cursor: ISQLCursor25;
  vSupportsMetadata, vSupportsSchemaFilter: TOptionSwitches;
begin
  Result := False;
  //if fConnectionOptions[coSupportsMetadata] = osOn then
  begin
    vSupportsMetadata := fConnectionOptions[coSupportsMetadata];
    vSupportsSchemaFilter := fConnectionOptions[coSupportsSchemaFilter];
    try
      if getSQLMetaData(MetaData) <> DBXERR_NONE then
        Exit;
      fConnectionOptions[coSupportsMetadata] := osOn;
      fConnectionOptions[coSupportsSchemaFilter] := osOn;
      if (MetaData.getProcedures(PAnsiChar(Name), 0, Cursor) = SQL_SUCCESS) and Assigned(Cursor) then
      begin
        if Cursor.next = DBXERR_NONE then
        begin
          Result := True;
          Exit;
        end;
      end;
    except
    end;
    fConnectionOptions[coSupportsMetadata] := vSupportsMetadata;
    fConnectionOptions[coSupportsSchemaFilter] := vSupportsSchemaFilter;
  end;
end;

function TSqlConnectionOdbc.DecodeObjectFullName;//(
  //{$IFNDEF _D11UP_}const {$ENDIF}ObjectFullName: AnsiString;
  //var sCatalogName, sSchemaName, sObjectName: AnsiString; bStoredProcSpace: Boolean = False): Pointer;
  // ---
  procedure LDoClearResult;
  begin
    sCatalogName := '';
    sSchemaName := '';
    sObjectName := '';
  end;
  // ---
{$IFNDEF _RegExprParser_}
  procedure LDoDecodeObjectFullName;
  var
    TableName: PAnsiChar;
    // ---
    procedure DefaultParseTableName; //{$IFDEF _INLINE_} inline; {$ENDIF}
    var
      dot1, dot2: PAnsiChar;
      C_start, C_end, S_start, S_end, T_start, T_end: Integer;
    begin
      dot1 := StrPos(TableName, '.');

      C_start := 0;
      C_end := 0;

      S_start := 0;
      S_end := 0;

      T_start := 0;
      T_end := StrLen(TableName) - 1;

      if dot1 <> nil then
      begin
        dot2 := StrPos(dot1 + 1, '.');
        if (dot2 = nil) then
        begin
          S_end := dot1 - TableName - 1;
          T_start := dot1 - TableName + 1;
        end
        else
        begin
          C_end := dot1 - TableName - 1;
          S_start := dot1 - TableName + 1;
          S_end := dot2 - TableName - 1;
          T_start := dot2 - TableName + 1;
        end;
      end;

      if (C_end <> 0) then
      begin
        if (TableName[C_start] = fQuoteChar) and (TableName[C_end] = fQuoteChar) then
        begin
          Inc(C_start);
          Dec(C_end);
        end;
        SetLength(sCatalogName, C_end - C_Start + 1);
        StrLCopy(PansiChar(sCatalogName), @TableName[C_start], C_end - C_start + 1);
      end;
      if (S_end <> 0) then
      begin
        if (TableName[S_start] = fQuoteChar) and (TableName[S_end] = fQuoteChar) then
        begin
          Inc(S_start);
          Dec(S_end);
        end;
        SetLength(sSchemaName, S_end - S_Start + 1);
        StrLCopy(PAnsiChar(sSchemaName), @TableName[S_start], S_end - S_start + 1);
      end;

      if (TableName[T_start] = fQuoteChar) and (TableName[T_end] = fQuoteChar) then
      begin
        Inc(T_start);
        Dec(T_end);
      end;
      SetLength(sObjectName, T_end - T_Start + 1);
      StrLCopy(PAnsiChar(sObjectName), @TableName[T_start], T_end - T_start + 1);
    end;
    // ---
    procedure InformixParseTableName; //{$IFDEF _INLINE_} inline; {$ENDIF}
    var
      vTable, vStr: AnsiString;
      p: Integer;
    begin
      // format:   "catalog:schema:table" or "catalog::schema.table"
      //     catalog = database@server or database
      //     schema  = user
      // example:  dbdemos@infserver1:informix.biolife
      vTable := {$IFDEF _D12UP_}AnsiStrings.{$ENDIF}
        StringReplace(StrPas(TableName), '::', ':', [rfReplaceAll]);
      if Length(vTable) = 0 then
        Exit;
      //Catalog:
      p := AnsiPos(AnsiChar(':'), vTable);
      if p > 0 then
      begin
        vStr := Copy(vTable, 1, p - 1);
        if Length(vStr) > 0 then
        begin
          SetLength(sCatalogName, Length(vStr));
          StrLCopy(PansiChar(sCatalogName), PAnsiChar(vStr), Length(vStr));
        end;
        vTable := Copy(vTable, p + 1, Length(vTable) - p);
        if Length(vTable) = 0 then
        begin
          LDoClearResult;
          Exit;
        end;
      end;
      //Schema:
      p := AnsiPos(AnsiChar('.'), vTable);
      if p > 0 then
      begin
        vStr := Copy(vTable, 1, p - 1);
        if Length(vStr) > 0 then
        begin
          SetLength(sSchemaName, Length(vStr));
          StrLCopy(PAnsiChar(sSchemaName), PAnsiChar(vStr), Length(vStr));
        end;
        vTable := Copy(vTable, p + 1, Length(vTable) - p);
        if Length(vTable) = 0 then
        begin
          LDoClearResult;
          Exit;
        end;
      end;
      //Table:
      if Length(Trim(vTable)) = 0 then
      begin
        LDoClearResult;
        Exit;
      end;
      SetLength(sObjectName, Length(vTable));
      StrLCopy(PAnsiChar(sObjectName), PAnsiChar(vTable), Length(vTable));
    end;
    // ---
  begin
    TableName := PAnsiChar(ObjectFullName);
    LDoClearResult;
    if fDbmsType <> eDbmsTypeInformix then
      DefaultParseTableName
    else
      InformixParseTableName;
  end;
{$ENDIF}
  // ---
begin
  if (ObjectFullName = '') or (GetUnquotedNameLen(ObjectFullName) = 0) then
  begin
    LDoClearResult;
    Result := nil;
    Exit;
  end;
{$IFDEF _RegExprParser_}
  Result := fObjectNameParser.DecodeObjectFullName(
    ObjectFullName, sCatalogName, sSchemaName, sObjectName);
{$ELSE}
  LDoDecodeObjectFullName;
  Result := nil;
{$ENDIF}
  case fDbmsType of
    eDbmsTypeOracle:
      begin
        if (sCatalogName <> '') and (fConnectionOptions[coSupportsCatalog] <> osOn) then
        begin
          if sSchemaName <> '' then
          begin
            sObjectName := sSchemaName + '.' + sObjectName;
            sSchemaName := sCatalogName;
            sCatalogName := '';
          end;
        end;

        if fWantQuotedTableName and (fQuoteChar <> cNullAnsiChar) and (sObjectName <> '') then
        begin
          if (sCatalogName = '') and (sSchemaName <> '') then
          begin
            // check oracle sSchemaName equl package name
            if bStoredProcSpace and (ObjectIsStoredProc(AnsiUpperCase(sSchemaName) + '.' + AnsiUpperCase(sObjectName))) then
            begin
              sObjectName := sSchemaName + '.' + sObjectName;
              sSchemaName := '';
            end;
          end;
          // remova quotation for: "package"."function"
          sObjectName := {$IFDEF _D12UP_}AnsiStrings.{$ENDIF}
            StringReplace(sObjectName, fQuoteChar, AnsiString(''), [rfReplaceAll]);
        end;
      end;
  end; // case
end;

function TSqlConnectionOdbc.EncodeObjectFullName;//(
  //const CatalogName, SchemaName, ObjectName: AnsiString;
  //pTemplateInfo: Pointer = nil): AnsiString;
begin
  if fWantQuotedTableName and (fQuoteChar <> cNullAnsiChar) then
  begin
    CatalogName := GetUnquotedName(CatalogName);
    SchemaName := GetUnquotedName(SchemaName);
    ObjectName := GetUnquotedName(ObjectName);
  end;
{$IFDEF _RegExprParser_}
  Result := fObjectNameParser.EncodeObjectFullName(
    CatalogName, SchemaName, ObjectName, PObjectNameTemplateInfo(pTemplateInfo));
{$ELSE}
  case fDbmsType of
    eDbmsTypeInformix:
      begin
        Result := ObjectName;
        if SchemaName <> '' then
          Result := SchemaName + '.' + Result;
        if CatalogName <> '' then
          Result := CatalogName + ':' + Result;
      end;
    else
    begin
      if fWantQuotedTableName and ( fQuoteChar <> cNullAnsiChar) then
      begin
        Result := fQuoteChar + ObjectName + fQuoteChar;
        if SchemaName <> '' then
          Result := fQuoteChar + SchemaName + fQuoteChar + '.' + Result;
        if CatalogName <> '' then
          Result := fQuoteChar + CatalogName + fQuoteChar + '.' + Result;
      end
      else
      begin
        Result := ObjectName;
        if SchemaName <> '' then
          Result := SchemaName + '.' + Result;
        if CatalogName <> '' then
          Result := CatalogName + '.' + Result;
      end
      else
    end;
  end;
{$ENDIF}
  case fDbmsType of
    eDbmsTypeOracle:
      begin
        if fWantQuotedTableName and (fQuoteChar <> cNullAnsiChar) and (CatalogName = '') and (SchemaName = '')
          and (Result <> '') and (AnsiPos(AnsiChar('.'), Result) > 0) then
        begin
          // remove quotation for: "package.function"
          Result := {$IFDEF _D12UP_}AnsiStrings.{$ENDIF}
            StringReplace(Result, fQuoteChar, AnsiString(''), [rfReplaceAll]);
        end;
      end;
  end; // case
end;

function TSqlConnectionOdbc.GetQuotedObjectName;
//(const ObjectName: AnsiString; bStoredProcSpace: Boolean = False; bWithoutQuoting: Boolean = False): AnsiString;
var
  vCatalogName, vSchemaName, vObjectName: AnsiString;
  pTemplate: Pointer;
begin
  if (ObjectName = '') or (GetUnquotedNameLen(ObjectName) = 0) then
  begin
    Result := '';
  end
  else if not bWithoutQuoting then
  begin
    // Extract of parts name from full name
    pTemplate := DecodeObjectFullName(ObjectName, vCatalogName, vSchemaName, vObjectName);
    // Agregate of parts name into full dbms name ...
    Result := EncodeObjectFullName(vCatalogName, vSchemaName, vObjectName, pTemplate);
  end
  else
  begin
    // remove quotations:
    DecodeObjectFullName(ObjectName, vCatalogName, vSchemaName, vObjectName, bStoredProcSpace);
    Result := vObjectName;
    if vSchemaName <> '' then
      Result := vSchemaName + '.' + Result;
    if vCatalogName <> '' then
    begin
      case fDbmsType of
        eDbmsTypeInformix:
          Result := vCatalogName + ':' + Result;
        else
          Result := vCatalogName + '.' + Result;
      end;
    end
  end;
end;

{$IFDEF _DBXCB_}
procedure TSqlConnectionOdbc.DbxCallBackSendMsg;//(TraceCat: TRACECat; const Msg: AnsiString);
var
  {$IFDEF _DBX30_}
  vSQLTRACEDesc30: SQLTRACEDesc30;
  wMsg: WideString;
  {$ENDIF}
  vSQLTRACEDesc25: SQLTRACEDesc25 {$IFDEF _DBX30_}absolute vSQLTRACEDesc30{$ENDIF};
  sMsg: AnsiString;
  iLen: Word;
begin
  if Assigned(fDbxTraceCallbackEven) and (fDbxTraceClientData > 0) then
  if not ((fOwnerDbxDriver.fClientVersion = 30) and (TraceCat = cTDBXTraceFlags_Command)) then { <=: QC: 58678: skip Delphi 2006 callback bag for TSQLMonitor.UpdateTraceCallBack }
  // ^--=: detailed explanation for footnote "20080224#2".
  try
    {$IFDEF _DBX30_}
    if fOwnerDbxDriver.fDBXVersion >= 30 then with vSQLTRACEDesc30 do
    begin
      eTraceCat := TraceCat;
      ClientData := fDbxTraceClientData;
      wMsg := WideString(Msg);
      iLen := Length(wMsg);
      uTotalMsgLen := min(1023, iLen);
      if iLen > uTotalMsgLen then
        SetLength(wMsg, uTotalMsgLen);
      Move(wMsg[1], pszTrace[0], uTotalMsgLen * SizeOf(WideChar));
      // Fix: Delphi 2007 last symbol
      pszTrace[uTotalMsgLen] := cNullWideChar;
    end
    else
    {$ENDIF}
    with vSQLTRACEDesc25 do
    begin
{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
{

*** ref "20080224#1":

QC: 58675

Delphi 2006 bug:

SqlExpr.pas:

function SQLCallBack25(CallType: TRACECat; CBInfo25: Pointer): CBRType; stdcall;
var
  CBInfo30 : SQLTraceDesc30;
begin
  Result := cbrUSEDEF;
  if CBInfo25 <> nil then
  begin
    WStrPLCopy(CBInfo30.pszTrace, pSQLTRACEDesc25(CBInfo25).pszTrace, 1023);
    CBInfo30.eTraceCat := pSQLTRACEDesc30(CBInfo25).eTraceCat; // *** bug
    CBInfo30.ClientData   := pSQLTRACEDesc30(CBInfo25).ClientData; // *** bug
    CBInfo30.uTotalMsgLen := pSQLTRACEDesc30(CBInfo25).uTotalMsgLen; // *** bug
    Result := TSQLMonitor(pSQLTRACEDesc25(CBInfo25).ClientData).InvokeCallback(CallType, @CBInfo30);
  end;
end;

fix:

function SQLCallBack25(CallType: TRACECat; CBInfo25: Pointer): CBRType; stdcall;
var
  CBInfo30 : SQLTraceDesc30;
begin
  Result := cbrUSEDEF;
  if CBInfo25 <> nil then with pSQLTRACEDesc25(CBInfo25)^ do
  begin
    WStrPLCopy(CBInfo30.pszTrace, pszTrace, 1023);
    CBInfo30.eTraceCat := eTraceCat;
    CBInfo30.ClientData   := ClientData;
    CBInfo30.uTotalMsgLen := uTotalMsgLen;
    Result := TSQLMonitor(ClientData).InvokeCallback(CallType, @CBInfo30);
  end;
end;

QC: 58678

*** ref "20080224#2":

Delphi 2006 bug:

SqlExpr.pas:

Mistake reveals itself when SQLMonitor.Active=False and SQLMonitor.Connection <> nil.
When closing the form on which there is unbalanced connection appears AV...

fix:

procedure TSQLMonitor.UpdateTraceCallBack;
begin
  if Assigned(FSQLConnection) then
  begin
    // OLD:
    //if Assigned(FSQLConnection.SQLConnection) then
    // NEW:
    if Active and Assigned(FSQLConnection.SQLConnection) then

todo:
  check on D2007

}
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
      eTraceCat := TraceCat;
      ClientData := fDbxTraceClientData;
      sMsg := Msg;
      iLen := Length(sMsg);
      uTotalMsgLen := min(1024, iLen);
      if iLen > uTotalMsgLen then
        SetLength(sMsg, uTotalMsgLen);
      Move(sMsg[1], pszTrace[0], uTotalMsgLen);
    end;
    fDbxTraceCallbackEven(TraceCat, @vSQLTRACEDesc25);
  except
    fDbxTraceCallbackEven := nil;
    fDbxTraceClientData := 0;
  end;
end;

procedure TSqlConnectionOdbc.DbxCallBackSendMsgFmt;//(TraceCat: TRACECat;
// const FmtMsg: AnsiString; const FmtArgs: array of const);
var
  aMsg: AnsiString;
begin
  try
    aMsg := Format(FmtMsg, FmtArgs);
    DbxCallBackSendMsg(TraceCat, aMsg);
  except
  end;
end;

{$ENDIF _DBXCB_}

function TSqlConnectionOdbc.FindFreeConnection;//(out DbxConStmtInfo: TDbxConStmtInfo;
//  MaxStatementsPerConnection: Integer; bMetadataRead: Boolean = False;
//  bOnlyPreservedCursors: Boolean = False): Boolean;

  // =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  // Search of free connection with a status of transaction equivalent current (fInTransaction).
  // In case of reading the metadata (bMetadataRead = True) the status of transaction is
  // unimportant.
  // **(1):
  // (ERROR: It is incorrect in case INFORMIX. INFORMIX processes transactions for DDL).
  // But for INFORMIX StatementsPerConnection == 0 :) .
  // =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

var
  i, iAddCount: Integer;
  iDbxConStmt, NullDbxConStmt: PDbxConStmt;
  DbxHStmtNode: PDbxHStmtNode;
  // ---
  function IsFreeDbxConStmt(aDbxConStmt: PDbxConStmt): Boolean;
  begin
    Result := False;
    if// Is reserved SqlHstmt:
      (aDbxConStmt.fSqlHStmtAllocated <= MaxStatementsPerConnection) // =>: nil <> aDbxConStmt.fNullDbxHStmtNodes when MaxStatementsPerConnection > 0
      // Check Transaction mode:
      and (
        // Metadata can be read in any transaction state:
        {
        ( bMetadataRead ) // Look a footnote 1 **(1)
        or                //                         }
        ( // We exclude transactions distinct from the current status:
          (aDbxConStmt.fInTransaction = fInTransaction)
        )
        or
        ( // Connection can change a status of transaction:
          (fInTransaction <> 0)
          and
          (aDbxConStmt.fSqlHStmtAllocated = 0)
        )
      )
      // Check fCursorPreserved (The probability of blocking increases, but situations
      //                         of destruction of the cursor are removed):
      (*
      and (
        ( fCursorPreserved )
        or
        (aDbxConStmt.fActiveCursors = 0)
        //or
        // At reading the Metadata the status of transaction will not be changed:
        //( bMetadataRead )
      )
      //*)
    then
    begin

      if bOnlyPreservedCursors  // When you will start transaction, but have open cursors then
         and                    // for transaction allocate clean or new connection.
         ( not fCursorPreserved )
         and
         (aDbxConStmt.fActiveCursors > 0)
         and
         aDbxConStmt.fDeadConnection
      then
        exit;

      // Synchronize Transaction:

      if (aDbxConStmt.fInTransaction <> fInTransaction)
        and
        ( (fInTransaction - aDbxConStmt.fInTransaction) = 1)
      then
        SynchronizeInTransaction(aDbxConStmt^)
      else
      if (fInTransaction = 0) and (aDbxConStmt.fInTransaction = 0) then
        aDbxConStmt.fOutOfDateCon := False
      else
      if aDbxConStmt.fOutOfDateCon then
        exit;

      DbxConStmtInfo.fDbxConStmt := aDbxConStmt;

      // Search of "not allocated SqlHStmt Statement ( == SQL_NULL_HANDLE)":
      if aDbxConStmt.fNullDbxHStmtNodes = nil then
      begin
         iAddCount := fStatementPerConnection - aDbxConStmt.fSqlHStmtAllocated;
         if iAddCount > cStatementPerConnectionBlockCount then
           iAddCount := cStatementPerConnectionBlockCount;
         AllocateDbxHStmtNodes(@DbxConStmtInfo, {allocate new statements buffer}iAddCount);
      end;

      DbxHStmtNode := aDbxConStmt.fNullDbxHStmtNodes;
      aDbxConStmt.fNullDbxHStmtNodes := DbxHStmtNode.fNextDbxHStmtNode;
      if Assigned(DbxHStmtNode.fNextDbxHStmtNode) then
        DbxHStmtNode.fNextDbxHStmtNode.fPrevDbxHStmtNode := nil;
      DbxConStmtInfo.fDbxHStmtNode := DbxHStmtNode;

      Result := True;
    end;
  end;
  // ---
begin
  //
  // use only when sStatementsPerConnection > 0
  //
  {$IFDEF _TRACE_CALLS_} Result := False; try try LogEnterProc('TSqlConnectionOdbc.FindFreeConnection', ['MaxStatementsPerConnection =', MaxStatementsPerConnection, 'MetadataRead =', bMetadataRead]); {$ENDIF _TRACE_CALLS_}
  if Assigned(fCurrDbxConStmt)
    and (fCurrDbxConStmt.fHCon <> SQL_NULL_HANDLE)
    and IsFreeDbxConStmt( fCurrDbxConStmt ) then
  begin
    Result := True;
    exit;
  end;

  {$ifNdef _debug_blocking_}
  GetCurrentDbxConStmt(); // calculate connection contained fRowsAffected > 0.
  if Assigned(fCurrDbxConStmt)
    and (fCurrDbxConStmt.fHCon <> SQL_NULL_HANDLE)
    and IsFreeDbxConStmt( fCurrDbxConStmt ) then
  begin
    Result := True;
    exit;
  end;
  {$endif}

  DbxConStmtInfo.fDbxConStmt := nil;
  DbxConStmtInfo.fDbxHStmtNode := nil;
  if MaxStatementsPerConnection < 0 then
    MaxStatementsPerConnection := 0;
  NullDbxConStmt := nil;
  // Search of connection not involved completely:
  for i := fDbxConStmtList.Count - 1 downto 0 do
  begin
    iDbxConStmt := fDbxConStmtList[i];
    if iDbxConStmt = nil then
      continue;
    if (iDbxConStmt.fHCon = SQL_NULL_HANDLE) then
      NullDbxConStmt := iDbxConStmt
    else
    begin
      if IsFreeDbxConStmt( iDbxConStmt ) then
      begin
        Result := True;
        exit;
      end;
    end;
  end;//of: for i
  if Assigned(NullDbxConStmt) then
  begin
    Result := True;
    DbxConStmtInfo.fDbxConStmt := NullDbxConStmt;
  end
  else
    Result := False; // not found
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.FindFreeConnection', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.FindFreeConnection', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlConnectionOdbc.AllocHStmt;//(out HStmt: SqlHStmt;
//  aDbxConStmtInfo: PDbxConStmtInfo = nil; bMetadataRead: Boolean = False);
var
  OdbcRetcode: OdbcApi.SqlReturn;
  DbxConStmtInfo: TDbxConStmtInfo;
  DbxHStmtNode: PDbxHStmtNode;
begin
  {$IFDEF _TRACE_CALLS_}
    try try
    LogEnterProc('TSqlConnectionOdbc.AllocHStmt', ['HStmt =', HStmt, 'fSqlHStmtAllocated =', fSqlHStmtAllocated]);
    if (fStatementPerConnection > 0) then
      LogInfoProc(['aDbxConStmtInfo =', aDbxConStmtInfo,
        'fDbxHStmtNode =', aDbxConStmtInfo.fDbxHStmtNode, 'fDbxConStmt =', aDbxConStmtInfo.fDbxConStmt]);
  {$ENDIF _TRACE_CALLS_}
  with fOwnerDbxDriver.fOdbcApi do
  begin

  if (fStatementPerConnection > 0) then
  begin
    FindFreeConnection(DbxConStmtInfo, fStatementPerConnection-1, bMetadataRead);

    if ( DbxConStmtInfo.fDbxConStmt = nil )
      or (DbxConStmtInfo.fDbxConStmt.fHCon = SQL_NULL_HANDLE)
    then
      CloneOdbcConnection(DbxConStmtInfo);

    if DbxConStmtInfo.fDbxHStmtNode = nil then
    begin
      // Search of "not allocated SqlHStmt Statement ( == SQL_NULL_HANDLE)":
      DbxHStmtNode := DbxConStmtInfo.fDbxConStmt.fNullDbxHStmtNodes;
      if DbxHStmtNode = nil then
        raise EDbxInternalError.Create('TSqlConnectionOdbc.AllocHStmt(): cannot alocate new SqlStmt.');
      DbxConStmtInfo.fDbxConStmt.fNullDbxHStmtNodes := DbxHStmtNode.fNextDbxHStmtNode;
      if Assigned(DbxHStmtNode.fNextDbxHStmtNode) then
        DbxHStmtNode.fNextDbxHStmtNode.fPrevDbxHStmtNode := nil;
      DbxConStmtInfo.fDbxHStmtNode := DbxHStmtNode;
    end;

    with DbxConStmtInfo do
    begin
      OdbcRetcode := SQLAllocHandle(SQL_HANDLE_STMT, fDbxConStmt.fHCon, HStmt);
      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLAllocHandle(SQL_HANDLE_STMT)',
          SQL_HANDLE_STMT, fDbxConStmt.fHCon, @fDbxConStmt, nil, nil, nil, 0, cTDBXTraceFlags_Command);
      {$IFDEF _DBXCB_}
      if Assigned(fDbxTraceCallbackEven) then
        DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'stmt (allocate): "$%x"', [Integer(HStmt)]);
      {$ENDIF}
      DbxConStmtInfo.fDbxHStmtNode.HStmt := HStmt;
      if fDbxConStmt.fSqlHStmtAllocated = 0 then
        dec(Self.fCon0SqlHStmt);
      inc(fDbxConStmt.fSqlHStmtAllocated);
    end;

    if Assigned(aDbxConStmtInfo) then
      aDbxConStmtInfo^ := DbxConStmtInfo;
  end
  else
  begin
    OdbcRetcode := SQLAllocHandle(SQL_HANDLE_STMT, fhCon, HStmt);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLAllocHandle(SQL_HANDLE_STMT)',
        SQL_HANDLE_STMT, fhCon, nil, nil, nil, nil, 0, cTDBXTraceFlags_Command);
    {$IFDEF _DBXCB_}
    if Assigned(fDbxTraceCallbackEven) then
      DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'stmt (allocate): "$%x"', [Integer(HStmt)]);
    {$ENDIF}
    if Assigned(aDbxConStmtInfo) then
      aDbxConStmtInfo.fDbxConStmt := nil;
  end;

  inc(fSqlHStmtAllocated);

  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.AllocHStmt', e);  raise; end; end;
    finally
      if (fStatementPerConnection > 0) then
        LogExitProc('TSqlConnectionOdbc.AllocHStmt', ['HStmt =', HStmt, 'fSqlHStmtAllocated =', fSqlHStmtAllocated,
          '; { aDbxConStmtInfo =', aDbxConStmtInfo,
          'fDbxHStmtNode =', aDbxConStmtInfo.fDbxHStmtNode,
          'fDbxConStmt =', aDbxConStmtInfo.fDbxConStmt,
          'fDbxHStmtNode.HStmt =', aDbxConStmtInfo.fDbxHStmtNode.HStmt, ' }'])
      else
        LogExitProc('TSqlConnectionOdbc.AllocHStmt', ['HStmt =', HStmt, 'fSqlHStmtAllocated =', fSqlHStmtAllocated]);
    end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlConnectionOdbc.FreeHStmt;//(out HStmt: SqlHStmt;
//  aDbxConStmtInfo: PDbxConStmtInfo = nil);
var
  OdbcRetcode: OdbcApi.SqlReturn;
  iDbxConStmt: PDbxConStmt;
  // ---
  procedure DoRelaseDbxConStmt;
  var
    aDbxHStmtNode: PDbxHStmtNode;
  begin
    with fOwnerDbxDriver.fOdbcApi do
    begin

    if iDbxConStmt.fHCon <> SQL_NULL_HANDLE then
    begin
      {$IFDEF _DBXCB_}
      if Assigned(fDbxTraceCallbackEven) then
        DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'stmt (free): "$%x"', [Integer(HStmt)]);
      {$ENDIF}
      OdbcRetcode := SQLFreeHandle(SQL_HANDLE_STMT, HStmt);
      if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) and (not iDbxConStmt.fDeadConnection) then
        fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLFreeHandle(SQL_HANDLE_STMT)',
          SQL_HANDLE_STMT, HStmt, @iDbxConStmt, nil, nil, nil, 0, cTDBXTraceFlags_Command);
    end;
    HStmt := SQL_NULL_HANDLE;
    aDbxConStmtInfo.fDbxHStmtNode.HStmt := SQL_NULL_HANDLE;
    // Indicate that connection is free to be re-used...
    dec(iDbxConStmt.fSqlHStmtAllocated);
    if iDbxConStmt.fSqlHStmtAllocated = 0 then
      inc(Self.fCon0SqlHStmt);
    dec(fSqlHStmtAllocated);

    // remove SqlHStmt from active list
    aDbxHStmtNode := aDbxConStmtInfo.fDbxConStmt.fActiveDbxHStmtNodes;
    if aDbxHStmtNode = aDbxConStmtInfo.fDbxHStmtNode then
    begin
      aDbxConStmtInfo.fDbxConStmt.fActiveDbxHStmtNodes := aDbxHStmtNode.fNextDbxHStmtNode;
      aDbxHStmtNode.fNextDbxHStmtNode.fPrevDbxHStmtNode := nil;
    end
    else
    begin
      aDbxHStmtNode := aDbxConStmtInfo.fDbxHStmtNode.fPrevDbxHStmtNode;
      if Assigned(aDbxHStmtNode) then
      begin
        if Assigned(aDbxConStmtInfo.fDbxHStmtNode.fNextDbxHStmtNode) then
        begin
          aDbxHStmtNode.fNextDbxHStmtNode := aDbxConStmtInfo.fDbxHStmtNode.fNextDbxHStmtNode;
          aDbxConStmtInfo.fDbxHStmtNode.fNextDbxHStmtNode.fPrevDbxHStmtNode :=
            aDbxHStmtNode.fNextDbxHStmtNode;
        end
        else
        begin
          aDbxHStmtNode.fNextDbxHStmtNode := nil;
        end;
      end
      else
      begin
        aDbxHStmtNode := aDbxConStmtInfo.fDbxHStmtNode.fNextDbxHStmtNode;
        if Assigned(aDbxHStmtNode) then
          aDbxHStmtNode.fPrevDbxHStmtNode := nil;
      end;
    end;

    // insert SqlHStmt to no allocated list:
    aDbxConStmtInfo.fDbxHStmtNode.fPrevDbxHStmtNode := nil;
    aDbxHStmtNode := aDbxConStmtInfo.fDbxConStmt.fNullDbxHStmtNodes;
    if Assigned(aDbxHStmtNode) then
    begin
      aDbxHStmtNode.fPrevDbxHStmtNode := aDbxConStmtInfo.fDbxHStmtNode;
      aDbxConStmtInfo.fDbxHStmtNode.fNextDbxHStmtNode := aDbxHStmtNode;
    end
    else
    begin
      aDbxConStmtInfo.fDbxHStmtNode.fNextDbxHStmtNode := nil;
      aDbxConStmtInfo.fDbxConStmt.fNullDbxHStmtNodes :=
        aDbxConStmtInfo.fDbxHStmtNode;
    end;

    if (Length(iDbxConStmt.fBucketDbxHStmtNodes) - iDbxConStmt.fSqlHStmtAllocated) >
      ( cStatementPerConnectionBlockCount + (cStatementPerConnectionBlockCount * 2) div 3 )
    then
      // Remove Null DbxHStmtNodes:
      AllocateDbxHStmtNodes(aDbxConStmtInfo, {!!!: negative } - cStatementPerConnectionBlockCount);

    aDbxConStmtInfo.fDbxHStmtNode := nil;
    aDbxConStmtInfo.fDbxConStmt := nil;

    if (iDbxConStmt.fInTransaction <> 0) then
    begin
      if (Self.fInTransaction <> 0) then
        iDbxConStmt.fOutOfDateCon := False;

      // compact connection:
      {begin:}
        if (iDbxConStmt <> fDbxConStmtList[0]) //first connection is locked
           and
           (iDbxConStmt.fSqlHStmtAllocated = 0) // to compact probably connection without SqlHStmt
        then
        begin
          // compact empty connection
          if fDbxConStmtActive - Self.fCon0SqlHStmt > cMaxCacheConnectionCount then
          begin
            SQLDisconnect(iDbxConStmt.fHCon);
            iDbxConStmt.fHCon := SQL_NULL_HANDLE;
            iDbxConStmt.fDeadConnection := False;
            dec(fDbxConStmtActive);
            iDbxConStmt.fRowsAffected := 0;
          end;
          iDbxConStmt.fOutOfDateCon := False;
          // compact SQL_NULL_HANDLE
          if fDbxConStmtList.Count - fDbxConStmtActive > cMaxCacheNullConnectionCount then
          begin
            if fCurrDbxConStmt = iDbxConStmt then
              fCurrDbxConStmt := nil;
            fDbxConStmtList.Remove(iDbxConStmt);
            DisposeDbxConStmt(iDbxConStmt);
          end;
        end;
      {end.}
    end;

    end;//of with fOdbcApi
  end;//of: procedure DoRelaseDbxConStmt;
  // ---
begin
  {$IFDEF _TRACE_CALLS_}
    try try
    LogEnterProc('TSqlConnectionOdbc.FreeHStmt', ['HStmt =', HStmt]);
    if (fStatementPerConnection > 0) then
      LogInfoProc(['aDbxConStmtInfo =', aDbxConStmtInfo,
        'fDbxHStmtNode =', aDbxConStmtInfo.fDbxHStmtNode,
        'fDbxConStmt =', aDbxConStmtInfo.fDbxConStmt,
        'HStmt =', aDbxConStmtInfo.fDbxHStmtNode.HStmt]);
  {$ENDIF _TRACE_CALLS_}
  if HStmt = SQL_NULL_HANDLE then
    exit;
  if (fStatementPerConnection > 0) then
  begin
    if Assigned(aDbxConStmtInfo) and Assigned(aDbxConStmtInfo.fDbxHStmtNode)then
    with aDbxConStmtInfo^ do
    begin
      iDbxConStmt := aDbxConStmtInfo.fDbxConStmt;
      if fDbxHStmtNode.HStmt = SQL_NULL_HANDLE then
      begin
        fDbxHStmtNode.HStmt := HStmt;
        //{$IFDEF _TRACE_CALLS_}
        //  LogInfoProc(['### BUG ###', HStmt]);
        //{$endif}
      end;
      if ( fDbxHStmtNode.HStmt = HStmt ) then
      begin
        DoRelaseDbxConStmt();
        exit;
      end;
    end;
    //if we reach here, the statement handle was not found in the list
    raise
      EDbxInternalError.Create('TSqlConnectionOdbc.FreeHStmt - Statement handle was not found in list');
  end
  else
  with fOwnerDbxDriver.fOdbcApi do
  begin
    {$IFDEF _DBXCB_}
    if Assigned(fDbxTraceCallbackEven) then
      DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'stmt (free): "$%x"', [Integer(HStmt)]);
    {$ENDIF}
    OdbcRetcode := SQLFreeHandle(SQL_HANDLE_STMT, HStmt);
    if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) and (not fConnectionClosed) then
      OdbcCheck(OdbcRetcode, 'SQLFreeHandle(SQL_HANDLE_STMT)', nil, cTDBXTraceFlags_Command);
    HStmt := SQL_NULL_HANDLE;
    dec(fSqlHStmtAllocated);
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.FreeHStmt', e);  raise; end; end;
    finally
      if (fStatementPerConnection > 0) then
        LogExitProc('TSqlConnectionOdbc.FreeHStmt', ['HStm t=', HStmt,
          '; { aDbxConStmtInfo =', aDbxConStmtInfo,
          'fDbxHStmtNode =', aDbxConStmtInfo.fDbxHStmtNode,
          'fDbxConStmt =', aDbxConStmtInfo.fDbxConStmt, ' }'])
      else
        LogExitProc('TSqlConnectionOdbc.FreeHStmt', ['HStmt =', HStmt]);
    end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc.GetCurrentDbxConStmt: PDbxConStmt;
var
  i: Integer;
  iDbxConStmt, iN0DbxConStmt, iNNDbxConStmt : PDbxConStmt;
begin
  Result := nil;
  if (fStatementPerConnection > 0) then
  begin
    if (fDbxConStmtList.Count = 1) then
      Result := fDbxConStmtList[0]
    else
    if Assigned(fCurrDbxConStmt)
      and (fCurrDbxConStmt.fHCon <> SQL_NULL_HANDLE)
      and (not fCurrDbxConStmt.fOutOfDateCon)
      and (fCurrDbxConStmt.fInTransaction = fInTransaction)
    then
      Result := fCurrDbxConStmt
    else
    begin
      fCurrDbxConStmt := nil;
      iN0DbxConStmt := nil;
      iNNDbxConStmt := nil;
      for i := fDbxConStmtList.Count-1 downto 1 do
      begin
        iDbxConStmt := fDbxConStmtList[i];
        if (iDbxConStmt = nil)
          or (iDbxConStmt.fHCon = SQL_NULL_HANDLE)
          or (iDbxConStmt.fOutOfDateCon)
          or (iDbxConStmt.fInTransaction <> fInTransaction)
        then
          continue;

        if (iDbxConStmt.fRowsAffected > 0)
          and
          (iDbxConStmt.fSqlHStmtAllocated >= 0)
          and
          (iDbxConStmt.fSqlHStmtAllocated < fStatementPerConnection)
        then
        begin
          fCurrDbxConStmt := iDbxConStmt;
          Result := iDbxConStmt;
          exit;
        end
        else
          iN0DbxConStmt := iDbxConStmt;
        iNNDbxConStmt := iDbxConStmt;
      end;//of: for i
      if iN0DbxConStmt <> nil then
        Result := iN0DbxConStmt
      else
      if iNNDbxConStmt <> nil then
        Result := iNNDbxConStmt
      else
        Result := fDbxConStmtList[0];
    end;
  end;
end;

function TSqlConnectionOdbc.GetCurrentDbxConStmt(out HStmt: SqlHStmt): PDbxConStmt;
begin
  Result := GetCurrentDbxConStmt();
  if Result <> nil then
    HStmt := Result.fHCon
  else
    HStmt := fhCon;
end;

function TSqlConnectionOdbc.GetCurrentConnectionHandle: SqlHDbc;
var
  aDbxConStmt: PDbxConStmt;
begin
  aDbxConStmt := GetCurrentDbxConStmt();
  if aDbxConStmt = nil then
    Result := fhCon
  else
    Result := aDbxConStmt.fHCon
end;

procedure TSqlConnectionOdbc.OdbcCheck;//(OdbcCode: SqlReturn;
//  const OdbcFunctionName: AnsiString; DbxConStmt: PDbxConStmt; TraceCat: TRACECat = cTDBXTraceFlags_none);
begin
  fOwnerDbxDriver.OdbcCheck(OdbcCode, OdbcFunctionName, SQL_HANDLE_DBC,
    GetCurrentConnectionHandle, DbxConStmt, Self, nil, nil, 0, TraceCat);
end;

function TSqlConnectionOdbc.GetCatalog;//(aHConStmt: SqlHDbc = SQL_NULL_HANDLE): AnsiString;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  aCurrentCatalogLen: SqlInteger;
begin
  Result := '';
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc.GetCatalog', ['HConStmt =', aHConStmt]); {$ENDIF _TRACE_CALLS_}
  if fSupportsCatalog then
  begin
    SetLength(Result, fOdbcMaxCatalogNameLen);
    FillChar(Result[1], fOdbcMaxCatalogNameLen, 0);
    if aHConStmt = SQL_NULL_HANDLE then
      aHConStmt := fhCon;
    aCurrentCatalogLen := 0;
    with fOwnerDbxDriver.fOdbcApi do
    OdbcRetcode := SQLGetConnectAttr(
      aHConStmt,
      SQL_ATTR_CURRENT_CATALOG,
      PAnsiChar(Result),
      fOdbcMaxCatalogNameLen,
      @aCurrentCatalogLen);
    if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
    begin
      Result := '';
      // clear last error:
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, aHConStmt, nil, Self,
        nil, nil, {clear last error count=}1);
    end
    else
    begin
      // check returned catalog length:
      if (aCurrentCatalogLen >= 0) and (aCurrentCatalogLen <= fOdbcMaxCatalogNameLen) then
        SetLength(Result, aCurrentCatalogLen) // trim #0 chars
      else // Incorrect value aCurrentCatalogLen is returned:
        Result := StrPas( PAnsiChar(Result) );
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.GetCatalog', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.GetCatalog', ['Catalog =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlConnectionOdbc.GetCurrentCatalog;//(aHConStmt: SqlHDbc = SQL_NULL_HANDLE);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc.GetCurrentCatalog', ['HConStmt =', aHConStmt, 'CurrentCatalog =', fCurrentCatalog]);{$ENDIF _TRACE_CALLS_}
  if fSupportsCatalog then
  begin
    fCurrentCatalog := GetCatalog(aHConStmt);
    if fCurrentCatalog = '' then
      fSupportsCatalog := False;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.GetCurrentCatalog', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.GetCurrentCatalog', ['CurrentCatalog =', fCurrentCatalog]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc.beginTransaction;//(TranID: Longword): SQLResult;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  aTranID: pTTransactionDesc;
  NewOdbcIsolationLevel: SqlUInteger;
  aDbxConStmtInfo: TDbxConStmtInfo;
  aHCon: SqlHDbc;
  AttrVal: SqlUInteger;
  iDbxConStmt: PDbxConStmt;
  i: Integer;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc.beginTransaction', ['TranID =', GetTransactionDescStr(pTTransactionDesc(TranID))]); {$ENDIF _TRACE_CALLS_}
  {$IFDEF _DBXCB_}
  if Assigned(fDbxTraceCallbackEven) then
    DbxCallBackSendMsgFmt(cTDBXTraceFlags_Transact, 'transact (begin): conn: "$%x", support:"%d" :', [Integer(Self), Integer(fSupportsTransaction)]);
  {$ENDIF}
  {
    Transactions in ODBC are not explicitly initiated.
    But we must make sure we are in Manual Commit Mode.
    Also, if a statement is executed after the transation has been committed,
    without another call to beginTransaction, we must go back to Auto Commit mode
    (see procedure TransactionCheck)
  }
  with fOwnerDbxDriver.fOdbcApi do
  try

    if (fInTransaction > 0) and (not fSupportsNestedTransactions ) then
      raise EDbxInvalidCall.Create(
        'TSqlConnectionOdbc.beginTransaction - Cannot start a new transaction because a ' +
        'transaction is already active.');

    NewOdbcIsolationLevel := 0;
    if fSupportsTransaction then
    begin
      if fInTransaction = 0 then
      begin
        aTranID := pTTransactionDesc(TranId);
        case TTransIsolationLevel(aTranId.IsolationLevel) of
          // Note that ODBC defines an even higher level of isolation, viz, SQL_TXN_SERIALIZABLE;
          // In this mode, Phantoms are not possible. (See ODBC spec).
          xilREPEATABLEREAD:
            // Dirty reads and nonrepeatable reads are not possible. Phantoms are possible
            NewOdbcIsolationLevel := SQL_TXN_REPEATABLE_READ;
          xilREADCOMMITTED:
            // Dirty reads are not possible. Nonrepeatable reads and phantoms are possible
            NewOdbcIsolationLevel := SQL_TXN_READ_COMMITTED;
          xilDIRTYREAD:
            // Dirty reads, nonrepeatable reads, and phantoms are possible.
            NewOdbcIsolationLevel := SQL_TXN_READ_UNCOMMITTED;
          xilCUSTOM:
            // Custom Level
            NewOdbcIsolationLevel := aTranID.CustomIsolation;
        else
          raise
            EDbxInvalidCall.Create('TSqlConnectionOdbc.beginTransaction(TranID)' +
              ' invalid isolation value: ' + IntToStr(Ord(aTranId.IsolationLevel)));
        end;
      end
      else
        NewOdbcIsolationLevel := fOdbcIsolationLevel;
    end;

    if (fStatementPerConnection = 0) then
    begin
      aHCon := fhCon;
    end
    else
    begin
      aDbxConStmtInfo.fDbxConStmt := nil;
      aDbxConStmtInfo.fDbxHStmtNode := nil;
      if fCursorPreserved then
        // connection can containing cursors
        i := fStatementPerConnection-1
      else
        // connection cannot contain cursors
        i := 0;
      FindFreeConnection(aDbxConStmtInfo, i, {MetadataRead=}False, {bOnlyPreservedCursors=}True);

      if (aDbxConStmtInfo.fDbxConStmt = nil)
        or (aDbxConStmtInfo.fDbxConStmt.fHCon = SQL_NULL_HANDLE)
        or (fCurrDbxConStmt = nil)
      then
        CloneOdbcConnection(aDbxConStmtInfo);
      fCurrDbxConStmt := aDbxConStmtInfo.fDbxConStmt;
      aHCon := fCurrDbxConStmt.fHCon;
    end;

    if fSupportsTransaction then
    begin

      if fInTransaction = 0 then
      begin
        OdbcRetCode := SQLGetConnectAttr(aHCon, SQL_ATTR_TXN_ISOLATION, @AttrVal, 0, nil);
        if OdbcRetCode in [OdbcApi.SQL_SUCCESS, OdbcApi.SQL_SUCCESS_WITH_INFO ] then
        begin
          if AttrVal <> fOdbcIsolationLevel then
            fOdbcIsolationLevel := AttrVal;
        end;

        if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
        begin
          // clear last error:
          fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, aHCon, @fCurrDbxConStmt,
            Self, nil, nil, {clear last error count=}1);
          //fOwnerDbxDriver.OdbcCheck(OdbcRetCode, 'beginTransaction - SQLGetConnectAttr(SQL_ATTR_TXN_ISOLATION)',
          //  SQL_HANDLE_DBC, aHCon);
        end;

        if (fOdbcIsolationLevel <> NewOdbcIsolationLevel) then
        begin
          OdbcRetcode := SQLSetConnectAttr(aHCon, SQL_ATTR_TXN_ISOLATION,
            Pointer(NewOdbcIsolationLevel), 0);
          if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
            fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLSetConnectAttr(SQL_ATTR_TXN_ISOLATION)',
              SQL_HANDLE_DBC, aHCon, @fCurrDbxConStmt);
          fOdbcIsolationLevel := NewOdbcIsolationLevel;
        end;

        AttrVal := SQL_AUTOCOMMIT_OFF;
        OdbcRetCode := SQLGetConnectAttr(aHCon, SQL_ATTR_AUTOCOMMIT, @AttrVal, 0, nil);
        if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
          fOwnerDbxDriver.OdbcCheck(OdbcRetCode, 'beginTransaction - SQLGetConnectAttr(SQL_ATTR_AUTOCOMMIT)',
            SQL_HANDLE_DBC, aHCon, fCurrDbxConStmt);
      end
      else
        AttrVal := SQL_AUTOCOMMIT_ON;

      if (AttrVal = SQL_AUTOCOMMIT_ON) then
      begin
        OdbcRetcode := SQLSetConnectAttr(aHCon, SQL_ATTR_AUTOCOMMIT,
          Pointer(Smallint(SQL_AUTOCOMMIT_OFF)), 0);
        if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
          fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLSetConnectAttr(SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF)',
            SQL_HANDLE_DBC, aHCon, fCurrDbxConStmt);
        fAutoCommitMode := SQL_AUTOCOMMIT_OFF;
      end;
    end;//of: if fSupportsTransaction

    if fInTransaction = 0 then
      fRowsAffected := 0;
    inc(fInTransaction);
    if fStatementPerConnection > 0  then
    begin
      if fCurrDbxConStmt.fInTransaction = 0 then
      begin
        fCurrDbxConStmt.fAutoCommitMode := fAutoCommitMode;
        fCurrDbxConStmt.fRowsAffected := 0;
        fCurrDbxConStmt.fOutOfDateCon := False;

        for i := fDbxConStmtList.Count - 1 downto 0 do
        begin
          iDbxConStmt := fDbxConStmtList[i];
          if (iDbxConStmt = nil) or (iDbxConStmt = fCurrDbxConStmt) then
            continue;
          iDbxConStmt.fOutOfDateCon := True;  // SQLServer hung old connections.
        end;
      end;
      inc(fCurrDbxConStmt.fInTransaction);
    end;

  except
    on E: Exception{EDbxError} do
    begin
      CheckMaxLines(fConnectionErrorLines);
      fConnectionErrorLines.Add(E.Message);
      // Next line unneccessary - connection string already added in OdbcCheck
      //    fConnectionErrorLines.Add('Connection string: ' + fOdbcConnectStringHidePassword);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.beginTransaction', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.beginTransaction', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc.commit;//(TranID: Longword): SQLResult;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  iDbxConStmt: PDbxConStmt;
  i, iNullConn, iConn0SqlHStmt: Integer;
  // ---
  procedure CompactNullConn;
  begin
    if iNullConn <= cMaxCacheNullConnectionCount then
      inc(iNullConn)
    else
    begin
      fDbxConStmtList.Delete(i);
      DisposeDbxConStmt(iDbxConStmt);
    end;
  end;
  // ---
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc.commit', ['TranID =', TranID]); {$ENDIF _TRACE_CALLS_}
  {$IFDEF _DBXCB_}
  if Assigned(fDbxTraceCallbackEven) then
    DbxCallBackSendMsgFmt(cTDBXTraceFlags_Transact, 'transact (commit): conn: "$%x":', [Integer(Self)]);
  {$ENDIF}
  with fOwnerDbxDriver.fOdbcApi do
  try
    if fStatementPerConnection = 0  then
    begin
      if fSupportsTransaction then
      begin
        OdbcRetcode := SQLEndTran(SQL_HANDLE_DBC, fhCon, SQL_COMMIT);
        if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
          fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLEndTran(SQL_COMMIT)',
            SQL_HANDLE_DBC, fhCon, nil);
      end;
    end
    else
    begin
      iNullConn := 0;
      iConn0SqlHStmt := 0;
      for i := fDbxConStmtList.Count - 1 downto 0 do
      begin

        iDbxConStmt := fDbxConStmtList[i];

        if (iDbxConStmt = nil) or iDbxConStmt.fDeadConnection then
          continue;

        // compact SQL_NULL_HANDLE
        {begin:}
          if (iDbxConStmt.fHCon = SQL_NULL_HANDLE) then
          begin
            if i > 0 then
              CompactNullConn();
            continue;
          end;
        {end.}

        if iDbxConStmt.fInTransaction > 0 then
        begin
          if fSupportsTransaction then
          begin
            OdbcRetcode := SQLEndTran(SQL_HANDLE_DBC, iDbxConStmt.fHCon, SQL_COMMIT);
            if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
              fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLEndTran(SQL_COMMIT)',
                SQL_HANDLE_DBC, iDbxConStmt.fHCon, @iDbxConStmt);
          end;
          if iDbxConStmt.fInTransaction > 0 then
            dec(iDbxConStmt.fInTransaction);
          if iDbxConStmt.fInTransaction = 0 then
          begin
            iDbxConStmt.fOutOfDateCon := False;
            iDbxConStmt.fRowsAffected := 0;
          end;
        end;

        // compact empty connection
        {begin:}
          if iDbxConStmt.fSqlHStmtAllocated = 0 then
          begin
            if iConn0SqlHStmt <= cMaxCacheConnectionCount then
              inc(iConn0SqlHStmt)
            else
            if i > 0 then // first connection is locked
            begin
              if fCurrDbxConStmt = iDbxConStmt then
                fCurrDbxConStmt := nil;
              SQLDisconnect(iDbxConStmt.fHCon);
              iDbxConStmt.fHCon := SQL_NULL_HANDLE;
              iDbxConStmt.fDeadConnection := False;
              dec(fDbxConStmtActive);
              iDbxConStmt.fRowsAffected := 0;
              CompactNullConn();
            end;
          end;
        {end.}

      end;//of: for i
    end;
    if fInTransaction > 0 then
      dec(fInTransaction);
    if fInTransaction = 0 then
      fRowsAffected := 0;
  except
    on E: Exception{EDbxError} do
    begin
      CheckMaxLines(fConnectionErrorLines);
      fConnectionErrorLines.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.commit', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.commit', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc.rollback;//(TranID: Longword): SQLResult;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  iDbxConStmt: PDbxConStmt;
  i, iNullConn, iConn0SqlHStmt: Integer;
  // ---
  procedure CompactNullConn;
  begin
    if iNullConn <= cMaxCacheNullConnectionCount then
      inc(iNullConn)
    else
    begin
      fDbxConStmtList.Delete(i);
      DisposeDbxConStmt(iDbxConStmt);
    end;
  end;
  // ---
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc.rollback', ['TranID =', TranID]); {$ENDIF _TRACE_CALLS_}
  {$IFDEF _DBXCB_}
  if Assigned(fDbxTraceCallbackEven) then
    DbxCallBackSendMsgFmt(cTDBXTraceFlags_Transact, 'transact (rollback): conn: "$%x":', [Integer(Self)]);
  {$ENDIF}
  with fOwnerDbxDriver.fOdbcApi do
  try
    if fStatementPerConnection = 0  then
    begin
      if fSupportsTransaction then
      begin
        OdbcRetcode := SQLEndTran(SQL_HANDLE_DBC, fhCon, SQL_ROLLBACK);
        if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
          fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLEndTran(SQL_ROLLBACK)',
            SQL_HANDLE_DBC, fhCon, nil);
      end;
    end
    else
    begin
      iNullConn := 0;
      iConn0SqlHStmt := 0;
      for i := fDbxConStmtList.Count - 1 downto 0 do
      begin

        iDbxConStmt := fDbxConStmtList[i];

        if (iDbxConStmt = nil) or iDbxConStmt.fDeadConnection then
          continue;

        // compact SQL_NULL_HANDLE
        {begin:}
          if (iDbxConStmt.fHCon = SQL_NULL_HANDLE) then
          begin
            if i > 0 then
              CompactNullConn();
            continue;
          end;
        {end.}

        if iDbxConStmt.fInTransaction > 0 then
        begin
          if fSupportsTransaction then
          begin
            OdbcRetcode := SQLEndTran(SQL_HANDLE_DBC, iDbxConStmt.fHCon, SQL_ROLLBACK);
            if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
              fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLEndTran(SQL_ROLLBACK)',
                SQL_HANDLE_DBC, iDbxConStmt.fHCon, @iDbxConStmt);
          end;
          if iDbxConStmt.fInTransaction > 0 then
            dec(iDbxConStmt.fInTransaction);
          if iDbxConStmt.fInTransaction = 0 then
          begin
            iDbxConStmt.fOutOfDateCon := False;
            iDbxConStmt.fRowsAffected := 0;
          end;
        end;

        // compact empty connection
        {begin:}
          if iDbxConStmt.fSqlHStmtAllocated = 0 then
          begin
            if iConn0SqlHStmt <= cMaxCacheConnectionCount then
              inc(iConn0SqlHStmt)
            else
            if i > 0 then
            begin
              if fCurrDbxConStmt = iDbxConStmt then
                fCurrDbxConStmt := nil;
              SQLDisconnect(iDbxConStmt.fHCon);
              iDbxConStmt.fHCon := SQL_NULL_HANDLE;
              iDbxConStmt.fDeadConnection := False;
              dec(fDbxConStmtActive);
              iDbxConStmt.fRowsAffected := 0;
              CompactNullConn();
            end;
          end;
        {end.}

      end;//of: for i
    end;
    if fInTransaction > 0 then
      dec(fInTransaction);
    if fInTransaction = 0 then
      fRowsAffected := 0;
  except
    on E: Exception{EDbxError} do
    begin
      CheckMaxLines(fConnectionErrorLines);
      fConnectionErrorLines.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.rollback', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.rollback', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlConnectionOdbc.ClearConnectionOptions;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc.ClearConnectionOptions'); {$ENDIF _TRACE_CALLS_}
  // default connection fields:
  fSafeMode := True;
  fWantQuotedTableName := True;
  fConnBlobSizeLimitK := fOwnerDbxDriver.fDrvBlobSizeLimitK;
  // disconnect:
  fConnectionClosed := True;
  fOdbcReturnedConnectString := '';
  fOdbcDriverName := '';
  fOdbcDriverType := eOdbcDriverTypeUnspecified;
  fCurrentCatalog := '';
  fDbxCatalog := '';
  fOdbcCatalogPrefix := '';
  fSupportsTransaction := True;
  fSupportsNestedTransactions := False;
  fSupportsTransactionMetadata := False;
  // default extended fields:
  FillChar(fConnectionOptions, Length(fConnectionOptions), osDefault);
  fBlobChunkSize := cBlobChunkSizeDefault;
  fNetwrkPacketSize := cNetwrkPacketSizeDefault;
  fOdbcDriverLevel := 3;
  fSupportsBlockRead := True;
  fDbmsName := '';
  fConnConnectionString := '';
  fCursorPreserved := False;
  fCurrDbxConStmt := nil;
  fDbxConStmtActive := 0;
  fCon0SqlHStmt := 0;
  fSqlHStmtAllocated := 0;
  fStatementPerConnection := 0;
  fRowsAffected := 0;
  fOdbcIsolationLevel := 0;
  fActiveCursors := 0;
  fBindMapDateTimeOdbc := nil;
  fConnectionTimeout := cConnectionTimeoutDefault;
  fNetworkTimeout := coNetTimeoutDefault;
  fLockMode := cLockModeDefault;
  fDecimalSeparator := cDecimalSeparatorDefault;
  fPrepareSQL := True;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.ClearConnectionOptions', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.ClearConnectionOptions'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc.connect;//(
//  ServerName: PAnsiChar;
//  UserName: PAnsiChar;
//  Password: PAnsiChar
//  ): SQLResult;

{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
// ServerName is either the ODBC DSN name (must be already set up in ODBC admin)
// or it is the complete ODBC connect string (to allow complete flexibility)
// In this case, the UserName and Password are passed only if they are not blank
// (Because you might want to specifiy the UID and PWD in the FileDsn, for example)
//
// Example using DSN name
//   ServerName: 'ODBCDSN'
//   UserName:   'USER'
//   Password:   'SECRET'
//
// Examples using ODBC connect string:
//   ServerName: 'DSN=Example;UID=USER;PWD=SECRET'
//   ServerName: 'DSN=Example;DB=MyDB;HOSTNAME=MyHost;Timeout=10;UID=USER;PWD=SECRET''
//   ServerName: 'FILEDSN=FileDsnExample'
//   ServerName: 'DRIVER=Microsoft Access Driver (*.mdb);DBQ=c:\work\odbctest\odbctest.mdb'
//
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
  // ---
  function HidePassword(const ConnectString: AnsiString; bAddDelim: Boolean = False): AnsiString;
  var
    sTempl: AnsiString;
  begin
    sTempl := '***';
    if bAddDelim then
      sTempl := sTempl + ';';
    Result := ConnectString;
    if GetOptionValue(Result, 'PWD',
      {HideOption=}True,{TrimResult=}False, {bOneChar=}False, {HideTemplate=}sTempl) = cNullAnsiChar
    then
      GetOptionValue(Result, 'PASSWORD',
      {HideOption=}True,{TrimResult=}False, {bOneChar=}False, {HideTemplate=}sTempl);
  end;
  // ---
var
  OdbcRetcode: OdbcApi.SqlReturn;
  {$IFDEF MSWINDOWS}
  ParentWindowHandle: HWND;
  {$ELSE}
  ParentWindowHandle: Integer;
  {$ENDIF}
  cbConnStrOut: SqlSmallint;
  FunctionSupported: SqlUSmallint;
  aBuffer: array[0..1] of AnsiChar;
  StringLength: SqlSmallint;
  tmpS: AnsiString;
  // Cache ConnectionOptions from Database property in following variables:
  i: TConnectionOption;
  ConnectionOptionsValues: TConnectionOptionsNames;
  pConnectionOptionsDefault: PConnectionOptions;

  sUserName: AnsiString;
  sPassword: AnsiString;

  Len: Smallint;
  aOdbcSchemaUsage: SqlUInteger;
  aOdbcCatalogUsage: SqlUInteger;
  aOdbcGetDataExtensions: SqlUInteger;
  aDbxConStmtInfo: PDbxConStmt;

  bIsConnConnectionString: Boolean;
  bCursorPreservedOnCommit: Boolean;
  bCursorPreservedOnRollback: Boolean;

 {$IFDEF _MULTIROWS_FETCH_}
  aHStmt: SqlHStmt;
 {$ENDIF IFDEF _MIXED_FETCH_}

  {$IFDEF _RegExprParser_}
  vObjectNameTemplateInfo: PObjectNameTemplateInfo;
  {$ENDIF}

  // ---
  procedure MergeAndSetConnOptions; // initiate connection boolean options
  var
    i: TConnectionOption;
  begin
    for i := Low(TConnectionOption) to High(TConnectionOption) do
    begin
      if (cConnectionOptionsTypes[i] <> cot_Bool) then
        continue;
      if (fConnectionOptions[i] = osDefault) and
         // Cannot be changed to value other from in driver option. It automatically checked
         // in method IsRestrictedConnectionOptionValue(cor_driver_off):
        ( not IsRestrictedConnectionOptionValue(i, fConnectionOptions[i],
          @fConnectionOptionsDrv, Self) )
      then
      begin
        // when custom option is undefined then fill it from driver options
        if (not IsRestrictedConnectionOptionValue(i, fConnectionOptions[i], @fConnectionOptionsDrv,
          Self) )
        then
         fConnectionOptions[i] := fConnectionOptionsDrv[i];
      end;
      if fConnectionOptions[i] = osDefault then
      // when options is undefined then set is from common default options
        fConnectionOptions[i] := pConnectionOptionsDefault[i];
    end;
  end;
  // ---
  procedure ParseConnectionOptions;
  var
    iConnectionOption: TConnectionOption;
  begin
    {Parse custom options (Parse ConnectionOptions in Database property string)}
    for iConnectionOption := Low(TConnectionOption) to High(TConnectionOption) do
    begin
      if cConnectionOptionsNames[iConnectionOption] = '' then
      begin
        ConnectionOptionsValues[iConnectionOption] := cNullAnsiChar;
        continue;
      end;
      ConnectionOptionsValues[iConnectionOption] := GetOptionValue(fOdbcConnectString,
        cConnectionOptionsNames[iConnectionOption], {HideOption=}True, {TrimResult=}True, {bOneChar=}False);
      if (ConnectionOptionsValues[iConnectionOption] <> cNullAnsiChar) and ( not SetConnectionOption(fConnectionOptions,
        nil, iConnectionOption, ConnectionOptionsValues[iConnectionOption], Self))
      then
        ConnectionOptionsValues[iConnectionOption] := cNullAnsiChar;
    end;
  end;//of: procedure ParseConnectionOptions;
  // ---
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc.connect', ['ConnectString =', fConnConnectionString, 'ServerName=', StrPas(ServerName), 'UserName =', StrPas(UserName){, 'Password =', StrPas(Password)}]); {$ENDIF _TRACE_CALLS_}
  {$IFDEF _DBXCB_}
  if Assigned(fDbxTraceCallbackEven) then
    DbxCallBackSendMsgFmt(cTDBXTraceFlags_Connect, 'connect: self "$%x" , user "%s", server "%s"', [Integer(Self), StrPas(UserName), StrPas(ServerName)]);
  {$ENDIF}
  if fConnected then
    Exit;

  pConnectionOptionsDefault := GetDefaultConnectionOptions();

  with fOwnerDbxDriver.fOdbcApi do
  begin

  aDbxConStmtInfo := nil;
  fStatementPerConnection := 0;
  bIsConnConnectionString := Length(fConnConnectionString) > 0; // The parameter
  // of connection "ConnectionString" is entered in Delphi8. This parameter is more correct than
  // define odbc connection string in parameter Database.

  for i := Low(TConnectionOption) to High(TConnectionOption) do
    ConnectionOptionsValues[i] := '';

  try
    // Read ConnectionString User Options. Need hide this option for next analyses.

    if bIsConnConnectionString then
      fOdbcConnectString := fConnConnectionString
    else
      fOdbcConnectString := StrPas(ServerName);

    ParseConnectionOptions(); // remove "connection options" from "connection string":

    // '=' in connect string: it is a Custom ODBC Connect String
    if bIsConnConnectionString then
    begin
      // Check for DSN Name Only:
      if not ( (Length(fOdbcConnectString) > 0) and (fOdbcConnectString[1] = '?') ) then
      begin
        fDbxCatalog := Trim(StrPas(ServerName));
        if fDbxCatalog = '?' then
          fDbxCatalog := '';
        if Length(fDbxCatalog) > 0 then
        begin // replace fDbxCatalog to DATABASE Option
          // The user can specify a name of parameter of a database completely.
          // It is necessary when the name of parameter is distinct from 'DATABASE'
          // For example for MSAccess:
          // DBQ=C:\mydatabase.mdb
          if fOdbcCatalogPrefix <> '' then // when fOdbcCatalogPrefix is not set manually
            cbConnStrOut := AnsiPos(AnsiString('='),  fDbxCatalog)
          else
            cbConnStrOut := 0;
          if  cbConnStrOut > 1 then
          begin
            fOdbcCatalogPrefix := UpperCase(Trim(Copy(fDbxCatalog, 1, cbConnStrOut-1)));
            Delete(fDbxCatalog, 1, cbConnStrOut);
          end;
          {begin:}// replace catalog option in connection string:
          if fOdbcCatalogPrefix <> '' then
          begin
            tmpS := GetOptionValue(fOdbcConnectString, fOdbcCatalogPrefix);
            if tmpS <> cNullAnsiChar then // if exist it option:
            begin
              // replace
              if CompareText(fDbxCatalog, tmpS) <> 0 then
                GetOptionValue(fOdbcConnectString, fOdbcCatalogPrefix, {HideOption=}True,
                  {TrimResult=}False, {bOneChar=}False, {HideTemplate=}fDbxCatalog);
            end
            else
              fOdbcConnectString := fOdbcCatalogPrefix + '=' + fDbxCatalog + ';' +
                fOdbcConnectString;
          end
          else
          begin
            tmpS := GetOptionValue(fOdbcConnectString, 'DATABASE');
            if tmpS <> cNullAnsiChar then // if exist it option:
            begin
              // replace
              fOdbcCatalogPrefix := 'DATABASE';
              if CompareText(fDbxCatalog, tmpS) <> 0 then
                GetOptionValue(fOdbcConnectString, fOdbcCatalogPrefix, {HideOption=}True,
                  {TrimResult=}False, {bOneChar=}False, {HideTemplate=}fDbxCatalog);
            end
            else
            begin
              tmpS := GetOptionValue(fOdbcConnectString, 'DB');
              if tmpS <> cNullAnsiChar then // if exist it option:
              begin
                // replace
                fOdbcCatalogPrefix := 'DB';
                if CompareText(fDbxCatalog, tmpS) <> 0 then
                  GetOptionValue(fOdbcConnectString, fOdbcCatalogPrefix, {HideOption=}True,
                    {TrimResult=}False, {bOneChar=}False, {HideTemplate=}fDbxCatalog);
              end
              else
              begin
                tmpS := GetOptionValue(fOdbcConnectString, 'DBQ'); // MSJet: Access, Excel.
                if tmpS <> cNullAnsiChar then // if exist it option:
                begin
                  // replace
                  fOdbcCatalogPrefix := 'DBQ';
                  if CompareText(fDbxCatalog, tmpS) <> 0 then
                    GetOptionValue(fOdbcConnectString, fOdbcCatalogPrefix, {HideOption=}True,
                      {TrimResult=}False, {bOneChar=}False, {HideTemplate=}fDbxCatalog);
                end
                else
                begin
                  tmpS := GetOptionValue(fOdbcConnectString, 'DBNAME'); // IBPhoenix: Interbase.
                  if tmpS <> cNullAnsiChar then // if exist it option:
                  begin
                    // replace
                    fOdbcCatalogPrefix := 'DBNAME';
                    if CompareText(fDbxCatalog, tmpS) <> 0 then
                      GetOptionValue(fOdbcConnectString, fOdbcCatalogPrefix, {HideOption=}True,
                        {TrimResult=}False, {bOneChar=}False, {HideTemplate=}fDbxCatalog);
                  end
                  else
                  begin
                    tmpS := GetOptionValue(fOdbcConnectString, 'DefaultDir'); // MSJet: dBase, Paradox, FoxPro, CSV.
                    if tmpS <> cNullAnsiChar then // if exist it option:
                    begin
                      // replace
                      fOdbcCatalogPrefix := 'DefaultDir';
                      if CompareText(fDbxCatalog, tmpS) <> 0 then
                        GetOptionValue(fOdbcConnectString, fOdbcCatalogPrefix, {HideOption=}True,
                          {TrimResult=}False, {bOneChar=}False, {HideTemplate=}fDbxCatalog);
                    end
                    else
                    begin
                      tmpS := GetOptionValue(fOdbcConnectString, fOdbcCatalogPrefix);
                      if tmpS <> cNullAnsiChar then // if exist it option:
                      begin
                        // replace
                        if CompareText(fDbxCatalog, tmpS) <> 0 then
                          GetOptionValue(fOdbcConnectString, fOdbcCatalogPrefix, {HideOption=}True,
                            {TrimResult=}False, {bOneChar=}False, {HideTemplate=}fDbxCatalog);
                      end
                      else
                      begin
                        fOdbcConnectString := fOdbcCatalogPrefix + '=' + fDbxCatalog + ';' +
                          fOdbcConnectString;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
          {end.}// of: replace catalog option in connection string.
        end;//of: if Length(fDbxCatalog) > 0
      end;
    end
    else
    begin
      // SqlExpr calls SetCatalog for the server name after connect,
      // so save server name to enable check for this case and bypass the call
      if fOdbcCatalogPrefix <> '' then
        fDbxCatalog := GetOptionValue(fOdbcConnectString, fOdbcCatalogPrefix)
      else if fDbxCatalog = cNullAnsiChar then
      begin
        fDbxCatalog := GetOptionValue(fOdbcConnectString, 'DATABASE');
        if fDbxCatalog <> cNullAnsiChar then
          fOdbcCatalogPrefix := 'DATABASE'
        else
        begin
          fDbxCatalog := GetOptionValue(fOdbcConnectString, 'DB');
          if fDbxCatalog <> cNullAnsiChar then
            fOdbcCatalogPrefix := 'DB'
          else
          begin
            fDbxCatalog := GetOptionValue(fOdbcConnectString, 'DBQ'); // MSJet: Access, Excel. Oterro RBase.
            if fDbxCatalog <> cNullAnsiChar then
              fOdbcCatalogPrefix := 'DBQ'
            else
            begin
              fDbxCatalog := GetOptionValue(fOdbcConnectString, 'DBNAME'); // IBPhoenix: Interbase.
              if fDbxCatalog <> cNullAnsiChar then
                fOdbcCatalogPrefix := 'DBNAME'
              else
              begin
                fDbxCatalog := GetOptionValue(fOdbcConnectString, 'DefaultDir'); // MSJet: dBase, Paradox, FoxPro, CSV.
                if fDbxCatalog <> cNullAnsiChar then
                  fOdbcCatalogPrefix := 'DefaultDir'
                else
                  fDbxCatalog := '';
              end;
            end;
          end;
        end;
      end;
    end;

    {$IFDEF MSWINDOWS}
    { fix ms odbc: remove last splash. When is last splash driver not returned table list.}
    if {bIsConnConnectionString and} (fOdbcCatalogPrefix = '') then
    begin
      tmpS := GetOptionValue(fOdbcConnectString, 'DRIVER', False);
      if (tmpS <> cNullAnsiChar ) and (Pos('microsoft', LowerCase(string(tmpS))) > 0) then
      begin
        tmpS := GetOptionValue(fOdbcConnectString, 'DefaultDir'); // MSJet: dBase, Paradox, FoxPro, CSV.
        if tmpS <> cNullAnsiChar then
        begin
          fOdbcCatalogPrefix := 'DefaultDir';
          fDbxCatalog := tmpS;
        end
      end;
    end;

    if fOdbcCatalogPrefix = 'DefaultDir' then
    begin { remove last splash }
      tmpS := GetOptionValue(fOdbcConnectString, fOdbcCatalogPrefix, False);
      if (tmpS <> cNullAnsiChar ) and (tmpS <> '') and (tmpS[Length(tmpS)] = '\') then
      begin
        if Sametext(fDbxCatalog, tmpS) then
        begin
          SetLength(tmpS, Length(tmpS)-1);
          fDbxCatalog := tmpS;
        end
        else
          SetLength(tmpS, Length(tmpS)-1);
        // replace
        GetOptionValue(fOdbcConnectString, fOdbcCatalogPrefix, {HideOption=}True,
          {TrimResult=}False, {bOneChar=}False, {HideTemplate=}tmpS);
      end;
    end;
    {$ENDIF}

    if fDbxCatalog = cNullAnsiChar then
      fDbxCatalog := '';

    sUserName := StrPas(UserName);
    sPassword := StrPas(Password);

    if AnsiPos(AnsiChar('='), fOdbcConnectString) = 0 then
    begin
      // No '=' in connect string: it is a normal Connect String
      if not ((Length(fOdbcConnectString) = 0) or (fOdbcConnectString[1] = '?')) then
        fOdbcConnectString := 'DSN=' + fOdbcConnectString;
      if (sUserName <> '') then
        fOdbcConnectString := fOdbcConnectString + ';UID=' + UserName;
      fOdbcConnectStringHidePassword := fOdbcConnectString;
      if (sUserName <> '') then
      begin
        fOdbcConnectString := fOdbcConnectString + ';PWD=' + sPassword;
        fOdbcConnectStringHidePassword := fOdbcConnectStringHidePassword + ';PWD=***';
      end;
    end
    else
    if (sUserName <> '') or (sPassword <> '') then
    begin

      // Check to see if User Id already specified in connect string -
      // If not already specified in connect string,
      // we use UserName passed in the Connect function call (if non-blank)
      if (sUserName <> '') then
      begin
        // replace user name option
        tmpS :=GetOptionValue(fOdbcConnectString, 'UID',
          {HideOption=}True,{TrimResult=}False, {bOneChar=}False, {HideTemplate=}sUserName);
        if tmpS = cNullAnsiChar then
          tmpS :=GetOptionValue(fOdbcConnectString, 'USERID',
             {HideOption=}True,{TrimResult=}False, {bOneChar=}False, {HideTemplate=}sUserName);
        if tmpS = cNullAnsiChar then
          fOdbcConnectString := fOdbcConnectString + ';UID=' + sUserName;
      end
      else
      begin
        sUserName := GetOptionValue(fOdbcConnectString, 'UID');
        if sUserName = cNullAnsiChar then
          sUserName := GetOptionValue(fOdbcConnectString, 'USERID');
        if sUserName = cNullAnsiChar then
          sUserName := '';
      end;

      // Check to see if Password already specified in connect string -
      // If not already specified in connect string,
      // we use Password passed in the Connect function call (if non-blank)
      if (sUserName<>'') then
      begin
        // PWD it is desirable to specify to the last in a line of connection. It is connected
        // with restriction of a line of connection on syntax. Sometimes (IB XTG) in a line of
        // connection presence is required at the end of a line a symbol of a separator ";".
        // On this case the pattern for PWD is entered?: " %; ". The symbol of "%" will be
        // replaced on PWD, and ";" it will be kept in a line of connection.
        tmpS := GetOptionValue(fOdbcConnectString, 'PWD',
          {HideOption=}True,{TrimResult=}False, {bOneChar=}False, {HideTemplate=}sPassword);
        if tmpS = '%;' then // template: last symbol in (connection string/password) must equal ";"
          GetOptionValue(fOdbcConnectString, 'PWD',
            {HideOption=}True,{TrimResult=}False, {bOneChar=}False, {HideTemplate=}sPassword+';')
        else
        if tmpS = cNullAnsiChar then
        begin
          tmpS := GetOptionValue(fOdbcConnectString, 'PASSWORD',
            {HideOption=}True,{TrimResult=}False, {bOneChar=}False, {HideTemplate=}sPassword);
          if tmpS = '%;' then // template: last symbol in (connection string/password) must equal ";"
            GetOptionValue(fOdbcConnectString, 'PASSWORD',
              {HideOption=}True,{TrimResult=}False, {bOneChar=}False, {HideTemplate=}sPassword+';')
        end;
        if tmpS = cNullAnsiChar then
          fOdbcConnectString := fOdbcConnectString + ';PWD=' + sPassword;
      end;
      fOdbcConnectStringHidePassword := HidePassword(fOdbcConnectString, tmpS = '%;');
    end
    else
      fOdbcConnectStringHidePassword := HidePassword(fOdbcConnectString);

    {$IFDEF _DBXCB_}
    if Assigned(fDbxTraceCallbackEven) then
      DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'connect (prepare): conn: "$%x": "%s"', [Integer(Self), fOdbcConnectStringHidePassword]);
    {$ENDIF}

    if fOdbcCatalogPrefix <> '' then
      fDbxCatalog := GetOptionValue(fOdbcConnectString, fOdbcCatalogPrefix);

{$IFDEF MSWINDOWS}
    {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
    {+2.01}
    //Vadim> ???Vad>Ed/All: If process is not NT service (need checked)
    //Edward> When doing SQLDriverConnect, the Driver manager and/or Driver may display a
    //Edward> dialog box to prompt user for additional connect parameters.
    //Edward> So SQLDriverConnect has a Window Handle Parameter to use as the parent.
    //Edward> In Windows I pass the Active Window handle for this parameter,
    //Edward> but in Kylix, I do not know the equivalent call, so I just pass 0.
    {/+2.01}
    {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
    ParentWindowHandle := Windows.GetActiveWindow;
{$ELSE}
    ParentWindowHandle := 0;
{$ENDIF}

    if fConnectionOptions[coReadOnly] = osOn then
    begin
      OdbcRetcode := SQLSetConnectAttr(fhCon, SQL_ATTR_ACCESS_MODE, SqlPointer(SQL_MODE_READ_ONLY), 0);
      // clear last error:
      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
    end;

    // SQL_ATTR_LOGIN_TIMEOUT
    if (fConnectionTimeout >= 0) {and (fConnectionTimeout <> cConnectionTimeoutDefault)} then
      SetLoginTimeout(fhCon, fConnectionTimeout);
    // SQL_ATTR_CONNECTION_TIMEOUT
    if fNetworkTimeout >= 0 then
      SetNetworkTimeout(fhCon, fNetworkTimeout);

    fOdbcReturnedConnectString := fOdbcConnectString;
    tmpS := GetOptionValue(fOdbcReturnedConnectString, 'DSN', {HideOption=}True, {TrimResult=}True);
    if ( ((tmpS = '?') or (tmpS='') ) and (Trim(fOdbcReturnedConnectString) = '' ) ) then
      fOdbcConnectString := tmpS;

    {$IFDEF _TRACE_CALLS_}
      LogInfoProc(['Odbc Connection String =', fOdbcConnectString]);
      LogInfoProc(['Vendor Library =', ModuleName]);
      LogInfoProc(['DBXVersion =', fOwnerDbxDriver.fDBXVersion]);
      LogInfoProc(['ClientVersion =', fOwnerDbxDriver.fClientVersion]);
    {$ENDIF _TRACE_CALLS_}

    SetLength(fOdbcReturnedConnectString, cOdbcReturnedConnectStringMax);
    FillChar(fOdbcReturnedConnectString[1], Length(fOdbcReturnedConnectString), 0);

    if ((Length(fOdbcConnectString) = 0) or (fOdbcConnectString[1] = '?')) then
    begin
      OdbcRetcode := SQLDriverConnect(
        fhCon,
        ParentWindowHandle,
        PAnsiChar(fOdbcConnectString), SQL_NTS,
        PAnsiChar(fOdbcReturnedConnectString), cOdbcReturnedConnectStringMax, cbConnStrOut,
        SQL_DRIVER_COMPLETE_REQUIRED); // SQL_DRIVER_COMPLETE_REQUIRED  SQL_DRIVER_NOPROMPT  SQL_DRIVER_PROMPT  SQL_DRIVER_COMPLETE
      if (OdbcRetcode = OdbcApi.SQL_NO_DATA) then
      begin
        SetLength(fOdbcReturnedConnectString, cbConnStrOut);
        fOdbcReturnedConnectString := StrPas(PAnsiChar(fOdbcReturnedConnectString));
        if (fOdbcReturnedConnectString<>'?') and (fOdbcReturnedConnectString<>'') then
        begin
          Result := {DBXpress.}DBXERR_INVALIDUSRPASS; // DBXERR_INVALIDPARAM
          {$IFDEF _TRACE_CALLS_}
          LogInfoProc('  W:  Improper login');
          {$ENDIF _TRACE_CALLS_}
        end
        else { User canceled dialog }
        begin
          {$IFDEF _TRACE_CALLS_}
          LogInfoProc('  W:  User canceled dialog');
          {$ENDIF _TRACE_CALLS_}
          CheckMaxLines(fConnectionErrorLines);
          fConnectionErrorLines.Add(rsNotSpecifiedDNSName);
          Result := DBX_FIRSTSTATICERRORS;
        end;
        ClearConnectionOptions;
        exit; // User Clicked Cancel
      end;
      if (OdbcRetcode <> OdbcApi.SQL_SUCCESS)and(OdbcRetcode <> OdbcApi.SQL_SUCCESS_WITH_INFO) then
        fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLDriverConnect (Driver Complete Required)',
          SQL_HANDLE_DBC, fhCon, nil, Self);
      SetLength(fOdbcReturnedConnectString, cbConnStrOut);
      fOdbcReturnedConnectString := StrPas(PAnsiChar(fOdbcReturnedConnectString));
      fOdbcConnectString := fOdbcReturnedConnectString;
      fConnected := True;
    end
    else
    begin
      OdbcRetcode := SQLDriverConnect(
        fhCon,
        ParentWindowHandle,
        PAnsiChar(fOdbcConnectString), SQL_NTS,
        PAnsiChar(fOdbcReturnedConnectString), cOdbcReturnedConnectStringMax, cbConnStrOut,
        SQL_DRIVER_NOPROMPT); // SQL_DRIVER_NOPROMPT  SQL_DRIVER_PROMPT  SQL_DRIVER_COMPLETE_REQUIRED  SQL_DRIVER_COMPLETE
      if (OdbcRetcode <> OdbcApi.SQL_SUCCESS)and(OdbcRetcode <> OdbcApi.SQL_SUCCESS_WITH_INFO) then
        fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLDriverConnect (NoPrompt)',
          SQL_HANDLE_DBC, fhCon, nil, Self);
      fConnected := True;
      SetLength(fOdbcReturnedConnectString, cbConnStrOut);
      fOdbcReturnedConnectString := StrPas(PAnsiChar(fOdbcReturnedConnectString));
    end;
    fOdbcConnectStringHidePassword := HidePassword(fOdbcReturnedConnectString);
    {$IFDEF _DBXCB_}
    if Assigned(fDbxTraceCallbackEven) then
      DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'connect (returned): conn: "$%x": "%s"', [Integer(Self), fOdbcConnectStringHidePassword]);
    {$ENDIF}

    {$IFDEF _TRACE_CALLS_}
    LogInfoProc('  I:  ODBC connection is done.');
    LogInfoProc(['Odbc Returned Connection String =', fOdbcReturnedConnectString]);
    {$ENDIF _TRACE_CALLS_}

    fConnectionClosed := False;

    // Calculate fCursorPreserved:
    {begin:}
      // 1) Get Cursor Behavior Type: Get Cursor Preserved On Commit:
      bCursorPreservedOnCommit := False;
      FunctionSupported := SQL_CB_CLOSE;
      OdbcRetcode := SQLGetInfoSmallInt(fhCon, SQL_CURSOR_COMMIT_BEHAVIOR, FunctionSupported, SizeOf(SQLUSMALLINT), nil );
      if OdbcRetcode = OdbcApi.SQL_SUCCESS then
      begin
        bCursorPreservedOnCommit := FunctionSupported = SQL_CB_PRESERVE;
        // clear last error:
        fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
      end;
      // 2) Get Cursor Behavior Type: Get Cursor Preserved On Rollback:
      bCursorPreservedOnRollback := False;
      if bCursorPreservedOnCommit then
      begin
        FunctionSupported := SQL_CB_CLOSE;
        OdbcRetcode := SQLGetInfoSmallInt(fhCon, SQL_CURSOR_ROLLBACK_BEHAVIOR, FunctionSupported, SizeOf(SQLUSMALLINT), nil );
        if OdbcRetcode = OdbcApi.SQL_SUCCESS then
          bCursorPreservedOnRollback := FunctionSupported = SQL_CB_PRESERVE
        else
          // clear last error:
          fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
      end;
      fCursorPreserved := ( bCursorPreservedOnCommit and bCursorPreservedOnRollback );
    {end.}

    // read default fOdbcIsolationLevel (need for cloning connecton).
    OdbcRetCode := SQLGetConnectAttr(fhCon, SQL_ATTR_TXN_ISOLATION, @fOdbcIsolationLevel, 0, nil);
    fSupportsTransaction := OdbcRetCode = OdbcApi.SQL_SUCCESS;
    if not fSupportsTransaction then
      // clear last error:
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);

    RetrieveDriverName; // some sets then fConnectionOptionsDrv
    // ***************

    // Calculate catalog in connection string:
    if fDbxCatalog = '' then
    begin
      if fOdbcCatalogPrefix <> '' then
        fDbxCatalog := GetOptionValue(fOdbcReturnedConnectString, fOdbcCatalogPrefix)
      else
      begin
        fDbxCatalog := GetOptionValue(fOdbcReturnedConnectString, 'DATABASE');
        if fDbxCatalog <> cNullAnsiChar then
          fOdbcCatalogPrefix := 'DATABASE'
        else
        begin
          fDbxCatalog := GetOptionValue(fOdbcReturnedConnectString, 'DB');
          if fDbxCatalog <> cNullAnsiChar then
            fOdbcCatalogPrefix := 'DB'
          else
          begin
            fDbxCatalog := GetOptionValue(fOdbcReturnedConnectString, 'DBQ'); // MSJet: Access, Excel.
            if fDbxCatalog <> cNullAnsiChar then
              fOdbcCatalogPrefix := 'DBQ'
            else
            begin
              fDbxCatalog := GetOptionValue(fOdbcReturnedConnectString, 'DBNAME'); // IBPhoenix: Interbase.
              if fDbxCatalog <> cNullAnsiChar then
                fOdbcCatalogPrefix := 'DBNAME'
              else
              begin
                fDbxCatalog := GetOptionValue(fOdbcReturnedConnectString, 'DefaultDir'); // MSJet: dBase, Paradox, FoxPro, CSV.
                if fDbxCatalog <> cNullAnsiChar then
                  fOdbcCatalogPrefix := 'DefaultDir'
                else
                  fDbxCatalog := '';
              end;
            end;
          end;
        end;
      end;
      if fDbxCatalog = cNullAnsiChar then
        fDbxCatalog := '';
    end;
    if fOdbcCatalogPrefix = '' then
      fOdbcCatalogPrefix := 'DATABASE';

    // init date time fields mapping rules:
    {begin:}
    if fBindMapDateTimeOdbc = nil then
    begin
      if (fOdbcDriverLevel>0) and (fOdbcDriverLevel < 3) then
        // In case of errors in SQlBindCol, value fBindMapDateTimeOdbc will be
        // changed to @cBindMapDateTimeOdbc3:
        fBindMapDateTimeOdbc := @cBindMapDateTimeOdbc2 // - oterro odbc driver
      else
        fBindMapDateTimeOdbc := @cBindMapDateTimeOdbc3;
    end;
    {end.}

    OdbcRetcode := SQLGetInfoInt(fhCon, SQL_GETDATA_EXTENSIONS, aOdbcGetDataExtensions,
      SizeOf(aOdbcGetDataExtensions), nil);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    begin
      //fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_GETDATA_EXTENSIONS',
       // SQL_HANDLE_DBC, fhCon, Self);
      aOdbcGetDataExtensions := 0;
      // clear last error:
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
    end;

    {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
    (*
    SQL_GD_ANY_COLUMN = SQLGetData can be called for any unbound column,
    including those before the last bound column.
    Note that the columns must be called in order of ascending column number
    unless SQL_GD_ANY_ORDER is also returned.

    SQL_GD_ANY_ORDER = SQLGetData can be called for unbound columns in any order.
    Note that SQLGetData can be called only for columns after the last bound column
    unless SQL_GD_ANY_COLUMN is also returned.
    *)
    {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}

    fGetDataAnyColumn := ((aOdbcGetDataExtensions and SQL_GD_ANY_COLUMN) <> 0);

    {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
    (*
    SQL_GD_BLOCK:
    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/odbc/htm/odch21epr_3.asp

    As SQLFetch returns each row, it places the data for each bound column in the buffer
    bound to that column. If no columns are bound, SQLFetch does not return any data but
    does move the block cursor forward. The data can still be retrieved with SQLGetData.
    If the cursor is a multirow cursor (that is, the SQL_ATTR_ROW_ARRAY_SIZE is greater than
     1), SQLGetData can be called only if SQL_GD_BLOCK is returned when SQLGetInfo is called
     with an InfoType of SQL_GETDATA_EXTENSIONS. (For more information, see SQLGetData.)
    *)
    {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}

    fConnectionOptions[coMixedFetch] := osOff;
    {$IFNDEF _MULTIROWS_FETCH_}
      fSupportsBlockRead := False;
      fConnectionOptionsDrv[coMixedFetch] := osOff;
    {$ENDIF}
    if not SQLFunctionSupported(fhCon, SQL_API_SQLGETSTMTATTR) then
    begin
      fSupportsBlockRead := False;
      fConnectionOptionsDrv[coMixedFetch] := osOff;
    end;

    if fSupportsBlockRead and ((aOdbcGetDataExtensions and SQL_GD_BLOCK) <> 0) then
    begin
      // The "fConnectionOptionsDrv[coMixedFetch]" must equal "odbc driver option" (fSupportsBlockRead can changed at runtime)
      {$IFDEF _MULTIROWS_FETCH_}
      AllocHStmt(aHStmt, nil);
      try
      try
        {$IFDEF _MIXED_FETCH_} // Check supported SQL_CURSOR_STATIC
        try
          if (fConnectionOptionsDrv[coMixedFetch] <> osOff) then
          begin
            fConnectionOptionsDrv[coMixedFetch] := osOff;
            fSupportsBlockRead := False;
              OdbcRetcode := SQLSetStmtAttr(aHStmt, SQL_ATTR_CURSOR_TYPE,
                SqlPointer(SQL_CURSOR_STATIC), 0);
              if OdbcRetcode = OdbcApi.SQL_SUCCESS then
                fConnectionOptionsDrv[coMixedFetch] := osOn
              else
                // clear last error:
                fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
          end;
        except
        end;
       {$ENDIF IFDEF _MIXED_FETCH_}
       fSupportsBlockRead := False;
        OdbcRetcode := SQLSetStmtAttr(aHStmt, SQL_ATTR_ROW_ARRAY_SIZE,
          SqlPointer(1), 0);
        if OdbcRetcode = OdbcApi.SQL_SUCCESS then
          fSupportsBlockRead := True
        else
          // clear last error:
          fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
      except
      end;
      finally
        FreeHStmt(aHStmt, nil);
      end;
      {$ENDIF IFDEF _MULTIROWS_FETCH_}

      // not detected and not defined in RetrieveDriverName
      if fConnectionOptionsDrv[coMixedFetch] = osDefault then
        fConnectionOptionsDrv[coMixedFetch] := osOff;

      // if driver not supported this mode then disable user defined same option
      //if fConnectionOptionsDrv[coMixedFetch] = osOff then
      //  fConnectionOptions[coMixedFetch] := osOff;
    end
    else
    begin
      fSupportsBlockRead := False;
      fConnectionOptionsDrv[coMixedFetch] := osOff;
      //fConnectionOptions[coMixedFetch] := osOff;
    end;

    //We unite of set-up of the user to customizations of the driver
    // Set-up of the user have the greater priority before customizations defined automatically
    MergeAndSetConnOptions;

    // Parsing default(current) SchemaName. It is equal logoon UserName
    tmpS := fOdbcReturnedConnectString;
    sUserName := GetOptionValue(tmpS, 'UID', True, False);
    if sUserName = cNullAnsiChar then
    begin
      sUserName := GetOptionValue(tmpS, 'USERID');
      if sUserName = cNullAnsiChar then
        sUserName := '';
    end;
    fCurrentSchema := sUserName;

    if fConnectionOptions[coSupportsCatalog] = osOn then
    begin
      OdbcRetcode := SQLGetInfoString(fhCon, SQL_CATALOG_NAME, @aBuffer,
        SizeOf(aBuffer), StringLength);
      aBuffer[0] := cNullAnsiChar;
      fSupportsCatalog := (OdbcRetcode = OdbcApi.SQL_SUCCESS) and (aBuffer[0] = 'Y');
      if not fSupportsCatalog then
        fConnectionOptions[coSupportsCatalog] := osOff;
      // clear last error:
      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
    end
    else
      fSupportsCatalog := False;

    // IBM DB2 has driver-specific longdata type, but setting this option makes it ODBC compatible:
    if Self.fOdbcDriverType = eOdbcDriverTypeIbmDb2 then
    begin
      OdbcRetcode := SQLSetConnectAttr(fhCon, SQL_IBMDB2_LONGDATA_COMPAT,
        SqlPointer(SQL_IBMDB2_LD_COMPAT_YES), 0);
      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLSetConnectAttr(SQL_IBMDB2_LONGDATA_COMPAT)',
          SQL_HANDLE_DBC, fhCon, nil, Self);
    end;

    // INFORMIX has driver-specific longdata type, but setting this option makes it ODBC compatible:
    if (Self.fOdbcDriverType = eOdbcDriverTypeInformix)
        and (StrLComp( PAnsiChar(UpperCase(fOdbcDriverName)), 'ICLI', 4) = 0) // (It is meaningful only for the native informix driver)
    then
    begin
      OdbcRetcode := SQLSetConnectAttr(fhCon, SQL_INFX_ATTR_LO_AUTOMATIC, SqlPointer(SQL_TRUE), 0);
      // clear last error:
      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
    end;

    {+2.43}
    // MSSQL SERVER: The situation with a mistake of cloning connection is corrected at connection
    // through PIPE.
    if (Self.fOdbcDriverType in [eOdbcDriverTypeMsSqlServer, eOdbcDriverTypeMsSqlServer2005Up]) then
    begin
      tmpS := fOdbcReturnedConnectString;
      tmpS := GetOptionValue(tmpS, 'NETWORK', False);
      if (tmpS<>cNullAnsiChar) and (Pos('\\.\PIPE\', UpperCase(string(tmpS)))>0) then
      begin
        // remove options "Network":
        tmpS := fOdbcReturnedConnectString;
        GetOptionValue(tmpS, 'NETWORK', True); // remove option 'NETWORK'
        fOdbcReturnedConnectString := tmpS;
        {$IFDEF _TRACE_CALLS_}
          LogInfoProc(['Odbc Returned Connection String (fixed) =', fOdbcReturnedConnectString]);
        {$ENDIF _TRACE_CALLS_}
      end;
    end;
    {/+2.43}

    OdbcRetcode := SQLGetInfoInt(fhCon, SQL_SCHEMA_USAGE, aOdbcSchemaUsage,
      SizeOf(aOdbcSchemaUsage), nil);
    if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
    begin
      aOdbcSchemaUsage := 0;
      // clear last error:
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
    end;
    fSupportsSchemaDML := ((aOdbcSchemaUsage and SQL_SU_DML_STATEMENTS) <> 0);
    fSupportsSchemaProc := ((aOdbcSchemaUsage and SQL_SU_PROCEDURE_INVOCATION) <> 0);

    OdbcRetcode := SQLGetInfoSmallint(fhCon, SQL_MAX_COLUMN_NAME_LEN, fOdbcMaxColumnNameLen,
      SizeOf(fOdbcMaxColumnNameLen), nil);
    if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
    begin
      fOdbcMaxColumnNameLen := cOdbcMaxColumnNameLenDefault;
      // clear last error:
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
    end;
    OdbcRetcode := SQLGetInfoSmallint(fhCon, SQL_MAX_TABLE_NAME_LEN, fOdbcMaxTableNameLen,
      SizeOf(fOdbcMaxTableNameLen), nil);
    if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
    begin
      fOdbcMaxTableNameLen := cOdbcMaxTableNameLenDefault;
      // clear last error:
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
    end
    else
    if fOdbcMaxTableNameLen <= 0 then
      fOdbcMaxTableNameLen := cOdbcMaxTableNameLenDefault;
    if fSupportsCatalog then
    begin
      OdbcRetcode := SQLGetInfoSmallint(fhCon, SQL_MAX_CATALOG_NAME_LEN, fOdbcMaxCatalogNameLen,
        SizeOf(fOdbcMaxCatalogNameLen), nil);
      if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
      begin
        fSupportsCatalog := False;
        // clear last error:
        fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
      end;
      if fOdbcMaxCatalogNameLen <= 0 then
        fOdbcMaxCatalogNameLen := cOdbcMaxCatalogNameLenDefault; // or: fSupportsCatalog := False;
      OdbcRetcode := SQLGetInfoInt(fhCon, SQL_CATALOG_USAGE, aOdbcCatalogUsage,
        SizeOf(aOdbcCatalogUsage), nil);
      if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
      begin
        aOdbcCatalogUsage := 0;
        // clear last error:
        fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
      end;
      fSupportsCatalogDML := ((aOdbcCatalogUsage and SQL_CU_DML_STATEMENTS) <> 0);
      fSupportsCatalogProc := ((aOdbcCatalogUsage and SQL_CU_PROCEDURE_INVOCATION) <> 0);
    end;

    OdbcRetcode := SQLGetInfoSmallint(fhCon, SQL_MAX_SCHEMA_NAME_LEN, fOdbcMaxSchemaNameLen,
      SizeOf(fOdbcMaxSchemaNameLen), nil);
    if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
    begin
      fOdbcMaxSchemaNameLen := 0;
      // clear last error:
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
    end
    else
    if fOdbcMaxSchemaNameLen <= 0 then
      fOdbcMaxSchemaNameLen := cOdbcMaxSchemaNameLenDefault;

    OdbcRetcode := SQLGetInfoSmallint(fhCon, SQL_MAX_IDENTIFIER_LEN, fOdbcMaxIdentifierLen,
      SizeOf(fOdbcMaxIdentifierLen), nil);
    if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
    begin
      fOdbcMaxIdentifierLen := cOdbcMaxIdentifierLenDefault;
      // clear last error:
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
    end
    else
    if fOdbcMaxIdentifierLen <= 0 then
      fOdbcMaxIdentifierLen := cOdbcMaxIdentifierLenDefault;

    FunctionSupported := OdbcApi.SQL_FALSE;
    OdbcRetcode := SQLGetFunctions(fhCon, SQL_API_SQLSTATISTICS, FunctionSupported);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    begin
      FunctionSupported := OdbcApi.SQL_FALSE;
      // clear last error:
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
    end;
    fSupportsSQLSTATISTICS := (FunctionSupported = OdbcApi.SQL_TRUE);
    FunctionSupported := OdbcApi.SQL_FALSE;
    OdbcRetcode := SQLGetFunctions(fhCon, SQL_API_SQLPRIMARYKEYS, FunctionSupported);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    begin
      FunctionSupported := OdbcApi.SQL_FALSE;
      // clear last error:
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
    end;
    fSupportsSQLPRIMARYKEYS := (FunctionSupported = OdbcApi.SQL_TRUE);

    GetMetaDataOption(eMetaObjectQuoteChar, @fQuoteChar, 1, Len);

    {$IFDEF _DBX30_}
    if (fOwnerDbxDriver.fDBXVersion >= 30) then
    begin
      // Auto detection of the client supporting fldWIDESTRING, fldstWIDEMEMO
      if (fConnectionOptions[coEnableUnicode] = osDefault) and (fConnectionOptionsDrv[coEnableUnicode] <> osOff) then
        fConnectionOptions[coEnableUnicode] := osOn;
    end;
    {$ENDIF}
    {
    else if (fOwnerDbxDriver.fClientVersion >= 30) then
    begin //is conflicted with ansi driver (2.5)
      // Auto detection of the client supporting fldWIDESTRING, fldstWIDEMEMO
      if (fConnectionOptions[coEnableUnicode] = osDefault) and (fConnectionOptionsDrv[coEnableUnicode] <> osOff) then
        fConnectionOptions[coEnableUnicode] := osOn;
    end;
    }

    RetrieveDbmsOptions;

{$IFDEF _RegExprParser_}
    FreeAndNil(fObjectNameParser);
    vObjectNameTemplateInfo := GetDbmsObjectNameTemplateInfo(fDbmsType);
    {$IFDEF _TRACE_CALLS_}
    LogInfoProc(['xx-ObjectNameParser =', vObjectNameTemplateInfo.sName]);
    {$ENDIF _TRACE_CALLS_}
    {$IFDEF _DBXCB_}
    if Assigned(fDbxTraceCallbackEven) then
      DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'connect (object name parser): conn: "$%x": "%s"', [Integer(Self), vObjectNameTemplateInfo.sName]);
    {$ENDIF}
    fObjectNameParser := TObjectNameParser.Create(vObjectNameTemplateInfo, StrPas(PAnsiChar(@fQuoteChar)));
  {$IFDEF _TRACE_CALLS_}
    OdbcRetcode := SQLGetInfoSmallint(fhCon, SQL_MAX_CONCURRENT_ACTIVITIES,
      fStatementPerConnection, 2, nil);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetConnectAttr(SQL_MAX_CONCURRENT_ACTIVITIES)',
        SQL_HANDLE_DBC, fhCon, nil, Self);
  {$ENDIF _TRACE_CALLS_}
{$ENDIF}

    OdbcRetcode := SQLGetConnectAttr(fhCon, SQL_ATTR_AUTOCOMMIT, @fAutoCommitMode, 0, nil);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    begin
      fSupportsTransaction := False;
      // clear last error:
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
      //fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetConnectAttr(SQL_ATTR_AUTOCOMMIT)',
      //  SQL_HANDLE_DBC, fhCon, Self);
    end
    else
      CheckTransactionSupport;

    {
    if fSupportsTransaction then
    begin
      // Any is not known odbc the driver having expansion for support of this opportunity:
      fSupportsNestedTransactions := fDbmsType = eDbmsTypeInterbase;
    end;
    {}

    if not fSupportsTransaction then
      fSupportsTransactionMetadata := False;

    // Get max no of statements per connection.
    // If necessary, we will internally clone connection for databases that
    // only support 1 statement handle per connection, such as MsSqlServer
    fStatementPerConnection := 0;
    OdbcRetcode := SQLGetInfoSmallint(fhCon, SQL_MAX_CONCURRENT_ACTIVITIES,
      fStatementPerConnection, 2, nil);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetConnectAttr(SQL_MAX_CONCURRENT_ACTIVITIES)',
        SQL_HANDLE_DBC, fhCon, nil, Self);
    {$IFDEF _debug_emulate_stmt_per_con_}
      // emulated fStatementPerConnection:
      if (fStatementPerConnection = 0) or (fStatementPerConnection > cStmtPerConnEmulate) then
      begin
        {$IFDEF _TRACE_CALLS_}
        LogInfoProc(['xx-Emulate StatementPerConnection to "' + IntToStr(cStmtPerConnEmulate) +
          '" from "', fStatementPerConnection]);
        {$ENDIF}
        fStatementPerConnection := cStmtPerConnEmulate;
      end;
    {$ENDIF}
    if (fStatementPerConnection > 0) then
    begin
      // Create the Connection + Statement cache, for databases that support
      // only 1 statement per connection
      fDbxConStmtList := TDbxConStmtList.Create;
      aDbxConStmtInfo := NewDbxConStmt();
      fDbxConStmtList.Add(aDbxConStmtInfo);
      if fStatementPerConnection < cStatementPerConnectionBlockCount then
        AllocateDbxHStmtNodes(@aDbxConStmtInfo, fStatementPerConnection)
      else
        AllocateDbxHStmtNodes(@aDbxConStmtInfo, cStatementPerConnectionBlockCount);
      aDbxConStmtInfo.fhCon := fhCon;
      aDbxConStmtInfo.fAutoCommitMode := fAutoCommitMode;
      fCurrDbxConStmt := aDbxConStmtInfo;
      fDbxConStmtActive := 1;
      fCon0SqlHStmt := 1;

      {
      // todo: The Value "SQL_CUR_USE_ODBC" should be is established before connection
      if not fCursorPreserved then
      begin
        OdbcRetcode := SQLSetConnectAttr(fhCon, SQL_ATTR_ODBC_CURSORS, SqlPointer(SQL_CUR_USE_ODBC), 0);
        // clear last error:
        if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
          fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, Self, nil, nil, 1);
      end;
      {}
    end;

    // Vadim V.Lopushansky:
    // for support cloning connection when returning database connection string
    {begin}
      for i := Low(TConnectionOption) to High(TConnectionOption) do
      begin
        if ConnectionOptionsValues[i] <> cNullAnsiChar then
          fOdbcConnectString := cConnectionOptionsNames[i] + '=' + ConnectionOptionsValues[i] + ';' +
            fOdbcConnectString;
      end;
      // Support Delphi 8 Connection Option:
        fConnConnectionString := fOdbcConnectString;
    {end}

{$IFDEF _TRACE_CALLS_}
  Len := 7;
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-SupportsCatalog =', fSupportsCatalog]);
  Inc(Len);
  if fSupportsCatalog then
  begin
    LogInfoProc([AnsiString(Format('%2d', [Len]))+'-MaxCatalogNameLen =', fOdbcMaxCatalogNameLen]);
    LogInfoProc([AnsiString(Format('%2d', [Len]))+'-SupportsCatalogDML =', fSupportsCatalogDML]);
    LogInfoProc([AnsiString(Format('%2d', [Len]))+'-SupportsCatalogProc =', fSupportsCatalogProc]);
  end;
  Inc(Len);
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-MaxSchemaNameLen =', fOdbcMaxSchemaNameLen]);
  Inc(Len);
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-MaxTableNameLen =', fOdbcMaxTableNameLen]);
  Inc(Len);
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-MaxColumnNameLen =', fOdbcMaxColumnNameLen]);
  Inc(Len);
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-QuoteChar =', '<'+StrPas(PAnsiChar(@fQuoteChar))+'>']);
  Inc(Len);
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-MaxIdentifierLen =', fOdbcMaxIdentifierLen]);
  Inc(Len);
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-SupportsSQLSTATISTICS =', fSupportsSQLSTATISTICS]);
  Inc(Len);
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-SupportsSQLPRIMARYKEYS =', fSupportsSQLPRIMARYKEYS]);
  Inc(Len);
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-GetDataAnyColumn =', fGetDataAnyColumn]);
  Inc(Len);
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-AutoCommitMode =', fAutoCommitMode]);
  Inc(Len);
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-SupportsTransaction =', fSupportsTransaction]);
  Inc(Len);
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-StatementPerConnection =', fStatementPerConnection]);
  Inc(Len);
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-cBlobChunkSize =',fBlobChunkSize]);
  Inc(Len);
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-SupportsBlockRead =', fSupportsBlockRead]);
  Inc(Len);
  if (fNetwrkPacketSize <> cNetwrkPacketSizeDefault) then
    LogInfoProc([AnsiString(Format('%2d', [Len]))+'cNetwkrPacketSize=', fNetwrkPacketSize])
  else
    LogInfoProc([AnsiString(Format('%2d', [Len]))+'cNetwkrPacketSize = "Default"']);
  Inc(Len);
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-CursorPreserved =', fCursorPreserved]);
  Inc(Len);
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-SystemODBCManager =', {fOwnerDbxDriver.fOdbcApi.}SystemODBCManager]);
  Inc(Len);
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-OdbcDriverLevel =', fOdbcDriverLevel]);
  Inc(Len);
  LogInfoProc([AnsiString(Format('%2d', [Len]))+'-OdbcCatalogPrefix =', fOdbcCatalogPrefix]);
  Inc(Len);
  for i := Low(TConnectionOption) to High(TConnectionOption) do
  begin
    LogInfoProc([AnsiString(Format('%2d', [Len+Byte(i)]))+'-'+string(cConnectionOptionsNames[i])+' =',
      cOptionSwitchesNames[fConnectionOptions[i]] ]);
  end;
{$ENDIF} // of: IFDEF _TRACE_CALLS_

    Result := DBXERR_NONE;

  except
    on E: Exception{EDbxError} do
    begin
      CheckMaxLines(fConnectionErrorLines);
      fConnectionErrorLines.Add(E.Message);
      //fConnectionErrorLines.Add('Connection string: ' + fOdbcConnectStringHidePassword);
      Result := DBX_FIRSTSTATICERRORS;
      if fConnected then
        disconnect
      else
        ClearConnectionOptions;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;

  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.connect', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.connect', ['Result =', Result, 'Connected =', fConnected]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc.RetrieveDbmsOptions: SQLResult;
var
  pBuffer: PAnsiChar;
  iComm: ISQLCommand25;
  iCursor: ISQLCursor25;
  S: AnsiString;
  pLength: Longword;
  IsBlank: LongBool;
begin
  Result := DBXERR_NONE;
  try
  try
    {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc.RetrieveDbmsOptions'); {$ENDIF _TRACE_CALLS_}
    pBuffer := PAnsiChar(fOdbcDriverName);
    case fDbmsType of
      eDbmsTypeOracle:
        begin
          if ((StrLComp(pBuffer, 'SQORA', 5) = 0)) // Oracle ODBC Driver
          { Microsoft ODBC for Oracle always demand '.' }
            {- or (StrLComp(pBuffer, 'MSORCL', 6) = 0)   // Microsoft ODBC for Oracle}
          {{???: or (StrLComp(pBuffer, 'IVORA', 5) = 0)    // DataDirect Oracle Wire Protocol ODBC Driver}
          {{???: or (StrLComp(pBuffer, 'IVOR', 4) = 0)     // DataDirect Oracle ODBC Driver}
          {{???: or (StrLComp(pBuffer, 'PBOR', 4) = 0)     // PB INTERSOLV OEM ODBC Driver}
          then
          begin
            if (getSQLCommand(iComm) = DBXERR_NONE) and (iComm <> nil) then
            begin
              S := 'SELECT (VALUE || ''.'') DECSEPR FROM V$NLS_PARAMETERS WHERE PARAMETER=''NLS_NUMERIC_CHARACTERS''';
              if (iComm.executeImmediate(PAnsiChar(S), iCursor) = DBXERR_NONE) and (iCursor <> nil) then
              begin
                pLength := 0;
                if (iCursor.getColumnLength(1, pLength) = DBXERR_NONE) and (pLength>0) and (pLength <255) then
                begin
                  IsBlank := True;
                  SetLength(S, pLength + 1);
                  S[1] := cNullAnsiChar;
                  if (iCursor.next = DBXERR_NONE) and (iCursor.getString(1, PAnsiChar(S), IsBlank) = DBXERR_NONE) and (not IsBlank) then
                  begin
                    if SetOption(
                        TSQLConnectionOption(xeConnDecimalSeparator), Integer(PAnsiChar(AnsiString(S[1])))
                      ) = DBXERR_NONE then
                    begin
                      fConnectionOptions[coNumericSeparator] := osDefault;
                    end;
                  end;
                end;
              end
            end;
          end;
        end;
    end;// of: case fDbmsType

    {$IFDEF _TRACE_CALLS_}
      except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.RetrieveDbmsOptions', e);  raise; end; end;
      finally LogExitProc('TSqlConnectionOdbc.RetrieveDbmsOptions', ['Result =', Result]); end;
    {$ENDIF _TRACE_CALLS_}
  finally
    if iCursor <> nil then
    try
      iCursor := nil;
    except
      Pointer(iCursor) := nil;
    end;
    if iComm <> nil then
    try
      iComm := nil;
    except
      Pointer(iComm) := nil;
    end;
  end;
  except
    on e:exception do
    begin
      {empty}
      //debug: E.Message := 'TSqlConnectionOdbc.RetrieveDbmsOptions: ' + E.Message;
    end;
  end;
end;

function TSqlConnectionOdbc.RetrieveDriverName: SQLResult;

var
  OdbcRetcode: OdbcApi.SqlReturn;
  sBuffer: AnsiString;
  pBuffer: PAnsiChar;
  uDbmsName: AnsiString;
  BufLen: SqlSmallint;
  {$IFDEF _DBXCB_}
  S: AnsiString;
  {$ENDIF}
  // ---
  procedure VersionStringToNumeric(const VersionString: AnsiString;
    var VersionMajor, VersionMinor, VersionRelease, VersionBuild: Integer);
  const
    cDigits = ['0'..'9'];
  var
    c: AnsiChar;
    NextNumberFound: Boolean;
    sVer: array[1..4] of AnsiString;
    VerIndex: Integer;
    i: Integer;
  begin
    VerIndex := 0;
    NextNumberFound := False;

    for i := 1 to Length(VersionString) do
    begin
      c := VersionString[i];
      if c in cDigits then
      begin
        if not NextNumberFound then
        begin
          NextNumberFound := True;
          Inc(VerIndex);
          if VerIndex > High(sVer) then
            break;
        end;
        sVer[VerIndex] := sVer[VerIndex] + c;
      end
      else
        NextNumberFound := False;
    end;
    if sVer[1] <> '' then
      VersionMajor := StrToIntDef(string(sVer[1]), -1)
    else
      VersionMajor := 0;
    if sVer[2] <> '' then
      VersionMinor := StrToIntDef(string(sVer[2]), -1)
    else
      VersionMinor := 0;
    if sVer[3] <> '' then
      VersionRelease := StrToIntDef(string(sVer[3]), -1)
    else
      VersionRelease := 0;
    if sVer[4] <> '' then
      VersionBuild := StrToIntDef(string(sVer[4]), -1)
    else
      VersionBuild := 0;
  end;
  // ---
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlConnectionOdbc.RetrieveDriverName'); {$ENDIF _TRACE_CALLS_}

  {Get DBMS info:}
  with fOwnerDbxDriver.fOdbcApi do
  begin

  SetLength(sBuffer, SQL_MAX_OPTION_STRING_LENGTH);
  FillChar(sBuffer[1], Length(sBuffer), 0);
  OdbcRetcode := SQLGetInfoString(fhCon, SQL_DBMS_NAME, PAnsiChar(sBuffer), Length(sBuffer)-1, BufLen);
  if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_DBMS_NAME)',
      SQL_HANDLE_DBC, fhCon, nil, Self);
  fDbmsName := StrPas(PAnsiChar(sBuffer));
  uDbmsName := UpperCase(fDbmsName);
  {$IFDEF _DBXCB_}
  if Assigned(fDbxTraceCallbackEven) then
    DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'connect (dbms name): conn: "$%x": %s', [Integer(Self), uDbmsName]);
  {$ENDIF}

  { RDBMS NAME }
      // GUPTA
  if uDbmsName = 'SQLBASE' then
  begin
    fDbmsType := eDbmsTypeGupta;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeGupta';
    {$ENDIF}
      // MS SQL 2005 Up
  //else if uDbmsName = ? then // Yucon
  //  fDbmsType := eDbmsTypeMsSqlServer2005Up
  end
      // MS SQL
  else if uDbmsName = 'MICROSOFT SQL SERVER' then
  begin
    fDbmsType := eDbmsTypeMsSqlServer;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeMsSqlServer';
    {$ENDIF}
  end
      // DB2
  else if uDbmsName = 'IBMDB2' then
  begin
    fDbmsType := eDbmsTypeIbmDb2;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeIbmDb2';
    {$ENDIF}
  end
      // IBM DB2/400 SQL:
  else if (StrLComp(PAnsiChar(uDbmsName), 'DB2', 3)=0) then
  begin
    fDbmsType := eDbmsTypeIbmDb2AS400;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeIbmDb2AS400';
    {$ENDIF}
  end
      // My SQL:
  else if (StrLComp(PAnsiChar(uDbmsName), 'MYSQL', 5)=0) then
  begin
    fDbmsType := eDbmsTypeMySql;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeMySql';
    {$ENDIF}
  end
      // JET databases
  else if uDbmsName = 'ACCESS' then
  begin
    fDbmsType := eDbmsTypeMsAccess;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeMsAccess';
    {$ENDIF}
  end
      // EXCEL
  else if uDbmsName = 'EXCEL' then
  begin
    fDbmsType := eDbmsTypeExcel;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeExcel';
    {$ENDIF}
  end
      // TEXT
  else if uDbmsName = 'TEXT' then
  begin
    fDbmsType := eDbmsTypeText;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeText';
    {$ENDIF}
  end
      // DBASE II, IV, V
  else if (StrLComp(PAnsiChar(uDbmsName), 'DBASE', 5)=0) then // DBASE II, IV, V
  begin
    fDbmsType := eDbmsTypeDBase;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeDBase';
    {$ENDIF}
  end
      // PARADOX
  else if uDbmsName = 'PARADOX' then
  begin
    fDbmsType := eDbmsTypeParadox;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeParadox';
    {$ENDIF}
  end
      // ORACLE
  else if (StrLComp(PAnsiChar(uDbmsName), 'ORACLE', 6)=0) then
  begin
    fDbmsType := eDbmsTypeOracle;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeOracle';
    {$ENDIF}
  end
      // INFORMIX
  else if uDbmsName = 'INFORMIX' then
  begin
    fDbmsType := eDbmsTypeInformix;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeInformix';
    {$ENDIF}
  end
      // INTERBASE
  else if uDbmsName = 'INTERBASE' then
  begin
    fDbmsType := eDbmsTypeInterbase;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeInterbase';
    {$ENDIF}
  end
      // FIREBIRD
  else if (StrLComp(PAnsiChar(uDbmsName), 'FIREBIRD', 8)=0) then // 'FIREBIRD / INTERBASE(R)'
  begin
    fDbmsType := eDbmsTypeInterbase;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeInterbase';
    {$ENDIF}
  end
      // SYBASE
  else if uDbmsName = 'SQL SERVER' then // Sybase System 11
  begin
    fDbmsType := eDbmsTypeSybase;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeSybase';
    {$ENDIF}
  end
  else if uDbmsName = 'SYBASE' then
  begin
    fDbmsType := eDbmsTypeSybase;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeSybase';
    {$ENDIF}
  end
      // SYBASE: Adaptive Server Anywhere
  else if uDbmsName = 'ADAPTIVE SERVER ANYWHERE' then
  begin
    fDbmsType := eDbmsTypeSybase;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeSybase';
    {$ENDIF}
  end
      // SQLITE
  else if uDbmsName = 'SQLITE' then
  begin
    fDbmsType := eDbmsTypeSQLite;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeSQLite';
    {$ENDIF}
  end
      // THINK SQL
  else if (StrLComp(PAnsiChar(uDbmsName), 'THINKSQL', 8)=0) then // 'THINKSQL RELATIONAL DATABASE MANAGEMENT SYSTEM'
  begin
    fDbmsType := eDbmsTypeThinkSQL;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeThinkSQL';
    {$ENDIF}
  end
      // SAP DB
  else if uDbmsName = 'SAP DB' then
  begin
    fDbmsType := eDbmsTypeSAPDB;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeSAPDB';
    {$ENDIF}
  end
      // PERVASIVE SQL
  else if uDbmsName = 'PERVASIVE.SQL' then
  begin
    fDbmsType := eDbmsTypePervasiveSQL;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypePervasiveSQL';
    {$ENDIF}
  end
      // POSTGRE SQL
  else if (StrLComp(PAnsiChar(uDbmsName), 'POSTGRESQL', 10)=0) then
  begin
    fDbmsType := eDbmsTypePostgreSQL;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypePostgreSQL';
    {$ENDIF}
  end
      // Cache
  else if uDbmsName = 'INTERSYSTEMS CACHE' then
  begin
    fDbmsType := eDbmsTypeInterSystemCache;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeInterSystemCache';
    {$ENDIF}
  end
      // FoxPro
  else if uDbmsName = 'FOXPRO' then
  begin
    fDbmsType := eDbmsTypeFoxPro;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeFoxPro';
    {$ENDIF}
  end
      // Clipper
  else if uDbmsName = 'CLIPPER' then
  begin
    fDbmsType := eDbmsTypeClipper;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeClipper';
    {$ENDIF}
  end
      // Btrieve
  else if uDbmsName = 'BTRIEVE' then      //??? - unchecked
  begin
    fDbmsType := eDbmsTypeBtrieve;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeBtrieve';
    {$ENDIF}
  end
      // Ingres
  else if uDbmsName = 'OPENINGRES' then   //??? - unchecked
  begin
    fDbmsType := eDbmsTypeOpenIngres;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeOpenIngres';
    {$ENDIF}
  end
      // Progress
  else if uDbmsName = 'PROGRESS' then     //??? - unchecked
  begin
    fDbmsType := eDbmsTypeProgress;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeProgress';
    {$ENDIF}
  end
      // Flash Filler
  else if (StrLComp(PAnsiChar(uDbmsName), 'TURBOPOWER FLASHFILER', 21)=0) then
  begin
    fDbmsType := eDbmsTypeFlashFiler;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeFlashFiler';
    {$ENDIF}
  end
      // Oterro
  else if (StrLComp(PAnsiChar(uDbmsName), 'OTERRO', 6)=0) then
  begin
    fDbmsType := eDbmsTypeOterroRBase;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeOterroRBase';
    {$ENDIF}
  end
  else
  begin
    fDbmsType := eDbmsTypeUnspecified;
    {$IFDEF _DBXCB_}
    S := 'eDbmsTypeUnspecified';
    {$ENDIF}
  end;

  {$IFDEF _DBXCB_}
  if Assigned(fDbxTraceCallbackEven) then
    DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'connect (dbms type): conn: "$%x": %s', [Integer(Self), S]);
  S := '';
  {$ENDIF}

  // RDBMS VERSION
  SetLength(sBuffer, 2048); // PostgreSQL returned very large result string (more then SQL_MAX_OPTION_STRING_LENGTH).
  FillChar(sBuffer[1], Length(sBuffer), 0);
  OdbcRetcode := SQLGetInfoString(fhCon, SQL_DBMS_VER, PAnsiChar(sBuffer), Length(sBuffer), BufLen);
  if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
  begin
    //fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_DBMS_VER)',
    //  SQL_HANDLE_DBC, fhCon, Self);
    fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
    fDbmsVersionString := 'Unknown version';
    fDbmsVersionMajor := 0;
    fDbmsVersionMinor := 0;
    fDbmsVersionRelease := 0;
    fDbmsVersionBuild := 0;
  end
  else
  begin
    fDbmsVersionString := StrPas(PAnsiChar(sBuffer));
    VersionStringToNumeric(fDbmsVersionString, fDbmsVersionMajor, fDbmsVersionMinor,
      fDbmsVersionRelease, fDbmsVersionBuild);
  end;

  {$IFDEF _DBXCB_}
  if Assigned(fDbxTraceCallbackEven) then
    DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'connect (dbms version): conn: "$%x": %s', [Integer(Self), fDbmsVersionString]);
  {$ENDIF}

  SetLength(sBuffer, SQL_MAX_OPTION_STRING_LENGTH);
  // SQL_DRIVER_ODBC_VER
  FillChar(sBuffer[1], Length(sBuffer), 0);
  OdbcRetcode := SQLGetInfoString(fhCon, SQL_DRIVER_ODBC_VER, PAnsiChar(sBuffer), Length(sBuffer), BufLen);
  if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_DRIVER_ODBC_VER)',
      SQL_HANDLE_DBC, fhCon, nil, Self);
  fOdbcDriverVersionString := StrPas(PAnsiChar(sBuffer));
  VersionStringToNumeric(fOdbcDriverVersionString, Self.fOdbcDriverLevel, fOdbcDriverVersionMinor,
    fOdbcDriverVersionRelease, fOdbcDriverVersionBuild);
  fOwnerDbxDriver.fOdbcApi.OdbcDriverLevel := Self.fOdbcDriverLevel;
  if (fOwnerDbxDriver.fOdbcApi.OdbcDriverLevel > 0) and
    (fOwnerDbxDriver.fOdbcApi.OdbcDriverLevel < Self.fOdbcDriverLevel)
  then
    Self.fOdbcDriverLevel := fOwnerDbxDriver.fOdbcApi.OdbcDriverLevel;

  // ODBC DRIVER VERSION
  FillChar(sBuffer[1], Length(sBuffer), 0);
  OdbcRetcode := SQLGetInfoString(fhCon, SQL_DRIVER_VER, PAnsiChar(sBuffer), Length(sBuffer), BufLen);
  if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
  begin
    //fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_DRIVER_VER)',
    //  SQL_HANDLE_DBC, fhCon, Self);
    fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
    fOdbcDriverVersionString := 'Unknown version';
    fOdbcDriverVersionMajor := 0;
    fOdbcDriverVersionMinor := 0;
    fOdbcDriverVersionRelease := 0;
    fOdbcDriverVersionBuild := 0;
  end
  else
  begin
    fOdbcDriverVersionString := StrPas(PAnsiChar(sBuffer));
    VersionStringToNumeric(fOdbcDriverVersionString, fOdbcDriverVersionMajor, fOdbcDriverVersionMinor,
      fOdbcDriverVersionRelease, fOdbcDriverVersionBuild);
  end;
  // ODBC DRIVER NAME:
  FillChar(sBuffer[1], Length(sBuffer), 0);
  OdbcRetcode := SQLGetInfoString(fhCon, SQL_DRIVER_NAME, PAnsiChar(sBuffer), Length(sBuffer), BufLen);
  if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
  begin
    //fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_DRIVER_NAME)',
    //  SQL_HANDLE_DBC, fhCon, Self);
    fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
    fOdbcDriverName := 'Unknown driver name';
    sBuffer := '';
  end
  else
  begin
    fOdbcDriverName := StrPas(PAnsiChar(sBuffer));
    sBuffer := fOdbcDriverName;
  end;
  {Get DRIVER info:}
  sBuffer := UpperCase(sBuffer);
  pBuffer := PAnsiChar(sBuffer);
  {$IFDEF _DBXCB_}
  if Assigned(fDbxTraceCallbackEven) then
    DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'connect (odbc driver name, version): conn: "$%x": "%s", "%s"', [Integer(Self), sBuffer, fOdbcDriverVersionString]);
  {$ENDIF}
    // SQL Base:
  if (StrLComp(pBuffer, 'C2GUP', 5) = 0) or
    (StrLComp(pBuffer, 'IVGUP', 5) = 0) // DataDirect SQLBase ODBC Driver
    or
    (StrLComp(pBuffer, 'PBGUP', 5) = 0) // INTERSOLV OEM SQLBase ODBC Driver
  then
  begin
    fOdbcDriverType := eOdbcDriverTypeGupta;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeGupta';
    {$ENDIF}
    // SQL Server 2005 Up:
  //
  //else if (StrLComp(pBuffer, ?, 7) = 0) then // SQL Server 2005
  //  fOdbcDriverType := eOdbcDriverTypeMsSqlServer2005Up
  //
  end
    // SQL Server:
  else if (fDbmsType = eDbmsTypeMsSqlServer) and (
    (StrLComp(pBuffer, 'SQLSRV', 6) = 0)    // SQL Server ( Microsoft ODBC Driver )
    or (StrLComp(pBuffer, 'SQLNCLI', 7) = 0)    // SQL Native Client  ( Microsoft ODBC Driver )
    or (StrLComp(pBuffer, 'IVSS', 4) = 0)   // DataDirect SQL Server ODBC Driver
    or (StrLComp(pBuffer, 'IVMSSS', 6) = 0) // DataDirect SQL Server Wire Protocol ODBC Driver
    or (StrLComp(pBuffer, 'PBSS', 4) = 0)   // PB INTERSOLV OEM SqlServer ODBC Driver
    or // extended comparing
    ((StrLComp(pBuffer, 'NTL', 3) = 0) and (pBuffer[5] = 'M'))
      {// OpenLink Lite for MS-SQL Server (32 Bit) ODBC Driver}
  ) then
  begin
    fOdbcDriverType := eOdbcDriverTypeMsSqlServer;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeMsSqlServer';
    {$ENDIF}
  end
    // IBM DB2:
  else if (StrLComp(pBuffer, 'DB2CLI', 6) = 0) // IBM DB2 ODBC DRIVER
    or (StrLComp(pBuffer, 'LIBDB2', 6) = 0)    // IBM
    or (StrLComp(pBuffer, 'IVDB2', 5) = 0)     // DataDirect DB2 Wire Protocol ODBC Driver
    or (StrLComp(pBuffer, 'PBDB2', 5) = 0)     // INTERSOLV OEM ODBC Driver}
  then
  begin
    fOdbcDriverType := eOdbcDriverTypeIbmDb2;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeIbmDb2';
    {$ENDIF}
  end
    // IBM DB2/400 SQL:
  else if (StrLComp(pBuffer, 'CWBODBC', 7) = 0)   // IBM DB2/400 SQL
  then
  begin
    fOdbcDriverType := eOdbcDriverTypeIbmDb2AS400;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeIbmDb2AS400';
    {$ENDIF}
  end
    // Microsoft desktop databases:
  else if StrLComp(pBuffer, 'ODBCJT', 6) = 0
    {//(Microsoft Paradox Driver, Microsoft dBASE Driver, ...).}then
  begin
    fOdbcDriverType := eOdbcDriverTypeMsJet;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeMsJet';
    {$ENDIF}
      // This driver does not allow SQL_DECIMAL.
      // It driverType usagheb for detecting this situation.
  end
// My SQL ODBC Version 3 Driver:
  else if StrLComp(pBuffer, 'MYODBC3', 7) = 0 then
    fOdbcDriverType := eOdbcDriverTypeMySql3
    // My SQL:
  else if StrLComp(pBuffer, 'MYODBC', 6) = 0 then
  begin
    fOdbcDriverType := eOdbcDriverTypeMySql;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeMySql';
    {$ENDIF}
  end
    // INFORMIX:
  else if (StrLComp(pBuffer, 'ICLI', 4) = 0) // "INFORMIX 3.32 32 BIT" ODBC Driver
    // begin (other informix linux drivers):
  or (StrLComp(pBuffer, 'LIBTHCLI', 8) = 0)
  or (StrLComp(pBuffer, 'LIBIFCLI', 8) = 0)
  or (StrLComp(pBuffer, 'LIBIFDRM', 8) = 0)
  or (StrLComp(pBuffer, 'IDMRM', 5) = 0)
    // end.
  or (StrLComp(pBuffer, 'IVINF', 5) = 0)  // DataDirect Informix ODBC Driver
  or (StrLComp(pBuffer, 'IVIFCL', 6) = 0) // DataDirect Informix Wire Protocol ODBC Driver
  or (StrLComp(pBuffer, 'PDINF', 5) = 0)  // INTERSOLV Inc ODBC Driver (1997. Now is DataDirect)
  or (StrLComp(pBuffer, 'PBINF', 5) = 0)  // PB INTERSOLV OEM ODBC Driver
  or // extended comparing
  ((StrLComp(pBuffer, 'NTL', 3) = 0) and (pBuffer[5] = 'I'))
    {// OpenLink Lite for Informix 7 (32 Bit) ODBC Driver}then
  begin
    fOdbcDriverType := eOdbcDriverTypeInformix;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeInformix';
    {$ENDIF}
  end
    // SYBASE
  else if (fDbmsType = eDbmsTypeSybase) and (
    (StrLComp(pBuffer, 'SYODASE', 7) = 0)       // SYBASE ACE ODBC Driver
    or (StrLComp(pBuffer, 'DBODBC', 6) = 0)     // Adaptive Server Anywhere
    or (StrLComp(pBuffer, 'SYSYBNT', 7) = 0) )  // Sybase System 11
  then
  begin
    fOdbcDriverType := eOdbcDriverTypeSybase;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeSybase';
    {$ENDIF}
    //if fConnectionOptions[coBlobNotTerminationChar] = osDefault then
    //  fConnectionOptions[coBlobNotTerminationChar] := osOn;
  end
  else if ( StrLComp(pBuffer, 'IVASE', 5) = 0 ) then // DataDirect SybaseWire Protocol ODBC Driver
  begin
    fOdbcDriverType := eOdbcDriverTypeSybase;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeSybase';
    {$ENDIF}
  end
  (*
  else if (fDbmsType = eDbmsTypeMsSqlServer) and (
    (StrLComp(pBuffer, 'SYSYBNT', 7) = 0)       // Sybase System 11
  ) then
  begin
    fDbmsType := eDbmsTypeSybase;
    fOdbcDriverType := eOdbcDriverTypeSybase;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeSybase';
    {$ENDIF}
  end
  *)
    // SQLite:
  else if StrLComp(pBuffer, 'SQLITEODBC', 10) = 0 then
  begin
    fOdbcDriverType := eOdbcDriverTypeSQLite;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeSQLite';
    {$ENDIF}
  end
    // INTERBASE:
  else if StrLComp(pBuffer, 'IB6ODBC', 7) = 0 {// Easysoft ODBC Driver} then
  begin
    fOdbcDriverType := eOdbcDriverTypeInterbase;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeInterbase';
    {$ENDIF}
    fOdbcDriverLevel := 2
  end
  else if StrLComp(pBuffer, 'ODBCJDBC', 8) = 0 then {// IBPhoenix ODBC Driver: http://www.ibphoenix.com/}
  begin
    fOdbcDriverType := eOdbcDriverTypeInterbase;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeInterbase';
    {$ENDIF}
    fSupportsBlockRead := False; // Driver fetched only one record for "array fetch".
    fConnectionOptionsDrv[coMixedFetch] := osOff; // driver unsupported STATIC cursor
  end
  else if StrLComp(pBuffer, 'IB6XTG', 6) = 0 then
  begin  {// Open Firebird, Interbase6 ODBC Driver: http://www.xtgsystems.com/ }
    fOdbcDriverType := eOdbcDriverTypeInterbase;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeInterbase';
    {$ENDIF}
    // bug in bcd: returned uncorrected BCD column info (SQLDescribeCol)
    // fConnectionOptionsDrv[coMaxBCD] := osOn; // - added handles in BindResultSet
  end
  else if StrLComp(pBuffer, 'IBGEM', 5) = 0 then
  begin  {// Gemini ODBC: http://www.ibdatabase.com/ }
    fOdbcDriverType := eOdbcDriverTypeInterbase;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeInterbase';
    {$ENDIF}
  end
    // Think SQL:
  else if StrLComp(pBuffer, 'THINKSQL', 8) = 0 {// ThinkSQL ODBC Driver} then
  begin
    fOdbcDriverType := eOdbcDriverTypeThinkSQL;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeThinkSQL';
    {$ENDIF}
  end
    // ORACLE:
  else if (StrLComp(pBuffer, 'SQORA', 5) = 0) // Oracle ODBC Driver
    or (StrLComp(pBuffer, 'MSORCL', 6) = 0)   // Microsoft ODBC for Oracle
    or (StrLComp(pBuffer, 'IVORA', 5) = 0)    // DataDirect Oracle Wire Protocol ODBC Driver
    or (StrLComp(pBuffer, 'IVOR', 4) = 0)     // DataDirect Oracle ODBC Driver
    or (StrLComp(pBuffer, 'PBOR', 4) = 0)     // PB INTERSOLV OEM ODBC Driver
  then
  begin
    fOdbcDriverType := eOdbcDriverTypeOracle;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeOracle';
    {$ENDIF}
  end
  else if (StrLComp(pBuffer, 'INOLE', 5) = 0) {// MERANT ODBC-OLE DB Adapter Driver} then
  begin
    fOdbcDriverType := eOdbcDriverTypeMerantOle;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeMerantOle';
    {$ENDIF}
  end
    // Pervasive.SQL
  else if (StrLComp(pBuffer, 'W3ODBCCI', 8) = 0) {// Pervasive.SQL ODBC Driver Client Interface } then
  begin
    fOdbcDriverType := eOdbcDriverTypePervasive;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypePervasive';
    {$ENDIF}
  end
  else if (StrLComp(pBuffer, 'W3ODBCEI', 8) = 0) {// Pervasive.SQL ODBC Driver Engine Interface } then
  begin
    fOdbcDriverType := eOdbcDriverTypePervasive;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypePervasive';
    {$ENDIF}
  end
    // FlasfFiller
  else if (StrLComp(pBuffer, 'NXODBCDRIVER', 12) = 0) {// NexusDb FlashFiler Driver } then
  begin
    fOdbcDriverType := eOdbcDriverTypeNexusDbFlashFiler;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeNexusDbFlashFiler';
    {$ENDIF}
    if fConnectionOptions[coFldReadOnly] = osDefault then
      fConnectionOptions[coFldReadOnly] := osOff;
  end
    // PostgreSQL
  else if (StrLComp(pBuffer, 'PSQLODBC', 8) = 0) then
  begin
    fOdbcDriverType := eOdbcDriverTypePostgreSQL;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypePostgreSQL';
    {$ENDIF}
  end
    // Cache
  else if (StrLComp(pBuffer, 'CACHEODBC', 9) = 0)  then
  begin
    fOdbcDriverType := eOdbcDriverTypeInterSystemCache;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeInterSystemCache';
    {$ENDIF}
  end
    // "MERANT"/"PowerBuilder Intersolv OEM" Clipper, DBASE, FoxPro
  else if
    (StrLComp(pBuffer, 'IVDBF', 5) = 0)  {MERANT dBASE File ODBC DRIVER}
    or
    (StrLComp(pBuffer, 'PBDBF', 5) = 0)  {PB INTERSOLV OEM dBASE File ODBC DRIVER}
  then
  begin
    fOdbcDriverType := eOdbcDriverTypeMerantDBASE; // Clipper, DBASE, FoxPro
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeMerantDBASE';
    {$ENDIF}
    //fSupportsBlockRead := False; // not supported SQL_CURSOR_STATIC (it is autodetected)
    { MERANT Odbc Driver bug:
        Cannot convert from SQL type SQL_TYPE_DATE to C type SQL_C_DATE...
    }
    if fConnectionOptions[coFldReadOnly] = osDefault then
      fConnectionOptions[coFldReadOnly] := osOff;
    if fConnectionOptions[coParamDateByOdbcLevel3] = osDefault then
      fConnectionOptions[coParamDateByOdbcLevel3] := osOn;
  end
    // 'SAP DB' ODBC Driver by SAP AG
  else if (fDbmsType = eDbmsTypeSAPDB) and
    (StrLComp(pBuffer, 'SQLOD', 5) = 0)  then {'SAP DB' ODBC Driver by SAP AG}
  begin
    fOdbcDriverType := eOdbcDriverTypeSAPDB;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeSAPDB';
    {$ENDIF}
    fOdbcDriverLevel := 2;
    fSupportsBlockRead := False; // Driver fetched only one record for "array fetch".
    //fConnectionOptionsDrv[coMixedFetch] := osOff;
  end
   // PARADOX
  else if (fDbmsType = eDbmsTypeParadox) and (
      (StrLComp(pBuffer, 'PBIDP', 5) = 0) // PB INTERSOLV OEM ParadoxFile ODBC Driver:
      or                                 //   supports Paradox 3.0, 3.5, 4.0, 4.5, 5.0, 7.0, and 8.0 tables.
      (StrLComp(pBuffer, 'IVDP', 5) = 0)  // DataDirect Paradox File (*.db) ODBC Driver:
    ) then
  begin
    fOdbcDriverType := eOdbcDriverTypeParadox;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeParadox';
    {$ENDIF}
    //if (StrLComp(Buffer, 'PBIDP', 5) = 0) then // ???: 'IVDP'
    begin
      if fConnectionOptions[coFldReadOnly] = osDefault then
        fConnectionOptions[coFldReadOnly] := osOff;
      if fConnectionOptions[coParamDateByOdbcLevel3] = osDefault then
        fConnectionOptions[coParamDateByOdbcLevel3] := osOn;
    end;
  end
   // Btrieve
  else if (fDbmsType = eDbmsTypeBtrieve) and (
    (StrLComp(pBuffer, 'IVBTR', 5) = 0 ) // DataDirect Btrieve (*.dta) ODBC Driver
    or
    (StrLComp(pBuffer, 'PBBTR', 5) = 0)  // PB INTERSOLV OEM Btrieve ODBC Driver:
    ) then
  begin
    fOdbcDriverType := eOdbcDriverTypeBtrieve;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeBtrieve';
    {$ENDIF}
  end
   // OpenIngres
  else if (fDbmsType = eDbmsTypeBtrieve) and (
      (StrLComp(pBuffer, 'PBOING', 6) = 0) // PB INTERSOLV OEM OpenIngres ODBC Driver:
      or
      (StrLComp(pBuffer, 'PBOI2', 5) = 0)  // PB INTERSOLV OEM OpenIngres2 ODBC Driver:
    ) then
  begin
    fOdbcDriverType := eOdbcDriverTypeOpenIngres;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeOpenIngres';
    {$ENDIF}
  end
    // FoxPro
  else if (fDbmsType = eDbmsTypeFoxPro) and (
      (StrLComp(pBuffer, 'VFPODBC', 7) = 0) // Microsoft Visual FoxPro Driver (*.dbf)    'VFPODBC'
      or
      (StrLComp(pBuffer, 'IVDBF', 5) = 0) // DataDirect FoxPro 3.0 database (*.dbc)    'IVDBF'
    ) then
  begin
    fOdbcDriverType := eOdbcDriverTypeFoxPro;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeFoxPro';
    {$ENDIF}
    if (StrLComp(pBuffer, 'IVDBF', 5) = 0) then  // ???: 'IVDBF'
    begin
      if fConnectionOptions[coFldReadOnly] = osDefault then
        fConnectionOptions[coFldReadOnly] := osOff;
      if fConnectionOptions[coParamDateByOdbcLevel3] = osDefault then
        fConnectionOptions[coParamDateByOdbcLevel3] := osOn;
    end;
  end
    // Progress
  else if (fDbmsType = eDbmsTypeProgress) and (
      ( StrLComp(pBuffer, 'IVPRO', 5) = 0 )  // DataDirect Progress ODBC Driver
      or
      (StrLComp(pBuffer, 'PBPRO', 5) = 0)    // INTERSOLV OEM Progress ODBC Driver
    ) then
  begin
    fOdbcDriverType := eOdbcDriverTypeProgress;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeProgress';
    {$ENDIF}
  end
    // Oterro RBase 2, 3
  else if (fDbmsType = eDbmsTypeOterroRBase) and (
      ( StrLComp(pBuffer, 'OT2K_32', 7) = 0 )
      or
      (StrLComp(pBuffer, 'OTERRO', 6) = 0)
    ) then
  begin
    fOdbcDriverType := eOdbcDriverTypeOterroRBase;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeOterroRBase';
    {$ENDIF}
    if fConnectionOptions[coFldReadOnly] = osDefault then
      fConnectionOptions[coFldReadOnly] := osOff;
  end

  else
  begin
    fOdbcDriverType := eOdbcDriverTypeUnspecified;
    {$IFDEF _DBXCB_}
    S := 'eOdbcDriverTypeUnspecified';
    {$ENDIF}
  end;
  // OTHER:
  {

  DataDirect dBASE File (*.dbf)             'IVDBF'
  DataDirect Excel Workbook (*.xls)         'IVXLWB'
  DataDirect Text File (*.*)                'IVTXT'
  DatDirect XML                             'IVXML'
  }

  {$IFDEF _DBXCB_}
  if Assigned(fDbxTraceCallbackEven) then
    DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'connect (odbc driver type): conn: "$%x": %s', [Integer(Self), S]);
  S := '';
  {$ENDIF}

  {Initialize Server specific parameters:}

  case fDbmsType of
    eDbmsTypeText:
      begin
        // Table name is equal FileName with extension.
        // It do not allow correctly parsing table name in Provider when TableName contained '.' (Look in 'Provider.pas' procedure GetQuotedTableName).
        fWantQuotedTableName := False;
        fConnectionOptionsDrv[coSupportsCatalog] := osOff;
        // 'Microsoft Text Driver (*.txt, *.csv)', ver: '4.00.6019.00' do not allow update or delete data by this ISAM driver (Allows only an insert but only if the name of a column is simple).
      end;
    eDbmsTypeOracle:
      begin
        if fOdbcCatalogPrefix = '' then
          fOdbcCatalogPrefix := 'UID';
//#0xx: todo: need check for edit table
        fWantQuotedTableName := False;
      end;
    eDbmsTypeInformix:
      begin
        fWantQuotedTableName := False;
        fConnectionOptionsDrv[coSupportsCatalog] := osOff; // INFORMIX supports operation with
        // the catalog, but usage of this option is inconvenient for the developers and there is no
        // large sense  by work with INFORMIX. If you want to work with the catalog, comment out this line.
        fConnectionOptionsDrv[coIgnoreUnknownFieldType] := osOn;
        fSupportsTransactionMetadata := True;
      end;
    eDbmsTypeOterroRBase:
      begin
        fConnectionOptionsDrv[coSupportsCatalog] := osOff;
      end;
  end;

  case fOdbcDriverType of
    eOdbcDriverTypeUnspecified:
      begin
        // ODBC SQLBindParameter Buffer Precision Limitation (see: TSqlCommandOdbc.setParameter)
        fConnectionOptionsDrv[coOBPBPL] := osOff;
      end;
    eOdbcDriverTypeGupta:
      begin
        { empty }
      end;
    eOdbcDriverTypeMsSqlServer2005Up:
      begin
        if not SystemODBCManager then
        begin
          if fDbmsVersionMajor = 9 then  // MSSQL 2005
            fCursorPreserved := False; // Most likely will change simultaneously with change of restriction fStatementPerConnection=1.
        end;
      end;
    eOdbcDriverTypeMsSqlServer:
      begin
        if fDbmsType = eDbmsTypeMsSqlServer then
        begin
          // DataDirect SQL Server ODBC Driver (Contains an error of installation of the
          // unknown catalog)
          if (StrLComp(pBuffer, 'IVSS', 4) = 0) then
          begin
             fConnectionOptionsDrv[coSupportsCatalog] := osOff;
             // TODO: need check MixedFetch for DataDirect
          end;
          // d'nt work SQLFetch when (fSupportsBlockRead = True) and ( SystemODBCManager = False) and (fCursorFetchRowCount > 1).
          { // fixed TOdbcApiProxy.
          fSupportsBlockRead := fOwnerDbxDriver.fOdbcApi.SystemODBCManager;
          {}
          // The odbc driver "sqlsrv32.dll ver: '2000.81.9030.04'" returns incorrect value
          // SQLGetInfo(SQL_CURSOR_COMMIT_BEHAVIOR or SQL_CURSOR_ROLLBACK_BEHAVIOR)
          {begin:}
          if not SystemODBCManager then
            fCursorPreserved := False; // Most likely will change simultaneously with change of restriction fStatementPerConnection=1.
          {end.}
          if not (
              (fDbmsVersionMajor >= 8) and
              (fDbmsVersionMinor >= 0) and
              (fDbmsVersionRelease >= 384)
             )
          then
          begin
            {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
            (*
              It do not work for MSSQL2K: SELECT * FROM "dbo"."syscomments" when MixedFetch is
              turned On. It is detected on:
                Server version: SQL2K '08.00.0194'
                Odbc version: '2000.81.9030.04'
              --------------------------------------------------------------
              Error returned from ODBC function SQLExecute
              ODBC Return Code: -1: SQL_ERROR
              ODBC SqlState:        42000
              Native Error Code:    510
              [Microsoft][ODBC SQL Server Driver][SQL Server]Cannot create a
              worktable row larger than allowable maximum.
                Resubmit your query with the ROBUST PLAN hint.
              --------------------------------------------------------------
              All works when Server version is: SQL2K DE '08.00.0384' and
              ODBC version is: '03.81.9030'.
            *)
            {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
            fConnectionOptionsDrv[coMixedFetch] := osOff;
          end;
        end;
      end;
    eOdbcDriverTypeIbmDb2:
      begin
        // Columns Fetch Consecutively (see: TSqlCursorOdbc.CheckFetchCacheColumns)
        fConnectionOptionsDrv[coCFC] := osOff;
      end;
    eOdbcDriverTypeIbmDb2AS400:
      begin
        // Stored Proc Sys Naming (see:  TSqlCursorMetaDataProcedureParams.FetchProcedureParams)
        fConnectionOptionsDrv[coSPSN] := osOn;
        // Columns Fetch Consecutively (see: TSqlCursorOdbc.CheckFetchCacheColumns)
        fConnectionOptionsDrv[coCFC] := osOff;
      end;
    eOdbcDriverTypeMsJet:
      begin
        fConnectionOptionsDrv[coMixedFetch] := osOn;
        // ODBC SQLBindParameter Buffer Precision Limitation (see: TSqlCommandOdbc.setParameter)
        fConnectionOptionsDrv[coOBPBPL] := osOff;
        // Table List Support Only Tables (see: TSqlCursorMetaDataTables.FetchTables)
        if fDbmsType = eDbmsTypeExcel then
        //if fDbmsType in [eDbmsTypeExcel, eDbmsTypeText, eDbmsTypeDBase, eDbmsTypeParadox] then
          fConnectionOptionsDrv[coTLSTO] := osOn;
      end;
    eOdbcDriverTypeMySql3: // New MySql Driver - Odbc Version 3!
      begin
        { empty }
      end;
    eOdbcDriverTypeMySql:
      begin
        //fOdbcDriverLevel := 2; // MySql is Level 2
        fConnectionOptionsDrv[coSupportsCatalog] := osOff;
      end;
    eOdbcDriverTypeInformix:
      begin
        fWantQuotedTableName := False;
        //if ( StrLComp(pBuffer, 'PDINF', 5) = 0 ) // INTERSOLV Inc ODBC Driver (1997. Now is DataDirect)
        fConnectionOptionsDrv[coSupportsCatalog] := osOff; // INFORMIX supports operation with
        // the catalog, but usage of this option is inconvenient for the developers and there is no
        // large sense  by work with INFORMIX. If you want to work with the catalog, comment out this line.
        fConnectionOptionsDrv[coIgnoreUnknownFieldType] := osOn;
        //fConnectionOptionsDrv[coMixedFetch] := osOn;
        if fConnectionOptions[coFldReadOnly] = osDefault then
          fConnectionOptions[coFldReadOnly] := osOff;
        // Columns Fetch Consecutively (see: TSqlCursorOdbc.CheckFetchCacheColumns)
        fConnectionOptionsDrv[coCFC] := osOff;
      end;
    eOdbcDriverTypeSybase:
      begin
        {$IFDEF _ASA_MESSAGE_CALLBACK_}
        // https://sourceforge.net/tracker/index.php?func=detail&aid=1508015&group_id=38250&atid=422097
        OdbcRetcode := SQLSetConnectAttr(fhCon, ASA_REGISTER_MESSAGE_CALLBACK,
          SqlPointer(@DbxOpenOdbcASA.ASA_MESSAGE_CALLBACK), SQL_IS_POINTER);
        WM_AsaCallback_Supported := OdbcRetcode = OdbcApi.SQL_SUCCESS;
        if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
          //fOwnerDbxDriver.OdbcCheck(OdbcRetCode, 'SQLSetConnectAttr(ASA_REGISTER_MESSAGE_CALLBACK)',
          //  SQL_HANDLE_DBC, fhCon, nil, Self);
          fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
        {$ENDIF}
      end;
    eOdbcDriverTypeSQLite:
      begin
        //fOdbcDriverLevel := 2; // SQLite is Level 2
        fConnectionOptionsDrv[coSupportsCatalog] := osOff;
        fConnectionOptionsDrv[coNullStrParam] := osOn;
      end;
    eOdbcDriverTypeThinkSQL:
      begin
        fConnectionOptionsDrv[coSupportsCatalog] := osOff;
      end;
    eOdbcDriverTypeOracle:
      begin
        //fConnectionOptionsDrv[coSupportsSchemaFilter] := osOn;
        //fConnectionOptionsDrv[coMixedFetch] := osOn;
        // fConnectionOptions[coBcd2Exp] := osOn; // for verion >= 9

        // Columns Fetch Consecutively (see: TSqlCursorOdbc.CheckFetchCacheColumns)
        fConnectionOptionsDrv[coCFC] := osOff;
      end;
    eOdbcDriverTypePervasive:
      begin
        //fOdbcDriverLevel := 2; // Pervasive is Level 2. Not supported OrdinalPosition for MetadataColumns.
        fConnectionOptionsDrv[coMixedFetch] := osOff; // Bug in driver. Driver not correctly supported
        // this option, but user can set it option in connection string or by custom options.
      end;
    eOdbcDriverTypeNexusDbFlashFiler:
      begin
        { empty }
      end;
    eOdbcDriverTypePostgreSQL:
      begin
        { empty }
      end;
    eOdbcDriverTypeInterSystemCache:
      begin
        //fOdbcDriverLevel := 2; // Cache is Level 2
        //fConnectionOptionsDrv[coMixedFetch] := osOn;
      end;
    eOdbcDriverTypeMerantDBASE:
      begin
        fConnectionOptionsDrv[coMixedFetch] := osOff;
        //fOdbcDriverLevel := 2;

        // Columns Fetch Consecutively (see: TSqlCursorOdbc.CheckFetchCacheColumns)
        fConnectionOptionsDrv[coCFC] := osOff;
      end;
    eOdbcDriverTypeSAPDB:
      begin
        { empty }
      end;
    eOdbcDriverTypeParadox:
      begin
        { empty }
      end;
    eOdbcDriverTypeFoxPro:
      begin
        { empty }
      end;
    eOdbcDriverTypeClipper:
      begin
        { empty }
      end;
    eOdbcDriverTypeBtrieve:
      begin
        { empty }
      end;
    eOdbcDriverTypeOpenIngres:
      begin
        { empty }
      end;
    eOdbcDriverTypeProgress:
      begin
        { empty }
      end;
    eOdbcDriverTypeOterroRBase:
      begin
        { empty }
      end;
  end; //of: case

  if (fOdbcDriverType = eOdbcDriverTypeUnspecified) or (fDbmsType = eDbmsTypeUnspecified) then
  begin // when connected to unknown dbms and use unknown driver:
    // when not defined coFldReadOnly connection option:
    if (fConnectionOptionsDrv[coFldReadOnly] = osDefault)
      and (fConnectionOptions[coFldReadOnly] = osDefault)
    then
      fConnectionOptions[coFldReadOnly] := osOff;
    // when not defined coSupportsAutoInc connection option:
    if (fConnectionOptionsDrv[coSupportsAutoInc] = osDefault)
      and (fConnectionOptions[coSupportsAutoInc] = osDefault)
  then
      fConnectionOptions[coSupportsAutoInc] := osOff;
  end;

  if fConnectionOptionsDrv[coParamDateByOdbcLevel3] = osDefault then
  begin
    if fOdbcDriverLevel <> 2 then
      fConnectionOptionsDrv[coParamDateByOdbcLevel3] := osOn
    else
      fConnectionOptionsDrv[coParamDateByOdbcLevel3] := osOff;
  end;

  if fOwnerDbxDriver.fDBXVersion >= 30 then
  begin
    if fConnectionOptionsDrv[coEnableUnicode] = osDefault then
      fConnectionOptionsDrv[coEnableUnicode] := osOn;
  end;

  Result := DBXERR_NONE;

  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.RetrieveDriverName', e);  raise; end; end;
    finally
      LogInfoProc(['01-DbmsName =', fDbmsName]);
      LogInfoProc(['02-DbmsVersion =', fDbmsVersionString]);
      LogInfoProc(['03-OdbcDriverName =', fOdbcDriverName]);
      LogInfoProc(['04-OdbcDriverVer =', fOdbcDriverVersionString]);
      LogInfoProc(['05-OdbcDriverType =', cOdbcDriverType[fOdbcDriverType]]);
      LogInfoProc(['06-DbmsType =', cDbmsType[fDbmsType]]);
      LogExitProc('TSqlConnectionOdbc.RetrieveDriverName', ['Result =', Result]);
    end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc.disconnect: SQLResult;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  i, iT: Integer;
  iDbxConStmt: PDbxConStmt;
  AttrVal: SqlUInteger;
  bSafeMode: Boolean;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlConnectionOdbc.disconnect'); {$ENDIF _TRACE_CALLS_}
  {$IFDEF _DBXCB_}
  if Assigned(fDbxTraceCallbackEven) then
    DbxCallBackSendMsgFmt(cTDBXTraceFlags_Connect, 'connect (close): conn: "$%x"', [Integer(Self)]);
  {$ENDIF}
  with fOwnerDbxDriver.fOdbcApi do
  try
    bSafeMode := fSafeMode or fConnectionClosed;
    try
    if (fStatementPerConnection > 0) and (fDbxConStmtList <> nil) then
    begin
      fCurrDbxConStmt := nil;
      try
      for i := (fDbxConStmtList.Count - 1) downto 0 do // "0" is equal main fhCon
      begin
        iDbxConStmt := fDbxConStmtList[i];
        if iDbxConStmt = nil then
          continue;
        fDbxConStmtList[i] := nil;
        try
          if (iDbxConStmt.fHCon <> SQL_NULL_HANDLE) then
          begin
            dec(fDbxConStmtActive);
            if iDbxConStmt.fSqlHStmtAllocated > 0 then
              inc(Self.fCon0SqlHStmt);
            AttrVal := SQL_AUTOCOMMIT_ON;
            if fSupportsTransaction and (not iDbxConStmt.fDeadConnection) {and (not fConnectionClosed)} then
            begin
              OdbcRetCode := SQLGetConnectAttr(iDbxConStmt.fHCon, SQL_ATTR_AUTOCOMMIT, @AttrVal, 0, nil);
              if (OdbcRetCode <> OdbcApi.SQL_SUCCESS) then
              begin
                if bSafeMode then
                  // clear last error:
                  fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, iDbxConStmt.fHCon, @iDbxConStmt, Self, nil, nil, 1)
                else
                  fOwnerDbxDriver.OdbcCheck(OdbcRetCode, 'TransactionCheck - SQLGetConnectAttr(SQL_ATTR_AUTOCOMMIT)',
                    SQL_HANDLE_DBC, iDbxConStmt.fHCon, @iDbxConStmt, Self);
              end;
              if iDbxConStmt.fInTransaction > 0 then
              begin
                if (AttrVal = SQL_AUTOCOMMIT_OFF) then
                begin
                  for iT := iDbxConStmt.fInTransaction downto 1 do
                  begin
                    dec(iDbxConStmt.fInTransaction);
                    OdbcRetcode := SQLEndTran(SQL_HANDLE_DBC, iDbxConStmt.fHCon, SQL_ROLLBACK);
                    if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
                    begin
                      if bSafeMode then
                        // clear last error:
                        fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, iDbxConStmt.fHCon, @iDbxConStmt, Self, nil, nil, 1)
                      else
                        fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLEndTran',
                          SQL_HANDLE_DBC, iDbxConStmt.fHCon, @iDbxConStmt, Self);
                    end;
                    iDbxConStmt.fAutoCommitMode := SQL_AUTOCOMMIT_ON; // = SQL_AUTOCOMMIT_DEFAULT
                  end;
                end;
              end
              else
              begin
                if (AttrVal = SQL_AUTOCOMMIT_OFF) then
                begin
                  OdbcRetcode := SQLSetConnectAttr(iDbxConStmt.fHCon, SQL_ATTR_AUTOCOMMIT,
                    Pointer(Smallint(SQL_AUTOCOMMIT_ON)), 0);
                  if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
                  begin
                    if bSafeMode then
                      // clear last error:
                      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, iDbxConStmt.fHCon, @iDbxConStmt, Self, nil, nil, 1)
                    else
                      fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLSetConnectAttr(hCon, SQL_ATTR_AUTOCOMMIT)',
                        SQL_HANDLE_DBC, iDbxConStmt.fHCon, @iDbxConStmt, Self);
                  end;
                end;
              end;
            end;
            if i = 0 then
            begin
              fhCon := SQL_NULL_HANDLE;
              fConnected := False;
            end;
            iDbxConStmt.fAutoCommitMode := SQL_AUTOCOMMIT_ON; // = SQL_AUTOCOMMIT_DEFAULT
            try
              OdbcRetcode := SQLDisconnect(iDbxConStmt.fHCon);
              if (i > 0)and(OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
              begin
                if bSafeMode or iDbxConStmt.fDeadConnection then
                  // clear last error:
                  fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, iDbxConStmt.fHCon, @iDbxConStmt, Self, nil, nil, 1)
                else
                  fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLDisconnect',
                    SQL_HANDLE_DBC, iDbxConStmt.fHCon, @iDbxConStmt, Self);
              end;
            finally
              fOwnerDbxDriver.FreeHCon(iDbxConStmt.fHCon, @iDbxConStmt, fSafeMode or iDbxConStmt.fDeadConnection);
            end;
          end;//of: if (iDbxConStmt.fHCon <> SQL_NULL_HANDLE)
        finally
          DisposeDbxConStmt(iDbxConStmt);
        end;
      end;//of: for i := (fDbxConStmtList.Count - 1) downto 0
      finally
        if not fConnected then
          FreeAndNil(fDbxConStmtList);
      end;
    end
    else
    if (fhCon <> SQL_NULL_HANDLE) and fConnected then
    begin
      if (fInTransaction > 0 ) and (fAutoCommitMode = SQL_AUTOCOMMIT_OFF) then
      begin
        for iT := fInTransaction downto 1 do
        begin
          dec(fInTransaction);
          if fSupportsTransaction and (not fConnectionClosed) then
          begin
            OdbcRetcode := SQLEndTran(SQL_HANDLE_DBC, fhCon, SQL_ROLLBACK);
            if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
            begin
              if bSafeMode then
                // clear last error:
                fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1)
              else
                fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLEndTran', SQL_HANDLE_DBC, fhCon, nil, Self);
            end;
          end;
        end;
        fAutoCommitMode := SQL_AUTOCOMMIT_ON; // = SQL_AUTOCOMMIT_DEFAULT
      end;
      try
        OdbcRetcode := SQLDisconnect(fhCon);
        if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
        begin
          if bSafeMode then
            // clear last error:
            fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1)
          else
            fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLDisconnect', SQL_HANDLE_DBC, fhCon, nil, Self);
        end;
      finally
        fConnected := False;
        fOwnerDbxDriver.FreeHCon(fHCon, nil, fSafeMode or fConnectionClosed);
      end;
    end;
    fConnected := False;
{$IFDEF _RegExprParser_}
    FreeAndNil(fObjectNameParser);
{$ENDIF}
    finally
      if not fConnected then
        ClearConnectionOptions();
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      CheckMaxLines(fConnectionErrorLines);
      fConnectionErrorLines.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.disconnect', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.disconnect', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc.getErrorMessage;//(Error: PAnsiChar): SQLResult;
begin
  if Error <> nil then
    StrCopy(Error, PAnsiChar(AnsiString(fConnectionErrorLines.Text)));
  fConnectionErrorLines.Clear;
  Result := DBXERR_NONE;
end;

function TSqlConnectionOdbc.getErrorMessageLen;//(out ErrorLen: Smallint): SQLResult;
begin
  ErrorLen := Length(fConnectionErrorLines.Text);
  Result := DBXERR_NONE;
end;

procedure TSqlConnectionOdbc.CheckTransactionSupport;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  GetInfoSmallInt: SqlUSmallint;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc.CheckTransactionSupport'); {$ENDIF _TRACE_CALLS_}
  with fOwnerDbxDriver.fOdbcApi do
  begin

  {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
  {
   ODBC Transaction support info values...

   SQL_TC_NONE = Transactions not supported. (ODBC 1.0)

   SQL_TC_DML = Transactions can only contain Data Manipulation Language
   (DML) statements (SELECT, INSERT, UPDATE, DELETE).
   Data Definition Language (DDL) statements encountered in a transaction
   cause an error. (ODBC 1.0)

   SQL_TC_DDL_COMMIT = Transactions can only contain DML statements.
   DDL statements (CREATE TABLE, DROP INDEX, and so on) encountered in a transaction
   cause the transaction to be committed. (ODBC 2.0)

   SQL_TC_DDL_IGNORE = Transactions can only contain DML statements.
   DDL statements encountered in a transaction are ignored. (ODBC 2.0)

   SQL_TC_ALL = Transactions can contain DDL statements and DML statements in any order.
   (ODBC 1.0)

   Mapping to DbExpress transaction support is based on DML support (ie SELECT, INSERT etc)
  }
  {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}

  OdbcRetcode := SQLGetInfoSmallint(fhCon, SQL_TXN_CAPABLE, GetInfoSmallInt,
    SizeOf(GetInfoSmallInt), nil);
  if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    // clear last error:
    fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
    //fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(fhCon, SQL_TXN_CAPABLE)',
    //  SQL_HANDLE_DBC, fhCon);
  fSupportsTransaction := (OdbcRetcode = OdbcApi.SQL_SUCCESS) and (GetInfoSmallInt <> SQL_TC_NONE);
  // Workaund MySql bug - MySql ODBC driver can INCORRECTLY report that it
  // supports transactions, so we test it to make sure..
  if fSupportsTransaction and
     (
//       fOdbcDriverType in  [eOdbcDriverTypeMySql, eOdbcDriverTypeMySql3]
       fDbmsType = eDbmsTypeMySql // ???: Need for MySQL 4 betta
     )
  then
  begin
    OdbcRetcode := SQLSetConnectAttr(fhCon, SQL_ATTR_AUTOCOMMIT,
      Pointer(Smallint(SQL_AUTOCOMMIT_OFF)), 0);
    if OdbcRetcode = -1 then
      fSupportsTransaction := False;
    // clear last error:
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
    OdbcRetcode := SQLSetConnectAttr(fhCon, SQL_ATTR_AUTOCOMMIT,
      Pointer(Smallint(SQL_AUTOCOMMIT_ON)), 0);
    if fSupportsTransaction and (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
      fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLSetConnectAttr(fhCon, SQL_ATTR_AUTOCOMMIT)',
        SQL_HANDLE_DBC, fhCon, nil)
    else
    // clear last error:
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC, fhCon, nil, Self, nil, nil, 1);
  end;

  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.CheckTransactionSupport', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.CheckTransactionSupport'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSQLConnectionOdbc.GetMetaDataOption;//(eDOption: TSQLMetaDataOption;
//  PropValue: Pointer; MaxLength: Smallint; out Length: Smallint): SQLResult;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  GetInfoStringBuffer: array[0..1] of AnsiChar;
  GetInfoSmallInt: SqlUSmallint;
  ConnectAttrLength: SqlInteger;
  MaxIdentifierLen: SqlUSmallint;
  MaxColumnNameLen: SqlUSmallint;
  MaxTableNameLen: SqlUSmallint;
  MaxObjectNameLen: Integer;
  BatchSupport: SqlUInteger;
  xeDOption: TXSQLMetaDataOption absolute eDOption;
begin
  {$IFDEF _TRACE_CALLS_}
    Result := DBXERR_NONE;
    try try
    {$R+}
    LogEnterProc('TSQLConnectionOdbc.GetMetaDataOption', ['eDOption =', cSQLMetaDataOption[xeDOption]]);
    {$IFDEF RANGECHECKS_OFF} {$R-} {$ENDIF}
  {$ENDIF _TRACE_CALLS_}

  {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
  // Note on calls to SQLGetInfo -
  //
  // ODBC API specification states that where returned value is of type SQLUSMALLINT,
  // the driver ignores the length parameter (ie assumes length of 2)
  // However, Centura driver REQUIRES length parameter, even for SQLUSMALLINT value;
  // If omitted, Centura driver returns SQL_SUCCESS_WITH_INFO - Data Truncated,
  // and does not return the data.
  // So I have had to code the length parameter for all SQLGetInfo calls.
  // Never mind, compliant ODBC driver will just ignore the length parameter...
  {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}

  with fOwnerDbxDriver.fOdbcApi do
  try
    Result := DBXERR_NONE;
    Length := 0;
    case xeDOption of
      xeMetaCatalogName: // Dbx Read/Write
        begin
          // Do not return cached catalog name, could be changed, eg. by Sql statement USE catalogname
          GetCurrentCatalog( GetCurrentConnectionHandle{???: or 0 for main connection} );
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get metadata option MetaCatalogName: (%s)', [
              fCurrentCatalog ]);
          {$ENDIF}
          GetStringOptions(Self,
            fCurrentCatalog,
            PAnsiChar(PropValue),
            MaxLength,
            Length,
            eiMetaCatalogName);
        end;
      xeMetaSchemaName: // Dbx Read/Write
        begin
          // There is no ODBC function to get this
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get metadata option MetaSchemaName: (%s)', [
              fCurrentSchema ]);
          {$ENDIF}
          GetStringOptions(Self,
            fCurrentSchema,
            PAnsiChar(PropValue),
            MaxLength,
            Length,
            eiMetaSchemaName);
        end;
      xeMetaDatabaseName: // Readonly
        if (PropValue <> nil) and (MaxLength > 0) then
        begin
          OdbcRetcode := SQLGetConnectAttr(fhCon, SQL_DATABASE_NAME,
            PAnsiChar(PropValue), MaxLength, @ConnectAttrLength);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetConnectAttr(fhCon, SQL_DATABASE_NAME)',
              SQL_HANDLE_DBC, fhCon, nil);
          Length := ConnectAttrLength;
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get metadata option MetaDatabaseName: (%s)', [
              StrPas(PAnsiChar(PropValue)) ]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeMetaDatabaseVersion: // Readonly
        if (PropValue <> nil) and (MaxLength >= SizeOf(Integer)) then
        begin
          Integer(PropValue^) := fDbmsVersionMajor;
          Length := SizeOf(Integer);
          {
          OdbcRetcode := SQLGetConnectAttr(fhCon, SQL_DBMS_VER,
            PAnsiChar(PropValue), MaxLength, @ConnectAttrLength);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetConnectAttr(fhCon, SQL_DBMS_VER)',
              SQL_HANDLE_DBC, fhCon);
          Length := ConnectAttrLength;
          {}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeMetaTransactionIsoLevel: // Readonly
        begin
          {empty}
        end;
      xeMetaSupportsTransaction: // Readonly
        if (PropValue <> nil) and (MaxLength >= SizeOf(Boolean)) then
        begin
          // Metadata transaction support
          Boolean(PropValue^) := fSupportsTransactionMetadata and fSupportsTransaction;
          Length := SizeOf(Boolean);
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get metadata option MetaSupportsTransaction: (%d)', [
              Integer(Boolean(PropValue^)) ]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeMetaMaxObjectNameLength: // Readonly
        if (PropValue <> nil) and (MaxLength >= SizeOf(Integer)) then
        begin
          OdbcRetcode := SQLGetInfoSmallint(fhCon, SQL_MAX_IDENTIFIER_LEN, MaxIdentifierLen,
            SizeOf(GetInfoSmallInt), nil);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_MAX_IDENTIFIER_LEN)',
              SQL_HANDLE_DBC, fhCon, nil);
          Integer(PropValue^) := GetInfoSmallInt;

          OdbcRetcode := SQLGetInfoSmallint(fhCon, SQL_MAX_COLUMN_NAME_LEN, MaxColumnNameLen,
            SizeOf(GetInfoSmallInt), nil);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_MAX_COLUMN_NAME_LEN)',
              SQL_HANDLE_DBC, fhCon, nil);

          OdbcRetcode := SQLGetInfoSmallint(fhCon, SQL_MAX_TABLE_NAME_LEN, MaxTableNameLen,
            SizeOf(GetInfoSmallInt), nil);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_MAX_TABLE_NAME_LEN)',
              SQL_HANDLE_DBC, fhCon, nil);

          MaxObjectNameLen := MaxIdentifierLen;
          if MaxColumnNameLen < MaxObjectNameLen then
            MaxObjectNameLen := MaxColumnNameLen;
          if MaxTableNameLen < MaxObjectNameLen then
            MaxTableNameLen := MaxColumnNameLen;
          Integer(PropValue^) := MaxColumnNameLen;
          Length := SizeOf(Integer);

          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get metadata option MetaMaxObjectNameLength: (%d)', [
              Integer(PropValue^) ]);
          {$ENDIF}

        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeMetaMaxColumnsInTable: // Readonly
        if (PropValue <> nil) and (MaxLength >= SizeOf(Integer)) then
        begin
          OdbcRetcode := SQLGetInfoSmallint(fhCon, SQL_MAX_COLUMNS_IN_TABLE, GetInfoSmallInt,
            SizeOf(GetInfoSmallInt), nil);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_MAX_COLUMNS_IN_TABLE)',
              SQL_HANDLE_DBC, fhCon, nil);
          Integer(PropValue^) := GetInfoSmallInt;
          Length := SizeOf(Integer);
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get metadata option MetaMaxColumnsInTable: (%d)', [
              Integer(PropValue^) ]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeMetaMaxColumnsInSelect: // Readonly
        if (PropValue <> nil) and (MaxLength >= SizeOf(Integer)) then
        begin
          OdbcRetcode := SQLGetInfoSmallint(fhCon, SQL_MAX_COLUMNS_IN_SELECT, GetInfoSmallInt,
            SizeOf(GetInfoSmallInt), nil);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_MAX_COLUMNS_IN_SELECT)',
              SQL_HANDLE_DBC, fhCon, nil);
          Integer(PropValue^) := GetInfoSmallInt;
          Length := SizeOf(Integer);
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get metadata option MetaMaxColumnsInSelect: (%d)', [
              Integer(PropValue^) ]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeMetaMaxRowSize: // Readonly
        if (PropValue <> nil) and (MaxLength >= SizeOf(Integer)) then
        begin
          OdbcRetcode := SQLGetInfoSmallint(fhCon, SQL_MAX_ROW_SIZE, GetInfoSmallInt,
            SizeOf(GetInfoSmallInt), nil);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_MAX_ROW_SIZE)',
              SQL_HANDLE_DBC, fhCon, nil);
          Integer(PropValue^) := GetInfoSmallInt;
          Length := SizeOf(Integer);
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get metadata option MetaMaxRowSize: (%d)', [
              Integer(PropValue^) ]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeMetaMaxSQLLength: // Readonly
        if (PropValue <> nil) and (MaxLength >= SizeOf(Integer)) then
        begin
          OdbcRetcode := SQLGetInfoSmallint(fhCon, SQL_MAX_STATEMENT_LEN, GetInfoSmallInt,
            SizeOf(GetInfoSmallInt), nil);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_MAX_STATEMENT_LEN)',
              SQL_HANDLE_DBC, fhCon, nil);
          Integer(PropValue^) := GetInfoSmallInt;
          Length := SizeOf(Integer);
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get metadata option MetaMaxSQLLength: (%d)', [
              Integer(PropValue^) ]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeMetaObjectQuoteChar: // Readonly
        if (PropValue <> nil) and (MaxLength > 0) then
        begin
          if fWantQuotedTableName then
          begin
            if (MaxLength = 1) then
            begin
              OdbcRetcode := SQLGetInfoString(fhCon, SQL_IDENTIFIER_QUOTE_CHAR,
                @GetInfoStringBuffer, SizeOf(GetInfoStringBuffer), Length);
              if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
                fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_IDENTIFIER_QUOTE_CHAR)',
                  SQL_HANDLE_DBC, fhCon, nil);
              if (GetInfoStringBuffer[0] = ' ') or (GetInfoStringBuffer[0] = cNullAnsiChar) then
              begin
                PAnsiChar(PropValue)^ := cNullAnsiChar;
                fWantQuotedTableName := False;
                Length := 0;
              end
              else
              begin
                AnsiChar(PropValue^) := GetInfoStringBuffer[0];
                Length := 1;
              end
            end
            else
            begin
              OdbcRetcode := SQLGetInfoString(fhCon, SQL_IDENTIFIER_QUOTE_CHAR,
                PropValue, MaxLength, Length);
              if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
                fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_IDENTIFIER_QUOTE_CHAR)',
                  SQL_HANDLE_DBC, fhCon, nil);
            end;
          end
          else
          begin
            PAnsiChar(PropValue)^ := cNullAnsiChar;
            Length := 0;
          end;
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get metadata option MetaObjectQuoteChar: (%s)', [
              string(AnsiChar(PropValue^)) ]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeMetaSQLEscapeChar: // Readonly
        if (PropValue <> nil) and (MaxLength > 0) then
        begin
          if (MaxLength = 1) then
          begin
            OdbcRetcode := SQLGetInfoString(fhCon, SQL_SEARCH_PATTERN_ESCAPE,
              @GetInfoStringBuffer, SizeOf(GetInfoStringBuffer), Length);
            if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
              fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_SEARCH_PATTERN_ESCAPE)',
                SQL_HANDLE_DBC, fhCon, nil);
            AnsiChar(PropValue^) := GetInfoStringBuffer[0];
            Length := 1;
            {$IFDEF _DBXCB_}
            if Assigned(fDbxTraceCallbackEven) then
              DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get metadata option MetaSQLEscapeChar: (%s)', [
                string(AnsiChar(PropValue^)) ]);
            {$ENDIF}
          end
          else
          begin
            OdbcRetcode := SQLGetInfoString(fhCon, SQL_SEARCH_PATTERN_ESCAPE,
              PropValue, MaxLength, Length);
            if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
              fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_SEARCH_PATTERN_ESCAPE)',
                SQL_HANDLE_DBC, fhCon, nil);
            {$IFDEF _DBXCB_}
            if Assigned(fDbxTraceCallbackEven) then
              DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get metadata option MetaSQLEscapeChar: (%s)', [
                string(PAnsiChar(PropValue)^) ]);
            {$ENDIF}
          end;
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeMetaProcSupportsCursor: // Readonly
        // whether stored procedures can return a cursor
        // If ODBC driver indicates support for Stored Procedures,
        // it is assumed that they may return result sets (ie Cursors)
        if (PropValue <> nil) and (MaxLength >= SizeOf(Boolean)) then
        begin
          OdbcRetcode := SQLGetInfoString(fhCon, SQL_PROCEDURES,
            @GetInfoStringBuffer, SizeOf(GetInfoStringBuffer), Length);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_PROCEDURES)',
              SQL_HANDLE_DBC, fhCon, nil);
          if GetInfoStringBuffer[0] = 'Y' then
            Boolean(PropValue^) := True
          else
            Boolean(PropValue^) := False;
          Length := SizeOf(Boolean);
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get metadata option MetaProcSupportsCursor: (%d)', [
              Integer(Boolean(PropValue^)) ]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeMetaProcSupportsCursors: // Readonly
        // whether stored procedures can return multiple cursors
        if (PropValue <> nil) and (MaxLength >= SizeOf(Boolean)) then
        begin
          OdbcRetcode := SQLGetInfoInt(fhCon, SQL_BATCH_SUPPORT, BatchSupport,
            SizeOf(GetInfoSmallInt), nil);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(SQL_BATCH_SUPPORT)',
              SQL_HANDLE_DBC, fhCon, nil);

          if ((BatchSupport and SQL_BS_SELECT_PROC) <> 0) then
            // This indicates that the driver supports batches of procedures
            // that can have result-set generating statements
            Boolean(PropValue^) := True
          else
            Boolean(PropValue^) := False;
          Length := SizeOf(Boolean);
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get metadata option MetaProcSupportsCursor(s): (%d)', [
              Integer(Boolean(PropValue^)) ]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeMetaSupportsTransactions: // Readonly
        if (PropValue <> nil) and (MaxLength >= SizeOf(LongBool)) then
        begin
          // Nested transactions - Not supported by ODBC
          // (N.B. Non-nested transaction support is eMetaSupportsTransaction)
          LongBool(PropValue^) := fSupportsNestedTransactions;//False;
          Length := SizeOf(Boolean);
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get metadata option MetaSupportsTransactions(s): (%d)', [
              Integer(fSupportsNestedTransactions) ]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
{.$IFDEF _D7UP_}
      xeMetaPackageName:
        if (PropValue <> nil) and (MaxLength > 0) then
        begin
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get metadata option MetaPackageName(s): (%s)', [
              '!DBXERR_NOTSUPPORTED' ]);
          {$ENDIF}
          AnsiChar(PropValue^) := cNullAnsiChar;
          Length := 0;
        end
        else
          Result := DBXERR_INVALIDPARAM;
{.$ENDIF}
{.$IFDEF _D7UP_}
      xeMetaDefaultSchemaName:
        begin
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get metadata option MetaDefaultSchemaName(s): (%s)', [
              '!DBXERR_NOTSUPPORTED' ]);
          {$ENDIF}
          Result := DBXERR_NOTSUPPORTED;
        end;
{.$ENDIF}
        else
          Result := DBXERR_INVALIDPARAM;
    end; //of: case
  except
    on E: EDbxNotSupported do
      Result := DBXERR_NOTSUPPORTED;
    on E: EDbxInvalidParam do
      Result := DBXERR_INVALIDPARAM;
    on E: EDbxError do
    begin
      CheckMaxLines(fConnectionErrorLines);
      fConnectionErrorLines.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLConnectionOdbc.GetMetaDataOption', e);  raise; end; end;
    finally LogExitProc('TSQLConnectionOdbc.GetMetaDataOption', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc.GetOption;//(eDOption: TSQLConnectionOption;
//  PropValue: Pointer; MaxLength: Smallint; out Length: Smallint): SQLResult;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  AttrVal: SqlUInteger;
  aDbxConStmt: PDbxConStmt;
  aHConStmt: SqlHDbc;
{.$IFDEF _K3UP_}
  SmallintAttrVal: SqlUSmallint;
{.$ELSE}
{.$IFNDEF _InternalCloneConnection_}
//  SmallintAttrVal: SqlUSmallint;
{.$ENDIF}
{.$ENDIF} // of: $IFDEF _K3UP_}
  // ---
  procedure GetConnectionCustomOptions;
  var
    i: TConnectionOption;
    OptionsString: AnsiString;
  begin
    OptionsString := '';
    for i := Low(TConnectionOption) to High(TConnectionOption) do
    begin
      if cConnectionOptionsTypes[i] <> cot_Bool then
        continue;
      if fConnectionOptions[i] = osOn then
        OptionsString := OptionsString + cConnectionOptionsNames[i] + AnsiChar('=') + cOptCharTrue + ';'
      else
        OptionsString := OptionsString + cConnectionOptionsNames[i] + AnsiChar('=') + cOptCharFalse + ';';
    end;

    // other no boolean option
    OptionsString := OptionsString +
      // Blob Chank Size:
      cConnectionOptionsNames[coBlobChunkSize] + AnsiChar('=') + AnsiString(IntToStr(fBlobChunkSize) + ';') +
      // Network Packet Size:
      cConnectionOptionsNames[coNetwrkPacketSize] + AnsiChar('=') + AnsiString(IntToStr(fNetwrkPacketSize) + ';') +
      // Lock Mode:
      cConnectionOptionsNames[coLockMode] + AnsiChar('=') + AnsiString(IntToStr(fLockMode) + ';') +
      // Catalog Prefix
      cConnectionOptionsNames[coCatalogPrefix] + AnsiChar('=') + fOdbcCatalogPrefix;
      // Decimal Separator
      //if fDecimalSeparator <> cDecimalSeparatorDefault then
      //  cConnectionOptionsNames[coNumericSeparator] + '=' + fDecimalSeparator;

    // make result from string:
    GetStringOptions(Self, OptionsString, PAnsiChar(PropValue), MaxLength, Length, eiConnCustomInfo);
  end;
  // ---
  procedure GetDatabaseNameOption;
  var
    S: AnsiString;
    ConnectAttrLength: SqlUInteger;
  begin
     // ???:
     //
     //  OdbcApi.pas:
     //
     // Deprecated defines from prior versions of ODBC
     //SQL_DATABASE_NAME = 16; // Use SQLGetConnectOption/SQL_CURRENT_QUALIFIER
     //
     //  ->:
     //
     // eConnDatabaseName == xeConnCatalogName
     //

     {
     GetCurrentCatalog(GetCurrentConnectionHandle);
     if Self.fSupportsCatalog then
       GetStringOptions(fCurrentCatalog, PAnsiChar(PropValue), MaxLength, Length)
     else
       Length := 0;
     //}
     aDbxConStmt := GetCurrentDbxConStmt(aHConStmt);
     SetLength(S, fOdbcMaxCatalogNameLen);
     with fOwnerDbxDriver.fOdbcApi do
     begin
       OdbcRetcode := SQLGetConnectAttr(aHConStmt, SQL_DATABASE_NAME, PAnsiChar(S),
         MaxLength, @ConnectAttrLength);
       if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
         fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetConnectAttr(fhCon, SQL_DATABASE_NAME)',
           SQL_HANDLE_DBC, aHConStmt, aDbxConStmt);
     end;
     if (ConnectAttrLength >= 0) and (ConnectAttrLength <= fOdbcMaxCatalogNameLen) then
       SetLength(S, ConnectAttrLength)
     else // returned uncorrected length value
       S := StrPas( PAnsiChar(S) );
    {$IFDEF _DBXCB_}
    if Assigned(fDbxTraceCallbackEven) then
      DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnDatabaseName: %s', [S]);
    {$ENDIF}
     GetStringOptions(Self, S, PAnsiChar(PropValue), MaxLength, Length, eiConnDatabaseName);
  end;
  // ---
var
  xeDOption: TXSQLConnectionOption absolute eDOption;
  // ---
{$IFDEF _DBXCB_}
  function GetTransactionTypeName: AnsiString;
  begin
    case TTransIsolationLevel(PropValue^) of
      xilREPEATABLEREAD:
        Result := 'xilREPEATABLEREAD';
      xilREADCOMMITTED:
        Result := 'xilREADCOMMITTED';
      xilDIRTYREAD:
        Result := 'xilDIRTYREAD';
      else
        Result := AnsiString(IntToStr(Integer(PropValue^)));
    end;
  end;
  // ---
{$ENDIF}
{$IFDEF _TRACE_CALLS_}
  function Result2Str: AnsiString;
  begin
    case xeDOption of
      xeConnAutoCommit,
      xeConnBlockingMode:
        Result := AnsiString(BoolToStr(Boolean(PropValue^)));
      xeConnBlobSize:
        Result := AnsiString(IntToStr(Integer(PropValue^)));
      xeConnTxnIsoLevel:
        case TTransIsolationLevel(PropValue^) of
          xilREPEATABLEREAD:
            Result := 'xilREPEATABLEREAD';
          xilREADCOMMITTED:
            Result := 'xilREADCOMMITTED';
          xilDIRTYREAD:
            Result := 'xilDIRTYREAD';
          else
            Result := AnsiString(IntToStr(Integer(PropValue^)));
        end;
      xeConnServerVersion,
      xeConnDatabaseName,
      xeConnServerCharSet,
      xeConnObjectQuoteChar,
      xeConnConnectionName:
        Result := AnsiString(StrPas(PAnsiChar(PropValue)));
      xeConnSupportsTransaction,
      xeConnMultipleTransaction,
      xeConnTrimChar:
        Result := AnsiString(BoolToStr(Boolean(PropValue^)));
      xeConnQualifiedName,
      xeConnCatalogName,
      xeConnSchemaName,
      xeConnObjectName,
      xeConnQuotedObjectName,
      xeConnCustomInfo,
      xeConnConnectionString:
        Result := AnsiString(StrPas(PAnsiChar(PropValue)));
      xeConnDecimalSeparator:
        Result := PAnsiChar(PropValue)^;
      xeVendorProperty:
        Result := AnsiString(StrPas(PAnsiChar(PropValue)));
      else
        Result := AnsiString(IntToStr(Longint(PropValue^)));
    end; //of: case
  end;
{$ENDIF _TRACE_CALLS_}
  // ---
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
    try try
    {$R+}
    LogEnterProc('TSqlConnectionOdbc.GetOption','eDOption = '+cSQLConnectionOption[xeDOption]);
    {$IFDEF RANGECHECKS_OFF} {$R-} {$ENDIF}
  {$ENDIF _TRACE_CALLS_}
  with fOwnerDbxDriver.fOdbcApi do
  try
    case xeDOption of
      xeConnAutoCommit:
        if (MaxLength >= SizeOf(Boolean)) and (PropValue <> nil) then
        begin
          aDbxConStmt := GetCurrentDbxConStmt(aHConStmt);
          OdbcRetcode := SQLGetConnectAttr(aHConStmt, SQL_ATTR_AUTOCOMMIT,
            @AttrVal, 0, nil);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetConnectAttr(SQL_ATTR_AUTOCOMMIT)',
              SQL_HANDLE_DBC, aHConStmt, aDbxConStmt);
          Boolean(PropValue^) := (AttrVal = SQL_AUTOCOMMIT_OFF);
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnAutoCommit: %d', [Integer(AttrVal = SQL_AUTOCOMMIT_OFF)]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnBlockingMode:
        // We do not support Asynchronous statement execution
        // From ODBC API:
        // "On multithread operating systems, applications should execute functions on
        // separate threads, rather than executing them asynchronously on the same thread.
        // Drivers that operate only on multithread operating systems
        // do not need to support asynchronous execution."
        if (MaxLength >= SizeOf(Boolean)) and (PropValue <> nil) then
          Boolean(PropValue^) := False
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnBlobSize:
        // "For drivers that don’t provide the available blob size before fetching, this
        // specifies the number of kilobytes of BLOB data that is fetched for BLOB fields.
        // This overrides any value specified at the driver level using eDrvBlobSize."
        if (MaxLength >= SizeOf(Integer)) and (PropValue <> nil) then
        begin
          Integer(PropValue^) := fConnBlobSizeLimitK;
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnBlobSize: %d', [fConnBlobSizeLimitK]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnRoleName:
        // String that specifies the role to use when establishing a connection. (Interbase only)
        raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnRoleName) not ' +
          'supported - Applies to Interbase only');
      xeConnWaitOnLocks:
        // Boolean that indicates whether application should wait until a locked
        // resource is free rather than raise an exception. (Interbase only)
        raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnWaitOnLocks) not ' +
          'supported - Applies to Interbase only');
      xeConnCommitRetain:
        // Cursors dropped after commit
        raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnCommitRetain) not ' +
          'supported - Applies to Interbase only');
      xeConnTxnIsoLevel:
        if (MaxLength >= SizeOf(TTransIsolationLevel)) and (PropValue <> nil) then
        begin
          if fSupportsTransaction then
          begin
            aDbxConStmt := GetCurrentDbxConStmt(aHConStmt);
            OdbcRetcode := SQLGetConnectAttr(aHConStmt, SQL_ATTR_TXN_ISOLATION,
              @AttrVal, 0, nil);
            if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
              fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetConnectAttr(SQL_ATTR_TXN_ISOLATION)',
                SQL_HANDLE_DBC, aHConStmt, aDbxConStmt);
          end
          else
            AttrVal := SQL_TXN_READ_COMMITTED;
          if (AttrVal and SQL_TXN_SERIALIZABLE) <> 0 then
            // Transactions are serializable.
            // Serializable transactions do not allow dirty reads, nonrepeatable reads, or phantoms.
            TTransIsolationLevel(PropValue^) := xilREPEATABLEREAD
          else if (AttrVal and SQL_TXN_REPEATABLE_READ) <> 0 then
            // Dirty reads and nonrepeatable reads are not possible. Phantoms are possible
            TTransIsolationLevel(PropValue^) := xilREPEATABLEREAD
          else if (AttrVal and SQL_TXN_READ_COMMITTED) <> 0 then
            // Dirty reads are not possible. Nonrepeatable reads and phantoms are possible
            TTransIsolationLevel(PropValue^) := xilREADCOMMITTED
          else if (AttrVal and SQL_TXN_READ_UNCOMMITTED) <> 0 then
            // Dirty reads, nonrepeatable reads, and phantoms are possible.
            TTransIsolationLevel(PropValue^) := xilDIRTYREAD;

          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnTxnIsoLevel: %s', [GetTransactionTypeName]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnNativeHandle:
        // The native SQL connection handle (Read-only)
        if (MaxLength >= SizeOf(SqlHDbc)) and (PropValue <> nil) then
        begin
          //OLD:
          //SqlHDbc(PropValue^) := fhCon;
          //NEW:
          ISqlConnectionOdbc(PropValue^) := Self;
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnServerVersion:
        begin
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnServerVersion: %s', [fDbmsVersionString]);
          {$ENDIF}
          //GetConnServerVersion();
          GetStringOptions(Self, fDbmsVersionString, PAnsiChar(PropValue), MaxLength, Length,
            eiConnServerVersion);
        end;
      xeConnCallBack:
        if (MaxLength >= SizeOf(TSQLCallBackEvent)) and (PropValue <> nil) then
          TSQLCallBackEvent(PropValue^) := fDbxTraceCallbackEven
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnCallBackInfo:
        if (MaxLength >= SizeOf(Integer)) and (PropValue <> nil) then
          Integer(PropValue^) := fDbxTraceClientData
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnHostName:
        raise EDbxNotSupported.Create(
          'TSqlConnectionOdbc.GetOption(eConnHostName) not supported - applies to MySql only');
      xeConnDatabaseName: // Readonly
        GetDatabaseNameOption();
      xeConnObjectMode:
        // Boolean value to enable or disable object fields in Oracle8 tables
        raise EDbxNotSupported.Create(
          'TSqlConnectionOdbc.GetOption(eConnObjectMode) not supported - applies to Oracle only');
      {.$IFDEF _K3UP_}
      xeConnMaxActiveComm:
      {.$ELSE}
      //eConnMaxActiveConnection:
      {.$ENDIF}
        if (MaxLength >= SizeOf(Smallint)) and (PropValue <> nil) then
        begin
          // The maximum number of active commands that can be executed by a single connection.
          // Read-only.
          // If database does not support multiple statements, we internally clone
          // connection, so return 0 to DbExpress (unlimited statements per connection)
          if fConnectionOptions[coInternalCloneConnection] = osOn then
            Smallint(PropValue^) := 0
          else
          begin
            // Old code below commented out, v1.04:
            // Back in again v2.04, _InternalCloneConnection_ can now be turned off
            if not fConnected then
            begin
              try
                // We cannot determine this setting until after we have connected
                // Normally we should raise an exception and return error code,
                // but unfortunately SqlExpress calls this option BEFORE connecting
                // so we'll just raise a WARNING, set return value of 1
                // (ie assume only 1 concurrent connection), and Success code
                Smallint(PropValue^) := 1;
                raise EDbxOdbcWarning.Create(
                  'TSqlConnectionOdbc.GetOption(eConnMaxActiveConnection) called, but not connected');
              except
                on EDbxOdbcWarning do
                  ;
              end;
            end
            else
            begin
              // The maximum number of active commands that can be executed by a single connection.
              // Read-only.
              Smallint(PropValue^) := fStatementPerConnection;
              {
              OdbcRetcode := SQLGetInfoSmallint(fhCon, SQL_MAX_CONCURRENT_ACTIVITIES,
                SmallintAttrVal, 2, nil);
              if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
                fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetConnectAttr(SQL_MAX_CONCURRENT_ACTIVITIES)',
                  SQL_HANDLE_DBC, fhCon);
              Smallint(PropValue^) := SmallintAttrVal;
              //}
            end;
          end;
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnMaxActiveComm: %d; Real: %d', [Integer(Smallint(PropValue^)), fStatementPerConnection]);
          {$ENDIF}
          {$IFDEF _TRACE_CALLS_}
          LogInfoProc(['eConnMaxActiveConnection =', Smallint(PropValue^)]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnServerCharSet:
        if (MaxLength > 0) and (PropValue <> nil) then
        begin
          aDbxConStmt := GetCurrentDbxConStmt(aHConStmt);
          OdbcRetcode := SQLGetInfoString(aHConStmt, SQL_COLLATION_SEQ, PropValue,
            MaxLength, Length);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetConnectAttr(SQL_COLLATION_SEQ)',
              SQL_HANDLE_DBC, aHConStmt, aDbxConStmt);
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnServerCharSet: $%x', [Integer(PropValue)]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnSqlDialect:
        // Interbase only
        raise EDbxNotSupported.Create(
          'TSqlConnectionOdbc.GetOption(eConnSqlDialect) not supported - applies to Interbase only');
    {.$IFDEF _K3UP_}
      xeConnRollbackRetain:
        if (MaxLength >= SizeOf(Pointer)) and (PropValue <> nil) then
          Pointer(PropValue^) := nil
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnObjectQuoteChar:
        if (MaxLength > 1) and (PropValue <> nil) then
        begin
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnObjectQuoteChar: %s', [string(fQuoteChar)]);
          {$ENDIF}
          PAnsiChar(PropValue)^ := fQuoteChar;
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnConnectionName:
        if (MaxLength >= 0) then
        begin
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnConnectionName: %s', [fOdbcConnectString]);
          {$ENDIF}
          GetStringOptions(Self, fOdbcConnectString, PAnsiChar(PropValue), MaxLength, Length,
            eiConnConnectionName);
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnOSAuthentication:
        raise EDbxNotSupported.Create(
          'TSqlConnectionOdbc.GetOption(eConnOSAuthentication) not supported');
      xeConnSupportsTransaction: { not use in "SqlExpr.pas" }
        if (MaxLength >= SizeOf(Boolean)) and (PropValue <> nil) then
        begin
          if fConnected or (fhCon = SQL_NULL_HANDLE) then
            Boolean(PropValue^) := fSupportsTransaction
          else
          if fhCon <> SQL_NULL_HANDLE then
          begin
            OdbcRetcode := SQLGetInfoSmallint(fhCon, SQL_TXN_CAPABLE, SmallintAttrVal,
              SizeOf(SmallintAttrVal), nil);
            if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
              fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetInfo(fhCon, SQL_TXN_CAPABLE)',
                SQL_HANDLE_DBC, fhCon, nil);
            Boolean(PropValue^) := SmallintAttrVal <> SQL_TC_NONE;
          end;
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnSupportsTransaction: %d', [Integer(Boolean(PropValue^))]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnMultipleTransaction:
        //raise EDbxNotSupported.Create(
        //  'TSqlConnectionOdbc.GetOption(eConnMultipleTransaction) not supported');
        if (MaxLength >= SizeOf(LongBool)) and (PropValue <> nil) then
        begin
          LongBool(PropValue^) := fSupportsNestedTransactions;
          Length := SizeOf(LongBool);
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnServerPort:
        raise EDbxNotSupported.Create(
          'TSqlConnectionOdbc.GetOption(eConnServerPort) not supported');
      xeConnOnLine:
        raise EDbxNotSupported.Create(
          'TSqlConnectionOdbc.GetOption(eConnOnLine) not supported');
      xeConnTrimChar:
        if (MaxLength >= SizeOf(Boolean)) and (PropValue <> nil) then
        begin
          Boolean(PropValue^) := fConnectionOptions[coTrimChar] = osOn;
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnTrimChar: %d', [Integer(Boolean(PropValue^))]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
    {.$ENDIF} //of: IFDEF _K3UP_
    {.$IFDEF _D7UP_}
      xeConnQualifiedName:
        if (MaxLength >= 0) then
        begin
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnQualifiedName: %s', [fQualifiedName]);
          {$ENDIF}
          GetStringOptions(Self, fQualifiedName, PAnsiChar(PropValue), MaxLength, Length,
            eiConnQualifiedName);
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnCatalogName:
        // Do not cache catalog name, could be changed, eg. by Sql statement USE catalogname
        if (MaxLength >= 0) then
        begin
          if fSupportsCatalog then
          begin
            GetCurrentCatalog(GetCurrentConnectionHandle);
            GetStringOptions(Self, fCurrentCatalog, PAnsiChar(PropValue), MaxLength, Length,
              eiConnCatalogName);
          end
          else
          begin
            GetStringOptions(Self, '', PAnsiChar(PropValue), MaxLength, Length,
              eiConnCatalogName);
          end;
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnCatalogName: %s', [StrPas(PAnsiChar(PropValue))]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnSchemaName:
        if (MaxLength >= 0) then
        begin
          if (fConnectionOptions[coSupportsSchemaFilter] = osOn) then
            GetStringOptions(Self, fCurrentSchema, PAnsiChar(PropValue), MaxLength, Length,
              eiConnSchemaName)
          else
          begin
            GetStringOptions(Self, '', PAnsiChar(PropValue), MaxLength, Length,
              eiConnSchemaName)
          end;
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnSchemaName: %s', [StrPas(PAnsiChar(PropValue))]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnObjectName:
        if (MaxLength >= 0) then
        begin
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnObjectName: %s', [fQualifiedName]);
          {$ENDIF}
          GetStringOptions(Self, fQualifiedName, PAnsiChar(PropValue), MaxLength, Length,
            eiConnObjectName);
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnQuotedObjectName:
        if (MaxLength >= 0) then
        begin
          // This is right for multi-part names
          GetStringOptions(Self,
            GetQuotedObjectName(fQualifiedName),
            PAnsiChar(PropValue),
            MaxLength,
            Length,
            eiConnQuotedObjectName);
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnQuotedObjectName: %s', [StrPas(PAnsiChar(PropValue))]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnCustomInfo:
        if (MaxLength >= 0) then
          GetConnectionCustomOptions
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnTimeout:
        if (MaxLength >= SizeOf(Longint)) and (PropValue <> nil) then
        begin
          if fConnectionTimeout < 0 then
            Longint(PropValue^) := SQL_LOGIN_TIMEOUT_DEFAULT
          else
            Longint(PropValue^) := fConnectionTimeout
        end
        else
          Result := DBXERR_INVALIDPARAM;
    {.$ENDIF} //of: $IFDEF _D7UP_
      xeConnConnectionString:
        if (MaxLength >= 0) then
        begin
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnConnectionString: %s', [fConnConnectionString]);
          {$ENDIF}
          GetStringOptions(Self, fConnConnectionString, PAnsiChar(PropValue), MaxLength, Length,
            eiConnConnectionString);
        end
        else
          Result := DBXERR_INVALIDPARAM;
    {.$IFDEF _D9UP_}
      xeConnTDSPacketSize:
        if (MaxLength >= SizeOf(Longint)) and (PropValue <> nil) then
          Longint(PropValue^) := fNetwrkPacketSize
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnClientHostName:
        raise EDbxNotSupported.Create(
          'TSqlConnectionOdbc.GetOption(eConnClientHostName) not supported');
      xeConnClientAppName:
        raise EDbxNotSupported.Create(
          'TSqlConnectionOdbc.GetOption(eConnClientAppName) not supported');
      xeConnCompressed:
        raise EDbxNotSupported.Create(
          'TSqlConnectionOdbc.GetOption(eConnCompressed) not supported');
      xeConnEncrypted:
        raise EDbxNotSupported.Create(
          'TSqlConnectionOdbc.GetOption(eConnEncrypted) not supported');
      xeConnPrepareSQL:
        if (MaxLength >= SizeOf(Boolean)) and (PropValue <> nil) then
          Boolean(PropValue^) := fPrepareSQL
        else
          Result := DBXERR_INVALIDPARAM;
      xeConnDecimalSeparator:
        if (MaxLength >= SizeOf(Char)) and (PropValue <> nil) then
        begin
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ConnDecimalSeparator: %s', [string(fDecimalSeparator)]);
          {$ENDIF}
          PAnsiChar(PropValue)^ := fDecimalSeparator;
        end
        else
          Result := DBXERR_INVALIDPARAM;
    {.$ENDIF} // of: _D9UP_
    {.$IFDEF _D11UP_}
      xe41:
        Result := DBXERR_NOTSUPPORTED;
      xeVendorProperty:
        if (MaxLength >= 0) then
        begin
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get VendorProperty: %s', [DbxOpenOdbcVersion]);
          {$ENDIF}
          GetStringOptions(Self, DbxOpenOdbcVersion, PAnsiChar(PropValue), MaxLength, Length,
            eiVendorProperty);
        end
        else
          Result := DBXERR_INVALIDPARAM;
    {.$ENDIF} // of: _D11UP_
    else
      raise EDbxInvalidCall.Create('Invalid option passed to TSqlConnectionOdbc.GetOption: ' +
        IntToStr(Ord(eDOption)));
    end; //of: case

    {$IFDEF _TRACE_CALLS_}
      if Result = DBXERR_NONE then
        LogInfoProc('Result.PropValue = "' + Result2Str() + '"')
      else
        LogInfoProc('Result <> SQL_SUCCESS');
    {$ENDIF _TRACE_CALLS_}

  except
    on E: EDbxNotSupported do
      Result := DBXERR_NOTSUPPORTED;
    on E: EDbxInvalidParam do
      Result := DBXERR_INVALIDPARAM;
    on E: Exception{EDbxError} do
    begin
      CheckMaxLines(fConnectionErrorLines);
      fConnectionErrorLines.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.GetOption', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.GetOption', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc.getSQLCommand;//(out pComm: ISQLCommand25): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc.getSQLCommand'); {$ENDIF _TRACE_CALLS_}
  try
    Pointer(pComm) := nil;
    // Cannot get command object until we have successfully connected
    if (not fConnected) or fConnectionClosed then
      raise EDbxInvalidCall.Create('getSQLCommand called but not yet connected');
    pComm := TSqlCommandOdbc.Create(Self);
  except
    on E: Exception{EDbxError} do
    begin
      pComm := nil;
      CheckMaxLines(fConnectionErrorLines);
      fConnectionErrorLines.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.getSQLCommand', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.getSQLCommand', ['Result =', Result, 'pComm =', Pointer(pComm)]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc.getSQLMetaData;//(out pMetaData: ISQLMetaData25): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc.getSQLMetaData'); {$ENDIF _TRACE_CALLS_}
  try
    Pointer(pMetaData) := nil;
    // Cannot get metadata object until we have successfully connected
    if (not fConnected) or fConnectionClosed then
      raise EDbxInvalidCall.Create('getSQLMetaData called but not yet connected');
    pMetaData := TSqlMetaDataOdbc.Create({SupportWideString:}False, Self);
  except
    on E: Exception{EDbxError} do
    begin
      pMetaData := nil;
      CheckMaxLines(fConnectionErrorLines);
      fConnectionErrorLines.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.getSQLMetaData', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.getSQLMetaData', ['Result =', Result, 'pMetaData =', Pointer(pMetaData)]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc.SetOption;//(eConnectOption: TSQLConnectionOption;
//  lValue: LongInt): SQLResult;
var
  AttrValue, AttrValueMain: SqlUInteger;
  aHConStmt: SqlHDbc;
  iDbxConStmt, vCurrDbxConStmt: PDbxConStmt;
  i: Integer;
  S: AnsiString;
  // ---
  { Delphi 2006 contain bug for  ansi drivers. Instead of call SetStringOption it is used SetOption:
    FISQLConnection.SetOption(eConnCustomInfo,
    FISQLConnection.SetOption(eConnDecimalSeparator,
  }
  function GetValueAsAnsiStr: AnsiString;
  begin
    if (fOwnerDbxDriver.fDBXVersion < 30) and  (fOwnerDbxDriver.fClientVersion >= 30) then
      // Delphi 2006 bug: put parameter as unicode for ansi dbx driver.
      Result := AnsiChar(WideChar(PWideChar(lValue)))
    else
      Result := AnsiString(StrPas(PAnsiChar(lValue)));
  end;
  // ---
  {.$IFDEF _K3UP_}
  procedure SetConnectionCustomOptions;
  var
    i: TConnectionOption;
    OptionsString, sValue: AnsiString;
    bIsConStr: Boolean;
    pConnectionOptionsDefault: PConnectionOptions;
  begin
    OptionsString := GetValueAsAnsiStr();
    if Length(OptionsString) = 0 then
      exit;

    pConnectionOptionsDefault := GetDefaultConnectionOptions();

    bIsConStr := False;
    if not fConnected then
    begin
      sValue := LowerCase(Trim(OptionsString));
      if Pos(LowerCase(cConnectionOptionsNames[coConnectionString]) + '=', sValue) = 1 then
      begin
        OptionsString := Copy(Trim(OptionsString), Length(cConnectionOptionsNames[coConnectionString]) + 2, MaxInt );
        fConnConnectionString := OptionsString;
        bIsConStr := True;
      end;
    end;

    // Set Options:
    if Length(OptionsString)>0 then
    for i := Low(TConnectionOption) to High(TConnectionOption) do
    begin
      sValue := GetOptionValue(
          OptionsString,
          {OptionName=}cConnectionOptionsNames[i],
          {HideOption=}True,
          {TrimResult=}True,
          {bOneChar=}False
        );
      if (sValue = '') or (sValue = cNullAnsiChar) then
        continue;
      if SetConnectionOption(
          fConnectionOptions,
          {OptionDriverDefault=}@fConnectionOptionsDrv{need when value = 'x'},
          {Option=}i,
          {Value=}sValue,
          Self
         ) and (
          fConnected
         )
      then
      begin
        if cConnectionOptionsTypes[i] = cot_Bool then
        begin
          if fConnectionOptions[i] = pConnectionOptionsDefault[i] then
            sValue := cNullAnsiChar; // remove option
          // for support cloning connection when returning database connection string
          if GetOptionValue(fOdbcConnectString, cConnectionOptionsNames[i], {HideOption=}True,
            {TrimResult=}False, {bOneChar=}False, {HideTemplate=}sValue) = cNullAnsiChar
          then
          if sValue <> cNullAnsiChar then
            fOdbcConnectString := cConnectionOptionsNames[i] + '=' + sValue + ';' + fOdbcConnectString;
        end
        else
        begin
          case i of
            coBlobChunkSize:
              begin
                if fBlobChunkSize <> cBlobChunkSizeDefault then
                  fOdbcConnectString := cConnectionOptionsNames[i] + AnsiChar('=') +
                    AnsiString(IntToStr(fBlobChunkSize) + ';') + fOdbcConnectString;
              end;
            coNetwrkPacketSize:
              begin
                if fNetwrkPacketSize <> cNetwrkPacketSizeDefault then
                  fOdbcConnectString := cConnectionOptionsNames[i] + AnsiChar('=') +
                    AnsiString(IntToStr(fNetwrkPacketSize) + ';') + fOdbcConnectString;
              end;
            coCatalogPrefix:
              begin
                if CompareText(fOdbcCatalogPrefix, AnsiString('DATABASE')) <> 0 then
                  fOdbcConnectString := cConnectionOptionsNames[i] + AnsiChar('=') +
                    fOdbcCatalogPrefix + AnsiChar(';') + fOdbcConnectString;
              end;
            coLockMode:
              begin
                if fLockMode <> cLockModeDefault then
                  fOdbcConnectString := cConnectionOptionsNames[i] + AnsiChar('=') +
                    AnsiString(IntToStr(fLockMode) + ';') + fOdbcConnectString;
              end;
            else
              continue;
          end;
        end;
      end;
      if Length(OptionsString) = 0 then
        break;
    end;//of: for i
    // Support Delphi 8 Connection Option:

    if not bIsConStr then
      fConnConnectionString := fOdbcConnectString
    else
      fConnConnectionString := OptionsString;
  end;
  // ---
const
  cBoolOptionSwitches: array[Boolean] of TOptionSwitches = (osOff, osOn);
  {.$ENDIF}
  // ---
var
  OdbcRetcode: OdbcApi.SqlReturn;
  xeConnectOption: TXSQLConnectionOption absolute eConnectOption;
  // ---
  {$IFDEF _TRACE_CALLS_}
  function lValue2Str: AnsiString;
  begin
    case xeConnectOption of
      xeConnRoleName:
        Result := StrPas(PAnsiChar(lValue));
      xeConnTxnIsoLevel:
          case TTransIsolationLevel(lValue) of
            xilREPEATABLEREAD:
              Result :=  'xilREPEATABLEREAD - > SQL_TXN_REPEATABLE_READ';
            xilREADCOMMITTED:
              Result :=  'xilREADCOMMITTED - > SQL_TXN_READ_COMMITTED';
            xilDIRTYREAD:
              Result :=  'xilDIRTYREAD - > SQL_TXN_READ_UNCOMMITTED';
          else
            Result := StrPas(PAnsiChar(lValue));
          end;
      xeConnHostName,
      xeConnDatabaseName,
      xeConnServerCharSet,
      xeConnObjectQuoteChar,
      xeConnConnectionName,
      xeConnServerPort,
      xeConnQualifiedName,
      xeConnCatalogName,
      xeConnSchemaName,
      xeConnObjectName,
      xeConnQuotedObjectName,
      xeConnCustomInfo,
      xeConnConnectionString,
      xeConnClientHostName,
      xeConnClientAppName:
        Result := StrPas(PAnsiChar(lValue));
      xeConnDecimalSeparator:
          Result := GetValueAsAnsiStr();
      else
        Result := AnsiString(IntToStr(lValue));
    end;
  end;
  {$ENDIF _TRACE_CALLS_}
  // ---
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
    try try
    {$R+}
    LogEnterProc('TSqlConnectionOdbc.SetOption', ['eConnectOption =', cSQLConnectionOption[xeConnectOption], 'lValue =', lValue2Str()]);
    {$IFDEF RANGECHECKS_OFF} {$R-} {$ENDIF}
  {$ENDIF _TRACE_CALLS_}
  with fOwnerDbxDriver.fOdbcApi do
  try
    case xeConnectOption of
      xeConnAutoCommit:
        begin
          if lValue = 0 then
            AttrValue := SQL_AUTOCOMMIT_OFF
          else
            AttrValue := SQL_AUTOCOMMIT_ON;

          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ConnAutoCommit: %d', [Integer(lValue = 0)]);
          {$ENDIF}

          vCurrDbxConStmt := GetCurrentDbxConStmt(aHConStmt);

          OdbcRetCode := SQLGetConnectAttr(aHConStmt, SQL_ATTR_AUTOCOMMIT, @AttrValueMain, 0, nil);
          if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
            fOwnerDbxDriver.OdbcCheck(OdbcRetCode, 'SQLGetConnectAttr(SQL_ATTR_AUTOCOMMIT)',
              SQL_HANDLE_DBC, aHConStmt, vCurrDbxConStmt);

          if AttrValueMain <> AttrValue then
          begin
            //???
            {if ( fStatementPerConnection > 0)  then
            begin
              for i := fDbxConStmtList.Count-1 downto 0 do
              begin
                iDbxConStmt := fDbxConStmtList[i];
                if (iDbxConStmt = nil) or (iDbxConStmt.fHCon = SQL_NULL_HANDLE)
                  or (iDbxConStmt.fHCon = aHConStmt)
                  or (iDbxConStmt.fInTransaction <> fInTransaction)
                then
                  continue;
                OdbcRetcode := SQLSetConnectAttr(iDbxConStmt.fHCon, SQL_ATTR_AUTOCOMMIT, Pointer(AttrValue), 0);
                if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
                  fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLSetConnectAttr(SQL_ATTR_AUTOCOMMIT)',
                    SQL_HANDLE_DBC, iDbxConStmt.fHCon);
                iDbxConStmt.fAutoCommitMode := AttrValue;
              end;
            end;{}
            OdbcRetcode := SQLSetConnectAttr(aHConStmt, SQL_ATTR_AUTOCOMMIT, Pointer(AttrValue), 0);
            if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
              fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLSetConnectAttr(SQL_ATTR_AUTOCOMMIT)',
                SQL_HANDLE_DBC, aHConStmt, vCurrDbxConStmt);
          end;
          if (vCurrDbxConStmt <> nil)  then
            vCurrDbxConStmt.fAutoCommitMode := AttrValue;
          fAutoCommitMode := AttrValue

        end;
      xeConnBlockingMode:
        // Asynchronous support
        raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnBlockingMode) not valid '
          +
          '(Read-only)');
      xeConnBlobSize:
        begin
          // "For drivers that don’t provide the available blob size before fetching, this
          // specifies the number of kilobytes of BLOB data that is fetched for BLOB fields."
          fConnBlobSizeLimitK := lValue;
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ConnBlobSize: %d', [fConnBlobSizeLimitK]);
          {$ENDIF}
        end;
      xeConnRoleName:
        // String that specifies the role to use when establishing a connection. (Interbase only)
        raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnRoleName) not supported '
          +
          '- Applies to Interbase only');
      xeConnWaitOnLocks:
        begin
          // Boolean that indicates whether application should wait until a locked
          // resource is free rather than raise an exception. (Interbase only)
          //raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnWaitOnLocks) not ' +
          //  'supported - Applies to Interbase only');
          if Boolean(lValue) then
          begin
            fLockMode := -1;
          end
          else
          begin
            if fLockMode < 0 then
              fLockMode := cLockModeDefault;
          end;
        end;
      xeConnCommitRetain:
        // Cursors dropped after commit
        raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnCommitRetain) not ' +
          'supported - Applies to Interbase only');
      xeConnTxnIsoLevel:
        if fConnected and fSupportsTransaction then
        begin
          case TTransIsolationLevel(lValue) of
            // Note that ODBC defines an even higher level of isolation, viz, SQL_TXN_SERIALIZABLE;
            // In this mode, Phantoms are not possible. (See ODBC spec).
            xilREPEATABLEREAD:
              begin
                // Dirty reads and nonrepeatable reads are not possible. Phantoms are possible
                AttrValue := SQL_TXN_REPEATABLE_READ;
                {$IFDEF _DBXCB_}
                S := 'SQL_TXN_REPEATABLE_READ';
                {$ENDIF}
              end;
            xilREADCOMMITTED:
              begin
                // Dirty reads are not possible. Nonrepeatable reads and phantoms are possible
                AttrValue := SQL_TXN_READ_COMMITTED;
                {$IFDEF _DBXCB_}
                S := 'SQL_TXN_READ_COMMITTED';
                {$ENDIF}
              end;
            xilDIRTYREAD:
              begin
                // Dirty reads, nonrepeatable reads, and phantoms are possible.
                AttrValue := SQL_TXN_READ_UNCOMMITTED;
                {$IFDEF _DBXCB_}
                S := 'SQL_TXN_READ_COMMITTED';
                {$ENDIF}
              end;
          else
            begin
              {$IFDEF _DBXCB_}
              S := '?';
              {$ENDIF}
              raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnTxnIsoLevel) ' +
                'invalid isolation value: ' + IntToStr(lValue));
            end;
          end;

          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set transaction type: %s', [S]);
          {$ENDIF}

          //aHConStmt := fhCon;
          vCurrDbxConStmt := GetCurrentDbxConStmt(aHConStmt); // ???: when fStatementPerConnection > 0

          OdbcRetCode := SQLGetConnectAttr(aHConStmt, SQL_ATTR_TXN_ISOLATION, @AttrValueMain, 0, nil);
          if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
            fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLGetConnectAttr(SQL_ATTR_TXN_ISOLATION)',
              SQL_HANDLE_DBC, aHConStmt, vCurrDbxConStmt);

          if AttrValueMain <> AttrValue then
          begin
            if ( fStatementPerConnection > 0)  then
            begin
              for i := fDbxConStmtList.Count-1 downto 0 do
              begin
                iDbxConStmt := fDbxConStmtList[i];
                if (iDbxConStmt = nil) or (iDbxConStmt.fHCon = SQL_NULL_HANDLE)
                  or (iDbxConStmt.fDeadConnection)
                  or (iDbxConStmt.fHCon = aHConStmt)
                  or (iDbxConStmt.fInTransaction <> fInTransaction)
                then
                  continue;
                OdbcRetcode := SQLSetConnectAttr(iDbxConStmt.fHCon, SQL_ATTR_TXN_ISOLATION, Pointer(AttrValue), 0);
                if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
                  fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLSetConnectAttr(SQL_ATTR_TXN_ISOLATION)',
                    SQL_HANDLE_DBC, iDbxConStmt.fHCon, iDbxConStmt);
              end;
            end;
            OdbcRetcode := SQLSetConnectAttr(aHConStmt, SQL_ATTR_TXN_ISOLATION, Pointer(AttrValue), 0);
            if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
              fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLSetConnectAttr(SQL_ATTR_TXN_ISOLATION)',
                SQL_HANDLE_DBC, aHConStmt, vCurrDbxConStmt);
          end;
        end;
      xeConnNativeHandle:
        // The native SQL connection handle (Read-only)
        raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnNativeHandle) not valid '
          +
          '(Read-only)');
      xeConnServerVersion:
        begin
          // The server version (Read-only)
          raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnServerVersion) not valid '
            +
            '(Read-only)');
          // TODO: Connection Shared by interface ISqlConnectionOdbc
        end;
      xeConnCallback:
        begin
          if (lValue <> 0) and ((fOwnerDbxDriver.fDBXVersion < 30) and (fOwnerDbxDriver.fClientVersion = 30)) then
            // QC: 58675: skip Delphi 2006 callback bag for ansi driver
            // detailed explanation for footnote "20080224#1".
          else
          begin
            {$IFDEF _DBXCB_}
            if Assigned(fDbxTraceCallbackEven) and (lValue > 0) and (fDbxTraceClientData > 0) then
            begin
              i := 1;
              DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ConnCallback: conn: "$%x"; callback: $%x; data: $%x', [Integer(Self), Integer(lValue), Integer(fDbxTraceClientData)]);
            end
            else
              i := 0;
            {$ENDIF}
            fDbxTraceCallbackEven := TSQLCallBackEvent(lValue);
            {$IFDEF _DBXCB_}
            if (i = 0) and Assigned(fDbxTraceCallbackEven) and (fDbxTraceClientData > 0) then
              DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ConnCallback: conn: "$%x"; callback: $%x; data: $%x', [Integer(Self), Integer(lValue), Integer(fDbxTraceClientData)]);
            {$ENDIF}
          end;
        end;
      xeConnCallBackInfo:
        begin
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) and (lValue > 0) and (fDbxTraceClientData > 0) then
          begin
            i := 1;
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ConnCallBackInfo: conn: "$%x"; callback: $%x; data: $%x', [Integer(Self), Integer(@fDbxTraceCallbackEven), Integer(lValue)]);
          end
          else
            i := 0;
          {$ENDIF}
          fDbxTraceClientData := lValue;
          {$IFDEF _DBXCB_}
          if (i = 0) and Assigned(fDbxTraceCallbackEven) and (fDbxTraceClientData > 0) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ConnCallBackInfo: conn: "$%x"; callback: $%x; data: $%x', [Integer(Self), Integer(@fDbxTraceCallbackEven), Integer(lValue)]);
          {$ENDIF}
        end;
      xeConnHostName:
        raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnHostName) not valid ' +
          '(Read-only)');
      xeConnDatabaseName: // Readonly
        raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnDatabaseName) not valid '
          +
          '(Read-only)');
      xeConnObjectMode:
        // Boolean value to enable or disable object fields in Oracle8 tables
        raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnObjectMode) not ' +
          'supported - applies to Oracle only');
    {.$IFDEF _K3UP_}
    // This was renamed in Kylix3/Delphi7
      xeConnMaxActiveComm:
    {.$ELSE}
    //eConnMaxActiveConnection:
    {.$ENDIF}
        raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnMaxActiveConnection) not '
          +
          'valid (Read-only)');
      xeConnServerCharSet:
        raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnServerCharSet) not valid '
          +
          '(Read-only)');
      xeConnSqlDialect:
        // Interbase only
        raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnSqlDialect) not ' +
          'supported - applies to Interbase only');
      {+2.01 New options for Delphi 7}
    {.$IFDEF _K3UP_}
      xeConnRollbackRetain:
        raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnRollbackRetain) ' +
          'not supported');
      xeConnObjectQuoteChar:
        raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnObjectQuoteChar) not ' +
          'valid (Read-only)');
      xeConnConnectionName:
        raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnConnectionName) not ' +
          'valid (Read-only');
      xeConnOSAuthentication:
        raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnOSAuthentication) not '
          +
          'supported');
      xeConnSupportsTransaction:
        raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnSupportsTransaction) ' +
          'not supported');
      xeConnMultipleTransaction:
        raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnMultipleTransaction) ' +
          'not supported');
      xeConnServerPort:
        raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnServerPort) not ' +
          'supported');
      xeConnOnLine: ;
      //raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnOnLine) not supported');
      xeConnTrimChar:
        fConnectionOptions[coTrimChar] := cBoolOptionSwitches[Boolean(lValue)];
    {.$ENDIF} //of: IFNDEF _K3UP_
    {.$IFDEF _D7UP_}
      xeConnQualifiedName:
        begin
          fQualifiedName := StrPas(PAnsiChar(lValue));
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ConnQualifiedName begin: %s', [fQualifiedName]);
          {$ENDIF}
          // Fixed QC2289:
          if (Length(fQualifiedName) > 0) and (fQualifiedName[1] = '.') then
            fQualifiedName := Copy(fQualifiedName, 2, Length(fQualifiedName) - 1);
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ConnQualifiedName end: %s', [fQualifiedName]);
          {$ENDIF}
        end;
      xeConnCatalogName:
        if fSupportsCatalog then
        begin
          {+2.03}
          // Vadim> Error if NewCatalog=Currentcatalog (informix, mssql)
          // Edward> ???Ed>Vad I still don't think code below is correct

          S := StrPas(PAnsiChar(lValue));

          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ConnCatalogName begin: %s', [S]);
          {$ENDIF}

          S := ExtractCatalog(S, fOdbcCatalogPrefix);
          if ( S <> '' ) then
          begin
            if not fConnected then
            begin
              fDbxCatalog := S;
              fCurrentCatalog := S;
            end
            else
            begin
              vCurrDbxConStmt := GetCurrentDbxConStmt(aHConStmt); // {fhCon} ???: when fStatementPerConnection > 0
              GetCurrentCatalog(aHConStmt);
              if fSupportsCatalog and
                // catalog name <> current catalog
                ( CompareText(fCurrentCatalog, S) <> 0 ) then
              begin
                if ( fStatementPerConnection > 0)  then // set new Current catalog for all cached connection (when it connection is same transaction state)
                begin
                  for i := fDbxConStmtList.Count-1 downto 0 do
                  begin
                    iDbxConStmt := fDbxConStmtList[i];
                    if (iDbxConStmt = nil) or (iDbxConStmt.fHCon = SQL_NULL_HANDLE)
                      or (iDbxConStmt.fHCon = aHConStmt) // We skip the current connection. It will be is processed the last.
                      or (iDbxConStmt.fInTransaction <> fInTransaction)
                    then
                      continue;
                    if CompareText(GetCatalog(iDbxConStmt.fHCon), S) <> 0 then
                    begin
                      OdbcRetcode := SQLSetConnectAttr(iDbxConStmt.fHCon, SQL_ATTR_CURRENT_CATALOG,
                        PAnsiChar(S), SQL_NTS);
                      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
                        fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLSetConnectAttr(SQL_ATTR_CURRENT_CATALOG)',
                          SQL_HANDLE_DBC, iDbxConStmt.fHCon, iDbxConStmt);
                    end;
                  end;
                end;
                //  Processing of the current connection:
                OdbcRetcode := SQLSetConnectAttr(aHConStmt, SQL_ATTR_CURRENT_CATALOG,
                  PAnsiChar(S), SQL_NTS);
                if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
                  fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLSetConnectAttr(SQL_ATTR_CURRENT_CATALOG)',
                    SQL_HANDLE_DBC, aHConStmt, vCurrDbxConStmt);
                fCurrentCatalog := S;
              end;
            end;
          end;

          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ConnCatalogName end: %s', [fCurrentCatalog]);
          {$ENDIF}

          {/+2.03}
        end;
      xeConnSchemaName: ;
      //raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnSchemaName) not supported');
      xeConnObjectName: ;
      //raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnObjectName) not supported');
      xeConnQuotedObjectName: ;
      //raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnQuotedObjectName) not supported');
      xeConnCustomInfo:
        SetConnectionCustomOptions();
      xeConnTimeout:
        begin
          if not fConnected then
          begin
            if lValue >= 0 then
              fConnectionTimeout := lValue
            else
              fConnectionTimeout := cConnectionTimeoutDefault;
          end;
        end;
    {.$ENDIF} //of: IFDEF _D7UP_
      xeConnConnectionString:
        if not fConnected then
        begin
          fConnConnectionString := StrPas(PAnsiChar(lValue));
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ConnConnectionString: %s', [fConnConnectionString]);
          {$ENDIF}
        end;
    {.$IFDEF _D9UP_}
      xeConnTDSPacketSize:
        begin
          if lValue >= cNetwrkPacketSizeMin then
            SetConnectionOption(fConnectionOptions, nil, coNetwrkPacketSize, AnsiString(IntToStr(lValue)), Self);
        end;
      xeConnClientHostName:
        begin
          raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnClientHostName)'
            + ' not supported');
        end;
      xeConnClientAppName:
        begin
          raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnClientAppName)'
            + ' not supported');
        end;
      xeConnCompressed:
        begin
          raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnCompressed)'
            + ' not supported');
        end;
      xeConnEncrypted:
        begin
          raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnEncrypted)'
            + ' not supported');
        end;
      xeConnPrepareSQL:
        fPrepareSQL := Boolean(lValue);
      xeConnDecimalSeparator:
        begin
          S := GetValueAsAnsiStr();
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ConnDecimalSeparator begin: %s', [S]);
          {$ENDIF}
          if S <> '' then
          begin
            if not (S[1] in ['0'..'9', '+', '-', 'E', 'e']) then
            begin
              if S[1] = cOptCharDefault {'X'} then
              begin
                fDecimalSeparator := cDecimalSeparatorDefault;
                fConnectionOptions[coNumericSeparator] := osDefault;
              end
              else
              begin
                fDecimalSeparator := S[1];
                fConnectionOptions[coNumericSeparator] := osOff;
              end;
            end
            else
            begin
              //Result := DBXERR_INVALIDPARAM;
              raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnDecimalSeparator) not ' +
                'supported separator value : "' + Char(lValue) + '"');
            end;
          end;
          {$IFDEF _DBXCB_}
          if Assigned(fDbxTraceCallbackEven) then
            DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ConnDecimalSeparator end: %s', [string(fDecimalSeparator)]);
          {$ENDIF}
        end;
    {.$ENDIF} // of: _D9UP_
    else
      raise EDbxInvalidCall.Create('Invalid option passed to TSqlConnectionOdbc.SetOption: ' +
        IntToStr(Ord(eConnectOption)));
    end;
  except
    on E: Exception{EDbxError} do
    begin
      CheckMaxLines(fConnectionErrorLines);
      fConnectionErrorLines.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.SetOption', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.SetOption', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlConnectionOdbc.TransactionCheck;//(const DbxConStmtInfo: TDbxConStmtInfo);
var
  OdbcRetcode: OdbcApi.SqlReturn;
  aHConStmt: SqlHDbc;
  AttrVal: SqlUInteger;
  vCurrDbxConStmt: PDbxConStmt;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc.TransactionCheck'); {$ENDIF _TRACE_CALLS_}
  if (fStatementPerConnection = 0) or (DbxConStmtInfo.fDbxConStmt = nil) then
  begin
    if (not fSupportsTransaction)or(fInTransaction>0) then
      exit; // It's OK - already in a transaction
    //if (fAutoCommitMode = SQL_AUTOCOMMIT_ON) then
    //  exit; // It's OK - already in AutoCommit mode
    if (fAutoCommitMode = SQL_AUTOCOMMIT_ON) then
      exit;
    aHConStmt := Self.fhCon;
    vCurrDbxConStmt := nil;
  end
  else
  with DbxConStmtInfo do
  begin
    if (not fSupportsTransaction)or(fDbxConStmt.fInTransaction>0) then
      exit; // It's OK - already in a transaction
    //if (fAutoCommitMode = SQL_AUTOCOMMIT_ON) then
    //  exit; // It's OK - already in AutoCommit mode
    if (fDbxConStmt.fAutoCommitMode = SQL_AUTOCOMMIT_ON) then
      exit;
    aHConStmt := fDbxConStmt.fHCon;
    vCurrDbxConStmt := fDbxConStmt
  end;

  with fOwnerDbxDriver.fOdbcApi do
  begin

  AttrVal := SQL_AUTOCOMMIT_OFF;
  OdbcRetCode := SQLGetConnectAttr(aHConStmt, SQL_ATTR_AUTOCOMMIT, @AttrVal, 0, nil);
  if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
    fOwnerDbxDriver.OdbcCheck(OdbcRetCode, 'TransactionCheck - SQLGetConnectAttr(SQL_ATTR_AUTOCOMMIT)',
      SQL_HANDLE_DBC, aHConStmt, vCurrDbxConStmt);

  if AttrVal <> SQL_AUTOCOMMIT_ON then
  begin
    OdbcRetcode := SQLSetConnectAttr(aHConStmt, SQL_ATTR_AUTOCOMMIT,
      Pointer(SqlUInteger(SQL_AUTOCOMMIT_ON)), 0);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      fOwnerDbxDriver.OdbcCheck(OdbcRetcode, 'SQLSetConnectAttr(SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_ON)',
        SQL_HANDLE_DBC, aHConStmt, vCurrDbxConStmt);
  end;
  if DbxConStmtInfo.fDbxConStmt <> nil then
    DbxConStmtInfo.fDbxConStmt.fAutoCommitMode := SQL_AUTOCOMMIT_ON;
  fAutoCommitMode := SQL_AUTOCOMMIT_ON;

  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.TransactionCheck', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.TransactionCheck'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc.GetDbmsType: TDbmsType;
begin
  Result := fDbmsType;
end;

function TSqlConnectionOdbc.GetOdbcDriverType: TOdbcDriverType;
begin
  Result := fOdbcDriverType;
end;

procedure TSqlConnectionOdbc.GetOdbcConnectStrings;//(ConnectStringList: TStrings);
var
  i: Integer;
  s: AnsiString;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc.GetOdbcConnectStrings'); {$ENDIF _TRACE_CALLS_}
  if ConnectStringList = nil then
    ConnectStringList := TStringList.Create;
  s := '';
  ConnectStringList.BeginUpdate;
  for i := 1 to Length(fOdbcConnectString) do
  begin
    s := s + fOdbcConnectString[i];
    if fOdbcConnectString[i] = ';' then
    begin
      ConnectStringList.Add(string(s));
      s := '';
    end;
  end;
  if s <> '' then
    ConnectStringList.Add(string(s));
  ConnectStringList.EndUpdate;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.GetOdbcConnectStrings', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.GetOdbcConnectStrings'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc.GetLastOdbcSqlState: PAnsiChar;
begin
  Result := @fSqlStateChars;
end;

function TSqlConnectionOdbc.GetDbmsName: AnsiString;
begin
  Result := fDbmsName;
end;

function TSqlConnectionOdbc.GetDbmsVersionMajor: Integer;
begin
  Result := fDbmsVersionMajor;
end;

function TSqlConnectionOdbc.GetDbmsVersionMinor: Integer;
begin
  Result := fDbmsVersionMinor;
end;

function TSqlConnectionOdbc.GetDbmsVersionRelease: Integer;
begin
  Result := fDbmsVersionRelease;
end;

function TSqlConnectionOdbc.GetDbmsVersionBuild: Integer;
begin
  Result := fDbmsVersionBuild;
end;

function TSqlConnectionOdbc.GetDbmsVersionString: AnsiString;
begin
  Result := fDbmsVersionString;
end;

function TSqlConnectionOdbc.GetOdbcDriverName: AnsiString;
begin
  Result := fOdbcDriverName;
end;

function TSqlConnectionOdbc.GetOdbcDriverVersionMajor: Integer;
begin
  Result := fOdbcDriverVersionMajor;
end;

function TSqlConnectionOdbc.GetOdbcDriverVersionMinor: Integer;
begin
  Result := fOdbcDriverVersionMinor;
end;

function TSqlConnectionOdbc.GetOdbcDriverVersionRelease: Integer;
begin
  Result := fOdbcDriverVersionRelease;
end;

function TSqlConnectionOdbc.GetOdbcDriverVersionBuild: Integer;
begin
  Result := fOdbcDriverVersionBuild;
end;

function TSqlConnectionOdbc.GetOdbcDriverVersionString: AnsiString;
begin
  Result := fOdbcDriverVersionString;
end;

function TSqlConnectionOdbc.GetOdbcConnectString: AnsiString;
begin
  Result := fOdbcConnectString;
end;

function TSqlConnectionOdbc.GetCursorPreserved: Boolean;
begin
  Result := fCursorPreserved;
end;

function TSqlConnectionOdbc.GetIsSystemODBCManager: Boolean;
begin
  Result := fOwnerDbxDriver.fOdbcApi.SystemODBCManager;
end;

function TSqlConnectionOdbc.GetOdbcDriverLevel: Integer;
begin
  Result := fOdbcDriverLevel;
end;

function TSqlConnectionOdbc.GetSupportsSqlPrimaryKeys: Boolean;
begin
  Result := fSupportsSQLPRIMARYKEYS;
end;

function TSqlConnectionOdbc.GetStatementsPerConnection: Integer;
begin
  Result := fStatementPerConnection;
end;

function TSqlConnectionOdbc.GetEnvironmentHandle: Pointer;
begin
  Result := fOwnerDbxDriver.fhEnv;
end;

function TSqlConnectionOdbc.GetConnectionHandle: Pointer;
begin
  Result := fhCon;
end;

function TSqlConnectionOdbc.GetOdbcApiIntf: IUnknown;
begin
  Result := fOwnerDbxDriver.fOdbcApi.GetOdbcApiIntf;
end;

function TSqlConnectionOdbc.GetDecimalSeparator: AnsiChar;
begin
  Result := fDecimalSeparator;
end;

{ TSqlCommandOdbc }

{$hints off} // OdbcRetcode := ...
constructor TSqlCommandOdbc.Create;//(OwnerDbxConnection: TSqlConnectionOdbc);
var
  OdbcRetcode: OdbcApi.SqlReturn;
  AttrValue: SqlUInteger;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc(AnsiString(ClassName) + '.Create'); {$ENDIF _TRACE_CALLS_}
  inherited Create;
  fHStmt := SQL_NULL_HANDLE;
  fOwnerDbxConnection := OwnerDbxConnection;
  fOwnerDbxDriver := fOwnerDbxConnection.fOwnerDbxDriver;
  fCommandBlobSizeLimitK := fOwnerDbxConnection.fConnBlobSizeLimitK;
  fDbxConStmtInfo.fDbxConStmt := nil;
  fDbxConStmtInfo.fDbxHStmtNode := nil;
  fSupportsBlockRead := OwnerDbxConnection.fSupportsBlockRead;
  fSupportsMixedFetch := False;
  fCommandRowSetSize := 1;
  fIsMoreResults := -1;
  DoAllocateStmt();
{$IFDEF _K3UP_}
  //Support Trim of Fixed Char when connection parameter "Trim Char" is True
  fTrimChar := fOwnerDbxConnection.fConnectionOptions[coTrimChar] = osOn;
{$ENDIF}
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc(AnsiString(ClassName) + '.Create', e);  raise; end; end;
    finally LogExitProc(AnsiString(ClassName) + '.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;
{$hints on}

procedure TSqlCommandOdbc.DoAllocateStmt;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  AttrValue: SqlUInteger;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCommandOdbc.DoAllocateStmt'); {$ENDIF _TRACE_CALLS_}
  // Clear Stmt
  fPreparedOnly := False;
  fExecutedOk := False;
  if (fHStmt <> SQL_NULL_HANDLE) then
    CloseStmt({bClearParams:}False);
  // Allocate Stmt
  fOwnerDbxConnection.AllocHStmt(fHStmt, @fDbxConStmtInfo);
  fIsMoreResults := -1;
  if fStoredProc = 2 then
    fStoredProc := 0;
  // set stmt attributes
  with fOwnerDbxDriver.fOdbcApi do
  if (not fOwnerDbxConnection.fCursorPreserved)
    and SQLFunctionSupported(fOwnerDbxConnection.fhCon, SQL_API_SQLGETSTMTATTR) then
  begin
    AttrValue := 1;
    OdbcRetcode := SQLGetStmtAttr(fHStmt, SQL_ATTR_MAX_ROWS,
      SqlPointer(@AttrValue), 0{SizeOf(AttrValue)}, nil);
    if (OdbcRetcode = OdbcApi.SQL_SUCCESS) and (AttrValue <> 0{SQL_MAX_ROWS_DEFAULT}) then
    begin
      // Default value for SQL_ATTR_MAX_ROWS is zero (SQL_MAX_ROWS_DEFAULT): the driver returs all rows:
      OdbcRetcode := SQLSetStmtAttr( fHStmt, SQL_ATTR_MAX_ROWS, SqlPointer(0), 0 );
    end;
    // clear last error:
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, nil, fOwnerDbxConnection, Self, nil, 1);
  end;
  if fCommandRowSetSize > 1 then
  begin
    fExecutedOk := False;                      //!!!: Otherwise it will not be set fCommandRowSetSize.
    try
      OdbcRetcode := fCommandRowSetSize;
      SetOption(eCommRowsetSize, OdbcRetcode); //!!!
    finally
      fExecutedOk := True;                     //!!!: restore fExecutedOk
    end;
  end;
  // set query Timeouts:
  if fOwnerDbxConnection.fLockMode > 0 then
    SetLockTimeout(fOwnerDbxConnection.fLockMode + 1);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.DoAllocateStmt', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.DoAllocateStmt'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCommandOdbc.CloseStmt;//(bClearParams: Boolean = True; bFreeStmt: Boolean = True);
var
  OdbcRetcode: OdbcApi.SqlReturn;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCommandOdbc.CloseStmt', ['bClearParams =', bClearParams, 'bFreeStmt =', bFreeStmt]); {$ENDIF _TRACE_CALLS_}
  fPreparedOnly := False;
  fExecutedOk := False;
  if (fHStmt <> SQL_NULL_HANDLE) then
  with fOwnerDbxDriver.fOdbcApi do
  begin
    if (scsStmtExecuted in fStmtStatus) then
    begin
//
// SQLMoreResults:
//  http://publib.boulder.ibm.com/infocenter/db2luw/v8/index.jsp?topic=/com.ibm.db2.udb.doc/ad/r0000628.htm
//  If SQLCloseCursor() or if SQLFreeStmt() is called with the SQL_CLOSE option, or SQLFreeHandle() is called with
//  HandleType set to SQL_HANDLE_STMT, all pending result sets on this statement handle are discarded.
//
      if scsIsCursor in fStmtStatus then
      begin
        fIsMoreResults := -1;
        {$IFDEF _DBXCB_}
        if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
          fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'stmt (close cursor): "$%x"', [Integer(fHStmt)]);
        {$ENDIF}
        OdbcRetcode := SQLCloseCursor(fHStmt);
        if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
          // clear last error:
          fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, nil, fOwnerDbxConnection, Self, nil, 1);
      end;

      OdbcRetcode := SQLFreeStmt(fHStmt, SQL_CLOSE);
      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        //OdbcCheck(OdbcRetcode, 'SQLFreeStmt(SQL_CLOSE)');
        // clear last error:
        fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, nil, fOwnerDbxConnection, Self, nil, 1);
    end;
    if (scsStmtBinded in fStmtStatus) then
    begin
      OdbcRetcode := SQLFreeStmt(fHStmt, SQL_UNBIND);
      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        //OdbcCheck(OdbcRetcode, 'SQLFreeStmt(SQL_UNBIND)');
        // clear last error:
        fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, nil, fOwnerDbxConnection, Self, nil, 1);

      OdbcRetcode := SQLFreeStmt(fHStmt, SQL_RESET_PARAMS);
      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        //OdbcCheck(OdbcRetcode, 'SQLFreeStmt(SQL_RESET_PARAMS)');
        // clear last error:
        fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, nil, fOwnerDbxConnection, Self, nil, 1);
    end;
    fStmtStatus := [];
    // calls freehandle & sets SQL_NULL_HANDLE
    if bFreeStmt then
    begin
      fIsMoreResults := -1;
      fOwnerDbxConnection.FreeHStmt(fHStmt, @fDbxConStmtInfo);
    end;
    if fStoredProc = 2 then
      fStoredProc := 0;
  end;

  if bClearParams then
    ClearParams();

  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.CloseStmt', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.CloseStmt'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCommandOdbc.ClearParams;
var
  i: Integer;
  aOdbcBindParam: TOdbcBindParam;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCommandOdbc.ClearParams'); {$ENDIF _TRACE_CALLS_}
  if (fOdbcParamList <> nil) then
  begin
    for i := fOdbcParamList.Count - 1 downto 0 do
    begin
      aOdbcBindParam := TOdbcBindParam(fOdbcParamList[i]);
      fOdbcParamList[i] := nil;
      aOdbcBindParam.Free;
    end;
    FreeAndNil(fOdbcParamList);
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.ClearParams', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.ClearParams'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TSqlCommandOdbc.Destroy;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc(AnsiString(ClassName) + '.Destroy'); {$ENDIF _TRACE_CALLS_}
  Close();
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc(AnsiString(ClassName) + '.Destroy', e);  raise; end; end;
    finally LogExitProc(AnsiString(ClassName) + '.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCommandOdbc.AddError;//(eError: Exception);
begin
  CheckMaxLines(fOwnerDbxConnection.fConnectionErrorLines);
  fOwnerDbxConnection.fConnectionErrorLines.Add(eError.Message);
end;

procedure TSqlCommandOdbc.OdbcCheck;//(OdbcCode: SqlReturn; const OdbcFunctionName: AnsiString; eTraceCat: TRACECat = cTDBXTraceFlags_none);
begin
  fOwnerDbxDriver.OdbcCheck(OdbcCode, OdbcFunctionName, SQL_HANDLE_STMT, fHStmt,
    fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, Self, nil, 0, eTraceCat);
end;

function TSqlCommandOdbc.close: SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCommandOdbc.Close'); {$ENDIF _TRACE_CALLS_}
  if (fOwnerDbxConnection <> nil) and (fOwnerDbxConnection.fLastStoredProc = Self) then
    fOwnerDbxConnection.fLastStoredProc := nil;
  CloseStmt();
  fIsMoreResults := -1;
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.Close', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.Close'); end;
  {$ENDIF _TRACE_CALLS_}
  Result := DBXERR_NONE;
end;

function TSqlCommandOdbc.BuildStoredProcSQL: AnsiString;
var
  i, iParams: Integer;
  S: AnsiString;
begin
  Result := '{';
  if fStoredProcWithResult then
    Result := Result + '? = ';
  Result := Result + 'CALL ';
  if fStoredProcPackName <> '' then
    S := fOwnerDbxConnection.EncodeObjectFullName(
      fCatalogName, fSchemaName, fStoredProcPackName) + '.' + fSql
  else
    S := fOwnerDbxConnection.EncodeObjectFullName(fCatalogName, fSchemaName, fSql);
  S := fOwnerDbxConnection.GetQuotedObjectName(S, {StoredProcSpace:}True,
     // without ORACLE quotations:
     {WithoutQuotation:}fOwnerDbxConnection.fWantQuotedTableName and (fOwnerDbxConnection.fDbmsType = eDbmsTypeOracle));
  Result := Result + S;
  Result := Result + '(';

  if fOdbcParamList <> nil then
    iParams := fOdbcParamList.Count
  else
    iParams := 0;
  if iParams > 0 then begin
    if fStoredProcWithResult then
      Dec(iParams);
    if iParams > 0 then begin
      for i := 1 to iParams do begin
        if i > 1 then
          Result := Result + ', ';
        Result := Result + '?';
      end;
    end;
  end;
  Result := Result + ')}';
  {$IFDEF _DBXCB_}
  if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
    fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'BuildStoredProcSQL: %s', [Result]);
  {$ENDIF}
end;

function TSqlCommandOdbc.DoSQLMoreResults: OdbcApi.SqlReturn;
const
  iLoopLimit = 17000;
var
  iCount: Integer;
  OdbcNumCols: SqlSmallint;
begin
  Result := OdbcApi.SQL_NO_DATA;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCommandOdbc.DoSQLMoreResults'); {$ENDIF _TRACE_CALLS_}
  with fOwnerDbxDriver.fOdbcApi do
  begin
    //
    // OLD:
    //
    {
    Result := SQLMoreResults(fHStmt);
    if Result <> OdbcApi.SQL_SUCCESS then
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(Result, SQL_HANDLE_STMT, fHStmt, fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, Self, nil, 1);
    Exit;
    {}
    //
    // NEW:
    //
    iCount := 0;
    OdbcNumCols := 0;
    //
    // skip result sets without columns
    //
    while OdbcNumCols <= 0 do
    begin
      Result := SQLMoreResults(fHStmt);
      if Result <> OdbcApi.SQL_SUCCESS then
      begin
        if (Result = OdbcApi.SQL_ERROR) or (Result = OdbcApi.SQL_INVALID_HANDLE) then
          OdbcCheck(Result, 'SQLMoreResults')
        else
          fOwnerDbxDriver.RetrieveOdbcErrorInfo(Result, SQL_HANDLE_STMT, fHStmt, fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, Self, nil, 1);
        Exit;
      end;
      Result := SQLNumResultCols(fHStmt, OdbcNumCols);
      if Result <> OdbcApi.SQL_SUCCESS then
      begin
        fOwnerDbxDriver.RetrieveOdbcErrorInfo(Result, SQL_HANDLE_STMT, fHStmt, fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, Self, nil, 1);
        Exit;
      end;
      inc(iCount);
      if iCount > iLoopLimit then
      begin
        Result := OdbcApi.SQL_NO_DATA;
        Break;
      end;
    end; // of: while
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.DoSQLMoreResults', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.DoSQLMoreResults', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc.DoExecute;//(var Cursor: ISQLCursor25; bUseUnicodeOdbc: Boolean): SQLResult;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  OdbcNumCols: SqlSmallint;
  OdbcRowsAffected: SqlInteger;
  // ---
  procedure DoReBindParams;
  var
    i: integer;
    aOdbcBindParam: TOdbcBindParam;
  begin
    //if Assigned(fOdbcParamList) then
    begin
      {$IFDEF _DBXCB_}
      if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) and (fOdbcParamList.Count > 0) then
        fOwnerDbxConnection.DbxCallBackSendMsg(cTDBXTraceFlags_Vendor, FormatParameters(Self));
      {$ENDIF}
      for i := 1 to fOdbcParamList.Count do
      begin
        aOdbcBindParam := TOdbcBindParam(fOdbcParamList[i-1]);
        with fOwnerDbxDriver.fOdbcApi, aOdbcBindParam do
        begin
{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
{$IFDEF _TRACE_CALLS_}
          LogInfoProc([
            'SQLBindParameter: stmt = $', IntToHex(Integer(fHStmt), 8),
            'num =', IntToStr(i),
            'IOtype =', IntToStr(fOdbcInputOutputType),
            'ValType =', IntToStr(fOdbcParamCType),
            'ParType =', IntToStr(fOdbcParamSqlType),
            'ColSize =', IntToStr(fOdbcParamCbColDef),
            'DecDig =', IntToStr(fOdbcParamIbScale),
            'Val = $', IntToHex(Integer(fBindData), 8),
            'BufLen =', IntToStr(fBindOutputBufferLength),
            'StrLen_Ind =', IntToStr(fOdbcParamLenOrInd)
          ]);
{$ENDIF _TRACE_CALLS_}
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
          OdbcRetcode := SQLBindParameter(
            fHStmt,
            i,
            fOdbcInputOutputType,
            fOdbcParamCType,
            fOdbcParamSqlType,
            fOdbcParamCbColDef, fOdbcParamIbScale,
            fBindData,
            fBindOutputBufferLength, @fOdbcParamLenOrInd);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            OdbcCheck(OdbcRetcode, 'SQLBindParameter');
          Include(fStmtStatus, scsStmtBinded);
        end;
      end;
    end;
  end;
  // ---
  procedure DoPrepareNow;
  begin
    // reallocate SqlHStmt:
    DoAllocateStmt();
    fSqlPrepared := '';
    fSqlPreparedW := '';

    // prepare now:
    if bUseUnicodeOdbc then
      OdbcRetcode := DoPrepare(PAnsiChar(PWideChar(fSqlW)), 0,
        {UpdateParams:}False, {bPrepareSQL:}fOwnerDbxConnection.fPrepareSQL, {bUseUnicodeOdbc:} True)
    else
      OdbcRetcode := DoPrepare(PAnsiChar(fSql), 0,
        {UpdateParams:}False, {bPrepareSQL:}fOwnerDbxConnection.fPrepareSQL, {bUseUnicodeOdbc:} False);

    if (OdbcRetcode = DBXERR_NONE) and Assigned(fOdbcParamList) then
      DoReBindParams;
  end;
  // ---
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCommandOdbc.DoExecute', ['UseUnicodeOdbc =', bUseUnicodeOdbc]); {$ENDIF _TRACE_CALLS_}
  Pointer(Cursor) := nil;
  fOdbcRowsAffected := 0;
  with fOwnerDbxDriver.fOdbcApi do
  try
    {+}
    fExecutedOk := False;
    fIsMoreResults := -1;
    if (not fPreparedOnly) or ((fStoredProc = 1) and (fSqlPrepared = '')) then
    begin
      DoPrepareNow;
      if OdbcRetcode <> DBXERR_NONE then
      begin
        fPreparedOnly := False;
        Result := DBX_FIRSTSTATICERRORS;
        Exit;
      end;
    end;
    {+.}

    fExecutedOk := False;
    fOwnerDbxConnection.TransactionCheck(Self.fDbxConStmtInfo);

    if fOwnerDbxConnection.fPrepareSQL then
    begin
      OdbcRetcode := SQLExecute(fHStmt);
      if (OdbcRetcode <> OdbcApi.SQL_NO_DATA) and (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
        OdbcCheck(OdbcRetcode, 'SQLExecute');
    end
    else
    begin
      if bUseUnicodeOdbc then
      begin
        OdbcRetcode := SQLExecDirectW(fHStmt, PAnsiChar(PWideChar(fSqlPreparedW)), SQL_NTSL);
        if (OdbcRetcode <> OdbcApi.SQL_NO_DATA) and (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
          OdbcCheck(OdbcRetcode, 'SQLExecDirectW');
      end
      else
      begin
        OdbcRetcode := SQLExecDirect(fHStmt, PAnsiChar(fSqlPrepared), SQL_NTS);
        if (OdbcRetcode <> OdbcApi.SQL_NO_DATA) and (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
          OdbcCheck(OdbcRetcode, 'SQLExecDirect');
      end;
    end;

    fPreparedOnly := False;
    Include(fStmtStatus, scsStmtExecuted);

    // Get no of columns:
    OdbcNumCols := 0;
    OdbcRetcode := SQLNumResultCols(fHStmt, OdbcNumCols);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    begin
      //OdbcCheck(OdbcRetcode, 'SQLNumResultCols in TSqlCommandOdbc.execute');
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, Self, nil, 1);
      OdbcNumCols := 0;
    end;

    OdbcRowsAffected := 0;
    OdbcRetcode := SQLRowCount(fHStmt, OdbcRowsAffected);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    begin
      //OdbcCheck(OdbcRetcode, 'SQLRowCount in TSqlCommandOdbc.execute');
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, Self, nil, 1);
      OdbcRowsAffected := 0;
    end;
    if OdbcRowsAffected > 0 then
      fOdbcRowsAffected := OdbcRowsAffected
    else
      fOdbcRowsAffected := 0;

    {$IFDEF _DBXCB_}
    if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
      fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'SQLNumResultCols: %d; SQLRowCount: %d', [OdbcNumCols, OdbcRowsAffected]);
    {$ENDIF}

    if (OdbcRowsAffected > 0) then
    begin
      if not (
        // bug: SQLite return in OdbcRowsAffected then count of selected rows.
        (OdbcNumCols > 0)
        and
        (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeSQLite)
      ) then
      begin
        if fOwnerDbxConnection.fStatementPerConnection = 0 then
        begin
          if (fOwnerDbxConnection.fInTransaction > 0) then
            inc(fOwnerDbxConnection.fRowsAffected, OdbcRowsAffected)
          else
            fOwnerDbxConnection.fRowsAffected := OdbcRowsAffected;
        end
        else
        begin
          if (fDbxConStmtInfo.fDbxConStmt.fInTransaction = fOwnerDbxConnection.fInTransaction)
          then
          begin
            if (fOwnerDbxConnection.fInTransaction > 0) then
              inc(fOwnerDbxConnection.fRowsAffected, OdbcRowsAffected)
            else
              fOwnerDbxConnection.fRowsAffected := OdbcRowsAffected;
          end;
          if (fDbxConStmtInfo.fDbxConStmt.fInTransaction > 0) then
            inc(fDbxConStmtInfo.fDbxConStmt.fRowsAffected, OdbcRowsAffected)
          else
            fDbxConStmtInfo.fDbxConStmt.fRowsAffected := OdbcRowsAffected;
        end;
      end;
    end;

    if (OdbcNumCols = 0) then
    begin
      if (OdbcRowsAffected > 0) and (fOwnerDbxConnection.fStatementPerConnection > 0)
        and ( fDbxConStmtInfo.fDbxConStmt.fInTransaction = fOwnerDbxConnection.fInTransaction)
      then
        fOwnerDbxConnection.fCurrDbxConStmt := fDbxConStmtInfo.fDbxConStmt;

      {+}
      //if OdbcNumCols = 0 then
      begin
        OdbcRetcode := DoSQLMoreResults();
        fIsMoreResults := 0;
        if (OdbcRetcode = OdbcApi.SQL_SUCCESS) then
        begin
          fIsMoreResults := 1;
          OdbcNumCols := 0;
          OdbcRetcode := SQLNumResultCols(fHStmt, OdbcNumCols);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            OdbcCheck(OdbcRetcode, 'SQLNumResultCols in TSqlCommandOdbc.DoExecute');
          {$IFDEF _DBXCB_}
          if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
            fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'IsMoreResults=1; SQLNumResultCols: %d', [OdbcNumCols]);
          {$ENDIF}
          if (OdbcNumCols > 0) then
          begin
            if fStoredProc = 0 then
              fStoredProc := 2;
            {$IFDEF _DBX30_}
            if fOwnerDbxDriver.fDBXVersion >= 30 then
              ISQLCursor30(Cursor) := TSqlCursorOdbc3.Create(Self)
            else
            {$ENDIF}
              Cursor := TSqlCursorOdbc.Create(Self);
          end;
        end;
      end;
      {+.}
    end
    else
    begin
      {+} // 2008-02-02: was not tested on server different from MSSQL 2000
      //if (fStoredProc = 0) and ( (fOwnerDbxConnection.fStatementPerConnection > 0)
      //  or (fOwnerDbxConnection.fOdbcDriverType in [eOdbcDriverTypeMsSqlServer, eOdbcDriverTypeMsSqlServer2005Up]) )
      //then
      if (fStoredProc = 0) then
        fStoredProc := 2;
      {+.}
      {$IFDEF _DBX30_}
      if fOwnerDbxDriver.fDBXVersion >= 30 then
        ISQLCursor30(Cursor) := TSqlCursorOdbc3.Create(Self)
      else
      {$ENDIF}
        Cursor := TSqlCursorOdbc.Create(Self);
    end;

    fExecutedOk := True;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      Cursor := nil;
      AddError(E);
      Result := DBX_FIRSTSTATICERRORS;
      // unprepare stmt when error:
      DoUnprepareStmt();
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.DoExecute', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.DoExecute', ['Result =', Result, 'CursorPtr =', Pointer(Cursor)]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc.execute;//(var Cursor: ISQLCursor25): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCommandOdbc.execute'); {$ENDIF _TRACE_CALLS_}
  {$IFDEF _DBXCB_}
  if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
    fOwnerDbxConnection.DbxCallBackSendMsg(cTDBXTraceFlags_Execute, fSQL);
  {$ENDIF}

  Result := DoExecute(Cursor, {bUseUnicodeOdbc:}False);

  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.execute', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.execute', ['Result =', Result, 'CursorPtr =', Pointer(Cursor)]); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCommandOdbc.DoUnprepareStmt;
begin
  if fHStmt <> SQL_NULL_HANDLE then
    DoAllocateStmt()
  else if fStoredProc = 2 then
    fStoredProc := 0;
  fPreparedOnly := False;
  fExecutedOk := False;
  fIsMoreResults := -1;
end;

function TSqlCommandOdbc.DoExecuteImmediate;//(SQL: PAnsiChar; var Cursor: ISQLCursor25; bUseUnicodeOdbc: Boolean): SQLResult;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  OdbcNumCols: SqlSmallint;
  OdbcRowsAffected: SqlInteger;
begin
  {$IFDEF _TRACE_CALLS_}
  Result := DBXERR_NONE;
  try try LogEnterProc('TSqlCommandOdbc.DoExecuteImmediate', ['SQL =', StrPtrToString(SQL, bUseUnicodeOdbc), 'UseUnicodeOdbc =', bUseUnicodeOdbc]);
  {$ENDIF _TRACE_CALLS_}
  Pointer(Cursor) := nil;
  fOdbcRowsAffected := 0;
  with fOwnerDbxDriver.fOdbcApi do
  try
    {+}
    if (fStoredProc = 2) and (fHStmt <> SQL_NULL_HANDLE) then
    begin
      ClearParams();
      DoAllocateStmt();
    end
    else
    if scsIsCursor in fStmtStatus then
    begin
      DoAllocateStmt(); // Close Cursor
    end;
    {+.}
    fExecutedOk := False;
    if bUseUnicodeOdbc then
    begin
      fSqlW := WideString(PWideChar(SQL));
      fSql := AnsiString(fSqlW);
    end
    else
      fSql := StrPas(SQL);

    fPreparedOnly := False;
    fOwnerDbxConnection.TransactionCheck(Self.fDbxConStmtInfo);
    fStoredProcWithResult := False;
    if fStoredProc = 1 then
    begin
      bUseUnicodeOdbc := False;
      fSqlPrepared := BuildStoredProcSQL;
      if bUseUnicodeOdbc then
        fSqlPreparedW := WideString(fSqlPrepared);
    end
    else
    begin
      fSqlPrepared := fSql;
      if bUseUnicodeOdbc then
        fSqlPreparedW := fSqlW;
    end;

    fIsMoreResults := -1;
    if bUseUnicodeOdbc then
    begin
      OdbcRetcode := SQLExecDirectW(fHStmt, PAnsiChar(PWideChar(fSqlPreparedW)), SQL_NTSL);
      // Some ODBC drivers return SQL_NO_DATA if update/delete statement did not
      // update/delete any rows
      if (OdbcRetcode <> OdbcApi.SQL_NO_DATA) and (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
        OdbcCheck(OdbcRetcode, 'SQLExecDirectW', cTDBXTraceFlags_Execute);
    end
    else
    begin
      OdbcRetcode := SQLExecDirect(fHStmt, PAnsiChar(fSqlPrepared), SQL_NTS);
      // Some ODBC drivers return SQL_NO_DATA if update/delete statement did not
      // update/delete any rows
      if (OdbcRetcode <> OdbcApi.SQL_NO_DATA) and (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
        OdbcCheck(OdbcRetcode, 'SQLExecDirect', cTDBXTraceFlags_Execute);
    end;

    {+}
    Include(fStmtStatus, scsStmtExecuted);
    {+.}

    // Get no of columns:
    OdbcNumCols := 0;
    OdbcRetcode := SQLNumResultCols(fHStmt, OdbcNumCols);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    begin
      //OdbcCheck(OdbcRetcode, 'SQLNumResultCols in TSqlCommandOdbc.ExecuteImmediate');
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, Self, nil, 1);
      OdbcNumCols := 0;
    end;

    OdbcRowsAffected := 0;
    OdbcRetcode := SQLRowCount(fHStmt, OdbcRowsAffected);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    begin
      //OdbcCheck(OdbcRetcode, 'SQLRowCount in TSqlCommandOdbc.ExecuteImmediate');
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, Self, nil, 1);
      OdbcRowsAffected := 0;
    end;
    if OdbcRowsAffected > 0 then
      fOdbcRowsAffected := OdbcRowsAffected
    else
      fOdbcRowsAffected := 0;

    {$IFDEF _DBXCB_}
    if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
      fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'SQLNumResultCols: %d; SQLRowCount: %d', [OdbcNumCols, OdbcRowsAffected]);
    {$ENDIF}

    if (OdbcRowsAffected > 0) then
    begin
      if not (
        // bug: SQLite return in OdbcRowsAffected then count of selected rows.
        (OdbcNumCols > 0)
        and
        (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeSQLite)
      ) then
      begin
        if fOwnerDbxConnection.fStatementPerConnection = 0 then
        begin
          if (fOwnerDbxConnection.fInTransaction > 0) then
            inc(fOwnerDbxConnection.fRowsAffected, OdbcRowsAffected)
          else
            fOwnerDbxConnection.fRowsAffected := OdbcRowsAffected;
        end
        else
        begin
          if (fDbxConStmtInfo.fDbxConStmt.fInTransaction = fOwnerDbxConnection.fInTransaction)
          then
          begin
            if (fOwnerDbxConnection.fInTransaction > 0) then
              inc(fOwnerDbxConnection.fRowsAffected, OdbcRowsAffected)
            else
              fOwnerDbxConnection.fRowsAffected := OdbcRowsAffected;
          end;
          if (fDbxConStmtInfo.fDbxConStmt.fInTransaction > 0) then
            inc(fDbxConStmtInfo.fDbxConStmt.fRowsAffected, OdbcRowsAffected)
          else
            fDbxConStmtInfo.fDbxConStmt.fRowsAffected := OdbcRowsAffected;
        end;
      end;
    end;

    if (OdbcNumCols = 0) then
    begin
      Cursor := nil;
      if (OdbcRowsAffected > 0) and (fOwnerDbxConnection.fStatementPerConnection > 0)
        and ( fDbxConStmtInfo.fDbxConStmt.fInTransaction = fOwnerDbxConnection.fInTransaction)
      then
        fOwnerDbxConnection.fCurrDbxConStmt := fDbxConStmtInfo.fDbxConStmt;

      {+}
      if OdbcNumCols = 0 then
      begin
        OdbcRetcode := DoSQLMoreResults();
        fIsMoreResults := 0;
        if (OdbcRetcode = OdbcApi.SQL_SUCCESS) then
        begin
          fIsMoreResults := 1;
          OdbcNumCols := 0;
          OdbcRetcode := SQLNumResultCols(fHStmt, OdbcNumCols);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            OdbcCheck(OdbcRetcode, 'SQLNumResultCols in TSqlCommandOdbc.DoExecuteImmediate');
          {$IFDEF _DBXCB_}
          if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
            fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'IsMoreResults=1; SQLNumResultCols: %d', [OdbcNumCols]);
          {$ENDIF}
          if (OdbcNumCols > 0) then
          begin
            if fStoredProc = 0 then
              fStoredProc := 2;
            {$IFDEF _DBX30_}
            if fOwnerDbxDriver.fDBXVersion >= 30 then
              ISQLCursor30(Cursor) := TSqlCursorOdbc3.Create(Self)
            else
            {$ENDIF}
              Cursor := TSqlCursorOdbc.Create(Self);
          end;
        end;
      end;
      {+.}

    end
    else
    begin
      {+} // 2008-02-02: was not tested on server different from MSSQL 2000
      //if (fStoredProc = 0) and ( (fOwnerDbxConnection.fStatementPerConnection > 0)
      //  or (fOwnerDbxConnection.fOdbcDriverType in [eOdbcDriverTypeMsSqlServer, eOdbcDriverTypeMsSqlServer2005Up]) )
      //then
      if (fStoredProc = 0) then
        fStoredProc := 2;
      {+.}
      {$IFDEF _DBX30_}
      if fOwnerDbxDriver.fDBXVersion >= 30 then
        ISQLCursor30(Cursor) := TSqlCursorOdbc3.Create(Self)
      else
      {$ENDIF}
        Cursor := TSqlCursorOdbc.Create(Self);
    end;

    fExecutedOk := True;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      Cursor := nil;
      AddError(E);
      Result := DBX_FIRSTSTATICERRORS;
      // unprepare stmt when error:
      DoUnprepareStmt();
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.DoExecuteImmediate', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.DoExecuteImmediate', ['Result =', Result, 'CursorPtr =', Pointer(Cursor)]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc.ExecuteImmediate;//(SQL: PAnsiChar; var Cursor: ISQLCursor25): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCommandOdbc.ExecuteImmediate', ['SQL =', StrPas(SQL)]); {$ENDIF _TRACE_CALLS_}
  {$IFDEF _DBXCB_}
  if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
    fOwnerDbxConnection.DbxCallBackSendMsg(cTDBXTraceFlags_Execute, StrPas(SQL));
  {$ENDIF}

  if SQL <> nil then
    Result := DoExecuteImmediate(SQL, Cursor, {bUseUnicodeOdbc:} False)
  else
    Result := DBXERR_INVALIDPARAM;

  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.ExecuteImmediate', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.ExecuteImmediate', ['Result =', Result, 'CursorPtr =', Pointer(Cursor)]); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCommandOdbc.Cancel;
var
  OdbcRetcode: OdbcApi.SqlReturn;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCommandOdbc.Cancel'); {$ENDIF _TRACE_CALLS_}
  with fOwnerDbxDriver.fOdbcApi do
  begin

  OdbcRetcode := SQLCancel(fHStmt);
  if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    OdbcCheck(OdbcRetcode, 'SQLPrepare');
  //Close();

  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.Cancel', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.Cancel'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc.GetConnectionHandle: SqlHDbc;
begin
  if fDbxConStmtInfo.fDbxConStmt = nil then
    Result := fOwnerDbxConnection.fhCon
  else
    Result := fDbxConStmtInfo.fDbxConStmt.fHCon;
end;

function TSqlCommandOdbc.SetLockTimeout;//(TimeoutSeconds: Integer): Boolean;
var
  vTimeoutSeconds, StmtValue: SQLUINTEGER;
  OdbcRetcode: OdbcApi.SqlReturn;
begin
  {$IFDEF _TRACE_CALLS_} Result := False; try try LogEnterProc('TSqlCommandOdbc.SetLockTimeout', ['TimeoutSeconds =', TimeoutSeconds]); {$ENDIF _TRACE_CALLS_}
  with fOwnerDbxDriver.fOdbcApi do
  begin

  Result := False;
  if fExecutedOk then
    exit;
  // Set Timeout to the number of seconds to wait for an SQL statement to execute before returning to the application
  if TimeoutSeconds < 0 then
  begin
    Result := True;
    Exit;
  end;
  vTimeoutSeconds := TimeoutSeconds;

  StmtValue := SQL_QUERY_TIMEOUT_DEFAULT;
  OdbcRetcode := SQLGetStmtAttr( fHStmt, SQL_ATTR_QUERY_TIMEOUT,
    SQLPOINTER( @StmtValue ), 0{SizeOf(StmtValue)}, nil );
  if (OdbcRetcode = OdbcApi.SQL_SUCCESS) and (StmtValue <> vTimeoutSeconds) then
  begin
    OdbcRetcode := SQLSetStmtAttr( fHStmt, SQL_ATTR_QUERY_TIMEOUT, SqlPointer(vTimeoutSeconds), 0 );
    Result := OdbcRetcode = OdbcApi.SQL_SUCCESS;
  end;
  if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, Self, nil, 1);

  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.SetLockTimeout', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.SetLockTimeout', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc.GetLockTimeout: Integer;
var
  StmtValue: SQLUINTEGER;
  OdbcRetcode: OdbcApi.SqlReturn;
begin
  {$IFDEF _TRACE_CALLS_} Result := -1; try try LogEnterProc('TSqlCommandOdbc.GetLockTimeout'); {$ENDIF _TRACE_CALLS_}
  with fOwnerDbxDriver.fOdbcApi do
  begin

  if (fHStmt <> SQL_NULL_HANDLE) then
  begin
    StmtValue := SQL_QUERY_TIMEOUT_DEFAULT;
    OdbcRetcode := SQLGetStmtAttr( fHStmt, SQL_ATTR_QUERY_TIMEOUT,
      SQLPOINTER( @StmtValue ), 0{SizeOf(StmtValue)}, nil );
    if (OdbcRetcode = OdbcApi.SQL_SUCCESS) then
    begin
      Result := StmtValue;
    end
    else
    begin
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, Self, nil, 1);
      Result := -1;
    end;
  end
  else
  begin
    Result := fOwnerDbxConnection.fLockMode;
  end;

  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.GetLockTimeout', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.GetLockTimeout', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc.getErrorMessage;//(Error: PAnsiChar): SQLResult;
begin
  if Error <> nil then
    StrCopy(Error, PAnsiChar(AnsiString(fOwnerDbxConnection.fConnectionErrorLines.Text)));
  fOwnerDbxConnection.fConnectionErrorLines.Clear;
  Result := DBXERR_NONE;
end;

function TSqlCommandOdbc.getErrorMessageLen;//(out ErrorLen: Smallint): SQLResult;
begin
  ErrorLen := Length(fOwnerDbxConnection.fConnectionErrorLines.Text);
  Result := DBXERR_NONE;
end;

function TSqlCommandOdbc.getNextCursor;//(var Cursor: ISQLCursor25): SQLResult;
{ TODO : getNextCursor - THIS HAS NOT BEEN TESTED }
var
  OdbcRetcode: OdbcApi.SqlReturn;
  OdbcNumCols: SqlSmallint;
//  aOdbcBindParam: TOdbcBindParam;
//  aSqlCursorOdbc: TSqlCursorOdbc;
//  i, iCursor: integer;
//  aSqlHStmt: SqlHStmt;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCommandOdbc.getNextCursor'); {$ENDIF _TRACE_CALLS_}
  Pointer(Cursor) := nil;
  with fOwnerDbxDriver.fOdbcApi do
  try
    {$IFDEF _DBXCB_}
    if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
      fOwnerDbxConnection.DbxCallBackSendMsg(cTDBXTraceFlags_Vendor, 'ISQLCursor.getNextCursor');
    {$ENDIF}
    if (fIsMoreResults = 0) or (fHStmt = SQL_NULL_HANDLE) then
    begin
      Result := DBX_SQL_NULL_DATA;
      Exit;
    end;

    if fIsMoreResults <> 2 then
      OdbcRetcode := DoSQLMoreResults()
    else
      OdbcRetcode := OdbcApi.SQL_SUCCESS;

    // Minimization of use of cursors.
    // It is critical when fStatementPerConnection is very small (SQL Server).
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    begin
      fIsMoreResults := 0;
      if (fOwnerDbxConnection.fStatementPerConnection > 0)
        and  // Restriction on quantity SqllHStmt is exhausted:
        (fDbxConStmtInfo.fDbxConStmt.fSqlHStmtAllocated = fOwnerDbxConnection.fStatementPerConnection)
      then
      begin
        CloseStmt({ClearParams}False, {True}True);
      end;
      Result := DBX_SQL_NULL_DATA;
      Exit;
    end;

    // Code below is the same as for Execute...

    fIsMoreResults := 1;
    // Get number of columns:
    OdbcNumCols := 0;
    if fHStmt <> SQL_NULL_HSTMT then
    begin
      OdbcRetcode := SQLNumResultCols(fHStmt, OdbcNumCols);
      if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
        OdbcCheck(OdbcRetcode, 'SQLNumResultCols in TSqlCommandOdbc.getNextCursor');
    end;

    if (OdbcNumCols = 0) then
    begin
      //fIsMoreResults := 0;
      Result := DBX_SQL_NULL_DATA;
    end
    else
    begin
      if fStoredProc = 0 then
        fStoredProc := 2;
      {$IFDEF _DBX30_}
      if fOwnerDbxDriver.fDBXVersion >= 30 then
        ISQLCursor30(Cursor) := TSqlCursorOdbc3.Create(Self)
      else
      {$ENDIF}
        Cursor := TSqlCursorOdbc.Create(Self);
    end;

  except
    on E: Exception{EDbxError} do
    begin
      Cursor := nil;
      AddError(E);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.getNextCursor', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.getNextCursor', ['Result =', Result, 'CursorPtr =', Pointer(Cursor)]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc.GetOption;//(eSqlCommandOption: TSQLCommandOption;
// Borland changed GetOption function prototype between Delphi V6 and V7
// Kylix 3 uses Delphi 6 prototype
//{$IFDEF _D7UP_}
//  PropValue: Pointer;
//{$ELSE}
//  var pValue: Integer;
//{$ENDIF}
//  MaxLength: Smallint; out Length: Smallint): SQLResult;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  ValueLength: SqlSmallint;
{$IFDEF _D7UP_}
  xPropValue: Pointer absolute PropValue;
{$ELSE}
  xPropValue: Pointer; // this method is not used in Delphi6 "SqlExpr.pas".
{$ENDIF}
  xeSqlCommandOption: TXSQLCommandOption absolute eSqlCommandOption;

begin
  Result := DBXERR_NONE;
  {$IFNDEF _D7UP_}
  xPropValue := @pValue;
  {$ENDIF}
  {$IFDEF _TRACE_CALLS_}
    try try
    {$R+}
    LogEnterProc('TSqlCommandOdbc.GetOption', ['eSqlCommandOption =', cSQLCommandOption[xeSqlCommandOption], 'pPropValue =', xPropValue, 'MaxLength=', MaxLength]);
    {$IFDEF RANGECHECKS_OFF} {$R-} {$ENDIF}
  {$ENDIF _TRACE_CALLS_}
  with fOwnerDbxDriver.fOdbcApi do
  try
    Length := 0;
    case xeSqlCommandOption of
      xeCommRowsetSize:
        if (xPropValue <> nil) and (MaxLength >= SizeOf(Integer)) then
        begin // New for Delphi 6.02
          Integer(xPropValue^) := fCommandRowSetSize;
          Length := SizeOf(Integer);
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeCommBlobSize:
        if (xPropValue <> nil) and (MaxLength >= SizeOf(Integer)) then
        begin
          Integer(xPropValue^) := fCommandBlobSizeLimitK;
          Length := SizeOf(Integer);
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeCommBlockRead:
        if (xPropValue <> nil) and (MaxLength >= SizeOf(Boolean)) then
        begin
          Boolean(xPropValue^) := fSupportsBlockRead;
          Length := SizeOf(Boolean);
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeCommBlockWrite:
        raise EDbxNotSupported.Create(
          'TSqlCommandOdbc.GetOption(eCommBlockWrite) not yet implemented');
      xeCommParamCount:
        if (xPropValue <> nil) and (MaxLength >= SizeOf(Integer)) then
        begin
          if (fOdbcParamList <> nil) and (fOdbcParamList.Count > 0) then
          begin
            { ???
            if fStoredProcWithResult then
              Integer(xPropValue^) := fOdbcParamList.Count - 1
            else
            {}
            Integer(xPropValue^) := fOdbcParamList.Count;
          end
          else
            Integer(xPropValue^) := 0;
          Length := SizeOf(Integer);
          {$IFDEF _DBXCB_}
          if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
            fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ISQLCursor CommParamCount: %d', [Integer(xPropValue^)]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeCommNativeHandle:
        if (xPropValue <> nil) and (MaxLength >= SizeOf(Integer)) then
        begin
          Integer(xPropValue^) := Integer(fHStmt);
          Length := SizeOf(Integer);
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeCommCursorName:
        if (MaxLength >= 0) then
        begin
          if (MaxLength > 0) and Assigned(xPropValue) then
          begin
            PAnsiChar(xPropValue)^ := cNullAnsiChar;
            if (MaxLength > 1) then
              PAnsiChar(xPropValue)[1] := cNullAnsiChar;
          end;
          OdbcRetcode := SQLGetCursorName(fHStmt, xPropValue, MaxLength, ValueLength);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            OdbcCheck(OdbcRetcode, 'SQLGetCursorName in TSqlCommandOdbc.GetOption');
          Length := ValueLength;
          {$IFDEF _DBXCB_}
          if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) and (MaxLength > 0) and Assigned(xPropValue) then
            fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ISQLCursor CommCursorName: %s', [StrPas(PAnsiChar(xPropValue))]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeCommStoredProc:
        if (xPropValue <> nil) and (MaxLength >= SizeOf(Integer)) then
        begin
          Integer(xPropValue^) := Integer(fStoredProc = 1);
          Length := SizeOf(Integer);
          {$IFDEF _DBXCB_}
          if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
            fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ISQLCursor CommStoredProc: %d', [Integer(xPropValue^)]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeCommSQLDialect: // INTERBASE ONLY
        raise EDbxInvalidCall.Create(
          'TSqlCommandOdbc.GetOption(eCommSQLDialect) valid only for Interbase');
      xeCommTransactionID:
        // get transaction level for current statement (it is equal global transaction level).
        if (xPropValue <> nil) and (MaxLength >= SizeOf(Integer)) then
        begin
          Integer(xPropValue^) := Self.fOwnerDbxConnection.fOdbcIsolationLevel;
          Length := SizeOf(Integer);
          {$IFDEF _DBXCB_}
          if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
            fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ISQLCursor CommTransactionID: %d', [Integer(xPropValue^)]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
{.$IFDEF _D7UP_}
      xeCommPackageName:
        if MaxLength >= 0 then
        begin
          GetStringOptions(Self, fStoredProcPackName, PAnsiChar(xPropValue), MaxLength, Length,
            eiCommPackageName);
          {$IFDEF _DBXCB_}
          if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
            fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ISQLCursor CommPackageName: %s', [fStoredProcPackName]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeCommTrimChar:
        if (xPropValue <> nil) and (MaxLength >= SizeOf(Integer)) then
        begin
          Integer(xPropValue^) := Integer(fTrimChar);
          Length := SizeOf(Integer);
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeCommQualifiedName:
        raise EDbxNotSupported.Create(
          'TSqlCommandOdbc.GetOption(eCommQualifiedName) not yet implemented');
      xeCommCatalogName:
        if MaxLength >= 0 then
        begin
          GetStringOptions(Self, fCatalogName, PAnsiChar(xPropValue), MaxLength, Length,
            eiCommCatalogName);
          {$IFDEF _DBXCB_}
          if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
            fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ISQLCursor CommCatalogName: %s', [fCatalogName]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeCommSchemaName:
        if MaxLength >= 0 then
        begin
          GetStringOptions(Self, fSchemaName, PAnsiChar(xPropValue), MaxLength, Length,
            eiCommSchemaName);
          {$IFDEF _DBXCB_}
          if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
            fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ISQLCursor CommSchemaName: %s', [fSchemaName]);
          {$ENDIF}
        end
        else
          Result := DBXERR_INVALIDPARAM;
      xeCommObjectName:
        raise EDbxNotSupported.Create(
          'TSqlCommandOdbc.GetOption(eCommObjectName) not yet implemented');
      xeCommQuotedObjectName:
        raise EDbxNotSupported.Create(
          'TSqlCommandOdbc.GetOption(eCommQuotedObjectName) not yet implemented');
{.$ENDIF} //of: IFDEF _D7UP_
{.$IFDEF _D9UP_}
      xeCommPrepareSQL:
        raise EDbxNotSupported.Create(
          'TSqlCommandOdbc.GetOption(eCommPrepareSQL) not yet implemented');
      xeCommDecimalSeparator:
        raise EDbxNotSupported.Create(
          'TSqlCommandOdbc.GetOption(eCommDecimalSeparator) not yet implemented');
{.$ENDIF} //of: IFDEF _D9UP_
    else
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption - Invalid option ' +
        IntToStr(Ord(eSqlCommandOption)));
    end;
  except
    on EDbxNotSupported do
    begin
      Length := 0;
      Integer(xPropValue^) := 0;
      Result := DBXERR_NOTSUPPORTED;
    end;
    on EDbxInvalidParam do
    begin
      Length := 0;
      Result := DBXERR_INVALIDPARAM;
    end;
    on EDbxInvalidCall do
    begin
      Length := 0;
      Integer(xPropValue^) := 0;
      Result := DBXERR_INVALIDPARAM;
    end;
    on E: Exception{EDbxError} do
    begin
      Length := 0;
      Integer(xPropValue^) := 0;
      AddError(E);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.GetOption', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.GetOption', ['Result =', Result, 'Length =', Length]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc.getParameter;//(ParameterNumber, ulChildPos: Word;
//  Value: Pointer; Length: Integer; var IsBlank: Integer): SQLResult;
var
  aOdbcBindParam: TOdbcBindParam;
  vData: Pointer;
  vDataSize: Word;
begin
  Result := DBXERR_NONE;
  IsBlank := 1;
  {$IFDEF _TRACE_CALLS_}
    try try
    LogEnterProc('TSqlCommandOdbc.getParameter', ['ParameterNumber =', ParameterNumber, 'ulChildPos =', ulChildPos]);
  {$ENDIF _TRACE_CALLS_}
  {$IFDEF _DBXCB_}
  if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
    fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ISQLCursor getParameter: %d; enter', [ParameterNumber]);
  {$ENDIF}
  if Value = nil then
  begin
    if Length > 0 then
      Result := DBXERR_INVALIDPARAM;
    Exit;
  end;
  try
    if Length >= SizeOf(Pointer) then
      Pointer(Value^) := nil;
    if ParameterNumber > fOdbcParamList.Count then
      raise EDbxInvalidCall.Create(
        'TSqlConnectionOdbc.getParameter - ParameterNumber exceeds parameter count');
    aOdbcBindParam := TOdbcBindParam(fOdbcParamList.Items[ParameterNumber - 1]);
    {$IFDEF _DBXCB_}
    if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
      fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ISQLCursor getParameter: %d; value: %s',
        [ParameterNumber, FormatParameter(aOdbcBindParam, fOwnerDbxConnection)]);
    {$ENDIF}
    with aOdbcBindParam do
    begin
      if fOdbcParamLenOrInd = OdbcApi.SQL_NULL_DATA then
      begin
        //IsBlank := 1;
        case fDbxType of
          fldZSTRING, fldWIDESTRING, fldUNICODE: {empty} ;
          else
            Exit;
        end;
        vData := nil;
      end
      else
      begin
        IsBlank := 0;
        if fBuffer <> nil then
          vData := fBuffer
        else
          vData := @fValue;
      end;
      case fDbxType of
        fldZSTRING:
          begin
            // handle coEmptyStrParam, coNullStrParam:
            {begin:}
              if (vData = nil) and
                (fOwnerDbxConnection.fConnectionOptions[coNullStrParam] = osOn) then
              begin
                IsBlank := 0;
                Length := 1;
                vData := @cNullAnsiCharBuf;
              end
              else
              if (fOwnerDbxConnection.fConnectionOptions[coEmptyStrParam] = osOn)
                and
                (  // unicode check
                   ( ((fDbxSubType and fldstWIDEMEMO) <> 0) and (PWideChar(vData)^ = cNullWideChar) )
                   or
                   // ansi char check
                   ( PAnsiChar(vData)^ = cNullAnsiChar )
                ) then
              begin
                IsBlank := 1;
                Length := 0;
              end;
            {end.}
            if (vData <> nil) then
            begin
              if (Length > SizeOf(TOdbcBindParamRec)) then
                Length := SizeOf(TOdbcBindParamRec);
              Move(vData^, Value^, Length);
            end;
          end;
        fldWIDESTRING, fldUNICODE:
//todo: ????? not tested it:
          begin
            // handle coEmptyStrParam, coNullStrParam:
            {begin:}
              if (vData = nil) and
                (fOwnerDbxConnection.fConnectionOptions[coNullStrParam] = osOn) then
              begin
                IsBlank := 0;
                Length := SizeOf(WideChar);
                vData := @cNullWideCharBuf;
              end
              else
              if (fOwnerDbxConnection.fConnectionOptions[coEmptyStrParam] = osOn)
                and
                (  // unicode check
                   ( ((fDbxSubType and fldstWIDEMEMO) <> 0) and (PWideChar(vData)^ = cNullWideChar) )
                   or
                   // ansi char check
                   ( PAnsiChar(vData)^ = cNullAnsiChar )
                ) then
              begin
                IsBlank := 1;
                Length := 0;
              end;
            {end.}
            if (vData <> nil) then
            begin
              if (Length > SizeOf(TOdbcBindParamRec)) then
                Length := SizeOf(TOdbcBindParamRec);
//todo: ????? not tested it:
              Move({src}vData^, {dst}Value^, Length);
            end;
          end;
        fldINT32, fldUINT32:
          begin
            Integer(Value^) := fValue.OdbcParamValueInteger;
          end;
        fldINT16, fldUINT16:
          begin
            Smallint(Value^) := fValue.OdbcParamValueShort;
          end;
        fldINT64, fldUINT64:
          begin
            Int64(Value^) := fValue.OdbcParamValueBigInt;
          end;
        fldFLOAT:
          begin
            Double(Value^) := fValue.OdbcParamValueDouble;
          end;
        fldBCD,
        fldFMTBCD:
          { // OLD:
          begin
            SetString(s, fValue.OdbcParamValueString, StrLen(fValue.OdbcParamValueString));
            PBcd(Value)^ := StrToBcd(s);
          end; // }
          Str2BCD(fValue.OdbcParamValueString,
            StrLen(fValue.OdbcParamValueString), PBcd(Value)^, cDecimalSeparatorDefault);
        fldBOOL:
            PWordBool(Value)^ := fValue.OdbcParamValueBit = 1;
        fldDATE:
          with fValue.OdbcParamValueDate do
            PLongWord(Value)^ := Trunc(EncodeDate(Year, Month, Day) + DateDelta);
        fldTIME:
          with fValue.OdbcParamValueTime do
            PLongWord(Value)^ := (Second + Minute * 60 + Hour * 3600) * 1000;
        fldDATETIME:
          with fValue.OdbcParamValueTimeStamp do begin
            PSQLTimeStamp(Value)^.Year := Year;
            PSQLTimeStamp(Value)^.Month := Month;
            PSQLTimeStamp(Value)^.Day := Day;
            PSQLTimeStamp(Value)^.Hour := Hour;
            PSQLTimeStamp(Value)^.Minute := Minute;
            PSQLTimeStamp(Value)^.Second := Second;
            PSQLTimeStamp(Value)^.Fractions := Fraction div 1000000;
          end;
        fldTIMESTAMP:
          with fValue.OdbcParamValueTimeStamp do
            PDouble(Value)^ := TimeStampToMSecs(DateTimeToTimeStamp(
              EncodeDateTime(Year, Month, Day, Hour, Minute, Second, Fraction div 1000000)));
        fldBLOB:
          begin
            if fBindOutputBufferLength < 0 then
              vDataSize := 0
            else if fBindOutputBufferLength <= High(Word) then
              vDataSize := fBindOutputBufferLength
            else
              vDataSize := High(Word);

            if Length > vDataSize then
              Length := vDataSize;

            Move(Value^, vData^, Length);

            // fDbxSubType in [fldstMEMO, fldstFMTMEMO, fldstHMEMO, fldstWIDEMEMO]
          end;
        fldBYTES, fldVARBYTES:
          begin
            if fBindOutputBufferLength < 0 then
              vDataSize := 0
            else if fBindOutputBufferLength <= High(Word) then
              vDataSize := fBindOutputBufferLength
            else
              vDataSize := High(Word);

            if Length > vDataSize then
              Length := vDataSize;

            if fDbxType = fldVARBYTES then
            begin
              PWord(vData)^ := vDataSize;
              inc( DWORD(vData), SizeOf(Word) );
            end;

            Move(Value^, vData^, Length);
          end;
        fldCURSOR:  { For Oracle Cursor type }
          begin
            Integer(Value^) := fValue.OdbcParamValueInteger;
          end;
        else
          begin
            if Length > SizeOf(TOdbcBindParamRec) then
              Length := SizeOf(TOdbcBindParamRec);
            Move(vData^, Value^, Length);
          end
      end;//of: case fDbxType
    end;// of with aOdbcBindParam
  except
    on E: Exception{EDbxError} do
    begin
      IsBlank := 1;
      AddError(E);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.getParameter', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.getParameter', ['Value =', Pointer(Value^), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc.getRowsAffected;//(var Rows: Longword): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCommandOdbc.getRowsAffected'); {$ENDIF _TRACE_CALLS_}
  Rows := fOdbcRowsAffected;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.getRowsAffected', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.getRowsAffected', ['Rows =', Rows]); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCommandOdbc.DoAllocateParams(ParamCount: Word);
var
  i: Integer;
  iParam: TOdbcBIndParam;
begin
  if (fOdbcParamList <> nil) then
  begin
    for i := fOdbcParamList.Count - 1 downto 0 do
      TOdbcBindParam(fOdbcParamList[i]).Free;
    FreeAndNil(fOdbcParamList)
  end;

  if ParamCount > 0 then
  begin
    fOdbcParamList := TList.Create;
    fOdbcParamList.Count := ParamCount;
    for i := 0 to ParamCount - 1 do
    begin
      iParam := TOdbcBindParam.Create;
      fOdbcParamList[i] := iParam;
      iParam.fOdbcParamNumber := i + 1;
    end;
  end;
end;

procedure TSqlCommandOdbc.DoExpandParams(ParamCount: Word);
var
  i: Integer;
  iParam: TOdbcBIndParam;
begin
  if fOdbcParamList = nil then
    DoAllocateParams(ParamCount)
  else if ParamCount > fOdbcParamList.Count then
  begin
    fOdbcParamList.Count := ParamCount;
    i := fOdbcParamList.Count;
    for i := i to ParamCount - 1 do
    begin
      iParam := TOdbcBindParam.Create;
      fOdbcParamList[i] := iParam;
      iParam.fOdbcParamNumber := i + 1;
    end;
  end;
end;

function TSqlCommandOdbc.DoPrepare;//(SQL: PAnsiChar; ParamCount: Word; UpdateParams: Boolean; bUseUnicodeOdbc: Boolean): SQLResult;
var
  OdbcRetcode: OdbcApi.SqlReturn;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
  try try
  if bUseUnicodeOdbc then
    LogEnterProc('TSqlCommandOdbc.DoPrepare', ['SQL =', StrPtrToString(SQL, bUseUnicodeOdbc), 'ParamCount =', ParamCount,
      'UpdateParams =', UpdateParams, 'UseUnicodeOdbc =', bUseUnicodeOdbc, 'StoredProc =', fStoredProc=1]);
  {$ENDIF _TRACE_CALLS_}
  if SQL = nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  with fOwnerDbxDriver.fOdbcApi do
  try
    // reallocate stmt:
    if bPrepareSQL and ((fHStmt = SQL_NULL_HANDLE) or (fStoredProc = 2)
      or ((fStoredProc = 1) and (fSqlPrepared <> '')) ) then
    begin
      DoAllocateStmt();
    end;

    fExecutedOk := False;
    fIsMoreResults := -1;
    if bUseUnicodeOdbc then
    begin
      fSqlW := WideString(PWideChar(SQL));
      fSQL := AnsiString(fSQLW);
    end
    else
      fSql := StrPas(SQL);
    if bPrepareSQL then
      fOwnerDbxConnection.TransactionCheck(Self.fDbxConStmtInfo);
    fStoredProcWithResult := False;

    if UpdateParams then
      DoAllocateParams(ParamCount);

    if fStoredProc = 1 then
    begin
      fSqlPrepared := '';
      fOwnerDbxConnection.fLastStoredProc := Self;
      //fOwnerDbxConnection.DecodeObjectFullName(fSql, fCatalogName, fSchemaName, fSql);
      //if (fOwnerDbxConnection.fDbmsType = eDbmsTypeOracle) then
      //begin
        {
        if CompareText(fSchemaName, fOwnerDbxConnection.fDbxCatalog) = 0 then
        begin
          fSchemaName := '';
        end;
        {}
      //end;
      //fSqlPrepared := fSql;
      fSqlPrepared := BuildStoredProcSQL;
      if bUseUnicodeOdbc then
      begin
        fSqlPreparedW := WideString(fSqlPrepared);
        fSqlW := WideString(fSQL);
      end;
      if bPrepareSQL then
      begin
        if bUseUnicodeOdbc then
        begin
          OdbcRetcode := SQLPrepareW(fHStmt, PAnsiChar(PWideChar(fSqlPreparedW)), SQL_NTSL);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
          begin
            fSqlPrepared := AnsiString(fSqlPreparedW);
            OdbcCheck(OdbcRetcode, 'SQLPrepareW', cTDBXTraceFlags_Prepare);
          end;
        end
        else
        begin
          OdbcRetcode := SQLPrepare(fHStmt, PAnsiChar(fSqlPrepared), SQL_NTS);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            OdbcCheck(OdbcRetcode, 'SQLPrepare', cTDBXTraceFlags_Prepare);
        end;
      end;
      fPreparedOnly := True;
    end
    else
    begin
      fSqlPrepared := fSql;
      if bUseUnicodeOdbc then
        fSqlPreparedW := fSqlW;
      fOwnerDbxConnection.fLastStoredProc := nil;
      if bPrepareSQL then
      begin
        if bUseUnicodeOdbc then
        begin
          OdbcRetcode := SQLPrepareW(fHStmt, PAnsiChar(PWideChar(fSqlPreparedW)), SQL_NTSL);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
          begin
            fSqlPrepared := AnsiString(fSqlPreparedW);
            OdbcCheck(OdbcRetcode, 'SQLPrepareW', cTDBXTraceFlags_Prepare);
          end;
        end
        else
        begin
          OdbcRetcode := SQLPrepare(fHStmt, PAnsiChar(fSqlPrepared), SQL_NTS);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            OdbcCheck(OdbcRetcode, 'SQLPrepare', cTDBXTraceFlags_Prepare);
        end;
      end;
      fPreparedOnly := True;
    end;

  except
    on E: Exception{EDbxError} do
    begin
      AddError(E);
      Result := DBX_FIRSTSTATICERRORS;
      // unprepare stmt when error:
      DoUnprepareStmt();
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.DoPrepare', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.DoPreparee', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc.prepare;//(SQL: PAnsiChar; ParamCount: Word): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCommandOdbc.prepare', ['SQL =', SQL, 'ParamCount =', ParamCount, 'StoredProc =', fStoredProc=1]); {$ENDIF _TRACE_CALLS_}
  {$IFDEF _DBXCB_}
  if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
    fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Prepare, 'ParamCount: %d; SQL: %s', [ParamCount, StrPas(SQL)]);
  {$ENDIF}
  if SQL = nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  Result := DoPrepare(SQL, ParamCount, {UpdateParams:}True, {bPrepareSQL:}fOwnerDbxConnection.fPrepareSQL, {bUseUnicodeOdbc:} False);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.prepare', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.prepare', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc.SetOption;//(eSqlCommandOption: TSQLCommandOption;
//  ulValue: Integer): SQLResult;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  {$IFDEF _MULTIROWS_FETCH_}
  {$IFDEF _MIXED_FETCH_}
  AttrValue: SqlInteger;
  {$ENDIF IFDEF _MIXED_FETCH_}
  {$ENDIF IFDEF _MULTIROWS_FETCH_}
  xeSqlCommandOption: TXSQLCommandOption absolute eSqlCommandOption;
  // ---
  procedure MakeStoredProcParams(ParamCount: Integer);
  var
    i: Integer;
    vParam: TOdbcBindParam;
  begin
    if fOdbcParamList <> nil then
      i := fOdbcParamList.Count
    else
      i := 0;
    if i < ParamCount then
    begin
      fOdbcParamList.Count := ParamCount;
      for i := 0 to ParamCount - 1 do
      begin
        if fOdbcParamList[i] = nil then
        begin
          vParam := TOdbcBindParam.Create;
          fOdbcParamList[i] := vParam;
          vParam.fOdbcParamNumber := i + 1;
        end;
      end;
    end;
  end;
  // ---
  {$IFDEF _TRACE_CALLS_}
  function ulValue2Str: AnsiString;
  begin
    case xeSqlCommandOption of
      xeCommBlockRead:
        Result := AnsiString(BoolToStr(Boolean(ulValue)));
      xeCommCursorName:
        Result := AnsiString(StrPas(PAnsiChar(ulValue)));
      xeCommStoredProc:
        Result := AnsiString(BoolToStr(Boolean(ulValue)));
      xeCommPackageName:
        Result := AnsiString(StrPas(PAnsiChar(ulValue)));
      xeCommTrimChar:
        Result := AnsiString(BoolToStr(Boolean(ulValue)));
      xeCommCatalogName:
        Result := AnsiString(StrPas(PAnsiChar(ulValue)));
      xeCommSchemaName:
        Result := AnsiString(StrPas(PAnsiChar(ulValue)));
      else
        Result := AnsiString(IntToStr(ulValue));
    end;
  end;
  {$ENDIF _TRACE_CALLS_}
  // ---
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
    try try
    {$R+}
    LogEnterProc('TSqlCommandOdbc.SetOption', ['eSqlCommandOption =', cSQLCommandOption[xeSqlCommandOption], 'ulValue =', ulValue2Str()]);
    {$IFDEF RANGECHECKS_OFF} {$R-} {$ENDIF}
  {$ENDIF _TRACE_CALLS_}
  with fOwnerDbxDriver.fOdbcApi do
  try
    case xeSqlCommandOption of
      xeCommRowsetSize:
        // Delphi 6.02 workaround - RowSetSize now set for all drivers
        begin
          {$IFDEF _MULTIROWS_FETCH_}
          if fExecutedOk then
            ulValue := fCommandRowSetSize
          else
          if (ulValue = 0)or(ulValue = -1) then
            ulValue := 1
          else
          if (ulValue < 0) then
            ulValue := fCommandRowSetSize;
          if (not fExecutedOk) and (not fSupportsBlockRead) and (ulValue>1) then
            ulValue := 1;
          if ulValue <> fCommandRowSetSize then
          begin
          {$IFDEF _MIXED_FETCH_}
            fSupportsMixedFetch := fSupportsBlockRead and
              (fOwnerDbxConnection.fConnectionOptions[coMixedFetch] = osOn);
            if not fSupportsMixedFetch then
              ulValue := 1;
            if fSupportsMixedFetch and (ulValue <> fCommandRowSetSize) then
            begin
              OdbcRetcode := SQLGetStmtAttr(fHStmt, SQL_ATTR_CURSOR_TYPE,
                SqlPointer(@AttrValue), 0{SizeOf(AttrValue)}, nil);
              if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
              begin
                ulValue := 1;
                fOwnerDbxConnection.fConnectionOptions[coMixedFetch] := osOff;
                fSupportsMixedFetch := False;
                fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, Self, nil, 1);
              end;
              if fSupportsMixedFetch then
              begin
                if ulValue>1 then
                begin
                  if AttrValue <> SQL_CURSOR_STATIC then
                  begin
                    OdbcRetcode := SQLSetStmtAttr(fHStmt, SQL_ATTR_CURSOR_TYPE,
                      SqlPointer(SQL_CURSOR_STATIC), 0);
                    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
                    begin
                      ulValue := 1;
                      //fOwnerDbxConnection.fConnectionOptions[coMixedFetch] := osOff;
                      fSupportsMixedFetch := False;
                      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, Self, nil, 1);
                    end;
                    {$IFDEF _TRACE_CALLS_}
                    LogInfoProc(['Set cursor type to SQL_CURSOR_STATIC: ', OdbcRetcode = OdbcApi.SQL_SUCCESS]);
                    {$ENDIF IFDEF _TRACE_CALLS_}
                  end;
                end
                else
                begin
                  if AttrValue <> SQL_CURSOR_FORWARD_ONLY then
                  begin
                    OdbcRetcode := SQLSetStmtAttr(fHStmt, SQL_ATTR_CURSOR_TYPE,
                      SqlPointer(SQL_CURSOR_FORWARD_ONLY{=SQL_CURSOR_TYPE_DEFAULT}), 0);
                    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
                    begin
                      ulValue := 1;
                      fSupportsMixedFetch := False;
                      OdbcCheck(OdbcRetcode, 'SQLSetStmtAttr(SQL_ATTR_CURSOR_TYPE,SQL_CURSOR_FORWARD_ONLY)');
                    end;
                    {$IFDEF _TRACE_CALLS_}
                    LogInfoProc(['Set cursor type to SQL_CURSOR_FORWARD_ONLY: ', OdbcRetcode = OdbcApi.SQL_SUCCESS]);
                    {$ENDIF IFDEF _TRACE_CALLS_}
                  end;
                end;
              end;
            end;
          {$ENDIF IFDEF _MIXED_FETCH_}
            fCommandRowSetSize := ulValue;
            {$IFDEF _TRACE_CALLS_}
            LogInfoProc(['Set Fetch Rows Count: CommandRowSetSize = ', fCommandRowSetSize]);
            {$ENDIF IFDEF _TRACE_CALLS_}
          end;
          {$ENDIF _MULTIROWS_FETCH_}
        end;
      xeCommBlobSize:
        begin
          fCommandBlobSizeLimitK := ulValue;
          {$IFDEF _DBXCB_}
          if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
            fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ISQLCommand CommBlobSize: %d', [fCommandBlobSizeLimitK]);
          {$ENDIF}
        end;
      xeCommBlockRead:
        begin
          if Boolean(ulValue) <> fSupportsBlockRead then
          begin
            if Boolean(ulValue) and (not fOwnerDbxConnection.fSupportsBlockRead) then
              fSupportsBlockRead := False
            else
              fSupportsBlockRead := Boolean(ulValue);
          end;
        end;
      xeCommBlockWrite:
        raise EDbxNotSupported.Create(
          'TSqlCommandOdbc.SetOption(eCommBlockWrite) not yet implemented');
      xeCommParamCount:
        begin
          //raise EDbxInvalidCall.Create(
          //  'TSqlCommandOdbc.SetOption(eCommParamCount) not valid (Read-only)');
          if fStoredProc = 1 then
          begin
            if not fExecutedOk then
            begin
              MakeStoredProcParams(Integer(ulValue));
              {$IFDEF _DBXCB_}
              if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
                fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ISQLCommand StoredProc CommParamCount: %d', [Integer(ulValue)]);
              {$ENDIF}
            end
          end
          else
          raise EDbxInvalidCall.Create(
            'TSqlCommandOdbc.SetOption(eCommParamCount) not valid (Read-only)');
        end;
      xeCommNativeHandle:
        raise EDbxInvalidCall.Create(
          'TSqlCommandOdbc.SetOption(eCommNativeHandle) not valid (Read-only)');
      xeCommCursorName:
        begin
          OdbcRetcode := SQLSetCursorName(fHStmt, Pointer(ulValue), SQL_NTS);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            OdbcCheck(OdbcRetcode, 'SQLSetCursorName');
          {$IFDEF _DBXCB_}
          if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
            fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ISQLCommand CommCursorName: %s', [StrPas(PAnsiChar(ulValue))]);
          {$ENDIF}
        end;
      xeCommStoredProc:
        begin
          if Boolean(ulValue) then
            fStoredProc := 1
          else
            fStoredProc := 0;
          {$IFDEF _DBXCB_}
          if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
            fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ISQLCommand CommStoredProc: %d', [fStoredProc]);
          {$ENDIF}
        end;
      xeCommSQLDialect:
        raise EDbxInvalidCall.Create(
          'TSqlCommandOdbc.SetOption(eCommStoredProc) not valid for ' +
          'this DBExpress driver (Interbase only)');
      xeCommTransactionID:
        // set transaction level for current statement (it is equal global transaction level).
        {ignored};
{.$IFDEF _D7UP_}
      xeCommPackageName:
        begin
          fStoredProcPackName := PAnsiChar(ulValue);
          {$IFDEF _DBXCB_}
          if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
            fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ISQLCommand CommPackageName: %s', [fStoredProcPackName]);
          {$ENDIF}
        end;
      xeCommTrimChar:
        begin
          fTrimChar := Boolean(ulValue);
        end;
      xeCommQualifiedName:
        raise EDbxNotSupported.Create(
          'TSqlCommandOdbc.SetOption(eCommQualifiedName) not yet implemented');
      xeCommCatalogName:
        begin
          fCatalogName := PAnsiChar(ulValue);
          {$IFDEF _DBXCB_}
          if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
            fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ISQLCommand CommCatalogName: %s', [fCatalogName]);
          {$ENDIF}
        end;
      xeCommSchemaName:
        begin
          fSchemaName := PAnsiChar(ulValue);
          {$IFDEF _DBXCB_}
          if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
            fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'set ISQLCommand CommSchemaName: %s', [fSchemaName]);
          {$ENDIF}
        end;
      xeCommObjectName:
        raise EDbxNotSupported.Create(
          'TSqlCommandOdbc.SetOption(eCommObjectName) not yet implemented');
      xeCommQuotedObjectName:
        raise EDbxNotSupported.Create(
          'TSqlCommandOdbc.SetOption(eCommQuotedObjectName) not yet implemented');
{.$ENDIF} //of: IFDEF _D7UP_
    else
      raise EDbxInvalidCall.Create(
        'TSqlCommandOdbc.SetOption - Invalid option ' + IntToStr(Ord(eSqlCommandOption)));
    end;
  except
    on E: EDbxNotSupported do
      Result := DBXERR_NOTSUPPORTED;
    on E: Exception{EDbxError} do
    begin
      AddError(E);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.SetOption', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.SetOption', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc.setParameter;//(
//  ulParameter,
//  ulChildPos: Word;
//  eParamType: TSTMTParamType;
//  uLogType,
//  uSubType: Word;
//  iPrecision,
//  iScale: Integer;
//  Length: Longword;
//  pBuffer: Pointer;
//  bIsNull: Integer
//  ): SQLResult;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  bIsParamVar, bIsParamIn: Boolean;
  iStrLen, iDelta: Integer;
  aMSecs: Double;
  aDays: Integer absolute iDelta;
  aSeconds: Integer absolute iStrLen;
  aTimeStamp: TTimeStamp;
  aDateTime: TDateTime;
  aYear, aMonth, aDay: Word;
  aHour, aMinute, aSecond, aMSec: Word;
  aOdbcBindParam: TOdbcBindParam;
  bUnicodeString: Boolean;
  vLength: Longword;
  bHandled: Boolean;
  // ---
  procedure ProcessVarDataLength(AAddLen: Longword; bCharType: Boolean = True);
  var
    iBufLenLimit: Integer;
  begin
    {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
    //
    // Workaround Centura SqlBase bug - it crashes if parameter length increases
    // So, because string is null-terminated, we just indicate it has maximum length
    // Similar error affects MSSqlServer with MDAC 2.6 (but not earlier or later versions)
    // If parameter length increases, if does not crash, but it does not find the item
    // So we set to 255 for all drivers, not just Centura.
    //
    {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
    //if fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeGupta then
    with aOdbcBindParam do
    begin
      // minimum char buffer size = 255 bytes
      fBindOutputBufferLength := Length + AAddLen;
      if bCharType then
      begin
        if bUnicodeString then
          iBufLenLimit := 256
        else
          iBufLenLimit := 255;
        fOdbcParamCbColDef := iPrecision;
        if (iPrecision < iBufLenLimit)
          and (fOwnerDbxConnection.fConnectionOptions[coOBPBPL] <> osOff) then
        begin
          fOdbcParamCbColDef := iBufLenLimit;
        end;
        if fBindOutputBufferLength < iBufLenLimit then
          fBindOutputBufferLength := iBufLenLimit;
      end
      else
      begin
        fOdbcParamCbColDef := iPrecision;
      end;
    end;
  end;
  // ---
  procedure SetVarData(AParamLenOrInd: SqlInteger);
  begin
    if (bIsNull = 0) or bIsParamVar then
      with aOdbcBindParam do
      begin // Not NULL
        if (bIsNull = 0) then
          fOdbcParamLenOrInd := AParamLenOrInd;
        if fBindOutputBufferLength > SizeOf(TOdbcBindParamRec) then
        begin
          GetMem(fBuffer, fBindOutputBufferLength);
          fBindData := fBuffer;
        end;
        if bIsParamIn and (bIsNull = 0) then
        begin
          Move(pBuffer^, fBindData^, Length);
        end;
      end;
  end;
  // ---
{$IFDEF _FIX_PostgreSQL_ODBC_}
var
  sUTF8Buffer: {$IFDEF _D12UP_}RawByteString{$ELSE}UTF8String{$ENDIF};
  procedure WideCharToUtf8(Source: PWideChar; SourceChars: Integer);
  var
    L: Integer;
  begin
    SetLength(sUTF8Buffer, SourceChars * 3); // SetLength includes space for null terminator
    L := UnicodeToUtf8(PAnsiChar(sUTF8Buffer), System.Length(sUTF8Buffer) + 1, Source, Length);
    if L > 0 then
      SetLength(sUTF8Buffer, L - 1)
    else
      sUTF8Buffer := '';
  end;
  procedure LDo_PostgreSQL_Make_Buffer;
  begin
    if not bUnicodeString then
    begin
      sUTF8Buffer := AnsiToUtf8(string(StrPas(PAnsiChar(pBuffer))));
    end
    else
    begin
      WideCharToUtf8(PWideChar(pBuffer), Length);
      // fix: PostgreSQL ODBC driver second bug:
      with aOdbcBindParam do
      begin
        fOdbcParamCType := SQL_C_CHAR;
        if (uSubType and fldstFIXED = 0) then
          fOdbcParamSqlType := SQL_VARCHAR
        else
          fOdbcParamSqlType := SQL_CHAR;
      end;
    end;
  end;
{$ENDIF IFDEF _FIX_PostgreSQL_ODBC_}
// ---
begin
  Result := DBXERR_NONE;
  //Fix Somehow the type gets corrupted
  eParamType := TSTMTParamType(ShortInt(eParamType));

  {$IFDEF _TRACE_CALLS_}
    try try
    LogEnterProc('TSqlCommandOdbc.setParameter', ['ulParameter =', ulParameter,
    'ulChildPos =', ulChildPos, 'eParamType =', cSTMTParamType[eParamType], 'uLogType =', uLogType,
    'uSubType =', uSubType, 'iPrecision =', iPrecision, 'iScale =', iScale, 'Length =', Length,
    'pBuffer =', 'pBuffer =', pBuffer, 'bIsNull =', bIsNull]);
  {$ENDIF _TRACE_CALLS_}
  with fOwnerDbxDriver.fOdbcApi do
  try
    vLength := Length;
    bUnicodeString := False;

    DoExpandParams(ulParameter-1);
    aOdbcBindParam := TOdbcBindParam(fOdbcParamList.Items[ulParameter - 1]);
    with aOdbcBindParam do
    begin

      case eParamType of
        paramIN:
          begin
            fOdbcInputOutputType := SQL_PARAM_INPUT;
          end;
        paramINOUT:
          begin
            fOdbcInputOutputType := SQL_PARAM_INPUT_OUTPUT;
          end;
        paramOUT:
          begin
            fOdbcInputOutputType := SQL_PARAM_OUTPUT;
            bIsNull := 1;
          end;
        paramRET:
          begin
            bIsNull := 1;
            fStoredProcWithResult := True;
            fOdbcInputOutputType := SQL_PARAM_OUTPUT;
          end;
        else {paramUNKNOWN:}
          begin
            raise EDbxNotSupported.Create(
              'TSqlCommandOdbc.setParameter - ParamType paramUNKNOWN not yet supoorted');
          end;
      end;

      bIsParamIn := (eParamType in [paramIN, paramINOUT]);
      bIsParamVar := (eParamType in [paramINOUT, paramOUT, paramRET]);

      fDbxType := uLogType;
      fDbxSubType := uSubType;

      fOdbcParamLenOrInd := 0;
      fOdbcParamIbScale := 0;
      fBindOutputBufferLength := -1;
      FreeMemAndNil(fBuffer);

      if (bIsNull <> 0) or (pBuffer = nil) then
        bIsNull := 1;

      // pointer to the Data Value
      if (bIsNull = 0) or bIsParamVar then
        fBindData := @fValue // Not NULL
      else
        fBindData := nil;    // NULL

      if bIsNull <> 0 then
        fOdbcParamLenOrInd := OdbcApi.SQL_NULL_DATA;

      bHandled := True;
      case uLogType of
        fldZSTRING,
        fldWIDESTRING,
        fldUNICODE:
          (*
          { fldZSTRING subtype }
            fldstPASSWORD      = 1;               { Password }
            fldstFIXED         = 31;              { CHAR type }
            fldstWIDEMEMO/fldstUNICODE = 32;      { WideMemoo }
          *)
          begin
            bUnicodeString := (uLogType = fldWIDESTRING) or ((uSubType and fldstWIDEMEMO) <> 0) or (uLogType = fldUNICODE);
            if bUnicodeString then
            begin { NCHAR, NVARCHAR }
              //
              // QC: 58473:
              // Delphi not will not send important information about field type (fldstFIXED).
              // See: SqlExpr.pas: procedure SetQueryProcParams
              //
              Length := Length div SizeOf(WideChar) * SizeOf(WideChar);
              iPrecision := iPrecision div SizeOf(WideChar) * SizeOf(WideChar);
              if bIsParamIn then
              begin
                if ((iPrecision = 0) and (Length > 0)) or (iPrecision >= Integer(Length)) then
                begin
                  iPrecision := Length - SizeOf(WideChar);
                  if iPrecision < SizeOf(WideChar) then
                  begin
                    iPrecision := SizeOf(WideChar);
                    if fOwnerDbxConnection.fDbmsType = eDbmsTypeOracle then
                    begin
                      // oracle empty string == null
                      bIsNull := 1;
                      fOdbcParamLenOrInd := OdbcApi.SQL_NULL_DATA;
                    end;
                  end;
                end;
              end
              else
              begin
                // need allocate buffer for ret parameter
                if iPrecision < SizeOf(WideChar) then
                  iPrecision := SizeOf(WideChar);
                if Integer(Length) <= iPrecision then
                  Length := iPrecision + SizeOf(WideChar);
              end;
              //
              fOdbcParamCType := SQL_C_WCHAR;
              if {(iPrecision > 128 * SizeOf(WideChar)) and} (uSubType and fldstFIXED = 0) then
                fOdbcParamSqlType := SQL_WVARCHAR
              else
                fOdbcParamSqlType := SQL_WCHAR;
            end
            else  { CHAR, VARCAHR }
            begin
              if bIsParamIn then
              begin
                if iPrecision >= Integer(Length) then
                  iPrecision := Length - 1;
                if iPrecision < 1 then
                begin
                  iPrecision := 1;
                  if fOwnerDbxConnection.fDbmsType = eDbmsTypeOracle then
                  begin
                    // oracle empty string == null
                    bIsNull := 1;
                    fOdbcParamLenOrInd := OdbcApi.SQL_NULL_DATA;
                  end;
                end;
              end
              else
              begin
                // need allocate buffer for ret parameter
                if Length = 0 then
                  Length := 1;
                if iPrecision <= 0 then
                  iPrecision := Length - 1;
                if iPrecision < 1 then
                  iPrecision := 1;
              end;
              //
              fOdbcParamCType := SQL_C_CHAR;
              if (uSubType and fldstFIXED = 0) then
                fOdbcParamSqlType := SQL_VARCHAR
              else
                fOdbcParamSqlType := SQL_CHAR;
            end;
            // handle coEmptyStrParam, coNullStrParam:
            {begin:}
              if (bIsNull = 0) then
              begin // NOT NULL DATA
                if bIsParamIn
                  and (fOwnerDbxConnection.fConnectionOptions[coEmptyStrParam] = osOn)
                  and
                  (  // unicode check
                     ( bUnicodeString and (PWideChar(pBuffer)^ = cNullWideChar) )
                     or
                     // ansi char check
                     ( (not bUnicodeString) and (PAnsiChar(pBuffer)^ = cNullAnsiChar) )
                  ) then
                begin
                  pBuffer := nil;
                  Length := 0;
                  bIsNull := 1;
                  fOdbcParamLenOrInd := OdbcApi.SQL_NULL_DATA;
                end;
              end
              else  // NULL DATA
              begin
                if (fBindData = nil)
                  and (fOwnerDbxConnection.fConnectionOptions[coNullStrParam] = osOn) then
                begin
                  pBuffer := @cNullWideCharBuf;
                  if bUnicodeString then
                    Length := SizeOf(WideChar)
                  else
                    Length := 1;
                  bIsNull := 0;
                  fOdbcParamLenOrInd := 0;
                end;
              end;
            {end.}

            {$IFDEF _FIX_PostgreSQL_ODBC_}
            // Driver supported only utf8 charsets.
            if (bIsNull = 0) and (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypePostgreSQL) then
            begin
              if bIsParamIn then
              begin
                LDo_PostgreSQL_Make_Buffer;
                pBuffer := PAnsiChar(sUTF8Buffer);
                Length := System.Length(sUTF8Buffer);
                iPrecision := Length + 1;
              end;
              if Length = 0 then
              begin
                bIsNull := 1;
                fOdbcParamLenOrInd := OdbcApi.SQL_NULL_DATA;
                pBuffer := nil;
                // NULL DATA
                if (fBindData = nil)
                  and (fOwnerDbxConnection.fConnectionOptions[coNullStrParam] = osOn) then
                begin
                  pBuffer := @cNullWideCharBuf;
                  if bUnicodeString then
                    Length := SizeOf(WideChar)
                  else
                    Length := 1;
                  bIsNull := 0;
                  fOdbcParamLenOrInd := 0;
                end;
              end;

              bUnicodeString := False;
            end;
            {$ENDIF IFDEF _FIX_PostgreSQL_ODBC_}

            //
            // fix: update empty field type "sql_variant"
            //
            //if (fOwnerDbxConnection.fOdbcDriverType in [eOdbcDriverTypeMsSqlServer, eOdbcDriverTypeMsSqlServer2005Up]) then
            if (bIsNull = 1) then
            begin
              if not bIsParamVar then
                iPrecision := Length;
            end;
            // fix.

            case fOdbcParamSqlType of
              SQL_VARCHAR, SQL_LONGVARCHAR, SQL_CHAR:
                ProcessVarDataLength(1);
              else //SQL_WVARCHAR, SQL_WLONGVARCHAR:
                ProcessVarDataLength(SizeOf(WideChar));
            end;
            SetVarData(SQL_NTS);

            case fOwnerDbxConnection.fDbmsType of
              eDbmsTypeMsAccess:
                case fOdbcParamSqlType of
                  SQL_CHAR, SQL_VARCHAR:
                    begin
                      if fOdbcParamCbColDef >= 255 then
                      begin
                        { MSAccess will not be able to bind field of the big size (>255) to simple types (SQL_VARCHAR, SQL_WVARCHAR) }
                        fOdbcParamSqlType := SQL_LONGVARCHAR;
                      end;
                    end;
                  SQL_WCHAR, SQL_WVARCHAR: { bUnicodeString == True }
                    begin
                      if fOdbcParamCbColDef >= 128 then
                        fOdbcParamSqlType := SQL_WLONGVARCHAR;
                    end;
                end;
              //eDbmsTypeMsSqlServer, eDbmsTypeMsSqlServer2005Up:
              else
                {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
                //
                //  Notes:
                //
                // MSSQL: can be a problems when UpdateMode <> upWhereKeyOnly:
                //  error: 402:
                //    Data types given nchar and ntext in operator equal there is incompatible.
                //  example schema:
                //    CREATE TABLE dbx_test_nchar (
                //      f_bigint bigint NOT NULL,
                //      f_nchar nchar(1024) NULL,
                //      PRIMARY KEY (f_bigint))
                //
                {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
                case fOdbcParamSqlType of
                  SQL_CHAR, SQL_VARCHAR:
                    begin
                      if fOdbcParamCbColDef >= 2048 then
                      begin
                        { MSSQL will not be able to bind field of the big size (>2048) to simple types (SQL_VARCHAR, SQL_WVARCHAR) }
                        fOdbcParamSqlType := SQL_LONGVARCHAR;
                      end;
                    end;
                  SQL_WCHAR, SQL_WVARCHAR: { bUnicodeString == True }
                    begin
                      if fOdbcParamCbColDef  >= 1024 then
                        fOdbcParamSqlType := SQL_WLONGVARCHAR;
                    end;
                end;
            end;

            if (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeSQLite) and (bIsNull = 1) and
              (fBindData = nil) then
            begin
              fBindData := @fValue;
              PWideChar(fBindData)^ := cNullWideChar;
            end;
          end;
        fldINT32, fldUINT32:
          begin
            if uLogType = fldINT32 then
              fOdbcParamCType := SQL_C_LONG
            else
              fOdbcParamCType := SQL_C_ULONG;
            fOdbcParamSqlType := SQL_INTEGER;
            fOdbcParamCbColDef := SizeOf(SqlInteger);
            if (bIsNull = 0) then
            begin
              fOdbcParamLenOrInd := SizeOf(SqlInteger);
              fValue.OdbcParamValueInteger := SqlInteger(pBuffer^);
            end;
          end;
        fldINT16, fldUINT16:
          begin
            if uLogType = fldINT16 then
              fOdbcParamCType := SQL_C_SHORT
            else
              fOdbcParamCType := SQL_C_USHORT;
            fOdbcParamSqlType := SQL_SMALLINT;
            fOdbcParamCbColDef := SizeOf(SqlSmallint);
            if (bIsNull = 0) then
            begin
              fOdbcParamLenOrInd := SizeOf(SqlSmallint);
              fValue.OdbcParamValueShort := SqlSmallint(pBuffer^);
            end;
          end;
        fldINT64, fldUINT64:
          begin
            if uLogType = fldINT64 then
              fOdbcParamCType := SQL_C_SBIGINT
            else
              fOdbcParamCType := SQL_C_UBIGINT;
            fOdbcParamSqlType := SQL_BIGINT;
            fOdbcParamCbColDef := SizeOf(SqlBigInt);
            if (bIsNull = 0) then
            begin
              fOdbcParamLenOrInd := SizeOf(SqlBigInt);
              fValue.OdbcParamValueBigInt := SqlBigInt(pBuffer^);
            end;
          end;
        fldFLOAT: // 64-bit floating point
          (*
          { fldFLOAT subtype }
            fldstMONEY         = 21;              { Money }
          *)
          begin
            fOdbcParamCType := SQL_C_DOUBLE;
            fOdbcParamSqlType := SQL_DOUBLE;
            fOdbcParamCbColDef := SizeOf(SqlDouble);
            if (bIsNull = 0) then
            begin
              fOdbcParamLenOrInd := SizeOf(SqlDouble);
              fValue.OdbcParamValueDouble := SqlDouble(pBuffer^);
            end;
          end;
        fldDATE:
          begin
            (*
            { fldDATE subtype }
              fldstADTDATE       = 37;              { DATE (OCIDate) with in an ADT }
            *)

            if (fOwnerDbxConnection.fConnectionOptions[coParamDateByOdbcLevel3] <> osOff) then
            begin
              // Merant, Intersolv odbc bugs:
              fOdbcParamCType := cBindMapDateTimeOdbc3[biDate]; // == SQL_C_TYPE_DATE;
              fOdbcParamSqlType := fOdbcParamCType;
            end
            else
            begin
              fOdbcParamCType := SQL_C_DATE;
              fOdbcParamSqlType := SQL_DATE;
            end;

            fOdbcParamCbColDef := SizeOf(TSqlDateStruct);
            if (bIsNull = 0) then
            begin
              fOdbcParamLenOrInd := SizeOf(TSqlDateStruct);
              aDays := Integer(pBuffer^) - DateDelta;
              {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
              // DateDelta: Days between 1/1/0001 and 12/31/1899 = 693594,
              // ie (1899 * 365) (normal days) + 460 (leap days) - 1
              //(-1: correction for being last day of 1899)
              // leap days between 0001 and 1899 = 460, ie 1896/4 - 14
              // (-14: because 14 years weren't leap years:
              // 100,200,300, 500,600,700, 900,1000,1100, 1300,1400,1500, 1700,1800)
              {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
              DecodeDate(aDays, aYear, aMonth, aDay);
              fValue.OdbcParamValueDate.Year := aYear;
              fValue.OdbcParamValueDate.Month := aMonth;
              fValue.OdbcParamValueDate.Day := aDay;
            end;
          end;
        fldTIME:
          begin
            if (fOwnerDbxConnection.fConnectionOptions[coParamDateByOdbcLevel3] <> osOff) then
            begin
              // Merant, Intersolv odbc bugs:
              fOdbcParamCType := cBindMapDateTimeOdbc3[biTime]; // == SQL_C_TYPE_TIME
              fOdbcParamSqlType := fOdbcParamCType;
            end
            else
            begin
              fOdbcParamCType := SQL_C_TIME;
              fOdbcParamSqlType := SQL_TIME;
            end;

            fOdbcParamCbColDef := SizeOf(TSqlTimeStruct);
            if (bIsNull = 0) then
            begin
              // Value is time in Microseconds
              aSeconds := Longword(pBuffer^) div 1000;
              fOdbcParamLenOrInd := SizeOf(TSqlTimeStruct);
              fValue.OdbcParamValueTime.Hour := aSeconds div 3600;
              fValue.OdbcParamValueTime.Minute := (aSeconds div 60) mod 60;
              fValue.OdbcParamValueTime.Second := aSeconds mod 60;
            end;
          end;
        fldDATETIME:
          begin
            if (fOwnerDbxConnection.fConnectionOptions[coParamDateByOdbcLevel3] <> osOff) then
            begin
              // Merant, Intersolv odbc bugs:
              fOdbcParamCType := cBindMapDateTimeOdbc3[biDateTime]; // == SQL_C_TYPE_TIMESTAMP
              fOdbcParamSqlType := fOdbcParamCType;
            end
            else
            begin
              fOdbcParamCType := SQL_C_TIMESTAMP;
              fOdbcParamSqlType := SQL_TIMESTAMP;
            end;

            fOdbcParamCbColDef := 26;
            fOdbcParamIbScale := 6;
            if (fOwnerDbxConnection.fOdbcDriverType in [eOdbcDriverTypeMsSqlServer, eOdbcDriverTypeMsSqlServer2005Up]) then
            begin
              // Workaround SqlServer driver - it only allows max scale of 3
              fOdbcParamCbColDef := 23;
              fOdbcParamIbScale := 3;
            end;
            if (bIsNull = 0) then with TSQLTimeStamp(pBuffer^), fValue do
            begin
              fOdbcParamLenOrInd := SizeOf(SQL_TIMESTAMP_STRUCT);
              {fValue.}OdbcParamValueTimeStamp.Year := {TSQLTimeStamp(pBuffer^).}Year;
              OdbcParamValueTimeStamp.Month := Month;
              OdbcParamValueTimeStamp.Day := Day;
              OdbcParamValueTimeStamp.Hour := Hour;
              OdbcParamValueTimeStamp.Minute := Minute;
              OdbcParamValueTimeStamp.Second := Second;
              // Odbc in nanoseconds; DbExpress in milliseconds; so multiply by 1 million
              if (1.0 * TSQLTimeStamp(pBuffer^).Fractions * 1000000) < High(OdbcParamValueTimeStamp.Fraction) then
                OdbcParamValueTimeStamp.Fraction := Fractions * 1000000;
            end;
          end;
        fldTIMESTAMP: // fldTIMESTAMP added by Michael Schwarzl, to support MS SqlServer 2000
          begin
            // Fix by David McCammond-Watts (not tested)
            // Old code assumes that the pBuffer parameter points to a TSQLTimeStamp record.
            // In fact, it points to a Double that contains the number of milliseconds since
            // 01/01/0001 minus one day.
            fOdbcParamCType := SQL_C_TIMESTAMP;
            fOdbcParamSqlType := SQL_TIMESTAMP;
            fOdbcParamCbColDef := 26;
            fOdbcParamIbScale := 6;
            if (fOwnerDbxConnection.fOdbcDriverType in [eOdbcDriverTypeMsSqlServer, eOdbcDriverTypeMsSqlServer2005Up]) then
            begin
              // Workaround SqlServer driver - it only allows max scale of 3
              fOdbcParamCbColDef := 23;
              fOdbcParamIbScale := 3;
            end;
            if (bIsNull = 0) then with fValue.OdbcParamValueTimeStamp do
            begin
//              aTimeStamp := TTimeStamp(pBuffer^);
              aMSecs := Double(pBuffer^);
              aTimeStamp := MSecsToTimeStamp(aMSecs);
              aDateTime := TimeStampToDateTime(aTimeStamp);
              DecodeDate(aDateTime, aYear, aMonth, aDay);
              DecodeTime(aDateTime, aHour, aMinute, aSecond, aMSec);
              {fValue.OdbcParamValueTimeStamp.}Year := aYear;
              Month := aMonth;
              Day := aDay;
              Hour := aHour;
              Minute := aMinute;
              Second := aSecond;
              // Odbc in nanoseconds; DbExpress in  milliseconds; so multiply by 1 million
              if (1.0 * aMSec * 1000000) < High(Fraction) then
                Fraction := aMSec * 1000000;
            end;
          end;
        fldBCD,
        fldFMTBCD:
          begin
            fOdbcParamCType := SQL_C_CHAR;
            fOdbcParamSqlType := SQL_DECIMAL;
            if (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeMsJet) then
              fOdbcParamSqlType := SQL_NUMERIC; // MS ACCESS driver does not allow SQL_DECIMAL
            fOdbcParamCbColDef := iPrecision;
            fOdbcParamIbScale := iScale;
            if (bIsNull = 0) and not CompareMem(pBuffer, @NullBcd, SizeOf(TBcd)) then
            begin
              fOdbcParamLenOrInd := SQL_NTS;
              BCD2Str(fValue.OdbcParamValueString, TBcd(pBuffer^), fOwnerDbxConnection.fDecimalSeparator,
                {bExpFormat=}fOwnerDbxConnection.fConnectionOptions[coBCD2Exp] = osOn);
              {  // OLD:
              S := BcdToStr(TBcd(pBuffer^));
              StrCopy(fValue.OdbcParamValueString, PAnsiChar(S)); }
            end
            else
            begin
              bIsNull := 1;
              fOdbcParamLenOrInd := OdbcApi.SQL_NULL_DATA;
              if (not bIsParamVar)
                 and (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeMsSqlServer)
                 and (fOwnerDbxConnection.fDbmsVersionMajor < 9) // MSSQL 2000 and lowwer
              then
              begin
                // MsSqlServer driver insists on non-zero length, even for NULL values
                fOdbcParamCbColDef := 1;
                fOdbcParamIbScale := 0;
              end;
            end;
          end;
        fldBOOL:
          begin
            fOdbcParamCType := SQL_C_BIT;
            fOdbcParamSqlType := SQL_BIT; // MS ACCESS driver does not allow SQL_DECIMAL
            fOdbcParamCbColDef := 1;
            if (bIsNull = 0) then
            begin
              fOdbcParamLenOrInd := 1;
              if SqlByte(pBuffer^) = 0 then
                fValue.OdbcParamValueBit := 0
              else
                fValue.OdbcParamValueBit := 1;
            end;
          end;
        fldBLOB:
          {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
          (*
          { fldBLOB subtypes }
            fldstMEMO          = 22;              { Text Memo }
            fldstBINARY        = 23;              { Binary data }
            fldstFMTMEMO       = 24;              { Formatted Text }
            fldstOLEOBJ        = 25;              { OLE object (Paradox) }
            fldstGRAPHIC       = 26;              { Graphics object }
            fldstDBSOLEOBJ     = 27;              { dBASE OLE object }
            fldstTYPEDBINARY   = 28;              { Typed Binary data }
            fldstACCOLEOBJ     = 30;              { Access OLE object }
            fldstHMEMO         = 33;              { CLOB }
            fldstHBINARY       = 34;              { BLOB }
            fldstBFILE         = 36;              { BFILE }
          *)
          {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
          begin
            if (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeSQLite) then
              uSubType := fldstMEMO;
            case uSubType of
              fldstBINARY, fldstGRAPHIC, fldstTYPEDBINARY, fldstHBINARY:
                begin
                  if not bIsParamVar then
                    iPrecision := Length; { !!! }
                  if iPrecision < 1 then
                    iPrecision := 1;
                  fOdbcParamCType := SQL_C_BINARY;
                  fOdbcParamSqlType := SQL_LONGVARBINARY;
                  //
                  ProcessVarDataLength(0, False);
                  SetVarData(Length);
                end;
              fldstMEMO, fldstFMTMEMO, fldstHMEMO, fldstWIDEMEMO:
                begin
                  //
                  // !!! (iPrecision <> 0): // MSSQL not bind fot null value when fOdbcParamCbColDef = 0
                  //
                  bUnicodeString := uSubType = fldstWIDEMEMO;
                  if not bUnicodeString then
                  begin
                    fOdbcParamCType := SQL_C_CHAR;
                    fOdbcParamSqlType := SQL_LONGVARCHAR;
                    iDelta := 0;
                    if bIsParamIn then
                    begin
                      if iPrecision <= 0 then
                      begin
                        bIsNull := 1;
                        fOdbcParamLenOrInd := OdbcApi.SQL_NULL_DATA;
                      end;
                      iPrecision := Length; { !!! }
                      if iPrecision < 1 then
                      begin
                        iPrecision := 1;
                        bIsNull := 1;
                        fOdbcParamLenOrInd := OdbcApi.SQL_NULL_DATA;
                      end;
                    end
                    else
                    begin
                      // need allocate buffer (iStrLen) for OUT parameter
                      if Integer(Length) <= iPrecision then
                      begin
                        if iPrecision < 1 then
                        begin
                          iPrecision := 1;
                          bIsNull := 1;
                          fOdbcParamLenOrInd := OdbcApi.SQL_NULL_DATA;
                        end;
                        Length := iPrecision;
                      end;
                    end;
                    iStrLen := Length;
                  end
                  else { if bUnicodeString then }
                  begin
                    fOdbcParamCType := SQL_C_WCHAR;
                    fOdbcParamSqlType := SQL_WLONGVARCHAR;
                    if bIsParamIn then
                    begin
                      iDelta := Length;
                      iStrLen := (Length + 1) div SizeOf(WideChar) * SizeOf(WideChar);
                      iDelta := iStrLen - iDelta;
                      iPrecision := Length - SizeOf(WideChar); { !!! }
                      if iPrecision < SizeOf(WideChar) then
                        iPrecision := SizeOf(WideChar);
                    end
                    else
                    begin
                      iDelta := 0;
                      // need allocate buffer (iStrLen) for OUT parameter
                      iPrecision := (iPrecision + 1) div SizeOf(WideChar) * SizeOf(WideChar);
                      if iPrecision < SizeOf(WideChar) then
                        iPrecision := SizeOf(WideChar);
                      if Integer(Length) <= iPrecision then
                        Length := iPrecision + SizeOf(WideChar)
                      else
                        Length := (Length + 1) div SizeOf(WideChar) * SizeOf(WideChar);
                      iStrLen := Length;
                    end;
                    if (bIsNull = 0) and (iPrecision = SizeOf(WideChar)) and (Length = SizeOf(WideChar)) then
                    begin
                      iDelta := 0;
                      bIsNull := 1;
                      fOdbcParamLenOrInd := OdbcApi.SQL_NULL_DATA;
                    end;
                  end;
                  {$IFDEF _FIX_PostgreSQL_ODBC_}
                  if (bIsNull = 0) and (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypePostgreSQL) then
                  begin
                    if bUnicodeString then
                    begin
                      bUnicodeString := False;
                      fOdbcParamCType := SQL_C_CHAR;
                      fOdbcParamSqlType := SQL_LONGVARCHAR;
                    end;
                    if bIsParamIn then
                    begin
                      sUTF8Buffer := AnsiToUtf8( string(StrPas(PAnsiChar(pBuffer))) );
                      pBuffer := PAnsiChar(sUTF8Buffer);
                      Length := System.Length(sUTF8Buffer);
                    end;
                    if Length = 0 then
                    begin
                      bIsNull := 1;
                      pBuffer := nil;
                      fOdbcParamLenOrInd := OdbcApi.SQL_NULL_DATA;
                    end
                    else if bUnicodeString then
                    begin
                      iDelta := 0;
                    end;
                  end;
                  {$ENDIF IFDEF _FIX_PostgreSQL_ODBC_}
                  ProcessVarDataLength(iDelta, {bCharType:}False); // MSSQL not bind fot null value when fOdbcParamCbColDef = 0
                  SetVarData(Length);
                  if (iDelta > 0) then
                  begin
                    PWideChar(pBuffer)[iStrLen div SizeOf(WideChar) - 1] := cNullWideChar;
                  end;
                end;
              else
                begin
                  raise EDbxNotSupported.Create(
                    'TSqlCommandOdbc.setParameter - This data sub-type not yet supported');
                end;
            end;
          end; // of case uSubType
        fldBYTES, fldVARBYTES:
          begin
            if not bIsParamVar then
              iPrecision := Length; { !!! }
            if iPrecision < 1 then
              iPrecision := 1;
            if uLogType = fldBYTES then
              fOdbcParamSqlType := SQL_BINARY
            else
              fOdbcParamSqlType := SQL_VARBINARY;
            fOdbcParamCType := SQL_C_BINARY;
            ProcessVarDataLength(0, False);
            SetVarData(Length);
          end;
//  fldLOCKINFO        = 16;              { Look for LOCKINFO typedef }
        fldCURSOR:  { For Oracle Cursor type }
          begin
            if (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeOracle) then
            begin
              fOdbcParamSqlType := SQL_ORA_CURSOR;
              fOdbcParamCType := SQL_C_ULONG;
              fOdbcParamCbColDef := SizeOf(SqlInteger);
              if (bIsNull = 0) then
              begin
                fOdbcParamLenOrInd := SizeOf(SqlInteger);
                fValue.OdbcParamValueInteger := SqlInteger(pBuffer^);
              end;
            end
            else
              bHandled := False;
          end;
{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
//  fldADT             = 20;              { Abstract datatype (structure) }
     (*
     { fldADT subtype }
       fldstADTNestedTable = 35;             { ADT for nested table (has no name) }
     *)
//  fldARRAY           = 21;              { Array field type }
//  fldREF             = 22;              { Reference to ADT }
//  fldTABLE           = 23;              { Nested table (reference) }
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
      else
        bHandled := False;
      end; //of:case

      if not bHandled then
        raise EDbxNotSupported.Create('TSqlCommandOdbc.setParameter(Type='+IntToStr(uLogType)+
          ') - This data type not yet supported');

      if fBindOutputBufferLength = -1 then
        fBindOutputBufferLength := fOdbcParamCbColDef;

      // begin: 3.0.26:
      if ulChildPos = High(Word) then
      begin
        fBindOutputBufferLength := vLength;
        {$IFDEF _DBXCB_}
        if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
          fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ISQLCommand SetParameter: %d; value: %s',
            [Integer(ulParameter), FormatParameter(aOdbcBindParam, fOwnerDbxConnection)]);
        {$ENDIF}
        Result := DBXERR_NONE;
        Exit;
      end;
      // end: 3.0.26.

      {$IFDEF _DBXCB_}
      if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
        fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Vendor, 'get ISQLCommand SetParameter: %d; value: %s',
          [Integer(ulParameter), FormatParameter(aOdbcBindParam, fOwnerDbxConnection)]);
      {$ENDIF}

{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
{$IFDEF _TRACE_CALLS_}
      LogInfoProc([
        'SQLBindParameter: stmt = $', IntToHex(Integer(fHStmt), 8),
        'num =', IntToStr(ulParameter),
        'IOtype =', IntToStr(fOdbcInputOutputType),
        'ValType =', IntToStr(fOdbcParamCType),
        'ParType =', IntToStr(fOdbcParamSqlType),
        'ColSize =', IntToStr(fOdbcParamCbColDef),
        'DecDig =', IntToStr(fOdbcParamIbScale),
        'Val = $', IntToHex(Integer(fBindData), 8),
        'BufLen =', IntToStr(fBindOutputBufferLength),
        'StrLen_Ind =', IntToStr(fOdbcParamLenOrInd)
      ]);
{$ENDIF _TRACE_CALLS_}
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
      OdbcRetcode := SQLBindParameter(
        fHStmt, // Odbc statement handle
        ulParameter, // Parameter number, starting at 1
        fOdbcInputOutputType, // Parameter InputOutputType
        fOdbcParamCType, // 'C' data type of paremeter - Sets SQL_DESC_TYPE of APD (application parameter descriptor)
        fOdbcParamSqlType, // 'Sql' data type of paremeter - Sets SQL_DESC_TYPE of IPD (implementation parameter descriptor)
        fOdbcParamCbColDef, fOdbcParamIbScale,
        fBindData, // pointer to the Data Value
        fBindOutputBufferLength, @fOdbcParamLenOrInd);
      // Second to last argument applies to Output (or Input/Output) parameters only
      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        OdbcCheck(OdbcRetcode, AnsiString('SQLBindParameter( paramNum='+IntToStr(ulParameter)+')'));
      Include(fStmtStatus, scsStmtBinded);
    end; //of: with aOdbcBindParam
  except
    on EDbxNotSupported do
      Result := DBXERR_NOTSUPPORTED;
    on E: Exception{EDbxError} do
    begin
      AddError(E);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc.setParameter', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc.setParameter'); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSQLMetaDataOdbc }

constructor TSQLMetaDataOdbc.Create;//(ASupportWideString: Boolean; OwnerDbxConnection: TSqlConnectionOdbc);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSQLMetaDataOdbc.Create'); {$ENDIF _TRACE_CALLS_}
  inherited Create;
  fSupportWideString := ASupportWideString;
  fMetaDataErrorLines := TStringList.Create;
  fOwnerDbxConnection := OwnerDbxConnection;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc.Create', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TSQLMetaDataOdbc.Destroy;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSQLMetaDataOdbc.Destroy'); {$ENDIF _TRACE_CALLS_}
  FreeAndNil(fMetaDataErrorLines);
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc.Destroy', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSQLMetaDataOdbc.getColumns;//(TableName, ColumnName: PAnsiChar;
//  ColType: Longword; out Cursor: ISQLCursor25): SQLResult;
var
  aCursor: TSqlCursorMetaDataColumns;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSQLMetaDataOdbc.getColumns', ['TableName =', TableName, 'ColumnName =', ColumnName, 'ColType =', ColType]); {$ENDIF _TRACE_CALLS_}
  Pointer(Cursor) := nil;
  {$IFDEF _DBX30_}
  if fOwnerDbxConnection.fOwnerDbxDriver.fDBXVersion >= 30 then
    aCursor := TSqlCursorMetaDataColumns3.Create(fSupportWideString, Self)
  else
  {$ENDIF}
    aCursor := TSqlCursorMetaDataColumns.Create(fSupportWideString, Self);
  try
    {+2.01}//Vadim V.Lopushansky:
    if fOwnerDbxConnection.fConnectionOptions[coSupportsMetadata] = osOn then
      {/+2.01}
      aCursor.FetchColumns(PAnsiChar(FMetaCatalogName), PAnsiChar(FMetaSchemaName),
        TableName, ColumnName, ColType);
    {$IFDEF _DBX30_}
    if fOwnerDbxConnection.fOwnerDbxDriver.fDBXVersion >= 30 then
        ISQLCursor30(Cursor) := TSqlCursorMetaDataColumns3(aCursor)
    else
    {$ENDIF}
      Cursor := aCursor;
  except
    on E: Exception{EDbxError} do
    begin
      aCursor.Free;
      Cursor := nil;
      CheckMaxLines(fMetaDataErrorLines);
      fMetaDataErrorLines.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc.getColumns', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc.getColumns'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSQLMetaDataOdbc.getErrorMessage;//(Error: PAnsiChar): SQLResult;
begin
  if Error=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  StrCopy(Error, PAnsiChar(AnsiString(fMetaDataErrorLines.Text)));
  fMetaDataErrorLines.Clear;
  Result := DBXERR_NONE;
end;

function TSQLMetaDataOdbc.getErrorMessageLen;//(out ErrorLen: Smallint): SQLResult;
begin
  ErrorLen := Length(fMetaDataErrorLines.Text);
  Result := DBXERR_NONE;
end;

function TSQLMetaDataOdbc.getIndices;//(TableName: PAnsiChar;
//  IndexType: Longword; out Cursor: ISQLCursor25): SQLResult;
var
  aCursor: TSqlCursorMetaDataIndexes;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSQLMetaDataOdbc.getIndices', ['TableName =', TableName, 'IndexType =', IndexType]); {$ENDIF _TRACE_CALLS_}
  Pointer(Cursor) := nil;
  {$IFDEF _DBX30_}
  if fOwnerDbxConnection.fOwnerDbxDriver.fDBXVersion >= 30 then
    aCursor := TSqlCursorMetaDataIndexes3.Create(fSupportWideString, Self)
  else
  {$ENDIF}
    aCursor := TSqlCursorMetaDataIndexes.Create(fSupportWideString, Self);
  try
    {+2.01}//Vadim V.Lopushansky:
    if fOwnerDbxConnection.fConnectionOptions[coSupportsMetadata] = osOn then
      {/+2.01}
      if fOwnerDbxConnection.fSupportsSQLSTATISTICS then
        aCursor.FetchIndexes(PAnsiChar(FMetaCatalogName), PAnsiChar(FMetaSchemaName),
          TableName, IndexType);
    {$IFDEF _DBX30_}
    if fOwnerDbxConnection.fOwnerDbxDriver.fDBXVersion >= 30 then
      ISQLCursor30(Cursor) := TSqlCursorMetaDataIndexes3(aCursor)
    else
    {$ENDIF}
      Cursor := aCursor;
  except
    on E: Exception{EDbxError} do
    begin
      aCursor.Free;
      Cursor := nil;
      CheckMaxLines(fMetaDataErrorLines);
      fMetaDataErrorLines.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc.getIndices', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc.getIndices'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSQLMetaDataOdbc.getObjectList;//(eObjType: TSQLObjectType;
//  out Cursor: ISQLCursor25): SQLResult;
begin
  Result := DBXERR_NOTSUPPORTED;
  Pointer(Cursor) := nil;
  {
  try
    raise EDbxNotSupported.Create(
      'TSQLMetaDataOdbc.getObjectList - not yet supported');
  except
    on E: EDbxNotSupported do
      Result := DBXERR_NOTSUPPORTED;
  end;
  }
end;

function TSQLMetaDataOdbc.GetOption;//(eDOption: TSQLMetaDataOption;
//  PropValue: Pointer; MaxLength: Smallint; out Length: Smallint): SQLResult;
var
  xeDOption: TXSQLMetaDataOption absolute eDOption;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
    try try
    {$R+}
    LogEnterProc('TSQLMetaDataOdbc.GetOption', ['eDOption =', cSQLMetaDataOption[xeDOption]]);
    {$IFDEF RANGECHECKS_OFF} {$R-} {$ENDIF}
  {$ENDIF _TRACE_CALLS_}
  try
    case xeDOption of
      xeMetaCatalogName:
        GetStringOptions(Self, fMetaCatalogName, PAnsiChar(PropValue), MaxLength, Length,
          eiMetaCatalogName);
      xeMetaSchemaName:
        GetStringOptions(Self, fMetaSchemaName, PAnsiChar(PropValue), MaxLength, Length,
          eiMetaSchemaName);
{.$IFDEF _K3UP_}
      xeMetaPackageName:
        GetStringOptions(Self, fMetaPackName, PAnsiChar(PropValue), MaxLength, Length,
          eiMetaPackageName);
{.$ENDIF}
      else
        Result := fOwnerDbxConnection.GetMetaDataOption(eDOption, PropValue, MaxLength, Length);
    end;
  except
    on E: EDbxNotSupported do
    begin
      Length := 0;
      Result := DBXERR_NOTSUPPORTED;
    end;
    on E: EDbxInvalidParam do
    begin
      Length := 0;
      Result := DBXERR_INVALIDPARAM;
    end;
    on E: Exception{EDbxError} do
    begin
      Length := 0;
      CheckMaxLines(fMetaDataErrorLines);
      fMetaDataErrorLines.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc.GetOption', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc.GetOption'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSQLMetaDataOdbc.getProcedureParams;//(ProcName, ParamName: PAnsiChar;
//  out Cursor: ISQLCursor25): SQLResult;
var
  aCursor: TSqlCursorMetaDataProcedureParams;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSQLMetaDataOdbc.getProcedureParams', ['ProcName =', ProcName, 'ParamName =', ParamName]); {$ENDIF _TRACE_CALLS_}
  Pointer(Cursor) := nil;
  {$IFDEF _DBX30_}
  if fOwnerDbxConnection.fOwnerDbxDriver.fDBXVersion >= 30 then
    aCursor := TSqlCursorMetaDataProcedureParams3.Create(fSupportWideString, Self)
  else
  {$ENDIF}
    aCursor := TSqlCursorMetaDataProcedureParams.Create(fSupportWideString, Self);
  try
    {+2.01}//Vadim V.Lopushansky:
    if fOwnerDbxConnection.fConnectionOptions[coSupportsMetadata] = osOn then
      {/+2.01}
      aCursor.FetchProcedureParams(PAnsiChar(FMetaCatalogName), PAnsiChar(FMetaSchemaName),
        ProcName, ParamName);
    {$IFDEF _DBX30_}
    if fOwnerDbxConnection.fOwnerDbxDriver.fDBXVersion >= 30 then
      ISQLCursor30(Cursor) := TSqlCursorMetaDataProcedureParams3(aCursor)
    else
    {$ENDIF}
      Cursor := aCursor;
  except
    on E: Exception{EDbxError} do
    begin
      aCursor.Free;
      Cursor := nil;
      CheckMaxLines(fMetaDataErrorLines);
      fMetaDataErrorLines.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc.getProcedureParams', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc.getProcedureParams'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSQLMetaDataOdbc.getProcedures;//(ProcedureName: PAnsiChar;
//  ProcType: Longword; out Cursor: ISQLCursor25): SQLResult;
var
  aCursor: TSqlCursorMetaDataProcedures;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSQLMetaDataOdbc.getProcedures', ['ProcedureName =', ProcedureName, 'ProcType =', ProcType]); {$ENDIF _TRACE_CALLS_}
  Pointer(Cursor) := nil;
  {$IFDEF _DBX30_}
  if fOwnerDbxConnection.fOwnerDbxDriver.fDBXVersion >= 30 then
    aCursor := TSqlCursorMetaDataProcedures3.Create(fSupportWideString, Self)
  else
  {$ENDIF}
    aCursor := TSqlCursorMetaDataProcedures.Create(fSupportWideString, Self);
  try
    {+2.01}//Vadim V.Lopushansky:
    if fOwnerDbxConnection.fConnectionOptions[coSupportsMetadata] = osOn then
      {/+2.01}
      aCursor.FetchProcedures(ProcedureName, ProcType);
    {$IFDEF _DBX30_}
    if fOwnerDbxConnection.fOwnerDbxDriver.fDBXVersion >= 30 then
      ISQLCursor30(Cursor) := TSqlCursorMetaDataProcedures3(aCursor)
    else
    {$ENDIF}
      Cursor := aCursor;
  except
    on E: Exception{EDbxError} do
    begin
      aCursor.Free;
      Cursor := nil;
      CheckMaxLines(fMetaDataErrorLines);
      fMetaDataErrorLines.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc.getProcedures', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc.getProcedures'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSQLMetaDataOdbc.DoGetTables;//(TableName: PAnsiChar;
//  TableType: Longword; out Cursor: Pointer; bUnicode: Boolean ): SQLResult; stdcall;
var
  aCursor: TSqlCursorMetaDataTables;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSQLMetaDataOdbc.DoGetTables', ['TableName =', StrPtrToString(TableName, bUnicode), 'TableType =', TableType, 'Unicode= ', bUnicode]); {$ENDIF _TRACE_CALLS_}
  Pointer(Cursor) := nil;
  {$IFDEF _DBX30_}
  if fOwnerDbxConnection.fOwnerDbxDriver.fDBXVersion >= 30 then
    aCursor := TSqlCursorMetaDataTables3.Create(fSupportWideString, Self)
  else
  {$ENDIF}
    aCursor := TSqlCursorMetaDataTables.Create(fSupportWideString, Self);
  try
    aCursor.FetchTables(TableName, TableType, bUnicode);

    {$IFDEF _DBX30_}
    if fOwnerDbxConnection.fOwnerDbxDriver.fDBXVersion >= 30 then
      ISQLCursor30(Cursor) := TSqlCursorMetaDataTables3(aCursor)
    else
    {$ENDIF}
      ISQLCursor25(Cursor) := aCursor;

  except
    on E: Exception{EDbxError} do
    begin
      aCursor.Free;
      Cursor := nil;
      CheckMaxLines(fMetaDataErrorLines);
      fMetaDataErrorLines.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc.DoGetTables', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc.DoGetTables'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSQLMetaDataOdbc.getTables;//(TableName: PAnsiChar;
//  TableType: Longword; out Cursor: ISQLCursor25): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSQLMetaDataOdbc.getTables', ['TableName =', StrPas(TableName), 'TableType =', TableType]); {$ENDIF _TRACE_CALLS_}
  Result := DoGetTables(TableName, TableType, Pointer(Cursor), {Unicode}False);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc.getTables', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc.getTables'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSQLMetaDataOdbc.SetOption;//(eDOption: TSQLMetaDataOption;
//  PropValue: Integer): SQLResult;
var
  xeDOption: TXSQLMetaDataOption absolute eDOption;
  {$IFDEF _TRACE_CALLS_}
  function PropValue2Str: AnsiString;
  begin
    case xeDOption of
      xeMetaCatalogName,
      xeMetaSchemaName,
      xeMetaDatabaseName,
      xeMetaPackageName:
        Result := AnsiString(StrPas(PAnsiChar(PropValue)));
      else
        Result := AnsiString(IntToStr(Integer(PropValue)));
    end;
  end;
  {$ENDIF _TRACE_CALLS_}
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
    try try
    {$R+}
    LogEnterProc('TSQLMetaDataOdbc.SetOption', ['eDOption =', cSQLMetaDataOption[xeDOption], 'PropValue =', PropValue2Str()]);
    {$IFDEF RANGECHECKS_OFF} {$R-} {$ENDIF}
  {$ENDIF _TRACE_CALLS_}
  try
    case xeDOption of
      xeMetaCatalogName:
        fMetaCatalogName := ExtractCatalog(StrPas(PAnsiChar(PropValue)),
          fOwnerDbxConnection.fOdbcCatalogPrefix);
      xeMetaSchemaName:
        if (fOwnerDbxConnection.fConnectionOptions[coSupportsSchemaFilter] = osOn) then
          fMetaSchemaName := StrPas(PAnsiChar(PropValue));
      xeMetaDatabaseName: // Read-only
        raise EDbxInvalidCall.Create(
          'TSQLMetaDataOdbc.SetOption(eMetaDatabaseName) not valid (Read-only)');
      xeMetaDatabaseVersion: // Read-only
        raise EDbxInvalidCall.Create(
          'TSQLMetaDataOdbc.SetOption(eMetaDatabaseVersion) not valid (Read-only)');
      xeMetaTransactionIsoLevel: // (Read-only:
        // use the options of SQLConnection to set the transaction isolation level)
        raise EDbxInvalidCall.Create(
          'TSQLMetaDataOdbc.SetOption(eMetaTransactionIsoLevel) not valid (Read-only) ' +
          '(Use options of ISQLConnection instead)');
      xeMetaSupportsTransaction: // Read-only
        raise EDbxInvalidCall.Create(
          'TSQLMetaDataOdbc.SetOption(eMetaSupportsTransaction) not valid (Read-only)');
      xeMetaMaxObjectNameLength: // Read-only
        raise EDbxInvalidCall.Create(
          'TSQLMetaDataOdbc.SetOption(eMetaMaxObjectNameLength) not valid (Read-only)');
      xeMetaMaxColumnsInTable: // Read-only
        raise EDbxInvalidCall.Create(
          'TSQLMetaDataOdbc.SetOption(eMetaMaxColumnsInTable) not valid (Read-only)');
      xeMetaMaxColumnsInSelect: // Read-only
        raise EDbxInvalidCall.Create(
          'TSQLMetaDataOdbc.SetOption(eMetaMaxColumnsInSelect) not valid (Read-only)');
      xeMetaMaxRowSize: // Read-only
        raise EDbxInvalidCall.Create(
          'TSQLMetaDataOdbc.SetOption(eMetaMaxRowSize) not valid (Read-only)');
      xeMetaMaxSQLLength: // Read-only
        raise EDbxInvalidCall.Create(
          'TSQLMetaDataOdbc.SetOption(eMetaMaxSQLLength) not valid (Read-only)');
      xeMetaObjectQuoteChar: // Read-only
        raise EDbxInvalidCall.Create(
          'TSQLMetaDataOdbc.SetOption(eMetaObjectQuoteChar) not valid (Read-only)');
      xeMetaSQLEscapeChar: // Read-only
        raise EDbxInvalidCall.Create(
          'TSQLMetaDataOdbc.SetOption(eMetaSQLEscapeChar) not valid (Read-only)');
      xeMetaProcSupportsCursor: // Read-only
        raise EDbxInvalidCall.Create(
          'TSQLMetaDataOdbc.SetOption(eMetaProcSupportsCursor) not valid (Read-only)');
      xeMetaProcSupportsCursors: // Read-only
        raise EDbxInvalidCall.Create(
          'TSQLMetaDataOdbc.SetOption(eMetaProcSupportsCursors) not valid (Read-only)');
      xeMetaSupportsTransactions: // Read-only
        raise EDbxInvalidCall.Create(
          'TSQLMetaDataOdbc.SetOption(eMetaSupportsTransactions) not valid (Read-only)');
{.$IFDEF _K3UP_}
      xeMetaPackageName:
        FMetaPackName := StrPas(PAnsiChar(PropValue));
{.$ENDIF}
    end;
  except
    on E: EDbxNotSupported do
      Result := DBXERR_NOTSUPPORTED;
    on E: EDbxInvalidCall do
      Result := DBXERR_INVALIDPARAM;
    on E: Exception{EDbxError} do
    begin
      CheckMaxLines(fMetaDataErrorLines);
      fMetaDataErrorLines.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc.SetOption', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc.SetOption', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSqlCursorOdbc }

constructor TSqlCursorOdbc.Create;//(OwnerCommand: TSqlCommandOdbc);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc(AnsiString(ClassName) + '.Create'); {$ENDIF _TRACE_CALLS_}
  inherited Create;

  fOwnerCommand := OwnerCommand;
  fOwnerDbxConnection := OwnerCommand.fOwnerDbxConnection;
  fOwnerDbxDriver := fOwnerDbxConnection.fOwnerDbxDriver;
  fHStmt := OwnerCommand.fHStmt;
  Include(OwnerCommand.fStmtStatus, scsIsCursor);
  fOdbcColumnsFetchConsecutively := fOwnerDbxConnection.fConnectionOptions[coCFC] <> osOff;

  if fOwnerDbxConnection.fStatementPerConnection > 0 then
    inc(fOwnerCommand.fDbxConStmtInfo.fDbxConStmt.fActiveCursors);
  inc(fOwnerDbxConnection.fActiveCursors);

  if OwnerCommand.fSupportsBlockRead and fOwnerDbxConnection.fSupportsBlockRead then
    fCursorFetchRowCount := OwnerCommand.fCommandRowSetSize
  else
    fCursorFetchRowCount := 1;

  BindResultSet;

  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc(AnsiString(ClassName) + '.Create', e);  raise; end; end;
    finally LogExitProc(AnsiString(ClassName) + '.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCursorOdbc.ClearCursor;//(bFreeStmt: Boolean);
var
  OdbcRetcode: OdbcApi.SqlReturn;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorOdbc.ClearCursor'); {$ENDIF _TRACE_CALLS_}
  if fHStmt = SQL_NULL_HANDLE then
    Exit;

  with fOwnerDbxDriver.fOdbcApi do
  begin

    if (fOwnerDbxConnection.fStatementPerConnection > 0) then
      dec(fOwnerCommand.fDbxConStmtInfo.fDbxConStmt.fActiveCursors);
    dec(fOwnerDbxConnection.fActiveCursors); // ??? conflicted with SQLMoreResults

    if fOwnerCommand.fIsMoreResults < 0 then
    begin
      OdbcRetcode := fOwnerCommand.DoSQLMoreResults(); // fHStmt == fOwnerCommand.fHStmt
      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        fOwnerCommand.fIsMoreResults := 0
      else
        fOwnerCommand.fIsMoreResults := 2
    end;

    if fOwnerCommand.fIsMoreResults = 0 then
      fOwnerCommand.CloseStmt({ClearParams}True, bFreeStmt);

    fHStmt := SQL_NULL_HANDLE;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.ClearCursor', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.ClearCursor'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TSqlCursorOdbc.Destroy;
var
  i: Integer;
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc(AnsiString(ClassName) + '.Destroy'); {$ENDIF _TRACE_CALLS_}

  ClearCursor(False);

  if fOdbcBindList <> nil then
  begin
    for i := fOdbcBindList.Count - 1 downto 0 do
    begin
      aOdbcBindCol := TOdbcBindCol(fOdbcBindList[i]);
      fOdbcBindList[i] := nil;
      aOdbcBindCol.Free;
    end;
    FreeAndNil(fOdbcBindList);
  end;

  FreeMemAndNil(fOdbcBindBuffer);
  SetLength(fOdbcRowsStatus, 0);

  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc(AnsiString(ClassName) + '.Destroy', e);  raise; end; end;
    finally LogExitProc(AnsiString(ClassName) + '.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCursorOdbc.OdbcCheck;//(OdbcCode: SqlReturn;
//  const OdbcFunctionName: AnsiString; maxErrorCount: Integer = 0);
begin
  fOwnerDbxDriver.OdbcCheck(OdbcCode, OdbcFunctionName, SQL_HANDLE_STMT, fHStmt,
    fOwnerCommand.fDbxConStmtInfo.fDbxConStmt, nil, nil, Self, maxErrorCount);
end;

{$IFDEF _TRACE_CALLS_}
function OdbcSqlTypeToStr(OdbcType: Integer; OdbcDriverType: TOdbcDriverType): AnsiString;
var
  bFieldHandled: Boolean;
begin
  case OdbcType of
    SQL_INTEGER: Result := 'SQL_INTEGER';
    SQL_BIGINT: Result := 'SQL_BIGINT';
    SQL_SMALLINT: Result := 'SQL_SMALLINT';
    SQL_TINYINT: Result := 'SQL_TINYINT';
    SQL_NUMERIC: Result := 'SQL_NUMERIC';
    SQL_DECIMAL: Result := 'SQL_DECIMAL';
    SQL_DOUBLE: Result := 'SQL_DOUBLE';
    SQL_FLOAT: Result := 'SQL_FLOAT';
    SQL_REAL: Result := 'SQL_REAL';
    SQL_CHAR: Result := 'SQL_CHAR';
    SQL_VARCHAR: Result := 'SQL_VARCHAR';
    SQL_WCHAR: Result := 'SQL_WCHAR';
    SQL_WVARCHAR: Result := 'SQL_WVARCHAR';
    SQL_GUID: Result := 'SQL_GUID';
    SQL_BINARY: Result := 'SQL_BINARY';
    SQL_VARBINARY: Result := 'SQL_VARBINARY';
    SQL_TYPE_DATE: Result := 'SQL_TYPE_DATE';
    SQL_TYPE_TIME: Result := 'SQL_TYPE_TIME';
    SQL_TIME: Result := 'SQL_TIME';
    SQL_TYPE_TIMESTAMP: Result := 'SQL_TYPE_TIMESTAMP';
    SQL_DATETIME: Result := 'SQL_DATETIME';
    SQL_TIMESTAMP: Result := 'SQL_TIMESTAMP';
    SQL_BIT: Result := 'SQL_BIT';
    SQL_LONGVARCHAR: Result := 'SQL_LONGVARCHAR';
    SQL_WLONGVARCHAR: Result := 'SQL_WLONGVARCHAR';
    SQL_LONGVARBINARY: Result := 'SQL_LONGVARBINARY';
    SQL_INTERVAL_YEAR: Result := 'SQL_INTERVAL_YEAR';
    SQL_INTERVAL_MONTH: Result := 'SQL_INTERVAL_MONTH';
    SQL_INTERVAL_DAY: Result := 'SQL_INTERVAL_DAY';
    SQL_INTERVAL_HOUR: Result := 'SQL_INTERVAL_HOUR';
    SQL_INTERVAL_MINUTE: Result := 'SQL_INTERVAL_MINUTE';
    SQL_INTERVAL_SECOND: Result := 'SQL_INTERVAL_SECOND';
    SQL_INTERVAL_YEAR_TO_MONTH: Result := 'SQL_INTERVAL_YEAR_TO_MONTH';
    SQL_INTERVAL_DAY_TO_HOUR: Result := 'SQL_INTERVAL_DAY_TO_HOUR';
    SQL_INTERVAL_DAY_TO_MINUTE: Result := 'SQL_INTERVAL_DAY_TO_MINUTE';
    SQL_INTERVAL_DAY_TO_SECOND: Result := 'SQL_INTERVAL_DAY_TO_SECOND';
    SQL_INTERVAL_HOUR_TO_MINUTE: Result := 'SQL_INTERVAL_HOUR_TO_MINUTE';
    SQL_INTERVAL_HOUR_TO_SECOND: Result := 'SQL_INTERVAL_HOUR_TO_SECOND';
    SQL_INTERVAL_MINUTE_TO_SECOND: Result := 'SQL_INTERVAL_MINUTE_TO_SECOND';
    else
      begin
        bFieldHandled := False;
        if (OdbcDriverType = eOdbcDriverTypeInformix) then
        case OdbcType of
          SQL_INFX_UDT_BLOB:
            begin
              bFieldHandled := True;
              Result := 'SQL_INFX_UDT_BLOB';
            end;
          SQL_INFX_UDT_CLOB:
            begin
              bFieldHandled := True;
              Result := 'SQL_INFX_UDT_CLOB';
            end;
          SQL_INFX_UDT_FIXED:
            begin
              bFieldHandled := True;
              Result := 'SQL_INFX_UDT_FIXED';
            end;
          SQL_INFX_UDT_VARYING:
            begin
              bFieldHandled := True;
              Result := 'SQL_INFX_UDT_VARYING';
            end;
          SQL_INFX_UDT_LVARCHAR:
            begin
              bFieldHandled := True;
              Result := 'SQL_INFX_UDT_LVARCHAR';
            end;
          SQL_INFX_RC_ROW:
            begin
              bFieldHandled := True;
              Result := 'SQL_INFX_RC_ROWL';
            end;
          SQL_INFX_RC_COLLECTION:
            begin
              bFieldHandled := True;
              Result := 'SQL_INFX_RC_COLLECTION';
            end;
          SQL_INFX_RC_LIST:
            begin
              bFieldHandled := True;
              Result := 'SQL_INFX_RC_LIST';
            end;
          SQL_INFX_RC_SET:
            begin
              bFieldHandled := True;
              Result := 'SQL_INFX_RC_SET';
            end;
          SQL_INFX_RC_MULTISET:
            begin
              bFieldHandled := True;
              Result := 'SQL_INFX_RC_MULTISET';
            end;
        end //of: case 2
        else if (OdbcDriverType = eOdbcDriverTypeOracle) then
        case OdbcType of
          SQL_ORA_CURSOR:
            begin
              bFieldHandled := True;
              Result := 'SQL_ORA_CURSOR';
            end;
        end;
        if not bFieldHandled then
          Result := 'Unknown';
      end;
  end;//of: case 1
end;
{$ENDIF IFDEF _TRACE_CALLS_}

procedure TSqlCursorOdbc.BindResultSet;
const
  COLUMN_BIND_SIZE_LIMIT = High(SmallInt)-1;{ == "32767 - 1" }
var
  OdbcRetcode: OdbcApi.SqlReturn;
  aOdbcBindCol: TOdbcBindCol;
  ColNo: Integer;
  ColNameTemp: PAnsiChar;
  pCharTemp: PAnsiChar;
  IntAttribute: SqlInteger;
  IntResult: SqlInteger;
  OdbcLateBoundFound: Boolean;
  DefaultFieldName: AnsiString;
  LastColNo: Integer;
  bFieldHandled: Boolean;
  vCursorFetchRowCount: Integer;
  vLastHostVarAddress: Pointer;
  vUnbindedColsBuffSize: Integer;
  vUnbindedFirstColIdx: Integer; // first column allocate all buffer
  vBindedColsCnt: Integer;
  aOdbcBindColPrev: TOdbcBindCol;
  vOdbcMaxColumnNameLen: Integer;
  bUnicodeString: Boolean;
  vOdbcHostVarTypeFix: SqlSmallint;
  // ---
  procedure CheckAutoIncSubType;
  begin
    with aOdbcBindCol, fOwnerDbxDriver.fOdbcApi do
    begin
      if Self.fOwnerDbxConnection.fConnectionOptions[coSupportsAutoInc] = osOn then
      begin
        IntAttribute := OdbcApi.SQL_FALSE;
        // Check to see if field is an AUTO-INCREMENTING value
        OdbcRetcode := SQLColAttributeInt(fHStmt, ColNo, SQL_DESC_AUTO_UNIQUE_VALUE,
          nil, 0, nil, IntAttribute);
        {+2.01}
        // SQLite does not support this option
        // Old code:
        // if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
        //   OdbcCheck(OdbcRetCode, 'SQLColAttribute(SQL_DESC_AUTO_UNIQUE_VALUE)');
        // if (IntAttribute = SQL_TRUE) then
        //   fDbxSubType:= fldstAUTOINC;
        // New code:
        if (OdbcRetcode = OdbcApi.SQL_SUCCESS) and (IntAttribute = SQL_TRUE) then
          fDbxSubType := fldstAUTOINC
        else
        if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
          fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
            fOwnerCommand.fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, fOwnerCommand, nil, 1);
        {/+2.01}
      end;
    end;
  end;
  // ---
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorOdbc.BindResultSet'); {$ENDIF _TRACE_CALLS_}

  ColNameTemp := nil;
  OdbcLateBoundFound := False;
  with fOwnerDbxDriver.fOdbcApi do
  try
    vOdbcMaxColumnNameLen := fOwnerDbxConnection.fOdbcMaxColumnNameLen;
    ColNameTemp := AllocMem(vOdbcMaxColumnNameLen + 1);
    // Get no of columns:
    OdbcRetcode := SQLNumResultCols(fHStmt, fOdbcNumCols);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetcode, 'SQLNumResultCols');
    // Set up bind columns:
    if (fOdbcBindList <> nil) then
    begin
      for ColNo := fOdbcBindList.Count - 1 downto 0 do
        TOdbcBindCol(fOdbcBindList[ColNo]).Free;
      fOdbcBindList.Free;
    end;
    fOdbcBindList := TList.Create;
    fOdbcBindList.Count := fOdbcNumCols;
    LastColNo := 0;

    // Describe each column...
    for ColNo := 1 to fOdbcNumCols do
    begin
      aOdbcBindCol := TOdbcBindCol.Create;
      fOdbcBindList.Items[LastColNo] := aOdbcBindCol;

      with aOdbcBindCol do
      begin
        fOdbcColNo := ColNo;
        vOdbcHostVarTypeFix := 0;
        //fColNameW
        OdbcRetcode := SQLDescribeCol(
          fHStmt, fOdbcColNo,
          ColNameTemp, vOdbcMaxColumnNameLen + 1, fColNameSize,
          fSqlType, fColSize, fColScale, fNullable);
        if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        begin
          if vOdbcMaxColumnNameLen < cOdbcMaxColumnNameLenDefault then
          begin
            fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
              fOwnerCommand.fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, fOwnerCommand, nil, 1);
            if vOdbcMaxColumnNameLen <> fOwnerDbxConnection.fOdbcMaxColumnNameLen then
            begin
              FreeMemAndNil(ColNameTemp);
              vOdbcMaxColumnNameLen := fOwnerDbxConnection.fOdbcMaxColumnNameLen;
              ColNameTemp := AllocMem(vOdbcMaxColumnNameLen + 1);
            end;
            OdbcRetcode := SQLDescribeCol(
              fHStmt, fOdbcColNo,
              ColNameTemp, vOdbcMaxColumnNameLen + 1, fColNameSize,
              fSqlType, fColSize, fColScale, fNullable);
            if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
              OdbcCheck(OdbcRetcode, 'SQLDescribeCol');
          end
          else
            OdbcCheck(OdbcRetcode, 'SQLDescribeCol');
        end;

        // Trim Column Name:
        if (fColNameSize > 0) then
        begin
          pCharTemp := @ColNameTemp[fColNameSize-1];
          fColName := ColNameTemp;
          while (pCharTemp >= fColName) and (pCharTemp^=' ') do
            dec(pCharTemp);
          fColNameSize := DWORD(pCharTemp)-DWORD(fColName)+1;
        end;

        if (fColNameSize <= 0) then
          // Allow for blank column names (returned by Informix stored procedures),
          // blank column names are also returned by functions, eg Max(Col)
          // Added v1.4 2002-01-16, for Bulent Erdemir
          // (Similar fix also posted by Michael Schwarzl)
        begin
          DefaultFieldName := 'Column_' + AnsiString(IntToStr(ColNo));
          fColNameSize := Length(DefaultFieldName);
          GetMem(fColName, fColNameSize + 1);
          StrLCopy(fColName, PAnsiChar(DefaultFieldName), fColNameSize + 1);
        end
        else
        begin
          GetMem(fColName, fColNameSize + 1);
          StrLCopy(fColName, ColNameTemp, fColNameSize + 1);
        end;

        if (fOwnerDbxConnection.fOdbcDriverType in [eOdbcDriverTypeMsSqlServer, eOdbcDriverTypeMsSqlServer2005Up]) then
        begin
          case fSqlType of
            SQL_CHAR, SQL_VARCHAR, SQL_WCHAR, SQL_WVARCHAR:
              begin
                if fColSize <= 0 then
                begin
                  case fSqlType of
                    SQL_CHAR, SQL_VARCHAR:
                      fSqlType := SQL_LONGVARCHAR;
                    else {SQL_WCHAR, SQL_WVARCHAR:}
                      fSqlType := SQL_WLONGVARCHAR;
                  end;
                end;
              end;
            SQL_MSSQL_VARIANT: // sql_variant: SELECT value FROM "dbo"."sysproperties"
              begin
                if fOwnerDbxConnection.fConnectionOptions[coEnableUnicode] = osOn then
                  fSqlType := SQL_WVARCHAR
                else
                  fSqlType := SQL_VARCHAR;
              end;
            SQL_GUID:
              begin
                //if fOwnerDbxConnection.fDbmsVersionMajor < 9 then // MSSQL 2000 Down
                //  fColSize := 36
                //else
                //  fColSize := 38; // MSSQL 2005 Up: guid wrapped into {}
                fColSize := 36;
              end;
            SQL_MSSQL_XML:
              begin
                if fOwnerDbxConnection.fConnectionOptions[coEnableUnicode] = osOn then
                  fSqlType := SQL_WLONGVARCHAR
                else
                  fSqlType := SQL_LONGVARCHAR;
              end;
            {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
            (*
            SQL_BINARY:
              begin
                if fColSize = 8 then
                begin
                  //
                  // timestamp is a  data type that exposes automatically generated binary numbers, which are guaranteed to be unique within a database.
                  // timestamp is used typically as a mechanism for version-stamping table rows. The storage size is 8 Byte.
                  //
                  fReadOnly := 1;
                  //
                  // this field type (timestamp) can be not changed
                  // this field changes after change of any other field
                  // so its it is impossible use in offers where
                  // if field in CDS.Provider are updated on rule "UpdateMode=upWhereAll",
                  // that you will not be able again update the field, since server will change
                  // this field but you have not its new value.
                  // decision: use rule upWhereKeyOnly or update field after each apply updates
                end;
              end;
            //*)
            {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
          end;
        end
        else
        begin
          case fSqlType of
            SQL_GUID:
              begin
                if not (fColSize in [36, 38]) then
                  fSqlType := SQL_UNKNOWN_TYPE;
              end;
          end;
        end;

        {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
        (*
         SQL_DATETIME == SQL_DATE
            SQL_DATETIME = 9          // Standard SQL data type codes
            SQL_DATE = 9;             // SQL extended datatypes
         In ODBC 3.x, the SQL date, time, and timestamp data types are
           SQL_TYPE_DATE, SQL_TYPE_TIME, and SQL_TYPE_TIMESTAMP, respectively;
         in ODBC 2.x, the data types are
           SQL_DATE, SQL_TIME, and SQL_TIMESTAMP.
        *)
        {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
        if (fSqlType = SQL_DATETIME) and (OdbcDriverLevel < 3) {and (OdbcDriverLevel > 0)} then
          fSqlType := SQL_TYPE_DATE;

        fColValueSizePtr := @fColValueSizeLoc;
        fDbxSubType := 0;
        fIsBuffer:= False;
        {$IFDEF _TRACE_CALLS_}
           LogInfoProc(['Column =', StrPas(fColname), 'OdbcColType =', OdbcSqlTypeToStr(fSqlType, fOwnerDbxConnection.fOdbcDriverType)]);
        {$ENDIF}

        case fSqlType of
          SQL_INTEGER:
            begin
              fDbxType := fldINT32;
              fOdbcHostVarType := SQL_C_LONG;
              fOdbcHostVarSize := SizeOf(SqlInteger);
              CheckAutoIncSubType();
            end;
          SQL_BIGINT:
            begin
              // DbExpress does not currently support Int64 - use Int32 instead!
              // Re-instate next 3 statements when DbExpress does support int64 }
              {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
              {
                fDbxType := fldINT64;
                fOdbcHostVarType := SQL_C_SBIGINT;
                fOdbcHostVarSize := SizeOf(SqlBigInt);
              }
              {+2.01}//Vadim V.Lopushansky:
              // Vadim> ???Vad>All: For supporting int64 remapping it to fldBCD type
              // Edward> This is a good idea.
              // Edward> ???Ed>All: I think it should be the default option,
              // Edward> I think the Borland dbexpress drivers map int64 to BCD
              // ???: I still think BCD should be default option - actually, I think we should REMOVE Int32 mapping
              {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
              {$IFDEF _DBX30_}
              if (fOwnerDbxDriver.fDBXVersion >= 30) and ((fOwnerDbxConnection.fConnectionOptions[coMapInt64ToBcd] = osOff)) then
              begin
                // QC: 58681: Delphi not correctly supported Int64 (TLargeintField). See db.pas: "function TParam.GetDataSize: Integer;".
                // The Field it is impossible will change since it is impossible calculate parameter data size.
                {$IFDEF _INT64_BUGS_FIXED_}
                fDbxType := fldINT64;
                fOdbcHostVarType := SQL_C_SBIGINT;
                fOdbcHostVarSize := SizeOf(SqlBigInt);
                {$ELSE}
                fDbxType := fldFLOAT;
                fOdbcHostVarType := SQL_C_DOUBLE;
                fOdbcHostVarSize := SizeOf(SqlDouble);
                {$ENDIF}
              end
              else
              {$ENDIF}
              if (fOwnerDbxConnection.fConnectionOptions[coMapInt64ToBcd] = osOff) or
                (fOwnerDbxConnection.fConnectionOptions[coEnableBCD] = osOff) then
              begin
                // Default code:
                {
                fDbxType := fldINT32;
                fOdbcHostVarType := SQL_C_LONG;
                fOdbcHostVarSize := SizeOf(SqlInteger);
                {}
                fDbxType := fldFLOAT;
                fOdbcHostVarType := SQL_C_DOUBLE;
                fOdbcHostVarSize := SizeOf(SqlDouble);
              end
              else
              begin
                // Remapping to BCD
                fDbxType := fldBCD;
                fOdbcHostVarType := SQL_C_CHAR; // Odbc prefers to return BCD as string
                fColSize := 18;
                fColScale := 0;
                fOdbcHostVarSize := fColSize + 3;
                // add 3 to number of digits: sign, decimal point, null terminator
              end;
              {/+2.01}
              CheckAutoIncSubType();
            end;
          SQL_SMALLINT, SQL_TINYINT:
            begin
              fDbxType := fldINT16;
              fOdbcHostVarType := SQL_C_SHORT;
              fOdbcHostVarSize := SizeOf(SqlSmallint);
              CheckAutoIncSubType();
            end;
          SQL_NUMERIC, SQL_DECIMAL:
            begin
              if (fOwnerDbxConnection.fConnectionOptions[coEnableBCD] = osOff) or
                 (fColSize > MaxFMTBcdDigits) // Not supported more then MaxFMTBcdDigits
              then
              begin
                // Map BCD to Float as in BDE
                fDbxType := fldFLOAT;
                fOdbcHostVarType := SQL_C_DOUBLE;
                fOdbcHostVarSize := SizeOf(SqlDouble);
              end
              else
              if (fOwnerDbxConnection.fConnectionOptions[coMaxBCD] = osOn) then
              begin
                fDbxType := fldBCD;
                fOdbcHostVarType := SQL_C_CHAR; // Odbc prefers to return BCD as string
                if (fColSize - fColScale <= 2) then // fix BCD error info
                  inc(fColSize);
                if (fColScale + fColSize) > MaxFMTBcdDigits then
                  fColScale := MaxFMTBcdDigits - fColSize;
                fColSize := MaxFMTBcdDigits;
                fOdbcHostVarSize := fColSize + 3;
              end
              else
              begin
                if not (fColSize >= 0) then //Fix for null float fields
                  raise EDbxOdbcError.Create(
                    'ODBC function "SQLDescribeCol" returned Column Size < 1 for SQL_NUMERIC or SQL_DECIMAL' + #13#10
                    + 'Column name=' + string(StrPas(fColname))
                    + ' Scale=' + IntToStr(fColScale)
                    + ' Size=' + IntToStr(fColSize));
                fDbxType := fldBCD;
                fOdbcHostVarType := SQL_C_CHAR; // Odbc prefers to return BCD as string

                {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
                {+2.01 Workaround for bad MERANT driver}
                // Vadim> ???Vad>All: MERANT 2.10 ODBC-OLE DB Adapter Driver: Error: "BCD Everflow" on query:
                // Edward> ???Ed>Vad: Which underlying DBMS were you connecting to with this driver?
                // Edward> I have never heard of this ODBC-OLE DB Adapter driver
                // Edward> only the other way round!
                // Edward> ???Ed>Ed: We sould have another eOdbcDriverType for this driver
                //
                //   select first 1
                //     unit_price
                //   from
                //    stores7:stock
                //
                // Native ODBC:
                // aOdbcBindCol = ('unit_price', 10, 3, 6, 2, 1, 0, 8, 0, 1, 9, ...
                // Merant bad format:
                // aOdbcBindCol = ('unit_price', 10, 3, 6, 4, 1, 0, 8, 0, 1, 9, ...
                //           value: 250.000
                // INOLE
                {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}

                if (fColSize - fColScale <= 2)
{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
// Vadim> ???Vad>All: for any driver?
// Edward> OK. It does no harm to other drivers if ColSize is 1 bigger
// Edward> to allow for this bug in Merant driver.

// and // Detect "MERANT 2.10 ODBC-OLE DB Adapter Driver"
{// ( Pos('INOLE',UpperCase(fOwnerDbxConnection.fOdbcDriverName))=1 )}then
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
                  inc(fColSize);
                {/+2.01 /Workaround for bad MERANT driver}

                // for 'Open Firebird, Interbase6 ODBC Driver': http://www.xtgsystems.com/
                if (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeInterbase) and
                   ( StrLComp(PAnsiChar(UpperCase(fOwnerDbxConnection.fOdbcDriverName)),
                     'IB6XTG', 6 ) = 0 )
                then
                  fColSize := fColSize * 2 - 1;

                fOdbcHostVarSize := fColSize + 3;
                // add 3 to number of digits: sign, decimal point, null terminator
              {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
              {+2.01 Workaround for bad INFORMIX behavior}
              //INFORMIX:
              {
              fColScale mast be less or equal fColSize.
              "INFORMIX 3.32 32 BIT" ODBC Returned fColScale equal 255 in next example:
              1) script tables
              --------------------------------------------------
              create table tbl (custno FLOAT primary key);
              insert into tbl values (1);
              2) exexute next query in DbExpress TSQLQuery:
              --------------------------------------------------
              select custno+1 from tbl;
              --------------------------------------------------
              When executing returned error in SqlExpr.pas:
              "invalid field size."
              It is error in informix metadata.
              Example:
              1) create view v1_tbl (custno) as select custno+1 from tbl
              2) look metadata columns info for view "v1_tbl": custno DECIMAL (17,255).
              It error handled in DataDirect ODBC driver.
              }
              // INFORMIX: Error-checking in the metadata about the datatype of columns in informix

              // Edward> ???Ed>All: Really, this bug should be fixed in Informix DBMS,
              // Edward> not with such ugly hacks in here. But I have kept Vadim's fix.
              {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}

                if (fOwnerDbxConnection.fDbmsType = eDbmsTypeInformix)
                  and (fColSize <= 18) and (fColScale = 255) then
                begin
                  fDbxType := fldFLOAT;
                  fOdbcHostVarType := SQL_C_DOUBLE;
                  fOdbcHostVarSize := SizeOf(SqlDouble);
                end;
                {/+2.01 /Workaround for bad INFORMIX behavior}

                // if for any driver fColSize is changed then check of support of such size is necessary:
                if (fColSize > MaxFMTBcdDigits) // Not supported more then MaxFMTBcdDigits
                then
                begin
                  // Map BCD to Float as in BDE
                  fDbxType := fldFLOAT;
                  fOdbcHostVarType := SQL_C_DOUBLE;
                  fOdbcHostVarSize := SizeOf(SqlDouble);
                end;

                if (fColScale > fColSize) then
                  raise EDbxOdbcError.Create(
                    'ODBC function "SQLDescribeCol" returned Column Scale > Column Size' + #13#10
                    + 'Column name=' + string(StrPas(fColname))
                    + ' Scale=' + IntToStr(fColScale)
                    + ' Size=' + IntToStr(fColSize));
                {+2.01 Option for BCD mapping}
                // Vadim V.Lopushansky:
                // Vadim > ???Vad>All: If BCD is small then remap it to native type:
                // Edward> Nice idea.
                if fOwnerDbxConnection.fConnectionOptions[coMapSmallBcdToNative] = osOn then
                begin
                  if (fColSize <= 4) and (fColScale = 0) then
                  begin
                    fDbxType := fldINT16;
                    fOdbcHostVarType := SQL_C_SHORT;
                    fOdbcHostVarSize := SizeOf(SqlSmallInt);
                  end
                  else if (fColSize <= 9) and (fColScale = 0) then
                  begin
                    fDbxType := fldINT32;
                    fOdbcHostVarType := SQL_C_LONG;
                    fOdbcHostVarSize := SizeOf(SqlInteger);
                  end
                  else if (fColSize <= 10) then
                  begin
                    fDbxType := fldFLOAT;
                    fOdbcHostVarType := SQL_C_DOUBLE;
                    fOdbcHostVarSize := SizeOf(SqlDouble);
                  end
                end;
                {/+2.01 /Option for BCD mapping}
                if (fDbxType <> fldFLOAT) and (fColScale = 0) then
                  CheckAutoIncSubType();
              end;
            end;
          SQL_DOUBLE, SQL_FLOAT, SQL_REAL:
            begin
              fDbxType := fldFLOAT;
              fOdbcHostVarType := SQL_C_DOUBLE;
              fOdbcHostVarSize := SizeOf(SqlDouble);
            end;
          SQL_CHAR, SQL_VARCHAR, SQL_WCHAR, SQL_WVARCHAR, SQL_GUID:
            begin
              fDbxType := fldZSTRING;
              bUnicodeString := ((fSqlType = SQL_WCHAR) or (fSqlType = SQL_WVARCHAR)) and
                (fOwnerDbxConnection.fConnectionOptions[coEnableUnicode] = osOn);
              if bUnicodeString then
              begin
                if fOwnerDbxDriver.fClientVersion >= 30 then
                begin
                  fDbxType := fldWIDESTRING;
                end
                else
                begin
                  fDbxSubType := fldstWIDEMEMO;
                end;
                fOdbcHostVarType := SQL_C_WCHAR;
                if fSqlType = SQL_WCHAR then
                  fDbxSubType := fldstFIXED;
              end
              else if (fSqlType = SQL_CHAR) or (fSqlType = SQL_WCHAR) then
              begin // Fixed length field
                fOdbcHostVarType := SQL_C_CHAR;
                fDbxSubType := fldstFIXED;
              end
              else
              begin
                fOdbcHostVarType := SQL_C_CHAR;
              end;

              if fColSize <= 0 then
              begin
                fColSize := 1;
                // SQL SERVER 2005:
                {
                declare @content varchar(max)
                select @content = 'hello world'
                select content1 = @content
                }
                if (fOwnerDbxConnection.fOdbcDriverType in [eOdbcDriverTypeMsSqlServer, eOdbcDriverTypeMsSqlServer2005Up]) then
                begin
                  fColSize := 8000;
                end;
              end;
              fOdbcHostVarSize := fColSize + 1; // Add 1 for null terminator
              if fOdbcHostVarType = SQL_C_WCHAR then
                fOdbcHostVarSize := fOdbcHostVarSize * SizeOf(WideChar);

              {+2.03 INFORMIX LVARCHAR}
              { Vadim V.Lopushansky:
                 Fixed when error for mapping INFORMIX LVARCHAR type over native ODBC.
                 Example query: select amparam from sysindices
              }
              if ( (fColSize > 255) and
                 (fOwnerDbxConnection.fConnectionOptions[coMapCharAsBDE] = osOn) )
              then
              begin
                fDbxType := fldBLOB;
                if bUnicodeString then
                  fDbxSubType := fldstWIDEMEMO
                else
                  fDbxSubType := fldstMEMO;
              end;
              if {((fOwnerCommand.fCommandBlobSizeLimitK>0) and (fColSize > fOwnerCommand.fCommandBlobSizeLimitK*1024)) or} (fColSize > COLUMN_BIND_SIZE_LIMIT) then
              begin // large size:
                if (fOwnerCommand.fCommandBlobSizeLimitK <= 0) then
                begin
                  fOdbcLateBound := True;
                end
                else
                begin
                  { Vadim>???Vad>All if fColSize > 2 Gb ???
                     Informix native odbc supported fOdbcLateBound, but if not ?
                     DataDirect ODBC for this informix type return length 2048.
                  }
                  // trim column:
                  fColSize := 2048;
                  fOdbcHostVarSize := fColSize + 1;
                end;
              end;
              if fOdbcLateBound then
                fIsBuffer := True;
              if (Self.fOwnerDbxConnection.fDbmsType = eDbmsTypeFlashFiler) and (fColSize = 4) then
              begin
                IntAttribute := OdbcApi.SQL_FALSE;
                OdbcRetcode := SQLColAttributeInt(fHStmt, ColNo, SQL_DESC_AUTO_UNIQUE_VALUE,
                  nil, 0, nil, IntAttribute);
                if {AutoIncr=} (OdbcRetcode = OdbcApi.SQL_SUCCESS) and (IntAttribute = SQL_TRUE) then
                begin
                  fDbxType := fldINT32;
                  fOdbcHostVarType := SQL_C_LONG;
                  fOdbcHostVarSize := SizeOf(SqlInteger);
                  fDbxSubType := fldstAUTOINC;
                  fIsBuffer := False;
                end
                else
                if (OdbcRetcode = OdbcApi.SQL_SUCCESS) then
                  fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
                    fOwnerCommand.fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, fOwnerCommand, nil, 1);
              end;
              (*
              Field Size is very big (Firebird, IBPhoenix OpenSource ODBC Driver):
                CREATE TABLE ... (
                    ...
                    LANGUAGE_REQ     VARCHAR(15) [1:5] CHARACTER SET NONE
                );
              *)
              if fColSize > dsMaxStringSize then
              begin // analog of SQL_LONGVARCHAR:
                if bUnicodeString then
                begin
                  fDbxType := fldWIDESTRING;
                  fDbxSubType := fldstMEMO;
                  fOdbcHostVarType := SQL_C_WCHAR;
                end
                else
                begin
                  fDbxType := fldBLOB;
                  fDbxSubType := fldstMEMO;
                  fOdbcHostVarType := SQL_C_CHAR;
                end;
                if fOwnerCommand.fCommandBlobSizeLimitK > 0 then
                // If BLOBSIZELIMIT specified, we early bind, just like normal column
                // Otherwise get size and column data AFTER every row fetch, using SqlGetData
                begin
                  fOdbcHostVarSize := fOwnerCommand.fCommandBlobSizeLimitK * 1024;
                end
                else
                if (fOwnerDbxConnection.fOdbcDriverType <> eOdbcDriverTypeSQLite) then
                begin
                  fOdbcLateBound := True;
                end
                else // SQL LITE:
                begin
                  fOdbcHostVarSize := fColSize + 1;
                end;
                if fOdbcLateBound then
                  fIsBuffer := True;
              end;
            end;
          SQL_BINARY, SQL_VARBINARY:
            begin
              if fSqlType = SQL_BINARY then
                fDbxType := fldBYTES
              else
                fDbxType := fldVARBYTES;{ The first word is equal to an length of data }
              fOdbcHostVarType := SQL_C_BINARY;
              if fColSize > 0 then
                fOdbcHostVarSize := fColSize
              else
              begin
                if fColSize = 0 then fColSize := 1;
                fOdbcHostVarSize := -1;
              end;
              if (fColSize < 0) or (fColSize > COLUMN_BIND_SIZE_LIMIT) then
              begin
                if (fOwnerCommand.fCommandBlobSizeLimitK > 0) then
                begin
                  if (fColSize <= 0) or
                     (COLUMN_BIND_SIZE_LIMIT > fOwnerCommand.fCommandBlobSizeLimitK * 1024) then
                    // trim data
                    fOdbcHostVarSize := fOwnerCommand.fCommandBlobSizeLimitK * 1024;
                end
                else
                  fOdbcLateBound := True;
              end;
              // check/set buffer allocation status
              if fOdbcLateBound then
                fIsBuffer := True;
            end;
          SQL_TYPE_DATE: {SQL_DATE = SQL_DATETIME}
            begin
              fDbxType := fldDATE;
              //fOdbcHostVarType := SQL_C_DATE;
              fOdbcHostVarType := fOwnerDbxConnection.fBindMapDateTimeOdbc^[biDate];
              fOdbcHostVarSize := SizeOf(TSqlDateStruct);
            end;
          SQL_TYPE_TIME, SQL_TIME:
            begin
              fDbxType := fldTIME;
              //fOdbcHostVarType := SQL_C_TIME;
              fOdbcHostVarType := fOwnerDbxConnection.fBindMapDateTimeOdbc^[biTime];
              fOdbcHostVarSize := SizeOf(TSqlTimeStruct);
            end;
          SQL_DATETIME:
            begin
              fDbxType := fldDATETIME;
              //fOdbcHostVarType := SQL_C_TIMESTAMP;
              fOdbcHostVarType := fOwnerDbxConnection.fBindMapDateTimeOdbc^[biDateTime];
              fOdbcHostVarSize := SizeOf(TOdbcTimestamp);
            end;
          SQL_TIMESTAMP, SQL_TYPE_TIMESTAMP: { == fldTIMESTAMP but it not support in SqlExpr.pas:TCustomSQLDataSet.GetFieldData }
            begin
              //fDbxType := fldTIMESTAMP;
              // Delphi (6..2006) bug: fldTIMESTAMP not support in SqlExpr.pas:TCustomSQLDataSet.GetFieldData
              fDbxType := fldDATETIME;
              //fOdbcHostVarType := SQL_C_TIMESTAMP;
              fOdbcHostVarType := fOwnerDbxConnection.fBindMapDateTimeOdbc^[biDateTime];
              fOdbcHostVarSize := SizeOf(TOdbcTimestamp);
            end;
          SQL_BIT:
            begin
              fDbxType := fldBOOL;
              fOdbcHostVarType := SQL_C_BIT;
              fOdbcHostVarSize := SizeOf(SqlByte);
            end;
          SQL_LONGVARCHAR, SQL_WLONGVARCHAR:
            begin
              bUnicodeString := (fSqlType = SQL_WLONGVARCHAR) and
                (fOwnerDbxConnection.fConnectionOptions[coEnableUnicode] = osOn);
              if bUnicodeString then
              begin
                fDbxType := fldBLOB;
                fDbxSubType := fldstWIDEMEMO;
                fOdbcHostVarType := SQL_C_WCHAR;
              end
              else
              begin
                fDbxType := fldBLOB;
                fDbxSubType := fldstMEMO;
                fOdbcHostVarType := SQL_C_CHAR;
              end;
              if fOwnerCommand.fCommandBlobSizeLimitK > 0 then
              // If BLOBSIZELIMIT specified, we early bind, just like normal column
              // Otherwise get size and column data AFTER every row fetch, using SqlGetData
              begin
                fOdbcHostVarSize := fOwnerCommand.fCommandBlobSizeLimitK * 1024;
              end
              else
              if (fOwnerDbxConnection.fOdbcDriverType <> eOdbcDriverTypeSQLite) then
                fOdbcLateBound := True
              else // SQL LITE:
                fOdbcHostVarSize := fColSize + 1;
              if fOdbcLateBound then
                fIsBuffer := True;
              if fColSize=0 then
                fColSize := -1;
            end;
          SQL_LONGVARBINARY:
            begin
              fDbxType := fldBLOB;
              fDbxSubType := fldstBINARY;
              fOdbcHostVarType := SQL_C_BINARY;
              // We cannot Bind a BLOB - Determine size AFTER every row fetch
              // We igmore BlobSizeLimit, because binary data (Images etc) cannot normally be truncated
              fOdbcLateBound := True;
              fIsBuffer := True;
              if fColSize=0 then
                fColSize := -1;
            end;
          SQL_INTERVAL_YEAR..SQL_INTERVAL_MINUTE_TO_SECOND:
            begin
              fDbxType := fldZSTRING;
              fOdbcHostVarType := SQL_C_CHAR;
              fOdbcHostVarSize := 28;
              fDbxSubType := fldstFIXED;
            end;
        else
          begin
            bFieldHandled := False;
            if (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeInformix) then
            begin
              case fSqlType of
                SQL_INFX_UDT_BLOB, { INFORMIX BLOB } // fldstHBINARY
                SQL_INFX_UDT_CLOB: { INFORMIX CLOB } // fldstHMEMO
                  begin
                    //fDbxType := fldBLOB;
                    fDbxType := fldUnknown;
                    OdbcRetcode := SQLGetInfo(fOwnerDbxConnection.fhCon,
                      SQL_INFX_LO_PTR_LENGTH, @fOdbcHostVarSize, SizeOf(fOdbcHostVarSize), nil);
                    if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
                    begin
                      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC,
                        fOwnerDbxConnection.fhCon, nil, fOwnerDbxConnection, nil, nil, 1);
                      fOdbcHostVarSize := SizeOf(SqlByte);
                      //fOdbcLateBound := True;
                      // set buffer status
                      //fIsBuffer := True;
                    end;
                    {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
                    (*
                    fOdbcLateBound := True;
                    // set buffer status
                    fIsBuffer := True;
                    //*)
                    (*
                    if (fSqlType = SQL_INFX_UDT_BLOB) then
                    begin
                      fOdbcHostVarType := SQL_C_BINARY;
                      fDbxSubType := fldstHBINARY;
                    end
                    else
                    begin
                      fOdbcHostVarType := SQL_C_CHAR;
                      fDbxSubType := fldstHMEMO;
                    end;
                    //if fColSize=0 then fColSize := -1;
                    //*)
                    {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
                    fColSize := 0; // to hide a field from Delphi.
                    bFieldHandled := True;

                  end;
                SQL_INFX_UDT_FIXED,
                SQL_INFX_UDT_VARYING,
                SQL_INFX_UDT_LVARCHAR,
                SQL_INFX_RC_ROW,
                SQL_INFX_RC_COLLECTION,
                SQL_INFX_RC_LIST,
                SQL_INFX_RC_SET,
                SQL_INFX_RC_MULTISET:
                  begin
                    //if fSqlType = SQL_INFX_UDT_FIXED then
                    //  fDbxType := fldBYTES
                    //else
                    //if fSqlType = SQL_INFX_UDT_VARYING then
                    //  fDbxType := fldVARBYTES;
                    fDbxType := fldUnknown;

                    OdbcRetcode := SQLGetInfo(fOwnerDbxConnection.fhCon,
                      SQL_INFX_LO_PTR_LENGTH, @fOdbcHostVarSize, SizeOf(fOdbcHostVarSize), nil);
                    if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
                    begin
                      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_DBC,
                        fOwnerDbxConnection.fhCon, nil, fOwnerDbxConnection, nil, nil, 1);
                      fOdbcHostVarSize := SizeOf(SqlByte);
                    end;
                    fOdbcHostVarType := SQL_C_BINARY;
                    fColSize := 0; // to hide a field from Delphi.
                    bFieldHandled := True;
                  end;
              end;//of: case fSqlType of
            end;
            if (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeOracle) then
            begin
              case fSqlType of
                SQL_ORA_CURSOR:
                  begin
                    fDbxType := fldCursor;
                    fColSize := -1;
                    bFieldHandled := True;
                    fOdbcHostVarType := SQL_C_ULONG;
                  end;
              end;//of: case fSqlType of
            end;
            if not bFieldHandled then
            begin
              {$IFDEF _TRACE_CALLS_}
                 LogInfoProc(['WARNING: cannot bind column:"', StrPas(fColname), '",  OdbcType =', fSqlType]);
              {$ENDIF}
              {+2.03 IgnoreUnknownFieldType option}
              if (fOwnerDbxConnection.fConnectionOptions[coIgnoreUnknownFieldType] = osOn) and
                (not ((LastColNo = 0) and (ColNo = fOdbcNumCols)))
                {// when in query only one unknown field type}then
              begin
                fOdbcBindList.Items[LastColNo] := nil;
                aOdbcBindCol.Free;
                OdbcLateBoundFound := True; // field may be equal long type
                Continue;
              end
              else
                {/+2.03 /IgnoreUnknownFieldType option}
                raise EDbxOdbcError.Create(
                  'ODBC function "SQLDescribeCol" returned unknown data type' + #13#10 +
                  'Data type code = "' + IntToStr(fSqlType) + '" ' +
                  'Column name = "' + string(StrPas(fColname)) + '"');
            end;
          end;
        end; //of: case fSqlType

        if vOdbcHostVarTypeFix <> 0 then
          fOdbcHostVarType := vOdbcHostVarTypeFix;

        Inc(LastColNo);
        // correct fOdbcLateBound
        if fOdbcLateBound then
          OdbcLateBoundFound := True
        else
          if (OdbcLateBoundFound and (not fOwnerDbxConnection.fGetDataAnyColumn)) then
            // Driver does not support early-bound after late-bound columns,
            // and we have already had a late bound column, so we force this
            // column to be late-bound, even though normally it would be early-bound.
            fOdbcLateBound := True;
      end; //of: with aOdbcBindCol
    end; //of: for ColNo := 1 to fOdbcNumCols

    fOdbcBindList.Count := LastColNo;
    fOdbcNumCols := LastColNo;

(*

 Column-Wise Buffer Structure:

 o-----------------------------------------------o    ------------ LateBounds -----------
 |row1: [col1_len][col_1])...([colN_len][col_N]) |  /         ( One row Buffers )         \
 | ...   ...   ...   ...  ...   ...   ...   ...  | |                                       |
 |rowN: [col1_len][col_1])...([colN_len][col_N]) | ([col_A]),([col_D])....|.....BLOBs......|
 |                                               | |                      |       |        |
 o---------------- SqlBindCol() -----------------o o----- SqlGetData() ---o   SqlGetData() |
 |                    \ /                                   \ /           |       \ /
 |                     |                                     |            |        |
 |              Cols-Wise Buffer                       Static  Buffer     |   Dynamic Buffer
 |                                                     ( Small Size )     |   ( Large Size )
 o--------------------------- Common Buffer ------------------------------o

*)

    // Clear Bund buffer info:
    fOdbcBindBufferPos := -1; // = status buffer not fetched
    fOdbcBindBufferRowSize := 0; // unknown buffer size
    // unknown buffer status for simple LateBound columns:
    vUnbindedColsBuffSize := 0;
    vUnbindedFirstColIdx := -1;
    vBindedColsCnt := 0;
    // calculate buffer size for "binded" columns and "unbinded non blobs" columns
    for ColNo := 0 to fOdbcNumCols-1 do
    begin
      aOdbcBindCol := TOdbcBindCol(fOdbcBindList.Items[ColNo]);
      with aOdbcBindCol do
      begin
        if not fOdbcLateBound then
        begin
          inc(vBindedColsCnt);
          inc(fOdbcBindBufferRowSize, fOdbcHostVarSize);
        end
        else
        if not fIsBuffer then
        begin
          inc(vUnbindedColsBuffSize, fOdbcHostVarSize);
          if vUnbindedFirstColIdx<0 then
            vUnbindedFirstColIdx := ColNo;
        end;
      end;
    end;
    fOdbcBindBufferRowSize := fOdbcBindBufferRowSize + vBindedColsCnt*SizeOf(SqlInteger);

    fOdbcLateBoundsFound := OdbcLateBoundFound;

    if (fCursorFetchRowCount <= 0) or (not fOwnerCommand.fSupportsMixedFetch) then
      fCursorFetchRowCount := 1;

    // Check Parameters and BufferSize memory limitation:
    if (fCursorFetchRowCount > 1) then
    begin
      if (OdbcLateBoundFound and (not fOwnerCommand.fSupportsMixedFetch)) or
         (fCursorFetchRowCount < 2)
      then
        fCursorFetchRowCount := 1
      else
      if // set limitatiuon to commonn rows buffer size: ???:
         ( (fOdbcBindBufferRowSize * fCursorFetchRowCount) > 1024*2000{2Mb} )
      then
      begin
        fCursorFetchRowCount := 1024 * 2000 div fOdbcBindBufferRowSize;
        if fCursorFetchRowCount = 0 then
          fCursorFetchRowCount := 1;
      end;
      (*
      if (fCursorFetchRowCount>1) and //(vUnbindedColsBuffSize>0) and
        // optimize fetch method when Binded buffer is very small
        (fOdbcBindBufferRowSize < vUnbindedColsBuffSize)
      then
      begin
        fCursorFetchRowCount := 1;
      end;
      //*)
    end;

    if (fCursorFetchRowCount > 1) then
    begin // set array mode fetch:
      // temporary copy fCursorFetchRowCount into vCursorFetchRowCount.
      vCursorFetchRowCount := fCursorFetchRowCount;
      try // <- protected from bad odbc driver
        while vCursorFetchRowCount > 1 do
        begin // set ODBC cursor option SQL_ATTR_ROW_ARRAY_SIZE
          // TODO: ???: need check for ODBC 2
          OdbcRetcode := SQLSetStmtAttr(fHStmt, SQL_ATTR_ROW_ARRAY_SIZE,
            SqlPointer(vCursorFetchRowCount), 0);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
          begin
            vCursorFetchRowCount := -1; // (-1) after setting SQL_ATTR_ROW_ARRAY_SIZE
            fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
              fOwnerCommand.fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, fOwnerCommand, nil, 1);
            break;
          end;
          OdbcRetcode := SQLGetStmtAttr(
            fHStmt,
            SQL_ATTR_ROW_ARRAY_SIZE,
            SqlPointer(@IntAttribute),
            0{SizeOf(SqlInteger)},
            nil
          );
          if (OdbcRetcode <> OdbcApi.SQL_SUCCESS)or
            (IntAttribute<>vCursorFetchRowCount)
          then
          begin
            vCursorFetchRowCount := -1;
            if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
              fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
                fOwnerCommand.fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, fOwnerCommand, nil, 1);
            break;
          end;
          // Column-Wise Binding:
          OdbcRetcode := SQLSetStmtAttr (fHStmt, SQL_ATTR_ROW_BIND_TYPE,
            SQLPOINTER(fOdbcBindBufferRowSize), 0);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
          begin
            vCursorFetchRowCount := -1;
            fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
              fOwnerCommand.fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, fOwnerCommand, nil, 1);
            break;
          end;
          OdbcRetcode := SQLGetStmtAttr(fHStmt, SQL_ATTR_ROW_BIND_TYPE,
            SqlPointer(@IntAttribute), 0{SizeOf(SqlInteger)}, nil );
          if (OdbcRetcode<>OdbcApi.SQL_SUCCESS) or (IntAttribute<>fOdbcBindBufferRowSize) then
          begin
            vCursorFetchRowCount := -1;
            if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
              fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
                fOwnerCommand.fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, fOwnerCommand, nil, 1);
            break;
          end;
          SetLength(fOdbcRowsStatus, fCursorFetchRowCount);
          if ( SQLSetStmtAttr(fHStmt, SQL_ATTR_ROW_STATUS_PTR,
               @fOdbcRowsStatus[0], 0 ) <> OdbcApi.SQL_SUCCESS )
             or
             ( SQLSetStmtAttr(fHStmt, SQL_ATTR_ROWS_FETCHED_PTR, @fOdbcRowsFetched, 0)
             <> OdbcApi.SQL_SUCCESS )
          then
          begin
            SetLength(fOdbcRowsStatus, 0);
            vCursorFetchRowCount := -1;
          end;
          break;
        end;//of: while fCursorFetchRowCount>1
        if vCursorFetchRowCount <= 0 then // is error when applying SQL_ATTR_ROW_ARRAY_SIZE:
        begin // set fetching mode: fetch only one record
          if vCursorFetchRowCount < 0 then // cancel multirow fetching option:
          begin
            {OdbcRetcode := }SQLSetStmtAttr (fHStmt, SQL_ATTR_ROW_BIND_TYPE,
              SqlPointer(SQL_BIND_TYPE_DEFAULT{=SQL_BIND_BY_COLUMN}), 0);
            // ???: (0) - uncorrect value for MSSQL only or All ?
            OdbcRetcode := SQLSetStmtAttr(fHStmt, SQL_ATTR_ROW_ARRAY_SIZE, SqlPointer(0), 0);
            if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
            begin
              fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
                fOwnerCommand.fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, fOwnerCommand, nil, 1);
              OdbcRetcode := SQLSetStmtAttr(fHStmt, SQL_ATTR_ROW_ARRAY_SIZE,
                SqlPointer(1), 0);
              if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
                OdbcCheck(OdbcRetcode, 'SQLSetStmtAttr(SQL_ATTR_ROW_ARRAY_SIZE,1)');
            end;
          end;
          fCursorFetchRowCount := 1;
          fOwnerCommand.fCommandRowSetSize := 1;
          fOwnerCommand.fSupportsBlockRead := False;
          fOwnerDbxConnection.fSupportsBlockRead := False;
        end;
      except  // set fetching mode: fetch only one record
        on e: exception do
        begin
          if e is EDbxErrorCustom then
            fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
              fOwnerCommand.fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, fOwnerCommand, nil, 1);
          if vCursorFetchRowCount <0 then // cancel multirow fetching option:
          begin
            OdbcRetcode := SQLSetStmtAttr (fHStmt, SQL_ATTR_ROW_BIND_TYPE,
              SqlPointer(SQL_BIND_TYPE_DEFAULT{=SQL_BIND_BY_COLUMN}), 0);
            // clear last error:
            if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
             fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
               fOwnerCommand.fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, fOwnerCommand, nil, 1);
            // ???: (0) - uncorrect value for MSSQL only or All ?
            OdbcRetcode := SQLSetStmtAttr(fHStmt, SQL_ATTR_ROW_ARRAY_SIZE, SqlPointer(0), 0);
            if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
            begin
              // clear last error:
              fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
                fOwnerCommand.fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, fOwnerCommand, nil, 1);
              OdbcRetcode := SQLSetStmtAttr(fHStmt, SQL_ATTR_ROW_ARRAY_SIZE,
                SqlPointer(1), 0);
              if (OdbcRetcode <> OdbcApi.SQL_SUCCESS) then
              begin
                {if Self.fOwnerDbxConnection.fOdbcDriverType <> eOdbcDriverTypeOterroRBase then
                  OdbcCheck(OdbcRetcode, 'SQLSetStmtAttr(SQL_ATTR_ROW_ARRAY_SIZE,1)')
                else} //clear last error
                  fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
                    fOwnerCommand.fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, fOwnerCommand, nil, 1);
              end;
            end;
          end;
          fCursorFetchRowCount := 1;
          fOwnerCommand.fCommandRowSetSize := 1;
          fOwnerCommand.fSupportsBlockRead := False;
          fOwnerDbxConnection.fSupportsBlockRead := False;
        end;
      end;
    end;

    if (fCursorFetchRowCount = 1) then
    begin // remove ColSize rows allocation
      if vBindedColsCnt > 0 then
        dec(fOdbcBindBufferRowSize, vBindedColsCnt*SizeOf(SqlInteger) );
      if vUnbindedColsBuffSize>0 then
        inc(fOdbcBindBufferRowSize, vUnbindedColsBuffSize);
    end;

    // allocate common buffer memory
      // IntResult = "FETCH ARRAY" Bufer size
    IntResult := fOdbcBindBufferRowSize*fCursorFetchRowCount;
    fOdbcBindBuffer := AllocMem( IntResult + vUnbindedColsBuffSize);

    // base address for cols values
    vLastHostVarAddress := fOdbcBindBuffer;

    if (fCursorFetchRowCount>1) then // start binding addresses and cols
    begin // Column-Wise Binding:
      // bind fOdbcHostVarAddress for unbinded columns:
      if (vUnbindedColsBuffSize>0) then // for "simple LateBound columns":
      begin // Nedd allocate buffer for "simple LateBound columns":
        // common buffer for "simple LateBound columns" is contained in aOdbcBindCol:
        aOdbcBindColPrev := TOdbcBindCol(fOdbcBindList.Items[vUnbindedFirstColIdx]);
        // set buffer first value:
        aOdbcBindColPrev.fOdbcHostVarAddress.Ptr := // seek to last pos of fetch array buffer
          Pointer( DWORD(fOdbcBindBuffer) + DWORD(IntResult) );
        //set buffer HostVarAddresses for "simple LateBound columns":
        for ColNo := vUnbindedFirstColIdx+1 to fOdbcNumCols-1 do
        begin
          aOdbcBindCol := TOdbcBindCol(fOdbcBindList.Items[ColNo]);
          with aOdbcBindCol do
          begin
            if (fOdbcLateBound) and (not fIsBuffer) then
            begin
              fOdbcHostVarAddress.Ptr :=
                Pointer(
                  DWORD(aOdbcBindColPrev.fOdbcHostVarAddress.Ptr) +
                  DWORD(aOdbcBindColPrev.fOdbcHostVarSize)
                );
              aOdbcBindColPrev := aOdbcBindCol;
            end;
          end;
        end;
      end;
      // bind fOdbcHostVarAddress for binded columns:
      {+?} // https://sourceforge.net/forum/message.php?msg_id=3248709
      try
        for ColNo := 0 to fOdbcNumCols-1 do
        begin
          aOdbcBindCol := TOdbcBindCol(fOdbcBindList.Items[ColNo]);
          with aOdbcBindCol do
          begin
            if (not fOdbcLateBound) then
            begin
              fColValueSizePtr := vLastHostVarAddress;
              inc(DWORD(vLastHostVarAddress), SizeOf(SqlInteger));
              // set fOdbcHostVarAddress to first row value buffer
              fOdbcHostVarAddress.Ptr := vLastHostVarAddress;
              inc(DWORD(vLastHostVarAddress), fOdbcHostVarSize);
              // bind
              OdbcRetcode := SQLBindCol(
                fHStmt, fOdbcColNo, fOdbcHostVarType,
                fOdbcHostVarAddress.Ptr, fOdbcHostVarSize,
                fColValueSizePtr);
              if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
                OdbcCheck(OdbcRetcode, 'SQLBindCol("' + StrPas(fColName) + '")');
            end;//of: if (not fOdbcLateBound)
          end;//of: with aOdbcBindCol do
        end;//of: for ColNo
      except
        on e: exception do
        begin
          if e is EDbxErrorCustom then
            fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, nil, fOwnerDbxConnection, fOwnerCommand, nil, 1);

          OdbcRetcode := SQLFreeStmt(fHStmt, SQL_UNBIND);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, nil, fOwnerDbxConnection, fOwnerCommand, nil, 1);

          FreeMemAndNil(fOdbcBindBuffer);
          SetLength(fOdbcRowsStatus, 0);

          for ColNo := fOdbcBindList.Count - 1 downto 0 do
          begin
            aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColNo]);
            fOdbcBindList[ColNo] := nil;
            aOdbcBindCol.Free;
          end;
          FreeAndNil(fOdbcBindList);

          fCursorFetchRowCount := 1;
          fOwnerCommand.fCommandRowSetSize := 1;
          fOwnerCommand.fSupportsBlockRead := False;
          fOwnerDbxConnection.fSupportsBlockRead := False;

          // call BindResultSet now:
          BindResultSet();
        end;
      end;
      {+.}
    end
    else  // one row binding
    begin
      // bind fOdbcHostVarAddress for any non BLOB column:
      for ColNo := 0 to fOdbcNumCols-1 do
      begin
        aOdbcBindCol := TOdbcBindCol(fOdbcBindList.Items[ColNo]);
        with aOdbcBindCol do
        begin
          if (not fIsBuffer) then
          begin
            fOdbcHostVarAddress.Ptr := vLastHostVarAddress;
            inc(DWORD(vLastHostVarAddress), fOdbcHostVarSize);
            if not fOdbcLateBound then
            begin
              OdbcRetcode := SQLBindCol(
                fHStmt, fOdbcColNo, fOdbcHostVarType,
                fOdbcHostVarAddress.Ptr, fOdbcHostVarSize,
                fColValueSizePtr);
              if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
              begin
                if Self.fOwnerDbxConnection.fDbmsType <> eDbmsTypeFlashFiler then
                  OdbcCheck(OdbcRetcode, 'SQLBindCol("' + StrPas(fColName) + '")')
                else
                begin
                  fOdbcLateBound := True;
                  // clear last error:
                  fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
                    fOwnerCommand.fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, fOwnerCommand, nil, 1);
                end;
              end;
            end;
          end;//of: if not fOdbcLateBound then
        end;//of: with aOdbcBindCol do
      end;//of: for ColNo
    end;//of: finished binding
  finally
    FreeMem(ColNameTemp);
  end;

  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.BindResultSet', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.BindResultSet', ['FetchRowCount =', fCursorFetchRowCount, 'OdbcLateBoundsFound =', fOdbcLateBoundsFound]); end;
  {$ENDIF _TRACE_CALLS_}

end;

procedure TSqlCursorOdbc.DoFetchLateBoundData;//(OdbcRetcode: OdbcApi.SqlReturn;);
var
  OdbcRetcode: OdbcApi.SqlReturn;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorOdbc.DoFetchLateBoundData', ['ColName =', aOdbcBindCol.fColName]); {$ENDIF _TRACE_CALLS_}
  with aOdbcBindCol, fOwnerDbxDriver.fOdbcApi do
  begin
    //if fIsFetched then
    //  Exit;
    OdbcRetcode := SQLGetData(
      fHStmt, fOdbcColNo, fOdbcHostVarType,
      fOdbcHostVarAddress.Ptr, fOdbcHostVarSize, fColValueSizePtr);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetcode, 'SQLGetData("'+StrPas(aOdbcBindCol.fColName)+'")');
    fIsFetched := True;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.DoFetchLateBoundData', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.DoFetchLateBoundData'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCursorOdbc.FetchLateBoundData;//(ColNo: SqlUSmallint);
var
  OdbcRetcode: OdbcApi.SqlReturn;
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorOdbc.FetchLateBoundData', ['ColNo =', ColNo]); {$ENDIF _TRACE_CALLS_}
  Dec(ColNo);
  aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColNo]);
  with aOdbcBindCol, fOwnerDbxDriver.fOdbcApi do
  begin
    if fIsFetched then
      Exit;
    // fix: Version 3.100, 2008-02-29
    if fOdbcColumnsFetchConsecutively and (ColNo > 0) and ( not TOdbcBindCol(fOdbcBindList[ColNo - 1]).fIsFetched ) then
      CheckFetchCacheColumns(ColNo);
    // fix.
    OdbcRetcode := SQLGetData(
      fHStmt, fOdbcColNo, fOdbcHostVarType,
      fOdbcHostVarAddress.Ptr, fOdbcHostVarSize, fColValueSizePtr);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetcode, 'SQLGetData("'+StrPas(aOdbcBindCol.fColName)+'")');
    fIsFetched := True;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.FetchLateBoundData', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.FetchLateBoundData'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCursorOdbc.CheckFetchCacheColumns;//(ColNoLimit: SqlUSmallint);
var
  aOdbcBindCol: TOdbcBindCol;
  bAllowFragmentation: Boolean;
  i: Integer;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorOdbc.CheckFetchCacheColumn', ['ColNoLimit =', ColNoLimit]); {$ENDIF _TRACE_CALLS_}
  //if (not fOdbcLateBoundsFound) or (ColNoLimit = 0) then
  //  Exit;
  //aOdbcBindCol := TOdbcBindCol(ColNoLimit);
  //if aOdbcBindCol.fIsFetched then
  //  Exit;
  //aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColNoLimit - 1]);
  //if aOdbcBindCol.fIsFetched then
  //  Exit;
  for i := 0 to ColNoLimit - 1 do
  begin
    aOdbcBindCol := TOdbcBindCol(fOdbcBindList[i]);
    with aOdbcBindCol do if not fIsFetched then
    begin
      if fOdbcLateBound then
      begin
        if not fIsBuffer then
          DoFetchLateBoundData(aOdbcBindCol)
        else { BLOB }
        begin
          case fDbxType of
            fldBLOB:
              begin
                if fDbxSubType in [fldstMEMO, fldstWIDEMEMO] then
                  bAllowFragmentation := (fColSize <= 0) or (fColSize > cBlobChunkSizeDefault)
                else
                  bAllowFragmentation := True;
              end;
            fldBYTES, fldVARBYTES:
              begin
                bAllowFragmentation := (fColSize <= 0) or (fColSize > cBlobChunkSizeDefault);
              end;
            else { fldZSTRING, fldZWIDESTRING, fldZUNICODE }
              bAllowFragmentation := False;
          end;
          DoFetchLongData(aOdbcBindCol, bAllowFragmentation, 0);
        end;
      end
      else
        fIsFetched := True;
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.CheckFetchCacheColumn', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.CheckFetchCacheColumn'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCursorOdbc.AddError;//(eError: Exception);
begin
  fOwnerCommand.AddError(eError);
end;

procedure TSqlCursorOdbc.DoFetchLongData;//(aOdbcBindCol: TOdbcBindCol;
//  bAllowFragmentation: Boolean; FirstChunkSize: Integer);
var
  OdbcRetcode: OdbcApi.SqlReturn;
  BlobChunkSize, BlobTerminationCharSize, BlobBuffSize, BlobFixSize: Integer;
  BlobChunkSizeNew: Int64;
  CurrentBlobSize: SqlInteger;
  PreviousBlobSize: SqlInteger;
  CurrentFetchPointer: PAnsiChar;
  vOdbcHostVarType: SqlSmallint;
  // ---
  procedure OptimizeNextChunkSize;
  begin
    //(*
    // Make ChunkSize bigger to avoid too many loop repetiontions
    with aOdbcBindCol do
    begin
      BlobChunkSizeNew := BlobChunkSize * 2;
      if BlobChunkSizeNew <= cBlobChunkSizeMax then
      begin
        BlobChunkSize := BlobChunkSizeNew;
        if fColValueSizePtr^ >= cBlobChunkSizeMin then
        begin
          BlobChunkSizeNew := fColValueSizePtr^ - PreviousBlobSize;
          if (BlobChunkSizeNew > 0) and (BlobChunkSizeNew < cBlobChunkSizeMax) then
          begin
            // Latest chunk
            if BlobChunkSizeNew >= cBlobChunkSizeMin then
              BlobChunkSize := BlobChunkSizeNew
            else
              BlobChunkSize := cBlobChunkSizeMin;
          end;
        end;
      end
      else
        BlobChunkSize := cBlobChunkSizeMax;
      {$IFDEF _TRACE_CALLS_}
      LogInfoProc(AnsiString('NextChunkSize = "' + IntToStr(BlobChunkSize) + '"'));
      {$ENDIF _TRACE_CALLS_}
    end;
    //*)
  end;
  // ---
  procedure CalculateBlobFixSize(ABlobSize: Integer);
  begin
    case BlobTerminationCharSize of
      1:
        begin
          if (ABlobSize <= 0) or (CurrentFetchPointer[ABlobSize - 1] = cNullAnsiChar) then
            BlobFixSize := 0
          else
          begin
            BlobFixSize := 1;
            {$IFDEF _TRACE_CALLS_}
            LogInfoProc('  @fix(vendor driver): Extend size of text blob on 1 byte');
            {$ENDIF _TRACE_CALLS_}
          end;
        end;
      2:
        begin
          if (ABlobSize <= 1) or (PWideChar(@CurrentFetchPointer[ABlobSize - 2])^ = cNullWideChar) then
            BlobFixSize := 0
          else
          begin
            BlobFixSize := 2;
            {$IFDEF _TRACE_CALLS_}
            LogInfoProc('  @fix(vendor driver): Extend size of wide text blob on 2 byte');
            {$ENDIF _TRACE_CALLS_}
          end;
        end;
    end;
  end;
  // ---
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorOdbc.DoFetchLongData', ['AllowFragmentation =', bAllowFragmentation]); {$ENDIF _TRACE_CALLS_}
  with aOdbcBindCol, fOwnerDbxDriver.fOdbcApi do
  begin
    //if fIsFetched then
    //  Exit;
    {$IFDEF _DBXCB_}
    if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
      fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Blob, 'fetch long column: "%s"', [StrPas(fColName)]);
    {$ENDIF}

    {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
    {+2.01}
    //Vadim V.Lopushansky: optimize BlobChunkSize:
    //
    //old:
    //
    //BlobChunkSize := 256; // == cBlobChunkSizeMin
    //
    //new:
    //
      // test only:
      //  FirstChunkSize := 256;
      //  BlobChunkSize := 256;
      // release:
    //(*
    {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}

    if (FirstChunkSize >= cBlobChunkSizeMin) and (FirstChunkSize <= cBlobChunkSizeMax) then
    begin
      BlobChunkSize := FirstChunkSize;
    end
    else
    begin
      FirstChunkSize := fOwnerDbxConnection.fBlobChunkSize;
      if (FirstChunkSize >= cBlobChunkSizeMin) and (FirstChunkSize <= cBlobChunkSizeMax) then
      begin
        BlobChunkSize := FirstChunkSize;
      end
      else
      begin
        fOwnerDbxConnection.fBlobChunkSize := cBlobChunkSizeDefault;
        BlobChunkSize := cBlobChunkSizeDefault;
      end;
      if (aOdbcBindCol.fColSize > cBlobChunkSizeMin) and (aOdbcBindCol.fColSize < BlobChunkSize) then
        BlobChunkSize := aOdbcBindCol.fColSize;
    end;
    //*)

    {$IFDEF _TRACE_CALLS_}
    LogInfoProc(AnsiString('FirstChunkSize = "' + IntToStr(BlobChunkSize) + '"'));
    {$ENDIF _TRACE_CALLS_}
    {/+2.01}
    PreviousBlobSize := 0;

    if fOwnerDbxConnection.fConnectionOptions[coBlobFragmentation] <> osOn then
      bAllowFragmentation := False;

    //bAllowFragmentation := False; // *** debug ***

    {$IFDEF _TRACE_CALLS_}
    LogInfoProc(['AllowFragmentation = ',  bAllowFragmentation]);
    {$ENDIF _TRACE_CALLS_}

    if (fOdbcHostVarAddress.Ptr = nil) or (fOdbcHostVarChunkSize < BlobChunkSize) then
    begin
      if not fIsBuffer then
      begin
        //  fIsBuffer := True; // ???: ERROR in TSqlCursorOdbc.BindResultSet
        raise EDbxInternalError.Create('TSqlCursorOdbc.FetchLongData. Not allocated host variable buffer in TSqlCursorOdbc.BindResultSet');
      end;

      FreeMemAndNil(fOdbcHostVarAddress.Ptr);

      if bAllowFragmentation then
      begin
        if aOdbcBindCol.fBlobChunkCollection <> nil then
          aOdbcBindCol.fBlobChunkCollection.Clear
        else
          aOdbcBindCol.fBlobChunkCollection := TBlobChunkCollection.Create;
        fOdbcHostVarChunkSize := 0;
      end
      else
      begin
        if aOdbcBindCol.fBlobChunkCollection <> nil then
          FreeAndNil(aOdbcBindCol.fBlobChunkCollection);
        fOdbcHostVarChunkSize := BlobChunkSize;
      end;
    end;

    vOdbcHostVarType := fOdbcHostVarType;

    if fOwnerDbxConnection.fConnectionOptions[coBlobNotTerminationChar] <> osOn then
    begin
      if vOdbcHostVarType = SQL_C_CHAR then
        { Each part is null-terminated }
        BlobTerminationCharSize := 1 // == SizeOf(AnsiChar)
      else if vOdbcHostVarType = SQL_C_WCHAR then
        { Each part is wide null-terminated }
        BlobTerminationCharSize := 2 // == SizeOf(WideChar)
      else
        { Each part is not null-terminated }
        BlobTerminationCharSize := 0;
    end
    else
      { Each part is not null-terminated }
      BlobTerminationCharSize := 0;
    {$IFDEF _TRACE_CALLS_}
    LogInfoProc(['BlobTerminationCharSize =', BlobTerminationCharSize]);
    {$ENDIF _TRACE_CALLS_}
    BlobFixSize := 0;
    //bBlobIsText := BlobTerminationCharSize <> 0;

    if bAllowFragmentation then
    begin // Fragmentation fetch to chunk collention.
      BlobBuffSize := BlobChunkSize + BlobTerminationCharSize;
      GetMem(CurrentFetchPointer, BlobBuffSize);
      fColValueSizePtr^ := 0;
      OdbcRetcode := SQLGetData( fHStmt, fOdbcColNo, vOdbcHostVarType,
        CurrentFetchPointer, BlobBuffSize, fColValueSizePtr);
      // *** debug: IntToHex(Ord(CurrentFetchPointer[BlobChunkSize-2]), 2)
      if (OdbcRetcode = SQL_SUCCESS_WITH_INFO) then
      begin
        CalculateBlobFixSize(BlobBuffSize);
        CurrentBlobSize := BlobChunkSize + BlobFixSize;
        while True do
        begin
          fBlobChunkCollection.AddFragment(CurrentFetchPointer, BlobChunkSize + BlobFixSize);

          PreviousBlobSize := CurrentBlobSize;

          // Make ChunkSize bigger to avoid too many loop repetiontions
          if BlobChunkSize <> cBlobChunkSizeMax then
            OptimizeNextChunkSize();

          Inc(CurrentBlobSize, BlobChunkSize + BlobFixSize);

          BlobBuffSize := BlobChunkSize + BlobTerminationCharSize;
          GetMem(CurrentFetchPointer, BlobChunkSize + BlobTerminationCharSize);
          fColValueSizePtr^ := 0;
          OdbcRetcode := SQLGetData(fHStmt, fOdbcColNo, vOdbcHostVarType,
            CurrentFetchPointer, BlobBuffSize, fColValueSizePtr);
          // *** debug: IntToHex(Ord(CurrentFetchPointer[0]), 2)
          if (OdbcRetcode <> SQL_SUCCESS_WITH_INFO) or ( (fColValueSizePtr^ <> SQL_NO_TOTAL) and
            (fColValueSizePtr^ <= 0) ) then
          begin
            { Check ODBC driver bug }
            if (fColValueSizePtr^ = SQL_NO_TOTAL) or (fColValueSizePtr^ > BlobBuffSize) then
              fColValueSizePtr^ := BlobBuffSize;
            { Add chunk }
            if fColValueSizePtr^ > 0 then
            begin
              CalculateBlobFixSize(fColValueSizePtr^);
              fColValueSizePtr^ := fColValueSizePtr^ - BlobTerminationCharSize + BlobFixSize;
            end;
            if fColValueSizePtr^ > 0 then
              fBlobChunkCollection.AddFragment(CurrentFetchPointer, fColValueSizePtr^)
            else
            begin
              FreeMem(CurrentFetchPointer);
              fColValueSizePtr^ := 0;
            end;
            Break;
          end;
          CalculateBlobFixSize(BlobBuffSize);
        end; // of while
        { Check ODBC driver bug }
        if (fColValueSizePtr^ = SQL_NO_TOTAL) or (fColValueSizePtr^ > BlobBuffSize) then
        begin
          CalculateBlobFixSize(BlobBuffSize);
          fColValueSizePtr^ := BlobBuffSize - BlobTerminationCharSize + BlobFixSize;
        end;
      end
      else
      begin
        { Check ODBC driver bug }
        if (fColValueSizePtr^ = SQL_NO_TOTAL) or (fColValueSizePtr^ > BlobBuffSize) then
          fColValueSizePtr^ := BlobBuffSize;
        { Correction of the size of last piece }
        if fColValueSizePtr^ > 0 then
        begin
          CalculateBlobFixSize(fColValueSizePtr^);
          fColValueSizePtr^ := fColValueSizePtr^ - BlobTerminationCharSize + BlobFixSize;
        end;
        { Add chunk }
        if fColValueSizePtr^ > 0 then
          fBlobChunkCollection.AddFragment(CurrentFetchPointer, fColValueSizePtr^)
        else
         FreeMemAndNil(CurrentFetchPointer);
      end;
      { Calculate BLOB Full Size }
      inc(fColValueSizePtr^, PreviousBlobSize);
      {
       if (fColValueSizePtr^ >= 0) and (fColValueSizePtr^ <> fBlobChunkCollection.fSize) then
        // !!! ERROR !!!
        fColValueSizePtr^ := fBlobChunkCollection.fSize;
      }
    end
    else // Fetch into one memory block (many memory reallocation).
    begin
      {  Code below is very tricky
         Call SQLGetData to get first chunk of the BLOB
         If MORE blob data to get, Odbc returns SQL_SUCCESS_WITH_INFO, SQLSTATE 01004 (String truncated)
         Keep calling SqlGetData for each part of the blob, reallocating more
         memory for the BLOB data on each successive call.
         Odbc always
      }
      BlobBuffSize := BlobChunkSize + BlobTerminationCharSize;
      GetMem(fOdbcHostVarAddress.Ptr, BlobBuffSize);
      fColValueSizePtr^ := 0;
      CurrentFetchPointer := fOdbcHostVarAddress.Ptr;
      OdbcRetcode := SQLGetData( fHStmt, fOdbcColNo, vOdbcHostVarType,
        CurrentFetchPointer, BlobBuffSize, fColValueSizePtr);
      CurrentBlobSize := BlobChunkSize;
      while (OdbcRetcode = SQL_SUCCESS_WITH_INFO) do
      begin
        // clear last warning:
        //???: fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, fOwnerDbxConnection, fOwnerCommand, 1);
        CalculateBlobFixSize(BlobBuffSize);
        PreviousBlobSize := CurrentBlobSize + BlobFixSize;
        Inc(CurrentBlobSize, BlobChunkSize);
        if fOdbcHostVarChunkSize < CurrentBlobSize then
        begin
          ReallocMem(fOdbcHostVarAddress.Ptr, CurrentBlobSize + BlobTerminationCharSize);
          fOdbcHostVarChunkSize := CurrentBlobSize;
        end;
        CurrentFetchPointer := PAnsiChar(fOdbcHostVarAddress.Ptr) + PreviousBlobSize;
        fColValueSizePtr^ := 0;
        BlobBuffSize := BlobChunkSize + BlobTerminationCharSize;
        OdbcRetcode := SQLGetData( fHStmt, fOdbcColNo, vOdbcHostVarType,
          CurrentFetchPointer,
          BlobBuffSize, // Chunk size is +BlobTerminationCharSize because we overwrite previous null terminator
          fColValueSizePtr);

        if (OdbcRetcode <> SQL_SUCCESS_WITH_INFO) or (fColValueSizePtr^ = 0) then
          Break;

        // Make ChunkSize bigger to avoid too many loop repetiontions
        if BlobChunkSize <> cBlobChunkSizeMax then
          OptimizeNextChunkSize();
      end; // of while
      { Check ODBC driver bug }
      if (fColValueSizePtr^ = SQL_NO_TOTAL) or (fColValueSizePtr^ > BlobBuffSize) then
        fColValueSizePtr^ := BlobBuffSize;
      { Correction of the size of last piece }
      if fColValueSizePtr^ > 0 then
      begin
        CalculateBlobFixSize(fColValueSizePtr^);
        fColValueSizePtr^ := fColValueSizePtr^ - BlobTerminationCharSize + BlobFixSize;
      end;
      { Calculate BLOB Full Size }
      inc(fColValueSizePtr^, PreviousBlobSize);
    end;
    {+2.01}
    //Michael Schwarzl
    //-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    // blob load behavior
    // Michael Schwarzl 31.05.2002
    //-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    // on SQL-Server connections multiple reading of the blob leads into a error message from ODBC
    // the data has been read correctly at this time and when cursor leaves position next read will
    // be successful. So when returncode is SQL_NO_DATA but data has been loaded (fColValueSize > 0)
    // reset SQL Result Csode
    //-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    if (fColValueSizePtr^ > 0) and (OdbcRetcode = SQL_NO_DATA) then
      OdbcRetcode := OdbcApi.SQL_SUCCESS;
    //-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    {/+2.01}

    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetcode, 'SQLGetData("'+StrPas(aOdbcBindCol.fColName)+'")');
    fIsFetched := True;
  end; //of: with aOdbcBindCol
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.DoFetchLongData', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.DoFetchLongData'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCursorOdbc.FetchLongData;//(ColNo: SqlUSmallint;
//  bAllowFragmentation: Boolean;
//  FirstChunkSize: Integer);
var
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorOdbc.FetchLongData', ['ColNo =', ColNo, 'AllowFragmentation =', bAllowFragmentation]); {$ENDIF _TRACE_CALLS_}
  Dec(ColNo);
  aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColNo]);
  with aOdbcBindCol do
  begin
    if fIsFetched then
      Exit;
    {$IFDEF _DBXCB_}
    if Assigned(fOwnerDbxConnection.fDbxTraceCallbackEven) then
      fOwnerDbxConnection.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Blob, 'fetch long column: "%s"', [StrPas(fColName)]);
    {$ENDIF}
    // fix: Version 3.100, 2008-02-29
    if fOdbcColumnsFetchConsecutively and (ColNo > 0) and ( not TOdbcBindCol(fOdbcBindList[ColNo - 1]).fIsFetched ) then
      CheckFetchCacheColumns(ColNo);
    // fix.
    DoFetchLongData(aOdbcBindCol, bAllowFragmentation, FirstChunkSize);
  end; //of: with aOdbcBindCol
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.FetchLongData', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.FetchLongData'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.getBcd;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
var
{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
  { // OLD:
  i: Integer;
  c: AnsiChar;
  n: Byte;
  d: Integer;
  Places: Integer;
  DecimalPointFound: Boolean; }
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc.getBcd', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
  with aOdbcBindCol{TOdbcBindCol(fOdbcBindList[ColumnNumber - 1])} do
  begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    IsBlank := (fColValueSizePtr^ = OdbcApi.SQL_NULL_DATA) or
      (fColValueSizePtr^ = OdbcApi.SQL_NO_TOTAL);
    if IsBlank then
      PDWORD(Value)^ := 1 // set to zero BCD
    else
      Str2BCD(fOdbcHostVarAddress.ptrAnsiChar,
        StrLen(fOdbcHostVarAddress.ptrAnsiChar), PBcd(Value)^, cDecimalSeparatorDefault);
    {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
    (* // OLD:
    begin
      // with PBcd(Value)^ do
        // FillChar( Fraction[0], Length(Fraction), 0);
      d := 0; // Number of digits
      PBcd(Value).SignSpecialPlaces := 0; // Sign: 0=+; anything else =-
      DecimalPointFound := False;
      Places := 0;
      i := 0;
      if fOdbcHostVarAddress.ptrAnsiChar[0] = '-' then
      begin
        PBcd(Value).SignSpecialPlaces := $80; // Sign: 0=+; anything else =-
        i := 1;
      end;
      c := fOdbcHostVarAddress.ptrAnsiChar[i];
      while (c <> cNullAnsiChar)
        // added memory protected access to Fraction index
        and (d < fColSize{or cMaxBcdCharDigits}) // when usage fColSize then trim uncorrected value
        // check max places size
        and (places<=fColScale) // trim uncorrected value
      do
      begin
        if (c = '.') or (c = ',')
        // ???: or (c = DecimalSeparator ) //Theoretically the divider can be adhered to current in system
        then
          DecimalPointFound := True
        else
        begin
          n := Byte(c) - Byte('0');
          if not odd(d) then
            PBcd(Value).Fraction[d div 2] := n shl 4
          else
            Inc(PBcd(Value).Fraction[d div 2], n); // Array of nibbles
          Inc(d);
          {
          // added memory protected access to Fraction index
          if (d > cMaxBcdCharDigits) then
            raise EDbxOdbcError.Create(
              'BCD Overflow; Bug in ODBC Driver. '+
              'Fetched value length is uncorrected (length is more then '+
                IntToStr(cMaxBcdCharDigits)+' symbols): '+
              '"'+StrPas(fOdbcHostVarAddress.ptrAnsiChar) + '"'
            );
          {}
          if DecimalPointFound then
            Inc(places);
        end;
        Inc(i);
        c := fOdbcHostVarAddress.ptrAnsiChar[i];
      end;
      PBcd(Value).Precision := d; // Number of digits
      {
      if places > fColScale then
        raise EDbxOdbcError.Create(
          'BCD Overflow; Bug in ODBC Driver. '+
          'Returned uncorrected colunm precision (fColScale='+IntToStr(fColScale)+') '+
          'by SQLDescribeCol for column "'+fColName+'" or Fetched value is uncorrected: "'+
          StrPas(fOdbcHostVarAddress.ptrAnsiChar) + '"'
        );
      if d>fColSize //or cMaxBcdCharDigits
        then
        raise EDbxOdbcError.Create(
          'BCD Overflow; Bug in ODBC Driver. '+
          'Returned uncorrected colunm size (fColSize='+IntToStr(fColSize)+') '+
          'by SQLDescribeCol for column "'+fColName+'" or Fetched value is uncorrected: "'+
          StrPas(fOdbcHostVarAddress.ptrAnsiChar) + '"'
        );
      {}
      Inc(PBcd(Value).SignSpecialPlaces, places);
    end;
    // *)
    {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
    Result := DBXERR_NONE;
  end; //of: with TOdbcBindCol(fOdbcBindList[ColumnNumber-1])
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.getBcd', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.getBcd'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.getBlob;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool; Length: Longword): SQLResult;
var
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc.getBlob', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  //if Value = nil then
  //begin
  //  Result := DBXERR_INVALIDPARAM;
  //  exit;
  //end;
  try
    aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
    with aOdbcBindCol do
    begin

      if fOdbcLateBound then
        FetchLongData(ColumnNumber, {AllowFragmentation=}True,
          {FirstChunkSize=}fOwnerDbxConnection.fBlobChunkSize);

      if (fOwnerDbxConnection.fOdbcDriverType <> eOdbcDriverTypeSQLite) then
      begin
        IsBlank := (fColValueSizePtr^ = OdbcApi.SQL_NULL_DATA) or
          (fColValueSizePtr^ = OdbcApi.SQL_NO_TOTAL);
        if Assigned(Value) then
        begin
          if IsBlank then
            Pointer(Value^) := nil
          else
          begin
            // Note:
            if fOdbcHostVarAddress.Ptr = nil then
            //    ERROR in TSqlCursorOdbc.BindResultSet
            begin
              if fBlobChunkCollection <> nil then
                fBlobChunkCollection.Read(Value)
              else
                //  fIsBuffer := True; // ???: ERROR in TSqlCursorOdbc.BindResultSet
                //  Pointer(Value^) := nil;
                raise EDbxInternalError.Create('TSqlCursorOdbc.getBlob. Not allocated host variable buffer in TSqlCursorOdbc.BindResultSet');
            end
            else if fBlobChunkCollection <> nil then
              fBlobChunkCollection.Read(Value)
            else
              Move(fOdbcHostVarAddress.Ptr^, Value^, Length{fColValueSizePtr^});
          end;
          Result := DBXERR_NONE;
        end
        else // Workaround bug in TBlobField.GetIsNull
          Result := DBXERR_NONE;
      end
      else
      begin
        IsBlank := (fColValueSizePtr^ = OdbcApi.SQL_NULL_DATA) or (fColValueSizePtr^ = 0);
        if IsBlank then
          Pointer(Value^) := nil
        else
        begin
          if fBlobChunkCollection = nil then
            Move(fOdbcHostVarAddress.Ptr^, Value^, Length)
          else
            fBlobChunkCollection.Read(Value);
        end;
        Result := DBXERR_NONE;
      end;
    end;
  except
    on E: Exception{EDbxError} do
    begin
      Pointer(Value^) := nil;
      IsBlank := True;
      AddError( E );
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.getBlob', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.getBlob', ['Value =', Pointer(Value^), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.getBlobSize;//(ColumnNumber: Word;
//  var Length: Longword; var IsBlank: LongBool): SQLResult;
var
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_}
    Length := 0;
    IsBlank := True;
    Result := DBXERR_NONE;
    try try
    LogEnterProc('TSqlCursorOdbc.getBlobSize', ['ColumnNumber =', ColumnNumber]);
  {$ENDIF _TRACE_CALLS_}
  try
    aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
    with aOdbcBindCol do
    begin

      if (fDbxType <> fldBLOB) then
        raise EDbxInvalidCall.Create(
          'TSqlCursorOdbc.getBlobSize but field is not BLOB - column '
          + IntToStr(ColumnNumber));

      if fOdbcLateBound then
        FetchLongData(ColumnNumber, {AllowFragmentation=}True,
          {FirstChunkSize=}fOwnerDbxConnection.fBlobChunkSize);

      IsBlank := (fColValueSizePtr^ = OdbcApi.SQL_NULL_DATA);
      if IsBlank then
        Length := 0
      else if (fOwnerDbxConnection.fOdbcDriverType <> eOdbcDriverTypeSQLite) then
        Length := fColValueSizePtr^
      else
        Length := StrLen(fOdbcHostVarAddress.ptrAnsiChar) + 1;

      Result := DBXERR_NONE;
    end;
  except
    on E: Exception{EDbxError} do
    begin
      Length := 0;
      IsBlank := True;
      AddError( E );
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.getBlobSize', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.getBlobSize', ['Length =', Length, 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.getBytes;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
var
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc.getBytes', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
  with aOdbcBindCol{TOdbcBindCol(fOdbcBindList[ColumnNumber - 1])} do
  begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    IsBlank := (fColValueSizePtr^ = OdbcApi.SQL_NULL_DATA) or
      (fColValueSizePtr^ = OdbcApi.SQL_NO_TOTAL);
    if Assigned(Value) then
    begin
      if IsBlank then
        Pointer(Value^) := nil
      else
      begin
        // Note:
        //  if fOdbcHostVarAddress.Ptr = nil then
        //    ERROR in TSqlCursorOdbc.BindResultSet
        // FillChar(PAnsiChar(Value)[fColValueSizePtr^+1], fColSize-fColValueSizePtr^, 0);
        if fDbxType = fldVARBYTES then
        begin
          PWord(Value)^ := fColValueSizePtr^;//SizeOf(Word);
          inc( DWORD(Value), SizeOf(Word) );
        end;
        Move(fOdbcHostVarAddress.Ptr^, Value^, fColValueSizePtr^);
      end;
      Result := DBXERR_NONE;
    end
    else
      Result := DBXERR_INVALIDPARAM;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.getBytes', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.getBytes', ['Value =', Pointer(Value^), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.getColumnCount;//(var pColumns: Word): SQLResult;
begin
  pColumns := fOdbcNumCols;
  Result := DBXERR_NONE;
end;

function TSqlCursorOdbc.getColumnLength;//(ColumnNumber: Word;
//  var pLength: Longword): SQLResult;
var
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc.getColumnLength', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
    with aOdbcBindCol do
    begin
      case fDbxType of
        { DSIntf.pas }
        fldUNKNOWN:        // = 0;
          begin
            pLength := 0;
          end;
        fldZSTRING:        // = 1;               { Null terminated string }
          begin
            { quantity of symbols without null terminator }
            pLength := fColSize;
          end;
        fldDATE:           // = 2;               { Date     (32 bit) }
          begin
            pLength := 4; // == SizeOf(TDateTime) div 2
          end;
        fldBLOB:           // = 3;               { Blob }
          begin
            pLength := fColSize;
          end;
        fldBOOL:           // = 4;               { Boolean  (16 bit) }
          begin
            pLength := 2; // == SizeOf(WordBool)
          end;
        fldINT16:          // = 5;               { 16 bit signed number }
          begin
            pLength := 2; // == SizeOf(SmallInt)
          end;
        fldINT32:          // = 6;               { 32 bit signed number }
          begin
            pLength := 4; // == SizeOf(Integer)
          end;
        fldFLOAT:          // = 7;               { 64 bit floating point }
          begin
            pLength := 8; // == SizeOf(Double)
          end;
        fldBCD:            // = 8;               { BCD }
          begin
            pLength := (fColSize + 1) div 2 + 2; // max size == 34 = SizeOf(TBCD)
          end;
        fldBYTES:          // = 9;               { Fixed number of bytes }
          begin
            pLength := fColSize;
          end;
        fldTIME:           // = 10;              { Time        (32 bit) }
          begin
            pLength := 4; // == SizeOf(TDateTime) div 2
          end;
        fldTIMESTAMP:      // = 11;              { Time-stamp  (64 bit) }
          begin { fldTIMESTAMP no support in SqlExpr.pas:TCustomSQLDataSet.GetFieldData }
            pLength := 8; // == SizeOf(TDateTime)
          end;
        fldUINT16:         // = 12;              { Unsigned 16 bit integer }
          begin
            pLength := 2; // == SizeOf(SmallInt)
          end;
        fldUINT32:         // = 13;              { Unsigned 32 bit integer }
          begin
            pLength := 4; // == SizeOf(Integer)
          end;
        fldFLOATIEEE:      // = 14;              { 80-bit IEEE float }
          begin
            pLength := 10; // == SizeOf(Extended);
          end;
        fldVARBYTES:       // = 15;              { Length prefixed var bytes }
          begin
            pLength := fColSize;
          end;
        fldLOCKINFO:       // = 16;              { Look for LOCKINFO typedef }
          begin
            pLength := fColSize;
          end;
        fldCURSOR:         // = 17;              { For Oracle Cursor type }
          begin
            pLength := fColSize;
          end;
        fldINT64:          // = 18;              { 64 bit signed number }
          begin
            pLength := 8; // == SizeOf(Int64)
          end;
        fldUINT64:         // = 19;              { Unsigned 64 bit integer }
          begin
            pLength := 8; // == SizeOf(Int64)
          end;
        fldADT:            // = 20;              { Abstract datatype (structure) }
          begin
            pLength := fColSize;
          end;
        fldARRAY:          // = 21;              { Array field type }
          begin
            pLength := fColSize;
          end;
        fldREF:            // = 22;              { Reference to ADT }
          begin
            pLength := fColSize;
          end;
        fldTABLE:          // = 23;              { Nested table (reference) }
          begin
            pLength := fColSize;
          end;
        fldDATETIME:       // = 24;              { Datetime structure for DBExpress }
          begin
            //
            // TSQLTimeStamp type:
            //
            pLength := 16; // == SizeOf(TSQLTimeStamp)
          end;
        fldFMTBCD:         // = 25;              { BCD Variant type: required by Midas, same as BCD for DBExpress}
          begin
            pLength := (fColSize + 1) div 2 + 2; // max size == 34 = SizeOf(TBCD)
          end;
        fldWIDESTRING,     // = 26;              { UCS2 null terminated string }
        fldUNICODE:        // = $1007;           { Unicode }
          begin
            { quantity of symbols without null terminator }
            pLength := fColSize;
          end;
      else
        pLength:= fColSize;
        raise EDbxNotSupported.Create
          ('TSqlCursorOdbc.getColumnLength('+IntToStr(ColumnNumber)+') - not supported type "'
          + IntToStr(fDbxType)+'" for column "'+string(StrPas(fColName))+'".');
      end;
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      pLength := 0; // When Length = 0 then Delphi ignored this field.
      AddError( E );
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.getColumnLength', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.getColumnLength', ['pLength =', pLength]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.getColumnName;//(ColumnNumber: Word;
//  pColumnName: PAnsiChar): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc.getColumnName', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  //OLD: StrCopy(pColumnName, TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]).fColName);
  StrLCopy(pColumnName, TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]).fColName, SizeOf(DBINAME32) - 1);
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.getColumnName', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.getColumnName'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.getColumnNameLength;//(ColumnNumber: Word;
//  var pLen: Word): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorOdbc.getColumnNameLength', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  //pLen := min(StrLen(TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]).fColName), SizeOf(DBINAME32) - 1);
  pLen := min(TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]).fColNameSize, SizeOf(DBINAME32) - 1);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.getColumnNameLength', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.getColumnNameLength', ['pLen =', pLen]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.getColumnPrecision;//(ColumnNumber: Word;
//  var piPrecision: Smallint): SQLResult;
var
  aOdbcBindCol: TOdbcBindCol;
  vColSize: SqlUInteger;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc.getColumnPrecision', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
    with aOdbcBindCol do
      case fDbxType of
        fldBCD,
        fldFMTBCD,
        fldBYTES,
        fldZSTRING,
        fldVARBYTES,
        fldWIDESTRING, fldUNICODE:
          begin
            { quantity of symbols without null terminator for string types: fldZSTRING, fldWIDESTRING, fldUNICODE }
            vColSize := fColSize;
            if vColSize < High(Smallint) then
              piPrecision := vColSize
            else
              piPrecision := High(Smallint);
          end;
      else
        // DBXpress help says "Do not call getColumnPrecision for any other column type."
        // But the donkey SqlExpress calls for EVERY column, so we cannot raise error
        piPrecision := 0;
        // raise EDbxNotSupported.Create(
        //   'TSqlCursorOdbc.getColumnPrecision - not yet supported for data type - column '
        //   + IntToStr(ColumnNumber));
      end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      piPrecision := 0;
      AddError( E );
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.getColumnPrecision', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.getColumnPrecision', ['piPrecision =', piPrecision]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.getColumnScale;//(ColumnNumber: Word;
//  var piScale: Smallint): SQLResult;
var
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc.getColumnScale', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
    with aOdbcBindCol do
      case fDbxType of
        fldBCD:
          piScale := fColScale;
      else
        // getColumnScale is should only be called for fldBCD, fldADT, or fldArray
        // But SqlExpress calls it for EVERY column, so we cannot raise error...
        // raise EDbxNotSupported.Create('TSqlCursorOdbc.getColumnScale - not yet supported '+
        //   'for data type - column ' + IntToStr(ColumnNumber));
        piScale := 0;
      end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      piScale := 0;
      AddError( E );
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.getColumnScale', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.getColumnScale', ['piScale =', piScale]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.getColumnType;//(ColumnNumber: Word;
//  var puType, puSubType: Word): SQLResult;
var
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc.getColumnType', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if ColumnNumber <= fOdbcBindList.Count then
  begin
    aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
    with aOdbcBindCol do
    begin
      puType := fDbxType;
      puSubType := fDbxSubType;
    end;
    Result := DBXERR_NONE;
  end
  else
  begin
    //Result := DBXERR_INVALIDPARAM;

    puType := fldUNKNOWN;
    puSubType := 0;
    Result := DBXERR_NONE;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.getColumnType', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.getColumnType', ['puType =', puType, 'puSubType =', puSubType]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.getDate;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
var
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc.getDate', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
  with aOdbcBindCol do
  begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    IsBlank := (fColValueSizePtr^ = OdbcApi.SQL_NULL_DATA) or
      (fColValueSizePtr^ = OdbcApi.SQL_NO_TOTAL)
      {+?}// HyperFile DBMS:
      {
      or (fOdbcHostVarAddress.ptrAnsiChar^ = cNullAnsiChar)
      or (fOdbcHostVarAddress.ptrSqlDateStruct.Year < 0)
      };
    if IsBlank then
      Integer(Value^) := 0
    else
      with fOdbcHostVarAddress.ptrSqlDateStruct^ do
      {+} // https://sourceforge.net/tracker/index.php?func=detail&aid=1232037&group_id=38250&atid=422094
      begin
        try
          Integer(Value^) := ((365 * 1900) + 94) + Trunc( EncodeDate(Year, Month, Day) );
        except
          // bad date format:
          //if Self.fOwnerDbxConnection.fConnectionOptions[coSafeMode] = osOn then
          begin
            IsBlank := True;
            Integer(Value^) := 0;
          end
          //else
          //  raise;
        end;
      end;
      {+.}
    Result := DBXERR_NONE;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.getDate', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.getDate'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.getDouble;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
var
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc.getDouble', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
  with aOdbcBindCol do
  begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    IsBlank := (fColValueSizePtr^ = OdbcApi.SQL_NULL_DATA) or
      (fColValueSizePtr^ = OdbcApi.SQL_NO_TOTAL);
    if IsBlank then
      Double(Value^) := 0
    else
      Double(Value^) := fOdbcHostVarAddress.ptrSqlDouble^;
    Result := DBXERR_NONE;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.getDouble', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.getDouble'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.getErrorMessage;//(Error: PAnsiChar): SQLResult;
begin
  if Error=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  StrCopy(Error, PAnsiChar(AnsiString(fOwnerCommand.fOwnerDbxConnection.fConnectionErrorLines.Text)));
  fOwnerCommand.fOwnerDbxConnection.fConnectionErrorLines.Clear;
  Result := DBXERR_NONE;
end;

function TSqlCursorOdbc.getErrorMessageLen;//(out ErrorLen: Smallint): SQLResult;
begin
  ErrorLen := Length(fOwnerCommand.fOwnerDbxConnection.fConnectionErrorLines.Text);
  Result := DBXERR_NONE;
end;

function TSqlCursorOdbc.getLong;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
var
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc.getLong', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
  with aOdbcBindCol do
  begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    IsBlank := (fColValueSizePtr^ = OdbcApi.SQL_NULL_DATA) or
      (fColValueSizePtr^ = OdbcApi.SQL_NO_TOTAL);
    if IsBlank then
      Integer(Value^) := 0
    else
      Integer(Value^) := fOdbcHostVarAddress.ptrSqlInteger^;
  end;
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.getLong', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.getLong', ['Value =', Integer(Value^), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.GetOption;//(eOption: TSQLCursorOption;
//  PropValue: Pointer; MaxLength: Smallint; out Length: Smallint): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc.GetOption', ['eOption =', cSQLCursorOption[eOption]]); {$ENDIF _TRACE_CALLS_}
  if PropValue = nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  try
    raise EDbxNotSupported.Create('TSqlCursorOdbc.GetOption - not yet supported');
  except
    on E: Exception{EDbxError} do
    begin
      AddError( E );
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.GetOption', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.GetOption'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.getShort;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
var
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc.getShort', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
  with aOdbcBindCol do
  begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    IsBlank := (fColValueSizePtr^ = OdbcApi.SQL_NULL_DATA) or
      (fColValueSizePtr^ = OdbcApi.SQL_NO_TOTAL);
    if IsBlank then
      Smallint(Value^) := 0
    else if fOdbcHostVarType = SQL_C_BIT then
      Smallint(Value^) := fOdbcHostVarAddress.ptrSqlByte^
    else
      Smallint(Value^) := fOdbcHostVarAddress.ptrSqlSmallint^;
    Result := DBXERR_NONE;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.getShort', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.getShort', ['Value =', Smallint(Value^), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.getString;//(ColumnNumber: Word;
//  Value: Pointer; { - String buffer. Delphi DB RTL allocated memory for it buffer: 'fColSize + 1'.}
//  var IsBlank: LongBool): SQLResult;
var
  vColValueSize: SqlUInteger;
  RCh: PAnsiChar;
  aOdbcBindCol: TOdbcBindCol;
  bNotUnicodeString: Boolean;
  pDestA: PAnsiChar absolute Value;
  pDestW: PWideChar absolute Value;
begin
  {$IFDEF _TRACE_CALLS_}
    if Value<>nil then
      pDestA^ := cNullAnsiChar;
    IsBlank := True;
    Result := DBXERR_NONE;
    try try
    LogEnterProc('TSqlCursorOdbc.getString', ['ColumnNumber =', ColumnNumber]);
  {$ENDIF _TRACE_CALLS_}
  if Value = nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
  with aOdbcBindCol do
  begin
    if fOdbcLateBound then
    begin
      if not fIsBuffer then
        FetchLateBoundData(ColumnNumber)
      else
        FetchLongData(ColumnNumber);
    end;

    bNotUnicodeString := not ( (fDbxType = fldWIDESTRING) or ((fDbxSubType and fldstWIDEMEMO) <> 0) or (fDbxType = fldUNICODE) );

    // check buffer overflow (for bad odbc drivers).
    vColValueSize := fColValueSizePtr^;

    IsBlank := (vColValueSize = OdbcApi.SQL_NULL_DATA) or
      (vColValueSize = OdbcApi.SQL_NO_TOTAL);

    if (not IsBlank) and (vColValueSize > 0) then
    begin
      if bNotUnicodeString then
      begin
        if vColValueSize > fColSize then
          vColValueSize := fColSize;
      end
      else
      begin
        if vColValueSize > fColSize * SizeOf(WideChar) then
          vColValueSize := fColSize * SizeOf(WideChar);
      end
    end
    else
    begin
      vColValueSize := 0;
      IsBlank := True;
    end;

    if IsBlank then
    begin
      if bNotUnicodeString then
        pDestA^ := cNullAnsiChar
      else
        pDestW^ := cNullWideChar;
    end
    else
    begin
      if vColValueSize = 0 then
      begin
        if bNotUnicodeString then
          pDestA^ := cNullAnsiChar
        else
          pDestW^ := cNullWideChar;
      end
      else
      if (fDbxSubType and fldstFIXED <> 0) and fOwnerCommand.fTrimChar then
      begin
        RCh := PAnsiChar(DWORD(fOdbcHostVarAddress.ptrAnsiChar) + DWORD(vColValueSize - 1));
        if bNotUnicodeString then
        begin
          while (RCh >= fOdbcHostVarAddress.ptrAnsiChar) and (RCh^ = ' ') do
            Dec(RCh);
          vColValueSize := DWORD(RCh - fOdbcHostVarAddress.ptrAnsiChar) + 1;
          if vColValueSize > 0 then
            Move(fOdbcHostVarAddress.ptrAnsiChar^, pDestA^, vColValueSize);
          pDestA[vColValueSize] := cNullAnsiChar;
        end
        else
        begin
          while (RCh <> fOdbcHostVarAddress.ptrAnsiChar) and (PWideChar(RCh)^ = WideChar(' ')) do
            Dec(RCh, SizeOf(WideChar));
          vColValueSize := DWORD(RCh - fOdbcHostVarAddress.ptrAnsiChar) div SizeOf(WideChar) + 1;
          if vColValueSize >= 0 then
            Move(fOdbcHostVarAddress.ptrAnsiChar^, pDestW[1], vColValueSize * SizeOf(WideChar));
          Word(Value^) := vColValueSize * SizeOf(WideChar); // == wide string chars
          pDestW[vColValueSize + 1] := cNullWideChar;
        end;
      end
      else
      begin
        if bNotUnicodeString then
        begin
          Move(fOdbcHostVarAddress.ptrAnsiChar^, pDestA^, vColValueSize);
          Inc(pDestA, vColValueSize);
          if (fDbxSubType and fldstFIXED <> 0) and (vColValueSize < fColSize) then
          begin
            // we shall add lacking spaces if driver their cuts itself
            vColValueSize := fColSize - vColValueSize;
            FillChar(pDestA^, vColValueSize,  AnsiChar(' '));
            Inc(pDestA, vColValueSize);
          end;
          pDestA^ := cNullAnsiChar;
        end
        else
        begin
          Move(fOdbcHostVarAddress.ptrWideChar^, pDestW[1], vColValueSize);
          Word(Value^) := vColValueSize; // == wide string chars
          pDestW[vColValueSize div SizeOf(WideChar) + 1] := cNullWideChar;
        end;
      end;
    end;
    Result := DBXERR_NONE;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.getString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.getString', ['Value =', PAnsiChar(Value), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.getTime;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
var
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc.getTime', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
  with aOdbcBindCol do
  begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    IsBlank := (fColValueSizePtr^ = OdbcApi.SQL_NULL_DATA) or
      (fColValueSizePtr^ = OdbcApi.SQL_NO_TOTAL);
    if IsBlank then
      Longword(Value^) := 0
    else // Returned value is time, in Microseconds
      with fOdbcHostVarAddress.ptrSqlTimeStruct^ do
        Longword(Value^) := ( (Hour * 60 * 60) + (Minute * 60) + Second) * 1000;
    Result := DBXERR_NONE;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.getTime', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.getTime'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.getTimeStamp;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
var
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc.getTimeStamp', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
  with aOdbcBindCol do
  begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    IsBlank := (fColValueSizePtr^ = OdbcApi.SQL_NULL_DATA) or
      (fColValueSizePtr^ = OdbcApi.SQL_NO_TOTAL);
    if IsBlank then
      FillChar(PSQLTimeStamp(Value)^, SizeOf(TSQLTimeStamp), 0)
    else
    //if fOdbcHostVarType = SQL_BINARY then
    // ...
    //else
    begin
      with fOdbcHostVarAddress.ptrOdbcTimestamp^ do
      begin
        if Year <> 0 then
        begin
          PSQLTimeStamp(Value).Year := Year;
          PSQLTimeStamp(Value).Month := Month;
          PSQLTimeStamp(Value).Day := Day;
          PSQLTimeStamp(Value).Hour := Hour;
          PSQLTimeStamp(Value).Minute := Minute;
          PSQLTimeStamp(Value).Second := Second;
          // Odbc returns nanoseconds; DbExpress expects milliseconds; so divide by 1 million
          PSQLTimeStamp(Value).Fractions := Fraction div 1000000;
        end
        else
        begin
          IsBlank := True;
          FillChar(PSQLTimeStamp(Value)^, SizeOf(TSQLTimeStamp), 0)
        end;
      end;
    end;
  end;
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.getTimeStamp', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.getTimeStamp'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.isAutoIncrement;//(ColumnNumber: Word;
//  var AutoIncr: LongBool): SQLResult;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  IntAttribute: Integer;
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc.isAutoIncrement', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  with fOwnerDbxDriver.fOdbcApi do
  try
    if Self.fOwnerDbxConnection.fConnectionOptions[coSupportsAutoInc] = osOff then
    begin
      AutoIncr := False;
      Result := DBXERR_NONE;
      exit;
    end;
    aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
    case aOdbcBindCol.fDbxType of
      fldINT16, fldUINT16,
      fldINT32, fldUINT32:; // Delphi supported AutoInc only for TIntegerField. See DB.PAS: "function TParam.GetDataSize: Integer;".
      else
        begin
          AutoIncr := False;
          Result := DBXERR_NONE;
          exit;
        end;
    end;
    IntAttribute := OdbcApi.SQL_FALSE;
    OdbcRetcode := SQLColAttributeInt(fHStmt, aOdbcBindCol.fOdbcColNo, SQL_DESC_AUTO_UNIQUE_VALUE,
      nil, 0, nil, IntAttribute);
    // SQLite does not support this option
    // Old code:
    //if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
    //  OdbcCheck(OdbcRetcode, 'SQLColAttribute');
    //AutoIncr := (IntAttribute = SQL_TRUE);
    // New code:
    AutoIncr := (OdbcRetcode = OdbcApi.SQL_SUCCESS) and (IntAttribute = SQL_TRUE);
    // clear last error:
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
        fOwnerCommand.fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, fOwnerCommand, nil, 1);
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      AutoIncr := (False);
      AddError( E );
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.isAutoIncrement', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.isAutoIncrement', ['AutoIncr =', Boolean(AutoIncr)]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.isBlobSizeExact;//(ColumnNumber: Word;
//  var IsExact: LongBool): SQLResult;
var
  aOdbcBindCol: TOdbcBindCol;
begin
  // It is not used in "SqlExpr.pas"
  Result := DBXERR_NONE;
  //IsExact := True;
  aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
  IsExact := (aOdbcBindCol.fColSize > 0);
end;

function TSqlCursorOdbc.isNullable;//(ColumnNumber: Word;
//  var Nullable: LongBool): SQLResult;
var
  aOdbcBindCol: TOdbcBindCol;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorOdbc.isNullable', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
  Nullable := aOdbcBindCol.fNullable <> SQL_NO_NULLS;
  (*
  case aOdbcBindCol.fNullable of
    SQL_NULLABLE:
      Nullable := True;
    SQL_NO_NULLS:
      Nullable := False;
  else { SQL_NULLABLE_UNKNOWN: }
    Nullable := True;
  end;
  *)
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.isNullable', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.isNullable'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.isReadOnly;//(ColumnNumber: Word;
//  var ReadOnly: LongBool): SQLResult;
{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
{
var
  OdbcRetcode: OdbcApi.SqlReturn;
  IntAttribute: Integer;
  aOdbcBindCol: TOdbcBindCol;
{}
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorOdbc.isReadOnly', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  with fOwnerDbxDriver.fOdbcApi do
  try
    ReadOnly := False;
(*
//
// QC: 58471: any field after readonly field is readonly also.
//
procedure TCustomSQLDataSet.InternalInitFieldDefs;
  ...
  LoadFieldDef(Word(FID), FieldDescs[0]); // this method does not clean readonly flag in FieldDescs[0]

fixes: SqlExp.pas:
...
procedure TCustomSQLDataSet.LoadFieldDef(FieldID: Word; var FldDesc: TFLDDesc);
...
  if ReadOnly then
  // ******************************************** begin changes.
    FldDesc.efldrRights := fldrREADONLY
  else
    FldDesc.efldrRights := fldrREADWRITE;
  // ******************************************** end changes.
end;
...
*)
(*
    if fOwnerDbxConnection.fConnectionOptions[coFldReadOnly] = osOff then
    begin
      ReadOnly := False;
      exit;
    end;
    // OLD:
    //Result := isAutoIncrement(ColumnNumber, ReadOnly);

    // NEW
    aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
    if aOdbcBindCol.fReadOnly < 0 then
    begin
      IntAttribute := SQL_ATTR_WRITE;
      OdbcRetcode := SQLColAttributeInt(fHStmt, aOdbcBindCol.fOdbcColNo, SQL_DESC_UPDATABLE,
        nil, 0, nil, IntAttribute);
      ReadOnly := (OdbcRetcode = OdbcApi.SQL_SUCCESS) and (IntAttribute = SQL_ATTR_READONLY);
      // clear last error:
      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      begin
        fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
          fOwnerCommand.fDbxConStmtInfo.fDbxConStmt, fOwnerDbxConnection, fOwnerCommand, nil, 1);
        ReadOnly := False;
      end;
      if not ReadOnly then
        aOdbcBindCol.fReadOnly := 0
      else
        aOdbcBindCol.fReadOnly := 1;
    end;
    ReadOnly := aOdbcBindCol.fReadOnly > 0;
//*)
  except
    on E: Exception{EDbxError} do
    begin
      AddError( E );
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.isReadOnly', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.isReadOnly', ['ReadOnly =', Boolean(ReadOnly)]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.isSearchable;//(ColumnNumber: Word;
//  var Searchable: LongBool): SQLResult;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  IntAttribute: Integer;
  aOdbcBindCol: TOdbcBindCol;
begin
 // It is not used in "SqlExpr.pas"
 Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorOdbc.isSearchable', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  with fOwnerDbxDriver.fOdbcApi do
  try
    aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
    OdbcRetcode := SQLColAttributeInt(fHStmt, aOdbcBindCol.fOdbcColNo, SQL_DESC_SEARCHABLE,
      nil, 0, nil, IntAttribute);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetcode, 'SQLColAttribute(isSearchable)');
    Searchable := (IntAttribute <> SQL_PRED_NONE);
  except
    on E: Exception{EDbxError} do
    begin
      AddError( E );
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.isSearchable', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.isSearchable', ['Searchable =', Boolean(Searchable)]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.next: SQLResult;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  i: Integer;
  bSkipFetch: Boolean;
  aOdbcBindCol: TOdbcBindCol;
  vLastHostVarAddress: Pointer;

  function GetRowStatus( Status: Integer ): AnsiString; {$IFDEF _INLINE_} inline; {$ENDIF}
  begin
    case Status of
      SQL_ROW_SUCCESS: // == SQL_ROW_PROCEED:
        Result := '(SQL_ROW_SUCCESS)';
      SQL_ROW_DELETED: // == SQL_ROW_IGNORE
        Result := '(SQL_ROW_IGNORE)';
      SQL_ROW_UPDATED:
        Result := '(SQL_ROW_UPDATED)';
      SQL_ROW_NOROW:
        Result := '(SQL_ROW_NOROW)';
      SQL_ROW_ADDED:
        Result := '(SQL_ROW_ADDED)';
      SQL_ROW_ERROR:
        Result := '(SQL_ROW_ERROR)';
      SQL_ROW_SUCCESS_WITH_INFO:
        Result := '(SQL_ROW_SUCCESS_WITH_INFO)';
      else
        Result := AnsiString('Unknown "(' + IntToStr(Status) + ')');
    end;
  end;

begin
  {$IFDEF _TRACE_CALLS_}
    Result := DBXERR_NONE; try try
    if fCursorFetchRowCount > 1 then
      LogEnterProc('TSqlCursorOdbc.next',['OdbcBindBufferPos', fOdbcBindBufferPos])
    else
      LogEnterProc('TSqlCursorOdbc.next');
  {$ENDIF _TRACE_CALLS_}
  with fOwnerDbxDriver.fOdbcApi do
  try

    if (fHStmt = SQL_NULL_HANDLE) or (fOwnerCommand.fIsMoreResults = 2) then
    begin
      Result := DBXERR_EOF;
      Exit;
    end;

    fRowNo := fRowNo + 1;

    //???: TODO: add fRowLimit to Connection Options (as in  BDE: "MAX ROWS")
    //  if (fRowNo > fOwnerDbxConnection.fRowLimit) then
    //    Result := DBXERR_EOF;

    if fCursorFetchRowCount > 1 then
    begin
      if fOdbcBindBufferPos >= 0 then
      begin
        inc(fOdbcBindBufferPos);
        bSkipFetch := (fOdbcBindBufferPos < fCursorFetchRowCount);
        if bSkipFetch then
        begin
          // check buffer pos
          if (fOdbcBindBufferPos >= fOdbcRowsFetched) then
          begin
            {
             Odbc driver can ignore the specified value of quantity of rows in a 'ARRAY BUFFER' and to give any other quantity of rows. It is defined by value in a variable fOdbcRowsFetched.
             For example: 'SAP DB' ODBC Driver, ver: '7.04.03.00' always returns "1".
            }
            // OLD CODE:
            //
            //  Result := DBXERR_EOF;
            //  fOdbcBindBufferPos := -1;
            //  exit;// EOF in Buffer
            //
            // NEW CODE:
            //
            fOdbcBindBufferPos := 0;
            vLastHostVarAddress := fOdbcBindBuffer;
            bSkipFetch := False;
            //
          end
          else
          begin
            // rebase base fetching addresses buffer to next record
            vLastHostVarAddress := Pointer( DWORD(fOdbcBindBuffer) +
              (DWORD(fOdbcBindBufferPos) * DWORD(fOdbcBindBufferRowSize)) );
          end;
        end
        else // buffer pos = last record, need fetched next block
        begin
          // rebase base fetching addresses buffer to first record
          fOdbcBindBufferPos := 0;
          vLastHostVarAddress := fOdbcBindBuffer;
        end;

        // rebase buffer values and size addresses for binded columns
        for i := 0 to fOdbcNumCols-1 do
        begin
          aOdbcBindCol := TOdbcBindCol(fOdbcBindList.Items[i]);
          with aOdbcBindCol do
          begin
            if not fOdbcLateBound then
            begin
              fColValueSizePtr := vLastHostVarAddress;
              inc(DWORD(vLastHostVarAddress), SizeOf(SqlInteger));
              fOdbcHostVarAddress.Ptr := vLastHostVarAddress;
              inc(DWORD(vLastHostVarAddress), fOdbcHostVarSize);
            end;
          end;
        end;

      end
      else  // first call Fetch
      begin
        bSkipFetch := False;
        fOdbcBindBufferPos := 0;
      end;

      if not bSkipFetch then
      begin
        OdbcRetcode := SQLFetch(fHStmt);
        case OdbcRetcode of
          OdbcApi.SQL_SUCCESS:
            begin
              Result := DBXERR_NONE;
            end;
          OdbcApi.SQL_SUCCESS_WITH_INFO:
            begin
              // clear last error or warning:
              //???: fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, fOwnerDbxConnection, fOwnerCommand, 1);
              case fOdbcRowsStatus[fOdbcBindBufferPos] of
                SQL_ROW_SUCCESS:
                  Result := DBXERR_NONE;
                SQL_ROW_SUCCESS_WITH_INFO:
                  Result := DBXERR_NONE;
                else
                begin
                  Result := DBX_FIRSTSTATICERRORS; // Dummy to prevent compiler warning
                  if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
                    OdbcCheck(OdbcRetcode, 'SQLFetch(BlockRead)');
                end
              end;
            end;
          OdbcApi.SQL_NO_DATA:
            begin
              Result := DBXERR_EOF;
              fOdbcBindBufferPos := -1;
            end;
          else
            begin
              Result := DBX_FIRSTSTATICERRORS; // Dummy to prevent compiler warning
              if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
                OdbcCheck(OdbcRetcode, 'SQLFetch(BlockRead)',
                  {Limitation of errors quantity for buffered fetch = }1);
            end
        end;//of: case OdbcRetcode.
      end//of:if not bSkipFetch
      else
      begin
        if fOdbcLateBoundsFound then
        begin
          OdbcRetcode := SQLSetPos(fHStmt, fOdbcBindBufferPos+1, SQL_POSITION, SQL_LOCK_NO_CHANGE);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            OdbcCheck(OdbcRetcode, AnsiString('SQLSetPos(buffer pos='+IntToStr(fOdbcBindBufferPos+1)+'). '+
            ' Absolute row: ' + FloatToStr(fRowNo) + '.'));
        end;
        case fOdbcRowsStatus[fOdbcBindBufferPos] of
          SQL_ROW_SUCCESS:
            Result := DBXERR_NONE;
          SQL_ROW_SUCCESS_WITH_INFO:
            Result := DBXERR_NONE;
          else
          begin
            raise EDbxError.Create( 'SQLFetch(BlockRead): Error in fetched buffer for row: '+
              IntToStr(fOdbcBindBufferPos)+'. Absolute row: ' + FloatToStr(fRowNo)+
              ' . Row Status: '+
              string(GetRowStatus(fOdbcRowsStatus[fOdbcBindBufferPos])) + '.' );
          end
        end;
      end;
    end
    else
    begin
      OdbcRetcode := SQLFetch(fHStmt);
      case OdbcRetcode of
        OdbcApi.SQL_SUCCESS:
          Result := DBXERR_NONE;
        OdbcApi.SQL_SUCCESS_WITH_INFO: // EOdbcWarning raised (warning only)
          begin
            Result := DBXERR_NONE;
            // clear last error or warning:
            //???: fOwnerDbxDriver.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt, fOwnerDbxConnection, fOwnerCommand, 1);
          end;
        OdbcApi.SQL_NO_DATA:
          Result := DBXERR_EOF
        else
          begin
            Result := DBX_FIRSTSTATICERRORS; // Dummy to prevent compiler warning
            if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
              OdbcCheck(OdbcRetcode, 'SQLFetch');
          end
      end;//of: case OdbcRetcode.
    end;

    // clear fetched flags & free temporary (BLOBs) buffers
    for i := 0 to fOdbcBindList.Count - 1 do
    begin
      aOdbcBindCol := TOdbcBindCol(fOdbcBindList[i]);
      with aOdbcBindCol do
      begin
        // clear fetched flags:
        fIsFetched := False;
        // free temporary (BLOBs) buffers:
        //if fIsBuffer then // Free Allocated temporary buffer (Next blob value can be NULL).
        //  FreeMemAndNil(fOdbcHostVarAddress.Ptr);
        //if fBlobChunkCollection <> nil then
        //  fBlobChunkCollection.Clear;
        //fOdbcHostVarChunkSize := 0;
      end;
    end;

    //{
    // Minimization of use of cursors.
    // It is critical when fStatementPerConnection is very small (SQL Server).
    if (fOwnerDbxConnection.fStatementPerConnection > 0)
       and
       (Result = DBXERR_EOF)
       and  // Restriction on quantity SqllHStmt is exhausted:
       (fOwnerCommand.fDbxConStmtInfo.fDbxConStmt.fSqlHStmtAllocated = fOwnerDbxConnection.fStatementPerConnection)
    then
    begin
      Self.ClearCursor({FreeStmt:}True); // close cursor for update queries
    end;
    {}

  except
    on E: Exception{EDbxError} do
    begin
      AddError( E );
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.next', e);  raise; end; end;
    finally
      if fCursorFetchRowCount > 1 then
        LogExitProc('TSqlCursorOdbc.next',['OdbcBindBufferPos', fOdbcBindBufferPos])
      else
        LogExitProc('TSqlCursorOdbc.next');
    end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc.SetOption;//(eOption: TSQLCursorOption;
//  PropValue: Integer): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc.SetOption', ['eOption =', cSQLCursorOption[eOption], 'PropValue =', PropValue]); {$ENDIF _TRACE_CALLS_}
  try
    raise EDbxNotSupported.Create('TSqlCursorOdbc.SetOption - not yet supported');
  except
    on E: Exception{EDbxError} do
    begin
      AddError( E );
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc.SetOption', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc.SetOption'); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSqlCursorMetaData }

constructor TSqlCursorMetaData.Create;//(ASupportWideString: Boolean; OwnerSqlMetaData: TSqlMetaDataOdbc);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaData.Create'); {$ENDIF _TRACE_CALLS_}
  inherited Create;
  fSupportWideString := ASupportWideString;
  fHStmt := SQL_NULL_HANDLE;
  fSqlCursorErrorMsg := TStringList.Create;
  fOwnerMetaData := OwnerSqlMetaData;
  fSqlConnectionOdbc := fOwnerMetaData.fOwnerDbxConnection;
  fSqlDriverOdbc := fSqlConnectionOdbc.fOwnerDbxDriver;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.Create', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TSqlCursorMetaData.Destroy;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaData.Destroy'); {$ENDIF _TRACE_CALLS_}
  FreeAndNil(fSqlCursorErrorMsg);
  ClearMetaData;
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.Destroy', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCursorMetaData.ClearMetaData;
begin
  FreeMemAndNil(fMetaCatalogName);
  FreeMemAndNil(fMetaSchemaName);
  FreeMemAndNil(fMetaTableName);
end;

procedure TSqlCursorMetaData.ParseTableNameBase;//(TableName: PAnsiChar);
var
  CatalogName, SchemaName, ObjectName: AnsiString;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaData.ParseTableNameBase', ['TableName =', TableName]); {$ENDIF _TRACE_CALLS_}
  ClearMetaData;
  if (TableName=nil)or(TableName^ = cNullAnsiChar) then
    Exit;
  fSqlConnectionOdbc.DecodeObjectFullName(
    StrPas(TableName), CatalogName, SchemaName, ObjectName);
  if Length(ObjectName) = 0 then
    exit;
  // OBJECT:
  GetMem(fMetaTableName, Length(ObjectName) + 1);
  StrLCopy(fMetaTableName, PAnsiChar(ObjectName), Length(ObjectName) + 1);
  // SCHEMA:
  if Length(SchemaName) > 0 then
  begin
    GetMem(fMetaSchemaName, Length(SchemaName) + 1);
    StrLCopy(fMetaSchemaName, PAnsiChar(SchemaName), Length(SchemaName) + 1);
  end;
  // CATALOG:
  if Length(CatalogName) > 0 then
  begin
    GetMem(fMetaCatalogName, Length(CatalogName) + 1);
    StrLCopy(fMetaCatalogName, PAnsiChar(CatalogName), Length(CatalogName) + 1);
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.ParseTableNameBase', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.ParseTableNameBase', ['CatalogName =', CatalogName, 'SchemaName =', SchemaName, 'MetaTableName =', fMetaTableName]); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCursorMetaData.ParseTableName;//(CatalogName, SchemaName, TableName: PAnsiChar);
var
  iLen: Integer;
begin
  ParseTableNameBase(TableName);
  iLen := StrLenNil(CatalogName);
  if (iLen > 0) and (CatalogName^ = '?') then
  begin
    CatalogName := nil;
    iLen := 0;
  end;
  if (fMetaCatalogName = nil) and (iLen <> 0) then
  begin
    GetMem(fMetaCatalogName, iLen + 1);
    StrLCopy(fMetaCatalogName, CatalogName, iLen + 1);
  end;
  iLen := StrLenNil(SchemaName);
  if (fMetaSchemaName = nil) and (iLen <> 0) then
  begin
    GetMem(fMetaSchemaName, iLen + 1);
    StrLCopy(fMetaSchemaName, SchemaName, iLen + 1);
  end;
end;

function TSqlCursorMetaData.DescribeAllocBindString;//(ColumnNo: SqlUSmallint;
//  var BindString: PAnsiChar; var BindInd: SqlInteger; bIgnoreError: Boolean = False): Boolean;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  cbColName: SqlSmallint;
  szColNameTemp: AnsiString;
  aSqlType: SqlSmallint;
  aScale: SqlSmallint;
  aNullable: SqlSmallint;
  aColSize: SqlUInteger;
begin
  {$IFDEF _TRACE_CALLS_} Result := False; try try LogEnterProc('TSqlCursorMetaData.DescribeAllocBindString', ['ColumnNo =', ColumnNo]); {$ENDIF _TRACE_CALLS_}
  with fSqlDriverOdbc.fOdbcApi do
  begin

  SetLength(szColNameTemp, 255);
  OdbcRetcode := SQLDescribeCol(
    fHStmt, ColumnNo, PAnsiChar(szColNameTemp), 255, cbColName,
    aSqlType, aColSize, aScale, aNullable);
  Result := OdbcRetcode = OdbcApi.SQL_SUCCESS;
  if not Result then
  begin
    if bIgnoreError then
    begin
      fSqlDriverOdbc.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
        nil, fSqlConnectionOdbc, nil, nil, 1);
      exit;
    end;
    OdbcCheck(OdbcRetcode, 'SQLDescribeCol');
  end;
  BindString := AllocMem(aColSize + 1);
  OdbcRetcode := SQLBindCol(fHStmt, ColumnNo, SQL_C_CHAR, BindString, aColSize + 1, @BindInd);
  Result := OdbcRetcode = OdbcApi.SQL_SUCCESS;
  if (not Result) then
    if (not bIgnoreError) then
      OdbcCheck(OdbcRetcode, 'SQLBindCol(SQL_C_CHAR)')
    else
      fSqlDriverOdbc.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
        nil, fSqlConnectionOdbc, nil, nil, 1);

  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.DescribeAllocBindString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.DescribeAllocBindString'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.DescribeAllocBindWString;//(ColumnNo: SqlUSmallint;
//  var BindString: PWideChar; var BindInd: SqlInteger; bIgnoreError: Boolean = False): Boolean;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  cbColName: SqlSmallint;
  wszColNameTemp: WideString;
  aSqlType: SqlSmallint;
  aScale: SqlSmallint;
  aNullable: SqlSmallint;
  aColSize: SqlUInteger;
begin
  Result := False;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaData.DescribeAllocBindWString', ['ColumnNo =', ColumnNo]); {$ENDIF _TRACE_CALLS_}
  with fSqlDriverOdbc.fOdbcApi do
  begin

  if not Assigned(SQLDescribeColW) then
    Exit;

  SetLength(wszColNameTemp, 255);
  OdbcRetcode := SQLDescribeColW(
    fHStmt, ColumnNo, PAnsiChar(PWideChar(wszColNameTemp)), 255 * SizeOf(WideChar), cbColName,
    aSqlType, aColSize, aScale, aNullable);
  Result := OdbcRetcode = OdbcApi.SQL_SUCCESS;
  if not Result then
  begin
    if bIgnoreError then
    begin
      fSqlDriverOdbc.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
        nil, fSqlConnectionOdbc, nil, nil, 1);
      exit;
    end;
    OdbcCheck(OdbcRetcode, 'SQLDescribeColW');
  end;
  BindString := AllocMem((aColSize + 1) * SizeOf(WideChar));
  OdbcRetcode := SQLBindCol(fHStmt, ColumnNo, SQL_C_WCHAR, BindString, (aColSize + 1) * SizeOf(WideChar), @BindInd);
  Result := OdbcRetcode = OdbcApi.SQL_SUCCESS;
  if (not Result) then
    if (not bIgnoreError) then
      OdbcCheck(OdbcRetcode, 'SQLBindCol(SQL_C_WCHAR)')
    else
      fSqlDriverOdbc.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
        nil, fSqlConnectionOdbc, nil, nil, 1);

  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.DescribeAllocBindWString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.DescribeAllocBindWString'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.BindSmallint;//(ColumnNo: SqlUSmallint;
//  var BindSmallint: Smallint; PBindInd: PSqlInteger; bIgnoreError: Boolean = False): Boolean;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  cbColName: SqlSmallint;
  szColNameTemp: array[0..255] of AnsiChar;
  aSqlType: SqlSmallint;
  aScale: SqlSmallint;
  aNullable: SqlSmallint;
  aColSize: SqlUInteger;
begin
  {$IFDEF _TRACE_CALLS_} Result := False; try try LogEnterProc('TSqlCursorMetaData.BindSmallint', ['ColumnNo =', ColumnNo]); {$ENDIF _TRACE_CALLS_}
  with fSqlDriverOdbc.fOdbcApi do
  begin

  OdbcRetcode := SQLDescribeCol(
    fHStmt, ColumnNo, szColNameTemp, 255, cbColName,
    aSqlType, aColSize, aScale, aNullable);
  Result := OdbcRetcode = OdbcApi.SQL_SUCCESS;
  if not Result then
  begin
    if bIgnoreError then
    begin
      fSqlDriverOdbc.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
        nil, fSqlConnectionOdbc, nil, nil, 1);
      exit;
    end;
    OdbcCheck(OdbcRetcode, 'SQLDescribeCol');
  end;

  if (aSqlType <> SQL_C_SHORT) then
    {+2.01}
    //Think SQL:
    // Vadim> ???Vad>All:
    // Edward> I do not have ThinkSQL, but if that's how it works, your fix is OK
    if (fSqlConnectionOdbc.fOdbcDriverType = eOdbcDriverTypeThinkSQL) and
      not (aSqlType in [SQL_INTEGER, SQL_NUMERIC]) then
      {/+2.01}
    begin
      if bIgnoreError then
        exit;
      raise EDbxInternalError.Create(
        'BindSmallInt called for non SmallInt column no '
        + IntToStr(ColumnNo) + ' - ' + string(szColNameTemp));
    end;
  if (PBindInd = nil) and (aNullable <> OdbcApi.SQL_NO_NULLS) then
  begin
    Result := False;
    if bIgnoreError then
      exit;
    raise EDbxInternalError.Create(
      'BindInteger without indicator var for nullable column '
      + IntToStr(ColumnNo) + ' - ' + string(szColNameTemp));
  end;
  OdbcRetcode := SQLBindCol(
    fHStmt, ColumnNo, SQL_C_SHORT, @BindSmallint, Sizeof(Smallint), PBindInd);
  Result := OdbcRetcode = OdbcApi.SQL_SUCCESS;
  if not Result then
  begin
    if bIgnoreError then
    begin
      fSqlDriverOdbc.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
        nil, fSqlConnectionOdbc, nil, nil, 1);
      exit;
    end;
    OdbcCheck(OdbcRetcode, 'SQLBindCol');
  end;

  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.BindSmallint', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.BindSmallint'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.BindInteger;//(ColumnNo: SqlUSmallint;
//  var BindInteger: Integer; BindInd: PSqlInteger; bIgnoreError: Boolean = False): Boolean;
var
  OdbcRetcode: OdbcApi.SqlReturn;
  cbColName: SqlSmallint;
  szColNameTemp: array[0..255] of AnsiChar;
  aSqlType: SqlSmallint;
  aScale: SqlSmallint;
  aNullable: SqlSmallint;
  aColSize: SqlUInteger;
begin
  {$IFDEF _TRACE_CALLS_} Result := False; try try LogEnterProc('TSqlCursorMetaData.BindInteger', ['ColumnNo =', ColumnNo]); {$ENDIF _TRACE_CALLS_}
  with fSqlDriverOdbc.fOdbcApi do
  begin

  OdbcRetcode := SQLDescribeCol(
    fHStmt, ColumnNo, szColNameTemp, 255, cbColName,
    aSqlType, aColSize, aScale, aNullable);
  Result := OdbcRetcode = OdbcApi.SQL_SUCCESS;
  if not Result then
  begin
    if bIgnoreError then
    begin
      fSqlDriverOdbc.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
        nil, fSqlConnectionOdbc, nil, nil, 1);
      exit;
    end;
    OdbcCheck(OdbcRetcode, 'SQLDescribeCol');
  end;

  {+2.01}
  // INFORMIX: SQL_C_SHORT in INFORMIX
  // Edward> This is fine -
  // Edward> ???Ed>Ed: I thought I had already fixed this -
  // ORIGINAL CODE:
  // if (aSqlType <> SQL_C_LONG) then
  // NEW CODE:
  if not (aSqlType in [SQL_C_LONG, SQL_C_SHORT]) then
    {/+2.01}
  begin
    Result := False;
    if bIgnoreError then
      exit;
    raise EDbxInternalError.Create
      ('BindInteger called for non Integer column no '
      + IntToStr(ColumnNo) + ' - ' + string(szColNameTemp));
  end;
  if (BindInd = nil) and (aNullable <> OdbcApi.SQL_NO_NULLS) then
  begin
    Result := False;
    if bIgnoreError then
      exit;
    raise EDbxInternalError.Create
      ('BindInteger without indicator var for nullable column '
      + IntToStr(ColumnNo) + ' - ' + string(szColNameTemp));
  end;
  OdbcRetcode := SQLBindCol(
    fHStmt, ColumnNo, SQL_C_LONG, @BindInteger, Sizeof(Integer), BindInd);
  Result := OdbcRetcode = OdbcApi.SQL_SUCCESS;
  if not Result then
  begin
    if bIgnoreError then
    begin
      fSqlDriverOdbc.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
        nil, fSqlConnectionOdbc, nil, nil, 1);
      exit;
    end;
    OdbcCheck(OdbcRetcode, 'SQLBindCol');
  end;

  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.BindInteger', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.BindInteger'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.getBcd;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.getBcd', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    raise EDbxInternalError.Create
      ('getBcd - Unimplemented method invoked on metadata cursor');
  except
    on E: EDbxError do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.getBcd', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.getBcd'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.getBlob;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool; Length: Longword): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.getBlob', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  //with fSqlDriverOdbc.fOdbcApi do
  try
    raise EDbxInternalError.Create
      ('getBlob - Unimplemented method invoked on metadata cursor');
  except
    on E: EDbxError do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.getBlob', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.getBlob'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.getBlobSize;//(ColumnNumber: Word;
//  var Length: Longword; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.getBlobSize', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  //with fSqlDriverOdbc.fOdbcApi do
  try
    raise EDbxInternalError.Create
      ('getBlobSize - Unimplemented method invoked on metadata cursor');
  except
    on E: EDbxError do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.getBlobSize', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.getBlobSize'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.getBytes;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.getBytes', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  //with fSqlDriverOdbc.fOdbcApi do
  try
    raise EDbxInternalError.Create
      ('getBytes - Unimplemented method invoked on metadata cursor');
  except
    on E: EDbxError do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.getBytes', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.getBytes'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.getColumnCount;//(var pColumns: Word): SQLResult;
begin
  pColumns := fColumnCount;
  Result := DBXERR_NONE;
end;

function TSqlCursorMetaData.getColumnLength;//(ColumnNumber: Word;
//  var pLength: Longword): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.getColumnLength', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  //with fSqlDriverOdbc.fOdbcApi do
  try
    raise EDbxInternalError.Create
      ('getColumnLength - Unimplemented method invoked on metadata cursor');
  except
    on E: Exception{EDbxError} do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.getColumnLength', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.getColumnLength'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.getColumnName;//(ColumnNumber: Word;
//  pColumnName: PAnsiChar): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.getColumnName', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  StrCopy(pColumnName, PAnsiChar(fColumnNames[ColumnNumber - 1]));
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.getColumnName', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.getColumnName'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.getColumnNameLength;//(ColumnNumber: Word;
//  var pLen: Word): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.getColumnNameLength', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  pLen := Length(fColumnNames[ColumnNumber - 1]);
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.getColumnNameLength', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.getColumnNameLength'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.getColumnPrecision;//(ColumnNumber: Word;
//  var piPrecision: Smallint): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.getColumnPrecision', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    raise EDbxInternalError.Create
      ('getColumnPrecision - Unimplemented method invoked on metadata cursor');
  except
    on E: Exception{EDbxError} do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.getColumnPrecision', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.getColumnPrecision'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.getColumnScale;//(ColumnNumber: Word;
//  var piScale: Smallint): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.getColumnScale', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  piScale := 0;
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.getColumnScale', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.getColumnScale'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.getColumnType;//(ColumnNumber: Word;
//  var puType, puSubType: Word): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.getColumnType', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  puType := fColumnTypes[ColumnNumber - 1];
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.getColumnType', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.getColumnType'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.getDate;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.getDate', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    raise EDbxInternalError.Create
      ('getDate - Unimplemented method invoked on metadata cursor');
  except
    on E: Exception{EDbxError} do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.getDate', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.getDate'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.getDouble;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.getDouble', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    raise EDbxInternalError.Create
      ('getDouble - Unimplemented method invoked on metadata cursor');
  except
    on E: Exception{EDbxError} do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.getDouble', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.getDouble'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.getErrorMessage;//(Error: PAnsiChar): SQLResult;
begin
  if Error=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  StrCopy(Error, PAnsiChar(AnsiString(fSqlCursorErrorMsg.Text)));
  fSqlCursorErrorMsg.Clear;
  Result := DBXERR_NONE;
end;

function TSqlCursorMetaData.getErrorMessageLen;//(out ErrorLen: Smallint): SQLResult;
begin
  ErrorLen := Length(fSqlCursorErrorMsg.Text);
  Result := DBXERR_NONE;
end;

function TSqlCursorMetaData.getLong;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.getLong', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    raise EDbxInternalError.Create
      ('getLong - Unimplemented method invoked on metadata cursor');
  except
    on E: Exception{EDbxError} do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.getLong', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.getLong'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.GetOption;//(eOption: TSQLCursorOption;
//  PropValue: Pointer; MaxLength: Smallint; out Length: Smallint): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.GetOption', ['eOption =', cSQLCursorOption[eOption]]); {$ENDIF _TRACE_CALLS_}
  try
    raise EDbxInternalError.Create
      ('GetOption - Unimplemented method invoked on metadata cursor');
  except
    on E: Exception{EDbxError} do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.GetOption', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.GetOption'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.getShort;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.getShort', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    raise EDbxInternalError.Create
      ('getShort - Unimplemented method invoked on metadata cursor');
  except
    on E: Exception{EDbxError} do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.getShort', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.getShort'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.getString;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.getString', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    raise EDbxInternalError.Create
      ('getString - Unimplemented method invoked on metadata cursor');
  except
    on E: Exception{EDbxError} do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.getString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.getString'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.getTime;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.getTime', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    raise EDbxInternalError.Create
      ('getTime - Unimplemented method invoked on metadata cursor');
  except
    on E: Exception{EDbxError} do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.getTime', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.getTime'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.getTimeStamp;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.getTimeStamp', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    raise EDbxInternalError.Create
      ('getTimeStamp - Unimplemented method invoked on metadata cursor');
  except
    on E: Exception{EDbxError} do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.getTimeStamp', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.getTimeStamp'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.isAutoIncrement;//(ColumnNumber: Word;
//  var AutoIncr: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.isAutoIncrement', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    AutoIncr := False; //TGUL work arround
    Result := DBXERR_NONE;
    //raise EDbxInternalError.Create
    //  ('isAutoIncrement - Unimplemented method invoked on metadata cursor');
  except
    on E: Exception{EDbxError} do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.isAutoIncrement', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.isAutoIncrement'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.isBlobSizeExact;//(ColumnNumber: Word;
//  var IsExact: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.isBlobSizeExact', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    IsExact := False; //TGUL work arround
    Result := DBXERR_NONE;
    //raise EDbxInternalError.Create
    //  ('isBlobSizeExact - Unimplemented method invoked on metadata cursor');
  except
    on E: EDbxError do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.isBlobSizeExact', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.isBlobSizeExact'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaData.isNullable;//(ColumnNumber: Word;
//  var Nullable: LongBool): SQLResult;
begin
  Nullable := False;
  Result := DBXERR_NONE;
end;

function TSqlCursorMetaData.isReadOnly;//(ColumnNumber: Word;
//  var ReadOnly: LongBool): SQLResult;
begin
  ReadOnly := True; // Cannot update metadata directly
  Result := DBXERR_NONE;
end;

function TSqlCursorMetaData.isSearchable;//(ColumnNumber: Word;
//  var Searchable: LongBool): SQLResult;
//
{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
// From DbExpress help:
// "isSearchable indicates whether a specified column represents
// a field that can appear in the WHERE clause of an SQL query."
//
// But with metadata, you do not use a WHERE clause.
// So this is completely inappropriate for metadata
//
// Previously raised an error here
// Now, following suggestion from Dmitry Arefiev, just indicate not searchable
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
//
begin
  Searchable := False;
  Result := DBXERR_NONE;
end;

function TSqlCursorMetaData.next: SQLResult;
begin
  Inc(fRowNo);
  Result := DBXERR_NONE;
end;

function TSqlCursorMetaData.SetOption;//(eOption: TSQLCursorOption;
//  PropValue: Integer): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaData.SetOption', ['eOption =', cSQLCursorOption[eOption], 'PropValue=', PropValue]); {$ENDIF _TRACE_CALLS_}
  try
    raise EDbxInternalError.Create
      ('SetOption - Unimplemented method invoked on metadata cursor');
  except
    on E: Exception{EDbxError} do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaData.SetOption', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaData.SetOption'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCursorMetaData.OdbcCheck;//(OdbcCode: SqlReturn; const OdbcFunctionName: AnsiString);
var
  vOdbcErrorLines: TStringList;
begin
  case OdbcCode of
    OdbcApi.SQL_SUCCESS:
      exit;
    OdbcApi.SQL_NO_DATA:
      begin
        fSqlCursorErrorMsg.Clear;
        fSqlCursorErrorMsg.Add('Unexpected end of data returned from ODBC function: ' +
          string(OdbcFunctionName));
        raise EDbxODBCError.Create(fSqlCursorErrorMsg.Text);
      end;
  else
    begin
      vOdbcErrorLines := fSqlDriverOdbc.fOdbcErrorLines;
      fSqlDriverOdbc.fOdbcErrorLines := fSqlCursorErrorMsg;
      try
        fSqlDriverOdbc.OdbcCheck(OdbcCode, OdbcFunctionName, SQL_HANDLE_STMT, fHStmt, nil);
      finally
        fSqlDriverOdbc.fOdbcErrorLines := vOdbcErrorLines;
      end;
    end;
  end;
end;

{ TOdbcBindCol }

constructor TOdbcBindCol.Create;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TOdbcBindCol.Create'); {$ENDIF _TRACE_CALLS_}
  inherited;
  fReadOnly := -1;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TOdbcBindCol.Create', e);  raise; end; end;
    finally LogExitProc('TOdbcBindCol.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TOdbcBindCol.Destroy;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TOdbcBindCol.Destroy'); {$ENDIF _TRACE_CALLS_}
  FreeMemAndNil(fColName);
  if fIsBuffer and (fOdbcHostVarAddress.Ptr<>nil) then
  begin
    FreeMemAndNil(fOdbcHostVarAddress.Ptr);
    fOdbcHostVarChunkSize := 0;
  end;
  FreeAndNil(fBlobChunkCollection);
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TOdbcBindCol.Destroy', e);  raise; end; end;
    finally LogExitProc('TOdbcBindCol.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TOdbcBindParam }

constructor TOdbcBindParam.Create;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TOdbcBindParam.Create'); {$ENDIF _TRACE_CALLS_}
  inherited;
  fOdbcParamSqlType := SQL_UNKNOWN_TYPE;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TOdbcBindParam.Create', e);  raise; end; end;
    finally LogExitProc('TOdbcBindParam.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TOdbcBindParam.Destroy;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TOdbcBindParam.Destroy'); {$ENDIF _TRACE_CALLS_}
  FreeMemAndNil(fBuffer);
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TOdbcBindParam.Destroy', e);  raise; end; end;
    finally LogExitProc('TOdbcBindParam.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TMetaTable }

constructor TMetaTable.Create;//(
//  SqlConnectionOdbc: TSqlConnectionOdbc;
//  Cat: PAnsiChar;
//  Schema: PAnsiChar;
//  TableName: PAnsiChar;
//  TableType: Integer);
var
  aCatLen: Integer;
  aSchemaLen: Integer;
  WantCatalog: Boolean;
  WantSchema: Boolean;
  vCatalogName, vSchemaName, vObjectName: AnsiString;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TMetaTable.Create', ['Cat=', Cat, 'Schema =', Schema, 'TableName =', TableName, 'TableType =', TableType]); {$ENDIF _TRACE_CALLS_}
  aCatLen := 0;

  if (Cat <> nil) then
  begin
    if (Cat^ <> '?') then
      aCatLen := StrClone(Cat, fCat)
    else
      Cat := nil;
  end;
  aSchemaLen := StrClone(Schema, fSchema);
  StrClone(TableName, fTableName);

  WantCatalog := True;
  WantSchema := True;

  {
  if (TableType = eSQLSynonym) then
  begin
    WantCatalog := False;
    WantSchema := False;
  end;
  //}

  if (aCatLen = 0) or (not SqlConnectionOdbc.fSupportsSchemaDML) then
    WantCatalog := False
  else
  if (SqlConnectionOdbc.fCurrentCatalog = '')
    or (StrCompNil(PAnsiChar(SqlConnectionOdbc.fCurrentCatalog), Cat) = 0)
  then
    WantCatalog := False;

  if (aSchemaLen = 0) or (not SqlConnectionOdbc.fSupportsSchemaDML) then
    WantSchema := False;

  //INFORMIX: tablename without owner
  //{
  if SqlConnectionOdbc.fDbmsType = eDbmsTypeInformix then
  begin // INFORMIX supports operation with the catalog, but usage of this
    WantCatalog := False; // option is inconvenient for the developers and there is no large
    WantSchema := False;  // sense  by work with INFORMIX. If you want to work with the catalog,
  end;                    // comment out this block.
  // }

  if WantCatalog and Assigned(Cat) then
    vCatalogName := StrPas(Cat)
  else
    SetLength(vCatalogName, 0);

  if WantSchema and Assigned(Schema) then
    vSchemaName := StrPas(Schema)
  else
    SetLength(vSchemaName, 0);

  if Assigned(TableName) then
    vObjectName := StrPas(TableName)
  else
    SetLength(vObjectName, 0);

  // The calculation of a full qualified name:
  vObjectName := SqlConnectionOdbc.EncodeObjectFullName(
    vCatalogName, vSchemaName, vObjectName);

  if Length(vObjectName) > 0 then
    StrClone(vObjectName, fQualifiedTableName)
  else // The conversion was not successful:
    fQualifiedTableName := nil;

  fTableType := TableType;

  StrClone(fCat, fWCat);
  StrClone(fSchema, fWSchema);
  StrClone(fTableName, fWTableName);

  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TMetaTable.Create', e);  raise; end; end;
    finally LogExitProc('TMetaTable.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

constructor TMetaTable.CreateW;//(
//  SqlConnectionOdbc: TSqlConnectionOdbc;
//  Cat: PWideChar;
//  Schema: PWideChar;
//  TableName: PWideChar;
//  TableType: Integer);
var
  aCatLen: Integer;
  aSchemaLen: Integer;
  WantCatalog: Boolean;
  WantSchema: Boolean;
  vCatalogName, vSchemaName, vObjectName: AnsiString;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TMetaTable.CreateW', ['Cat=', Cat, 'Schema =', Schema, 'TableName =', TableName, 'TableType =', TableType]); {$ENDIF _TRACE_CALLS_}
  aCatLen := 0;
  if (Cat <> nil) then
  begin
    if (Cat^ <> '?') then
      aCatLen := StrClone(Cat, fWCat)
    else
      Cat := nil;
  end;
  aSchemaLen := StrClone(Schema, fWSchema);
  StrClone(TableName, fWTableName);

  WantCatalog := True;
  WantSchema := True;

  {
  if (TableType = eSQLSynonym) then
  begin
    WantCatalog := False;
    WantSchema := False;
  end;
  //}

  if (aCatLen = 0) or (not SqlConnectionOdbc.fSupportsSchemaDML) then
    WantCatalog := False
  else
  if (SqlConnectionOdbc.fCurrentCatalog = '')
    or (StrCompNil(PAnsiChar(SqlConnectionOdbc.fCurrentCatalog), PAnsiChar(AnsiString(WideString(Cat)))) = 0)
  then
    WantCatalog := False;

  if (aSchemaLen = 0) or (not SqlConnectionOdbc.fSupportsSchemaDML) then
    WantSchema := False;

  //INFORMIX: tablename without owner
  //{
  if SqlConnectionOdbc.fDbmsType = eDbmsTypeInformix then
  begin // INFORMIX supports operation with the catalog, but usage of this
    WantCatalog := False; // option is inconvenient for the developers and there is no large
    WantSchema := False;  // sense  by work with INFORMIX. If you want to work with the catalog,
  end;                    // comment out this block.
  // }

  if WantCatalog and Assigned(Cat) then
    vCatalogName := AnsiString(WideString(Cat))
  else
    SetLength(vCatalogName, 0);

  if WantSchema and Assigned(Schema) then
    vSchemaName := AnsiString(WideString(Schema))
  else
    SetLength(vSchemaName, 0);

  if Assigned(TableName) then
    vObjectName := AnsiString(WideString(TableName))
  else
    SetLength(vObjectName, 0);

  // The calculation of a full qualified name:
  vObjectName := SqlConnectionOdbc.EncodeObjectFullName(
    vCatalogName, vSchemaName, vObjectName);

  if Length(vObjectName) > 0 then
    StrClone(vObjectName, fQualifiedTableName)
  else // The conversion was not successful:
    fQualifiedTableName := nil;

  fTableType := TableType;

  StrClone(fWCat, fCat);
  StrClone(fWSchema, fSchema);
  StrClone(fWTableName, fTableName);

  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TMetaTable.CreateW', e);  raise; end; end;
    finally LogExitProc('TMetaTable.CreateW'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TMetaTable.Destroy;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TMetaTable.Destroy'); {$ENDIF _TRACE_CALLS_}
  FreeMemAndNil(fCat);
  FreeMemAndNil(fSchema);
  FreeMemAndNil(fTableName);

  FreeMemAndNil(fWCat);
  FreeMemAndNil(fWSchema);
  FreeMemAndNil(fWTableName);

  FreeMemAndNil(fQualifiedTableName);

  FreeAndNil(fIndexColumnList);
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TMetaTable.Destroy', e);  raise; end; end;
    finally LogExitProc('TMetaTable.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TMetaColumn }

constructor TMetaColumn.Create;//(
//  ColumnName: PAnsiChar;
//  OrdinalPosition: Smallint;
//  TypeName: PAnsiChar);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TMetaColumn.Create', ['ColumnName =', ColumnName, 'OrdinalPosition =', OrdinalPosition, 'TypeName =', TypeName]); {$ENDIF _TRACE_CALLS_}
  StrClone(ColumnName, fColumnName);
  StrClone(TypeName, fTypeName);

  fOrdinalPosition := OrdinalPosition;

  StrClone(fColumnName, fWColumnName);
  StrClone(fTypeName, fWTypeName);

  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TMetaColumn.Create', e);  raise; end; end;
    finally LogExitProc('TMetaColumn.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

constructor TMetaColumn.CreateW;//(
//  ColumnName: PWideChar;
//  OrdinalPosition: Smallint;
//  TypeName: PWideChar);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TMetaColumn.CreateW', ['ColumnName =', ColumnName, 'OrdinalPosition =', OrdinalPosition, 'TypeName =', TypeName]); {$ENDIF _TRACE_CALLS_}
  StrClone(ColumnName, fWColumnName);
  StrClone(TypeName, fWTypeName);

  fOrdinalPosition := OrdinalPosition;

  StrClone(fWColumnName, fColumnName);
  StrClone(fWTypeName, fTypeName);

  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TMetaColumn.CreateW', e);  raise; end; end;
    finally LogExitProc('TMetaColumn.CreateW'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TMetaColumn.Destroy;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TMetaColumn.Destroy'); {$ENDIF _TRACE_CALLS_}
  FreeMemAndNil(fColumnName);
  FreeMemAndNil(fTypeName);

  FreeMemAndNil(fWColumnName);
  FreeMemAndNil(fWTypeName);
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TMetaColumn.Destroy', e);  raise; end; end;
    finally LogExitProc('TMetaColumn.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TMetaIndexColumn }

constructor TMetaIndexColumn.Create;//(MetaTable: TMetaTable;
//  CatName, SchemaName, TableName, IndexName, IndexColumnName: PAnsiChar);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TMetaIndexColumn.Create', ['MetaTable =', MetaTable, 'IndexName =', IndexName, 'IndexColumnName =', IndexColumnName]); {$ENDIF _TRACE_CALLS_}
  fMetaTable := MetaTable;

  StrClone(CatName, fCatName);
  StrClone(SchemaName, fSchemaName);
  StrClone(TableName, fTableName);
  StrClone(IndexName, fIndexName);
  StrClone(IndexColumnName, fIndexColumnName);

  StrClone(fCatName, fWCatName);
  StrClone(fSchemaName, fWSchemaName);
  StrClone(fTableName, fWTableName);
  StrClone(fIndexName, fWIndexName);
  StrClone(fIndexColumnName, fWIndexColumnName);

  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TMetaIndexColumn.Create', e);  raise; end; end;
    finally LogExitProc('TMetaIndexColumn.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

constructor TMetaIndexColumn.CreateW;//(MetaTable: TMetaTable;
//  CatName, SchemaName, TableName, IndexName, IndexColumnName: PWideChar);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TMetaIndexColumn.CreateW', ['MetaTable =', MetaTable, 'IndexName =', IndexName, 'IndexColumnName =', IndexColumnName]); {$ENDIF _TRACE_CALLS_}
  fMetaTable := MetaTable;

  StrClone(CatName, fWCatName);
  StrClone(SchemaName, fWSchemaName);
  StrClone(TableName, fWTableName);
  StrClone(IndexName, fWIndexName);
  StrClone(IndexColumnName, fWIndexColumnName);

  StrClone(fWCatName, fCatName);
  StrClone(fWSchemaName, fSchemaName);
  StrClone(fWTableName, fTableName);
  StrClone(fWIndexName, fIndexName);
  StrClone(fWIndexColumnName, fIndexColumnName);

  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TMetaIndexColumn.CreateW', e);  raise; end; end;
    finally LogExitProc('TMetaIndexColumn.CreateW'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TMetaIndexColumn.Destroy;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TMetaIndexColumn.Destroy'); {$ENDIF _TRACE_CALLS_}
  FreeMemAndNil(fCatName);
  FreeMemAndNil(fSchemaName);
  FreeMemAndNil(fTableName);
  FreeMemAndNil(fIndexName);
  FreeMemAndNil(fIndexColumnName);
  FreeMemAndNil(fFilter);

  FreeMemAndNil(fWCatName);
  FreeMemAndNil(fWSchemaName);
  FreeMemAndNil(fWTableName);
  FreeMemAndNil(fWIndexName);
  FreeMemAndNil(fWIndexColumnName);
  FreeMemAndNil(fWFilter);
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TMetaIndexColumn.Destroy', e);  raise; end; end;
    finally LogExitProc('TMetaIndexColumn.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TMetaProcedure }

constructor TMetaProcedure.Create;//(Cat, Schema, ProcName: PAnsiChar; ProcType: Integer);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TMetaProcedure.Create', ['Cat =', Cat, 'Schema =', Schema, 'ProcName =', ProcName, 'ProcType =', ProcType]); {$ENDIF _TRACE_CALLS_}
  StrClone(Cat, fCat);
  {
  if fCat = nil then
  begin
    fCat := AllocMem(1);
    fCat^ := cNullAnsiChar;
  end;
  {}
  StrClone(Schema, fSchema);
  StrClone(ProcName, fProcName);

  fProcType := ProcType;

  StrClone(fCat, fWCat);
  StrClone(fSchema, fWSchema);
  StrClone(fProcName, fWProcName);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TMetaProcedure.Create', e);  raise; end; end;
    finally LogExitProc('TMetaProcedure.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

constructor TMetaProcedure.CreateW;//(Cat, Schema, ProcName: PWideChar; ProcType: Integer);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TMetaProcedure.CreateW', ['Cat =', Cat, 'Schema =', Schema, 'ProcName =', ProcName, 'ProcType =', ProcType]); {$ENDIF _TRACE_CALLS_}
  StrClone(Cat, fWCat);
  {
  if fWCat = nil then
  begin
    fWCat := AllocMem(2);
    fWCat^ := cNullWideChar;
  end;
  {}
  StrClone(Schema, fWSchema);
  StrClone(ProcName, fWProcName);

  fProcType := ProcType;

  StrClone(fWCat, fCat);
  StrClone(fWSchema, fSchema);
  StrClone(fWProcName, fProcName);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TMetaProcedure.CreateW', e);  raise; end; end;
    finally LogExitProc('TMetaProcedure.CreateW'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TMetaProcedure.Destroy;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TMetaProcedure.Destroy'); {$ENDIF _TRACE_CALLS_}
  FreeMemAndNil(fCat);
  FreeMemAndNil(fSchema);
  FreeMemAndNil(fProcName);

  FreeMemAndNil(fWCat);
  FreeMemAndNil(fWSchema);
  FreeMemAndNil(fWProcName);
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TMetaProcedure.Destroy', e);  raise; end; end;
    finally LogExitProc('TMetaProcedure.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TMetaProcedureParam }

constructor TMetaProcedureParam.Create;//(ParamName: PAnsiChar);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TMetaProcedureParam.Create', ['ParamName =', ParamName]); {$ENDIF _TRACE_CALLS_}
  fParamType := paramUNKNOWN;
  StrClone(ParamName, fParamName);
  StrClone(fParamName, fWParamName);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TMetaProcedureParam.Create', e);  raise; end; end;
    finally LogExitProc('TMetaProcedureParam.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

constructor TMetaProcedureParam.CreateW;//(ParamName: PWideChar);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TMetaProcedureParam.CreateW', ['ParamName =', ParamName]); {$ENDIF _TRACE_CALLS_}
  fParamType := paramUNKNOWN;
  StrClone(ParamName, fWParamName);
  StrClone(fWParamName, fParamName);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TMetaProcedureParam.CreateW', e);  raise; end; end;
    finally LogExitProc('TMetaProcedureParam.CreateW'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TMetaProcedureParam.Destroy;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TMetaProcedureParam.Destroy'); {$ENDIF _TRACE_CALLS_}
  FreeMemAndNil(fParamName);
  FreeMemAndNil(fDataTypeName);

  FreeMemAndNil(fWParamName);
  FreeMemAndNil(fWDataTypeName);
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TMetaProcedureParam.Destroy', e);  raise; end; end;
    finally LogExitProc('TMetaProcedureParam.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSqlCursorTables }

{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
{
 Dbx returned cursor columns
  1. RECNO         fldINT32
       A record number that uniquely identifies each record.
  2. CATALOG_NAME  fldZSTRING
       The name of the catalog (database) that contains the table.
  3. SCHEMA_NAME   fldZSTRING
       The name of the schema that identifies the owner of the table.
  4. TABLE_NAME    fldZSTRING
       The name of the table.
  5. TABLE_TYPE    fldINT32
       An eSQLTableType value (C++) or table type constant (Object Pascal)
       that indicates the type of table.

 ODBC Result set columns
  1. TABLE_CAT     Varchar
       Catalog name; NULL if not applicable to the data source
  2. TABLE_SCHEM   Varchar
       Schema name; NULL if not applicable to the data source.
  3. TABLE_NAME    Varchar
       Table name
  4. TABLE_TYPE    Varchar
       Table type name eg TABLE, VIEW, SYNONYM, ALIAS etc
  5. REMARKS       Varchar
       A description of the table
}
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}

const
  TableColumnNames: array[1..5] of AnsiString = { Do not localize }
  ('RECNO', 'CATALOG_NAME', 'SCHEMA_NAME', 'TABLE_NAME', 'TABLE_TYPE');
  TableColumnTypes: array[1..5] of Word =
  (fldINT32, fldZSTRING, fldZSTRING, fldZSTRING, fldINT32);
  TableColumnTypesW: array[1..5] of Word =
  (fldINT32, fldWIDESTRING, fldWIDESTRING, fldWIDESTRING, fldINT32);
  TableColumnCount = Length(TableColumnNames);

constructor TSqlCursorMetaDataTables.Create;//(ASupportWideString: Boolean; OwnerMetaData: TSQLMetaDataOdbc);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataTables.Create'); {$ENDIF _TRACE_CALLS_}
  inherited Create(ASupportWideString, OwnerMetaData);
  fColumnCount := TableColumnCount;
  fColumnNames := @TableColumnNames;
  if fSupportWideString then
    fColumnTypes := @TableColumnTypesW
  else
    fColumnTypes := @TableColumnTypes;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataTables.Create', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataTables.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TSqlCursorMetaDataTables.Destroy;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataTables.Destroy'); {$ENDIF _TRACE_CALLS_}
  Clear;
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataTables.Destroy', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataTables.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCursorMetaDataTables.Clear;
var
  i: Integer;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataTables.Clear'); {$ENDIF _TRACE_CALLS_}
  if Assigned(fTableList) then
  begin
    for i := fTableList.Count - 1 downto 0 do
      TMetaTable(fTableList[i]).Free;
    FreeAndNil(fTableList);
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataTables.Clear', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataTables.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCursorMetaDataTables.FetchTables;//(
//  SearchTableName: PAnsiChar;
//  SearchTableType: Longword
//  bUnicode: Boolean);
var
  OdbcRetcode: OdbcApi.SqlReturn;
  TableTypes: AnsiString;
  wsTableTypes: WideString;
  sTableTypes: PAnsiChar;
  wTableTypes: PWideChar absolute sTableTypes;

  bUnicodeApi: Boolean;

  Cat: PAnsiChar;
  Schema: PAnsiChar;
  TableName: PAnsiChar;
  OdbcTableType: PAnsiChar;
  sTableName: AnsiString;

  wCat: PWideChar absolute Cat;
  wSchema: PWideChar absolute Schema;
  wTableName: PWideChar absolute TableName;
  wOdbcTableType: PWideChar absolute OdbcTableType;

  //wsCat: WideString;
  wsSchema: WideString;
  wsTableName: WideString;

  DbxTableType: Integer;

  cbCat: SqlInteger;
  cbSchema: SqlInteger;
  cbTableName: SqlInteger;
  cbOdbcTableType: SqlInteger;

  aMetaTable: TMetaTable;
  i: Integer;
  aDbxConStmtInfo: TDbxConStmtInfo;
  OLDCurrentDbxConStmt: PDbxConStmt;
  aHConStmt: SqlHDbc;

  {$IFDEF MSWINDOWS}
  //
  // msjet odbc not return correctry unicode table name (SQLTablesW for DBF)
  //
  S: AnsiString;
  b_odbc_read_table_list_w_from_folder: Boolean;

  function fix_odbc_read_table_list_w_from_folder(const s_filter_ext: WideString): Boolean;

    function DirectoryExistsW(const Dir: WideString): Boolean;
    var
      Code: Cardinal;
    begin
      if Dir <> '' then
      begin
        Code := GetFileAttributesW(PWideChar(Dir));
        Result := (Code <> $FFFFFFFF) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
      end
      else
        Result := False;
    end;

  var
    wsCatalog: WideString;
    FindFileData: TWIN32FindDataW;
    hFindFile: THandle;
  const
    FILE_ATTRIBUTE_DEVICE = $00000040;
    FILE_ATTRIBUTE_SPARSE_FILE = $00000200;
  begin
    Result := False;
    OdbcRetcode := ODBCapi.SQL_NO_DATA;
    try
      wsCatalog := WideString(fSqlConnectionOdbc.fDbxCatalog);
      if (wsCatalog = '') then
        Exit;
      if wsCatalog[Length(wsCatalog)] <> '\' then
        wsCatalog := wsCatalog + '\';
      if not DirectoryExistsW(wsCatalog) then
        Exit;
      wsCatalog := wsCatalog + '*.' + s_filter_ext;
      hFindFile := FindFirstFileW(PWideChar(wsCatalog), FindFileData);
      if hFindFile <> INVALID_HANDLE_VALUE then
      try
        repeat
          if ((FindFileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0)
            and ((FindFileData.dwFileAttributes and
              (FILE_ATTRIBUTE_TEMPORARY or FILE_ATTRIBUTE_DEVICE or FILE_ATTRIBUTE_SPARSE_FILE)) = 0) then
          begin
            wsTableName := DbxOpenOdbcFuncs.ChangeFileExtW(WideString(FindFileData.cFileName), '');

            aMetaTable := TMetaTable.CreateW(fSqlConnectionOdbc, wCat, wSchema, PWideChar(wsTableName), eSQLTable);
            if Assigned(aMetaTable.fQualifiedTableName) then // If the conversion was successful:
              fTableList.Add(aMetaTable)
            else
              aMetaTable.Free;
          end;
        until not FindNextFileW(hFindFile, FindFileData);
        Result := True;
      finally
        FindClose(hFindFile);
      end;
    except
      { empty }
    end;
  end;
  {$ENDIF}

begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataTables.FetchTables', ['SearchTableName =', StrPtrToString(SearchTableName, bUnicode), 'SearchTableType =', SearchTableType, 'Unicode =', bUnicode]); {$ENDIF _TRACE_CALLS_}
  Clear;
  Cat := nil;
  Schema := nil;
  TableName := nil;
  OdbcTableType := nil;
  fHStmt := SQL_NULL_HANDLE;
  OLDCurrentDbxConStmt := nil;

  with fSqlDriverOdbc.fOdbcApi do
  try

    bUnicodeApi := fSupportWideString and fSqlDriverOdbc.fIsUnicodeOdbcApi and Assigned(SQLDescribeColW) and Assigned(SQLTablesW);

    aDbxConStmtInfo.fDbxConStmt := nil;
    aDbxConStmtInfo.fDbxHStmtNode := nil;
    if fSqlConnectionOdbc.fStatementPerConnection > 0 then
    begin
      OLDCurrentDbxConStmt := fSqlConnectionOdbc.GetCurrentDbxConStmt();
      if fSqlConnectionOdbc.fCurrDbxConStmt = nil then
        OLDCurrentDbxConStmt := nil;
      //fSqlConnectionOdbc.fCurrDbxConStmt := aDbxConStmtInfo.fDbxConStmt;
    end;
    fSqlConnectionOdbc.AllocHStmt(fHStmt, @aDbxConStmtInfo, {bMetadataRead=}True);

    TableTypes := '';

    if (SearchTableType and eSQLTable) <> 0 then
      TableTypes := 'TABLE, ';
    if (SearchTableType and eSQLView) <> 0 then
      TableTypes := TableTypes + 'VIEW, ';
    if (SearchTableType and eSQLSystemTable) <> 0 then
      TableTypes := TableTypes + 'SYSTEM TABLE, ';
    if (SearchTableType and eSQLSynonym) <> 0 then
      TableTypes := TableTypes + 'SYNONYM, ';
    if (SearchTableType and eSQLTempTable) <> 0 then
      TableTypes := TableTypes + 'GLOBAL TEMPORARY, ';
    if (SearchTableType and eSQLLocal) <> 0 then
      TableTypes := TableTypes + 'LOCAL TEMPORARY, ';

    if fSqlConnectionOdbc.fConnectionOptions[coTLSTO] = osOn then
    begin
      if (SearchTableType = 0) or ((SearchTableType and eSQLTable) <> 0) then
        TableTypes := '';
    end;

    if TableTypes = '' then
      sTableTypes := nil
    else
    begin
      TableTypes := Copy(TableTypes, 1, Length(TableTypes) - 2); // remove trailing comma
      sTableTypes := PAnsiChar(TableTypes);
    end;

    {$IFDEF _DBXCB_}
    if Assigned(fSqlConnectionOdbc.fDbxTraceCallbackEven) then
      fSqlConnectionOdbc.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Misc, 'metadata (tables): ("%s", "%s"); u: %d',
        [StrPtrToString(SearchTableName, bUnicode), TableTypes, Integer(bUnicodeApi)]);
    {$ENDIF}
    {$IFDEF _TRACE_CALLS_}
      LogInfoProc(['metadata (tables): SearchTableName =', StrPtrToString(SearchTableName, bUnicode),
        'TableTypes =', TableTypes, 'UnicodeApi =', bUnicodeApi]);
    {$ENDIF}

    if fSqlConnectionOdbc.fStatementPerConnection = 0 then
      aHConStmt :=fSqlConnectionOdbc.fhCon
    else
      aHConStmt := aDbxConStmtInfo.fDbxConStmt.fHCon;

    fSqlConnectionOdbc.GetCurrentCatalog(aHConStmt);

    {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
    {+2.01 Metadata CurrentSchema Filter}
    // Vadim V.Lopushansky: Set Metadata CurrentSchema Filter
    // Edward> ???Ed>Vad: ODBC V3 certainly has the capability to support this,
    // Edward> but I don't think any DbExpress application would ever want it.
    // Edward> ???Ed>All:
    // Edward> This is much more tricky than it looks at first.
    // Edward> ODBC V2 and V3 specifications differ in their behavior here,
    // Edward> and different databases also behave differently.
    // Edward> Also, there is a particular problem if the real Schema name might
    // Edward> contain underscore character, which just happens to be the ODBC
    // Edward> wildcard character. In this case you should use an escape character,
    // Edward> but dbexpress cannot easily handle this,
    // Edward> The consistent handling of other metadata objects also needs to
    // Edward> be considered, and this requires investigation and careful thought.
    // Edward> As far as I remember, dbExpress "specificiation" (ha ha) is
    // Edward> inconsistent/unclear between the various metadata querying interfaces,
    // Edward> and it is not easily compatible with the ODBC specification (for
    // Edward> example, ODBC specification allows the catalog to be specified, but
    // Edward> dbexpress does not.
    // Edward> Really this is getting too complicated, and my feeling it is best
    // Edward> just to leave it out. But I have kept Vadim's code for now.
    {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
    if (fSqlConnectionOdbc.fConnectionOptions[coSupportsSchemaFilter] = osOn)
      and ((SearchTableType and eSQLSystemTable) = 0)
      and (fSqlConnectionOdbc.fCurrentSchema <> '')
    then
    begin
      if not bUnicodeApi then
        Schema := PAnsiChar(fSqlConnectionOdbc.fCurrentSchema)
      else
      begin
        wsSchema := fSqlConnectionOdbc.fCurrentSchema;
        wSchema := PWideChar(wsSchema);
      end;
    end;

    if StrPtrToString(SearchTableName, bUnicode) = '' then
      SearchTableName := nil;
    if (SearchTableName <> nil) then
    begin
      if bUnicode then
      begin
        if (PWideChar(SearchTableName)^ <> cNullWideChar) then
        begin
          if not bUnicodeApi then
          begin
            sTableName := StrPtrToString(SearchTableName, bUnicode);
            TableName := PAnsiChar(sTableName);
          end
          else
          begin
            wTableName := PWideChar(SearchTableName);
          end;
        end;
      end
      else if (SearchTableName^ <> cNullAnsiChar) then
      begin
        if not bUnicodeApi then
          TableName := SearchTableName
        else
        begin
          wsTableName := WideString(StrPas(SearchTableName));
          wTableName := PWideChar(wsTableName);
        end;
      end;
    end;
    if not bUnicodeApi then
    begin
      OdbcRetcode := SQLTables(fHStmt,
        Cat, SQL_NTS,
        Schema, SQL_NTS,
        TableName, SQL_NTS,
        sTableTypes, SQL_NTS // Table types
      );
      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        OdbcCheck(OdbcRetcode, 'SQLTables');
    end
    else
    begin
      if sTableTypes <> nil then
      begin
        wsTableTypes := TableTypes;
        wTableTypes := PWideChar(wsTableTypes);
      end;
      OdbcRetcode := SQLTablesW(fHStmt,
        Cat, SQL_NTS,
        Schema, SQL_NTS,
        TableName, SQL_NTS,
        sTableTypes, SQL_NTS // Table types
      );
      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        OdbcCheck(OdbcRetcode, 'SQLTablesW');
    end;

    Cat := nil;
    Schema := nil;
    TableName := nil;
    OdbcTableType := nil;

    if not bUnicodeApi then
    begin
      if fSqlConnectionOdbc.fSupportsCatalog then
        DescribeAllocBindString(1, Cat, cbCat, True);
      DescribeAllocBindString(2, Schema, cbSchema, {IgnoreError=}True{ERROR FOR INFORMIX DIRECT ODBC});
      DescribeAllocBindString(3, TableName, cbTableName);
      DescribeAllocBindString(4, OdbcTableType, cbOdbcTableType);
    end
    else
    begin
      if fSqlConnectionOdbc.fSupportsCatalog then
        DescribeAllocBindWString(1, wCat, cbCat, True);
      DescribeAllocBindWString(2, wSchema, cbSchema, {IgnoreError=}True{ERROR FOR INFORMIX DIRECT ODBC});
      DescribeAllocBindWString(3, wTableName, cbTableName);
      DescribeAllocBindWString(4, wOdbcTableType, cbOdbcTableType);
    end;

    fTableList := TList.Create;
    {$IFDEF MSWINDOWS}
    { fix ms odbc: remove last splash. When is last splash driver not returned table list.}
    b_odbc_read_table_list_w_from_folder := False;
    if {bUnicodeApi and} ((SearchTableType = 0) or ((SearchTableType and eSQLTable) <> 0)) then
    begin
      S := '';
      case fSqlConnectionOdbc.fOdbcDriverType of
        eOdbcDriverTypeMsJet:
        begin
          case fSqlConnectionOdbc.fDbmsType of
            //eDbmsTypeExcel:
            //  S := 'xls';
            eDbmsTypeText:
              S := 'csv';
            eDbmsTypeDBase:
              S := 'dbf';
            eDbmsTypeParadox:
              S := 'db';
          end;
        end;
      end;
      if S <> '' then
      begin
        b_odbc_read_table_list_w_from_folder := fix_odbc_read_table_list_w_from_folder(S);
      end;
    end;
    if not b_odbc_read_table_list_w_from_folder then
    {$ENDIF MSWINDOWS}
    OdbcRetcode := SQLFetch(fHStmt);

    while (OdbcRetcode <> ODBCapi.SQL_NO_DATA) do
    begin
      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        OdbcCheck(OdbcRetcode, 'SQLFetch');

      if (OdbcTableType = 'TABLE') then
        DbxTableType := eSQLTable
      else if (OdbcTableType = 'VIEW') then
        DbxTableType := eSQLView
      else if (OdbcTableType = 'SYNONYM') or
        (OdbcTableType = 'ALIAS') then
      begin
        // in IBM DB2, Alias is evivalent to Synonym
        DbxTableType := eSQLSynonym;
        // ORACLE does not support concept of the scheme for a synonym. Example:
        //   'SELECT * FROM PUBLIC.ALL_CLUSTERS'
        if fSqlConnectionOdbc.fDbmsType = eDbmsTypeOracle then
        begin
          if (Schema <> nil) then
          begin
            if not bUnicodeApi then
            begin
              if (StrLen(Schema) > 0) then
                Schema^ := cNullAnsiChar;
            end
            else
            begin
              if (WStrLen(wSchema) > 0) then
                wSchema^ := cNullWideChar;
            end;
          end;
        end;
      end
      else if (OdbcTableType = 'SYSTEM TABLE') then
        DbxTableType := eSQLSystemTable
      else if (OdbcTableType = 'GLOBAL TEMPORARY') then
        DbxTableType := eSQLTempTable
      else if (OdbcTableType = 'LOCAL TEMPORARY') then
        DbxTableType := eSQLLocal
      else
        // Database-specific table type - assume its a table
        DbxTableType := eSQLTable;

      if not bUnicodeApi then
        aMetaTable := TMetaTable.Create(fSqlConnectionOdbc, Cat, Schema, TableName, DbxTableType)
      else
        aMetaTable := TMetaTable.CreateW(fSqlConnectionOdbc, wCat, wSchema, wTableName, DbxTableType);

      if Assigned(aMetaTable.fQualifiedTableName) then // If the conversion was successful:
        fTableList.Add(aMetaTable)
      else
        aMetaTable.Free;

      OdbcRetcode := SQLFetch(fHStmt);
    end;

    fCatLenMax := 0;
    fSchemaLenMax := 0;
    fQualifiedTableLenMax := 1;

    if not bUnicodeApi then
    begin
      for i := 0 to fTableList.Count - 1 do
      begin
        aMetaTable := TMetaTable(fTableList.Items[i]);

        if Assigned(aMetaTable.fCat) then
          MaxSet(fCatLenMax, StrLen(aMetaTable.fCat));

        if Assigned(aMetaTable.fSchema) then
          MaxSet(fSchemaLenMax, StrLen(aMetaTable.fSchema));

        if Assigned(aMetaTable.fQualifiedTableName) then
          MaxSet(fQualifiedTableLenMax, StrLen(aMetaTable.fQualifiedTableName));
      end;
    end
    else
    begin
      for i := 0 to fTableList.Count - 1 do
      begin
        aMetaTable := TMetaTable(fTableList.Items[i]);

        if Assigned(aMetaTable.fWCat) then
          MaxSet(fCatLenMax, WStrLen(aMetaTable.fWCat));

        if Assigned(aMetaTable.fWSchema) then
          MaxSet(fSchemaLenMax, WStrLen(aMetaTable.fWSchema));

        if Assigned(aMetaTable.fQualifiedTableName) then
          MaxSet(fQualifiedTableLenMax, StrLen(aMetaTable.fQualifiedTableName));
      end;
    end;

  finally
    FreeMem(Cat);
    FreeMem(Schema);
    FreeMem(TableName);
    FreeMem(OdbcTableType);

    if fHStmt <> SQL_NULL_HANDLE then
    begin
      // calls freehandle & sets SQL_NULL_HANDLE
      fSqlConnectionOdbc.FreeHStmt(fHStmt, @aDbxConStmtInfo);
      if (fSqlConnectionOdbc.fStatementPerConnection > 0)
        and (fSqlConnectionOdbc.fCurrDbxConStmt = nil)
      then
        fSqlConnectionOdbc.SetCurrentDbxConStmt(OLDCurrentDbxConStmt);
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataTables.FetchTables', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataTables.FetchTables'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataTables.getColumnLength;//(ColumnNumber: Word;
//  var pLength: Longword): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataTables.getColumnLength', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    case ColumnNumber of
      1: // RECNO
        pLength := SizeOf(Integer);
      2: // CATALOG_NAME
        pLength := fCatLenMax;
      3: // SCHEMA_NAME
        pLength := fSchemaLenMax;
      4: // TABLE_NAME
        pLength := fQualifiedTableLenMax;
      5: // TABLE_TYPE
        pLength := SizeOf(Integer);
    else
      begin
        pLength := 0;
        raise EDbxInvalidCall.Create(
          'TSqlCursorMetaDataTables.getColumnLength invalid column no: '
          + IntToStr(ColumnNumber));
      end;
    end;
    {+2.01}
    // Vadim V.Lopushansky:
    // If length is equal 0 that Delphi will ignore this column.
    // It is bad, since the column describes the metadata.
    if (pLength = 0) and (TableColumnTypes[ColumnNumber] = fldZSTRING) then
      pLength := 1;
    {/+2.01}
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataTables.getColumnLength', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataTables.getColumnLength', ['pLength =', pLength]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataTables.getColumnPrecision;//(ColumnNumber: Word;
//  var piPrecision: Smallint): SQLResult;
var
  Length: Longword;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataTables.getColumnPrecision', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  Result := getColumnLength(ColumnNumber, Length);
  piPrecision := Length;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataTables.getColumnPrecision', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataTables.getColumnPrecision', ['piPrecision =', piPrecision]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataTables.getLong;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataTables.getLong', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    case ColumnNumber of
      1: // RECNO
        begin
          Integer(Value^) := fRowNo;
          IsBlank := False;
        end;
      5: // TABLE_TYPE
        begin
          Integer(Value^) := fMetaTableCurrent.fTableType;
          IsBlank := False;
        end;
    else
      begin
        Integer(Value^) := 0;
        raise EDbxInvalidCall.Create(
          'TSqlCursorMetaDataTables.getLong not valid for column '
          + IntToStr(ColumnNumber));
      end;
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataTables.getLong', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataTables.getLong', ['Value =', Integer(Value^), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataTables.getString;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_}
    if Value <> nil then PAnsiChar(Value)^ := cNullAnsiChar;
    IsBlank := True;
    Result := DBXERR_NONE;
    try try
    LogEnterProc('TSqlCursorMetaDataTables.getString', ['ColumnNumber =', ColumnNumber]);
  {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    Exit;
  end;
  try
    case ColumnNumber of
      2: // CATALOG_NAME
        begin
          if (fMetaTableCurrent.fCat = nil) then
          begin
            PAnsiChar(Value)^ := cNullAnsiChar;
            IsBlank := True;
          end
          else
          begin
            StrCopy(Value, PAnsiChar(fMetaTableCurrent.fCat));
            IsBlank := False;
          end;
        end;
      3: // SCHEMA_NAME
        begin
          if (fMetaTableCurrent.fSchema = nil) then
          begin
            PAnsiChar(Value)^ := cNullAnsiChar;
            IsBlank := True;
          end
          else
          begin
            StrCopy(Value, PAnsiChar(fMetaTableCurrent.fSchema));
            IsBlank := False;
          end;
        end;
      4: // TABLE_NAME
        begin
          StrCopy(Value, PAnsiChar(fMetaTableCurrent.fTableName));
          IsBlank := False;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataTables.getString not valid for column '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      PAnsiChar(Value)^ := cNullAnsiChar;
      IsBlank := True;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataTables.getString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataTables.getString', ['Value =', PAnsiChar(Value), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataTables.DoGetWideString;//(ColumnNumber: Word;
// Value: PWideChar; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_}
    if Value <> nil then PWideChar(Value)^ := cNullWideChar;
    IsBlank := True;
    Result := DBXERR_NONE;
    try try
    LogEnterProc('TSqlCursorMetaDataTables.DoGetWideString', ['ColumnNumber =', ColumnNumber]);
  {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    Exit;
  end;
  try
    case ColumnNumber of
      2: // CATALOG_NAME
        begin
          if (fMetaTableCurrent.fWCat = nil) then
          begin
            Value^ := cNullWideChar;
            IsBlank := True;
          end
          else
          begin
            WStrCopy(Value, fMetaTableCurrent.fWCat);
            IsBlank := False;
          end;
        end;
      3: // SCHEMA_NAME
        begin
          if (fMetaTableCurrent.fWSchema = nil) then
          begin
            Value^ := cNullWideChar;
            IsBlank := True;
          end
          else
          begin
            WStrCopy(Value, fMetaTableCurrent.fWSchema);
            IsBlank := False;
          end;
        end;
      4: // TABLE_NAME
        begin
          WStrCopy(Value, fMetaTableCurrent.fWTableName);
          IsBlank := False;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataTables.DoGetWideString not valid for column '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      Value^ := cNullWideChar;
      IsBlank := True;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataTables.DoGetWideString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataTables.DoGetWideString', ['Value =', Value, 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataTables.next: SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataTables.next'); {$ENDIF _TRACE_CALLS_}
  Inc(fRowNo);
  {+2.01}
  if (fTableList = nil) or
    (fRowNo > fTableList.Count) then
    {/+2.01}
  begin
    Result := DBXERR_EOF;
    exit;
  end;
  fMetaTableCurrent := fTableList[fRowNo - 1];
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataTables.next', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataTables.next'); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSqlCursorColumns }

{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
{
1.  RECNO            fldINT32
      A record number that uniquely identifies each record.
2.  CATALOG_NAME     fldZSTRING
      The name of the catalog (database) that contains the table.
3.  SCHEMA_NAME      fldZSTRING
      The name of the schema that identifies the owner of the table.
4.  TABLE_NAME       fldZSTRING
      The name of the table in which the column appears.
5.  COLUMN_NAME      fldZSTRING
      The name of the field (column).
6.  COLUMN_POSITION  fldINT16
      The position of the column in its table.
7.  COLUMN_TYPE      fldINT32
      An eSQLColType value (C++) or column type constant (Object Pascal)
      that indicates the type of field.
8.  COLUMN_DATATYPE  fldINT16
      The logical data type for the field.
9.  COLUMN_TYPENAME  fldZSTRING
      A string describing the datatype.
      This is the same information as contained in COLUMN_DATATYPE
      and COLUMN_SUBTYPE, but in a form used in some DDL statements.
10. COLUMN_SUBTYPE   fldINT16
      The logical data subtype for the field.
11. COLUMN_PRECISION fldINT32
      The size of the field type (number of characters in a string, bytes in a
      bytes field, significant digits in a BCD value, members of an ADT field, and so on)
12. COLUMN_SCALE     fldINT16
      The number of digits to the right of the decimal on BCD values,
      or descendants on ADT and array fields.
13. COLUMN_LENGTH    fldINT32
      The number of bytes required to store field values.
14. COLUMN_NULLABLE  fldINT16
      If the field requires a value, nonzero if it can be blank.

ODBC result set columns
1.  TABLE_CAT         Varchar
      Catalog name; NULL if not applicable to the data source
2.  TABLE_SCHEM       Varchar
      Schema name; NULL if not applicable to the data source.
3.  TABLE_NAME        Varchar
      Table name
4.  COLUMN_NAME       Varchar not NULL
      Column name. Empty string for a column that does not have a name
5.  DATA_TYPE         Smallint not NULL
      SQL data type
6.  TYPE_NAME         Varchar not NULL
      Data source – dependent data type name
7.  COLUMN_SIZE       Integer
     Column Size
     If DATA_TYPE is SQL_CHAR or SQL_VARCHAR, then this column contains the
     maximum length in characters of the column
     For datetime data types, this is the total number of characters required
     to display the value when converted to characters.
     For numeric data types, this is either the total number of digits or the total
     number of bits allowed in the column, according to the NUM_PREC_RADIX column
8.  BUFFER_LENGTH     Integer
      The length in bytes of data transferred on SqlFetch etc if SQL_C_DEFAULT is specified
9.  DECIMAL_DIGITS    Smallint
      The total number of significant digits to the right of the decimal point
10. NUM_PREC_RADIX    Smallint
      For numeric data types, either 10 or 2.
11. NULLABLE          Smallint not NULL
      SQL_NO_NULLS / SQL_NULLABLE / SQL_NULLABLE_UNKNOWN
12. REMARKS           Varchar
      A description of the column
13. COLUMN_DEF        Varchar
      The default value of the column
14. SQL_DATA_TYPE     Smallint not NULL
     SQL data type,
     This column is the same as the DATA_TYPE column, with the exception of
     datetime and interval data types.
     This column returns the nonconcise data type (such as SQL_DATETIME or SQL_INTERVAL),
     rather than the concise data type (such as SQL_TYPE_DATE or SQL_INTERVAL_YEAR_TO_MONTH)
15. SQL_DATETIME_SUB  Smallint
      The subtype code for datetime and interval data types.
      For other data types, this column returns a NULL.
16. CHAR_OCTET_LENGTH Integer
      The maximum length in bytes of a Character or binary data type column.
17. ORDINAL_POSITION  Integer not NULL
      The ordinal position of the column in the table
18. IS_NULLABLE       Varchar
      'NO' if the column does not include NULLs
      'YES' if the column could include NULLs
      zero-length string if nullability is unknown.
}
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}

const
  ColumnColumnNames: array[1..14] of AnsiString = { Do not localize }
  ('RECNO', 'CATALOG_NAME', 'SCHEMA_NAME', 'TABLE_NAME', 'COLUMN_NAME',
    'COLUMN_POSITION', 'COLUMN_TYPE', 'COLUMN_DATATYPE', 'COLUMN_TYPENAME', 'COLUMN_SUBTYPE',
    'COLUMN_PRECISION', 'COLUMN_SCALE', 'COLUMN_LENGTH', 'COLUMN_NULLABLE');
  ColumnColumnTypes: array[1..14] of Word =
  (fldINT32, fldZSTRING, fldZSTRING, fldZSTRING, fldZSTRING,
    fldINT16, fldINT32, fldINT16, fldZSTRING, fldINT16,
    fldINT32, fldINT16, fldINT32, fldINT16);
  ColumnColumnTypesW: array[1..14] of Word =
  (fldINT32, fldWIDESTRING, fldWIDESTRING, fldWIDESTRING, fldWIDESTRING,
    fldINT16, fldINT32, fldINT16, fldWIDESTRING, fldINT16,
    fldINT32, fldINT16, fldINT32, fldINT16);
  ColumnColumnCount = Length(ColumnColumnNames);

constructor TSqlCursorMetaDataColumns.Create;//(ASupportWideString: Boolean; OwnerMetaData: TSQLMetaDataOdbc);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataColumns.Create'); {$ENDIF _TRACE_CALLS_}
  inherited Create(ASupportWideString, OwnerMetaData);
  fColumnCount := ColumnColumnCount;
  fColumnNames := @ColumnColumnNames;
  if fSupportWideString then
    fColumnTypes := @ColumnColumnTypesW
  else
    fColumnTypes := @ColumnColumnTypes;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataColumns.Create', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataColumns.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TSqlCursorMetaDataColumns.Destroy;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataColumns.Destroy'); {$ENDIF _TRACE_CALLS_}
  Clear;
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataColumns.Destroy', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataColumns.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCursorMetaDataColumns.Clear;
var
  i: Integer;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataColumns.Clear'); {$ENDIF _TRACE_CALLS_}
  if Assigned(fTableList) then
  begin
    for i := fTableList.Count - 1 downto 0 do
      TMetaTable(fTableList[i]).Free;
    FreeAndNil(fTableList);
  end;
  if Assigned(fColumnList) then
  begin
    for i := fColumnList.Count - 1 downto 0 do
      TMetaColumn(fColumnList[i]).Free;
    FreeAndNil(fColumnList);
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataColumns.Clear', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataColumns.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCursorMetaDataColumns.FetchColumns;//(
//  SearchCatalogName,
//  SearchSchemaName,
//  SearchTableName,
//  SearchColumnName: PAnsiChar;
//  SearchColType: Longword);
var
  OdbcRetcode: OdbcApi.SqlReturn;
  Cat: PAnsiChar;
  Schema: PAnsiChar;
  TableName: PAnsiChar;
  ColumnName: PAnsiChar;
  TypeName: PAnsiChar;
  DefaultValue: PAnsiChar;
  OrdinalPosition: Integer;
  OdbcDataType: Smallint;
  Nullable: Smallint;
  OdbcColumnSize: Integer;
  DecimalScale: Smallint;
  OdbcRadix: Smallint;
  OdbcColumnBufferLength: Integer;

  cbCat: Integer;
  cbSchema: Integer;
  cbTableName: Integer;
  cbColumnName: Integer;
  cbTypeName: Integer;
  cbDefaultValue: Integer;
  cbDecimalScale: Integer; // allow for NULL values
  cbOdbcDataType: Integer;
  cbOdbcColumnSize: Integer;
  cbOdbcRadix: Integer;
  cbNullable: Integer;
  cbOrdinalPosition: Integer;
  cbOdbcColumnBufferLength: Integer;

  i: Integer;
  aMetaTable: TMetaTable;
  aMetaColumn: TMetaColumn;
  bTableFound: Boolean;

  aDbxConStmtInfo: TDbxConStmtInfo;
  OLDCurrentDbxConStmt: PDbxConStmt;

begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataColumns.FetchColumns', ['SearchTableName =', SearchTableName, 'SearchColumnName =', SearchColumnName, 'SearchColType =', SearchColType]); {$ENDIF _TRACE_CALLS_}
  Clear;
  Cat := nil;
  Schema := nil;
  TableName := nil;
  ColumnName := nil;
  TypeName := nil;
  DefaultValue := nil;
  fHStmt := SQL_NULL_HANDLE;
  OLDCurrentDbxConStmt := nil;

  {$IFDEF _DBXCB_}
  if Assigned(fSqlConnectionOdbc.fDbxTraceCallbackEven) then
    fSqlConnectionOdbc.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Misc, 'metadata (columns): (%s,%s,%s,%s,%d)', [
      StrPas(SearchCatalogName), StrPas(SearchSchemaName),
      StrPas(SearchTableName), StrPas(SearchColumnName),
      Integer(SearchColType) ]);
  {$ENDIF}

  with fSqlDriverOdbc.fOdbcApi do
  try
//
//todo: bUnicodeApi := fSupportWideString and fSqlDriverOdbc.fIsUnicodeOdbcApi and Assigned(SQLDescribeColW) and Assigned(SQLColumnsW);
//
    aDbxConStmtInfo.fDbxConStmt := nil;
    aDbxConStmtInfo.fDbxHStmtNode := nil;
    if fSqlConnectionOdbc.fStatementPerConnection > 0 then
    begin
      OLDCurrentDbxConStmt := fSqlConnectionOdbc.GetCurrentDbxConStmt();
      if fSqlConnectionOdbc.fCurrDbxConStmt = nil then
        OLDCurrentDbxConStmt := nil;
      //fSqlConnectionOdbc.fCurrDbxConStmt := aDbxConStmtInfo.fDbxConStmt;
    end;
    fSqlConnectionOdbc.AllocHStmt(fHStmt, @aDbxConStmtInfo, {bMetadataRead=}True);

    if not fSqlConnectionOdbc.fSupportsCatalog then
      SearchCatalogName := nil;

    ParseTableName(SearchCatalogName, SearchSchemaName, SearchTableName);

    if (SearchColumnName <> nil) then
      if (SearchColumnName[0] = cNullAnsiChar) then
        SearchColumnName := nil;

    OdbcRetcode := SQLColumns(fHStmt,
      fMetaCatalogName, SQL_NTS, // Catalog
      fMetaSchemaName, SQL_NTS, // Schema
      fMetaTableName, SQL_NTS, // Table name match pattern
      SearchColumnName, SQL_NTS); // Column name match pattern

    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetcode, 'SQLColumns');

    if fSqlConnectionOdbc.fSupportsCatalog then
      DescribeAllocBindString(1, Cat, cbCat, True);
    if (fSqlConnectionOdbc.fOdbcMaxSchemaNameLen > 0) then
      DescribeAllocBindString(2, Schema, cbSchema, True);
    DescribeAllocBindString(3, TableName, cbTableName);
    DescribeAllocBindString(4, ColumnName, cbColumnName);
    BindSmallint(5, OdbcDataType, @cbOdbcDataType);
    DescribeAllocBindString(6, TypeName, cbTypeName);
    BindInteger(7, OdbcColumnSize, @cbOdbcColumnSize);
    BindInteger(8, OdbcColumnBufferLength, @cbOdbcColumnBufferLength);
    BindSmallint(9, DecimalScale, @cbDecimalScale);
    BindSmallint(10, OdbcRadix, @cbOdbcRadix);
    BindSmallint(11, Nullable, @cbNullable);
    // Level 2 Drivers do not support Oridinal Position
    if (fSqlConnectionOdbc.fOdbcDriverLevel = 2) then
    begin
      OrdinalPosition := 0;
      cbDefaultValue := OdbcAPi.SQL_NULL_DATA;
    end
    else
    begin
      {+2.01}
      //Vadim V.Lopushansky:
      // Automatically assign fOdbcDriverLevel mode to 2 when exception
      try
        DescribeAllocBindString(13, DefaultValue, cbDefaultValue);
        BindInteger(17, OrdinalPosition, @cbOrdinalPosition);
      except
        fSqlConnectionOdbc.fOdbcDriverLevel := 2;
        // Initialize as Level 2
        OrdinalPosition := 0;
        cbDefaultValue := OdbcAPi.SQL_NULL_DATA;
      end;
      {/+2.01}
    end;
    fTableList := TList.Create;
    fColumnList := TList.Create;

    OdbcRetcode := SQLFetch(fHStmt);

    while (OdbcRetcode <> ODBCapi.SQL_NO_DATA) do
    begin

      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        OdbcCheck(OdbcRetcode, 'SQLFetch');
      {+2.01}
      //Vadim V.Lopushansky:
      // The code for drivers which not supporting filter
      // (Easysoft IB6 ODBC Driver [ver:1.00.01.67] contain its error).
      // Edward> ???Ed>Vad/All: I think column name filter is a bad idea (see long
      // Edward> comment under TSqlCursorMetaDataTables.FetchTables).
      // Edward> ???Ed>Ed: I think the filter should also be removed from my code above.
      // Edward> But I have kept it all for now.
      if Assigned(SearchColumnName) then
        i := StrLen(SearchColumnName)
      else
        i := 0;
      if (i > 0) and ((i <> Integer(StrLen(ColumnName))) or
        (StrLComp(SearchColumnName, ColumnName, i) <> 0)) then
      begin
        OdbcRetcode := SQLFetch(fHStmt);
        continue;
      end;
      {/+2.01}

      bTableFound := False;
      aMetaTable := nil; // suppress compiler warning
      for i := 0 to fTableList.Count - 1 do
      begin
        aMetaTable := fTableList.Items[i];
        if (StrCompNil(aMetaTable.fCat, Cat) = 0) and
          (StrCompNil(aMetaTable.fSchema, Schema) = 0) and
          (StrCompNil(aMetaTable.fTableName, TableName) = 0) then
        begin
          bTableFound := True;
          break;
        end;
      end;
      if not bTableFound then
      begin
        aMetaTable := TMetaTable.Create(fSqlConnectionOdbc, Cat, Schema, TableName, eSQLTable);
        fTableList.Add(aMetaTable);
      end;

      aMetaColumn := TMetaColumn.Create(ColumnName, OrdinalPosition, TypeName);
      fColumnList.Add(aMetaColumn);
      aMetaColumn.fMetaTable := aMetaTable;
      if (cbOdbcColumnBufferLength = OdbcAPi.SQL_NULL_DATA) then
        aMetaColumn.fLength := Low(Integer) // this indicates null data
      else
        aMetaColumn.fLength := OdbcColumnBufferLength;

      if cbDecimalScale = OdbcAPi.SQL_NULL_DATA then
        aMetaColumn.fDecimalScale := Low(Smallint) // this indicates null data
      else
        aMetaColumn.fDecimalScale := DecimalScale;
      if cbOdbcColumnSize = OdbcAPi.SQL_NULL_DATA then
        aMetaColumn.fPrecision := Low(Smallint) // this indicates null data
      else
      begin
        if (cbOdbcRadix <> OdbcAPi.SQL_NULL_DATA) and (OdbcRadix = 2) then
          // if RADIX = 2, Odbc column size is number of BITs;
          // Decimal Digits is log10(2) * BITS = 0.30103 * No of BITS
          aMetaColumn.fPrecision := ((OdbcColumnSize * 3) div 10) + 1
        else
          aMetaColumn.fPrecision := OdbcColumnSize
      end;

      case Nullable of
        SQL_NULLABLE:
          aMetaColumn.fDbxNullable := 1; // it can be null
        SQL_NO_NULLS:
          aMetaColumn.fDbxNullable := 0; // null not allowed
      else { SQL_NULLABLE_UNKNOWN: }
        aMetaColumn.fDbxNullable := 1; // Odbc doesn't know - assume it might contain nulls
      end;

      OdbcDataTypeToDbxType(OdbcDataType, aMetaColumn.fDbxType, aMetaColumn.fDbxSubType,
        fSqlConnectionOdbc, fSqlConnectionOdbc.fConnectionOptions[coEnableUnicode] = osOn);
      if aMetaColumn.fDbxType = fldUNKNOWN then
      begin
        if (fSqlConnectionOdbc.fConnectionOptions[coIgnoreUnknownFieldType] = osOn) then
        begin
          { // We make comments: we shall allow to see a field in the metadata.
          // remove unknown field from list
          fColumnList.Remove(aMetaColumn);
          FreeAndNil(aMetaColumn);
          // fetch next field
          OdbcRetcode := SQLFetch(fHStmt);
          if (OdbcRetcode <> OdbcApi.SQL_SUCCESS)and(OdbcRetcode <> ODBCapi.SQL_NO_DATA) then
            OdbcCheck(OdbcRetcode, 'SQLFetch');
          continue;
          {}
        end
        else
          raise EDbxInternalError.Create('Unsupported ODBC data type ' + IntToStr(OdbcDataType)+
            ' for column: ' + string(ColumnName));
      end;
      {+2.01}
      // Vadim> ???Vad>All: OpenLink Lite for Informix 7 (32 Bit) ODBC Driver:
      // (aMetaColumn.fDbxType = 3 = BLOB )
      // Edward> I do not have Informix, I do not know
      // Vadim> Problems with loss of accuracy at type conversion.
      if aMetaColumn.fPrecision > High(Smallint) then
      begin
        aMetaColumn.fPrecision := -1;
        // Edward> ???Ed>Vad/All: This does not look right!
        // Edward> But I do not understand exactly what you are trying to do
        if aMetaColumn.fLength > High(Smallint) then
          aMetaColumn.fLength := High(Integer);
      end;
      {/+2.01}

      {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
      { Dbx Column type is combination of following flags
      eSQLRowId         Row Id number.
      eSQLRowVersion    Version number.
      eSQLAutoIncr      Auto-incrementing field (server generates value).
      eSQLDefault       Field with a default value. (server can generate value)

      eSQLRowId      - This can be determined by Odbc call SQLSpecialColumns SQL_BEST_ROWID
      eSQLRowVersion - This can be determined by Odbc call SQLSpecialColumns SQL_ROWVER
      eSQLAutoIncr   - Odbc does not have facility to determine this until actual Result set
      eSQLDefault    - Odbc will return the defaulkt value
      }
      {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
      if (cbDefaultValue <> OdbcAPi.SQL_NULL_DATA) then
        aMetaColumn.fDbxColumnType := aMetaColumn.fDbxColumnType + eSQLDefault;
      OdbcRetcode := SQLFetch(fHStmt);
    end; //of: while (OdbcRetCode <> ODBCapi.SQL_NO_DATA)

    OdbcRetcode := SQLCloseCursor(fHStmt);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetcode, 'SQLCloseCursor');
    OdbcRetcode := SQLFreeStmt(fHStmt, SQL_UNBIND);
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetcode, 'SQLFreeStmt - SQL_UNBIND');

    // Next block of code to determine eSQLRowId and eSQLRowVersion
    {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
    {// But there's no point, DbExpress does not use this information

    // This is to determine eSQLRowId
    OdbcRetCode := SQLSpecialColumns(fhStmt,
      SQL_BEST_ROWID,
      fMetaCatalogName, SQL_NTS, // Catalog
      fMetaSchemaName, SQL_NTS, // Schema
      fMetaTableName, SQL_NTS, // Table name match pattern
      SQL_SCOPE_TRANSACTION, // Minimum required scope of the rowid
      SQL_NULLABLE); // Return even if column can be null
    if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetCode, 'SQLSpecialColumns');

    DescribeAllocBindString(2, ColumnName, cbColumnName);
    BindSmallInt(3, OdbcDataType, @cbOdbcDataType);
    DescribeAllocBindString(4, TypeName, cbTypeName);
    BindInteger(5, OdbcColumnSize, @cbOdbcColumnSize);

    OdbcRetCode := SQLFetch(fhStmt);

    while (OdbcRetCode <> ODBCapi.SQL_NO_DATA) do
    begin
      if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
        OdbcCheck(OdbcRetCode, 'SQLFetch');
      for i := 0 to fColumnList.Count - 1 do
      begin
        aMetaColumn := TMetaColumn(fColumnList.Items[i]);
        if StrComp(aMetaColumn.fColumnName, ColumnName) = 0 then
          aMetaColumn.fDbxColumnType := aMetaColumn.fDbxColumnType + eSQLRowId;
      end;
      OdbcRetCode := SQLFetch(fhStmt);
    end;

    OdbcRetCode := SQLCloseCursor(fhStmt);
    if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetCode, 'SQLCloseCursor');
    OdbcRetCode := SQLFreeStmt (fhStmt, SQL_UNBIND);
    if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetCode, 'SQLFreeStmt - SQL_UNBIND');

    // This is to determine eSQLRowVersion
    OdbcRetCode := SQLSpecialColumns(fhStmt,
      SQL_ROWVER,
      fMetaCatalogName, SQL_NTS, // Catalog
      fMetaSchemaName, SQL_NTS, // Schema
      fMetaTableName, SQL_NTS, // Table name match pattern
      0, // Does not apply to SQL_ROWVER
      SQL_NULLABLE); // Return even if column can be null
    if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetCode, 'SQLSpecialColumns');

    DescribeAllocBindString(2, ColumnName, cbColumnName);
    BindSmallInt(3, OdbcDataType, @cbOdbcDataType);
    DescribeAllocBindString(4, TypeName, cbTypeName);
    BindInteger(5, OdbcColumnSize, @cbOdbcColumnSize);

    OdbcRetCode := SQLFetch(fhStmt);

    while (OdbcRetCode <> ODBCapi.SQL_NO_DATA) do
    begin
      if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
        OdbcCheck(OdbcRetCode, 'SQLFetch');
      for i := 0 to fColumnList.Count - 1 do
      begin
        aMetaColumn := TMetaColumn(fColumnList.Items[i]);
        if StrComp(aMetaColumn.fColumnName, ColumnName) = 0 then
          aMetaColumn.fDbxColumnType := aMetaColumn.fDbxColumnType + eSQLRowVersion;
      end;
      OdbcRetCode := SQLFetch(fhStmt);
    end;

    OdbcRetCode := SQLCloseCursor(fhStmt);
    if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetCode, 'SQLCloseCursor');
    OdbcRetCode := SQLFreeStmt (fhStmt, SQL_UNBIND);
    if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetCode, 'SQLFreeStmt - SQL_UNBIND');
    {}
    {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}

    fCatLenMax := 0;
    fSchemaLenMax := 0;
    fTableLenMax := 1;
    fColumnLenMax := 1;

    for i := 0 to fTableList.Count - 1 do
    begin
      aMetaTable := TMetaTable(fTableList.Items[i]);

      if Assigned(aMetaTable.fCat) then
        MaxSet(fCatLenMax, StrLen(aMetaTable.fCat));

      if Assigned(aMetaTable.fSchema) then
        MaxSet(fSchemaLenMax, StrLen(aMetaTable.fSchema));

      if Assigned(aMetaTable.fTableName) then
        MaxSet(fTableLenMax, StrLen(aMetaTable.fTableName));
    end;

    for i := 0 to fColumnList.Count - 1 do
    begin
      aMetaColumn := TMetaColumn(fColumnList.Items[i]);

      if Assigned(aMetaColumn.fColumnName) then
        MaxSet(fColumnLenMax, StrLen(aMetaColumn.fColumnName));

      if Assigned(aMetaColumn.fTypeName) then
        MaxSet(fTypeNameLenMax, StrLen(aMetaColumn.fTypeName));
    end;

  finally
    FreeMem(Cat);
    FreeMem(Schema);
    FreeMem(TableName);
    FreeMem(ColumnName);
    FreeMem(TypeName);
    FreeMem(DefaultValue);

    if (fHStmt <> SQL_NULL_HANDLE) then
    begin
      // calls freehandle & sets SQL_NULL_HANDLE
      fSqlConnectionOdbc.FreeHStmt(fHStmt, @aDbxConStmtInfo);
      if (fSqlConnectionOdbc.fStatementPerConnection > 0)
        and (fSqlConnectionOdbc.fCurrDbxConStmt = nil)
      then
        fSqlConnectionOdbc.SetCurrentDbxConStmt(OLDCurrentDbxConStmt);
    end;


  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataColumns.FetchColumns', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataColumns.FetchColumns'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataColumns.getColumnLength;//(ColumnNumber: Word;
//  var pLength: Longword): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataColumns.getColumnLength', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    case ColumnNumber of
      1: // RECNO
        pLength := SizeOf(Integer);
      2: // CATALOG_NAME
        pLength := fCatLenMax;
      3: // SCHEMA_NAME
        pLength := fSchemaLenMax;
      4: // TABLE_NAME
        pLength := fTableLenMax;
      5: // COLUMN_NAME
        pLength := fColumnLenMax;
      6: // COLUMN_POSITION   fldINT16
        pLength := SizeOf(Smallint);
      7: // COLUMN_TYPE       fldINT32
        pLength := SizeOf(Longint);
      8: // COLUMN_DATATYPE   fldINT16
        pLength := SizeOf(Smallint);
      9: // COLUMN_TYPENAME
        pLength := fTypeNameLenMax;
      10: // COLUMN_SUBTYPE   fldINT16
        pLength := SizeOf(Smallint);
      11: // COLUMN_PRECISION fldINT32
        pLength := SizeOf(Longint);
      12: // COLUMN_SCALE     fldINT16
        pLength := SizeOf(Smallint);
      13: // COLUMN_LENGTH    fldINT32
        pLength := SizeOf(Longint);
      14: // COLUMN_NULLABLE  fldINT16
        pLength := SizeOf(Smallint);
    else
    begin
      pLength := 0;
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataColumns.getColumnLength invalid column no: '
        + IntToStr(ColumnNumber));
    end;
    end; //of: case ColumnNumber
    {+2.01}
    // Vadim V.Lopushansky:
    // If length is equal 0 that Delphi will ignore this column.
    // It is bad, since the column describes the metadata.
    if (pLength = 0) and (ColumnColumnTypes[ColumnNumber] = fldZSTRING) then
      pLength := 1;
    {/+2.01}
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      pLength := 0;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataColumns.getColumnLength', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataColumns.getColumnLength', ['pLength =', pLength]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataColumns.getColumnPrecision;//(ColumnNumber: Word;
//  var piPrecision: Smallint): SQLResult;
var
  Length: Longword;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataColumns.getColumnPrecision', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  Result := getColumnLength(ColumnNumber, Length);
  {+2.01}
  //Vadim V.Lopushansky:
  // Problems with loss of accuracy at type conversion
  // Edward> I assume this is correct
  if Length < Longword(High(Smallint)) then
    piPrecision := Length
  else
    piPrecision := -1;
  {/+2.01}
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataColumns.getColumnPrecision', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataColumns.getColumnPrecision', ['piPrecision =', piPrecision]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataColumns.getLong;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataColumns.getLong', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    case ColumnNumber of
      1: // RECNO
        begin
          IsBlank := False;
          Integer(Value^) := fRowNo;
        end;
      7: // COLUMN_TYPE      fldINT32
        begin
          IsBlank := False;
          Integer(Value^) := fMetaColumnCurrent.fDbxColumnType
        end;
      11: // COLUMN_PRECISION  fldINT32
        begin
          if fMetaColumnCurrent.fPrecision = low(Integer) then
          begin
            IsBlank := True;
            Integer(Value^) := 0;
          end
          else
          begin
            IsBlank := False;
            Integer(Value^) := fMetaColumnCurrent.fPrecision;
          end;
        end;
      13: // COLUMN_LENGTH    fldINT32
        begin
          if fMetaColumnCurrent.fLength = low(Integer) then
          begin
            IsBlank := True;
            Integer(Value^) := 0;
          end
          else
          begin
            IsBlank := False;
            Integer(Value^) := fMetaColumnCurrent.fLength;
          end;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataColumns.getLong not valid for column '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      Integer(Value^) := 0;
      IsBlank := True;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataColumns.getLong', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataColumns.getLong', ['Value =', Integer(Value^), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataColumns.getShort;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataColumns.getShort', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    case ColumnNumber of
      6: // COLUMN_POSITION    fldINT16
        begin
          IsBlank := False;
          Smallint(Value^) := fMetaColumnCurrent.fOrdinalPosition;
        end;
      8: // COLUMN_DATATYPE  fldINT16
        begin
          IsBlank := False;
          Smallint(Value^) := fMetaColumnCurrent.fDbxType;
        end;
      10: // COLUMN_SUBTYPE   fldINT16
        begin
          IsBlank := False;
          Smallint(Value^) := fMetaColumnCurrent.fDbxSubType;
        end;
      12: // COLUMN_SCALE     fldINT16
        begin
          if fMetaColumnCurrent.fDecimalScale = low(Smallint) then
          begin
            IsBlank := True;
            Smallint(Value^) := 0;
          end
          else
          begin
            IsBlank := False;
            Smallint(Value^) := fMetaColumnCurrent.fDecimalScale;
          end;
        end;
      14: // COLUMN_NULLABLE  fldINT16
        begin
          IsBlank := False;
          Smallint(Value^) := fMetaColumnCurrent.fDbxNullable;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataColumns.getShort not valid for column '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      IsBlank := True;
      Smallint(Value^) := 0;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataColumns.getShort', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataColumns.getShort', ['Value =', Smallint(Value^), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataColumns.getString;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_}
    if Value <> nil then PAnsiChar(Value)^ := cNullAnsiChar;
    IsBlank := True;
    Result := DBXERR_NONE;
    try try
    LogEnterProc('TSqlCursorMetaDataColumns.getString', ['ColumnNumber =', ColumnNumber]);
  {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    Exit;
  end;
  try
    case ColumnNumber of
      2: // CATALOG_NAME
        begin
          if (fMetaTableCurrent.fCat = nil) then
          begin
            PAnsiChar(Value)^ := cNullAnsiChar;
            IsBlank := True;
          end
          else
          begin
            StrCopy(Value, PAnsiChar(fMetaTableCurrent.fCat));
            IsBlank := False;
          end;
        end;
      3: // SCHEMA_NAME
        begin
          if (fMetaTableCurrent.fSchema = nil) then
          begin
            PAnsiChar(Value)^ := cNullAnsiChar;
            IsBlank := True;
          end
          else
          begin
            StrCopy(Value, PAnsiChar(fMetaTableCurrent.fSchema));
            IsBlank := False;
          end;
        end;
      4: // TABLE_NAME
        begin
          StrCopy(Value, PAnsiChar(fMetaTableCurrent.fTableName));
          IsBlank := False;
        end;
      5: // COLUMN_NAME      fldZSTRING
        begin
          StrCopy(Value, PAnsiChar(fMetaColumnCurrent.fColumnName));
          IsBlank := False;
        end;
      9: // COLUMN_TYPENAME  fldZSTRING
        begin
          StrCopy(Value, PAnsiChar(fMetaColumnCurrent.fTypeName));
          IsBlank := False;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataColumns.getString not valid for column '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      PAnsiChar(Value)^ := cNullAnsiChar;
      IsBlank := True;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataColumns.getString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataColumns.getString', ['Value =', PAnsiChar(Value), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataColumns.DoGetWideString;//(ColumnNumber: Word;
// Value: PWideChar; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_}
    if Value <> nil then PWideChar(Value)^ := cNullWideChar;
    IsBlank := True;
    Result := DBXERR_NONE;
    try try
    LogEnterProc('TSqlCursorMetaDataColumns.DoGetWideString', ['ColumnNumber =', ColumnNumber]);
  {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    Exit;
  end;
  try
    case ColumnNumber of
      2: // CATALOG_NAME
        begin
          if (fMetaTableCurrent.fWCat = nil) then
          begin
            Value^ := cNullWideChar;
            IsBlank := True;
          end
          else
          begin
            WStrCopy(Value, fMetaTableCurrent.fWCat);
            IsBlank := False;
          end;
        end;
      3: // SCHEMA_NAME
        begin
          if (fMetaTableCurrent.fWSchema = nil) then
          begin
            Value^ := cNullWideChar;
            IsBlank := True;
          end
          else
          begin
            WStrCopy(Value, fMetaTableCurrent.fWSchema);
            IsBlank := False;
          end;
        end;
      4: // TABLE_NAME
        begin
          WStrCopy(Value, fMetaTableCurrent.fWTableName);
          IsBlank := False;
        end;
      5: // COLUMN_NAME      fldZSTRING
        begin
          WStrCopy(Value, fMetaColumnCurrent.fWColumnName);
          IsBlank := False;
        end;
      9: // COLUMN_TYPENAME  fldZSTRING
        begin
          WStrCopy(Value, fMetaColumnCurrent.fWTypeName);
          IsBlank := False;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataColumns.DoGetWideString not valid for column '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      Value^ := cNullWideChar;
      IsBlank := True;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataColumns.DoGetWideString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataColumns.DoGetWideString', ['Value =', Value, 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataColumns.next: SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataColumns.next', ['fRowNo =', fRowNo]); {$ENDIF _TRACE_CALLS_}
  Inc(fRowNo);
  if (fColumnList = nil) or
    (fRowNo > fColumnList.Count) then
  begin
    Result := DBXERR_EOF;
    exit;
  end;
  Result := DBXERR_NONE;
  fMetaColumnCurrent := fColumnList.Items[fRowNo - 1];
  fMetaTableCurrent := fMetaColumnCurrent.fMetaTable;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataColumns.next', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataColumns.next', ['fRowNo =', fRowNo]); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSqlCursorMetaDataIndexes }

const
  IndexColumnNames: array[1..11] of AnsiString = { Do not localize }
  ('RECNO', 'CATALOG_NAME', 'SCHEMA_NAME', 'TABLE_NAME', 'INDEX_NAME',
    'PKEY_NAME', 'COLUMN_NAME', 'COLUMN_POSITION', 'INDEX_TYPE', 'SORT_ORDER',
    'FILTER');
  IndexColumnTypes: array[1..11] of Word =
  (fldINT32, fldZSTRING, fldZSTRING, fldZSTRING, fldZSTRING,
    fldZSTRING, fldZSTRING, fldINT16, fldINT16, fldZSTRING,
    fldZSTRING);
  IndexColumnTypesW: array[1..11] of Word =
  (fldINT32, fldWIDESTRING, fldWIDESTRING, fldWIDESTRING, fldWIDESTRING,
    fldWIDESTRING, fldWIDESTRING, fldINT16, fldINT16, fldWIDESTRING,
    fldWIDESTRING);
  IndexColumnCount = Length(IndexColumnNames);

{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
  {
  1.  RECNO           fldINT32
        A record number that uniquely identifies each record.
  2.  CATALOG_NAME    fldZSTRING
        The name of the catalog (database) that contains the index.
  3.  SCHEMA_NAME     fldZSTRING
        The name of the schema that identifies the owner of the index.
  4.  TABLE_NAME      fldZSTRING
        The name of the table for which the index is defined.
  5.  INDEX_NAME      fldZSTRING
        The name of the index.
  6.  PKEY_NAME       fldZSTRING
        The name of the primary key.
  7.  COLUMN_NAME     fldZSTRING
        The name of the column (field) in the index.
  8.  COLUMN_POSITION fldINT16
        The position of this field in the index.
  9.  INDEX_TYPE      fldINT16
        An eSQLIndexType value (C++) or index type constant (Object Pascal) that
        indicates any special properties of the index.
  10. SORT_ORDER      fldZSTRING
        Indicates whether the index sorts on this field in
        ascending (a) or descending (d) order.
  11. FILTER          fldZSTRING
        A string that gives a filter condition limiting indexed records.

  ODBC SqlStatistics Result set columns:

  1.  TABLE_CAT        Varchar
        Catalog name of the table to which the statistic or index applies;
        NULL if not applicable to the data source.
  2.  TABLE_SCHEM      Varchar
        Schema name of the table to which the statistic or index applies;
        NULL if not applicable to the data source.
  3.  TABLE_NAME       Varchar not NULL
        Table name of the table to which the statistic or index applies.
  4.  NON_UNIQUE       Smallint
        Indicates whether the index prohibits duplicate values:
        SQL_TRUE if the index values can be nonunique.
        SQL_FALSE if the index values must be unique.
        NULL is returned if TYPE is SQL_TABLE_STAT.
  5.  INDEX_QUALIFIER  Varchar
        The identifier that is used to qualify the index name doing a DROP INDEX;
        NULL is returned if an index qualifier is not supported by the data source
        or if TYPE is SQL_TABLE_STAT.
        If a non-null value is returned in this column, it must be used to qualify
        the index name on a DROP INDEX statement; otherwise the TABLE_SCHEM
        should be used to qualify the index name.
  6.  INDEX_NAME       Varchar
         Index name; NULL is returned if TYPE is SQL_TABLE_STAT.
  7.  TYPE             Smallint not NULL
        Type of information being returned:
        SQL_TABLE_STAT indicates a statistic for the table (in the CARDINALITY or PAGES column).
        SQL_INDEX_BTREE indicates a B-Tree index.
        SQL_INDEX_CLUSTERED indicates a clustered index.
        SQL_INDEX_CONTENT indicates a content index.
        SQL_INDEX_HASHED indicates a hashed index.
        SQL_INDEX_OTHER indicates another type of index.
  8.  ORDINAL_POSITION Smallint
        Column sequence number in index (starting with 1);
        NULL is returned if TYPE is SQL_TABLE_STAT.
  9.  COLUMN_NAME      Varchar
        Column name.
        If the column is based on an expression, such as SALARY + BENEFITS,
        the expression is returned;
        if the expression cannot be determined, an empty string is returned.
        NULL is returned if TYPE is SQL_TABLE_STAT.
  10. ASC_OR_DESC      Char(1)         Sort sequence for the column;
       'A' for ascending; 'D' for descending;
       NULL is returned if column sort sequence is not supported by the
       data source or if TYPE is SQL_TABLE_STAT.
  11. CARDINALITY      Integer         Cardinality of table or index;
       number of rows in table if TYPE is SQL_TABLE_STAT;
       number of unique values in the index if TYPE is not SQL_TABLE_STAT;
       NULL is returned if the value is not available from the data source.
  12. PAGES            Integer
       Number of pages used to store the index or table;
       number of pages for the table if TYPE is SQL_TABLE_STAT;
       number of pages for the index if TYPE is not SQL_TABLE_STAT;
       NULL is returned if the value is not available from the data source,
       or if not applicable to the data source.
  13. FILTER_CONDITION Varchar
       If the index is a filtered index, this is the filter condition,
       such as SALARY > 30000;
       if the filter condition cannot be determined, this is an empty string.
       NULL if the index is not a filtered index, it cannot be determined whether
       the index is a filtered index, or TYPE is SQL_TABLE_STAT.

  ODBC SqlPrimaryKeys Result set columns:

  1.  TABLE_CAT   Varchar
        Primary key table catalog name;
        NULL if not applicable to the data source.
        If a driver supports catalogs for some tables but not for others,
        such as when the driver retrieves data from different DBMSs,
        it returns an empty string ('') for those tables that do not have catalogs.
  2.  TABLE_SCHEM Varchar
        Primary key table schema name;
        NULL if not applicable to the data source.
        If a driver supports schemas for some tables but not for others,
        such as when the driver retrieves data from different DBMSs,
        it returns an empty string ('') for those tables that do not have schemas.
  3.  TABLE_NAME  Varchar not NULL
        Primary key table name.
  4.  COLUMN_NAME Varchar not NULL
        Primary key column name.
        The driver returns an empty string for a column that does not have a name.
  5.  KEY_SEQ     Smallint not NULL  Column sequence number in key (starting with 1).
  6.  PK_NAME     Varchar
        Primary key name. NULL if not applicable to the data source.
  }
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}

constructor TSqlCursorMetaDataIndexes.Create;//(ASupportWideString: Boolean; OwnerMetaData: TSQLMetaDataOdbc);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataIndexes.Create'); {$ENDIF _TRACE_CALLS_}
  inherited Create(ASupportWideString, OwnerMetaData);
  fColumnCount := IndexColumnCount;
  fColumnNames := @IndexColumnNames;
  if fSupportWideString then
    fColumnTypes := @IndexColumnTypesW
  else
    fColumnTypes := @IndexColumnTypes;
  fTableList := TList.Create;
  fIndexList := TList.Create;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataIndexes.Create', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataIndexes.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TSqlCursorMetaDataIndexes.Destroy;
var
  i: Integer;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataIndexes.Destroy'); {$ENDIF _TRACE_CALLS_}
  if fTableList<>nil then
  begin
    for i := fTableList.Count - 1 downto 0 do
      TMetaTable(fTableList[i]).Free;
    FreeAndNil(fTableList);
  end;
  if fIndexList<>nil then
  begin
    for i := fIndexList.Count - 1 downto 0 do
      TMetaIndexColumn(fIndexList[i]).Free;
    FreeAndNil(fIndexList);
  end;
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataIndexes.Destroy', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataIndexes.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCursorMetaDataIndexes.FetchIndexes;//(
//  SearchCatalogName,
//  SearchSchemaName,
//  SearchTableName: PAnsiChar;
//  SearchIndexType: Longword);
var
  OdbcRetcode: OdbcApi.SqlReturn;
  {$IFDEF _DBXCB_}
  sIndexType: AnsiString;
  {$ENDIF}

  OdbcPkName: PAnsiChar;
  OdbcPkColumnName: PAnsiChar;
  OdbcPkCatName: PAnsiChar;
  OdbcPkSchemaName: PAnsiChar;
  OdbcPkTableName: PAnsiChar;

  IndexName: PAnsiChar;
  IndexColumnName: PAnsiChar;
  IndexFilter: PAnsiChar;
  IndexColumnPosition: Smallint;
  AscDesc: array[0..1] of AnsiChar;
  CatName: PAnsiChar;
  SchemaName: PAnsiChar;
  TableName: PAnsiChar;

  { Vars below were used for search pattern logic - now commented out
  Cat:                    PAnsiChar;
  Schema:                 PAnsiChar;
  TableName:              PAnsiChar;
  OdbcTableType:          PAnsiChar;

  cbCat:                  Integer;
  cbSchema:               Integer;
  cbTableName:            Integer;
  cbOdbcTableType:        Integer;{}

  cbOdbcPkColumnName: Integer;
  cbOdbcPkName: Integer;
  cbOdbcPkCatName: Integer;
  cbOdbcPkSchemaName: Integer;
  cbOdbcPkTableName: Integer;

  cbIndexName: Integer;
  cbIndexColumnName: Integer;
  cbIndexFilter: Integer;
  cbOdbcNonUnique: Integer;
  cbAscDesc: Integer;
  cbIndexColumnPosition: Smallint;
  cbOdbcIndexType: Integer;
  cbCatName: Integer;
  cbSchemaName: Integer;
  cbTableName: Integer;

  OdbcIndexType: Smallint;
  OdbcNonUnique: Smallint;

  i: Integer;
  aMetaTable: TMetaTable;
  aMetaIndexColumn: TMetaIndexColumn;
  sQuoteChar: AnsiString;

  aDbxConStmtInfo: TDbxConStmtInfo;
  OLDCurrentDbxConStmt: PDbxConStmt;

begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataIndexes.FetchIndexes', ['SearchTableName =', SearchTableName, 'SearchIndexType =', SearchIndexType]); {$ENDIF _TRACE_CALLS_}

  { Vars below were used for search pattern logic - now commented out
  Cat := nil;
  Schema := nil;
  TableName := nil;
  OdbcTableType := nil;{}
  fHStmt := SQL_NULL_HANDLE;

  OdbcPkName := nil;
  OdbcPkColumnName := nil;
  OdbcPkCatName := nil;
  OdbcPkSchemaName := nil;
  OdbcPkTableName := nil;

  IndexName := nil;
  IndexColumnName := nil;
  IndexFilter := nil;
  CatName := nil;
  SchemaName := nil;
  TableName := nil;

  fCatLenMax := 1;
  fSchemaLenMax := 1;
  fTableLenMax := 1;
  fIndexNameLenMax := 1;
  fIndexColumnNameLenMax := 1;
  fPkCatalogLenMax := 1;
  fPkSchemaLenMax := 1;
  fPkTableLenMax := 1;
  fPkNameLenMax := 1;
  fFilterLenMax := 1;
  OLDCurrentDbxConStmt := nil;

  {$IFDEF _DBXCB_}
  if Assigned(fSqlConnectionOdbc.fDbxTraceCallbackEven) then
  begin
    case SearchIndexType of
      eSQLPrimaryKey:
        sIndexType := 'eSQLPrimaryKey(SQL_INDEX_UNIQUE)';
      eSQLUnique:
        sIndexType := 'eSQLUnique(SQL_INDEX_UNIQUE)';
      eSQLPrimaryKey + eSQLUnique:
        sIndexType := 'eSQLPrimaryKey + eSQLUnique(SQL_INDEX_UNIQUE)';
      else
        sIndexType := 'eSQLUnique(SQL_INDEX_ALL)';
    end;
    fSqlConnectionOdbc.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Misc, 'metadata (indexes): (%s,%s,%s,,%s)', [
      StrPas(SearchCatalogName), StrPas(SearchSchemaName),
      StrPas(SearchTableName), sIndexType ]);
  end;
  {$ENDIF}

  with fSqlDriverOdbc.fOdbcApi do
  try
//
//todo: bUnicodeApi := fSupportWideString and fSqlDriverOdbc.fIsUnicodeOdbcApi and Assigned(SQLDescribeColW) and Assigned(SQLPrimaryKeysW);
//
    aDbxConStmtInfo.fDbxConStmt := nil;
    aDbxConStmtInfo.fDbxHStmtNode := nil;
    if fSqlConnectionOdbc.fStatementPerConnection > 0 then
    begin
      OLDCurrentDbxConStmt := fSqlConnectionOdbc.GetCurrentDbxConStmt();
      if fSqlConnectionOdbc.fCurrDbxConStmt = nil then
        OLDCurrentDbxConStmt := nil;
      //fSqlConnectionOdbc.fCurrDbxConStmt := aDbxConStmtInfo.fDbxConStmt;
    end;
    fSqlConnectionOdbc.AllocHStmt(fHStmt, @aDbxConStmtInfo, {bMetadataRead=}True);

    if not fSqlConnectionOdbc.fSupportsCatalog then
      SearchCatalogName := nil;

    ParseTableName(SearchCatalogName, SearchSchemaName, SearchTableName);

    if (SearchIndexType = eSQLPrimaryKey) or
      (SearchIndexType = eSQLUnique) or
      (SearchIndexType = eSQLPrimaryKey + eSQLUnique) then
      OdbcIndexType := OdbcApi.SQL_INDEX_UNIQUE
    else
      OdbcIndexType := OdbcApi.SQL_INDEX_ALL;
    {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
    {
    // Accoring to DBXpress help, ISqlMetaDate.GetIndices allows for SEARCH PATTERN
    // As Odbc Index function don't allow for search pattern, we have to get all
    // matching tables first, then call Odbc Index functions for EACH table found.

    // NOW COMMENTED OUT - DBXpress HELP IS WRONG
    // Table names containing underscore (Odbc single char wildcard) fuck it up

    OdbcRetCode := SQLTables(fhStmt,
    fMetaCatalogName, SQL_NTS, // Catalog name
    fMetaSchemaName, SQL_NTS,  // Schema name
    fMetaTableName, SQL_NTS,   // Table name match pattern
    nil, SQL_NTS);             // Table types

    if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetCode, 'SQLTables');

    if fSqlConnectionOdbc.fSupportsCatalog then
      DescribeAllocBindString(1, Cat, cbCat);
    DescribeAllocBindString(2, Schema, cbSchema);
    DescribeAllocBindString(3, TableName, cbTableName);
    DescribeAllocBindString(4, OdbcTableType, cbOdbcTableType);

    OdbcRetCode := SQLFetch(fhStmt);

    // -----------------------------------------------
    // This is to find the TABLES that match search parameters...
    while (OdbcRetCode <> ODBCapi.SQL_NO_DATA) do
    begin
      if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
        OdbcCheck(OdbcRetCode, 'SQLFetch');
      aMetaTable := TMetaTable.Create(fSqlConnectionOdbc, Cat, Schema, TableName, eSQLTable);
      fTableList.Add(aMetaTable);
      OdbcRetCode := SQLFetch(fhStmt);
    end;
    OdbcRetCode := SQLCloseCursor(fhStmt);
    if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetCode, 'SQLCloseCursor');
    OdbcRetCode := SQLFreeStmt (fhStmt, SQL_UNBIND);
    if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetCode, 'SQLFreeStmt - SQL_UNBIND');
    }  // End of commented section (Table name matching)
    {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
    aMetaTable := TMetaTable.Create(fSqlConnectionOdbc,
      fMetaCatalogName, fMetaSchemaName, fMetaTableName, eSQLTable);
    fTableList.Add(aMetaTable);
    // -----------------------------------------------
    for i := 0 to fTableList.Count - 1 do
    begin
      aMetaTable := TMetaTable(fTableList.Items[i]);
      fPkNameLenMax := 0;

      // -----------------------------------------------
      // This is to find the PRIMARY KEY of the table...
      if fSqlConnectionOdbc.fSupportsSQLPRIMARYKEYS then
      begin
        OdbcRetcode := SQLPrimaryKeys(fHStmt,
          aMetaTable.fCat, SQL_NTS, // Catalog name (match pattern not allowed)
          aMetaTable.fSchema, SQL_NTS, // Schema name (match pattern not allowed)
          aMetaTable.fTableName, SQL_NTS); // Table name (match pattern not allowed)
        // INFORMIX: The error is possible at call to other database.
        // Example:  select username from sysmaster::informix.syssessions
        // OdbcCheck(OdbcRetCode, 'SQLPrimaryKeys');
        if OdbcRetcode = OdbcApi.SQL_SUCCESS then
        begin
          if fSqlConnectionOdbc.fSupportsCatalog then
            DescribeAllocBindString(1, OdbcPkCatName, cbOdbcPkCatName);
          DescribeAllocBindString(2, OdbcPkSchemaName, cbOdbcPkSchemaName);
          DescribeAllocBindString(3, OdbcPkTableName, cbOdbcPkTableName);
          DescribeAllocBindString(4, OdbcPkColumnName, cbOdbcPkColumnName);
          BindSmallint(5, IndexColumnPosition, @cbIndexColumnPosition);
          if (fSqlConnectionOdbc.fOdbcDriverType = eOdbcDriverTypeMySql) then
          begin
            // Work around bug in MySql Driver - It incorrectluy returns length ZERO for column 6
            GetMem(OdbcPkName, 129);
            OdbcRetcode := SQLBindCol(fHStmt, 6, SQL_C_CHAR, OdbcPkName, 129, @cbOdbcPkName);
            if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
              OdbcCheck(OdbcRetcode, 'SQLBindCol');
          end
          else
            DescribeAllocBindString(6, OdbcPkName, cbOdbcPkName);

          OdbcRetcode := SQLFetch(fHStmt);
          // if (OdbcRetCode <> OdbcApi.SQL_SUCCESS) then
          //   OdbcPkName[0] := cNullAnsiChar;
          // aMetaTable.fPkName := OdbcPkName;

          // Get the PRIMARY KEY index columns(s)
          while (OdbcRetcode <> ODBCapi.SQL_NO_DATA) do
          begin
            if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
              OdbcCheck(OdbcRetcode, 'SQLFetch');
            //if (OdbcPkName = nil) or (OdbcPkName[0] = cNullAnsiChar) then
            if Trim(StrPas(OdbcPkName)) = '' then
            begin
              {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
              {
                TClientDataSet do not correctly worked witch PacketRecords>0 when is unnamed PK.
                It has been detected on "PostgreSQL Legacy":
                  version: 07.03.0100 PostgreSQL 7.3.4 on i686-pc-cygwin,
                  compiled by GCC gcc (GCC) 3.2 20020927 (prerelease)
                  ODBC Driver: "PSQLODBC.DLL", version: 07.03.0100.

                Metadata:

                  create table test(
                    id integer primary key,
                    vc varchar(254)
                  );
                  insert into test(id, vc) values (1, null);
                  insert into test(id, vc) values (3, null);
                  insert into test(id, vc) values (2, 'test string');

                Code:

                CDS.PacketRecords := 2;
                SQLDataSet.GetMetadata := True;

                SELECT * FROM "public"."test"

                when query is:

                SELECT * FROM "public"."test"  order by id

                then all works truly.

              }
              {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
              // skip unnamed primary key
              aMetaIndexColumn := nil;
              {
              MaxSet(fPkNameLenMax, 23);//23=StrLen('[primary key - unnamed]'#0));
              aMetaIndexColumn := TMetaIndexColumn.Create(aMetaTable, '[primary key - unnamed]',
                OdbcPkColumnName);
              {}
            end
            else
            begin
              if fSqlConnectionOdbc.fSupportsCatalog then
                MaxSet(fPkCatalogLenMax, StrLenNil(OdbcPkCatName));
              MaxSet(fPkSchemaLenMax, StrLenNil(OdbcPkSchemaName));
              MaxSet(fPkTableLenMax, StrLen(OdbcPkTableName));
              MaxSet(fPkNameLenMax, StrLen(OdbcPkColumnName));
              aMetaIndexColumn := TMetaIndexColumn.Create(aMetaTable, OdbcPkCatName,
                OdbcPkSchemaName, OdbcPkTableName, OdbcPkName, OdbcPkColumnName);
            end;
            if aMetaIndexColumn <> nil then
            begin
              if (aMetaTable.fIndexColumnList = nil) then
                aMetaTable.fIndexColumnList := TList.Create;
              if aMetaTable.fPrimaryKeyColumn1 = nil then
                aMetaTable.fPrimaryKeyColumn1 := aMetaIndexColumn;

              aMetaTable.fIndexColumnList.Add(aMetaIndexColumn);
              fIndexList.Add(aMetaIndexColumn);

              aMetaIndexColumn.fColumnPosition := IndexColumnPosition;
              // Assume Primary key is unique, ascending, no filter
              aMetaIndexColumn.fIndexType := eSQLPrimaryKey + eSQLUnique;
              aMetaIndexColumn.fSortOrder := 'A';
              aMetaIndexColumn.fFilter := nil;
            end;
            OdbcRetcode := SQLFetch(fHStmt);
          end; //of: while (OdbcRetCode <> ODBCapi.SQL_NO_DATA)

          OdbcRetcode := SQLCloseCursor(fHStmt);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            OdbcCheck(OdbcRetcode, 'SQLCloseCursor');
          OdbcRetcode := SQLFreeStmt(fHStmt, SQL_UNBIND);
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            OdbcCheck(OdbcRetcode, 'SQLFreeStmt - SQL_UNBIND');
        end; //of: if OdbcRetCode = OdbcApi.SQL_SUCCESS
      end; //of: if fSqlConnectionOdbc.fSupportsSQLPRIMARYKEYS
      // -----------------------------------------------

    // -----------------------------------------------
    // Get INDEX columns...
      OdbcRetcode := SQLStatistics(fHStmt,
        aMetaTable.fCat, SQL_NTS, // Catalog name (match pattern not allowed)
        aMetaTable.fSchema, SQL_NTS, // Schema name (match pattern not allowed)
        aMetaTable.fTableName, SQL_NTS, // Table name (match pattern not allowed)
        OdbcIndexType, // Type of Index to return
        0); // Reserved
      // clear last error:
      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        fSqlDriverOdbc.RetrieveOdbcErrorInfo(OdbcRetcode, SQL_HANDLE_STMT, fHStmt,
          nil, fSqlConnectionOdbc, nil, nil, 1);

      // if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
      //   OdbcCheck(OdbcRetCode, 'SQLStatistics');
      if OdbcRetcode = OdbcApi.SQL_SUCCESS then
      begin
        if fSqlConnectionOdbc.fSupportsCatalog then
          DescribeAllocBindString(1, CatName, cbCatName);
        DescribeAllocBindString(2, SchemaName, cbSchemaName);
        DescribeAllocBindString(3, TableName, cbTableName);
        DescribeAllocBindString(6, IndexName, cbIndexName);
        DescribeAllocBindString(9, IndexColumnName, cbIndexColumnName);
        BindSmallint(4, OdbcNonUnique, @cbOdbcNonUnique);
        {+2.01}
        //BindSmallInt(7, OdbcIndexType, nil);
        BindSmallint(7, OdbcIndexType, @cbOdbcIndexType);
        {/+2.01}
        BindSmallint(8, IndexColumnPosition, @cbIndexColumnPosition);
        OdbcRetcode := SQLBindCol(fHStmt, 10, SQL_C_CHAR,
          @AscDesc, SizeOf(AscDesc), @cbAscDesc);
        if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
          OdbcCheck(OdbcRetcode, 'SQLBindCol');
        DescribeAllocBindString(13, IndexFilter, cbIndexFilter);

        OdbcRetcode := SQLFetch(fHStmt);
        while (OdbcRetcode <> ODBCapi.SQL_NO_DATA) do
        begin
          if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
            OdbcCheck(OdbcRetcode, 'SQLFetch');

          if (OdbcIndexType <> OdbcApi.SQL_TABLE_STAT) then // Ignore table statistics
          begin
            if ( (IndexName = nil) or  (IndexName^ = cNullAnsiChar) ) // skip all unnamed indexes
              or
               ( (IndexName <> nil) and (aMetaTable.fPrimaryKeyColumn1 <> nil) and
                 (AnsiStrComp(IndexName, aMetaTable.fPrimaryKeyColumn1.fIndexName) = 0)
               ) then
              // This is the Primary index - Index column already loaded
            else
            begin
              aMetaIndexColumn := TMetaIndexColumn.Create(aMetaTable,
                CatName, SchemaName, TableName, IndexName, IndexColumnName);
              if (aMetaTable.fIndexColumnList = nil) then
                aMetaTable.fIndexColumnList := TList.Create;
              fIndexList.Add(aMetaIndexColumn);
              aMetaTable.fIndexColumnList.Add(aMetaIndexColumn);

              aMetaIndexColumn.fColumnPosition := IndexColumnPosition;

              aMetaIndexColumn.fIndexType := eSQLNonUnique;
              if (cbOdbcNonUnique <> OdbcApi.SQL_NULL_DATA) and (OdbcNonUnique = SQL_FALSE) then
              begin
                if (fSqlConnectionOdbc.fOdbcDriverType = eOdbcDriverTypePostgreSQL) and
                   (aMetaTable.fTableName='pg_aggregate') then
                begin
                  if fSqlConnectionOdbc.fWantQuotedTableName then
                    sQuoteChar := PAnsiChar(@fSqlConnectionOdbc.fQuoteChar) // fQuoteChar can equal #0
                  else
                    sQuoteChar := '';
                  if not ( aMetaTable.fQualifiedTableName = // '"pg_catalog"."pg_aggregate"'
                     sQuoteChar+'pg_catalog'+sQuoteChar+'.'+sQuoteChar+'pg_aggregate'+sQuoteChar )
                  then
                    aMetaIndexColumn.fIndexType := eSQLUnique;
                end
                else
                  aMetaIndexColumn.fIndexType := eSQLUnique;
              end;

              if UpperCase(AscDesc[0]) = 'D' then
                aMetaIndexColumn.fSortOrder := 'D'
              else
                aMetaIndexColumn.fSortOrder := 'A';

              if cbIndexFilter > 0 then
              begin
                GetMem(aMetaIndexColumn.fFilter, cbIndexFilter);
                StrCopy(aMetaIndexColumn.fFilter, IndexFilter);
              end;

               // MERANT DBASE returned multicolumns as 'Col_1 + Col2 + ...'
              if (fSqlConnectionOdbc.fOdbcDriverType = eOdbcDriverTypeMerantDBASE) //and
                 //( Pos('+', StrPas(aMetaIndexColumn.fIndexColumnName))>1 )
              then
              begin
                sQuoteChar := aMetaIndexColumn.fIndexColumnName;
                cbIndexName := AnsiPos(AnsiString('+'), sQuoteChar);
                while cbIndexName>0 do
                begin
                  if aMetaIndexColumn<>nil then
                  begin // first call
                    FreeMemAndNil(aMetaIndexColumn.fIndexColumnName);
                    GetMem(aMetaIndexColumn.fIndexColumnName, cbIndexName);
                    Move(PAnsiChar(sQuoteChar)^, aMetaIndexColumn.fIndexColumnName^, cbIndexName-1);
                    aMetaIndexColumn.fIndexColumnName[cbIndexName-1] := cNullAnsiChar;
                  end
                  else  // second call
                  begin
                    // create new column
                    aMetaIndexColumn := TMetaIndexColumn.Create(aMetaTable,
                      CatName, SchemaName, TableName, IndexName,
                      PAnsiChar(Copy(sQuoteChar, 1, cbIndexName-1)));
                    // add column to lists
                    fIndexList.Add(aMetaIndexColumn);
                    aMetaTable.fIndexColumnList.Add(aMetaIndexColumn);
                    // fill column from previous column info
                    with TMetaIndexColumn(fIndexList.Items[fIndexList.Count-2]) do
                    begin
                      aMetaIndexColumn.fIndexType := fIndexType;
                      aMetaIndexColumn.fSortOrder := fSortOrder;// ???: It is incorrect, but the alternative does not exist.
                    end;
                    if cbIndexFilter > 0 then
                    begin
                      GetMem(aMetaIndexColumn.fFilter, cbIndexFilter);
                      StrCopy(aMetaIndexColumn.fFilter, IndexFilter);
                    end;
                  end;
                  aMetaIndexColumn := nil;
                  if cbIndexName<Length(sQuoteChar) then
                  begin
                    sQuoteChar := StrPas(PAnsiChar(@sQuoteChar[cbIndexName+1]));
                    cbIndexName := AnsiPos(AnsiString('+'), sQuoteChar);
                    if cbIndexName<=0 then
                      cbIndexName := Length(sQuoteChar)+1;
                  end
                  else
                    cbIndexName := 0;
                end;
              end;// end: of MERAND fixed.

            end;
          end;
          OdbcRetcode := SQLFetch(fHStmt);
        end; //of: while (OdbcRetCode <> ODBCapi.SQL_NO_DATA)

        OdbcRetcode := SQLCloseCursor(fHStmt);
        if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
          OdbcCheck(OdbcRetcode, 'SQLCloseCursor');
        OdbcRetcode := SQLFreeStmt(fHStmt, SQL_UNBIND);
        if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
          OdbcCheck(OdbcRetcode, 'SQLFreeStmt - SQL_UNBIND');
      end; //of: if OdbcRetCode = OdbcApi.SQL_SUCCESS
    end; //of: for i := 0 to fTableList.Count - 1

    fCatLenMax := 0;
    fSchemaLenMax := 0;
    fTableLenMax := 1;
    fIndexNameLenMax := 1;
    fIndexColumnNameLenMax := 1;
    fFilterLenMax := 0;

    for i := 0 to fTableList.Count - 1 do
    begin
      aMetaTable := TMetaTable(fTableList.Items[i]);

      if aMetaTable.fCat <> nil then
        MaxSet(fCatLenMax, StrLen(aMetaTable.fCat));

      if aMetaTable.fSchema <> nil then
        MaxSet(fSchemaLenMax, StrLen(aMetaTable.fSchema));

      if Assigned(aMetaTable.fTableName) then
        MaxSet(fTableLenMax, StrLen(aMetaTable.fTableName));
    end;

    for i := 0 to fIndexList.Count - 1 do
    begin
      aMetaIndexColumn := TMetaIndexColumn(fIndexList.Items[i]);

      if aMetaIndexColumn.fCatName <> nil then
        MaxSet(fCatLenMax, StrLen(aMetaIndexColumn.fCatName));

      if aMetaIndexColumn.fSchemaName <> nil then
        MaxSet(fSchemaLenMax, StrLen(aMetaIndexColumn.fSchemaName));

      if aMetaIndexColumn.fTableName <> nil then
        MaxSet(fTableLenMax, StrLen(aMetaIndexColumn.fTableName));

      if aMetaIndexColumn.fMetaTable.fCat <> nil then
        MaxSet(fCatLenMax, StrLen(aMetaIndexColumn.fMetaTable.fCat));

      if aMetaIndexColumn.fMetaTable.fSchema <> nil then
        MaxSet(fSchemaLenMax, StrLen(aMetaIndexColumn.fMetaTable.fSchema));

      if aMetaIndexColumn.fMetaTable.fTableName <> nil then
        MaxSet(fTableLenMax, StrLen(aMetaIndexColumn.fMetaTable.fTableName));

      if aMetaIndexColumn.fIndexName <> nil then
        MaxSet(fIndexNameLenMax, StrLen(aMetaIndexColumn.fIndexName));

      if aMetaIndexColumn.fIndexColumnName <> nil then
        MaxSet(fIndexColumnNameLenMax, StrLen(aMetaIndexColumn.fIndexColumnName));

      if aMetaIndexColumn.fFilter <> nil then
        MaxSet(fFilterLenMax, StrLen(aMetaIndexColumn.fFilter));
    end;

  finally
    { Vars below were used for search pattern logic - now commented out
    FreeMem(Cat);
    FreeMem(Schema);
    FreeMem(TableName);
    FreeMem(OdbcTableType);{}
    FreeMem(OdbcPkName);
    FreeMem(OdbcPkCatName);
    FreeMem(OdbcPkSchemaName);
    FreeMem(OdbcPkTableName);
    FreeMem(OdbcPkColumnName);
    FreeMem(IndexFilter);
    FreeMem(IndexName);
    FreeMem(IndexColumnName);
    FreeMem(CatName);
    FreeMem(SchemaName);
    FreeMem(TableName);

    if (fHStmt <> SQL_NULL_HANDLE) then
    begin
      // calls freehandle & sets SQL_NULL_HANDLE
      fSqlConnectionOdbc.FreeHStmt(fHStmt, @aDbxConStmtInfo);
      if (fSqlConnectionOdbc.fStatementPerConnection > 0)
        and (fSqlConnectionOdbc.fCurrDbxConStmt = nil)
      then
        fSqlConnectionOdbc.SetCurrentDbxConStmt(OLDCurrentDbxConStmt);
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataIndexes.FetchIndexes', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataIndexes.FetchIndexes'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataIndexes.getColumnLength;//(ColumnNumber: Word;
//  var pLength: Longword): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataIndexes.getColumnLength', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    case ColumnNumber of
      1: // RECNO
        pLength := SizeOf(Integer);
      2: // CATALOG_NAME
        pLength := fCatLenMax;
      3: // SCHEMA_NAME
        pLength := fSchemaLenMax;
      4: // TABLE_NAME
        pLength := fTableLenMax;
      5: // INDEX_NAME      fldZSTRING
        pLength := fIndexNameLenMax;
      6: // PKEY_NAME       fldZSTRING
        pLength := fPkNameLenMax;
      7: // COLUMN_NAME     fldZSTRING
        pLength := fIndexColumnNameLenMax;
      8: // COLUMN_POSITION fldINT16
        pLength := SizeOf(Smallint);
      9: // INDEX_TYPE       fldINT16
        pLength := SizeOf(Smallint);
      10: // SORT_ORDER     fldZSTRING
        pLength := 1;
      11: // FILTER         fldZSTRING
        pLength := fFilterLenMax;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataIndexes.getColumnLength invalid column '
        + IntToStr(ColumnNumber));
    end; //of: case ColumnNumber
    {+2.01}
    // Vadim V.Lopushansky:
    // If length is equal 0 that Delphi will ignore this column.
    // It is bad, since the column describes the metadata.
    if (pLength = 0) and (IndexColumnTypes[ColumnNumber] = fldZSTRING) then
      pLength := 1;
    {/+2.01}
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      pLength := 0;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataIndexes.getColumnLength', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataIndexes.getColumnLength', ['pLength =', pLength]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataIndexes.getColumnPrecision;//(ColumnNumber: Word;
//  var piPrecision: Smallint): SQLResult;
var
  Length: Longword;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataIndexes.getColumnPrecision', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  Result := getColumnLength(ColumnNumber, Length);
  {+2.01}
  //Vadim V.Lopushansky: Problems with loss of accuracy at type conversion
  // Edward> ???Ed>Vad: SqlIndexes should never return such a long column,
  // Edward> but this does no harm, so I leave it in
  if Length < Longword(High(Smallint)) then
    piPrecision := Length
  else
    piPrecision := -1;
  {/+2.01}
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataIndexes.getColumnPrecision', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataIndexes.getColumnPrecision', ['piPrecision =', piPrecision]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataIndexes.getLong;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataIndexes.getLong', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  try
    case ColumnNumber of
      1: // RECNO
        begin
          Integer(Value^) := fRowNo;
          IsBlank := False;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataIndexes.getLong not valid for column '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      Integer(Value^) := 0;
      IsBlank := True;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataIndexes.getLong', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataIndexes.getLong', ['Value =', Integer(Value^), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataIndexes.getShort;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataIndexes.getShort', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  try
    case ColumnNumber of
      8: // COLUMN_POSITION fldINT16
        begin
          Smallint(Value^) := fCurrentIndexColumn.fColumnPosition;
          IsBlank := False;
        end;
      9: // INDEX_TYPE       fldINT16
        begin
          Smallint(Value^) := fCurrentIndexColumn.fIndexType;
          IsBlank := False;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataIndexes.getLong not valid for column '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      IsBlank := True;
      Smallint(Value^) := 0;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataIndexes.getShort', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataIndexes.getShort', ['Value =', Smallint(Value^), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataIndexes.getString;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_}
    IsBlank := True;
    if Value <> nil then PAnsiChar(Value)^ := cNullAnsiChar;
    Result := DBXERR_NONE;
    try try
    LogEnterProc('TSqlCursorMetaDataIndexes.getString', ['ColumnNumber =', ColumnNumber]);
  {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  try
    case ColumnNumber of
      1: ; // RECNO             fldINT32
      2: // CATALOG_NAME
        begin
          IsBlank := (not fSqlConnectionOdbc.fSupportsCatalog) or
            (fCurrentIndexColumn.fCatName = nil) and
            (fCurrentIndexColumn.fMetaTable.fCat = nil);
          if not IsBlank then
            if fCurrentIndexColumn.fCatName <> nil then
              StrCopy(Value, PAnsiChar(fCurrentIndexColumn.fCatName))
            else
              StrCopy(Value, PAnsiChar(fCurrentIndexColumn.fMetaTable.fCat))
          else
            PAnsiChar(Value)^ := cNullAnsiChar;
        end;
      3: // SCHEMA_NAME
        begin
          IsBlank := (fSchemaLenMax = 0) or
            (fCurrentIndexColumn.fSchemaName = nil) and
            (fCurrentIndexColumn.fMetaTable.fSchema = nil);
          if not IsBlank then
            if fCurrentIndexColumn.fSchemaName <> nil then
              StrCopy(Value, PAnsiChar(fCurrentIndexColumn.fSchemaName))
            else
              StrCopy(Value, PAnsiChar(fCurrentIndexColumn.fMetaTable.fSchema))
          else
            PAnsiChar(Value)^ := cNullAnsiChar;
        end;
      4: // TABLE_NAME
        begin
          if fCurrentIndexColumn.fTableName <> nil then
            StrCopy(Value, PAnsiChar(fCurrentIndexColumn.fTableName))
          else
            StrCopy(Value, PAnsiChar(fCurrentIndexColumn.fMetaTable.fTableName));
          IsBlank := False;
        end;
      5: // INDEX_NAME        fldZSTRING
        begin
          StrCopy(Value, PAnsiChar(fCurrentIndexColumn.fIndexName));
          IsBlank := False;
        end;
      6: // PKEY_NAME        fldZSTRING
        begin
          if (fCurrentIndexColumn.fMetaTable.fPrimaryKeyColumn1 <> nil) and
             (StrIComp(fCurrentIndexColumn.fIndexName,
                       fCurrentIndexColumn.fMetaTable.fPrimaryKeyColumn1.fIndexName) = 0)
          then
          begin
            IsBlank := False;
            StrCopy(Value, fCurrentIndexColumn.fMetaTable.fPrimaryKeyColumn1.fIndexName);
          end
          else
          begin
            IsBlank := True;
            PAnsiChar(Value)^ := cNullAnsiChar;
          end;
        end;
      7: // COLUMN_NAME     fldZSTRING
        begin
          StrCopy(Value, PAnsiChar(fCurrentIndexColumn.fIndexColumnName));
          IsBlank := False;
        end;
      8: ; // COLUMN_POSITION fldINT16
      9: ; // INDEX_TYPE      fldINT16
      10: // SORT_ORDER      fldZSTRING
        begin
          PAnsiChar(Value)[0] := fCurrentIndexColumn.fSortOrder;
          PAnsiChar(Value)[1] := cNullAnsiChar;
          IsBlank := False;
        end;
      11: // FILTER         fldZSTRING
        begin
          if fFilterLenMax = 0 then
          begin
            IsBlank := True;
            PAnsiChar(Value)^ := cNullAnsiChar;
          end
          else
          begin
            StrCopy(Value, PAnsiChar(fCurrentIndexColumn.fFilter));
            IsBlank := False;
          end;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataIndexes.getString not valid for column '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      IsBlank := True;
      PAnsiChar(Value)^ := cNullAnsiChar;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataIndexes.getString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataIndexes.getString', ['Value =', PAnsiChar(Value), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataIndexes.DoGetWideString;//(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_}
    IsBlank := True;
    if Value <> nil then PAnsiChar(Value)^ := cNullAnsiChar;
    Result := DBXERR_NONE;
    try try
    LogEnterProc('TSqlCursorMetaDataIndexes.DoGetWideString', ['ColumnNumber =', ColumnNumber]);
  {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    Exit;
  end;
  try
    case ColumnNumber of
      1: ; // RECNO             fldINT32
      2: // CATALOG_NAME
        begin
          IsBlank := (not fSqlConnectionOdbc.fSupportsCatalog) or
            (fCurrentIndexColumn.fWCatName = nil) and
            (fCurrentIndexColumn.fMetaTable.fWCat = nil);
          if not IsBlank then
            if fCurrentIndexColumn.fWCatName <> nil then
              WStrCopy(Value, fCurrentIndexColumn.fWCatName)
            else
              WStrCopy(Value, fCurrentIndexColumn.fMetaTable.fWCat)
          else
            Value^ := cNullWideChar;
        end;
      3: // SCHEMA_NAME
        begin
          IsBlank := (fSchemaLenMax = 0) or
            (fCurrentIndexColumn.fWSchemaName = nil) and
            (fCurrentIndexColumn.fMetaTable.fWSchema = nil);
          if not IsBlank then
            if fCurrentIndexColumn.fWSchemaName <> nil then
              WStrCopy(Value, fCurrentIndexColumn.fWSchemaName)
            else
              WStrCopy(Value, fCurrentIndexColumn.fMetaTable.fWSchema)
          else
            Value^ := cNullWideChar;
        end;
      4: // TABLE_NAME
        begin
          if fCurrentIndexColumn.fWTableName <> nil then
            WStrCopy(Value, fCurrentIndexColumn.fWTableName)
          else
            WStrCopy(Value, fCurrentIndexColumn.fMetaTable.fWTableName);
          IsBlank := False;
        end;
      5: // INDEX_NAME        fldZSTRING
        begin
          WStrCopy(Value, fCurrentIndexColumn.fWIndexName);
          IsBlank := False;
        end;
      6: // PKEY_NAME        fldZSTRING
        begin
          if (fCurrentIndexColumn.fMetaTable.fPrimaryKeyColumn1 <> nil) and
             (WStrIComp(fCurrentIndexColumn.fWIndexName,
                       fCurrentIndexColumn.fMetaTable.fPrimaryKeyColumn1.fWIndexName) = 0)
          then
          begin
            IsBlank := False;
            WStrCopy(Value, fCurrentIndexColumn.fMetaTable.fPrimaryKeyColumn1.fWIndexName);
          end
          else
          begin
            IsBlank := True;
            Value^ := cNullWideChar;
          end;
        end;
      7: // COLUMN_NAME     fldZSTRING
        begin
          WStrCopy(Value, fCurrentIndexColumn.fWIndexColumnName);
          IsBlank := False;
        end;
      8: ; // COLUMN_POSITION fldINT16
      9: ; // INDEX_TYPE      fldINT16
      10: // SORT_ORDER      fldZSTRING
        begin
          PWideChar(Value)[0] := fCurrentIndexColumn.fWSortOrder;
          PWideChar(Value)[1] := cNullWideChar;
          IsBlank := False;
        end;
      11: // FILTER         fldZSTRING
        begin
          if fFilterLenMax = 0 then
          begin
            IsBlank := True;
            Value^ := cNullWideChar;
          end
          else
          begin
            WStrCopy(Value, fCurrentIndexColumn.fWFilter);
            IsBlank := False;
          end;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataIndexes.DoGetWideString not valid for column '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      IsBlank := True;
      Value^ := cNullWideChar;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataIndexes.DoGetWideString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataIndexes.DoGetWideString', ['Value =', Value, 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataIndexes.next: SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataIndexes.next'); {$ENDIF _TRACE_CALLS_}
  Inc(fRowNo);
  if Assigned(fIndexList) and (fRowNo <= fIndexList.Count) then
  begin
    fCurrentIndexColumn := fIndexList[fRowNo - 1];
    Result := DBXERR_NONE;
  end
  else
    Result := DBXERR_EOF;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataIndexes.next', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataIndexes.next'); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSqlCursorProcedures }

{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
{
Dbx returned cursor columns
 1. RECNO         fldINT32
      A record number that uniquely identifies each record.
 2. CATALOG_NAME  fldZSTRING
      The name of the catalog (database) that contains the stored procedure.
 3. SCHEMA_NAME   fldZSTRING
      The name of the schema that identifies the owner of the stored procedure.
 4. PROC_NAME     fldZSTRING
      The name of the stored procedure.
 5. PROC_TYPE     fldINT32
      An eSQLProcType value (C++) or stored procedure type constant (Object Pascal)
      that indicates the type of stored procedure.
 6. IN_PARAMS     fldINT16
      The number of input parameters.
 7. OUT_PARAMS    fldINT16
      The number of output parameters.

ODBC Result set columns from SQLProcedures
 1. PROCEDURE_CAT     Varchar
      Catalog name; NULL if not applicable to the data source
 2. PROCEDURE_SCHEM   Varchar
      Schema name; NULL if not applicable to the data source.
 3. PROCEDURE_NAME    Varchar not null
      Procedure identifier
 4. NUM_INPUT_PARAMS  N/A         Reserved for future use
 5. NUM_OUTPUT_PARAMS N/A         Reserved for future use
 6. NUM_RESULT_SETS   N/A         Reserved for future use
 7. REMARKS           Varchar
      A description of the procedure
 8. PROCEDURE_TYPE    Smallint    Defines the procedure type:
      SQL_PT_UNKNOWN:   It cannot be determined whether the procedure returns a value.
      SQL_PT_PROCEDURE: The returned object is a procedure;
       that is, it does not have a return value.
      SQL_PT_FUNCTION:  The returned object is a function;
       that is, it has a return value.
}
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}

const
  ProcedureColumnNames: array[1..7] of AnsiString = { Do not localize }
  ('RECNO', 'CATALOG_NAME', 'SCHEMA_NAME', 'PROC_NAME', 'PROC_TYPE', 'IN_PARAMS', 'OUT_PARAMS');
  ProcedureColumnTypes: array[1..7] of Word =
  (fldINT32, fldZSTRING, fldZSTRING, fldZSTRING, fldINT32, fldINT16, fldINT16);
  ProcedureColumnTypesW: array[1..7] of Word =
  (fldINT32, fldWIDESTRING, fldWIDESTRING, fldWIDESTRING, fldINT32, fldINT16, fldINT16);
  ProcedureColumnCount = Length(ProcedureColumnNames);

constructor TSqlCursorMetaDataProcedures.Create;//(ASupportWideString: Boolean; OwnerMetaData: TSQLMetaDataOdbc);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataProcedures.Create'); {$ENDIF _TRACE_CALLS_}
  inherited Create(ASupportWideString, OwnerMetaData);
  fColumnCount := ProcedureColumnCount;
  fColumnNames := @ProcedureColumnNames;
  if fSupportWideString then
    fColumnTypes := @ProcedureColumnTypesW
  else
    fColumnTypes := @ProcedureColumnTypes;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedures.Create', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedures.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TSqlCursorMetaDataProcedures.Destroy;
var
  i: Integer;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataProcedures.Destroy'); {$ENDIF _TRACE_CALLS_}
  if Assigned(fProcList) then
  begin
    for i := fProcList.Count - 1 downto 0 do
      TMetaProcedure(fProcList[i]).Free;
    FreeAndNil(fProcList);
  end;
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedures.Destroy', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedures.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCursorMetaDataProcedures.FetchProcedures;//(ProcedureName: PAnsiChar;
//  ProcType: Longword);
var
  OdbcRetcode: OdbcApi.SqlReturn;
  aMetaProcedure: TMetaProcedure;

  Cat: PAnsiChar;
  Schema: PAnsiChar;
  ProcName: PAnsiChar;
  OdbcProcType: Smallint;

  cbCat: SqlInteger;
  cbSchema: SqlInteger;
  cbProcName: SqlInteger;
  cbOdbcProcType: SqlInteger;
  aDbxConStmtInfo: TDbxConStmtInfo;
  OLDCurrentDbxConStmt: PDbxConStmt;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataProcedures.FetchProcedures', ['ProcedureName =', ProcedureName, 'ProcType =', ProcType]); {$ENDIF _TRACE_CALLS_}
  Cat := nil;
  Schema := nil;
  cbSchema := 0;
  ProcName := nil;
  fHStmt := SQL_NULL_HANDLE;
  OLDCurrentDbxConStmt := nil;

  {$IFDEF _DBXCB_}
  if Assigned(fSqlConnectionOdbc.fDbxTraceCallbackEven) then
    fSqlConnectionOdbc.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Misc, 'metadata (procedures): (%s,%d)', [
      StrPas(ProcedureName), Integer(ProcType) ]);
  {$ENDIF}

  with fSqlDriverOdbc.fOdbcApi do
  try
//
//todo: bUnicodeApi := fSupportWideString and fSqlDriverOdbc.fIsUnicodeOdbcApi and Assigned(SQLDescribeColW) and Assigned(SQLProceduresW);
//
    aDbxConStmtInfo.fDbxConStmt := nil;
    aDbxConStmtInfo.fDbxHStmtNode := nil;
    if fSqlConnectionOdbc.fStatementPerConnection > 0 then
    begin
      OLDCurrentDbxConStmt := fSqlConnectionOdbc.GetCurrentDbxConStmt();
      if fSqlConnectionOdbc.fCurrDbxConStmt = nil then
        OLDCurrentDbxConStmt := nil;
      //fSqlConnectionOdbc.fCurrDbxConStmt := aDbxConStmtInfo.fDbxConStmt;
    end;
    fSqlConnectionOdbc.AllocHStmt(fHStmt, @aDbxConStmtInfo, {bMetadataRead=}True);

    {  ProcType is a combination of flags:
       eSQLProcedure, eSQLFunction, eSQLPackage, eSQLSysProcedure
       But ODBC always returns all procedures }

    {+2.01}
    //Vadim V.Lopushansky:
    // Set Metadata CurrentSchema Filter
    // Edward> Again, I don't think any real dbxpress application will use
    // Edward> schema filter, but I leave this code sa it is harmless
    if (fSqlConnectionOdbc.fConnectionOptions[coSupportsSchemaFilter] = osOn) and
      (Length(fSqlConnectionOdbc.fCurrentSchema) > 0)
    then
    begin
      Schema := PAnsiChar(fSqlConnectionOdbc.fCurrentSchema);
      cbSchema := SQL_NTS; //Length(fSqlConnectionOdbc.fCurrentSchema);
    end;

    OdbcRetcode := SQLProcedures(fHStmt,
      nil, 0, // all catalogs
      Schema, cbSchema, // current schemas
      ProcedureName, SQL_NTS); // Procedure name match pattern

    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetcode, 'SQLProcedures');

    Schema := nil;
    if fSqlConnectionOdbc.fSupportsCatalog then
      DescribeAllocBindString(1, Cat, cbCat);
    DescribeAllocBindString(2, Schema, cbSchema);
    DescribeAllocBindString(3, ProcName, cbProcName);
    BindSmallint(8, OdbcProcType, @cbOdbcProcType);

    fCatLenMax := 0;
    fSchemaLenMax := 0;
    fProcLenMax := 1;
    fProcList := TList.Create;
    OdbcRetcode := SQLFetch(fHStmt);

    while (OdbcRetcode <> ODBCapi.SQL_NO_DATA) do
    begin

      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        OdbcCheck(OdbcRetcode, 'SQLFetch');

      aMetaProcedure := TMetaProcedure.Create(Cat, Schema, ProcName, OdbcProcType);
      fProcList.Add(aMetaProcedure);

      if Cat <> nil then
        MaxSet(fCatLenMax, StrLen(Cat));

      if Assigned(Schema) then
        MaxSet(fSchemaLenMax, StrLen(Schema));

      if Assigned(ProcName) then
        MaxSet(fProcLenMax, StrLen(ProcName));

      OdbcRetcode := SQLFetch(fHStmt);
    end;

  finally
    FreeMem(Cat);
    FreeMem(Schema);
    FreeMem(ProcName);

    if fHStmt <> SQL_NULL_HANDLE then
    begin
      // calls freehandle & sets SQL_NULL_HANDLE
      fSqlConnectionOdbc.FreeHStmt(fHStmt, @aDbxConStmtInfo);
      if (fSqlConnectionOdbc.fStatementPerConnection > 0)
        and (fSqlConnectionOdbc.fCurrDbxConStmt = nil)
      then
        fSqlConnectionOdbc.SetCurrentDbxConStmt(OLDCurrentDbxConStmt);
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedures.FetchProcedures', e);  raise; end; end;
    //except raise; end;
    finally LogExitProc('TSqlCursorMetaDataProcedures.FetchProcedures'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedures.getColumnLength;//(ColumnNumber: Word;
//  var pLength: Longword): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedures.getColumnLength', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    case ColumnNumber of
      1: // RECNO
        pLength := SizeOf(Integer);
      2: // CATALOG_NAME
        pLength := fCatLenMax;
      3: // SCHEMA_NAME
        pLength := fSchemaLenMax;
      4: // PROCEDURE_NAME
        pLength := fProcLenMax;
      5: // PROC_TYPE
        pLength := SizeOf(Integer);
      6: // IN_PARAMS
        pLength := SizeOf(Smallint);
      7: // OUT_PARAMS
        pLength := SizeOf(Smallint);
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataProcedures.getColumnLength invalid column no: '
        + IntToStr(ColumnNumber));
    end;
    {+2.01}
    // Vadim V.Lopushansky:
    // If length is equal 0 that Delphi will ignore this column.
    // It is bad, since the column describes the metadata.
    if (pLength = 0) and (ProcedureColumnTypes[ColumnNumber] = fldZSTRING) then
      pLength := 1;
    {/+2.01}
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      pLength := 0;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedures.getColumnLength', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedures.getColumnLength', ['pLength =', pLength]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedures.getColumnPrecision;//(ColumnNumber: Word;
//  var piPrecision: Smallint): SQLResult;
var
  Length: Longword;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedures.getColumnPrecision', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  Result := getColumnLength(ColumnNumber, Length);
  piPrecision := Length;
  {+2.01}
  //Vadim V.Lopushansky: Problems with loss of accuracy at type conversion
  // Edward> ???Ed>Vad: No column from SqlProcedures should ever be this big,
  // Edward> but it does no harm, and it is consistent with other changes
  if Length < Longword(High(Smallint)) then
    piPrecision := Length
  else
    piPrecision := -1;
  {/+2.01}
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedures.getColumnPrecision', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedures.getColumnPrecision', ['piPrecision =', piPrecision]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedures.getLong;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedures.getLong', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  try
    case ColumnNumber of
      1: // RECNO
        begin
          Integer(Value^) := fRowNo;
          IsBlank := False;
        end;
      5: // PROC_TYPE
        begin
          { TODO : CHECK FOR PROCEDURE TYPE - Assume Procedure for now }
          Integer(Value^) := eSQLProcedure;
          IsBlank := False;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataProcedures.getLong invalid column no: '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      Integer(Value^) := 0;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedures.getLong', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedures.getLong', ['Value =', Integer(Value^), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedures.getShort;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedures.getShort', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  try
    case ColumnNumber of
      6: // IN_PARAMS
        begin
          SmallInt(Value^) := 0;
          IsBlank := False;
        end;
      7: // OUT_PARAMS
        begin
          SmallInt(Value^) := 0;
          IsBlank := False;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataProcedures.getShort invalid column no: '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      Integer(Value^) := 0;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedures.getShort', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedures.getShort', ['Value =', SmallInt(Value^), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedures.getString;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedures.getString', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    Exit;
  end;
  try
    case ColumnNumber of
      2: // CATALOG_NAME
        begin
          if (fMetaProcedureCurrent.fCat = nil) then
          begin
            PAnsiChar(Value)^ := cNullAnsiChar;
            IsBlank := True;
          end
          else
          begin
            StrCopy(Value, PAnsiChar(fMetaProcedureCurrent.fCat));
            IsBlank := False;
          end;
        end;
      3: // SCHEMA_NAME
        begin
          if (fMetaProcedureCurrent.fSchema = nil) then
          begin
            PAnsiChar(Value)^ := cNullAnsiChar;
            IsBlank := True;
          end
          else
          begin
            StrCopy(Value, PAnsiChar(fMetaProcedureCurrent.fSchema));
            IsBlank := False;
          end;
        end;
      4: // PROCEDURE_NAME
        begin
          StrCopy(Value, PAnsiChar(fMetaProcedureCurrent.fProcName));
          IsBlank := False;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataProcedures.getString invalid column no: '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      PAnsiChar(Value)^ := cNullAnsiChar;
      IsBlank := True;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedures.getString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedures.getString', ['Value =', PAnsiChar(Value), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedures.DoGetWideString;//(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedures.DoGetWideString', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    Exit;
  end;
  try
    case ColumnNumber of
      2: // CATALOG_NAME
        begin
          if (fMetaProcedureCurrent.fWCat = nil) then
          begin
            Value^ := cNullWideChar;
            IsBlank := True;
          end
          else
          begin
            WStrCopy(Value, fMetaProcedureCurrent.fWCat);
            IsBlank := False;
          end;
        end;
      3: // SCHEMA_NAME
        begin
          if (fMetaProcedureCurrent.fWSchema = nil) then
          begin
            Value^ := cNullWideChar;
            IsBlank := True;
          end
          else
          begin
            WStrCopy(Value, fMetaProcedureCurrent.fWSchema);
            IsBlank := False;
          end;
        end;
      4: // PROCEDURE_NAME
        begin
          WStrCopy(Value, fMetaProcedureCurrent.fWProcName);
          IsBlank := False;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataProcedures.DoGetWideString invalid column no: '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      Value^ := cNullWideChar;
      IsBlank := True;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedures.DoGetWideString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedures.DoGetWideString', ['Value =', Value, 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedures.next: SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedures.next'); {$ENDIF _TRACE_CALLS_}
  Inc(fRowNo);
  if (fProcList <> nil) and
    (fRowNo <= fProcList.Count) then
  begin
    fMetaProcedureCurrent := fProcList[fRowNo - 1];
    Result := DBXERR_NONE;
  end
  else
    Result := DBXERR_EOF;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedures.next', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedures.next'); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSqlCursorMetaDataProcedureParams }

{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
{
Dbx returned cursor columns
 1.  RECNO          fldINT32
       A record number that uniquely identifies each record.
 2.  CATALOG_NAME      fldZSTRING
       The name of the catalog (database) that contains the stored procedure.
 3.  SCHEMA_NAME       fldZSTRING
       The name of the schema that identifies the owner of the stored procedure.
 4.  PROC_NAME         fldZSTRING
       The name of the procedure in which the parameter appears.
 5.  PARAM_NAME        fldZSTRING
       The name of the parameter.
 6.  PARAM_TYPE        fldINT16
       A STMTParamType value that indicates whether the parameter is used
       for input, output, or result.
 7.  PARAM_DATATYPE    fldINT16
       The logical data type for the parameter.
 8.  PARAM_SUBTYPE     fldINT16
       The logical data subtype for the parameter.
 9.  PARAM_TYPENAME    fldZSTRING
      A string describing the datatype.
      This is the same information as contained in PARAM_DATATYPE
      and PARAM_SUBTYPE, but in a form used in some DDL statements.
 10. PARAM_PRECISION   fldINT32
      The size of the parameter type
      (number of characters in a string, bytes in a bytes field,
      significant digits in a BCD value, members of an ADT, and so on)
 11. PARAM_SCALE       fldINT16
       The number of digits to the right of the decimal on BCD values,
       or descendants on ADT and array values.
 12. PARAM_LENGTH      fldINT32
       The number of bytes required to store parameter values.
 13. PARAM_NULLABLE    fldINT16
       0 if the parameter requires a value, nonzero if it can be blank.
 {+2.01}
 { Vadim V.Lopushansky: add support parameter position.
   For an example look: ($DELPHI$)\Demos\Db\DbxExplorer\dbxexplorer.dpr (Read PARAM_POSITION error).
 14. PARAM_POSITION    fldINT16
       The position of the param in its procedure.
 {/+2.01}
{
ODBC result set columns from SQLProcedureColumns
 1.  PROCEDURE_CAT        Varchar
       Procedure catalog name; NULL if not applicable to the data source.
 2.  PROCEDURE_SCHEM      Varchar
       Procedure schema name; NULL if not applicable to the data source.
 3.  PROCEDURE_NAME       Varchar not NULL
       Procedure name. An empty string is returned for a procedure
       that does not have a name.
 4.  COLUMN_NAME          Varchar not NULL
       Procedure column name. The driver returns an empty string for
       a procedure column that does not have a name.
 5.  COLUMN_TYPE          Smallint not NULL
       Defines the procedure column as a parameter or a result set column:
       SQL_PARAM_TYPE_UNKNOWN: The procedure column is a parameter whose type is unknown
       SQL_PARAM_INPUT:        The procedure column is an input parameter
       SQL_PARAM_INPUT_OUTPUT: The procedure column is an input/output parameter
       SQL_PARAM_OUTPUT:       The procedure column is an output parameter
       SQL_RETURN_VALUE:       The procedure column is the return value of the procedure
       SQL_RESULT_COL:         The procedure column is a result set column
 6.  DATA_TYPE            Smallint not NULL
       SQL data type. This can be an ODBC SQL data type or a driver-specific SQL data type.
       For datetime and interval data types, this column returns the concise
       data types (for example, SQL_TYPE_TIME or SQL_INTERVAL_YEAR_TO_MONTH)
 7.  TYPE_NAME            Varchar not NULL
       Data source – dependent data type name
 8.  COLUMN_SIZE          Integer
       The column size of the procedure column on the data source.
       NULL is returned for data types where column size is not applicable.
       For more information concerning precision, see 'Column Size, Decimal
       Digits, Transfer Octet Length, and Display Size,' in Appendix D, 'Data Types.'
 9.  BUFFER_LENGTH        Integer
      The length in bytes of data transferred on an SQLGetData or SQLFetch
      operation if SQL_C_DEFAULT is specified.
      For numeric data, this size may be different than the size of the data
      stored on the data source.
      For more information concerning precision, see 'Column Size, Decimal
      Digits, Transfer Octet Length, and Display Size,' in Appendix D, 'Data Types.'
 10. DECIMAL_DIGITS       Smallint
      The decimal digits of the procedure column on the data source.
      NULL is returned for data types where decimal digits is not applicable.
      For more information concerning decimal digits, see 'Column Size, Decimal
      Digits, Transfer Octet Length, and Display Size,' in Appendix D, 'Data Types.'
 11. NUM_PREC_RADIX       Smallint
      For numeric data types, either 10 or 2.
      If it is 10, the values in COLUMN_SIZE and DECIMAL_DIGITS give the number
      of decimal digits allowed for the column.
      For example, a DECIMAL(12,5) column would return a NUM_PREC_RADIX of 10,
      a COLUMN_SIZE of 12, and a DECIMAL_DIGITS of 5;
      a FLOAT column could return a NUM_PREC_RADIX of 10, a COLUMN_SIZE of 15
      and a DECIMAL_DIGITS of NULL.
      If it is 2, the values in COLUMN_SIZE and DECIMAL_DIGITS give the number
      of bits allowed in the column.
      For example, a FLOAT column could return a NUM_PREC_RADIX of 2,
      a COLUMN_SIZE of 53, and a DECIMAL_DIGITS of NULL.
      NULL is returned for data types where NUM_PREC_RADIX is not applicable.
 12.NULLABLE             Smallint not NULL
     Whether the procedure column accepts a NULL value:
     SQL_NO_NULLS: The procedure column does not accept NULL values.
     SQL_NULLABLE: The procedure column accepts NULL values.
     SQL_NULLABLE_UNKNOWN: It is not known if the procedure column accepts NULL values.
 13.REMARKS              Varchar
      A description of the procedure column.
 14.COLUMN_DEF           Varchar
     The default value of the column.
     If NULL was specified as the default value, then this column is
     the word NULL, not enclosed in quotation marks.
     If the default value cannot be represented without truncation, then this
     column contains TRUNCATED, with no enclosing single quotation marks.
     If no default value was specified, then this column is NULL.
     The value of COLUMN_DEF can be used in generating a new column definition,
     except when it contains the value TRUNCATED.
 15.SQL_DATA_TYPE        Smallint not NULL
      The value of the SQL data type as it appears in the SQL_DESC_TYPE field
      of the descriptor.
      This column is the same as the DATA_TYPE column, except for datetime and
      interval data types.
      For datetime and interval data types, the SQL_DATA_TYPE field in the
      result set will return SQL_INTERVAL or SQL_DATETIME,
      and the SQL_DATETIME_SUB field will return the subcode for the
      specific interval or datetime data type (see Appendix D, “Data Types”).
 16.SQL_DATETIME_SUB     Smallint
      The subtype code for datetime and interval data types.
      For other data types, this column returns a NULL.
 17.CHAR_OCTET_LENGTH    Integer
      The maximum length in bytes of a character or binary data type column.
      For all other data types, this column returns a NULL.
 18.ORDINAL_POSITION     Integer not NULL
     For input and output parameters, the ordinal position of the parameter
     in the procedure definition (in increasing parameter order, starting at 1).
     For a return value (if any), 0 is returned.
     For result-set columns, the ordinal position of the column in the result set,
     with the first column in the result set being number 1.
     If there are multiple result sets, column ordinal positions are returned in
     a driver-specific manner.
 19.IS_NULLABLE          Varchar
      'NO' if the column does not include NULLs.
      'YES' if the column can include NULLs.
      This column returns a zero-length string if nullability is unknown.
      ISO rules are followed to determine nullability.
      An ISO SQL – compliant DBMS cannot return an empty string.
      The value returned for this column is different from the value returned
      for the NULLABLE column. (See the description of the NULLABLE column.)
}
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}

const
  ProcedureParamColumnNames: array[1..14] of AnsiString = { Do not localize }
  ( {1}'RECNO', {2}'CATALOG_NAME', {3}'SCHEMA_NAME', {4}'PROC_NAME', {5}'PARAM_NAME',
    {6}'PARAM_TYPE', {7}'PARAM_DATATYPE', {8}'PARAM_SUBTYPE', {9}'PARAM_TYPENAME', {10}'PARAM_PRECISION',
    {11}'PARAM_SCALE', {12}'PARAM_LENGTH', {13}'PARAM_NULLABLE', {14}'PARAM_POSITION');
  ProcedureParamColumnTypes: array[1..14] of Word =
  ( {1}fldINT32, {2}fldZSTRING, {3}fldZSTRING, {4}fldZSTRING, {5}fldZSTRING,
    {6}fldINT16, {7}fldINT16, {8}fldINT16, {9}fldZSTRING, {10}fldINT32,
    {11}fldINT16, {12}fldINT32, {13}fldINT16, {14}fldINT16);
  ProcedureParamColumnTypesW: array[1..14] of Word =
  ( {1}fldINT32, {2}fldWIDESTRING, {3}fldWIDESTRING, {4}fldWIDESTRING, {5}fldWIDESTRING,
    {6}fldINT16, {7}fldINT16, {8}fldINT16, {9}fldWIDESTRING, {10}fldINT32,
    {11}fldINT16, {12}fldINT32, {13}fldINT16, {14}fldINT16);
  ProcedureParamColumnCount = Length(ProcedureParamColumnNames);

constructor TSqlCursorMetaDataProcedureParams.Create;//(ASupportWideString: Boolean; OwnerMetaData: TSQLMetaDataOdbc);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataProcedureParams.Create'); {$ENDIF _TRACE_CALLS_}
  inherited Create(ASupportWideString, OwnerMetaData);
  fColumnCount := ProcedureParamColumnCount;
  fColumnNames := @ProcedureParamColumnNames;
  if fSupportWideString then
    fColumnTypes := @ProcedureParamColumnTypesW
  else
    fColumnTypes := @ProcedureParamColumnTypes;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedureParams.Create', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedureParams.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

destructor TSqlCursorMetaDataProcedureParams.Destroy;
var
  i: Integer;
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataProcedureParams.Destroy'); {$ENDIF _TRACE_CALLS_}
  if Assigned(fProcColumnList) then
  begin
    for i := fProcColumnList.Count - 1 downto 0 do
      TMetaProcedureParam(fProcColumnList[i]).Free;
    FreeAndNil(fProcColumnList);
  end;
  if Assigned(fProcList) then
  begin
    for i := fProcList.Count - 1 downto 0 do
      TMetaProcedure(fProcList[i]).Free;
    FreeAndNil(fProcList);
  end;
  inherited;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedureParams.Destroy', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedureParams.Destroy'); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure TSqlCursorMetaDataProcedureParams.FetchProcedureParams;//(
//  SearchCatalogName,  SearchSchemaName,
//  SearchProcedureName,  SearchParamName: PAnsiChar);
var
  vSearchCatalogName,  vSearchSchemaName,
  vSearchProcedureName, vSearchParamName: AnsiString;
  bConverParamsToUpper: Boolean;
  OdbcRetcode: OdbcApi.SqlReturn;
  Cat: PAnsiChar;
  Schema: PAnsiChar;
  ProcName: PAnsiChar;
  ProcColumnName: PAnsiChar;
  TypeName: PAnsiChar;
  OrdinalPosition: Integer;
  bOrdinalPositionEmulate: Boolean;
  ColumnType: Smallint;
  OdbcDataType: Smallint;
  v_DECIMAL_DIGITS: Smallint;
  cbv_DECIMAL_DIGITS: Integer;
  v_NUM_PREC_RADIX: Smallint;
  cbv_NUM_PREC_RADIX: Integer;
  v_COLUMN_SIZE: Integer;
  cbv_COLUMN_SIZE: Integer;
//  v_CHAR_OCTET_LENGTH: Integer;
//  cbv_CHAR_OCTET_LENGTH: Integer;
  v_BUFFER_LENGTH: Integer;
  cbv_BUFFER_LENGTH: Integer;
  OdbcNullable: Smallint;
  cbv_OdbcNullable: Integer;

  cbCat: Integer;
  cbSchema: Integer;
  cbProcName: Integer;
  cbProcColumnName: Integer;
  cbTypeName: Integer;
  cbColumnType: Integer;
  cbOdbcDataType: Integer;
  cbOrdinalPosition: Integer;

  DbxDataType: Smallint;
  DbxDataSubType: Smallint;
  i: Integer;
  aMetaProcedure: TMetaProcedure;
  aMetaProcedureParam: TMetaProcedureParam;
  aDbxConStmtInfo: TDbxConStmtInfo;
  OLDCurrentDbxConStmt: PDbxConStmt;
  aOdbcBindParam: TOdbcBindParam;
  sTypeName: AnsiString;
  {$IFDEF _TRACE_CALLS_}
  //iFetchCount: Integer;
  {$ENDIF _TRACE_CALLS_}
  //
  // DB2: eOdbcDriverTypeIbmDb2AS400
  // https://sourceforge.net/forum/message.php?msg_id=5280484
  //
  // Added by Sebastien to remove duplicated parameter:
  SysNamingSchema: AnsiString;
  pSysNamingSchema: PAnsiChar;
  procedure RetrieveProcedureSchemaForSysNaming(pProcName: PAnsiChar);
  var
    Len: LongWord;
    Cursor: ISQLCursor25;
    IsBlank: LongBool;
    aResult: SQLResult;
  begin
    pSysNamingSchema := fMetaSchemaName;
    if (fSqlConnectionOdbc.fConnectionOptions[coSPSN] = osOn) then //Stored Proc Sys Naming
    begin
      aResult := Self.fOwnerMetaData.getProcedures(pProcName, 0, Cursor);
      if (aResult = SQL_SUCCESS) and Assigned(Cursor) then
      begin
        Cursor.getColumnLength({SCHEMA_NAME:}3, Len);
        if Len > 0 then
        begin
          SetLength(SysNamingSchema, Len + 1);
          aResult := Cursor.next;
          if aResult = SQL_SUCCESS then
          begin
            Cursor.getString({SCHEMA_NAME:}3, PAnsiChar(SysNamingSchema), IsBlank);
            if not IsBlank then
              pSysNamingSchema := PAnsiChar(SysNamingSchema)
            else
              pSysNamingSchema := nil;
          end;
        end
        else
          pSysNamingSchema := nil;
      end;
    end;
  end;

begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataProcedureParams.FetchProcedureParams', [
    'SearchCatalogName =', SearchCatalogName, 'SearchSchemaName =', SearchSchemaName,
    'SearchProcedureName =', SearchProcedureName, 'SearchParamName =', SearchParamName]);
  {$ENDIF _TRACE_CALLS_}
  Cat := nil;
  Schema := nil;
  ProcName := nil;
  ProcColumnName := nil;
  TypeName := nil;
  fHStmt := SQL_NULL_HANDLE;
  OLDCurrentDbxConStmt := nil;

  {$IFDEF _DBXCB_}
  if Assigned(fSqlConnectionOdbc.fDbxTraceCallbackEven) then
    fSqlConnectionOdbc.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Misc, 'metadata (params:enter): (%s,%s,%s,%s)', [
      StrPas(SearchCatalogName),  StrPas(SearchSchemaName),
      StrPas(SearchProcedureName), StrPas(SearchParamName) ]);
  {$ENDIF}

  with fSqlDriverOdbc.fOdbcApi do
  try
//
//todo: bUnicodeApi := fSupportWideString and fSqlDriverOdbc.fIsUnicodeOdbcApi and Assigned(SQLDescribeColW) and Assigned(SQLProceduresW);
//
    aDbxConStmtInfo.fDbxConStmt := nil;
    aDbxConStmtInfo.fDbxHStmtNode := nil;
    if fSqlConnectionOdbc.fStatementPerConnection > 0 then
    begin
      OLDCurrentDbxConStmt := fSqlConnectionOdbc.GetCurrentDbxConStmt();
      if fSqlConnectionOdbc.fCurrDbxConStmt = nil then
        OLDCurrentDbxConStmt := nil;
      //fSqlConnectionOdbc.fCurrDbxConStmt := aDbxConStmtInfo.fDbxConStmt;
    end;
    fSqlConnectionOdbc.AllocHStmt(fHStmt, @aDbxConStmtInfo, {bMetadataRead=}True);

    bConverParamsToUpper := False;
    case fSqlConnectionOdbc.fDbmsType of
      eDbmsTypeOracle:
//#0xx: + òåðÿåòñÿ èìÿ ïàêåòà
        bConverParamsToUpper := True;  //if fWantQuotedTableName and (fQuoteChar <> cNullAnsiChar) and (sObjectName <> '') then
    end; // case

    if bConverParamsToUpper then
    begin
      if (SearchCatalogName <> nil) and (SearchCatalogName^ <> cNullAnsiChar) then
      begin
        vSearchCatalogName := AnsiUpperCase(StrPas(SearchCatalogName));
        SearchCatalogName := PAnsiChar(vSearchCatalogName);
      end;
      if (SearchSchemaName <> nil) and (SearchSchemaName^ <> cNullAnsiChar) then
      begin
        vSearchSchemaName := AnsiUpperCase(StrPas(SearchSchemaName));
        SearchSchemaName := PAnsiChar(vSearchSchemaName);
      end;
      if (SearchProcedureName <> nil) and (SearchProcedureName^ <> cNullAnsiChar) then
      begin
        vSearchProcedureName := AnsiUpperCase(StrPas(SearchProcedureName));
        if (fSqlConnectionOdbc.fDbmsType = eDbmsTypeOracle) and (vSearchSchemaName <> '') then
        begin
          if (not fSqlConnectionOdbc.ObjectIsStoredProc(vSearchProcedureName))
            and fSqlConnectionOdbc.ObjectIsStoredProc(vSearchSchemaName + '.' + vSearchProcedureName) then
          begin
            vSearchProcedureName := vSearchSchemaName + '.' + vSearchProcedureName;
            vSearchSchemaName := cNullAnsiChar;
            SearchSchemaName := PAnsiChar(vSearchSchemaName);
          end;
        end;
        SearchProcedureName := PAnsiChar(vSearchProcedureName);
      end;
      if (SearchParamName <> nil) and (SearchParamName^ <> cNullAnsiChar) then
      begin
        vSearchParamName := AnsiUpperCase(StrPas(SearchParamName));
        SearchParamName := PAnsiChar(vSearchParamName);
      end;
    end;

    if (SearchParamName <> nil) then
      if (SearchParamName[0] = cNullAnsiChar) then
        SearchParamName := nil;

    if not fSqlConnectionOdbc.fSupportsCatalog then
      SearchCatalogName := nil;

    ParseTableName(SearchCatalogName, SearchSchemaName, SearchProcedureName);
    RetrieveProcedureSchemaForSysNaming(SearchProcedureName);

  {$IFDEF _DBXCB_}
  if Assigned(fSqlConnectionOdbc.fDbxTraceCallbackEven) then
    fSqlConnectionOdbc.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Misc, 'metadata (params:schema): (%s,%s,%s,%s)', [
      StrPas(fMetaCatalogName),  StrPas(pSysNamingSchema),
      StrPas(fMetaTableName), StrPas(SearchParamName) ]);
  {$ENDIF}

    OdbcRetcode := SQLProcedureColumns(fHStmt,
      fMetaCatalogName, SQL_NTS, // Catalog name
      pSysNamingSchema, SQL_NTS, // Schema name ( fMetaSchemaName )
      fMetaTableName, SQL_NTS, // Procedure name match pattern
      SearchParamName, SQL_NTS); // Column name match pattern
    if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
      OdbcCheck(OdbcRetcode, 'SQLProcedureColumns');

    if fSqlConnectionOdbc.fSupportsCatalog then
      DescribeAllocBindString(1, Cat, cbCat);
    if (fSqlConnectionOdbc.fOdbcMaxSchemaNameLen > 0) then
      DescribeAllocBindString(2, Schema, cbSchema);
    DescribeAllocBindString(3, ProcName, cbProcName);
    DescribeAllocBindString(4, ProcColumnName, cbProcColumnName);
    BindSmallint(5, ColumnType, @cbColumnType);
    BindSmallint(6, OdbcDataType, @cbOdbcDataType);
    DescribeAllocBindString(7, TypeName, cbTypeName);
    //Vadim V.Lopushansky: Reading of the information about types of parameters
    BindInteger(8, v_COLUMN_SIZE, @cbv_COLUMN_SIZE);
    BindInteger(9, v_BUFFER_LENGTH, @cbv_BUFFER_LENGTH);
    BindSmallint(10, v_DECIMAL_DIGITS, @cbv_DECIMAL_DIGITS);
    BindSmallint(11, v_NUM_PREC_RADIX, @cbv_NUM_PREC_RADIX);
//    try
//      BindInteger(17, v_CHAR_OCTET_LENGTH, @cbv_CHAR_OCTET_LENGTH);
//    except
//      v_CHAR_OCTET_LENGTH := -1;
//    end;
    v_DECIMAL_DIGITS := 0;
    v_NUM_PREC_RADIX := 0;
    v_COLUMN_SIZE := 0;
    v_BUFFER_LENGTH := 0;
//    v_CHAR_OCTET_LENGTH := 0;
    {/+2.01}
    BindSmallint(12, OdbcNullable, @cbv_OdbcNullable{nil}); // NULLABLE
    try
      BindInteger(18, OrdinalPosition, @cbOrdinalPosition);
      bOrdinalPositionEmulate := False;
    except
      OrdinalPosition := -1;
      bOrdinalPositionEmulate := True;
    end;

    fProcList := TList.Create;
    fProcColumnList := TList.Create;

    OdbcRetcode := SQLFetch(fHStmt);
    {$IFDEF _TRACE_CALLS_}
    //  iFetchCount := 0;
    //  LogInfoProc('SQLFetch (#1) = ' + IntToStr(OdbcRetcode));
    {$ENDIF _TRACE_CALLS_}

    while (OdbcRetcode <> ODBCapi.SQL_NO_DATA) do
    begin

      if OdbcRetcode <> OdbcApi.SQL_SUCCESS then
        OdbcCheck(OdbcRetcode, 'SQLFetch');
      {+2.01}
      //Vadim V.Lopushansky: The code for drivers which not supporting filter
      // (Easysoft IB6 ODBC Driver [ver:1.00.01.67] contain its error).
      // Edward> Again, I don't think a real dbxpress application will use filter,
      // Edward> but I leave the code, as it is correct
      if Assigned(SearchParamName) then
        i := StrLen(SearchParamName)
      else
        i := 0;
      if (i > 0) and ((i <> Integer(StrLen(ProcColumnName))) or
        (StrLComp(SearchParamName, ProcColumnName, i) <> 0)) then
      begin
        OdbcRetcode := SQLFetch(fHStmt);
        {$IFDEF _TRACE_CALLS_}
        //  inc(iFetchCount);
        //  LogInfoProc('SQLFetch (#' + IntToStr(iFetchCount) + ') = ' + IntToStr(OdbcRetcode));
        {$ENDIF _TRACE_CALLS_}
        continue;
      end;
      {/+2.01}

      if (ColumnType <> SQL_RESULT_COL) then
      begin
        aMetaProcedure := TMetaProcedure.Create(Cat, Schema, ProcName, 0);
        fProcList.Add(aMetaProcedure);
        aMetaProcedureParam := TMetaProcedureParam.Create(ProcColumnName);
        fProcColumnList.Add(aMetaProcedureParam);
        //Correction to reference from ProcedureParam to Procedure
        aMetaProcedureParam.fMetaProcedure := aMetaProcedure;
        case ColumnType of
          SQL_PARAM_TYPE_UNKNOWN:
            aMetaProcedureParam.fParamType := {DBXpress.}paramUNKNOWN;
          SQL_PARAM_INPUT:
            aMetaProcedureParam.fParamType := {DBXpress.}paramIN;
          SQL_PARAM_INPUT_OUTPUT:
            aMetaProcedureParam.fParamType := {DBXpress.}paramINOUT;
          SQL_PARAM_OUTPUT:
            aMetaProcedureParam.fParamType := {DBXpress.}paramOUT;
          SQL_RETURN_VALUE:
            aMetaProcedureParam.fParamType := {DBXpress.}paramRET;
          SQL_RESULT_COL: ; // Already discarded
        end;

        //Vadim V.Lopushansky: Calculating metadata:
        if (cbv_BUFFER_LENGTH = OdbcAPi.SQL_NULL_DATA) then
          aMetaProcedureParam.fLength := Low(Integer) // this indicates null data
        else
          aMetaProcedureParam.fLength := v_BUFFER_LENGTH;

        if cbv_DECIMAL_DIGITS = OdbcAPi.SQL_NULL_DATA then
          aMetaProcedureParam.fScale := Low(Smallint) // this indicates null data
        else
          aMetaProcedureParam.fScale := v_DECIMAL_DIGITS;

        if cbv_COLUMN_SIZE = OdbcAPi.SQL_NULL_DATA then
          aMetaProcedureParam.fPrecision := Low(Integer) // this indicates null data
        else
        begin
          if (cbv_NUM_PREC_RADIX <> OdbcAPi.SQL_NULL_DATA) and (v_NUM_PREC_RADIX = 2) then
            aMetaProcedureParam.fPrecision := ((v_COLUMN_SIZE * 3) div 10) + 1
          else
            aMetaProcedureParam.fPrecision := v_COLUMN_SIZE
        end;
        OdbcDataTypeToDbxType(OdbcDataType, DbxDataType, DbxDataSubType, fSqlConnectionOdbc,
          fSqlConnectionOdbc.fConnectionOptions[coEnableUnicode] = osOn);

        if DbxDataType = fldUNKNOWN then
          raise EDbxInternalError.Create('Unsupported ODBC data type ' + IntToStr(OdbcDataType));

        aMetaProcedureParam.fDataType := DbxDataType;
        aMetaProcedureParam.fDataSubtype := DbxDataSubType;
        StrClone(TypeName, aMetaProcedureParam.fDataTypeName);

        if (OdbcNullable <> SQL_NULLABLE) then
          aMetaProcedureParam.fNullable := 0 // Requires a value
        else
          aMetaProcedureParam.fNullable := 1; // Does not require a value
        if bOrdinalPositionEmulate then
          inc(OrdinalPosition);
        aMetaProcedureParam.fPosition := OrdinalPosition;
      end; //of: if (ColumnType <> SQL_Result_COL)

      {+2.01}
      v_DECIMAL_DIGITS := 0;
      v_NUM_PREC_RADIX := 0;
      v_COLUMN_SIZE := 0;
//      v_CHAR_OCTET_LENGTH := 0;
      v_BUFFER_LENGTH := 0;
      {/+2.01}

      OdbcRetcode := SQLFetch(fHStmt);
      {$IFDEF _TRACE_CALLS_}
      //  inc(iFetchCount);
      //  LogInfoProc('SQLFetch (#' + IntToStr(iFetchCount) + ') = ' + IntToStr(OdbcRetcode));
      {$ENDIF _TRACE_CALLS_}
    end; //of: while (OdbcRetCode <> ODBCapi.SQL_NO_DATA)

    // SqlExpr.pas: "List index out of bounds (0)"
    // For a case when metadatas are not returned (SqlExpr.pas: procedure SetProcedureParams).
    // 3.0.26: +:
    //(*
    if (fProcList.Count <= 1) and (fMetaTableName <> nil) and (SearchParamName = nil)
      and (fSqlConnectionOdbc.fLastStoredProc <> nil) and (fSqlConnectionOdbc.fLastStoredProc.fStoredProc = 1)
      and (fSqlConnectionOdbc.fLastStoredProc.fOdbcParamList <> nil)
      and (fSqlConnectionOdbc.fLastStoredProc.fOdbcParamList.Count > 0)
    then
    begin
      i := 0;

      //sTypeName := UpperCase(fSqlConnectionOdbc.EncodeObjectFullName(fMetaCatalogName,
      //  fMetaSchemaName, fMetaTableName));
      //if sTypeName = UpperCase(fSqlConnectionOdbc.fLastStoredProc.fSql) then
      //  i := 1;
      if  (StrCompNil(fMetaTableName, PAnsiChar(fSqlConnectionOdbc.fLastStoredProc.fSql)) = 0)
        and (StrCompNil(fMetaCatalogName, PAnsiChar(fSqlConnectionOdbc.fLastStoredProc.fCatalogName)) = 0)
        and (StrCompNil(fMetaSchemaName, PAnsiChar(fSqlConnectionOdbc.fLastStoredProc.fSchemaName)) = 0)
      then
        i := 1;

      if (i <> 0)then
      begin
        i := fProcColumnList.Count ;
        if i < fSqlConnectionOdbc.fLastStoredProc.fOdbcParamList.Count then
        begin
          {$IFDEF _TRACE_CALLS_}
          //LogInfoProc('# Warning (potential problem): SqlExpr.pas: procedure SetProcedureParams: "List index out of bounds"');
          //LogInfoProc('# Read the file of "ChangesLog.Txt" (search reference:  "P#001: "List index out of bounds" ).');
          LogInfoProc('# Fix "SqlExpr.pas:SetProcedureParams": List index out of bounds');
          {$ENDIF _TRACE_CALLS_}
          if fProcList.Count = 0 then
          begin
            aMetaProcedure := TMetaProcedure.Create(fMetaCatalogName, fMetaSchemaName, SearchProcedureName, 0);
            fProcList.Add(aMetaProcedure);
          end
          else
            aMetaProcedure := TMetaProcedure(fProcList[0]);
          for i:= i to fSqlConnectionOdbc.fLastStoredProc.fOdbcParamList.Count - 1 do
          begin
            aOdbcBindParam := TOdbcBindParam(fSqlConnectionOdbc.fLastStoredProc.fOdbcParamList.Items[i]);

            if bOrdinalPositionEmulate then
              inc(OrdinalPosition);
            aMetaProcedureParam := TMetaProcedureParam.Create(PAnsiChar(AnsiString('@fix_' + IntToStr(OrdinalPosition))));
            fProcColumnList.Add(aMetaProcedureParam);
            aMetaProcedureParam.fMetaProcedure := aMetaProcedure;
            case aOdbcBindParam.fOdbcInputOutputType of
              SQL_PARAM_TYPE_UNKNOWN:
                aMetaProcedureParam.fParamType := {DBXpress.}paramIN;
              SQL_PARAM_INPUT:
                aMetaProcedureParam.fParamType := {DBXpress.}paramIN;
              SQL_PARAM_INPUT_OUTPUT:
                aMetaProcedureParam.fParamType := {DBXpress.}paramINOUT;
              SQL_PARAM_OUTPUT:
                aMetaProcedureParam.fParamType := {DBXpress.}paramOUT;
              SQL_RETURN_VALUE:
                aMetaProcedureParam.fParamType := {DBXpress.}paramRET;
              SQL_RESULT_COL: ; // Already discarded
            end;
            aMetaProcedureParam.fDataType := aOdbcBindParam.fDbxType;
            aMetaProcedureParam.fDataSubtype := aOdbcBindParam.fDbxSubType;
            aMetaProcedureParam.fLength := aOdbcBindParam.fBindOutputBufferLength;
            aMetaProcedureParam.fPrecision := aOdbcBindParam.fOdbcParamCbColDef;
            aMetaProcedureParam.fScale := aOdbcBindParam.fOdbcParamIbScale;
            aMetaProcedureParam.fNullable := 1; // Does not require a value
            sTypeName := 'UNKNOWN';
            case aMetaProcedureParam.fDataType of
              fldZSTRING:
                begin
                  if (aOdbcBindParam.fDbxSubType and fldstWIDEMEMO) = 0 then
                    sTypeName := 'VARCHAR'
                  else
                    sTypeName := 'NVARCHAR';
                end;
              fldWIDESTRING, fldUNICODE:
                sTypeName := 'NVARCHAR';
              fldINT32, fldUINT32,
              fldINT16, fldUINT16:
                sTypeName := 'INTEGER';
              fldFLOAT:
                sTypeName := 'FLOAT';
              fldDATE:
                sTypeName := 'DATE';
              fldTIME:
                sTypeName := 'TIME';
              fldDATETIME:
                sTypeName := 'DATETIME';
              fldTIMESTAMP:
                sTypeName := 'TIMESTAMP';
              fldBCD, fldFMTBCD:
                sTypeName := AnsiString(Format('DECIMAL(%d,%d)', [aMetaProcedureParam.fPrecision, aMetaProcedureParam.fScale]));
              fldBOOL:
                sTypeName := 'BYTE';
              fldBLOB:
                case aMetaProcedureParam.fDataSubtype of
                  fldstMEMO:
                    sTypeName := 'MEMO';
                  fldstBINARY:
                    sTypeName := 'BYTES';
                  fldstFMTMEMO:
                    sTypeName := 'MEMO';
                  fldstOLEOBJ:
                    sTypeName := 'BYTES';
                  fldstGRAPHIC:
                    sTypeName := 'IMAGE';
                  fldstDBSOLEOBJ:
                    sTypeName := 'BYTES';
                  fldstTYPEDBINARY:
                    sTypeName := 'BYTES';
                  fldstACCOLEOBJ:
                    sTypeName := 'BYTES';
                  fldstWIDEMEMO:
                    sTypeName := 'MEMO';
                  fldstHMEMO:
                    sTypeName := 'CLOB';
                  fldstHBINARY:
                    sTypeName := 'BLOB';
                  fldstBFILE:
                    sTypeName := 'FILE';
                  else
                    sTypeName := 'BYTES';
                end;
              fldBYTES, fldVARBYTES:
                sTypeName := 'VARBYTES';
              fldCursor:
                sTypeName := 'CURSOR';
            end;
            GetMem(aMetaProcedureParam.fDataTypeName, Length(sTypeName) + 1);
            StrCopy(aMetaProcedureParam.fDataTypeName, PAnsiChar(sTypeName));
            aMetaProcedureParam.fPosition := OrdinalPosition;
          end;
        end;
      end;
    end;
    //*)
    //3.0.26: +.

  {$IFDEF _DBXCB_}
  if Assigned(fSqlConnectionOdbc.fDbxTraceCallbackEven) then
    fSqlConnectionOdbc.DbxCallBackSendMsgFmt(cTDBXTraceFlags_Misc, 'metadata (params:count): (%d)', [
      fProcList.Count ]);
  {$ENDIF}

    fCatLenMax := 1;
    fSchemaLenMax := 1;
    fProcNameLenMax := 1;
    fParamNameLenMax := 1;
    fDataTypeNameLenMax := 1;

    for i := 0 to fProcList.Count - 1 do
    begin
      aMetaProcedure := TMetaProcedure(fProcList.Items[i]);

      if Assigned(aMetaProcedure.fCat) then
        MaxSet(fCatLenMax, StrLen(aMetaProcedure.fCat));

      if Assigned(aMetaProcedure.fSchema) then
        MaxSet(fSchemaLenMax, StrLen(aMetaProcedure.fSchema));

      if Assigned(aMetaProcedure.fProcName) then
        MaxSet(fProcNameLenMax, StrLen(aMetaProcedure.fProcName));
    end;

    for i := 0 to fProcColumnList.Count - 1 do
    begin
      aMetaProcedureParam := TMetaProcedureParam(fProcColumnList.Items[i]);

      if Assigned(aMetaProcedureParam.fParamName) then
        MaxSet(fParamNameLenMax, StrLen(aMetaProcedureParam.fParamName));

      if Assigned(aMetaProcedureParam.fDataTypeName) then
        MaxSet(fDataTypeNameLenMax, StrLen(aMetaProcedureParam.fDataTypeName));
    end;

  finally
    FreeMem(Cat);
    FreeMem(Schema);
    FreeMem(ProcName);
    FreeMem(ProcColumnName);
    FreeMem(TypeName);

    if (fHStmt <> SQL_NULL_HANDLE) then
    begin
      // calls freehandle & sets SQL_NULL_HANDLE
      fSqlConnectionOdbc.FreeHStmt(fHStmt, @aDbxConStmtInfo);
      if (fSqlConnectionOdbc.fStatementPerConnection > 0)
        and (fSqlConnectionOdbc.fCurrDbxConStmt = nil)
      then
        fSqlConnectionOdbc.SetCurrentDbxConStmt(OLDCurrentDbxConStmt);
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedureParams.FetchProcedureParams', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedureParams.FetchProcedureParams', AnsiString('ProcList.Count = ' + IntToStr(fProcList.Count))); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedureParams.getColumnLength;//(ColumnNumber: Word;
//  /var pLength: Longword): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedureParams.getColumnLength', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  try
    case ColumnNumber of
      1: // RECNO
        pLength := SizeOf(Integer);
      2: // CATALOG_NAME
        pLength := fCatLenMax;
      3: // SCHEMA_NAME
        pLength := fSchemaLenMax;
      4: // PROCEDURE_NAME
        pLength := fProcNameLenMax;
      5: // PARAM_NAME
        pLength := fParamNameLenMax;
      6: // PARAM_TYPE
        pLength := SizeOf(Smallint);
      7: // PARAM_DATATYPE
        pLength := SizeOf(Smallint);
      8: // PARAM_SUBTYPE
        pLength := SizeOf(Smallint);
      9: // PARAM_TYPENAME
        pLength := fDataTypeNameLenMax;
      10: // PARAM_PRECISION
        pLength := SizeOf(Integer);
      11: // PARAM_SCALE
        pLength := SizeOf(Smallint);
      12: // PARAM_LENGTH
        pLength := SizeOf(Integer);
      13: // PARAM_NULLABLE
        pLength := SizeOf(Smallint);
      14: // PARAM_POSITION
        pLength := SizeOf(Smallint);
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataProcedureParams.getColumnLength invalid column no: '
        + IntToStr(ColumnNumber));
    end;
    {+2.01}
    // Vadim V.Lopushansky:
    // If length is equal 0 that Delphi will ignore this column.
    // It is bad, since the column describes the metadata.
    if (pLength = 0) and (ProcedureParamColumnTypes[ColumnNumber] = fldZSTRING) then
      pLength := 1;
    {/+2.01}
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      pLength := 0;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedureParams.getColumnLength', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedureParams.getColumnLength', ['pLength =', pLength]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedureParams.getColumnPrecision;//(ColumnNumber: Word;
//  var piPrecision: Smallint): SQLResult;
var
  Length: Longword;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedureParams.getColumnPrecision', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  Result := getColumnLength(ColumnNumber, Length);
  piPrecision := Length;
  {+2.01}
  //Vadim V.Lopushansky: Problems with loss of accuracy at type conversion
  // Edward> ???Ed>Vad: SqlProcedureColumns should never return such a long column
  if Length < Longword(High(Smallint)) then
    piPrecision := Length
  else
    piPrecision := -1;
  {/+2.01}
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedureParams.getColumnPrecision', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedureParams.getColumnPrecision', ['piPrecision =', piPrecision]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedureParams.getLong;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedureParams.getLong', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  try
    case ColumnNumber of
      1: // RECNO
        begin
          Integer(Value^) := fRowNo;
          IsBlank := False;
        end;
      10: // PARAM_PRECISION
        begin
          Integer(Value^) := fMetaProcedureParamCurrent.fPrecision;
          IsBlank := False;
        end;
      12: // PARAM_LENGTH
        begin
          Integer(Value^) := fMetaProcedureParamCurrent.fLength;
          IsBlank := False;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataProcedures.getLong invalid column no: '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      Integer(Value^) := 0;
      IsBlank := True;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedureParams.getLong', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedureParams.getLong', ['Value =', Integer(Value^), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedureParams.getShort;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedureParams.getShort', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  try
    case ColumnNumber of
      6: // PARAM_TYPE
        begin
          Smallint(Value^) := Smallint(fMetaProcedureParamCurrent.fParamType);
          IsBlank := False;
        end;
      7: // PARAM_DATATYPE
        begin
          Smallint(Value^) := fMetaProcedureParamCurrent.fDataType;
          IsBlank := False;
        end;
      8: // PARAM_SUBTYPE
        begin
          Smallint(Value^) := fMetaProcedureParamCurrent.fDataSubType;
          IsBlank := False;
        end;
      11: // PARAM_SCALE
        begin
          Smallint(Value^) := fMetaProcedureParamCurrent.fScale;
          IsBlank := False;
        end;
      13: // PARAM_NULLABLE
        begin
          Smallint(Value^) := fMetaProcedureParamCurrent.fNullable;
          IsBlank := False;
        end;
      14: // PARAM_POSITION
        begin
          Smallint(Value^) := fMetaProcedureParamCurrent.fPosition;
          IsBlank := False;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataProcedures.getShort invalid column no: '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      Smallint(Value^) := 0;
      IsBlank := True;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedureParams.getShort', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedureParams.getShort', ['Value =', Smallint(Value^), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedureParams.getString;//(ColumnNumber: Word;
//  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedureParams.getString', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    Exit;
  end;
  try
    case ColumnNumber of
      2: // CATALOG_NAME
        begin
          IsBlank := fMetaProcedureParamCurrent.fMetaProcedure.fCat = nil;
          if not IsBlank then
            StrCopy(Value, fMetaProcedureParamCurrent.fMetaProcedure.fCat)
          else
            PAnsiChar(Value)^ := cNullAnsiChar;
        end;
      3: // SCHEMA_NAME
        begin
          IsBlank := fMetaProcedureParamCurrent.fMetaProcedure.fSchema = nil;
          if not IsBlank then
            StrCopy(Value, fMetaProcedureParamCurrent.fMetaProcedure.fSchema)
          else
            PAnsiChar(Value)^ := cNullAnsiChar;
        end;
      4: // PROC_NAME
        begin
          IsBlank := fMetaProcedureParamCurrent.fMetaProcedure.fProcName = nil;
          if not IsBlank then
            StrCopy(Value, fMetaProcedureParamCurrent.fMetaProcedure.fProcName)
          else
            PAnsiChar(Value)^ := cNullAnsiChar;
        end;
      5: // PARAM_NAME
        begin
          IsBlank := fMetaProcedureParamCurrent.fParamName = nil;
          if not IsBlank then
            StrCopy(Value, fMetaProcedureParamCurrent.fParamName)
          else
            PAnsiChar(Value)^ := cNullAnsiChar;
        end;
      9: // PARAM_TYPENAME
        begin
          IsBlank := fMetaProcedureParamCurrent.fDataTypeName = nil;
          if not IsBlank then
            StrCopy(Value, fMetaProcedureParamCurrent.fDataTypeName)
          else
            PAnsiChar(Value)^ := cNullAnsiChar;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataProcedures.getString invalid column no: '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      PAnsiChar(Value)^ := cNullAnsiChar;
      IsBlank := True;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedureParams.getString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedureParams.getString', ['Value =', PAnsiChar(Value), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedureParams.DoGetWideString;//(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedureParams.DoGetWideString', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    Exit;
  end;
  try
    case ColumnNumber of
      2: // CATALOG_NAME
        begin
          IsBlank := fMetaProcedureParamCurrent.fMetaProcedure.fWCat = nil;
          if not IsBlank then
            WStrCopy(Value, fMetaProcedureParamCurrent.fMetaProcedure.fWCat)
          else
            Value^ := cNullWideChar;
        end;
      3: // SCHEMA_NAME
        begin
          IsBlank := fMetaProcedureParamCurrent.fMetaProcedure.fWSchema = nil;
          if not IsBlank then
            WStrCopy(Value, fMetaProcedureParamCurrent.fMetaProcedure.fWSchema)
          else
            Value^ := cNullWideChar;
        end;
      4: // PROC_NAME
        begin
          IsBlank := fMetaProcedureParamCurrent.fMetaProcedure.fWProcName = nil;
          if not IsBlank then
            WStrCopy(Value, fMetaProcedureParamCurrent.fMetaProcedure.fWProcName)
          else
            Value^ := cNullWideChar;
        end;
      5: // PARAM_NAME
        begin
          IsBlank := fMetaProcedureParamCurrent.fWParamName = nil;
          if not IsBlank then
            WStrCopy(Value, fMetaProcedureParamCurrent.fWParamName)
          else
            Value^ := cNullWideChar;
        end;
      9: // PARAM_TYPENAME
        begin
          IsBlank := fMetaProcedureParamCurrent.fWDataTypeName = nil;
          if not IsBlank then
            WStrCopy(Value, fMetaProcedureParamCurrent.fWDataTypeName)
          else
            Value^ := cNullWideChar;
        end;
    else
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataProcedures.DoGetWideString invalid column no: '
        + IntToStr(ColumnNumber));
    end;
    Result := DBXERR_NONE;
  except
    on E: Exception{EDbxError} do
    begin
      Value^ := cNullWideChar;
      IsBlank := True;
      fSqlCursorErrorMsg.Add(E.Message);
      Result := DBX_FIRSTSTATICERRORS;
      {$IFDEF _TRACE_CALLS_} if not (E is EDbxError) then raise; {$ENDIF _TRACE_CALLS_}
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedureParams.DoGetWideString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedureParams.DoGetWideString', ['Value =', Value, 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedureParams.next: SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedureParams.next'); {$ENDIF _TRACE_CALLS_}
  Inc(fRowNo);
  if Assigned(fProcColumnList) and (fRowNo <= fProcColumnList.Count) then
  begin
    fMetaProcedureParamCurrent := fProcColumnList[fRowNo - 1];
    Result := DBXERR_NONE;
  end
  else
    Result := DBXERR_EOF;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedureParams.next', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedureParams.next'); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TBlobChunkCollection }

procedure TBlobChunkCollection.AddFragment;//(Data: Pointer; DataSize: LongInt);
var
  vFragment: PBlobChunkCollectionItem;
begin
  vFragment := New(PBlobChunkCollectionItem);

  vFragment.Data := Data;
  vFragment.Size := DataSize;
  vFragment.NextFragment := nil;

  if fFragments <> nil then
    fFragmentLast.NextFragment := vFragment
  else
    fFragments := vFragment;
  fFragmentLast := vFragment;
  inc(fSize, DataSize);
  {$IFDEF _DEBUG_}
  inc(fCount, 1);
  {$ENDIF}
end;

procedure TBlobChunkCollection.Clear;
var
  vFragment: PBlobChunkCollectionItem;
begin
  while fFragments <> nil do
  begin
    vFragment := fFragments;
    fFragments := fFragments.NextFragment;
    FreeMem(vFragment.Data);
    Dispose(vFragment);
    {$IFDEF _DEBUG_}
    Dec(fCount); // <=: debug.
    {$ENDIF}
  end;
  {$IFDEF _DEBUG_}
  fCount := 0;
  {$ENDIF}
  fFragmentLast := nil;
end;

destructor TBlobChunkCollection.Destroy;
begin
  Clear;
  inherited;
end;

function TBlobChunkCollection.GetSize: Int64;
begin
  Result := fSize;
end;

procedure TBlobChunkCollection.Read;//(var Buffer: Pointer);
var
  pDest: Pointer;
  vFragment: PBlobChunkCollectionItem;
begin
  if Buffer = nil then
    Exit;
  pDest := Buffer;
  vFragment := fFragments;
  while vFragment <> nil do
  begin
    // *** debug: IntToHex(Ord(vFragment.Data[vFragment.Size-2]),2)
    Move(vFragment.Data^, pDest^, vFragment.Size);
    // *** debug: IntToHex(Ord(PAnsiChar(pDest)[vFragment.Size-1]),2)
    pDest := Pointer( DWORD(pDest) + DWORD(vFragment.Size) );
    // *** debug: IntToHex(Ord(PAnsiChar(pDest)[0]),2)
    vFragment := vFragment.NextFragment;
  end;
end;

function TBlobChunkCollection.ReadBlobToVariant;//(out Data: Variant): int64;
//var
//  P: Pointer;
begin
  Result := 0;
  Data := varNull;
  {
  if fSize <= 0 then
  begin
    Result := 0;
    Data := varNull;
  end
  else
  begin
     Result := fSize;
     Data := VarArrayCreate([0, fSize-1], varByte);
     P := VarArrayLock(Data);
     try
       Read(P);
     finally
       VarArrayUnLock(Data);
     end;
  end;
  }
end;

function TBlobChunkCollection.ReadBlobToStream;//(Stream: ISequentialStream): Int64;
var
  h: HResult;
  iTransfered: Longint;
  vFragment: PBlobChunkCollectionItem;
begin
  if (fSize <= 0) or (Stream = nil) then
  begin
    Result := 0;
  end
  else
  begin
     Result := 0;
     vFragment := fFragments;
     while vFragment <> nil do
     begin
       iTransfered := 0;
       h := Stream.Write( vFragment.Data, vFragment.Size, @iTransfered );
       if (h <> S_OK) then
         Break;
       inc(Result, iTransfered);
       if iTransfered <> vFragment.Size then
         Break;
       vFragment := vFragment.NextFragment;
     end;
  end;
end;

procedure DoRegisterDbXpressLibA();
begin
{$IFDEF MSWINDOWS}
  {$IFNDEF _D11UP_}
    SqlExpr.RegisterDbXpressLib(@getSQLDriverODBC);
  {$ENDIF}
{$ENDIF}
end;

procedure DoRegisterDbXpressLibAW();
begin
{$IFDEF MSWINDOWS}
  {$IFNDEF _D11UP_}
    SqlExpr.RegisterDbXpressLib(@getSQLDriverODBCAW);
  {$ENDIF}
{$ENDIF}
end;

initialization
begin
  IsMultiThread := True;
{$IFDEF _TRACE_CALLS_}
  LogInfoProc(['** DbxOpenOdbcVersion =', DbxOpenOdbcVersion]);
  LogInfoProc(['** RegExprParser =', {$IFDEF _RegExprParser_}'On'{$ELSE}'Off'{$ENDIF}]);
  LogInfoProc(['** InternalCloneConnection =',{$IFDEF _InternalCloneConnection_}'On'{$ELSE}'Off'{$ENDIF}]);
  LogInfoProc(['** MultiRowsFetch =',{$IFDEF _MULTIROWS_FETCH_}'On'{$ELSE}'Off'{$ENDIF}]);
  LogInfoProc(['** MixedFetch =',{$IFDEF _MIXED_FETCH_}'On'{$ELSE}'Off'{$ENDIF}]);
{$ENDIF _TRACE_CALLS_}
  RegisterDbXpressLibProc(DoRegisterDbXpressLibA, oaAnsi);
  RegisterDbXpressLibProc(DoRegisterDbXpressLibAW, oaAnsiToUnicode);

// Deprecated:
//  This allows option of static linking the DbExpress driver into your app
//{$IFDEF MSWINDOWS}
//  {$IFNDEF _D11UP_}
//    DoRegisterDbXpressLibA();
//  {$ENDIF}
//{$ENDIF}

end;

finalization
begin

// Deprecated:
//  This allows option of static linking the DbExpress driver into your app
//{$IFDEF MSWINDOWS}
//{$ENDIF}

end;

end.
