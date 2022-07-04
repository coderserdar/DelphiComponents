{
  Kylix / Delphi open source DbExpress driver for ODBC
  Version 3.205 2008-11-10

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
unit DbxOpenOdbcInterface;

interface

{.$i DbxOpenOdbc.inc}

{$UNDEF _D10UP_}
{$UNDEF _D11UP_}

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF Declared(CompilerVersion)}
    {$IF CompilerVersion >= 18.00}
      // Delphi 2006 and up
      {$DEFINE _D10UP_}
    {$IFEND}
    {$IF CompilerVersion >= 18.50}
      // Delphi 2007 and up
      {$DEFINE _D11UP_}
    {$IFEND}
  {$IFEND} // of: $IF Declared(CompilerVersion)
{$ELSE}  // of: $IFDEF CONDITIONALEXPRESSIONS
  {$MESSAGE ERROR 'Compiler must be one of the following, or higher: Delphi 6; C++ Builder 6; Kylix 1'}
{$ENDIF}

uses
  Classes;

{ --- DBXpress.pas --- }

{$IFNDEF _D10UP_}
const
  DBXERR_MISSINGPARAMINSQL       = $001A;
  DBXERR_NOTIMPLEMENT            = $001B;
  DBXERR_DRIVERINCOMPATIBLE      = $001C;
{$ENDIF}

{ --- DBXpress.pas --- }

{ --- SqlExpr.pas --- }

{$IFNDEF _D7UP_}
const
  CUSTOM_INFO        = 'Custom String';         { Do not localize }
{$ENDIF}

{ --- SqlExpr.pas --- }

{$IFNDEF _D10UP_}

{ --- DSIntf.pas --- }
const
  { Additional (non-BDE fieldtypes }
  fldWIDESTRING       = 26; // == DSIntf.fldWIDESTRING or DSIntf.fldUNICODE;   { Unicode }
  fldstWIDEMEMO       = 32; // == DSIntf.fldstWIDEMEMO or DSIntf.fldstUNICODE; { WideString Memo }
{ --- DSIntf.pas --- }

{ --- SqlExpr.pas --- }
  DECIMALSEPARATOR   = 'Decimal Separator';     { Do not localize }
{ --- SqlExpr.pas --- }

{$ENDIF}

type
  TConnectionOption = (
    // Connection features:
    coSafeMode,                // Safe mode: ignored many errors (silent mode). Default is True.
    coInternalCloneConnection, // redefine strategy for cloning connectuion (default is true).
                               // Wneh is false then cloning connection is managed by SqlExpr.pas
                               // else it is managed internal.
    coBlobChunkSize,           // redefines blob chunk size.
    coNetwrkPacketSize,        // redefines network packet size (default=4096).
    coReadOnly,                // redefines read only connection mode (defaut=false).
    coCatalogPrefix,           // redefines "catalog prefix format" for odbc connection string
                               // (default='DATABASE').
    coConTimeout,              // connection timeout limitation (in sec).
                               // <= 0 - not limitation
                               // >0   - timeout seconds
                               // Default value is  300 = 60 * 5 = 5 min
    coNumericSeparator,        // DecimalSeparator
    coConnectionString,        // ConnectionString over parameter CUSTOM_INFO
    // Metada features:
    coSupportsMetadata,        // redefines supports metadata.
    coSupportsCatalog,         // redefines supports catalog.
    coSupportsSchemaFilter,    // redefines Supports Schema Filter (Oracle).
    // BindField features:
    coMapInt64ToBcd,           // define mapping big int to bcd(9,0) (default=True).
    coMapSmallBcdToNative,     // define mapping bcd les then bcd(9,0) to native type
                               // (improving perfomance).
    coIgnoreUnknownFieldType,  // allow ignoring unsupported fields in select query (default=false;
                               // for informix=true)
    coMapCharAsBDE,            // redefines mapping string fields as old BDE strategy
                               // (default=false).
    coEnableBCD,               // redefine supports bcd (default is true).
    coMaxBCD,                  // redefine bcd size to bcd(32,?) (default is false).
    coEnableUnicode,           // redefine supports unicode (default=false).
    coSupportsAutoInc,         // redefine supports AutInc (default=true)
    coFldReadOnly,             // redefine supports odbc read only field attributes (default=true)
    // Field & Params features:
    coTrimChar,                // define trimm char for fixed char field (default=false).
    coEmptyStrParam,           // when is false then empty string is tracted as null (default=true).
    coNullStrParam,            // when is false then null string is tracted as empty string
                               // (default=true).
    coParamDateByOdbcLevel3,   // Handling DateTime parameter follow odbc level (default=false).
    coBCD2Exp,                 // Convert Bcd to exp format (default=false).
    // Rows Fetch features:
    coMixedFetch,              // redefines use "block fetching" (default=true and is driver depends).
    coBlobFragmentation,       // blob fragmentation when fetching
    coBlobNotTerminationChar,  // determines the not use of TerminationCharSize, at the fetch of text blob
    // ISQLCommand features:
    coNetTimeout,              // Network timeout limitation (in sec).
    coLockMode,                // redefines timeout to the number of seconds to wait for an SQL
                               // statement to execute before returning to the application
                               // (default=17).
                               // Values:
                               //   -1: Suspends the process until the lock is released.
                               //    0: Ends the operation immediately and returns an error code.
                               //   >0: Suspends the process until the lock is released, or until
                               //       the end of the specified number of seconds.
    coSPSN,                    // Stored Proc Sys Naming  (ISQLCursor metadata procedures)
    coTLSTO,                   // Table List Support Only Tables (ISQLCursor metadata tables)
    coOBPBPL,                  // ODBC SQLBindParameter Buffer Size Limitation (ISQLCommand)
    coCFC                      // Columns Fetch Consecutively (ISQLCursor)
  );
  TConnectionOptionType = ( cot_Bool, cot_String, cot_Int, cot_UInt, cot_Char );
  TConnectionOptionsNames = array[TConnectionOption] of AnsiString;
  TConnectionOptionsTypes = array[TConnectionOption] of TConnectionOptionType;

  TOptionSwitches = (osDefault {=='X'}, osOff {=='0'}, osOn {=='1'});
  TConnectionOptions = array[TConnectionOption] of TOptionSwitches;
  PConnectionOptions = ^TConnectionOptions;
  TOptionSwitchesNames = array[TOptionSwitches] of AnsiString;

  TRegisterDbXpressLib = procedure;

const
  cConnectionOptionsNames: TConnectionOptionsNames = ( { Do not localize }
    // Connection features:
    'coSafeMode',              // - coSafeMode
    'coICloneCon',             // - coInternalCloneConnection
    'coBlobChunkSize',         // - coBlobChunkSize,
    'coNetPacketSize',         // - coNetwrkPacketSize,
    'coReadOnly',              // - coReadOnly
    'coCatPrefix',             // - coCatalogPrefix
    'coConTimeout',            // - coConTimeout
    'coNumSepr',               // - coNumericSeparator
    'coConnectionString',      // - coConnectionString
    // Metada features:
    'coMetaData',              // - coSupportsMetadata,
    'coCatalog',               // - coSupportsCatalog,
    'coSchemFlt',              // - coSupportsSchemaFilter,
    // BindField features:
    'coMapInt64ToBCD',         // - coMapInt64ToBcd,
    'coMapSmallBcdToNative',   // - coMapSmallBcdToNative,
    'coIgnoreUnkFldType',      // - coIgnoreUnknownFieldType,
    'coMapCharAsBDE',          // - coMapCharAsBDE,
    'coEnableBCD',             // - coEnableBCD
    'coMaxBCD',                // - coMaxBCD
    'coEnableUnicode',         // - coEnableUnicode
    'coAutoInc',               // - coSupportsAutoInc
    'coFldReadOnly',           // - coFldReadOnly
    // Field & Params features:
    'coTrimChar',              // - coTrimChar,
    'coEmptyStrParam',         // - coEmptyStrParam
    'coNullStrParam',          // - coNullStrParam
    'coParDateByLev3',         // - coParamDateByOdbcLevel3
    'coBCD2Exp',               // - coBCD2Exp
    // Rows Fetch features:
    'coMixedFetch',            // - coMixedFetch
    'coBlobFragmntns',         // - coBlobFragmentation
    'coBlobNotTermChar',       // - coBlobNotTerminationChar
    // ISQLConnection, ISQLCommand features:
    'coNetTimeout',            // - coNetTimeout
    'coLockMode',              // - coLockMode
    'coSPSN',                  // - coSPSN
    'coTLSTO',                 // - coTLSTO
    'coOBPBPL',                // - coOBPBPL
    'coCFC'                    // - coCFC
  );

  cConnectionOptionsTypes: TConnectionOptionsTypes = (
    // Connection features:
    cot_Bool,                  // - coSafeMode
    cot_Bool,                  // - coInternalCloneConnection
    cot_UInt,                  // - coBlobChunkSize
    cot_UInt,                  // - coNetwrkPacketSize
    cot_Bool,                  // - coReadOnly
    cot_String,                // - coCatalogPrefix
    cot_UInt,                  // - coConTimeout
    cot_Char,                  // - coNumericSeparator
    cot_String,                // - coConnectionString
    // Metada features:
    cot_Bool,                  // - coSupportsMetadata
    cot_Bool,                  // - coSupportsCatalog
    cot_Bool,                  // - coSupportsSchemaFilter
    // BindField features:
    cot_Bool,                  // - coMapInt64ToBcd
    cot_Bool,                  // - coMapSmallBcdToNative
    cot_Bool,                  // - coIgnoreUnknownFieldType
    cot_Bool,                  // - coMapCharAsBDE
    cot_Bool,                  // - coEnableBCD
    cot_Bool,                  // - coMaxBCD
    cot_Bool,                  // - coEnableUnicode
    cot_Bool,                  // - coSupportsAutoInc
    cot_Bool,                  // - coFldReadOnly
    // Field & Params features:
    cot_Bool,                  // - coTrimChar
    cot_Bool,                  // - coEmptyStrParam
    cot_Bool,                  // - coNullStrParam
    cot_Bool,                  // - coParamDateByOdbcLevel3
    cot_Bool,                  // - coBCD2Exp
    // Rows Fetch features:
    cot_Bool,                  // - coMixedFetch
    cot_Bool,                  // - coBlobFragmentation
    cot_Bool,                  // - coBlobNotTerminationChar
    // ISQLCommand features:
    cot_UInt,                  // - coNetTimeout
    cot_Int,                   // - coLockMode
    // others
    cot_Bool,                  // - coSPSN
    cot_Bool,                  // - coTLSTO
    cot_Bool,                  // - coOBPBPL
    cot_Bool                   // - coCFC
  );

  cConnectionOptionsDescs: TConnectionOptionsNames = (
    // Connection features:
    'Safe(Silent) Mode',                       // - coSafeMode
    'Use Internal Clone Connection',           // - coInternalCloneConnection
    'Blob Chunk Size',                         // - coBlobChunkSize,
    'Network Packet Size',                     // - coNetwrkPacketSize,
    'Read Only Connection',                    // - coReadOnly
    'Catalog prefix format for odbc '          // - coCatalogPrefix
      + 'connection string',
    'Connection Timeout',                      // - coConTimeout
    'Decimal Separator transmitted into '      // - coNumericSeparator
      + 'odbc driver',
    'Custom Connection String',                // - coConnectionString
    // Metada features:
    'Supports Metadata',                       // - coSupportsMetadata,
    'Supports Catalog',                        // - coSupportsCatalog,
    'Supports Schema Filter',                  // - coSupportsSchemaFilter,
    // BindField features:
    'Map Int64(Big Int) to BCD',               // - coMapInt64ToBcd,
    'Map SmallBcd To Native',                  // - coMapSmallBcdToNative,
    'Ignore Unknown Field Type',               // - coIgnoreUnknownFieldType,
    'Map Char As BDE',                         // - coMapCharAsBDE,
    'Enable BCD',                              // - coEnableBCD
    'BCD bind with greatest possible of the '  // - coMaxBCD
      + 'bcd size',
    'Enable Unicode',                          // - coEnableUnicode
    'Supports Auto Increment Fields',          // - coSupportsAutoInc
    'Supports Odbc Field Attribute ReadOnly',  // - coFldReadOnly
    // Field & Params features:
    'Use Trim Char',                           // - coTrimChar,
    'Update empty string as NULL',             // - coEmptyStrParam
    'Update NULL string as empty string',      // - coNullStrParam
    'Handling DateTime parameter follow odbc'  // - coParamDateByOdbcLevel3
      + ' Level 2',
    'Supports Bcd exp format',                 // - coBCD2Exp
    // Rows Fetch features:
    'Use Mixed Fetch',                         // - coMixedFetch
    'Allow blob fragmentation (Minimization '  // - coBlobFragmentation
      + 'of memory reallocation)',
    'Determines the not use of TerminationCh'
      + 'arSize, at the fetch of text blob',   // - coBlobNotTerminationChar
    // ISQLCommand features:
    'Network Timeout',                         // - coNetTimeout
    'Lock Mode',                               // - coLockMode
    'Stored Proc Sys Naming',                  // - coSPSN
    'Table List Support Only Tables',          // - coTLSTO
    'ODBC SQLBindParameter Buffer Precision '  // - coOBPBPL
      + 'Limitation' ,
    'Columns Fetch Consecutively'              // - coCFC
  );

  cOptionSwitchesNames: TOptionSwitchesNames = ( { Do not localize }
    'osDefault', 'osOff', 'osOn'
  );

  { Character Values of type dxbOpenOdbc.pas:TOptionSwitches }
  cOptCharDefault = 'X';    // = TOptionSwitches.osDefault
  cOptCharFalse   = '0';    // = TOptionSwitches.osOff
  cOptCharTrue    = '1';    // = TOptionSwitches.osOn

type
  TXSQLDriverOption = (
      xeDrvBlobSize, xeDrvCallBack, xeDrvCallBackInfo, xeDrvRestrict,
      // Delphi 2006
      xeDrvVersion, xeDrvProductVersion
  );
  TSetOfXSQLDriverOption = set of TXSQLDriverOption;

  TXSQLConnectionOption = (
      xeConnAutoCommit, xeConnBlockingMode, xeConnBlobSize, xeConnRoleName,
      xeConnWaitOnLocks, xeConnCommitRetain, xeConnTxnIsoLevel,
      xeConnNativeHandle, xeConnServerVersion, xeConnCallBack, xeConnHostName,
      xeConnDatabaseName, xeConnCallBackInfo, xeConnObjectMode,
      xeConnMaxActiveComm, xeConnServerCharSet, xeConnSqlDialect,
      xeConnRollbackRetain, xeConnObjectQuoteChar, xeConnConnectionName,
      xeConnOSAuthentication, xeConnSupportsTransaction, xeConnMultipleTransaction,
      xeConnServerPort, xeConnOnLine, xeConnTrimChar, xeConnQualifiedName,
      xeConnCatalogName, xeConnSchemaName, xeConnObjectName, xeConnQuotedObjectName,
      xeConnCustomInfo, xeConnTimeout,
      // Delphi 8:
      xeConnConnectionString,
      // Delphi 2005:
      xeConnTDSPacketSize,
      xeConnClientHostName, xeConnClientAppName, xeConnCompressed, xeConnEncrypted,
      xeConnPrepareSQL, xeConnDecimalSeparator,
      // Delphi 2007
      xe41, xeVendorProperty);

  TSetOfXSQLConnectionOption = set of TXSQLConnectionOption;

  TXSQLCommandOption = (
      xeCommRowsetSize, xeCommBlobSize, xeCommBlockRead, xeCommBlockWrite,
      xeCommParamCount, xeCommNativeHandle, xeCommCursorName, xeCommStoredProc,
      xeCommSQLDialect, xeCommTransactionID, xeCommPackageName, xeCommTrimChar,
      xeCommQualifiedName, xeCommCatalogName, xeCommSchemaName, xeCommObjectName,
      xeCommQuotedObjectName,
      // Delphi 2005:
      xeCommPrepareSQL, xeCommDecimalSeparator
      );

  TSetOfXSQLCommandOption = set of TXSQLCommandOption;

  TXSQLMetaDataOption = (xeMetaCatalogName, xeMetaSchemaName, xeMetaDatabaseName,
      xeMetaDatabaseVersion, xeMetaTransactionIsoLevel, xeMetaSupportsTransaction,
      xeMetaMaxObjectNameLength, xeMetaMaxColumnsInTable, xeMetaMaxColumnsInSelect,
      xeMetaMaxRowSize, xeMetaMaxSQLLength, xeMetaObjectQuoteChar,
      xeMetaSQLEscapeChar, xeMetaProcSupportsCursor, xeMetaProcSupportsCursors,
      xeMetaSupportsTransactions, xeMetaPackageName,
      // Delphi 2005:
      xeMetaDefaultSchemaName);

  TSetOfXSQLMetaDataOption = set of TXSQLMetaDataOption;

  //TXSQLObjectType = (xeObjTypeDatabase, xeObjTypeDataType, xeObjTypeTable,
  //                xeObjTypeView, xeObjTypeSynonym, xeObjTypeProcedure, xeObjTypeUser,
  //                xeObjTypeRole, xeObjTypeUDT, xeObjTypePackage);

  TOdbcDriverType = (eOdbcDriverTypeUnspecified,
    eOdbcDriverTypeGupta, eOdbcDriverTypeMsSqlServer, eOdbcDriverTypeMsSqlServer2005Up,
    eOdbcDriverTypeIbmDb2, eOdbcDriverTypeIbmDb2AS400,
    eOdbcDriverTypeMsJet, eOdbcDriverTypeMySql, eOdbcDriverTypeMySql3,
    eOdbcDriverTypeInterbase, eOdbcDriverTypeInformix, eOdbcDriverTypeOracle,
    eOdbcDriverTypeSybase, eOdbcDriverTypeSQLite, eOdbcDriverTypeThinkSQL,
    eOdbcDriverTypeMerantOle, eOdbcDriverTypePervasive, eOdbcDriverTypeNexusDbFlashFiler,
    eOdbcDriverTypePostgreSQL, eOdbcDriverTypeInterSystemCache,
    eOdbcDriverTypeMerantDBASE, eOdbcDriverTypeSAPDB, eOdbcDriverTypeParadox,
    eOdbcDriverTypeFoxPro, eOdbcDriverTypeClipper, eOdbcDriverTypeBtrieve,
    eOdbcDriverTypeOpenIngres, eOdbcDriverTypeProgress, eOdbcDriverTypeOterroRBase
  );

  TDbmsType = (eDbmsTypeUnspecified,
   eDbmsTypeGupta, eDbmsTypeMsSqlServer, eDbmsTypeMsSqlServer2005Up,
   eDbmsTypeIbmDb2, eDbmsTypeIbmDb2AS400,
   eDbmsTypeMySql, eDbmsTypeMySqlMax,
   eDbmsTypeMsAccess, eDbmsTypeExcel, eDbmsTypeText, eDbmsTypeDBase, eDbmsTypeParadox,
   eDbmsTypeOracle, eDbmsTypeInterbase, eDbmsTypeInformix, eDbmsTypeSybase,
   eDbmsTypeSQLite, {Any type is mapped into the text, with maximum length 2048 }
   eDbmsTypeThinkSQL, eDbmsTypeSapDb, eDbmsTypePervasiveSQL,
   eDbmsTypeFlashFiler, eDbmsTypePostgreSQL, eDbmsTypeInterSystemCache,
   eDbmsTypeFoxPro, eDbmsTypeClipper, eDbmsTypeBtrieve, eDbmsTypeOpenIngres,
   eDbmsTypeProgress, eDbmsTypeOterroRBase
   );


{ ISqlConnectionOdbc interface  }
  //
  // ISqlConnectionOdbc introduces additional methods on ISqlConnection.
  // Here is an example of how you can access this interface:
  //
  //  Exampe:
  //
  {
    procedure OdbcInterfaceExample1(Conn: SqlConnection; Memo: TMemo);
    var
      aSqlConnectionInterface: ISqlConnection;
      aSqlConnectionOdbcInterface: ISqlConnectionOdbc;
      aResult: HResult;
    begin
      aSqlConnectionInterface := Conn.SqlConnection;
      aResult := aSqlConnectionInterface.QueryInterface(ISqlConnectionOdbc, aSqlConnectionOdbcInterface);
      if aResult = S_OK then
        aSqlConnectionOdbcInterface.GetConnectStrings(Memo.Lines);
    end;

    // If you have statically linked DbxOpenOdbc into your program,
    // so you know SqlConnection will be implemented by TSqlConnectionOdbc,
    // you can use "as" in place of "QueryInterface"
    procedure OdbcInterfaceExample2(Conn: SqlConnection; Memo: TMemo);
    var
      aSqlConnectionInterface: ISqlConnection;
      aSqlConnectionOdbcInterface: ISqlConnectionOdbc;
    begin
      aSqlConnectionInterface := Conn.SqlConnection;
      aSqlConnectionOdbcInterface := aSqlConnectionInterface as ISqlConnectionOdbc;
      aSqlConnectionOdbcInterface.GetConnectStrings(Memo.Lines);
    end;
  }

  ISqlConnectionOdbc = interface
    ['{136DD9D1-9B9C-4355-9AEF-959662CB095E}']
    function GetDbmsName: AnsiString;
    function GetDbmsType: TDbmsType;
    function GetDbmsVersionString: AnsiString;
    function GetDbmsVersionMajor: integer;
    function GetDbmsVersionMinor: Integer;
    function GetDbmsVersionRelease: Integer;
    function GetLastOdbcSqlState: PAnsiChar;
    procedure GetOdbcConnectStrings(ConnectStringList: TStrings);
    function GetOdbcDriverName: AnsiString;
    function GetOdbcDriverType: TOdbcDriverType;
    function GetOdbcDriverVersionString: AnsiString;
    function GetOdbcDriverVersionMajor: integer;
    function GetOdbcDriverVersionMinor: Integer;
    function GetOdbcDriverVersionRelease: Integer;
    // 2.9
    function GetDbmsVersionBuild: Integer;
    function GetOdbcDriverVersionBuild: Integer;
    // 2.015
    function GetCursorPreserved: Boolean;
    // 3.000
    function GetIsSystemODBCManager: Boolean;
    function GetOdbcDriverLevel: Integer;
    function GetSupportsSqlPrimaryKeys: Boolean;
    function GetStatementsPerConnection: Integer;
    // 3.023
    function GetEnvironmentHandle: Pointer;
    function GetConnectionHandle: Pointer;
    function GetOdbcApiIntf: IUnknown; // returned 'OdbaApi.pas:IOdbcApi' interface
    // 3.024
    function GetDecimalSeparator: AnsiChar;
  end;

  ISqlCommandOdbc = interface
    ['{136DD9D1-9B9C-4355-9AEF-959662CB095F}']
    procedure Cancel;
    function SetLockTimeout(TimeoutSeconds: Integer): Boolean;
    function GetLockTimeout(): Integer;
  end;

  ISequentialStream = interface(IUnknown)
    ['{0c733a30-2a1c-11ce-ade5-00aa0044773d}']
    function Read(pv: Pointer; cb: Longint; pcbRead: PLongint): HResult; stdcall;
    function Write(pv: Pointer; cb: Longint; pcbWritten: PLongint): HResult; stdcall;
  end;

  IBlobChunkCollection = interface
    ['{A4A8C6A2-786D-4DF7-938D-BC9301232BC8}']
    function GetSize: Int64;
    procedure Read(var Buffer: Pointer);
    function ReadBlobToVariant(out Data: Variant): int64;
    function ReadBlobToStream(Stream: ISequentialStream): Int64;
    procedure Clear;
  end;

const
  //
  //  string types of TXSQLDriverOption
  //
  cXSQLDriverOptionStringTypes: TSetOfXSQLDriverOption = [
    xeDrvVersion, xeDrvProductVersion
  ];
  //
  //  string types of TXSQLConnectionOption
  //
  cXSQLConnectionOptionStringTypes: TSetOfXSQLConnectionOption = [
    xeConnRoleName, xeConnServerVersion, xeConnHostName, xeConnDatabaseName,
    xeConnObjectQuoteChar, xeConnConnectionName, xeConnQualifiedName,
    xeConnCatalogName, xeConnSchemaName, xeConnObjectName, xeConnQuotedObjectName,
    xeConnCustomInfo, xeConnConnectionString, xeConnClientHostName, xeConnClientAppName,
    xeConnDecimalSeparator, xeVendorProperty
  ];
  //
  // string types of TXSQLCommandOption
  //
  cXSQLCommandOptionStringTypes: TSetOfXSQLCommandOption = [
    xeCommCursorName, xeCommPackageName, xeCommQualifiedName, xeCommCatalogName,
    xeCommSchemaName, xeCommObjectName,  xeCommQuotedObjectName
  ];
  //
  // string types of TXSQLCommandOption
  //
  cXSQLMetaDataOptionStringTypes: TSetOfXSQLMetaDataOption = [
    xeMetaCatalogName, xeMetaSchemaName, xeMetaDatabaseName, xeMetaObjectQuoteChar,
    xeMetaSQLEscapeChar, xeMetaPackageName, xeMetaDefaultSchemaName
  ];

//
// RegisterDbXpressLib access
//

procedure RegisterDbXpressLibA();  { deprecated }
procedure RegisterDbXpressLibAW(); { deprecated }
procedure RegisterDbXpressLibW();  { deprecated }
procedure RegisterDbXpressLibWA(); { deprecated }

type
  TOdbcApiType = ( oaAnsi, oaAnsiToUnicode, oaUnicode, oaUnicodeToAnsi );

procedure RegisterDbXpressLibProc(RegProc: TRegisterDbXpressLib; OdbcApiType: TOdbcApiType);

resourcestring
  rsNotSpecifiedDNSName = 'The user has not specified "ODBC DNS Name"';

implementation

var
  //
  // RegisterDbXpressLib access
  //
  _RegisterDbXpressLibs: array[TOdbcApiType] of TRegisterDbXpressLib;

procedure DoCallRegisterDbXpressLibProc(OdbcApiType: TOdbcApiType); {$IFDEF _INLINE_} inline; {$ENDIF}
var
  Proc: TRegisterDbXpressLib;
begin
  Proc := _RegisterDbXpressLibs[OdbcApiType];
  if Assigned(Proc) then
    Proc();
end;

procedure RegisterDbXpressLibA();
begin
  DoCallRegisterDbXpressLibProc(oaAnsi);
end;

procedure RegisterDbXpressLibAW();
begin
  DoCallRegisterDbXpressLibProc(oaAnsiToUnicode);
end;

procedure RegisterDbXpressLibW();
begin
  DoCallRegisterDbXpressLibProc(oaUnicode);
end;

procedure RegisterDbXpressLibWA();
begin
  DoCallRegisterDbXpressLibProc(oaUnicodeToAnsi);
end;

procedure RegisterDbXpressLibProc(RegProc: TRegisterDbXpressLib; OdbcApiType: TOdbcApiType);
begin
  _RegisterDbXpressLibs[OdbcApiType] := RegProc;
end;

end.
