{
  Part of Kylix / Delphi open source DbExpress driver for ODBC
  Version 2008.02.17

  Copyright (c) 2006 by "Vadim V.Lopushansky" <pult@ukr.net>

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2.1
  of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
}
//
// Delphi 6, 7.
//
(*

   Some runtime memory fixes of Borland Delphi (6 Upd2, 7, 7 Upd1) system modules.
   For correction of errors it is enough to include this module in your project or in any package
   used by your project. After loading such package in IDE the bugs in standard packages will be
   corrected.

   Fixed units (db.pas, dbCommon.pas, Provider.pas, SqlExpr.pas):

   "db.pas" - can be applied for any dbEngine
       TParams
         - fixed parsing parameters for informix syntax (database:tablename).
           It is adjusted by an option "db_TParams_ParseSQL".
           QC: 6319.
   "dbCommon.pas" - can be applied for any dbEngine
       NextSQLToken
         - added detected SQL line comments by two hyphen ("--"). It comment is supported in mssql,
           oracle, informix, ...
           It is adjusted by an option "dbCommon_NextSQLToken".
           QC: 6317
       IsMultiTableQuery
         It is adjusted by an option "dbCommon_IsMultiTableQuery".
       GetTableNameFromSQL
         - added check allowed words after from: ( where, union, ... ).
         - added support "select from select"
         - fixed: IsMultiTableQuery and GetTableNameFromSQL work by a similar
                  principle but have a separate code.
           It is adjusted by an option "dbCommon_GetTableNameFromSQL".
       QC: 6318.

   "Provider.pas" - can be applied for any dbEngine
       TDataSetProvider
         - fixed exceptin handling
           It is adjusted by an option "Provider_TDataSetProvider_InternalGetRecords".
           QC: 7872.

   "SqlExpr.pas"
       TSQLConnection
         - fixed clone connection.
           It is adjusted by an option "SqlExpr_TSQLConnection_CloneConnection".
           QC: 6315, 5867
         - added to Delphi6 connection handling options from Delphi7:
              CUSTOM_INFO        = 'Custom String'
              SERVERPORT         = 'Server Port'
              MULTITRANSENABLED  = 'Multiple Transaction'
              TRIMCHAR           = 'Trim Char'
              CONN_TIMEOUT       = 'Connection Timeout'
              OSAUTHENTICATION   = 'Os Authentication'
           It is adjusted by an option "SqlExpr_TSQLConnection_ConnectionOptions".

       TCustomSQLDataSet
         Delphi6: // 6, 6 Upd2
           - GetRecord: fixed check error code in Delphi 6.
             It is adjusted by an option "SqlExpr_TCustomSQLDataSet_GetRecord".
             QC: ????
         Delph7: // can be applied to Delphi6 ...
           - SetSchemaOption: fixed buffer oveflow:
             Crash of the application when the name of the database exceeds 256 characters.
             It is adjusted by an option "SqlExpr_TCustomSQLDataSet_SetSchemaOption".
             QC: 6316.
           - GetQueryFromType: fixed added delimiter '.' when SchemaName is empty.
             It is adjusted by an option "SqlExpr_TCustomSQLDataSet_GetQueryFromType".
             QC: ????

    - UNICODE: // added supports unicode fields. Is limitatuions. When unicode field is
               // very long it mapped to blob. Not all odbc the driver can update such fields
               // as blob. The reason in absence in Delphi TWideMemoField
       SqlExpr.pas:TSQLConnection:
         Execute
       SqlExpr.pas:TCustomSQLDataSet:
         ExecuteStatement, AddFieldDesc, GetFieldData
       db.pas:TDataSet:
         DataConvert
         ? GetFieldData (is problem for TClientDataSet)
       db.pas:TParam:
         GetDataSize
         GetData
         ? TWideStringField.GetDataSize
       Provider.pas:TDataPacketWriter
         AddColumn
       SqlExpr.pas:
         SetProcedureParams
         SetQueryParams

*)

unit SqlExprFix;

{$B-}
{$O+}
{$IFDEF _DEBUG_}
{$D+,L+}
{$ENDIF}

{$DEFINE _UNSUPPORTED_}

{$UNDEF _INLINE_}

{$UNDEF DELPHI_6}
{$UNDEF DELPHI_7}
{$UNDEF DELPHI_9}
{$UNDEF KYLIX_1}
{$UNDEF KYLIX_2}
{$UNDEF KYLIX_3}

{$UNDEF DELPHI_6UP}
{$UNDEF DELPHI_7UP}
{$UNDEF DELPHI_9UP}
{$UNDEF KYLIX_1UP}
{$UNDEF KYLIX_2UP}
{$UNDEF KYLIX_3UP}

{$IFDEF VER140}
  {$DEFINE DELPHI_6}
  {$UNDEF _UNSUPPORTED_}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE DELPHI_7}
  {$UNDEF _UNSUPPORTED_}
{$ENDIF}

{$IFDEF VER170}
  {$IFNDEF CLR}
    {$DEFINE DELPHI_9}
//TODO: ...
    {.$UNDEF _UNSUPPORTED_}
  {$ENDIF}
{$ENDIF}

{$IFNDEF CLR}
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$DEFINE KYLIX_1UP}
    {$IF Declared(CompilerVersion)}
      {$IF CompilerVersion >= 14.01}
         // Delphi 6.00 and up
        {$DEFINE DELPHI_6UP}
      {$IFEND}
      {$IF CompilerVersion >= 14.10}
        // Kylix 2 and up
        {$DEFINE KYLIX_2UP}
        {$DEFINE DELPHI_6UP}
      {$IFEND}
      {$IF CompilerVersion >= 14.50}
        // Kylix 3 and up
        // Kylix 3 supports something somewhere between D6 and D7
        {$DEFINE KYLIX_3UP}
        {$DEFINE DELPHI_7UP}
      {$IFEND}
      {$IF CompilerVersion >= 15.00}
        // Delphi 7.00 and up
        {$DEFINE DELPHI_7UP}
      {$IFEND}
      {$IF CompilerVersion >= 17.00}
        {.$DEFINE _UNSUPPORTED_}
        // Delphi 9.00 and up
        {$DEFINE DELPHI_9UP}
        {$DEFINE _INLINE_}
      {$IFEND}
      {$IF CompilerVersion >= 18.50}
        // Delphi 2007 and up
        {$DEFINE DELPHI_11UP}
      {$IFEND}
      {$IF CompilerVersion >= 18.00}
        {$DEFINE DELPHI_10UP}
      {$IFEND}
      {$IF CompilerVersion >= 19.00}
        // Delphi 2008 and up
        {$DEFINE DELPHI_12UP}
      {$IFEND}
    {$IFEND} // of: $IF Declared(CompilerVersion)
  {$ENDIF} // of: $IFDEF CONDITIONALEXPRESSIONS
{$ENDIF IFNDEF CLR}

interface

uses
  {$IFNDEF LINUX}
  Windows,
  {$ENDIF}
  {$IFNDEF DELPHI_7UP}
  IniFiles,
  {$ENDIF}
  SysUtils, Classes, DBClient, DB, DBCommon, DBXpress, SqlExpr, FMTBcd, Provider, SqlConst,
  Variants, DSIntf, DBConsts, SqlTimSt, MaskUtils, MidConst, TypInfo;

var
  bFixedSqlExpr: Boolean = False;
  bFixedUnicode: Boolean = False;

{$IFNDEF CLR}
type
  TSQLStoredProcBase = SqlExpr.TSQLStoredProc;

  TSQLStoredProc = class(TSQLStoredProcBase)
  //procedure OpenSchema; virtual;
  public
    procedure PrepareStatement; override;
  end;
{$ENDIF ~CLR}

implementation

{$IFNDEF _UNSUPPORTED_}

{$UNDEF db_TParams_ParseSQL}

{$UNDEF dbCommon_NextSQLToken}
{$UNDEF dbCommon_IsMultiTableQuery}
{$UNDEF dbCommon_GetTableNameFromSQL}

{$UNDEF Provider_TDataSetProvider_InternalGetRecords}

{$UNDEF SqlExpr_TSQLConnection_CloneConnection}
{$UNDEF SqlExpr_TSQLConnection_ConnectionOptions}
{$UNDEF SqlExpr_TCustomSQLDataSet_GetRecord}
{$UNDEF SqlExpr_TCustomSQLDataSet_SetSchemaOption}
{$UNDEF SqlExpr_TCustomSQLDataSet_GetQueryFromType}
{$UNDEF _UNICODE_}

// *****************************************************
// **   Can exclude correction not interesting you :  **
// *****************************************************

{$DEFINE db_TParams_ParseSQL}

{$DEFINE dbCommon_NextSQLToken}
{$DEFINE dbCommon_IsMultiTableQuery}
{$DEFINE dbCommon_GetTableNameFromSQL}

{$DEFINE Provider_TDataSetProvider_InternalGetRecords}

{$DEFINE SqlExpr_TSQLConnection_CloneConnection}
{$DEFINE SqlExpr_TSQLConnection_ConnectionOptions} // to Delphi 6 only.
{$DEFINE SqlExpr_TCustomSQLDataSet_GetRecord} // to Delphi 6 only.
{$DEFINE SqlExpr_TCustomSQLDataSet_SetSchemaOption}
{$DEFINE SqlExpr_TCustomSQLDataSet_GetQueryFromType}

// ****************************************************************************
{$DEFINE _UNICODE_} // ***  ***  ***  ***  ***  ***  ***  ***     UNICODE   ***
// ****************************************************************************

{$IFNDEF DELPHI_7UP}
  {.$UNDEF SqlExpr_TSQLConnection_ConnectionOptions}
  {.$UNDEF SqlExpr_TCustomSQLDataSet_SetSchemaOption}
  {.$UNDEF SqlExpr_TCustomSQLDataSet_GetQueryFromType}
  {$UNDEF _UNICODE_}
{$ENDIF}

// =============================================================================

{$IFNDEF DELPHI_9UP}
const
  DBXERR_NONE = DBXpress.SQL_SUCCESS;
  DBX_MAXSTATICERRORS = DBXpress.MaxReservedStaticErrors;
{$ENDIF}

{$IFNDEF DELPHI_10UP}
const
  fldWIDESTRING = 26; { UCS2 null terminated string }
{$ENDIF}

{$IFNDEF DELPHI_7UP}

{ TCustomSQLDataSet }

type

  {$HINTS OFF}
  TCustomSQLDataSetFix = class(TDataSet) // Delphi 6 Upd2
  private
    // CLONE:
    FBlobBuffer: TBlobByteData;
    FCalcFieldsBuffer: PChar;
    FCheckRowsAffected: Boolean;
    FClonedConnection: TSqlConnection;
    FCommandText: string;
    FCommandType: TSQLCommandType;
    FCurrentBlobSize: LongWord;
    FDataLink: TDataLink;
    FDesignerData: string;
    FFieldBuffer: PChar;
    FGetNextRecordSet: Boolean;
    FIndexDefs: TIndexDefs;
    FIndexDefsLoaded: Boolean;
    FLastError: string;  // DBExpress GetError() clears error; need to save last
    FMaxBlobSize: Integer;
    FMaxColSize: LongWord;
    FNativeCommand: string;
    FNoMetadata: Boolean {platform};
    FParamCheck: Boolean;
    FParamCount: Integer;
    FParams: TParams;
    FPrepared: Boolean;
    FProcParams: TList;
    FRecords: Integer;
    FRowsAffected: Integer;
    FSchemaInfo: TSQLSchemaInfo;
    FSortFieldNames: string;
    FSQLCommand: ISQLCommand;
    FSQLConnection: TSQLConnection;
    FSQLCursor: ISQLCursor;
    FStatementOpen: Boolean;
    FTransactionLevel: SmallInt;
  protected
    // FIXED:
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
  end;
  {$HINTS ON}

  TCustomSQLDataSetP = class(TCustomSQLDataSet);

function TCustomSQLDataSetFix.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  Status: SQLResult;
begin
  Status := FSQLCursor.next;
  {+} // *** FIXED:
  if (not (Status in [DBXERR_NONE, SQL_NULL_DATA, DBXERR_EOF])) then
  {+.}
     TCustomSQLDataSetP(Self).Check(Status);
  if Status = DBXERR_NONE then
  begin
    GetCalcFields(FCalcFieldsBuffer);
    Result := grOK
  end
  else
    Result := grEOF;
end;

{$ENDIF IFNDEF DELPHI_7UP}

type

{ TCustomSQLDataSet }

  {$HINTS OFF}
  {$IFNDEF DELPHI_7UP}
  TCustomSQLDataSetH = TCustomSQLDataSetFix;
  {$ENDIF}
  {$IFDEF DELPHI_7UP}
  TCustomSQLDataSetH = class(TDataSet)
  private
    FBlobBuffer: TBlobByteData;
    FCalcFieldsBuffer: PChar;
    FCheckRowsAffected: Boolean;
    FClonedConnection: TSqlConnection;
    FCommandText: string;
    FCommandType: TSQLCommandType;
    FCurrentBlobSize: LongWord;
    FDataLink: TDataLink;
    FDesignerData: string;
    FGetNextRecordSet: Boolean;
    FIndexDefs: TIndexDefs;
    FIndexDefsLoaded: Boolean;
    FLastError: string;  // DBExpress GetError() clears error; need to save last
    FMaxBlobSize: Integer;
    FMaxColSize: LongWord;
    FNativeCommand: string;
    {$IFNDEF DELPHI_9UP}
    FNoMetadata: Boolean {deprecated};
    {$ENDIF}
    FGetMetadata: Boolean;
    FNumericMapping: Boolean;
    FParamCheck: Boolean;
    FParamCount: Integer;
    FParams: TParams;
    FPrepared: Boolean;
    FProcParams: TList;
    FRecords: Integer;
    FRowsAffected: Integer;
    FSchemaInfo: TSQLSchemaInfo;
    {$IFDEF DELPHI_9UP}
    FParseSelectSql: TParseSqlEvent;
    FParseUpdateSql: TParseSqlEvent;
    FParseDeleteSql: TParseSqlEvent;
    FParseInsertSql: TParseInsertSqlEvent;
    {$ENDIF}
    FSortFieldNames: string;
    FSQLCommand: ISQLCommand;
    FSQLConnection: TSQLConnection;
    FSQLCursor: ISQLCursor;
    FStatementOpen: Boolean;
    FTransactionLevel: SmallInt;
    FSchemaName: string;
  end;
  TCustomSQLDataSetP = class(TCustomSQLDataSet);
  {$ENDIF IFDEF DELPHI_7UP}

  TCustomSQLDataSet_GetFieldData1 = function (FieldNo: Integer; Buffer: Pointer): Boolean of object;

  TCustomSQLDataSetFixD7 = class(TCustomSQLDataSetH)
  // *** FIXED:
  protected
    // function GetQueryFromType: string;
    // D 6,7: - > (PrepareStatement; PSGetDefaultOrder)
    procedure PrepareStatement; virtual;
    function PSGetDefaultOrder: TIndexDef; override;
    function GetQueryFromType: string;
    // procedure SetSchemaOption;
    // D 6, 7: -> OpenSchema -> ExecuteStatement
    procedure OpenSchema; virtual;
    // D 7 unicode: SetQueryParams -> ExecuteStatement
    procedure ExecuteStatement;
    // D 7 uicode:
    procedure AddFieldDesc(FieldDescs: TFieldDescList; DescNo: Integer;
        var FieldID: Integer; RequiredFields: TBits; FieldDefs: TFieldDefs);
    function GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean; override;
  protected
  // *** CLONE:
     procedure CheckStatement(ForSchema: Boolean = False);
    // Work in D6 as in D7 (???): need dbExpress drivers from Delphi 7.
    function AddQuoteCharToObjectName(Name, Q: string): string;
  end;

  TDataSet_GetFieldData2 = function (Field: TField; Buffer: Pointer;
    NativeFormat: Boolean): Boolean of object;

  TDataSetFix = class(TDataSet)
  protected
    procedure DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean); override;
  public
    function GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean; override;
  end;

  TDataSetP = class(TDataSet);

  TParamFix = class(TParam)
  public
    procedure GetData(Buffer: Pointer);
    function GetDataSize: Integer;
  end;

  TParamH = class(TCollectionItem)
  private
    FParamRef: TParam;
    FNativeStr: string;
    FData: Variant;
    FPrecision: Integer;
    FNumericScale: Integer;
    FNull: Boolean;
    FName: string;
    FDataType: TFieldType;
    FBound: Boolean;
    FParamType: TParamType;
    FSize: Integer;
  end;

  TWideStringFieldFix = class(TWideStringField)
  protected
    function GetDataSize: Integer; override;
  end;

  TWideStringFieldP = class(TWideStringField);

  TBlobFieldH = class(TField)
  private
    FModifiedRecord: Integer;
    FModified: Boolean;
    FGraphicHeader: Boolean;
    FTransliterate: Boolean;
  end;

  TFieldH = class(TComponent)
  private
    FAutoGenerateValue: TAutoRefreshFlag;
    FDataSet: TDataSet;
    FFieldName: string;
    FFields: TFields;
    FDataType: TFieldType;
    FReadOnly: Boolean;
    FFieldKind: TFieldKind;
    FAlignment: TAlignment;
    FVisible: Boolean;
    FRequired: Boolean;
    FValidating: Boolean;
    FSize: Integer;
    FOffset: Integer;
    FFieldNo: Integer;
    FDisplayWidth: Integer;
    FDisplayLabel: string;
    {$IFDEF MSWINDOWS}
    FEditMask: TEditMask;
    {$ENDIF}
    {$IFDEF LINUX}
    FEditMask: string;
    {$ENDIF}
    FValueBuffer: Pointer;
    FLookupDataSet: TDataSet;
    FKeyFields: string;
    FLookupKeyFields: string;
    FLookupResultField: string;
    FLookupCache: Boolean;
    FLookupList: TLookupList;
    FAttributeSet: string;
    FCustomConstraint: string;
    FImportedConstraint: string;
    FConstraintErrorMessage: string;
    FDefaultExpression: string;
    FOrigin: string;
    FProviderFlags: TProviderFlags;
    FParentField: TObjectField;
    FValidChars: TFieldChars;
    FOnChange: TFieldNotifyEvent;
    FOnValidate: TFieldNotifyEvent;
    FOnGetText: TFieldGetTextEvent;
    FOnSetText: TFieldSetTextEvent;
  end;

  TDataPacketWriterFix = class(TDataPacketWriter)
  protected
    procedure AddColumn(const Info: TPutFieldInfo); {$IFDEF DELPHI_9UP} override; {$ENDIF}
  end;

  TCustomPacketWriterH = class(TObject)
  {$IFNDEF DELPHI_9UP}
  private
    FIDSWriter: IDSWriter;
    FBuffer: array of Byte;
  {$ELSE}
  {strict }private
    FIDSWriter: IDSWriter;
  protected
    FBuffer: TBlobByteData;
  {$ENDIF}
  end;

//  {$ENDIF IFDEF DELPHI_7}
  {$HINTS ON}

  TDataPacketWriterP = class(TDataPacketWriter);

{ TDataSetProvider }

  {$HINTS OFF}
  TDataSetProviderFix = class(TBaseProvider) // Delphi 6 Upd2, Delphi 7
    {$IFDEF DELPHI_9UP}strict{$ENDIF}
  private
    // CLONE:
    FDataSet: TDataSet;
    FDataSetOpened: Boolean;
    FDSWriter: TDataPacketWriter;
    FGetDSProps: TGetDSProps;
    FParams: TParams;
    FResolveToDataSet: Boolean;
    FRecordsSent: Integer;
    FConstraints: Boolean;
    FTransactionStarted: Boolean;
    FGetTableName: TGetTableNameEvent;
  private
    // CLONE:
    procedure Reset;
    // CLONE:
    procedure CheckDataSet;
  protected
    // FIXED:
    function InternalGetRecords(Count: Integer; out RecsOut: Integer;
      Options: TGetRecordOptions; const CommandText: WideString;
      var Params: OleVariant): OleVariant; override;
  end;
  {$HINTS ON}

  TDataSetProviderP = class(TDataSetProvider);

  {$HINTS OFF}
  TBaseProviderH = class(TCustomProvider) // Delphi 6 Upd2, Delphi 7
    //{$IFDEF DELPHI_9UP}strict{$ENDIF}
  private
    // CLONE:
    FDataDS: TPacketDataSet;
    FUpdateMode: TUpdateMode;
    FResolver: TCustomResolver;
    FOnGetData: TProviderDataEvent;
    FOnUpdateData: TProviderDataEvent;
    FOnUpdateError: TResolverErrorEvent;
    FBeforeUpdateRecord: TBeforeUpdateRecordEvent;
    FAfterUpdateRecord: TAfterUpdateRecordEvent;
    {$IFDEF DELPHI_9UP}
    FBeforeCommit: TBeforeCommitEvent;
    {$ENDIF}
    FProviderOptions: TProviderOptions;
  end;
  {$HINTS ON}

function TDataSetProviderFix.InternalGetRecords(Count: Integer; out RecsOut: Integer;
  Options: TGetRecordOptions; const CommandText: WideString;
  var Params: OleVariant): OleVariant;
begin
  try
    if grReset in Options then
    begin
      Reset;
      { When doing only a reset and not getting more data then exit }
      if Count = 0 then Exit;
    end;
    if not FDataSet.Active then
    begin
      FDataSet.Open;
      FDataSetOpened := True;
    end;
    if (Count = 0) or (grMetaData in Options) then
    begin
      TBaseProviderH(Self).FDataDS.Free;
      TBaseProviderH(Self).FDataDS := nil;
      FRecordsSent := 0;
    end;
    FDataSet.CheckBrowseMode;
    FDataSet.BlockReadSize := Count;
    try
      Result := inherited InternalGetRecords(Count, RecsOut, Options,
        CommandText, Params);
      Inc(FRecordsSent, RecsOut);
      if (RecsOut <> Count) then Reset;
    finally
      if FDataSet.Active then
      begin
        FDataSet.BlockReadSize := 0;
        {+} // *** FIXED:
        if (Count > 0) and (RecsOut = Count) then
        {+.}
          FDataSet.Next;
      end;
    end;
  except
    Reset;
    raise;
  end;
end;

procedure TDataSetProviderFix.Reset;
begin
  CheckDataSet;
  if FDataSetOpened then
  begin
    TDataPacketWriterP(FDSWriter).Reset;
    FDataSet.Close;
    FDataSetOpened := False;
  end;
  IProviderSupport(FDataSet).PSReset;
  if FDataSet.Active then
    FDataSet.First;
  FRecordsSent := 0;
end;

procedure TDataSetProviderFix.CheckDataSet;
begin
  if not Assigned(FDataSet) then DatabaseError(SMissingDataSet);
end;

{ TSQLConnection }

type
  TSQLConnectionFix = class(TSQLConnection)
  public
    function CloneConnection: TSQLConnection;
    {$IFNDEF DELPHI_7UP}
    procedure DoConnect; override;
    {$ENDIF}
    // D 7 unicode: SetQueryParams -> Execute
    {$IFDEF DELPHI_7UP}
    function Execute(const SQL: string; Params: TParams;
      ResultSet: Pointer = nil): Integer;
    function GetQuoteChar: string;
    {$ENDIF}
  end;

  TSQLConnectionP = class(TSQLConnection);

  {$HINTS OFF}
  TSQLConnectionH = class(TCustomConnection) // Delphi 6 Upd2, Delphi 7
  private
    {$IFDEF DELPHI_9UP}
    FSelectStatements: LongWord;
    FPrevSelectStatements: LongWord;
    {$ENDIF}
    FActiveStatements: LongWord;
    FAutoClone: Boolean;
    FCloneParent: TSQLConnection;
    FConnectionState: TConnectionState;
    FConnectionName: string;
    FConnectionRegistryFile: string;
    FDriverName: string;
    FDriverRegistryFile: string;
    FGetDriverFunc: string;
    FTransactionCount: Integer;
    FIsCloned: Boolean;
    FISQLConnection: ISQLConnection;
    FKeepConnection: Boolean;
    FLastError: string;  // DBExpress GetError() clears error; need to save last
    FLibraryName: string;
    FLoadParamsOnConnect: Boolean;
    FMonitorUsers: TList;
    FOnLogin: TSQLConnectionLoginEvent;
    FParams: TStrings;
    FParamsLoaded: Boolean;
    FMaxStmtsPerConn: LongWord;
    FQuoteChar: String;
    {$IFDEF DELPHI_9UP}
    FDefaultSchemaName: string;
    {$ENDIF}
    FRefCount: Integer;
    FSQLDllHandle: THandle;
    FSQLDriver: ISQLDriver;
    FSQLHourGlass: Boolean;
    FSQLMetaData: ISQLMetaData;
    FSupportsMultiTrans: LongBool;
    FTableScope: TTableScopes;
    FTraceCallbackEvent: TSQLCallbackEvent;
    FTraceClientData: Integer;
    FTransactionsSupported: LongBool;
    FVendorLib: string;
    {$IFDEF DELPHI_7UP}
    FTransIsoLevel: TTransIsolationLevel;
    {$ENDIF}
    {$IFDEF DELPHI_9UP}
    FLoginUsername: String;
    {$ENDIF}
  end;
  {$HINTS ON}

  TSQLConnectionOptionX = (
    exConnAutoCommit, exConnBlockingMode, exConnBlobSize, exConnRoleName,
    exConnWaitOnLocks, exConnCommitRetain, exConnTxnIsoLevel,
    exConnNativeHandle, exConnServerVersion, exConnCallBack, exConnHostName,
    exConnDatabaseName, exConnCallBackInfo, exConnObjectMode,
    exConnMaxActiveComm, exConnServerCharSet, exConnSqlDialect,
    exConnRollbackRetain, exConnObjectQuoteChar, exConnConnectionName,
    exConnOSAuthentication, exConnSupportsTransaction, exConnMultipleTransaction,
    exConnServerPort, exConnOnLine, exConnTrimChar, exConnQualifiedName,
    exConnCatalogName, exConnSchemaName, exConnObjectName, exConnQuotedObjectName,
    exConnCustomInfo, exConnTimeOut);

  TSQLMonitorP = class(TSQLMonitor);

function TSQLConnectionFix.CloneConnection: TSQLConnection;
var
  SelfParent: TSQLConnection;
  I: Integer;
  {+}//Correction connection string for Cloned Connection
  Status: SQLResult;
  buf : string;
  Len : smallint;
  {+.}
begin      // do not allow nested clones
  if TSQLConnectionH(Self).FIsCloned then
    SelfParent := TSQLConnectionH(Self).FCloneParent
  else
    SelfParent := Self;
  Result := TSQLConnection.Create(nil);
  TSQLConnectionH(Result).FIsCloned := True;
  TSQLConnectionH(Result).FLoadParamsOnConnect := False;
  Result.LoginPrompt := False;
  TSQLConnectionH(Result).FDriverName := TSQLConnectionH(SelfParent).FDriverName;
  TSQLConnectionH(Result).FConnectionName := TSQLConnectionH(SelfParent).FConnectionName;
  Result.Name := SelfParent.Name + 'Clone1';
  TSQLConnectionH(Result).FParams.AddStrings(TSQLConnectionH(SelfParent).FParams);
  TSQLConnectionH(Result).FGetDriverFunc := TSQLConnectionH(SelfParent).FGetDriverFunc;
  TSQLConnectionH(Result).FLibraryName := TSQLConnectionH(SelfParent).FLibraryName;
  TSQLConnectionH(Result).FVendorLib := SelfParent.VendorLib;
  {+}
  if Self.Connected and (TSQLConnectionH(SelfParent).FMaxStmtsPerConn>0) then
  begin
    Len := 0;
    Status := TSQLConnectionH(SelfParent).FISQLConnection.getOption(
      TSQLConnectionOption(exConnConnectionName), nil, 0, Len);
    if (Status<>0)or(Len<=0) then
      Len := 1024;
    SetLength(buf, Len);
    FillChar(buf[1], Len, #0);
    Status := TSQLConnectionH(SelfParent).FISQLConnection.getOption(
      TSQLConnectionOption(exConnConnectionName), PChar(buf), Len, Len);
    if (Status = 0)and(Len>0) then
      Result.Params.Values[DATABASENAME_KEY] := PChar(buf);
  end;
  {+.}
  Result.Connected := Self.Connected;
  TSQLConnectionH(Result).FCloneParent := SelfParent;
  for I := 0 to TSQLConnectionH(Self).FMonitorUsers.Count -1 do
    TSQLMonitorP(TSQLConnectionH(Self).FMonitorUsers[I]).SwitchConnection( Result );
  {+}
  TSQLConnectionH(Result).FTableScope := SelfParent.TableScope;
  {+.}
end;

{$IFNDEF DELPHI_7UP}

const
  CUSTOM_INFO        = 'Custom String';         { Do not localize }
  SERVERPORT         = 'Server Port';           { Do not localize }
  MULTITRANSENABLED  = 'Multiple Transaction';  { Do not localize }
  TRIMCHAR           = 'Trim Char';             { Do not localize }
  CONN_TIMEOUT       = 'Connection Timeout';    { Do not localize }

function GetProfileString(Section, Setting, IniFileName: string): string;
var
  IniFile: TMemIniFile;
  List: TStrings;
begin
  List := TStringList.Create;
  try
    IniFile := TMemIniFile.Create(IniFileName);
    IniFile.ReadSectionValues(Section, List);
    try
      Result := List.Values[ Setting ];
    finally
      IniFile.Free;
    end;
  finally
    List.Free;
  end;
end;

procedure TSQLConnectionFix.DoConnect;

  procedure {TSQLConnection.}CheckLoginParams;
  var
    I: Integer;
  begin
    with TSQLConnectionH(Self) do
    begin

    if FLoadParamsOnConnect then LoadParamsFromIniFile;
    if LoadParamsOnConnect then FDriverName := GetProfileString(FConnectionName, DRIVERNAME_KEY, ConnectionRegistryFile);
    if FDriverName = '' then DataBaseError(SMissingDriverName);
    if LoadParamsOnConnect then
      FLibraryName := GetProfileString(FDriverName, DLLLIB_KEY, GetDriverRegistryFile(csDesigning in ComponentState));
    if FLibraryName = '' then DataBaseError(SMissingDLLName, Self);
    if LoadParamsOnConnect then
      FVendorLib := trim(GetProfileString(FDriverName, VENDORLIB_KEY, GetDriverRegistryFile));
    if FVendorLib = '' then DataBaseError(SMissingDLLName, Self);
    if LoadParamsOnConnect then
      FGetDriverFunc := GetProfileString(FDriverName, GETDRIVERFUNC_KEY, GetDriverRegistryFile);
    if Params.Values[DATABASENAME_KEY] = '' then
    begin
      if FConnectionName = '' then DataBaseError(SConnectionNameMissing)
      else DataBaseError(SMissingDatabaseName);
    end;
    for I := 0 to FMonitorUsers.Count -1 do
      TSQLMonitorP(FMonitorUsers[i]).SetStreamedActive;

    end;
  end;

  procedure {TSQLConnection.}SetCursor(CursorType: Integer);
  begin
    if SQLHourGlass or (CursorType = DefaultCursor) then
      if Assigned(ScreenCursorProc) then
        ScreenCursorProc(CursorType);
  end;

  procedure {TSQLConnection.}Login(LoginParams: TStrings);
  var
    UserName, Password: string;

    function Login: Boolean;
    begin
      with TSQLConnectionH(Self) do
      begin

      Result := Assigned(FOnLogin);
      if Result then FOnLogin(Self, LoginParams);

      end;
    end;

  begin
    with TSQLConnectionH(Self) do
    begin

    if not Login then
    begin
      UserName := LoginParams.Values[szUserName];
      if Assigned(LoginDialogExProc) then
      begin
        SetCursor(DefaultCursor);
        if not LoginDialogExProc(ConnectionName, UserName, Password, False) then
          DatabaseErrorFmt(SLoginError, [ConnectionName]);
        SetCursor(HourGlassCursor);
        LoginParams.Values[szUSERNAME] := UserName;
        LoginParams.Values[szPASSWORD] := Password;
      end;
    end;

    end;
  end;

  procedure {TSQLConnection.}GetLoginParams(LoginParams: TStrings);
  var
    I: Integer;
    PName: string;
  begin
    with TSQLConnectionH(Self) do
    begin

    LoginParams.BeginUpdate;
    try
      LoginParams.Clear;
      for I := 0 to FParams.Count - 1 do
        begin
          if LoginParams.IndexOf(FParams[I]) > -1 then continue;
          PNAME := FParams.Names[I];
          if CompareText(PName, szPASSWORD) = 0 then
             LoginParams.Add(format('%s=%s',[szPASSWORD, FParams.Values[szPASSWORD] ]))
          else if CompareText(PName, szUSERNAME) = 0 then
             LoginParams.Add(format('%s=%s',[szUSERNAME, FParams.Values[szUSERNAME]]))
          else if CompareText(PName, DATABASENAME_KEY) = 0 then
            LoginParams.Add(format('%s=%s',[DATABASENAME_KEY, trim(FParams.Values[DATABASENAME_KEY])]));
        end;
    finally
      LoginParams.EndUpdate;
    end;
    if LoginPrompt then
       Login(LoginParams);

    end;
  end;

  procedure {TSQLConnection.}RegisterTraceCallback(Value: Boolean);
  begin
    with TSQLConnectionH(Self) do
    begin

    if (Value) then
    begin
      if Assigned(FTraceCallbackEvent) and (FTraceClientData <> 0) then
      begin
        Check(FISQLConnection.SetOption(
             TSQLConnectionOption(eConnCallBack), Integer(@FTraceCallbackEvent)));
        Check(FISQLConnection.SetOption(
             TSQLConnectionOption(eConnCallBackInfo), Integer(FTraceClientData)));
      end;
    end else
    begin
      if Assigned(FISQLConnection) then
      begin
        Check(FISQLConnection.SetOption(
              TSQLConnectionOption(eConnCallback), Integer(0)));
        Check(FISQLConnection.SetOption(
              TSQLConnectionOption(eConnCallBackInfo), Integer(0)));
      end;
    end;

    end;
  end;

  procedure {TSQLConnection.}SetConnectionParams;
  var
    ServerCharSet, STransIsolationKey: string;
    ILevel: TTransIsolationLevel;
  begin
    with TSQLConnectionH(Self) do
    begin

    if FParams.Values[HOSTNAME_KEY] <> '' then
      FISQLConnection.SetOption(eConnHostName, LongInt(trim(FParams.Values[HOSTNAME_KEY])));
    if FParams.Values[ROLENAME_KEY] <> '' then
      FISQLConnection.SetOption(eConnRoleName, LongInt(trim(FParams.Values[ROLENAME_KEY])));
    if FParams.Values[WAITONLOCKS_KEY] <> '' then
      FISQLConnection.SetOption(eConnWaitOnLocks, LongInt(UpperCase(trim(FParams.Values[WAITONLOCKS_KEY])) = 'TRUE'));
    if FParams.Values[COMMITRETAIN_KEY] <> '' then
      FISQLConnection.SetOption(eConnCommitRetain, LongInt(UpperCase(trim(FParams.Values[COMMITRETAIN_KEY])) = 'TRUE'));
    if FParams.Values[AUTOCOMMIT_KEY] <> '' then
      FISQLConnection.SetOption(eConnAutoCommit, LongInt(UpperCase(trim(FParams.Values[AUTOCOMMIT_KEY])) = 'TRUE'));
    if FParams.Values[BLOCKINGMODE_KEY] <> '' then
      FISQLConnection.SetOption(eConnBlockingMode, LongInt(UpperCase(trim(FParams.Values[BLOCKINGMODE_KEY])) = 'TRUE'));
    ServerCharSet := trim(FParams.Values[SQLSERVER_CHARSET_KEY]);
    if ServerCharSet <> '' then
      FISQLConnection.SetOption(eConnServerCharSet, LongInt(PChar(ServerCharSet)));
    //FTransIsoLevel := xilReadCommitted;
    STransIsolationKey := Format(TRANSISOLATION_KEY, [DriverName]);
    if FParams.Values[STransIsolationKey] <> '' then
    begin
      if LowerCase(FParams.Values[STransIsolationKey]) = SRepeatRead then
        ILevel := xilRepeatableRead
      else if LowerCase(FParams.Values[STransIsolationKey]) = SDirtyRead then
        ILevel := xilDirtyRead
      else
        ILevel := xilReadCommitted;
      //FTransIsoLevel := ILevel;
      FISQLConnection.SetOption(eConnTxnIsoLevel, LongInt(ILevel));
    end;
    if FParams.Values[SQLDIALECT_KEY] <> '' then
      FISQLConnection.SetOption(eConnSQLDialect, LongInt(StrToInt(trim(FParams.Values[SQLDIALECT_KEY]))));

    if FParams.Values[OSAUTHENTICATION] <> '' then
      FISQLConnection.SetOption(TSQLConnectionOption(exConnOSAuthentication), LongInt(UpperCase(trim(FParams.Values[OSAUTHENTICATION])) = 'TRUE'));
    if FParams.Values[SERVERPORT] <> '' then
      FISQLConnection.SetOption(TSQLConnectionOption(exConnServerPort), LongInt(trim(FParams.Values[SERVERPORT])));
    if FParams.Values[MULTITRANSENABLED] <> '' then
      FISQLConnection.SetOption(TSQLConnectionOption(exConnMultipleTransaction), LongInt(UpperCase(trim(FParams.Values[MULTITRANSENABLED])) = 'TRUE'));
    if FParams.Values[TRIMCHAR] <> '' then
      FISQLConnection.SetOption(TSQLConnectionOption(exConnTrimChar), LongInt(UpperCase(trim(FParams.Values[TRIMCHAR])) = 'TRUE'));

    if FParams.Values[CUSTOM_INFO] <> '' then
      FISQLConnection.SetOption(TSQLConnectionOption(exConnCustomInfo), LongInt(trim(FParams.Values[CUSTOM_INFO])));
    if FParams.Values[CONN_TIMEOUT] <> '' then
      FISQLConnection.SetOption(TSQLConnectionOption(exConnTimeOut), LongInt(StrToInt(trim(FParams.Values[CONN_TIMEOUT]))));

    end;
  end;

var
  Status: SQLResult;
  LoginParams: TStrings;
  PropSize: SmallInt;
  TrimmedUserName: string;
begin
  with TSQLConnectionH(Self) do
  begin

  CheckLoginParams;
  ConnectionState := csStateConnecting;
  LoadSQLDll;
  LoginParams := TStringList.Create;
  try
    SetCursor(HourGlassCursor);
    Status := getDriver(PChar(FVendorLib), PChar(Trim(FParams.Values[ERROR_RESOURCE_KEY])), FSQLDriver);
    if Status <> DBXERR_NONE then
      DataBaseErrorFmt(sDLLLoadError, [FVendorLib]);
    Check(FSQLDriver.setOption(eDrvRestrict, GDAL));
    Check(FSQLDriver.getSQLConnection(FISQLConnection));
    GetLoginParams(LoginParams);
    SetCursor(HourGlassCursor);
    RegisterTraceCallback(True);
    SetConnectionParams;
    Check(Connection.connect(PChar(trim(LoginParams.Values[DATABASENAME_KEY])), PChar(LoginParams.Values[ szUSERNAME ]),
         PChar(LoginParams.Values[ szPASSWORD ])));
    FISQLConnection.getOption(TSQLConnectionOption(exConnMaxActiveComm), @FMaxStmtsPerConn, Sizeof(Integer), PropSize);
    Check(Connection.getSQLMetaData(FSQLMetaData));
    TrimmedUserName := trim(LoginParams.Values[ szUSERNAME ]);
    if TrimmedUserName <> '' then
      FSQLMetaData.SetOption(eMetaSchemaName, LongInt(TrimmedUserName));
    ConnectionOptions;
    ConnectionState := csStateOpen;
  finally
    SetCursor(DefaultCursor);
    LoginParams.Free;
    if ConnectionState = csStateConnecting then
    begin
      ConnectionState := csStateClosed;
      SQLDllHandle := THandle(0);
      if Assigned(FISQLConnection) then
        FISQLConnection := nil;
    end;
  end;

  end;
end;
{$ENDIF IFNDEF DELPHI_7UP}

{$IFDEF DELPHI_7UP}

function FixParams(SQL: string; Count: Integer; QuoteChar: string): string; forward;

procedure SetQueryParams(const Sender: TSQLConnection; const Command: ISQLCommand;
  const Params: TParams); forward;

function TSQLConnectionFix.Execute(const SQL: string; Params: TParams;
  ResultSet: Pointer = nil): Integer;
var
  Status: SQLResult;
  SQLText: string;
  RowsAffected: LongWord;
  DS: TCustomSQLDataSet;
  I, ParamCount: Integer;
begin
  Result := 0;
  DS := TCustomSQLDataSet.Create(nil);
  with TSQLConnectionH(Self) do
  try
    CheckConnection(eConnect);
//    SetCursor(HourGlassCursor);
    DS.SQLConnection := Self;
    ConnectionState := csStateExecuting;
    if (Params <> nil) and (Params.Count > 0) then
    begin
      SQLText := FixParams(SQL, Params.Count, Self.GetQuoteChar);
      ParamCount := Params.Count;
    end else
    begin
      SQLText := Copy(SQL, 1, Length(SQL));
      ParamCount := 0;
    end;
    TCustomSQLDataSetH(DS).FCommandText := SQLText;
    if ResultSet = nil then
    begin
      TCustomSQLDataSetFixD7(DS).CheckStatement;
      Status := TCustomSQLDataSetFixD7(DS).FSQLCommand.prepare(PChar(SQLText), ParamCount);
      if Status <> DBXERR_NONE then
        SQLError(Status, exceptCommand, TCustomSQLDataSetFixD7(DS).FSQLCommand);
      if ParamCount > 0 then
        SetQueryParams(Self, TCustomSQLDataSetFixD7(DS).FSQLCommand, Params);
      Status := TCustomSQLDataSetFixD7(DS).FSQLCommand.execute(TCustomSQLDataSetFixD7(DS).FSQLCursor);
      if Status <> DBXERR_NONE then
        SQLError(Status, exceptCommand, TCustomSQLDataSetFixD7(DS).FSQLCommand);
      Status := TCustomSQLDataSetFixD7(DS).FSQLCommand.getRowsAffected(RowsAffected);
      if Status <> DBXERR_NONE then
        SQLError(Status, exceptCommand, TCustomSQLDataSetFixD7(DS).FSQLCommand);
      Result := RowsAffected;
    end else
    begin
      if ParamCount > 0 then
      begin
        for I := 0 to ParamCount -1 do
        begin
          TCustomSQLDataSetP(DS).Params.CreateParam(Params.Items[I].DataType, format('P%d',[I+1]),
             ptInput);
          TCustomSQLDataSetP(DS).Params[I].Value := Params[I].Value;
        end;
      end;
      TCustomSQLDataSetP(DS).MaxBlobSize := DefaultMaxBlobSize;
      DS.Active := True;
    end;
  finally
    SetCursor(DefaultCursor);
    if ResultSet = nil then
      DS.Free
    else
      TCustomSQLDataSet(ResultSet^) := DS;
    ConnectionState := csStateOpen;
  end;
end;

function TSQLConnectionFix.GetQuoteChar: string;
var
  Status: SQLResult;
  Len: SmallInt;
  Q: Char;
begin
  with TSQLConnectionH(Self) do
  begin
    FQuoteChar := '';
    Len := 1;
    Q := #0;
    Status := FSQLMetadata.getOption(eMetaObjectQuoteChar, @Q, Len, Len);
    if (Q <> #0) and (Status = DBXERR_NONE) then
      FQuoteChar := Q;
    Result := FQuoteChar;
  end;
end;

{$ENDIF IFDEF DELPHI_7UP}

  function TCustomSQLDataSetFixD7.AddQuoteCharToObjectName({DS : TCustomSQLDataSet;} Name, Q: string): string;
  var
    Status: SQLResult;
    P: PChar;
    Len : smallint;
    buf : array [0..255] of char;
  begin
    with TCustomSQLDataSetP(Self) do
    begin

    Result := '';
    FillChar(buf, SizeOf(buf), #0);
    P := PChar(Name);
    Status := TSQLConnectionH({DS.}{Get}InternalConnection).FISQLConnection.setOption(TSQLConnectionOption(exConnQualifiedName), LongInt(P));
    if Status <> 0 then
      {DS.}SQLError(Status, exceptConnection);
    Status := TSQLConnectionH({DS.}{Get}InternalConnection).FISQLConnection.getOption(TSQLConnectionOption(exConnQuotedObjectName), @buf, SizeOf(buf), Len);
    if Status <> 0 then
      {DS.}SQLError(Status, exceptConnection);
    Result := buf;

    end;
  end;

  function TCustomSQLDataSetFixD7.GetQueryFromType: string;
  {$IFDEF DELPHI_7UP}
  var
    STableName : String;
  {$ENDIF}
  begin
    with TCustomSQLDataSetP(Self) do
    begin

    if (TObject(Self) is TSQLTable) and (FNativeCommand <> '') then
    begin
      Result := FNativeCommand;
      exit;
    end;

    case CommandType of
       ctTable:
         begin
           {$IFDEF DELPHI_7UP}
           if {Self.}FSchemaName <> '' then
             STableName := AddQuoteCharToObjectName({Self,} FSchemaName + '.' + FCommandText,
                        TSQLConnectionP(FSQLConnection).QuoteChar)
           else
             STableName := AddQuoteCharToObjectName({Self,} FCommandText, TSQLConnectionP(FSQLConnection).QuoteChar);
           if FSortFieldNames > '' then
             Result := SSelectStarFrom + STableName + SOrderBy + FSortFieldNames
           else
             if FNativeCommand = '' then
               Result := SSelectStarFrom + STableName
             else
             begin
               {+}
               if Trim(Self.FSchemaName) <> '' then
                 STableName := AddQuoteCharToObjectName({Self,} FSchemaName + '.' + FNativeCommand, TSQLConnectionP(FSQLConnection).QuoteChar)
               else
                 STableName := AddQuoteCharToObjectName({Self,} FNativeCommand, TSQLConnectionP(FSQLConnection).QuoteChar);
               {+.}
               Result := SSelectStarFrom + STableName;
             end;
           {$ENDIF}
           {$IFNDEF DELPHI_7UP}
           if FNativeCommand = '' then
             Result := SSelectStarFrom + AddQuoteCharToObjectName(FCommandText,
                    TSQLConnectionP(FSQLConnection).QuoteChar)
           else
             Result := SSelectStarFrom + AddQuoteCharToObjectName(FNativeCommand,
                    TSQLConnectionP(FSQLConnection).QuoteChar);
           {$ENDIF}
         end;
       ctStoredProc:
         begin
           {$IFDEF DELPHI_7UP}
           if FSchemaName <> '' then
             Result := FSchemaName + '.' + copy(FCommandText, 1, Length(FCommandText))
           else
           {$ENDIF}
             Result := copy(FCommandText, 1, Length(FCommandText));
         end;
       else
         if (FSortFieldNames > '') and (Pos(SOrderBy,
            LowerCase(FCommandText)) = 0) then
           Result := FNativeCommand + SOrderBy + FSortFieldNames
         else
           Result := FNativeCommand;
    end;

    end;
  end;

procedure TCustomSQLDataSetFixD7.PrepareStatement;


  function {TCustomSQLDataSet.}CheckDetail(const SQL: string): string;
  begin
    with TCustomSQLDataSetP(Self) do
    begin

    Result := SQL;
    if pos(SParam, SQL) = 0 then
      if pos(SSelect, LowerCase(SQL)) > 0 then // Select Query with no ?, but Parameters are set
        Result := AddParamSQLForDetail(Params, SQL, True);

    end;
  end;

var
  SQLText: string;
begin
  with TCustomSQLDataSetP(Self) do
  begin

  if Length(CommandText) = 0 then
    DatabaseError(SEmptySQLStatement, Self);
  CheckStatement;
  SQLText := GetQueryFromType;
  if Params.Count > 0 then
    SQLText := CheckDetail(SQLText);
  if CommandType = ctStoredProc then
    Check(FSQLCommand.SetOption(eCommStoredProc, Integer(True)))
  else
    Check(FSQLCommand.SetOption(eCommStoredProc, Integer(False)));
  Check(FSQLCommand.prepare(PChar(SQLText), ParamCount));

  end;
end;

function TCustomSQLDataSetFixD7.PSGetDefaultOrder: TIndexDef;

  function FieldsInQuery(IdxFields: string): Boolean;
  var
    I:  Integer;
    IdxFlds, Flds: TStrings;
    FldNames: string;
  begin
    Result := True;
    IdxFlds := TStringList.Create;
    try
      IdxFlds.CommaText := IdxFields;
      Flds := TStringList.Create;
      try
        Fields.GetFieldNames(Flds);
        FldNames := Flds.CommaText;
        for I := 0 to IdxFlds.Count -1 do
        begin
          if pos(IdxFlds[I], FldNames) = 0 then
          begin
            Result := False;
            exit;
          end;
        end;
      finally
        Flds.Free;
      end;
    finally
      IdxFlds.Free;
    end;
  end;

var
  I: Integer;
begin
  with TCustomSQLDataSetP(Self) do
  begin

  Result := inherited PSGetDefaultOrder;
  if not Assigned(Result) then
    Result := GetIndexForOrderBy(GetQueryFromType, Self);
  if (not Assigned(Result)) and
     (CommandType <> ctStoredProc) and (SchemaInfo.FType = stNoSchema) then
  begin
    if not FIndexDefsLoaded then
      AddIndexDefs(TCustomSQLDataSet(Self));
    for I := 0 to IndexDefs.Count - 1 do
    begin
      if (ixPrimary in TIndexDef(IndexDefs[I]).Options) and
         FieldsInQuery(TIndexDef(IndexDefs[I]).Fields) then
      begin
        Result := TIndexDef.Create(nil);
        Result.Assign(IndexDefs[I]);
        Break;
      end;
    end;
  end;

  end;
end;

procedure TCustomSQLDataSetFixD7.OpenSchema;

  // Damage of a code: access abroad the buffer.
  // Crash of the application when the name of the database exceeds 256 characters.
  // Such long name is possible at usage dbxoodbc.
  // The name of database file can be set as a file path.
  procedure {TCustomSQLDataSet.}SetSchemaOption;
  var
    Status: SQLResult;
    Len : smallint;
    {+}
    buf0,buf1,buf2 : String;
    UserName : String;
    ObjectName : String;
    CatalogName : String;
    {+.}
  begin
    with TCustomSQLDataSetP(Self) do
    begin

    Status := 0;
    ObjectName := FSchemaInfo.ObjectName;
    SetLength(buf0, 256);
    SetLength(buf1, 256);
    SetLength(buf2, 256);
    FillChar(buf0[1], Length(buf0), #0);
    FillChar(buf1[1], Length(buf1), #0);
    if ObjectName <> '' then
    begin
      Status := TSQLConnectionH({Get}InternalConnection).FISQLConnection.setOption(TSQLConnectionOption(exConnQualifiedName), LongInt(ObjectName));
      if Status <> 0 then
        SQLError(Status, exceptConnection);
      {+}
      try
        Len := 0;
        Status := TSQLConnectionH({Get}InternalConnection).FISQLConnection.getOption(TSQLConnectionOption(exConnCatalogName), PChar(buf0), 0, Len);
        if (Status = 0) and (Len > Length(buf0)) then
        begin
          SetLength(buf0, Len);
          FillChar(buf0[1], Length(buf0), #0);
        end;
      except
        //no open odbc driver and it driver not check buffer length or not return required buffer length
      end;
      {+.}
      Status := TSQLConnectionH({Get}InternalConnection).FISQLConnection.getOption(TSQLConnectionOption(exConnCatalogName), PChar(buf0), Length(buf0), Len);
      if Status <> 0 then
        SQLError(Status, exceptConnection);
      {+}
      try
        Len := 0;
        Status := TSQLConnectionH({Get}InternalConnection).FISQLConnection.getOption(TSQLConnectionOption(exConnSchemaName), PChar(buf1), 0, Len);
        if (Status = 0) and (Len > Length(buf1)) then
        begin
          SetLength(buf1, Len);
          FillChar(buf1[1], Length(buf1), #0);
        end;
      except
        //no open odbc driver and it driver not check buffer length or not return required buffer length
      end;
      {+.}
      Status := TSQLConnectionH({Get}InternalConnection).FISQLConnection.getOption(TSQLConnectionOption(exConnSchemaName), PChar(buf1), Length(buf1), Len);
      if Status <> 0 then
        SQLError(Status, exceptConnection);
      FillChar(buf2[1], Length(buf2), #0);
      {+}
      try
        Len := 0;
        Status := TSQLConnectionH({Get}InternalConnection).FISQLConnection.getOption(TSQLConnectionOption(exConnObjectName), PChar(buf2), 0, Len);
        if (Status = 0) and (Len > Length(buf2)) then
        begin
          SetLength(buf2, Len);
          FillChar(buf2[1], Length(buf2), #0);
        end;
      except
        //no open odbc driver and it driver not check buffer length or not return required buffer length
      end;
      {+.}
      Status := TSQLConnectionH({Get}InternalConnection).FISQLConnection.getOption(TSQLConnectionOption(exConnObjectName), PChar(buf2), Length(buf2), Len);
      if Status <> 0 then
        SQLError(Status, exceptConnection);
      FSchemaInfo.ObjectName := PChar(buf2);
    end;
    if buf0[1] = #0 then
    begin
      CatalogName := TSQLConnectionH({Get}InternalConnection).FParams.Values[DATABASENAME_KEY];
      if CatalogName <> '' then
        buf0 := CatalogName; // *** !!! FIXED: CatalogName can be very long (full name can contain login, network and other options).
    end;
    if buf0[1] <> #0 then // set catalog name option
      Status := TSQLConnectionH({Get}InternalConnection).FSQLMetaData.setOption(eMetaCatalogName, LongInt(PChar(buf0)));
    if Status <> 0 then
      SQLError(Status, exceptMetaData);

    {+}
    (*
    try
      SetLength(buf0, 256);
      Len := 0;
      Status := TSQLConnectionH({Get}InternalConnection).FISQLConnection.getOption(TSQLConnectionOption(eMetaCatalogName), PChar(buf1), 0, Len);
      if (Status = 0) then
      begin
        SetLength(buf0, Len);
        FillChar(buf1[0], Length(buf0), #0);
        Status := TSQLConnectionH({Get}InternalConnection).FISQLConnection.getOption(TSQLConnectionOption(exConnObjectName), PChar(buf0), Length(buf0), Len);
        CatalogName := buf0;
      end;
    except
      //no open odbc driver and it driver not check buffer length or not return required buffer length
    end;
    //*)
    {+.}

    {$IFDEF DELPHI_7UP}
    if {(buf1[1] = #0) and} (SchemaName <> '') then
      buf1 := SchemaName; // *** FIXED: SchemaName can be very long.
    {$ENDIF}
    if buf1[1] = #0 then
    begin
      UserName := TSQLConnectionH({Get}InternalConnection).FParams.Values[szUSERNAME];
      if UserName <> '' then
        buf1 := UserName;
    end;
    if buf1[1] <> #0 then // set schema name option
  {+.}
      Status := TSQLConnectionH({Get}InternalConnection).FSQLMetaData.setOption(eMetaSchemaName, LongInt(PChar(buf1)));
    if Status <> 0 then
      SQLError(Status, exceptMetaData);

    end;
  end;

  function GetTableScope(Scope: TTableScopes): LongWord;
  begin
    Result := 0;
    if tsTable in Scope then
      Result := Result OR eSQLTable;
    if tsView in Scope then
      Result := Result OR eSQLView;
    if tsSysTable in Scope then
      Result := Result OR eSQLSystemTable;
    if tsSynonym in Scope then
      Result := Result OR eSQLSynonym;
  end;

var
  Status: SQLResult;
  TblType: LongWord;
  WildCard: PChar;
begin
  with TCustomSQLDataSetP(Self) do
  begin

  Status := SQL_NULL_DATA;
  if FSQLConnection = nil then
    DatabaseError(sConnectionNameMissing);
  if FSchemaInfo.Pattern = '' then
    WildCard := nil
  else
    WildCard := PChar(FSchemaInfo.Pattern);
  SetSchemaOption;
  //OLD Style (Delphi 6):
  //FSchemaInfo.ObjectName := ExtractObjectName(FSchemaInfo.ObjectName);
  case FSchemaInfo.FType of
    stTables:
    begin
      TblType := GetTableScope({Get}TSQLConnectionH(InternalConnection).FTableScope);
      Status := {Get}TSQLConnectionH(InternalConnection).FSQLMetaData.getTables(
                  WildCard, TblType, FSQLCursor)
    end;
    stSysTables:
      Status := {Get}TSQLConnectionH(InternalConnection).FSQLMetaData.getTables(
                  WildCard, eSQLSystemTable, FSQLCursor);
    stColumns:
      Status := {Get}TSQLConnectionH(InternalConnection).FSQLMetaData.getColumns(
                  PChar(FSchemaInfo.ObjectName),
                  PChar(FSchemaInfo.Pattern), 0, FSQLCursor);
    stProcedures:
    begin
      {$IFDEF DELPHI_7UP}
      Status := {Get}TSQLConnectionH(InternalConnection).FSQLMetaData.setOption(
                  eMetaPackageName, Integer(PChar(FSchemaInfo.PackageName)));
      if Status = DBXERR_NONE then
      {$ENDIF}
        Status := {Get}TSQLConnectionH(InternalConnection).FSQLMetaData.getProcedures(
                    WildCard, eSQLProcedure, FSQLCursor);
    end;
    {$IFDEF DELPHI_7UP}
    stPackages:
      Status := {Get}TSQLConnectionH(InternalConnection).FSQLMetaData.getObjectList(
                  eObjTypePackage, FSQLCursor);
    {$ENDIF}
    stProcedureParams:
    begin
      {$IFDEF DELPHI_7UP}
      Status := {Get}TSQLConnectionH(InternalConnection).FSQLMetaData.setOption(
                  eMetaPackageName, Integer(PChar(FSchemaInfo.PackageName)));
      if Status = DBXERR_NONE then
      {$ENDIF}
        Status := {Get}TSQLConnectionH(InternalConnection).FSQLMetaData.getProcedureParams(
                    PChar(FSchemaInfo.ObjectName), WildCard,
                    FSQLCursor);
    end;
    stIndexes:
      Status := {Get}TSQLConnectionH(InternalConnection).FSQLMetaData.getIndices(
                  PChar(FSchemaInfo.ObjectName), 0, FSQLCursor);
  end;
  if Status <> DBXERR_NONE then
    {Get}TSQLConnectionP(InternalConnection).SQLError(Status, exceptMetaData);

  end;
end;

procedure CalcUnits( const Params: TParams; const ProcParams: TList;
          const Index: Integer; pArgDesc: pSPParamDesc; var ChildPos: array of Word );
var
  I: Integer;
  ArgDesc: SPParamDesc;
begin
  I := Index + 1;
  ArgDesc := pArgDesc^;
  pArgDesc.iUnits1 := 0;
  pArgDesc.iUnits2 := 0;
  while (I < Params.Count) do
  begin
    ArgDesc := (PSPParamDesc(ProcParams.Items[I]))^;
    if ArgDesc.iParamNum <> pArgDesc.iParamNum then
      break;
    Inc(pArgDesc.iUnits1);
    Inc(pArgDesc.iUnits2);
    ChildPos[I] := I - Index;
    if ArgDesc.iDataType = ftADT then
    begin
      CalcUnits(Params, ProcParams, I, @ArgDesc, ChildPos);
      Inc(pArgDesc.iUnits2, ArgDesc.iUnits2);
      Inc(I, ArgDesc.iUnits2);
    end else
      Inc(I);
  end;
end;

procedure GetParamData(Param: TParam; Buffer: Pointer; const DrvLocale: TLocale);

  function GetNativeStr: PChar;
  begin
    Param.NativeStr := VarToStr(Param.Value);
    Result := PChar(Param.NativeStr);
  end;

begin
  if Buffer <> nil then
  begin
    with Param do
      if DataType in [ftString, ftFixedChar, ftMemo]  then
      begin
        NativeStr := VarToStr(Value);
        GetData(Buffer);
      end
      else
        GetData(Buffer);
  end;
end;

{.$IFDEF SqlExpr_TCustomSQLDataSet_SetSchemaOption}
procedure SetProcedureParams(const Sender: TSQLConnection;
  const Command: ISQLCommand; const Params: TParams; ProcParams: TList);
var
  I, IInd, DataLen: Integer;
  iFldNum: LongWord;
  RecBuffer: PWideChar;
  iFldType, iSubType: Word;
  DrvLocale: TLocale;
  Status: SQLResult;
  ArgDesc: SPParamDesc;
  ChildPosArray: array of Word;
  SBcd: string;
  Bcd: TBcd;
begin
  DrvLocale := nil;
  SetLength(ChildPosArray, Params.Count);
  for I := 0 to Params.Count - 1 do
    begin
      RecBuffer := nil;
      try
        if Params[I].ParamType = ptUnknown then  // Midas assumes its Input
          Params[I].ParamType := ptInput;
        iFldNum := i + 1;
        iFldType := FldTypeMap[Params[I].DataType];
        iSubType := 0;
        if iFldType in [fldBlob, fldZString] then
          iSubType := Word(FldSubTypeMap[Params[I].DataType])
        else if iFldType = fldUNKNOWN then
          DatabaseErrorFmt(SNoParameterValue, [Params[I].Name]);
        if ProcParams <> nil then
          ArgDesc := (PSPParamDesc(ProcParams.Items[I]))^
        else
          with ArgDesc, Params[i] do
            begin
              iParamNum := iFldNum;
              szName := Name;
              iArgType := ParamType;
              iDataType := DataType;
              iUnits1 := Precision;
              iUnits2 := NumericScale;
              iLen := GetDataSize;
            end;
        iFldType := FldTypeMap[ArgDesc.iDataType];
        if Params[I].ParamType <> ptOutput then
          DataLen := Params[I].GetDataSize
        else
          DataLen := ArgDesc.iLen;
        {Check if the IN param is NULL and set the NULL indicator}
        if ((Params[I].ParamType = ptInput) and Params[I].IsNull) then
          iInd := 1
        else
        if (DataLen > 0) then
        begin
          iInd := 0;
          RecBuffer := AllocMem(DataLen);
          if Params[I].ParamType <> ptOutput then
            GetParamData(Params[I], RecBuffer, DrvLocale)
          else
            FillChar(RecBuffer^, DataLen, 0);
          if Params[I].ParamType = ptInput then
            Params[I].Size := 0;
          if (Params[I].ParamType = ptOutput) and not(iFldType in [fldFLOAT]) then
            ArgDesc.iLen := 0
          else
            case iFldType of
              fldBlob:
                 begin
                  ArgDesc.iLen := DataLen;
                  ArgDesc.iUnits2 := 0;
                  if ( (iSubType = fldstMemo) or (iSubType = fldstHMemo) {or
                       (iSubType = fldstWideMemo)} ) then
                    begin
                      if (DataLen > 0 ) then
                      begin
                        Params[I].Size := DataLen - 1; //Max precision
                        ArgDesc.iLen := DataLen -1;    //Length
                      end;
                    end;
                 end;
              fldZString, fldBYTES, fldVARBYTES:
                begin
                  ArgDesc.iLen := DataLen;
                  ArgDesc.iUnits2 := 0;

                  //Handle ptInput
                  if (Params[I].ParamType = ptInput) then
                  begin
                    if iFldType = fldVARBYTES then
                      Params[I].Size := DataLen - 2
                    else if iFldType = fldZString then
                    begin
                      if (DataLen > 0 ) then
                        Params[I].Size := DataLen - 1
                    end
                    else
                      Params[I].Size := DataLen;
                  end;
                  //Handle ptInput

                  if (Params[I].ParamType = ptInputOutput) and (DataLen > Params[I].Size) then
                  begin
                    if iFldType = fldVARBYTES then
                      Params[I].Size := DataLen - 2
                    else if iFldType = fldZString then
                      Params[I].Size := DataLen - 1
                    else
                      Params[I].Size := DataLen;
                  end;
                end;
              fldFLOAT:
                begin
                  if Params[I].Precision = 4 then
                    ArgDesc.iLen := 4
                  else
                    ArgDesc.iLen := Sizeof(Double);
                end;
              fldFMTBCD, fldBCD:
                begin
                  iFldType := fldBCD;   { DBExpress does not distinguish }
                  if Params[I].Size = 0 then
                  begin
                    SBcd := Params[I].Text;
                    //SBcd := BcdToStr(PBcd(RecBuffer)^);
                    Bcd := StrToBcd(SBcd);
                    Params[I].Size := Bcd.Precision;
                    ArgDesc.iUnits2 := Bcd.SignSpecialPlaces AND $3F;
                  end else
                  begin
                    ArgDesc.iUnits2 := Params[I].NumericScale;
                  end;
                end;
              fldADT, fldARRAY:
                begin
                  CalcUnits(Params, ProcParams, I, @ArgDesc, ChildPosArray);
                  ArgDesc.iLen := DataLen;
                end;
            end;
        end else  // leave RecBuffer nil
        begin
          if iFldType in [fldADT, fldARRAY] then
            DatabaseError(SObjectTypenameRequired);
          iInd := 1;
        end;
        Status := Command.setParameter(iFldNum - ChildPosArray[I], ChildPosArray[I], TSTMTParamType(ArgDesc.iArgType),
                iFldType, iSubType, Params[I].Size,
                Integer(ArgDesc.iUnits2), ArgDesc.iLen, RecBuffer, IInd);
        if (Status <> DBXERR_NONE) then
          TSQLConnectionP(Sender).SQLError(Status, exceptConnection);
      finally
        if RecBuffer <> nil then FreeMem(RecBuffer);
      end;
    end;
end;
{.$ENDIF}

(*
procedure SetProcedureParams(const Sender: TSQLConnection;
  const Command: ISQLCommand; const Params: TParams; ProcParams: TList);
var
  I, IInd, DataLen: Integer;
  iFldNum: LongWord;
  RecBuffer: PChar;
  iFldType, iSubType: Word;
  DrvLocale: TLocale;
  Status: SQLResult;
  ArgDesc: SPParamDesc;
  ChildPosArray: array of Word;
  SBcd: string;
  Bcd: TBcd;
begin
  DrvLocale := nil;
  SetLength(ChildPosArray, Params.Count);
  for I := 0 to Params.Count - 1 do
    begin
      RecBuffer := nil;
      try
        if Params[I].ParamType = ptUnknown then  // Midas assumes its Input
          Params[I].ParamType := ptInput;
        iFldNum := i + 1;
        iFldType := FldTypeMap[Params[I].DataType];
        iSubType := 0;
        if iFldType in [fldBlob, fldZString] then
          iSubType := Word(FldSubTypeMap[Params[I].DataType])
        else if iFldType = fldUNKNOWN then
          DatabaseErrorFmt(SNoParameterValue, [Params[I].Name]);
        ArgDesc := (PSPParamDesc(ProcParams.Items[I]))^;
        iFldType := FldTypeMap[ArgDesc.iDataType];
        if Params[I].ParamType <> ptOutput then
          DataLen := Params[I].GetDataSize
        else
          DataLen := ArgDesc.iLen;
        {Check if the IN param is NULL and set the NULL indicator}
        if ((Params[I].ParamType = ptInput) and Params[I].IsNull) then
          iInd := 1
        else
        if (DataLen > 0) then
        begin
          iInd := 0;
          RecBuffer := AllocMem(DataLen);
          if Params[I].ParamType <> ptOutput then
            GetParamData(Params[I], RecBuffer, DrvLocale)
          else
            FillChar(RecBuffer^, DataLen, 0);
          if Params[I].ParamType = ptInput then
            Params[I].Size := 0;
          if (Params[I].ParamType = ptOutput) and not(iFldType in [fldFLOAT]) then
            ArgDesc.iLen := 0
          else
            case iFldType of
              fldBlob, fldZString, fldBYTES, fldVARBYTES {$IFDEF _UNICODE_}, fldWIDESTRING{$ENDIF}: //???: not tested "fldWIDESTRING"
                begin
                  ArgDesc.iLen := DataLen;
                  ArgDesc.iLen := DataLen; //???: Why that the line is produplicated?
                  ArgDesc.iUnits2 := 0;
                  if (Params[I].ParamType = ptInputOutput) and (DataLen > Params[I].Size) then
                  begin
                    if iFldType = fldVARBYTES then
                      Params[I].Size := DataLen - 2
                    else if iFldType = fldZString then
                    begin
                      {$IFDEF _UNICODE_}
                      if iSubType = fldstUNICODE then
                        Params[I].Size := DataLen div SizeOf(WideChar) - 1
                      else
                      {$ENDIF}
                      Params[I].Size := DataLen - 1;
                    end
                    {$IFDEF _UNICODE_}
                    else if iFldType = fldWIDESTRING then
                      Params[I].Size := DataLen div SizeOf(WideChar) - 1
                    {$ENDIF}
                    else
                      Params[I].Size := DataLen;
                  end;
                end;
              fldFLOAT:
                begin
                  if Params[I].Precision = 4 then
                    ArgDesc.iLen := 4
                  else
                    ArgDesc.iLen := Sizeof(Double);
                end;
              fldFMTBCD, fldBCD:
                begin
                  iFldType := fldBCD;   { DBExpress does not distinguish }
                  if Params[I].Size = 0 then
                  begin
                    SBcd := BcdToStr(PBcd(RecBuffer)^);
                    Bcd := StrToBcd(SBcd);
                    Params[I].Size := Bcd.Precision;
                    ArgDesc.iUnits2 := Bcd.SignSpecialPlaces AND $3F;
                  end else
                  begin
                    ArgDesc.iUnits2 := Params[I].NumericScale;
                  end;
                end;
              fldADT, fldARRAY:
                begin
                  CalcUnits(Params, ProcParams, I, @ArgDesc, ChildPosArray);
                  ArgDesc.iLen := DataLen;
                end;
            end;
        end else  // leave RecBuffer nil
        begin
          if iFldType in [fldADT, fldARRAY] then
            DatabaseError(SObjectTypenameRequired);
          iInd := 1;
        end;
        Status := Command.setParameter(iFldNum - ChildPosArray[I], ChildPosArray[I], TSTMTParamType(ArgDesc.iArgType),
                iFldType, iSubType, Params[I].Size,
                Integer(ArgDesc.iUnits2), ArgDesc.iLen, RecBuffer, IInd);
        if (Status <> DBXERR_NONE) then
          TSQLConnectionP(Sender).SQLError(Status, exceptConnection);
      finally
        if RecBuffer <> nil then FreeMem(RecBuffer);
      end;
    end;
end;
//*)

procedure SetQueryParams(const Sender: TSQLConnection; const Command: ISQLCommand;
  const Params: TParams);
var
  I, IInd: Integer;
  NumBytes, iFldNum: LongWord;
  RecBuffer: PChar;
  iFldType, iSubType: Word;
  iUnits1, iUnits2: Integer;
  DrvLocale: TLocale;
  Status: SQLResult;
  SBcd: string;
  Bcd: TBcd;
begin
  DrvLocale := nil;
  for I := 0 to Params.Count - 1 do
    begin
      RecBuffer := nil;
      if Params[I].IsNull then iInd := 1 else iInd := 0;
      try
        if Params[I].ParamType = ptUnknown then  // Midas assumes its Input
          Params[I].ParamType := ptInput;
        iFldNum := i + 1;
        iFldType := FldTypeMap[Params[I].DataType];
        iSubType := 0;
        iUnits1 := 0;
        iUnits2 := 0;
        if iFldType in [fldBlob, fldZString] then
          iSubType := Word(FldSubTypeMap[Params[I].DataType])
        else if iFldType = fldUNKNOWN then
          DatabaseErrorFmt(SNoParameterValue, [Params[I].Name]);
        NumBytes := Params[I].GetDataSize;
        if iInd = 1 then
          RecBuffer := nil
        else begin
          RecBuffer := AllocMem(NumBytes);
          GetParamData(Params[I], RecBuffer, DrvLocale);
        end;
        case iFldType of
          fldZString:
            begin
              if NumBytes > 0 then
              {$IFNDEF _UNICODE_}
              //OLD:
                iUnits1 := Params[I].GetDataSize - 1    {Do not include null terminator}
              {$ELSE}
              //NEW:
              begin
                {+}
                if iSubType = fldstUNICODE then
                  iUnits1 := Params[I].GetDataSize - SizeOf(Word) {Do not include null terminator}
                else
                  iUnits1 := Params[I].GetDataSize - 1; {Do not include null terminator}
                {+.}
              end
              {$ENDIF}
              else
                iUnits1 := 0;
              iUnits2 := 0;
            end;
          fldBLOB:
            iUnits1 := NumBytes;
          fldBYTES:
            iUnits1 := Params[I].GetDataSize;
          fldVARBYTES:
            iUnits1 := Params[I].GetDataSize - 2;
          fldFLOAT:
            iUnits1 := SizeOf(double);
          fldFMTBCD, fldBCD:
            begin
              iFldType := fldBCD;   { DBExpress does not distinguish }
              if iInd = 0 then
              begin
                SBcd := BcdToStr(PBcd(RecBuffer)^);
                Bcd := StrToBcd(SBcd);
                iUnits1 := Bcd.Precision;
                iUnits2 := Bcd.SignSpecialPlaces AND $3F;
              end;
            end
        end;
        Status := Command.setParameter(iFldNum, 0, TSTMTParamType(Params[I].ParamType),
                iFldType, iSubType, Integer(iUnits1), Integer(iUnits2), NumBytes, RecBuffer, IInd);
        if (Status <> DBXERR_NONE) then
          TSQLConnectionP(Sender).SQLError(Status, exceptConnection);
      finally
        if RecBuffer <> nil then FreeMem(RecBuffer);
      end;
    end;
end;

procedure TCustomSQLDataSetFixD7.ExecuteStatement;
var
  Status: SQLResult;
begin
  with TCustomSQLDataSetP(Self) do
  begin

  if SchemaInfo.FType = stNoSchema then
    begin
      if Assigned(FParams) and not FGetNextRecordSet then
      begin
        if CommandType = ctStoredProc then
          SetProcedureParams({Self.}FSQLConnection, FSQLCommand, Params, ProcParams)
        else
        if (FParams.Count > 0) then
          SetQueryParams({Self.}FSQLConnection, FSQLCommand, Params);
      end;
      if FGetNextRecordSet then
      begin
        Status := FSQLCommand.getNextCursor(FSQLCursor);
        if not (Status in [DBXERR_NONE, SQL_NULL_DATA]) then
          Check(Status);
        if Status <> DBXERR_NONE then
          Active := False
        else if Params.Count > 0 then
          GetOutputParams(FProcParams);
      end else
      begin
        Check(FSQLCommand.execute(FSQLCursor));
        if (CommandType = ctStoredProc) and (Params.Count > 0) then
          GetOutputParams(FProcParams);
      end;
    end
  else
    OpenSchema;
  FStatementOpen := True;
  FRecords := -1;

  end;
end;

procedure NormalizeBcdData(BcdData: PBcd; Precision, Scale: Word);
var
  ABcd: TBcd;
  Success: Boolean;
begin
  if Assigned(BcdData) then
  begin
    if Precision > MaxFMTBcdDigits then Precision := MaxFMTBcdDigits;
    if (BcdData.SignSpecialPlaces = 38) and ((Scale and 63)in [38,0]) then
    begin
      if (Scale and (1 shl 7)) <> 0 then
        Success := NormalizeBcd( BcdData^, ABcd, MaxFMTBcdDigits, Word((DefaultFMTBcdScale and 63) or (1 shl 7)))
      else
        Success := NormalizeBcd( BcdData^, ABcd, MaxFMTBcdDigits, DefaultFMTBcdScale);
    end else
      Success := NormalizeBcd( BcdData^, ABcd, Precision, Scale);
    if Success then
      BcdData^ := ABcd
    else
      DatabaseError(SBcdOverflow);
 end;
end;

function GetBlobSize(DataSet: TCustomSQLDataSet; FieldNo: Integer): LongWord;
var
  IsNull: LongBool;
  Status: SQLResult;
begin
  Result := 0;
  if not DataSet.EOF then
  begin
    if TCustomSQLDataSetP(DataSet).MaxBlobSize = 0 then exit;
    Status := TCustomSQLDataSetFixD7(DataSet).FSQLCursor.GetBlobSize(Word(FieldNo), Result, IsNull);
    if Status <> DBXERR_NONE then
      TCustomSQLDataSetP(DataSet).SQLError(Status, exceptCursor);
    if IsNull then
      Result := 0;
  end;
  TCustomSQLDataSetP(DataSet).CurrentBlobSize := Result;
end;

function TCustomSQLDataSetFixD7.GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean;
var
  FldType, Subtype: Word;
  Status: SQLResult;
  FBlank: LongBool;
  Field: TField;
  Precision, Scale: Word;
begin
  with TCustomSQLDataSetP(Self) do
  begin

  if (FSQLCursor = nil) then
    DatabaseError(SDataSetClosed, self);

  {When EOF is True we should not be calling into the driver to get Data}
  if EOF = True then
  begin
    Result := False;
    Exit;
  end;
  FBlank := True;
  Status := FSQLCursor.getColumnType(FieldNo, FldType, SubType);
  if (Status = 0) then
  begin
    case FldType of
      fldZSTRING:
        Status := FSQLCursor.GetString(FieldNo, Buffer, FBlank);
      {+}
      fldWIDESTRING:
        Status := FSQLCursor.GetString(FieldNo, Buffer, FBlank);
      {+.}
      fldINT16, fldUINT16:
        Status := FSQLCursor.GetShort(FieldNo, Buffer, FBlank);
      fldINT32, fldUINT32:
        Status := FSQLCursor.GetLong(FieldNo, Buffer, FBlank);
      fldFLOAT:
        Status := FSQLCursor.GetDouble(FieldNo, Buffer, FBlank);
      fldFMTBCD, fldBCD:
        begin
          Status := FSQLCursor.GetBcd(FieldNo, Buffer, FBlank);
          Field := FieldByNumber(FieldNo);
          if (not FBlank) and (Status = DBXERR_NONE) and (Field <> nil) then
          begin
            if Field.DataType = ftBcd then
            begin
              Precision := TBcdField(Field).Precision;
              Scale := TBcdField(Field).Size;
            end else
            begin
              Precision := TFMTBcdField(Field).Precision;
              Scale := TFMTBcdField(Field).Size;
            end;
            NormalizeBcdData(PBcd(Buffer), Precision, Scale);
          end;
        end;
      fldDATE:
        Status := FSQLCursor.GetDate(FieldNo, Buffer, FBlank);
      fldTIME:
        Status := FSQLCursor.GetTime(FieldNo, Buffer, FBlank);
      fldDATETIME:
        Status := FSQLCursor.GetTimeStamp(FieldNo, Buffer, FBlank);
      fldBOOL:
        Status := FSQLCursor.GetShort(FieldNo, Buffer, FBlank);
      fldBYTES, fldVARBYTES:
        Status := FSQLCursor.GetBytes(FieldNo, Buffer, FBlank);
      fldBLOB:
        begin
          GetBlobSize(TCustomSQLDataSet(Self), FieldNo);
          if CurrentBlobSize = 0 then
            FBlank := True
          else
            Status := FSQLCursor.GetBlob(FieldNo, Buffer, FBlank, CurrentBlobSize);
        end;
    end;
  end;
  if Status <> 0 then
    SQLError(Status, exceptCursor);
  Result := not FBlank;

  end;
end;

procedure TCustomSQLDataSetFixD7.AddFieldDesc(FieldDescs: TFieldDescList; DescNo: Integer;
    var FieldID: Integer; RequiredFields: TBits; FieldDefs: TFieldDefs);

  procedure {TCustomSQLDataSet.}LoadFieldDef(FieldID: Word; var FldDesc: FLDDesc);
  var
    ReadOnly: LongBool;
  begin
    with TCustomSQLDataSetP(Self) do
    begin
      FldDesc.iFldNum := FieldID;
      FSQLCursor.getColumnName(FieldId, FldDesc.szName);
      FSQLCursor.getColumnType(FieldId, FldDesc.iFldType, FldDesc.iSubtype);
      FSQLCursor.getColumnLength(FieldId, FldDesc.iLen);
      {
        ???: todo: when iFldType = fldWIDESTRING or iSubtype - fldstUNICODE
          and iLen > dsMaxStringSize then
        begin
          iNnits1 := iLen.Hi
          iUnits2 := iLen.Lo
        end
        else
         ...

        also: InternalInitFieldDefs;
      }
      FSQLCursor.getColumnPrecision(FieldId, FldDesc.iUnits1);
      FSQLCursor.getColumnScale(FieldId, FldDesc.iUnits2);
      FSQLCursor.isReadOnly(FieldID, ReadOnly);
      if ReadOnly then
        FldDesc.efldrRights := fldrREADONLY;
    end;
  end;

const
  ArrayIndex = '[0]';
var
  FType: TFieldType;
  FSize: LongWord;
  FRequired: Boolean;
  FPrecision, I: Integer;
  FieldName, FName: string;
  FieldDesc: FLDDesc;
  FldDef: TFieldDef;
begin
  with TCustomSQLDataSetP(Self) do
  begin

  FieldDesc := FieldDescs[DescNo];
  with FieldDesc do
  begin
    SetString(FieldName, szName, StrLen(szName));
    FName := FieldName;
    I := 0;
    while FieldDefs.IndexOf(FName) >= 0 do
    begin
      Inc(I);
      FName := Format('%s_%d', [FieldName, I]);
    end;
    if iFldType < MAXLOGFLDTYPES then
    {+}
    begin
      FType := DataTypeMap[iFldType];
      if (Integer(iFldType) = Ord(ftString)) and (iSubType and fldstUNICODE <> 0) then
      begin
        FType := ftWideString;
      end
      else
      if iFldType = fldWIDESTRING then
      begin
        FType := ftWideString;
        if iSubType and fldstMEMO <> 0 then
          FType := ftVariant; // Read only. It field not updatable. DbExpress driver not
           // received correctly information about FldType/FldSubType.
      end
      else
      if iFldType = fldBLOB then
      begin
        if iSubType and fldstUNICODE <> 0 then
        begin
          if (iLen > 0) and (iLen < dsMaxStringSize) then // ftWideString not supported long values for TClientDataSet ...
          begin
            iFldType := fldWIDESTRING;
            FType := ftWideString;
          end
          else
            iLen := 0;
          //FType := ftVariant;
          {
          iFldType := fldWIDESTRING;
          FType := ftWideString;
          iLen := 0;
          {}
        end;
      end;
    end
    {+.}
    else
      FType := ftUnknown;
    if iFldType in [fldFMTBCD, fldBCD] then
    begin
      iUnits2 := Abs(iUnits2);
      if iUnits1 < iUnits2 then   // iUnits1 indicates Oracle 'usable decimals'
        iUnits1 := iUnits2;
      // ftBCD supports only up to 18-4.  If Prec > 14 or Scale > 4, make FMTBcd
      if (iUnits1 > (MaxBcdPrecision-4)) or (iUnits2 > MaxBcdScale) {$IFDEF DELPHI_7UP}or FNumericMapping{$ENDIF} then
      begin
        FType := ftFMTBcd;
        iFldType := fldFMTBCD;
        if (iUnits1 = 38) and (iUnits2 in [0,38]) then
        begin
          iUnits1 := 32;
          iUnits2 := 8;
        end;
        if iUnits1 > MaxFMTBcdDigits then
          iUnits1 := MaxFMTBcdDigits;
      end;
    end;
    FSize := 0;
    FPrecision := 0;
    if RequiredFields.Size > FieldID then
      FRequired := RequiredFields[FieldID] else
      FRequired := False;
    case iFldType of
      fldZSTRING, fldBYTES, fldVARBYTES, fldRef:
        begin
          if iUnits1 = 0 then { Ignore MLSLABEL field type on Oracle }
            FType := ftUnknown else
            FSize := iUnits1;
        end;
      {+}
      fldWIDESTRING:
        begin
          if iLen = 0 then
            //FType := ftUnknown
          else
          begin
            iUnits1 := iLen div SizeOf(WideChar);
            FSize := iUnits1;
          end;
        end;
      {+.}
      fldINT16, fldUINT16:
        if iLen <> 2 then FType := ftUnknown;
      fldINT32:
        if iSubType = fldstAUTOINC then
        begin
          FType := ftAutoInc;
          FRequired := False;
        end;
      fldFLOAT:
        if iSubType = fldstMONEY then FType := ftCurrency;
      fldFMTBCD, fldBCD:
        begin
          FSize := Abs(iUnits2);
          FPrecision := iUnits1;
        end;
      fldADT, fldARRAY:
        begin
          FSize := iUnits2;
          FPrecision := iUnits1;
        end;
      fldBLOB:
        begin
          FSize := iUnits1;
          if (iSubType >= fldstMEMO) and (iSubType <= fldstBFILE) then
            FType := BlobTypeMap[iSubType];
        end;
    end;
    FldDef := FieldDefs.AddFieldDef;
    with FldDef do
    begin
      FieldNo := FieldID;
      Inc(FieldID);
      Name := FName;
      DataType := FType;
      Size := FSize;
      Precision := FPrecision;
      if FRequired then
        Attributes := [faRequired];
      if efldrRights = fldrREADONLY then
        Attributes := Attributes + [faReadonly];
      if iSubType = fldstFIXED then
        Attributes := Attributes + [faFixed];
      InternalCalcField := bCalcField;
      case FType of
        ftADT:
          begin
            if iSubType = fldstADTNestedTable then
              Attributes := Attributes + [faUnNamed];
            for I := 1 to iUnits1 do
            begin
              LoadFieldDef(Word(FieldNo + I), FieldDescs[1]);
              AddFieldDesc(FieldDescs, 1, FieldID, RequiredFields, ChildDefs);
            end;
          end;
        ftArray:
          begin
            for I := 1 to iUnits1 do
            begin
              LoadFieldDef(Word(FieldNo + I), FieldDescs[1]);
              StrCat(StrLCopy(FieldDescs[1].szName, FieldDesc.szName,
                     SizeOf(FieldDesc.szName) - Length(ArrayIndex)), ArrayIndex);
              AddFieldDesc(FieldDescs, 1, FieldID, RequiredFields, ChildDefs);
            end;
          end;
      end;
    end;
  end;
  end;
end;

procedure TDataSetFix.DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean);
  { DateTime Conversions }

  function NativeToDateTime(DataType: TFieldType; Data: TDateTimeRec): TDateTime;
  var
    TimeStamp: TTimeStamp;
  begin
    case DataType of
      ftDate:
        begin
          TimeStamp.Time := 0;
          TimeStamp.Date := Data.Date;
        end;
      ftTime:
        begin
          TimeStamp.Time := Data.Time;
          TimeStamp.Date := DateDelta;
        end;
    else
      try
        TimeStamp := MSecsToTimeStamp(Data.DateTime);
      except
        TimeStamp.Time := 0;
        TimeStamp.Date := 0;
      end;
    end;
    Result := TimeStampToDateTime(TimeStamp);
  end;

  function DateTimeToNative(DataType: TFieldType; Data: TDateTime): TDateTimeRec;
  var
    TimeStamp: TTimeStamp;
  begin
    TimeStamp := DateTimeToTimeStamp(Data);
    case DataType of
      ftDate: Result.Date := TimeStamp.Date;
      ftTime: Result.Time := TimeStamp.Time;
    else
      Result.DateTime := TimeStampToMSecs(TimeStamp);
    end;
  end;

  { Byte Field Conversions }

  procedure BufferToByteArray(Data: Pointer; DataSize: Integer; var VarArray: OleVariant);
  var
    PVarData: Pointer;
  begin
    VarArray := VarArrayCreate([0, DataSize - 1], varByte);
    PVarData := VarArrayLock(VarArray);
    try
      Move(Data^, PVarData^, DataSize);
    finally
      VarArrayUnlock(VarArray);
    end;
  end;

  procedure ByteArrayToBuffer(const Data: OleVariant; Buffer: Pointer; var DataSize: Word);
  var
    PVarData: Pointer;
  begin
    DataSize := VarArrayHighBound(Data, 1)+1;
    PVarData := VarArrayLock(Data);
    try
      Move(PVarData^, Buffer^, DataSize);
      if DataSize < Field.DataSize then
        FillChar((PChar(Buffer)+DataSize)^, Field.DataSize - DataSize, 0);
    finally
      VarArrayUnlock(Data);
    end;
  end;

var
  DataSize: Word;
begin
  case Field.DataType of
    ftDate, ftTime, ftDateTime:
      if ToNative then
        TDateTimeRec(Dest^) := DateTimeToNative(Field.DataType, TDateTime(Source^)) else
        TDateTime(Dest^) := NativeToDateTime(Field.DataType, TDateTimeRec(Source^));
    ftTimeStamp:
        TSQLTimeStamp(Dest^) := TSQLTimeStamp(Source^);
    ftBCD:
    begin
      if ToNative then
        CurrToBCD(Currency(Source^), TBcd(Dest^), 32, Field.Size) else
        if not BCDToCurr(TBcd(Source^), Currency(Dest^)) then
          raise EOverFlow.CreateFmt(SFieldOutOfRange, [Field.DisplayName]);
      end;
    ftFMTBCD:
      TBcd(Dest^) := TBcd(Source^);
    ftBytes:
      if ToNative then
        ByteArrayToBuffer(POleVariant(Source)^, Dest, DataSize) else
        BufferToByteArray(Source, Field.DataSize, POleVariant(Dest)^);
    ftVarBytes:
      if ToNative then
        ByteArrayToBuffer(POleVariant(Source)^, PChar(Dest)+2, PWord(Dest)^) else
        BufferToByteArray(PChar(Source)+2, PWord(Source)^, POleVariant(Dest)^);
    {+} // from: TCustomClientDataSet.DataConvert
    ftWideString:
      if ToNative then
      begin
        // not tested
        Word(Dest^) := Length(PWideString(Source)^) * SizeOf(WideChar);
        Move(PWideChar(Source^)^, (PWideChar(Dest)+1)^, Word(Dest^));
      end
      else
        SetString(WideString(Dest^), PWideChar(PChar(Source) + SizeOf(WideChar)),
          Word(Source^) div SizeOf(WideChar));
    {+.}
  end;
end;

function TDataSetFix.GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean;
var
  Temp: String;
  pBuff: PChar;
  NativeBuf: array[0..dsMaxStringSize] of Char;
  {+}
  iSize: Integer;
  vBuffer: TBlobByteData;
  function ReadDataFromBlobStream: Boolean;
  var
    DataStream: TStream;
    bf: TBlobField;
    bfh: TFieldH absolute bf;
  begin
    bf := TBlobField.Create(nil);
    try
      bfh.fDataSet := Self;
      bfh.fFieldName := Field.FieldName;
      bfh.fFieldNo := Field.FieldNo;
      bfh.fFields := TFieldH(Field).fFields;
      bfh.fProviderFlags := Field.ProviderFlags;
      bfh.fValidChars := Field.ValidChars;
      //bf.fDataType := Field.fDataType;
      DataStream := CreateBlobStream(bf, bmRead);
      Result := (DataStream <> nil);
      if Result then
      try
        iSize := DataStream.Size;
        SetLength(Temp, iSize + SizeOf(Word));
        pBuff := pChar(Temp) + SizeOf(Word);
        Word(Pointer(PChar(Temp))^) := iSize;
        if iSize > 0 then
          DataStream.ReadBuffer(pBuff^, iSize);
        pBuff := pChar(Temp);
      finally
        DataStream.Free;
      end;
    finally
      bfh.fDataSet := nil;
      bfh.Free;
    end;
  end;
  function ReadDataFromBlobField: Boolean;
  begin
    iSize := GetBlobFieldData(Field.FieldNo, vBuffer);
    if iSize > 0 then
      pBuff := @vBuffer[0];
    Result := iSize > 0;
  end;
  {+.}
begin
  if NativeFormat then
    Result := GetFieldData(Field, Buffer) else
  {+}
  begin
    iSize := Field.DataSize;
    if (iSize = 0) then
    begin
      if Field.IsBlob then
        Result := ReadDataFromBlobField()
      else if (not (TObject(Self) is TCustomClientDataSet)) then // AV for read blob data for
        Result := ReadDataFromBlobStream // TCustomClientDataSet, but ok for TSQLDataSet.
      else
      begin
        // ???: todo: ...
        Result := False;
        // Result := GetFieldData(Field, @W);
        // Result := ReadDataFromBlobStream();
      end;
      if Result then
        DataConvert(Field, pBuff, Buffer, False);
    end
    else // if iSize > 0 then
    begin
      if (iSize > dsMaxStringSize) then
      begin
        SetLength(Temp, Field.DataSize);
        pBuff := pChar(Temp);
        Result := GetFieldData(Field, pBuff);
        if Field.DataType = ftString then
          SetLength(Temp, strlen(PChar(pBuff)));
        if Result then
          DataConvert(Field, pBuff, Buffer, False);
      end
      else
      begin
        Result := GetFieldData(Field, @NativeBuf);
        if Result then
          DataConvert(Field, @NativeBuf, Buffer, False);
      end;
    end;
  {+.}
  end;
end;

function TParamFix.GetDataSize: Integer;
begin
  Result := 0;
  case DataType of
    ftUnknown: DatabaseErrorFmt(SUnknownFieldType, [Name], DataSet);
    ftString, ftFixedChar, ftMemo, ftADT: Result := Length(VarToStr(TParamH(Self).FData)) + 1;
    {+}
    ftWideString:
      //Result := (Length(VarToStr(FData)) + 1) * SizeOf(WideChar);
      Result := (Length(VarToStr(TParamH(Self).FData)) + 1) * SizeOf(WideChar);
    {+.}
    ftBoolean: Result := SizeOf(WordBool);
    ftBCD, ftFMTBcd: Result := SizeOf(TBcd);
    ftTimeStamp: Result := SizeOf( TSqlTimeStamp );
    ftDateTime,
    ftCurrency,
    ftFloat: Result := SizeOf(Double);
    ftTime,
    ftDate,
    ftAutoInc,
    ftInteger: Result := SizeOf(Integer);
    ftSmallint: Result := SizeOf(SmallInt);
    ftWord: Result := SizeOf(Word);
    ftBytes, ftVarBytes:
      if VarIsArray(TParamH(Self).FData) then
        Result := VarArrayHighBound(TParamH(Self).FData, 1) + 1 else
        Result := 0;
    ftBlob, ftGraphic..ftTypedBinary,ftOraClob,ftOraBlob: Result := Length(VarToStr(TParamH(Self).FData));
    ftArray, ftDataSet,
    ftReference, ftCursor: Result := 0;
  else
    DatabaseErrorFmt(SBadFieldType, [Name], DataSet);
  end;
end;

procedure TParamFix.GetData(Buffer: Pointer);
var
  P: Pointer;
  {+}
  procedure CopyWideString;
  var
    W: WideString;
  begin
    W := TParamH(Self).FData;
    //Word(Buffer^) := Length(W);
    if Length(W) > 0 then
    //  Move(PWideChar(W)^, PWideChar(Buffer)[1], Length(W) * SizeOf(WideChar));
      Move(PWideChar(W)^, PWideChar(Buffer)^, (Length(W)+1) * SizeOf(WideChar))
    else
      Word(Buffer^) := 0;
  end;
  {+.}
begin
  case DataType of
    ftUnknown: DatabaseErrorFmt(SUnknownFieldType, [Name], DataSet);
    ftString, ftFixedChar, ftMemo, ftAdt:
      StrMove(Buffer, PChar(GetAsString), Length(GetAsString) + 1);
    {+}
    ftWideString:
      if VarIsEmpty(TParamH(Self).FData) or VarIsNull(TParamH(Self).FData) then
        Word(Buffer^) := 0
      else
        CopyWideString();
    {+.}
    ftSmallint: SmallInt(Buffer^) := GetAsInteger;
    ftWord: Word(Buffer^) := GetAsInteger;
    ftAutoInc,
    ftInteger: Integer(Buffer^) := GetAsInteger;
    ftTime: Integer(Buffer^) := DateTimeToTimeStamp(AsDateTime).Time;
    ftDate: Integer(Buffer^) := DateTimeToTimeStamp(AsDateTime).Date;
    ftDateTime:  Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(AsDateTime));
    ftBCD: CurrToBCD(AsBCD, TBcd(Buffer^));
    ftFMTBCD: TBcd(Buffer^) := AsFMTBcd;
    ftCurrency,
    ftFloat: Double(Buffer^) := GetAsFloat;
    ftTimeStamp:  TSQLTimeStamp(Buffer^) := AsSQLTimeStamp;
    ftBoolean: Word(Buffer^) := Ord(GetAsBoolean);
    ftBytes, ftVarBytes:
    begin
      if VarIsArray(TParamH(Self).FData) then
      begin
        P := VarArrayLock(TParamH(Self).FData);
        try
          Move(P^, Buffer^, VarArrayHighBound(TParamH(Self).FData, 1) + 1);
        finally
          VarArrayUnlock(TParamH(Self).FData);
        end;
      end;
    end;
    ftBlob, ftGraphic..ftTypedBinary,ftOraBlob,ftOraClob:
      Move(PChar(GetAsString)^, Buffer^, Length(GetAsString));
    ftArray, ftDataSet,
    ftReference, ftCursor: {Nothing};
  else
    DatabaseErrorFmt(SBadFieldType, [Name], DataSet);
  end;
end;

function TWideStringFieldFix.GetDataSize: Integer;
begin
  {+}
  // Result := SizeOf(WideString);
  if Size > 0 then
    Result := (Size + 1) * SizeOf(WideChar)
  else
    Result := 0;
  {+.}
end;

const
  PacketTypeMap: array [TFieldType] of Integer =
    (dsfldUNKNOWN, dsfldZSTRING, dsfldINT, dsfldINT, dsfldINT, dsfldBOOL,
     dsfldFLOATIEEE, dsfldFLOATIEEE, dsfldBCD, dsfldDATE, dsfldTIME,
     dsfldTIMESTAMP, dsfldBYTES, dsfldBYTES, dsfldINT, dsfldBYTES, dsfldBYTES,
     dsfldBYTES, dsfldBYTES, dsfldBYTES, dsfldBYTES, dsfldBYTES, dsfldUNKNOWN,
     dsfldZSTRING, dsfldUNICODE, dsfldINT, dsfldADT, dsfldARRAY, dsfldEMBEDDEDTBL,
     dsfldEMBEDDEDTBL, dsfldBYTES, dsfldBYTES, dsfldUNKNOWN, dsfldUNKNOWN,
     dsfldUNKNOWN, dsfldZSTRING, dsfldDATETIME, dsFLDFMTBCD);

  ExtraFieldProps: array [0..10] of string = ('Alignment', 'DisplayLabel',
    'DisplayWidth', 'Visible', 'EditMask', 'DisplayFormat', 'EditFormat',
    'MinValue', 'MaxValue', 'currency', 'DisplayValues');

type
  TPropWriter = class(TWriter);

procedure TDataPacketWriterFix.AddColumn(const Info: TPutFieldInfo);

  procedure {TDataPacketWriter.}AddExtraFieldProps(Field: TField);

    procedure WriteProp(Instance: TPersistent; const PropName: string;
      Writer: TPropWriter);
    var
      PropInfo: PPropInfo;
    begin
      PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, PropName);
      if (PropInfo <> nil) and IsStoredProp(Instance, PropInfo) then
        Writer.WriteProperty(Instance, PropInfo);
    end;

  var
    Writer: TPropWriter;
    Stream: TMemoryStream;
    i: Integer;
    Attr: Cardinal;
  begin
    Stream := TMemoryStream.Create;
    try
      Writer := TPropWriter.Create(Stream, 1024);
      try
        Writer.WriteListBegin;
        for i := 0 to High(ExtraFieldProps) do
          WriteProp(Field, ExtraFieldProps[i], Writer);
        Writer.WriteListEnd;
        Writer.FlushBuffer;
        if Stream.Size > 2 then
        begin
          Attr := (dsfldBYTES shl dsSizeBitsLen) or dsArrayFldType or SizeOf(Byte) or dsIncInDelta;
          PInteger(TCustomPacketWriterH(Self).FBuffer)^ := Stream.Size;
          Move(Stream.Memory^, TCustomPacketWriterH(Self).FBuffer[SizeOf(Integer)], Stream.Size);
          Check(TCustomPacketWriterH(Self).FIDSWriter.AddAttribute(fldAttrArea, szFIELDPROPS, Attr,
            Stream.Size + SizeOf(Integer), TCustomPacketWriterH(Self).FBuffer));
        end;
      finally
        Writer.Free;
      end;
    finally
      Stream.Free;
    end;
  end;

  procedure AddFieldDesc(const FldName: string; FldType, Attributes: Integer);
  var
    FldDesc: TDSDataPacketFldDesc;
  begin
    if Length(FldName) >= SizeOf(FldDesc.szFieldName) then
      raise EDSWriter.CreateFmt(SFieldNameTooLong,[SizeOf(FldDesc.szFieldName) - 1]);
    FillChar(FldDesc, SizeOf(FldDesc), 0);
    StrLCopy(FldDesc.szFieldName, PChar(FldName), SizeOf(FldDesc.szFieldName) - 1);
    FldDesc.iFieldType := FldType;
    FldDesc.iAttributes := Attributes;
    Check(TCustomPacketWriterH(Self).FIDSWriter.AddColumnDesc(FldDesc));
  end;

  function ComputeInfoCount(Info: TInfoArray): Integer;
  var
    i: Integer;
  begin
    Result := Length(Info);
    for i := 0 to High(Info) do
      if Info[i].FieldInfos <> nil then
        Inc(Result, ComputeInfoCount(Info[i].FieldInfos));
  end;

  procedure AddMinMax(AField: TField);
  begin
    case AField.DataType of
      ftInteger, ftSmallInt:
        if (TIntegerField(AField).MinValue <> 0) or
           (TIntegerField(AField).MaxValue <> 0)  then
           begin
             AddAttribute(fldAttrArea, szMINVALUE,
                           TIntegerField(AField).MinValue, False);
             AddAttribute(fldAttrArea, szMAXVALUE,
                          TIntegerField(AField).MaxValue, False);
           end;
      ftCurrency, ftFloat:
        if (TFloatField(AField).MinValue <> 0 ) or
           (TFloatField(AField).MaxValue <> 0 ) then
           begin
             AddAttribute(fldAttrArea, szMINVALUE,
                           TFloatField(AField).MinValue, False);
             AddAttribute(fldAttrArea, szMAXVALUE,
                          TFloatField(AField).MaxValue, False);
           end;
      ftBCD:
        if (TBCDField(AField).MinValue <> 0 ) or
           (TIntegerField(AField).MaxValue <> 0 ) then
           begin
             AddAttribute(fldAttrArea, szMINVALUE,
                           TBCDField(AField).MinValue, False);
             AddAttribute(fldAttrArea, szMAXVALUE,
                          TBCDField(AField).MaxValue, False);
           end;
      ftFMTBcd:
        if (TFMTBcdField(AField).MaxValue <> '') or
           (TFMTBcdField(AField).MinValue <> '') then
           begin
             AddAttribute(fldAttrArea, szMINVALUE,
                    VarFMTBcdCreate(TFMTBCDField(AField).MinValue, TFMTBCDField(AField).Precision, TFMTBCDField(AField).Size), False);
             AddAttribute(fldAttrArea, szMAXVALUE,
                    VarFMTBcdCreate(TFMTBCDField(AField).MaxValue, TFMTBCDField(AField).Precision, TFMTBCDField(AField).Size), False);
           end;
    end;
  end;

var
  FldType, Prec, Attr, i, Width: Integer;
  TempStr: string;
begin
  if Info.IsDetail and (Info.Field = nil) then
  begin
    FldType := (dsfldEMBEDDEDTBL shl dsSizeBitsLen) or
      ComputeInfoCount(Info.FieldInfos) or dsPseudoFldType;
    AddFieldDesc(Info.DataSet.Name, FldType, 0);
    WriteMetaData(Info.DataSet, TInfoArray(Info.FieldInfos));
  end else
  begin
    Width := 0;
    Attr := 0;
    if Info.Field.ReadOnly or (Info.Field.FieldKind <> fkData) then Attr := Attr or fldAttrREADONLY;
    if Info.Field.Required and (Info.Field.DataType <> ftAutoInc) then Attr := Attr or fldAttrREQUIRED;
    if (pfHidden in Info.Field.ProviderFlags) then Attr := Attr or fldAttrHIDDEN or fldAttrREADONLY;
    FldType := PacketTypeMap[Info.Field.DataType]; //dsfldUNICODE
    case Info.Field.DataType of
      ftTimeStamp:
        FldType := (FldType shl dsSizeBitsLen) or sizeof(TSQLTimeStamp);
      ftString, ftFixedChar, ftVarBytes, ftGUID, ftWideString:
      begin
        FldType := FldType shl dsSizeBitsLen or dsVaryingFldType;
        if {+}(Info.Size > 0 ) and {+.} (Info.Size < 255) then
          FldType := FldType or SizeOf(Byte) else
          FldType := FldType or SizeOf(Word);
        Width := Info.Size;
      end;
      ftBCD:
      begin
        if TBCDField(Info.Field).Precision = 0 then
          Width := 32 else
          Width := TBCDField(Info.Field).Precision;
        Prec := Width shr 1;
        Inc(Prec, Prec and 1);  { Make an even number }
        FldType := (FldType shl dsSizeBitsLen) or (Prec + 2);
      end;
      ftFMTBcd:
      begin
        if TFMTBCDField(Info.Field).Precision = 0 then
          Width := 32 else
          Width := TFMTBCDField(Info.Field).Precision;
        Prec := Width shr 1;
        Inc(Prec, Prec and 1);  { Make an even number }
        FldType := (FldType shl dsSizeBitsLen) or (Prec + 2);
      end;
      ftArray:
        FldType := (FldType shl dsSizeBitsLen) or dsPseudoFldType or
          dsCompArrayFldType or TObjectField(Info.Field).Size;
      ftADT:
        FldType := (FldType shl dsSizeBitsLen) or dsPseudoFldType or
          TObjectField(Info.Field).FieldCount;
      ftDataSet, ftReference:
        FldType := (FldType shl dsSizeBitsLen) or dsPseudoFldType or
          dsEmbeddedFldType or ComputeInfoCount(TInfoArray(Info.FieldInfos));
    else
      if Info.Field.IsBlob then
      begin
        FldType := (FldType shl dsSizeBitsLen) or dsVaryingFldType or SizeOf(Integer);
        Width := Info.Field.Size;
      end else
        FldType := (FldType shl dsSizeBitsLen) or Info.Size;
    end;
    AddFieldDesc(Info.Field.FieldName, FldType, Attr);
    if (Info.Field.FieldKind <> fkData) then
      AddAttribute(fldAttrArea, szSERVERCALC, True, True);
    if Info.Field.ProviderFlags <> [pfInWhere, pfInUpdate] then
      AddAttribute(fldAttrArea, szPROVFLAGS, Byte(Info.Field.ProviderFlags), True);
    if Info.Field.Origin <> '' then
      AddAttribute(fldAttrArea, szORIGIN, Info.Field.Origin, True);
    if Width > 0 then
      AddAttribute(fldAttrArea, szWIDTH, Width, False);
    if Info.Field is TBCDField then
    begin
      if TBCDField(Info.Field).Size <> 0 then
        AddAttribute(fldAttrArea, szDECIMALS, TBCDField(Info.Field).Size, False);
    end
    else if Info.Field is TFMTBCDField then
    begin
      if TFMTBCDField(Info.Field).Size <> 0 then
        AddAttribute(fldAttrArea, szDECIMALS, TFMTBCDField(Info.Field).Size, False);
    end;
    AddMinMax(Info.Field);
    case Info.Field.DataType of
      ftCurrency: TempStr := szstMONEY;
      ftAutoInc: TempStr := szstAUTOINC;
      ftVarBytes, ftBlob: TempStr := szstBINARY;
      ftMemo: TempStr := szstMEMO;
      ftFmtMemo: TempStr := szstFMTMEMO;
      ftParadoxOle: TempStr := szstOLEOBJ;
      ftGraphic: TempStr := szstGRAPHIC;
      ftDBaseOle: TempStr := szstDBSOLEOBJ;
      ftTypedBinary: TempStr := szstTYPEDBINARY;
      ftADT:
        if (Info.Field.ParentField <> nil) and
           (Info.Field.ParentField.DataType in [ftDataSet, ftReference]) then
          TempStr := szstADTNESTEDTABLE;
      ftReference: TempStr := szstREFNESTEDTABLE;
      ftString:
        if TStringField(Info.Field).FixedChar then
          TempStr := szstFIXEDCHAR else
          TempStr := '';
      {+}
      ftWideString:
        if TWideStringField(Info.Field).FixedChar then
          TempStr := szstFIXEDCHAR else
          TempStr := '';
      {+.}
      ftGUID: TempStr := szstGUID;
      ftOraClob: TempStr := szstHMEMO;
      ftOraBlob: TempStr := szstHBINARY;
    else
        TempStr := '';
    end;
    if TempStr <> '' then
      AddAttribute(fldAttrArea, szSUBTYPE, TempStr, False);
    if Info.Field is TObjectField then
      AddAttribute(fldAttrArea, szTYPENAME, TObjectField(Info.Field).ObjectType, False);
    if poIncFieldProps in Options then
      AddExtraFieldProps(Info.Field);
    case Info.Field.DataType of
      ftADT, ftArray: { Array will only have 1 child field }
        for i := 0 to High(TInfoArray(Info.FieldInfos)) do
          AddColumn(TInfoArray(Info.FieldInfos)[i]);
      ftDataSet, ftReference:
        with TDataSetField(Info.Field) do
          WriteMetaData(NestedDataSet, TInfoArray(Info.FieldInfos),
            Info.Field.DataType = ftReference);
    end;
  end;
end;

function NextPiece(Start: string; InLiteral: Boolean; QuoteChar: Char; EndParam: Boolean = False): Integer;
var
  P: PChar;
  Ctr: Integer;
  SearchChars: set of char;
begin
  SearchChars := [' ', ')', ',', '=', ':', '>', '<', #13, #10];
  P := (PChar(Start))+1;
  Ctr := 1;
  Result := 0;
  while (Result = 0) and (P^ <> #0) do
  begin
    if (P^ = '''') or (P^ = QuoteChar) then
      InLiteral := not InLiteral
    else
    if not InLiteral and (P^ in SearchChars) then
    begin
      if EndParam then
      begin
        if not (P^ in ['=', ':', '<', '>']) then
        begin
          Result := Ctr;
          Inc(Result);
        end
      end else
      begin
        if (P^ = ':') then
        begin
          if P[-1] in [' ', ')', ',', '=', '('] then
            Result := Ctr;
        end
        else if (P[1] = ':') then
        begin
          Result := Ctr;
          Inc(Result);
        end;
      end;
    end;
    Inc(P);
    Inc(Ctr);
  end;
end;

// SqlObjects does not support named params: convert to ?
// if not yet converted
function FixParams(SQL: string; Count: Integer; QuoteChar: string): string;
var
  Param, Start: string;
  Pos, EndPos: Integer;
  InLiteral: Boolean;
  Q: Char;
begin
  Q := PChar(QuoteChar)[0];
  if Q in [#0, ' '] then Q := '''';
  InLiteral := False;
  Start := SQL;
  Pos := NextPiece(Start, InLiteral, Q);
  while Pos > 0 do
  begin
    Start := copy(Start, Pos + 1, Length(Start) - Pos);
    EndPos := NextPiece(Start, InLiteral, Q, True);
    if EndPos = 0 then
      Param := copy(Start, 1, Length(Start))
    else
      Param := copy(Start, 1, EndPos-1);
    SQL := stringReplace(SQL, Param, ' ? ', []);
    Pos := NextPiece(Start, InLiteral, Q);
  end;
  Result := SQL;
end;

procedure TCustomSQLDataSetFixD7.CheckStatement(ForSchema: Boolean = False);
  function {TSQLConnection.}GetConnectionForStatement: TSQLConnection;
  var
    Con: TSqlConnectionP;
  begin
    Con := TSQLConnectionP(TCustomSQLDataSetH(Self).FSQLConnection);
    with TSQLConnectionH(Con) do
    begin

    if (FMaxStmtsPerConn > 0) and (FActiveStatements >= FMaxStmtsPerConn) then
    begin
      if not Con.AutoClone then
        DataBaseErrorFmt(SMultiConnNotSupported, [Con.DriverName]);
      Result := Con.CloneConnection;
    end else
      Result := Con;

    end;
  end;
var
  Connection: TSqlConnection;
  RowsetSize: Integer;
begin
  with TCustomSQLDataSetP(Self) do
  begin

  FLastError := '';
  RowsetSize := defaultRowsetSize;
  if not Assigned(FSQLConnection) then
    DatabaseError(SMissingSQLConnection);
  Connection := {FSQLConnection.}GetConnectionForStatement;
  if TSQLConnectionH(Connection).FIsCloned then
    FClonedConnection := Connection;
  if Connection.LoadParamsOnConnect then
    Connection.LoadParamsFromIniFile;
  if Assigned(FSQLCommand) then
    FreeStatement;
  if not Assigned(TSQLConnectionP(Connection).Connection) then
    DatabaseError(SdatabaseOpen, Self);
  Inc(TSQLConnectionH(Connection).FActiveStatements);
  if not ForSchema then
  begin
    if Length(FCommandText) = 0 then
      DatabaseError(SEmptySQLStatement, Self);
    {+}
    TSQLConnectionP(Connection).Check(TSQLConnectionP(Connection).Connection.getSQLCommand(FSQLCommand));
    {+.}

    if FSQLConnection.Params.Values[ROWSETSIZE_KEY] <> '' then
    try
      RowsetSize := StrToInt(trim(FSQLConnection.Params.Values[ROWSETSIZE_KEY]));
    except
      RowsetSize := defaultRowsetSize;
    end;

    FSQLCommand.setOption(eCommRowsetSize, RowsetSize);

    FStatementOpen := True;
    if FTransactionLevel > 0 then
      FSQLCommand.SetOption(eCommTransactionID, Integer(FTransactionLevel));
    if FNativeCommand = '' then
    begin
      if FParams.Count > 0 then
        FNativeCommand := FixParams(CommandText, FParams.Count, TSQLConnectionP(Connection).{Get}QuoteChar)
      else
        FNativeCommand := CommandText;
    end;
  end;

  end;
end;

//{$ENDIF IFDEF DELPHI_7UP}

{ TParams }

// QC: 6319.
// fixes: allow only param name classic name. Also allow problem informix fullname parsing
//  and type conversion parsing.
//
// examples:
//  1) select * from sysutils:sysusers
//  2) select username::VARCHAR(125), usertype from sysutils:sysusers
//
type

  TParamsFix = class(TParams)
  public
    function ParseSQL(SQL: String; DoCreate: Boolean): String;
  end;

function TParamsFix.ParseSQL(SQL: String; DoCreate: Boolean): String;
const
  Literals = ['''', '"', '`'];
var
  Value, CurPos, StartPos: PChar;
  CurChar: Char;
  Literal: Boolean;
  EmbeddedLiteral: Boolean;
  Name: string;

  {+}
  function IsNameDelimiter(ch: Char): Boolean;
  begin
    Result := ch in [' ', ',', ';', ')', #13, #10
        ,#1..#9, '-', '+', '*', '/', '\', '!', '~', '''', '"', '&',
        '%', '`', '|', ']', '[', '>', '<', '=', '?',  '(', '^'
    ];
  end;
  {+.}

  function NameDelimiter: Boolean;
  begin
    {+}
    //OLD:
    //Result := CurChar in [' ', ',', ';', ')', #13, #10];
    //NEW:
    Result := IsNameDelimiter(CurChar);
    {+.}
  end;

  function IsLiteral: Boolean;
  begin
    Result := CurChar in Literals;
  end;

  function StripLiterals(Buffer: PChar): string;
  var
    Len: Word;
    TempBuf: PChar;

    procedure StripChar;
    begin
      if TempBuf^ in Literals then
        StrMove(TempBuf, TempBuf + 1, Len - 1);
      if TempBuf[StrLen(TempBuf) - 1] in Literals then
        TempBuf[StrLen(TempBuf) - 1] := #0;
    end;

  begin
    Len := StrLen(Buffer) + 1;
    TempBuf := AllocMem(Len);
    try
      StrCopy(TempBuf, Buffer);
      StripChar;
      Result := StrPas(TempBuf);
    finally
      FreeMem(TempBuf, Len);
    end;
  end;

  {+} // ADDED NEW:
  function isDigit:Boolean;
   var LCurPos:PChar;
  begin
      Result := (CurPos-1)^ in ['0'..'9'];
      if Result and (CurPos-1>Value) then begin
          LCurPos := CurPos-1;
          while LCurPos>=Value do begin
              dec(LCurPos);
              if not (LCurPos^ in [#9, #10, #13, ' ', '0'..'9'])
              then begin
                  Result := False;
                  exit;
              end;
          end;
      end;
  end;
  {+.}

begin
  Result := SQL;
  Value := PChar(Result);
  if DoCreate then Clear;
  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  {+}
  StartPos := CurPos;
  {+.}
  repeat
    while (CurPos^ in LeadBytes) do Inc(CurPos, 2);
    CurChar := CurPos^;
    {+}
    //OLD:
    //if (CurChar = ':') and not Literal and ((CurPos + 1)^ <> ':') then
    //NEW:
    if (CurChar = ':') and not Literal and ( not ((CurPos + 1)^ in [':',#0,#9,#10,#13,#32]) )
       and( Value <> CurPos )
       and( isDigit or (not ( UpCase((CurPos - 1)^) in ['A'..'Z','_','0'..'9']  ))  ) then
    {+.}
    begin
      StartPos := CurPos;
      while (CurChar <> #0) and (Literal or not NameDelimiter) do
      begin
        Inc(CurPos);
        while (CurPos^ in LeadBytes) do Inc(CurPos, 2);
        CurChar := CurPos^;
        if IsLiteral then
        begin
          Literal := Literal xor True;
          if CurPos = StartPos + 1 then EmbeddedLiteral := True;
        end;
      end;
      CurPos^ := #0;
      if EmbeddedLiteral then
      begin
        Name := StripLiterals(StartPos + 1);
        EmbeddedLiteral := False;
      end
      else Name := StrPas(StartPos + 1);
      if DoCreate then
        TParam(Add).Name := Name;
      CurPos^ := CurChar;
      StartPos^ := '?';
      Inc(StartPos);
      StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
      CurPos := StartPos;
    end
    else
    if (CurChar = ':') and (not Literal) and ((CurPos + 1)^ = ':') then
    {+}
    begin
      if (CurPos>StartPos) and (not IsNameDelimiter((CurPos-1)^)) then
        inc(CurPos)
      else
        StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    end
    {+.}
    else
    if IsLiteral then
      Literal := Literal xor True;
    Inc(CurPos);
  until CurChar = #0;
end;

{  DBCommon.NextSQLToken }

// added detected SQL line comments by two hyphen ("--"):
// it comment is supported in mssql, oracle, informix, ...
function NextSQLTokenFix(var p: PChar; out Token: string; CurSection: TSQLToken): TSQLToken;
var
  DotStart: Boolean;

  function NextTokenIs(Value: string; var Str: string): Boolean;
  var
    Tmp: PChar;
    S: string;
  begin
    Tmp := p;
    NextSQLTokenFix(Tmp, S, CurSection);
    Result := AnsiCompareText(Value, S) = 0;
    if Result then
    begin
      Str := Str + ' ' + S;
      p := Tmp;
    end;
  end;

  function GetSQLToken(var Str: string): TSQLToken;
  var
    l: PChar;
    s: string;
  begin
    if Length(Str) = 0 then
      Result := stEnd else
    if (Str = '*') and (CurSection = stSelect) then
      Result := stAllFields else
    if DotStart then
      Result := stFieldName else
    if (AnsiCompareText('DISTINCT', Str) = 0) and (CurSection = stSelect) then
      Result := stDistinct else
    if (AnsiCompareText('ASC', Str) = 0) or (AnsiCompareText('ASCENDING', Str) = 0)then
      Result := stAscending else
    if (AnsiCompareText('DESC', Str) = 0) or (AnsiCompareText('DESCENDING', Str) = 0)then
      Result := stDescending else
    if AnsiCompareText('SELECT', Str) = 0 then
      Result := stSelect else
    if AnsiCompareText('AND', Str) = 0 then
      Result := stAnd else
    if AnsiCompareText('OR', Str) = 0 then
      Result := stOr else
    if AnsiCompareText('LIKE', Str) = 0 then
      Result := stLike else
    if (AnsiCompareText('IS', Str) = 0) then
    begin
      if NextTokenIs('NULL', Str) then
        Result := stIsNull else
      begin
        l := p;
        s := Str;
        if NextTokenIs('NOT', Str) and NextTokenIs('NULL', Str) then
          Result := stIsNotNull else
        begin
          p := l;
          Str := s;
          Result := stValue;
        end;
      end;
    end else
    if AnsiCompareText('FROM', Str) = 0 then
      Result := stFrom else
    if AnsiCompareText('WHERE', Str) = 0 then
      Result := stWhere else
    if (AnsiCompareText('GROUP', Str) = 0) and NextTokenIs('BY', Str) then
      Result := stGroupBy else
    if AnsiCompareText('HAVING', Str) = 0 then
      Result := stHaving else
    if AnsiCompareText('UNION', Str) = 0 then
      Result := stUnion else
    if AnsiCompareText('PLAN', Str) = 0 then
      Result := stPlan else
    if (AnsiCompareText('FOR', Str) = 0) and NextTokenIs('UPDATE', Str) then
      Result := stForUpdate else
    if (AnsiCompareText('ORDER', Str) = 0) and NextTokenIs('BY', Str)  then
      Result := stOrderBy else
    if AnsiCompareText('NULL', Str) = 0 then
      Result := stValue else
    if CurSection = stFrom then
      Result := stTableName else
      Result := stFieldName;
  end;

var
  TokenStart: PChar;

  procedure StartToken;
  begin
    if not Assigned(TokenStart) then
      TokenStart := p;
  end;

var
  Literal: Char;
  Mark: PChar;
begin
  TokenStart := nil;
  DotStart := False;
  while True do
  begin
    case p^ of
      '"','''','`':
      begin
        StartToken;
        Literal := p^;
        Mark := p;
        repeat Inc(p) until (p^ in [Literal,#0]);
        if p^ = #0 then
        begin
          p := Mark;
          Inc(p);
        end else
        begin
          Inc(p);
          SetString(Token, TokenStart, p - TokenStart);
          Mark := PChar(Token);
          Token := AnsiExtractQuotedStr(Mark, Literal);
          if DotStart then
            Result := stFieldName else
          if p^ = '.' then
            Result := stTableName else
            Result := stValue;
          Exit;
        end;
      end;
      '/':
      begin
        StartToken;
        Inc(p);
        if p^ in ['/','*'] then
        begin
          if p^ = '*' then
          begin
            repeat Inc(p) until (p = #0) or ((p^ = '*') and (p[1] = '/'));
          end else
            while not (p^ in [#0, #10, #13]) do Inc(p);
          SetString(Token, TokenStart, p - TokenStart);
          Result := stComment;
          Exit;
        end;
      end;
      {+} // ADDED NEW:
      '-':
      begin
        StartToken;
        Inc(p);
        if (p^ ='-') and (Length(Token) = 0) then
        begin
          while not (p^ in [#0, #10, #13]) do Inc(p);
          SetString(Token, TokenStart, p - TokenStart);
          Result := stComment;
          Exit;
        end;
      end;
      {+.}
      ' ', #10, #13, ',', '(':
      begin
        if Assigned(TokenStart) then
        begin
          SetString(Token, TokenStart, p - TokenStart);
          Result := GetSQLToken(Token);
          Exit;
        end else
          while (p^ in [' ', #10, #13, ',', '(']) do Inc(p);
      end;
      '.':
      begin
        if Assigned(TokenStart) then
        begin
          SetString(Token, TokenStart, p - TokenStart);
          Result := stTableName;
          Exit;
        end else
        begin
          DotStart := True;
          Inc(p);
        end;
      end;
      '=','<','>':
      begin
        if not Assigned(TokenStart) then
        begin
          TokenStart := p;
          while p^ in ['=','<','>'] do Inc(p);
          SetString(Token, TokenStart, p - TokenStart);
          Result := stPredicate;
          Exit;
        end;
        Inc(p);
      end;
      '0'..'9':
      begin
        if not Assigned(TokenStart) then
        begin
          TokenStart := p;
          while p^ in ['0'..'9','.'] do Inc(p);
          SetString(Token, TokenStart, p - TokenStart);
          Result := stNumber;
          Exit;
        end else
          Inc(p);
      end;
      #0:
      begin
        if Assigned(TokenStart) then
        begin
          SetString(Token, TokenStart, p - TokenStart);
          Result := GetSQLToken(Token);
          Exit;
        end else
        begin
          Result := stEnd;
          Token := '';
          Exit;
        end;
      end;
    else
      StartToken;
      Inc(p);
    end;//of: case p^
  end;
end;

{ DBCommon.IsMultiTableQuery & DBCommon.GetTableNameFromSQL }

// fixes: for IsMultiTableQuery:
// added check allowed words after from: ( where, union, ... ).
// added support "select from select"

// IsMultiTableQuery and GetTableNameFromSQL work by a similar principle but have a separate code.
// Now they have the common logic code.

// Now TClientDataSet can correctly establish indexes. It can make apply updates and use of unique
// indexes for updates.

function IsMultiTableQueryEx(const SQL: string; var TableName: string; DefaultResult: Boolean): Boolean;
const
  SInnerJoin = 'inner join ';       { do not localize }
  SOuterJoin = 'outer join ';       { do not localize }
var
  Start{+}, StartTmp{+.}: PChar;
  SResult, Token: string;
  SQLToken, CurSection{+}, SQLTokenTmp{+.}: TSQLToken;
begin
  SResult := '';
  Start := PChar(SQL);
  CurSection := stUnknown;
  Result := DefaultResult;
  repeat
    SQLToken := NextSQLToken(Start, Token, CurSection);
    if SQLToken in SQLSections then CurSection := SQLToken;
  until SQLToken in [stEnd, stFrom];
  if SQLToken = stFrom then
  begin
    repeat
      SQLToken := NextSQLToken(Start, Token, CurSection);
      {+}
      if SQLToken = stSelect then
      begin // select from select:
        Result := IsMultiTableQueryEx( Start, TableName, Result );
        exit;
      end
      else
      {+.}
      if SQLToken in SQLSections then
        CurSection := SQLToken else
      // stValue is returned if TableNames contain quote chars.
      if (SQLToken = stTableName) or (SQLToken = stValue) then
      begin
        SResult := Token;
        while (Start[0] = '.') and not (SQLToken in [stEnd]) do
        begin
          SQLToken := NextSqlToken(Start, Token, CurSection);
          SResult := SResult + '.' + Token;
        end;
        if (Start[0] = ',') or (Start[1] = ',') then
        begin
          Result := False;
          exit;
        end;
        {+}
        TableName := SResult;
        StartTmp := Start;
        SQLTokenTmp := NextSqlToken(Start, Token, CurSection);
        if SQLTokenTmp = stTableName then
        begin
          // finding delimiter ',' for support "select from select"
          // example:
          // select * from (select * from table1) t
          while not (StartTmp^ in [',', #0 ]) do
            inc(StartTmp);
          if StartTmp^=',' then
          begin
            Result := True;
            exit;
          end
          else
          if StartTmp^=#0 then
          begin
            Result := False;
            exit;
            //SQLTokenTmp := stEnd;
            //Start := StartTmp;
          end;
        end;
        {+.}
        if Assigned(AnsiStrPos(Start, PChar(SInnerJoin))) or
           Assigned(AnsiStrPos(Start, PChar(SOuterJoin))) then
        begin
          Result := False;
          Exit;
        end;
        {+}
        if not ( SQLTokenTmp in [stWhere, stGroupBy, stHaving, stPlan, stOrderBy, stForUpdate,
          stEnd] ) then
        begin
          SQLToken := NextSqlToken(Start, Token, CurSection);
          if SQLToken = stTableName then
          begin
            Result := True;
            Exit;
          end;
        end;
        // scan next select for multitables
        //  (example: select * from table1 UNION select * from table2). "UNION" is multiselect,
        // since unique indexes can be duplicated.
        if (SQLTokenTmp <> stEnd) then
        begin
          SResult := #1;
          Result := IsMultiTableQueryEx( Start, SResult, False ) or (SResult<>#1);
          //if (not Result) and (SResult<>TableName) then
          //  Result := True;
        end
        else
          Result := False;
        {+.}
        Exit;
      end;
    until (CurSection <> stFrom) or (SQLToken in [stEnd, stTableName]);
  end;
  {+}
  Result := True; // default result
  {+.}
end;

function IsMultiTableQueryFix(const SQL: string): Boolean;
var
  TableName: string;
begin
  TableName := '';
  Result := IsMultiTableQueryEx(SQL, TableName, True);
end;

function GetTableNameFromSQLFix(const SQL: string): string;
begin
  Result := '';
  if IsMultiTableQueryEx(SQL, Result, True) then
    Result := '';
end;

// *********************** RUNTIME PATCH ***********************

Type

  PJumpInstruction = ^TJumpInstruction;
  TJumpInstruction = packed record
    case byte of
      1:(
        Code   :Byte;  // jump instruction ($FF)
        Offset :DWORD; // jump offset
        UnUsed :Byte;
      );
      2:(
        DW1: DWORD;
        DW2: DWORD;
      );
  end;

  PPointer = ^Pointer;
  PPPointer = ^PPointer;

Const
  PageRWFlag = {$IFDEF LINUX} PROT_READ or PROT_WRITE {$ELSE} PAGE_READWRITE {$ENDIF};

function GetNativeAddr(Ptr:Pointer):Pointer;
begin
  Result := Ptr;
  {$IFDEF LINUX}
    // TODO: ???: Need check for LINUX.
  {$ENDIF}
  if  (PJumpInstruction(Result)^.Code=$FF) and            // long jmp to package_address_5b
      (PJumpInstruction(Integer(Result)+6)^.Code=$8B) and // mov
      (PJumpInstruction(Integer(Result)+7)^.Code=$C0)     // eax,eax
  then
    Result := PPPointer(Integer(Result) + 2)^^;
end;

function JumpFromCode( MSrc, MDest:Pointer; SaveJump :PJumpInstruction): Boolean;
 var
  SaveFlagSrc: DWORD;
begin
  Result := False;
  if Assigned(SaveJump) then
    FillChar(SaveJump^, SizeOf(TJumpInstruction), 0);
  if MSrc = nil then
    exit;
  MSrc := GetNativeAddr(MSrc);
  if VirtualProtect(MSrc,  SizeOf(TJumpInstruction), PageRWFlag, @SaveFlagSrc ) then
  try
     if Assigned(SaveJump) then
       SaveJump^ := PJumpInstruction(MSrc)^;
     with PJumpInstruction(MSrc)^ do begin
       Code := $E9; // JMP SIZE = 5b
       Offset := Integer(MDest) - Integer(MSrc) - 5;
     end;
     Result := True;
  finally
    VirtualProtect(MSrc,  SizeOf(TJumpInstruction), SaveFlagSrc,  @SaveFlagSrc );
    {$IFDEF LINUX}
    {$ELSE}
    FlushInstructionCache(GetCurrentProcess, MSrc, SizeOf(TJumpInstruction));
    {$ENDIF}
  end;
end;

function RestoreSavedCode( MSrc:Pointer; var SaveJump :TJumpInstruction): Boolean;
var
  SaveFlagSrc: DWORD;
begin
  Result := False;
  if (SaveJump.Code = 0) or (MSrc = nil) then
    exit;
  MSrc := GetNativeAddr(MSrc);
  try
    if VirtualProtect(MSrc,  SizeOf(TJumpInstruction), PageRWFlag, @SaveFlagSrc ) then
    try
       PJumpInstruction(MSrc)^ := SaveJump;  // Restore Code
       // Clear Buffer
       FillChar(SaveJump, SizeOf(TJumpInstruction), 0);
    finally
      VirtualProtect(MSrc,  SizeOf(TJumpInstruction), SaveFlagSrc, @SaveFlagSrc );
      {$IFDEF LINUX}
      {$ELSE}
      FlushInstructionCache(GetCurrentProcess, MSrc, SizeOf(TJumpInstruction));
      {$ENDIF}
    end;
    Result := True;
  except
  end;
end;

{$IFDEF SqlExpr_TCustomSQLDataSet_GetRecord}
  {$IFNDEF DELPHI_7UP}
var
  jmp_TCustomSQLDataSet_GetRecord: TJumpInstruction;
  {$ENDIF}
{$ENDIF}
{$IFDEF Provider_TDataSetProvider_InternalGetRecords}
var
  jmp_TDataSetProvider_InternalGetRecords: TJumpInstruction;
{$ENDIF}
{$IFDEF SqlExpr_TSQLConnection_CloneConnection}
var
  jmp_TSQLConnection_CloneConnection: TJumpInstruction;
{$ENDIF}
{$IFDEF SqlExpr_TSQLConnection_ConnectionOptions}
  {$IFNDEF DELPHI_7UP}
var
  jmp_TSQLConnection_DoConnect: TJumpInstruction;
  {$ENDIF}
{$ENDIF}
{$IFDEF SqlExpr_TCustomSQLDataSet_GetQueryFromType}
var
  jmp_TCustomSQLDataSet_PrepareStatement: TJumpInstruction;
  jmp_TCustomSQLDataSet_PSGetDefaultOrder: TJumpInstruction;
  jmp_TCustomSQLDataSet_OpenSchema: TJumpInstruction;
{$ENDIF}
{$IFDEF SqlExpr_TCustomSQLDataSet_SetSchemaOption}
var
  jmp_TCustomSQLDataSet_ExecuteStatement: TJumpInstruction;
{$ENDIF}
{$IFDEF db_TParams_ParseSQL}
var
  jmp_TParams_ParseSQL: TJumpInstruction;
{$ENDIF}
{$IFDEF dbCommon_NextSQLToken}
var
  jmp_NextSQLToken: TJumpInstruction;
{$ENDIF}
{$IFDEF dbCommon_IsMultiTableQuery}
var
  jmp_IsMultiTableQuery: TJumpInstruction;
{$ENDIF}
{$IFDEF dbCommon_GetTableNameFromSQL}
var
  jmp_GetTableNameFromSQL: TJumpInstruction;
{$ENDIF}

{$IFDEF _UNICODE_}
var
  jmp_TSQLConnection_Execute: TJumpInstruction;
  {$IFNDEF SqlExpr_TCustomSQLDataSet_SetSchemaOption}
  jmp_TCustomSQLDataSet_ExecuteStatement: TJumpInstruction;
  {$ENDIF}
  jmp_TCustomSQLDataSet_AddFieldDesc: TJumpInstruction;
  jmp_TCustomSQLDataSet_GetFieldData1: TJumpInstruction;
  jmp_TDataSet_DataConvert: TJumpInstruction;
  jmp_TDataSet_GetFieldData2: TJumpInstruction;
  jmp_TParam_GetDataSize: TJumpInstruction;
  jmp_TParam_GetData: TJumpInstruction;
  jmp_TWideStringField_GetDataSize: TJumpInstruction;
  jmp_TDataPacketWriterFix_AddColumn: TJumpInstruction;
{$ENDIF IFDEF _UNICODE_}

procedure InstallFix;
{$IFDEF _UNICODE_}
var
  pMethod1: TMethod;
  pMethod2: TMethod;
  pCustomSQLDataSet_GetFieldData1_old: TCustomSQLDataSet_GetFieldData1 absolute pMethod1;
  pCustomSQLDataSet_GetFieldData1_new: TCustomSQLDataSet_GetFieldData1 absolute pMethod2;
  pDataSet_GetFieldData2_old: TDataSet_GetFieldData2 absolute pMethod1;
  pDataSet_GetFieldData2_new: TDataSet_GetFieldData2 absolute pMethod2;
{$ENDIF}
begin
{$IFDEF SqlExpr_TCustomSQLDataSet_GetRecord}
  {$IFNDEF DELPHI_7UP}
  JumpFromCode(@TCustomSQLDataSetP.GetRecord, @TCustomSQLDataSetFix.GetRecord,
    @jmp_TCustomSQLDataSet_GetRecord);
  {$ENDIF}
{$ENDIF}

{$IFDEF Provider_TDataSetProvider_InternalGetRecords}
  JumpFromCode(@TDataSetProviderP.InternalGetRecords, @TDataSetProviderFix.InternalGetRecords,
    @jmp_TDataSetProvider_InternalGetRecords);
{$ENDIF}

{$IFDEF SqlExpr_TSQLConnection_CloneConnection}
  bFixedSqlExpr := JumpFromCode(@TSQLConnection.CloneConnection, @TSQLConnectionFix.CloneConnection,
    @jmp_TSQLConnection_CloneConnection);
{$ENDIF}

{$IFDEF SqlExpr_TSQLConnection_ConnectionOptions}
  {$IFNDEF DELPHI_7UP}
  bFixedSqlExpr := bFixedSqlExpr and
    JumpFromCode(@TSQLConnectionP.DoConnect, @TSQLConnectionFix.DoConnect,
      @jmp_TSQLConnection_DoConnect);
  {$ENDIF}
{$ENDIF}

{$IFDEF SqlExpr_TCustomSQLDataSet_GetQueryFromType}
  bFixedSqlExpr := bFixedSqlExpr and
    JumpFromCode(@TCustomSQLDataSetP.PrepareStatement, @TCustomSQLDataSetFixD7.PrepareStatement,
      @jmp_TCustomSQLDataSet_PrepareStatement);
  bFixedSqlExpr := bFixedSqlExpr and
    JumpFromCode(@TCustomSQLDataSetP.PSGetDefaultOrder, @TCustomSQLDataSetFixD7.PSGetDefaultOrder,
      @jmp_TCustomSQLDataSet_PSGetDefaultOrder);
  bFixedSqlExpr := bFixedSqlExpr and
    JumpFromCode(@TCustomSQLDataSetP.OpenSchema, @TCustomSQLDataSetFixD7.OpenSchema,
      @jmp_TCustomSQLDataSet_OpenSchema);
{$ENDIF}
{$IFDEF SqlExpr_TCustomSQLDataSet_SetSchemaOption}
  bFixedSqlExpr := bFixedSqlExpr and
    JumpFromCode(@TCustomSQLDataSetP.ExecuteStatement, @TCustomSQLDataSetFixD7.ExecuteStatement,
      @jmp_TCustomSQLDataSet_ExecuteStatement);
{$ELSE}
  bFixedSqlExpr := False;
{$ENDIF}

{$IFDEF db_TParams_ParseSQL}
  JumpFromCode(@TParams.ParseSQL, @TParamsFix.ParseSQL,
    @jmp_TParams_ParseSQL);
{$ENDIF}

{$IFDEF dbCommon_NextSQLToken}
  JumpFromCode(@NextSQLToken, @NextSQLTokenFix,
    @jmp_NextSQLToken);
{$ENDIF}
{$IFDEF dbCommon_IsMultiTableQuery}
  JumpFromCode(@IsMultiTableQuery, @IsMultiTableQueryFix,
    @jmp_IsMultiTableQuery);
{$ENDIF}
{$IFDEF dbCommon_GetTableNameFromSQL}
  JumpFromCode(@GetTableNameFromSQL, @GetTableNameFromSQLFix,
    @jmp_GetTableNameFromSQL);
{$ENDIF}

{$IFDEF _UNICODE_}
  bFixedUnicode := bFixedSqlExpr and
  JumpFromCode(@TSQLConnection.Execute, @TSQLConnectionFix.Execute,
    @jmp_TSQLConnection_Execute);
  {$IFNDEF SqlExpr_TCustomSQLDataSet_SetSchemaOption}
  bFixedUnicode := bFixedUnicode and
  JumpFromCode(@TCustomSQLDataSet.ExecuteStatement, @TCustomSQLDataSetFixD7.ExecuteStatement,
    @jmp_TCustomSQLDataSet_ExecuteStatement);
  {$ENDIF}
  bFixedUnicode := bFixedUnicode and
  JumpFromCode(@TCustomSQLDataSetP.AddFieldDesc, @TCustomSQLDataSetFixD7.AddFieldDesc,
    @jmp_TCustomSQLDataSet_AddFieldDesc);
  {$WARNINGS OFF}
  with TCustomSQLDataSet.Create(nil) do
  try
    pCustomSQLDataSet_GetFieldData1_old := GetFieldData;
  finally
    Free;
  end;
  with TCustomSQLDataSetFixD7.Create(nil) do
  try
    pCustomSQLDataSet_GetFieldData1_new := GetFieldData;
  finally
    Free;
  end;
  bFixedUnicode := bFixedUnicode and
  JumpFromCode(pMethod1.Code, pMethod2.Code,
    @jmp_TCustomSQLDataSet_GetFieldData1);
  bFixedUnicode := bFixedUnicode and
  JumpFromCode(@TDataSetP.DataConvert, @TDataSetFix.DataConvert,
    @jmp_TDataSet_DataConvert);
  with TDataSet.Create(nil) do
  try
    pDataSet_GetFieldData2_old := GetFieldData;
  finally
    Free;
  end;
  with TDataSetFix.Create(nil) do
  try
    pDataSet_GetFieldData2_new := GetFieldData;
  finally
    Free;
  end;
  {$WARNINGS ON}
  bFixedUnicode := bFixedUnicode and
  JumpFromCode(pMethod1.Code, pMethod2.Code,
    @jmp_TDataSet_GetFieldData2);
  bFixedUnicode := bFixedUnicode and
  JumpFromCode(@TParam.GetDataSize, @TParamFix.GetDataSize,
    @jmp_TParam_GetDataSize);
  bFixedUnicode := bFixedUnicode and
  JumpFromCode(@TParam.GetData, @TParamFix.GetData,
    @jmp_TParam_GetData);
  bFixedUnicode := bFixedUnicode and
  JumpFromCode(@TWideStringFieldP.GetDataSize, @TWideStringFieldFix.GetDataSize,
    @jmp_TWideStringField_GetDataSize);
  bFixedUnicode := bFixedUnicode and
  JumpFromCode(@TDataPacketWriterP.AddColumn, @TDataPacketWriterFix.AddColumn,
    @jmp_TDataPacketWriterFix_AddColumn);
{$ENDIF IFDEF _UNICODE_}

end;

procedure UnInstallFix;
{$IFDEF _UNICODE_}
var
  pMethod1: TMethod;
  pCustomSQLDataSet_GetFieldData1_old: TCustomSQLDataSet_GetFieldData1 absolute pMethod1;
  pDataSet_GetFieldData2_old: TDataSet_GetFieldData2 absolute pMethod1;
{$ENDIF}
begin
{$IFDEF SqlExpr_TCustomSQLDataSet_GetRecord}
  {$IFNDEF DELPHI_7UP}
  RestoreSavedCode(@TCustomSQLDataSetP.GetRecord, jmp_TCustomSQLDataSet_GetRecord);
  {$ENDIF}
{$ENDIF}

{$IFDEF Provider_TDataSetProvider_InternalGetRecords}
  RestoreSavedCode(@TDataSetProviderP.InternalGetRecords, jmp_TDataSetProvider_InternalGetRecords);
{$ENDIF}

{$IFDEF SqlExpr_TSQLConnection_CloneConnection}
  RestoreSavedCode(@TSQLConnection.CloneConnection, jmp_TSQLConnection_CloneConnection);
{$ENDIF}

{$IFDEF SqlExpr_TSQLConnection_ConnectionOptions}
  {$IFNDEF DELPHI_7UP}
  RestoreSavedCode(@TSQLConnectionP.DoConnect, jmp_TSQLConnection_DoConnect);
  {$ENDIF}
{$ENDIF}

{$IFDEF SqlExpr_TCustomSQLDataSet_GetQueryFromType}
  RestoreSavedCode(@TCustomSQLDataSetP.PrepareStatement, jmp_TCustomSQLDataSet_PrepareStatement);
  RestoreSavedCode(@TCustomSQLDataSetP.PSGetDefaultOrder, jmp_TCustomSQLDataSet_PSGetDefaultOrder);
  RestoreSavedCode(@TCustomSQLDataSetP.OpenSchema, jmp_TCustomSQLDataSet_OpenSchema);
{$ENDIF}
{$IFDEF SqlExpr_TCustomSQLDataSet_SetSchemaOption}
  RestoreSavedCode(@TCustomSQLDataSetP.ExecuteStatement, jmp_TCustomSQLDataSet_ExecuteStatement);
{$ENDIF}

{$IFDEF db_TParams_ParseSQL}
  RestoreSavedCode(@TParams.ParseSQL, jmp_TParams_ParseSQL);
{$ENDIF}
{$IFDEF dbCommon_NextSQLToken}
  RestoreSavedCode(@NextSQLToken, jmp_NextSQLToken);
{$ENDIF}
{$IFDEF dbCommon_IsMultiTableQuery}
  RestoreSavedCode(@IsMultiTableQuery, jmp_IsMultiTableQuery);
{$ENDIF}
{$IFDEF dbCommon_GetTableNameFromSQL}
  RestoreSavedCode(@GetTableNameFromSQL, jmp_GetTableNameFromSQL);
{$ENDIF}

{$IFDEF _UNICODE_}
  RestoreSavedCode(@TSQLConnection.Execute, jmp_TSQLConnection_Execute);
  {$IFNDEF SqlExpr_TCustomSQLDataSet_SetSchemaOption}
  RestoreSavedCode(@TCustomSQLDataSet.ExecuteStatement, jmp_TCustomSQLDataSet_ExecuteStatement);
  {$ENDIF}
  RestoreSavedCode(@TCustomSQLDataSetP.AddFieldDesc, jmp_TCustomSQLDataSet_AddFieldDesc);
  {$WARNINGS OFF}
  with TCustomSQLDataSet.Create(nil) do
  try
    pCustomSQLDataSet_GetFieldData1_old := GetFieldData;
  finally
    Free;
  end;
  RestoreSavedCode(pMethod1.Code, jmp_TCustomSQLDataSet_GetFieldData1);
  RestoreSavedCode(@TDataSetP.DataConvert, jmp_TDataSet_DataConvert);
  with TDataSet.Create(nil) do
  try
    pDataSet_GetFieldData2_old := GetFieldData;
  finally
    Free;
  end;
  {$WARNINGS ON}
  RestoreSavedCode(pMethod1.Code, jmp_TDataSet_GetFieldData2);
  RestoreSavedCode(@TParam.GetDataSize, jmp_TParam_GetDataSize);
  RestoreSavedCode(@TParam.GetData, jmp_TParam_GetData);
  RestoreSavedCode(@TWideStringFieldP.GetDataSize, jmp_TWideStringField_GetDataSize);
  RestoreSavedCode(@TDataPacketWriterP.AddColumn, jmp_TDataPacketWriterFix_AddColumn);
{$ENDIF IFDEF _UNICODE_}

  bFixedSqlExpr := False;
  bFixedUnicode := False;
end;

{$ENDIF IFNDEF _UNSUPPORTED_}

//(*
{$UNDEF TCustomSQLDataSetPrivate}

{$hints off}
{$IFNDEF CLR}
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF CompilerVersion <= 18.00}
      {$DEFINE TCustomSQLDataSetPrivate}
type
      {$IF CompilerVersion = 18.00}
        TCustomSQLDataSetPrivate = class(TWideDataSet)
        private
          FBlobBuffer: TBlobByteData;
          FCalcFieldsBuffer: PChar;
          FCheckRowsAffected: Boolean;
          FClonedConnection: TSqlConnection;
          FCommandText: WideString;
          FCommandType: TSQLCommandType;
          FCurrentBlobSize: LongWord;
          FDataLink: TDataLink;
          FDesignerData: string;
          FGetNextRecordSet: Boolean;
          FIndexDefs: TIndexDefs;
          FIndexDefsLoaded: Boolean;
          FLastError: string;  // DBExpress GetError() clears error; need to save last
          FMaxBlobSize: Integer;
          FMaxColSize: LongWord;
          FNativeCommand: WideString;
          FGetMetadata: Boolean;
          FNumericMapping: Boolean;
          FParamCheck: Boolean;
          FParamCount: Integer;
          FParams: TParams;
          FPrepared: Boolean;
          FProcParams: TList;
          FRecords: Integer;
          FRowsAffected: Integer;
          FSchemaInfo: TSQLSchemaInfo;
          FParseSelectSql: TParseSqlEvent;
          FParseUpdateSql: TParseSqlEvent;
          FParseDeleteSql: TParseSqlEvent;
          FParseInsertSql: TParseInsertSqlEvent;
          FSortFieldNames: WideString;

          FSQLCommand: TISQLCommand;
          FSQLConnection: TSQLConnection;
          FSQLCursor: TISQLCursor;

          FStatementOpen: Boolean;
          FTransactionLevel: SmallInt;
          FSchemaName: string;
        end;
      {$ELSEIF CompilerVersion = 17.00}
        TCustomSQLDataSetPrivate = class(TDataSet)
        private
          FBlobBuffer: TBlobByteData;
          FCalcFieldsBuffer: PChar;
          FCheckRowsAffected: Boolean;
          FClonedConnection: TSqlConnection;
          FCommandText: string;
          FCommandType: TSQLCommandType;
          FCurrentBlobSize: LongWord;
          FDataLink: TDataLink;
          FDesignerData: string;
          FGetNextRecordSet: Boolean;
          FIndexDefs: TIndexDefs;
          FIndexDefsLoaded: Boolean;
          FLastError: string;  // DBExpress GetError() clears error; need to save last
          FMaxBlobSize: Integer;
          FMaxColSize: LongWord;
          FNativeCommand: string;
          FGetMetadata: Boolean;
          FNumericMapping: Boolean;
          FParamCheck: Boolean;
          FParamCount: Integer;
          FParams: TParams;
          FPrepared: Boolean;
          FProcParams: TList;
          FRecords: Integer;
          FRowsAffected: Integer;
          FSchemaInfo: TSQLSchemaInfo;
          FParseSelectSql: TParseSqlEvent;
          FParseUpdateSql: TParseSqlEvent;
          FParseDeleteSql: TParseSqlEvent;
          FParseInsertSql: TParseInsertSqlEvent;
          FSortFieldNames: string;
          FSQLCommand: ISQLCommand;
          FSQLConnection: TSQLConnection;
          FSQLCursor: ISQLCursor;
          FStatementOpen: Boolean;
          FTransactionLevel: SmallInt;
          FSchemaName: string;
        end;
      {$ELSEIF CompilerVersion >= 14.50}
        TCustomSQLDataSetPrivate = class(TDataSet)
        private
          FBlobBuffer: TBlobByteData;
          FCalcFieldsBuffer: PChar;
          FCheckRowsAffected: Boolean;
          FClonedConnection: TSqlConnection;
          FCommandText: string;
          FCommandType: TSQLCommandType;
          FCurrentBlobSize: LongWord;
          FDataLink: TDataLink;
          FDesignerData: string;
          FGetNextRecordSet: Boolean;
          FIndexDefs: TIndexDefs;
          FIndexDefsLoaded: Boolean;
          FLastError: string;  // DBExpress GetError() clears error; need to save last
          FMaxBlobSize: Integer;
          FMaxColSize: LongWord;
          FNativeCommand: string;
          FNoMetadata: Boolean deprecated;
          FGetMetadata: Boolean;
          FNumericMapping: Boolean;
          FParamCheck: Boolean;
          FParamCount: Integer;
          FParams: TParams;
          FPrepared: Boolean;
          FProcParams: TList;
          FRecords: Integer;
          FRowsAffected: Integer;
          FSchemaInfo: TSQLSchemaInfo;
          FSortFieldNames: string;
          FSQLCommand: ISQLCommand;
          FSQLConnection: TSQLConnection;
          FSQLCursor: ISQLCursor;
          FStatementOpen: Boolean;
          FTransactionLevel: SmallInt;
          FSchemaName: string;
        end;
      {$ELSE}
        TCustomSQLDataSetPrivate = class(TDataSet)
        private
          FBlobBuffer: TBlobByteData;
          FCalcFieldsBuffer: PChar;
          FCheckRowsAffected: Boolean;
          FClonedConnection: TSqlConnection;
          FCommandText: string;
          FCommandType: TSQLCommandType;
          FCurrentBlobSize: LongWord;
          FDataLink: TDataLink;
          FDesignerData: string;
          FFieldBuffer: PChar;
          FGetNextRecordSet: Boolean;
          FIndexDefs: TIndexDefs;
          FIndexDefsLoaded: Boolean;
          FLastError: string;  // DBExpress GetError() clears error; need to save last
          FMaxBlobSize: Integer;
          FMaxColSize: LongWord;
          FNativeCommand: string;
          FNoMetadata: Boolean platform;
          FParamCheck: Boolean;
          FParamCount: Integer;
          FParams: TParams;
          FPrepared: Boolean;
          FProcParams: TList;
          FRecords: Integer;
          FRowsAffected: Integer;
          FSchemaInfo: TSQLSchemaInfo;
          FSortFieldNames: string;
          FSQLCommand: ISQLCommand;
          FSQLConnection: TSQLConnection;
          FSQLCursor: ISQLCursor;
          FStatementOpen: Boolean;
          FTransactionLevel: SmallInt;
        end;
      {$IFEND}
    {$IFEND}
  {$ENDIF} // of: $IFDEF CONDITIONALEXPRESSIONS
{$ENDIF IFNDEF CLR}
{$hints on}

{$IFNDEF CLR}

{ TSQLStoredProc }

{$hints off}
procedure TSQLStoredProc.PrepareStatement;
{$IFDEF TCustomSQLDataSetPrivate}
var
  vSelfPrivate: TCustomSQLDataSetPrivate;
  vSQLCommand: {$IF CompilerVersion >= 18.00}TISQLCommand{$ELSE}ISQLCommand{$IFEND};
  i: Integer;
  iFldNum: LongWord;
  vParam: TParam;
  vParamDesc: SPParamDesc;
  iFldType, iSubType: Word;
  DataLen, IInd: Integer;
  vProcParams: TList;
  SBcd: string;
  Bcd: TBcd;
  RecBuffer: PChar;
  DrvLocale: TLocale;

  procedure CalcUnits( const Params: TParams; const ProcParams: TList;
            const Index: Integer; pArgDesc: pSPParamDesc);
  var
    I: Integer;
    ArgDesc: SPParamDesc;
  begin
    I := Index + 1;
    ArgDesc := pArgDesc^;
    pArgDesc.iUnits1 := 0;
    pArgDesc.iUnits2 := 0;
    while (I < Params.Count) do
    begin
      ArgDesc := (PSPParamDesc(ProcParams.Items[I]))^;
      if ArgDesc.iParamNum <> pArgDesc.iParamNum then
        break;
      Inc(pArgDesc.iUnits1);
      Inc(pArgDesc.iUnits2);
      if ArgDesc.iDataType = ftADT then
      begin
        Inc(pArgDesc.iUnits2, ArgDesc.iUnits2);
        Inc(I, ArgDesc.iUnits2);
      end else
        Inc(I);
    end;
  end;

  {$IFDEF DELPHI_9UP}
  procedure GetParamData(Param: TParam; Buffer: Pointer; const DrvLocale: TLocale);

    function GetNativeStr: PChar;
    begin
      Param.NativeStr := VarToStr(Param.Value);
      Result := PChar(Param.NativeStr);
    end;

  begin
    if Buffer <> nil then
    begin
      with Param do
        if DataType in [ftString, ftFixedChar, ftMemo]  then
        begin
          NativeStr := VarToStr(Value);
          GetData(Buffer);
        end
        else
          GetData(Buffer);
    end;
  end;
  {$ENDIF}

{$ENDIF}
begin
  inherited;
  {$IFDEF TCustomSQLDataSetPrivate}
  if {NoMetadata and} (Params.Count > 0) then
  begin
    {.$IFDEF SqlExpr_TCustomSQLDataSet_SetSchemaOption}
    vSelfPrivate := TCustomSQLDataSetPrivate(Self);
    vSQLCommand := vSelfPrivate.FSQLCommand;
    if vSQLCommand <> nil then
    begin
      try
        vProcParams := vSelfPrivate.FProcParams;
        if vProcParams <> nil then
          i := vProcParams.Count
        else
          i := 0;
        DrvLocale := nil;
        if (i < Params.Count) and (vSQLCommand.SetOption(eCommParamCount, Params.Count) = DBXERR_NONE) then
        begin
          for i := i to Params.Count - 1 do
          begin
            RecBuffer := nil;
            vParam := Params[i];
            if vParam.ParamType = ptUnknown then  // Midas assumes its Input
              vParam.ParamType := ptInput;
            iFldNum := i + 1;
            iFldType := FldTypeMap[Params[I].DataType];
            if iFldType = fldUNKNOWN then
              iFldType := fldZSTRING;
            iSubType := 0;
            if iFldType in [fldBlob, fldZString] then
              iSubType := Word(FldSubTypeMap[Params[I].DataType]);
            FillChar(vParamDesc, SizeOf(vParamDesc), #0);
            DataLen := Params[I].GetDataSize;
            with vParamDesc, Params[i] do
              begin
                iParamNum := iFldNum;
                szName := Name;
                iArgType := ParamType;
                iDataType := DataType;
                iUnits1 := Precision;
                iUnits2 := NumericScale;
                iLen := DataLen;
              end;
            iInd := 0;
            if ((vParam.ParamType = ptInput) and vParam.IsNull) then
              iInd := 1
            else
            if DataLen > 0 then
            begin
              RecBuffer := AllocMem(DataLen);
              if Params[I].ParamType <> ptOutput then
                GetParamData(Params[I], RecBuffer, DrvLocale)
              else
                FillChar(RecBuffer^, DataLen, 0);
              if vParam.ParamType = ptInput then
                vParam.Size := 0;
              if (vParam.ParamType = ptOutput) and not(iFldType in [fldFLOAT]) then
                vParamDesc.iLen := 0
              else
                case iFldType of
                  fldBlob, fldZString, fldBYTES, fldVARBYTES:
                    begin
                      vParamDesc.iUnits2 := 0;
                      if (vParam.ParamType = ptInputOutput) and (DataLen > vParam.Size) then
                      begin
                        if iFldType = fldVARBYTES then
                          vParam.Size := DataLen - 2
                        else if iFldType = fldZString then
                          vParam.Size := DataLen - 1
                        else
                          vParam.Size := DataLen;
                      end;
                    end;
                  fldFLOAT:
                    begin
                      if vParam.Precision = 4 then
                        vParamDesc.iLen := 4
                      else
                        vParamDesc.iLen := Sizeof(Double);
                    end;
                  fldFMTBCD, fldBCD:
                    begin
                      iFldType := fldBCD;   { DBExpress does not distinguish }
                      if vParam.Size = 0 then
                      begin
                        SBcd := BcdToStr(PBcd(RecBuffer)^);
                        Bcd := StrToBcd(SBcd);
                        vParam.Size := Bcd.Precision;
                        vParamDesc.iUnits2 := Bcd.SignSpecialPlaces and $3F;
                      end else
                      begin
                        vParamDesc.iUnits2 := Params[I].NumericScale;
                      end;
                    end;
                  fldADT, fldARRAY:
                    begin
                      CalcUnits(Params, ProcParams, I, @vParamDesc);
                      vParamDesc.iLen := DataLen;
                    end;
                end;
            end else  // leave RecBuffer nil
            begin
              if not (iFldType in [fldADT, fldARRAY]) then
                iInd := 1;
            end;

            if vSQLCommand.setParameter(iFldNum, High(Word), TSTMTParamType(vParamDesc.iArgType),
                iFldType, iSubType, Params[I].Size,
                Integer(vParamDesc.iUnits2), vParamDesc.iLen, nil, IInd) <> DBXERR_NONE then
              Exit;
          end;

          FreeProcParams(vSelfPrivate.FProcParams);
        end;
      except
      end;
    end;
  end;
  //{$ELSE}
  //try
  // SQLCommand.SetOption(eCommParamCount, Params.Count);
  //except
  //end;
  {$ENDIF}
end;
{$hints on}
{$ENDIF ~CLR}

{$IFNDEF _UNSUPPORTED_}
initialization
  InstallFix;
finalization
  if IsLibrary then
    UnInstallFix;
// =============================================================================
{$ENDIF IFNDEF _UNSUPPORTED_}
end.
