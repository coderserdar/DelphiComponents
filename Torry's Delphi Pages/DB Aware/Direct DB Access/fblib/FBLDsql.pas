{
   Firebird Library
   Open Source Library No Data Aware for direct access to Firebird
   Relational Database from Borland Delphi / Kylix and Freepascal

   File:FBLDsql.pas
   Copyright (c) 2002 Alessandro Batisti
   fblib@altervista.org
   http://fblib.altervista.org

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
}

{$I fbl.inc}

{
@abstract(Dynamic SQL Statement)
@author(Alessandro Batisti <fblib@altervista.org>)
FbLib - Firebird Library @html(<br>)
FBLDsql.pas unit provides dsql Firebird query
}

unit FBLDsql;

interface

uses
  SysUtils,
 {$IFDEF D6P}
  Types,
  DateUtils,
 {$ENDIF}
 {$IFDEF FPC}
  DateUtils,
 {$ENDIF}
 {$IFDEF FBL_THREADSAFE}
  SyncObjs,
 {$ENDIF}
  Math,
  Classes,
  ibase_h, FBLTransaction;


type
  {QueryType property result values}
  TQueryTypes = (qtUnknown, qtSelect, qtInsert,
    qtUpdate, qtDelete, qtDDL,
    qtGetSegment, qtPutSegment,
    qtExecProcedure, qtStartTransaction,
    qtCommit, qtRollback,
    qtSelectForUpdate, qtSetGenerator);

  {@abstract(encapsulates properties and methods for managing dynamic SQL queries)}
  TFBLDsql = class(TComponent, IFBLTranEvent)
  private
    Fstmt: TISC_STMT_HANDLE;          // query statement
    FSQL: TStrings;                   // sql query
    FTransaction: TFBLTransaction;
    FPrepared: boolean;
    FQueryType: TQueryTypes;
    FPXSQLDA_IN: PXSQLDA;             //pointer to parameters
    FPXSQLDA_OUT: PXSQLDA;            //pointer to fields
    FParamcount: smallint;            //how many params ?
    FFieldcount: smallint;            //how many fields ?
    FParamAssigned: array of boolean; //for check params
    FCursor: string;                  // cursor name
    FCursorOpen: boolean;
    FBOF: boolean;
    FEOF: boolean;
    FFetchCount: integer;
    FAutoFetchFirst: boolean;
    FMaxFetch: integer;
    FFieldNames: TStringList;
    {$IFDEF FBL_THREADSAFE}
    procedure Lock;
    procedure UnLock;
    {$ENDIF}
    function GetDBHandle: PISC_DB_HANDLE;
    function GetTRHandle: PISC_TR_HANDLE;
    function GetQueryType: TQueryTypes;
    function GetEof: boolean;
    function GetParamCount: smallint;
    function GetFieldCount: smallint;
    function GetFetchCount: integer;
    function GetMaxFetch: integer;
    function GetNumericValue(const AValue: int64; const AScale: integer): double;
    function SetNumericValue(const AValue: double; const AScale: integer): int64;
    function GetRowAffected: integer;
    function GetPlan: string;
    procedure SetMaxFetch(const Value: integer);
    procedure SetTransaction(Value: TFBLTransaction);
    procedure SetSQL(Value: TStrings);
    procedure CheckTransaction;
    procedure CheckParamIdx(const AParamIdx: integer);
    procedure CheckFieldIdx(const AFieldIdx: integer);
    procedure AllocInOutXSQLDA;
    procedure FreeInOutXSQLDA;
    procedure OnSqlChanging(Sender: TObject);
    procedure CheckAssignParams;
    procedure RemoveFlagNull(const AParamIdx: integer);
  public
    {Create an istance  of a TFBLDsql}
    constructor Create(AOwner: TComponent); override;
    {Free up  all resources associated with this instance}
    destructor Destroy; override;
    {$IFDEF FPC_INFD}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: integer; stdcall;
    function _Release: integer; stdcall;
    {$ENDIF}

    {@EXCLUDE}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {@EXCLUDE}
    procedure DoOnEndTransaction; virtual;
    {@EXCLUDE}
    procedure DoOnDestroy; virtual;
    {Prepare statement for execution and alloc memory for output fields and input parameters}
    procedure Prepare;
    {Free the statement and free memory assigned to fields and parameters}
    procedure UnPrepare;
    {Execute the SQL statement currently assigned to the @link(SQL) Property @html(<br><br>)
    Note: @html(<br>)
    if query is not @link(Prepared) ExecSQL call internally @LINK(Prepare) method, raise Exception if
    SQL Statement contains parameters. @HTML(<Br><br>)
    example:
    @Longcode(#
      // myQry: TFBLDSql
      myQry.SQL.Add('Select * from Employee where JOB_COUNTRY = ?');
      // myQry.ExecSQL;   //raise exception
      // Correct code
      myQry.Prepare;
      myQry.ParamAsString(0,'USA');
      //now perform ExecSQL
      myQry.ExecSQL;
    #)}
    procedure ExecSQL;
    {Move an open cursor to first record}
    procedure First;
    {Move an open cursor to next record}
    procedure Next;
    {Close an open cursor , but not unprepare the sql statement}
    procedure Close;
    {Return field type : value are constant definites in ibase_h.pas
    @html(<pre>
    SQL_VARYING = 448;
    SQL_TEXT = 452;
    SQL_DOUBLE = 480;
    SQL_FLOAT = 482;
    SQL_LONG = 496;
    SQL_SHORT = 500;
    SQL_TIMESTAMP = 510;
    SQL_BLOB = 520;
    SQL_D_FLOAT = 530;
    SQL_ARRAY = 540;
    SQL_QUAD = 550;
    SQL_TYPE_TIME = 560;
    SQL_TYPE_DATE = 570;
    SQL_INT64 = 580;
    SQL_DATE = SQL_TIMESTAMP; </pre>)
    @param(AFieldIdx the index of output field)}
    function FieldType(const AFieldIdx: integer): smallint;
    {Return field sub type : value are constant definites in ibase_h.pas
    @param(AFieldIdx the index of output field)}
    function FieldSubType(const AFieldIdx: integer): smallint;
    {Return scale for numeric/decimal fields
    @param(AFieldIdx the index of output field)}
    function FieldScale(const AFieldIdx: integer): smallint;
    {Return field size in bytes
    @param(AFieldIdx the index of output field)}
    function FieldSize(const AFieldIdx: integer): smallint;
    {Return @True if field can accept null value
    @param(AFieldIdx the index of output field)}
    function FieldIsNullable(const AFieldIdx: integer): boolean;
    {Return @True if field is null value
    @param(AFieldIdx the index of output field)}
    function FieldIsNull(const AFieldIdx: integer): boolean;
    {Return field name (alias)
    @param(AFieldIdx the index of output field)}
    function FieldName(const AFieldIdx: integer): string;
    {Return table.field real name
    @param(AFieldIdx the index of output field)}
    function FieldRealName(const AFieldIdx: integer): string;
    {Return name of the table
    @param(AFieldIdx the index of output field)}
    function FieldTableName(const AFieldIdx: integer): string;
    {Return table ower name of a field
    @param(AFieldIdx the index of output field)}
    function FieldTableOwerName(const AFieldIdx: integer): string;
    {Return SQL description of a field (ex. 'INTEGER','VARCHAR','TEXT')
    @param(AFieldIdx the index of output field)}
    function FieldSQLTypeDesc(const AFieldIdx: integer): string;
    {Return field as  TXSQLVAR record see: ibase_h.pas and interbase api guide
    @param(AFieldIdx the index of output field)}
    function FieldAsXSQLVAR(const AFieldIdx: integer): TXSQLVAR;
    {Return field value as string
    @param(AFieldIdx the index of output field)}
    function FieldAsString(const AFieldIdx: integer): string;
    {Return field value as long
    @param(AFieldIdx the index of output field)}
    function FieldAsLong(const AFieldIdx: integer): longint;
    {Return field value as integer
    @param(AFieldIdx the index of output field)}
    function FieldAsInteger(const AFieldIdx: integer): integer;
    {Return field value as double
    @param(AFieldIdx the index of output field)}
    function FieldAsDouble(const AFieldIdx: integer): double;
    {Return field value as single
    @param(AFieldIdx the index of output field)}
    function FieldAsFloat(const AFieldIdx: integer): single;
    {Return field value as Int64
    @param(AFieldIdx the index of output field)}
    function FieldAsInt64(const AFieldIdx: integer): int64;
    {Return field value as TDateTime
    @param(AFieldIdx the index of output field)}
    function FieldAsDateTime(const AFieldIdx: integer): TDateTime;
    {Return blob field as string
    @param(AFieldIdx the index of output field)}
    function BlobFieldAsString(const AFieldIdx: integer): string;
    {Save blob field value into a TStream Object
    @param(AFieldIdx the index of output field ,
    @param(AStream Stream object where write data)}
    procedure BlobFieldSaveToStream(const AFieldIdx: integer; AStream: TStream);
    {Save blob field value into a Filename
    @param(AFieldIdx the index of output field ,
    @param(AFilename File where write data)}
    procedure BlobFieldSaveToFile(const AFieldIdx: integer; const AFileName: string);
    {Return field value as  TXSQLVAR record see: ibase_h.pas and interbase api guide
     @param(AFieldName name of the output field)}
    function FieldByNameAsXSQLVAR(const AFieldName: string): TXSQLVAR;
    {Return field value as string
    @param(AFieldName name of the output field)}
    function FieldByNameAsString(const AFieldName: string): string;
    {Return field value as Long
    @param(AFieldName name of the output field)}
    function FieldByNameAsLong(const AFieldName: string): longint;
    {Return field value as Integer
    @param(AFieldName name of the output field)}
    function FieldByNameAsInteger(const AFieldName: string): integer;
    {Return field value as Double
    @param(AFieldName name of the output field)}
    function FieldByNameAsDouble(const AFieldName: string): double;
    {Return field value as Float
    @param(AFieldName name of the output field)}
    function FieldByNameAsFloat(const AFieldName: string): single;
    {Return field value as Int64
    @param(AFieldName name of the output field)}
    function FieldByNameAsInt64(const AFieldName: string): int64;
    {Return field value as TDateTime
    @param(AFieldName name of the output field)}
    function FieldByNameAsDateTime(const AFieldName: string): TDateTime;
    {Return true if field is Null
    @param(AFieldName name of the output field)}
    function FieldByNameIsNull(const AFieldName: string): boolean;
    {Return blob field as string
    @param(AFieldIdx name of output field)}
    function BlobFieldByNameAsString(const AFieldName: string): string;
    {Save blob field value into a TStream Object
    @param(AFieldIdx name of output field) ,
    @param(AStream Stream object where write data)}
    procedure BlobFieldByNameSaveToStream(const AFieldName: string; AStream: TStream);
    {Save blob field value into a TStream Object
    @param(AFieldIdx name of output field ,
    @param(AStream Stream object where write data)}
    procedure BlobFieldByNameSaveToFile(const AFieldName: string;
      const AFileName: string);
    {Return True if Param can accept null value
    @param(AParamIdx index of the input param)}
    function ParamIsNullable(const AParamIdx: integer): boolean;
    {Return an integer value that identify parameter type
    valid value are constant in ibase_h.pas
    @param(AParamIdx index of the input param)
    @html(<br>) see also @link(FieldType)}
    function ParamType(const AParamIdx: integer): smallint;
    {Return an integer value that identify parameter subtype
    valid value are constant in ibase_h.pas
    @param(AParamIdx index of the input param)}
    function ParamSubType(const AParamIdx: integer): smallint;
    {Return scale for numeric/decimal param
     @param(AParamIdx index of the input param)}
    function ParamScale(const AParamIdx: integer): smallint;
    {Return size in bytes of param
     @param(AParamIdx index of the input param)}
    function ParamSize(const AParamIdx: integer): smallint;
    {Return extended sql description  of param
     @param(AParamIdx index of the input param)}
    function ParamSQLTypeDesc(const AParamIdx: integer): string;
    {Insert null value in param
    @param(AParamIdx index of the input param)}
    procedure ParamAsNull(const AParamIdx: integer);
    {Insert Short value (smallint) in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamAsShort(const AParamIdx: integer; AValue: smallint);
    {Insert Long value (Integer) in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamAsLong(const AParamIdx: integer; AValue: longint);
    {Insert Int64 value in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamAsInt64(const AParamIdx: integer; AValue: int64);
    {Insert string value in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamAsString(const AParamIdx: integer; const AValue: string);
    {Insert Double value in param  
    @param(AParamIdx index of the input param)
    @param(AValue value of param )}
    procedure ParamAsDouble(const AParamIdx: integer; AValue: double);
    {Insert Float (single) value in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamAsFloat(const AParamIdx: integer; AValue: single);
    {Insert TDateTime value in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamAsDateTime(const AParamIdx: integer; AValue: TDateTime);
    {Insert string value in BlobParam
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure BlobParamAsString(const AParamIdx: integer; const AValue: string);
    {Insert Value as TStream in BlobParam
    @param(AParamIdx index of the input param)
    @param(AValue TStream source data)}
    procedure BlobParamLoadFromStream(const AParamIdx: integer; AStream: TStream);
    {Copy the content of fileneme in BlobParam
    @param(AParamIdx index of the input param)
    @param(AValue TStream source data)}
    procedure BlobParamLoadFromFile(const AParamIdx: integer; const AFileName: string);
    {Return pointer to TFBLDatabase Handle see PISC_DB_HANDLE in ibase_hp.as}
    property DBHandle: PISC_DB_HANDLE read GetDbHandle;
    {Return pointer to TFBLTransaction Handle see PISC_TR_HANDLE in ibase_h.pas}
    property TRHandle: PISC_TR_HANDLE read GetTRHandle;
    {Return SQL Statement type this value is set after @link(Prepare)
    method has been called}
    property QueryType: TQueryTypes read GetQueryType;
    {Return number of parameters in SQL Statement this value is set after @link(Prepare)
    method has been called}
    property ParamCount: smallint read GetParamCount;
    {Return number of fields in SQL Statement this value is set after @link(Prepare)
    method has been called}
    property FieldCount: smallint read GetFieldCount;
    {Return @True if SQL Statemnet has been prepared}
    property Prepared: boolean read FPrepared;
    {Return @True if the cursor at the beginning of the query}
    property BOF: boolean read FBOF;
    {Return @True if the cursor has fetched all record}
    property EOF: boolean read GetEof;
    {Return the current count of record fetched}
    property FetchCount: integer read GetFetchCount;
    {Return number of rows affected after (UPDATE,DELETE,INSERT) SQL Statement}
    property RowsAffected: integer read GetRowAffected;
    {Return Plan of SQL statement, this value is set after @link(Prepare)
    method has been called}
    property Plan: string read GetPlan;
    {Return @True if cursor is open}
    property IsCursorOpen: boolean read FCursorOpen;
  published
    {set max number of fetched , if this property is set
     property  @link(EOF) is @True when MaxFetch = @link(FetchCount)}
    property MaxFetch: integer read GetMaxFetch write SetMaxFetch default 0;
    {TFBLTransaction object  where current TFBlDsql object is attached }
    property Transaction: TFBLTransaction read FTransaction write SetTransaction;
    {SQL Statement to execute @HTML(<br>)
    Example:
    @Longcode(#
      // myQry: TFBLDSql // myQry is an instance of TFBLDSql
      myQry.SQL.Add('Select * from Employee');
      myQry.ExecSQL;
    #)}
    property SQL: TStrings read FSQL write SetSQL;
    {if @true (default) move cursor to first record after ExecSQL}
    property AutoFetchFirst: boolean read FAutoFetchFirst write FAutoFetchfirst default True;
  end;

const
  {@exclude}
  BLOB_SEGMENT_LEN = 4095;

implementation

uses FBLmixf, iberror_h, FBLExcept, FBLConst;

{ TFBLDsql }

constructor TFBLDsql.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStmt := nil;
  FPXSQLDA_IN := nil;
  FPXSQLDA_IN := nil;
  FSQL := TStringList.Create;
  //TStringList(FSQL).OnChanging := OnSqlChanging;
  TStringList(FSQL).OnChange :=  OnSqlChanging;
  FPrepared := False;
  FCursor := RandomString;
  FCursorOpen := False;
  FAutoFetchFirst := True;
  FFieldNames := TStringList.Create;
end;

//------------------------------------------------------------------------------

destructor TFBLDsql.Destroy;
begin
  if FPrepared then UnPrepare;
  FStmt := nil;
  TStringList(FSQL).OnChange := nil;
  FSQL.Free;
  FFieldNames.Free;
  if Assigned(FTransaction) then FTransaction.RemQuery(self);
  inherited Destroy;
end;

//------------------------------------------------------------------------------
{$IFDEF FPC_INFD}
function TFBLDsql.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 
  else 
    Result := E_NOINTERFACE;
end;

function TFBLDsql._AddRef: integer;
begin
  Result := -1;
end;

function TFBLDsql._Release: integer;
begin
  Result := -1;
end;
{$ENDIF}
//------------------------------------------------------------------------------

procedure TFBLDsql.Notification(AComponent: TComponent;
  Operation: TOperation);//override;
begin
  if Operation = opRemove then
  begin
    if AComponent = FTransaction then FTransaction := nil;
  end;
  inherited Notification(AComponent, Operation);
end;


//------------------------------------------------------------------------------
{$IFDEF FBL_THREADSAFE}
procedure TFBLDSql.Lock;
begin
  if Assigned(FTransaction) then
    FTransaction.Database.Lock;
end;

procedure TFBLDSql.UnLock;
begin
  if Assigned(FTransaction) then
    FTransaction.Database.UnLock;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure TFBLDsql.DoOnEndTransaction;
begin
  if FPrepared then UnPrepare;
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.DoOnDestroy;
begin
  if FTransaction <> nil then FTransaction := nil;
end;

//------------------------------------------------------------------------------

function TFBLDsql.GetMaxFetch: integer;
begin
  Result := FMaxFetch;
end;

//------------------------------------------------------------------------------

function TFBLDsql.GetDBHandle: PISC_DB_HANDLE;
begin
  Result := FTransaction.DBHandle;
end;

//------------------------------------------------------------------------------

function TFBLDsql.GetTRHandle: PISC_TR_HANDLE;
begin
  Result := FTransaction.TRHandle;
end;

//------------------------------------------------------------------------------

function TFBLDsql.GetQueryType: TQueryTypes;
begin
  if FPrepared then
    Result := FQueryType
  else
    Result := qtUnknown;
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.SetMaxFetch(const Value: integer);
begin
  FMaxFetch := Value;
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.SetTransaction(Value: TFBLTransaction);
begin
  if Assigned(Value) and (Value <> FTransaction) then
    Value.AddQuery(self);
  if Assigned(Value) then
    FTransaction := Value
  else
    FTransaction := nil;
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.SetSQL(Value: TStrings);
begin
  FSQL.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.CheckTransaction;
begin
  if not Assigned(FTransaction) then
    FBLError(E_TR_NOT_ASSIGNED);
  if not FTransaction.InTransaction then
    FBLError(E_TR_NOACTIVE);
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.CheckParamIdx(const AParamIdx: integer);
begin
  if AParamIdx > (ParamCount - 1) then FBLError(E_QR_PAR_OUT_OFF_RANGE, [AParamIdx]);
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.CheckFieldIdx(const AFieldIdx: integer);
begin
  if AFieldIdx > (FieldCount - 1) then FBLError(E_QR_FIELD_OUT_OFF_RANGE, [AFieldIdx]);
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.CheckAssignParams;
var
  i: integer;
begin
  for i := 0 to ParamCount - 1 do
    if not FParamAssigned[i] then FBLError(E_QR_PARAM_NOT_SET, [i]);
end;

//------------------------------------------------------------------------------

function TFBLDsql.GetParamCount: smallint;
begin
  Result := FParamCount;
end;

//------------------------------------------------------------------------------

function TFBLDsql.GetFieldCount: smallint;
begin
  Result := FFieldCount;
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.Prepare;
var
  Status_vector: ISC_STATUS_VECTOR;
  res_buffer: array[0..7] of char;
  type_item: char;
  stmt_len: integer;
begin
  CheckTransaction;
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
    {$ENDIF}
    if FPrepared then Exit;
    if FSQL.Text = '' then
      FBLError(E_QR_EMPTY_SQL);
    if isc_dsql_alloc_statement2(@Status_Vector, DBHandle, @Fstmt) <> 0 then
      FBLShowError(@Status_vector);

    if isc_dsql_prepare(@Status_Vector, TRHandle, @Fstmt, 0,
      PChar(FSQL.Text), Transaction.Database.SQLDialect, nil) <> 0 then
      FBLShowError(@Status_vector);

    type_item := char(isc_info_sql_stmt_type);

    if isc_dsql_sql_info(@Status_Vector, @FStmt, 1, @type_item,
      SizeOf(res_buffer), res_buffer) <> 0 then
      FBLShowError(@Status_vector);
    if (res_buffer[0] <> char(isc_info_sql_stmt_type)) then
      FBLError(E_QR_UNKNOW);

    stmt_len := isc_vax_integer(@res_buffer[1], 2);
    FQueryType := TQueryTypes(isc_vax_integer(@res_buffer[3], stmt_len));

    case FQueryType of
      qtGetSegment, qtPutSegment,
      qtStartTransaction:
        FBLError(E_QR_NOTPERMITTED, [FSQL.Text]);
      qtDDL, qtSetGenerator,
      qtSelect, qtUpdate, qtInsert, qtDelete, qtExecProcedure,
      qtSelectForUpdate:
        AllocInOutXSQLDA;
      qtCommit, qtRollBack:
        AllocInOutXSQLDA;
      else
        FBLError(E_QR_UNKNOW);
    end;
    FPrepared := True;
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.UnPrepare;
var
  Status_vector: ISC_STATUS_VECTOR;
begin
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    try
      if Fstmt <> nil then
        if isc_dsql_free_statement(@Status_vector, @Fstmt, DSQL_drop) <> 0 then
          FBLShowError(@Status_vector);
    finally
      FCursorOpen := False;
      FPrepared := False;
      Fstmt := nil;
      FreeInOutXSQLDA;
      FParamcount := 0;
      FFieldcount := 0;
    end;
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.OnSqlChanging(Sender: TObject);
begin
  if Fstmt <> nil then
    UnPrepare;
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.AllocInOutXSQLDA;
var
  Status_vector: ISC_STATUS_VECTOR;
  i: integer;
begin
  // input parameters
  FBLCalloc(FPXSQLDA_IN, XSQLDA_LENGTH(1));
  FPXSQLDA_IN^.version := Short(SQLDA_VERSION1);
  FPXSQLDA_IN^.sqln := 1;
  if isc_dsql_describe_bind(@Status_vector, @Fstmt, Transaction.Database.SQLDialect,
    FPXSQLDA_IN) <> 0 then
    FBLShowError(@Status_vector);
  FParamcount := FPXSQLDA_IN^.sqld;
  if (FParamcount > 0) then
  begin
    FBLCalloc(FPXSQLDA_IN, XSQLDA_LENGTH(FParamcount));
    FPXSQLDA_IN^.sqln := FParamcount;
    FPXSQLDA_IN^.version := SQLDA_VERSION1;
    if isc_dsql_describe_bind(@Status_vector, @Fstmt, Transaction.Database.SQLDialect,
      FPXSQLDA_IN) <> 0 then
      FBLShowError(@Status_vector);
    SetLength(FParamAssigned, FParamcount);
    // alloc mem for params
    for i := 0 to FParamcount - 1 do
    begin
      FParamAssigned[i] := False;
      with FPXSQLDA_IN^.sqlvar[i] do
      begin
        case sqltype and (not 1) of
          SQL_TEXT, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP,
          SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT,
          SQL_LONG, SQL_INT64, SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
            FBLCalloc(sqldata, sqllen);
          SQL_VARYING:
            FBLCalloc(sqldata, sqllen + 2);
          else
            FBLError(E_QR_DATATYPE_UNKNOW);
        end;
        if (sqltype and 1 = 1) then // is nullable
          FBLCalloc(sqlind, SizeOf(Short))
        else if (sqlind <> nil) then
          FBLFree(sqlind);
      end;
    end;
  end
  else
    FBLFree(FPXSQLDA_IN);

  // output fields
  FBLCalloc(FPXSQLDA_OUT, XSQLDA_LENGTH(1));
  FPXSQLDA_OUT^.version := SQLDA_VERSION1;
  FPXSQLDA_OUT^.sqln := 1;
  if isc_dsql_describe(@Status_vector, @Fstmt, Transaction.Database.SQLDialect,
    FPXSQLDA_OUT) <> 0 then
    FBLShowError(@Status_vector);
  FFieldcount := FPXSQLDA_OUT^.SQLd;
  if FFieldcount > 0 then
  begin
    FBLCalloc(FPXSQLDA_OUT, XSQLDA_LENGTH(FFieldcount));
    FPXSQLDA_OUT^.sqln := FFieldcount;
    FPXSQLDA_OUT^.version := SQLDA_VERSION1;
    if isc_dsql_describe(@Status_vector, @Fstmt, Transaction.Database.SQLDialect,
      FPXSQLDA_OUT) <> 0 then
      FBLShowError(@Status_vector);
    //alloc memory for fields
    for i := 0 to FFieldcount - 1 do
    begin
      with FPXSQLDA_OUT^.sqlvar[i] do
      begin
        case sqltype and (not 1) of
          SQL_TEXT, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP,
          SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT,
          SQL_LONG, SQL_INT64, SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
            FBLCalloc(sqldata, sqllen);
          SQL_VARYING:
            FBLCalloc(sqldata, sqllen + 2);
          else
            FBLError(E_QR_DATATYPE_UNKNOW);
        end;
        if (sqltype and 1 = 1) then // is nullable
          FBLCalloc(sqlind, sizeof(Short))
        else if (sqlind <> nil) then
          FBLFree(sqlind);
        FFieldNames.Add(string(FPXSQLDA_OUT^.sqlvar[i].aliasname));
      end;
    end;
  end
  else
    FBLFree(FPXSQLDA_OUT);
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.FreeInOutXSQLDA;
var
  i: integer;
begin
  // free params
  for i := 0 to ParamCount - 1 do
  begin
    with FPXSQLDA_IN^.sqlvar[i] do
    begin
      FBLFree(sqldata);
      FBLFree(sqlind);
    end;
  end;

  // free fields
  for i := 0 to FieldCount - 1 do
  begin
    with FPXSQLDA_OUT^.sqlvar[i] do
    begin
      FBLFree(sqldata);
      FBLFree(sqlind);
    end;
  end;
  // free Pointers
  FBLFree(FPXSQLDA_IN);
  FBLFree(FPXSQLDA_OUT);
  FFieldNames.Clear;
  {$IFNDEF FPC}
  FParamAssigned := nil;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.ExecSQL;
var
  Status_vector: ISC_STATUS_VECTOR;
begin
  if not FPrepared then Prepare;
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    CheckAssignParams;
    case QueryType of
      qtSelect:
        begin
          isc_dsql_execute2(@Status_Vector, TRHandle, @FStmt,
            Transaction.Database.SQLDialect, FPXSQLDA_IN, nil);
          if (Status_vector[0] = 1) and (Status_vector[1] <> 0) then
            FBLShowError(@Status_vector);
          if isc_dsql_set_cursor_name(@Status_vector, @FStmt, PChar(FCursor), 0) <> 0 then
            FBLShowError(@Status_vector);
          FEOF := False;
          FBOF := True;
          FCursorOpen := True;
          FFetchCount := 0;
          if FAutoFetchFirst then Next;
        end;
      qtExecProcedure:
        begin
          if isc_dsql_execute2(@Status_Vector,
            TRHandle, @FStmt,
            Transaction.DataBase.SQLDialect,
            FPXSQLDA_IN,
            FPXSQLDA_OUT) <> 0 then FBLShowError(@Status_vector);
          if FFieldCount > 0 then    //procedure return values
            FEOF := False;
        end;
      else
        begin
          isc_dsql_execute(@Status_vector, TRHandle, @FStmt,
            Transaction.Database.SQLDialect, FPXSQLDA_IN);
          if (Status_vector[0] = 1) and (Status_vector[1] <> 0) then
            FBLShowError(@Status_vector);
        end;
    end;
    {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function TFBLDsql.GetEof: boolean;
begin
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
  if FcursorOpen then
    Result := FEOF
  else if (QueryType = qtExecProcedure) and (FieldCount > 0) then
    Result := FEOF
  else
    Result := True;
 {$IFDEF FBL_THREADSAFE}
 finally
   Unlock;
 end;
 {$ENDIF}
end;

//------------------------------------------------------------------------------

function TFBLDsql.GetFetchCount: integer;
begin
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    if FCursorOpen then
      Result := FFetchCount
    else if (QueryType = qtExecProcedure) and (FieldCount > 0) then
      Result := 1
    else
      Result := 0;
  {$IFDEF FBL_THREADSAFE}
  finally
   Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.First;
begin
  if FCursorOpen then
  begin
    if FetchCount = 0 then
    begin
      Next;
      Exit;
    end;
    if FetchCount = 1 then exit;
    if FetchCount > 1 then
    begin
      Close;
      ExecSQL;
      if not FAutoFetchFirst then Next;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.Next;
var
  fetch_res: ISC_STATUS;
  Status_Vector: ISC_STATUS_VECTOR;
begin
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
    {$ENDIF}
    FBOF := False;
    if (QueryType = qtExecProcedure) and (FieldCount > 0) then
      FEOF := True;
    if (MaxFetch > 0) and (FetchCount >= MaxFetch) then
      FEOF := True;

    if FCursorOpen and (not FEOF) then
    begin
      fetch_res := isc_dsql_fetch(@Status_vector, @FStmt,
        Transaction.Database.SqlDialect, FPXSQLDA_OUT);
      if (fetch_res <> 0) and (fetch_res <> 100) then
        FBLShowError(@Status_vector);

      if fetch_res = 100 then    // EOF
        FEOF := True
      else
      begin
        FEOF := False;
        Inc(FFetchCount);
      end;
    end;
    {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.Close;
var
  Status_Vector: ISC_STATUS_VECTOR;
begin
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    try
      if (FStmt <> nil) and (QueryType = qtSelect) and FCursorOpen then
        if isc_dsql_free_statement(@Status_vector, @Fstmt, DSQL_close) <> 0 then
          FBLShowError(@Status_vector);
    finally
      FCursorOpen := False;
      FEOF := True;
      FFetchCount := 0;
    end;
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function TFBLDsql.GetNumericValue(const AValue: int64; const AScale: integer): double;
var
  Scaling: double;
  Val: double;
begin
  Val := AValue;
  if AScale <> 0 then
  begin
    Scaling := IntPower(10,AScale);
    Result := Val * Scaling;
  end
  else
    Result := AValue;
end;

//------------------------------------------------------------------------------

function TFBLDsql.SetNumericValue(const AValue: double; const AScale: integer): int64;
var
  i: integer;
  Val, Scaling: double;
begin
  //val:=0;
  Scaling := 1;
  if AScale < 0 then
  begin
    Val := RoundTo(AValue, AScale);
    for i := -1 downto AScale do Scaling := Scaling * 10;
    //Result := Trunc(Val * Scaling);
    Result := Round(Val * Scaling);
  end
  else
    Result := Trunc(AValue);
end;

//------------------------------------------------------------------------------


function TFBLDsql.GetRowAffected: integer;
var
  Status_Vector: ISC_STATUS_VECTOR;
  Buffer: array[0..1048] of char;
  info_char: char;
begin
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    if not FPrepared then
      Result := -1
    else
    begin
      info_char := char(isc_info_sql_records);
      if isc_dsql_sql_info(@Status_vector, @FStmt, 1, @info_char,
        SizeOf(Buffer), Buffer) <> 0 then
        FBLShowError(@Status_vector);
      if Buffer[0] <> char(isc_info_sql_records) then
        FBLError(E_QR_INFO);

      case QueryType of
        qtUpdate: Result := isc_vax_integer(@Buffer[6], 4);
        qtDelete: Result := isc_vax_integer(@Buffer[13], 4);
        qtInsert: Result := isc_vax_integer(@Buffer[27], 4);
        else
          Result := -1;
      end;
    end;
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function TFBLDsql.GetPlan;
var
  Status_Vector: ISC_STATUS_VECTOR;
  Buffer: array[0..16384] of char;
  Len: integer;
  Info_char: char;
begin
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    Result := '';
    if not Prepared then Exit;
    case QueryType of
      qtSelect, qtUpdate, qtDelete, qtSelectForUpdate,
      qtExecProcedure:
        begin
          info_char := char(isc_info_sql_get_plan);
          isc_dsql_sql_info(@Status_vector, @FStmt, 1, //2
            @info_char, SizeOf(Buffer), Buffer);
          if (status_vector[0] = 1) and (status_vector[1] <> 0) then
            FBLShowError(@Status_vector);
          Len := isc_vax_integer(@buffer[1], 2);
          Setlength(Result, Len);
          Move(Buffer[3], Result[1], Len);
          Result := Trim(Result);
        end;
    end;
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}

end;

//------------------- Field routines -------------------------------------------

function TFBLDsql.FieldType(const AFieldIdx: integer): smallint;
begin
  CheckFieldIdx(AFIeldIDx);
  Result := FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqltype and (not 1);
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldSubType(const AFIeldIdx: integer): smallint;
begin
  CheckFieldIdx(AFieldIdx);
  Result := FPXSQLDA_OUT^.sqlvar[AFIeldIdx].sqlsubtype;
end;
//------------------------------------------------------------------------------

function TFBLDsql.FieldScale(const AFieldIdx: integer): smallint;
begin
  CheckFieldIdx(AFieldIdx);
  Result := FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlscale;
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldSize(const AFieldIdx: integer): smallint;
begin
  CheckFieldIdx(AFIeldIdx);
  Result := FPXSQLDA_OUT^.sqlvar[AFIeldIdx].sqllen;
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldAsXSQLVAR(const AFieldIdx: integer): TXSQLVAR;
begin
  CheckFieldIdx(AFieldIdx);
  Result := FPXSQLDA_OUT^.sqlvar[AFieldIdx];
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldIsNullable(const AFIeldIdx: integer): boolean;
begin
  CheckFieldIdx(AFieldIdx);
  Result := (FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqltype and 1 = 1);
end;

//---------------------------------------------------------------

function TFBLDsql.FieldIsNull(const AFieldIdx: integer): boolean;
var
  n: boolean;
begin
  CheckFieldIdx(AFieldIdx);
  n := FieldIsNullable(AFieldIdx);
  Result := n and
    (FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlind^ = -1);
end;

//---------------------------------------------------------------

function TFBLDsql.FieldName(const AFieldIdx: integer): string;
begin
  CheckFieldIdx(AFieldIdx);
  Result := string(FPXSQLDA_OUT^.sqlvar[AFieldIdx].aliasname);
end;

//---------------------------------------------------------------

function TFBLDsql.FieldRealName(const AFieldIdx: integer): string;
begin
  CheckFieldIdx(AFieldIdx);
  Result := string(FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlname);
end;

//----------------------------------------------------------------

function TFBLDsql.FieldTableName(const AFieldIdx: integer): string;
begin
  CheckFieldIdx(AFieldIdx);
  Result := string(FPXSQLDA_OUT^.sqlvar[AFieldIdx].relname);
end;

//---------------------------------------------------------------

function TFBLDsql.FieldTableOwerName(const AFieldIdx: integer): string;
begin
  CheckFieldIdx(AFieldIdx);
  Result := string(FPXSQLDA_OUT^.sqlvar[AFieldIdx].ownname);
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldSQLTypeDesc(const AFieldIdx: integer): string;
begin
  CheckFieldIdx(AFieldIdx);
  Result := SQLTypeDesc(FPXSQLDA_OUT^.sqlvar[AFieldIdx]);
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldAsString(const AFieldIdx: integer): string;
var
  DataPtr: PChar;  // pointer to xsqlda.sqldata
  l: integer;      // length of buffer
begin
  Result := '';
  if not FieldIsNull(AFieldIdx) then
  begin
    case FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqltype and (not 1) of
      SQL_ARRAY:
        Result := '(Array)';
      SQL_BLOB:
        begin
          if FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlsubtype = 1 then
            Result := BlobFieldAsString(AFieldIdx)
          else
            Result := '(Blob)';
        end;
      SQL_TEXT:
        begin
          DataPtr := FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqldata;
          l := FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqllen;
          SetString(Result, DataPtr, l);
        end;
      SQL_VARYING:
        begin
          DataPtr := FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqldata;
          l := isc_vax_integer(FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqldata, 2);
          Inc(DataPtr, 2);
          SetString(Result, DataPtr, l);
        end;
      SQL_SHORT,
      SQL_LONG:
        if FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlscale = 0 then
          Result := IntToStr(FieldAsLong(AFieldIdx))
        else
          Result := FloatToStr(FieldAsDouble(AFieldIdx));
        SQL_DOUBLE:
        Result := FloatToStr(FieldAsDouble(AFieldIdx));
      SQL_FLOAT,
      SQL_D_FLOAT:
        Result := FloatToStr(FieldAsFloat(AFieldIdx));
      SQL_TYPE_DATE:
        Result := DateToStr(FieldAsDatetime(AFieldIdx));
      SQL_TYPE_TIME:
        Result := TimeToStr(FieldAsDatetime(AFieldIdx));
      SQL_DATE:
        Result := DateTimeToStr(FieldAsDatetime(AFieldIdx));
      SQL_INT64:
        if FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlscale = 0 then
          Result := IntToStr(FieldAsInt64(AFieldIdx))
        else
          Result := FloatToStr(FieldAsDouble(AFieldIdx));
        else
          FBLError(E_QR_FIELD_CONV, [AFieldIdx]);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldAsLong(const AFieldIdx: integer): longint;
begin
  Result := 0;
  if not FieldIsNull(AFieldIdx) then
  begin
    case FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqltype and (not 1) of
      SQL_SHORT:
        begin
          if FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlscale = 0 then
            Result := Long(PShort(FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqldata)^)
          else
            Result := Long(Trunc(GetNumericValue(int64
              (PShort(FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqldata)^),
              FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlscale)));
        end;
      SQL_LONG:
        begin
          if FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlscale = 0 then
            Result := Long(PLong(FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqldata)^)
          else
            Result := Long(Trunc(GetNumericValue(int64
              (PLong(FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqldata)^),
              FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlscale)));
        end;
      else
        FBLError(E_QR_FIELD_CONV, [AFieldIdx]);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldAsInteger(const AFieldIdx: integer): integer;
begin
  Result := FieldAsLong(AFieldIdx);
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldAsDouble(const AFieldIdx: integer): double;
begin
  Result := 0;
  if not FieldIsNull(AFieldIdx) then
  begin
    case FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqltype and (not 1) of
      SQL_DOUBLE:
        begin
          if FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlscale = 0 then
            Result := PDouble(FPXSQLDA_OUT^.sqlvar[AFieldIdx].Sqldata)^
          else
            Result := RoundTo(PDouble(FPXSQLDA_OUT^.sqlvar[AFieldIdx].Sqldata)^,
              FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlscale)
        end;
      SQL_FLOAT, SQL_D_FLOAT:
        Result := FieldAsFloat(AFieldIdx);
      SQL_LONG:
        begin
          if FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlscale = 0 then
            Result := PLong(FPXSQLDA_OUT^.sqlvar[AFieldIdx].Sqldata)^
          else
            Result := GetNumericValue(int64(PLong(FPXSQLDA_OUT^.sqlvar[AFieldIdx].Sqldata)^),
              FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlscale);
        end;
      SQL_SHORT:
        begin
          if FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlscale = 0 then
            Result := PShort(FPXSQLDA_OUT^.sqlvar[AFieldIdx].Sqldata)^
          else
            Result := GetNumericValue(int64(PShort(FPXSQLDA_OUT^.sqlvar[AFieldIdx].Sqldata)^),
              FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlscale);
        end;
      SQL_INT64:
        begin
          if FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlscale = 0 then
            Result := PInt64(FPXSQLDA_OUT^.sqlvar[AFieldIdx].Sqldata)^
          else
            Result := GetNumericValue(PInt64(FPXSQLDA_OUT^.sqlvar[AFieldIdx].Sqldata)^,
              FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlscale);
        end;
      else
        FBLError(E_QR_FIELD_CONV, [AFieldIdx]);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldAsFloat(const AFieldIdx: integer): single;
begin
  Result := 0;
  case FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqltype and (not 1) of
    SQL_FLOAT, SQL_D_FLOAT:
      Result := PFloat(FPXSQLDA_OUT^.sqlvar[AFieldIdx].Sqldata)^;
    else
      begin
        try
          Result := FieldAsDouble(AFieldIdx);
        except
          on E: Exception do FBLError(E_QR_FIELD_CONV, [AFieldIdx]);
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldAsInt64(const AFieldIdx: integer): int64;
begin
  Result := 0;
  if not FieldIsNull(AFieldIdx) then
  begin
    if FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqlscale = 0 then
    begin
      case FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqltype and (not 1) of
        SQL_INT64:
          Result := PInt64(FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqldata)^;
        SQL_SHORT:
          Result := PShort(FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqldata)^;
        SQL_LONG:
          Result := PLong(FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqldata)^;
        else
          FBLError(E_QR_FIELD_CONV, [AFieldIdx]);
      end;
    end
    else
      FBLError(E_QR_FIELD_CONV, [AFieldIdx]);
  end;
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldAsDateTime(const AFieldIdx: integer): TDateTime;
var
  tm: TCTimeStructure;
begin
  Result := 0;
  if not FieldIsNull(AFieldIdx) then
  begin
    case FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqltype and (not 1) of
      SQL_TYPE_TIME:
        begin
          try
            isc_decode_sql_time(PISC_TIME(FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqldata), @tm);
            Result := EncodeTime(word(tm.tm_hour), word(tm.tm_min), word(tm.tm_sec), 0);
          except
            on E: EConvertError do FBLError(E_QR_FIELD_TIME_CONV, [AFieldIdx]);
          end;
        end;

      SQL_TYPE_DATE:
        begin
          try
            isc_decode_sql_date(PISC_DATE(FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqldata), @tm);
            Result := EncodeDate(word(tm.tm_year + 1900), word(tm.tm_mon + 1),
              word(tm.tm_mday));
          except
            on E: EConvertError do FBLError(E_QR_FIELD_DATE_CONV, [AFieldIdx]);
          end;
        end;

      SQL_TIMESTAMP:
        begin
          try
            isc_decode_timestamp(PISC_TIMESTAMP(FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqldata),
              @tm);
            Result := EncodeDatetime(word(tm.tm_year + 1900), word(tm.tm_mon + 1),
              word(tm.tm_mday),
              word(tm.tm_hour), word(tm.tm_min), word(tm.tm_sec), 0);
          except
            on E: EConvertError do FBLError(E_QR_FIELD_TIMESTAMP_CONV, [AFieldIdx]);
          end;
        end;
      else
        FBLError(E_QR_FIELD_CONV, [AFieldIdx]);
    end;
  end;
end;

// Field by Name ---------------------------------------------------------------

function TFBLDsql.FieldByNameAsXSQLVAR(const AFIeldName: string): TXSQLVAR;
var
  fpos: integer;
begin
  fpos := FFieldNames.IndexOf(AFieldName);
  if fPos < 0 then
    FBLError(E_QR_FIELD_NAME_NOT_EXISTS, [AFieldName])
  else
    Result := FieldAsXSQLVAR(fPos);
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldByNameAsString(const AFieldName: string): string;
var
  fpos: integer;
begin
  Result := '';
  fpos := FFieldNames.IndexOf(AFieldName);
  if fPos < 0 then
    FBLError(E_QR_FIELD_NAME_NOT_EXISTS, [AFieldName])
  else
    Result := FieldAsString(fPos);
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldByNameAsLong(const AFieldName: string): longint;
var
  fpos: integer;
begin
  Result := 0;
  fpos := FFieldNames.IndexOf(AFieldName);
  if fPos < 0 then
    FBLError(E_QR_FIELD_NAME_NOT_EXISTS, [AFieldName])
  else
    Result := FieldAsLong(fPos);
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldByNameAsInteger(const AFieldName: string): integer;
var
  fpos: integer;
begin
  Result := 0;
  fpos := FFieldNames.IndexOf(AFieldName);
  if fPos < 0 then
    FBLError(E_QR_FIELD_NAME_NOT_EXISTS, [AFieldName])
  else
    Result := FieldAsInteger(fPos);
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldByNameAsDouble(const AFieldName: string): double;
var
  fpos: integer;
begin
  Result := 0;
  fpos := FFieldNames.IndexOf(AFieldName);
  if fPos < 0 then
    FBLError(E_QR_FIELD_NAME_NOT_EXISTS, [AFieldName])
  else
    Result := FieldAsDouble(fPos);
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldByNameAsFloat(const AFieldName: string): single;
var
  fpos: integer;
begin
  Result := 0;
  fpos := FFieldNames.IndexOf(AFieldName);
  if fPos < 0 then
    FBLError(E_QR_FIELD_NAME_NOT_EXISTS, [AFieldName])
  else
    Result := FieldAsFloat(fPos);
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldByNameAsInt64(const AFieldName: string): int64;
var
  fpos: integer;
begin
  Result := 0;
  fpos := FFieldNames.IndexOf(AFieldName);
  if fPos < 0 then
    FBLError(E_QR_FIELD_NAME_NOT_EXISTS, [AFieldName])
  else
    Result := FieldAsInt64(fPos);
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldByNameAsDateTime(const AFieldName: string): TDateTime;
var
  fpos: integer;
begin
  Result := 0;
  fpos := FFieldNames.IndexOf(AFieldName);
  if fPos < 0 then
    FBLError(E_QR_FIELD_NAME_NOT_EXISTS, [AFieldName])
  else
    Result := FieldAsDateTime(fPos);
end;

//------------------------------------------------------------------------------

function TFBLDsql.FieldByNameIsNull(const AFieldName: string): boolean;
var
  fPos: integer;
begin
  Result := False;
  fpos := FFieldNames.IndexOf(AFieldName);
  if fPos < 0 then
    FBLError(E_QR_FIELD_NAME_NOT_EXISTS, [AFieldName])
  else
    Result := FieldIsNull(fPos);
end;

//------------------------------------------------------------------------------

function TFBLDsql.BlobFieldByNameAsString(const AFieldName: string): string;
var
  fpos: integer;
begin
  Result := '';
  fpos := FFieldNames.IndexOf(AFieldName);
  if fPos < 0 then
    FBLError(E_QR_FIELD_NAME_NOT_EXISTS, [AFieldName])
  else
    Result := BlobFieldAsString(fPos);
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.BlobFieldByNameSaveToStream(const AFieldName: string;
  AStream: TStream);
var
  fpos: integer;
begin
  fpos := FFieldNames.IndexOf(AFieldName);
  if fPos < 0 then
    FBLError(E_QR_FIELD_NAME_NOT_EXISTS, [AFieldName])
  else
    BlobFieldSaveToStream(fPos, AStream);
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.BlobFieldByNameSaveToFile(const AFieldName: string;
  const AFileName: string);
var
  fpos: integer;
begin
  fpos := FFieldNames.IndexOf(AFieldName);
  if fPos < 0 then
    FBLError(E_QR_FIELD_NAME_NOT_EXISTS, [AFieldName])
  else
    BlobFieldSaveToFile(fPos, AFileName);
end;

//---------------------- Param Rutines -----------------------------------------

function TFBLDsql.ParamIsNullable(const AParamIdx: integer): boolean;
begin
  CheckParamIdx(AParamIdx);
  Result := (FPXSQLDA_IN^.sqlvar[AParamIdx].sqltype and 1 = 1);
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.ParamAsNull(const AParamIdx: integer);
begin
  if ParamIsNullable(AParamIdx) then
    FPXSQLDA_in^.sqlvar[AParamIdx].sqlind^ := -1
  else
    FBLError(E_QR_PARAM_NOTNULL, [AParamIdx]);
  FParamAssigned[AParamIdx] := True;
end;

//------------------------------------------------------------------------------

function TFBLDsql.ParamType(const AParamIdx: integer): smallint;
begin
  CheckParamIdx(AParamIdx);
  Result := FPXSQLDA_IN^.sqlvar[AParamIdx].sqltype and (not 1);
end;
//------------------------------------------------------------------------------

function TFBLDsql.ParamSubType(const AParamIdx: integer): smallint;
begin
  CheckParamIdx(AParamIdx);
  Result := FPXSQLDA_IN^.sqlvar[AParamIdx].sqlsubtype;
end;

//------------------------------------------------------------------------------

function TFBLDsql.ParamScale(const AParamIdx: integer): smallint;
begin
  CheckParamIdx(AParamIdx);
  Result := FPXSQLDA_IN^.sqlvar[AParamIdx].sqlscale;
end;

//------------------------------------------------------------------------------

function TFBLDsql.ParamSize(const AParamIdx: integer): smallint;
begin
  CheckParamIdx(AParamIdx);
  Result := FPXSQLDA_IN^.sqlvar[AParamIdx].sqllen;
end;

//------------------------------------------------------------------------------

function TFBLDsql.ParamSQLTypeDesc(const AParamIdx: integer): string;
begin
  CheckParamIdx(AParamIdx);
  Result := SQLTypeDesc(FPXSQLDA_IN^.sqlvar[AParamIdx]);
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.ParamAsShort(const AParamIdx: integer; AValue: smallint);
begin
  ParamAsInt64(AParamIdx, AValue);
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.ParamAsLong(const AParamIdx: integer; AValue: longint);
begin
  ParamAsInt64(AParamIdx, AValue);
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.RemoveFlagNull(const AParamIdx: integer);
begin
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
  if (FPXSQLDA_IN^.sqlvar[AParamIdx].sqltype and 1 = 1) then // Param is nullable
    if FPXSQLDA_in^.sqlvar[AParamIdx].sqlind^ = -1 then
      FPXSQLDA_in^.sqlvar[AParamIdx].sqlind^ := 0;
   {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.ParamAsInt64(const AParamIdx: integer; AValue: int64);
var
  v: int64;
begin
  CheckParamIdx(AParamIdx);
  RemoveFlagNull(AParamIdx);
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
  case FPXSQLDA_IN^.sqlvar[AParamIdx].sqltype and (not 1) of
    SQL_SHORT:
      begin
        v := SetNumericValue(AValue, FPXSQLDA_IN^.sqlvar[AParamIdx].sqlscale);
        if (v > Low(Short)) and (v < High(Short)) then
          PShort(FPXSQLDA_IN^.sqlvar[AParamIdx].sqldata)^ := Short(v)
        else
          FBLError(E_QR_PARAM_SHORT_OVERFLOW, [AParamIdx]);
      end;
    SQL_LONG:
      begin
        v := SetNumericValue(AValue, FPXSQLDA_IN^.sqlvar[AParamIdx].sqlscale);
        if (v > Low(Long)) and (v < High(Long)) then
          PLong(FPXSQLDA_IN^.sqlvar[AParamIdx].sqldata)^ := Long(v)
        else
          FBLError(E_QR_PARAM_LONG_OVERFLOW, [AParamIdx]);
      end;
    SQL_INT64:
      begin
        v := SetNumericValue(AValue, FPXSQLDA_IN^.sqlvar[AParamIdx].sqlscale);
        if (v > Low(int64)) and (v < High(int64)) then
          PInt64(FPXSQLDA_IN^.sqlvar[AParamIdx].sqldata)^ := v
        else
          FBLError(E_QR_PARAM_INT64_OVERFLOW, [AParamIdx]);
      end;
    else
      FBLError(E_QR_PARAM_TYPE, [AParamIdx])
  end;
  FParamAssigned[AParamIdx] := True;
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.ParamAsString(const AParamIdx: integer; const AValue: string);
var
  len_value: integer;
  PvChar: PISC_VARYING;
  NumValue: double;
begin
  Pvchar := nil;
  CheckParamIdx(AParamIdx);
  RemoveFlagNull(AParamIdx);
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    len_value := Length(AValue);

    if len_value > FPXSQLDA_IN^.sqlvar[AParamIdx].sqllen then
      len_value := FPXSQLDA_IN^.sqlvar[AParamIdx].sqllen;

    case FPXSQLDA_IN^.sqlvar[AParamIdx].sqltype and (not 1) of
    SQL_TEXT:
      begin
        if len_value < FPXSQLDA_IN^.sqlvar[AParamIdx].sqllen then
          FillChar(FPXSQLDA_IN^.sqlvar[AParamIdx].sqldata^,FPXSQLDA_IN^.sqlvar[AParamIdx].sqllen,' ');
        if len_value > 0 then
          Move(AValue[1], FPXSQLDA_IN^.sqlvar[AParamIdx].sqldata^,len_value);
      end;
    SQL_VARYING:
      begin
        FBLCalloc(PvChar, FPXSQLDA_IN^.sqlvar[AParamIdx].sqllen + 2);
        try
          if len_value > 0 then
          begin
            Pvchar^.strlen := Short(len_value);
            Move(AValue[1], Pvchar^.str, len_value);
          end;
          Move(Pvchar^, FPXSQLDA_IN^.sqlvar[AParamIdx].sqldata^,
            FPXSQLDA_IN^.sqlvar[AParamIdx].sqllen + 2);
        finally
          FBLFree(PvChar);
        end;
      end;

    SQL_BLOB:
      begin
        if FPXSQLDA_IN^.sqlvar[AParamIdx].sqlsubtype = 1 then
          BlobParamAsString(AParamIdx, AValue);
      end;
    SQL_TYPE_TIME:
      begin
        try
          ParamAsDateTime(AParamIdx, StrToTime(AValue));
        except
          on E: Exception do FBLError(E_QR_PARAM_TIME_CONV, [AParamIdx]);
        end;
      end;
    SQL_TYPE_DATE:
      begin
        try
          ParamAsDateTime(AParamIdx, StrToDate(AValue));
        except
          on E: Exception do FBLError(E_QR_PARAM_DATE_CONV, [AParamIdx]);
        end;
      end;
    SQL_TIMESTAMP:
      begin
        try
          ParamAsDateTime(AParamIdx, StrToDateTime(AValue));
        except
          on E: Exception do FBLError(E_QR_PARAM_TIME_CONV, [AParamIdx]);
        end;
      end;
    SQL_SHORT, SQL_LONG, SQL_INT64, SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
      begin
        try
          NumValue := StrToFloat(AValue);
          ParamAsDouble(AParamIdx, NumValue);
        except
          on E: Exception do FBLError(E_QR_PARAM_CONV, [AParamIdx]);
        end;
      end;
    else
      FBLError(E_QR_PARAM_TYPE, [AParamIdx]);
  end;
    FParamAssigned[AParamIdx] := True;
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.ParamAsDouble(const AParamIdx: integer; AValue: double);
var
  v: int64;
begin
  CheckParamIdx(AParamIdx);
  RemoveFlagNull(AParamIdx);
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
  case FPXSQLDA_IN^.sqlvar[AParamIdx].sqltype and (not 1) of
    SQL_SHORT:
      begin
        v := SetNumericValue(AValue, FPXSQLDA_IN^.sqlvar[AParamIdx].sqlscale);
        if (v > Low(Short)) and (v < High(Short)) then
          PShort(FPXSQLDA_IN^.sqlvar[AParamIdx].sqldata)^ := Short(v)
        else
          FBLError(E_QR_PARAM_SHORT_OVERFLOW, [AParamIdx]);
      end;
    SQL_LONG:
      begin
        v := SetNumericValue(AValue, FPXSQLDA_IN^.sqlvar[AParamIdx].sqlscale);
        if (v > Low(Long)) and (v < High(Long)) then
          PLong(FPXSQLDA_IN^.sqlvar[AParamIdx].sqldata)^ := Long(v)
        else
          FBLError(E_QR_PARAM_LONG_OVERFLOW, [AParamIdx]);
      end;
    SQL_INT64:
      begin
        v := SetNumericValue(AValue, FPXSQLDA_IN^.sqlvar[AParamIdx].sqlscale);
        if (v > Low(int64)) and (v < High(int64)) then
          PInt64(FPXSQLDA_IN^.sqlvar[AParamIdx].sqldata)^ := v
        else
          FBLError(E_QR_PARAM_INT64_OVERFLOW, [AParamIdx]);
      end;

    SQL_DOUBLE:
      PDouble(FPXSQLDA_IN^.sqlvar[AParamIdx].sqldata)^ := AValue;

    SQL_FLOAT, SQL_D_FLOAT:
      ParamAsFloat(AParamIdx, AValue);
    else
      FBLError(E_QR_PARAM_TYPE, [AParamIdx]);
  end;
  FParamAssigned[AParamIdx] := True;
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;


//------------------------------------------------------------------------------

procedure TFBLDsql.ParamAsFloat(const AParamIdx: integer; AValue: single);
begin
  CheckParamIdx(AParamIdx);
  RemoveFlagNull(AParamIdx);
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
  case FPXSQLDA_IN^.sqlvar[AParamIdx].sqltype and (not 1) of
    SQL_FLOAT, SQL_D_FLOAT:
      PSingle(FPXSQLDA_IN^.sqlvar[AParamIdx].sqldata)^ := AValue;
    SQL_SHORT, SQL_INT64, SQL_LONG,
    SQL_DOUBLE:
      ParamAsDouble(AParamIdx, AValue);
    else
      FBLError(E_QR_PARAM_TYPE, [AParamIdx]);
  end;
  FParamAssigned[AParamIdx] := True;
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.ParamAsDateTime(const AParamIdx: integer; AValue: TDateTime);
var
  tm: TCTimeStructure;
  yy, mm, dd, h, m, s, ms: word;
begin
  CheckParamIdx(AParamIdx);
  RemoveFlagNull(AParamIdx);
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    case FPXSQLDA_IN^.sqlvar[AParamIdx].sqltype and (not 1) of
      SQL_TYPE_TIME:
        begin
          DecodeTime(AValue, h, m, s, ms);
          tm.tm_year := 0;
          tm.tm_mon := 0;
          tm.tm_mday := 0;
          tm.tm_hour := h;
          tm.tm_min := m;
          tm.tm_sec := s;
          isc_encode_sql_time(@tm, PISC_TIME(FPXSQLDA_IN^.sqlvar[AParamIdx].sqldata));
        end;
      SQL_TYPE_DATE:
        begin
          DecodeDate(AValue, yy, mm, dd);
          tm.tm_year := yy - 1900;
          tm.tm_mon := mm - 1;
          tm.tm_mday := dd;
          tm.tm_hour := 0;
          tm.tm_min := 0;
          tm.tm_sec := 0;
          isc_encode_sql_date(@tm, PISC_DATE(FPXSQLDA_IN^.sqlvar[AParamIdx].sqldata));
        end;
      SQL_TIMESTAMP:
        begin
          DecodeDateTime(AValue, yy, mm, dd, h, m, s, ms);
          tm.tm_year := yy - 1900;
          tm.tm_mon := mm - 1;
          tm.tm_mday := dd;
          tm.tm_hour := h;
          tm.tm_min := m;
          tm.tm_sec := s;
          isc_encode_timestamp(@tm, PISC_TIMESTAMP(FPXSQLDA_IN^.sqlvar[AParamIdx].sqldata));
        end;
      else
        FBLError(E_QR_PARAM_TYPE, [AParamIdx]);
    end;
    FParamAssigned[AParamIdx] := True;
    {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------
// BLOB Routines
//------------------------------------------------------------------------------

function TFBLDsql.BlobFieldAsString(const AFieldIdx: integer): string;
var
  StringStream: TStringStream;
begin
  Result := '';
  StringStream := TStringStream.Create('');
  try
    BlobFieldSaveToStream(AFieldIdx, StringStream);
    Result := StringStream.DataString;
  finally
    StringStream.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.BlobFieldSaveToStream(const AFieldIdx: integer; AStream: TStream);
var
  Status_Vector: ISC_STATUS_VECTOR;
  Blob_Handle: TISC_BLOB_HANDLE;
  Blob_stat: ISC_STATUS;
  actual_seg_len: UShort;
  Buffer: array [0..BLOB_SEGMENT_LEN] of char;
  TotLen: integer;
begin
    Blob_Handle := nil;
    CheckFieldIdx(AFieldIdx);
    if (FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqltype and (not 1)) = SQL_BLOB then
    begin
      isc_open_blob2(@Status_vector, DBHandle, TRHandle, @Blob_Handle,
        PISC_QUAD(FPXSQLDA_OUT^.sqlvar[AFieldIdx].sqldata), 0,nil);
      if (status_vector[0] = 1) and (Status_vector[1] <> 0) then
        FBLShowError(@Status_vector);
      if Assigned(AStream) then
      begin
        TotLen := FBLMixf.BlobSize(@Blob_handle);
        if TotLen > 0 then
        begin
          Blob_stat := isc_get_segment(@status_vector, @Blob_handle,
            @actual_seg_len, SizeOf(Buffer), Buffer);
          AStream.WriteBuffer(Buffer, actual_seg_len);
          while (blob_stat = 0) or (status_vector[1] = isc_segment) do
          begin
            blob_stat := isc_get_segment(@status_vector, @Blob_Handle,
              @actual_seg_len, SizeOf(Buffer), Buffer);
            AStream.WriteBuffer(Buffer, actual_seg_len);
          end;
        end;
      end;
      isc_close_blob(@status_vector, @Blob_Handle);
      if (Status_vector[0] = 1) and (Status_vector[1] <> 0) then
        FBLShowError(@Status_vector);
    end
    else
      FBLError(E_QR_FIELD_NOT_BLOB, [AFieldIdx]);
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.BlobParamAsString(const AParamIdx: integer; const AValue: string);
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create(AValue);
  try
    BlobParamLoadFromStream(AParamIdx, StringStream);
  finally
    StringStream.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.BlobFieldSaveToFile(const AFieldIdx: integer;
  const AFileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFileName, fmCreate);
  try
    BlobFieldSaveToStream(AFieldIdx, FileStream);
  finally
    FileStream.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.BlobParamLoadFromStream(const AParamIdx: integer; AStream: TStream);
var
  Status_Vector: ISC_STATUS_VECTOR;
  Blob_Handle: TISC_BLOB_HANDLE;
  Buffer: array [0..BLOB_SEGMENT_LEN] of char;
  TotLen, Len: cardinal;
begin
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
    {$ENDIF}
    CheckParamIdx(AParamIdx);
    Blob_Handle := nil;
    RemoveFlagNull(AParamIdx);
    if (FPXSQLDA_IN^.sqlvar[AParamIdx].sqltype and (not 1)) = SQL_BLOB then
    begin
      if Assigned(AStream) then
      begin
        TotLen := cardinal(AStream.Size);
        AStream.Seek(0,soFromBeginning);
        isc_create_blob2(@Status_Vector, DBHAndle, TRHandle, @Blob_Handle,
          PISC_QUAD(FPXSQLDA_IN^.sqlvar[AParamIdx].sqldata), 0,nil);
        if (Status_vector[0] = 1) and (Status_vector[1] <> 0) then
          FBLShowError(@Status_vector);
        repeat
          if TotLen > SizeOf(Buffer) then
            Len := SizeOf(Buffer)
          else
            Len := TotLen;
          AStream.ReadBuffer(Buffer, Len);
          isc_put_segment(@Status_vector, @blob_handle, UShort(Len), Buffer);
          if (Status_vector[0] = 1) and (Status_vector[1] <> 0) then
            FBLShowError(@Status_vector);
          Dec(TotLen, Len);
        until TotLen = 0;
        if Assigned(Blob_Handle) then
        begin
          isc_close_blob(@status_vector, @Blob_handle);
          if (Status_vector[0] = 1) and (Status_vector[1] <> 0) then
            FBLShowError(@Status_vector);
        end;
        FParamAssigned[AParamIdx] := True;
      end;
    end
    else
      FBLError(E_QR_PARAM_NOT_BLOB, [AParamIdx]);
    {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TFBLDsql.BlobParamLoadFromFile(const AParamIdx: integer;
  const AFileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    BlobParamLoadFromStream(AParamIdx, FileStream);
  finally
    FileStream.Free;
  end;
end;

//-----------------EOF----------------------------------------------------------

end.
