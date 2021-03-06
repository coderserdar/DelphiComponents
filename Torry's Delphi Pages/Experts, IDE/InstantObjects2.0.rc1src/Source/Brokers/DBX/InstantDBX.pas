(*
 *   InstantObjects
 *   dbExpress Support
 *)

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
 * The Original Code is: Seleqt InstantObjects
 *
 * The Initial Developer of the Original Code is: Seleqt
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Carlo Barazzetta, Adrea Petrelli, Nando Dessena
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantDBX;

interface

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  Controls,
{$ENDIF}
{$IFDEF LINUX}
  QControls,
{$ENDIF}
  Classes, DB, DBXpress, SqlExpr, InstantPersistence, InstantCommand;

type
  TInstantDBXConnectionDef = class(TInstantConnectionBasedConnectionDef)
  private
    FConnectionName: string;
    FDriverName: string;
    FGetDriverFunc: string;
    FLibraryName: string;
    FParams: string; //CB converted from TStrings for xml streaming compatibility
    FVendorLib: string;
  protected
    function CreateConnection(AOwner: TComponent): TCustomConnection; override;
  public
    function Edit: Boolean; override;
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
  published
    property ConnectionName: string read FConnectionName write FConnectionName;
    property DriverName: string read FDriverName write FDriverName;
    property GetDriverFunc: string read FGetDriverFunc write FGetDriverFunc;
    property LibraryName: string read FLibraryName write FLibraryName;
    property Params: string read FParams write FParams;
    property VendorLib: string read FVendorLib write FVendorLib;
  end;

  TInstantDBXConnector = class(TInstantConnectionBasedConnector)
  private
    FTransactionDesc: TTransactionDesc;
    FOnLogin: TSQLConnectionLoginEvent;
    function GetConnection: TSQLConnection;
    procedure SetConnection(Value: TSQLConnection);
    function GetCanTransaction: Boolean;
  protected
    procedure AssignLoginOptions; override;//CB
    function CreateBroker: TInstantBroker; override;
    procedure InitTransactionDesc(var ATransactionDesc: TTransactionDesc); virtual;
    procedure InternalCommitTransaction; override;
    procedure InternalRollbackTransaction; override;
    procedure InternalStartTransaction; override;
    function ParamByName(const AName: string): string;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); override;
  public
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
    property CanTransaction: Boolean read GetCanTransaction;
  published
    property Connection: TSQLConnection read GetConnection write SetConnection;
    property OnLogin: TSQLConnectionLoginEvent read FOnLogin write FOnLogin;
  end;

  TInstantDBXBroker = class(TInstantSQLBroker)
  private
    function GetConnector: TInstantDBXConnector;
  protected
    procedure AssignParam(SourceParam, TargetParam: TParam); virtual;
    procedure AssignParams(SourceParams, TargetParams: TParams);
    function ColumnTypeByDataType(DataType: TInstantDataType): string; virtual; abstract;
    function CreateResolver(Map: TInstantAttributeMap): TInstantSQLResolver; override;
    function GetDatabaseName: string; override;
    function InternalCreateQuery: TInstantQuery; override;
    procedure AssignDataSetParams(DataSet : TDataSet; AParams: TParams); override;
  public
    function CreateDBBuildCommand(
      const CommandType: TInstantDBBuildCommandType): TInstantDBBuildCommand; override;
    function CreateDataSet(const AStatement: string; AParams: TParams = nil): TDataSet; override;
    function DataTypeToColumnType(DataType: TInstantDataType; Size: Integer): string; override;
    function Execute(const AStatement: string; AParams: TParams = nil): Integer; override;
    property Connector: TInstantDBXConnector read GetConnector;
  end;

  TInstantDBXResolver = class(TInstantSQLResolver)
  protected
    function ReadBooleanField(DataSet: TDataSet; const FieldName: string): Boolean; override;
  end;

  TInstantDBXTranslator = class(TInstantRelationalTranslator)
  protected
    function TranslateConstant(Constant: TInstantIQLConstant; Writer: TInstantIQLWriter): Boolean; override;
  end;

  TInstantDBXQuery = class(TInstantSQLQuery)
  protected
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
  end;

  { InterBase / Firebird (abstract) }

  TInstantDBXInterBaseFirebirdBroker = class(TInstantDBXBroker)
  private
    function GetDialect: Integer;
  protected
    function ColumnTypeByDataType(DataType: TInstantDataType): string; override;
    function CreateCatalog(const AScheme: TInstantScheme): TInstantCatalog; override;
    function GetSQLQuote: Char; override;
  public
    property Dialect: Integer read GetDialect;
  end;

  { InterBase }

  TInstantDBXInterBaseBroker = class(TInstantDBXInterBaseFirebirdBroker)
  protected
    function GetDBMSName: string; override;
  end;

  { Firebird through the UIB dbX driver }

  TInstantDBXFirebirdUIBBroker = class(TInstantDBXInterBaseFirebirdBroker)
  protected
    function GetDBMSName: string; override;
  end;

  { MS SQL Server }

  TInstantDBXMSSQLBroker = class(TInstantDBXBroker)
  protected
    function CreateCatalog(const AScheme: TInstantScheme): TInstantCatalog; override;
    procedure AssignParam(SourceParam, TargetParam: TParam); override;
    function CreateResolver(Map: TInstantAttributeMap): TInstantSQLResolver; override;
    function ColumnTypeByDataType(DataType: TInstantDataType): string; override;
    function GetDBMSName: string; override;
    function GetSQLQuote: Char; override;
    function InternalCreateQuery: TInstantQuery; override;
  end;

  TInstantDBXMSSQLResolver = class(TInstantSQLResolver)
  end;

  TInstantDBXMSSQLQuery = class(TInstantSQLQuery)
  end;

  { Oracle }

  TInstantDBXOracleBroker = class(TInstantDBXBroker)
  protected
    procedure AssignParam(SourceParam, TargetParam: TParam); override;
    function ColumnTypeByDataType(DataType: TInstantDataType): string; override;
    function GetDBMSName: string; override;
    function GetSQLQuote: Char; override;
  end;

  { IBM DB2 }

  TInstantDBXDB2Broker = class(TInstantDBXBroker)
  protected
    function ColumnTypeByDataType(DataType: TInstantDataType): string; override;
    function GetDBMSName: string; override;
    function GetSQLQuote: Char; override;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); override;
  end;

  { MySQL }

  TInstantDBXMySQLBroker = class(TInstantDBXBroker)
  protected
    function ColumnTypeByDataType(DataType: TInstantDataType): string; override;
    function GetDBMSName: string; override;
    function GetSQLQuote: Char; override;
  end;

implementation

uses
  SysUtils, InstantDBXConnectionDefEdit, InstantUtils, InstantConsts, Math,
  InstantDBBuild, InstantIBFbCatalog, InstantMSSqlCatalog;

{ TInstantDBXConnector }

procedure TInstantDBXConnector.AssignLoginOptions;
begin
  inherited;
  if HasConnection then
  begin
    if Assigned(FOnLogin) and not Assigned(Connection.OnLogin) then
      Connection.OnLogin := FOnLogin;
  end;
end;

class function TInstantDBXConnector.ConnectionDefClass: TInstantConnectionDefClass;
begin
  Result := TInstantDBXConnectionDef;
end;

function TInstantDBXConnector.CreateBroker: TInstantBroker;
begin
  if SameText(Connection.DriverName, 'INTERBASE') then
    Result := TInstantDBXInterBaseBroker.Create(Self)
  else if SameText(Connection.DriverName, 'MSSQL') then
    Result := TInstantDBXMSSQLBroker.Create(Self)
  else if SameText(Connection.DriverName, 'SQLServer') then //For CoreLab driver
    Result := TInstantDBXMSSQLBroker.Create(Self)
  else if SameText(Connection.DriverName, 'Oracle') then
    Result := TInstantDBXOracleBroker.Create(Self)
  else if SameText(Connection.DriverName, 'Oracle (Core Lab)') then //For CoreLab driver
    Result := TInstantDBXOracleBroker.Create(Self)
  else if SameText(Connection.DriverName, 'Oracle Net (Core Lab)') then //For CoreLab driver
    Result := TInstantDBXOracleBroker.Create(Self)
  else if SameText(Connection.DriverName, 'DB2') then
    Result := TInstantDBXDB2Broker.Create(Self)
  else if SameText(Connection.DriverName, 'MySQL') then
    Result := TInstantDBXMySQLBroker.Create(Self)
  else if SameText(Connection.DriverName, 'FirebirdUIB') then
    Result := TInstantDBXFirebirdUIBBroker.Create(Self)
  else
    raise Exception.CreateFmt('dbExpress driver "%s" not supported',
      [Connection.DriverName]);
end;

function TInstantDBXConnector.GetCanTransaction: Boolean;
begin
  Result := Connection.TransactionsSupported;
end;

function TInstantDBXConnector.GetConnection: TSQLConnection;
begin
  Result := inherited Connection as TSQLConnection;
end;

procedure TInstantDBXConnector.InitTransactionDesc(
  var ATransactionDesc: TTransactionDesc);
begin
  ATransactionDesc.TransactionID := 1;
  ATransactionDesc.GlobalID := 0;
end;

procedure TInstantDBXConnector.InternalBuildDatabase(Scheme: TInstantScheme);
begin
  inherited;
end;

procedure TInstantDBXConnector.InternalCommitTransaction;
begin
  if CanTransaction then
    Connection.Commit(FTransactionDesc);
end;

procedure TInstantDBXConnector.InternalRollbackTransaction;
begin
  if CanTransaction then
    Connection.Rollback(FTransactionDesc);
end;

procedure TInstantDBXConnector.InternalStartTransaction;
begin
  if CanTransaction then
  begin
    InitTransactionDesc(FTransactionDesc);
    Connection.StartTransaction(FTransactionDesc);
  end;
end;

function TInstantDBXConnector.ParamByName(const AName: string): string;
begin
  Result := Connection.Params.Values[AName];
end;

procedure TInstantDBXConnector.SetConnection(Value: TSQLConnection);
begin
  inherited Connection := Value;
end;

{ TInstantDBXConnectionDef }

class function TInstantDBXConnectionDef.ConnectionTypeName: string;
begin
  Result := 'dbExpress';
end;

class function TInstantDBXConnectionDef.ConnectorClass: TInstantConnectorClass;
begin
  Result := TInstantDBXConnector;
end;

function TInstantDBXConnectionDef.CreateConnection(
  AOwner: TComponent): TCustomConnection;
var
  Connection: TSQLConnection;
begin
  Connection := TSQLConnection.Create(AOwner);
  try
    Connection.ConnectionName := ConnectionName;
    Connection.DriverName := DriverName;
    Connection.Params.Text := Params;
    Connection.LibraryName := LibraryName;
    Connection.VendorLib := VendorLib;
    Connection.GetDriverFunc := GetDriverFunc;
  except
    Connection.Free;
    raise;
  end;
  Result := Connection;
end;

function TInstantDBXConnectionDef.Edit: Boolean;
begin
  with TInstantDBXConnectionDefEditForm.Create(nil) do
  try
    LoadData(Self);
    Result := ShowModal = mrOk;
    if Result then
      SaveData(Self);
  finally
    Free;
  end;
end;

{ TInstantDBXBroker }

procedure TInstantDBXBroker.AssignDataSetParams(DataSet: TDataSet;
  AParams: TParams);
begin
  //don't call inherited!
  AssignParams(AParams, TSQLQuery(DataSet).Params);
end;

procedure TInstantDBXBroker.AssignParam(SourceParam, TargetParam: TParam);
begin
  case SourceParam.DataType of
    ftBoolean:
      TargetParam.AsInteger := Integer(SourceParam.AsBoolean);
    ftDateTime:
      begin
        TargetParam.DataType := ftTimeStamp;
        TargetParam.Value := SourceParam.AsDateTime;
      end;
    ftCurrency:
      begin
        TargetParam.DataType := ftBCD;
        TargetParam.Value := SourceParam.AsCurrency;
      end;
  else
    TargetParam.Assign(SourceParam);
  end;
end;

procedure TInstantDBXBroker.AssignParams(SourceParams, TargetParams: TParams);
var
  I: Integer;
  SourceParam, TargetParam: TParam;
begin
  for I := 0 to Pred(SourceParams.Count) do
  begin
    SourceParam := SourceParams[I];
    TargetParam := TargetParams.FindParam(SourceParam.Name);
    if Assigned(TargetParam) then
      AssignParam(SourceParam, TargetParam);
  end;
end;

function TInstantDBXBroker.CreateDataSet(const AStatement: string;
  AParams: TParams): TDataSet;
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  with Query do
  begin
    SQLConnection := Connector.Connection;
    SQL.Text := AStatement;
    if Assigned(AParams) then
      AssignParams(AParams, Params);
{$IFNDEF D9+}
    NoMetadata := True;
{$ENDIF}
  end;
  Result := Query;
end;

function TInstantDBXBroker.CreateDBBuildCommand(
  const CommandType: TInstantDBBuildCommandType): TInstantDBBuildCommand;
begin
  if CommandType = ctAddTable then
    Result := TInstantDBBuildAddTableSQLCommand.Create(CommandType, Connector)
  else if CommandType = ctDropTable then
    Result := TInstantDBBuildDropTableSQLCommand.Create(CommandType, Connector)
  else if CommandType = ctAddField then
    Result := TInstantDBBuildAddFieldSQLCommand.Create(CommandType, Connector)
  else if CommandType = ctAlterField then
    Result := TInstantDBBuildAlterFieldSQLCommand.Create(CommandType, Connector)
  else if CommandType = ctDropField then
    Result := TInstantDBBuildDropFieldSQLCommand.Create(CommandType, Connector)
  else if CommandType = ctAddIndex then
    Result := TInstantDBBuildAddIndexSQLCommand.Create(CommandType, Connector)
  else if CommandType = ctAlterIndex then
    Result := TInstantDBBuildAlterIndexSQLCommand.Create(CommandType, Connector)
  else if CommandType = ctDropIndex then
    Result := TInstantDBBuildDropIndexSQLCommand.Create(CommandType, Connector)
  else
    Result := inherited CreateDBBuildCommand(CommandType);
end;

function TInstantDBXBroker.CreateResolver(
  Map: TInstantAttributeMap): TInstantSQLResolver;
begin
  Result := TInstantDBXResolver.Create(Self, Map);
end;

function TInstantDBXBroker.DataTypeToColumnType(DataType: TInstantDataType;
  Size: Integer): string;
begin
  Result := ColumnTypeByDataType(DataType);
  if (DataType = dtString) and (Size > 0) then
    Result := Result + InstantEmbrace(IntToStr(Size), '()');
end;

function TInstantDBXBroker.Execute(const AStatement: string;
  AParams: TParams): Integer;
begin
  with CreateDataSet(AStatement, AParams) as TSQLQuery do
    try
      Result := ExecSQL;
    finally
      Free;
    end;
end;

function TInstantDBXBroker.GetConnector: TInstantDBXConnector;
begin
  Result := inherited Connector as TInstantDBXConnector;
end;

function TInstantDBXBroker.GetDatabaseName: string;
begin
  Result := Connector.ParamByName('Database');
end;

function TInstantDBXBroker.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantDBXQuery.Create(Connector);
end;

{ TInstantDBXResolver }

function TInstantDBXResolver.ReadBooleanField(DataSet: TDataSet;
  const FieldName: string): Boolean;
begin
  Result := Boolean(DataSet.FieldByName(FieldName).AsInteger);
end;

{ TInstantDBXTranslator }

function TInstantDBXTranslator.TranslateConstant(
  Constant: TInstantIQLConstant; Writer: TInstantIQLWriter): Boolean;
begin
  if SameText(Constant.Value, InstantTrueString) then
  begin
    Writer.WriteChar('1');
    Result := True;
  end else if SameText(Constant.Value, InstantFalseString) then
  begin
    Writer.WriteChar('0');
    Result := True;
  end else
    Result := inherited TranslateConstant(Constant, Writer);
end;

{ TInstantDBXQuery }

class function TInstantDBXQuery.TranslatorClass: TInstantRelationalTranslatorClass;
begin
  Result := TInstantDBXTranslator;
end;

{ TInstantDBXInterBaseFirebirdBroker }

function TInstantDBXInterBaseFirebirdBroker.ColumnTypeByDataType(
  DataType: TInstantDataType): string;
const
  Types: array[TInstantDataType] of string = (
    'INTEGER',
    'DOUBLE PRECISION',
    'DECIMAL(14,4)',
    'SMALLINT',
    'VARCHAR',
    'BLOB SUB_TYPE 1',
    'TIMESTAMP',
    'BLOB');
begin
  Result := Types[DataType];
end;

function TInstantDBXInterBaseFirebirdBroker.CreateCatalog(
  const AScheme: TInstantScheme): TInstantCatalog;
begin
  Result := TInstantIBFbCatalog.Create(AScheme, Self);
end;

function TInstantDBXInterBaseFirebirdBroker.GetDialect: Integer;
begin
  Result := StrToIntDef(Connector.ParamByName('SQLDialect'), 3);
end;

function TInstantDBXInterBaseFirebirdBroker.GetSQLQuote: Char;
begin
  Result := '''';
end;

{ TInstantDBXMSSQLBroker }

procedure TInstantDBXMSSQLBroker.AssignParam(SourceParam, TargetParam: TParam);
begin
  if SourceParam.DataType = ftBoolean then
    TargetParam.Assign(SourceParam)
  else
    inherited;
end;

function TInstantDBXMSSQLBroker.ColumnTypeByDataType(
  DataType: TInstantDataType): string;
const
  Types: array[TInstantDataType] of string = (
    'INTEGER',
    'FLOAT',
    'MONEY',
    'BIT',
    'VARCHAR',
    'TEXT',
    'DATETIME',
    'IMAGE');
begin
  Result := Types[DataType];
end;

function TInstantDBXMSSQLBroker.CreateCatalog(
  const AScheme: TInstantScheme): TInstantCatalog;
begin
  Result := TInstantMSSqlCatalog.Create(AScheme, Self);
end;

function TInstantDBXMSSQLBroker.CreateResolver(
  Map: TInstantAttributeMap): TInstantSQLResolver;
begin
  Result := TInstantDBXMSSQLResolver.Create(Self, Map);
end;

function TInstantDBXMSSQLBroker.GetDBMSName: string;
begin
  Result := 'MS SQL Server';
end;

function TInstantDBXMSSQLBroker.GetSQLQuote: Char;
begin
  Result := '''';
end;

function TInstantDBXMSSQLBroker.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantDBXMSSQLQuery.Create(Connector);
end;

{ TInstantDBXOracleBroker }

procedure TInstantDBXOracleBroker.AssignParam(SourceParam, TargetParam: TParam);
begin
  case SourceParam.DataType of
    ftBoolean:
      TargetParam.AsString := IntToStr(Integer(SourceParam.AsBoolean));
    ftInteger:
      TargetParam.AsString := IntToStr(SourceParam.AsInteger);
  else
    inherited;
  end;
end;

function TInstantDBXOracleBroker.ColumnTypeByDataType(
  DataType: TInstantDataType): string;
const
  Types: array[TInstantDataType] of string = (
    'INTEGER',
    'FLOAT',
    'DECIMAL(14,4)',
    'NUMBER(1)',
    'VARCHAR',
    'CLOB',
    'DATE',
    'BLOB');
begin
  Result := Types[DataType];
end;

function TInstantDBXOracleBroker.GetDBMSName: string;
begin
  Result := 'Oracle';
end;

function TInstantDBXOracleBroker.GetSQLQuote: Char;
begin
  Result := '''';
end;

{ TInstantDBXDB2Broker }

function TInstantDBXDB2Broker.ColumnTypeByDataType(
  DataType: TInstantDataType): string;
const
  Types: array[TInstantDataType] of string = (
    'INTEGER',
    'FLOAT',
    'DECIMAL(14,4)',
    'SMALLINT',
    'VARCHAR',
    'CLOB (1000 K)',
    'TIMESTAMP',
    'BLOB (1000 K)');
begin
  Result := Types[DataType];
end;

function TInstantDBXDB2Broker.GetDBMSName: string;
begin
  Result := 'IBM DB2';
end;

function TInstantDBXDB2Broker.GetSQLQuote: Char;
begin
  Result := '''';
end;

procedure TInstantDBXDB2Broker.InternalBuildDatabase(
  Scheme: TInstantScheme);
begin
  Connector.StartTransaction;
  try
    inherited;
    Connector.CommitTransaction;
  except
    Connector.RollbackTransaction;
    raise;
  end;
end;

{ TInstantDBXMySQLBroker }

function TInstantDBXMySQLBroker.ColumnTypeByDataType(
  DataType: TInstantDataType): string;
const
  Types: array[TInstantDataType] of string = (
    'INTEGER',
    'FLOAT',
    'DECIMAL(14,4)',
    'TINYINT(1)',
    'VARCHAR',
    'TEXT',
    'DATETIME',
    'BLOB');
begin
  Result := Types[DataType];
end;

function TInstantDBXMySQLBroker.GetDBMSName: string;
begin
  Result := 'MySQL';
end;

function TInstantDBXMySQLBroker.GetSQLQuote: Char;
begin
  Result := '''';
end;

{ TInstantDBXInterBaseBroker }

function TInstantDBXInterBaseBroker.GetDBMSName: string;
begin
  Result := 'InterBase';
end;

{ TInstantDBXFirebirdUIBBroker }

function TInstantDBXFirebirdUIBBroker.GetDBMSName: string;
begin
  Result := 'Firebird';
end;

initialization
  RegisterClass(TInstantDBXConnectionDef);
  TInstantDBXConnector.RegisterClass;

finalization
  TInstantDBXConnector.UnregisterClass;

end.

