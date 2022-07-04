unit SQLMultiDBQuery;

{*************************************************************}
{*                 Freeware dbExpress Plus                   *}
{* Copyright (c) Business Software Systems, Inc. 1995 - 2002 *}
{*                   All rights reserved.                    *}
{*************************************************************}

{$N+,P+,S-,R-}
                                
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, Clipbrd, DBxpress, SqlExpr;

type
  TSQLMultiDbQuery = class(TSQLQuery)
  private
    FAbout: String;
    FSQLFirebird: TStrings;
    FSQLIBMAS400: TStrings;
    FSQLIBMDB2: TStrings;
    FSQLInformix: TStrings;
    FSQLInterbase: TStrings;
    FSQLMSSQLServer: TStrings;
    FSQLMySQL: TStrings;
    FSQLOracle: TStrings;
    FSQLOther: TStrings;
    FSQLPostgres: TStrings;
    FSQLSQLAnywhere: TStrings;
    FSQLSybase: TStrings;
    { Private declarations }
  protected
    procedure SetSQLFirebirdQuery(Value: TStrings);
    procedure SetSQLIBMAS400Query(Value: TStrings);
    procedure SetSQLIBMDB2Query(Value: TStrings);
    procedure SetSQLInformixQuery(Value: TStrings);
    procedure SetSQLInterbaseQuery(Value: TStrings);
    procedure SetSQLMSSQLServerQuery(Value: TStrings);
    procedure SetSQLMySQLQuery(Value: TStrings);
    procedure SetSQLOracleQuery(Value: TStrings);
    procedure SetSQLOtherQuery(Value: TStrings);
    procedure SetSQLPostgresQuery(Value: TStrings);
    procedure SetSQLSQLAnywhereQuery(Value: TStrings);
    procedure SetSQLSybaseQuery(Value: TStrings);
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ResetSQL;
    { Public declarations }
  published
    property About: String read FAbout;                  
    property SQLFirebird: TStrings read FSQLFirebird write SetSQLFirebirdQuery;
    property SQLIBMAS400: TStrings read FSQLIBMAS400 write SetSQLIBMAS400Query;
    property SQLIBMDB2: TStrings read FSQLIBMDB2 write SetSQLIBMDB2Query;
    property SQLInformix: TStrings read FSQLInformix write SetSQLInformixQuery;
    property SQLInterbase: TStrings read FSQLInterbase write SetSQLInterbaseQuery;
    property SQLMSSQLServer: TStrings read FSQLMSSQLServer write SetSQLMSSQLServerQuery;
    property SQLMySQL: TStrings read FSQLMySQL write SetSQLMySQLQuery;
    property SQLOracle: TStrings read FSQLOracle write SetSQLOracleQuery;
    property SQLOther: TStrings read FSQLOther write SetSQLOtherQuery;
    property SQLPostgres: TStrings read FSQLPostgres write SetSQLPostgresQuery;
    property SQLSQLAnywhere: TStrings read FSQLSQLAnywhere write SetSQLSQLAnywhereQuery;
    property SQLSybase: TStrings read FSQLSybase write SetSQLSybaseQuery;
  end;

implementation

constructor TSQLMultiDbQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAbout := 'Ver 0.8.0.8';
  FSQLFirebird := TStringList.Create;
  FSQLIBMAS400 := TStringList.Create;
  FSQLIBMDB2 := TStringList.Create;
  FSQLInformix := TStringList.Create;
  FSQLInterbase := TStringList.Create;
  FSQLMSSQLServer := TStringList.Create;
  FSQLMySQL := TStringList.Create;
  FSQLOracle := TStringList.Create;
  FSQLOther := TStringList.Create;
  FSQLPostgres := TStringList.Create;
  FSQLSQLAnywhere := TStringList.Create;
  FSQLSybase := TStringList.Create;
end;

destructor TSQLMultiDbQuery.Destroy;
begin
  FSQLFirebird.Free;
  FSQLIBMAS400.Free;
  FSQLIBMDB2.Free;
  FSQLInformix.Free;
  FSQLInterbase.Free;
  FSQLMSSQLServer.Free;
  FSQLMySQL.Free;
  FSQLOracle.Free;
  FSQLOther.Free;
  FSQLPostgres.Free;
  FSQLSQLAnywhere.Free;
  FSQLSybase.Free;
  inherited Destroy;
end;

procedure TSQLMultiDbQuery.SetSQLFirebirdQuery(Value: TStrings);
begin
  if SQLFirebird.Text <> Value.Text then SQLFirebird.Assign(Value);
end;

procedure TSQLMultiDbQuery.SetSQLIBMAS400Query(Value: TStrings);
begin
  if SQLIBMAS400.Text <> Value.Text then SQLIBMAS400.Assign(Value);
end;

procedure TSQLMultiDbQuery.SetSQLIBMDB2Query(Value: TStrings);
begin
  if SQLIBMDB2.Text <> Value.Text then SQLIBMDB2.Assign(Value);
end;

procedure TSQLMultiDbQuery.SetSQLInformixQuery(Value: TStrings);
begin
  if SQLInformix.Text <> Value.Text then SQLInformix.Assign(Value);
end;

procedure TSQLMultiDbQuery.SetSQLInterbaseQuery(Value: TStrings);
begin
  if SQLInterbase.Text <> Value.Text then SQLInterbase.Assign(Value);
end;

procedure TSQLMultiDbQuery.SetSQLMSSQLServerQuery(Value: TStrings);
begin
  if SQLMSSQLServer.Text <> Value.Text then SQLMSSQLServer.Assign(Value);
end;

procedure TSQLMultiDbQuery.SetSQLMySQLQuery(Value: TStrings);
begin
  if SQLMySQL.Text <> Value.Text then SQLMySQL.Assign(Value);
end;

procedure TSQLMultiDbQuery.SetSQLOracleQuery(Value: TStrings);
begin
  if SQLOracle.Text <> Value.Text then SQLOracle.Assign(Value);
end;

procedure TSQLMultiDbQuery.SetSQLOtherQuery(Value: TStrings);
begin
  if SQLOther.Text <> Value.Text then SQLOther.Assign(Value);
end;

procedure TSQLMultiDbQuery.SetSQLPostgresQuery(Value: TStrings);
begin
  if SQLPostgres.Text <> Value.Text then SQLPostgres.Assign(Value);
end;

procedure TSQLMultiDbQuery.SetSQLSQLAnywhereQuery(Value: TStrings);
begin
  if SQLSQLAnywhere.Text <> Value.Text then SQLSQLAnywhere.Assign(Value);
end;

procedure TSQLMultiDbQuery.SetSQLSybaseQuery(Value: TStrings);
begin
  if SQLSybase.Text <> Value.Text then SQLSybase.Assign(Value);
end;

procedure TSQLMultiDbQuery.ResetSQL;
var
  aDatabaseVendor: String;
begin
  SQL.Clear;
  if SQLConnection = nil then Exit;

  aDatabaseVendor := UpperCase(SQLConnection.Params.Values['DatabaseVendor']);

  if aDatabaseVendor = 'FIREBIRD' then
    SQL.Assign(SQLIBMAS400)
  else if aDatabaseVendor = 'IBMAS400' then
    SQL.Assign(SQLIBMDB2)
  else if aDatabaseVendor = 'IBMDB2' then
    SQL.Assign(SQLIBMDB2)
  else if aDatabaseVendor = 'INFORMIX' then
    SQL.Assign(SQLInformix)
  else if aDatabaseVendor = 'INTERBASE' then
    SQL.Assign(SQLInterbase)
  else if aDatabaseVendor = 'MSSQLSERVER' then
    SQL.Assign(SQLMSSQLServer)
  else if aDatabaseVendor = 'MYSQL' then
    SQL.Assign(SQLMySQL)
  else if aDatabaseVendor = 'ORACLE' then
    SQL.Assign(SQLOracle)
  else if aDatabaseVendor = 'POSTGRES' then
    SQL.Assign(SQLPostgres)
  else if aDatabaseVendor = 'SQLANYWHERE' then
    SQL.Assign(SQLSQLAnywhere)
  else if aDatabaseVendor = 'SYBASE' then
    SQL.Assign(SQLSybase)
  else
    SQL.Assign(SQLOther);
end;

end.

