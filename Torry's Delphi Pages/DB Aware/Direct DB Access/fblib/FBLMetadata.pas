{
   Firebird Library
   Open Source Library No Data Aware for direct access to Firebird
   Relational Database from Borland Delphi / Kylix and Freepascal

   File : FBLMetadata.pas
   Copyright (c) 2002-2004 Alessandro Batisti
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
@abstract(Extract metadata from firebird database)
@author(Alessandro Batisti <fblib@altervista.org>)
FbLib - Firebird Library @html(<br>)
FBLMetadata.pas unit provides to extract metadata from firebird database
}
unit FBLMetadata;

interface

uses
  SysUtils,
  {$IFDEF D6P}
  Types,
  {$ENDIF}
  Classes,
  FBLDatabase, FBLTransaction, FBLDsql;

type
  {@abstract(encapsulates the properties and methods for extract metadata from firebird database)}
  TFBLMetadata = class(TComponent)
  private
    FDatabase: TFBLDatabase;
    FTransaction: TFBLTransaction;
    FTerminator: string;
    FSetTerm: boolean;
    FMetadata: TStringList;
    FTables: TStringList;
    FSystemTables: TStringList;
    FViews: TStringList;
    FProcedures: TStringList;
    FGenerators: TStringList;
    FDomains: TStringList;
    FUdfs: TStringList;
    FExceptions: TStringList;
    FRoles: TStringList;
    FTriggers: TStringList;
    FSystemTriggers: TStringList;
    FTriggersInTable: TStringList;

    function GetMetadata: TStringList;
    function GetTables: TStringList;
    function GetSystemTables: TStringList;
    function GetViews: TStringList;
    function GetProcedures: TStringList;
    function GetGenerators: TStringList;
    function GetDomains: TStringList;
    function GetUdfs: TStringList;
    function GetExceptions: TStringList;
    function GetRoles: TStringList;
    function GetTriggers: TStringList;
    function GetSystemTriggers: TStringList;
    function FieldType(const AType, ASubType, APrecision,
      AScale, ALength: integer): string;
    function FieldsInIndex(const AIndexName: string): string;
    function RefConstraints(const AConstraintName: string): string;
    function CheckConstraints(const AConstraintName: string): string;
    function ArrayFieldDimension(const AFieldName: string): string;
    function GetDefaultCharset: string;
    function GetUser: string;
  public
    {Create an instance  of a TFBLMetadata}
    constructor Create(AOwner: TComponent); override;
    {Free up  all resources associated with this instance}
    destructor Destroy; override;
    {@Exclude}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {Extract sql source from a Table
    @param(ATableName : Table name )}
    function TableSource(const ATableName: string): string;
    {Extract sql source from a View
    @param(AViewName : View name )}
    function ViewSource(const AViewName: string): string;
    {Extract sql source from a Stored Procedure
    @param(AProcName : Procedure name )}
    function ProcedureSource(const AProcName: string): string;
    {Extract sql source from a Trigger
    @param(ATriggerName : Trigger name )}
    function TriggerSource(const ATriggerName: string): string;
    {Extract sql source from a Domain
    @param(ADomainName: Domain name )}
    function DomainSource(const ADomainName: string): string;
    {Extract sql source from a UDF
    @param(AUDFName: UDF name )}
    function UDFSource(const AUDFName: string): string;
    {Extract Index sql source from a Table
    @param(ATableName: Table name )}
    function IndexSource(const ATableName: string): string;
    {Extract PrimaryKey sql source from a Table
    @param(ATableName: Table name )}
    function PrimaryKeyConstraintSource(const ATableName: string): string;
    {Extract ForeignKey sql source from a Table
    @param(ATableName: Table name )}
    function ForeignKeyConstraintSource(const ATableName: string): string;
    {Extract CheckConstraint sql source from a Table
    @param(ATableName: Table name )}
    function CheckConstraintSource(const ATableName: string): string;
    {Extract UniqueConstraint sql source from a Table
    @param(ATableName: Table name )}
    function UniqueConstraintSource(const ATablename: string): string;
    {Extract sql source from a Exception
    @param(AExceptionName: Exception name )}
    function ExceptionSource(const AExceptionName: string): string;
    {Extract the Value from a generator
    @param(AGeneratorName: Generator  name )}
    function GeneratorValue(const AGeneratorName: string): string;
    {Return List of trigger in a Table
    @param(ATableName: Table name )}
    function TriggersInTable(const ATableName: string): TStringList;
    {Extract All metadata of the database}
    property Metadata: TStringList read GetMetadata;
    {Return list of Table in the database}
    property Tables: TStringList read GetTables;
    {Return list of SystemTrigger in the database}
    property SystemTables: TStringList read GetSystemTables;
    {Return list of View in the database}
    property Views: TStringList read GetViews;
    {Return list of Procedure in the database}
    property Procedures: TStringList read GetProcedures;
    {Return list of Generator in the database}
    property Generators: TStringList read GetGenerators;
    {Return list of Domain in the database}
    property Domains: TStringList read GetDomains;
    {Return list of Udf in the database}
    property UDFs: TStringList read GetUdfs;
    {Return list of Exception in the database}
    property Exceptions: TStringList read GetExceptions;
    {Return list of Role in the database}
    property Roles: TStringList read GetRoles;
    {Return list of Trigger in the database}
    property Triggers: TStringList read GetTriggers;
    {Return list of SystemTrigger in the database}
    property SystemTriggers: TStringList read GetSystemTriggers;
    {Return current DefaultCharSet in the database}
    property DefaultCharset: string read GetDefaultCharset;
    {Return current user in the database}
    property User: string read GetUser;
  published
    {Instance of @link(TFBLDatabase) where exctracts metadata from}
    property Database: TFBLDatabase read FDatabase write FDatabase;
    {Current terminator value default := '^'}
    property Terminator: string read FTerminator write FTerminator;
    {if True print 'SET TERM' in stored procedure e trigger when extract metadata default := @True}
    property SetTerm: boolean read FSetTerm write FSetTerm;
  end;

implementation

uses FBLConst;

constructor TFBLMetadata.Create(AOwner: TComponent);
begin
  inherited;
  if AOwner is TFBLDatabase then Database := TFBLDatabase(AOwner);
  FMetadata := nil;
  FTables := nil;
  FSystemTables := nil;
  FViews := nil;
  FProcedures := nil;
  FGenerators := nil;
  FDomains := nil;
  FUdfs := nil;
  FExceptions := nil;
  FRoles := nil;
  FTriggers := nil;
  FSystemTriggers := nil;
  FTriggersInTable := nil;
  FTransaction := TFBLTransaction.Create(nil);
  FTerminator := '^';
  FSetTerm := True;
end;

//------------------------------------------------------------------------------

destructor TFBLMetadata.Destroy;
begin
  FMetadata.Free;
  FTables.Free;
  FSystemTables.Free;
  FViews.Free;
  FProcedures.Free;
  FGenerators.Free;
  FDomains.Free;
  FUdfs.Free;
  FExceptions.Free;
  FRoles.Free;
  FTriggers.Free;
  FSystemTriggers.Free;
  FTriggersInTable.Free;
  FTransaction.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TFBLMetadata.Notification(AComponent: TComponent;
  Operation: TOperation);//override;
begin
  if Operation = opRemove then 
  begin
    if AComponent = FDatabase then FDatabase := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

//------------------------------------------------------------------------------

function TFBLMetadata.GetMetadata: TStringList;
var
  i: integer;
  sTemp: string;
  bTemp: boolean;
begin
  bTemp := FSetTerm;
  if FMetadata = nil then FMetadata := TStringList.Create;
  Result := FMetadata;
  sTemp := '';
  with FMetadata do 
  begin
    Clear;
    add('/* Database "' + FDatabase.DBFileName + '" */');
    add('');
    add('/* Domains */ ');
    add('');
    for i := 0 to Domains.Count - 1 do
    begin
      Text := Text + DomainSource(Domains[i]);
      add('');
    end;

    add('');
    add('/* Generators */');
    add('');
    for i := 0 to Generators.Count - 1 do
      add('CREATE GENERATOR ' + Generators.Strings[i] + ' ;');
    add('');
    add('/* Tables */');
    add('');

    for i := 0 to Tables.Count - 1 do
    begin
      Text := Text + TableSource(Tables[i]);
      sTemp := PrimaryKeyConstraintSource(Tables[i]);
      if sTemp <> '' then
      begin
        add('');
        add(' /* Primary Key */');
        Text := Text + sTemp;
      end;
      add(' ');
    end;
    add('');
    add('/* Foreign key Constraints */');
    add('');
    for i := 0 to Tables.Count - 1 do
    begin
      sTemp := ForeignKeyConstraintSource(Tables[i]);
      if sTemp <> '' then
        Text := Text + sTemp;
    end;
    add('');
    add('/* Check Constraints */');
    add('');
    for i := 0 to Tables.Count - 1 do
    begin
      sTemp := CheckConstraintSource(Tables[i]);
      if sTemp <> '' then
        Text := Text + sTemp;
    end;

    add('');
    add('/* Unique Constraints */');
    add('');
    for i := 0 to Tables.Count - 1 do
    begin
      sTemp := UniqueConstraintSource(Tables[i]);
      if sTemp <> '' then
        Text := Text + sTemp;
    end;
    add('');
    add('/* indices */');
    add('');
    for i := 0 to Tables.Count - 1 do
    begin
      sTemp := IndexSource(Tables[i]);
      if sTemp <> '' then
        Text := Text + sTemp;
    end;
    add('');
    add('/* Views */');
    add('');
    for i := 0 to Views.Count - 1 do
    begin
      Text := Text + ViewSource(Views[i]);
      add(' ');
    end;
    add(' ');
    add('/* Stored procedures */');
    add(' ');
    for i := 0 to Procedures.Count - 1 do
    begin
      FSetTerm := True;
      Text := Text + ProcedureSource(Procedures[i]);
      Add(' ');
      FSetTerm := bTemp;
    end;
    add(' ');
    add('/* Triggers */');
    add(' ');
    for i := 0 to Triggers.Count - 1 do
    begin
      FSetTerm := True;
      Text := Text + TriggerSource(Triggers[i]);
      add(' ');
      FSetTerm := bTemp;
    end;
    add(' ');
    add('/* UDFs */');
    add('  ');
    for i := 0 to UDFs.Count - 1 do
    begin
      Text := Text + UdfSource(UDFs[i]);
      add(' ');
    end;
    add(' ');
    add('/* Exceptions */');
    add(' ');
    for i := 0 to Exceptions.Count - 1 do
    begin
      Text := Text + ExceptionSource(Exceptions[i]);
      add(' ');
    end;
    add(' ');
    add('/* Roles */');
    add(' ');
    for i := 0 to Roles.Count - 1 do
      add('CREATE ROLE ' + Roles.Strings[i] + ';')
  end; // end with
end;

//------------------------------------------------------------------------------

function TFBLMetadata.GetTables: TStringList;
var
  Qry: TFBLDsql;
begin
  if FTables = nil then FTables := TStringList.Create;
  Result := FTables;
  FTables.Clear;
  Qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;

    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    {
    qry.SQL.Text := 'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS WHERE ' +
      'RDB$SYSTEM_FLAG = 0 AND RDB$DBKEY_LENGTH = 8';
    }
    Qry.SQL.Text := 'SELECT RDB$RELATIONS.RDB$RELATION_NAME ' +
      'FROM RDB$RELATIONS ' +
      'WHERE RDB$SYSTEM_FLAG = 0 ' +
      'AND RDB$RELATIONS.RDB$RELATION_NAME NOT IN ' +
      '(SELECT RDB$VIEW_RELATIONS.RDB$VIEW_NAME FROM RDB$VIEW_RELATIONS)';
    qry.ExecSQL;
    while not Qry.EOF do
    begin
      FTables.Add(trim(Qry.FieldAsString(0)));
      qry.Next;
    end;
    qry.UnPrepare;
    if FTransaction.InTransaction then FTransaction.Commit;
  finally

    qry.Free;
  end;
end;

//-------------------------------------------------------------------------------

function TFBLMetadata.GetSystemTables: TStringList;
var
  Qry: TFBLDsql;
begin
  if FSystemTables = nil then FSystemTables := TStringList.Create;
  Result := FSystemTables;
  FSystemTables.Clear;
  Qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS WHERE ' +
      'RDB$SYSTEM_FLAG <> 0 AND RDB$DBKEY_LENGTH = 8';
    qry.ExecSQL;
    while not Qry.EOF do
    begin
      FSystemTables.Add(trim(Qry.FieldAsString(0)));
      qry.Next;
    end;
    qry.UnPrepare;
    if FTransaction.InTransaction then FTransaction.Commit;
  finally

    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.GetViews: TStringList;
var
  Qry: TFBLDsql;
begin
  if FViews = nil then FViews := TStringList.Create;
  Result := FViews;
  FViews.Clear;
  qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    {
    Qry.SQL.Text := 'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS WHERE ' +
      'RDB$SYSTEM_FLAG = 0 AND RDB$DBKEY_LENGTH <> 8;';
    }
    Qry.SQL.Text := 'SELECT RDB$RELATIONS.RDB$RELATION_NAME ' +
      'FROM RDB$RELATIONS ' +
      'WHERE RDB$SYSTEM_FLAG = 0 ' +
      'AND RDB$RELATIONS.RDB$RELATION_NAME IN ' +
      '(SELECT RDB$VIEW_RELATIONS.RDB$VIEW_NAME FROM RDB$VIEW_RELATIONS)';
    qry.ExecSQL;
    while not qry.EOF do
    begin
      FViews.Add(trim(qry.FieldAsString(0)));
      qry.Next;
    end;
    qry.UnPrepare;
    if FTransaction.InTransaction then FTransaction.Commit;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.GetProcedures: TStringList;
var
  qry: TFBLDsql;
begin
  if FProcedures = nil then FProcedures := TStringList.Create;
  Result := FProcedures;
  FProcedures.Clear;
  qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT RDB$PROCEDURE_NAME FROM RDB$PROCEDURES';
    qry.ExecSQL;
    while not qry.EOF do
    begin
      FProcedures.Add(trim(qry.FieldAsString(0)));
      qry.Next;
    end;
    qry.UnPrepare;
    if FTransaction.InTransaction then FTransaction.Commit;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.GetGenerators: TStringList;
var
  qry: TFBLDsql;
begin
  if FGenerators = nil then FGenerators := TStringList.Create;
  Result := FGenerators;
  FGenerators.Clear;
  qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT RDB$GENERATOR_NAME FROM RDB$Generators ' +
      'WHERE RDB$SYSTEM_FLAG IS NULL';
    qry.ExecSQL;
    while not qry.EOF do
    begin
      FGenerators.Add(trim(qry.FieldAsString(0)));
      qry.Next;
    end;
    qry.UnPrepare;
    if FTransaction.InTransaction then FTransaction.Commit;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.GetDomains: TStringList;
var
  qry: TFBLDsql;
begin
  if FDomains = nil then FDomains := TStringList.Create;
  Result := FDomains;
  FDomains.Clear;
  qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not Ftransaction.InTransaction then Ftransaction.StartTransaction;
    qry.SQL.Text := 'SELECT RDB$FIELD_NAME FROM RDB$FIELDS WHERE RDB$SYSTEM_FLAG = 0 AND ' +
      'RDB$FIELD_NAME NOT STARTING WITH ''RDB$''';
    qry.ExecSQL;
    while not qry.EOF do
    begin
      FDomains.Add(trim(qry.FieldAsString(0)));
      qry.Next;
    end;
    qry.UnPrepare;
    if FTransaction.InTransaction then FTransaction.Commit;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.GetUdfs: TStringList;
var
  qry: TFBLDsql;
begin
  if FUdfs = nil then FUdfs := TStringList.Create;
  Result := FUdfs;
  FUDfs.Clear;
  qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT RDB$FUNCTION_NAME FROM RDB$FUNCTIONS';
    qry.ExecSQL;
    while not qry.EOF do
    begin
      Fudfs.Add(trim(Qry.FieldAsString(0)));
      qry.Next;
    end;
    qry.UnPrepare;
    if FTransaction.InTransaction then FTransaction.Commit;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.GetExceptions: TStringList;
var
  qry: TFBLDsql;
begin
  if FExceptions = nil then FExceptions := TStringList.Create;
  Result := FExceptions;
  FExceptions.Clear;
  qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT RDB$EXCEPTION_NAME FROM RDB$EXCEPTIONS';
    qry.ExecSQL;
    while not qry.EOF do
    begin
      FExceptions.Add(trim(qry.FieldAsString(0)));
      qry.Next;
    end;
    qry.UnPrepare;
    if FTransaction.InTransaction then FTransaction.Commit;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.GetRoles: TStringList;
var
  qry: TFBLDsql;
begin
  if FRoles = nil then FRoles := TStringList.Create;
  Result := FRoles;
  FRoles.Clear;
  qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT RDB$ROLE_NAME FROM RDB$ROLES';
    qry.ExecSQL;
    while not qry.EOF do
    begin
      FRoles.Add(trim(qry.FieldAsString(0)));
      qry.Next;
    end;
    qry.UnPrepare;
    if FTransaction.InTransaction then FTransaction.Commit;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.GetTriggers: TStringList;
var
  qry: TFBLDsql;
begin
  if FTriggers = nil then FTriggers := TStringList.Create;
  Result := FTriggers;
  FTriggers.Clear;
  qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT RDB$TRIGGER_NAME FROM RDB$TRIGGERS ' +
      'WHERE (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG <> 1) ' +
      'AND  RDB$TRIGGER_NAME ' +
      'NOT IN (SELECT RDB$CHECK_CONSTRAINTS.RDB$TRIGGER_NAME ' +
      'FROM RDB$CHECK_CONSTRAINTS)';
    qry.ExecSQL;
    while not qry.EOF do
    begin
      FTriggers.Add(trim(qry.FieldAsString(0)));
      qry.Next;
    end;
    qry.UnPrepare;
    if FTransaction.InTransaction then FTransaction.Commit;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.GetSystemTriggers: TStringList;
var
  qry: TFBLDsql;
begin
  if FSystemTriggers = nil then FSystemTriggers := TStringList.Create;
  Result := FSystemTriggers;
  FSystemTriggers.Clear;
  qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT RDB$TRIGGER_NAME FROM RDB$TRIGGERS ' +
      'WHERE  RDB$SYSTEM_FLAG = 1 ' +
      'OR RDB$TRIGGER_NAME ' +
      'IN (SELECT RDB$CHECK_CONSTRAINTS.RDB$TRIGGER_NAME ' +
      'FROM RDB$CHECK_CONSTRAINTS)';
    qry.ExecSQL;
    while not qry.EOF do
    begin
      FSystemTriggers.Add(trim(qry.FieldAsString(0)));
      qry.Next;
    end;
    qry.UnPrepare;
  finally
    qry.Free;
    if FTransaction.InTransaction then FTransaction.Commit;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.FieldType(const AType, ASubType,
  APrecision, AScale, ALength: integer): string;
begin
  Result := '';
  case AType of
    14: //char
      Result := Format('CHAR(%d)', [ALength]);
    37: //varchar
      Result := Format('VARCHAR(%d)', [ALength]);
    35://timestamp
      Result := 'TIMESTAMP';
    13: //time
      Result := 'TIME';
    12: //Date
      Result := 'DATE';
    10: //Single
      Result := 'FLOAT';
    27: //Double precision
      begin
        if Abs(AScale) > 0 then   // Dialect 1
          Result := format('NUMERIC(15,%d)', [Abs(AScale)])
        else
          Result := 'DOUBLE PRECISION';
      end;
    16://int64
      begin
        Result := 'BIGINT';
        if ASubType = 1 then
          Result := Format('NUMERIC(%d,%d)', [APrecision,
            Abs(AScale)]);
        if ASubType = 2 then
          Result := Format('DECIMAL(%d,%d)', [APrecision,
            Abs(AScale)]);
      end;
    8: //integer
      begin
        Result := 'INTEGER';
        if ASubType = 1 then
          Result := Format('NUMERIC(%d,%d)', [APrecision,
            Abs(AScale)]);
        if ASubType = 2 then
          Result := Format('DECIMAL(%d,%d)', [APrecision,
            Abs(AScale)]);
      end;
    7:// smallint
      begin
        Result := 'SMALLINT';
        if ASubType = 1 then
          Result := Format('NUMERIC(%d,%d)', [APrecision,
            Abs(AScale)]);
        if ASubType = 2 then
          Result := Format('DECIMAL(%d,%d)', [APrecision,
            Abs(AScale)]);
      end;
    9:    // quad
      Result := 'QUAD';
    40:   //cstring
      Result := format('CSTRING(%d)', [ALength]);
    11:   //d_float
      Result := 'D_FLOAT';
    261:  //blob
      begin
        Result := 'BLOB SUB_TYPE ' + IntToStr(ASubType);
        if ASubType = 1 then
          Result := 'BLOB SUB_TYPE TEXT';
      end;
  end;
  //result:=trim(result);
end;

//------------------------------------------------------------------------------

function TFBLMetadata.FieldsInIndex(const AIndexName: string): string;
var
  qry: TFBLDsql;
begin
  Result := '';
  qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT RDB$FIELD_NAME FROM RDB$INDEX_SEGMENTS WHERE ' +
      'RDB$INDEX_NAME = ?';
    qry.Prepare;
    qry.ParamAsString(0,AIndexName);
    qry.ExecSQL;
    while not qry.EOF do 
    begin
      Result := Result + trim(qry.FieldAsString(0));
      qry.Next;
      if not qry.EOF then Result := Result + ',';
    end;
    qry.Unprepare;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.RefConstraints(const AConstraintName: string): string;
var
  qry: TFBLDsql;
  pk, up, de: string;
begin
  Result := '';
  qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;

    qry.SQL.Text := 'SELECT RDB$CONST_NAME_UQ,' +  //0
      'RDB$UPDATE_RULE,' +           //1
      'RDB$DELETE_RULE ' +          //2
      'FROM RDB$REF_CONSTRAINTS ' +
      'WHERE RDB$CONSTRAINT_NAME  = ? ';
    qry.Prepare;
    qry.ParamAsString(0, AConstraintName);
    qry.ExecSQL;
    pk := trim(qry.FieldAsString(0));
    up := trim(qry.FieldAsString(1));
    de := trim(qry.FieldAsString(2));
    qry.UnPrepare;
    qry.SQL.Text := 'SELECT RDB$RELATION_NAME,' +       //0
      'RDB$INDEX_NAME ' +                //1
      'FROM RDB$RELATION_CONSTRAINTS ' +
      'WHERE RDB$CONSTRAINT_NAME = ?';
    qry.Prepare;
    qry.ParamAsString(0,pk);
    qry.ExecSQL;
    Result := 'REFERENCES ' + trim(qry.FieldAsString(0)) +
      '(' + trim(FieldsInIndex(Qry.FieldAsString(1))) +
      ')' + NEW_LINE + INTEND + 'ON UPDATE ' + up + ' ON DELETE ' + de;
    qry.UnPrepare;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.CheckConstraints(const AConstraintName: string): string;
var
  qry: TFBLDsql;
begin
  Result := '';
  qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not Ftransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT  B.RDB$TRIGGER_SOURCE FROM ' +
      'RDB$CHECK_CONSTRAINTS A, RDB$TRIGGERS B ' +
      'WHERE (A.RDB$TRIGGER_NAME = B.RDB$TRIGGER_NAME) ' +
      'AND A.RDB$CONSTRAINT_NAME = ?';
    qry.Prepare;
    qry.ParamAsString(0,AConstraintName);
    qry.ExecSQL;
    Result := Trim(qry.FieldAsString(0));
    qry.UnPrepare;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.ArrayFieldDimension(const AFieldName: string): string;
var
  qry: TFBLDsql;
begin
  Result := '';
  qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT RDB$LOWER_BOUND,' +     //0
      'RDB$UPPER_BOUND ' +            //1
      'FROM RDB$FIELD_DIMENSIONS ' +
      'WHERE RDB$FIELD_NAME = ?';
    qry.Prepare;
    qry.ParamAsString(0,AFieldName);
    qry.ExecSQL;
    while not qry.EOF do 
    begin
      if qry.FetchCount = 1 then Result := Result + '[';
      Result := Result + qry.FieldAsString(0) + ':' + qry.FieldAsString(1);
      qry.Next;
      if qry.EOF then
        Result := Result + ']'
      else
        Result := Result + ',';
    end;
    qry.UnPrepare;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.TableSource(const ATableName: string): string;
var
  qry: TFBLDsql;
begin
  qry := TFBLDsql.Create(self);
  Result := '';
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT ' +
      'A.RDB$FIELD_NAME,' +      //0
      'A.RDB$FIELD_SOURCE,' +    //1
      'A.RDB$NULL_FLAG,' +       //2
      'A.RDB$DEFAULT_SOURCE,' +  //3
      'B.RDB$FIELD_TYPE,' +      //4
      'B.RDB$FIELD_SUB_TYPE,' +  //5
      'B.RDB$FIELD_PRECISION,' + //6
      'B.RDB$FIELD_SCALE,' +     //7
      'B.RDB$FIELD_LENGTH,' +    //8
      'B.RDB$COMPUTED_SOURCE,' + //9
      'B.RDB$CHARACTER_SET_ID,' + //10
      'B.RDB$DIMENSIONS,' +      //11
      'C.RDB$CHARACTER_SET_NAME ' + //12
      'FROM RDB$RELATION_FIELDS A ' +
      'INNER JOIN  RDB$FIELDS B ON A.RDB$FIELD_SOURCE = B.RDB$FIELD_NAME ' +
      'LEFT JOIN RDB$CHARACTER_SETS C ON B.RDB$CHARACTER_SET_ID = C.RDB$CHARACTER_SET_ID ' +
      'WHERE RDB$RELATION_NAME = ? ' +
      'ORDER BY A.RDB$FIELD_POSITION';
    qry.Prepare;
    qry.ParamAsString(0,ATableName);
    qry.ExecSQL;
    Result := 'CREATE TABLE ' + Trim(ATableName) + ' (' + NEW_LINE;
    while not qry.EOF do
    begin
      Result := Result + INTEND + qry.FieldAsString(0);
      if not qry.FieldIsNull(9) then
        Result := Result + 'COMPUTED BY ' + UpperCase(qry.FieldAsString(9))
      else
      begin
        if copy(qry.FieldAsString(1), 1,4) = 'RDB$' then
        begin
          Result := Result + FieldType(qry.FieldAsLong(4), qry.FieldAsLong(5),
            qry.FieldAsLong(6), qry.FieldAsLong(7),
            qry.FieldAsLong(8));
          if (not qry.FieldIsNull(10)) and (qry.FieldAsInteger(10) <> 0) then
            Result := Result + ' CHARACTER SET ' + trim(qry.FieldAsString(12));
          if qry.FieldAsLong(11) > 0 then   //array
            Result := Result + ArrayFieldDimension(qry.FieldAsString(1));
          if not qry.FieldIsNull(3) then   // default value
            Result := Result + ' ' + Trim(qry.FieldAsString(3));
        end
        else
          Result := Result + '"' + Trim(qry.FieldAsString(1)) + '"';
      end;
      if qry.FieldAsLong(2) = 1 then
        Result := Result + ' NOT NULL';
      qry.Next;
      if qry.EOF then
        Result := Result + ');'
      else
        Result := Result + ',' + NEW_LINE;
    end;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.ViewSource(const AViewName: string): string;
var
  qry: TFBLDsql;
begin
  qry := TFBLDsql.Create(self);
  Result := '';
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    Result := 'CREATE VIEW ' + Trim(AViewName) + NEW_LINE;
    qry.SQL.Text := 'SELECT RDB$FIELD_NAME ' +
      'FROM RDB$RELATION_FIELDS ' +
      'WHERE RDB$RELATION_NAME = ? ' +
      'ORDER BY RDB$FIELD_POSITION ';
    qry.Prepare;
    qry.ParamAsString(0, AViewName);
    qry.ExecSQL;
    if not qry.EOF then Result := Result + '(' + NEW_LINE;
    while not qry.EOF do
    begin
      Result := Result + INTEND + trim(Qry.FieldAsString(0));
      Qry.Next;
      if qry.EOF then
        Result := Result + NEW_LINE + ')' + NEW_LINE
      else
        Result := Result + ',' + NEW_LINE
    end;
    qry.UnPrepare;

    qry.SQL.Text := 'SELECT RDB$VIEW_SOURCE FROM RDB$RELATIONS ' +
      'WHERE RDB$RELATION_NAME = ?';
    qry.Prepare;
    qry.ParamAsString(0,AViewName);
    qry.ExecSQL;
    Result := Result + 'AS' + NEW_LINE + Trim(qry.FieldAsString(0)) + ';';
    qry.UnPrepare;
    if FTransaction.InTransaction then FTransaction.Commit;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.ProcedureSource(const AProcName: string): string;
var
  qry: TFBLDsql;
begin
  qry := TFBLDsql.Create(self);
  Result := '';
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    // intestazione procedura
    if FSetTerm then
      Result := Result + 'SET TERM ' + FTerminator + ' ;' + NEW_LINE;
    Result := Result + 'CREATE PROCEDURE ' + Trim(AProcName);
    //query for procedure parameters
    qry.SQL.Text := 'SELECT ' +
      'A.RDB$PARAMETER_NAME,' +  //0
      'A.RDB$FIELD_SOURCE,' +   //1
      'B.RDB$FIELD_TYPE,' +      //2
      'B.RDB$FIELD_SUB_TYPE,' +  //3
      'B.RDB$FIELD_PRECISION,' + //4
      'B.RDB$FIELD_SCALE,' +     //5
      'B.RDB$FIELD_LENGTH ' +    //6
      'FROM RDB$PROCEDURE_PARAMETERS A ' +
      'INNER JOIN RDB$FIELDS B ON A.RDB$FIELD_SOURCE = B.RDB$FIELD_NAME ' +
      'WHERE A.RDB$PARAMETER_TYPE = ? ' +
      'AND A.RDB$PROCEDURE_NAME = ?';
    qry.Prepare;
    //Input params
    QRY.ParamAsShort(0,0);
    Qry.ParamAsString(1, AProcName);
    Qry.ExecSQL;
    if not qry.EOF then Result := Result + '(';
    while not qry.EOF do
    begin
      Result := Result + NEW_LINE;
      Result := Result + INTEND + trim(qry.FieldAsString(0)) + ' ' +
        FieldType(qry.FieldAsInteger(2), qry.FieldAsInteger(3),
        qry.FieldAsInteger(4),
        qry.FieldAsInteger(5), qry.FieldAsInteger(6));
      qry.Next;
      if qry.EOF then Result := Result + ' )' 
      else 
        Result := Result + ',';
    end;
    qry.Close;
    // output params
    qry.ParamAsShort(0,1);
    qry.ParamAsString(1,AProcName);
    qry.ExecSQL;
    if not qry.EOF then Result := Result + NEW_LINE + 'RETURNS (';
    while not qry.EOF do
    begin
      Result := Result + NEW_LINE;
      Result := Result + '  ' + trim(qry.FieldAsString(0)) + ' ' +
        FieldType(qry.FieldAsLong(2), qry.FieldAsLong(3),
        qry.FieldAsLong(4),
        qry.FieldAsLong(5), qry.FieldAsLong(6));
      qry.Next;
      if qry.EOF then Result := Result + ')' 
      else 
        Result := Result + ',';
    end;
    qry.UnPrepare;
    Result := Result + NEW_LINE + 'AS' + NEW_LINE;
    // corpo procedura
    qry.SQL.Text := 'SELECT RDB$PROCEDURE_SOURCE ' + //BLOB SUB_TYPE TEXT
      'FROM RDB$PROCEDURES ' +
      'WHERE RDB$PROCEDURE_NAME = ?';
    qry.Prepare;
    qry.ParamAsString(0,AProcName);
    qry.ExecSQL;
    Result := Result + Trim(qry.FieldAsString(0));
    qry.UnPrepare;
    if FSetTerm then
      Result := Result + FTerminator + NEW_LINE + 'SET TERM ;' + FTerminator;
    if FTransaction.InTransaction then FTransaction.Commit;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.TriggerSource(const ATriggerName: string): string;
var
  qry: TFBLDsql;
begin
  qry := TFBLDsql.Create(self);
  Result := '';
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT RDB$TRIGGER_SOURCE,' + //0
      'RDB$TRIGGER_TYPE,' +          //1
      'RDB$RELATION_NAME,' +        //2
      'RDB$TRIGGER_INACTIVE,' +      //3
      'RDB$TRIGGER_SEQUENCE ' +      //4
      'FROM RDB$TRIGGERS ' +
      'WHERE RDB$TRIGGER_NAME = ?';
    qry.Prepare;
    qry.ParamAsString(0,ATriggerName);
    qry.ExecSQL;
    if FSetTerm then
      Result := Result + 'SET TERM ' + FTerminator + ' ;' + NEW_LINE;
    Result := Result + 'CREATE TRIGGER ' + Trim(ATriggerName) + ' FOR ' +
      qry.FieldAsString(2) + NEW_LINE;
    if qry.FieldAsLong(3) = 0 then
      Result := Result + 'ACTIVE ' 
    else 
      Result := Result + 'INACTIVE ';
    case qry.FieldAsLong(1) of
      1: Result := Result + 'BEFORE INSERT' + NEW_LINE;
      2: Result := Result + 'AFTER INSERT' + NEW_LINE;
      3: Result := Result + 'BEFORE UPDATE' + NEW_LINE;
      4: Result := Result + 'AFTER UPDATE' + NEW_LINE;
      5: Result := Result + 'BEFORE DELETE' + NEW_LINE;
      6: Result := Result + 'AFTER DELETE' + NEW_LINE;
    end;
    Result := Result + 'POSITION ' + Trim(qry.FieldAsString(4)) + NEW_LINE;
    Result := Result + Trim(qry.FieldAsString(0));
    if FSetTerm then
      Result := Result + FTerminator + NEW_LINE + 'SET TERM ;' + FTerminator;
    qry.UnPrepare;
    if FTransaction.InTransaction then FTransaction.Commit;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.DomainSource(const ADomainName: string): string;
var
  qry: TFBLDsql;
begin
  qry := TFBLDsql.Create(self);
  Result := '';
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT ' +
      'A.RDB$VALIDATION_SOURCE,' + //0
      'A.RDB$DEFAULT_SOURCE,' +    //1
      'A.RDB$NULL_FLAG,' +         //2
      'A.RDB$FIELD_TYPE,' +       //3
      'A.RDB$FIELD_SUB_TYPE, ' +    //4
      'A.RDB$FIELD_PRECISION, ' +  //5
      'A.RDB$FIELD_SCALE,' +       //6
      'A.RDB$FIELD_LENGTH, ' +      //7
      'A.RDB$CHARACTER_SET_ID, ' + //8
      'B.RDB$CHARACTER_SET_NAME ' + //9
      'FROM RDB$FIELDS A ' +
      'LEFT JOIN RDB$CHARACTER_SETS B ' +
      'ON A.RDB$CHARACTER_SET_ID = B.RDB$CHARACTER_SET_ID ' +
      'WHERE RDB$FIELD_NAME = ?';
    qry.Prepare;
    qry.ParamAsString(0,ADomainName);
    qry.ExecSQL;
    Result := 'CREATE DOMAIN ' + Trim(ADomainName) + ' AS ' +
      FieldType(qry.FieldAsLong(3), qry.FieldAsLong(4), qry.FieldAsLong(5),
      qry.FieldAsLong(6), qry.FieldAsLong(7));
    if (not qry.FieldIsNull(8)) and (qry.FieldAsInteger(8) <> 0) then
      Result := Result + ' CHARACTER SET ' + trim(qry.FieldAsString(9));
    if not qry.FieldIsNull(1) then
      Result := Result + NEW_LINE + Trim(qry.FieldAsString(1));
    if qry.FieldAsLong(2) = 1 then
      Result := Result + NEW_LINE + 'NOT NULL';
    if not qry.FieldIsNull(0) then
      Result := Result + NEW_LINE + Trim(qry.FieldAsString(0));
    qry.UnPrepare;
    Result := Result + ';';
    if FTransaction.InTransaction then FTransaction.Commit;
  finally
    qry.Free;
  end;
end;

//-------------------------------------------------------------------------------

function TFBLMetadata.UDFSource(const AUDFName: string): string;
var
  qry: TFBLDsql;
  sText, ParamType, ResultType: string;
  ModuleName, EntryPoint: string;
  Return_Argument: integer;
begin
  qry := TFBLDsql.Create(self);
  Result := '';
  sText := '';
  ParamType := '';
  ResultType := '';
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT RDB$MODULE_NAME,RDB$ENTRYPOINT, RDB$RETURN_ARGUMENT ' +
      'FROM RDB$FUNCTIONS WHERE RDB$FUNCTION_NAME = ?';
    qry.Prepare;
    qry.ParamAsString(0,AUDFName);
    qry.ExecSQL;
    ModuleName := trim(qry.FieldAsString(0));
    EntryPoint := trim(qry.FieldAsString(1));
    Return_Argument := qry.FieldAsLong(2);
    qry.UnPrepare;
    qry.SQL.Text := 'SELECT ' +
      'RDB$FIELD_TYPE ,' +       //0
      'RDB$FIELD_SUB_TYPE ,' +   //1
      'RDB$FIELD_PRECISION ,' +  //2
      'RDB$FIELD_SCALE ,' +      //3
      'RDB$FIELD_LENGTH ,' +     //4
      'RDB$ARGUMENT_POSITION ,' + //5
      'RDB$MECHANISM ' +          //6
      'FROM ' +
      'RDB$FUNCTION_ARGUMENTS ' +
      'WHERE RDB$FUNCTION_NAME = ? ' +
      'ORDER BY RDB$ARGUMENT_POSITION';
    qry.Prepare;
    qry.ParamAsString(0,AUDFName);
    qry.ExecSQL;

    sText := 'DECLARE EXTERNAL FUNCTION ' + Trim(AUDFName) + NEW_LINE;
    while not qry.EOF do 
    begin
      ParamType := FieldType(qry.FieldAsLong(0), qry.FieldAsLong(1), qry.FieldAsLong(2),
        qry.FieldAsLong(3), qry.FieldAsLong(4));

      if qry.FieldAsLong(5) = Return_Argument then
      begin
        if qry.FieldAsLong(6) = 0 then
          ResultType := 'RESULTS ' + ParamType + ' BY VALUE' + NEW_LINE
        else if qry.FieldAsLong(6) = -1 then
          ResultType := 'RESULTS ' + ParamType + ' FREE_IT' + NEW_LINE
        else
          ResultType := 'RESULTS ' + ParamType + NEW_LINE;
        qry.Next;
      end
      else
      begin
        sText := sText + ParamType;
        qry.Next;
        if qry.EOF then
          sText := sText + NEW_LINE
        else
          sText := sText + ',';
      end;
    end;
    qry.UnPrepare;
    sText := sText + ResultType;
    stext := stext + 'ENTRY_POINT ''' + EntryPoint + ''' ';
    stext := stext + 'MODULE_NAME ''' + ModuleName + '''; ';
    Result := sText;
    if FTransaction.InTransaction then FTransaction.Commit;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.IndexSource(const ATableName: string): string;
var
  qry: TFBLDsql;
begin
  qry := TFBLDsql.Create(self);
  Result := '';
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT ' +
      'RDB$INDEX_NAME ,' +       //0
      'RDB$UNIQUE_FLAG ,' +      //1
      'RDB$INDEX_INACTIVE, ' +   //2
      'RDB$INDEX_TYPE ' +        //3
      'FROM ' +
      'RDB$INDICES A WHERE (A.RDB$RELATION_NAME = ?) ' +
      'AND A.RDB$INDEX_NAME NOT IN(SELECT R.RDB$INDEX_NAME FROM ' +
      'RDB$RELATION_CONSTRAINTS R WHERE R.RDB$INDEX_NAME IS NOT NULL)';
    qry.Prepare;
    qry.ParamAsString(0,ATableName);
    qry.ExecSQL;

    while not qry.EOF do 
    begin
      Result := Result + 'CREATE ';
      if qry.FieldAsLong(1) = 1 then
        Result := Result + 'UNIQUE ';
      if Qry.FieldAsLong(3) = 1 then
        Result := Result + 'DESC ';

      Result := Result + 'INDEX ' + trim(Qry.FieldAsString(0)) + NEW_LINE +
        INTEND + 'ON ' + Trim(ATableName) + '(' +
        FieldsInIndex(Qry.FieldAsString(0)) + ');' + NEW_LINE;
      Qry.Next;
    end;
    qry.UnPrepare;
    Result := Trim(Result);
    if Ftransaction.InTransaction then FTransaction.Commit
    finally
      qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.PrimaryKeyConstraintSource(const ATableName: string): string;
var
  qry: TFBLDsql;
begin
  qry := TFBLDsql.Create(self);
  Result := '';
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT ' +
      'RDB$CONSTRAINT_NAME ,' +    //0
      'RDB$INDEX_NAME ' +          //1
      'FROM ' +
      'RDB$RELATION_CONSTRAINTS ' +
      'WHERE RDB$RELATION_NAME = ? AND ' +
      'RDB$CONSTRAINT_TYPE = ''PRIMARY KEY''';
    qry.Prepare;
    qry.ParamAsString(0,ATableName);
    qry.ExecSQL;

    if not Qry.EOF then
    begin
      Result := 'ALTER TABLE ' + Trim(ATableName) + ' ADD CONSTRAINT ' +
        trim(Qry.FieldAsString(0)) + ' PRIMARY KEY (' +
        FieldsInIndex(trim(Qry.FieldAsString(1))) + ');';
    end;
    Qry.UnPrepare;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.ForeignKeyConstraintSource(const ATableName: string): string;
var
  qry: TFBLDsql;
begin
  qry := TFBLDsql.Create(self);
  Result := '';
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    Qry.SQL.Text := 'SELECT ' +
      'RDB$CONSTRAINT_NAME ,' +
      'RDB$INDEX_NAME ' +
      'FROM ' +
      'RDB$RELATION_CONSTRAINTS WHERE RDB$RELATION_NAME = ? AND ' +
      'RDB$CONSTRAINT_TYPE = ''FOREIGN KEY''';
    Qry.Prepare;
    Qry.ParamAsString(0,ATableName);
    Qry.ExecSQL;
    while not Qry.EOF do
    begin
      Result := Result + 'ALTER TABLE ' + Trim(ATableName) + ' ADD CONSTRAINT ' +
        trim(Qry.FieldAsString(0)) + ' FOREIGN KEY (' +
        FieldsInIndex(trim(Qry.FieldAsString(1))) + ') ' + NEW_LINE + INTEND +
        RefConstraints(Qry.FieldAsString(0)) + ';' + NEW_LINE;
      qry.Next;
    end;
    Result := Trim(Result);
    Qry.UnPrepare;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.CheckConstraintSource(const ATableName: string): string;
var
  qry: TFBLDsql;
begin
  qry := TFBLDsql.Create(self);
  Result := '';
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT RDB$CONSTRAINT_NAME ' +  //0
      'FROM RDB$RELATION_CONSTRAINTS ' +
      'WHERE RDB$RELATION_NAME = ? AND ' +
      'RDB$CONSTRAINT_TYPE = ''CHECK''';
    qry.Prepare;
    qry.ParamAsString(0,ATableName);
    qry.ExecSQL;
    while not Qry.EOF do
    begin
      Result := Result + 'ALTER TABLE ' + Trim(ATableName) +
        ' ADD CONSTRAINT ' + trim(Qry.FieldAsString(0)) + ' ' +
        CheckConstraints(qry.FieldAsString(0)) + ';' + NEW_LINE;
      qry.Next;
    end;
    Result := trim(Result);
    Qry.UnPrepare;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.UniqueConstraintSource(const ATableName: string): string;
var
  qry: TFBLDsql;
begin
  qry := TFBLDsql.Create(self);
  Result := '';
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    Qry.SQL.Text := 'SELECT RDB$CONSTRAINT_NAME ,' + //0
      'RDB$INDEX_NAME ' +              //1
      'FROM ' +
      'RDB$RELATION_CONSTRAINTS WHERE RDB$RELATION_NAME = ? AND ' +
      'RDB$CONSTRAINT_TYPE = ''UNIQUE''';
    qry.Prepare;
    Qry.ParamAsString(0,ATableName);
    Qry.ExecSQL;
    while not Qry.EOF do
    begin
      Result := Result + 'ALTER TABLE ' + Trim(ATableName) +
        ' ADD CONSTRAINT ' + trim(Qry.FieldAsString(0)) + ' UNIQUE (' +
        FieldsInIndex(trim(Qry.FieldAsString(1))) + ');' + NEW_LINE;
      qry.Next;
    end;
    Result := Trim(Result);
    Qry.Prepare;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.ExceptionSource(const AExceptionName: string): string;
var
  qry: TFBLDsql;
begin
  qry := TFBLDsql.Create(self);
  Result := '';
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT RDB$MESSAGE FROM RDB$EXCEPTIONS WHERE RDB$EXCEPTION_NAME = ?';
    qry.Prepare;
    qry.ParamAsString(0,AExceptionName);
    qry.ExecSQL;
    Result := 'CREATE EXCEPTION ' + Trim(AExceptionName) + ' ''' +
      Trim(qry.FieldAsString(0)) + '''' + ';';
    qry.Prepare;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.GeneratorValue(const AGeneratorName: string): string;
var
  qry: TFBLDsql;
begin
  qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT GEN_ID(' + AGeneratorName + ',0) FROM ' +
      'RDB$DATABASE ';
    qry.ExecSQL;
    Result := qry.FieldAsString(0);
    qry.UnPrepare;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.TriggersInTable(const ATableName: string): TStringList;
var
  qry: TFBLDsql;
begin
  if FTriggersInTable = nil then FTriggersInTable := TStringList.Create;
  Result := FTriggersInTable;
  FTriggersInTable.Clear;
  qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not FTransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT RDB$TRIGGER_NAME FROM RDB$TRIGGERS ' +
      'WHERE RDB$RELATION_NAME = ? AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG <> 1) ' +
      'AND  RDB$TRIGGER_NAME ' +
      'NOT IN (SELECT RDB$CHECK_CONSTRAINTS.RDB$TRIGGER_NAME ' +
      'FROM RDB$CHECK_CONSTRAINTS)';
    qry.Prepare;
    qry.ParamAsString(0,ATableName);
    qry.ExecSQL;
    while not qry.EOF do
    begin
      FTriggersInTable.Add(Trim(Qry.FieldAsString(0)));
      qry.Next;
    end;
    qry.UnPrepare;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.GetDefaultCharset: string;
var
  qry: TFBLDsql;
begin
  Result := 'NONE';
  qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not Ftransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT RDB$CHARACTER_SET_NAME FROM RDB$DATABASE';
    qry.ExecSQL;
    if not qry.FieldIsNull(0) then
      Result := trim(qry.FieldAsString(0));
    qry.Close;
    FTransaction.Commit;
  finally
    qry.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFBLMetadata.GetUser: string;
var
  qry: TFBLDsql;
begin
  Result := '';
  qry := TFBLDsql.Create(self);
  try
    FTransaction.Database := FDatabase;
    Qry.Transaction := FTransaction;
    if not Ftransaction.InTransaction then FTransaction.StartTransaction;
    qry.SQL.Text := 'SELECT USER FROM RDB$DATABASE';
    qry.ExecSQL;
    if not qry.FieldIsNull(0) then
      Result := trim(qry.FieldAsString(0));
    qry.Close;
    FTransaction.Commit;
  finally
    qry.Free;
  end;
end;


end.
