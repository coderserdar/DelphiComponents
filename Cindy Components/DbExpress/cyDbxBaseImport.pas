unit cyDbxBaseImport;

{   Component(s):
    TcyDbxBaseImport class

    Description:
    TcyDbxBaseImport for TcyDbxBaseImportDataset/TcyDbxBaseImportXML



    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, SysUtils, Db, DBClient, Provider, SqlExpr, {$IFDEF DELPHI2007_OR_ABOVE} DBXCommon, {$ENDIF} cyDBX, cyDbxBaseTable;

type
  TcyDbxBaseImport = class;
  TcyDBXFields = class;
  TcyDBXFieldItem = class;

  TcyDBXFieldType = class(TPersistent)
  private
    FOwner: TcyDBXFieldItem;
    FSqlDataType: String;
    FDataType: TFieldType;
    FSize: Integer;
    FPrecision: Integer;
    procedure SetDataType(const Value: TFieldType);
    procedure SetSize(const Value: Integer);
    procedure SetPrecision(const Value: Integer);
    procedure GenerateSqlDataType;
  protected
  public
    constructor Create(AOwner: TcyDBXFieldItem); virtual;
  published
    property DataType: TFieldType read FDataType write SetDataType default ftString;
    property Size: Integer read FSize write SetSize default 10;
    property Precision: Integer read FPrecision write SetPrecision default 2;
    property SQLDataType: String read FSqlDataType write FSqlDataType;          // Exemple: INT(10)
  end;

  TcyDBXFieldItem = class(TCollectionItem)
  private
    FOwner: TcyDBXFields;
    FFieldName: String;
    FTag: Integer;
    FNotNull: Boolean;
    FUnsigned: Boolean;
    FBinary: Boolean;
    FZeroFill: Boolean;
    FAutoIncremental: Boolean;
    FIndexType: TDbxIndexType;
    FFieldType: TcyDBXFieldType;
    FIndexAscending: Boolean;
    FOrigin: String;
    procedure SetFieldType(const Value: TcyDBXFieldType);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property FieldName: String read FFieldName write FFieldName;
    property Origin: String read FOrigin write FOrigin;
    property FieldType: TcyDBXFieldType read FFieldType write SetFieldType;
    property IndexType: TDbxIndexType read FIndexType write FIndexType default itNone;
    property IndexAscending: Boolean read FIndexAscending write FIndexAscending default true;
    property AutoIncremental: Boolean read FAutoIncremental write FAutoIncremental default false;
    property Binary: Boolean read FBinary write FBinary default false;
    property NotNull: Boolean read FNotNull write FNotNull default false;
    property Unsigned: Boolean read FUnsigned write FUnsigned default false;
    property ZeroFill: Boolean read FZeroFill write FZeroFill default false;
    property Tag: Integer read FTag write FTag default 0;
  end;

  TcyDBXFieldItemClass = class of TcyDBXFieldItem;

  TcyDBXFields = Class(TCollection)
  private
    FOwner: TcyDbxBaseImport;
    function GetDBXFieldItem(Index: Integer): TcyDBXFieldItem;
  protected
    function GetOwner: TPersistent; Override;
    procedure Update(Item: TCollectionItem); Override;
  public
    constructor Create(aOwner: TcyDbxBaseImport; DBXFieldItemClass: TcyDBXFieldItemClass);
    function Add: TcyDBXFieldItem;
    function FindFieldIndex(aFieldName: String): Integer;
    procedure Delete(Index: Integer);
    property Fields[Index: Integer]: TcyDBXFieldItem read GetDBXFieldItem; default;
  end;

  TcyDbxBaseImport = class(TComponent)
  private
    FTableName: String;
    FSQLConnection: TSQLConnection;
    FPrimaryKey: String;
    FCharset: String;
    FEngine: String;
    FFields: TcyDBXFields;
    FImportErrorEvent: TReconcileErrorEvent;
    FOptions: TcyDbxSchemaOptions;
    procedure SetSQLConnection(const Value: TSQLConnection);
    procedure SetFields(const Value: TcyDBXFields);
    procedure SetPrimaryKey(const Value: String);
    procedure SetOptions(const Value: TcyDbxSchemaOptions);
    procedure SetTableName(const Value: String);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ImportReconcileErrorEvent(DataSet: TCustomClientDataSet; E: EReconcileError; UpdateKind: TUpdateKind; var Action: TReconcileAction);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GenerateFields(ClearCurrentFields: Boolean); virtual;
    function TableExists: Boolean;
    function ColumnExists(aColumnName: String): Boolean;
    function GetCreateTableSQL: String;
    procedure CreateTable;
    function GetAlterTableSQL: String;
    procedure AlterTable;
    procedure ImportRecords(const MaxErrors: Integer = -1); virtual;
  published
    property Fields: TcyDBXFields read FFields write SetFields;
    property PrimaryKey: String read FPrimaryKey write SetPrimaryKey;
    property TableName: String read FTableName write SetTableName;
    property Engine: String read FEngine write FEngine;
    property Charset: String read FCharset write FCharset;
    property Options: TcyDbxSchemaOptions read FOptions write SetOptions;
    property SQLConnection: TSQLConnection read FSQLConnection write SetSQLConnection;
    property OnImportErrorEvent: TReconcileErrorEvent read FImportErrorEvent write FImportErrorEvent;
  end;

implementation

{ TcyDBXFieldType }
constructor TcyDBXFieldType.Create(AOwner: TcyDBXFieldItem);
begin
  FOwner := AOwner;
  FSize := 10;
  FPrecision := 2;
  FDataType := ftString;
end;

procedure TcyDBXFieldType.SetDataType(const Value: TFieldType);
begin
  FDataType := Value;

  // Set some properties regarding Field type:
//  if FDataType = ftBoolean then  // Signed by MySQL default ...
//    FOwner.FUnsigned := true;

  if FDataType in [ftWord {$IFDEF UNICODE}, ftLongWord{$ENDIF}] then
    FOwner.FUnsigned := true;

  if not (csLoading in FOwner.FOwner.FOwner.ComponentState) then
    GenerateSqlDataType;
end;

procedure TcyDBXFieldType.SetPrecision(const Value: Integer);
begin
  FPrecision := Value;
  if not (csLoading in FOwner.FOwner.FOwner.ComponentState) then
    GenerateSqlDataType;
end;

procedure TcyDBXFieldType.SetSize(const Value: Integer);
begin
  FSize := Value;
  if not (csLoading in FOwner.FOwner.FOwner.ComponentState) then
    GenerateSqlDataType;
end;

procedure TcyDBXFieldType.GenerateSqlDataType;
begin
  case FDataType of
    ftString, ftWideString, ftVariant:
      FSqlDataType := 'VARCHAR(' + intToStr(FSize) + ')';

    ftFixedChar{$IFDEF UNICODE}, ftFixedWideChar {$ENDIF}:
      FSqlDataType := 'VARCHAR(1)';

    ftAutoInc, ftSmallint, ftInteger, ftWord, ftLargeint {$IFDEF UNICODE}, ftShortint, ftByte, ftLongWord {$ENDIF}:
      FSqlDataType := 'INT(' + intToStr(FSize) + ')';

    ftBoolean:
      FSqlDataType := 'TINYINT(1)';

    // Note: in MySQL5, FLOAT is deprecated and DOUBLE don' t need parameters and can be set "DOUBLE"
    ftFloat, ftCurrency {$IFDEF UNICODE}, ftExtended {$ENDIF}:
      FSqlDataType := 'DOUBLE(' + intToStr(FSize) + ',' + intToStr(FPrecision) + ')';

    ftFMTBcd:
      FSqlDataType := 'DECIMAL(' + intToStr(FSize) + ',' + intToStr(FPrecision) + ')';

    ftDate:
      FSqlDataType := 'DATE';

    ftTime:
      FSqlDataType := 'TIME';

    ftBlob, ftGraphic, ftTypedBinary:
      FSqlDataType := 'MEDIUMBLOB';

    ftMemo, ftFmtMemo {$IFDEF UNICODE}, ftWideMemo {$ENDIF}:
      FSqlDataType := 'MEDIUMTEXT';
    else
      FSqlDataType := 'VARCHAR(45)';
  end;

{ Not Handled for now ...
    , , , ftBCD, , , ftDateTime, // 5..11
    ftBytes, ftVarBytes, , , // 12..18
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, , , // 19..24
    , ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, // 25..31
    , ftInterface, ftIDispatch, ftGuid, ftTimeStamp, , // 32..37
    , , ftOraTimeStamp, ftOraInterval, // 38..41
    ftConnection, ftParams, ftStream); //42..48
}
end;

{ TcyDBXFieldItem }
constructor TcyDBXFieldItem.Create(Collection: TCollection);
begin
  inherited;
  FOwner := TcyDBXFields(Collection);
  FFieldType := TcyDBXFieldType.Create(Self);
  FIndexType := itNone;
  FIndexAscending := true;
  FAutoIncremental := false;
  FBinary := false;
  FNotNull := false;
  FUnsigned := false;
  FZeroFill := false;
  FTag := 0;
end;

destructor TcyDBXFieldItem.Destroy;
begin
  FFieldType.Free;
  inherited;
end;

function TcyDBXFieldItem.GetDisplayName: string;
begin
  Result := FFieldName + ' ' + FFieldType.FSqlDataType;
  if FUnsigned then Result := Result + ' UNSIGNED';
  if FZeroFill then Result := Result + ' ZEROFILL';
  if FBinary   then Result := Result + ' BINARY';
  if FNotNull  then Result := Result + ' NOT NULL';
  if FAutoIncremental then Result := Result + ' AUTO_INCREMENT';
end;

procedure TcyDBXFieldItem.SetFieldType(const Value: TcyDBXFieldType);
begin
  FFieldType := Value;
end;

procedure TcyDBXFieldItem.Assign(Source: TPersistent);
begin
  inherited;

  if Source is TcyDBXFieldItem then
  begin
    IndexType := TcyDBXFieldItem(Source).FIndexType;
    AutoIncremental := TcyDBXFieldItem(Source).FAutoIncremental;
    Binary := TcyDBXFieldItem(Source).FBinary;
    NotNull := TcyDBXFieldItem(Source).FNotNull;
    Unsigned := TcyDBXFieldItem(Source).FUnsigned;
    ZeroFill := TcyDBXFieldItem(Source).FZeroFill;
    Tag := TcyDBXFieldItem(Source).FTag;
  end;
end;


{ TcyDBXFields }
constructor TcyDBXFields.Create(aOwner: TcyDbxBaseImport; DBXFieldItemClass: TcyDBXFieldItemClass);
begin
  inherited Create(DBXFieldItemClass);
  FOwner := aOwner;
end;

function TcyDBXFields.Add: TcyDBXFieldItem;
begin
  Result := TcyDBXFieldItem(inherited Add);
  Result.Changed(false);      // It will call TcyDBXFields.Update only at run-time!
end;

procedure TcyDBXFields.Delete(Index: Integer);
begin
  Inherited;
  Update(Nil);
end;

function TcyDBXFields.FindFieldIndex(aFieldName: String): Integer;
var f: Integer;
begin
  Result := -1;
  aFieldName := AnsiUpperCase(aFieldName);

  for f := 0 to Count-1 do
    if AnsiUpperCase(Fields[f].FieldName) = aFieldName then
    begin
      Result := f;
      Break;
    end;
end;

function TcyDBXFields.GetDBXFieldItem(Index: Integer): TcyDBXFieldItem;
begin
  Result := TcyDBXFieldItem(inherited Items[Index]);
end;

function TcyDBXFields.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// Event Called by setting properties/events of TcyDBXFieldItem :
procedure TcyDBXFields.Update(Item: TCollectionItem);
begin
  inherited;

end;



{ TcyDbxBaseImport }
constructor TcyDbxBaseImport.Create(AOwner: TComponent);
begin
  inherited;
  FFields := TcyDBXFields.Create(self, TcyDBXFieldItem);
  FOptions := TcyDbxSchemaOptions.Create(self);
end;

destructor TcyDbxBaseImport.Destroy;
begin
  FOptions.Free;
  FFields.Free;
  FFields := Nil;

  inherited;
end;

procedure TcyDbxBaseImport.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if FSQLConnection <> nil then
      if AComponent = FSQLConnection then
        FSQLConnection := nil;
  end;
end;

procedure TcyDbxBaseImport.SetTableName(const Value: String);
begin
  FTableName := Value;

  if not (csLoading in ComponentState) then
    GenerateFields(True);
end;

procedure TcyDbxBaseImport.SetFields(const Value: TcyDBXFields);
begin
  FFields := Value;
end;

procedure TcyDbxBaseImport.SetOptions(const Value: TcyDbxSchemaOptions);
begin
  FOptions := Value;
end;

procedure TcyDbxBaseImport.SetPrimaryKey(const Value: String);
var f: Integer;
begin
  // Remove FPrimaryKey as Primary key :
  f := FFields.FindFieldIndex(FPrimaryKey);
  if f <> -1 then
    FFields[f].FIndexType := itNone;

  FPrimaryKey := Value;

  // Set Field Primary Key :
  f := FFields.FindFieldIndex(FPrimaryKey);
  if f <> -1 then
    FFields[f].FIndexType := itPrimaryKey;
end;

procedure TcyDbxBaseImport.SetSQLConnection(const Value: TSQLConnection);
begin
  FSQLConnection := Value;

  if Value <> nil then
    Value.FreeNotification(Self);  // Inform cyDbxUpdateSql if component removed ...
end;

function TcyDbxBaseImport.TableExists: Boolean;
var
  aQuery: TSQLQuery;
begin
  Result := false;
  if FTableName = '' then Exit;

  aQuery := TSQLQuery.Create(Nil);
  aQuery.SQLConnection := FSQLConnection;

  try
    if FOptions.MySQL_UseInformationSchema then
    begin
      aQuery.SQL.Text := 'SELECT DISTINCT TABLE_NAME FROM information_schema.columns WHERE TABLE_SCHEMA = '
                          + SQLGetStringExpr(FSQLConnection.Params.Values['Database'], false, 0, GetDbxConnectionDriver(FSQLConnection))
                          + ' AND TABLE_NAME = ' + SQLGetStringExpr(FTableName, false, 0, GetDbxConnectionDriver(FSQLConnection));
    end
    else begin
      // Not working on MySQL prior 5.x and on Delphi 7:
      aQuery.SQL.Text := 'SHOW TABLES LIKE ' + SQLGetStringExpr(FTableName, false, 0, GetDbxConnectionDriver(FSQLConnection));
    end;

    aQuery.Tag := aQuery.ExecSQL(True);         // Returns -1 if fails, else returns records number
    if aQuery.Tag > 0 then
      Result := true;
  finally
  end;

  aQuery.Free;
end;

function TcyDbxBaseImport.ColumnExists(aColumnName: String): Boolean;
var
  aQuery: TSQLQuery;
begin
  Result := false;
  if aColumnName = '' then Exit;
  if not TableExists then Exit;

  aQuery := TSQLQuery.Create(Nil);
  aQuery.SQLConnection := FSQLConnection;

  try
    if FOptions.MySQL_UseInformationSchema then
    begin
      aQuery.SQL.Text := 'SELECT COLUMN_NAME FROM information_schema.columns WHERE TABLE_SCHEMA = '
                           + SQLGetStringExpr(FSQLConnection.Params.Values['Database'], false, 0, GetDbxConnectionDriver(FSQLConnection))
                           + ' AND TABLE_NAME = ' + SQLGetStringExpr(FTableName, false, 0, GetDbxConnectionDriver(FSQLConnection))
                           + ' AND COLUMN_NAME = ' + SQLGetStringExpr(aColumnName, false, 0, GetDbxConnectionDriver(FSQLConnection));
    end
    else begin
      // Not working on MySQL prior 5.x :
      aQuery.SQL.Text := 'SHOW COLUMNS FROM ' + FTableName + ' LIKE ' + SQLGetStringExpr(aColumnName, false, 0, GetDbxConnectionDriver(FSQLConnection));
    end;

    aQuery.Tag := aQuery.ExecSQL(True);         // Returns -1 if fails, else returns records number
    if aQuery.Tag > 0 then
      Result := true;
  finally
  end;

  aQuery.Free;
end;

procedure TcyDbxBaseImport.GenerateFields(ClearCurrentFields: Boolean);
begin
  //
end;

function TcyDbxBaseImport.GetCreateTableSQL: String;
var
  f: Integer;
  PrimaryKeys, FieldExtras, Str: String;

        procedure IncludeToPrimaryKey(aFieldName: String);
        begin
          if PrimaryKeys <> '' then
            PrimaryKeys := PrimaryKeys + ', ';
          PrimaryKeys := PrimaryKeys + '`' + aFieldName + '`';
        end;

begin
  Result := '';
  if FTableName = '' then Exit;

  PrimaryKeys := '';
  Result := 'CREATE TABLE `' + FTableName + '` (';

  // Add primary key as a field if it is not in the Fields list :
  if FPrimaryKey <> '' then
  begin
    f := FFields.FindFieldIndex(FPrimaryKey);
    if f = -1 then
    begin
      // *** PK is not in FFields *** //
      Result := Result + #13#10 + '`' + FPrimaryKey + '` INT NOT NULL AUTO_INCREMENT,';
      IncludeToPrimaryKey(FPrimaryKey);
    end;
  end;

  // Add fields :
  for f := 0 to FFields.Count-1 do
  begin
    FieldExtras := '';
    if FFields[f].FUnsigned then FieldExtras := FieldExtras + ' UNSIGNED';
    if FFields[f].FZeroFill then FieldExtras := FieldExtras + ' ZEROFILL';
    if FFields[f].FBinary   then FieldExtras := FieldExtras + ' BINARY';
    if FFields[f].FNotNull  then FieldExtras := FieldExtras + ' NOT NULL';
    if FFields[f].FAutoIncremental then FieldExtras := FieldExtras + ' AUTO_INCREMENT';

    Result := Result + #13#10 + '`' + FFields[f].FFieldName + '` ' + FFields[f].FieldType.FSqlDataType + FieldExtras + ',';

    if FFields[f].FIndexType = itPrimaryKey then
      IncludeToPrimaryKey(FFields[f].FFieldName);
  end;

  // Add primary key statement :
  if PrimaryKeys <> '' then
    Result := Result + #13#10 + 'PRIMARY KEY (' + PrimaryKeys + '),';

  // Add indexes :
  for f := 0 to FFields.Count-1 do
  begin
    if FFields[f].FIndexAscending
    then Str := 'ASC'
    else Str := 'DESC';

    case FFields[f].FIndexType of
      itIndexed: Result := Result + #13#10 + 'INDEX `' + FFields[f].FFieldName + '` (`' + FFields[f].FFieldName + '` ' + Str + '),';
      itUnique: Result := Result + #13#10 + 'UNIQUE INDEX `' + FFields[f].FFieldName + '` (`' + FFields[f].FFieldName + '` ' + Str + '),';
    end;
  end;

  // Replace last ',' by ')' :
  Result[Length(Result)] := ')';

  // Others :
  if FEngine <> '' then
    Result := Result + #13#10 + 'ENGINE = ' + FEngine;

  if FCharset <> '' then
    Result := Result + #13#10 + 'DEFAULT CHARACTER SET = ' + FCharset;
end;

procedure TcyDbxBaseImport.CreateTable;
var SQLCode: String;
begin
  if not Assigned(FSQLConnection) then
    raise Exception.Create('Not SQLConnection specified !');

  SQLCode := GetCreateTableSQL;
  if SQLCode = '' then Exit;

  cyDBX.SQLExecute(FSQLConnection, SQLCode);
end;

function TcyDbxBaseImport.GetAlterTableSQL: String;
var
  f: Integer;
  FieldExtras, AfterClause, Str: String;
begin
  Result := '';
  if FTableName = '' then Exit;

  Result := 'ALTER TABLE `' + FTableName + '`';

  // Add/modify fields :
  for f := 0 to FFields.Count-1 do
  begin
    FieldExtras := '';
    if FFields[f].FUnsigned then FieldExtras := FieldExtras + ' UNSIGNED';
    if FFields[f].FZeroFill then FieldExtras := FieldExtras + ' ZEROFILL';
    if FFields[f].FBinary   then FieldExtras := FieldExtras + ' BINARY';
    if FFields[f].FNotNull  then FieldExtras := FieldExtras + ' NOT NULL';
    if FFields[f].FAutoIncremental then FieldExtras := FieldExtras + ' AUTO_INCREMENT';

    if not ColumnExists(FFields[f].FOrigin) then
    begin
      if f = 0
      then AfterClause := ''
      else AfterClause := ' AFTER `' + FFields[f-1].FFieldName + '`';

      Result := Result + #13#10 + 'ADD COLUMN `' + FFields[f].FFieldName + '` ' + FFields[f].FieldType.FSqlDataType + FieldExtras + AfterClause + ',';
    end
    else
      Result := Result + #13#10 + 'CHANGE COLUMN `' + FFields[f].FOrigin + '` `' + FFields[f].FFieldName + '` ' + FFields[f].FieldType.FSqlDataType + FieldExtras + ',';
  end;

  // Delete last ',' :
  Delete(Result, Length(Result), 1);
end;

procedure TcyDbxBaseImport.AlterTable;
var SQLCode: String;
begin
  if not Assigned(FSQLConnection) then
    raise Exception.Create('No SQLConnection specified !');

  SQLCode := GetAlterTableSQL;
  if SQLCode = '' then Exit;

  cyDBX.SQLExecute(FSQLConnection, SQLCode);
end;

procedure TcyDbxBaseImport.ImportReconcileErrorEvent(DataSet: TCustomClientDataSet; E: EReconcileError; UpdateKind: TUpdateKind; var Action: TReconcileAction);
begin
  if Assigned(FImportErrorEvent) then
    FImportErrorEvent(Dataset, E, UpdateKind, Action);
end;

procedure TcyDbxBaseImport.ImportRecords(const MaxErrors: Integer = -1);
begin
  //
end;

end.
