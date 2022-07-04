unit cyDBX;

{   Unit cyDBX

    Description:
    Unit with ClientDataset/DB Express functions

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

uses
  {$IFDEF MsWindows}
  Dialogs,
  {$ELSE}
  fmx.Dialogs, System.UITypes,
  {$ENDIF}

  Classes, Variants, Graphics, SimpleDS, DB, DBClient, {$IFDEF DELPHI2007_OR_ABOVE} DBXCommon, {$ENDIF} Provider, SysUtils, SqlExpr, cyStrUtils, cyDb;

var
  vDbxErrorRecordLocked: String;  // Set on initialization section!

const
  cDbxErrorServerInsert = 'Error! cannot insert record!';
  cDbxErrorServerModify   = 'Error! cannot modify: record modified or deleted by another connection!';
  cDbxErrorServerDelete = 'Error! cannot delete: record modified or deleted by another connection!';

type
  TDbxConnectionDriver = (cdASA, cdASE, cdBlackfishSQL, cdDatasnap, cdDb2, cdFireBird, cdIbLiteToGo, cdInformix, cdInterbase, cdMSSQL, cdMSSQL9, cdMySQL, cdOracle, cdSqlLite);
  TDbxIndexType = (itNone, itPrimaryKey, itUnique, itIndexed);

type TDatasetProviderDefaultErrorHandler = class
public
  procedure ReconcileError(DataSet: TCustomClientDataSet; E: EReconcileError; UpdateKind: TUpdateKind;
    var Action: TReconcileAction);
end;

{Utilities}
function GetDbxConnectionDriver(FromConnection: TSQLConnection): TDbxConnectionDriver;
function BackQuotedStr(aIdentifier: String): String;
function SQLIdentifier(aIdentifier: String; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
function ExtractSQLTableNames(fromSQL: String; const OnlyFirstFound: Boolean = false): String;
function ExtractSQLFields(fromSQL: String): String;
function ExtractSQLOrderByFields(fromSQL: String): String;
// Get lock string id for mutex lock :
function GetInternalRecordLockId(aDatabaseName, aTableName, aPrimaryKeyValue: String): String;
// Get Connection ID of user with specified lock
function GetUsedLockConnectionID(aConnection: TSQLConnection; const aLockId: String): String;
// Know if LockId is in use :
function IsFreeLockId(aConnection: TSQLConnection; const aLockId: String): Boolean;

{SQL Expressions}
function SQLGetFieldValueExpr(aField: TField; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
function SQLGetStringExpr(Value: String; const TrimValue: Boolean = true; const MaxFieldSize: Integer = 0; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
function SQLGetDateExpr(Value: TDateTime; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
function SQLGetTimeExpr(Value: TDateTime; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
function SQLGetDateTimeExpr(Value: TDateTime; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
function SQLGetFloatExpr(Value: Extended; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL; const Precision: Integer = 10; const Digits: Integer = 10): String;
function SQLGetBoolExpr(Value: Boolean; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;


{SQL Parameters}
function SQLGetDateParam(ParamValue: TDateTime; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
function SQLGetTimeParam(ParamValue: TDateTime; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
function SQLGetFloatParam(ParamValue: Extended; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;


{SQL commands}
function SQLGetInsertFromCurrentRecord(fromDataset: TDataset; const SQLTableName: string; var ResultSQL: String; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): Boolean;

// Execute commands (return number of rows affected) :
function SQLExecute(aConnection: TSQLConnection; SQL: TStrings): Integer; overload;
function SQLExecute(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                    const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): Integer; overload;

{$IFDEF DELPHI2007_OR_ABOVE}

function SQLExecuteUpdate(aConnection: TSQLConnection; TableName, WhereClause: string; FieldsList, ValuesList: TStrings; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): Integer;

// Specify an Insert command and the function will retrieve the Primary key of the inserted record:
function SQLExecuteInsert(aConnection: TSQLConnection; TableName: string; FieldsList, ValuesList: TStrings; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String; overload;
function SQLExecuteInsert(aConnection: TSQLConnection; SQL: TStrings; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String; overload;
function SQLExecuteInsert(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                    const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = '';
                    const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String; overload;
{$ENDIF}

function SQLGetPrimaryKey(aConnection: TSQLConnection; aTableName: String): String;

// Open query and returns first result field value:
function SQLReturnFieldValue(aConnection: TSQLConnection; SQL: TStrings): Variant; overload;
function SQLReturnFieldValue(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): Variant; overload;
function SQLReturnFieldValueAsString(aConnection: TSQLConnection; SQL: TStrings): string; overload;
function SQLReturnFieldValueAsString(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): String; overload;
function SQLReturnFieldValueAsInteger(aConnection: TSQLConnection; SQL: TStrings): Integer; overload;
function SQLReturnFieldValueAsInteger(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): Integer; overload;
function SQLReturnFieldValueAsFloat(aConnection: TSQLConnection; SQL: TStrings): Extended; overload;
function SQLReturnFieldValueAsFloat(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): Extended; overload;
function SQLReturnFieldValueAsBoolean(aConnection: TSQLConnection; SQL: TStrings; const DefaultValue: Boolean = false): Boolean; overload;
function SQLReturnFieldValueAsBoolean(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''; const DefaultValue: Boolean = false): Boolean; overload;
function SQLReturnFieldValueAsDateTime(aConnection: TSQLConnection; SQL: TStrings): TDateTime; overload;
function SQLReturnFieldValueAsDateTime(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): TDateTime; overload;

// Get last generated AutoInc value :
function SQLExecute_GetLastInsertID(aConnection: TSQLConnection; const SQLCommand: String = 'SELECT last_insert_id()'): String;
{ You need to include [poPropogateChanges] to TDatasetProvider.Options and call this function on TDatasetProvider.AfterUpdateRecord like this:
  if UpdateKind = ukInsert then
    DeltaDS.FieldByName('YourIndexFieldName').NewValue := SQLExecute_GetLastInsertID(SqlConnection1);
}



{TSimpleDataset functions}
// Apply updates and refresh SimpleDataset:
procedure SimpleDS_Refresh(aSimpleDataSet: TSimpleDataSet; const ApplyUpdates: Boolean = true);
// Apply avaible updates :
function SimpleDS_ApplyUpdates(aSimpleDataSet: TSimpleDataSet; const MaxErrors: Integer = -1): Integer;



{TClientDataset functions}
// Apply updates and refresh ClientDataset:
function ClientDS_Refresh(aClientDataSet: TClientDataSet; const ApplyUpdates: Boolean = true): Integer; overload;
function ClientDS_Refresh(aClientDataSet: TClientDataSet; aProvider: TDatasetProvider; const ApplyUpdates: Boolean = true): Integer; overload;
// Apply avaible updates :
function ClientDS_ApplyUpdates(aClientDataSet: TClientDataSet; const MaxErrors: Integer = -1): Integer;
// Get ClientDataset's TDatasetProvider :
function ClientDS_GetProvider(aClientDataSet: TCustomClientDataset): TDatasetProvider;

implementation

{ TDatasetProviderDefaultErrorHandler }
procedure TDatasetProviderDefaultErrorHandler.ReconcileError(DataSet: TCustomClientDataSet; E: EReconcileError; UpdateKind: TUpdateKind; var Action: TReconcileAction);
begin
  // Action default value = raSkip
  // Delphi self handling with : HandleReconcileError(DataSet, UpdateKind, E);

  case UpdateKind of
    ukInsert:
      begin
        {$IFDEF MsWindows}
        MessageDlg(cDbxErrorServerInsert, mtError, [mbOk], 0);
        {$ELSE}
        MessageDlg(cDbxErrorServerInsert, TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
        {$ENDIF}

        Action := raCancel;
      end;

    ukModify:
      begin
        {$IFDEF MsWindows}
        MessageDlg(cDbxErrorServerModify, mtError, [mbOk], 0);
        {$ELSE}
        MessageDlg(cDbxErrorServerModify, TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
        {$ENDIF}

        Action := raCancel;
      end;

    ukDelete:
      begin
        {$IFDEF MsWindows}
        MessageDlg(cDbxErrorServerDelete, mtError, [mbOk], 0);
        {$ELSE}
        MessageDlg(cDbxErrorServerDelete, TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
        {$ENDIF}
        Action := raCancel;
      end;

    else
      ShowMessage('Not handled UpadeKind!');
  end;

  // Erro ao alterar registo quando este está desactualizado:
  // Action := raSkip;     // Anula para o registo actual - necessita Rollback!
  // Action := raAbort;    // Anula para o registo actual e seguintes - necessita Rollback!
  // Action := raMerge;    // Altera sómente campos não alterados por autras conexões - Chama constantemente SimpleDataSet1ReconcileError!
  // Action := raCorrect;  // Altera o registo com dados no clientDataset - necessita Rollback!
  // Action := raCancel;   // Anula para o registo actual e retira alteração do delta - Mostra valores antes da alteração!
  // Action := raRefresh;  // Chama constantemente SimpleDataSet1ReconcileError!
end;

function GetDbxConnectionDriver(FromConnection: TSQLConnection): TDbxConnectionDriver;
var DriverName: String;
begin
  Result := cdMySQL;
  DriverName := AnsiUppercase(FromConnection.DriverName);
  if DriverName = 'ASA'          then Result := cdASA;
  if DriverName = 'ASE'          then Result := cdASE;
  if DriverName = 'BLACKFISHSQL' then Result := cdBlackfishSQL;
  if DriverName = 'DATASNAP'     then Result := cdDatasnap;
  if DriverName = 'DB2'          then Result := cdDb2;
  if DriverName = 'FIREBIRD'     then Result := cdFirebird;
  if DriverName = 'IBLITE/TOGO'  then Result := cdIbLiteToGo;
  if DriverName = 'INFORMIX'     then Result := cdInformix;
  if DriverName = 'INTERBASE'    then Result := cdInterbase;
  if DriverName = 'INTERBASE SERVER' then Result := cdInterbase;   // Added since XE4 ...
  if DriverName = 'MSSQL'        then Result := cdMSSQL;
  if DriverName = 'MSSQL9'       then Result := cdMSSQL9;          // Added since XE4 ...
  if DriverName = 'MYSQL'        then Result := cdMySQL;
  if DriverName = 'ORACLE'       then Result := cdOracle;
  if DriverName = 'SQLLITE'      then Result := cdSqlLite;         // Added since XE4 ...
end;

function SQLExecute_GetLastInsertID(aConnection: TSQLConnection; const SQLCommand: String = 'SELECT last_insert_id()'): String;
var aQuery: TSQLQuery;
begin
  Result := '';
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.SQL.Text := SQLCommand;
    aQuery.Active := true;
    Result := aQuery.Fields[0].AsString;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function BackQuotedStr(aIdentifier: String): String;
begin
  Result := aIdentifier;
  if Result = '' then
    Exit;

  if Result[1] <> '`' then
    Result := '`' + Result;

  if Result[Length(Result)] <> '`' then
    Result := Result + '`';
end;

function SQLIdentifier(aIdentifier: String; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
var
  _Start, _End: Char;
begin
  Result := aIdentifier;
  if Result = '' then
    Exit;

  case DbxConnectionDriver of
    cdMSSQL, cdMSSQL9:
    begin
      _Start := '[';
      _End   := ']';
    end;

    cdMySQL:
    begin
      _Start := '`';
      _End   := '`';
    end;

    else begin
      _Start := '`';
      _End   := '`';
    end;
  end;

  if Result[1] <> _Start then
    Result := _Start + Result;

  if Result[Length(Result)] <> _End then
    Result := Result + _End;
end;

function ExtractSQLTableNames(fromSQL: String; const OnlyFirstFound: Boolean = false): String;
var
  lengthFromSQL, i, j, p: Integer;
  Cont: Boolean;
begin
  Result := '';

  // No worry with enters:
  fromSQL := StringReplace(fromSQL, #$D#$A, ' ', [rfReplaceAll]);
  lengthFromSQL := length(fromSQL);

  // Locate FROM statement :
  p := pos(' FROM ', AnsiUpperCase(fromSQL));

  // Extract tablenames :
  if p = 0 then Exit;

  for i := p + 6 to lengthFromSQL do
    if fromSQL[i] = ' ' then
    begin
      // Check if there' s more any table :
      Cont := false;
      for j := i + 1 to lengthFromSQL do
        if not OnlyFirstFound then
          if fromSQL[j] <> ' ' then
          begin
            Cont := fromSQL[j] = ',';  // There' s more!
            Break;
          end;

      if not Cont then
        Break;
    end
    else
      Result := Result + fromSQL[i];
end;

function ExtractSQLFields(fromSQL: String): String;
var
  lengthFromSQL, i, j, p: Integer;
  Cont: Boolean;
begin
  Result := '';

  // No worry with enters:
  fromSQL := StringReplace(fromSQL, #$D#$A, ' ', [rfReplaceAll]);
  lengthFromSQL := length(fromSQL);

  // Locate SELECT statement :
  p := pos('SELECT ', AnsiUpperCase(fromSQL));

  // Extract fields :
  if p = 0 then Exit;

  for i := p + 7 to lengthFromSQL do
    if fromSQL[i] = ' ' then
    begin
      // Check if there' s more any field :
      Cont := false;
      for j := i + 1 to lengthFromSQL do
        if fromSQL[j] <> ' ' then
        begin
          Cont := fromSQL[j] = ',';  // There' s more!
          Break;
        end;

      if not Cont then
        Break;
    end
    else
      Result := Result + fromSQL[i];
end;

function ExtractSQLOrderByFields(fromSQL: String): String;
var
  lengthFromSQL, i, j, p: Integer;
  Cont: Boolean;
begin
  Result := '';

  // No worry with enters:
  fromSQL := StringReplace(fromSQL, #$D#$A, ' ', [rfReplaceAll]);
  lengthFromSQL := length(fromSQL);

  // Locate FROM statement :
  p := pos(' ORDER BY ', AnsiUpperCase(fromSQL));

  // Extract tablenames :
  if p = 0 then Exit;

  for i := p + 6 to lengthFromSQL do
    if fromSQL[i] = ' ' then
    begin
      // Check if there' s more any table :
      Cont := false;
      for j := i + 1 to lengthFromSQL do
        if fromSQL[j] <> ' ' then
        begin
          Cont := fromSQL[j] = ',';  // There' s more!
          Break;
        end;

      if not Cont then
        Break;
    end
    else
      Result := Result + fromSQL[i];
end;

function GetInternalRecordLockId(aDatabaseName, aTableName, aPrimaryKeyValue: String): String;
begin
  Result := SQLIdentifier(AnsiLowerCase(aDatabaseName)) + '.' + SQLIdentifier(AnsiLowerCase(aTableName)) + '.' + aPrimaryKeyValue;
end;

function GetUsedLockConnectionID(aConnection: TSQLConnection; const aLockId: String): String;
var
  v: Variant;
begin
  if aLockId = '' then
  begin
    Result := '';
    Exit;
  end;

  v := SQLReturnFieldValue(aConnection, 'SELECT IS_USED_LOCK(' + SQLGetStringExpr(aLockId, false, 0, GetDbxConnectionDriver(aConnection)) + ')');
  Result := v;
end;

function IsFreeLockId(aConnection: TSQLConnection; const aLockId: String): Boolean;
var
  v: Variant;
begin
  if aLockId = '' then
  begin
    Result := true;
    Exit;
  end;

  v := SQLReturnFieldValue(aConnection, 'SELECT IS_FREE_LOCK(' + SQLGetStringExpr(aLockId, false, 0, GetDbxConnectionDriver(aConnection)) + ')');
  Result := v = 1;
end;

function SQLGetInsertFromCurrentRecord(fromDataset: TDataset; const SQLTableName: string; var ResultSQL: String; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): Boolean;
var
  f: Integer;
  SQLFields, SQLValues: string;
  OutputFieldValue: string;
begin
  Result := false;
  if IsEmpty(fromDataset) then Exit;

  SQLFields := '';
  SQLValues := '';

  for f := 0 to fromDataset.Fields.Count-1 do
  begin
    if f <> 0 then
    begin
      SQLFields := SQLFields + ', ';
      SQLValues := SQLValues + ', ';
    end;

    // Add field name :
    SQLFields := SQLFields + SQLIdentifier(fromDataset.Fields[f].FieldName, DbxConnectionDriver);

    // Add field value :
    if fromDataset.Fields[f].Value <> Null
    then OutputFieldValue := SQLGetFieldValueExpr(fromDataset.Fields[f], DbxConnectionDriver)
    else OutputFieldValue := 'Null';

    SQLValues := SQLValues + OutputFieldValue;
  end;

  ResultSQL := 'INSERT INTO ' + SQLIdentifier(SQLTableName, DbxConnectionDriver) + ' (' + SQLFields + ') VALUES (' + SQLValues + ')';
  Result := SQLFields <> '';
end;

function SQLExecute(aConnection: TSQLConnection; SQL: TStrings): Integer;
var aQuery: TSQLQuery;
begin
  Result := 0;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.ParamCheck := false;
    aQuery.SQL.AddStrings(SQL);
    Result := aQuery.ExecSQL(True);  // Use True if query does not include any parameters ...
  finally
    aQuery.Free;
  end;
end;

function SQLExecute(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                    const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): Integer;
var aQuery: TSQLQuery;
begin
  Result := 0;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.ParamCheck := false;
    if SqlLine1 <> '' then aQuery.SQL.Add(SqlLine1);
    if SqlLine2 <> '' then aQuery.SQL.Add(SqlLine2);
    if SqlLine3 <> '' then aQuery.SQL.Add(SqlLine3);
    if SqlLine4 <> '' then aQuery.SQL.Add(SqlLine4);
    if SqlLine5 <> '' then aQuery.SQL.Add(SqlLine5);
    if SqlLine6 <> '' then aQuery.SQL.Add(SqlLine6);
    if SqlLine7 <> '' then aQuery.SQL.Add(SqlLine7);
    if SqlLine8 <> '' then aQuery.SQL.Add(SqlLine8);
    Result := aQuery.ExecSQL(True);  // Query does not include any parameters ...
  finally
    aQuery.Free;
  end;
end;

{$IFDEF DELPHI2007_OR_ABOVE}
function SQLExecuteUpdate(aConnection: TSQLConnection; TableName, WhereClause: string; FieldsList, ValuesList: TStrings; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): Integer;
var
  SQLFieldsAndValues: string;
  i: Integer;

  aQuery: TSQLQuery;
  TmpTransaction: TDBXTransaction;
  TransactionCreated: Boolean;
begin
  Result := 0;
  TransactionCreated := false;

  if (not aConnection.InTransaction) and aConnection.TransactionsSupported then
  begin
    TmpTransaction := aConnection.BeginTransaction;
    TransactionCreated := true;
  end;

  try
    aQuery := TSQLQuery.Create(Nil);

    try
      // Build fields and values list :
      SQLFieldsAndValues := '';
      for i := 0 to FieldsList.Count-1 do
      begin
        if i > 0 then
          SQLFieldsAndValues := SQLFieldsAndValues + ',';

        SQLFieldsAndValues := SQLFieldsAndValues + SQLIdentifier(FieldsList[i], DbxConnectionDriver) + '=' + ValuesList[i];
      end;

      // Execute command:
      aQuery.SQLConnection := aConnection;
      aQuery.ParamCheck := false;
      aQuery.SQL.Text := 'UPDATE ' + SQLIdentifier(TableName, DbxConnectionDriver) + ' SET ' + SQLFieldsAndValues + ' ' + WhereClause;
      Result := aQuery.ExecSQL(True);  // Use True if query does not include any parameters ...
    finally
      aQuery.Free;
    end;

  finally
    if TransactionCreated then
      aConnection.CommitFreeAndNil(TmpTransaction);
  end;
end;

function SQLExecuteInsert(aConnection: TSQLConnection; TableName: string; FieldsList, ValuesList: TStrings; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
var
  SQLFields, SQLValues: string;
  i: Integer;

  aQuery: TSQLQuery;
  TmpTransaction: TDBXTransaction;
  TransactionCreated: Boolean;
begin
  Result := '';

  TransactionCreated := false;
  if (not aConnection.InTransaction) and aConnection.TransactionsSupported then
  begin
    TmpTransaction := aConnection.BeginTransaction;
    TransactionCreated := true;
  end;

  try

    aQuery := TSQLQuery.Create(Nil);

    try
      // Build fields list :
      SQLFields := '';
      for i := 0 to FieldsList.Count-1 do
      begin
        if i > 0 then
          SQLFields := SQLFields + ',';

        SQLFields := SQLFields + SQLIdentifier(FieldsList[i], DbxConnectionDriver);
      end;

      // Build values list :
      SQLValues := '';
      for i := 0 to ValuesList.Count-1 do
      begin
        if i > 0 then
          SQLValues := SQLValues + ',';

        SQLValues := SQLValues + ValuesList[i];
      end;

      // Execute user insert command:
      aQuery.SQLConnection := aConnection;
      aQuery.ParamCheck := false;
      aQuery.SQL.Text := 'INSERT INTO ' + SQLIdentifier(TableName, DbxConnectionDriver) + ' (' + SQLFields + ') VALUES (' + SQLValues + ')';
      aQuery.ExecSQL(True);  // Use True if query does not include any parameters ...

      // Retrieve last_insert_id:
      case DbxConnectionDriver of
        cdMySQL:
        begin
          aQuery.Active := false;
          aQuery.SQL.Text := 'SELECT last_insert_id()';
          aQuery.Active := true;
          Result := aQuery.Fields[0].AsString;
          aQuery.Active := false;
        end;
      end;
    finally
      aQuery.Free;
    end;

  finally
    if TransactionCreated then
      aConnection.CommitFreeAndNil(TmpTransaction);
  end;
end;

function SQLExecuteInsert(aConnection: TSQLConnection; SQL: TStrings; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
var
  aQuery: TSQLQuery;
  TmpTransaction: TDBXTransaction;
  TransactionCreated: Boolean;
begin
  Result := '';

  TransactionCreated := false;
  if (not aConnection.InTransaction) and aConnection.TransactionsSupported then
  begin
    TmpTransaction := aConnection.BeginTransaction;
    TransactionCreated := true;
  end;

  try

    aQuery := TSQLQuery.Create(Nil);

    try
      // Execute user insert command:
      aQuery.SQLConnection := aConnection;
      aQuery.ParamCheck := false;
      aQuery.SQL.AddStrings(SQL);
      aQuery.ExecSQL(True);  // Use True if query does not include any parameters ...

      // Retrieve last_insert_id:
      case DbxConnectionDriver of
        cdMySQL:
        begin
        aQuery.Active := false;
        aQuery.SQL.Text := 'SELECT last_insert_id()';
        aQuery.Active := true;
        Result := aQuery.Fields[0].AsString;
        aQuery.Active := false;
        end;
      end;

    finally
      aQuery.Free;
    end;

  finally
    if TransactionCreated then
      aConnection.CommitFreeAndNil(TmpTransaction);
  end;
end;

function SQLExecuteInsert(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                    const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = '';
                    const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
var
  aQuery: TSQLQuery;
  TmpTransaction: TDBXTransaction;
  TransactionCreated: Boolean;
begin
  Result := '';

  TransactionCreated := false;
  if (not aConnection.InTransaction) and aConnection.TransactionsSupported then
  begin
    TmpTransaction := aConnection.BeginTransaction;
    TransactionCreated := true;
  end;

  try

    aQuery := TSQLQuery.Create(Nil);

    try
      // Execute user insert command:
      aQuery.SQLConnection := aConnection;
      aQuery.ParamCheck := false;
      if SqlLine1 <> '' then aQuery.SQL.Add(SqlLine1);
      if SqlLine2 <> '' then aQuery.SQL.Add(SqlLine2);
      if SqlLine3 <> '' then aQuery.SQL.Add(SqlLine3);
      if SqlLine4 <> '' then aQuery.SQL.Add(SqlLine4);
      if SqlLine5 <> '' then aQuery.SQL.Add(SqlLine5);
      if SqlLine6 <> '' then aQuery.SQL.Add(SqlLine6);
      if SqlLine7 <> '' then aQuery.SQL.Add(SqlLine7);
      if SqlLine8 <> '' then aQuery.SQL.Add(SqlLine8);
      aQuery.ExecSQL(True);  // Query does not include any parameters ...

      // Retrieve last_insert_id:
      case DbxConnectionDriver of
        cdMySQL:
        begin
          aQuery.Active := false;
          aQuery.SQL.Text := 'SELECT last_insert_id()';
          aQuery.Active := true;
          Result := aQuery.Fields[0].AsString;
          aQuery.Active := false;
        end;
      end;
    finally
      aQuery.Free;
    end;

  finally
    if TransactionCreated then
      aConnection.CommitFreeAndNil(TmpTransaction);
  end;
end;
{$ENDIF}

function SQLGetPrimaryKey(aConnection: TSQLConnection; aTableName: String): String;
var
  aQuery: TSQLQuery;
  Column_name: String;
begin
  Result := '';
  if aTableName = '' then Exit;

  Column_name := '';
  aQuery := TSQLQuery.Create(Nil);
  aQuery.SQLConnection := aConnection;

  case GetDbxConnectionDriver(aConnection) of
    cdASA: ;

    cdASE: ;

    cdBlackfishSQL: ;

    cdDatasnap: ;

    cdDb2: ;

    cdFireBird:
      begin
        aQuery.SQL.Clear;
        aQuery.SQL.Add('Select t1.RDB$INDEX_NAME as column_name');
        aQuery.SQL.Add('From RDB$INDICES t1');
        aQuery.SQL.Add('Inner join RDB$RELATION_CONSTRAINTS t2');
        aQuery.SQL.Add('on (t1.RDB$RELATION_NAME = t2.RDB$RELATION_NAME AND t1.RDB$INDEX_NAME = t2.RDB$INDEX_NAME)');
        aQuery.SQL.Add('Where t1.RDB$RELATION_NAME = '+ SQLIdentifier(aTableName));
        aQuery.SQL.Add('and t2.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY''');
        Column_name := 'column_name';

        aQuery.Active := true;
        if not aQuery.Eof then
          Result := aQuery.FieldByName(Column_name).AsString;
      end;

    cdIbLiteToGo: ;

    cdInformix: ;

    cdInterbase: ;

    cdMSSQL, cdMSSQL9:
      begin
        (* Also work on  MSSQL SERVER 2012 :
        'SELECT column_name
        FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
        WHERE OBJECTPROPERTY(OBJECT_ID(constraint_name), 'IsPrimaryKey') = 1
        AND table_name LIKE ' + SQLGetStringExpr(aTableName, GetDbxConnectionDriver(aConnection))   *)

        // From MSSQL SERVER 2005 :
        aQuery.SQL.Clear;
        aQuery.SQL.Add('SELECT i.name AS IndexName,');
        aQuery.SQL.Add('COL_NAME(ic.OBJECT_ID,ic.column_id) AS column_name');
        aQuery.SQL.Add('FROM sys.indexes AS i');
        aQuery.SQL.Add('INNER JOIN sys.index_columns AS ic');
        aQuery.SQL.Add('ON i.OBJECT_ID = ic.OBJECT_ID');
        aQuery.SQL.Add('AND i.index_id = ic.index_id');
        aQuery.SQL.Add('WHERE (i.is_primary_key = 1) and (OBJECT_NAME(ic.OBJECT_ID) LIKE ' + SQLGetStringExpr(aTableName, false, 0, GetDbxConnectionDriver(aConnection)) + ')');
        Column_name := 'column_name';

        aQuery.Active := true;
        if not aQuery.Eof then
          Result := aQuery.FieldByName(Column_name).AsString;
      end;

    cdMySQL:
      begin
        // Retrieve mySQL version: Str := SQLReturnFieldValueAsString(aConnection, 'SELECT VERSION()');

        aQuery.SQL.Text := 'show index from ' + cyDBX.SQLIdentifier(aTableName) + ' where Key_name = ''PRIMARY''';
        Column_name := 'column_name';

        aQuery.Active := true;
        if not aQuery.Eof then
          Result := aQuery.FieldByName(Column_name).AsString;
      end;

    cdOracle: ;

    cdSqlLite: ;
  end;


  aQuery.Active := false;
  aQuery.Free;
end;

function SQLReturnFieldValue(aConnection: TSQLConnection; SQL: TStrings): Variant;
var
  aQuery: TSQLQuery;
begin
  Result := 0;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.SQL.AddStrings(SQL);
    aQuery.Active := true;
    Result := aQuery.Fields[0].Value;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLReturnFieldValue(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): Variant;
var aQuery: TSQLQuery;
begin
  Result := 0;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    if SqlLine1 <> '' then aQuery.SQL.Add(SqlLine1);
    if SqlLine2 <> '' then aQuery.SQL.Add(SqlLine2);
    if SqlLine3 <> '' then aQuery.SQL.Add(SqlLine3);
    if SqlLine4 <> '' then aQuery.SQL.Add(SqlLine4);
    if SqlLine5 <> '' then aQuery.SQL.Add(SqlLine5);
    if SqlLine6 <> '' then aQuery.SQL.Add(SqlLine6);
    if SqlLine7 <> '' then aQuery.SQL.Add(SqlLine7);
    if SqlLine8 <> '' then aQuery.SQL.Add(SqlLine8);
    aQuery.Active := true;
    Result := aQuery.Fields[0].Value;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLReturnFieldValueAsString(aConnection: TSQLConnection; SQL: TStrings): string;
var
  aQuery: TSQLQuery;
  v: Variant;
begin
  Result := '';
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.SQL.AddStrings(SQL);
    aQuery.Active := true;
    v := aQuery.Fields[0].Value;
    if v <> Null then
      Result := v;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLReturnFieldValueAsString(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): String;
var
  aQuery: TSQLQuery;
  v: Variant;
begin
  Result := '';
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    if SqlLine1 <> '' then aQuery.SQL.Add(SqlLine1);
    if SqlLine2 <> '' then aQuery.SQL.Add(SqlLine2);
    if SqlLine3 <> '' then aQuery.SQL.Add(SqlLine3);
    if SqlLine4 <> '' then aQuery.SQL.Add(SqlLine4);
    if SqlLine5 <> '' then aQuery.SQL.Add(SqlLine5);
    if SqlLine6 <> '' then aQuery.SQL.Add(SqlLine6);
    if SqlLine7 <> '' then aQuery.SQL.Add(SqlLine7);
    if SqlLine8 <> '' then aQuery.SQL.Add(SqlLine8);
    aQuery.Active := true;
    v := aQuery.Fields[0].Value;
    if v <> Null then
      Result := v;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLReturnFieldValueAsInteger(aConnection: TSQLConnection; SQL: TStrings): Integer;
var
  aQuery: TSQLQuery;
begin
  Result := 0;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.SQL.AddStrings(SQL);
    aQuery.Active := true;
    Result := aQuery.Fields[0].AsInteger;
    aQuery.Close;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLReturnFieldValueAsInteger(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): Integer;
var
  aQuery: TSQLQuery;
begin
  Result := 0;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    if SqlLine1 <> '' then aQuery.SQL.Add(SqlLine1);
    if SqlLine2 <> '' then aQuery.SQL.Add(SqlLine2);
    if SqlLine3 <> '' then aQuery.SQL.Add(SqlLine3);
    if SqlLine4 <> '' then aQuery.SQL.Add(SqlLine4);
    if SqlLine5 <> '' then aQuery.SQL.Add(SqlLine5);
    if SqlLine6 <> '' then aQuery.SQL.Add(SqlLine6);
    if SqlLine7 <> '' then aQuery.SQL.Add(SqlLine7);
    if SqlLine8 <> '' then aQuery.SQL.Add(SqlLine8);
    aQuery.Active := true;
    Result := aQuery.Fields[0].AsInteger;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLReturnFieldValueAsFloat(aConnection: TSQLConnection; SQL: TStrings): Extended;
var
  aQuery: TSQLQuery;
begin
  Result := 0;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.SQL.AddStrings(SQL);
    aQuery.Active := true;
    Result := aQuery.Fields[0].AsFloat;
    aQuery.Close;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLReturnFieldValueAsFloat(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): Extended;
var
  aQuery: TSQLQuery;
begin
  Result := 0;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    if SqlLine1 <> '' then aQuery.SQL.Add(SqlLine1);
    if SqlLine2 <> '' then aQuery.SQL.Add(SqlLine2);
    if SqlLine3 <> '' then aQuery.SQL.Add(SqlLine3);
    if SqlLine4 <> '' then aQuery.SQL.Add(SqlLine4);
    if SqlLine5 <> '' then aQuery.SQL.Add(SqlLine5);
    if SqlLine6 <> '' then aQuery.SQL.Add(SqlLine6);
    if SqlLine7 <> '' then aQuery.SQL.Add(SqlLine7);
    if SqlLine8 <> '' then aQuery.SQL.Add(SqlLine8);
    aQuery.Active := true;
    Result := aQuery.Fields[0].AsFloat;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLReturnFieldValueAsBoolean(aConnection: TSQLConnection; SQL: TStrings; const DefaultValue: Boolean = false): Boolean;
var
  aQuery: TSQLQuery;
begin
  Result := DefaultValue;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.SQL.AddStrings(SQL);
    aQuery.Active := true;
    if not aQuery.Eof then
      Result := aQuery.Fields[0].AsBoolean;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLReturnFieldValueAsBoolean(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''; const DefaultValue: Boolean = false): Boolean;
var
  aQuery: TSQLQuery;
begin
  Result := DefaultValue;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    if SqlLine1 <> '' then aQuery.SQL.Add(SqlLine1);
    if SqlLine2 <> '' then aQuery.SQL.Add(SqlLine2);
    if SqlLine3 <> '' then aQuery.SQL.Add(SqlLine3);
    if SqlLine4 <> '' then aQuery.SQL.Add(SqlLine4);
    if SqlLine5 <> '' then aQuery.SQL.Add(SqlLine5);
    if SqlLine6 <> '' then aQuery.SQL.Add(SqlLine6);
    if SqlLine7 <> '' then aQuery.SQL.Add(SqlLine7);
    if SqlLine8 <> '' then aQuery.SQL.Add(SqlLine8);
    aQuery.Active := true;
    if not aQuery.Eof then
      Result := aQuery.Fields[0].AsBoolean;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLReturnFieldValueAsDateTime(aConnection: TSQLConnection; SQL: TStrings): TDatetime;
var
  aQuery: TSQLQuery;
begin
  Result := 0;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.SQL.AddStrings(SQL);
    aQuery.Active := true;
    Result := aQuery.Fields[0].AsDateTime;
    aQuery.Close;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLReturnFieldValueAsDateTime(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): TDatetime;
var
  aQuery: TSQLQuery;
begin
  Result := 0;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    if SqlLine1 <> '' then aQuery.SQL.Add(SqlLine1);
    if SqlLine2 <> '' then aQuery.SQL.Add(SqlLine2);
    if SqlLine3 <> '' then aQuery.SQL.Add(SqlLine3);
    if SqlLine4 <> '' then aQuery.SQL.Add(SqlLine4);
    if SqlLine5 <> '' then aQuery.SQL.Add(SqlLine5);
    if SqlLine6 <> '' then aQuery.SQL.Add(SqlLine6);
    if SqlLine7 <> '' then aQuery.SQL.Add(SqlLine7);
    if SqlLine8 <> '' then aQuery.SQL.Add(SqlLine8);
    aQuery.Active := true;
    Result := aQuery.Fields[0].AsDateTime;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLGetFieldValueExpr(aField: TField; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
begin
  if aField.Value = Null then
    Result := 'Null'
  else
    case aField.DataType of
      ftString, ftWideString, ftVariant, ftBlob, ftGraphic, ftTypedBinary, ftMemo, ftFixedChar, ftFmtMemo {$IFDEF UNICODE}, ftFixedWideChar, ftWideMemo {$ENDIF}:
        Result := SQLGetStringExpr(aField.AsString, false, 0, DbxConnectionDriver);


      ftAutoInc, ftSmallint, ftInteger, ftWord, ftLargeint {$IFDEF UNICODE}, ftShortint, ftByte, ftLongWord {$ENDIF}:
        Result := aField.AsString;

      ftBoolean:
        Result := SQLGetBoolExpr(aField.AsBoolean, DbxConnectionDriver);

      ftFloat, ftCurrency {$IFDEF UNICODE}, ftExtended {$ENDIF}:
        Result := SQLGetFloatExpr(aField.AsFloat, DbxConnectionDriver);

      ftFMTBcd, ftBCD:
        Result := SQLGetFloatExpr(aField.AsFloat, DbxConnectionDriver);

      ftDate, ftDateTime:
        Result := SQLGetDateExpr(aField.AsDateTime, DbxConnectionDriver);

      ftTime:
        Result := SQLGetTimeExpr(aField.AsDateTime, DbxConnectionDriver);


      else
        Result := SQLGetStringExpr(aField.AsString, false, 0, DbxConnectionDriver);
    end;

{ Not Handled for now ...

    ftBytes, ftVarBytes, , , // 12..18
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, , , // 19..24
    , ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, // 25..31
    , ftInterface, ftIDispatch, ftGuid, ftTimeStamp, , // 32..37
    , , ftOraTimeStamp, ftOraInterval, // 38..41
    ftConnection, ftParams, ftStream); //42..48
}
end;


function SQLGetStringExpr(Value: String; const TrimValue: Boolean = true; const MaxFieldSize: Integer = 0; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
begin
  if TrimValue then
    Value := Trim(Value);

  if MaxFieldSize <> 0 then
    if Length(Value) > MaxFieldSize then
      Value := Copy(Value, 1, MaxFieldSize);

  // Sanitize strings :
  case DbxConnectionDriver of
    cdMySQL:
    begin
      Result := Value;
      cyStrUtils.String_SubstFast('\',  '\\', Result);     // Need to be first line!
      cyStrUtils.String_SubstFast('''', '\''', Result);
      cyStrUtils.String_SubstFast('"',  '\"', Result);

      Result := '''' + Result + ''''; // QuotedStr(Value) doesn' t work if the value already contains quote car ...
    end;

    cdMSSQL, cdMSSQL9:
    begin
      Result := Value;

      // Chars with no special conversion are:     / \ ’ "
      cyStrUtils.String_SubstFast('''', '''''', Result);
      Result := '''' + Result + ''''; // QuotedStr(Value) doesn' t work if the value already contains quote car ...
    end;

    else
      Result := QuotedStr(Value);
  end;
end;

function SQLGetDateExpr(Value: TDateTime; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
var
  SQLDateFormat: String;
begin
  case DbxConnectionDriver of
    cdMSSQL, cdMSSQL9:
      SQLDateFormat := 'YYYYMMDD';

    else
      SQLDateFormat := 'YYYY-MM-DD';
  end;
  Result := SQLGetStringExpr( FormatDateTime(SQLDateFormat, Value), false, 0, DbxConnectionDriver );
end;

function SQLGetTimeExpr(Value: TDateTime; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
var
  SQLTimeFormat: String;
begin
  SQLTimeFormat := 'HH:NN:SS';
  Result := SQLGetStringExpr( FormatDateTime(SQLTimeFormat, Value), false, 0, DbxConnectionDriver);
end;

function SQLGetDateTimeExpr(Value: TDateTime; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
var
  SQLDateFormat: String;
begin
  case DbxConnectionDriver of
    cdMSSQL, cdMSSQL9:
      SQLDateFormat := 'YYYYMMDD';

    else
      SQLDateFormat := 'YYYY-MM-DD HH:NN:SS';
  end;

  Result := SQLGetStringExpr( FormatDateTime(SQLDateFormat, Value), false, 0, DbxConnectionDriver);
end;

//  OLD Precision: Integer = 18; const Digits: Integer = 18 : create float value anormal value like '41.1000000000000014' for float value 41.1 ...
function SQLGetFloatExpr(Value: Extended; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL; const Precision: Integer = 10; const Digits: Integer = 10): String;
var
  fs: TFormatSettings;
begin
  fs.DecimalSeparator := '.';
  Result := FloatToStrF(Value, ffGeneral, Precision, Digits, fs);
end;

function SQLGetBoolExpr(Value: Boolean; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
begin
  case DbxConnectionDriver of
    cdMySQL:
    begin
      if Value
      then Result := '-1'
      else Result := '0';
    end;

    cdMSSQL, cdMSSQL9:
    begin
      if Value
      then Result := '1'
      else Result := '0';
    end;

    else begin
      if Value
      then Result := '1'
      else Result := '0';
    end;
  end;
end;

function SQLGetDateParam(ParamValue: TDateTime; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
var
  SQLDateFormat: String;
begin
  SQLDateFormat := 'YYYY-MM-DD';
  Result := FormatDateTime(SQLDateFormat, ParamValue);
end;

function SQLGetTimeParam(ParamValue: TDateTime; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
var
  SQLTimeFormat: String;
begin
  SQLTimeFormat := 'HH:NN:SS';
  Result := FormatDateTime(SQLTimeFormat, ParamValue);
end;

function SQLGetFloatParam(ParamValue: Extended; const DbxConnectionDriver: TDbxConnectionDriver = cdMySQL): String;
begin
  Result := SQLGetFloatExpr(ParamValue, DbxConnectionDriver);
end;

procedure SimpleDS_Refresh(aSimpleDataSet: TSimpleDataSet; const ApplyUpdates: Boolean = true);
begin
  if aSimpleDataSet.State in [dsInsert, dsEdit] then
    aSimpleDataSet.Post;

  if aSimpleDataSet.ChangeCount > 0 then
    if ApplyUpdates
    then SimpleDS_ApplyUpdates(aSimpleDataSet)
    else aSimpleDataSet.CancelUpdates;

  if aSimpleDataSet.DataSet.Active then
    aSimpleDataSet.DataSet.Refresh;  // Refresh records from server

  aSimpleDataSet.Refresh;            // Refresh clientdataset records
end;

function SimpleDS_ApplyUpdates(aSimpleDataSet: TSimpleDataSet; const MaxErrors: Integer = -1): Integer;
var RemoveReconcileErrorEvent: Boolean;
begin
  Result := 0;
  RemoveReconcileErrorEvent := false;

  if aSimpleDataSet.ChangeCount > 0 then
  begin
    // Auto handle errors?
    if not Assigned(aSimpleDataSet.OnReconcileError) then
    begin
      aSimpleDataSet.OnReconcileError := TDatasetProviderDefaultErrorHandler(nil).ReconcileError;
      RemoveReconcileErrorEvent := true;
    end;

    Result := aSimpleDataSet.ApplyUpdates(MaxErrors);

    if RemoveReconcileErrorEvent then
      aSimpleDataSet.OnReconcileError := Nil;
  end;
end;

function ClientDS_Refresh(aClientDataSet: TClientDataSet; const ApplyUpdates: Boolean = true): Integer;
var DataSetProvider: TDatasetProvider;
begin
  DataSetProvider := ClientDS_GetProvider(aClientDataSet);
  Result := ClientDS_Refresh(aClientDataSet, DataSetProvider, ApplyUpdates);
end;

function ClientDS_Refresh(aClientDataSet: TClientDataSet; aProvider: TDatasetProvider; const ApplyUpdates: Boolean = true): Integer;
begin
  Result := 0;
  if aClientDataSet.State in [dsInsert, dsEdit] then
    aClientDataSet.Post;

  if aClientDataSet.ChangeCount > 0 then
    if ApplyUpdates
    then Result := ClientDS_ApplyUpdates(aClientDataSet)
    else aClientDataSet.CancelUpdates;

  if aProvider <> Nil then
    if aProvider.DataSet.Active then
      aProvider.DataSet.Refresh;  // Refresh server records (Works with TSQlTable)

  aClientDataSet.Refresh;         // Refresh clientdataset records
end;

function ClientDS_ApplyUpdates(aClientDataSet: TClientDataSet; const MaxErrors: Integer = -1): Integer;
var
  RemoveReconcileErrorEvent: Boolean;
begin
  Result := 0;
  RemoveReconcileErrorEvent := false;

  if aClientDataSet.ChangeCount > 0 then
  begin
    // Auto handle errors?
    if not Assigned(aClientDataSet.OnReconcileError) then
    begin
      aClientDataSet.OnReconcileError := TDatasetProviderDefaultErrorHandler(nil).ReconcileError;
      RemoveReconcileErrorEvent := true;
    end;

    Result := aClientDataSet.ApplyUpdates(MaxErrors);

    if RemoveReconcileErrorEvent then
      aClientDataSet.OnReconcileError := Nil;
  end;
end;

function ClientDS_GetProvider(aClientDataSet: TCustomClientDataset): TDatasetProvider;
var
  c: Integer;
  aComponent: TComponent;
begin
  Result := nil;

  {$IFDEF UNICODE}
  if aClientDataSet.ProviderName = '' then
  begin
    for c := 0 to aClientDataSet.ComponentCount-1 do
      if aClientDataSet.Components[c] is TDataSetProvider then
      begin
        Result := TDataSetProvider(aClientDataSet.Components[c]);
        Break;
      end;
  end
  else begin
    aComponent := aClientDataSet.Owner.FindComponent(aClientDataSet.ProviderName);
    if Assigned(aComponent) then
      if aComponent is TDataSetProvider then
        Result := TDataSetProvider(aComponent);
  end;
  {$ELSE}
    for c := 0 to aClientDataSet.ComponentCount-1 do
      if aClientDataSet.Components[c] is TDataSetProvider then
      begin
        Result := TDataSetProvider(aClientDataSet.Components[c]);
        Break;
      end;
  {$ENDIF}
end;

initialization

  vDbxErrorRecordLocked := 'Record locked by another user!';

end.
