 { This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  ---------------------------------------------------------------------------

    Author : Luc DAVID Email: luckylazarus@free.fr
    2006 - 2009

    Major changes are indicated in the \Documentation\Changes.pdf file
    Last update 10.09.2009

  --------------------------------------------------------------------------- }


{ TSqlitePassDatabase }

Constructor TSqlitePassDatabase.Create
(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  {$IFDEF DEBUG_SQLITEPASS}
    Debugger := TSqlitePassDebugger.Create;
  {$ENDIF}

  {$IFDEF Delphi2009}
    FSystemEncoding := sysUTF16;
  {$ELSE}
    {$IFDEF FPC}
      FSystemEncoding := sysUTF8;
    {$ELSE}
      FSystemEncoding := sysANSI;
    {$ENDIF FPC}
  {$ENDIF Delphi2009}

  {$IFDEF UseTNTComponents}
    FSystemEncoding := sysUTF8;
  {$ENDIF}

  { Events }
  FBeforeConnect      := Nil;
  FAfterConnect       := Nil;
  FBeforeDisconnect   := Nil;
  FAfterDisconnect    := Nil;
  FDataTypeConversion := Nil;

  { Properties }
  FState := [];
  FConnected := False;
  FReadOnly := False;
  FShowSysObjects := False;
  FDatabaseType := dbtUnknown;
  FQueryTimeout := 0;
  FDatabaseError := TSqlitePassDatabaseError.Create(Self);
  { First, creates the appropriate DbEngine }
  FEngine := TSqlitePassEngine.Create(Self);
  { Must set FTranslator to Nil }
  FTranslator := Nil;
  FVersionInfo := TSqlitePassDatabaseVersionInfo.Create(Self);
  FOptions := TSqlitePassDatabaseOptions.Create(Self);
  FDataTypeOptions := TSqlitePassDatabaseDataTypeOptions.Create(Self);
  FTableDefs := TSqlitePassTableDefs.Create(Self);
  FQueryDefs := TSqlitePassQueryDefs.Create(Self);
  FIndexDefs := TSqlitePassDatabaseIndexDefs.Create(Self);
  FViews := TSqlitePassViews.Create(Self);
  FTriggers := TSqlitePassTriggers.Create(Self);
  FSQLStmtDefs := TSqlitePassSQLStmtDefs.Create(Self);
  FDatasets := TSqlitePassDatasets.create(Self);
  FDatabases := TSqlitePassDatabasesAttached.Create(Self);
  FTempRecordset := TSqlitePassRecordset.Create(Self);
end;

Destructor  TSqlitePassDatabase.Destroy;
begin
   Connected := False;
   FTempRecordset.Free;
   If Assigned(FTranslator)
      then FTranslator.Free;
   FDatabases.Free;
   FDatasets.Free;
   FSQLStmtDefs.Free;
   FTriggers.Free;
   FViews.Free;
   FIndexDefs.Free;
   FQueryDefs.Free;
   FTableDefs.Free;
   FDataTypeOptions.Free;
   FOptions.Free;
   FVersionInfo.Free;
   FEngine.Free;
   FDatabaseError.Free;
  {$IFDEF DEBUG_SQLITEPASS}
  Debugger.Free;
  {$ENDIF}
 Inherited Destroy;
end;

function TSqlitePassDatabase.GetFConnected: Boolean;
begin
  Result := Assigned(FEngine) and FConnected;
end;


Procedure TSqlitePassDatabase.SetFConnected(Value : Boolean);
begin
if (csloading in ComponentState)
    then FWaitingForConnection := Value
    else begin
         if (Value <> FConnected)
            then If Value
                    then Open
                    else Close;
         end;
end;

function TSqlitePassDatabase.CheckCanChangePropertyValue(PropertyPrivateName, PropertyPublicName: String): Boolean;
begin
 Result := (dbsInternalOpen in FState) or Not Connected;
 if Not Result
    then DatabaseError.RaiseExceptionFmt(Msg1010,[PropertyPrivateName, PropertyPublicName]);
end;

function TSqlitePassDatabase.CheckCanOpen(VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow): Boolean;
var
DatabasePath: String;

begin
Result := True;
Try
  if Connected then Exit;

  DatabasePath := FDatabase;

  if (DatabasePath='')
     then DatabaseError.RaiseException(Msg1005, -1, Self, VerboseLevel);

  if not FileExists(DatabasePath)
     then DatabaseError.RaiseException(Msg1038+CRLF+DatabasePath, -1, Self, VerboseLevel);

  FEngine.QueryTimeout := FQueryTimeOut;
  FCollatingOrder := GetFCollatingOrder;

  { Tries to open the database and stores the handle to the database }
  Engine.OpenDatabase(DatabasePath, FSqliteLibrary);
  FDatabase := DatabasePath;
  Include(FState,dbsInternalOpen);
Except
  Result := False;
end;
end;

Procedure TSqlitePassDatabase.Open;
begin
if Connected then Exit;
if Assigned(FBeforeConnect) then FBeforeConnect(Self);
Try
If CheckCanOpen then
   begin
   FConnected:=True;
   { Set the requested pragmas settings }
   Options.Apply;
   { Load TableDefs, QueryDefs, IndexDefs from database schema }
   RefreshDefinitions;
   if Assigned(FAfterConnect) then FAfterConnect(Self);
   end;
Except
 Close;
 DatabaseError.RaiseException(Msg1007);
end;
end;

Procedure TSqlitePassDatabase.Close;
begin
if FConnected then
   begin
     Try
     if Assigned(FBeforeDisconnect) then FBeforeDisconnect(Self);
     { We save DatatypeOptions to Database if required }
     FDatatypeOptions.SaveToDatabase;
     FDatasets.Close;
     { We detach previously attached databases first }
     Databases.DetachAll;
     { Clear the Error Log }
     FDatabaseError.ErrorList.Clear;
     FEngine.CloseDatabase;
     if Assigned(FAfterDisconnect) then FAfterDisconnect(Self);
     finally
     FConnected:=False;
     end;
   end;
end;

procedure TSqlitePassDatabase.OpenAttachedDatabases;
var
i: Integer;
NewDb: TSqlitePassDatabase;
begin

{ Check if the Attached Databases List is already in use }
if dbsRefreshingDefinitions in FState
   then Exit;

FDatabasesList := TList.Create;
{ Create temporary databases to get informations from attached Databases }
for i := 0 to Pred(FDatabases.Count) do
   begin
   NewDb := TSqlitePassDatabase.Create(nil);
   NewDb.Database := FDatabases[i].FDatabase;
   NewDb.Open;
   FDatabasesList.Add(NewDb);
   end;
end;

procedure TSqlitePassDatabase.CloseAttachedDatabases;
var
i: Integer;
begin
{ Check if the Attached Databases List need to be used again }
if dbsRefreshingDefinitions in FState
   then Exit;

for i := 0 to Pred(FDatabases.Count)
   do TSqlitePassDatabase(FDatabasesList[i]).Free;

FDatabasesList.Free;
end;

{ Gets information about the database tables, queries...
  including attached databases info }
Procedure TSqlitePassDatabase.RefreshDefinitions;
begin
 if Not CheckCanOpen(vlSilent) then Exit;
 try
 OpenAttachedDatabases;
 Include(FState, dbsRefreshingDefinitions);
 if loDefaultProperties in FDatatypeOptions.LoadOptions
    then FDatatypeOptions.SetDefaultPropertiesValues;
 TableDefs.Refresh;
 QueryDefs.Refresh;
 IndexDefs.Refresh;
 Views.Refresh;
 Triggers.Refresh;
 SQLStmtDefs.Refresh;
 finally
 Exclude(FState, dbsRefreshingDefinitions);
 CloseAttachedDatabases;
 If Not Connected
    then FEngine.CloseDatabase;
 end;
end;

Procedure TSqlitePassDatabase.ClearDefinitions;
begin
  TableDefs.Clear;
  QueryDefs.Clear;
  IndexDefs.Clear;
  Views.Clear;
  Triggers.Clear;
  SQLStmtDefs.Clear;
end;

function TSqlitePassDatabase.GetStrPragma(PragmaName: String): String;
begin
Result := '';
if Connected then
   With FTempRecordset do
        begin
        Open('PRAGMA ' + PragmaName + ';');
        if RecordsCount > 0
           then Result := ItemsTextValue[0, 0];
        end;
end;

function TSqlitePassDatabase.GetIntPragma(PragmaName: String): Integer;
begin
 Result := StrToIntDef(GetStrPragma(PragmaName),-1);
end;

procedure TSqlitePassDatabase.SetPragma(PragmaName: String; value: Boolean);
var
 s:String;
begin
if Connected then
   begin
   if Value
      then s:='ON'
      else s:='OFF';
   FEngine.ExecSQL(Format('PRAGMA %s = %s',[PragmaName, s]));
   end;
end;

procedure TSqlitePassDatabase.SetPragma(PragmaName: String; value: integer);
begin
if Connected
   then FEngine.ExecSQL(Format('PRAGMA %s=%d',[PragmaName, Value]));
end;

procedure TSqlitePassDatabase.SetPragma(PragmaName, Value: String);
begin
if Connected
   then FEngine.ExecSQL(Format('PRAGMA %s=%s',[PragmaName, Value]));
end;

Function TSqlitePassDatabase.GetFCollatingOrder: String;
begin
Result := '';
// TODO : TSqlitePassDatabase.GetFCollatingOrder - To be implemented
end;

Procedure TSqlitePassDatabase.SetFShowSysObjects(Value : Boolean);
begin
 FShowSysObjects := Value;
end;

Procedure TSqlitePassDatabase.SetFReadOnly(Value : Boolean);
begin
 FReadOnly := Value;
end;

function TSqlitePassDatabase.IsSystemTable(TableName: String): Boolean;
begin
Result := Translator.IsSystemTable(TableName);
end;

function TSqlitePassDatabase.Compact: Integer;
var
f: TFileStream;
OriginalSize: Integer;
begin
f := TFileStream.Create(FDatabase, fmOpenRead or fmShareDenyNone);
OriginalSize := f.Size;
try
 Engine.ExecSQL('Vacuum');
finally
 Result := OriginalSize - f.Size;
 f.Free;
end;
end;

procedure TSqlitePassDatabase.CheckIntegrity(MsgList: TStringList; MaxErrorCount: Integer = 100);
var
i: Integer;
CheckRecordset: TSqlitePassRecordset;
begin
if Connected then
   begin
   CheckRecordset := TSqlitePassRecordset.Create(Self);
     Try
     CheckRecordset.Open('Pragma integrity_check('+ IntToStr(MaxErrorCount)+')');
     MsgList.Clear;
     For i := 0 to Pred(CheckRecordset.FRecordsCount)
        do MsgList.Add(CheckRecordset.ItemsTextValue[i,0]);
     finally
     CheckRecordset.Free;
     end;
   end;
end;

function TSqlitePassDatabase.AttachDatabase(FAttachedDatabase, DatabaseAlias: String): Boolean;
var
AttachedDb: TSqlitePassDatabaseAttached;
ExtPos: Integer;
begin
Result := False;

If FDatabaseType <> DatabaseTypeFromFileName(FAttachedDatabase)
   then DatabaseError.RaiseException(Msg1041 + ' : '+FAttachedDatabase+'.');

if DatabaseAlias = ''
   then begin
        DatabaseAlias := StringReplace(ExtractFileName(FAttachedDatabase), ' ', '', [rfReplaceAll]);
        ExtPos := AnsiPos('.', DatabaseAlias);
        DatabaseAlias := System.Copy(DatabaseAlias, 0, Pred(ExtPos));
        end;

Try
  Engine.ExecSQL(UTF8Encode('ATTACH DATABASE "' + FAttachedDatabase + '" AS ' + DatabaseAlias + ';'));
  AttachedDb := TSqlitePassDatabaseAttached.Create(Databases);
  AttachedDb.Database := FAttachedDatabase;
  AttachedDb.FAliasName := DatabaseAlias;
  FDatabases.Add(AttachedDb);
  Result := True;
Finally
  RefreshDefinitions;
end;
end;

function TSqlitePassDatabase.DetachDatabase(DatabaseAlias: String): Boolean;
var
AttachedDb: TSqlitePassDatabaseAttached;
begin
Result := False;
try
AttachedDb := FDatabases.DatabaseByName(DatabaseAlias);
if Assigned(AttachedDb) then
   begin
     try
      Engine.ExecSQL('DETACH DATABASE ' + DatabaseAlias + ';');
     finally
      FDatabases.Remove(AttachedDb);
      AttachedDb.Free;
      Result := True;
     end;
   end;
finally
 RefreshDefinitions;
end;
end;

procedure TSqlitePassDatabase.SetFDatabaseType(const Value: TSqlitePassDatabaseType);
begin
FDatabaseType := Value;

{ Creates the appropriate FieldTranslator }
If Assigned(FTranslator)
   then FTranslator.Free;

Case FDatabaseType of
  dbtSqlitePass  : FTranslator := TSqlitePassTranslator.Create(Self);
  dbtKexi        : FTranslator := TSqlitePassTranslator_Kexi.Create(Self);
  dbtSqliteAdmin : FTranslator := TSqlitePassTranslator_SqliteAdmin.Create(Self);
  dbtSqlite4Fpc  : FTranslator := TSqlitePassTranslator.Create(Self);
  dbtSqliteExpert: FTranslator := TSqlitePassTranslator_SqliteExpert.Create(Self);
  else             FTranslator := TSqlitePassTranslator.Create(Self);
  end;
FDatatypeOptions.FFieldTypesDefaultTranslationRules.InitDefaultTranslationRules;
if Not (csLoading in ComponentState) and Connected
   then RefreshDefinitions;
end;

function TSqlitePassDatabase.DatabaseTypeFromFileName(FileName: String): TSqlitePassDatabaseType;
begin
FileName := Lowercase(FileName);
      if (AnsiPos('.kexi',  FileName)) > 0 then Result := dbtKexi
 else if (AnsiPos('.s3db',  FileName)) > 0 then Result := dbtSqliteAdmin
 else if (AnsiPos('.db3',   FileName)) > 0 then Result := dbtSqliteExpert
 else if (AnsiPos('.s3fpc', FileName)) > 0 then Result := dbtSqlite4Fpc
 else if (AnsiPos('.sp3',   FileName)) > 0 then Result := dbtSqlitePass
 else Result := dbtUnknown;
end;

procedure TSqlitePassDatabase.SetFDatabase(Value: String);
begin
  if Connected and not (csLoading in ComponentState) 
     then DatabaseError.RaiseExceptionFmt(Msg1010,['TSqlitePassDatabase.SetFDatabase', Value]);

  FDatabase := Value;
  SetFDatabaseType(DatabaseTypeFromFileName(FDatabase));
end;


function TSqlitePassDatabase.GetFEngine: TSqlitePassEngine;
begin
Try
Result := FEngine
except
Result := nil;
FDatabaseError.RaiseException(Msg3014);
end;
end;


function TSqlitePassDatabase.CreateDatabase(DbName: String; DbType: TSqlitePassDatabaseType;
                                            DbEncoding: TSqlitePassEncoding = UTF8; PageSize: TSqlitePassPageSize = 4096;
                                            AutoVacuum: TSqlitePassAutoVacuumType = avNone): Boolean;
begin
Result := False;
if Not FileExists(DbName) then
   With TSqlitePassEngine.Create(Self) do
        begin
        Try

        OpenDatabase(DbName, FSqliteLibrary);
        ExecSQL('PRAGMA encoding = "'   + Options.GetEncodingAsString(DbEncoding) + '"');
        ExecSQL('PRAGMA page_size = '   + IntToStr(FOptions.CheckPageSize(PageSize)));
        ExecSQL('PRAGMA Auto_Vacuum = ' + IntToStr(Ord(AutoVacuum)));

        { To have encoding and pagesize set correctly we have to create at least one table
        
          Creates system tables for Kexi }
        if DbType = dbtKexi
           then begin
                ExecSQL(KexiDb_CreateSQLStmt);
                ExecSQL(KexiParts_CreateSQLStmt);
                ExecSQL(KexiObjects_CreateSQLStmt);
                ExecSQL(KexiObjectData_CreateSQLStmt);
                ExecSQL(KexiFields_CreateSQLStmt);
                ExecSQL(KexiBlobs_CreateSQLStmt);
                ExecSQL('INSERT INTO "kexi__db" (db_property, db_value) VALUES ("kexidb_major_ver", "1");');
                ExecSQL('INSERT INTO "kexi__db" (db_property, db_value) VALUES ("kexidb_minor_ver", "8");');
                ExecSQL('INSERT INTO "kexi__db" (db_property, db_value) VALUES ("kexiproject_major_ver", "1");');
                ExecSQL('INSERT INTO "kexi__db" (db_property, db_value) VALUES ("kexiproject_minor_ver", "0");');
                ExecSQL('INSERT INTO "kexi__db" (db_property, db_value) VALUES ("project_caption", "");');
                ExecSQL('INSERT INTO "kexi__db" (db_property, db_value) VALUES ("project_desc", "");');
           end
           { First Creates then drops a dummy table for other database types }
           else begin
                ExecSQL('CREATE TABLE Dummy_Table (TextField Text);');
                ExecSQL('DROP TABLE Dummy_Table;');
                end;
        Result := True;
        finally
        CloseDatabase;
        Free;
        end;
        end
end;

Function TSqlitePassDatabase.DeleteDatabase(Const DbName: String): Boolean;
begin
Result := FileExists(DbName);
if Result then
   begin
   Result := SysUtils.DeleteFile(DbName);
   if Not Result then DatabaseError.RaiseExceptionFmt(Msg1046,[DbName]);
   end;
end;

Function TSqlitePassDatabase.CreateFunction(FuncName: String; ArgCount: ShortInt; Func: TFuncHandler; UserData: Pointer; DefaultEncoding: Byte = SQLITE_ANY): Boolean;
begin
if Connected
   then Result := FEngine.CreateFunction(FuncName, ArgCount, DefaultEncoding, UserData, Func, nil, nil)
   else Result := False;
end;

Function TSqlitePassDatabase.CreateAggFunction(FuncName: String; ArgCount: ShortInt; FuncStep: TFuncHandler; FuncFinal: TFuncFinalizer; UserData: Pointer; DefaultEncoding: Byte = SQLITE_ANY): Boolean;
begin
if Connected
   then Result := FEngine.CreateFunction(FuncName, ArgCount, DefaultEncoding, UserData, nil, FuncStep, FuncFinal)
   else Result := False;
end;

Function TSqlitePassDatabase.DeleteFunction(FuncName: String): Boolean;
begin
if Connected
   then Result := FEngine.CreateFunction(FuncName, -1, SQLITE_ANY, nil, nil, nil, nil)
   else Result := False;
end;

Procedure TSqlitePassDatabase.Loaded;
begin
try
  inherited Loaded;
  Connected := FWaitingForConnection;
  FWaitingForConnection := False;
  RefreshDefinitions;
except
  Application.HandleException(Self)
end;
end;

{ ----- Screen Cursor Utilities ----- }

Procedure TSqlitePassDatabase.ShowBusyScreenCursor;
begin
 { Set the screen cursor }
 if Screen.Cursor <> crHourglass then
    begin
    FSavedScreenCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
    end;
end;

Procedure TSqlitePassDatabase.RestoreScreenCursor;
begin
   Screen.Cursor := FSavedScreenCursor;
end;


{ TSqlitePassDatabaseVersionInfo }

constructor TSqlitePassDatabaseVersionInfo.Create(Database: TSqlitePassDatabase);
begin
inherited Create();
FDatabase  := Database;
FComponent := SqlitePassDatabaseVersion;
FPackage   := SqlitePassPackageVersion;
FSchema  := 0;
end;

destructor TSqlitePassDatabaseVersionInfo.Destroy;
begin
FDatabase := nil;
inherited Destroy;
end;

function TSqlitePassDatabaseVersionInfo.GetFSqliteLibrary: String;
begin
Result := '';
{.$IFDEF WIN32}
if FDatabase.Connected
   then begin
        if FDatabase.SystemEncoding = sysUTF16
           then Result := SqlitePassUtils.UTF8Encode(SqliteDbv3_SqliteLibVersion)
           else Result := SqliteDbv3_SqliteLibVersion;
        end;
{.$ENDIF}
end;

function TSqlitePassDatabaseVersionInfo.GetFSqliteLibraryNumber: Integer;
begin
if FDatabase.Connected
   then Result := SqliteDbv3_SqliteLibVersionNumber
   else Result := 0;
end;

function TSqlitePassDatabaseVersionInfo.GetFSqliteLibrarySourceId: String;
begin
Result := '';
{$IFDEF WIN32}
if FDatabase.Connected
   then begin
        if FDatabase.SystemEncoding = sysUTF16
           then Result := SqlitePassUtils.UTF8Encode(SqliteDbv3_SqliteLibSourceId)
           else Result := SqliteDbv3_SqliteLibSourceId;
        end;
{$ENDIF}
end;

function TSqlitePassDatabaseVersionInfo.GetFUserTag: Integer;
begin
 Result := FDatabase.GetIntPragma('user_version');
end;

function TSqlitePassDatabaseVersionInfo.GetFDbSchema: Integer;
begin
 Result := FDatabase.GetIntPragma('schema_version');
end;

procedure TSqlitePassDatabaseVersionInfo.SetFSqliteLibrary(const Value: String);
begin
{ Read only - Used to display infos in object inspector }
end;

procedure TSqlitePassDatabaseVersionInfo.SetFSqliteLibraryNumber(const Value: Integer);
begin
{ Read only - Used to display infos in object inspector }
end;

procedure TSqlitePassDatabaseVersionInfo.SetFSqliteLibrarySourceId(const Value: String);
begin
{ Read only - Used to display infos in object inspector }
end;

procedure TSqlitePassDatabaseVersionInfo.SetFPackage(const Value: String);
begin
{ Read only - Used to display infos in object inspector }
end;

procedure TSqlitePassDatabaseVersionInfo.SetFDbSchema(const Value: Integer);
begin
{ Read only - Used to display infos in object inspector }
end;

procedure TSqlitePassDatabaseVersionInfo.SetFUserTag(const Value: Integer);
begin
If FDatabase.Connected
   then FDatabase.SetPragma('user_version', Value);
FUserTag := Value;
end;

procedure TSqlitePassDatabase.SetFSQLiteLibrary(Value: String);
begin
 FSqliteLibrary := Value;
end;

procedure TSqlitePassDatabaseVersionInfo.SetFComponent(
  const Value: String);
begin
{ Read only - Used to display infos in object inspector }
end;

{ TSqlitePassDatabaseOptions }

constructor TSqlitePassDatabaseOptions.Create(Database: TSqlitePassDatabase);
begin
inherited Create;
FApplyMode := [amOverwriteDatabaseFileSettings,
               amAutoVacuum,
               amCacheSize,
               amCaseSensitiveLike,
               amCountChanges,
               amDefaultCacheSize,
               amFullColumnNames,
               amForeignKeys,
               amJournalMode,
               { amJournalSizeLimit, default db setting is kept - this pragma should be used with caution }
               amLockingMode,
               { amMaxPageCount, default db setting is kept }
               { amPageSize, default db setting is kept - page size can be changed only when used with the vacuum pragma }
               amRecursiveTriggers,
               amSecureDelete,
               amSynchronous,
               amTemporaryStorage];
               { amTemporaryStorageDir , default db setting is kept - this pragma should be used with caution }
FDatabase := Database;
FAutoVacuum := avNone;
FCacheSize := 2000;
FCaseSensitiveLike := False;
FCountChanges := False;
FDefaultCacheSize := 2000;
FEncoding := UTF8; { arbitrary Default }
FFullColumnNames := False;
FForeignKeys := False;
FJournalMode := jmDelete;
FJournalSizeLimit := -1;
FLockingMode := lmNormal;
FMaxPageCount := MaxInt;
FPageSize := 1024;
FRecursiveTriggers := False;
FSecureDelete := False;
FSynchronous := SyncNormal;
FTemporaryStorage := tsDefault;
FTemporaryStorageDir := '';
{ other private fields }
FLogErrors := False;
QuoteStyle := qsDoubleQuote;
end;

Destructor TSqlitePassDatabaseOptions.Destroy;
begin
FDatabase := nil;
inherited Destroy;
end;

function TSqlitePassDatabaseOptions.GetFAutoVacuum: TSqlitePassAutoVacuumType;
begin
 if FDatabase.Connected
    then Result := TSqlitePassAutoVacuumType(FDatabase.GetIntPragma('auto_vacuum'))
    else Result := FAutoVacuum;
end;

function TSqlitePassDatabaseOptions.GetFCacheSize: Integer;
begin
 if FDatabase.Connected
    then Result := FDatabase.GetIntPragma('cache_size')
    else Result := FCacheSize;
end;

function TSqlitePassDatabaseOptions.GetFCountChanges: Boolean;
begin
 { do Not Use Result := Boolean(FDatabase.GetIntPragma('count_changes'));
   Compiler doesn't warn but Boolean Transtypage is not safe }
 if FDatabase.Connected
    then Result := FDatabase.GetIntPragma('count_changes') <> 0
    else Result := FCountChanges;
end;

function TSqlitePassDatabaseOptions.GetFDefaultCacheSize: Integer;
begin
 if FDatabase.Connected
    then Result := FDatabase.GetIntPragma('default_cache_size')
    else Result := FDefaultCacheSize;
end;

function TSqlitePassDatabaseOptions.GetEncodingAsString(Encoding: TSqlitePassEncoding): String;
begin
Case Encoding of
     UTF8:    Result:= 'UTF-8';
     UTF16:   Result:= 'UTF-16';
     UTF16le: Result:= 'UTF-16le';
     UTF16be: Result:= 'UTF-16be';
     end;
end;

function TSqlitePassDatabaseOptions.GetFEncoding: TSqlitePassEncoding;
var
EncodingType: String;
begin
 if FDatabase.Connected then
    begin
    EncodingType := FDatabase.GetStrPragma('encoding');
         if EncodingType = 'UTF-8'    then Result := UTF8
    else if EncodingType = 'UTF-16'   then Result := UTF16
    else if EncodingType = 'UTF-16le' then Result := UTF16le
    else Result := UTF16be;
    end
    else Result := FEncoding;
end;

function TSqlitePassDatabaseOptions.GetFForeignKeys: Boolean;
begin
 if FDatabase.Connected
    then Result := FDatabase.GetIntPragma('foreign_keys') <> 0
    else Result := FForeignKeys;
end;

function TSqlitePassDatabaseOptions.GetFFullColumnNames: Boolean;
begin
 if FDatabase.Connected
    then Result := FDatabase.GetIntPragma('full_column_names') <> 0
    else Result := FFullColumnNames;
end;

function TSqlitePassDatabaseOptions.GetFJournalMode: TSqlitePassJournalMode;
var
JmResult: String;
begin
if FDatabase.Connected then
   begin
   jmResult := Uppercase(FDatabase.GetStrPragma('journal_mode'));

        if jmResult = 'TRUNCATE' then Result := jmTruncate
   else if jmResult = 'PERSIST'  then Result := jmPersist
   else if jmResult = 'MEMORY'   then Result := jmMemory
   else if jmResult = 'OFF'      then Result := jmOff
   else Result := jmDelete;
   end
   else Result := FJournalMode;
end;

function TSqlitePassDatabaseOptions.GetFJournalSizeLimit: Integer;
begin
 if FDatabase.Connected
    then Result := FDatabase.GetIntPragma('journal_size_limit')
    else Result := FJournalSizeLimit;
end;

function TSqlitePassDatabaseOptions.GetFLockingMode: TSqlitePassLockingMode;
var
lmResult: String;
begin
 if FDatabase.Connected then
    begin
    lmResult := Uppercase(FDatabase.GetStrPragma('locking_mode'));
    if lmResult = 'EXCLUSIVE'
       then Result := lmExclusive
       else Result := lmNormal;
    end
    else Result := FLockingMode;
end;

function TSqlitePassDatabaseOptions.GetFMaxPageCount: Integer;
begin
 if FDatabase.Connected
    then Result := FDatabase.GetIntPragma('max_page_count')
    else Result := FMaxPageCount;
end;

function TSqlitePassDatabaseOptions.GetFPageSize: TSqlitePassPageSize;
begin
 if FDatabase.Connected
    then Result := FDatabase.GetIntPragma('page_size')
    else Result := FPageSize;
end;

function TSqlitePassDatabaseOptions.GetFRecursiveTriggers: Boolean;
begin
 if FDatabase.Connected
    then Result := FDatabase.GetIntPragma('recursive_triggers') <> 0
    else Result := FRecursiveTriggers;
end;

function TSqlitePassDatabaseOptions.GetFSecureDelete: Boolean;
begin
 if FDatabase.Connected
    then Result := FDatabase.GetIntPragma('secure_delete') <> 0
    else Result := FSecureDelete;
end;

function TSqlitePassDatabaseOptions.GetFSynchronous: TSqlitePassSynchronous;
begin
if FDatabase.Connected then
   begin
   Case FDatabase.GetIntPragma('synchronous') of
         1: Result := syncNormal;
         2: Result := syncFull;
       else Result := syncOff;
       end;
   end
   else Result := FSynchronous;
end;

function TSqlitePassDatabaseOptions.GetFTemporaryStorage: TSqlitePassTempStore;
begin
if FDatabase.Connected then
   begin
   Case FDatabase.GetIntPragma('temp_store') of
         1: Result := tsFile;
         2: Result := tsMemory;
       else Result := tsDefault;
       end;
   end
   else Result := FTemporaryStorage;
end;

function TSqlitePassDatabaseOptions.GetFTemporaryStorageDir: String;
begin
 if FDatabase.Connected
    then Result := FDatabase.GetStrPragma('temp_store_directory')
    else Result := FTemporaryStorageDir;
end;

procedure TSqlitePassDatabaseOptions.Apply;
begin
if FDatabase.Connected and (amOverwriteDatabaseFileSettings in FApplyMode) then
   begin
   if amAutoVacuum in FApplyMode
      then SetFAutoVacuum(FAutoVacuum);
   if amCaseSensitiveLike in FApplyMode
      then SetFCaseSensitiveLike(FCaseSensitiveLike);
   if amCountChanges in FApplyMode
      then SetFCountChanges(FCountChanges);
   if amDefaultCacheSize in FApplyMode
      then SetFDefaultCacheSize(FDefaultCacheSize);
   if amFullColumnNames in FApplyMode
      then SetFFullColumnNames(FFullColumnNames);
   if amForeignKeys in FApplyMode
      then SetFForeignKeys(FForeignKeys);
   if amJournalMode in FApplyMode
      then SetFJournalMode(FJournalMode);
   if amJournalSizeLimit in FApplyMode
      then SetFJournalSizeLimit(FJournalSizeLimit);
   if amMaxPageCount in FApplyMode
      then SetFMaxPageCount(FMaxPageCount);
   if amPageSize in FApplyMode
      then SetFPageSize(FPageSize);
   if amRecursiveTriggers in FApplyMode
      then SetFRecursiveTriggers(FRecursiveTriggers);
   if amSecureDelete in FApplyMode
      then SetFSecureDelete(FSecureDelete);
   if amSynchronous in FApplyMode
      then SetFSynchronous(FSynchronous);
   if amTemporaryStorage in FApplyMode
      then SetFTemporaryStorage(FTemporaryStorage);
   if amTemporaryStorageDir in FApplyMode
      then SetFTemporaryStorageDir(FTemporaryStorageDir);
  // TODO - Check Lockmode = exclusive prevents some operations to finish correctly...
   if amLockingMode in FApplyMode
      then SetFLockingMode(FLockingMode);
  end;
end;

procedure TSqlitePassDatabaseOptions.SetFAutoVacuum(const Value: TSqlitePassAutoVacuumType);
begin
if FDatabase.Connected and (AutoVacuum <> Value)
   then FDatabase.SetPragma('auto_vacuum', Ord(Value));
FAutoVacuum := Value;
end;

procedure TSqlitePassDatabaseOptions.SetFCacheSize(const Value: Integer);
begin
if FDatabase.Connected and (CacheSize <> Value)
   then FDatabase.SetPragma('cache_size',Value);
FCacheSize := Value;
end;

procedure TSqlitePassDatabaseOptions.SetFCaseSensitiveLike(const Value: Boolean);
begin
if FDatabase.Connected and (CaseSensitiveLike <> Value)
   then FDatabase.SetPragma('case_sensitive_like',Value);
FCaseSensitiveLike := Value;
end;

procedure TSqlitePassDatabaseOptions.SetFCountChanges(const Value: Boolean);
begin
if FDatabase.Connected and (CountChanges <> Value)
   then FDatabase.SetPragma('count_changes',Value);
FCountChanges := Value;
end;

procedure TSqlitePassDatabaseOptions.SetFDefaultCacheSize(const Value: Integer);
begin
if FDatabase.Connected and (DefaultCacheSize <> Value)
   then FDatabase.SetPragma('default_cache_size',Value);
FDefaultCacheSize := Value;
end;

procedure TSqlitePassDatabaseOptions.SetFEncoding(const Value: TSqlitePassEncoding);
begin
{ This property is read only - Used to display infos in object inspector
  Database real encoding can be written only once, when the database is created
  but you can use the StringEncoding properties to overwrite this as an internal setting
  and let user decides how strings will be encoded when using the component : UTF8 or UTF16... }
end;

procedure TSqlitePassDatabaseOptions.SetFForeignKeys(const Value: Boolean);
begin
if FDatabase.Connected and (ForeignKeys <> Value)
   then FDatabase.SetPragma('foreign_keys', Value);
FForeignKeys := Value;
end;

procedure TSqlitePassDatabaseOptions.SetFFullColumnNames(const Value: Boolean);
begin
if FDatabase.Connected and (FullColumnNames <> Value) then
   begin
   FDatabase.SetPragma('full_column_names', Value);
   FDatabase.SetPragma('short_column_names', not Value);
   end;
FFullColumnNames := Value;
end;

procedure TSqlitePassDatabaseOptions.SetFJournalMode(const Value: TSqlitePassJournalMode);
begin
if FDatabase.Connected and (JournalMode <> Value)
   then Case Value of
        jmDelete  : FDatabase.SetPragma('journal_mode', 'DELETE');
        jmTruncate: FDatabase.SetPragma('journal_mode', 'TRUNCATE');
        jmPersist : FDatabase.SetPragma('journal_mode', 'PERSIST');
        jmMemory  : FDatabase.SetPragma('journal_mode', 'MEMORY');
        jmOff     : FDatabase.SetPragma('journal_mode', 'OFF');
        end;
FJournalMode := Value;
end;

procedure TSqlitePassDatabaseOptions.SetFJournalSizeLimit(const Value: Integer);
begin
if FDatabase.Connected and (JournalSizeLimit <> Value)
   then FDatabase.SetPragma('journal_size_limit', Value);
FJournalSizeLimit := Value;
end;

procedure TSqlitePassDatabaseOptions.SetFLockingMode(const Value: TSqlitePassLockingMode);
begin
if FDatabase.Connected and (LockingMode <> Value)
   then if Value = lmExclusive
           then FDatabase.SetPragma('locking_mode', 'EXCLUSIVE')
           else FDatabase.SetPragma('locking_mode', 'NORMAL');
FLockingMode := Value;
end;

procedure TSqlitePassDatabaseOptions.SetFMaxPageCount(const Value: Integer);
begin
if FDatabase.Connected and (MaxPageCount <> Value)
   then FDatabase.SetPragma('max_page_count', Value);
FMaxPageCount := Value;
end;

function TSqlitePassDatabaseOptions.CheckPageSize(Value: TSqlitePassPageSize): TSqlitePassPageSize;
begin
 Case Value of
   512         : Result := 512;
   513..1024   : Result := 1024;
   1025..2048  : Result := 2048;
   2049..4096  : Result := 4096;
   4097..8192  : Result := 8192;
   8193..16384 : Result := 16384;
   16385..32768: Result := 32768;
   else Result := 512;
   end;
end;

procedure TSqlitePassDatabaseOptions.SetFPageSize(Const Value: TSqlitePassPageSize);
begin
if FDatabase.Connected and (PageSize <> Value)
   then  FDatabase.SetPragma('page_size', CheckPageSize(Value));
FPageSize := Value;
end;

procedure TSqlitePassDatabaseOptions.SetFRecursiveTriggers(const Value: Boolean);
begin
if FDatabase.Connected and (RecursiveTriggers <> Value)
   then FDatabase.SetPragma('recursive_triggers',Value);
FRecursiveTriggers := Value;
end;

procedure TSqlitePassDatabaseOptions.SetFSecureDelete(const Value: Boolean);
begin
if FDatabase.Connected and (SecureDelete <> Value)
   then FDatabase.SetPragma('secure_delete',Value);
FSecureDelete := Value;
end;

procedure TSqlitePassDatabaseOptions.SetFSynchronous(const Value: TSqlitePassSynchronous);
begin
if FDatabase.Connected and (Synchronous <> Value)
   then FDatabase.SetPragma('synchronous', Ord(Value));
FSynchronous := Value;
end;

procedure TSqlitePassDatabaseOptions.SetFQuoteStyle(const Value: TSqlitePassQuoteStyle);
begin
 Case Value of
      qsDoubleQuote: begin FQuoteOpen := '"'; FQuoteClose := '"'; end;
      qsBracket    : begin FQuoteOpen := '['; FQuoteClose := ']'; end;
      qsGraveQuote : begin FQuoteOpen := '`'; FQuoteClose := '`'; end;
      else           begin FQuoteOpen := #0; FQuoteClose := #0; end;
      end;
end;

procedure TSqlitePassDatabaseOptions.SetFTemporaryStorage(const Value: TSqlitePassTempStore);
begin
if FDatabase.Connected and (TemporaryStorage <> Value)
   then FDatabase.SetPragma('temp_store', Ord(Value));
FTemporaryStorage := Value;
end;

procedure TSqlitePassDatabaseOptions.SetFTemporaryStorageDir(const Value: String);
begin
if FDatabase.Connected and (TemporaryStorageDir <> Value)
   then FDatabase.SetPragma('temp_store_directory = ', '''+ Value + ''');
FTemporaryStorageDir := Value;
end;


{ TSqlitePassFieldTypesGenericTranslationRules }


constructor TSqlitePassFieldTypesGenericTranslationRules.Create(
  DataTypeOptions: TSqlitePassDatabaseDataTypeOptions);
begin
inherited Create;
FDataTypeOptions := DataTypeOptions;
end;

Destructor TSqlitePassFieldTypesGenericTranslationRules.Destroy;
begin
ClearAndFreeItems;
FDataTypeOptions := nil;
inherited Destroy;
end;

function TSqlitePassFieldTypesGenericTranslationRules.GetItem(Index: Integer): TSqlitePassFieldTypeTranslationRule;
begin
  Result:=TSqlitePassFieldTypeTranslationRule(Inherited Items[Index]);
end;

procedure TSqlitePassFieldTypesGenericTranslationRules.SetItem(Index: Integer;
  FieldTypeTranslationRule: TSqlitePassFieldTypeTranslationRule);
begin
   Put(Index,FieldTypeTranslationRule);
end;

function TSqlitePassFieldTypesGenericTranslationRules.Add(FieldTypeTranslationRule: TSqlitePassFieldTypeTranslationRule
  ): Integer;
begin
   Result:=Inherited Add(FieldTypeTranslationRule);
end;

procedure TSqlitePassFieldTypesGenericTranslationRules.ClearAndFreeItems;
var
i: Integer;
begin
{ Clear the internal items }
for i := 0 to Pred(Count)
    do Items[i].Free;
{ Clear the list pointers }
inherited Clear;
end;


{ TSqlitePassFieldTypesDefaultTranslationRules }

Procedure TSqlitePassFieldTypesDefaultTranslationRules.InitDefaultTranslationRules;
Type
TSqlitePassPresetFieldType = record
   Name:String;
   Fieldtype:TFieldType;
   end;

const
{ Default Fields Type when DatabaseType is dtbUnknown }
DefaultFieldsTypes:array[0..21] of TSqlitePassPresetFieldType =
           ((Name:'widestring'; FieldType:ftWideString), { must be before String }
            (Name:'string'; FieldType:ftString),
            (Name:'smallint'; FieldType:ftSmallInt), { must be before integer }
            (Name:'largeint'; FieldType:ftLargeint), { must be before integer }
            (Name:'integer'; Fieldtype:ftInteger),
            (Name:'word'; FieldType:ftWord),
            (Name:'float'; FieldType:ftFloat),
            (Name:'boolean'; FieldType:ftBoolean),
            (Name:'currency'; FieldType:ftCurrency),
            (Name:'bcd'; FieldType:ftBCD),
            (Name:'datetime'; FieldType:ftDateTime), { must be before 'date' }
            (Name:'date'; FieldType:ftDate),
            (Name:'time'; FieldType:ftTime),
            (Name:'varbytes'; FieldType:ftVarBytes),
            (Name:'bytes'; FieldType:ftBytes),
            (Name:'autoinc'; FieldType:ftLargeInt),  { Do not set as ftAutoInc }
            (Name:'fmtmemo'; FieldType:ftFmtMemo),   { must be before memo }
            (Name:'memo'; FieldType:ftMemo),
            (Name:'blob'; FieldType:ftBlob),
            (Name:'graphic'; FieldType:ftGraphic),
            (Name:'typedbinary'; FieldType:ftTypedBinary),
            (Name:'fixedchar'; FieldType:ftfixedchar));


{ Default Fields Type when DatabaseType is dtbKexi }
KexiFieldTypes:array[0..12] of TSqlitePassPresetFieldType =
           ((Name:'integer'; Fieldtype:ftInteger),
            (Name:'shortinteger'; FieldType:ftSmallInt),
            (Name:'biginteger'; FieldType:ftLargeint),
            (Name:'byte'; FieldType:ftSmallInt),
            (Name:'currency'; FieldType:ftFloat),
            (Name:'float'; FieldType:ftFloat),
            (Name:'boolean'; FieldType:ftBoolean),
            (Name:'datetime'; FieldType:ftDateTime), { must be before 'date' }
            (Name:'date'; FieldType:ftDate),
            (Name:'time'; FieldType:ftTime),
            (Name:'text'; FieldType:ftString),
            (Name:'clob'; FieldType:ftMemo),
            (Name:'blob'; FieldType:ftBlob));

SqliteAdminFieldTypes:array[0..11] of TSqlitePassPresetFieldType =
           ((Name:'boolean'; FieldType:ftBoolean),
            (Name:'date'; FieldType:ftDate),
            (Name:'float'; FieldType:ftFloat),
            (Name:'integer'; FieldType:ftInteger),
            (Name:'numeric'; FieldType:ftFloat),
            (Name:'real'; FieldType:ftFloat),
            (Name:'text'; FieldType:ftMemo),
            (Name:'timestamp'; FieldType:ftDateTime), { Must be before 'time' }
            (Name:'time'; FieldType:ftTime),
            (Name:'nvarchar'; FieldType:ftString),
            (Name:'varchar'; FieldType:ftString),
            (Name:'blob'; FieldType:ftblob));

{ Default Fields Type when DatabaseType is dbtSqliteExpert }
SqliteExpertFieldTypes:array[0..40] of TSqlitePassPresetFieldType =
           ((Name:'autoinc'; Fieldtype:ftLargeInt), { Do not set as ftAutoinc }
            (Name:'bigint'; FieldType:ftLargeint),
            (Name:'binary'; FieldType:ftBlob),
            (Name:'blob'; FieldType:ftBlob),
            (Name:'boolean'; FieldType:ftBoolean),
            (Name:'char'; FieldType:ftString),
            (Name:'clob'; FieldType:ftMemo),
            (Name:'currency'; FieldType:ftCurrency),
            (Name:'datetext'; FieldType:ftString),   { must be before 'date' }
            (Name:'datetime'; FieldType:ftDateTime), { must be before 'date' }
            (Name:'date'; FieldType:ftDate),
            (Name:'dec'; FieldType:ftFloat),
            (Name:'decimal'; FieldType:ftFloat),
            (Name:'Currency precision'; FieldType:ftFloat),
            (Name:'Currency'; FieldType:ftFloat),
            (Name:'float'; FieldType:ftFloat),
            (Name:'graphic'; FieldType:ftGraphic),
            (Name:'guid'; FieldType:ftMemo), //
            (Name:'int64'; FieldType:ftLargeInt),    { must be before integer }
            (Name:'int'; FieldType:ftInteger),       { must be before integer }
            (Name:'integer'; FieldType:ftInteger),
            (Name:'largeint'; FieldType:ftLargeInt),
            (Name:'memo'; FieldType:ftMemo),
            (Name:'nchar'; FieldType:ftString),
            (Name:'money'; FieldType:ftCurrency),
            (Name:'ntext'; FieldType:ftString),
            (Name:'number'; FieldType:ftFloat),
            (Name:'numeric'; FieldType:ftFloat),
            (Name:'nvarchar2'; FieldType:ftString),
            (Name:'nvarchar'; FieldType:ftString),
            (Name:'raw'; FieldType:ftBlob),
            (Name:'real'; FieldType:ftFloat),
            (Name:'smallint'; FieldType:ftInteger),
            (Name:'smallmoney'; FieldType:ftCurrency),
            (Name:'text'; FieldType:ftString),
            (Name:'timestamp'; FieldType:ftDateTime), { Must be before 'time' }
            (Name:'time'; FieldType:ftTime),
            (Name:'tinyint'; FieldType:ftInteger),
            (Name:'varbinary'; FieldType:ftBlob),
            (Name:'varchar2'; FieldType:ftString),
            (Name:'varchar'; FieldType:ftString));


{ Default Fields Type when DatabaseType is dbtSqlite4Fpc
  Support for TCustomSqliteDataset Component from Luiz Américo Pereira Câmara}
Sqlite4FpcFieldTypes:array[0..13] of TSqlitePassPresetFieldType =
           ((Name:'autoinc'; Fieldtype:ftLargeInt),
            (Name:'boolean'; FieldType:ftBoolean),
            (Name:'currency'; FieldType:ftCurrency),
            (Name:'datetime'; FieldType:ftDateTime),  { must be before 'date' }
            (Name:'date'; FieldType:ftDate),
            (Name:'float'; FieldType:ftFloat),
            (Name:'int'; FieldType:ftInteger),
            (Name:'integer'; FieldType:ftInteger),
            (Name:'largeint'; FieldType:ftLargeInt),
            (Name:'numeric'; FieldType:ftFloat),
            (Name:'text'; FieldType:ftMemo),
            (Name:'time'; FieldType:ftTime),
            (Name:'varchar'; FieldType:ftString),
            (Name:'word'; FieldType:ftWord));

    { Loads the requested Default Datatypes Mapping }
    procedure SetTranslationRules(FieldsTypes: array of TSqlitePassPresetFieldType);
    var
    i: Word;
    FieldTypeTranslator: TSqlitePassFieldTypeTranslationRule;
    begin
    ClearAndFreeItems;
    for i := 0 to High(FieldsTypes) do
       begin
       FieldTypeTranslator := TSqlitePassFieldTypeTranslationRule.Create;
       With FieldTypeTranslator do
            begin
            FDatatypeName := FieldsTypes[i].Name;
            FMappingMode  := mmAll;
            FFieldType    := FieldsTypes[i].FieldType;
            end;
       Add(FieldTypeTranslator);
       end;
    end;

begin
Case FDataTypeOptions.FDatabase.DatabaseType of
     dbtKexi         : SetTranslationRules(KexiFieldTypes);
     dbtSqliteExpert : SetTranslationRules(SqliteExpertFieldTypes);
     dbtSqliteAdmin  : SetTranslationRules(SqliteAdminFieldTypes);
     dbtSqlite4Fpc   : SetTranslationRules(Sqlite4FpcFieldTypes);
     else              SetTranslationRules(DefaultFieldsTypes);
     end;
end;


{ TSqlitePassFieldTypesTranslationRules }

procedure TSqlitePassFieldTypesTranslationRules.SetFieldDefDataType(FieldDef: TSqlitePassTableFieldDef);

    { Enforce Datatype if a special unicode setting is set}
    procedure ConvertNativeFieldTypeToUnicodeFieldType;
    begin
    Case FDataTypeOptions.FUnicodeEncoding of
         ueUTF8 : begin
                  if FieldDef.DataType = ftWideString then FieldDef.DataType := ftString;
                  {$IFDEF HasftWideMemo}
                  if FieldDef.DataType = ftWideMemo   then FieldDef.DataType := ftMemo;
                  {$ENDIF}
                  end;
         ueUTF16: begin
                  if FieldDef.DataType = ftString then FieldDef.DataType := ftWideString;
                  {$IFDEF HasftWideMemo}
                  if FieldDef.DataType = ftMemo   then FieldDef.DataType := ftWideMemo;
                  {$ENDIF}
                  end;
         end;
    end;

    { Convert a Field type to a specific ftField }
    procedure ConvertNativeFieldTypeToPascalFieldType;
    var
    i: integer;
    begin
    For i:=0 to Pred(Count) do
      begin
      Case Items[i].MappingMode of
           mmExact        : if AnsiCompareStr(Items[i].DatatypeName, FieldDef.FNativeDataType) = 0
                               then FieldDef.DataType := Items[i].FieldType;

           mmExactNoCase  : if AnsiCompareText(Items[i].DatatypeName,FieldDef.FNativeDataType) = 0
                               then FieldDef.DataType := Items[i].FieldType;

           mmPartial      : if AnsiPos(Items[i].DatatypeName, FieldDef.FNativeDataType) <> 0
                               then FieldDef.DataType := Items[i].FieldType;

           mmPartialNoCase: if AnsiPos(Uppercase(Items[i].DatatypeName), Uppercase(FieldDef.FNativeDataType)) <> 0
                               then FieldDef.DataType := Items[i].FieldType;

           mmAll          : if AnsiCompareText(Items[i].DatatypeName, FieldDef.FNativeDataType) = 0
                               then FieldDef.DataType := Items[i].FieldType
                               else if AnsiPos(Uppercase(Items[i].DatatypeName), Uppercase(FieldDef.FNativeDataType)) <> 0
                                       then FieldDef.DataType := Items[i].FieldType;
           end;
      if FieldDef.DataType <> ftUnknown
         then Break;
      end;

    { Set a default fieldtype if fieltype is ftUnknown }
    If FieldDef.DataType = ftUnknown
       then FieldDef.DataType := FDataTypeOptions.DefaultFieldType;
    end;


begin { procedure SetFieldDataType }
FieldDef.DataType := ftUnknown;

Case FDataTypeOptions.FDetectionMode of
     dmTypeName  : begin
                   ConvertNativeFieldTypeToPascalFieldType;
                   ConvertNativeFieldTypeToUnicodeFieldType;
                   end;
     dmDbSpecific: begin
                   FDataTypeOptions.FDatabase.FTranslator.ConvertNativeFieldTypeToPascalFieldType(FieldDef);
                   if FieldDef.DataType = ftUnknown
                      then ConvertNativeFieldTypeToPascalFieldType;
                   ConvertNativeFieldTypeToUnicodeFieldType;
                   end;
     dmCustom    : begin
                   if Assigned(FDataTypeOptions.FDatabase.OnDataTypeConversion) then
                      begin
                      ConvertNativeFieldTypeToPascalFieldType;
                      if FieldDef.DataType = ftUnknown
                         then FDataTypeOptions.FDatabase.Translator.ConvertNativeFieldTypeToPascalFieldType(FieldDef);
                      ConvertNativeFieldTypeToUnicodeFieldType;
                      FDataTypeOptions.FDatabase.OnDataTypeConversion(FDataTypeOptions.FDatabase, FieldDef.FieldDefs.TableDef, FieldDef);
                      end;
                   end;
     dmForceStr  : begin
                   Case FDataTypeOptions.FDatabase.SystemEncoding of
                        sysANSI  : FieldDef.DataType := ftString;
                        sysUTF8  : FieldDef.DataType := ftString;
                        sysUTF16 : FieldDef.DataType := ftWideString;
                        end;
                   ConvertNativeFieldTypeToUnicodeFieldType;
                   end;
     end;
end;

procedure TSqlitePassFieldTypesTranslationRules.Assign(
  Source: TSqlitePassFieldTypesGenericTranslationRules);
var
i: Integer;
NewItem: TSqlitePassFieldTypeTranslationRule;
begin
  ClearAndFreeItems;
  for i := 0 to Pred(Source.Count) do
      begin
      NewItem := TSqlitePassFieldTypeTranslationRule.Create;
      NewItem.FDatatypeName := Source.Items[i].DatatypeName;
      NewItem.FFieldType := Source.Items[i].FieldType;
      NewItem.FMappingMode:= Source.Items[i].MappingMode;
      Add(NewItem);
      end;
end;

procedure TSqlitePassFieldTypesTranslationRules.LoadDefaultTranslationRules;
begin
  Assign(FDatatypeOptions.FFieldTypesDefaultTranslationRules);
end;


function TSqlitePassFieldTypesTranslationRules.IsCustomized(
  Item: TSqlitePassFieldTypeTranslationRule): Boolean;
var
i: Integer;
begin
  Result := False;
  for i := 0 to Pred(FDatatypeOptions.FFieldTypesDefaultTranslationRules.Count) do
      begin
      Result := (AnsiCompareText(FDatatypeOptions.FFieldTypesDefaultTranslationRules.Items[i].FDatatypeName, Item.FDatatypeName) = 0)
                and (FDatatypeOptions.FFieldTypesDefaultTranslationRules.Items[i].FFieldType = Item.FFieldType)
                and (FDatatypeOptions.FFieldTypesDefaultTranslationRules.Items[i].FMappingMode = Item.FMappingMode);
      if Result
         then Break;
      end;
  Result := Not Result;
end;



{ TSqlitePassCustomFieldDefs }

constructor TSqlitePassCustomFieldDefs.Create;
begin
inherited Create;
end;

Destructor TSqlitePassCustomFieldDefs.Destroy;
begin
ClearAndFreeItems;
inherited Destroy;
end;

function TSqlitePassCustomFieldDefs.GetItem(Index: Integer): TSqlitePassCustomFieldDef;
begin
  Result:=TSqlitePassCustomFieldDef(Inherited Items[Index]);
end;

procedure TSqlitePassCustomFieldDefs.SetItem(Index: Integer;
  CustomFieldDef: TSqlitePassCustomFieldDef);
begin
   Put(Index,CustomFieldDef);
end;

function TSqlitePassCustomFieldDefs.Add(CustomFieldDef: TSqlitePassCustomFieldDef): Integer;
begin
   Result:=Inherited Add(CustomFieldDef);
end;

procedure TSqlitePassCustomFieldDefs.ClearAndFreeItems;
var
i: Integer;
begin
{ Clear the internal items }
for i := 0 to Pred(Count)
    do Items[i].Free;
{ Clear the list pointers }
inherited Clear;
end;


{ TSqlitePassTranslator }

{ --- constructor / Destructor --- }
constructor TSqlitePassTranslator.Create(Owner: TSqlitePassDatabase);
begin
Inherited Create;
FDatabase := Owner;
FFieldTypesTranslationRules := FDatabase.FDatatypeOptions.TranslationRules;
end;

destructor TSqlitePassTranslator.Destroy;
begin
FFieldTypesTranslationRules := nil;
FDatabase := nil;
Inherited Destroy;
end;


{ ************************************************************************************ }
{ *** Conversion functions from an internal MemRecord storage to a SQLite Db Value *** }

{ Translates a value from internal buffer to an sqlite value. }

{ Boolean }
procedure TSqlitePassTranslator.WordBoolToInteger(Const Value: WordBool; out IntegerValue: Integer);
begin
If Value
   then IntegerValue := 1
   else IntegerValue := 0;
end;

procedure TSqlitePassTranslator.WordBoolToBooleanText(Const Value: WordBool; out StrValue: String);
begin
If Value
   then StrValue := FDatabase.DatatypeOptions.FBooleanTrueValues[0]
   else StrValue := FDatabase.DatatypeOptions.FBooleanFalseValues[0];
end;

{ Date, Time and DateTime Conversion }

procedure TSqlitePassTranslator.IntegerToDateText
         (Const Value: Integer;
          Const DatePattern: String;
          Const YearStart, YearLength, MonthStart, MonthLength, DayStart, DayLength: Word;
          out StrValue: String);
var
TimeStamp: TTimeStamp;
Date: TDateTime;
Year, Month, Day: Word;
DatePartStr: String;
PStrValue: PChar;
begin
 TimeStamp.Date := Value;
 TimeStamp.Time := 0;
 Date := TimeStampToDateTime(TimeStamp);
 DecodeDate(Date, Year, Month, Day);
 { Necessary conversion otherwise we would be pointing on FDatabase.DatatypeOptions.FDateFormat }
 SetString(StrValue, PChar(DatePattern), Length(DatePattern));
 PStrValue := PChar(StrValue);
 {Year}
 DatePartStr := Copy(Format('%0.4d',[Year]), 5-YearLength, YearLength);
 StrMove(PStrValue + Pred(YearStart),PChar(DatePartStr),YearLength);
 {Month}
 DatePartStr := Copy(Format('%0.2d',[Month]), 3-MonthLength, MonthLength);
 StrMove(PStrValue + Pred(MonthStart),PChar(DatePartStr),MonthLength);
 {Day}
 DatePartStr := Copy(Format('%0.2d',[Day]), 3-DayLength, DayLength);
 StrMove(PStrValue + Pred(DayStart),PChar(DatePartStr),DayLength);
end;

procedure TSqlitePassTranslator.IntegerToDateText(Const Value: Integer; out StrValue: String);
begin
 IntegerToDateText(Value,
                   FDatabase.DatatypeOptions.FDateFormat,
                   FDatabase.DatatypeOptions.FDateFormatPattern[0,0],
                   FDatabase.DatatypeOptions.FDateFormatPattern[0,1],
                   FDatabase.DatatypeOptions.FDateFormatPattern[1,0],
                   FDatabase.DatatypeOptions.FDateFormatPattern[1,1],
                   FDatabase.DatatypeOptions.FDateFormatPattern[2,0],
                   FDatabase.DatatypeOptions.FDateFormatPattern[2,1],
                   StrValue);
end;


procedure TSqlitePassTranslator.IntegerToTimeText
         (Const Value: Integer;
          Const TimePattern: String;
          Const HourStart, HourLength, MinStart, MinLength, SecStart, SecLength, MSecStart, MSecLength: Word;
          out StrValue: String);
const
TimeDiv = 86400000; { 1000 / 60 / 60 / 24 }
var
Hour, Min, Sec, MSec: Word;
TimePartStr: String;
PStrValue: PChar;
TimeValue : TTime;
begin
 { 2009-03-17 Thanks to "Parcel" for this contribution }
 TimeValue := Value / TimeDiv;
 DecodeTime(TimeValue, Hour, Min, Sec, MSec);
 { }
 { Necessary conversion otherwise we would be pointing on FDatabase.DatatypeOptions.FTimeFormat }
 SetString(StrValue, PChar(TimePattern), Length(TimePattern));
 PStrValue := PChar(StrValue);
 {Hours}
 TimePartStr := Copy(Format('%0.2d',[Hour]), 3-HourLength, HourLength);
 StrMove(PStrValue + Pred(HourStart),PChar(TimePartStr),HourLength);
 {Minutes}
 TimePartStr := Copy(Format('%0.2d',[Min]), 3-MinLength, MinLength);
 StrMove(PStrValue + Pred(MinStart),PChar(TimePartStr),MinLength);
 {Secondes}
 TimePartStr := Copy(Format('%0.2d',[Sec]), 3-SecLength, SecLength);
 StrMove(PStrValue + Pred(SecStart),PChar(TimePartStr),SecLength);
 {MSecondes are optional, so we check the Time Format}
 if MSecStart > 0 then
    begin
    TimePartStr := Copy(Format('%0.3d',[MSec]), 4-MSecLength, MSecLength);
    StrMove(PStrValue + Pred(MSecStart),PChar(TimePartStr),MSecLength);
    end;
end;

procedure TSqlitePassTranslator.IntegerToTimeText(Const Value: Integer; out StrValue: String);
begin
IntegerToTimeText(Value,
                  FDatabase.DatatypeOptions.FTimeFormat,
                  FDatabase.DatatypeOptions.FTimeFormatPattern[0,0],
                  FDatabase.DatatypeOptions.FTimeFormatPattern[0,1],
                  FDatabase.DatatypeOptions.FTimeFormatPattern[1,0],
                  FDatabase.DatatypeOptions.FTimeFormatPattern[1,1],
                  FDatabase.DatatypeOptions.FTimeFormatPattern[2,0],
                  FDatabase.DatatypeOptions.FTimeFormatPattern[2,1],
                  FDatabase.DatatypeOptions.FTimeFormatPattern[3,0],
                  FDatabase.DatatypeOptions.FTimeFormatPattern[3,1],
                  StrValue);
end;

procedure TSqlitePassTranslator.DoubleToDateTimeText(Const Value: Double; out StrValue: String);
var
TimeStamp: TTimeStamp;
DateTimeStr: String;
begin
 TimeStamp := MSecsToTimeStamp(Trunc(Value));
 Dec(TimeStamp.Date, FDatabase.DatatypeOptions.FDateTimeDelta);
 IntegerToDateText(TimeStamp.Date,
                   FDatabase.DatatypeOptions.FDateTimeFormat,
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[0,0],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[0,1],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[1,0],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[1,1],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[2,0],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[2,1],
                   DateTimeStr);
 IntegerToTimeText(TimeStamp.Time,
                   DateTimeStr,
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[3,0],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[3,1],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[4,0],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[4,1],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[5,0],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[5,1],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[6,0],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[6,1],
                   StrValue);
end;

procedure TSqlitePassTranslator.DoubleToJulianDateTime(Const Value: Double; out DoubleValue: Double);
begin
  DoubleValue := Value - JulianEpoch;
end;

procedure TSqlitePassTranslator.DoubleToUnixDateTime(Const Value: Double; out Int64Value: Int64);
begin
  Int64Value := Trunc(Abs(UnixEpoch - Value)*SecsPerDay);
end;

procedure TSqlitePassTranslator.DoubleToMacDateTime(Const Value: Double; out Int64Value: Int64);
var
  MacEpoch:TDateTime;
begin
  MacEpoch := EncodeDate(1904, 1, 1) + EncodeTime(0, 0, 0, 0);
  Int64Value := Trunc(Abs(MacEpoch-Value)*SecsPerDay);
end;

{ Public functions to return a text representation based on TSqlitePassDatabaseDataTypeOptions formats }
function TSqlitePassTranslator.DateTimeToStr(Value: TDateTime): String;
begin
 DateTimeToString(Result, FDatabase.DatatypeOptions.FDateTimeFormat, Value)
end;

function TSqlitePassTranslator.DateToStr(Value: TDateTime): String;
begin
 DateTimeToString(Result, FDatabase.DatatypeOptions.FDateFormat, Value)
end;

function TSqlitePassTranslator.TimeToStr(Value: TDateTime): String;
begin
 DateTimeToString(Result, FDatabase.DatatypeOptions.FTimeFormat, Value)
end;

{ ************************************************************************************** }
{ *** Conversion procedures from a SQLite Db Value to it's internal MemRecord storage *** }

{ Boolean Conversions }

procedure TSqlitePassTranslator.IntegerToWordBool(Const IntegerValue: Integer; out Value: WordBool);
begin
 Value := IntegerValue <> 0;
end;

procedure TSqlitePassTranslator.BooleanTextToWordBool(Const StrValue:String; out Value: WordBool);
var
StrLower: String;
I: Integer;
begin
 StrLower := LowerCase(StrValue);
 For i:=0 to Pred(FDatabase.DatatypeOptions.FBooleanTrueValues.Count) do
     begin
     Value := AnsiPos(FDatabase.DatatypeOptions.FBooleanTrueValues[i], StrLower) > 0;
     if Value then Break;
     end;
end;

{ Date, Time and DateTime Conversion }

procedure TSqlitePassTranslator.DateTextToInteger(Const StrValue: String;
                                                  Const YearStart, YearLength, MonthStart, MonthLength, DayStart, DayLength: Word;
                                                  out Value: Integer);
var
Year, Month, Day: Word;
begin
 Year := StrToIntDef(System.Copy(StrValue,YearStart,YearLength),0);
 Month := StrToIntDef(System.Copy(StrValue,MonthStart,MonthLength),0);
 Day := StrToIntDef(System.Copy(StrValue,DayStart,DayLength),0);
 if (Year>0) and (Year<10000) and (Month in [1..12])
    and (Day>0) and (Day<=MonthDays[IsleapYear(Year),Month]) then
    begin
    Value := Trunc(EncodeDate(Year, Month, Day));
    Inc(Value, FDatabase.DatatypeOptions.FDateDelta);
    end;
end;


procedure TSqlitePassTranslator.DateTextToInteger(Const StrValue: String; out Value: Integer);
begin
 DateTextToInteger(StrValue,
                   FDatabase.DatatypeOptions.FDateFormatPattern[0,0],
                   FDatabase.DatatypeOptions.FDateFormatPattern[0,1],
                   FDatabase.DatatypeOptions.FDateFormatPattern[1,0],
                   FDatabase.DatatypeOptions.FDateFormatPattern[1,1],
                   FDatabase.DatatypeOptions.FDateFormatPattern[2,0],
                   FDatabase.DatatypeOptions.FDateFormatPattern[2,1],
                   Value);
end;


procedure TSqlitePassTranslator.TimeTextToInteger(Const StrValue: String;
                               Const HourStart, HourLength, MinStart, MinLength, SecStart, SecLength, MSecStart, MSecLength: Word;
                               out Value: Integer);
var
Hour, Min, Sec, MSec: Word;
begin
 Value := 0;
 Hour := StrToIntDef(System.Copy(StrValue,HourStart,HourLength),99);
 Min  := StrToIntDef(System.Copy(StrValue,MinStart,MinLength),99);
 Sec  := StrToIntDef(System.Copy(StrValue,SecStart,SecLength),99);
 MSec := StrToIntDef(System.Copy(StrValue,MSecStart,MSecLength),0);

 if (Hour in [0..24]) and (Min in [0..60]) and (Sec in [0..60]) and (MSec < 1000)
    then Value := DateTimeToTimeStamp(EncodeTime(Hour, Min, Sec, Msec)).Time;
end;

procedure TSqlitePassTranslator.TimeTextToInteger(Const StrValue: String; out Value: Integer);
begin
 TimeTextToInteger(StrValue,
                   FDatabase.DatatypeOptions.FTimeFormatPattern[0,0],
                   FDatabase.DatatypeOptions.FTimeFormatPattern[0,1],
                   FDatabase.DatatypeOptions.FTimeFormatPattern[1,0],
                   FDatabase.DatatypeOptions.FTimeFormatPattern[1,1],
                   FDatabase.DatatypeOptions.FTimeFormatPattern[2,0],
                   FDatabase.DatatypeOptions.FTimeFormatPattern[2,1],
                   FDatabase.DatatypeOptions.FTimeFormatPattern[3,0],
                   FDatabase.DatatypeOptions.FTimeFormatPattern[3,1],
                   Value);
end;

procedure TSqlitePassTranslator.DateTimeTextToDouble(Const StrValue: String; out Value: Double);
var
TimeStamp: TTimeStamp;
begin
 DateTextToInteger(StrValue,
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[0,0],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[0,1],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[1,0],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[1,1],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[2,0],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[2,1],
                   TimeStamp.Date);

 TimeTextToInteger(StrValue,
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[3,0],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[3,1],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[4,0],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[4,1],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[5,0],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[5,1],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[6,0],
                   FDatabase.DatatypeOptions.FDateTimeFormatPattern[6,1],
                   TimeStamp.Time);

 Value := TimeStampToMsecs(TimeStamp);
end;

procedure TSqlitePassTranslator.JulianDateTimeToDouble(Const DoubleValue: Double; out Value: Double);
begin
  Value := DoubleValue + JulianEpoch;
end;

procedure TSqlitePassTranslator.UnixDateTimeToDouble(Const Int64Value: Int64; out Value: Double);
begin
  Value := UnixEpoch + Int64Value / SecsPerDay;
end;

procedure TSqlitePassTranslator.MacDateTimeToDouble(Const Int64Value: Int64; out Value: Double);
var
 MacEpoch:TDateTime;
begin
 MacEpoch := EncodeDate(1904, 1, 1) + EncodeTime(0, 0, 0, 0);
 Value := MacEpoch + Int64Value / SecsPerDay;
end;

{ Strings conversion functions System Encoding  <->  UTF8}
procedure TSqlitePassTranslator.SystemEncodingToUTF8(Const StrValue: String;  Out UTF8String: UTF8String);
begin
Case FDatabase.SystemEncoding of
     sysANSI  : UTF8String := SqlitePassUtils.UTF8ToAnsi(PAnsiChar(StrValue));
     sysUTF8  : UTF8String := StrValue;
     sysUTF16 : UTF8String := SqlitePassUtils.UTF8Encode(StrValue);
     end;
end;

procedure TSqlitePassTranslator.UTF8ToSystemEncoding(Const UTF8String: String; Out StrValue: String);
begin
Case FDatabase.SystemEncoding of
     sysANSI  : StrValue := SqlitePassUtils.AnsiToUTF8(UTF8String);
     sysUTF8  : StrValue := UTF8String;
     sysUTF16 : StrValue := SqlitePassUtils.UTF8Decode(UTF8String);
     end;
end;

(*function TSqlitePassTranslator.CreateTable(const Dataset: TSqlitePassDataset): Boolean;
begin
// TODO : TSqlitePassTranslator.CreateTable - To be implemented
end;*)


{ Database objects information is set from sqlite_master and
 sqlite_temp_master tables. They return basic information :
 Rowid, type, name, tbl_name, rootpage, sql (Create stmt)}
procedure TSqlitePassTranslator.GetDatabaseTableDefs(TableDefs: TSqlitePassTableDefs);
const
Sql = 'SELECT * FROM sqlite_master WHERE type = "table" ' +
      'UNION ALL SELECT * FROM sqlite_temp_master WHERE type = "table" ' +
      'ORDER BY name;';

var
i: Integer;

  Procedure GetTableDefs(AttachedDb: TSqlitePassDatabaseAttached = nil);
  var
  j, NewRecIndex: integer;
  TempDb: TSqlitePassDatabase;
  TableDef: TSqlitePassTableDef;
  TableDefsRecordset: TSqlitePassRecordset;
  FieldDefsRecordset: TSqlitePassRecordset;
  begin
  if Assigned(AttachedDb)
     then begin
          TempDb := TSqlitePassDatabase(FDatabase.FDatabasesList[FDatabase.FDatabases.IndexOf(AttachedDb)]);
          TableDefs.AddTableDefs(TempDb.TableDefs, AttachedDb);
          end
     else begin
          TempDb := FDatabase;
          TableDefsRecordset := TSqlitePassRecordset.Create(TempDb);
          FieldDefsRecordset := TSqlitePassRecordset.Create(TempDb);
          try
          TableDefsRecordset.Open(Sql);
          { Result structure is : Type, name, tablename, rootpage, sql }
          { We manually add the sqlite_master table which is not returned by engine }
          NewRecIndex := TableDefsRecordset.AddRecord;
          With TableDefsRecordset do
               begin
               AddTextItem(NewRecIndex, 0, 'table');
               AddTextItem(NewRecIndex, 1, 'sqlite_master');
               AddTextItem(NewRecIndex, 2, 'sqlite_master');
               AddTextItem(NewRecIndex, 3, '');
               AddTextItem(NewRecIndex, 4, '');
               end;

          for j := 0 to Pred(TableDefsRecordset.RecordsCount) do
              begin
              TableDef := TSqlitePassTableDef.Create(TableDefs);
              TableDef.FTableName := TableDefsRecordset.ItemsTextValue[j,1];
              { Sets the FieldDefs properties }
              GetDatabaseTableFieldDefs(TableDef, FieldDefsRecordset);
              TableDef.FSql := TableDefsRecordset.ItemsTextValue[j,4];
              TableDefs.Add(TableDef);
              end;
          finally
          TableDefsRecordset.Free;
          FieldDefsRecordset.Free;
          end;
          end;
  end;

begin
TableDefs.ClearAndFreeItems;
GetTableDefs(nil);
{ Retrieves TableDefs info for attached databases }
For i := 0 to Pred(FDatabase.Databases.Count)
    do GetTableDefs(FDatabase.Databases[i]);
end;


procedure TSqlitePassTranslator.GetDatabaseTableFieldDefs(TableDef: TSqlitePassTableDef; FieldDefsRecordset: TSqlitePassRecordset);
var
i, NewRecIndex: integer;
Sql: String;
NewFieldDef: TSqlitePassTableFieldDef;

 function HasRowIdField: Boolean;
 var
 j: Integer;
 begin
 Result := False;
 for j := 0 to Pred(FieldDefsRecordset.RecordsCount) do
     begin
     Result :=    (AnsiCompareText(FieldDefsRecordset.ItemsTextValue[j,1], 'Rowid') = 0)
               or (AnsiCompareText(FieldDefsRecordset.ItemsTextValue[j,1], '_Rowid_') = 0)
               or (AnsiCompareText(FieldDefsRecordset.ItemsTextValue[j,1], 'oid') = 0);
     if Result then Exit;
     end;
 end;


begin
TableDef.FieldDefs.Clear;
Sql := SysUtils.Format('PRAGMA table_info ("%s");',[TableDef.TableName]);

Try
{ Returns table fieldDefs schema : cid, name, type, notnull, dflt_value, pk }
FieldDefsRecordset.Open(Sql);

{ Safe test in case no data in returned for the table (bad text encoding...etc) }
if FieldDefsRecordset.RecordsCount = 0 then
   begin
   FDatabase.DatabaseError.ShowError(Format(Msg3980, [TableDef.TableName]), mtWarning, [mbOk], 0);
   Exit;
   end;

 If not HasRowIdField then
   begin
   { We manually add the rowid which is not returned by engine }
   NewRecIndex := FieldDefsRecordset.AddRecord;
   With FieldDefsRecordset do
        begin
        AddTextItem(NewRecIndex, 1, 'rowid');
        AddTextItem(NewRecIndex, 2, 'INTEGER');
        AddTextItem(NewRecIndex, 3, '0');
        AddTextItem(NewRecIndex, 4, '');
        AddTextItem(NewRecIndex, 5, '1');
        end;
   end;

for i := 0 to Pred(FieldDefsRecordset.RecordsCount) do
    begin
    NewFieldDef := TSqlitePassTableFieldDef.Create(TableDef.FieldDefs);
    With NewFieldDef do
         begin
         With FieldDefsRecordset do
              begin
              SetFFieldName(ItemsTextValue[i,1]);
              FNativeDataType := ItemsTextValue[i,2];
              FNotNull := ItemsTextValue[i,3] <> '0';
              FDefaultValue := ItemsTextValue[i,4];
              FPrimaryKey := ItemsTextValue[i,5] = '1';

              { Get the FieldDef DataType From Translation Rules }
              FieldTypesTranslationRules.SetFieldDefDataType(NewFieldDef);

              { If a field is an Integer or Int64 and Primary Key then it is considered
                as an Int64 AutoInc Field }
              If (FDataType in [ftInteger, ftLargeInt]) and FPrimaryKey then
                 begin;
                 FAutoInc  := True;
                 FDataType := ftLargeInt;
                 end;

              { Now we have to set the field size and precision }
              GetFieldDefSize(NewFieldDef);
              end;
         end;
    TableDef.FieldDefs.Add(NewFieldDef);
    end;
finally
 FieldDefsRecordset.Close;
end;
end;

procedure TSqlitePassTranslator.GetDatabaseIndexDefs(IndexDefs: TSqlitePassDatabaseIndexDefs);
var
i: Integer;
Sql, SqlIndexDefs, SqlIndexList, SqlIndexColumns: String;
IndexDefsRecordset: TSqlitePassRecordset;

  Procedure GetIndexDefs(AttachedDb: TSqlitePassDatabaseAttached = nil);
  var
  i, j, SubStringBegin: Integer;
  TempDb: TSqlitePassDatabase;
  DirectionStr: String;
  Idx: TSqlitePassIndex;
  Col: TSqlitePassIndexColumn;
  begin
    if Assigned(AttachedDb)
     then begin
          TempDb := TSqlitePassDatabase(FDatabase.FDatabasesList[FDatabase.FDatabases.IndexOf(AttachedDb)]);
          IndexDefs.AddIndexes(TempDb.IndexDefs, AttachedDb);
          end
     else begin
          { We get info on IndexName, TableName and SQLCreateStmt }
          IndexDefsRecordset.Open(SqlIndexDefs);
          for i := 0 to Pred(IndexDefsRecordset.RecordsCount) do
              begin
              Idx := TSqlitePassDatabaseIndex.Create(FDatabase.IndexDefs);
              Idx.FIndexName := IndexDefsRecordset.ItemsTextValue[i,1];
              Idx.FTableName := IndexDefsRecordset.ItemsTextValue[i,2];
              Idx.FSql := IndexDefsRecordset.ItemsTextValue[i,4];
              IndexDefs.Add(Idx);
              end;

          { For each table, we check if the index is unique or not }
          for i := 0 to Pred(FDatabase.TableDefs.Count) do
              begin
              Sql := Format(SqlIndexList, [FDatabase.TableDefs[i].TableName]);
              IndexDefsRecordset.Open(Sql);
              for j := 0 to Pred(IndexDefsRecordset.RecordsCount) do
                  begin
                  Idx := IndexDefs.FindIndex(IndexDefsRecordset.ItemsTextValue[j,1]);
                  if Assigned(Idx)
                     then Idx.Unique := (IndexDefsRecordset.ItemsTextValue[j,2]= '1');
                  end;
              end;

          { For each index, we get info about fields used and sort order }
          for i := 0 to Pred(IndexDefs.Count) do
              begin
              Sql := Format(SqlIndexColumns, [IndexDefs[i].IndexName]);
              IndexDefsRecordset.Open(Sql);
              for j := 0 to Pred(IndexDefsRecordset.RecordsCount) do
                begin
                Col := TSqlitePassIndexColumn.Create;
                Col.Position := StrToInt(IndexDefsRecordset.ItemsTextValue[j,1]);
                Col.ColumnName := IndexDefsRecordset.ItemsTextValue[j,2];
                SubStringBegin := AnsiPos('"'+Col.ColumnName+'"', IndexDefs[i].Sql);
                If SubStringBegin <> -1 then
                   begin
                   DirectionStr := Copy(IndexDefs[i].Sql, SubStringBegin + 2 + length(Col.ColumnName), 6);
                   If AnsiPos('ASC',DirectionStr) <> 0
                      then Col.Direction := cidAscending
                      else if AnsiPos('DESC',DirectionStr) <> 0
                              then Col.Direction := cidDescending
                              else Col.Direction := cidUnknown;
                   end;
                IndexDefs[i].Columns.Add(Col);
                end;
               end;
          IndexDefsRecordset.Close;
          end; {Else begin}
  end; {GetIndexDefs}

begin
SqlIndexDefs := 'SELECT * FROM sqlite_master WHERE type = "index" ' +
                'UNION ALL SELECT * FROM sqlite_temp_master WHERE type = "index" ' +
                'ORDER BY type, tbl_name;';
SqlIndexList := 'PRAGMA index_list ("%s");';
SqlIndexColumns := 'PRAGMA index_info ("%s");';

IndexDefs.ClearAndFreeItems;
IndexDefsRecordset:= TSqlitePassRecordset.Create(FDatabase);
try
  GetIndexDefs(nil);
  { Retrieves IndexDefs info for attached databases }
  For i := 0 to Pred(FDatabase.Databases.Count)
      do GetIndexDefs(FDatabase.Databases[i]);
finally
  IndexDefsRecordset.Free;
end;
end;

function TSqlitePassTranslator.IsSystemTable(TableName: String): Boolean;
begin
Result :=    (CompareText(TableName, SqlitePassDbSettingsTable) = 0)
          or (CompareText(TableName, SqlitePassSQLStmtDefsTable) = 0)
          or (CompareText(TableName, 'sqlite_master') = 0);
end;

procedure TSqlitePassTranslator.ConvertNativeFieldTypeToPascalFieldType(FieldDef: TSqlitePassTableFieldDef);
begin
{ Implementation should be done in TSqlitePassTranslator descendants }
end;


{ Extracts and sets the FieldDef Size, Precision and the Datatype }
procedure TSqlitePassTranslator.GetFieldDefSize(FieldDef: TSqlitePassTableFieldDef);
var
StartPos, EndPos, DotPos: Word;
FieldNativeDataType, FieldSize: String;

begin
FieldDef.FSize := 0;
FieldDef.FPrecision := 0;
FieldNativeDataType := FieldDef.FNativeDataType;

StartPos:= AnsiPos('(', FieldNativeDataType);
if StartPos > 0 then
   begin
   EndPos := AnsiPos(')', FieldNativeDataType);
   FieldNativeDataType := Copy(FieldNativeDataType, 1, StartPos-1);

   if EndPos > StartPos then
      begin
      Inc(StartPos);
      FieldSize := Copy(FieldDef.FNativeDataType, StartPos, EndPos - StartPos);
      DotPos := AnsiPos('.', FieldSize);
      if DotPos > 0 then
         begin
         FieldSize := Copy(FieldSize, 1, DotPos -1);
         FieldDef.FPrecision := StrToIntDef(Copy(FieldSize, DotPos, EndPos - DotPos),2);
         end;
      FieldDef.FSize := StrToIntDef(FieldSize, 0);
      end;
   end;

Case FieldDef.Datatype of
  ftString,
  ftWideString,
  ftFixedChar,
  {$IFDEF HasftFixedWideChar}
  ftFixedWideChar,
  {$ENDIF HasftFixedWideChar}
  ftBytes,
  ftVarBytes      : if FieldDef.FSize = 0
                       then FieldDef.FSize := DefaultStringFieldSize;
  ftFloat,
  ftCurrency      : if FieldDef.FPrecision = 0
                       then FieldDef.FPrecision := 2;
  ftBCD           : begin
                    if FieldDef.FSize = 0
                       then FieldDef.FSize := 4;
                    if FieldDef.FPrecision = 0
                       then FieldDef.FPrecision := 20;
                    end;
  end;  { Case }

FieldDef.FNativeDataType := FieldNativeDataType;
end;

procedure TSqlitePassTranslator.GetDatabaseQueryDefs(QueryDefs: TSqlitePassQueryDefs);
begin
QueryDefs.ClearAndFreeItems;
{ QueryDefs are not stored by sqlite engine, so there is nothing to do
  Implementation must be done in TSqlitePassTranslator descendants }
end;

procedure TSqlitePassTranslator.GetDatabaseViews(Views: TSqlitePassViews);
const
Sql = 'SELECT * FROM sqlite_master WHERE type = "view" ' +
      'UNION ALL SELECT * FROM sqlite_temp_master WHERE type = "view" ' +
      'ORDER BY type, tbl_name;';
var
i: Integer;

  Procedure GetViews(AttachedDb: TSqlitePassDatabaseAttached = nil);
  var
  CreateStmtStart, i: Integer;
  TempDb: TSqlitePassDatabase;
  View: TSqlitePassView;
  ViewsRecordset: TSqlitePassRecordset;

  begin

  if Assigned(AttachedDb)
     then begin
          TempDb := TSqlitePassDatabase(FDatabase.FDatabasesList[FDatabase.FDatabases.IndexOf(AttachedDb)]);
          Views.AddViews(TempDb.Views, AttachedDb);
          end
     else begin
          TempDb := FDatabase;
          ViewsRecordset := TSqlitePassRecordset.Create(TempDb);

          try
          ViewsRecordset.Open(Sql);

          for i := 0 to Pred(ViewsRecordset.RecordsCount) do
              begin
              View := TSqlitePassView.Create(Views);
              View.FViewName := ViewsRecordset.ItemsTextValue[i,2];
              View.FSqlCreateStmt := ViewsRecordset.ItemsTextValue[i,4];
              CreateStmtStart := AnsiPos(kwSelect + ' ', Uppercase(View.FSqlCreateStmt));

              if CreateStmtStart <> 0
                 then View.FSqlSelectStmt := Copy(View.FSqlCreateStmt, CreateStmtStart, Length(View.FSqlCreateStmt));

              Views.Add(View);
              end;

          finally
          ViewsRecordset.Free;
          end;
          end;
    end;


begin
Views.ClearAndFreeItems;
GetViews(nil);
{ Retrieves Views info for attached databases }
for i := 0 to Pred(FDatabase.Databases.Count)
    do GetViews(FDatabase.Databases[i]);
end;

// TODO - Triggers implementation

procedure TSqlitePassTranslator.GetDatabaseTriggers(Triggers: TSqlitePassTriggers);
var
i: Integer;
Sql: String;
TriggersRecordset: TSqlitePassRecordset;

  Procedure GetTriggers(DatabaseConnection: Pointer);
  var
  i: integer;
  Trigger: TSqlitePassTrigger;

  begin
  TriggersRecordset.Open(Sql);

  for i := 0 to Pred(TriggersRecordset.RecordsCount)
   do begin
      Trigger := TSqlitePassTrigger.Create(Triggers);
      Trigger.FTriggerName := TriggersRecordset.ItemsTextValue[i,2];
      Trigger.FSQL := TriggersRecordset.ItemsTextValue[i,3];
      //Trigger.FAttached := False;  // TODO could be True too...
      Triggers.Add(Trigger);
      end;

  TriggersRecordset.Close;
  end;

begin
Sql := 'SELECT * FROM sqlite_master WHERE type = "trigger" ' +
       'UNION ALL SELECT * FROM sqlite_temp_master WHERE type = "trigger" ' +
       'ORDER BY type, tbl_name;';
Triggers.ClearAndFreeItems;
TriggersRecordset := TSqlitePassRecordset.Create(FDatabase);
GetTriggers(FDatabase.Engine.Connection);
{ Retrieves Triggers info for attached databases }
For i := 0 to Pred(FDatabase.Databases.Count) do
    begin
//    FDatabase.Databases[i].FEngine.OpenDatabase(FDatabase.Databases[i].Database);
//    GetTriggers(FDatabase.Databases[i].FEngine.Connection);
//    FDatabase.Databases[i].FEngine.CloseDatabase;
    end;
TriggersRecordset.Free;
end;



{ TSqlitePassTranslator_Kexi }

function TSqlitePassTranslator_Kexi.GetNativeFieldTypeName(FieldType: Word): String;
begin
Case FieldType of
     // TODO - Result := ftBytes doesn't return correct data - Changed to ftSmallInt;
     1: Result := 'byte';
     2: Result := 'shortinteger';
     3: Result := 'integer';
     // TODO - LargeInt doesn't seem to be supported by kexi binding (Check autoInc field) 4: Result := ftLargeint;
     4: Result := 'biginteger';
     5: Result := 'boolean';
     6: Result := 'date';
     7: Result := 'datetime';
     8: Result := 'time';
     9: Result := 'float';
     10: Result := 'float';
     11: Result := 'text';
     12: Result := 'clob';
     13: Result := 'blob';// ftGraphic;
     else Result := 'unknown';
     end;
end;

procedure TSqlitePassTranslator_Kexi.ConvertNativeFieldTypeToPascalFieldType(FieldDef: TSqlitePassTableFieldDef);
begin
{ Translated from Kexi project : field.h }
Case FieldDef.FNativeDataTypeCode of
     { Byte = 1 }
     // TODO - Result := ftBytes doesn't return correct data - Changed to ftSmallInt;
     1: FieldDef.DataType := ftSmallInt;
     { ShortInteger = 2 }
     2: FieldDef.DataType := ftSmallInt;
     { Integer = 3 }
     3: FieldDef.DataType := ftInteger;
     { BigInteger = 4 }
     // TODO - LargeInt doesn't seem to be supported by kexi binding (Check autoInc field) 4: FieldDef.DataType := ftLargeint;
     4: FieldDef.DataType := ftLargeInt;
     { Boolean = 5 }
     5: FieldDef.DataType := ftBoolean;
     { Date = 6 }
     6: FieldDef.DataType := ftDate;
     { DateTime = 7 }
     7: FieldDef.DataType := ftDateTime;
     { Time = 8 }
     8: FieldDef.DataType := ftTime;
     { Float = 9 }
     9: FieldDef.DataType := ftFloat;
     { Currency = 10 }
     10: FieldDef.DataType := ftFloat;
     { Text = 11 }
     11: FieldDef.DataType := ftString;
     { LongText = 12 }
     12: FieldDef.DataType := ftMemo;
     { BLOB = 13 }
     13: FieldDef.DataType := ftBlob; { Should be set to ftGraphic ? }
     else FieldDef.DataType := ftunknown;
     end;
end;

procedure TSqlitePassTranslator_Kexi.GetDatabaseTableFieldDefs(TableDef: TSqlitePassTableDef; FieldDefsRecordset: TSqlitePassRecordset);
const
SqlConst = 'SELECT t1.o_id,t1.o_name,t1.o_type,t2.f_type,t2.f_name,t2.f_length,'
    + 't2.f_precision,t2.f_constraints,t2.f_options,t2.f_default,t2.f_order,'
    + 't2.f_caption,t2.f_help '
    + 'FROM kexi__objects t1,kexi__fields t2 '
    + 'WHERE t1.o_id=t2.t_id AND t1.o_name = "%s";' ;
var
i: integer;
FieldConstraints: TSqlitePassBitArray;
aFieldDef: TSqlitePassTableFieldDef;
Sql: String;

begin
TableDef.FieldDefs.Clear;
Sql := Format(SqlConst,[TableDef.TableName]);
FieldConstraints := TSqlitePassBitArray.Create;
Try
  Try
  FieldDefsRecordset.Open(Sql);
  FieldConstraints.Items := KexiFieldConstraints;
  // TODO - Handle System Tables Kexi__...

  for i := 0 to Pred(FieldDefsRecordset.RecordsCount) do
      begin
      aFieldDef := TSqlitePassTableFieldDef.Create(TableDef.FieldDefs);
      With aFieldDef do
           begin
           With FieldDefsRecordset do
                begin
                SetFFieldName(ItemsTextValue[i,4]);
                FDisplayName :=  ItemsTextValue[i,11];
                FNativeDataTypeCode := StrToInt(ItemsTextValue[i,3]);
                FNativeDataType := GetNativeFieldTypeName(FNativeDataTypeCode);
                FieldTypesTranslationRules.SetFieldDefDataType(aFieldDef);
                FTableFieldNo := StrToInt(ItemsTextValue[i,10]);
                FDefaultValue := ItemsTextValue[i,9];
                FHint := ItemsTextValue[i,12];

                { Now we have to set the field size and precision }
                Case AFieldDef.DataType of
                      ftString,
                      ftWideString,
                      ftFixedChar,
                      {$IFDEF HasftFixedWideChar}
                      ftFixedWideChar,
                      {$ENDIF HasftFixedWideChar}
                      ftBytes,
                      ftVarBytes: begin
                                  FSize := StrToIntDef(ItemsTextValue[i,5],0);
                                  if FSize = 0
                                     then FSize := DefaultStringFieldSize;
                                  end;    
                      ftBCD     : FPrecision := StrToInt(ItemsTextValue[i,6]);
                     end;

                { Now we have to set the field constraints }
                FieldConstraints.Value := StrToInt(ItemsTextValue[i,7]);
                end; {With FieldDefsRecordset[i] }
            FNoConstraints := FieldConstraints.IsTrue('NoConstraints');
            FAutoInc := FieldConstraints.IsTrue('AutoInc');
            FUnique := FieldConstraints.IsTrue('Unique');
            FPrimaryKey := FieldConstraints.IsTrue('PrimaryKey');
            FForeignKey := FieldConstraints.IsTrue('ForeignKey');
            FNotNull := FieldConstraints.IsTrue('NotNull');
            FNotEmpty := FieldConstraints.IsTrue('NotEmpty');
            FIndexed := FieldConstraints.IsTrue('Indexed');
            end; { With aFieldDef }
      TableDef.FieldDefs.Add(aFieldDef);
      end; { for }

  { If we don't find any fieldefs (system tables fielddefs are not returned by kexi)
    we try the classic method... }
  if TableDef.FieldDefs.Count = 0
     then inherited GetDatabaseTableFieldDefs(TableDef, FieldDefsRecordset);

  Except
  FDatabase.DatabaseError.RaiseExceptionFmt(Msg4000,['kexi__objects']);
  end;

Finally
FieldConstraints.Free;
end;
end;

procedure TSqlitePassTranslator_Kexi.GetDatabaseQueryDefs(QueryDefs: TSqlitePassQueryDefs);
var
i: Integer;
Sql: String;
QueryDefsRecordset: TSqlitePassRecordset;

  Procedure GetQueryDefs(AliasName: String; DatabaseConnection: Pointer);
  var
  i: integer;
  QueryDef: TSqlitePassQueryDef;
  begin
  QueryDefsRecordset.Open(Sql);
  for i := 0 to Pred(QueryDefsRecordset.RecordsCount)
   do begin
      QueryDef := TSqlitePassQueryDef.Create(QueryDefs);
      QueryDef.FQueryName := QueryDefsRecordset.ItemsTextValue[i,3];
//      if AliasName <> ''
//         then QueryDef.FQueryFullName := AliasName + '.' + QueryDef.FQueryName
//         else QueryDef.FQueryFullName := QueryDef.FQueryName;
      QueryDef.FSql := QueryDefsRecordset.ItemsTextValue[i,4];
      QueryDefs.Add(QueryDef);
      end;
  QueryDefsRecordset.Close;
  end;

begin
Sql := 'SELECT K1.Rowid, K2.rowid, K1.o_type,K1.o_name,K2.o_data '+
       'FROM kexi__objects K1, kexi__objectdata K2 '+
       'WHERE (K1.o_id=K2.o_id) AND (K1.o_type=2) AND (K2.o_sub_id="sql");';
QueryDefs.ClearAndFreeItems;

QueryDefsRecordset := TSqlitePassRecordset.Create(FDatabase);
GetQueryDefs('', FDatabase.Engine.Connection);

{ Retrieves TableDefs info for attached databases }
For i := 0 to Pred(FDatabase.Databases.Count) do
    begin
//    FDatabase.Databases[i].FEngine.OpenDatabase(FDatabase.Databases[i].Database);
//    GetQueryDefs(FDatabase.Databases[i].AliasName, FDatabase.Databases[i].FEngine.Connection);
//    FDatabase.Databases[i].FEngine.CloseDatabase;
    end;

QueryDefsRecordset.Free;
end;

function TSqlitePassTranslator_Kexi.IsSystemTable(
  TableName: String): Boolean;
var
i: Integer;
begin
for i := 0 to High(KexiSystemTables) do
    begin
    Result :=  (CompareText(TableName, KexiSystemTables[i]) = 0);
    if Result then break;
    end;
If Not Result
   then Result := Inherited IsSystemTable(TableName);//((CompareTableName = SqlitePassSystemTable)
end;


{ SQLiteExpert - TSqlitePassTranslator_sqliteExpert }
{ Nothing to override...}

{ SQLite Administrator - TSqlitePassTranslator_sqliteAdmin }
{ Nothing to override...}



{ TSqlitePassDatabaseDataTypeOptions }

constructor TSqlitePassDatabaseDataTypeOptions.Create(
  Database: TSqlitePassDatabase);
begin
  Inherited Create;
  FDatabase := Database;
  UnicodeEncoding := ueAuto;
  FDecimalSeparator := Char(SysUtils.DecimalSeparator);
  DateFormat := 'YYYY-MM-DD';
  TimeFormat := 'hh:mm:ss';
  DateTimeFormat := 'YYYY-MM-DD hh:mm:ss.zzz';
  FLoadOptions := [loDefaultProperties, loCustomProperties, loTranslationRules, loCustomFieldDefs];
  FSaveOptions := [soCustomProperties, soTranslationRules, soCustomFieldDefs];
  FFieldTypesDefaultTranslationRules := TSqlitePassFieldTypesDefaultTranslationRules.Create(Self);
  FFieldTypesTranslationRules := TSqlitePassFieldTypesTranslationRules.Create(Self);
  FCustomFieldDefs := TSqlitePassCustomFieldDefs.Create;
  FBooleanTrueValues := TStringList.Create;
  FBooleanFalseValues := TStringList.Create;
end;

destructor TSqlitePassDatabaseDataTypeOptions.Destroy;
begin
  FBooleanTrueValues.Free;
  FBooleanFalseValues.Free;
  FCustomFieldDefs.Free;
  FFieldTypesTranslationRules.Free;
  FFieldTypesDefaultTranslationRules.Free;
  Inherited Destroy;
end;

procedure TSqlitePassDatabaseDataTypeOptions.LoadFromDatabase(_LoadOptions: TsqlitePassDataTypeStorageLoadOptions = []);
Const
SqlTableExists = 'select * from sqlite_master where name = "' + SqlitePassDbSettingsTable + '"';
Sql = 'SELECT * FROM "' + SqlitePassDbSettingsTable +'";';
var
i: Integer;
DbSettingsRecordset: TSqlitePassRecordset;
StorageInfoStrings, MainPropertiesStrings, TranslationRulesStrings, CustomFieldDefsStrings: TStringList;
TranslationRule: TSqlitePassFieldTypeTranslationRule;
CustomFieldDef: TSqlitePassCustomFieldDef;
begin
If Not FDatabase.Connected then Exit;

If _LoadOptions = []
   then _LoadOptions := FLoadOptions;

If _LoadOptions = [] then Exit;

StorageInfoStrings      := TStringList.Create;
MainPropertiesStrings   := TStringList.Create;
TranslationRulesStrings := TStringList.Create;
CustomFieldDefsStrings  := TStringList.Create;
DbSettingsRecordset:= TSqlitePassRecordset.Create(FDatabase);

{ If the table doesn't exist, jump to the finally part.
  We don't rely on FDatabase.TableDefs.FindTable(SqlitePassDbSettingsTable)
  because FDatabase.TableDefs could be out of date }
DbSettingsRecordset.Open(SqlTableExists);

if DbSettingsRecordset.RecordsCount = 0
  then Exit;

Try {1}
 Try {2}
 { Loads Options from database }
 DbSettingsRecordset.Open(Sql);

 If (DbSettingsRecordset.RecordsCount > 0) then
    begin
    StorageInfoStrings.Text := DbSettingsRecordset.ItemsTextValue[0,0];
    MainPropertiesStrings.Text := DbSettingsRecordset.ItemsTextValue[0,1];
    TranslationRulesStrings.Text := DbSettingsRecordset.ItemsTextValue[0,2];
    CustomFieldDefsStrings.Text := DbSettingsRecordset.ItemsTextValue[0,3];
    end;

 { Storage Version }
 if StorageInfoStrings.Count = 2
    then FStorageVersion := StorageInfoStrings[1];

 if FStorageVersion <> SqlitePassStorageVersion
    then Abort;

 { Main Properties }
 if loCustomProperties in _LoadOptions then
    begin
    BooleanFormat := MainPropertiesStrings[1];
    BooleanStorage := TSqlitePassBooleanStorage(StrToInt(MainPropertiesStrings[3]));
    DateFormat := MainPropertiesStrings[5];
    DateStorage := TSqlitePassDateStorage(StrToInt(MainPropertiesStrings[7]));
    DateTimeFormat := MainPropertiesStrings[9];
    DateTimeStorage := TSqlitePassDateTimeStorage(StrToInt(MainPropertiesStrings[11]));
    DecimalSeparator := Char(MainPropertiesStrings[13][1]);
    DefaultFieldType := StringToFieldType(MainPropertiesStrings[15]);
    DetectionMode := TSqlitePassDataTypeDetectionMode(StrToInt(MainPropertiesStrings[17]));
    TimeFormat := MainPropertiesStrings[19];
    TimeStorage := TSqlitePassTimeStorage(StrToInt(MainPropertiesStrings[21]));
    end;

 { TranslationRules }
 if loTranslationRules in _LoadOptions then
    begin
    i := 1;
    While i < Pred(TranslationRulesStrings.Count) do
          begin
          TranslationRule := TSqlitePassFieldTypeTranslationRule.Create;
          TranslationRule.DatatypeName := TranslationRulesStrings[i];
          Inc(i,2);
          TranslationRule.MappingMode :=  TSqlitePassDataTypeMappingMode(StrToInt(TranslationRulesStrings[i]));
          Inc(i,2);
          TranslationRule.FieldType := StringToFieldType(TranslationRulesStrings[i]);
          { Add only customized fieldDefs (different of default translation rules) }
          if TranslationRules.IsCustomized(TranslationRule)
            then TranslationRules.Add(TranslationRule);
          Inc(i,2);
          end;
     end;

 { CustomFieldDefs }
 if loCustomFieldDefs in _LoadOptions then
    begin
    CustomFieldDefs.ClearAndFreeItems;
    i := 1;
    While i < Pred(CustomFieldDefsStrings.Count) do
          begin
          CustomFieldDef := TSqlitePassCustomFieldDef.Create;
          CustomFieldDef.TableName := CustomFieldDefsStrings[i];
          Inc(i,2);
          CustomFieldDef.FieldName := CustomFieldDefsStrings[i];
          Inc(i,2);
          CustomFieldDef.FieldType := StringToFieldType(CustomFieldDefsStrings[i]);
          Inc(i,2);
          CustomFieldDef.FieldSize := StrToInt(CustomFieldDefsStrings[i]);
          Inc(i,2);
          CustomFieldDef.FieldPrecision := StrToInt(CustomFieldDefsStrings[i]);
          CustomFieldDefs.Add(CustomFieldDef);
          Inc(i,2);
          end;
    end;
 except {Try 2}
    if FDatabase.DatabaseError.ShowError(Msg1044, mtWarning, [mbYes, mbNo], 0) = mrYes
       then FDatabase.Engine.ExecSQL('Drop table "SQLitePass__DbSettings";');
 end; {Try 2 }
finally
StorageInfoStrings.Free;
MainPropertiesStrings.Free;
TranslationRulesStrings.Free;
CustomFieldDefsStrings.Free;
DbSettingsRecordset.Free;
end; {Try 1}
end;


procedure TSqlitePassDatabaseDataTypeOptions.SaveToDatabase(_SaveOptions: TsqlitePassDataTypeStorageSaveOptions = []);
Const
Sql = 'CREATE TABLE IF NOT EXISTS "' + SqlitePassDbSettingsTable +'" ("StorageVersion" CBLOB, "MainProperties" CBLOB, "TranslationRules" CBLOB, "CustomFieldDefs" CBLOB);';
var
i: Integer;
WasConnected, WasReadOnly: Boolean;
DbSettingsDataset: TSqlitePassDataset;
DbSettingTableDef: TSqlitePassTableDef;
Lines: TStringList;
begin
WasConnected := False;
WasReadOnly  := False;
Lines := TStringList.Create;

If _SaveOptions = []
   then _SaveOptions := FSaveOptions;

If _SaveOptions = [] then Exit;

If Assigned(FDatabase) and Assigned(FDatabase.FEngine) then
   begin
   try
     DbSettingsDataset := nil;
     WasConnected := FDatabase.Connected;
     WasReadOnly  := FDatabase.ReadOnly;
     FDatabase.ReadOnly := False;
     FDatabase.Open;

     { Creates the table if it doesn't already exist }
     DbSettingTableDef := FDatabase.FTableDefs.FindTable(SqlitePassDbSettingsTable);
     if DbSettingTableDef = nil then
        begin
        FDatabase.FEngine.ExecSQL(Sql);
        FDatabase.Translator.GetDatabaseTableDefs(FDatabase.TableDefs);
        DbSettingTableDef := FDatabase.FTableDefs.FindTable(SqlitePassDbSettingsTable)
        end;

     { Enforces fields fieldTypes to ftMemo whatever Databasetype is used }
     For i := 0 to Pred(DbSettingTableDef.FieldDefs.Count)
         do DbSettingTableDef.FieldDefs[i].DataType := ftMemo;

     { Saves Options to database }
     DbSettingsDataset:= TSqlitePassDataset.Create(nil);
     DbSettingsDataset.Database := FDatabase;

     With DbSettingsDataset do
          begin
          DatasetName := SqlitePassDbSettingsTable;
          Open;
          First;
          Edit;
          end;

    { Save Storage Version }
     With Lines do
          begin
          Add('StorageVersion');Add(SqlitePassStorageVersion);
          end;
     {$IFDEF FPC}
     DbSettingsDataset.Fields[0].Text := Lines.Text;
     {$ELSE}
     DbSettingsDataset.Fields[0].Assign(Lines);
     {$ENDIF}

     { Main Properties }
     if soCustomProperties in _SaveOptions then
        begin
        With Lines do
             begin
             Clear;
             Add('BooleanExtension'); Add(BooleanFormat);
             Add('BooleanStorage'); Add(IntToStr(Ord(BooleanStorage)));
             Add('DateFormat'); Add(DateFormat);
             Add('DateStorage'); Add(IntToStr(Ord(DateStorage)));
             Add('DateTimeFormat'); Add(DateTimeFormat);
             Add('DateTimeStorage'); Add(IntToStr(Ord(DateTimeStorage)));
             Add('DecimalSeparator'); Add(DecimalSeparator);
             Add('DefaultFieldType'); Add(FieldTypeToString(DefaultFieldType));
             Add('DetectionMode'); Add(IntToStr(Ord(DetectionMode)));
             Add('TimeFormat'); Add(TimeFormat);
             Add('TimeStorage'); Add(IntToStr(Ord(TimeStorage)));
             {$IFDEF FPC}
             DbSettingsDataset.Fields[1].Text := Lines.Text;
             {$ELSE}
             DbSettingsDataset.Fields[1].Assign(Lines);
             {$ENDIF}
             end;
        end;

     { TranslationRules }
     if soTranslationRules in _SaveOptions then
        begin
          With Lines do
          begin
          Clear;
          for i := 0 to Pred(TranslationRules.Count) do
              begin
              Add('DatatypeName');
              Add(TranslationRules[i].DatatypeName);
              Add('MappingMode');
              Add(IntToStr(Ord(TranslationRules[i].MappingMode)));
              Add('FieldType');
              Add(FieldTypeToString(TranslationRules[i].FieldType));
              end;
          end;
         {$IFDEF FPC}
         DbSettingsDataset.Fields[2].Text := Lines.Text;
         {$ELSE}
         DbSettingsDataset.Fields[2].Assign(Lines);
         {$ENDIF}
        end;

     { Custom FieldDefs }
      if soCustomFieldDefs in _SaveOptions then
        begin
          With Lines do
          begin
          Clear;
          for i := 0 to Pred(CustomFieldDefs.Count) do
              begin
              Add('TableName'); Add(CustomFieldDefs[i].TableName);
              Add('FieldName'); Add(CustomFieldDefs[i].FieldName);
              Add('FieldType'); Add(FieldTypeToString(CustomFieldDefs[i].FieldType));
              Add('FieldSize'); Add(IntToStr(CustomFieldDefs[i].FieldSize));
              Add('FieldPrecision'); Add(IntToStr(CustomFieldDefs[i].FieldPrecision));
              end;
          end;
         {$IFDEF FPC}
         DbSettingsDataset.Fields[3].Text := Lines.Text;
         {$ELSE}
         DbSettingsDataset.Fields[3].Assign(Lines);
         {$ENDIF}
        end;

   finally
     if Assigned(DbSettingsDataset) then
        begin
        DbSettingsDataset.Post;
        DbSettingsDataset.Free;
        end;
     Lines.Free;
     FDatabase.ReadOnly  := WasReadOnly;
     FDatabase.Connected := WasConnected;
   end;
   end;
end;

procedure TSqlitePassDatabaseDataTypeOptions.DefineProperties(Filer: TFiler);
begin
{ Property are prefixed with 'p' to have a different name of the original
  property otherwise, DefineProperty is not called by the filer }
  Filer.DefineProperty('pCustomFieldDefs', ReadCustomFieldDefs, WriteCustomFieldDefs, True);
  Filer.DefineProperty('pTranslationsRules', ReadTranslationRules, WriteTranslationRules, True);
end;

procedure TSqlitePassDatabaseDataTypeOptions.ReadTranslationRules(Reader: TReader);
var
TranslationRule: TSqlitePassFieldTypeTranslationRule;
begin
FFieldTypesTranslationRules.ClearAndFreeItems;
Reader.ReadListBegin;
while not(Reader.EndofList) do
      begin
      TranslationRule := TSqlitePassFieldTypeTranslationRule.Create;
      With TranslationRule do
           begin
           DatatypeName := Reader.ReadString;
           MappingMode := TSqlitePassDataTypeMappingMode(Reader.ReadInteger);
           FieldType := StringToFieldType(Reader.ReadString);
           end;
      FFieldTypesTranslationRules.Add(TranslationRule);
      end;
Reader.ReadListEnd;
end;

procedure TSqlitePassDatabaseDataTypeOptions.WriteTranslationRules(Writer: TWriter);
var
  i: Integer;
begin
  With Writer do
       begin
       WriteListBegin;
       for i := 0 to Pred(FFieldTypesTranslationRules.Count) do
          begin
          WriteString(FFieldTypesTranslationRules[i].DatatypeName);
          WriteInteger(Ord(FFieldTypesTranslationRules[i].MappingMode));
          WriteString(FieldTypeToString(FFieldTypesTranslationRules[i].FieldType));
          end;
       WriteListEnd;
       end;
end;

procedure TSqlitePassDatabaseDataTypeOptions.ReadCustomFieldDefs(Reader: TReader);
var
CustomFieldDef: TSqlitePassCustomFieldDef;
begin
FCustomFieldDefs.ClearAndFreeItems;
Reader.ReadListBegin;
while not(Reader.EndofList) do
      begin
      CustomFieldDef := TSqlitePassCustomFieldDef.Create;
      With CustomFieldDef do
           begin
           TableName := Reader.ReadString;
           FieldName := Reader.ReadString;
           FieldType := StringToFieldType(Reader.ReadString);
           FieldSize := Reader.ReadInteger;
           FieldPrecision := Reader.ReadInteger;
           end;
      FCustomFieldDefs.Add(CustomFieldDef);
      end;
Reader.ReadListEnd;
end;

procedure TSqlitePassDatabaseDataTypeOptions.WriteCustomFieldDefs(Writer: TWriter);
var
  i: Integer;
begin
  With Writer do
       begin
       WriteListBegin;
       for i := 0 to Pred(FCustomFieldDefs.Count) do
          begin
          WriteString(FCustomFieldDefs[i].TableName);
          WriteString(FCustomFieldDefs[i].FieldName);
          WriteString(FieldTypeToString(FCustomFieldDefs[i].FieldType));
          WriteInteger(FCustomFieldDefs[i].FieldSize);
          WriteInteger(FCustomFieldDefs[i].FieldPrecision);
          end;
       WriteListEnd;
       end;
end;

procedure TSqlitePassDatabaseDataTypeOptions.SetDefaultPropertiesValues;
begin

{ Generic Default Values }
FDetectionMode := dmTypeName; { dmTypeName, dmDbSpecific, dmCustom, dmForceStr }
{ Date }
FDateDelta := SysUtils.DateDelta;
DateFormat := 'YYYY-MM-DD';
FDateStorage   := asInteger;
{ Time }
TimeFormat := 'hh:mm:ss';
FTimeStorage := asInteger;
{ DateTime }
FDateTimeDelta := SysUtils.DateDelta;
DateTimeFormat := 'YYYY-MM-DD hh:mm:ss';
FDateTimeStorage := dtsText;
{ Boolean }
BooleanFormat := 'True,Yes,On,1,-False,No,Off,0';
FBooleanStorage := asInteger;
{ Decimal }
FDecimalSeparator := '.';

FDefaultFieldType := ftUnknown;
FDatabase.FOptions.FQuoteStyle := qsDoubleQuote;

{ Overrides default values depending on DatabaseType }
Case FDatabase.FDatabaseType of
  dbtSqlitePass:
           begin
           FDateTimeStorage := dtsDateTime;
           DateTimeFormat := 'YYYY-MM-DD hh:mm:ss.zzz';
           BooleanFormat := '1-0'
           end;

  dbtKexi: begin
           FDetectionMode := dmDbSpecific;
           FDateTimeDelta := 0;
           DateTimeFormat := 'YYYY-MM-DDThh:mm:ss';
           FDateStorage := asText;
           FTimeStorage := asText;
           BooleanFormat := '1-0'
           end;

  dbtSqliteAdmin:
           begin
           FDateStorage := asText;
           FTimeStorage := asText;
           FDateTimeDelta := 0;
           FDatabase.FOptions.FQuoteStyle := qsBracket;
           FBooleanFormat := '1-0'
           end;

  dbtSqliteExpert:
           begin
           FDatabase.FOptions.FQuoteStyle := qsDoubleQuote;
           FDateStorage := asText;
           FTimeStorage := asText;
           FDateTimeDelta := 0;
           BooleanFormat := '1-0'
           end;

  dbtSqlite4FPC:
           begin
           FDefaultFieldType := ftString;
           end;
  end;
end;

procedure TSqlitePassDatabaseDataTypeOptions.ApplyCustomFieldDefs;
var
  i: Integer;
  FieldDef: TSqlitePassTableFieldDef;
begin
for i:= 0 to Pred(FCustomFieldDefs.Count) do
    begin
    FieldDef := FDatabase.TableDefs.FindFieldDef(FCustomFieldDefs[i].TableName,FCustomFieldDefs[i].FieldName);
    if Assigned(FieldDef) then
       begin
       FieldDef.FDataType  := FCustomFieldDefs[i].FieldType;
       FieldDef.FSize      := FCustomFieldDefs[i].FieldSize;
       if FieldDef.FSize = 0
          then FieldDef.FSize := DefaultStringFieldSize;
       FieldDef.FPrecision := FCustomFieldDefs[i].FieldPrecision;
       end;
    end;
end;

procedure TSqlitePassDatabaseDataTypeOptions.FindSubString
(Chr: Char; LowerStr: String; Var StartPos, EndPos: Word);
var
StrLength: Integer;
begin
StartPos := AnsiPos(Chr,LowerStr);
StrLength := Length(LowerStr);
if StartPos > 0 then
   begin
   EndPos := StartPos;
   { fix : range check error by Parcel }
   While (EndPos <= StrLength) Do
   if (LowerStr[EndPos] = Chr)
      then Inc(EndPos)
      else Break;
   end;
Dec(EndPos,StartPos);
end;

procedure TSqlitePassDatabaseDataTypeOptions.SetFDateFormat(const Value: String);
var
LowerStr: String;
begin

{Year-Month-Day
FDateFormatPattern : Array[0..2, 0..1] of Word;}
if FDatabase.CheckCanChangePropertyValue('TSqlitePassDatabaseDataTypeOptions.SetFDateFormat','DateFormat') then
   begin
   LowerStr := AnsiLowercase(Value);
   FindSubString('y', LowerStr, FDateFormatPattern[0,0], FDateFormatPattern[0,1]);
   FindSubString('m', LowerStr, FDateFormatPattern[1,0], FDateFormatPattern[1,1]);
   FindSubString('d', LowerStr, FDateFormatPattern[2,0], FDateFormatPattern[2,1]);
   FDateFormat := Value;
   end;
end;

procedure TSqlitePassDatabaseDataTypeOptions.SetFTimeFormat(const Value: String);
var
LowerStr: String;
begin

{Hour-Minute-Sec-Msec
FTimeFormatPattern : Array[0..3, 0..1] of Word;}
if FDatabase.CheckCanChangePropertyValue('TSqlitePassDatabaseDataTypeOptions.SetFTimeFormat','TimeFormat') then
   begin
   LowerStr := AnsiLowercase(Value);
   FindSubString('h', LowerStr, FTimeFormatPattern[0,0], FTimeFormatPattern[0,1]);
   FindSubString('m', LowerStr, FTimeFormatPattern[1,0], FTimeFormatPattern[1,1]);
   FindSubString('s', LowerStr, FTimeFormatPattern[2,0], FTimeFormatPattern[2,1]);
   FindSubString('z', LowerStr, FTimeFormatPattern[3,0], FTimeFormatPattern[3,1]);
   FTimeFormat := Value;
   end;
end;

procedure TSqlitePassDatabaseDataTypeOptions.SetFDateTimeFormat(const Value: String);
begin

{Year-Month-Day-Hour-Minute-Sec-Msec
FDateTimeFormatPattern : Array[0..6, 0..1] of Word;

DateTimeFormat is Case Sensitive}
if FDatabase.CheckCanChangePropertyValue('TSqlitePassDatabaseDataTypeOptions.SetFDateTimeFormat','DateTimeFormat') then
   begin
   FindSubString('Y', Value, FDateTimeFormatPattern[0,0], FDateTimeFormatPattern[0,1]);
   FindSubString('M', Value, FDateTimeFormatPattern[1,0], FDateTimeFormatPattern[1,1]);
   FindSubString('D', Value, FDateTimeFormatPattern[2,0], FDateTimeFormatPattern[2,1]);
   FindSubString('h', Value, FDateTimeFormatPattern[3,0], FDateTimeFormatPattern[3,1]);
   FindSubString('m', Value, FDateTimeFormatPattern[4,0], FDateTimeFormatPattern[4,1]);
   FindSubString('s', Value, FDateTimeFormatPattern[5,0], FDateTimeFormatPattern[5,1]);
   FindSubString('z', Value, FDateTimeFormatPattern[6,0], FDateTimeFormatPattern[6,1]);
   FDateTimeFormat := Value;
   end;
end;

procedure TSqlitePassDatabaseDataTypeOptions.SetFBooleanFormat(const Value: String);
var
TrueStr, FalseStr: String;
P: Integer;
begin

if FDatabase.CheckCanChangePropertyValue('TSqlitePassDatabaseDataTypeOptions.SetFBooleanFormat','BooleanFormat') then
   begin
   P := AnsiPos('-',Value);
   TrueStr  := Copy(Value,1, Pred(P));
   if TrueStr = ''
      then TrueStr := '1';
   FalseStr := Copy(Value,Succ(P), MaxInt);
   if FalseStr = ''
      then FalseStr := '0';
   FBooleanTrueValues.CommaText := TrueStr;
   FBooleanFalseValues.CommaText := FalseStr;
   FBooleanFormat := Value;
   end;
end;

procedure TSqlitePassDatabaseDataTypeOptions.SetFUnicodeEncoding(const Value: TSqlitePassUnicodeEncoding);
begin

if FDatabase.CheckCanChangePropertyValue('TSqlitePassDatabaseDataTypeOptions.SetFUnicodeEncoding','UnicodeEncoding') then
   begin
   if Value = ueAuto
   then begin
        Case FDatabase.SystemEncoding of
             sysANSI  : FUnicodeEncoding := ueUTF8;
             sysUTF8  : FUnicodeEncoding := ueUTF8;
             sysUTF16 : FUnicodeEncoding := ueUTF16;
             end;
         end
   else FUnicodeEncoding := Value;
   end;
end;

{ TSqlitePassDatabaseError }

constructor TSqlitePassDatabaseError.Create(Database: TSqlitePassDatabase);
begin
 Inherited Create;
 FDatabase := Database;
 FErrorStringList := TStringList.Create;
 FErrorCount := 0;
 FLastErrorCode := -1;
 FLastErrorMsg := Msg1002;
 FLastErrorTime := Now();
end;

destructor TSqlitePassDatabaseError.Destroy;
begin
 FDatabase := nil;
 FErrorStringList.Free;
 Inherited Destroy;
end;

procedure TSqlitePassDatabaseError.GetLastError(var ErrorCode: Integer; var Msg: String;
 var Time: TDateTime);
begin
 ErrorCode := FLastErrorCode;
 Msg  := FLastErrorMsg;
 Time := FLastErrorTime;
end;

procedure TSqlitePassDatabaseError.LogError(Msg: String; const Args: array of const; const ErrorCode: Integer = -1; Component: TComponent = nil);
begin
 FLastErrorCode := ErrorCode;

 if Component is TSqlitePassDataset
    then Msg := 'Dataset : ' + TSqlitePassDataset(Component).DatasetName + ' : ' + Msg;

 if High(Args) = -1
    then FLastErrorMsg := Msg
    else FLastErrorMsg := Format(Msg, Args);

 FLastErrorTime := Now();
 
 if FDatabase.FOptions.LogErrors
    then with FErrorStringList do
         begin
         Inc(FErrorCount);
         Add('--- Error ' + IntToStr(FErrorCount) + ' - ' + DateTimeToStr(FLastErrorTime) + ' Begin ---');
         Add('    Message : ' + FLastErrorMsg);
         Add('    Error code : ' + IntToStr(FLastErrorCode));
         Add('--- Error ' + IntToStr(FErrorCount) + ' End ---');
         Add('');
         end;
{$IFDEF DEBUG_SQLITEPASS}
 FDatabase.Debugger.Display(FLastErrorMsg);
{$ENDIF}
end;

procedure TSqlitePassDatabaseError.RaiseException(const Msg: String; const ErrorCode: Integer = -1;  Component: TComponent = nil; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow);
begin
 Case VerboseLevel of
      vlLog        : LogError(Msg, [], ErrorCode, Component);
      vlShow       : DatabaseError(Msg, Component);
      vlLogAndShow : begin
                     LogError(Msg, [], ErrorCode, Component);
                     DatabaseError(Msg, Component);
                     end;
      else Abort;
      end;
end;

procedure TSqlitePassDatabaseError.RaiseExceptionFmt(const Msg: String; const Args: array of const; const ErrorCode: Integer = -1; Component: TComponent = nil; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow);
begin
 If High(Args) = -1
    then RaiseException(Msg, ErrorCode, Component, VerboseLevel)
    else RaiseException(Format(Msg, Args), ErrorCode, Component, VerboseLevel);
end;

procedure TSqlitePassDatabaseError.SaveToFile(FileName: String);
begin
 FErrorStringList.SaveToFile(FileName);
end;

function TSqlitePassDatabaseError.ShowError(const Msg: String; DlgType: TMsgDlgType = mtWarning;
 DlgButtons: TMsgDlgButtons = [mbOk]; HelpCtx: Longint = 0): Word;
begin
 LogError(Msg, []);
 Result := MessageDlg(Msg, DlgType, DlgButtons, HelpCtx);
end;

procedure TSqlitePassDatabaseError.Clear;
begin
  FErrorStringList.Clear;
  FErrorCount := 0;
end;

