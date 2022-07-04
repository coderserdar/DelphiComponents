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
    Last update 05.01.2009

  --------------------------------------------------------------------------- }

{ Utility fonction that try to return the SQL type of a Statement
  from the Statement Text }
function GetStmtType(SQLText: String): StmtType;
var
i: Integer;
UpperSQL: String;
begin
Result := stUnknown;
UpperSQL := UpperCase(SQLText);

for i := 0 to High(StmtTypeKeywords) do
  if AnsiPos(StmtTypeKeywords[i], UpperSQL) > 0 then
     begin
     Case i of
     0   : Result := stCreate;
     1..3: Result := stCreateTable;
     4   : Result := stSelect;
     5   : Result := stInsert;
     6   : Result := stUpdate;
     7   : Result := stDelete;
     end;
     Break;
     end;
end;

{ TSqlitePassIndexColumn }

procedure TSqlitePassIndexColumn.Assign(Const Source: TSqlitePassIndexColumn);
begin
 Position          := Source.Position;
 CollatingSequence := Source.CollatingSequence;
 ColumnName        := Source.ColumnName;
 Direction         := Source.Direction;
end;

{ TSqlitePassIndexColumns }

constructor TSqlitePassIndexColumns.Create(Owner: TSqlitePassIndex);
begin
inherited Create;
FIndex := Owner;
end;

destructor TSqlitePassIndexColumns.Destroy;
begin
ClearAndFreeItems;
FIndex := nil;
inherited Destroy;
end;

procedure TSqlitePassIndexColumns.ClearAndFreeItems;
var
i: Integer;
begin
{ Clears the internal items }
for i := 0 to Pred(Count)
    do Items[i].Free;
{ Clears the list pointers }
inherited Clear;
end;

procedure TSqlitePassIndexColumns.Assign(Const Source: TSqlitePassIndexColumns);
begin
  ClearAndFreeItems;
  AddColumns(Source);
end;

procedure TSqlitePassIndexColumns.AddColumns(Source: TSqlitePassIndexColumns);
var
i: Integer;
NewColumn: TSqlitePassIndexColumn;
begin
for i := 0 to Pred(Source.Count) do
  begin
  NewColumn := TSqlitePassIndexColumn.Create;
  NewColumn.Assign(Source[i]);
  Add(NewColumn);
  end;
end;


function TSqlitePassIndexColumns.ColumnByName(
  const Value: String): TSqlitePassIndexColumn;
begin
  Result := FindColumn(Value);
  if Result = nil
     then DatabaseErrorFmt(Msg3200, [Value], nil);
end;

function TSqlitePassIndexColumns.FindColumn(
  const Value: String): TSqlitePassIndexColumn;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := TSqlitePassIndexColumn(inherited Items[I]);
    if AnsiCompareText(Result.ColumnName, Value) = 0 then Exit;
  end;
  Result := nil;
end;

function TSqlitePassIndexColumns.GetItem(
  Index: Integer): TSqlitePassIndexColumn;
begin
  Result:=TSqlitePassIndexColumn(Inherited Items[Index]);
end;

procedure TSqlitePassIndexColumns.SetItem(Index: Integer;
  const Value: TSqlitePassIndexColumn);
begin
  Put(Index,Value);
end;


{ TSqlitePassIndex }

constructor TSqlitePassIndex.Create;
begin
 inherited create;
 FColumns := TSqlitePassIndexColumns.Create(Self);
end;

destructor TSqlitePassIndex.Destroy;
begin
 FColumns.Free;
 inherited Destroy;
end;

procedure TSqlitePassIndex.SetFIndexName(Value: String);
begin
if IsValidIdent(Value)
   then FIndexName := Value
   else raise Exception.CreateFmt(Msg3220,[Value]);
end;

procedure TSqlitePassIndex.Assign(Source: TSqlitePassIndex; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
begin
 FAttachedDatabase  := AttachedDatabase;
 FTableName         := Source.FTableName;
 FIndexName         := Source.FIndexName;
 FOverwriteIfExists := Source.FOverwriteIfExists;
 FSql               := Source.FSql;
 FUnique            := Source.FUnique;
 FColumns.Assign(Source.FColumns);
end;

function TSqlitePassIndex.GetIndexFullName: String;
begin
 if Attached
    then Result :=  FAttachedDatabase.AliasName + '.' + FIndexName
    else Result :=  FIndexName;
end;

function TSqlitePassIndex.GetAttached: Boolean;
begin
  Result := (FAttachedDatabase <> nil);
end;

{ TSqlitePassDatabaseIndex }

constructor TSqlitePassDatabaseIndex.Create(Owner: TSqlitePassDatabaseIndexDefs);
begin
 inherited create;
 FIndexDefs := Owner;
end;

destructor TSqlitePassDatabaseIndex.Destroy;
begin
 FIndexDefs := nil;
 inherited Destroy;
end;

procedure TSqlitePassDatabaseIndex.DeleteIndex;
begin
 FindexDefs.FDatabase.Engine.ExecSQL('DROP INDEX ' + FIndexName);
end;

procedure TSqlitePassDatabaseIndex.Reindex;
begin
 FIndexDefs.FDatabase.Engine.ExecSQL('REINDEX ' + FIndexName);
end;


{ TSqlitePassDatabaseIndexDefs }

constructor TSqlitePassDatabaseIndexDefs.Create(Owner: TSqlitePassDatabase);
begin
inherited create;
FDatabase := Owner;
end;

destructor TSqlitePassDatabaseIndexDefs.Destroy;
begin
ClearAndFreeItems;
FDatabase := nil;
inherited Destroy;
end;

procedure TSqlitePassDatabaseIndexDefs.Assign(Source: TSqlitePassDatabaseIndexDefs; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
begin
  ClearAndFreeItems;
  AddIndexes(Source, AttachedDatabase);
end;

procedure TSqlitePassDatabaseIndexDefs.AddIndexes(Source: TSqlitePassDatabaseIndexDefs; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
var
i: Integer;
NewIndex: TSqlitePassDatabaseIndex;
begin
for i := 0 to Pred(Source.Count) do
  begin
  NewIndex := TSqlitePassDatabaseIndex.Create(Self);
  NewIndex.Assign(Source[i], AttachedDatabase);
  Add(NewIndex);
  end;
end;


function TSqlitePassDatabaseIndexDefs.GetItem(Index: Integer): TSqlitePassIndex;
begin
Result:=TSqlitePassIndex(Inherited Items[Index]);
end;

procedure TSqlitePassDatabaseIndexDefs.SetItem(Index: Integer;
  const Value: TSqlitePassIndex);
begin
  Put(Index,Value);
end;

procedure TSqlitePassDatabaseIndexDefs.ClearAndFreeItems;
var
i: Integer;
begin
{ The Datasets.IndexDefs points to DatabaseIndexDefs, so we clear it first }
// TODO - Verify if necessary -> Raise Exception
(*if Assigned(FDatabase.Datasets) then
   begin
   for i := 0 to Pred(FDatabase.Datasets.Count)
       do FDatabase.Datasets[i].IndexDefs.Clear;
   end; *)
{ Clear the internal items }
for i := 0 to Pred(Count) do
    Items[i].Free;
{ Clear the list pointers }
inherited Clear;
end;

procedure TSqlitePassDatabaseIndexDefs.CreateIndex(Sql: String);
begin
 FDatabase.Engine.ExecSQL(Sql);
 // TODO - Optimize -> don't refresh ALL indexes info but just add only the new one
 Refresh; 
end;

procedure TSqlitePassDatabaseIndexDefs.CreateIndex(Index: TSqlitePassDatasetIndex);
var
i: integer;
SqlStmt, UniqueStmt, FieldStmt: String;
begin
If Index.Unique
   then UniqueStmt := 'UNIQUE';

SqlStmt :=  'CREATE ' + UniqueStmt + ' INDEX "' + Index.IndexName +
            '" ON "' + Index.TableName + '" (';

For i := 0 to Pred(Index.Columns.Count) do
    begin
    FieldStmt := '"'+Index.Columns[i].ColumnName+'"';
    if Index.Columns[i].Direction = cidAscending
       then FieldStmt := FieldStmt + ' ASC, '
       else FieldStmt := FieldStmt + ' DESC, ';
    SqlStmt := SqlStmt + FieldStmt;
    end;
System.Delete(SqlStmt, Length(SqlStmt)-1,1);
SqlStmt := SqlStmt + ');';
FDatabase.Engine.ExecSQL(SqlStmt);
// TODO - Optimize -> don't refresh ALL indexes info but just add only the new one
Refresh;
end;

procedure TSqlitePassDatabaseIndexDefs.DeleteIndex(IndexName: String);
begin
 FDatabase.Engine.ExecSQL('DROP INDEX ' + IndexName + ';');
 // TODO - Optimize -> don't refresh ALL indexes info but just delete only the new one
 Refresh;
end;

function TSqlitePassDatabaseIndexDefs.FindIndex(
  const Value: String): TSqlitePassIndex;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := TSqlitePassIndex(inherited Items[I]);
    if (AnsiCompareText(Result.IndexName, Value) = 0)
       or (AnsiCompareText(Result.IndexFullName, Value) = 0)
       then Exit;
  end;
  Result := nil;
end;

function TSqlitePassDatabaseIndexDefs.IndexByName(
  const Value: String): TSqlitePassIndex;
begin
  Result := FindIndex(Value);
  if Result = nil then
    DatabaseErrorFmt(Msg3200, [Value], FDatabase);
end;

procedure TSqlitePassDatabaseIndexDefs.Refresh;
var
i: Integer;
begin
With FDatabase do
     begin
     OpenAttachedDatabases;
     Translator.GetDatabaseIndexDefs(Self);
     CloseAttachedDatabases;
     For i := 0 to Pred(Datasets.Count)
         do Datasets[i].IndexDefs.RefreshFromCache;
     end;
end;

procedure TSqlitePassDatabaseIndexDefs.Reindex;
begin
// TODO - TSqlitePassDatabaseIndexDefs.Reindex - Optimise
 FDatabase.Engine.ExecSQL('Reindex Binary');
end;

procedure TSqlitePassDatabaseIndexDefs.RenameIndex(OldIndexName,
  NewIndexName: String);
var
CurrentIndex: TSqlitePassIndex;
begin
if NewIndexName = ''
   then exit;
CurrentIndex := IndexByName(OldIndexName);
If Assigned(CurrentIndex)
   then begin
        CurrentIndex.IndexName := NewIndexName;
        CreateIndex(TSqlitePassDatasetIndex(CurrentIndex));
        DeleteIndex(OldIndexName);
        Refresh;
        end;
end;

{ TSqlitePassGenericTableDef }

procedure TSqlitePassGenericTableDef.Assign(Source: TSqlitePassGenericTableDef; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
begin
 FAttachedDatabase := AttachedDatabase;
 FAliasName := Source.AliasName;
 FCanModify := Source.CanModify;
 FOnConflict := Source.OnConlict;
 FSql := Source.Sql;
 FTableName := Source.TableName;
 FTemporary := Source.Temporary;
end;

function TSqlitePassGenericTableDef.GetTableFullName: String;
begin
 if Attached
    then Result :=  FAttachedDatabase.AliasName + '.' + FTableName
    else Result :=  FTableName;
end;

function TSqlitePassGenericTableDef.GetAttached: Boolean;
begin
 Result := (FAttachedDatabase <> nil);
end;


{ TSqlitePassTableDef }

constructor TSqlitePassTableDef.Create(Owner: TSqlitePassTableDefs);
begin
  Inherited Create;
  FTableDefs := Owner;
  FFieldDefs := TSqlitePassTableFieldDefs.Create(Self);
end;

destructor TSqlitePassTableDef.Destroy;
begin
  FFieldDefs.Free;
  FTableDefs := nil;
  Inherited Destroy;
end;

procedure TSqlitePassTableDef.Assign(Source: TSqlitePassTableDef; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
begin
 Inherited Assign(Source, AttachedDatabase);
 FFieldDefs.Assign(Source.FFieldDefs);
end;

procedure TSqlitePassTableDef.CopyTable(NewTableName: String; TempTable: Boolean = False; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
var
Tokenizer: TSqlitePassSQLTokenizer;
begin
Tokenizer := TSqlitePassSQLTokenizer.Create;
Tokenizer.Text := SQL;
// TODO - Check for Quotes
Tokenizer.Replace(FTableName, NewTableName);

if TempTable
   then Tokenizer.Replace('CREATE', 'CREATE TEMPORARY');

With FTableDefs.FDatabase.Engine do
 begin
 Transaction.Start;
   try
   ExecSQL(Tokenizer.Text);
   ExecSQL('INSERT INTO "' + NewTableName + '" SELECT * FROM "'+ FTableName + '";');
   Transaction.Commit;
   except
   Transaction.RollBack;
   end;
 end;
end;

procedure TSqlitePassTableDef.DeleteTable(Silent: Boolean = False);
begin
 FTableDefs.DeleteTable(FTableName, Silent);
end;

procedure TSqlitePassTableDef.Reindex;
begin
 FTableDefs.Reindex(FTableName);
end;

procedure TSqlitePassTableDef.RenameAs(NewName: String);
begin
 FTableDefs.RenameAs(FTableName, NewName);
end;

{ TSqlitePassTableDefs }

constructor TSqlitePassTableDefs.Create(Owner: TSqlitePassDatabase);
begin
inherited Create;
FDatabase := Owner;
end;

destructor TSqlitePassTableDefs.Destroy;
begin
ClearAndFreeItems;
FDatabase := nil;
inherited Destroy;
end;

procedure TSqlitePassTableDefs.ClearAndFreeItems;
var
i: Integer;
begin
{ Clear the internal items }
for i := 0 to Pred(Count)
   do if Assigned(Items[i]) then Items[i].Free;
{ Clear the list pointers }
inherited Clear;
end;

procedure TSqlitePassTableDefs.Assign(Source: TSqlitePassTableDefs; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
begin
  ClearAndFreeItems;
  AddTableDefs(Source, AttachedDatabase);
end;

procedure TSqlitePassTableDefs.AddTableDefs(Source: TSqlitePassTableDefs; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
var
i: Integer;
NewTableDef: TSqlitePassTableDef;
begin
for i := 0 to Pred(Source.Count) do
  begin
  NewTableDef := TSqlitePassTableDef.Create(Self);
  NewTableDef.Assign(Source[i], AttachedDatabase);
  Add(NewTableDef);
  end;
end;

procedure TSqlitePassTableDefs.CreateTable(Const Sql: String);
begin
 FDatabase.Engine.ExecSQL(Sql);
 { Gets Tables Definitions from database }
 FDatabase.Translator.GetDatabaseTableDefs(Self);
end;

procedure TSqlitePassTableDefs.CopyTable(Const TableName, NewTableName: String; TempTable: Boolean = False; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
var
TableDef: TSqlitePassTableDef;
begin
 TableDef := TableByName(TableName);
 TableDef.CopyTable(NewTableName, TempTable, AttachedDatabase);
 { Gets Tables Definitions from database }
 FDatabase.Translator.GetDatabaseTableDefs(Self);
end;

procedure TSqlitePassTableDefs.DeleteTable(Const TableName: String; Silent: Boolean = False);
var
SilentMode: String;
begin
 If Silent
    then SilentMode := 'if exists '
    else SilentMode := '';

 FDatabase.Engine.ExecSQL('Drop table ' + SilentMode + TableName + ';');
 { Gets Tables Definitions from database }
 FDatabase.Translator.GetDatabaseTableDefs(Self);
end;

procedure TSqlitePassTableDefs.EmptyTable(Const TableName: String);
begin
 FDatabase.Engine.ExecSQL('DELETE FROM "' + TableName + '";');
end;

function TSqlitePassTableDefs.FindTable(const Value: String): TSqlitePassTableDef;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := TSqlitePassTableDef(inherited Items[I]);
    if (AnsiCompareText(Result.TableName, Value) = 0)
    or (AnsiCompareText(Result.TableFullName, Value)= 0)
       then Exit;
  end;
  Result := nil;
end;

function TSqlitePassTableDefs.GetItem(Index: Integer): TSqlitePassTableDef;
begin
Result:=TSqlitePassTableDef(Inherited Items[Index]);
end;

procedure TSqlitePassTableDefs.Refresh;
begin
With FDatabase do
     begin
     { Sets the default Fields Translation rules for the current database type }
     FDatatypeOptions.TranslationRules.LoadDefaultTranslationRules;
     { Loads and applies the DatatypeOptions depending on the LoadOptions setting }
     FDatatypeOptions.LoadFromDatabase;
     { Gets Tables Definitions from database }
     OpenAttachedDatabases;
     Translator.GetDatabaseTableDefs(Self);
     CloseAttachedDatabases;
     { Overrides fieldDefs settings with CustomFieldDefs Settings }
     DataTypeOptions.ApplyCustomFieldDefs;
     end;
end;

procedure TSqlitePassTableDefs.Reindex(Const TableName: String);
begin
 FDatabase.Engine.ExecSQL('Reindex ' + TableName);
end;


procedure TSqlitePassTableDefs.RenameAs(Const TableName, NewName: String);
begin
 // TODO - Change Associated triggers to new table name
 if NewName <> '' then
    begin
    FDatabase.Engine.ExecSQL('ALTER TABLE ' + TableName + ' RENAME TO ' + NewName);
    FDatabase.Translator.GetDatabaseTableDefs(Self);
    end;
end;

procedure TSqlitePassTableDefs.SetItem(Index: Integer;
  const Value: TSqlitePassTableDef);
begin
  Put(Index,Value);
end;

function TSqlitePassTableDefs.TableByName(
  const Value: String): TSqlitePassTableDef;
begin
  Result := FindTable(Value);
  if Result = nil then
     DatabaseErrorFmt(Msg3700, [Value], FDatabase);
end;

function TSqlitePassTableDefs.FindFieldDef(Const TableName, FieldName: String): TSqlitePassTableFieldDef;
var
TableDef: TSqlitePassTableDef;
begin
 Result := nil;
 TableDef := FindTable(TableName);
 if TableDef <> nil
    then Result := TableDef.FieldDefs.FindFieldDef(FieldName);
end;

function TSqlitePassTableDefs.FindFieldTableName(Const FieldName: String; TableNames: TStringList): String;
var
i: integer;
FieldDef: TSqlitePassTableFieldDef;
begin
{ If TableNames is empty, we scan ALL the TableDefs.FieldDefs }
if TableNames.Count = 0 then
   For i := 0 to Pred(Count)
       do TableNames.Add(Items[i].TableName);

Result := '';
for i := 0 to Pred(TableNames.Count) do
  begin
  FieldDef := FindFieldDef(TableNames[i], FieldName);
  if FieldDef <> nil then
     begin
     Result := TableNames[i];
     Exit;
     end;
  end;
end;


{ TSqlitePassView }

constructor TSqlitePassView.Create(Owner: TSqlitePassViews);
begin
 inherited Create;
 FViews := Owner;
end;

destructor TSqlitePassView.Destroy;
begin
 FViews := nil;
 inherited Destroy;
end;

procedure TSqlitePassView.Assign(Source: TSqlitePassView; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
begin
 FAttachedDatabase := AttachedDatabase;
 FViewName         := Source.FViewName;
 FTemp             := Source.FTemp;
 FSqlCreateStmt    := Source.FSqlCreateStmt;
 FSqlSelectStmt    := Source.FSqlSelectStmt;
end;

procedure TSqlitePassView.DeleteView;
begin
 FViews.DeleteView(ViewFullName);
end;

function TSqlitePassView.GetAttached: Boolean;
begin
 Result := (FAttachedDatabase <> nil);
end;

function TSqlitePassView.GetViewFullName: String;
begin
 if Attached
    then Result :=  FAttachedDatabase.AliasName + '.' + FViewName
    else Result :=  FViewName;
end;

{ TSqlitePassViews }

constructor TSqlitePassViews.Create(Owner: TSqlitePassDatabase);
begin
 inherited Create;
 FDatabase := Owner;
end;

destructor TSqlitePassViews.Destroy;
begin
ClearAndFreeItems;
FDatabase := nil;
inherited Destroy;
end;

procedure TSqlitePassViews.ClearAndFreeItems;
var
i: Integer;
begin
{ Clear the internal items }
for i := 0 to Pred(Count)
    do Items[i].Free;
{ Clear the list pointers }
inherited Clear;
end;

procedure TSqlitePassViews.CreateView(Const ViewName, SQL: String;
          Const Temp: Boolean = False; Const Silent: Boolean = True);
var
TempView, SilentMode: String;
begin
If Temp
   then TempView := ' TEMPORARY '
   else TempView := '';

If Silent
  then SilentMode := 'IF NOT EXISTS '
  else SilentMode := '';

 FDatabase.Engine.ExecSQL('CREATE VIEW ' + TempView + SilentMode + ' "' + ViewName + '" AS ' + SQL + ';');
 Refresh;
end;

procedure TSqlitePassViews.DeleteView(ViewName: String);
begin
 FDatabase.Engine.ExecSQL('DROP VIEW ' + ViewName + ';');
 Refresh;
end;

function TSqlitePassViews.FindView(const Value: String): TSqlitePassView;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := TSqlitePassView(inherited Items[I]);
    if (AnsiCompareText(Result.ViewName, Value) = 0)
       or (AnsiCompareText(Result.ViewFullName, Value) = 0)
       then Exit;
  end;
  Result := nil;
end;

function TSqlitePassViews.GetItem(Index: Integer): TSqlitePassView;
begin
 Result:=TSqlitePassView(Inherited Items[Index]);
end;

procedure TSqlitePassViews.Refresh;
begin
With FDatabase do
     begin
     OpenAttachedDatabases;
     Translator.GetDatabaseViews(Self);
     CloseAttachedDatabases;
     end;
end;

procedure TSqlitePassViews.SetItem(Index: Integer;
  const Value: TSqlitePassView);
begin
  Put(Index,Value);
end;

function TSqlitePassViews.ViewByName(const Value: String): TSqlitePassView;
begin
  Result := FindView(Value);
  if Result = nil
     then DatabaseErrorFmt(Msg3400, [Value], FDatabase);
end;

procedure TSqlitePassViews.Assign(Source: TSqlitePassViews; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
begin
  ClearAndFreeItems;
  AddViews(Source, AttachedDatabase);
end;

procedure TSqlitePassViews.AddViews(Source: TSqlitePassViews; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
var
i: Integer;
NewView: TSqlitePassView;
begin
for i := 0 to Pred(Source.Count) do
  begin
  NewView := TSqlitePassView.Create(Self);
  NewView.Assign(Source[i], AttachedDatabase);
  Add(NewView);
  end;
end;

{ TSqlitePassTrigger }

constructor TSqlitePassTrigger.Create(Owner: TSqlitePassTriggers);
begin
 inherited Create;
 FTriggers := Owner;
end;

destructor TSqlitePassTrigger.Destroy;
begin
 FTriggers := nil;
 inherited Destroy;
end;

procedure TSqlitePassTrigger.Assign(Source: TSqlitePassTrigger; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
begin
 FAttachedDatabase := AttachedDatabase;
 FTriggerName      := Source.FTriggerName;
 FTemporary        := Source.FTemporary;
 FSql              := Source.FSql;
end;

procedure TSqlitePassTrigger.DeleteTrigger;
begin
 FTriggers.FDatabase.Engine.ExecSQL('Drop trigger '+ FTriggerName + ';');
end;

function TSqlitePassTrigger.GetAttached: Boolean;
begin
 Result := (FAttachedDatabase <> nil);
end;

function TSqlitePassTrigger.GetTriggerFullName: String;
begin
 if Attached
    then Result :=  FAttachedDatabase.AliasName + '.' + FTriggerName
    else Result :=  FTriggerName;
end;


{ TSqlitePassTriggers }

constructor TSqlitePassTriggers.Create(Owner: TSqlitePassDatabase);
begin
 inherited Create;
 FDatabase := Owner;
end;

destructor TSqlitePassTriggers.Destroy;
begin
ClearAndFreeItems;
FDatabase := nil;
inherited Destroy;
end;

procedure TSqlitePassTriggers.ClearAndFreeItems;
var
i: Integer;
begin
{ Clear the internal items }
for i := 0 to Pred(Count)
    do Items[i].Free;
{ Clear the list pointers }
inherited Clear;
end;

procedure TSqlitePassTriggers.Assign(Source: TSqlitePassTriggers; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
begin
  ClearAndFreeItems;
  AddTriggers(Source, AttachedDatabase);
end;

procedure TSqlitePassTriggers.AddTriggers(Source: TSqlitePassTriggers; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
var
i: Integer;
NewTrigger: TSqlitePassTrigger;
begin
for i := 0 to Pred(Source.Count) do
  begin
  NewTrigger := TSqlitePassTrigger.Create(Self);
  NewTrigger.Assign(Source[i], AttachedDatabase);
  Add(NewTrigger);
  end;
end;

procedure TSqlitePassTriggers.CreateTrigger(Sql: String);
begin
 FDatabase.Engine.ExecSQL(Sql);
end;

procedure TSqlitePassTriggers.DeleteTrigger(TriggerName: String);
begin
  FDatabase.Engine.ExecSQL('Drop trigger ' + TriggerName + ';');
end;

function TSqlitePassTriggers.FindTrigger(
  const Value: String): TSqlitePassTrigger;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := TSqlitePassTrigger(inherited Items[I]);
    if (AnsiCompareText(Result.TriggerName, Value) = 0)
       or (AnsiCompareText(Result.TriggerFullName, Value) = 0)
       then Exit;
  end;
  Result := nil;
end;

function TSqlitePassTriggers.GetItem(Index: Integer): TSqlitePassTrigger;
begin
Result:=TSqlitePassTrigger(Inherited Items[Index]);
end;

procedure TSqlitePassTriggers.Refresh;
begin
With FDatabase do
     begin
     OpenAttachedDatabases;
     Translator.GetDatabaseTriggers(Self);
     CloseAttachedDatabases;
     end;
end;

procedure TSqlitePassTriggers.SetItem(Index: Integer;
  Value: TSqlitePassTrigger);
begin
   Put(Index,Value);
end;

function TSqlitePassTriggers.TriggerByName(
  const Value: String): TSqlitePassTrigger;
begin
  Result := FindTrigger(Value);
  if Result = nil then
    DatabaseErrorFmt(Msg3500, [Value], FDatabase);
end;


{ TSqlitePassQueryDef }

constructor TSqlitePassQueryDef.Create(Owner: TSqlitePassQueryDefs);
begin
inherited Create;
FQueryDefs:=Owner;
FFieldDefs := TFieldDefs.Create(nil);
end;

destructor TSqlitePassQueryDef.Destroy;
begin
FFieldDefs.Free;
FQueryDefs:=nil;
inherited Destroy;
end;

procedure TSqlitePassQueryDef.Assign(Source: TSqlitePassQueryDef; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
begin
 FAttachedDatabase := AttachedDatabase;
 FQueryDefs := Source.FQueryDefs;
 FQueryName := Source.FQueryName;
 FSql       := Source.FSql;
 FFieldDefs.Assign(Source.FFieldDefs);
 FParams.Assign(Source.FParams);
end;

procedure TSqlitePassQueryDef.DeleteQuery;
begin
// TODO : TSqlitePassQueryDef.DeleteQuery - To be implemented
end;

function TSqlitePassQueryDef.GetAttached: Boolean;
begin
 Result := (FAttachedDatabase <> nil);
end;

function TSqlitePassQueryDef.GetQueryFullName: String;
begin
 if Attached
    then Result :=  FAttachedDatabase.AliasName + '.' + FQueryName
    else Result :=  FQueryName;
end;

procedure TSqlitePassQueryDef.RenameAs(NewName: String);
begin
// TODO : TSqlitePassQueryDef.RenameAs - To be implemented
end;


{ TSqlitePassQueryDefs }

constructor TSqlitePassQueryDefs.Create(Owner: TSqlitePassDatabase; AttachedDb: TSqlitePassDatabaseAttached = nil);
begin
inherited Create;
FDatabase := Owner;
FAttachedDatabase := nil;
end;

destructor TSqlitePassQueryDefs.Destroy;
begin
ClearAndFreeItems;
FAttachedDatabase := nil;
FDatabase := nil;
inherited Destroy;
end;

procedure TSqlitePassQueryDefs.ClearAndFreeItems;
var
i: integer;
begin
{ Clears the internal items }
For i := 0 to Pred(Count)
    do Items[i].Free;
{ Clears the list pointers }
inherited Clear;
end;

procedure TSqlitePassQueryDefs.Assign(Source: TSqlitePassQueryDefs; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
begin
  ClearAndFreeItems;
  AddQueryDefs(Source, AttachedDatabase);
end;

procedure TSqlitePassQueryDefs.AddQueryDefs(Source: TSqlitePassQueryDefs; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
var
i: Integer;
NewQueryDef: TSqlitePassQueryDef;
begin
for i := 0 to Pred(Source.Count) do
  begin
  NewQueryDef := TSqlitePassQueryDef.Create(Self);
  NewQueryDef.Assign(Source[i], AttachedDatabase);
  Add(NewQueryDef);
  end;
end;

procedure TSqlitePassQueryDefs.CreateQuery(Sql: String);
begin
// TODO : TSqlitePassQueryDefs.CreateQuery - To be implemented
end;

procedure TSqlitePassQueryDefs.DeleteQuery(QueryName: String);
begin
// TODO : TSqlitePassQueryDefs.DeleteQuery - To be implemented
end;

function TSqlitePassQueryDefs.FindQuery(
  const Value: String): TSqlitePassQueryDef;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := TSqlitePassQueryDef(inherited Items[I]);
    if AnsiCompareText(Result.QueryFullName, Value) = 0 then Exit;
  end;
  Result := nil;
end;

function TSqlitePassQueryDefs.GetItem(Index: Integer): TSqlitePassQueryDef;
begin
 Result:=TSqlitePassQueryDef(Inherited Items[Index]);
end;

procedure TSqlitePassQueryDefs.SetItem(Index: Integer;
  const Value: TSqlitePassQueryDef);
begin
  Put(Index,Value);
end;

function TSqlitePassQueryDefs.QueryByName(
  const Value: String): TSqlitePassQueryDef;
begin
  Result := FindQuery(Value);
  if Result = nil then
    DatabaseErrorFmt(Msg3300, [Value], FDatabase);
end;

procedure TSqlitePassQueryDefs.Refresh;
begin
With FDatabase do
     begin
     OpenAttachedDatabases;
     Translator.GetDatabaseQueryDefs(Self);
     CloseAttachedDatabases;
     end;
end;


{ TSqlitePassDatabasesAttached }

constructor TSqlitePassDatabasesAttached.Create(Owner: TSqlitePassDatabase);
begin
 Inherited Create;
 FDatabase := Owner;
end;

Destructor TSqlitePassDatabasesAttached.Destroy;
begin
 DetachAll;
 FDatabase := nil;
 Inherited Destroy;
end;

procedure TSqlitePassDatabasesAttached.DetachAll;
begin
While Count > 0 do FDatabase.DetachDatabase(GetItem(Pred(Count)).AliasName)
end;

function TSqlitePassDatabasesAttached.DatabaseByName(
  const Name: String): TSqlitePassDatabaseAttached;
begin
  Result := FindDatabase(Name);
  if Result = nil
     then DatabaseErrorFmt(Msg1800, [Name], FDatabase);
end;

function TSqlitePassDatabasesAttached.FindDatabase(
  const Value: String): TSqlitePassDatabaseAttached;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := TSqlitePassDatabaseAttached(inherited Items[I]);
    if AnsiCompareText(Result.AliasName, Value) = 0 then Exit;
  end;
  Result := nil;
end;

function TSqlitePassDatabasesAttached.GetItem(
  Index: Integer): TSqlitePassDatabaseAttached;
begin
 Result:=TSqlitePassDatabaseAttached(Inherited Items[Index]);
end;

procedure TSqlitePassDatabasesAttached.SetItem(Index: Integer;
  Database: TSqlitePassDatabaseAttached);
begin
 Put(Index,Database);
end;

{ TSqlitePassDatabaseAttached }

constructor TSqlitePassDatabaseAttached.Create(Databases: TSqlitePassDatabasesAttached);
begin
  Inherited Create;
  FDatabasesList := Databases;
end;

destructor TSqlitePassDatabaseAttached.Destroy;
begin
  FDatabasesList := nil;
  Inherited Destroy;
end;


{ TSqlitePassGenericFieldDef }
Procedure TSqlitePassGenericFieldDef.Assign(Source: TSqlitePassGenericFieldDef);
begin
  FNoConstraints := Source.NoConstraints;
  FAutoInc:= Source.AutoInc;
  FDefaultValue := Source.DefaultValue;
  FUnique := Source.Unique;
  FPrimaryKey := Source.PrimaryKey;
  FForeignKey := Source.ForeignKey;
  FNotNull:= Source.NotNull;
  FNotEmpty := Source.NotEmpty;
  FIndexed := Source.Indexed;
  SetFFieldName(Source.FieldName);
  FNativeDataType := Source.FNativeDataType;
  FNativeDataTypeCode := Source.NativeDataTypeCode;
  FDataType := Source.DataType;
  FDisplayName := Source.DisplayName;
  FSize := Source.Size;
  FPrecision:= Source.Precision;
  FHint := Source.Hint;
end;

Procedure TSqlitePassGenericFieldDef.SetFFieldName(Value: String);
begin
  FFieldName := Value;
end;


Procedure TSqlitePassGenericFieldDef.SetFDataType(Value: TFieldType);
begin
  FDataType := Value;
end;


{ TSqlitePassTableFieldDef }

constructor TSqlitePassTableFieldDef.Create(
  Owner: TSqlitePassTableFieldDefs);
begin
 Inherited Create;
 FFieldDefs := Owner;
 FRecordFieldIndex := -1;
end;

destructor TSqlitePassTableFieldDef.Destroy;
begin
 FFieldDefs := nil;
 Inherited Destroy;
end;

procedure TSqlitePassTableFieldDef.Assign(Source: TSqlitePassTableFieldDef);
begin
 inherited Assign(TSqlitePassGenericFieldDef(Source));
end;

function TSqlitePassTableFieldDef.GetFFieldFullName: String;
begin
 Result := FFieldDefs.FTableDef.TableName + '.' + FFieldName;
end;


{ TSqlitePassTableFieldDefs }

constructor TSqlitePassTableFieldDefs.Create(Owner: TSqlitePassTableDef);
begin
 Inherited Create;
 FTableDef := Owner;
end;

destructor TSqlitePassTableFieldDefs.Destroy;
begin
 ClearAndFreeItems;
 FTableDef := nil;
 Inherited Destroy;
end;

procedure TSqlitePassTableFieldDefs.Assign(Source: TSqlitePassTableFieldDefs);
var
i: integer;
NewFieldDef: TSqlitePassTableFieldDef;
begin
for i := 0 to Pred(Source.Count) do
   begin
   NewFieldDef := TSqlitePassTableFieldDef.Create(Self);
   NewFieldDef.Assign(Source[i]);
   Self.Add(NewFieldDef);
   end;
end;

function TSqlitePassTableFieldDefs.FieldDefByName(
  const Value: String): TSqlitePassTableFieldDef;
begin
  Result := FindFieldDef(Value);
  if Result = nil
     then DatabaseErrorFmt(Msg3600, [Value], nil);
end;

function TSqlitePassTableFieldDefs.FindFieldDef(
  const Value: String): TSqlitePassTableFieldDef;
var
  i: Integer;
begin
if (AnsiCompareText(Value,'Rowid') = 0)
or (AnsiCompareText(Value,'Oid') = 0)
or (AnsiCompareText(Value,'_Rowid_') = 0)
   then begin
        Result := TSqlitePassTableFieldDef(inherited Items[0]);
        Exit;
        end
   else begin
        for i := 0 to Pred(Count) do
            begin
            Result := TSqlitePassTableFieldDef(inherited Items[I]);
            if (AnsiCompareText(Result.FieldName, Value) = 0)
            or (AnsiCompareText(Result.FieldFullName, Value) = 0)
               then Exit;
            end;
         end;
Result := nil;
end;

function TSqlitePassTableFieldDefs.GetItem(Index: Integer): TSqlitePassTableFieldDef;
begin
  Result:=TSqlitePassTableFieldDef(Inherited Items[Index]);
end;

procedure TSqlitePassTableFieldDefs.SetItem(Index: Integer;
  const Value: TSqlitePassTableFieldDef);
begin
  Put(Index,Value);
end;

procedure TSqlitePassTableFieldDefs.ClearAndFreeItems;
var
i: Integer;
begin
{ Clears the internal items }
for i := 0 to Pred(Count)
    do Items[i].Free;
{ Clears the list pointers }
inherited Clear;
end;


{ TSqlitePassDatasets }

constructor TSqlitePassDatasets.Create(Owner: TSqlitePassDatabase);
begin
inherited Create;
FDatabase := Owner;
end;

destructor TSqlitePassDatasets.Destroy;
begin
FDatabase := nil;
inherited Destroy;
end;

function TSqlitePassDatasets.FindDataset(
  const Value: String): TSqlitePassDataset;
var
  I: Integer;
begin
for I := 0 to Pred(Count) do
  begin
    Result := TSqlitePassDataset(inherited Items[I]);
    if AnsiCompareText(Result.FDatasetFullName, Value) = 0 then Exit;
  end;
  Result := nil;
end;

Function TSqlitePassDatasets.GetItem(Index: Integer): TSqlitePassDataset;
begin
 Result:=TSqlitePassDataset(Inherited Items[Index]);
end;

Procedure TSqlitePassDatasets.SetItem(Index: Integer; Dataset: TSqlitePassDataset);
begin
Put(Index,Dataset);
end;

function TSqlitePassDatasets.DatasetByName(Const Name: String): TSqlitePassDataset;
var
Index: Integer;
begin
Result := Nil;
for index := 0 to Count -1 do
    if Items[Index].Name = Name
       then begin
            result := Items[Index];
            Exit
            end;
If Result = nil then raise Exception.CreateFmt(Msg2070,[Name])
end;

{ Closes all opened datasets }
procedure TSqlitePassDatasets.Close;
var
i: Integer;
begin
For i := 0 to Pred(Count)
    do Items[i].Close;
end;


{ TSqlitePassSQLStmtDef }

constructor TSqlitePassSQLStmtDef.Create(SQLStmts: TSqlitePassSqlStmtDefs);
begin
 inherited Create;
 FSQLStmtDefs := SQLStmts;
end;

destructor TSqlitePassSQLStmtDef.Destroy;
begin
 FSQLStmtDefs := nil;
 inherited Destroy;
end;

procedure TSqlitePassSQLStmtDef.Assign(Source: TSqlitePassSQLStmtDef;
  AttachedDatabase: TSqlitePassDatabaseAttached);
begin
 FAttachedDatabase := AttachedDatabase;
 FSQLText          := Source.FSQLText;
 FSQLStmtName      := Source.FSQLStmtName;
end;

function TSqlitePassSQLStmtDef.GetStmtType: StmtType;
begin
 Result := SqlitePassDbo.GetStmtType(FSQLText);
end;

function TSqlitePassSQLStmtDef.GetAttached: Boolean;
begin
 Result := (FAttachedDatabase <> nil);
end;

procedure TSqlitePassSQLStmtDef.DeleteSQLStmt;
begin
 FSQLStmtDefs.DeleteSQLStmt(SQLStmtFullName);
end;

function TSqlitePassSQLStmtDef.GetSQLStmtFullName: String;
begin
  if Attached
    then Result :=  FAttachedDatabase.AliasName + '.' + FSQLStmtName
    else Result :=  FSQLStmtName;
end;

{ TSqlitePassSQLStmtDefs }

constructor TSqlitePassSQLStmtDefs.Create(Owner: TSqlitePassDatabase);
begin
inherited Create;
FDatabase := Owner;
end;

destructor TSqlitePassSQLStmtDefs.Destroy;
begin
ClearAndFreeItems;
FDatabase := nil;
inherited Destroy;
end;

procedure TSqlitePassSQLStmtDefs.ClearAndFreeItems;
var
i: Integer;
begin
{ Clear the internal items }
for i := 0 to Pred(Count)
   do if Assigned(Items[i]) then Items[i].Free;
{ Clear the list pointers }
inherited Clear;
end;

procedure TSqlitePassSQLStmtDefs.Assign(Source: TSqlitePassSQLStmtDefs; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
begin
  ClearAndFreeItems;
  AddSQLStmts(Source, AttachedDatabase);
end;

procedure TSqlitePassSQLStmtDefs.AddSQLStmts(Source: TSqlitePassSQLStmtDefs; AttachedDatabase: TSqlitePassDatabaseAttached = nil);
var
i: Integer;
NewSQLStmtDef: TSqlitePassSQLStmtDef;
begin
for i := 0 to Pred(Source.Count) do
  begin
  NewSQLStmtDef := TSqlitePassSQLStmtDef.Create(Self);
  NewSQLStmtDef.Assign(Source[i], AttachedDatabase);
  Add(NewSQLStmtDef);
  end;
end;


{ UpdateSQLStmtTable create a internal table in the current database.
  This internal table is used to store user sql statements inside the database }

procedure TSqlitePassSQLStmtDefs.UpdateSQLStmtTable(Const UpdateType: Integer; Const SQLStmtName: String;
                                                 Const NewSQLStmtName: String; Const SQLStmt: TStringList);
Const
Sql = 'CREATE TABLE IF NOT EXISTS "' + SqlitePassSQLStmtDefsTable + '" ("Name" TEXT(100) NOT NULL UNIQUE, "Statement" CLOB);';
var
WasConnected, WasReadOnly: Boolean;
NewSQLStmtDef: TSqlitePassSQLStmtDef;
DbSQLStmtsDataset: TSqlitePassDataset;
DbSQLStmtsTableDef: TSqlitePassTableDef;

begin
WasConnected := False;
WasReadOnly := False;
If Assigned(FDatabase) and Assigned(FDatabase.FEngine) then
   begin
   try
     DbSQLStmtsDataset := nil;
     WasConnected := FDatabase.Connected;
     WasReadOnly  := FDatabase.ReadOnly;
     FDatabase.ReadOnly := False;
     FDatabase.Open;

     { Creates the table if it doesn't already exist }
     DbSQLStmtsTableDef := FDatabase.FTableDefs.FindTable(SqlitePassSQLStmtDefsTable);
     if DbSQLStmtsTableDef = nil then
        begin
        FDatabase.FEngine.ExecSQL(Sql);
        FDatabase.Translator.GetDatabaseTableDefs(FDatabase.TableDefs);
        DbSQLStmtsTableDef := FDatabase.FTableDefs.FindTable(SqlitePassSQLStmtDefsTable)
        end;

     { Enforces fields fieldTypes to ftMemo whatever Databasetype is used }
      DbSQLStmtsTableDef.FieldDefs[0].DataType := ftString;
      DbSQLStmtsTableDef.FieldDefs[1].DataType := ftMemo;

     { Saves SQLStmts to database }
     DbSQLStmtsDataset:= TSqlitePassDataset.Create(nil);
     DbSQLStmtsDataset.Database := FDatabase; { Kept before With .. do because of incorrect compiler assigment ? }
     With DbSQLStmtsDataset do
          begin
          DatasetName := SqlitePassSQLStmtDefsTable;
          ReadOnly := False;
          Open;
          Case UpdateType of
          { Create or Update }
          0: begin
             if Locate('Name', SQLStmtName, [loCaseInsensitive])
                then begin
                     Edit;
                     SQLStmtByName(SQLStmtName).SQL := SQLStmt.Text;

                     end
                else begin
                     Append;
                     NewSQLStmtDef := TSqlitePassSQLStmtDef.Create(Self);
                     NewSQLStmtDef.SQLStmtName := SQLStmtName;
                     NewSQLStmtDef.SQL := SQLStmt.Text;
                     Add(NewSQLStmtDef);
                     end;
              FieldByName('Name').Value := SQLStmtName;
              FieldByName('Statement').Assign(SQLStmt);
              Post;
              end;
          { Rename }
          1: begin
             if Locate('Name', SQLStmtName, [loCaseInsensitive])
                then begin
                     Edit;
                     FieldByName('Name').Value := NewSQLStmtName;
                     Post;
                     SQLStmtByName(SQLStmtName).SQLStmtName := NewSQLStmtName;
                     end;
             end;

          { Delete }
          2: begin
             if Locate('Name', SQLStmtName, [loCaseInsensitive])
                then begin
                     Delete;
                     Remove(SQLStmtByName(SQLStmtName));
                     end;
             end;
          end;
      end { With }
   finally
     if Assigned(DbSQLStmtsDataset)
        then DbSQLStmtsDataset.Free;
     FDatabase.ReadOnly  := WasReadOnly;
     FDatabase.Connected := WasConnected;
   end;
   end;
end;

procedure TSqlitePassSQLStmtDefs.CreateSQLStmt(Const SQLStmtName: String; Const SQLStmt: TStringList);
begin
UpdateSQLStmtTable(0, SQLStmtName, '', SQLStmt);
end;

procedure TSqlitePassSQLStmtDefs.RenameSQLStmt(Const OldName, NewName: String);
begin
UpdateSQLStmtTable(1, OldName, NewName, nil);
end;

procedure TSqlitePassSQLStmtDefs.DeleteSQLStmt(Const SQLStmtName: String);
begin
UpdateSQLStmtTable(2, SQLStmtName, '', nil);
end;

function TSqlitePassSQLStmtDefs.FindSQLStmt(const Value: String): TSqlitePassSqlStmtDef;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := TSqlitePassSqlStmtDef(inherited Items[I]);
    if (AnsiCompareText(Result.SQLStmtName, Value) = 0)
       or (AnsiCompareText(Result.SQLStmtFullName, Value) = 0)
       then Exit;
  end;
  Result := nil;
end;

function TSqlitePassSQLStmtDefs.GetItem(Index: Integer): TSqlitePassSqlStmtDef;
begin
Result:=TSqlitePassSqlStmtDef(Inherited Items[Index]);
end;

procedure TSqlitePassSQLStmtDefs.Refresh;
const
Sql = 'SELECT * FROM "' + SqlitePassSQLStmtDefsTable + '"';
var
i: integer;
SQLStmtsRecordset: TSqlitePassRecordset;
SQLStmtsTableName: String;

  Procedure GetSQLStmts(AliasName: String; DatabaseConnection: Pointer);
  var
  i: integer;
  SQLStmtDef: TSqlitePassSQLStmtDef;
  begin
  if AliasName <> ''
     then SQLStmtsTableName := AliasName + '.' + SqlitePassSQLStmtDefsTable
     else SQLStmtsTableName := SqlitePassSQLStmtDefsTable;

  If (FDatabase.TableDefs.FindTable(SQLStmtsTableName) = nil) then Exit;

  SQLStmtsRecordset.Open(Sql);
  for i := 0 to Pred(SQLStmtsRecordset.RecordsCount)
   do begin
      SQLStmtDef := TSqlitePassSQLStmtDef.Create(Self);
      SQLStmtDef.FSQLStmtName := SQLStmtsRecordset.ItemsTextValue[i,0];
      SQLStmtDef.SQL := SQLStmtsRecordset.ItemsTextValue[i,1];
      Self.Add(SQLStmtDef);
      end;
  SQLStmtsRecordset.Close;
  end;

begin
Self.ClearAndFreeItems;
Try
  SQLStmtsRecordset:= TSqlitePassRecordset.Create(FDatabase);
  GetSQLStmts('', FDatabase.Engine.Connection);
  { Retrieves SQLStmts info for attached databases }
  For i := 0 to Pred(FDatabase.Databases.Count) do
      begin
//      FDatabase.Databases[i].FEngine.OpenDatabase(FDatabase.Databases[i].Database);
//      GetSQLStmts(FDatabase.Databases[i].AliasName, FDatabase.Databases[i].FEngine.Connection);
//      FDatabase.Databases[i].FEngine.CloseDatabase;
      end;
finally
  SQLStmtsRecordset.Free;
end;
end;

procedure TSqlitePassSQLStmtDefs.SetItem(Index: Integer;
  const Value: TSqlitePassSqlStmtDef);
begin
  Put(Index,Value);
end;

function TSqlitePassSQLStmtDefs.SQLStmtByName(
  const Value: String): TSqlitePassSqlStmtDef;
begin
  Result := FindSQLStmt(Value);
  if Result = nil
     then DatabaseErrorFmt(Msg3950, [Value], FDatabase);
end;


