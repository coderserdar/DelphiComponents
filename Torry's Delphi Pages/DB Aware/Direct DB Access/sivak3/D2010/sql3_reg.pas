unit sql3_reg;

interface

uses
  Windows, SysUtils, Classes, Controls, DesignEditors, DesignIntf,
  DB, Dialogs, sql3_defs, sql3_utils;

type

  TSimpleTableEditor = class(TComponentEditor)
  private
    table: TSivak3SimpleTable;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TCreateNewDatabase = class(TComponentEditor)
  private
    db: TSivak3Database;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TSivakStringProp = class(TStringProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure GetValueList(List: TStrings); virtual;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TTableNameProperty = class(TSivakStringProp)
  private
    procedure GetAsSimpleTable(table: TSivak3SimpleTable; List: TStrings);
    procedure GetAsTable(table: TSivak3Table; List: TStrings);
  public
    procedure GetValueList(List: TStrings); override;
  end;

  TFieldNameProperty = class(TSivakStringProp)
  private
    procedure GetAsSimpleTable(table: TSivak3SimpleTable; List: TStrings);
    procedure GetAsTable(table: TSivak3Table; List: TStrings);
  public
    procedure GetValueList(List: TStrings); override;
  end;

  TIndexNameProperty = class(TSivakStringProp)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  TFieldListProperty = class(TStringProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TPragmaProperty = class(TStringProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TMasterFieldsProperty = class(TStringProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure Register;

implementation

uses
  pe_simpletable_collist, ce_simple_data, Forms, ce_db_params,
  ce_db_create, pe_masterlinks;

procedure Register;
begin
  RegisterComponents('Sivak3', [TSivak3Database, TSivak3SimpleTable, TSivak3Query, TSivak3Table, TSivak3Exec]);
  //RegisterComponents('Sivak3', [TSivak3SimpleTable]);
  //RegisterComponents('Sivak3', [TSivak3Query]);
  //RegisterComponents('Sivak3', [TSivak3Table]);
  //RegisterComponents('Sivak3', [TSivak3Exec]);

  RegisterComponentEditor(TSivak3Database, TCreateNewDatabase);
  RegisterComponentEditor(TSivak3SimpleTable, TSimpleTableEditor);

  RegisterPropertyEditor(TypeInfo(String), TSivak3Database, 'Params', TPragmaProperty);
  RegisterPropertyEditor(TypeInfo(String), TSivak3SimpleTable, 'TableName', TTableNameProperty);
  RegisterPropertyEditor(TypeInfo(String), TSivak3SimpleTable, 'OrderField', TFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(String), TSivak3SimpleTable, 'ColumnList', TFieldListProperty);
  RegisterPropertyEditor(TypeInfo(String), TSivak3Table, 'TableName', TTableNameProperty);
  RegisterPropertyEditor(TypeInfo(String), TSivak3Table, 'IndexFieldName', TFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(String), TSivak3Table, 'IndexName', TIndexNameProperty);
  RegisterPropertyEditor(TypeInfo(String), TSivak3Table, 'MasterFields', TMasterFieldsProperty);
end;

{ TSivakStringProp }

procedure TSivakStringProp.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do
    Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

procedure TSivakStringProp.GetValueList(List: TStrings);
begin
end;

function TSivakStringProp.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

{ TTableNameProperty }

procedure TTableNameProperty.GetAsSimpleTable(table: TSivak3SimpleTable; List: TStrings);
var
  b: Boolean;
begin
  b := false;
  if Assigned(Table.Database) then
  try
    b := Table.Database.Connected;
    try
      if not b then
      Table.Database.Open;
      Table.Database.GetTableNames(List);
      Table.Database.GetViewNames(List);
    except
    end;
  finally
    if not b and Table.Database.Connected then
    Table.Database.Close;
  end;
end;

procedure TTableNameProperty.GetAsTable(table: TSivak3Table; List: TStrings);
var
  b: Boolean;
begin
  b := false;
  if Assigned(Table.Database) then
  try
    b := Table.Database.Connected;
    try
      if not b then
      Table.Database.Open;
      Table.Database.GetTableNames(List);
      Table.Database.GetViewNames(List);
    except
    end;
  finally
    if not b and Table.Database.Connected then
    Table.Database.Close;
  end;
end;

procedure TTableNameProperty.GetValueList(List: TStrings);
var
  Table: TPersistent;
begin
  List.Clear;
  Table := GetComponent(0);
  if Assigned(Table) then
  if Table is TSivak3SimpleTable then
  GetAsSimpleTable(Table as TSivak3SimpleTable, List)
  else GetAsTable(table as TSivak3Table, List);
end;

{ TFieldNameProperty }

procedure TFieldNameProperty.GetAsSimpleTable(table: TSivak3SimpleTable; List: TStrings);
var
  b: Boolean;
begin
  b := false;
  if Assigned(Table.Database) then
  try
    b := Table.Database.Connected;
    try
      if not b then
      Table.Database.Open;
      Table.Database.GetFieldNames(List, Table.TableName);
    except
    end;
  finally
    if not b and Table.Database.Connected then
    Table.Database.Close;
  end;
end;

procedure TFieldNameProperty.GetAsTable(table: TSivak3Table; List: TStrings);
var
  b: Boolean;
begin
  b := false;
  if Assigned(Table.Database) then
  try
    b := Table.Database.Connected;
    try
      if not b then
      Table.Database.Open;
      Table.Database.GetFieldNames(List, Table.TableName);
    except
    end;
  finally
    if not b and Table.Database.Connected then
    Table.Database.Close;
  end;
end;

procedure TFieldNameProperty.GetValueList(List: TStrings);
var
 Table: TPersistent;
begin
  List.Clear;
  Table := GetComponent(0);
  if Assigned(Table) then
  if Table is TSivak3SimpleTable then
  GetAsSimpleTable(Table as TSivak3SimpleTable, List)
  else GetAsTable(Table as TSivak3Table, List);
end;

{ TFieldListProperty }

function TFieldListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TFieldListProperty.GetValues(Proc: TGetStrProc);
var
  Table: TSivak3SimpleTable;
  b: Boolean;
  s: String;
begin
  b := false;
  Table := GetComponent(0) as TSivak3SimpleTable;
  if Assigned(Table) then
  if Assigned(Table.Database) then
  try
    b := Table.Database.Connected;
    try
      if not b then
      Table.Database.Open;
      with Tfpe_st_collist.Create(Application) do
      try
        if show_editor(Table, s) then
        Proc(s);
      finally
        Free;
      end;
    except
    end;
  finally
    if not b and Table.Database.Connected then
    Table.Database.Close;
  end;
end;

{ TIndexNameProperty }

procedure TIndexNameProperty.GetValueList(List: TStrings);
var
  Table: TSivak3Table;
  b: Boolean;
begin
  b := false;
  List.Clear;
  Table := GetComponent(0) as TSivak3Table;
  if Assigned(Table) then
  if Assigned(Table.Database) then
  if not str_empty(Table.TableName) then
  try
    b := Table.Database.Connected;
    try
      if not b then
      Table.Database.Open;
      Table.Database.GetIndexDefs(Table.IndexDefs, Table.TableName);
      Table.IndexDefs.GetItemNames(List);
    except
      raise;
    end;
  finally
    if not b and Table.Database.Connected then
    Table.Database.Close;
  end;
end;

{ TPragmaProperty }

function TPragmaProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TPragmaProperty.GetValues(Proc: TGetStrProc);
var
  db: TSivak3Database;
  s: String;
begin
  db := GetComponent(0) as TSivak3Database;
  if Assigned(db) then
  with Tfce_db_params.Create(Application) do
  try
    if show_params(db.Params, s) = mrOk then
    Proc(s);
  finally
    Free;
  end;
end;

{ TMasterFieldsProperty }

function TMasterFieldsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TMasterFieldsProperty.GetValues(Proc: TGetStrProc);
var
  table: TSivak3Table;
  d, m: TStringList;
  b, b2: Boolean;
  s: String;
begin
  table := GetComponent(0) as TSivak3Table;
  if not Assigned(table) then Exit;

  if str_empty(table.TableName) then
  DatabaseError(e_table_name_missing);

  if not Assigned(table.Database) then
  DatabaseError(e_database_not_assigned);

  if not Assigned(table.MasterSource) then
  DatabaseError(e_mastersource_missing);

  if not Assigned(table.MasterSource.DataSet) then
  DatabaseError(e_mastersource_error);

  b := table.Database.Connected;
  b2:= table.MasterSource.DataSet.Active;

  with Tfpe_masterlinks.Create(Application) do
  try
    s := table.MasterFields;
    d := TStringList.Create;
    m := TStringList.Create;
    try
      table.Database.GetFieldNames(d, table.TableName);
      if not table.MasterSource.DataSet.Active then
      table.MasterSource.DataSet.Open;
      table.MasterSource.DataSet.GetFieldNames(m);
      if not b2 then
      table.MasterSource.DataSet.Close;
      ValueList.TitleCaptions.Clear;
      ValueList.TitleCaptions.Add('Detail fields (' + table.TableName + '):');
      ValueList.TitleCaptions.Add('Master fields (' + table.MasterSource.DataSet.Name + '):');
      if show_fields(d, m, s) = mrOk then
      Proc(s);
    finally
      m.Free;
      d.Free;
      if not b then
      table.Database.Close;
    end;
  finally
    Free;
  end;
end;

{ TSimpleTableEditor }

procedure TSimpleTableEditor.ExecuteVerb(Index: Integer);
begin
  table := TSivak3SimpleTable(Component);
  with Tfce_simple_data.Create(Application) do
  try
    set_table(table);
    ShowModal;
  finally
    Free;
  end;
end;

function TSimpleTableEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Show data ...';
end;

function TSimpleTableEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TCreateNewDatabase }

procedure TCreateNewDatabase.ExecuteVerb(Index: Integer);
var
  d: TSivak3Database;
  e: TDatabaseEncoding;
begin
  db := TSivak3Database(Component);
  if FileExists(db.Driver) then
  with Tfce_db_create.Create(Application) do
  try
    if ShowModal = mrOK then
    begin
      case encoding.ItemIndex of
      1:   e := deUTF_16le;
      2:   e := deUTF_16be;
      else e := deUTF_8;
      end;
      if db.Connected then
      db.Close;
      d := TSivak3Database.Create(Application);
      try
        d.Driver := db.Driver;
        d.MustExists := false;
        d.CreateDatabase(e, not ChecOver.Checked, epath.Text, edesc.Text);
        d.Close;
        db.DatabaseFile := epath.Text;
        db.Description := edesc.Text;
      finally
        d.Free;
      end;
    end;
  finally
    Free;
  end
  else MessageBox(GetForegroundWindow, 'Cannot load dll driver!', 'Error', MB_OK or MB_ICONERROR)
end;

function TCreateNewDatabase.GetVerb(Index: Integer): string;
begin
  Result := 'Create new database';
end;

function TCreateNewDatabase.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
