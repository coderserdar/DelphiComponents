{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         DB registration                               }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgDBRg;

interface
uses Classes, DsgnIntf, vgVCLRg;

type
{ TDBStringProperty }
  TDBStringProperty = class(TListNamesProperty)
  public
    procedure GetValueList(List: TStrings); virtual; abstract;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ TSessionNameProperty }
  TSessionNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

{ TDatabaseNameProperty }
  TDatabaseNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

{ TAliasNameProperty }
  TAliasNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

{ TDriverNameProperty }
  TDriverNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

{ TTableNameProperty }
  TTableNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

{ TIndexNameProperty }
  TIndexNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

{ TProcedureNameProperty }
  TProcedureNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

{ TIndexFieldNamesProperty }
  TIndexFieldNamesProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

{ TDataFieldProperty }
  TDataFieldProperty = class(TDBStringProperty)
  public
    function GetDataSourcePropName: string; virtual;
    procedure GetValueList(List: TStrings); override;
  end;

{ TFieldNameProperty }
  TFieldNameProperty = class(TDBStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); override;
  end;

{ TLookupSourceProperty }
  TLookupSourceProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

{ TLookupDestProperty }
  TLookupDestProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

{ TListFieldProperty }
  TListFieldProperty = class(TDataFieldProperty)
  public
    function GetDataSourcePropName: string; override;
  end;

{ TLookupFieldProperty }
  TLookupFieldProperty = class(TDataFieldProperty)
  public
    function GetDataSourcePropName: string; override;
  end;

{ TLookupIndexProperty }
  TLookupIndexProperty = class(TLookupFieldProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

{$IFNDEF _D4_}
  TParamsProperty = class(TPropertyEditor)
  public
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;
{$ENDIF}

procedure Register;

implementation
uses TypInfo, SysUtils, DB, Forms, DBTables, vgVCLRes, vgTools, vgDB, vgDBUtl, vgDBCtrl,
  vgDBConv, vgBDECon, vgConvEd, vgBDE, vgDBTree, vgWPDB, vgWPBDE, Explorer, ExplrDB,
  ExplrBDE, vgDBExpr
  {$IFNDEF _D4_}, QBindDlg{$ENDIF}
  ;

{$R vgDDB.dcr}

{ TDBStringProperty }
procedure TDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

{ TSessionNameProperty }
procedure TSessionNameProperty.GetValueList(List: TStrings);
begin
  Sessions.GetSessionNames(List);
end;

{ TDatabaseNameProperty }
procedure TDatabaseNameProperty.GetValueList(List: TStrings);
var
  S: string;
  P: PPropInfo;
begin
  if GetComponent(0) is TDBDataSet then
    TDBDataSet(GetComponent(0)).DBSession.GetDatabaseNames(List)
  else begin
    P := TypInfo.GetPropInfo(GetComponent(0).ClassInfo, 'SessionName');
    if Assigned(P) then
    begin
      S := GetStrProp(GetComponent(0), P);
      Sessions.FindSession(S).GetDatabaseNames(List)
    end;
  end;
end;

{ TAliasNameProperty }
procedure TAliasNameProperty.GetValueList(List: TStrings);
begin
  (GetComponent(0) as TDatabase).Session.GetAliasNames(List);
end;

{ TDriverNameProperty }
procedure TDriverNameProperty.GetValueList(List: TStrings);
begin
  (GetComponent(0) as TDatabase).Session.GetDriverNames(List);
end;

{ TTableNameProperty }
procedure TTableNameProperty.GetValueList(List: TStrings);
const
  Masks: array[TTableType] of string[5] = ('', '*.DB', '*.DBF', '*.TXT' {$IFDEF _D4_}, '*.DBF' {$ELSE} {$IFDEF CBuilder}, '*.DBF'{$ENDIF}{$ENDIF});
var
  Table: TTable;
begin
  Table := GetComponent(0) as TTable;
  Table.DBSession.GetTableNames(Table.DatabaseName, Masks[Table.TableType],
    Table.TableType = ttDefault, False, List);
end;

{ TIndexNameProperty }
procedure TIndexNameProperty.GetValueList(List: TStrings);
begin
  (GetComponent(0) as TTable).GetIndexNames(List);
end;

{ TProcedureNameProperty }
procedure TProcedureNameProperty.GetValueList(List: TStrings);
var
  DBDataSet: TDBDataSet;
begin
  DBDataSet := GetComponent(0) as TDBDataSet;
  DBDataSet.DBSession.GetStoredProcNames(DBDataSet.DatabaseName, List);
end;

{ TIndexFieldNamesProperty }
procedure TIndexFieldNamesProperty.GetValueList(List: TStrings);
var
  I: Integer;
begin
  with GetComponent(0) as TTable do
  begin
    IndexDefs.Update;
    for I := 0 to IndexDefs.Count - 1 do
      with IndexDefs[I] do
        if not (ixExpression in Options) then List.Add(Fields);
  end;
end;

{ TDataFieldProperty }
function TDataFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'DataSource';
end;

procedure TDataFieldProperty.GetValueList(List: TStrings);
var
  Instance: TComponent;
  PropInfo: PPropInfo;
  DataSource: TDataSource;
begin
  Instance := GetComponent(0) as TComponent;
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, GetDataSourcePropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
  begin
    DataSource := TObject(GetOrdProp(Instance, PropInfo)) as TDataSource;
    if (DataSource <> nil) and (DataSource.DataSet <> nil) then
      DataSource.DataSet.GetFieldNames(List);
  end;
end;

{ TFieldNameProperty }
function TFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

procedure TFieldNameProperty.GetValueList(List: TStrings);
begin
  with GetComponent(0) as TDataSet do GetFieldNames(List);
end;

{ TLookupSourceProperty }
procedure TLookupSourceProperty.GetValueList(List: TStrings);
begin
  with GetComponent(0) as TField do
    if DataSet <> nil then DataSet.GetFieldNames(List);
end;

{ TLookupDestProperty }
procedure TLookupDestProperty.GetValueList(List: TStrings);
begin
  with GetComponent(0) as TField do
    if LookupDataSet <> nil then LookupDataSet.GetFieldNames(List);
end;

{ TListFieldProperty }
function TListFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'ListSource';
end;

{ TLookupFieldProperty }
function TLookupFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'LookupSource';
end;

{ TLookupIndexProperty }
procedure TLookupIndexProperty.GetValueList(List: TStrings);
var
  Instance: TComponent;
  PropInfo: PPropInfo;
  DataSource: TDataSource;
begin
  Instance := GetComponent(0) as TComponent;
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, GetDataSourcePropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
  begin
    DataSource := TObject(GetOrdProp(Instance, PropInfo)) as TDataSource;
    if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    begin
      if (DataSource.DataSet is TTable) and
          (TTable(DataSource.DataSet).IndexFieldCount > 0) then
        List.Add(TTable(DataSource.DataSet).IndexFields[0].FieldName)
      else
        DataSource.DataSet.GetFieldNames(List);
    end;
  end;
end;

{$IFNDEF _D4_}
{ TParamsProperty }
function TParamsProperty.GetValue: string;
begin
  Result := Format('(%s)', [GetPropInfo.Name]);
end;

function TParamsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog];
end;

procedure TParamsProperty.Edit;
var
  List: TParams;
  Query: TQuery;
  Params: TParams;
  I: Integer;
begin
  Query := TQuery.Create(Application);
  try
    Params := TParams(GetOrdProp(GetComponent(0), GetPropInfo));
    Query.Params := Params;
    List := TParams.Create;
    try
      List.Assign(Params);
      if EditQueryParams(Query, List) and not List.IsEqual(Query.Params) then
      begin
        Modified;
        for I := 0 to PropCount - 1 do
        begin
          Params := TParams(GetOrdProp(GetComponent(I), TypInfo.GetPropInfo(GetComponent(I).ClassInfo, GetPropInfo.Name)));
          Params.AssignValues(List);
        end;
      end;
    finally
      List.Free;
    end;
  finally
    Query.Free;
  end;
end;
{$ENDIF}

procedure Register;
begin
{$IFNDEF _D3_}
  RegisterFields([TvgDateTimeField, TvgDateField, TvgTimeField]);
{$ELSE}
{ There is no Unregister procedure for TFieldClass and unloading this package }
{ will cause errors in desing-time packages. }
{ Using Delphi 3 or Delphi 4 you can manually edit field type in View as Text window }
  RegisterClasses([TvgDateTimeField, TvgDateField, TvgTimeField]);
{$ENDIF}

  RegisterComponents(LoadStr(SRegDataAccess), [
    TvgDatabase, TFieldSource, TOpenTables, TBDEDataSetHook, TDBConverter, TBDEConverter
  ]);

  RegisterNoIcon([TDBConvertItem, TBDEConvertItem]);
  RegisterConvertItems([TDBConvertItem, TBDEConvertItem]);

  ConvertItemClass := TBDEConvertItem;

  RegisterComponents(LoadStr(SRegDataControls), [
    TvgDBMenu, TvgQuickSearch, TDBRadioButton, TvgDBTreeView,
    TvgDBText, TvgDBListBox, TvgDBComboBox
    {$IFDEF _D4_}, TDBExpression{$ENDIF}
  ]);

  RegisterComponents(LoadStr(SRegReports), [
    TvgDBWordPrint, TvgBDEWordPrint]);

  RegisterComponentEditor(TDBConverter, TConverterEditor);
  RegisterPropertyEditor(TypeInfo(string), TComponent, 'DataFieldID', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TComponent, 'DataFieldParentID', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TComponent, 'DataFieldText', TDataFieldProperty);

  RegisterExplorerNodesClasses([
    TExplorerDBTreeRootNode,
    TExplorerSessionListNode, TExplorerSessionNode, TExplorerDatabaseNode,
    TExplorerTablesNode, TExplorerProceduresNode]);

  RegisterPropertyEditor(TypeInfo(string), TComponent, 'DatabaseName', TDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TComponent, 'SessionName', TSessionNameProperty);

{$IFNDEF _D4_}
  RegisterPropertyEditor(TypeInfo(TParams), nil, '', TParamsProperty);
{$ENDIF}
end;

initialization
  InitFieldAutoSizers([TTable, TQuery]);

finalization
  UnRegisterExplorerNodesClasses([
    TExplorerDBTreeRootNode,
    TExplorerSessionListNode, TExplorerSessionNode, TExplorerDatabaseNode,
    TExplorerTablesNode, TExplorerProceduresNode]);

  DoneFieldAutoSizers([TTable, TQuery]);

end.
