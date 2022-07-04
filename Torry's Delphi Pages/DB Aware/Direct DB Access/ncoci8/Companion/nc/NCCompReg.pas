{*******************************************************}
{File:      NCCompReg.PAS                               }
{Revision:  0.01.00 / 06.02.2000                        }
{Comment:   NC OCI8 Companion VCL: registration unit    }
{Copyright: (c) 1997-2000, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, dmitrya@inthink.com         }
{*******************************************************}
{$I NCOciDef.inc}

unit NCCompReg;

interface

    procedure Register;

implementation

{$R NCCOMP.dcr}

Uses Classes, NCTimer, NCDBTree, NCDblLst, NCUtil,
     NCDBUtil, NCUIUtil, DB, NCOciDB, NCFldEdt, NCMemo
{$IFDEF OCI_D6}
     , DesignIntf, DesignEditors, PropertyCategories, DBReg
{$ELSE}
     , DsgnIntf
{$ENDIF}
    ;

{ --------------------------------------------------------------------------- }
{ --------------------------------------------------------------------------- }

type
    TOCIDBNameProperty = class(TStringProperty)
        function GetAttributes: TPropertyAttributes; override;
        procedure GetValues(Proc: TGetStrProc); override;
    end;

    TOCITableNameProperty = class(TStringProperty)
        function GetAttributes: TPropertyAttributes; override;
        procedure GetValues(Proc: TGetStrProc); override;
    end;

    TOCIFieldsProperty = class(TStringProperty)
        function GetAttributes: TPropertyAttributes; override;
        procedure Edit; override;
        procedure BuildQuery(ASelect, AFrom, AWhere, AOrderBy: String; q: TStrings);
        procedure BuildParams(AQuery: TOCIQuery); virtual;
        procedure GetDbTblDs(var db, tbl: String; var ds: TDataSet; q: TStrings); virtual; abstract;
    end;

function TOCIDBNameProperty.GetAttributes: TPropertyAttributes;
begin
    Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TOCIDBNameProperty.GetValues(Proc: TGetStrProc);
var
    List: TStringList;
    i: Integer;
begin
    List := TStringList.Create;
    try
        TOCIDatabase.GetObjectsList('', List, '', okDatabase, True);
        for i := 0 to List.Count - 1 do
            Proc(List[i]);
    finally
        List.Free;
    end;
end;

function TOCITableNameProperty.GetAttributes: TPropertyAttributes;
begin
    Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TOCITableNameProperty.GetValues(Proc: TGetStrProc);
var
    i: Integer;
    List: TStringList;
    db: String;
begin
    db := ReadStrProp(GetComponent(0), 'DATABASENAME');
    List := TStringList.Create;
    try
        TOCIDatabase.GetObjectsList(db, List, '', okSelectable, True);
        for i := 0 to List.Count - 1 do
            Proc(List[i]);
    finally
        List.Free;
    end;
end;

function TOCIFieldsProperty.GetAttributes: TPropertyAttributes;
begin
    Result := [paDialog];
end;

procedure TOCIFieldsProperty.BuildQuery(ASelect, AFrom, AWhere, AOrderBy: String; q: TStrings);
var
    s: String;
    wasWhere: Boolean;
begin
    s := 'select ';
    if ASelect = '' then
        s := s + '*'
    else
        s := s + ASelect;
    q.Add(s);
    q.Add('from ' + AFrom);
    wasWhere := False;
    if AWhere <> '' then
        DUContWhere(q, wasWhere, AWhere);
    if AOrderBy <> '' then
        q.Add('order by ' + AOrderBy);
end;

procedure TOCIFieldsProperty.BuildParams(AQuery: TOCIQuery);
begin
end;

procedure TOCIFieldsProperty.Edit;
var
    db, tbl: String;
    ds: TDataSet;
    fields: String;
    q: TStringList;
begin
    ds := nil;
    db := '';
    tbl := '';
    q := TStringList.Create;
    GetDbTblDs(db, tbl, ds, q);
    if (db <> '') and (tbl <> '') then
        q.Text := 'select * from ' + tbl;
    if (db <> '') and (q.Count > 0) then begin
        ds := TOCIQuery.Create(nil);
        with TOCIQuery(ds) do begin
            DatabaseName := db;
            SQL.Assign(q);
        end;
        BuildParams(TOCIQuery(ds));
    end
    else if ds = nil then
        Exit;
    try
        fields := GetStrValue;
        if EditFields(fields, ds, GetComponent(0).GetNamePath + '.' + GetName) then
            SetStrValue(fields);
    finally
        if (db <> '') and (q.Count > 0) then
            ds.Free;
    end;
end;

{ --------------------------------------------------------------------------- }
{ --------------------------------------------------------------------------- }

Type
    TNCDBTreeViewFieldsProperty = class(TOCIFieldsProperty)
        procedure BuildParams(AQuery: TOCIQuery); override;
        procedure GetDbTblDs(var db, tbl: String; var ds: TDataSet; q: TStrings); override;
    end;

procedure TNCDBTreeViewFieldsProperty.GetDbTblDs(var db, tbl: String; var ds: TDataSet; q: TStrings);
var
    tv: TNCDBTreeView;
begin
    tv := (GetComponent(0) as TNCDBTreeView);
    db := tv.DatabaseName;
    BuildQuery(tv.SQLSelect, tv.SQLFrom, tv.SQLWhere, '', q);
end;

procedure TNCDBTreeViewFieldsProperty.BuildParams(AQuery: TOCIQuery);
var
    tv: TNCDBTreeView;
begin
    tv := (GetComponent(0) as TNCDBTreeView);
    AQuery.Params.Assign(tv.SQLParams);
end;

{ --------------------------------------------------------------------------- }
{ --------------------------------------------------------------------------- }

Type
    TNCMemoComponentEditor = class(TComponentEditor)
        procedure ExecuteVerb(Index: Integer); override;
        function GetVerb(Index: Integer): string; override;
        function GetVerbCount: Integer; override;
    end;

procedure TNCMemoComponentEditor.ExecuteVerb(Index: Integer);
begin
    if TNCMemoDialog(Component).Execute then
        Designer.Modified;
end;

function TNCMemoComponentEditor.GetVerb(Index: Integer): string;
begin
    Result := '&Execute...';
end;

function TNCMemoComponentEditor.GetVerbCount: Integer;
begin
    Result := 1;
end;

{ --------------------------------------------------------------------------- }
{ --------------------------------------------------------------------------- }

procedure Register;
begin
    { TNCDBTreeView }
    RegisterPropertyEditor(TypeInfo(String), TNCDBTreeView, 'DatabaseName', TOCIDBNameProperty);
    RegisterPropertyEditor(TypeInfo(String), TNCDBTreeView, 'OwnerFields', TNCDBTreeViewFieldsProperty);
    RegisterPropertyEditor(TypeInfo(String), TNCDBTreeView, 'KeyFields', TNCDBTreeViewFieldsProperty);
    RegisterPropertyEditor(TypeInfo(String), TNCDBTreeView, 'DisplayFields', TNCDBTreeViewFieldsProperty);
    RegisterPropertyEditor(TypeInfo(String), TNCDBTreeView, 'DataFields', TNCDBTreeViewFieldsProperty);
    RegisterPropertyEditor(TypeInfo(String), TNCDBTreeView, 'SQLOrderBy', TNCDBTreeViewFieldsProperty);
    RegisterPropertyEditor(TypeInfo(String), TNCDBTreeView, 'SQLFrom', TOCITableNameProperty);
{$IFDEF OCI_D5}
    RegisterPropertiesInCategory({$IFDEF OCI_D6} sDatabaseCategoryName {$ELSE} TDatabaseCategory {$ENDIF},
        TNCDBTreeView,
        ['DataBaseName', 'DataFields', 'KeyFields', 'DisplayFields',
         'DisplayFormat', 'HierarchyType', 'KeySegments', 'OwnerFields',
         'RootName', 'RootValue', 'AfterQueryData', 'BeforeQueryData']);
{$ENDIF}

    { TNCMemoDialog }
    RegisterComponentEditor(TNCMemoDialog, TNCMemoComponentEditor);

    { Register components }
    RegisterComponents('NC Other', [TNCTimer, TNCDBTreeView, TNCDblListBox,
        TNCMemoDialog]);
end;

end.
