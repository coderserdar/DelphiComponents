unit VolColEditor;

interface
uses
  SysUtils, Classes,
  {$IFDEF VER140} DesignIntf, DesignEditors,
{$ELSE}{$IFDEF VER150} DesignIntf, DesignEditors,
  {$ELSE} DsgnIntf, {$ENDIF}
  {$ENDIF}
  ColnEdit, DB, Menus, Dialogs, Controls;

type
  TVolgaDBGridColumnsEditor = class(TCollectionEditor)
  private
    procedure AddAllClick(Sender: TObject);
  protected
    function CanAdd(Index: Integer): Boolean; override;
  public
    mnuAddAll: TMenuItem;
    mnuLine: TMenuItem;
    constructor Create(AOwner: TComponent); override;
  end;

{ TVolgaDBGridColumnsProperty }

  TVolgaDBGridColumnsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

{ TVolgaDBGridEditor }

  TVolgaDBGridEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TVolgaColumnDataFieldProperty }

  TVolgaColumnDataFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ TVolgaColumnLookupKeyProperty }

  TVolgaColumnLookupKeyProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation
uses VolDBGrid;

{ TVolgaDBGridColumnsProperty }

procedure TVolgaDBGridColumnsProperty.Edit;
begin
  ShowCollectionEditorClass(Designer, TVolgaDBGridColumnsEditor,
    GetComponent(0) as TComponent, TVolgaDBGridColumns(GetOrdValue), GetName);
end;

function TVolgaDBGridColumnsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TVolgaDBGridColumnsProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

{ TVolgaDBGridEditor }

procedure TVolgaDBGridEditor.ExecuteVerb(Index: Integer);
begin
  ShowCollectionEditorClass(Designer, TVolgaDBGridColumnsEditor, Component,
    TVolgaDBGrid(Component).Columns, 'Columns');
end;

function TVolgaDBGridEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Columns Editor...';
end;

function TVolgaDBGridEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TVolgaColumnDataFieldProperty }

procedure TVolgaColumnDataFieldProperty.GetValueList(List: TStrings);
var
  Grid: TVolgaCustomDBGrid;
  DataSource: TDataSource;
begin
  Grid := (GetComponent(0) as VolDBGrid.TVolgaColumn).Grid;
  if (Grid = nil) then Exit;
  DataSource := Grid.DataSource;
  if (DataSource <> nil) and (DataSource.DataSet <> nil)
  and (DataSource.DataSet.FieldCount > 0) then
    DataSource.DataSet.GetFieldNames(List);
end;

function TVolgaColumnDataFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TVolgaColumnDataFieldProperty.GetValues(Proc: TGetStrProc);
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

{ TVolgaColumnLookupKeyProperty }

procedure TVolgaColumnLookupKeyProperty.GetValueList(List: TStrings);
var
  Column: TVolgaColumn;
begin
  Column := GetComponent(0) as VolDBGrid.TVolgaColumn;
  if (Column = nil) then Exit;
  if (Column.LookupDataSet <> nil) and (Column.LookupDataSet.FieldCount>0) then
    Column.LookupDataSet.GetFieldNames(List);
end;

function TVolgaColumnLookupKeyProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TVolgaColumnLookupKeyProperty.GetValues(Proc: TGetStrProc);
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

{ TVolgaDBGridColumnsEditor }

constructor TVolgaDBGridColumnsEditor.Create(AOwner: TComponent);
begin
  inherited;
  mnuAddAll := NewItem('Add all fields', 0, false, true, AddAllClick, 0, 'mnuAddAll');
  PopupMenu1.Items.Add(mnuAddAll);
  mnuAddAll.MenuIndex := 0;
  mnuLine := NewItem('-', 0, false, true, nil, 0, 'mnuLine');
  PopupMenu1.Items.Add(mnuLine);
  mnuLine.MenuIndex := 1;
end;

function TVolgaDBGridColumnsEditor.CanAdd(Index: Integer): Boolean;
begin  //показываем только те колонки, которые были ранее изменены
  Result := TVolgaDBGridColumns(Collection).State = csCustomized;
end;

procedure TVolgaDBGridColumnsEditor.AddAllClick(Sender: TObject);
begin
  with TVolgaDBGridColumns(Collection) do
    if State = csDefault then State := csCustomized;
  UpdateListbox;
  Designer.Modified;
end;

end.
