unit ColMSEditor;

interface
uses
  SysUtils, Classes,
  {$IFDEF VER140} DesignIntf, DesignEditors,
{$ELSE}{$IFDEF VER150} DesignIntf, DesignEditors,
  {$ELSE} DsgnIntf, {$ENDIF}
  {$ENDIF}
  ColnEdit, DB, Menus, Dialogs, Controls;

type
  TESDBGridColumnsEditor = class(TCollectionEditor)
  private
    procedure AddAllClick(Sender: TObject);
  protected
    function CanAdd(Index: Integer): Boolean; override;
  public
    mnuAddAll: TMenuItem;
    mnuLine: TMenuItem;
    constructor Create(AOwner: TComponent); override;
  end;

{ TESDBGridColumnsProperty }

  TESDBGridColumnsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

{ TESDBGridEditor }

  TESDBGridEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TColumnMSDataFieldProperty }

  TColumnMSDataFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation
uses ESDBGrids;

{ TESDBGridColumnsProperty }

procedure TESDBGridColumnsProperty.Edit;
begin
  ShowCollectionEditorClass(Designer, TESDBGridColumnsEditor,
    GetComponent(0) as TComponent, TDBGridColumns(GetOrdValue), GetName);
end;

function TESDBGridColumnsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TESDBGridColumnsProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

{ TESDBGridEditor }

procedure TESDBGridEditor.ExecuteVerb(Index: Integer);
begin
  ShowCollectionEditorClass(Designer, TESDBGridColumnsEditor, Component,
    TESDBGrid(Component).Columns, 'Columns');
end;

function TESDBGridEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Columns Editor...';
end;

function TESDBGridEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TColumnMSDataFieldProperty }

procedure TColumnMSDataFieldProperty.GetValueList(List: TStrings);
var
  Grid: TCustomESDBGrid;
  DataSource: TDataSource;
begin
  Grid := (GetComponent(0) as ESDBGrids.TColumn).Grid;
  if (Grid = nil) then Exit;
  DataSource := Grid.DataSource;
  if (DataSource <> nil) and (DataSource.DataSet <> nil)
  and (DataSource.DataSet.FieldCount > 0) then
    DataSource.DataSet.GetFieldNames(List);
end;

function TColumnMSDataFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TColumnMSDataFieldProperty.GetValues(Proc: TGetStrProc);
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

{ TESDBGridColumnsEditor }

constructor TESDBGridColumnsEditor.Create(AOwner: TComponent);
begin
  inherited;
  mnuAddAll := NewItem('Add all fields', 0, false, true, AddAllClick, 0, 'mnuAddAll');
  PopupMenu1.Items.Add(mnuAddAll);
  mnuAddAll.MenuIndex := 0;
  mnuLine := NewItem('-', 0, false, true, nil, 0, 'mnuLine');
  PopupMenu1.Items.Add(mnuLine);
  mnuLine.MenuIndex := 1;
end;

function TESDBGridColumnsEditor.CanAdd(Index: Integer): Boolean;
begin  //показываем только те колонки, которые были ранее изменены
  Result := TDBGridColumns(Collection).State = csCustomized;
end;

procedure TESDBGridColumnsEditor.AddAllClick(Sender: TObject);
begin
  with TDBGridColumns(Collection) do
    if State = csDefault then State := csCustomized;
  UpdateListbox;
  Designer.Modified;
end;

end.
