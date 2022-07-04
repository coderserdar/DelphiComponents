{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{                                                       }
{       Copyright (c) 1997,99 Inprise Corporation       }
{                                                       }
{*******************************************************}

unit FxReg;

interface

uses
  DesignIntf, DesignEditors, CHARTREG, CHART, DbReg;

type
  TDSSChartCompEditor = class(TChartCompEditor)
  public
    procedure Edit; override;
  end;

  procedure Register;

implementation

uses
  SysUtils, Classes, FxGrid, FxPivSrc, Fxdb, FxDConst,
  Forms, Graphics, StdCtrls, ComCtrls, FxStore, Controls,
  DBTables, FxGraph, bdeconst, FxButton, FxDimEdt, FxDCube,
  EDITCHAR, FxDimPager, FxCommon, FxHtml, FxMap, Db, TypInfo;

  { TSourceEditor }

type
  TSourceEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TSourceEditor.ExecuteVerb(Index: Integer);
var
  Source: TFxSource;
begin
  Source := Component as TFxSource;
  case Index of
    0:
    begin
      Source.SparseRows := not Source.SparseRows;
      Source.SparseCols := Source.SparseRows;
    end;
  end;
end;

function TSourceEditor.GetVerb(Index: Integer): string;
var
  Source: TFxSource;
begin
  Source := Component as TFxSource;
  case Index of
    0:
    begin
      if Source.SparseRows then
        Result := sSourceVerb0
      else
        Result := sSourceVerb1;
    end;
  end;
end;

function TSourceEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

 { TCubeEditor }

type
  TCubeEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure Edit;override;
  end;


procedure TCubeEditor.Edit;
begin
//  (Component as TFxCube).DimensionMap;
//  IProperty
end;

procedure TCubeEditor.ExecuteVerb(Index: Integer);
var
  Cube: TFxCube;
begin
  Cube := Component as TFxCube;
  case Index of
    0: ShowFDCubeEditor(Cube);
  else
  end;
end;

function TCubeEditor.GetVerb(Index: Integer): string;
begin
  Result := SCubeVerb0;
end;

function TCubeEditor.GetVerbCount: Integer;
begin
    Result := 1;
end;

  { TDSSChartCompEditor }

procedure TDSSChartCompEditor.Edit;
begin
  EditDSSChart(nil, TCustomChart(Component));
end;

  { TGridEditor }

type
  TGridEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TGridEditor.ExecuteVerb(Index: Integer);
var
  Grid: TFxGrid;
begin
  Grid := Component as TFxGrid;
  case Index of
    0: Grid.Totals := not Grid.Totals;
  end;
end;

function TGridEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := sGridVerb0;
  end;
end;

function TGridEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

  { TDecisionGridDimsProperty }

type
  TDecisionGridDimsProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

function TDecisionGridDimsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TDecisionGridDimsProperty.GetValue: String;
begin
  Result := '(' + TDisplayDims.ClassName + ')';
end;

procedure TDecisionGridDimsProperty.Edit;
begin
  ShowDisplayDimEditor(Designer, GetComponent(0) as TFxGrid);
end;

  { TDimensionMapProperty }

type
  TDimensionMapProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

function TDimensionMapProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TDimensionMapProperty.GetValue: String;
begin
  Result := 'TDimensionMaps';
end;

procedure TDimensionMapProperty.Edit;
begin
  ShowFDCubeEditor(GetComponent(0)as TFxCube);
end;

type
  TFxMapFieldProperty=class(TDbStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  TFxMapLookupFieldProperty=class(TDbStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;
  { Registration }

procedure Register;
begin
  RegisterComponents(sComponentTabName, [TFxCube,
    TFxSource, TFxPivot, TFxGrid, TFxChart,
    TDimPager,TSumCombo,TFxHtml]);

  RegisterNonActiveX([TFxCube, TFxSource,
                      TFxPivot, TFxGrid, TFxChart, TFxHtml,
                      TCustomFxGrid, TFxCustomChart], axrIncludeDescendants);

  RegisterComponentEditor(TFxCube, TCubeEditor);
  RegisterComponentEditor(TFxSource, TSourceEditor);
  RegisterComponentEditor(TFxGrid, TGridEditor);
  RegisterComponentEditor(TFxChart,TDSSChartCompEditor);

  RegisterPropertyEditor(TypeInfo(TCollection), TFxGrid, 'Dimensions', TDecisionGridDimsProperty);
//  RegisterPropertyEditor(TypeInfo(TCollection), TFxCube, 'DimensionMap', TDimensionMapProperty);
  RegisterPropertyEditor(TypeInfo(TDate), nil, '', TDateProperty);
  RegisterPropertyEditor(TypeInfo(string), TFxMapItem, 'FieldName', TFxMapFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TFxMapItem, 'KeyFields', TFxMapLookupFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TFxMapItem, 'LookupResultField', TFxMapLookupFieldProperty);
end;

{ TFxMapFieldProperty }

procedure TFxMapFieldProperty.GetValueList(List: TStrings);
var
  DataSet:TDataSet;
begin
  DataSet:=(GetComponent(0) as TFxMapItem).Collection.DataSet;
  if DataSet<>nil then
    DataSet.GetFieldNames(List);
end;

{ TFxMapLookupFieldProperty }

procedure TFxMapLookupFieldProperty.GetValueList(List: TStrings);
var
  DataSet:TDataSet;
begin
  DataSet:=(GetComponent(0) as TFxMapItem).LookupDataSet;
  if DataSet<>nil then
    DataSet.GetFieldNames(List);
end;

end.

