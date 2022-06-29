unit FxGraph;

interface

uses
  SysUtils, Windows, Controls, StdCtrls, Graphics, DB, Classes, Grids,
  Dialogs, FxStore, FxButton, FxConsts, FxDb, Forms, ExtCtrls, CHART,
  SERIES, TEENGINE, TEEPROCS, FxGrid, FxCommon;

resourcestring
  SBuildChart='Chart Building...';
type
  TDimEvent = procedure (Sender: TObject; iDim: Integer) of object;

  TSeriesType = (stNormal, stTemplate, st1D);

  TFxCustomChart = class;

  TFxChartDataLink = class(TDecisionDataLink)
  private
    FGraph: TFxCustomChart;
  protected
    procedure DecisionDataEvent(Event: TDecisionDataEvent); override;
  public
    constructor Create(AGraph: TFxCustomChart);
    destructor Destroy; override;
  end;

  TSeriesList=class(TList)
  private
    function GetItems(Index: Integer): TChartSeries;
    procedure SetItems(Index: Integer; const Value: TChartSeries);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure ChangeClass(AClass:TChartSeriesClass);
    property Items[Index:Integer]:TChartSeries read GetItems write SetItems;default;
  end;


  TChartType=(ctBlank,ct1D,ct2D);

  TFxCustomChart = class(TCustomChart)
  private
    FActive: Boolean;
    FActiveSeries:TChartSeries;
    FDataLink: TFxChartDataLink;
    FDerived:TSeriesList;
    FChartType:TChartType;
    FColors: array[0..15] of TColor;
    FGraphName: string;
    FOnChartChanged:TNotifyEvent;
    FFxProgress:IFxProgress;
    function MakeTemplate(Dim:Integer):TChartSeries;
    function MakeDerived:TChartSeries;
    function GetLabel(iDim: Integer; ValueIndex: Integer): string;
    function GetDecisionSource: TFxSource;
    procedure Build1DChart(Dim:Integer);
    procedure Build2DChart(RowDim,ColDim:Integer);
    procedure BuildBlankChart;
    procedure SetActive(const Value: Boolean);
    procedure SetActiveSeries(const Value:TChartSeries);
    procedure SetDecisionSource(Value: TFxSource);
    procedure SelectLayout;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ChangeTemplateClass(const AClass:TChartSeriesClass);
    procedure Updated; override;
    property Active:Boolean read FActive write SetActive;
    property ActiveSeries:TChartSeries read FActiveSeries write SetActiveSeries;
    property ChartType:TChartType read FChartType;
    property DecisionSource: TFxSource read GetDecisionSource write SetDecisionSource;
    property Derived:TSeriesList read FDerived;
    property FxProgress:IFxProgress read FFxProgress write FFxProgress;
    property OnChartChanged:TNotifyEvent read FOnChartChanged write FOnChartChanged;
  end;

  TFxChart = class(TFxCustomChart)
  public
    destructor Destroy;override;
  published
    property DecisionSource;
    property AllowPanning;
    property AllowZoom;
    property AnimatedZoom;
    property AnimatedZoomSteps;
    property BackImage;
    property BackImageInside;
    property BackImageMode;
    property BottomWall;
    property Foot;
    property Gradient;
    property LeftWall;
    property MarginBottom;
    property MarginLeft;
    property MarginRight;
    property MarginTop;
    property Title;
    property OnChartChanged;
    { TCustomChart Events }
    property OnAllowScroll;
    property OnClickAxis;
    property OnClickLegend;
    property OnClickSeries;
    property OnClickBackground;
    property OnGetLegendPos;
    property OnGetLegendRect;
    property OnScroll;
    property OnUndoZoom;
    property OnZoom;
    { TCustomAxisPanel properties }
    property AxisVisible;
    property BackColor;
    property BottomAxis;
    property Chart3DPercent;
    property ClipPoints;
    property Frame;
    property LeftAxis;
    property Legend;
    property MaxPointsPerPage;
    property Monochrome;
    property Page;
    property RightAxis;
    property ScaleLastPage;
    property SeriesList;
    property TopAxis;
    property View3D;
    property View3DOptions;
    property View3DWalls;
    { TCustomAxisPanel events }
    property OnAfterDraw;
    property OnGetAxisLabel;
    property OnGetLegendText;
    property OnGetNextAxisLabel;
    property OnPageChange;
    { TPanel properties }
    property Align;
    property Anchors;
    property AutoSize;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    { TPanel events }
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

{ TFxCustomChart }

constructor TFxCustomChart.Create(AOwner: TComponent);
var
  I:Integer;
begin
  inherited Create(AOwner);
  FxProgress:=TFxNullProgress.Create;
  FActive  := False;
  FDataLink:= TFxChartDataLink.Create(Self);
  FDerived :=TSeriesList.Create;
  FGraphName := '';
  for I:= 0 to 15 do
    FColors[I]:=GetDefaultColor(I);
  RCS;
end;

destructor TFxCustomChart.Destroy;
begin
  FreeAndNil(FDataLink);
  FreeAndNil(FDerived);
  inherited Destroy;
end;

procedure TFxCustomChart.Updated;
begin
  inherited Updated;
//  NewGraphLayout;       { Force data update in case series were manipulated. }
end;

procedure TFxCustomChart.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent is TPivotButton) and (Operation = opInsert) then
  begin
    if assigned(DecisionSource) then
      TPivotButton(AComponent).DecisionSource := DecisionSource;
  end;
end;

procedure TFxCustomChart.Build1DChart(Dim: Integer);
var
  I:Integer;
  aSeries:TChartSeries;
begin
  FChartType:=ct1D;
  ActiveSeries:=MakeTemplate(Dim);
  with DecisionSource do begin
    FGraphName:=GetDimensionName(Dim);
    aSeries:=MakeDerived;
    aSeries.ColorEachPoint:=True;
    aSeries.Clear;
    FxProgress.Text:=SBuildChart;
    FxProgress.StartProgress(GetDimensionMemberCount(Dim));
    for I:=0 to GetDimensionMemberCount(Dim)-1 do begin
      aSeries.AddY(Get2DDataAsVariant(-1,Dim,-1,I), GetLabel(Dim,I), clTeeColor);
      FxProgress.UpdateProgress;
      if FxProgress.Cancel then Break;
    end;
    aSeries.Active:=True;
    FxProgress.FinishProgress;
    { title the x-axis graph with ColDim }
    BottomAxis.Title.Caption := GetDimensionName(Dim);
    TopAxis   .Title.Caption := GetDimensionName(Dim);
  end;
end;

procedure TFxCustomChart.Build2DChart(RowDim, ColDim: Integer);
var
  I,J,nSeries:Integer;
  Labs:array of string;
begin
  FChartType:=ct2D;
  ActiveSeries:=MakeTemplate(RowDim);
  with DecisionSource do begin
    FGraphName:=GetDimensionName(ColDim)+ '+' + GetDimensionName(RowDim);
    nSeries:=GetDimensionMemberCount(RowDim);
    SetLength(Labs,GetDimensionMemberCount(ColDim));
    for J:=0 to High(Labs)do
      Labs[J]:=GetLabel(ColDim,J);
    if SeriesList.Capacity<nSeries then
      SeriesList.Capacity:=nSeries+32;
    FxProgress.Text:=SBuildChart;
    FxProgress.StartProgress(nSeries);
    for I:=0 to nSeries-1 do begin
      with MakeDerived do begin
        Title:=GetMemberAsString(RowDim,I);
        SeriesColor:=FColors[I mod 16];
        for J:=0 to GetDimensionMemberCount(ColDim)-1 do
          AddY(Get2DDataAsVariant(RowDim,ColDim,I,J),Labs[J],clTeeColor);
      end;
      FxProgress.UpdateProgress;
      if FxProgress.Cancel then Break;
    end;
    for I:=0 to Derived.Count-1 do
      Derived[I].Active:=True;
    FxProgress.FinishProgress;
    { title the x-axis graph with ColDim }
    BottomAxis.Title.Caption := DecisionSource.GetDimensionName(ColDim);
    TopAxis   .Title.Caption := DecisionSource.GetDimensionName(ColDim);
  end;
end;

procedure TFxCustomChart.BuildBlankChart;
var
  I:Integer;
begin
  FDerived.Clear;
  ActiveSeries:=nil;
  FChartType:=ctBlank;
  FGraphName := '';
  for I:=0 to SeriesCount-1 do
    Series[i].Active := False;
  BottomAxis.Title.Caption := '';
  TopAxis   .Title.Caption := '';
  LeftAxis  .Title.Caption := '';
  RightAxis .Title.Caption := '';
end;

function TFxCustomChart.MakeDerived: TChartSeries;
begin
  Result:=CreateNewSeries
    (Parent,Self,TChartSeriesClass(ActiveSeries.ClassType),nil);
  Result.Assign(ActiveSeries);
  Result.Identifier:='';
  Result.Style:=[tssHideDataSource];
  Derived.Add(Result);
end;

function TFxCustomChart.MakeTemplate(Dim:Integer): TChartSeries;
var
  I:Integer;
  Ident:string;
begin
  Ident:=DecisionSource.GetDimensionName(Dim);
  case ChartType of
  ct1D: Ident:='1D::'+Ident;
  ct2D: Ident:='2D::'+Ident;
  end;
  for I:=0 to SeriesCount-1 do begin
    if Series[I].Identifier=Ident then begin
      Result:=Series[I];
      Exit;
    end;
  end;
  Result:=CreateNewSeries(Parent,Self,TBarSeries,nil);
  Result.Name:='';
  Result.Active:=False;
  Result.Identifier:=Ident;
  Result.Marks.Visible := False;
  Result.Title:=Ident;
  Result.Style:=[tssHideDataSource, tssDenyDelete, tssIsTemplate];
  TBarSeries(Result).MultiBar:=mbNone;
end;

function TFxCustomChart.GetDecisionSource: TFxSource;
begin
  Result := TFxSource(FDataLink.DecisionSource);
end;

function TFxCustomChart.GetLabel(iDim:Integer; ValueIndex:Integer):string;
begin
  Result:= '';
  if Assigned(DecisionSource) then
    Result:=DecisionSource.GetMemberAsString(IDim, ValueIndex);
end;

procedure TFxCustomChart.SelectLayout;
begin
  Derived.Clear;
  with DecisionSource,Pivot do begin
    if (ActiveRowCount=0)and(ActiveColCount=0)then
      BuildBlankChart
    else if ActiveRowCount=0 then
      Build1DChart(IndexInfo(dgCol,0,True).Dim)
    else if ActiveColCount=0 then
      Build1DChart(IndexInfo(dgRow,0,True).Dim)
    else
      Build2DChart(IndexInfo(dgRow,0,True).Dim,IndexInfo(dgCol,0,True).Dim);
    // title the y-axis with the summary name
    if Active and (nSums>0) then begin
      LeftAxis .Title.Caption:= GetSummaryName(CurrentSum);
      RightAxis.Title.Caption:= GetSummaryName(CurrentSum);
    end else  begin
      LeftAxis .Title.Caption:= '';
      RightAxis.Title.Caption:= '';
    end;
  end;
end;

procedure TFxCustomChart.SetActive(const Value: Boolean);
begin
  if Value<>FActive then begin
    FActive:= Value;
    if FActive then
      SelectLayout
    else
      BuildBlankChart;
  end;
end;

procedure TFxCustomChart.SetActiveSeries(const Value: TChartSeries);
begin
  if Value<>FActiveSeries then begin
    FActiveSeries:=Value;
    if Assigned(FOnChartChanged)then
      OnChartChanged(Self);
  end;
end;

procedure TFxCustomChart.SetDecisionSource(Value: TFxSource);
begin
  FDataLink.DecisionSource := Value;
end;

procedure TFxCustomChart.ChangeTemplateClass(const AClass:TChartSeriesClass);
var
  ASeries:TChartSeries;
begin
  ASeries:=ActiveSeries;
  ChangeSeriesType(ASeries,AClass);
  Derived.ChangeClass(AClass);
  ActiveSeries:=ASeries;
end;

  { Datalink Methods }

procedure TFxChartDataLink.DecisionDataEvent(Event: TDecisionDataEvent);
begin
  if FBlocked then Exit;
  FBlocked:=True;
  try
    case Event of
      xeSummaryChanged : FGraph.SelectLayout;
      xePivot          : FGraph.SelectLayout;
      xeNewMetaData,
      xeStateChanged,
      xeSourceChange   : FGraph.Active:=(DecisionSource<>nil) and DecisionSource.Ready;
    end;
  finally
    FBlocked:= False;
  end;
end;

constructor TFxChartDataLink.Create(AGraph: TFxCustomChart);
begin
  FGraph := AGraph;
end;

destructor TFxChartDataLink.Destroy;
begin
  inherited Destroy;
end;

{ TSeriesList }

procedure TSeriesList.ChangeClass(AClass: TChartSeriesClass);
var
  I:Integer;
  Item:TChartSeries;
begin
  for I:=0 to Count-1 do begin
    Item:=List[I];
    ChangeSeriesType(Item,AClass);
    List[I]:=Item;
  end;
end;

function TSeriesList.GetItems(Index: Integer): TChartSeries;
begin
  Result:=inherited Items[Index];
end;

procedure TSeriesList.Notify(Ptr: Pointer; Action: TListNotification);
var
  Item:TChartSeries;
begin
  Item:=Ptr;
  if Action=lnDeleted then begin
    Item.ParentChart:=nil;
    Item.Free;
  end;
end;

procedure TSeriesList.SetItems(Index: Integer; const Value: TChartSeries);
begin
  inherited Items[Index]:=Value;
end;

{ TFxChart }

destructor TFxChart.Destroy;
begin
  Derived.Clear;
  //WriteComponentResFile('chart.dat',Self);
  inherited;
end;

initialization
  StartClassGroup(TControl);
  GroupDescendentsWith(TFxChart, TControl);
  RegisterClass(TFxChart);
end.


