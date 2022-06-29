unit FxStore;

interface

uses
  Windows, Variants, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, DBCommon, FxArrays, Menus, FxCache, FxMap,
  FxPBar, FxCommon, SyncObjs;

const
  SubTotal  = -1;
  NonSparseAgg = -4;
  SparseUnknown = -3;
  SparseAgg = -2;
  SparseSum = -1;
  MaxBinDimensions = 16;
  LargeValueCount = 50;

type
  TFxCustomStore = class;
  TErrorAction = (eaFail, eaContinue);

  TCapacityErrorEvent = procedure(var EAction: TErrorAction) of object;
  TCubeSQLEvent=procedure (var Value:string) of object;
  TCubeNotifyEvent  = procedure(DataCube: TFxCustomStore) of object;
  TCubeRefreshEvent = procedure(DataCube: TFxCustomStore; DimMap: TFxMap) of object;

  { Designtime state }
  TCubeDataState = (dsNoData, dsMetaData, dsDimensionData, dsAllData);

  TBuildType = (btHardRebuild, btSoftRebuild, btNoRebuild);

  { Public cube state }
  TCubeState = (dcInactive, dcBrowseMetaData, dcBrowseMemberData, dcBrowseAllData);

  // This is the multi-dimensional data store component
  TFxCustomStore = class(TCommonDataStore)
  private
    FBinData: Boolean;
    FCache: TDataCache;
    FDataSet:TDataSet;
    FDesignState: TCubeDataState;
    FDimensionMap: TFxMap;
    FDirty: Boolean;
    FState: TCubeState;
    FStreamedActive:Boolean;
    FShowProgress: Boolean;
    FOnCapacityError: TCapacityErrorEvent;
    FOnGetSql:TCubeSQLEvent;
    FAfterOpen: TCubeNotifyEvent;
    FAfterClose: TCubeNotifyEvent;
    FBeforeClose: TCubeNotifyEvent;
    FBeforeOpen: TCubeNotifyEvent;
    FOnRefresh: TCubeRefreshEvent;
    function GetActive: Boolean;
    function GetCapacity: Integer;
    function GetCubeState:TCubeState;
    procedure CheckDimensionMap(DimMap: TFxMap);
    procedure InternalOpenCache;
    procedure SetActive(Value: Boolean);
    procedure SetCapacity(Value: Integer);
    procedure SetDataSet(ADataSet: TDataSet);
    procedure SetDesignState(Value: TCubeDataState);
    procedure SetState(Value: TCubeState);
    function GetDimensions: TDimensions;
    function GetSummaries: TSummaries;
  protected
    function BinMapHasBinData: Boolean;
    function GetCurrentSummary: Integer;
    function GetDataSet:TDataSet;override;
    function GetDomain(const DimensionIDs:array of Integer;
      Vector:TVector; ATotals: Boolean; var Domain: TTwoDimArray): Integer;
    procedure DoActive;virtual;
    procedure DoBeforeOpen; virtual;
    procedure DoAfterOpen; virtual;
    procedure DoBeforeClose; virtual;
    procedure DoAfterClose; virtual;
    procedure DoOnRefresh(DimMap: TFxMap); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifyDataSources(Event: TDecisionDataEvent);virtual;abstract;
    procedure LayoutChanged; virtual;
    procedure Loaded;override;
    procedure SetCurrentSummary(Value: Integer);
    procedure StateChanged; virtual;
    property Active: Boolean read GetActive write SetActive default False;
    property State: TCubeState read FState write SetState; { Returns the cube state }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetMemoryUsage: Integer;		                                { Gets the total memory consumed by the DecisionCube}
    function GetMemberAsString(Dimension, Index: Integer): String; virtual;  { Returns the value of the member at index as a string }
    function GetMemberAsVariant(Dimension, Index: Integer): Variant;{ Returns the value of the member at index as a variant }
    function GetSummaryAsString(const Coord:TVector): String;	{ Gets the summary value as a string }
    function GetSummaryAsVariant(const Coord:TVector): Variant; virtual;	{ Gets the summary value as a variant }
    procedure Refresh(Rebuild:Boolean);overload;override;
    procedure Refresh(DimMap: TFxMap; bForce: Boolean);reintroduce;overload;deprecated;
    property Capacity: Integer read GetCapacity write SetCapacity; { Sets the Internal capacity limit for the cache }
    property CurrentSummary: Integer read GetCurrentSummary write SetCurrentSummary;  { Returns the active summary }
    property DataCache: TDataCache read FCache;
    property DataSet: TDataSet read GetDataSet write SetDataSet;		              { Reads and sets the data set }
    property DesignState: TCubeDataState read FDesignState write SetDesignState default dsAllData;
    property DimensionMap: TFxMap read FDimensionMap write FDimensionMap;     { Reads or sets TFxMap }
    property Dimensions:TDimensions read GetDimensions;
    property Summaries :TSummaries read GetSummaries;
    property ShowProgressDialog: Boolean read FShowProgress write FShowProgress;
    property OnLowCapacity: TCapacityErrorEvent read FOnCapacityError write FOnCapacityError;
    property OnGetSQL: TCubeSQLEvent read FOnGetSQL write FOnGetSQL;
    property BeforeOpen: TCubeNotifyEvent read FBeforeOpen write FBeforeOpen;
    property AfterOpen: TCubeNotifyEvent read FAfterOpen write FAfterOpen;
    property BeforeClose: TCubeNotifyEvent read FBeforeClose write FBeforeClose;
    property AfterClose: TCubeNotifyEvent read FAfterClose write FAfterClose;
    property OnRefresh: TCubeRefreshEvent read FOnRefresh write FOnRefresh;
  end;

  ECacheError = class(Exception);
  ECacheWarn  = class(Exception);

var
  DomainTicker,SparseTicker,ScopeTicker,AggTicker:Integer;

implementation

uses
  Math, DateUtils, DBConsts, FxBin;

resourcestring
  SDataSetNotAssigned   = 'DataSet not assigned*';

function GetDisplayFormat(fld: TField): string;
begin
  case fld.DataType of
    ftCurrency,
    ftFloat,
    ftBCD,
    ftInteger : Result := TNumericField(fld).DisplayFormat;
    ftDate,
    ftTime,
    ftDateTime: Result := TDateTimeField(fld).DisplayFormat;
    else
      Result := '';
  end;
end;

function GetPrecision(fld: TField): Integer;
begin
  case fld.DataType of
    ftCurrency,
    ftFloat: Result := TFloatField(fld).Precision;
    else
      Result := 0;
  end;
end;

  { TFxCustomStore }

constructor TFxCustomStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCache := TDataCache.Create(Self);
  FDesignState := dsAllData;
  FShowProgress:= True;
  FBinData := False;
  FDirty := False;
  FDimensionMap:= TFxMap.Create(Self, TFxMapItem);
end;

destructor TFxCustomStore.Destroy;
begin
  FreeAndNil(FCache);
  FreeAndNil(FDimensionMap);
  inherited Destroy;
end;

procedure TFxCustomStore.SetDesignState(Value: TCubeDataState);
begin
  if FDesignState <> Value then
  begin
    FDesignState := Value;
    FDirty := True;
  end;
end;

function TFxCustomStore.GetCubeState:TCubeState;
begin
  if True or (csDesigning in ComponentState) then begin
    case FDesignState of
      dsNoData: Result:=dcInactive;
      dsMetaData: Result:=dcBrowseMetaData;
      dsDimensionData: Result:=dcBrowseMemberData;
    else
      Result:=dcBrowseAllData;
    end;
  end else
    Result:=dcBrowseAllData;
end;

procedure TFxCustomStore.SetState(Value: TCubeState);
begin
  if Value<>FState then begin
    FState := Value;
    StateChanged;
  end;
end;

function TFxCustomStore.GetActive: Boolean;
begin
  Result := State <> dcInactive;
end;

  { Sets the cache active or inactive. }

procedure TFxCustomStore.SetActive(Value: Boolean);
begin
  if csReading in ComponentState then begin
    FStreamedActive:=Value;
    Exit;
  end;
  if Active<>Value then begin
    if Value then begin
      DoBeforeOpen;
      DoActive;
      DoAfterOpen;
    end else begin
      State:=dcInactive;
      if not (csDestroying in ComponentState) then DoBeforeClose;
      DataCache.Active:=False;
      DimensionMap.Active:=False;
      if not (csDestroying in ComponentState) then DoAfterClose;
    end;
  end;
end;

procedure TFxCustomStore.SetDataSet(ADataSet: TDataSet);
begin
  if ADataSet<>FDataSet then begin
    if FDataSet<>nil then FDataSet.RemoveFreeNotification(Self);
    FDataSet:= ADataSet;
    if ADataSet<>nil then ADataSet.FreeNotification(Self);
  end;
end;

procedure TFxCustomStore.Notification(AComponent: TComponent; Operation: TOperation);
var
  I:Integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent is TDataSet) then begin
    if AComponent=FDataSet then begin
      Active:=False;
      FDataSet := nil;
    end;
    for I:=0 to DimensionMap.Count-1 do begin
      if DimensionMap[I].LookupDataSet=AComponent then
        DimensionMap[I].LookupDataSet:=nil;
    end;
  end;
end;

procedure TFxCustomStore.LayoutChanged;
begin
end;

procedure TFxCustomStore.StateChanged;
begin
end;

function TFxCustomStore.GetMemberAsString(Dimension, Index: Integer): String;
begin
  Result := FCache.Dimensions[Dimension].AsString(Index);
end;

function TFxCustomStore.GetMemberAsVariant(Dimension, Index: Integer): Variant;
begin
  Result := FCache.Dimensions[Dimension][Index];
end;

function TFxCustomStore.GetDomain(const DimensionIDs:array of Integer;
  Vector:TVector; ATotals: Boolean; var Domain: TTwoDimArray): Integer;
begin
  Result:= FCache.GetDomain(DimensionIDs, Vector, ATotals, Domain);
end;

function TFxCustomStore.GetSummaryAsString(const Coord:TVector): String;
begin
  Result := FCache.GetSummaryAsString(Coord);
end;

function TFxCustomStore.GetSummaryAsVariant(const Coord:TVector): Variant;
begin
  Result := FCache.GetSummaryAsVariant(Coord);
end;

function TFxCustomStore.GetCurrentSummary: Integer;
begin
  Result := FCache.ActiveSummary;
end;

procedure TFxCustomStore.SetCurrentSummary(Value: Integer);
begin
  if FCache.ActiveSummary<>Value then begin
    FCache.ActiveSummary := Value;
    NotifyDataSources(xeSummaryChanged);
  end;
end;

function TFxCustomStore.GetMemoryUsage: Integer;
begin
  Result := 0;
  if Assigned(FCache) then Result := FCache.GetMemoryUsage;
end;

function TFxCustomStore.BinMapHasBinData: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FDimensionMap.Count-1 do begin
    if (FDimensionMap[I].BinType<>binNone) or (FDimensionMap[I].active = False) then
    begin
      Result := True;
      break;
    end;
  end;
end;

procedure TFxCustomStore.SetCapacity(Value: Integer);
begin
  SetMemoryCapacity(Value);
end;

function TFxCustomStore.GetCapacity: Integer;
begin
  Result := GetMemoryCapacity;
end;

procedure TFxCustomStore.DoBeforeOpen;
begin
  if Assigned(FBeforeOpen) then
    FBeforeOpen(Self);
end;

procedure TFxCustomStore.DoAfterOpen;
begin
  if Assigned(FAfterOpen) then
    FAfterOpen(Self);
end;

procedure TFxCustomStore.DoBeforeClose;
begin
  if Assigned(FBeforeClose) then
    FBeforeClose(Self);
end;

procedure TFxCustomStore.DoAfterClose;
begin
  if Assigned(FAfterClose) then
    FAfterClose(Self);
end;

procedure TFxCustomStore.DoOnRefresh(DimMap: TFxMap);
begin
  if Assigned(FOnRefresh) then
    FOnRefresh(Self, DimMap);
end;

procedure TFxCustomStore.CheckDimensionMap(DimMap: TFxMap);
var
  OldMap: TFxMap;
  I: Integer;
begin
  if FDirty then begin
    FDirty:= False;
    Active:=False;
    Exit;
  end;
  OldMap:= DimensionMap;
  Active:=Active and (OldMap.Count<>0);
  Active:=Active and (OldMap.Count=DimMap.Count);
  for I:=0 to DimMap.Count-1 do begin
    Active:= Active and (OldMap[I].DimensionType=DimMap[I].DimensionType);
    Active:= Active and (OldMap[I].FieldName=DimMap[I].FieldName);
    Active:= Active and (OldMap[I].Active=DimMap[I].Active);
    Active:= Active and (OldMap[I].StartDate=DimMap[I].StartDate);
    Active:= Active and (OldMap[I].BinType=DimMap[I].BinType);
    if not Active then Break;
  end;
end;

procedure TFxCustomStore.Refresh(Rebuild:Boolean);
begin
  if not Active then Exit;
  if not Rebuild then
    NotifyDataSources(xeNewMetaData)
  else begin
    Active:=False;
    Active:=True;
  end;
end;

{ TODO -cФункциональность :
Связь DimentionItem.DimLink обрабатывается неясно и есть ситуации
где это делается неверно }
procedure TFxCustomStore.Refresh(DimMap: TFxMap; bForce: Boolean);	//pg
var
  i:integer;
  D:array of TCommonDimItem;
begin
  if DimMap=nil then begin
    Active:=False;
    Active:=True;
    Exit;
  end;
  DoOnRefresh(DimMap);
  // Set up the load states for the dimensions
  Active:=Active and not bForce;
  CheckDimensionMap(DimMap);
  SetLength(D,DimensionMap.Count);
  for I:=0 to DimensionMap.Count-1 do
    D[I]:=DimensionMap[I].DimLink as TCommonDimItem;
  DimensionMap.Assign(DimMap);
  for I:=0 to DimensionMap.Count-1 do
    if D[I]<>nil then D[I].Def:=DimensionMap[I];
  DimensionMap.Active:=True;
  if Active then begin
    SetState(dcInactive);
    SetState(dcBrowseAllData);
  end else begin
    Active:=True;
  end;
end;

procedure TFxCustomStore.InternalOpenCache;
var
  OldCursor: HCursor;
begin
  if ShowProgressDialog then
    OldCursor:= GetCursor
  else
    OldCursor:= SetCursor(LoadCursor(0, IDC_WAIT));
  try
    try
      if ShowProgressDialog then
        ExecuteProgressDialog(DataCache.UpdateCache)
      else
        DataCache.UpdateCache(Self);
    except
      on EAbort do ;
    else
      raise;
    end;
  finally
    SetCursor(OldCursor);
  end;
end;

procedure TFxCustomStore.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive then Active := True;
  except
    if csDesigning in ComponentState then begin
      if Assigned(Classes.ApplicationHandleException) then
        ApplicationHandleException(Self);
    end else
      raise;
  end;
end;

procedure TFxCustomStore.DoActive;
begin
  try
    if DataSet=nil then 
      raise ECacheError.Create(SDataSetNotAssigned);
    DataSet.Active:=True;
    DimensionMap.Active:=True;
    FCache.Active := True;
    InternalOpenCache;
    State:=GetCubeState;
  except
    State:=dcInactive;
    DataCache.Active:=False;
    DimensionMap.Active:=False;
    raise;
  end;
end;

function TFxCustomStore.GetDataSet: TDataSet;
begin
  Result:=FDataSet;
end;

function TFxCustomStore.GetDimensions: TDimensions;
begin
  Result:=DataCache.Dimensions;
end;

function TFxCustomStore.GetSummaries: TSummaries;
begin
  Result:=DataCache.Summaries;
end;

end.

